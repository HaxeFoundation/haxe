open Globals
open Type

class hxb_restore
	(cs : CompilationCache.t)
	(com : Common.context)
	(load_module : Globals.path -> string -> module_def)
	(* (check_module : Globals.path -> module_def_extra -> Type.module_skip_reason option) *)
	(check_module : Globals.path -> module_def_extra -> Globals.pos -> Type.module_skip_reason option)
	(pos : Globals.pos)
= object(self)

	(* method maybe_remove_dirty path = try begin *)
	(* 	let m = com.module_lut#find path in *)
	(* 	(1* trace (Printf.sprintf "dep %s state = %s" (s_type_path path) (Printer.s_module_skip_reason reason)); *1) *)
	(* 	match m.m_extra.m_cache_state with *)
	(* 	| MSBad _ -> *)
	(* 		trace (Printf.sprintf " -> removing dirty dependency %s" (s_type_path path)); *)
	(* 		(1* com.module_lut#remove path; *1) *)
	(* 	| _ -> *)
	(* 		(1* trace (Printf.sprintf " -> dependency %s was fine" (s_type_path path)); *1) *)
	(* 		(); *)
	(* end with Not_found -> () *)

	(* method remove_dirty_dep = function *)
	(* 	| DependencyDirty (path, reason) -> *)
	(* 		self#maybe_remove_dirty path; *)
	(* 		self#remove_dirty_dep reason *)
	(* 	| _ -> () *)

	method find (path : path) (sign : string) =
		try begin
			let m = com.module_lut#find path in
			if m.m_extra.m_sign <> sign then raise Not_found;
			(match m.m_extra.m_cache_state with
				| MSBad reason ->
					trace (Printf.sprintf "com.module_lut has dirty module %s ?!" (s_type_path path));
					(* self#maybe_remove_dirty path; *)
					(* self#remove_dirty_dep (DependencyDirty (path, reason)); *)
					raise (Bad_module (path, reason))
					(* raise Not_found *)
				| _ -> m
			)
		end with Not_found ->
			let cc = cs#get_context sign in
			match cc#find_module_opt path with
			| Some m -> m
			| None ->
				begin match cc#get_hxb_module path with
					| None -> raise Not_found
					| Some { mc_extra = { m_cache_state = MSBad reason }} ->
						raise (Bad_module (path, reason))
						(* raise Not_found *)
					| Some mc -> self#load cc mc
				end

	method load (cc : CompilationCache.context_cache) (mc : module_cache) =
		let tcheck = Timer.timer ["server";"module cache";"check"] in
		begin match check_module mc.mc_path mc.mc_extra pos with
		| None -> ()
		| Some reason ->
			tcheck();
			(* trace (Printf.sprintf "Skip loading %s from hxb because %s" (s_type_path mc.mc_path) (Printer.s_module_skip_reason reason)); *)
			raise (Bad_module (mc.mc_path, reason))
		end;
		tcheck();

		(* if com.module_ignore_hxb#find mc.mc_path then raise Not_found; *)
		(* trace (Printf.sprintf "Loading module %s from hxb cache" (s_type_path mc.mc_path)); *)
		let reader = new HxbReader.hxb_reader (self#make_module mc) self#add_module self#resolve_type (fun () -> ()) in
		try reader#read (IO.input_bytes mc.mc_bytes) true null_pos with
		| Bad_module (path, reason) ->
			trace (Printf.sprintf "Error loading module %s from hxb cache: dirty!" (s_type_path mc.mc_path));
			ServerMessage.skipping_dep com "" (path,(Printer.s_module_skip_reason reason));

			(* com.module_lut#remove mc.mc_path; *)
			let skip_reason = DependencyDirty (path, reason) in
			(* self#remove_dirty_dep skip_reason; *)
			raise (Bad_module (mc.mc_path, skip_reason))
		| HxbData.HxbFailure e ->
			prerr_endline (Printf.sprintf "Error loading %s from hxb: %s" (s_type_path mc.mc_path) e);
			(* com.module_lut#remove mc.mc_path; *)
			trace (Printf.sprintf "Error loading %s from hxb: %s" (s_type_path mc.mc_path) e);
			raise (HxbData.HxbFailure e)
		| e ->
			prerr_endline (Printf.sprintf "Error loading %s from hxb" (s_type_path mc.mc_path));
			(* com.module_lut#remove mc.mc_path; *)
			trace (Printf.sprintf "Error loading %s from hxb" (s_type_path mc.mc_path));
			trace (Printexc.to_string e);
			trace (Printexc.get_backtrace ());
			raise e

	method add_module (m : module_def) =
		trace (Printf.sprintf "Add module %s" (s_type_path m.m_path));
		if com.module_lut#mem m.m_path then
				prerr_endline (Printf.sprintf "Hxb restore adding already existing module %s" (s_type_path m.m_path));

		ServerMessage.reusing com "" m;
		(* com.module_lut#add m.m_path m *)
		match m.m_extra.m_cache_state with
		(* | MSBad _ | MSRestored MSBad _ -> *)
		| MSBad _ ->
			trace (Printf.sprintf "[hxb restore] Trying to add module %s with state %s" (s_type_path m.m_path) (Printer.s_module_cache_state m.m_extra.m_cache_state))
		| _ ->
			(* (match m.m_extra.m_cache_state with *)
			(* | MSRestored _ -> () *)
			(* | state -> m.m_extra.m_cache_state <- MSRestored state); *)
			com.module_lut#add m.m_path m;

	method resolve_type (sign : string) (pack : string list) (mname : string) (tname : string) =
		let path = (pack,mname) in
		let m = try self#find path sign with Not_found -> load_module path sign in
		List.find (fun t -> snd (t_path t) = tname) m.m_types

	method make_module (mc : module_cache) (path : path) (file : string) =
		{
			m_id = alloc_mid();
			m_path = path;
			m_types = [];
			m_statics = None;
			m_extra = { mc.mc_extra with
				m_added = com.compilation_step;
				m_checked = 0;
				m_processed = 1;
				m_features = Hashtbl.create 0; (* ? *)
				m_if_feature = [];
				m_cache_state = MSGood;
				(* m_cache_state = MSRestored mc.mc_extra.m_cache_state; *)
			}
		}

end

let find
	(cs : CompilationCache.t)
	(sign : string)
	(com : Common.context)
	(load_module : Globals.path -> string -> module_def)
	(check_module : Globals.path -> module_def_extra -> Globals.pos -> Type.module_skip_reason option)
	(path : path)
	(pos : Globals.pos)
	=
	(* trace (Printf.sprintf "Find module %s" (s_type_path path)); *)
	(* trace_call_stack (); *)
	(* let group = com.module_lut#start_group in *)
	let loader = new hxb_restore cs com load_module check_module pos in
	(* try *)
		let m = loader#find path sign in
		(* ignore(com.module_lut#commit_group group); *)
		m
	(* with e -> *)
	(* 	let n = com.module_lut#discard_group group in *)
	(* 	if n > 0 then begin *)
	(* 		trace (Printf.sprintf "Failed to load %s - discarded %i modules" (s_type_path path) n); *)
	(* 		trace (Printexc.to_string e); *)
	(* 	end; *)
	(* 	raise e *)

let find_type
	(cs : CompilationCache.t)
	(sign : string)
	(com : Common.context)
	(load_module : Globals.path -> string -> module_def)
	(check_module : Globals.path -> module_def_extra -> Globals.pos -> Type.module_skip_reason option)
	(* (check_module : Globals.path -> module_def_extra -> Type.module_skip_reason option) *)
	(path : path)
	(pos : Globals.pos)
	=
	(* trace (Printf.sprintf "Find type %s" (s_type_path path)); *)
	(* trace_call_stack (); *)
	let m = find cs sign com load_module check_module path pos in
	List.find (fun t -> snd (t_path t) = (snd path)) m.m_types

