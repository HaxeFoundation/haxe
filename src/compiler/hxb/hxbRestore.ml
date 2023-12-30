open Globals
open Type
open Common

let restore_counter = ref 0

let handle_cache_bound_objects com cbol =
	List.iter (function
		| Resource(name,data) ->
			Hashtbl.replace com.resources name data
		| IncludeFile(file,position) ->
			com.include_files <- (file,position) :: com.include_files
	) cbol

class hxb_restore
	(cs : CompilationCache.t)
	(com : Common.context)
	(load_module : Globals.path -> string -> module_def)
	(* (check_module : Globals.path -> module_def_extra -> Type.module_skip_reason option) *)
	(check_module : Globals.path -> module_def_extra -> Globals.pos -> Type.module_skip_reason option)
	(pos : Globals.pos)
= object(self)
	method find (path : path) (sign : string) =
		try begin
			let m = com.module_lut#find path in
			if m.m_extra.m_sign <> sign then raise Not_found;
			(match m.m_extra.m_cache_state with
				| MSBad reason -> raise (Bad_module (path, reason))
				| _ -> m
			)
		end with Not_found ->
			let cc = cs#get_context sign in
			match cc#find_module_opt path with
			| Some m -> m
			| None ->
				begin match cc#get_hxb_module path with
					| None -> raise Not_found
					| Some { mc_extra = { m_cache_state = MSBad reason }} -> raise (Bad_module (path, reason))
					| Some mc -> self#load cc mc sign
				end

	method load (cc : CompilationCache.context_cache) (mc : module_cache) (sign : string) =
		let tcheck = Timer.timer ["server";"module cache";"check"] in
		(match check_module mc.mc_path mc.mc_extra pos with
		| None -> ()
		| Some reason ->
			tcheck();
			raise (Bad_module (mc.mc_path, reason)));
		tcheck();

		let reader = new HxbReader.hxb_reader (self#make_module mc) self#add_module self#resolve_type (fun () -> ()) in
		try
			let m = reader#read (IO.input_bytes mc.mc_bytes) true null_pos in
			incr restore_counter;
			m
		with
		| Bad_module (path, reason) ->
			prerr_endline (Printf.sprintf "Error loading module %s from hxb cache: dirty!" (s_type_path mc.mc_path));
			ServerMessage.skipping_dep com "" (path,(Printer.s_module_skip_reason reason));
			let skip_reason = DependencyDirty (path, reason) in
			raise (Bad_module (mc.mc_path, skip_reason))
		| HxbData.HxbFailure e ->
			prerr_endline (Printf.sprintf "Error loading %s from hxb: %s" (s_type_path mc.mc_path) e);
			raise (HxbData.HxbFailure e)
		| e ->
			(* prerr_endline (Printf.sprintf "Error loading %s from hxb" (s_type_path mc.mc_path)); *)
			(* trace (Printexc.to_string e); *)
			(* trace (Printexc.get_backtrace ()); *)
			raise e

	method add_module (m : module_def) =
		ServerMessage.reusing com "" m;
		match m.m_extra.m_cache_state with
		| MSBad _ ->
			trace (Printf.sprintf "[hxb restore] Trying to add module %s with state %s" (s_type_path m.m_path) (Printer.s_module_cache_state m.m_extra.m_cache_state))
		| _ ->
			com.module_lut#add m.m_path m;
			handle_cache_bound_objects com m.m_extra.m_cache_bound_objects;

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
	let loader = new hxb_restore cs com load_module check_module pos in
	loader#find path sign

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
	let m = find cs sign com load_module check_module path pos in
	List.find (fun t -> snd (t_path t) = (snd path)) m.m_types

