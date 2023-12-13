open Globals
open Type

class hxb_restore
	(cs : CompilationCache.t)
	(com : Common.context)
	(local_module_lut : ((string * path),module_def) Lookup.hashtbl_lookup)
= object(self)

	method find (path : path) (sign : string) =
		try
			local_module_lut#find (sign, path)
		with Not_found -> try
			let m = com.module_lut#find path in
			if m.m_extra.m_sign <> sign then raise Not_found;
			(match m.m_extra.m_cache_state with
				| MSBad reason -> raise (Bad_module (path, reason))
				| _ -> m
			)
		with Not_found ->
			let cc = cs#get_context sign in
			match cc#find_module_opt path with
			| Some m -> m
			| None ->
				begin match cc#get_hxb_module path with
					| None -> raise Not_found
					| Some { mc_extra = { m_cache_state = MSBad reason }} -> raise (Bad_module (path, reason))
					| Some mc -> self#load cc mc
				end

	method load (cc : CompilationCache.context_cache) (mc : module_cache) =
		(* if com.module_ignore_hxb#find mc.mc_path then raise Not_found; *)
		trace (Printf.sprintf "Loading module %s from hxb cache" (s_type_path mc.mc_path));
		let reader = new HxbReader.hxb_reader (self#make_module mc) self#add_module self#resolve_type (fun () -> ()) in
		try reader#read (IO.input_bytes mc.mc_bytes) true null_pos with
		| Bad_module (path, reason) ->
			ServerMessage.skipping_dep com "" (path,(Printer.s_module_skip_reason reason));
			local_module_lut#remove (mc.mc_extra.m_sign, mc.mc_path);
			(* local_module_lut#remove path; *)
			raise (Bad_module (mc.mc_path, DependencyDirty (path, reason)))
		| HxbData.HxbFailure e ->
			prerr_endline (Printf.sprintf "Error loading %s from hxb: %s" (s_type_path mc.mc_path) e);
			trace (Printf.sprintf "Error loading %s from hxb: %s" (s_type_path mc.mc_path) e);
			local_module_lut#remove (mc.mc_extra.m_sign, mc.mc_path);
			raise (HxbData.HxbFailure e)
		| e ->
			prerr_endline (Printf.sprintf "Error loading %s from hxb" (s_type_path mc.mc_path));
			trace (Printf.sprintf "Error loading %s from hxb" (s_type_path mc.mc_path));
			local_module_lut#remove (mc.mc_extra.m_sign, mc.mc_path);
			raise e

	method add_module (m : module_def) =
		trace (Printf.sprintf "Add module %s" (s_type_path m.m_path));
		local_module_lut#add (m.m_extra.m_sign, m.m_path) m

	method resolve_type (sign : string) (pack : string list) (mname : string) (tname : string) =
		let path = (pack,mname) in
		try
			let m = try self#find path sign with Not_found -> print_endline "cannot find module"; raise Not_found in
			List.find (fun t -> snd (t_path t) = tname) m.m_types
		with
			| Bad_module (_, reason) -> raise (Bad_module (path, reason))
			| Not_found -> raise Not_found

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
				m_cache_state = MSRestored mc.mc_extra.m_cache_state;
			}
		}

end

let find
	(local_module_lut : ((string * path),module_def) Lookup.hashtbl_lookup)
	(cs : CompilationCache.t)
	(sign : string)
	(com : Common.context)
	(path : path)
	=
	(* trace (Printf.sprintf "Find module %s" (s_type_path path)); *)
	(* trace_call_stack (); *)
	let loader = new hxb_restore cs com local_module_lut in
	loader#find path sign

let find_type
	(local_module_lut : ((string * path),module_def) Lookup.hashtbl_lookup)
	(cs : CompilationCache.t)
	(sign : string)
	(com : Common.context)
	(path : path)
	=
	(* trace (Printf.sprintf "Find type %s" (s_type_path path)); *)
	(* trace_call_stack (); *)
	let m = find local_module_lut cs sign com path in
	List.find (fun t -> snd (t_path t) = (snd path)) m.m_types

