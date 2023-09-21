open Globals
open Type

class hxb_restore
	(cc : CompilationCache.context_cache)
	(com : Common.context)
= object(self)

	method find (path : path) =
		try begin
			let m = com.module_lut#find path in
			(match m.m_extra.m_cache_state with
				| MSBad reason -> raise (Bad_module (path, reason))
				| _ -> m
			)
		end with
		| Not_found ->
			match cc#find_module_opt path with
			| Some m -> m
			| None ->
				begin match cc#get_hxb_module path with
					| None -> raise Not_found
					| Some { mc_extra = { m_cache_state = MSBad reason }} -> raise (Bad_module (path, reason))
					| Some mc -> self#load mc
				end

	method load (mc : module_cache) =
		let reader = new HxbReader.hxb_reader (self#make_module mc) self#add_module self#resolve_type (fun () -> ()) in
		try reader#read (IO.input_bytes mc.mc_bytes) true null_pos with
		| Bad_module (path, reason) ->
			ServerMessage.skipping_dep com "" (path,(Printer.s_module_skip_reason reason));
			com.module_lut#remove mc.mc_path;
			(* com.module_lut#remove path; *)
			raise (Bad_module (mc.mc_path, DependencyDirty (path, reason)))
		| e ->
			ServerMessage.debug_msg (Printf.sprintf "[1] Error loading %s from hxb" (s_type_path mc.mc_path));
			com.module_lut#remove mc.mc_path;
			raise e

	method add_module (m : module_def) =
		if com.module_lut#mem m.m_path then
			ServerMessage.debug_msg (Printf.sprintf "Hxb restore adding already existing module %s" (s_type_path m.m_path));

		ServerMessage.reusing com "" m;
		com.module_lut#add m.m_path m

	method resolve_type (pack : string list) (mname : string) (tname : string) =
		let path = (pack,mname) in
		try
			let m = try self#find path with Not_found -> print_endline "cannot find module"; raise Not_found in
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
				m_time = (Common.file_time file)
			}
		}

end

let find (cc : CompilationCache.context_cache) (com : Common.context) (path : path) =
	let loader = new hxb_restore cc com in
	loader#find path

let find_type (cc : CompilationCache.context_cache) (com : Common.context) (path : path) =
	let m = find cc com path in
	List.find (fun t -> snd (t_path t) = (snd path)) m.m_types

