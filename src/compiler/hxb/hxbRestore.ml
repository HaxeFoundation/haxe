open Globals
open Type

class hxb_restore
	(cc : CompilationCache.context_cache)
	(com : Common.context)
= object(self)

	method find (path : path) =
		try com.module_lut#find path with
		| Not_found ->
			match cc#find_module_opt path with
			| Some m -> m
			| None ->
					begin match cc#get_hxb_module path with
						| None -> raise Not_found
						| Some mc -> self#load mc
					end

	method load (mc : module_cache) =
		(* print_endline (Printf.sprintf "[1] Load %s from hxb" (s_type_path mc.mc_path)); *)
		let reader = new HxbReader.hxb_reader (self#make_module mc) self#add_module self#resolve_type (fun () -> ()) in
		reader#read (IO.input_bytes mc.mc_bytes) true null_pos

	method add_module (m : module_def) =
		com.module_lut#add m.m_path m

	method resolve_type (pack : string list) (mname : string) (tname : string) =
		let path = (pack,mname) in
		let m = self#find path in
		List.find (fun t -> snd (t_path t) = tname) m.m_types

	method make_module (mc : module_cache) (path : path) (file : string) =
		{
			m_id = alloc_mid();
			m_path = path;
			m_types = [];
			m_statics = None;
			m_extra = { mc.mc_extra with
				m_added = 1;
				m_checked = 0;
				m_processed = 1;
				m_time = (Common.file_time file)
			}
		}

end

let find (cc : CompilationCache.context_cache) (com : Common.context) (path : path) =
	let loader = new hxb_restore cc com in
	loader#find path
