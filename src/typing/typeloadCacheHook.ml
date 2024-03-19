open Globals
open TType
open Common
open TFunctions

type find_module_result =
	| GoodModule of module_def
	| BadModule of module_skip_reason
	| BinaryModule of HxbData.module_cache
	| NoModule

let type_module_hook : (Common.context -> ((unit -> unit) -> unit) -> path -> pos -> find_module_result) ref = ref (fun _ _ _ _ -> NoModule)

let fake_modules = Hashtbl.create 0

let create_fake_module com file =
	let key = com.file_keys#get file in
	let file = Path.get_full_path file in
	let mdep = (try Hashtbl.find fake_modules key with Not_found ->
		let mdep = {
			m_id = alloc_mid();
			m_path = (["$DEP"],file);
			m_types = [];
			m_statics = None;
			m_extra = module_extra file (Define.get_signature com.defines) (file_time file) MFake com.compilation_step;
		} in
		Hashtbl.add fake_modules key mdep;
		mdep
	) in
	com.module_lut#add mdep.m_path mdep;
	mdep
