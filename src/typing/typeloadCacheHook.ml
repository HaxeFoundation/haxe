open Globals
open TType
open TyperPass
open Common
open TFunctions

type find_module_result =
	| GoodModule of module_def
	| BadModule of module_skip_reason
	| BadBinaryModule of (HxbData.module_cache * module_skip_reason)
	| BinaryModule of HxbData.module_cache
	| NoModule

let type_module_hook : (Common.context -> (typer_pass -> (unit -> unit) -> unit) -> path -> pos -> find_module_result) ref = ref (fun _ _ _ _ -> NoModule)

let create_fake_module com file =
	let file = Path.get_full_path file in
	let path = (["$DEP"],file) in
	try
		com.module_lut#find path
	with Not_found ->
		let mdep = {
			m_id = alloc_mid();
			m_path = path;
			m_types = [];
			m_statics = None;
			m_extra = module_extra file (Define.get_signature com.defines) (file_time file) MFake com.part_scope.compilation_step [];
		} in
		com.module_lut#add path mdep;
		mdep