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
	let sign = Define.get_signature com.defines in
	let cc = com.cs#get_context sign in
	let key = com.part_scope.file_keys#get file in
	let file = Path.get_full_path file in
	let mdep = (try cc#find_fake_module key with Not_found ->
		let mdep = {
			m_id = alloc_mid();
			m_path = (["$DEP"],file);
			m_types = [];
			m_statics = None;
			m_extra = module_extra file sign (file_time file) MFake com.part_scope.compilation_step [];
		} in
		cc#add_fake_module key mdep;
		mdep
	) in
	com.module_lut#add mdep.m_path mdep;
	mdep