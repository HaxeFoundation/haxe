open Globals
open Ast
open Json
open Type
open Common

type cached_file = {
	c_time : float;
	c_package : string list;
	c_decls : type_decl list;
	mutable c_module_name : string option;
}

type cached_directory = {
	c_path : string;
	mutable c_mtime : float;
}

type cache = {
	c_haxelib : (string list, string list) Hashtbl.t;
	c_files : ((string * string), cached_file) Hashtbl.t;
	c_modules : (path * string, module_def) Hashtbl.t;
	c_directories : (string, cached_directory list) Hashtbl.t;
}

type context_sign = {
	cs_json : Json.t;
	cs_index : int;
}

type t = {
	cache : cache;
	mutable signs : (string * context_sign) list;
	mutable initialized : bool;
}

type context_options =
	| NormalContext
	| MacroContext
	| NormalAndMacroContext

let instance : t option ref = ref None

let create_cache () = {
	c_haxelib = Hashtbl.create 0;
	c_files = Hashtbl.create 0;
	c_modules = Hashtbl.create 0;
	c_directories = Hashtbl.create 0;
}

let create () =
	let cs = {
		cache = create_cache();
		signs = [];
		initialized = false;
	} in
	instance := Some cs;
	cs

let get () =
	!instance

let runs () =
	!instance <> None

let force () = match !instance with None -> assert false | Some i -> i

let is_initialized cs =
	cs.initialized = true

let set_initialized cs =
	cs.initialized <- true

let get_context_files cs signs =
	Hashtbl.fold (fun (file,sign) cfile acc ->
		if (List.mem sign signs) then (file,cfile) :: acc
		else acc
	) cs.cache.c_files []

(* signatures *)

let get_sign cs sign =
	List.assoc sign cs.signs

let has_sign cs sign =
	List.mem_assoc sign cs.signs

let add_sign cs sign desc com =
	let i = List.length cs.signs in
	let jo = JObject [
		"index",JInt i;
		"desc",JString desc;
		"platform",JString (platform_name com.platform);
		"classPaths",JArray (List.map (fun s -> JString s) com.class_path);
		"signature",JString (Digest.to_hex sign);
		"defines",JArray (PMap.foldi (fun k v acc -> JObject [
			"key",JString k;
			"value",JString v;
		] :: acc) com.defines.values []);
	] in
	cs.signs <- (sign,{cs_json = jo;cs_index = i}) :: cs.signs;
	i

let maybe_add_context_sign cs com desc =
	let sign = Define.get_signature com.defines in
	if not (has_sign cs sign) then ignore (add_sign cs sign desc com)

let get_signs cs =
	cs.signs

(* modules *)

let find_module cs key =
	Hashtbl.find cs.cache.c_modules key

let cache_module cs key value =
	Hashtbl.replace cs.cache.c_modules key value

let taint_modules cs file =
	Hashtbl.iter (fun _ m -> if m.m_extra.m_file = file then m.m_extra.m_dirty <- Some m) cs.cache.c_modules

let iter_modules cs com f =
	let sign = Define.get_signature com.defines in
	Hashtbl.iter (fun (_,sign') m -> if sign = sign' then f m) cs.cache.c_modules

let is_cached_module cs com path =
	let sign = Define.get_signature com.defines in
	Hashtbl.mem cs.cache.c_modules (path,sign)

(* files *)

let find_file cs key =
	Hashtbl.find cs.cache.c_files key

let cache_file cs key time data =
	Hashtbl.replace cs.cache.c_files key { c_time = time; c_package = fst data; c_decls = snd data; c_module_name = None }

let remove_file cs key =
	Hashtbl.remove cs.cache.c_files key

let remove_files cs file =
	List.iter (fun (sign,_) -> remove_file cs (file,sign)) cs.signs

let iter_files cs com f =
	let sign = Define.get_signature com.defines in
	Hashtbl.iter (fun (file,sign') decls -> if sign = sign' then f file decls) cs.cache.c_files

let get_file_list cs com =
	let sign = Define.get_signature com.defines in
	Hashtbl.fold (fun (file,sign') decls acc -> if sign = sign' then (file,decls) :: acc else acc) cs.cache.c_files []

let get_module_name_of_cfile file cfile = match cfile.c_module_name with
	| None ->
		let name = Path.module_name_of_file file in
		cfile.c_module_name <- Some name;
		name
	| Some name ->
		name

let get_files cs =
	cs.cache.c_files

(* haxelibs *)

let find_haxelib cs key =
	Hashtbl.find cs.cache.c_haxelib key

let cache_haxelib cs key value =
	Hashtbl.replace cs.cache.c_haxelib key value

(* directories *)

let create_directory path mtime = {
	c_path = path;
	c_mtime = mtime;
}

let find_directories cs key =
	Hashtbl.find cs.cache.c_directories key

let add_directories cs key value =
	Hashtbl.replace cs.cache.c_directories key value

let remove_directory cs key value =
	try
		let current = find_directories cs key in
		Hashtbl.replace cs.cache.c_directories key (List.filter (fun dir -> dir.c_path <> value) current);
	with Not_found ->
		()

let has_directory cs key value =
	try
		List.exists (fun dir -> dir.c_path = value) (find_directories cs key)
	with Not_found ->
		false

let add_directory cs key value =
	try
		let current = find_directories cs key in
		add_directories cs key (value :: current)
	with Not_found ->
		add_directories cs key [value]

let clear_directories cs key =
	Hashtbl.remove cs.cache.c_directories key

(* context *)

let rec cache_context cs com =
	let cache_module m =
		cache_module cs (m.m_path,m.m_extra.m_sign) m;
	in
	List.iter cache_module com.modules;
	match com.get_macros() with
	| None -> ()
	| Some com -> cache_context cs com