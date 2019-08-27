open Globals
open Ast
open Json
open Type
open Define

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

type cached_native_lib = {
	c_nl_mtime : float;
	c_nl_files : (path,Ast.package) Hashtbl.t;
}

type context_cache = {
	c_files : (string,cached_file) Hashtbl.t;
	c_modules : (path,module_def) Hashtbl.t;
	c_removed_files : (string,unit) Hashtbl.t;
}

type cache = {
	c_contexts : (string,context_cache) Hashtbl.t;
	c_haxelib : (string list, string list) Hashtbl.t;
	c_directories : (string, cached_directory list) Hashtbl.t;
	c_native_libs : (string,cached_native_lib) Hashtbl.t;
	c_initialization_status : (string,bool) Hashtbl.t;
}

type context_sign = {
	cs_json : Json.t;
	cs_index : int;
}

type t = {
	cache : cache;
	mutable signs : (string * context_sign) list;
}

type context_options =
	| NormalContext
	| MacroContext
	| NormalAndMacroContext

let instance : t option ref = ref None

let create_cache () = {
	c_haxelib = Hashtbl.create 0;
	c_contexts = Hashtbl.create 0;
	c_directories = Hashtbl.create 0;
	c_native_libs = Hashtbl.create 0;
	c_initialization_status = Hashtbl.create 0;
}

let create_context_cache () = {
	c_modules = Hashtbl.create 0;
	c_files = Hashtbl.create 0;
	c_removed_files = Hashtbl.create 0;
}

let create () =
	let cs = {
		cache = create_cache();
		signs = [];
	} in
	instance := Some cs;
	cs

let get () =
	!instance

let runs () =
	!instance <> None

let force () = match !instance with None -> assert false | Some i -> i

let is_initialized cs sign =
	try Hashtbl.find cs.cache.c_initialization_status sign with Not_found -> false

let set_initialized cs sign value =
	Hashtbl.replace cs.cache.c_initialization_status sign value

let get_context_files cs signs =
	Hashtbl.fold (fun sign cc acc ->
		if List.mem sign signs then Hashtbl.fold (fun file cfile acc -> (file,cfile) :: acc) cc.c_files acc
		else acc
	) cs.cache.c_contexts []

(* signatures *)

let get_sign cs sign =
	List.assoc sign cs.signs

let has_sign cs sign =
	List.mem_assoc sign cs.signs

let add_sign cs sign desc platform class_path defines =
	let i = List.length cs.signs in
	let jo = JObject [
		"index",JInt i;
		"desc",JString desc;
		"platform",JString (platform_name platform);
		"classPaths",JArray (List.map (fun s -> JString s) class_path);
		"signature",JString (Digest.to_hex sign);
		"defines",JArray (PMap.foldi (fun k v acc -> JObject [
			"key",JString k;
			"value",JString v;
		] :: acc) defines.values []);
	] in
	cs.signs <- (sign,{cs_json = jo;cs_index = i}) :: cs.signs;
	i

let get_signs cs =
	cs.signs

let get_cache cs sign =
	try
		Hashtbl.find cs.cache.c_contexts sign
	with Not_found ->
		let cache = create_context_cache () in
		Hashtbl.add cs.cache.c_contexts sign cache;
		cache

let get_caches cs =
	cs.cache.c_contexts

(* modules *)

let find_module cc path =
	Hashtbl.find cc.c_modules path

let cache_module cc path value =
	Hashtbl.replace cc.c_modules path value

let taint_modules cs file =
	Hashtbl.iter (fun _ cc ->
		Hashtbl.iter (fun _ m ->
			if m.m_extra.m_file = file then m.m_extra.m_dirty <- Some m
		) cc.c_modules
	) cs.cache.c_contexts

let filter_modules cs file =
	let removed = DynArray.create () in
	(* TODO: Using filter_map_inplace would be better, but we can't move to OCaml 4.03 yet *)
	Hashtbl.iter (fun _ cc ->
		Hashtbl.iter (fun k m ->
			if m.m_extra.m_file = file then DynArray.add removed (cc,k,m);
		) cc.c_modules
	) cs.cache.c_contexts;
	DynArray.iter (fun (cc,k,_) -> Hashtbl.remove cc.c_modules k) removed;
	DynArray.to_list removed

(* files *)

let find_file cc key =
	Hashtbl.find cc.c_files key

let cache_file cc key time data =
	Hashtbl.replace cc.c_files key { c_time = time; c_package = fst data; c_decls = snd data; c_module_name = None }

let remove_file cc key =
	if Hashtbl.mem cc.c_files key then begin
		Hashtbl.remove cc.c_files key;
		Hashtbl.replace cc.c_removed_files key ()
	end

(* Like remove_file, but doesn't keep track of the file *)
let remove_file_for_real cc key =
	Hashtbl.remove cc.c_files key

let remove_files cs file =
	Hashtbl.iter (fun _ cc-> remove_file cc file) cs.cache.c_contexts

let get_module_name_of_cfile file cfile = match cfile.c_module_name with
	| None ->
		let name = Path.module_name_of_file file in
		cfile.c_module_name <- Some name;
		name
	| Some name ->
		name

let get_files cs =
	Hashtbl.fold (fun sign cc acc ->
		Hashtbl.fold (fun file cfile acc -> (sign,file,cfile) :: acc) cc.c_files acc
	) cs.cache.c_contexts []

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