open StringHelper
open ClassPath

type resolved_file = {
	file : string;
	class_path : class_path;
}

let create_resolved_file file class_path = {
	file;
	class_path;
}

(* We need to clean-up absolute ("") vs. cwd ("."). *)
let absolute_class_path = new directory_class_path "" User

class class_paths = object(self)
	val mutable l = []
	val file_lookup_cache = new Lookup.hashtbl_lookup;
	val mutable platform_ext = ""
	val mutable is_loading_core_api = false

	method lock_context (platform_name : string) (core_api : bool) : unit =
		platform_ext <- "." ^ platform_name;
		is_loading_core_api <- core_api;
		self#clear_cache

	method as_string_list =
		List.map (fun cp -> cp#path) l

	method add (cp : class_path) =
		l <- cp :: l;
		self#clear_cache

	method push (cp : class_path) =
		l <- l @ [cp];
		self#clear_cache

	method find (f : class_path -> bool) =
		List.find f l

	method iter (f : class_path -> unit) =
		List.iter f l

	method exists (f : class_path -> bool) =
		List.exists f l

	method filter (f : class_path -> bool) =
		List.filter f l

	method modify (f : class_path -> class_path list) (cpl : class_path list) =
		let rec loop acc l = match l with
			| [] ->
				List.rev acc
			| cp :: l ->
				let cpl = f cp in
				loop (cpl @ acc) l
		in
		l <- loop [] cpl;
		self#clear_cache

	method modify_inplace (f : class_path -> class_path list) =
		self#modify f l

	method get_std_paths =
		self#filter (fun cp -> cp#is_std_path)

	method as_list =
		l

	method clear_cache =
		file_lookup_cache#clear;
		List.iter (fun cp -> cp#clear_cache) l

	method cache_directory (cp : class_path) (dir : string) (f_search : string) (dir_listing : string array) =
		(*
			This function is invoked for each file in the `dir`.
			Each file is checked if it's specific for current platform
			(e.g. ends with `.js.hx` while compiling for JS).
			If it's not platform-specific:
				Check the lookup cache and if the file is not there store full file path in the cache.
			If the file is platform-specific:
				Store the full file path in the lookup cache probably replacing the cached path to a
				non-platform-specific file.
		*)
		let found = ref None in
		let f_dir = Filename.dirname f_search in
		let prepare_file file_own_name =
			let relative_to_classpath = if f_dir = "." then file_own_name else f_dir ^ "/" ^ file_own_name in
			(* `representation` is how the file is referenced to. E.g. when it's deduced from a module path. *)
			let is_platform_specific,representation =
				(* Platform specific file extensions are not allowed for loading @:coreApi types. *)
				if is_loading_core_api then
					false,relative_to_classpath
				else begin
					let ext = extension relative_to_classpath in
					let second_ext = extension (remove_extension relative_to_classpath) in
					(* The file contains double extension and the secondary one matches current platform *)
					if platform_ext = second_ext then
						true,(remove_extension (remove_extension relative_to_classpath)) ^ ext
					else
						false,relative_to_classpath
				end
			in
			(*
				Store current full path for `representation` if
				- we're loading @:coreApi
				- or this is a platform-specific file for `representation`
				- this `representation` was never found before
			*)
			if is_loading_core_api || is_platform_specific || not (file_lookup_cache#mem representation) then begin
				let full_path = if dir = "." then file_own_name else dir ^ "/" ^ file_own_name in
				let full_path = Some(create_resolved_file full_path cp) in
				file_lookup_cache#add representation full_path;
				if representation = f_search then found := full_path
			end
		in
		Array.iter prepare_file dir_listing;
		!found

	method find_file_noraise (f : string) =
		try
			match file_lookup_cache#find f with
			| None ->
				None
			| Some f ->
				Some f
		with
		| Not_found when Path.is_absolute_path f ->
			let r = if Sys.file_exists f then
				Some (create_resolved_file f absolute_class_path)
			else
				None
			in
			file_lookup_cache#add f r;
			r
		| Not_found ->
			let rec loop = function
				| [] ->
					None
				| cp :: l ->
					begin match cp#get_uncached_dir_listing f with
						| None ->
							loop l
						| Some(dir,dir_listing) ->
							match self#cache_directory cp dir f dir_listing with
								| Some f ->
									Some f
								| None ->
									loop l
					end
			in
			let r = loop l in
			file_lookup_cache#add f r;
			r

	method find_file (f : string) =
		match self#find_file_noraise f with
		| None -> raise Not_found
		| Some f -> f

	method relative_path file =
		let slashes path = String.concat "/" (ExtString.String.nsplit path "\\") in
		let fpath = slashes (Path.get_full_path file) in
		let fpath_lower = String.lowercase_ascii fpath in
		let flen = String.length fpath_lower in
		let rec loop = function
			| [] ->
				file
			| path :: l ->
				let path = path#path in
				let spath = String.lowercase_ascii (slashes path) in
				let slen = String.length spath in
				if slen > 0 && slen < flen && String.sub fpath_lower 0 slen = spath then String.sub fpath slen (flen - slen) else loop l
		in
		loop l

	method dump =
		print_endline (Printf.sprintf "Class paths for %s%s:" platform_ext (if is_loading_core_api then " (coreApi)" else ""));
		List.iter (fun cp -> cp#dump) l
end
