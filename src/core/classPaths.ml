open StringHelper
open ClassPath

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
		file_lookup_cache#clear

	method push (cp : class_path) =
		l <- l @ [cp];
		file_lookup_cache#clear

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
		file_lookup_cache#clear

	method modify_inplace (f : class_path -> class_path list) =
		self#modify f l

	method get_std_paths =
		self#filter (fun cp -> cp#is_std_path)

	method as_list =
		l

	method clear_cache =
		file_lookup_cache#clear;
		List.iter (fun cp -> cp#clear_cache) l

	method cache_directory (dir : string) (f_dir : string) (dir_listing : string array) =
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
				file_lookup_cache#add representation (Some full_path);
			end
		in
		Array.iter prepare_file dir_listing

	method find_file (f : string) =
		try
			match file_lookup_cache#find f with
			| None ->
				raise Exit
			| Some f ->
				f
		with
		| Exit ->
			raise Not_found
		| Not_found when Path.is_absolute_path f ->
			file_lookup_cache#add f (Some f);
			f
		| Not_found ->
			let rec loop = function
				| [] -> raise Not_found
				| p :: l ->
					begin match p#get_uncached_dir_listing f with
						| None ->
							loop l
						| Some(dir,dir_listing) ->
							let f_dir = Filename.dirname f in
							self#cache_directory dir f_dir dir_listing;
							(* Caching might have located the file we're looking for, so check the lookup cache again. *)
							try
								begin match file_lookup_cache#find f with
								| Some f ->
									f
								| None -> raise Not_found
								end
							with Not_found ->
								loop l
					end
			in
			let r = try Some (loop l) with Not_found -> None in
			file_lookup_cache#add f r;
			match r with
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
