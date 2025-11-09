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

type file_resolution_specificity =
	| SpecificityNormal            (* Standard Module.hx file *)
	| SpecificityPlatformSpecific  (* Module.[platform].hx file matching current platform *)
	| SpecificityCustomExtension   (* Module.[custom].hx file matching --custom-extension config *)
	| SpecificityMacroSpecific     (* Module.macro.hx file while in macro context *)
	| SpecificityCoreApi           (* Module.hx takes priority when loading @:coreApi types *)

(* We need to clean-up absolute ("") vs. cwd ("."). *)
let absolute_class_path = new directory_class_path "" User

class class_paths = object(self)
	val mutable l = []
	val file_lookup_cache = new Lookup.hashtbl_lookup;
	val mutable custom_ext = None
	val mutable platform_ext = ""
	val mutable is_loading_core_api = false

	method lock_context (custom_extension : string option) (platform_name : string) (core_api : bool) : unit =
		custom_ext <- Option.map (fun ext -> "." ^ ext) custom_extension;
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
			(e.g. ends with `.js.hx` while compiling for JS) or current
			custom extension (e.g. ends with `.custom.hx` while compiling
			with `--custom-extension custom`)

			The lookup cache will store the full file path which is the more
			specific in current context (see `file_resolution_specificity` type)
		*)
		let found = ref None in
		let f_dir = Filename.dirname f_search in
		let prepare_file file_own_name =
			let relative_to_classpath = if f_dir = "." then file_own_name else f_dir ^ "/" ^ file_own_name in
			(* `representation` is how the file is referenced to. E.g. when it's deduced from a module path. *)
			let specificity,representation =
				if is_loading_core_api then
					SpecificityCoreApi,relative_to_classpath
				else begin
					let ext = extension relative_to_classpath in
					let second_ext = extension (remove_extension relative_to_classpath) in
					(* The file contains double extension and the secondary one matches current custom extension *)
					if (Option.map_default (fun custom_ext -> custom_ext = second_ext) false custom_ext) then
						SpecificityCustomExtension,(remove_extension (remove_extension relative_to_classpath)) ^ ext
					(* The file contains ".macro.hx" double extension and we are in macro context *)
					else if platform_ext = second_ext && second_ext = ".macro" then
						SpecificityMacroSpecific,(remove_extension (remove_extension relative_to_classpath)) ^ ext
					(* The file contains double extension and the secondary one matches current platform *)
					else if platform_ext = second_ext then
						SpecificityPlatformSpecific,(remove_extension (remove_extension relative_to_classpath)) ^ ext
					else
						SpecificityNormal,relative_to_classpath
				end
			in

			let full_path = if dir = "." then file_own_name else dir ^ "/" ^ file_own_name in
			let full_path = Some(create_resolved_file full_path cp, specificity) in

			match file_lookup_cache#find_opt representation with
			| Some (Some (_, old_specificity)) when (old_specificity >= specificity)-> ()
			| _ -> file_lookup_cache#add representation full_path;

			if representation = f_search then
				match !found with
				| Some (_, old_specificity) when (old_specificity >= specificity) -> ()
				| _ -> found := full_path;
		in
		Array.iter prepare_file dir_listing;
		!found

	method find_file_noraise (f : string) =
		try
			file_lookup_cache#find f
		with
		| Not_found when Path.is_absolute_path f ->
			let r = if Sys.file_exists f then
				Some (create_resolved_file f absolute_class_path, SpecificityNormal)
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
								| Some (f, specificity) ->
									Some (f, specificity)
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
		| Some (f, _) -> f

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
