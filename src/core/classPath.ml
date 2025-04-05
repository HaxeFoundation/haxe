type class_path_scope =
	| Std
	| StdTarget
	| Lib
	| User

type file_kind =
	| FFile

class virtual class_path (path : string) (scope : class_path_scope) (file_kind : file_kind) = object(self)
	method path = path;
	method scope = scope;
	method file_kind = file_kind;

	method virtual clone : class_path
	method virtual clear_cache : unit
	method virtual get_directory_path : string option
	method virtual get_uncached_dir_listing : string -> (string * string array) option
	method virtual dump : unit

	method is_std_path = match scope with
		| Std -> true
		| _ -> false

	method scope_string = match scope with
		| Std -> "Std"
		| StdTarget -> "StdTarget"
		| Lib -> "Lib"
		| User -> "User"
end

class directory_class_path (path : string) (scope : class_path_scope) = object(self)
	inherit class_path path scope FFile

	val readdir_cache = new Lookup.hashtbl_lookup

	method clear_cache =
		readdir_cache#clear

	method get_directory_path =
		Some path

	method clone =
		new directory_class_path path scope

	method get_uncached_dir_listing (f : string) =
		let file = path ^ f in
		let dir = Filename.dirname file in
		if readdir_cache#mem dir then
			None
		else begin
			let dir_listing =
				try Some (dir,Sys.readdir dir);
				with Sys_error _ -> None
			in
			readdir_cache#add dir dir_listing;
			dir_listing
		end

	method dump =
		print_endline (Printf.sprintf "    dir %-9s: %s" (self#scope_string) path)
end