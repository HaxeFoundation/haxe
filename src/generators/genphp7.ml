
open Ast
open Type
open Common

(*
	@param path Something like [ "/some/path/first_dir_to_create"; "nested_level1"; "nested_level2" ]
*)
let create_dir_recursive path =
	let rec create dir nested_dirs =
		if not (Sys.file_exists dir) then (Unix.mkdir dir 0o755);
		match nested_dirs with
			| [] -> ();
			| next :: rest -> create (dir ^ "/" ^ next) rest
	in
	match path with
		| [] -> ();
		| root :: rest -> create root rest

(**
	@param php7_path Output path specified by `-php7` compiler flag
	@param php_lib_path Path specified by `--php-lib` compiler flag
	@return Full path of a directory to generate php classes into.
*)
let create_output_dir php7_path php_lib_path =
	let lib_path =
		match php_lib_path with
			| None -> ["lib"];
			| Some path -> (Str.split (Str.regexp "/")  path)
	in
	let build_path = (php7_path :: lib_path) in
	create_dir_recursive build_path;
	String.concat "/" build_path

let generate_class build_dir cls =
	let namespace = (match cls.cl_path with (module_path, _) -> module_path) in
	create_dir_recursive (build_dir :: namespace)

let generate_type build_dir type_declaration =
	match type_declaration with
		| TClassDecl cls -> generate_class build_dir cls;
		| TEnumDecl cenum -> ();
		| TTypeDecl ctypedef -> ();
		| TAbstractDecl cabstract -> ()

let generate com =
 	let build_dir = create_output_dir com.file com.php_lib in
 	List.iter (generate_type build_dir) com.types