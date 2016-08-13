
open Ast
open Type
open Common


(**
	@param path Something like [ "/some/path/first_dir_to_create"; "nested_level1"; "nested_level2" ]
	@return String representation of created path (E.g. "/some/path/first_dir_to_create/nested_level1/nested_level2")
*)
let create_dir_recursive path =
	let rec create dir nested_dirs =
		if not (Sys.file_exists dir) then (Unix.mkdir dir 0o755);
		match nested_dirs with
			| [] -> ();
			| next :: rest -> create (dir ^ "/" ^ next) rest
	in
	match path with
		| [] -> "";
		| root :: rest ->
			create root rest;
			(String.concat "/" path)

(**
	@return String representation of specified type path. E.g. returns "\example\Test" for (["example"], "Test")
*)
let get_full_type_name (type_path:path) =
	match type_path with
		| (module_path, type_name) -> (String.concat "\\" ("" :: module_path)) ^ "\\" ^ type_name

(**
	@return Short type name. E.g. returns "Test" for (["example"], "Test")
*)
let get_type_name (type_path:path) = match type_path with (_, type_name) -> type_name

(**
	@return E.g. returns ["example"] for (["example"], "Test")
*)
let get_module_path (type_path:path) = match type_path with (module_path, _) -> module_path

(**
	Base class for type builders
*)
class virtual type_builder =
	object (self)
		(** List of types for "use" section *)
		val use_table = Hashtbl.create 50;
		(** Output buffer *)
		val buffer = Buffer.create 1024;
		(**
			Get namespace path for this type
		*)
		method virtual get_namespace : string list
		(**
			Get type name
		*)
		method virtual get_name : string
		(**
			Returns generated file contents
		*)
		method virtual get_contents : string
		(**
			Adds type to "use" section if not added yet.
			If it's a top-level type then type name returned without adding to "use" section.
			@return Unique alias for specified type.
		*)
		method use (type_path:path) =
			let module_path = get_module_path type_path
			and type_name = get_type_name type_path in
			let add () =
				let alias_source = ref (List.rev module_path) in
				let get_alias_next_part () =
					match !alias_source with
						| [] -> failwith ("Failed to find already used type: " ^ get_full_type_name type_path)
						| name :: rest ->
							alias_source := rest;
							String.capitalize name
				and added = ref false
				and alias = ref type_name in
				while not !added do
					try
						let used_type = Hashtbl.find use_table !alias in
						if used_type = type_path then
							added := true
						else
							alias := get_alias_next_part () ^ !alias;
					with
						| Not_found ->
							Hashtbl.add use_table !alias type_path;
							added := true
						| _ -> failwith "Unknown"
				done;
				!alias
			in
			match type_path with
				| ([], type_name) -> "\\" ^ type_name
				| _ -> add ()
		(**
			Writes specified string to output buffer
		*)
		method private write str =
			Buffer.add_string buffer str
		(**
			Writes specified line to output buffer and appends \n
		*)
		method private write_line line =
			Buffer.add_string buffer (line ^ "\n")
		(**
			Writes specified statement to output buffer and appends ";\n"
		*)
		method private write_stmnt statement =
			Buffer.add_string buffer (statement ^ ";\n")
		(**
			Build file header (<?php, namespace and file doc block)
		*)
		method private write_header =
			self#write_line "<?php";
			let namespace = self#get_namespace in
			if List.length namespace > 0 then
				self#write_line ("namespace " ^ (String.concat "\\" namespace) ^ ";\n");
			self#write_use
		(**
			Build "use" statements
		*)
		method private write_use =
			let write alias type_path =
				if get_type_name type_path = alias then
					self#write_stmnt ("use " ^ (get_full_type_name type_path))
				else
					let full_name = get_full_type_name type_path in
					self#write_stmnt ("use " ^ full_name ^ " as " ^ alias)
			in
			Hashtbl.iter write use_table
	end

(**
	Builds class contents
*)
class class_builder (cls:tclass) =
	object (self)
		inherit type_builder
		val mutable contents = ""
		(**
			Get namespace path for this type
		*)
		method get_namespace =
			match cls.cl_path with (module_path, _) -> module_path
		(**
			Get type name
		*)
		method get_name =
			match cls.cl_path with (_, class_name) -> class_name
		(**
			Returns generated file contents
		*)
		method get_contents =
			if (String.length contents) = 0 then begin
				self#use (["example"], "Test");
				self#use (["example"; "another"], "Test");
				self#write_header;
				self#write_declaration;
				self#write_line "{";
				self#write_line "}";
				contents <- Buffer.contents buffer
			end;
			contents
		(**
			Generates class declaration line.
			E.g. "class SomeClass extends Another implements IFace"
		*)
		method private write_declaration =
			self#write ("class " ^ self#get_name);
			(* match cls.cl_super with
				| None -> ()
				| Some (super_class, _) ->
					let super_builder = new 'self super_class in
					let super_name = self#use
					self#write (" " ^  *)
			self#write "\n"
	end

(**
	Handles generation process
*)
class generator (com:context) =
	object (self)
		val mutable build_dir = ""
		val root_dir = com.file
		(**
			Perform required action before actual php files generation
		*)
		method initialize =
			self#create_output_dirs;
		(**
			Generates php file for specified type
		*)
		method generate (builder:type_builder) =
			let namespace = builder#get_namespace
			and name = builder#get_name in
			let filename = (create_dir_recursive (build_dir :: namespace)) ^ "/" ^ name ^ ".php" in
			let channel = open_out filename in
			output_string channel builder#get_contents;
			close_out channel
		(**
			Create necessary directories  before processing types
		*)
		method private create_output_dirs =
			let lib_path =
				match com.php_lib with
					| None -> ["lib"];
					| Some path -> (Str.split (Str.regexp "/")  path)
			in
			let build_path = (root_dir :: lib_path) in
			build_dir <- create_dir_recursive build_path
	end

(**
	Entry point to Genphp7
*)
let generate (com:context) =
	let gen = new generator com in
	gen#initialize;
	let generate com_type =
		match com_type with
			| TClassDecl cls -> gen#generate (new class_builder cls);
			| TEnumDecl cenum -> ();
			| TTypeDecl ctypedef -> ();
			| TAbstractDecl cabstract -> ()
	in
	List.iter generate com.types