
open Ast
open Type
open Common

(* Check lists:
tclass = {
	[x] mutable cl_path : path;
	[ ] mutable cl_module : module_def;
	[ ] mutable cl_pos : Ast.pos;
	[ ] mutable cl_private : bool;
	[ ] mutable cl_doc : Ast.documentation;
	[ ] mutable cl_meta : metadata;
	[ ] mutable cl_params : type_params;
	[ ] mutable cl_kind : tclass_kind;
	[ ] mutable cl_extern : bool;
	[x] mutable cl_interface : bool;
	[x] mutable cl_super : (tclass * tparams) option;
	[x] mutable cl_implements : (tclass * tparams) list;
	[ ] mutable cl_fields : (string , tclass_field) PMap.t;
	[ ] mutable cl_statics : (string, tclass_field) PMap.t;
	[ ] mutable cl_ordered_statics : tclass_field list;
	[ ] mutable cl_ordered_fields : tclass_field list;
	[ ] mutable cl_dynamic : t option;
	[ ] mutable cl_array_access : t option;
	[ ] mutable cl_constructor : tclass_field option;
	[ ] mutable cl_init : texpr option;
	[ ] mutable cl_overrides : tclass_field list;
	[ ] mutable cl_build : unit -> build_state;
	[ ] mutable cl_restore : unit -> unit;
}

*)

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
		val use_table = Hashtbl.create 50
		(** Output buffer *)
		val buffer = Buffer.create 1024
		(** Cache for generated conent *)
		val mutable contents = ""
		(** intendation used for each line written *)
		val mutable indentation = ""
		(**
			Get namespace path for this type
		*)
		method virtual get_namespace : string list
		(**
			Get type name
		*)
		method virtual get_name : string
		(**
			Writes type declaration line to output buffer.
			E.g. "class SomeClass extends Another implements IFace"
		*)
		method virtual private write_declaration : unit
		(**
			Writes type body to output buffer.
			E.g. for "class SomeClass { <BODY> }" writes <BODY> part.
		*)
		method virtual private write_body : unit
		(**
			Increase indentation by one level
		*)
		method indent_more =
			indentation <- indentation ^ "\t";
		(**
			Decrease indentation by one level
		*)
		method indent_less =
			indentation <- String.make ((String.length indentation) - 1) '\t';
		(**
			Set indentation level (starting from zero for no indentation)
		*)
		method indent level =
			indentation <- String.make level '\t';
		(**
			Returns generated file contents
		*)
		method get_contents =
			if (String.length contents) = 0 then begin
				self#write_declaration;
				self#indent 0;
				self#write_line "{";
				self#write_body;
				self#indent 0;
				self#write_line "}";
				let body = Buffer.contents buffer in
				Buffer.clear buffer;
				self#write_header;
				self#write "\n";
				let header = Buffer.contents buffer in
				contents <- header ^ body;
			end;
			contents
		(**
			Adds type to "use" section if not added yet.
			If it's a top-level type then type name returned without adding to "use" section.
			@return Unique alias for specified type.
		*)
		method use (type_path:path) =
			let module_path = get_module_path type_path in
			match type_path with
				| ([], type_name) -> "\\" ^ type_name
				| _ ->
					let alias_source = ref (List.rev module_path) in
					let get_alias_next_part () =
						match !alias_source with
							| [] -> failwith ("Failed to find already used type: " ^ get_full_type_name type_path)
							| name :: rest ->
								alias_source := rest;
								String.capitalize name
					and added = ref false
					and alias = ref (get_type_name type_path) in
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
		(**
			Extracts type path from Type.t value and execute self#use on it
			@return Unique alias for specified type.
		*)
		method use_t t_inst =
			match t_inst with
				| TEnum (tenum, _) -> self#use tenum.e_path
				| TInst (tcls, _) ->
					(
						match tcls.cl_path with
							| ([], "String") -> "string"
							| _ -> self#use tcls.cl_path
					)
				| TFun _ -> self#use ([], "Closure")
				| TAnon _ -> failwith "TAnon not implemented"
				| TDynamic _ -> failwith "TDynamic not implemented"
				| TLazy _ -> failwith "TLazy not implemented"
				| TAbstract (tabs, _) ->
					match tabs.a_path with
						| ([],"Int") -> "int"
						| ([],"Float") -> "float"
						| ([],"Bool") -> "bool"
						| ([],"Void") -> "void"
						| _ -> failwith "TAbstract not implemented"
		(**
			Writes specified string to output buffer
		*)
		method private write str =
			Buffer.add_string buffer str
		(**
			Writes specified line to output buffer and appends \n
		*)
		method private write_line line =
			Buffer.add_string buffer (indentation ^ line ^ "\n")
		(**
			Writes specified statement to output buffer and appends ";\n"
		*)
		method private write_stmnt statement =
			Buffer.add_string buffer (indentation ^ statement ^ ";\n")
		(**
			Build file header (<?php, namespace and file doc block)
		*)
		method private write_header =
			self#indent 0;
			self#write_line "<?php";
			let namespace = self#get_namespace in
			if List.length namespace > 0 then
				self#write_line ("namespace " ^ (String.concat "\\" namespace) ^ ";\n");
			self#write_use
		(**
			Build "use" statements
		*)
		method private write_use =
			self#indent 0;
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
		(**
			Get namespace path for this type
		*)
		method get_namespace = get_module_path cls.cl_path
		(**
			Get type name
		*)
		method get_name = get_type_name cls.cl_path
		(**
			Writes type declaration line to output buffer.
			E.g. "class SomeClass extends Another implements IFace"
		*)
		method private write_declaration =
			self#write "\n";
			self#write (if cls.cl_interface then "interface " else "class ");
			self#write self#get_name;
			(
				match cls.cl_super with
					| None -> ();
					| Some (super_class, _) ->
						let super_name = self#use super_class.cl_path in
						self#write (" extends " ^ super_name)
			);
			if List.length cls.cl_implements > 0 then begin
				self#write (if cls.cl_interface then " extends " else " implements ");
				let use_interface iface =
					match iface with
						| (i, _) -> self#use i.cl_path
				in
				let interfaces = List.map use_interface cls.cl_implements in
				self#write (String.concat ", " interfaces);
			end;
			self#write "\n"
		(**
			Writes type body to output buffer.
			E.g. for "class SomeClass { <BODY> }" writes <BODY> part.
		*)
		method private write_body =
			PMap.iter self#write_field cls.cl_fields
		(**
			Writes single field to output buffer
		*)
		method private write_field field_name field =
			match (field.cf_kind) with
				| Var _ -> self#write_var field
				| Method _ -> () (* Method of method -> self#write method *)
		(**
			Writes var-field to output buffer
		*)
		method private write_var field =
			self#indent 1;
			self#write_line ("/** @var " ^ (self#use_t field.cf_type) ^ " */");
			self#write_stmnt ("public " ^ field.cf_name)
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