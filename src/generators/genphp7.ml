
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
(*
let php_type_path_tbl = Hashtbl.create 1000

let get_php_type_path (cls:tclass) =
	try
		let php_type_path = Hashtbl.find php_type_path_tbl cls.cl_path in
		php_type_path
	with
		| Not_found ->
			let php_type_path = ref cls.cl_path in
			Hashtbl.add php_type_path_tbl cls.cl_path !php_type_path;
			php_type_path;
		| e -> raise e
 *)

(**
	@return `opt` value or `default` if `opt` is None
*)
let get_option_value (opt:'a option) default =
	match opt with
		| None -> default
		| Some value -> value

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
	PHP DocBlock types
*)
type doc_type =
	| DocVar of string * (string option) (* (type name, description) *)
	| DocMethod of tfunc * (string option) (* (function, description) *)

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
				| TDynamic _ -> "mixed"
				| TLazy _ -> failwith "TLazy not implemented"
				| TMono mono ->
					(
						match !mono with
							| None -> "mixed"
							| Some t -> self#use_t t
					)
				| TType _ -> failwith "TType not implemented"
				| TAbstract (abstr, _) ->
					match abstr.a_path with
						| ([],"Int") -> "int"
						| ([],"Float") -> "float"
						| ([],"Bool") -> "bool"
						| ([],"Void") -> "void"
						| _ -> self#use_t abstr.a_this
		(**
			Writes specified string to output buffer
		*)
		method private write str =
			Buffer.add_string buffer str
		(**
			Writes current indentation to output buffer
		*)
		method private write_indentation =
			Buffer.add_string buffer indentation
		(**
			Writes specified line to output buffer and appends \n
		*)
		method private write_line line =
			Buffer.add_string buffer (indentation ^ line ^ "\n")
		(**
			Writes specified statement to output buffer and appends ";\n"
		*)
		method private write_statement statement =
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
					self#write_statement ("use " ^ (get_full_type_name type_path))
				else
					let full_name = get_full_type_name type_path in
					self#write_statement ("use " ^ full_name ^ " as " ^ alias)
			in
			Hashtbl.iter write use_table
		(**
			Generates PHP docblock to output buffer.
		*)
		method private write_doc doc_block =
			let write_description doc =
				let lines = Str.split (Str.regexp "\n") (String.trim doc)
				and write_line line =
					let trimmed = String.trim line in
					if String.get trimmed 0 = '*' then
						self#write_line (" " ^ trimmed)
					else
						self#write_line (" * " ^ trimmed)
				in
				List.iter write_line lines
			in
			match doc_block with
				| DocVar (type_name, None) -> self#write_line ("/** @var " ^ type_name ^ " */")
				| DocVar (type_name, Some doc) ->
					self#write_line "/**";
					self#write_line (" * @var " ^ type_name);
					write_description doc;
					self#write_line " */"
				| DocMethod (func, doc) ->
					self#write_line "/**";
					(match doc with None -> () | Some txt -> write_description txt);
					self#write_line "*/";
		(**
			Writes expression to output buffer
		*)
		method private write_expr (expr:texpr) =
			(match expr.eexpr with
				| TConst const -> self#write_expr_const const
				(* | TLocal of tvar *)
				| TArray (target, index) -> self#write_expr_array_access target index
				(* | TBinop of Ast.binop * texpr * texpr *)
				(* | TField of texpr * tfield_access *)
				(* | TTypeExpr of module_type *)
				(* | TParenthesis of texpr *)

				(* | TObjectDecl of (string * texpr) list *)
				| TArrayDecl exprs -> self#write_expr_array_decl exprs
				(* | TCall of texpr * texpr list *)
				(* | TNew of tclass * tparams * texpr list *)
				(* | TUnop of Ast.unop * Ast.unop_flag * texpr *)
				(* | TFunction of tfunc *)
				(* | TVar of tvar * texpr option *)
				(* | TBlock of texpr list *)
				(* | TFor of tvar * texpr * texpr *)
				(* | TIf of texpr * texpr * texpr option *)
				(* | TWhile of texpr * texpr * Ast.while_flag *)
				(* | TSwitch of texpr * (texpr list * texpr) list * texpr option *)
				(* | TTry of texpr * (tvar * texpr) list *)
				(* | TReturn of texpr option *)
				(* | TBreak *)
				(* | TContinue *)
				(* | TThrow of texpr *)
				(* | TCast of texpr * module_type option *)
				(* | TMeta of metadata_entry * texpr *)
				(* | TEnumParameter of texpr * tenum_field * int *)
				| _ -> ()
			);
		(**
			Writes TConst to output buffer
		*)
		method private write_expr_const const =
			match const with
				| TInt value -> self#write (Int32.to_string value)
				| TFloat str -> self#write str
				| TString str -> self#write ("\"" ^ (String.escaped str) ^ "\"")
				| TBool value -> self#write (if value then "true" else "false")
				| TNull -> self#write "null"
				| TThis -> self#write "$this->"
				| TSuper -> self#write "parent::"
		(**
			Writes TArrayDecl to output buffer
		*)
		method private write_expr_array_decl exprs =
			self#write "[\n";
			self#indent_more;
			List.iter
				(fun expr ->
					self#write_indentation;
					self#write_expr expr;
					self#write ",\n"
				)
				exprs;
			self#write "\n";
			self#indent_less;
			self#write_indentation;
			self#write "]"
		(**
			Writes TArray to output buffer
		*)
		method private write_expr_array_access target index =
			self#write_expr target;
			self#write "[";
			self#write_expr index;
			self#write "]"
	end

(**
	Builds class contents
*)
and class_builder (cls:tclass) =
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
			let write_method is_static field_name field =
				match field.cf_kind with
					| Var _ -> ()
					| Method _ -> self#write_field field_name field is_static
			and write_var is_static field_name field =
				match field.cf_kind with
					| Var _ -> self#write_field field_name field is_static
					| Method _ -> ()
			in
			PMap.iter (write_var true) cls.cl_statics;
			self#write "\n";
			PMap.iter (write_var false) cls.cl_fields;
			PMap.iter (write_method true) cls.cl_statics;
			PMap.iter (write_method false) cls.cl_fields
		(**
			Writes single field to output buffer
		*)
		method private write_field field_name field is_static =
			match (field.cf_kind) with
				| Var _ -> self#write_var field is_static
				| Method MethNormal -> self#write_method field is_static
				| Method MethInline -> self#write_method field is_static
				| Method MethDynamic -> self#write_method field is_static
				| Method MethMacro -> ()
		(**
			Writes var-field to output buffer
		*)
		method private write_var field is_static =
			self#indent 1;
			self#write_doc (DocVar (self#use_t field.cf_type, field.cf_doc));
			self#write_indentation;
			if is_static then self#write "static ";
			self#write ("public $" ^ field.cf_name);
			match field.cf_expr with
				| None -> self#write ";\n"
				| Some expr ->
					self#write " = ";
					self#write_expr expr;
					self#write ";\n"
		(**
			Writes method to output buffer
		*)
		method private write_method field is_static =
			self#write "\n";
			self#indent 1;
			let func =
				match field.cf_expr with
					| Some ({ eexpr = TFunction fn }) -> fn
					(* | None -> *)
					| _ -> failwith ("Invalid expr for method " ^ field.cf_name)
			in
			self#write_doc (DocMethod (func, field.cf_doc));
			if is_static then self#write "static ";
			self#write ("public $" ^ field.cf_name);
			self#write " (";
			self#write ")\n";
			self#indent 1;
			self#write_line "{";
			(** method body *)
			self#indent 1;
			self#write_line "}"

	end

(**
	Handles generation process
*)
and generator (com:context) =
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
			| TEnumDecl tenum -> ();
			| TTypeDecl typedef -> ();
			| TAbstractDecl abstr -> ()
	in
	List.iter generate com.types