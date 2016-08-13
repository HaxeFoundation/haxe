
open Ast
open Type
open Common
open Meta

let debug = ref false

(**
	Type path for native PHP Exception class
*)
let native_exception_path = ([], "Exception")
(**
	Type path for Haxe exceptions wrapper
*)
let haxe_exception_path = (["php7"], "HException")
(**
	type path of `php7.Boot`
*)
let boot_class_path = (["php7"], "Boot")

(**
	Resolve real type (bypass abstracts and typedefs)
*)
let rec follow = Abstract.follow_with_abstracts

(**
	@return Error message with position information
*)
let error_message pos message = (Lexer.get_error_pos (Printf.sprintf "%s:%d:") pos) ^ message

(**
	Check if `target` is a `Dynamic` type
*)
let rec is_dynamic (target:Type.t) = match follow target with TDynamic _ -> true | _ -> false

(**
	Check if `target` is 100% guaranteed to be a scalar type in PHP.
	Inversion of `is_sure_scalar` does not guarantee `target` is not scalar.
*)
let is_sure_scalar (target:Type.t) =
	match follow target with
		| TInst ({ cl_path = ([], "String") }, _) -> true
		| TAbstract (abstr, _) ->
			(match abstr.a_path with
				| ([],"Int") -> true
				| ([],"Float") -> true
				| ([],"Bool") -> true
				| _ -> false
			)
		| _ -> false

(**
	Check if `target` is 100% guaranteed to be or extend an extern class.
	Inversion of `sure_extends_extern` does not guarantee `target` does not extend an extern class.
*)
let rec sure_extends_extern (target:Type.t) =
	match follow target with
		| TInst ({ cl_path = ([], "String") }, _) -> false
		| TInst ({ cl_extern = true }, _) -> true
		| TInst ({ cl_super = Some (tsuper, params) }, _) -> sure_extends_extern (TInst (tsuper,params))
		| _ -> false

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
let create_dir_recursive (path:string list) =
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
	Check if `target` is or extends native PHP `Extension` class
*)
let rec is_native_exception (target:Type.t) =
	match follow target with
		| TInst ({ cl_path = path }, _) -> path = native_exception_path
		| TInst ({ cl_super = Some (parent, params) }, _) -> is_native_exception (TInst (parent, params))
		| _ -> false

(**
	@return Short type name. E.g. returns "Test" for (["example"], "Test")
*)
let get_type_name (type_path:path) = match type_path with (_, type_name) -> type_name

(**
	@return E.g. returns ["example"] for (["example"], "Test")
*)
let get_module_path (type_path:path) = match type_path with (module_path, _) -> module_path

(**
	@return PHP visibility keyword.
*)
let get_visibility (meta:metadata) = if Meta.has Meta.Protected meta then "protected" else "public"

(**
	Writes arguments list to output buffer
*)
let rec write_args buffer arg_writer (args:'a) =
	match args with
		| [] -> ()
		| [arg] -> arg_writer arg
		| arg :: rest ->
			arg_writer arg;
			Buffer.add_string buffer ", ";
			write_args buffer arg_writer rest

(**
	Check if specified field is a var with non-constant expression
*)
let is_var_with_nonconstant_expr (field:tclass_field) =
	match field.cf_kind with
		| Var _ ->
			(match field.cf_expr with
				| None -> false
				| Some ({eexpr = TConst _ }) -> false
				| Some _ -> true
			)
		| Method _ -> false

(**
	@return New TBlock expression which is composed of setting default values for optional arguments and function body.
*)
let inject_defaults ctx (func:tfunc) =
	let rec inject args body_exprs =
		match args with
			| [] -> body_exprs
			| (_, None) :: rest -> inject rest body_exprs
			| (_, Some TNull) :: rest -> inject rest body_exprs
			| (var, Some const) :: rest ->
				let expr = Codegen.set_default ctx var const func.tf_expr.epos in
			 	expr :: (inject rest body_exprs)
	in
	let exprs =
		match func.tf_expr.eexpr with
			| TBlock exprs -> inject func.tf_args exprs
			| _ -> inject func.tf_args [ func.tf_expr ]
	in
	{
		eexpr = TBlock exprs;
		etype = follow func.tf_expr.etype;
		epos  = func.tf_expr.epos;
	}

(**
	Check if specified expression is of String type
*)
let is_string expr =
	match follow expr.etype with
		| TInst ({ cl_path = ([], "String") }, _) -> true
		| _ -> false

(**
	PHP DocBlock types
*)
type doc_type =
	| DocVar of string * (string option) (* (type name, description) *)
	| DocMethod of (string * bool * t) list * t * (string option) (* (arguments, return type, description) *)
	| DocClass of string option

(**
	Common interface for module_type instances
*)
class virtual type_wrapper (type_path:path) (meta:metadata) (needs_generation:bool) =
	object (self)
		(**
			Indicates if this type should be rendered to corresponding php file
		*)
		method needs_generation = needs_generation
		(**
			Indicates if class initialization method should be executed upon class loaded
		*)
		method virtual needs_initialization : bool
		(**
			Returns expression of a user-defined static __init__ method
			@see http://old.haxe.org/doc/advanced/magic#initialization-magic
		*)
		method get_magic_init : texpr option = None
		(**
			Namespace path. E.g. ["some"; "pack"] for "some.pack.MyType"
		*)
		method get_namespace = get_module_path type_path
		(**
			Short type name. E.g. `SomeType` for `pack.SomeType`
		*)
		method get_name = get_type_name type_path
		(**
			Full type path
		*)
		method get_type_path = type_path
	end

(**
	TClassDecl
*)
class class_wrapper (cls) =
	object (self)
		inherit type_wrapper cls.cl_path cls.cl_meta (not cls.cl_extern)
		(**
			Indicates if class initialization method should be executed upon class loaded
		*)
		method needs_initialization =
			match cls.cl_init with
				| Some _ -> true
				| None ->
					let needs = ref false in
					PMap.iter
						(fun _ field ->
							if not !needs then
								needs := is_var_with_nonconstant_expr field
						)
						cls.cl_statics;
					!needs
		(**
			Returns expression of a user-defined static __init__ method
			@see http://old.haxe.org/doc/advanced/magic#initialization-magic
		*)
		method get_magic_init = cls.cl_init
	end

(**
	TEnumDecl
*)
class enum_wrapper (enm) =
	object (self)
		inherit type_wrapper enm.e_path enm.e_meta (not enm.e_extern)
		(**
			Indicates if class initialization method should be executed upon class loaded
		*)
		method needs_initialization = false
	end

(**
	TTypeDecl
*)
class typedef_wrapper (tdef) =
	object (self)
		inherit type_wrapper tdef.t_path tdef.t_meta false
		(**
			Indicates if class initialization method should be executed upon class loaded
		*)
		method needs_initialization = false
	end

(**
	TAbstractDecl
*)
class abstract_wrapper (abstr) =
	object (self)
		inherit type_wrapper abstr.a_path abstr.a_meta false
		(**
			Indicates if class initialization method should be executed upon class loaded
		*)
		method needs_initialization = false
	end

(**
	type_wrapper for classes
*)
let classes = Hashtbl.create 1000
let get_class_wrapper cls  : type_wrapper =
	try
		let wrapper = Hashtbl.find classes cls in
		wrapper
	with
		| Not_found ->
			let wrapper = new class_wrapper cls in
			Hashtbl.add classes cls wrapper;
			wrapper
		| e -> raise e

(**
	type_wrapper for enums
*)
let enums = Hashtbl.create 200
let get_enum_wrapper enm : type_wrapper=
	try
		let wrapper = Hashtbl.find enums enm in
		wrapper
	with
		| Not_found ->
			let wrapper = new enum_wrapper enm in
			Hashtbl.add enums enm wrapper;
			wrapper
		| e -> raise e

(**
	type_wrapper for typedefs
*)
let typedefs = Hashtbl.create 200
let get_typedef_wrapper typedef : type_wrapper =
	try
		let wrapper = Hashtbl.find typedefs typedef in
		wrapper
	with
		| Not_found ->
			let wrapper = new typedef_wrapper typedef in
			Hashtbl.add typedefs typedef wrapper;
			wrapper
		| e -> raise e

(**
	type_wrapper for abstracts
*)
let abstracts = Hashtbl.create 200
let get_abstract_wrapper abstr : type_wrapper =
	try
		let wrapper = Hashtbl.find abstracts abstr in
		wrapper
	with
		| Not_found ->
			let wrapper = new abstract_wrapper abstr in
			Hashtbl.add abstracts abstr wrapper;
			wrapper
		| e -> raise e

(**
	Returns wrapper for module_type.
	Caches wrappers so that each type will always return the same wrapper instance.
*)
let get_wrapper (mtype:module_type) : type_wrapper =
	match mtype with
		| TClassDecl cls -> get_class_wrapper cls
		| TEnumDecl enm -> get_enum_wrapper enm
		| TTypeDecl typedef -> get_typedef_wrapper typedef
		| TAbstractDecl abstr -> get_abstract_wrapper abstr

(**
	Drop cached instances of type_wrapper
*)
let clear_wrappers () =
	Hashtbl.clear classes;
	Hashtbl.clear enums;
	Hashtbl.clear typedefs;
	Hashtbl.clear abstracts

(**
	Base class for type builders
*)
class virtual type_builder ctx wrapper =
	object (self)
		(** This is required to make wrapper accessible by extending classes *)
		val wrapper = wrapper
		(** This is required to make conext accessible by extending classes *)
		val ctx = ctx
		(** List of types for "use" section *)
		val use_table = Hashtbl.create 50
		(** Output buffer *)
		val buffer = Buffer.create 1024
		(** Cache for generated conent *)
		val mutable contents = ""
		(** Intendation used for each line written *)
		val mutable indentation = ""
		(**
			Expressions nesting. E.g. "if(callFn(ident))" will be represented as [ident, callFn, if]
		*)
		val mutable expr_hierarchy : texpr list = []
		(**
			Get PHP namespace path
		*)
		method get_namespace = wrapper#get_namespace
		(**
			Get type name
		*)
		method get_name : string = wrapper#get_name
		(**
			Get full type path
		*)
		method get_type_path : path = wrapper#get_type_path
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
			Writes expressions for `__hx__init` method
		*)
		method virtual private write_hx_init_body : unit
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
			Indicates if class has user-defined static __init__ method
			@see http://old.haxe.org/doc/advanced/magic#initialization-magic
		*)
		method has_magic_init = match wrapper#get_magic_init with None -> false | Some _ -> true
		(**
			Returns generated file contents
		*)
		method get_contents =
			if (String.length contents) = 0 then begin
				self#write_declaration;
				self#indent 0;
				self#write_line "{"; (** opening bracket for a class *)
				self#write_body;
				if wrapper#needs_initialization then self#write_hx_init;
				self#indent 0;
				self#write_line "}"; (** closing bracket for a class *)
				if wrapper#needs_initialization then begin
					let qualified_name = get_full_type_name wrapper#get_type_path in
					self#write_empty_lines;
					self#write_statement (qualified_name ^ "::__hx__init()")
				end;
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
		method use_t (t_inst:Type.t) =
			match follow t_inst with
				| TEnum (tenum, _) -> self#use tenum.e_path
				| TInst (tcls, _) ->
					(match tcls.cl_kind with
						| KTypeParameter _ -> "mixed"
						| _ ->
							(match tcls.cl_path with
								| ([], "String") -> "string"
								| _ -> self#use tcls.cl_path
							)
					)
				| TFun _ -> self#use ([], "Closure")
				| TAnon _ -> "object"
				| TDynamic _ -> "mixed"
				| TLazy _ -> failwith "TLazy not implemented"
				| TMono mono ->
					(match !mono with
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
						| ([], "Class") -> "string"
						| _ -> self#use_t abstr.a_this
		(**
			Indicates whether current expression nesting level is a top level of a block
		*)
		method private parent_expr_is_block =
			match expr_hierarchy with
				| _ :: { eexpr = TBlock _ } :: _ -> true
				| _ :: { eexpr = TIf _ } :: _ -> true
				| _ :: { eexpr = TTry _ } :: _ -> true
				| _ :: { eexpr = TWhile _ } :: _ -> true
				| _ :: { eexpr = TSwitch _ } :: _ -> true
				| _ -> false
		(**
			Writes specified string to output buffer
		*)
		method private write str =
			Buffer.add_string buffer str
		(**
			Writes fixed amount of empty lines (E.g. between methods)
		*)
		method private write_empty_lines =
			self#write "\n";
			self#write "\n"
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
			let namespace = wrapper#get_namespace in
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
			Writes array item declaration to output buffer and appends ",\n"
		*)
		method private write_array_item ?key value_expr =
			(match key with
				| None ->
					self#write_indentation;
					self#write_expr value_expr;
				| Some key_str ->
					self#write (indentation  ^ "\"" ^ (String.escaped key_str) ^ "\" => ");
					self#write_expr value_expr
			);
			self#write ",\n"
		(**
			Generates PHP docblock to output buffer.
		*)
		method private write_doc doc_block =
			match doc_block with
				| DocVar (type_name, doc) ->
					self#write_line "/**";
					self#write_line (" * @var " ^ type_name);
					(match doc with
						| None -> ()
						| Some txt -> self#write_doc_description txt
					);
					self#write_line " */"
				| DocClass doc ->
					(match doc with
						| None -> ()
						| Some txt ->
							self#write_line "/**";
							self#write_doc_description txt;
							self#write_line " */"
					)
				| DocMethod (args, return, doc) ->
					self#write_method_docblock args return doc
		(**
			Writes description section of docblocks
		*)
		method write_doc_description (doc:string) =
			let lines = Str.split (Str.regexp "\n") (String.trim doc)
			and write_line line =
				let trimmed = String.trim line in
				if String.length trimmed > 0 then (
					if String.get trimmed 0 = '*' then
						self#write_line (" " ^ trimmed)
					else
						self#write_line (" * " ^ trimmed)
				)
			in
			List.iter write_line lines
		(**
			Generates docblock for a method and writes it to output buffer
		*)
		method write_method_docblock args return_type doc =
			self#write_line "/**";
			(match doc with
				| None -> ()
				| Some txt ->
					self#write_doc_description txt;
					self#write_line " * "
			);
			let write_arg arg =
				match arg with
					| (arg_name, is_optional, arg_type) ->
						self#write_line (" * @param " ^ (self#use_t arg_type) ^ " $" ^ arg_name)
			in
			List.iter write_arg args;
			if List.length args > 0 then self#write_line " * ";
			self#write_line (" * @return " ^ (self#use_t return_type));
			self#write_line " */"
		(**
			Writes expression to output buffer
		*)
		method private write_expr (expr:texpr) =
			expr_hierarchy <- expr :: expr_hierarchy;
			let pos = expr.epos in
			(match expr.eexpr with
				| TConst const -> self#write_expr_const const pos
				| TLocal var -> self#write ("$" ^ var.v_name)
				| TArray (target, index) -> self#write_expr_array_access target index pos
				| TBinop (operation, expr1, expr2) -> self#write_expr_binop operation expr1 expr2 pos
				| TField (expr, access) -> self#write_expr_field expr access pos
				| TTypeExpr mtype -> self#write (self#use (get_wrapper mtype)#get_type_path)
				| TParenthesis expr ->
					self#write "(";
					self#write_expr expr;
					self#write ")"
				| TObjectDecl fields -> self#write_expr_object_declaration fields pos
				| TArrayDecl exprs -> self#write_expr_array_decl exprs pos
				| TCall (target, args) -> self#write_expr_call target args pos
				| TNew (tcls, _, args) -> self#write_expr_new tcls args pos
				| TUnop (operation, flag, expr) -> self#write_expr_unop operation flag expr pos
				| TFunction fn -> self#write_expr_function fn pos
				| TVar (var, expr) -> self#write_expr_var var expr pos
				| TBlock exprs -> self#write_expr_block expr pos
				| TFor (var, iterator, body) -> assert false
				| TIf (condition, if_expr, else_expr) -> self#write_expr_if condition if_expr else_expr pos
				| TWhile (condition, expr, do_while) -> self#write_expr_while condition expr do_while pos
				| TSwitch (switch, cases, default ) -> self#write_expr_switch switch cases default pos
				| TTry (try_expr, catches) -> self#write_expr_try_catch try_expr catches pos
				| TReturn expr -> self#write_expr_return expr pos
				| TBreak -> self#write "break"
				| TContinue -> self#write "continue"
				| TThrow expr -> self#write_expr_throw expr pos
				| TCast (expr, mtype) -> self#write_expr_cast expr mtype pos
				| TMeta (_, expr) -> self#write_expr expr
				(* | TEnumParameter of texpr * tenum_field * int *)
				| _ -> ()
			);
			expr_hierarchy <- List.tl expr_hierarchy
		(**
			Writes type initialization method.
		*)
		method private write_hx_init =
			self#write_empty_lines;
			self#indent 1;
			self#write_line "/**";
			self#write_line " * @internal";
			self#write_line " * @access private";
			self#write_line " */";
			self#write_line "static public function __hx__init ()";
			self#write_line "{";
			self#indent_more;
			self#write_statement "static $called = false";
			self#write_statement "if ($called) return";
			self#write_statement "$called = true";
			self#write "\n";
			(match wrapper#get_magic_init with
				| None -> ()
				| Some expr ->
					self#write_indentation;
					self#write_as_block ~inline:true expr expr.epos
			);
			self#write "\n";
			self#write_hx_init_body;
			self#indent 1;
			self#write_line "}"
		(**
			Writes TConst to output buffer
		*)
		method private write_expr_const const pos =
			match const with
				| TInt value -> self#write (Int32.to_string value)
				| TFloat str -> self#write str
				| TString str -> self#write ("\"" ^ (String.escaped str) ^ "\"")
				| TBool value -> self#write (if value then "true" else "false")
				| TNull -> self#write "null"
				| TThis -> self#write "$this"
				| TSuper -> self#write "parent"
		(**
			Writes TArrayDecl to output buffer
		*)
		method private write_expr_array_decl exprs pos =
			match exprs with
				| [] -> self#write "[]"
				| [expr] ->
					self#write "[";
					self#write_expr expr;
					self#write "]"
				| _ ->
					self#write "[\n";
					self#indent_more;
					List.iter (fun expr -> self#write_array_item expr) exprs;
					self#indent_less;
					self#write_indentation;
					self#write "]"
		(**
			Writes TArray to output buffer
		*)
		method private write_expr_array_access target index pos =
			self#write_expr target;
			self#write "[";
			self#write_expr index;
			self#write "]"
		(**
			Writes TVar to output buffer
		*)
		method private write_expr_var var expr pos =
			self#write ("$" ^ var.v_name ^ " = ");
			match expr with
				| None -> self#write "null"
				| Some expr -> self#write_expr expr
		(**
			Writes TFunction to output buffer
		*)
		method private write_expr_function ?name func pos =
			let write_arg arg =
				match arg with
					| ({ v_name = arg_name }, None) -> self#write ("$" ^ arg_name)
					| ({ v_name = arg_name }, Some const) ->
						self#write ("$" ^ arg_name ^ " = ");
						self#write_expr_const const pos
			in
			let str_name = match name with None -> "" | Some str -> str ^ " " in
			self#write ("function " ^ str_name ^ "(");
			write_args buffer write_arg func.tf_args;
			self#write ")";
			(* Closures don't have names. Bracket on same line for closures *)
			if str_name = "" then
				self#write " "
			(* Only methods can be named functions. We want bracket on new line for methods. *)
			else begin
				self#indent 1;
				self#write "\n";
				self#write_indentation
			end;
			self#write_expr (inject_defaults ctx func)
		(**
			Writes TBlock to output buffer
		*)
		method private write_expr_block block_expr pos =
			let inline_block = self#parent_expr_is_block in
			self#write_as_block ~inline:inline_block block_expr pos
		(**
			Writes "{ <expressions> }" to output buffer
		*)
		method private write_as_block ?inline expr pos =
			let exprs = match expr.eexpr with TBlock exprs -> exprs | _ -> [expr] in
			let write_body () =
				let write_expr expr =
					self#write_expr expr;
					match expr.eexpr with
						| TBlock _ | TIf _ | TTry _ | TSwitch _ -> self#write "\n"
						| _ -> self#write ";\n"
				in
				let write_expr_with_indent expr =
					self#write_indentation;
					write_expr expr
				in
				match exprs with
					| [] -> ()
					| first :: rest ->
						write_expr first; (* write first expression without indentation in case of block inlining *)
						List.iter write_expr_with_indent rest
			in
			match inline with
				| Some true -> write_body ()
				| _ ->
					self#write "{\n";
					self#indent_more;
					(match exprs with
						| [] -> ()
						| _ ->
							self#write_indentation; (* indentation for the first expression in block *)
							write_body ()
					);
					self#indent_less;
					self#write_indentation;
					self#write "}"
		(**
			Writes TReturn to output buffer
		*)
		method private write_expr_return expr pos =
			match expr with
				| None -> self#write "return";
				| Some expr ->
					self#write "return ";
					self#write_expr expr
		(**
			Writes TThrow to output buffer
		*)
		method private write_expr_throw expr pos =
			self#write "throw ";
			if is_native_exception expr.etype then
				self#write_expr expr
			else if sure_extends_extern expr.etype or is_dynamic expr.etype then
				begin
					self#write "throw (is_object($__hx__throw = ";
					self#write_expr expr;
					self#write (") && $__hx__throw instanceof \Exception ? $__hx__throw : new " ^ (self#use haxe_exception_path) ^ "($__hx__throw))")
				end
			else
				begin
					self#write ("new " ^ (self#use haxe_exception_path) ^ "(");
					self#write_expr expr;
					self#write ")"
				end
		(**
			Writes try...catch to output buffer
		*)
		method private write_expr_try_catch try_expr catches pos =
			let catching_dynamic = ref false in
			let haxe_exception = self#use haxe_exception_path
			and first_catch = ref true in
			let write_catch (var, expr) =
				let dynamic = ref false in
				(match follow var.v_type with
					| TInst ({ cl_path = ([], "String") }, _) -> self#write "if (is_string($__hx__real_e)) {\n"
					| TAbstract ({ a_path = ([], "Float") }, _) -> self#write "if (is_float($__hx__real_e)) {\n"
					| TAbstract ({ a_path = ([], "Int") }, _) -> self#write "if (is_int($__hx__real_e)) {\n"
					| TAbstract ({ a_path = ([], "Bool") }, _) -> self#write "if (is_float($__hx__real_e)) {\n"
					| TDynamic _ ->
						dynamic := true;
						catching_dynamic := true;
						if not !first_catch then self#write "{\n"
					| vtype -> self#write ("if ($__hx__real_e instanceof " ^ (self#use_t vtype) ^ ") {\n")
				);
				if !dynamic && !first_catch then
					begin
						self#write ("$" ^ var.v_name ^ " = $__hx__real_e;\n");
						self#write_indentation;
						self#write_as_block ~inline:true expr pos;
					end
				else
					begin
						self#indent_more;
						self#write_statement ("$" ^ var.v_name ^ " = $__hx__real_e");
						self#write_indentation;
						self#write_as_block ~inline:true expr pos;
						self#indent_less;
						self#write_indentation;
						self#write "}";
					end;
				if not !dynamic then self#write " else ";
				first_catch := false;
			in
			self#write "try ";
			self#write_as_block try_expr pos;
			self#write " catch (\\Exception $__hx__caught_e) {\n";
			self#indent_more;
			self#write_statement ("$__hx__real_e = ($__hx__caught_e instanceof " ^ haxe_exception ^ " ? $__hx__caught_e->e : $__hx__caught_e)");
			self#write_indentation;
			List.iter write_catch catches;
			if not !catching_dynamic then
				self#write " throw $__hx__caught_e;\n"
			else
				(match catches with [_] -> () | _ -> self#write "\n");
			self#indent_less;
			self#write_indentation;
			self#write "}"
		(**
			Writes TCast to output buffer
		*)
		method private write_expr_cast expr (mtype:module_type option) pos =
			(* of texpr * module_type option *)
			Printf.printf "%s" "dsffs"
		(**
			Writes binary operation to output buffer
		*)
		method private write_expr_binop operation expr1 expr2 pos =
			let write_shiftRightUnsigned () =
				self#write ((self#use boot_class_path) ^ "::shiftRightUnsigned(");
				self#write_expr expr1;
				self#write ", ";
				self#write_expr expr2;
				self#write ")"
			and write_binop str =
				let parent_is_binop = match expr_hierarchy with _ :: { eexpr = TBinop _ } :: _ -> true | _ -> false in
				if parent_is_binop then self#write "(";
				self#write_expr expr1;
				self#write str;
				self#write_expr expr2;
				if parent_is_binop then self#write ")"
			in
			match operation with
				| OpAdd ->
					if (is_string expr1) or (is_string expr2) then
						write_binop " . "
					else
						write_binop " + "
				| OpMult -> write_binop " * "
				| OpDiv -> write_binop " / "
				| OpSub -> write_binop " - "
				| OpAssign -> write_binop " = "
				| OpEq -> write_binop " === "
				| OpNotEq -> write_binop " !== "
				| OpGt -> write_binop " > "
				| OpGte -> write_binop " >= "
				| OpLt -> write_binop " < "
				| OpLte -> write_binop " <= "
				| OpAnd -> write_binop " & "
				| OpOr -> write_binop " | "
				| OpXor -> write_binop " ^ "
				| OpBoolAnd -> write_binop " && "
				| OpBoolOr -> write_binop " || "
				| OpShl  -> write_binop " << "
				| OpShr -> write_binop " >> "
				| OpMod -> write_binop " % "
				| OpUShr -> write_shiftRightUnsigned ()
				| OpAssignOp OpAdd ->
					if (is_string expr1) then
						write_binop " .= "
					else
						write_binop " += "
				| OpAssignOp OpMult -> write_binop " *= "
				| OpAssignOp OpDiv -> write_binop " /= "
				| OpAssignOp OpSub -> write_binop " -= "
				| OpAssignOp OpAnd -> write_binop " &= "
				| OpAssignOp OpOr -> write_binop " |= "
				| OpAssignOp OpXor -> write_binop " ^= "
				| OpAssignOp OpShl  -> write_binop " <<= "
				| OpAssignOp OpShr -> write_binop " >>= "
				| OpAssignOp OpMod -> write_binop " %= "
				| OpAssignOp OpUShr ->
					self#write_expr expr1;
					self#write " = ";
					write_shiftRightUnsigned ()
				| _ -> failwith (error_message pos "Binary operation is not implemented")
		(**
			Writes TUnOp to output buffer
		*)
		method private write_expr_unop operation flag expr pos =
			let write_unop operation =
				match operation with
					| Increment -> self#write "++"
					| Decrement -> self#write "--"
					| Not -> self#write "!"
					| Neg -> self#write "-"
					| NegBits -> self#write "~"
			in
			match flag with
				| Prefix ->
					write_unop operation;
					self#write_expr expr
				| Postfix ->
					self#write_expr expr;
					write_unop operation
		(**
			Writes TField to output buffer
		*)
		method private write_expr_field expr access pos =
			let write_access fieldStr =
				self#write_expr expr;
				self#write fieldStr
			in
			match (follow expr.etype, access) with
				| (TInst ({ cl_path = ([], "String") }, _), FInstance (_, _, { cf_name = "length" })) ->
					self#write "strlen(";
					self#write_expr expr;
					self#write ")"
				| (_, FInstance (_, _, { cf_name = name })) -> write_access ("->" ^ name)
				| (_, FStatic (_, { cf_name = name; cf_kind = Var _ })) -> write_access ("::$" ^ name)
				| (_, FStatic (_, { cf_name = name; cf_kind = Method _ })) -> write_access ("::" ^ name)
				| (_, FAnon _) -> failwith (error_message pos ": FAnon is not implemented")
				(* | FDynamic of string *)
				(* | FClosure of (tclass * tparams) option * tclass_field (* None class = TAnon *) *)
				(* | FEnum of tenum * tenum_field *)
				| _ -> failwith (error_message pos ": Field access is not implemented")
		(**
			Write anonymous object declaration to output buffer
		*)
		method private write_expr_object_declaration fields pos =
			match fields with
				| [] ->  self#write "new \StdClass()"
				| _ ->
					self#write "(object)[\n";
					self#indent_more;
					let write_field (key, value) = self#write_array_item ~key:key value in
					List.iter write_field fields;
					self#indent_less;
					self#write_indentation;
					self#write "]"
		(**
			Writes TCall to output buffer
		*)
		method private write_expr_call target_expr args pos =
			self#write_expr target_expr;
			self#write "(";
			let rec write_next_arg exprs =
				match exprs with
					| [] -> ()
					| [expr] -> self#write_expr expr
					| expr :: rest ->
						self#write_expr expr;
						self#write ", ";
						write_next_arg rest
			in
			write_next_arg args;
			self#write ")";
		(**
			Writes TNew to output buffer
		*)
		method private write_expr_new inst_class args pos =
			self#write ("new " ^ (self#use inst_class.cl_path) ^ "(");
			write_args buffer self#write_expr args;
			self#write ")"
		(**
			Writes ternary operator expressions to output buffer
		*)
		method private write_expr_ternary condition if_expr (else_expr:texpr) pos =
			(match condition.eexpr with
				| TParenthesis expr -> self#write_expr expr;
				| _ -> self#write_expr else_expr
			);
			self#write " ? ";
			self#write_expr if_expr;
			self#write " : ";
			self#write_expr else_expr
		(**
			Writes "if...else..." expression to output buffer
		*)
		method private write_expr_if condition if_expr (else_expr:texpr option) pos =
			let is_ternary =
				if self#parent_expr_is_block then
					false
				else
					match (if_expr.eexpr, else_expr) with
						| (TBlock _, _) | (_, Some { eexpr=TBlock _ }) -> assert false
						| _ -> true
			in
			if is_ternary then
				match else_expr with
					| None -> assert false
					| Some expr ->
						self#write_expr_ternary condition if_expr expr pos
			else begin
				self#write "if ";
				self#write_expr condition;
				self#write " ";
				self#write_as_block if_expr pos;
				(match else_expr with
					| None -> ()
					| Some expr ->
						self#write " else ";
						match expr.eexpr with
							| TIf _ -> self#write_expr expr
							| _ -> self#write_as_block expr pos
				)
			end
		(**
			Writes TWhile ("while..." or "do...while") to output buffer
		*)
		method private write_expr_while condition expr do_while pos =
			match do_while with
				| NormalWhile ->
					self#write "while ";
					self#write_expr condition;
					self#write " ";
					self#write_as_block expr pos
				| DoWhile ->
					self#write "do ";
					self#write_as_block expr pos;
					self#write " while ";
					self#write_expr condition
		(**
			Writes TSwitch to output buffer
		*)
		method private write_expr_switch switch cases default pos =
			let write_case (conditions, expr) =
				List.iter
					(fun condition ->
						self#write_indentation;
						self#write "case ";
						self#write_expr condition;
						self#write ":\n";
					)
					conditions;
				self#indent_more;
				self#write_indentation;
				self#write_as_block ~inline:true expr pos;
				self#write_statement "break";
				self#indent_less
			in
			self#write "switch ";
			self#write_expr switch;
			self#write " {\n";
			self#indent_more;
			List.iter write_case cases;
			(match default with
				| None -> ()
				| Some expr ->
					self#write_line "default:";
					self#indent_more;
					self#write_indentation;
					self#write_as_block ~inline:true expr pos;
					self#write_statement "break";
					self#indent_less
			);
			self#indent_less;
			self#write_indentation;
			self#write "}"
	end

(**
	Builds class contents
*)
class class_builder ctx (cls:tclass) =
	object (self)
		inherit type_builder ctx (get_wrapper (TClassDecl cls))
		(**
			Writes type declaration line to output buffer.
			E.g. "class SomeClass extends Another implements IFace"
		*)
		method private write_declaration =
			self#write_doc (DocClass cls.cl_doc);
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
			let empty_lines_before_var = ref false (* write empty lines to visually separate groups of fields *)
			and empty_lines_before_method = ref false in
			let write_if_const _ field =
				match field.cf_kind with
					| Var { v_read = AccInline; v_write = AccNever } -> self#write_field true field
					| _ -> ()
			and write_if_method is_static _ field =
				match field.cf_kind with
					| Var _ -> ()
					| Method _ ->
						if !empty_lines_before_method then
							self#write_empty_lines
						else
							empty_lines_before_method := true;
						self#write_field is_static field
			and write_if_var is_static _ field =
				match field.cf_kind with
					| Var { v_read = AccInline; v_write = AccNever } -> ()
					| Method _ -> ()
					| Var _ ->
						if !empty_lines_before_var then begin
							self#write_empty_lines;
							empty_lines_before_var := false
						end;
						self#write_field is_static field
			in
		 	if not cls.cl_interface then begin
		 		(* Inlined statc vars (constants) *)
				PMap.iter (write_if_const) cls.cl_statics;
				empty_lines_before_var := true;
		 		(* Statc vars *)
				PMap.iter (write_if_var true) cls.cl_statics;
				empty_lines_before_var := true;
				(* instance vars *)
				PMap.iter (write_if_var false) cls.cl_fields
			end;
			empty_lines_before_method := not !empty_lines_before_var;
			(* Statc methods *)
			PMap.iter (write_if_method true) cls.cl_statics;
			(* Constructor *)
			(match cls.cl_constructor with None -> () | Some field -> self#write_field false field);
			(* Instance methods *)
			PMap.iter (write_if_method false) cls.cl_fields
		(**
			Writes expressions for `__hx__init` method
		*)
		method private write_hx_init_body =
			let write_var_initialization _ field =
				if is_var_with_nonconstant_expr field then begin
					self#write_indentation;
					self#write ("self::$" ^ field.cf_name ^ " = ");
					(match field.cf_expr with
						| None -> ()
						| Some expr -> self#write_expr expr
					);
					self#write ";\n"
				end
			in
			PMap.iter write_var_initialization cls.cl_statics
		(**
			Writes single field to output buffer
		*)
		method private write_field is_static field =
			match (field.cf_kind) with
				| Var { v_read = AccInline; v_write = AccNever } -> self#write_const field
				| Var _ -> self#write_var field is_static
				| Method MethMacro -> ()
				| Method _ -> self#write_method field is_static
		(**
			Writes var-field to output buffer
		*)
		method private write_var field is_static =
			self#indent 1;
			self#write_doc (DocVar (self#use_t field.cf_type, field.cf_doc));
			self#write_indentation;
			if is_static then self#write "static ";
			let visibility = get_visibility field.cf_meta in
			self#write (visibility ^ " $" ^ field.cf_name);
			match field.cf_expr with
				| None -> self#write ";\n"
				| Some expr ->
					match expr.eexpr with
						| TConst _ ->
							self#write " = ";
							self#write_expr expr;
							self#write ";\n"
						| _ -> self#write ";\n"
		(**
			Writes "inline var" to output buffer as constant
		*)
		method private write_const field =
			self#indent 1;
			self#write_doc (DocVar (self#use_t field.cf_type, field.cf_doc));
			self#write_indentation;
			self#write ("const " ^ field.cf_name ^ " = ");
			match field.cf_expr with
				| None -> assert false
				| Some expr ->
					self#write_expr expr;
					self#write ";\n"
		(**
			Writes method to output buffer
		*)
		method private write_method field is_static =
			(* self#write_empty_lines; *)
			self#indent 1;
			let (args, return_type) =
				(match follow field.cf_type with
					| TFun (args, return_type) -> (args, return_type)
					| _ -> failwith ("Invalid signature of method " ^ field.cf_name)
				)
			in
			self#write_doc (DocMethod (args, return_type, field.cf_doc));
			self#write_indentation;
			if is_static then self#write "static ";
			self#write ((get_visibility field.cf_meta) ^ " ");
			match field.cf_expr with
				| None ->
					let write_arg (arg_name, optional, _) =
						self#write ("$" ^ arg_name ^ (if optional then " = null" else ""))
					in
					self#write (field.cf_name ^ " (");
					write_args buffer write_arg args;
					self#write ")";
					self#write " ;\n"
				| Some { eexpr = TFunction fn; epos = pos } ->
					let name = if field.cf_name = "new" then "__construct" else field.cf_name in
					self#write_expr_function ~name:name fn pos;
					self#write "\n"
				| _ -> failwith ("invalid expression for method " ^ field.cf_name)
	end

(**
	Handles generation process
*)
class generator (com:context) =
	object (self)
		val mutable build_dir = ""
		val root_dir = com.file
		val mutable init_types = []
		(**
			Perform required actions before actual php files generation
		*)
		method initialize =
			self#create_output_dirs;
		(**
			Generates php file for specified type
		*)
		method generate (builder:type_builder) =
			let contents = builder#get_contents
			and namespace = builder#get_namespace
			and name = builder#get_name in
			let filename = (create_dir_recursive (build_dir :: namespace)) ^ "/" ^ name ^ ".php" in
			let channel = open_out filename in
			output_string channel contents;
			close_out channel;
			if builder#has_magic_init then
				init_types <- (get_full_type_name builder#get_type_path) :: []
		(**
			Generates calls to static __init__ methods in Boot.php
		*)
		method generate_magic_init : unit =
			failwith "Not implemented"
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
		let wrapper = get_wrapper com_type in
		if wrapper#needs_generation then
			match com_type with
				| TClassDecl cls -> gen#generate (new class_builder com cls);
				| TEnumDecl tenum -> ();
				| TTypeDecl typedef -> ();
				| TAbstractDecl abstr -> ()
	in
	List.iter generate com.types;
	(* gen#generate_magic_init; *)
	clear_wrappers ();