
(**
	Compatible with PHP 7+
*)

open Ast
open Type
open Common
open Meta

let debug = ref false

(**
	Get list of keys in Hashtbl
*)
let hashtbl_keys tbl = Hashtbl.fold (fun key _ lst -> key :: lst) tbl []

(**
	@return List of items in `list1` which `list2` does not contain
*)
let diff_lists list1 list2 = List.filter (fun x -> not (List.mem x list2)) list1

(**
	Type path for native PHP Exception class
*)
let native_exception_path = ([], "Throwable")
(**
	Type path for Haxe exceptions wrapper
*)
let haxe_exception_path = (["php7"], "HException")
(**
	Type path of `php7.Boot`
*)
let boot_type_path = (["php7"], "Boot")
(**
	Type path of the base class for all enums: `php7.Boot.HxEnum`
*)
let hxenum_type_path = (["php7"; "_Boot"], "HxEnum")
(**
	Type path of the implementation class for `Class<Dynamic>`
*)
let hxclass_type_path = (["php7"; "_Boot"], "HxClass")
(**
	Special abstract which enables passing function arguments and return value by reference
*)
let ref_type_path = (["php7"], "Ref")
(**
	Type path of the implementation class for `Array<T>`
*)
let array_type_path = ([], "Array")
(**
	Type path of the `Void`
*)
let void_type_path = ([], "Void")

(**
	Stub to use when you need a `Ast.pos` instance, but don't have one
*)
let dummy_pos = { pfile = ""; pmin = 0; pmax = 0 }

(**
	Check if specified string is a reserved word in PHP
*)
let is_keyword str =
	match String.lowercase str with
		| "__halt_compiler" | "abstract" | "and" | "array" | "as" | "break" | "callable" | "case" | "catch" | "class"
		| "clone" | "const" | "continue" | "declare" | "default" | "die" | "do" | "echo" | "else" | "elseif" | "empty"
		| "enddeclare" | "endfor" | "endforeach" | "endif" | "endswitch" | "endwhile" | "eval" | "exit" | "extends"
		| "final" | "finally" | "for" | "foreach" | "function" | "global" | "goto" | "if" | "implements" | "include"
		| "include_once" | "instanceof" | "insteadof" | "interface" | "isset" | "list" | "namespace" | "new" | "or"
		| "print" | "private" | "protected" | "public" | "require" | "require_once" | "return" | "static" | "switch"
		| "throw" | "trait" | "try" | "unset" | "use" | "var" | "while" | "xor" | "yield" | "__class__" | "__dir__"
		| "__file__" | "__function__" | "__line__" | "__method__" | "__trait__" | "__namespace__"
			-> true
		| _ -> false

(**
	If `name` is not a reserved word in PHP then `name` is returned as-is.
	Otherwise this method returns another string, which can be used instead of `name`
*)
let get_real_name name = if is_keyword name then name ^ "_hx" else name

(**
	If `path` contains some reserved in PHP words, they will be replaced with allowed words.
*)
let get_real_path path = List.map get_real_name path

(**
	Resolve real type (bypass abstracts and typedefs)
*)
let rec follow = Abstract.follow_with_abstracts

let prefix = ref None
(**
	Returns value of `--php-prefix` compiler flag
*)
let get_php_prefix ctx =
	match !prefix with
		| Some prefix -> prefix
		| None ->
			let lst =
				match ctx.php_prefix with
					| None -> []
					| Some str ->
						if String.length str = 0 then
							[]
						else
							Str.split (Str.regexp "\\.") str
			in
			prefix := Some lst;
			lst

(**
	Adds packages specified by `--php-prefix` to `type_path`.
	E.g. if `--php-prefix some.sub` and `type_path` is `(["pack"], "MyClass")`, then this function
	will return `(["some", "sub", "pack"], "MyClass")`
*)
let add_php_prefix ctx type_path =
	match type_path with
		| (pack, name) -> ((get_php_prefix ctx) @ pack, name)

(**
	@return Error message with position information
*)
let error_message pos message = (Lexer.get_error_pos (Printf.sprintf "%s:%d:") pos) ^ ": " ^ message

let fail hxpos mlpos =
	match mlpos with
		| (file, line, _, _) ->
			Printf.printf "%s\n" (error_message hxpos "Unexpected expression. Please submit an issue with expression example and following information:");
			Printf.printf "%s:%d\n" file line;
			assert false

(**
	Check if `target` is a `Dynamic` type
*)
let rec is_dynamic_type (target:Type.t) = match follow target with TDynamic _ -> true | _ -> false

(**
	Check if `target` is `php7.Ref`
*)
let is_ref (target:Type.t) = match target with TAbstract ({ a_path = type_path }, _) -> type_path = ref_type_path | _ -> false

(**
	Check if `field` is a `dynamic function`
*)
let rec is_dynamic_method (field:tclass_field) =
	match field.cf_kind with
		| Method MethDynamic -> true
		| _ -> false

(**
	@return `Type.t` instance for `Void`
*)
let void = ref None
let get_void ctx : Type.t =
	match !void with
		| Some value -> value
		| None ->
			let find com_type =
				match com_type with
					| TAbstractDecl ({ a_path = ([], "Void") } as abstr) -> void := Some (TAbstract (abstr, []));
					| _ -> ()
			in
			List.iter find ctx.types;
			match !void with
				| Some value -> value
				| None -> fail dummy_pos __POS__

(**
	@return `expr` wrapped in parenthesis
*)
let parenthesis expr = {eexpr = TParenthesis expr; etype = expr.etype; epos = expr.epos}

(**
	Check if `current` binary should be surrounded with parenthesis
*)
let need_parenthesis_for_binop current parent =
	if current = parent then
		false
	else
		match (current, parent) with
			| (_, OpAssign) -> false
			| (_, OpAssignOp _) -> false
			| (OpAdd, OpSub) -> false
			| (OpSub, OpAdd) -> false
			| (OpMult, OpDiv) -> false
			| (OpDiv, OpMult) -> false
			| (OpMult, OpAdd) -> false
			| (OpMult, OpSub) -> false
			| (OpDiv, OpAdd) -> false
			| (OpDiv, OpSub) -> false
			| _ -> true

(**
	@return (arguments_list, return_type)
*)
let get_function_signature (field:tclass_field) : (string * bool * Type.t) list * Type.t =
	match follow field.cf_type with
		| TFun (args, return_type) -> (args, return_type)
		| _ -> fail field.cf_pos __POS__

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
	Indicates if `expr` is an access to a `var` field.
*)
let is_var_field_access expr =
	match expr.eexpr with
		| TField (_, FStatic (_, { cf_kind = Var _ })) -> true
		| TField (_, FInstance (_, _, { cf_kind = Var _ })) -> true
		| TField (_, FAnon _) -> true
		| _ -> false

(**
	Indicates whether `expr` is a field access which should be generated as global namespace function
*)
let is_php_global expr =
	match expr.eexpr with
		| TField (_, FStatic ({ cl_extern = true; cl_meta = meta }, _)) -> Meta.has Meta.PhpGlobal meta
		| _ -> false

(**
	Check if specified enum constructor has arguments
*)
let is_enum_constructor_with_args (constructor:tenum_field) =
	match follow constructor.ef_type with
		| TFun _ -> true
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
let get_full_type_name ?escape (type_path:path) =
	let name =
		(match type_path with
			| (module_path, type_name) -> (String.concat "\\" ("" :: get_real_path module_path)) ^ "\\" ^ type_name
		)
	in
	match escape with
		| Some true -> String.escaped name
		| _ -> name

(**
	Check if `target` is or extends native PHP `Extension` class
*)
let rec is_native_exception (target:Type.t) =
	match follow target with
		| TInst ({ cl_path = path }, _) when path = native_exception_path -> true
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
(*let inject_defaults ctx (func:tfunc) = func.tf_expr
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
	}*)

(**
	Check if specified expression is of String type
*)
let is_string expr =
	match follow expr.etype with
		| TInst ({ cl_path = ([], "String") }, _) -> true
		| _ -> false

(**
	Indicates if `expr` is actually a call to Haxe->PHP magic function
	@see http://old.haxe.org/doc/advanced/magic#php-magic
*)
let is_magic expr =
	match expr.eexpr with
	| TCall ({ eexpr = TLocal { v_name = name }}, _) ->
		(match name with
			| "__php__" -> true
			| "__call__" -> true
			| "__physeq__" -> true
			| "__var__" -> true
			| _ -> false
		)
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
							(* Check static vars with non-constant expressions *)
							if not !needs then needs := is_var_with_nonconstant_expr field;
							(* Check static dynamic functions *)
							if not !needs then needs := is_dynamic_method field
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
	Class to simplify collecting lists of declared and used local vars.
	Collected data is needed to generate closures correctly.
*)
class local_vars =
	object (self)
		(** Hashtbl to collect local var used in current scope *)
		val mutable used_locals = [Hashtbl.create 100]
		(** Hashtbl to collect local vars declared in current scope *)
		val mutable declared_locals = [Hashtbl.create 100]
		(**
			Clear collected data
		*)
		method clear : unit =
			used_locals <- [Hashtbl.create 100];
			declared_locals <- [Hashtbl.create 100]
		(**
			This method should be called upone entering deeper scope.
			E.g. right before processing a closure. Just before closure arguments handling.
		*)
		method dive : unit =
			used_locals <- (Hashtbl.create 100) :: used_locals;
			declared_locals <- (Hashtbl.create 100) :: declared_locals
		(**
			This method should be called right after leaving a scope.
			@return List of vars names used in finished scope, but declared in higher scopes
		*)
		method pop : string list =
			match used_locals with
				| [] -> assert false
				| used :: rest_used ->
					match declared_locals with
						| [] -> assert false
						| declared :: rest_declared ->
							let higher_vars = diff_lists (hashtbl_keys used) (hashtbl_keys declared) in
							used_locals <- rest_used;
							declared_locals <- rest_declared;
							List.iter self#used higher_vars;
							higher_vars
		(**
			Specify local var name declared in current scope
		*)
		method declared (name:string) : unit =
			match declared_locals with
				| [] -> assert false
				| current :: _ -> Hashtbl.replace current name name
		(**
			Specify local var name used in current scope
		*)
		method used (name:string) : unit =
			match used_locals with
				| [] -> assert false
				| current :: _ -> Hashtbl.replace current name name

	end

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
		val mutable buffer = Buffer.create 1024
		(** Cache for generated conent *)
		val mutable contents = ""
		(** Intendation used for each line written *)
		val mutable indentation = ""
		(** Expressions nesting. E.g. "if(callFn(ident))" will be represented as [ident, callFn, if] *)
		val mutable expr_hierarchy : texpr list = []
		(** Object to collect local vars declarations and usage as we iterate through methods' expressions *)
		val vars = new local_vars
		(**
			Get PHP namespace path
		*)
		method get_namespace =
			match get_php_prefix ctx with
				| [] -> get_real_path wrapper#get_namespace
				| prefix -> get_real_path (prefix @ wrapper#get_namespace)
		(**
			Get type name
		*)
		method get_name : string = get_real_name wrapper#get_name
		(**
			Get full type path
		*)
		method get_type_path : path =
			match wrapper#get_type_path with
				| (path, name) -> (get_real_path path, get_real_name name)
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
			Writes initialization code for type instances
		*)
		method virtual private write_instance_initialization : unit
		(**
			Indicates if type should be declared as `final`
		*)
		method is_final = false
		(**
			Indicates if `field` should be declared as `final`
		*)
		method is_final_field (field:tclass_field) : bool = false
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
				self#write_empty_lines;
				let boot_class = self#use boot_type_path in
				(* Boot initialization *)
				if boot_type_path <> self#get_type_path then
					self#write_statement (boot_class ^ "::__hx__init()");
				let php_class = get_full_type_name ~escape:true (add_php_prefix ctx self#get_type_path)
				and haxe_class = match wrapper#get_type_path with (path, name) -> String.concat "." (path @ [name]) in
				self#write_statement (boot_class ^ "::registerClass('" ^ php_class ^ "', '" ^ haxe_class ^ "')");
				(* Current class initialization *)
				if wrapper#needs_initialization then
					self#write_statement (self#get_name ^ "::__hx__init()");
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
		method use ?prefix (type_path:path) =
			if type_path = wrapper#get_type_path then
				self#get_name
			else
				let type_path = match type_path with (pack, name) -> (pack, get_real_name name) in
				let type_path =
					match prefix with
						| Some false -> type_path
						| _ -> add_php_prefix ctx type_path
				in
				let module_path = get_module_path type_path in
				match type_path with
					| ([], type_name) -> "\\" ^ type_name
					| _ ->
						let alias_source = ref (List.rev module_path) in
						let get_alias_next_part () =
							match !alias_source with
								| [] ->  failwith ("Failed to find already used type: " ^ get_full_type_name type_path)
								| name :: rest ->
									alias_source := rest;
									String.capitalize name
						and added = ref false
						and alias = ref (get_type_name type_path) in
						if !alias = self#get_name then
							alias := get_alias_next_part () ^ !alias;
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
								| _ -> fail self#pos __POS__
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
								| _ -> self#use ~prefix:(not tcls.cl_extern) tcls.cl_path
							)
					)
				| TFun _ -> self#use ~prefix:false ([], "Closure")
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
						| ([], "Class") -> self#use hxclass_type_path
						| _ when Meta.has Meta.CoreType abstr.a_meta -> "mixed"
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
			Position of currently generated code in source hx files
		*)
		method private pos =
			match expr_hierarchy with
				| { epos = pos } :: _ -> pos
				| _ -> dummy_pos
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
			(match expr.eexpr with
				| TConst const -> self#write_expr_const const
				| TLocal var ->
					vars#used var.v_name;
					self#write ("$" ^ var.v_name)
				| TArray (target, index) -> self#write_expr_array_access target index
				| TBinop (operation, expr1, expr2) -> self#write_expr_binop operation expr1 expr2
				| TField (fexpr, access) when is_php_global expr -> self#write_expr_php_global expr
				| TField (expr, access) -> self#write_expr_field expr access
				| TTypeExpr mtype -> self#write_expr_type mtype
				| TParenthesis expr ->
					self#write "(";
					self#write_expr expr;
					self#write ")"
				| TObjectDecl fields -> self#write_expr_object_declaration fields
				| TArrayDecl exprs -> self#write_expr_array_decl exprs
				| TCall ({ eexpr = TLocal { v_name = name }}, args) when is_magic expr -> self#write_expr_magic name args
				| TCall (target, args) when is_var_field_access target -> self#write_expr_call (parenthesis target) args
				| TCall (target, args) -> self#write_expr_call target args
				| TNew (tcls, _, args) -> self#write_expr_new tcls args
				| TUnop (operation, flag, expr) -> self#write_expr_unop operation flag expr
				| TFunction fn -> self#write_expr_function fn
				| TVar (var, expr) -> self#write_expr_var var expr
				| TBlock exprs -> self#write_expr_block expr
				| TFor (var, iterator, body) -> fail self#pos __POS__
				| TIf (condition, if_expr, else_expr) -> self#write_expr_if condition if_expr else_expr
				| TWhile (condition, expr, do_while) -> self#write_expr_while condition expr do_while
				| TSwitch (switch, cases, default ) -> self#write_expr_switch switch cases default
				| TTry (try_expr, catches) -> self#write_expr_try_catch try_expr catches
				| TReturn expr -> self#write_expr_return expr
				| TBreak -> self#write "break"
				| TContinue -> self#write "continue"
				| TThrow expr -> self#write_expr_throw expr
				| TCast (expr, mtype) -> self#write_expr_cast expr mtype
				| TMeta (_, expr) -> self#write_expr expr
				| TEnumParameter (expr, constructor, index) -> self#write_expr_enum_parameter expr constructor index
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
			vars#clear;
			(match wrapper#get_magic_init with
				| None -> ()
				| Some expr -> self#write_fake_block expr
			);
			self#write "\n";
			vars#clear;
			self#write_hx_init_body;
			self#indent 1;
			self#write_line "}"
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
				| TThis -> self#write "$this"
				| TSuper -> self#write "parent"
		(**
			Writes TArrayDecl to output buffer
		*)
		method private write_expr_array_decl exprs =
			match exprs with
				| [] -> self#write ("new " ^ (self#use array_type_path) ^ "()")
				| [expr] ->
					self#write ((self#use array_type_path) ^ "::wrap([");
					self#write_expr expr;
					self#write "])"
				| _ ->
					self#write ((self#use array_type_path) ^ "::wrap([\n");
					self#indent_more;
					List.iter (fun expr -> self#write_array_item expr) exprs;
					self#indent_less;
					self#write_indentation;
					self#write "])"
		(**
			Writes TArray to output buffer
		*)
		method private write_expr_array_access target index =
			self#write_expr target;
			(match follow target.etype with
				| TInst ({ cl_path = path }, _) when path = array_type_path ->
					(match expr_hierarchy with
						| _ :: { eexpr = TBinop (OpAssign, { eexpr = TArray (t, i) }, _) } :: _ when t == target -> ()
						| _ :: { eexpr = TBinop (OpAssignOp _, { eexpr = TArray (t, i) }, _) } :: _ when t == target -> ()
						| _ -> self#write "->arr" (* inline array index read *)
					)
				| _ -> ()
			);
			self#write "[";
			self#write_expr index;
			self#write "]"
		(**
			Writes TVar to output buffer
		*)
		method private write_expr_var var expr =
			vars#declared var.v_name;
			self#write ("$" ^ var.v_name ^ " = ");
			match expr with
				| None -> self#write "null"
				| Some expr -> self#write_expr expr
		(**
			Writes TFunction to output buffer
		*)
		method private write_expr_function ?name func =
			let write_arg arg =
				match arg with
					| ({ v_name = arg_name; v_type = arg_type }, default_value) ->
						vars#declared arg_name;
						if is_ref arg_type then self#write "&";
						self#write ("$" ^ arg_name);
						match default_value with
							| None -> ()
							| Some const ->
								self#write " = ";
								self#write_expr_const const
			in
			match name with
				| None -> self#write_closure_declaration func write_arg
				| Some "__construct" -> self#write_constructor_function_declaration func write_arg
				| Some name -> self#write_method_function_declaration name func write_arg
		(**
			Writes constructor declaration (except visibility and `static` keywords) to output buffer
		*)
		method private write_constructor_function_declaration func write_arg =
			self#write ("function __construct (");
			write_args buffer write_arg func.tf_args;
			self#write ")";
			self#indent 1;
			self#write "\n";
			self#write_line "{";
			self#indent_more;
			self#write_instance_initialization;
			self#write_fake_block func.tf_expr; (* (inject_defaults ctx func); *)
			self#indent_less;
			self#write_indentation;
			self#write "}"
		(**
			Writes method declaration (except visibility and `static` keywords) to output buffer
		*)
		method private write_method_function_declaration name func write_arg =
			self#write ("function " ^ name ^ " (");
			write_args buffer write_arg func.tf_args;
			self#write ")";
			self#indent 1;
			self#write "\n";
			self#write_indentation;
			self#write_expr func.tf_expr (* (inject_defaults ctx func) *)
		(**
			Writes closure declaration to output buffer
		*)
		method private write_closure_declaration func write_arg =
			vars#dive;
			self#write "function (";
			write_args buffer write_arg func.tf_args;
			self#write ")";
			(* Generate closure body to separate buffer *)
			let original_buffer = buffer in
			buffer <- Buffer.create 256;
			self#write_expr func.tf_expr; (* (inject_defaults ctx func); *)
			let body = Buffer.contents buffer in
			buffer <- original_buffer;
			(* Use captured local vars *)
			let used_vars = vars#pop in
			self#write " ";
			if List.length used_vars > 0 then begin
				self#write " use (";
				write_args buffer (fun name -> self#write ("&$" ^ name)) used_vars;
				self#write ") "
			end;
			self#write body
		(**
			Writes TBlock to output buffer
		*)
		method private write_expr_block block_expr =
			let inline_block = self#parent_expr_is_block in
			self#write_as_block ~inline:inline_block block_expr
		(**
			Emulates TBlock for parent expression and writes `expr` as inlined block
		*)
		method private write_fake_block expr =
			self#write_indentation;
			let fake_block = { expr with eexpr = TBlock [expr] } in
			expr_hierarchy <- fake_block :: expr_hierarchy;
			self#write_as_block ~inline:true expr;
			expr_hierarchy <- List.tl expr_hierarchy
		(**
			Writes "{ <expressions> }" to output buffer
		*)
		method private write_as_block ?inline expr =
			let exprs = match expr.eexpr with TBlock exprs -> exprs | _ -> [expr] in
			let write_body () =
				let write_expr expr =
					self#write_expr expr;
					match expr.eexpr with
						| TBlock _ | TIf _ | TTry _ | TSwitch _ | TWhile (_, _, NormalWhile) -> self#write "\n"
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
		method private write_expr_return expr =
			match expr with
				| None -> self#write "return";
				| Some expr ->
					self#write "return ";
					self#write_expr expr
		(**
			Writes TThrow to output buffer
		*)
		method private write_expr_throw expr =
			self#write "throw ";
			if is_native_exception expr.etype then
				self#write_expr expr
			else if sure_extends_extern expr.etype || is_dynamic_type expr.etype then
				begin
					self#write "(is_object($__hx__throw = ";
					self#write_expr expr;
					self#write (") && $__hx__throw instanceof \\Throwable ? $__hx__throw : new " ^ (self#use haxe_exception_path) ^ "($__hx__throw))")
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
		method private write_expr_try_catch try_expr catches =
			let catching_dynamic = ref false in
			let haxe_exception = self#use haxe_exception_path
			and first_catch = ref true in
			let write_catch (var, expr) =
				let dynamic = ref false in
				(match follow var.v_type with
					| TInst ({ cl_path = ([], "String") }, _) -> self#write "if (is_string($__hx__real_e)) {\n"
					| TAbstract ({ a_path = ([], "Float") }, _) -> self#write "if (is_float($__hx__real_e)) {\n"
					| TAbstract ({ a_path = ([], "Int") }, _) -> self#write "if (is_int($__hx__real_e)) {\n"
					| TAbstract ({ a_path = ([], "Bool") }, _) -> self#write "if (is_bool($__hx__real_e)) {\n"
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
						self#write_as_block ~inline:true expr;
					end
				else
					begin
						self#indent_more;
						self#write_statement ("$" ^ var.v_name ^ " = $__hx__real_e");
						self#write_indentation;
						self#write_as_block ~inline:true expr;
						self#indent_less;
						self#write_indentation;
						self#write "}";
					end;
				if not !dynamic then self#write " else ";
				first_catch := false;
			in
			self#write "try ";
			self#write_as_block try_expr;
			self#write " catch (\\Throwable $__hx__caught_e) {\n";
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
		method private write_expr_cast expr (mtype:module_type option) =
			match mtype with
				| None -> self#write_expr expr
				| Some mtype ->
					self#write_expr_type mtype;
					self#write "->typedCast(";
					self#write_expr expr;
					self#write ")"
		(**
			Write Haxe->PHP magic function call
			@see http://old.haxe.org/doc/advanced/magic#php-magic
		*)
		method private write_expr_magic name args =
			let error = error_message self#pos ("Invalid arguments for " ^ name ^ " magic call") in
			match args with
				| [] -> failwith error
				| { eexpr = TConst (TString code) } as expr :: args ->
					(match name with
						| "__php__" ->
							(match args with
								| [] -> self#write code
								| _ -> failwith error
							)
						| "__call__" ->
							self#write (code ^ "(");
							write_args buffer self#write_expr args;
							self#write ")"
						| "__physeq__" ->
							(match args with
								| [expr2] -> self#write_expr_binop OpEq expr expr2
								| _ -> failwith error
							)
						| "__var__" ->
							(match args with
								| [expr2] ->
									self#write ("$" ^ code ^ "[");
									self#write_expr expr2;
									self#write "]"
								| _ -> failwith error
							)
						| _ -> failwith error
					)
				| [expr1; expr2] ->
					(match name with
						| "__physeq__" ->
							(match args with
								| [expr1; expr2] -> self#write_expr_binop OpEq expr1 expr2
								| _ -> failwith error
							)
						| _ -> failwith error
					)
				| _ -> failwith error
		(**
			Writes TTypeExpr to output buffer
		*)
		method private write_expr_type (mtype:module_type) =
			let type_path = (get_wrapper mtype)#get_type_path in
			match expr_hierarchy with
				| _ :: { eexpr = TField _ } :: _ -> self#write (self#use type_path)
				| _ ->
					let type_name = get_full_type_name ~escape:true type_path in
					self#write ((self#use boot_type_path) ^ "::getClass('" ^ type_name ^ "')")
		(**
			Writes binary operation to output buffer
		*)
		method private write_expr_binop operation expr1 expr2 =
			let write_shiftRightUnsigned () =
				self#write ((self#use boot_type_path) ^ "::shiftRightUnsigned(");
				self#write_expr expr1;
				self#write ", ";
				self#write_expr expr2;
				self#write ")"
			and write_binop str =
				let need_parenthesis =
					match expr_hierarchy with
						| _ :: { eexpr = TBinop (parent, _, _) } :: _ -> need_parenthesis_for_binop operation parent
						| _ -> false
				in
				if need_parenthesis then self#write "(";
				self#write_expr expr1;
				self#write str;
				self#write_expr expr2;
				if need_parenthesis then self#write ")"
			in
			match operation with
				| OpAdd ->
					if (is_string expr1) || (is_string expr2) then
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
				| _ -> fail self#pos __POS__
		(**
			Writes TUnOp to output buffer
		*)
		method private write_expr_unop operation flag expr =
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
		method private write_expr_field expr access =
			let write_access access_str field_str =
				let access_str = ref access_str in
				(match expr.eexpr with
					| TNew _ -> self#write_expr (parenthesis expr)
					| TConst TSuper ->
						self#write "parent";
						access_str := "::"
					| _ -> self#write_expr expr
				);
				self#write (!access_str ^ field_str)
			in
			match (follow expr.etype, access) with
				| (TInst ({ cl_path = ([], "String") }, _), FInstance (_, _, { cf_name = "length" })) ->
					self#write "strlen(";
					self#write_expr expr;
					self#write ")"
				| (_, FInstance (_, _, { cf_name = name })) -> write_access "->" name
				| (_, FStatic (_, { cf_name = name; cf_kind = Var _ })) -> write_access "::" ("$" ^ name)
				| (_, FStatic (_, { cf_name = name; cf_kind = Method MethDynamic })) ->
					(match expr_hierarchy with
						| _ :: { eexpr = TCall ({ eexpr = TField (e, a) }, _) } :: _ when a == access ->
							self#write "(";
							write_access "::" ("$" ^ name);
							self#write ")"
						| _ ->
							write_access "::" ("$" ^ name)
					)
				| (_, FStatic (_, ({ cf_name = name; cf_kind = Method _ } as field))) -> self#write_expr_field_static expr field
				| (_, FAnon { cf_name = name }) -> write_access "->" name
				| (_, FDynamic field_name) -> self#write_expr expr; self#write ("->" ^ field_name)
				| (_, FClosure (tcls, field)) -> self#write_expr_field_closure tcls field expr
				| (_, FEnum (_, field)) ->
					write_access "::" field.ef_name;
					if not (is_enum_constructor_with_args field) then self#write "()"
		(**
			Writes FStatic field access for methods to output buffer
		*)
		method private write_expr_field_static expr field =
			match expr_hierarchy with
				| _ :: { eexpr = TCall ({ eexpr = TField (e, FStatic (_, f)) }, _) } :: _ when e == expr && f == field ->
					self#write_expr expr;
					self#write ("::" ^ field.cf_name)
				| _ ->
					let (args, return_type) = get_function_signature field  in
					self#write "function(";
					write_args buffer (self#write_arg true) args;
					self#write ") { return ";
					self#write_expr expr;
					self#write ("::" ^ field.cf_name ^ "(");
					write_args buffer (self#write_arg false) args;
					self#write "); }"
		(**
			Writes FClosure field access to output buffer
		*)
		method private write_expr_field_closure tcls field expr =
			(* backup originl output buffer *)
			let original_buffer = buffer in
			(* generate feild access expression string *)
			buffer <- Buffer.create 128;
			vars#dive;
			self#write " { return ";
			(match expr.eexpr with
				| TField (_, FClosure _)  -> self#write_expr (parenthesis expr)
				| TField _  -> self#write_expr expr
				| TLocal _ -> self#write_expr expr
				| TConst TThis -> self#write_expr expr
				| _ -> self#write_expr (parenthesis expr)
			);
			self#write ("->" ^ field.cf_name ^ "(");
			let access_str = Buffer.contents buffer
			and used_vars = vars#pop in
			(* Restore original output buffer and local vars *)
			buffer <- original_buffer;
			(* Write whole closure to output buffer *)
			let (args, return_type) = get_function_signature field in
			let args = (** Make sure arguments will not shadow local vars declared in higher scopes *)
				List.map
					(fun (arg_name, optional, arg_type) ->
						if List.mem arg_name used_vars then
							("__hx__" ^ arg_name, optional, arg_type)
						else
							(arg_name, optional, arg_type)
					)
					args
			in
			self#write "function (";
			write_args buffer (self#write_arg true) args;
			self#write ")";
			(match used_vars with
				| [] -> ()
				| _ ->
					self#write " use (";
					write_args buffer (fun name -> self#write ("&$" ^ name)) used_vars;
					self#write ")"
			);
			self#write access_str;
			write_args buffer (self#write_arg false) args;
			self#write "); }"

		(**
			Write anonymous object declaration to output buffer
		*)
		method private write_expr_object_declaration fields =
			match fields with
				| [] ->  self#write "new \\StdClass()"
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
		method private write_expr_call target_expr args =
			(match target_expr.eexpr with
				| TConst TSuper -> self#write "parent::__construct"
				| _ -> self#write_expr target_expr
			);
			self#write "(";
			write_args buffer self#write_expr args;
			self#write ")";
		(**
			Writes a name of a function or a constant from global php namespace
		*)
		method private write_expr_php_global target_expr =
			let name =
				match target_expr.eexpr with
					| TField (_, FStatic (_, field)) -> field.cf_name
					| _ -> fail self#pos __POS__
			in
			self#write name;
		(**
			Writes TNew to output buffer
		*)
		method private write_expr_new inst_class args =
			let needs_php_prefix = not inst_class.cl_extern in
			self#write ("new " ^ (self#use ~prefix:needs_php_prefix inst_class.cl_path) ^ "(");
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
		method private write_expr_if condition if_expr (else_expr:texpr option) =
			let is_ternary =
				if self#parent_expr_is_block then
					false
				else
					match (if_expr.eexpr, else_expr) with
						| (TBlock _, _) | (_, Some { eexpr=TBlock _ }) -> fail self#pos __POS__
						| _ -> true
			in
			if is_ternary then
				match else_expr with
					| None -> fail self#pos __POS__
					| Some expr ->
						self#write_expr_ternary condition if_expr expr self#pos
			else begin
				self#write "if ";
				self#write_expr condition;
				self#write " ";
				self#write_as_block if_expr;
				(match else_expr with
					| None -> ()
					| Some expr ->
						self#write " else ";
						match expr.eexpr with
							| TIf _ -> self#write_expr expr
							| _ -> self#write_as_block expr
				)
			end
		(**
			Writes TWhile ("while..." or "do...while") to output buffer
		*)
		method private write_expr_while condition expr do_while =
			match do_while with
				| NormalWhile ->
					self#write "while ";
					self#write_expr condition;
					self#write " ";
					self#write_as_block expr
				| DoWhile ->
					self#write "do ";
					self#write_as_block expr;
					self#write " while ";
					self#write_expr condition
		(**
			Writes TSwitch to output buffer
		*)
		method private write_expr_switch switch cases default =
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
				self#write_as_block ~inline:true expr;
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
					self#write_as_block ~inline:true expr;
					self#write_statement "break";
					self#indent_less
			);
			self#indent_less;
			self#write_indentation;
			self#write "}"
		(**
			Write TEnumParameter expression to output buffer
		*)
		method private write_expr_enum_parameter expr constructor index =
			self#write_expr expr;
			self#write ("->args[" ^ (string_of_int index) ^ "]")
		(**
			Writes list of arguments for function declarations or calls
		*)
		method write_arg with_optionals (arg_name, optional, (arg_type:Type.t)) =
			self#write ("$" ^ arg_name ^ (if with_optionals && optional then " = null" else ""))
	end

(**
	Builds enum contents
*)
class enum_builder ctx (enm:tenum) =
	object (self)
		inherit type_builder ctx (get_wrapper (TEnumDecl enm))
		(**
			Writes type declaration line to output buffer.
			E.g. "class SomeClass extends Another implements IFace"
		*)
		method private write_declaration =
			self#write_doc (DocClass enm.e_doc);
			self#write_line ("class " ^ self#get_name ^ " extends " ^ (self#use hxenum_type_path))
		(**
			Writes type body to output buffer.
			E.g. for "class SomeClass { <BODY> }" writes <BODY> part.
		*)
		method private write_body =
			let write_empty_lines = ref false in
			PMap.iter
				(fun name field ->
					if !write_empty_lines then
						self#write_empty_lines
					else
						write_empty_lines := true;
					self#write_constructor name field
				)
				enm.e_constrs
		(**
			Writes constructor declaration to output buffer
		*)
		method private write_constructor name (field:tenum_field) =
			let args =
				match follow field.ef_type with
					| TFun (args, _) -> args
					| TEnum _ -> []
					| _ -> fail field.ef_pos __POS__
			in
			self#indent 1;
			self#write_doc (DocMethod (args, TEnum (enm, []), field.ef_doc));
			self#write_indentation;
			self#write ("static public function " ^ name ^ " (");
			write_args buffer (self#write_arg true) args;
			self#write ")\n";
			self#write_line "{";
			self#indent_more;
			self#write_indentation;
			self#write "return ";
			let type_name = get_full_type_name ~escape:true self#get_type_path
			and index_str = string_of_int field.ef_index in
			(match args with
				| [] -> self#write ((self#use hxenum_type_path) ^ "::singleton('" ^ type_name ^ "', '" ^ name ^ "', " ^ index_str ^")")
				| args ->
					self#write ("new " ^ self#get_name ^ "('" ^ name ^ "', " ^ index_str ^", [");
					write_args buffer (fun (name, _, _) -> self#write ("$" ^ name)) args;
					self#write "])"
			);
			self#write ";\n";
			self#indent_less;
			self#write_line "}"
		(**
			Method `__hx__init` is not needed for enums
		**)
		method private write_hx_init_body = ()
		(**
			No need for additional initialization of enum instances
		*)
		method private write_instance_initialization = ()
	end

(**
	Builds class contents
*)
class class_builder ctx (cls:tclass) =
	object (self)
		inherit type_builder ctx (get_wrapper (TClassDecl cls))
		(**
			Indicates if type should be declared as `final`
		*)
		method is_final =
			if not (Meta.has Meta.Final cls.cl_meta) then
				false
			else begin
				let hacked = ref false in
				List.iter
					(fun com_type ->
						if not !hacked then
							match com_type with
								| TClassDecl tcls ->
									if self#extended_by tcls then hacked := Meta.has Meta.Hack tcls.cl_meta
								| _ -> ()
					)
					ctx.types;
				not !hacked
			end
		(**
			Indicates if `field` should be declared as `final`
		*)
		method is_final_field (field:tclass_field) : bool =
			Meta.has Meta.Final field.cf_meta
		(**
			Recursively check if current class is a parent class for a `child`
		*)
		method private extended_by child =
			let result =
				if child == cls then
					false
				else
					let rec check current =
						match current.cl_super with
							| None -> false
							| Some (scls, _) ->
								if scls == cls then true else check scls
					in
					check child
			in
			result
		(**
			Writes type declaration line to output buffer.
			E.g. "class SomeClass extends Another implements IFace"
		*)
		method private write_declaration =
			if self#is_final then self#write "final ";
			self#write_doc (DocClass cls.cl_doc);
			self#write (if cls.cl_interface then "interface " else "class ");
			self#write self#get_name;
			(
				match cls.cl_super with
					| None -> ();
					| Some (super_class, params) ->
						let super_name = self#use_t (TInst (super_class, params)) in
						self#write (" extends " ^ super_name)
			);
			if List.length cls.cl_implements > 0 then begin
				self#write (if cls.cl_interface then " extends " else " implements ");
				let use_interface iface =
					match iface with
						| (i, params) -> self#use_t (TInst (i, params))
				in
				let interfaces = List.map use_interface cls.cl_implements in
				self#write (String.concat ", " interfaces);
			end;
			self#write "\n"
		(**
			Returns either user-defined constructor or creates empty constructor if instance initialization is required.
		*)
		method private get_constructor : tclass_field option =
			match cls.cl_constructor with
				| Some field -> Some field
				| None ->
					if not self#constructor_is_required then
						None
					else
						Some {
							cf_name = "new";
							cf_type = TFun ([], get_void ctx);
							cf_public = true;
							cf_pos = cls.cl_pos;
							cf_doc = None;
							cf_meta = [];
							cf_kind = Method MethNormal;
							cf_params = [];
							cf_expr = Some {
								eexpr = TFunction {
									tf_args = [];
									tf_type = get_void ctx;
									tf_expr = { eexpr = TBlock []; epos = cls.cl_pos; etype = get_void ctx; };
								};
								epos = cls.cl_pos;
								etype = get_void ctx;
							};
							cf_overloads = [];
						}
		(**
			Writes type body to output buffer.
			E.g. for "class SomeClass { <BODY> }" writes <BODY> part.
		*)
		method private write_body =
			let at_least_one_field_written = ref false in
			let write_if_constant _ field =
				match field.cf_kind with
					| Var { v_read = AccInline; v_write = AccNever } ->
						at_least_one_field_written := true;
						self#write_field true field
					| _ -> ()
			and write_if_method is_static _ field =
				match field.cf_kind with
					| Var _ -> ()
					| Method MethDynamic when is_static -> ()
					| Method _ ->
						if !at_least_one_field_written then self#write_empty_lines;
						at_least_one_field_written := true;
						self#write_field is_static field
			and write_if_var is_static _ field =
				match field.cf_kind with
					| Var { v_read = AccInline; v_write = AccNever } -> ()
					| Method MethDynamic ->
						at_least_one_field_written := true;
						let kind = Var { v_read = AccNormal; v_write = AccNormal; } in
						self#write_field is_static { field with cf_kind = kind }
					| Var _ ->
						at_least_one_field_written := true;
						self#write_field is_static field
					| Method _ -> ()
			in
			if boot_type_path = self#get_type_path then begin
				self#write_php_prefix ();
				at_least_one_field_written := true
			end;
		 	if not cls.cl_interface then begin
		 		(* Inlined statc vars (constants) *)
				PMap.iter (write_if_constant) cls.cl_statics;
				if !at_least_one_field_written then self#write_empty_lines;
				at_least_one_field_written := false;
		 		(* Statc vars *)
				PMap.iter (write_if_var true) cls.cl_statics;
				if !at_least_one_field_written then self#write_empty_lines;
				at_least_one_field_written := false;
				(* instance vars *)
				PMap.iter (write_if_var false) cls.cl_fields
			end;
			(* Statc methods *)
			PMap.iter (write_if_method true) cls.cl_statics;
			(* Constructor *)
			(match self#get_constructor with
				| Some field -> write_if_method false "new" field
				| None -> ()
			);
			(* Instance methods *)
			PMap.iter (write_if_method false) cls.cl_fields
		(**
			Check if this class requires constructor to be generated even if there is no user-defined one
		*)
		method private constructor_is_required =
			if List.length self#get_namespace > 0 then
				false
			else begin
				let required = ref false in
				List.iter
					(fun field ->
						if not !required then
							required := (String.lowercase field.cf_name = String.lowercase self#get_name)
					)
					(cls.cl_ordered_statics @ cls.cl_ordered_fields);
				!required
			end
		(**
			Writes `--php-prefix` value as class constant PHP_PREFIX
		*)
		method private write_php_prefix () =
			let prefix = String.concat "\\\\" (get_php_prefix ctx) in
			let indentation = String.length indentation in
			self#indent 1;
			self#write_statement ("const PHP_PREFIX = \"" ^ (String.escaped prefix) ^ "\"");
			self#indent indentation
		(**
			Writes expressions for `__hx__init` method
		*)
		method private write_hx_init_body =
			(* `static dynamic function` initialization *)
			let write_dynamic_method_initialization field =
				let field_access = "self::$" ^ field.cf_name in
				self#write_indentation;
				self#write (field_access ^ " = ");
				(match field.cf_expr with
					| Some expr -> self#write_expr expr
					| None -> fail field.cf_pos __POS__
				);
				self#write ";\n"
			in
			PMap.iter
				(fun _ field ->
					match field.cf_kind with
						| Method MethDynamic -> write_dynamic_method_initialization field
						| _ -> ()
				)
				cls.cl_statics;
			(* `static var` initialization *)
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
			Writes single field to output buffer.
		*)
		method private write_field is_static field =
			match field.cf_kind with
				| Var { v_read = AccInline; v_write = AccNever } -> self#write_const field
				| Var _ -> self#write_var field is_static
				| Method MethMacro -> ()
				| Method MethDynamic when is_static -> ()
				| Method MethDynamic -> self#write_dynamic_method field
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
				| None -> fail self#pos __POS__
				| Some expr ->
					self#write_expr expr;
					self#write ";\n"
		(**
			Writes method to output buffer
		*)
		method private write_method field is_static =
			vars#clear;
			self#indent 1;
			let (args, return_type) = get_function_signature field in
			List.iter (fun (arg_name, _, _) -> vars#declared arg_name) args;
			self#write_doc (DocMethod (args, return_type, field.cf_doc));
			self#write_indentation;
			if self#is_final_field field then self#write "final ";
			if is_static then self#write "static ";
			self#write ((get_visibility field.cf_meta) ^ " ");
			match field.cf_expr with
				| None ->
					self#write (field.cf_name ^ " (");
					write_args buffer (self#write_arg true) args;
					self#write ")";
					self#write " ;\n"
				| Some { eexpr = TFunction fn } ->
					let name = if field.cf_name = "new" then "__construct" else field.cf_name in
					self#write_expr_function ~name:name fn;
					self#write "\n"
				| _ -> fail field.cf_pos __POS__
		(**
			Writes dynamic method to output buffer.
			Only for non-static methods. Static methods are created as static vars in `__hx__init`.
		*)
		method private write_dynamic_method field =
			vars#clear;
			self#indent 1;
			let (args, return_type) = get_function_signature field in
			List.iter (fun (arg_name, _, _) -> vars#declared arg_name) args;
			self#write_doc (DocMethod (args, return_type, field.cf_doc));
			self#write_indentation;
			self#write ((get_visibility field.cf_meta) ^ " function ");
			(match field.cf_expr with
				| Some { eexpr = TFunction fn } ->
					self#write (field.cf_name ^ " (");
					write_args buffer (self#write_arg true) args;
					self#write ")\n";
					self#write_line "{";
					self#indent_more;
					self#write_indentation;
					let field_access = "$this->" ^ field.cf_name
					and default_value = "$this->__hx__default__" ^ field.cf_name in
					self#write ("if (" ^ field_access ^ " !== " ^ default_value ^ ") return (" ^ field_access ^ ")(");
					write_args buffer (self#write_arg false) args;
					self#write ");\n";
					self#write_fake_block fn.tf_expr; (* (inject_defaults ctx fn); *)
					self#indent_less;
					self#write_line "}"
				| _ -> fail field.cf_pos __POS__
			);
			(* Don't forget to create a field for default value *)
			self#write_statement ("protected $__hx__default__" ^ field.cf_name)
		(**
			Writes initialization code for instances of this class
		*)
		method private write_instance_initialization =
			let init_dynamic_method field =
				let (args, _) = get_function_signature field
				and default_field = "$this->__hx__default__" ^ field.cf_name in
				self#write_line ("if (!" ^ default_field ^ ") {");
				self#indent_more;
				self#write_indentation;
				self#write (default_field ^ " = function (");
				write_args buffer (self#write_arg true) args;
				self#write (") { return $this->" ^ field.cf_name ^"(");
				write_args buffer (self#write_arg false) args;
				self#write "); };\n";
				self#write_statement ("$this->" ^ field.cf_name ^ " = " ^ default_field);
				self#indent_less;
				self#write_line "}"
			in
			PMap.iter
				(fun _ field ->
					match field.cf_kind with
						| Method MethDynamic -> init_dynamic_method field
						| _ -> ()
				)
				cls.cl_fields
	end

(**
	Handles generation process
*)
class generator (com:context) =
	object (self)
		val mutable build_dir = ""
		val root_dir = com.file
		val mutable init_types = []
		val mutable boot : (type_builder * string) option  = None
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
			if builder#get_type_path = boot_type_path then
				boot <- Some (builder, filename)
			else if builder#has_magic_init then
				init_types <- (get_full_type_name (namespace, name)) :: init_types
		(**
			Perform actions which should be executed after all classes were processed
		*)
		method finalize : unit =
			self#generate_magic_init;
			self#generate_entry_point
		(**
			Generates calls to static __init__ methods in Boot.php
		*)
		method generate_magic_init : unit =
			match init_types with
				| [] -> ()
				| _ ->
					match boot with
						| None -> fail dummy_pos __POS__
						| Some (_, filename) ->
							let channel = open_out_gen [Open_creat; Open_text; Open_append] 0o644 filename in
							List.iter
								(fun class_name -> output_string channel (class_name ^ "::__hx__init();\n"))
								init_types;
							close_out channel
		(**
			Creates `index.php` which can be used as entry-point for standalone Haxe->PHP app
		*)
		method generate_entry_point =
			match self#get_main_class with
				| None -> ()
				| Some main_class ->
					let channel = open_out (root_dir ^ "/index.php") in
					output_string channel "<?php\n";
					output_string channel ("set_include_path(__DIR__.'/" ^ (String.concat "/" self#get_lib_path) ^ "');\n");
					output_string channel "spl_autoload_register(\n";
					output_string channel "	function($class){\n";
					output_string channel "		$file = stream_resolve_include_path(str_replace('\\\\', '/', $class) .'.php');\n";
					output_string channel "		if ($file) {\n";
					output_string channel "			include_once $file;\n";
					output_string channel "		}\n";
					output_string channel "	}\n";
					output_string channel ");\n";
					(match boot with
						| None -> fail dummy_pos __POS__
						| Some (builder, filename) ->
							let boot_class = get_full_type_name (add_php_prefix com builder#get_type_path) in
							output_string channel (boot_class ^ "::__hx__init();\n")
					);
					output_string channel (main_class ^ "::main();\n");
					close_out channel
		(**
			Create necessary directories  before processing types
		*)
		method private create_output_dirs =
			let build_path = (root_dir :: self#get_lib_path) in
			build_dir <- create_dir_recursive build_path
		(**
			Returns path from `index.php` to directory which will contain all generated classes
		*)
		method private get_lib_path : string list =
			match com.php_lib with
				| None -> ["lib"];
				| Some path -> (Str.split (Str.regexp "/")  path)
		(**
			Returns FQN for main class if defined
		*)
		method private get_main_class : string option =
			match com.main_class with
				| None -> None
				| Some type_path -> Some (get_full_type_name (add_php_prefix com type_path))
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
				| TEnumDecl enm -> gen#generate (new enum_builder com enm);
				| TTypeDecl typedef -> ();
				| TAbstractDecl abstr -> ()
	in
	List.iter generate com.types;
	gen#finalize;
	(* gen#generate_magic_init; *)
	clear_wrappers ();