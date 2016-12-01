(**
	Compatible with PHP 7+
*)

open Ast
open Type
open Common
open Meta
open Globals

let debug = ref false

(**
	Escape string for constant strings generation.
	Copy-pasted from genphp.
*)
let escape_bin s =
	let b = Buffer.create 0 in
	for i = 0 to String.length s - 1 do
		match Char.code (String.unsafe_get s i) with
		| c when c = Char.code('\\') || c = Char.code('"') || c = Char.code('$') ->
			Buffer.add_string b "\\";
			Buffer.add_char b (Char.chr c)
		| c when c < 32 ->
			Buffer.add_string b (Printf.sprintf "\\x%.2X" c)
		| c ->
			Buffer.add_char b (Char.chr c)
	done;
	Buffer.contents b

(**
	Write resources passed to compiler via `-resource` flag
	Copy-pasted from genphp
*)
let write_resource dir name data =
	let rdir = dir ^ "/res" in
	if not (Sys.file_exists dir) then Unix.mkdir dir 0o755;
	if not (Sys.file_exists rdir) then Unix.mkdir rdir 0o755;
	let name = Codegen.escape_res_name name false in
	let ch = open_out_bin (rdir ^ "/" ^ name) in
	output_string ch data;
	close_out ch

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
let hxexception_type_path = (["php"; "_Boot"], "HxException")
(**
	Type path of `php.Boot`
*)
let boot_type_path = (["php"], "Boot")
(**
	Type path of the base class for all enums: `php.Boot.HxEnum`
*)
let hxenum_type_path = (["php"; "_Boot"], "HxEnum")
(**
	Type path of the implementation class for `Class<Dynamic>`
*)
let hxclass_type_path = (["php"; "_Boot"], "HxClass")
(**
	Type path of the implementation class for `String`
*)
let hxstring_type_path = (["php"; "_Boot"], "HxString")
(**
	Type path of the special implementation class for `String`
	which is used when Dynamic value is suspected to be a string
*)
let hxdynamicstr_type_path = (["php"; "_Boot"], "HxDynamicStr")
(**
	Type path of the implementation class for anonymous objects
*)
let hxanon_type_path = (["php"; "_Boot"], "HxAnon")
(**
	Type path of the implementation class for closures
*)
let hxclosure_type_path = (["php"; "_Boot"], "HxClosure")
(**
	Type path for special PHP extern class to support specific language expressions
*)
let syntax_type_path = (["php"], "Syntax")
(**
	Special abstract which enables passing function arguments and return value by reference
*)
let ref_type_path = (["php"], "Ref")
(**
	Type path of the implementation class for `Array<T>`
*)
let array_type_path = ([], "Array")
(**
	Type path of the implementation class for `Array<T>`
*)
let native_array_type_path = (["php"], "NativeArray")
(**
	Type path of the `Void`
*)
let void_type_path = ([], "Void")
(**
	Type path of the `Bool`
*)
let bool_type_path = ([], "Bool")

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
		| "__file__" | "__function__" | "__line__" | "__method__" | "__trait__" | "__namespace__" | "int" | "float"
		| "bool" | "string" | "true" | "false" | "null" | "parent"
			-> true
		| _ -> false

(**
	Check if specified type is Void
*)
let is_void_type t = match follow t with TAbstract ({ a_path = void_type_path }, _) -> true | _ -> false

(**
	Check if specified type is Bool
*)
let is_bool_type t = match follow t with TAbstract ({ a_path = bool_type_path }, _) -> true | _ -> false

(**
	Check if specified type is php.NativeArray
*)
let is_native_array_type t = match follow t with TAbstract ({ a_path = native_array_type_path }, _) -> true | _ -> false

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
	If `expr` is a TCast returns underlying expression (recursively bypassing nested casts).
	Otherwise returns `expr` as is.
*)
let rec reveal_expr expr =
	match expr.eexpr with
		| TCast (e, _) -> reveal_expr e
		| TMeta (_, e) -> reveal_expr e
		| _ -> expr

(**
	@return Error message with position information
*)
let error_message pos message = (Lexer.get_error_pos (Printf.sprintf "%s:%d:") pos) ^ ": " ^ message

(**
	Terminates compiler process and prints user-friendly instructions about filing an issue in compiler repo.
*)
let fail hxpos mlpos =
	match mlpos with
		| (file, line, _, _) ->
			Printf.printf "%s\n" (error_message hxpos "Unexpected expression. Please submit an issue with expression example and following information:");
			Printf.printf "%s:%d\n" file line;
			assert false

(**
	Print compilation error message and abort compilation process.
*)
let error_and_exit pos message =
	Printf.printf "%s" (error_message pos message);
	exit 1

(**
	Check if `target` is a `Dynamic` type
*)
let rec is_dynamic_type (target:Type.t) = match follow target with TDynamic _ -> true | _ -> false

(**
	Check if `target` is `php.Ref`
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
	Check if specified expression is of `Dynamic` type
*)
let is_dynamic expr = is_dynamic_type expr.etype

(**
	Check if specified expression is of `Int` type
*)
let is_int expr = match follow expr.etype with TAbstract ({ a_path = ([], "Int") }, _) -> true | _ -> false

(**
	Check if specified expression is of `Float` type
*)
let is_float expr = match follow expr.etype with TAbstract ({ a_path = ([], "Float") }, _) -> true | _ -> false

(**
	Check if specified type is String
*)
let is_string_type t = match follow t with TInst ({ cl_path = ([], "String") }, _) -> true | _ -> false

(**
	Check if specified expression is of String type
*)
let is_string expr = is_string_type expr.etype

(**
	Check if `expr` is an access to a method of special `php.PHP` class
*)
let is_lang_extern expr =
	match expr.eexpr with
		| TField ({ eexpr = TTypeExpr (TClassDecl { cl_path = path }) }, _) when path = syntax_type_path -> true
		| _ -> false

(**
	Check if specified type is actually a generic parameter
*)
let is_generic_parameter (target:Type.t) =
	match follow target with
		| TInst ({ cl_kind = KTypeParameter _ }, _) -> true
		| _ -> false

(**
	Check if `target` type cannot be clarified on compilation
*)
let is_unknown_type (target:Type.t) = is_dynamic_type target || is_generic_parameter target

(**
	Check if `expr1` and `expr2` can be reliably checked for equality only with `Boot.equal()`
*)
let need_boot_equal expr1 expr2 =
	(is_int expr1 && (is_float expr2 || is_unknown_type expr2.etype))
	|| (is_float expr1 && (is_int expr2 || is_unknown_type expr2.etype))
	|| (is_unknown_type expr1.etype && (is_int expr2 || is_float expr2))
	|| (is_unknown_type expr1.etype && is_unknown_type expr2.etype)

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
	@return `tclass` instance for `php.Boot`
*)
let boot = ref None
let get_boot ctx : tclass =
	match !boot with
		| Some value -> value
		| None ->
			let find com_type =
				match com_type with
					| TClassDecl ({ cl_path = path } as cls) when path = boot_type_path -> boot := Some cls;
					| _ -> ()
			in
			List.iter find ctx.types;
			match !boot with
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
	Check if specified expression may require dereferencing if used as "temporary expression"
*)
let needs_dereferencing expr =
	let rec is_create target_expr =
		match target_expr.eexpr with
			| TParenthesis e -> is_create e
			| TCast (e, _) -> is_create e
			| TNew _ -> true
			| TArrayDecl _ -> true
			| TObjectDecl _ -> true
			| _ -> false
	in
	match expr.eexpr with
		| TField (target_expr, _) -> is_create target_expr
		| TArray (target_expr, _) -> is_create target_expr
		| _ -> false

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
	Indicates if `expr` is guaranteed to be an access to a `var` field.
*)
let is_sure_var_field_access expr =
	match (reveal_expr expr).eexpr with
		| TField (_, FStatic (_, { cf_kind = Var _ })) -> true
		| TField (_, FInstance (_, _, { cf_kind = Var _ })) -> true
		(* | TField (_, FAnon { cf_kind = Var _ }) -> true *) (* Sometimes we get anon access to non-anonymous objects *)
		| _ -> false

(**
	Check if specified unary operation modifies value in place
*)
let is_modifying_unop op =
	match op with
		| Increment
		| Decrement -> true
		| _ -> false

(**
	Indicates whether `expr` is a field access which should be generated as global namespace function
*)
let is_php_global expr =
	match expr.eexpr with
		| TField (_, FStatic ({ cl_extern = true; cl_meta = meta }, _)) -> Meta.has Meta.PhpGlobal meta
		| _ -> false

(**
	Indicates whether `expr` is a field access which should be generated as class constant access
*)
let is_php_class_const expr =
	match expr.eexpr with
		| TField (_, FStatic ({ cl_extern = true }, { cf_meta = meta; cf_kind = Var _ })) ->
			Meta.has Meta.PhpClassConst meta
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
let get_full_type_name ?escape ?omit_first_slash (type_path:path) =
	let name =
		match type_path with
			| (module_path, type_name) ->
				let parts =
					match omit_first_slash with
						| Some true -> get_real_path module_path
						| _ -> "" :: get_real_path module_path
				in
				(String.concat "\\" parts) ^ "\\" ^ type_name
	in
	match escape with
		| Some true -> String.escaped name
		| _ -> name

(**
	Check if `target` is or implements native PHP `Throwable` interface
*)
let rec is_native_exception (target:Type.t) =
	match follow target with
		| TInst ({ cl_path = path }, _) when path = native_exception_path -> true
		| TInst ({ cl_super = parent ; cl_implements = interfaces ; cl_path = path }, _) ->
			let (parent, params) =
				match parent with
					| Some (parent, params) -> (Some parent, params)
					| None -> (None, [])
			in
			let found = ref false in
			List.iter
				(fun (cls, params) ->
					if not !found then
						found := is_native_exception (TInst (cls, params))
				)
				interfaces;
			if !found then
				true
			else
				(match parent with
					| Some parent -> is_native_exception (TInst (parent, params))
					| None -> false
				)
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
let inject_defaults (ctx:Common.context) (func:tfunc) =
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
	Check if `expr` is a constant string
*)
let is_constant_string expr =
	match expr.eexpr with
		| TConst (TString _) -> true
		| _ -> false

(**
	Check if `expr` is a constant null
*)
let is_constant_null expr =
	match expr.eexpr with
		| TConst TNull -> true
		| _ -> false

(**
	Check if `expr` is a constant
*)
let is_constant expr =
	match expr.eexpr with
		| TConst _ -> true
		| _ -> false

(**
	Check if `expr` is a concatenation
*)
let is_concatenation expr =
	match expr.eexpr with
		| TBinop (OpAdd, expr1, expr2) -> (is_string expr1) || (is_string expr2)
		| _ -> false

(**
	Check if provided expression is a binary operation
*)
let is_binop expr = match expr.eexpr with TBinop _ -> true | _ -> false

(**
	Check if provided expression is an assignment binary operation
*)
let is_binop_assign expr =
	match expr.eexpr with
		| TBinop (operation, _, _) ->
			(match operation with
				| OpAssign | OpAssignOp _ -> true
				| _ -> false
			)
		| _ -> false

(**
	Check if specified expression is field access or array access
*)
let is_access expr =
	match expr.eexpr with
		| TField _ | TArray _ -> true
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
	Adds `return` expression to block if it does not have one already
*)
let ensure_return_in_block block_expr =
	match block_expr.eexpr with
		| TBlock [] -> fail block_expr.epos __POS__
		| TBlock exprs ->
			let reversed = List.rev exprs in
			let last_expr = List.hd reversed in
			let return_expr = { last_expr with eexpr = TReturn (Some last_expr) } in
			let reversed = return_expr::(List.tl reversed) in
			{ block_expr with eexpr = TBlock (List.rev reversed) }
		| _ -> fail block_expr.epos __POS__

(**
	Check if specified type has rtti meta
*)
let has_rtti_meta ctx mtype =
	match Codegen.build_metadata ctx mtype with
		| None -> false
		| Some _ -> true

(**
	Check if this var accesses and meta combination should generate a variable
*)
let is_real_var field =
	if Meta.has IsVar field.cf_meta then
		true
	else
		match field.cf_kind with
			| Var { v_read = read; v_write = write } -> read = AccNormal || write = AccNormal
			| _ -> false

(**
	Check if user-defined field has the same name as one of php magic methods, but with not compatible signature.
*)
let field_needs_rename field =
	match field.cf_kind with
		| Var _ -> false
		| Method _ ->
			match field.cf_name with
				| "__construct" | "__destruct" | "__call" | "__callStatic" | "__get" | "__set" | "__isset"
				| "__unset" | "__sleep" | "__wakeup" | "__toString" | "__invoke" | "__set_state" | "__clone"
				| "__debugInfo" -> not (Meta.has Meta.PhpMagic field.cf_meta)
				| _ -> false
(**
	Get valid `field` name.
*)
let field_name field =
	if field_needs_rename field then
		"__hx__renamed" ^ field.cf_name
	else
		field.cf_name

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
			Returns hx source file name where this type was declared
		*)
		method virtual get_source_file : string
		(**
			Returns `Type.module_type` instance for this type
		*)
		method virtual get_module_type : module_type
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
			(* Interfaces may need initialization only for RTTI meta data.
				But that meta is written in `class_wrapper#write_rtti_meta` *)
			if cls.cl_interface then
				false
			else
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
		(**
			Returns hx source file name where this type was declared
		*)
		method get_source_file = cls.cl_pos.pfile
		(**
			Returns `Type.module_type` instance for this type
		*)
		method get_module_type = TClassDecl cls
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
		(**
			Returns hx source file name where this type was declared
		*)
		method get_source_file = enm.e_pos.pfile
		(**
			Returns `Type.module_type` instance for this type
		*)
		method get_module_type = TEnumDecl enm
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
		(**
			Returns hx source file name where this type was declared
		*)
		method get_source_file = tdef.t_pos.pfile
		(**
			Returns `Type.module_type` instance for this type
		*)
		method get_module_type = TTypeDecl tdef
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
		(**
			Returns hx source file name where this type was declared
		*)
		method get_source_file = abstr.a_pos.pfile
		(**
			Returns `Type.module_type` instance for this type
		*)
		method get_module_type = TAbstractDecl abstr
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
	Check if specified type name is used in specified namespace
*)
let namespaces_types_cache = Hashtbl.create 512
let type_name_used_in_namespace ctx name namespace =
	let types = Hashtbl.find_all namespaces_types_cache namespace in
	match types with
		| [] ->
			List.iter
				(fun ctx_type ->
					let wrapper = get_wrapper ctx_type in
					Hashtbl.add namespaces_types_cache wrapper#get_namespace wrapper#get_name
				)
				ctx.types;
			let types = Hashtbl.find_all namespaces_types_cache namespace in
			List.mem name types
		| _ ->
			List.mem name types

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
			@return List of vars names used in finished scope, but declared in higher scopes.
					And list of vars names declared in finished scope.
		*)
		method pop : string list * string list =
			match used_locals with
				| [] -> assert false
				| used :: rest_used ->
					match declared_locals with
						| [] -> assert false
						| declared :: rest_declared ->
							let higher_vars = diff_lists (hashtbl_keys used) (hashtbl_keys declared)
							and declared_vars = hashtbl_keys declared in
							used_locals <- rest_used;
							declared_locals <- rest_declared;
							List.iter self#used higher_vars;
							(higher_vars, declared_vars)
		(**
			This method should be called right after leaving a scope.
			@return List of vars names used in finished scope, but declared in higher scopes
		*)
		method pop_used : string list = match self#pop with (higher_vars, _) -> higher_vars
		(**
			This method should be called right after leaving a scope.
			@return List of vars names declared in finished scope
		*)
		method pop_declared : string list = match self#pop with (_, declared_vars) -> declared_vars
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
			Returns hx source file name where this type was declared
		*)
		method get_source_file : string = wrapper#get_source_file
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
			Writes additional initialization code, which should be called before `__hx__init()`
		*)
		method virtual private write_pre_hx_init : unit
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
				if boot_type_path = self#get_type_path then
					self#write_statement (boot_class ^ "::__hx__init()");
				(*let php_class = get_full_type_name ~escape:true ~omit_first_slash:true (add_php_prefix ctx self#get_type_path)*)
				let haxe_class = match wrapper#get_type_path with (path, name) -> String.concat "." (path @ [name]) in
				self#write_statement (boot_class ^ "::registerClass(" ^ (self#get_name) ^ "::class, '" ^ haxe_class ^ "')");
				self#write_rtti_meta;
				self#write_pre_hx_init;
				(* Current class initialization *)
				if wrapper#needs_initialization && boot_type_path <> self#get_type_path then
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
								if (get_module_path type_path) <> wrapper#get_namespace && type_name_used_in_namespace ctx !alias wrapper#get_namespace then
									alias := get_alias_next_part () ^ !alias
								else
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
						| ([],"Enum") -> "Enum"
						| ([], "Class") -> "Class"
						| _ when Meta.has Meta.CoreType abstr.a_meta -> "mixed"
						| _ -> self#use_t abstr.a_this
		(**
			Indicates whether current expression nesting level is a top level of a block
		*)
		method private parent_expr_is_block =
			let rec expr_is_block expr parents =
				match expr.eexpr with
					| TBlock _ -> true
					| TIf (_, if_expr, Some else_expr) ->
						if (expr_is_block if_expr []) || (expr_is_block else_expr []) then
							true
						else
							(match parents with
								| parent :: rest -> expr_is_block parent rest
								| [] -> false
							)
					| TIf (_, _, None) -> true
					| TTry _ -> true
					| TWhile _ -> true
					| TSwitch _ -> true
					| _ -> false
			in
			match expr_hierarchy with
				| _ :: parent :: rest -> expr_is_block parent rest
				| _ -> false
		(**
			Returns parent expression.
		*)
		method private parent_expr =
			match expr_hierarchy with
				| _ :: expr :: _ -> Some expr
				| _ -> None
		(**
			Indicates if parent expression is a call (bypasses casts and metas)
		*)
		method private parent_expr_is_call =
			let rec expr_is_call expr parents =
				match expr.eexpr with
					| TCast _
					| TMeta _ ->
						(match parents with
							| parent :: rest -> expr_is_call parent rest
							| [] -> false
						)
					| TCall _ -> true
					| _ -> false
			in
			match expr_hierarchy with
				| _ :: parent :: rest -> expr_is_call parent rest
				| _ -> false
		(**
			Position of currently generated code in source hx files
		*)
		method private pos =
			match expr_hierarchy with
				| { epos = pos } :: _ -> pos
				| _ -> dummy_pos
		(**
			Add a function call to "dereference" part of expression to avoid "Cannot use temporary expression in write context"
			erro in expressions like:
			```
			new MyClass().fieldName = 'value';
			```
		*)
		method private dereference expr =
			let boot_cls = get_boot ctx in
			let deref_field = PMap.find "deref" boot_cls.cl_statics in
			match expr.eexpr with
				| TField (target_expr, access) ->
					{
						expr with eexpr = TField (
							{
								target_expr with eexpr = TCall (
									{
										target_expr with eexpr = TField (
											{
												target_expr with eexpr = TTypeExpr (TClassDecl boot_cls)
											},
											FStatic (boot_cls, deref_field)
										)
									},
									[ target_expr ]
								)
							},
							access
						)
					}
				| TArray (target_expr, access_expr) ->
					{
						expr with eexpr = TArray (
							{
								target_expr with eexpr = TCall (
									{
										target_expr with eexpr = TField (
											{
												target_expr with eexpr = TTypeExpr (TClassDecl boot_cls)
											},
											FStatic (boot_cls, deref_field)
										)
									},
									[ target_expr ]
								)
							},
							access_expr
						)
					}
				| _ -> fail self#pos __POS__
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
				if (get_module_path type_path) <> wrapper#get_namespace then
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
			Writes rtti meta to output buffer
		*)
		method write_rtti_meta =
			match Codegen.build_metadata ctx wrapper#get_module_type with
				| None -> ()
				| Some meta_expr ->
					let boot_class = self#use boot_type_path in
					self#write (boot_class ^ "::registerMeta(" ^ (self#get_name) ^ "::class, ");
					self#write_expr meta_expr;
					self#write ");\n"
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
				| TBinop (operation, expr1, expr2) when needs_dereferencing expr1 ->
					self#write_expr { expr with eexpr = TBinop (operation, self#dereference expr1, expr2) }
				| TBinop (operation, expr1, expr2) -> self#write_expr_binop operation expr1 expr2
				| TField (fexpr, access) when is_php_global expr -> self#write_expr_php_global expr
				| TField (fexpr, access) when is_php_class_const expr -> self#write_expr_php_class_const expr
				| TField (expr, access) -> self#write_expr_field expr access
				| TTypeExpr mtype -> self#write_expr_type mtype
				| TParenthesis expr ->
					self#write "(";
					self#write_expr expr;
					self#write ")"
				| TObjectDecl fields -> self#write_expr_object_declaration fields
				| TArrayDecl exprs -> self#write_expr_array_decl exprs
				| TCall ({ eexpr = TLocal { v_name = name }}, args) when is_magic expr -> self#write_expr_magic name args
				| TCall ({ eexpr = TField (expr, access) }, args) when is_string expr -> self#write_expr_call_string expr access args
				| TCall (expr, args) when is_lang_extern expr -> self#write_expr_call_lang_extern expr args
				| TCall (target, args) when is_sure_var_field_access target -> self#write_expr_call (parenthesis target) args
				| TCall (target, args) -> self#write_expr_call target args
				| TNew (_, _, args) when is_string expr -> write_args buffer self#write_expr args
				| TNew (tcls, _, args) -> self#write_expr_new tcls args
				| TUnop (operation, flag, target_expr) when needs_dereferencing target_expr ->
					self#write_expr { expr with eexpr = TUnop (operation, flag, self#dereference target_expr) }
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
				| TBreak -> self#write_expr_loop_flow "break"
				| TContinue -> self#write_expr_loop_flow "continue"
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
			Writes `continue N` or `break N` with required N depending on nearest parent loop and amount of `switch` between loop and
			`continue/break`
		*)
		method private write_expr_loop_flow word =
			let rec count_N parent_exprs count =
				match parent_exprs with
					| [] -> count
					| { eexpr = TWhile _ } :: _ -> count
					| { eexpr = TSwitch _ } :: rest -> count_N rest (count + 1)
					| _ :: rest -> count_N rest count
			in
			let count = count_N expr_hierarchy 1 in
			if count > 1 then
				self#write (word ^ " " ^ (string_of_int count))
			else
				self#write word
		(**
			Writes TConst to output buffer
		*)
		method private write_expr_const const =
			match const with
				| TInt value -> self#write (Int32.to_string value)
				| TFloat str -> self#write str
				| TString str -> self#write ("\"" ^ (escape_bin str) ^ "\"")
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
			(*self#write_expr target;
			self#write "[";
			self#write_expr index;
			self#write "]"*)
			let write_index left_bracket right_bracket =
				self#write left_bracket;
				self#write_expr index;
				self#write right_bracket
			in
			match follow target.etype with
				| TInst ({ cl_path = path }, _) when path = array_type_path ->
					(match self#parent_expr with
						| Some { eexpr = TBinop (OpAssign, { eexpr = TArray (t, i) }, _) } when t == target ->
							self#write_expr target;
							write_index "[" "]"
						| Some { eexpr = TBinop (OpAssignOp _, { eexpr = TArray (t, i) }, _) } when t == target ->
							self#write_expr target;
							write_index "[" "]"
						| Some { eexpr = TUnop (op, _, { eexpr = TArray (t, i) }) } when t == target && is_modifying_unop op ->
							self#write_expr target;
							write_index "[" "]"
						| Some { eexpr = TField ({ eexpr = TArray (t, i) }, _) } ->
							self#write_expr target;
							write_index "[" "]"
						| _ ->
							self#write "(";
							self#write_expr target;
							self#write "->arr";
							write_index "[" "] ?? null)"
					)
				| _ ->
					self#write_expr target;
					write_index "[" "]"
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
			match name with
				| None -> self#write_closure_declaration func self#write_function_arg
				| Some "__construct" -> self#write_constructor_function_declaration func self#write_function_arg
				| Some name -> self#write_method_function_declaration name func self#write_function_arg
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
			self#write_fake_block (inject_defaults ctx func);
			self#indent_less;
			self#write_indentation;
			self#write "}"
		(**
			Writes method declaration (except visibility and `static` keywords) to output buffer
		*)
		method private write_method_function_declaration name func write_arg =
			let by_ref = if is_ref func.tf_type then "&" else "" in
			self#write ("function " ^ by_ref ^ name ^ " (");
			write_args buffer write_arg func.tf_args;
			self#write ")";
			self#indent 1;
			self#write "\n";
			self#write_indentation;
			self#write_expr (inject_defaults ctx func)
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
			self#write_expr (inject_defaults ctx func);
			let body = Buffer.contents buffer in
			buffer <- original_buffer;
			(* Use captured local vars *)
			let used_vars = vars#pop_used in
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
			(* Check if parent expr could not contain blocks in PHP, adn this block needs to be wrapped in a closure. *)
			let needs_closure = match self#parent_expr with
				| None -> false
				| Some e ->
					match e.eexpr with
						| TIf (_, _, _) -> false
						| TWhile (_, _, _) -> false
						| TTry (_, _) -> false
						| TFor (_, _, _) -> false
						| TFunction _ -> false
						| TBlock _ -> false
						| TSwitch (_, _, _) -> false
						| _ -> true
			in
			if needs_closure then
				begin
					self#write "(";
					self#write_expr {
						block_expr with eexpr = TFunction {
							tf_args = [];
							tf_type = block_expr.etype;
							tf_expr = ensure_return_in_block block_expr;
						}
					};
					self#write ")()"
				end
			else
				begin
					let inline_block = self#parent_expr_is_block in
					self#write_as_block ~inline:inline_block block_expr
				end
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
		method private write_as_block ?inline ?unset_locals expr =
			let unset_locals = match unset_locals with Some true -> true | _ -> false
			and exprs = match expr.eexpr with TBlock exprs -> exprs | _ -> [expr] in
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
				let write_exprs () =
					match exprs with
						| [] -> ()
						| first :: rest ->
							write_expr first; (* write first expression without indentation in case of block inlining *)
							List.iter write_expr_with_indent rest
				in
				if unset_locals then
					begin
						let original_buffer = buffer in
						buffer <- Buffer.create 256;
						vars#dive;
						write_exprs();
						let body = Buffer.contents buffer in
						buffer <- original_buffer;
						let locals = vars#pop_declared in
						if List.length locals > 0 then begin
							self#write ("unset($" ^ (String.concat ", $" locals) ^ ");\n");
							self#write_indentation
						end;
						self#write body
					end
				else
					write_exprs()
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
					self#write (") && $__hx__throw instanceof \\Throwable ? $__hx__throw : new " ^ (self#use hxexception_type_path) ^ "($__hx__throw))")
				end
			else
				begin
					self#write ("new " ^ (self#use hxexception_type_path) ^ "(");
					self#write_expr expr;
					self#write ")"
				end
		(**
			Writes try...catch to output buffer
		*)
		method private write_expr_try_catch try_expr catches =
			let catching_dynamic = ref false in
			let haxe_exception = self#use hxexception_type_path
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
			if ctx.debug then
				self#write_statement ((self#use (["haxe"], "CallStack")) ^ "::saveExceptionTrace($__hx__caught_e)");
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
					self#write ((self#use boot_type_path) ^ "::typedCast(");
					self#write_expr_type mtype;
					self#write ", ";
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
							(match expr.eexpr with
								| TConst (TString php) ->
									Codegen.interpolate_code ctx php args self#write self#write_expr self#pos
								| _ -> fail self#pos __POS__
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
			let ttype = type_of_module_type mtype in
			match expr_hierarchy with
				| _ :: { eexpr = TField _ } :: _ -> self#write (self#use_t ttype)
				| _ ->
					let class_name =
						match self#use_t ttype with
							| "int" -> "'Int'"
							| "float" -> "'Float'"
							| "bool" -> "'Bool'"
							| "string" -> "'String'"
							| "mixed" -> "'Dynamic'"
							| "Enum" -> "'Enum'"
							| "Class" -> "'Class'"
							| name -> name ^ "::class"
					in
					self#write ((self#use boot_type_path) ^ "::getClass(" ^ class_name ^ ")")
		(**
			Writes binary operation to output buffer
		*)
		method private write_expr_binop operation expr1 expr2 =
			let write_method method_name =
				self#write (method_name ^ "(");
				self#write_expr expr1;
				self#write ", ";
				self#write_expr expr2;
				self#write ")"
			in
			let write_for_concat expr =
				if ((is_constant expr) && not (is_constant_null expr)) || (is_concatenation expr) then
					self#write_expr expr
				else begin
					self#write "(";
					self#write_expr expr;
					self#write "??'null')"
				end
			and write_binop ?writer ?right_writer str =
				let write_left = match writer with None -> self#write_expr | Some writer -> writer in
				let write_right = match right_writer with None -> write_left | Some writer -> writer
				and need_parenthesis =
					match expr_hierarchy with
						| _ :: { eexpr = TBinop (parent, _, _) } :: _ -> need_parenthesis_for_binop operation parent
						| _ -> false
				in
				if need_parenthesis then self#write "(";
				write_left expr1;
				self#write str;
				write_right expr2;
				if need_parenthesis then self#write ")"
			and compare_strings op =
				write_method "strcmp";
				self#write (op ^ "0")
			in
			let compare op =
				if is_string expr1 && is_string expr2 then
						compare_strings op
					else
						write_binop op
			in
			match operation with
				| OpAdd ->
					if (is_string expr1) || (is_string expr2) then
						write_binop ~writer:write_for_concat " . "
					else if (is_unknown_type expr1.etype) && (is_unknown_type expr2.etype) then
						write_method ((self#use boot_type_path) ^ "::addOrConcat")
					else
						write_binop " + "
				| OpMult -> write_binop " * "
				| OpDiv -> write_binop " / "
				| OpSub -> write_binop " - "
				| OpAssign -> write_binop " = "
				| OpEq ->
					if need_boot_equal expr1 expr2 then
						write_method ((self#use boot_type_path) ^ "::equal")
					else
						write_binop " === "
				| OpNotEq ->
					if need_boot_equal expr1 expr2 then
						begin
							self#write "!";
							write_method ((self#use boot_type_path) ^ "::equal")
						end
					else
						write_binop " !== "
				| OpGt -> compare " > "
				| OpGte -> compare " >= "
				| OpLt -> compare " < "
				| OpLte -> compare " <= "
				| OpAnd -> write_binop " & "
				| OpOr -> write_binop " | "
				| OpXor -> write_binop " ^ "
				| OpBoolAnd -> write_binop " && "
				| OpBoolOr -> write_binop " || "
				| OpShl  -> write_binop " << "
				| OpShr -> write_binop " >> "
				| OpMod ->
					if is_int expr1 && is_int expr2 then
						write_binop " % "
					else
						write_method "fmod"
				| OpUShr -> write_method ((self#use boot_type_path) ^ "::shiftRightUnsigned")
				| OpAssignOp OpAdd ->
					if (is_string expr1) then
						begin
							self#write_expr expr1;
							self#write " = ";
							write_binop ~writer:write_for_concat " . "
						end
					else if (is_unknown_type expr1.etype) && (is_unknown_type expr2.etype) then
						begin
							self#write_expr expr1;
							self#write " = ";
							write_method ((self#use boot_type_path) ^ "::addOrConcat")
						end
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
				| OpAssignOp OpMod ->
					if is_int expr1 && is_int expr2 then
						write_binop " %= "
					else
						write_method "fmod"
				| OpAssignOp OpUShr ->
					self#write_expr expr1;
					self#write " = ";
					write_method ((self#use boot_type_path) ^ "::shiftRightUnsigned")
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
				let expr_without_casts = reveal_expr expr in
				(match expr_without_casts.eexpr with
					| TNew _
					| TArrayDecl _
					| TObjectDecl _ -> self#write_expr (parenthesis expr)
					| TConst TSuper ->
						self#write "parent";
						access_str := "::"
					| _ -> self#write_expr expr
				);
				self#write (!access_str ^ field_str)
			in
			match (follow expr.etype, access) with
				| (TInst ({ cl_path = ([], "String") }, _), FInstance (_, _, { cf_name = "length"; cf_kind = Var _ })) ->
					self#write "strlen(";
					self#write_expr expr;
					self#write ")"
				| (_, FInstance (_, _, field)) -> write_access "->" (field_name field)
				| (_, FStatic (_, ({ cf_kind = Var _ } as field))) ->
					(match (reveal_expr expr).eexpr with
						| TTypeExpr _ -> write_access "::" ("$" ^ (field_name field))
						| _ -> write_access "->" (field_name field)
					)
				| (_, FStatic (_, ({ cf_kind = Method MethDynamic } as field))) ->
					(match expr_hierarchy with
						| _ :: { eexpr = TCall ({ eexpr = TField (e, a) }, _) } :: _ when a == access ->
							self#write "(";
							write_access "::" ("$" ^ (field_name field));
							self#write ")"
						| _ ->
							write_access "::" ("$" ^ (field_name field))
					)
				| (_, FStatic (_, ({ cf_kind = Method _ } as field))) -> self#write_expr_field_static expr field
				| (_, FAnon field) ->
					let written_as_probable_string = self#write_expr_field_if_string expr (field_name field) in
					if not written_as_probable_string then write_access "->" (field_name field)
				| (_, FDynamic field_name) ->
					let written_as_probable_string = self#write_expr_field_if_string expr field_name in
					if not written_as_probable_string then write_access "->" field_name
				| (_, FClosure (tcls, field)) -> self#write_expr_field_closure tcls field expr
				| (_, FEnum (_, field)) ->
					if is_enum_constructor_with_args field then
						if not self#parent_expr_is_call then
							begin
								self#write (self#use boot_type_path ^ "::closure(");
								self#write_expr expr;
								(match (reveal_expr expr).eexpr with
									| TTypeExpr _ -> self#write "::class"
									| _ -> self#write "->phpClassName"
								);
								self#write (", '" ^ field.ef_name ^ "')")
							end
						else
							write_access "::" field.ef_name
					else
						begin
							write_access "::" field.ef_name;
							self#write "()"
						end

		(**
			Writes field access on Dynamic expression to output buffer
		*)
		method private write_expr_field_if_string expr field_name =
			(* Special case for String fields *)
			match field_name with
				| "length"
				| "toUpperCase"
				| "toLowerCase"
				| "charAt"
				| "indexOf"
				| "lastIndexOf"
				| "split"
				| "toString"
				| "substring"
				| "substr"
				| "charCodeAt" ->
					self#write ((self#use hxdynamicstr_type_path) ^ "::wrap(");
					self#write_expr expr;
					self#write (")->" ^ field_name);
					true
				| _ ->
					false
		(**
			Convert field access expressions for strings to native PHP string functions and write to output buffer
		*)
		method private write_expr_call_string expr access args =
			match access with
				| FInstance (_, _, ({ cf_kind = Method _ } as field)) ->
					self#write ((self#use hxstring_type_path) ^ "::" ^ (field_name field) ^ "(");
					write_args buffer self#write_expr (expr :: args);
					self#write ")"
				| _ -> fail self#pos __POS__
		(**
			Writes FStatic field access for methods to output buffer
		*)
		method private write_expr_field_static expr field =
			let write_expr () =
				match expr.eexpr with
					| TTypeExpr (TClassDecl { cl_path = ([], "String") }) -> self#write (self#use hxstring_type_path)
					| _ -> self#write_expr expr
			and operator =
				match (reveal_expr expr).eexpr with
					| TTypeExpr _ -> "::"
					| _ -> "->"
			in
			match expr_hierarchy with
				| _ :: { eexpr = TCall ({ eexpr = TField (e, FStatic (_, f)) }, _) } :: _ when e == expr && f == field ->
					write_expr ();
					self#write (operator ^ (field_name field))
				| _ ->
					let (args, return_type) = get_function_signature field  in
					self#write "function(";
					write_args buffer (self#write_arg true) args;
					self#write ") { return ";
					write_expr ();
					self#write (operator ^ (field_name field) ^ "(");
					write_args buffer (self#write_arg false) args;
					self#write "); }"
		(**
			Writes FClosure field access to output buffer
		*)
		method private write_expr_field_closure tcls field expr =
			let new_closure = "new " ^ (self#use hxclosure_type_path) in
			match expr.eexpr with
				| TTypeExpr mtype ->
					let class_name = self#use_t (type_of_module_type mtype) in
					self#write (new_closure ^ "(" ^ class_name ^ "::class, '" ^ (field_name field) ^ "')");
				| _ ->
					self#write (new_closure ^ "(");
					(match follow expr.etype with
						| TInst ({ cl_path = ([], "String") }, []) ->
							self#write ((self#use hxdynamicstr_type_path) ^ "::wrap(");
							self#write_expr expr;
							self#write ")"
						| _ ->
							self#write_expr expr
					);
					self#write (", '" ^ (field_name field) ^ "')")
		(**
			Write anonymous object declaration to output buffer
		*)
		method private write_expr_object_declaration fields =
			match fields with
				| [] ->  self#write ("new " ^ (self#use hxanon_type_path) ^ "()")
				| _ ->
					self#write ("new " ^ (self#use hxanon_type_path)  ^ "([\n");
					self#indent_more;
					let write_field (key, value) = self#write_array_item ~key:key value in
					List.iter write_field fields;
					self#indent_less;
					self#write_indentation;
					self#write "])"
		(**
			Writes specified type to output buffer depending on type of expression.
		*)
		method private write_type type_expr =
			match type_expr.eexpr with
				| TTypeExpr (TClassDecl tcls) ->
					self#write (self#use_t (TInst (tcls, [])))
				| _ ->
					if is_string type_expr then
						self#write_expr type_expr
					else begin
						self#write "(";
						self#write_expr type_expr;
						self#write "->phpClassName)";
					end
		(**
			Write language specific expression declared in `php.PHP` extern
		*)
		method private write_expr_call_lang_extern expr args =
			let name = match expr.eexpr with
				| TField (_, FStatic (_, field)) -> field_name field
				| _ -> fail self#pos __POS__
			in
			match name with
				| "int" | "float"
				| "string" | "bool"
				| "object" | "array" -> self#write_expr_lang_cast name args
				| "binop" -> self#write_expr_lang_binop args
				| "instanceof" -> self#write_expr_lang_instanceof args
				| "foreach" -> self#write_expr_lang_foreach args
				| "construct" -> self#write_expr_lang_construct args
				| "getField" -> self#write_expr_lang_get_field args
				| "setField" -> self#write_expr_lang_set_field args
				| "getStaticField" -> self#write_expr_lang_get_static_field args
				| "setStaticField" -> self#write_expr_lang_set_static_field args
				| "call" -> self#write_expr_lang_call args
				| "staticCall" -> self#write_expr_lang_static_call args
				| "arrayDecl" -> self#write_expr_lang_array_decl args
				| "splat" -> self#write_expr_lang_splat args
				| "suppress" -> self#write_expr_lang_suppress args
				| "keepVar" -> ()
				| _ -> fail self#pos __POS__
		(**
			Writes splat operator (for `php.Syntax.splat()`)
		*)
		method private write_expr_lang_splat args =
			match args with
				| [ args_expr ] ->
					self#write "...";
					self#write_expr args_expr
				| _ -> fail self#pos __POS__
		(**
			Writes error suppression operator (for `php.Syntax.suppress()`)
		*)
		method private write_expr_lang_suppress args =
			match args with
				| [ args_expr ] ->
					self#write "@";
					self#write_expr args_expr
				| _ -> fail self#pos __POS__
		(**
			Writes native array declaration (for `php.Syntax.arrayDecl()`)
		*)
		method private write_expr_lang_array_decl args =
			self#write "[";
			write_args buffer (fun e -> self#write_expr e) args;
			self#write "]"
		(**
			Writes a call to instance method (for `php.Syntax.call()`)
		*)
		method private write_expr_lang_call args =
			match args with
				| obj_expr :: method_expr :: args ->
					self#write_expr obj_expr;
					self#write "->{";
					self#write_expr method_expr;
					self#write "}(";
					write_args buffer (fun e -> self#write_expr e) args;
					self#write ")"
				| _ -> fail self#pos __POS__
		(**
			Writes a call to a static method (for `php.Syntax.staticCall()`)
		*)
		method private write_expr_lang_static_call args =
			match args with
				| type_expr :: method_expr :: args ->
					self#write_type type_expr;
					self#write "::{";
					self#write_expr method_expr;
					self#write "}(";
					write_args buffer (fun e -> self#write_expr e) args;
					self#write ")"
				| _ -> fail self#pos __POS__
		(**
			Writes field access for reading (for `php.Syntax.getField()`)
		*)
		method private write_expr_lang_get_field args =
			match args with
				| obj_expr :: field_expr :: [] ->
					self#write_expr obj_expr;
					self#write "->{";
					self#write_expr field_expr;
					self#write "}"
				| _ -> fail self#pos __POS__
		(**
			Writes field access for writing (for `php.Syntax.setField()`)
		*)
		method private write_expr_lang_set_field args =
			match args with
				| obj_expr :: field_expr :: value_expr :: [] ->
					self#write_expr obj_expr;
					self#write "->{";
					self#write_expr field_expr;
					self#write "}";
					self#write " = ";
					self#write_expr value_expr
				| _ -> fail self#pos __POS__
		(**
			Writes static field access for reading (for `php.Syntax.getStaticField()`)
		*)
		method private write_expr_lang_get_static_field args =
			match args with
				| type_expr :: field_expr :: [] ->
					self#write_type type_expr;
					self#write "::${";
					self#write_expr field_expr;
					self#write "}"
				| _ -> fail self#pos __POS__
		(**
			Writes static field access for writing (for `php.Syntax.setField()`)
		*)
		method private write_expr_lang_set_static_field args =
			match args with
				| type_expr :: field_expr :: value_expr :: [] ->
					self#write_expr type_expr;
					self#write "::${";
					self#write_expr field_expr;
					self#write "}";
					self#write " = ";
					self#write_expr value_expr
				| _ -> fail self#pos __POS__
		(**
			Writes `new` expression with class name taken local variable (for `php.Syntax.construct()`)
		*)
		method private write_expr_lang_construct args =
			let (class_expr, args) = match args with
				| class_expr :: args -> (class_expr, args)
				| _ -> fail self#pos __POS__
			in
			self#write "new ";
			self#write_expr class_expr;
			self#write "(";
			write_args buffer (fun e -> self#write_expr e) args;
			self#write ")"
		(**
			Writes native php type conversion to output buffer (e.g. `php.Syntax.int()`)
		*)
		method private write_expr_lang_cast type_name args =
			match args with
				| expr :: [] ->
					let add_parentheses = match self#parent_expr with Some e -> is_access e | None -> false
					and expr = match expr.eexpr with
						| TLocal e -> expr
						| _ -> parenthesis expr
					in
					if add_parentheses then self#write "(";
					self#write ("(" ^ type_name ^")");
					self#write_expr expr;
					if add_parentheses then self#write ")"
				| _ -> fail self#pos __POS__
		(**
			Generates binary operation to output buffer (for `php.Syntax.binop()`)
		*)
		method private write_expr_lang_binop args =
			match args with
				| val_expr1 :: operator_expr :: val_expr2 :: [] ->
					let operator = match operator_expr.eexpr with
						| TConst (TString operator) -> operator
						| _ -> error_and_exit self#pos "Second argument for php.Syntax.binop() must be a constant string"
					in
					self#write "(";
					self#write_expr val_expr1;
					self#write (" " ^ operator ^ " ");
					self#write_expr val_expr2;
					self#write ")"
				| _ -> fail self#pos __POS__
		(**
			Writes `instanceof` expression to output buffer (for `php.Syntax.instanceof()`)
		*)
		method private write_expr_lang_instanceof args =
			match args with
				| val_expr :: type_expr :: [] ->
					self#write_expr val_expr;
					self#write " instanceof ";
					(match type_expr.eexpr with
						| TTypeExpr (TClassDecl tcls) ->
							self#write (self#use_t (TInst (tcls, [])))
						| _ ->
							self#write_expr type_expr;
							if not (is_string type_expr) then self#write "->phpClassName"
					)
				| _ -> fail self#pos __POS__
		(**
			Writes `foreach` expression to output buffer (for `php.Syntax.foreach()`)
		*)
		method private write_expr_lang_foreach args =
			match args with
				| collection_expr :: { eexpr = TFunction fn } :: [] ->
					let (key_name, value_name) = match fn.tf_args with
						| ({ v_name = key_name }, _) :: ({ v_name = value_name }, _) :: [] -> (key_name, value_name)
						| _ -> fail self#pos __POS__
					and add_parentheses =
						match collection_expr.eexpr with
							| TLocal _ -> false
							| _ -> true
					in
					self#write "foreach (";
					if add_parentheses then self#write "(";
					self#write_expr collection_expr;
					if add_parentheses then self#write ")";
					self#write (" as $" ^ key_name ^ " => $" ^ value_name ^ ") ");
					self#write_as_block fn.tf_expr
				| _ ->
					error_and_exit self#pos "PHP.foreach() only accepts anonymous function declaration for second argument."
		(**
			Writes TCall to output buffer
		*)
		method private write_expr_call target_expr args =
			let target_expr = reveal_expr target_expr in
			(match target_expr.eexpr with
				| TConst TSuper -> self#write "parent::__construct"
				| TField (expr, FClosure (_,_)) -> self#write_expr (parenthesis target_expr)
				| _ -> self#write_expr target_expr
			);
			self#write "(";
			write_args buffer self#write_expr args;
			self#write ")";
		(**
			Writes a name of a function or a constant from global php namespace
		*)
		method private write_expr_php_global target_expr =
			match target_expr.eexpr with
				| TField (_, FStatic (_, field)) -> self#write (field_name field)
				| _ -> fail self#pos __POS__
		(**
			Writes access to PHP class constant
		*)
		method private write_expr_php_class_const target_expr =
			match target_expr.eexpr with
				| TField (_, FStatic (ecls, field)) ->
					self#write ((self#use_t (TInst (ecls, []))) ^ "::" ^ (field_name field))
				| _ -> fail self#pos __POS__
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
			let parent_is_if = match self#parent_expr with Some { eexpr = TIf _ } -> true | _ -> false in
			if parent_is_if then self#write "(";
			(match condition.eexpr with
				| TParenthesis expr -> self#write_expr expr;
				| _ -> self#write_expr else_expr
			);
			self#write " ? ";
			self#write_expr if_expr;
			self#write " : ";
			self#write_expr else_expr;
			if parent_is_if then self#write ")"
		(**
			Writes "if...else..." expression to output buffer
		*)
		method private write_expr_if condition if_expr (else_expr:texpr option) =
			let is_ternary =
				if self#parent_expr_is_block then
					false
				else
					match (if_expr.eexpr, else_expr) with
						| (TBlock _, _) | (_, Some { eexpr=TBlock _ }) -> false
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
					self#write_as_block ~unset_locals:true expr
				| DoWhile ->
					self#write "do ";
					self#write_as_block ~unset_locals:true expr;
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
			(match expr.eexpr with
				| TConst TNull -> self#write "(null)"
				| _ -> self#write_expr expr
			);
			self#write ("->params[" ^ (string_of_int index) ^ "]")
		(**
			Writes argument for function declarations or calls
		*)
		method write_arg with_optionals (arg_name, optional, (arg_type:Type.t)) =
			self#write ("$" ^ arg_name ^ (if with_optionals && optional then " = null" else ""))
		(**
			Writes argument with optional value for function declarations
		*)
		method write_function_arg arg =
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
				enm.e_constrs;
			self#write_reflection
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
			let index_str = string_of_int field.ef_index in
			(match args with
				| [] -> self#write ((self#use hxenum_type_path) ^ "::singleton(static::class, '" ^ name ^ "', " ^ index_str ^")")
				| args ->
					self#write ("new " ^ self#get_name ^ "('" ^ name ^ "', " ^ index_str ^", [");
					write_args buffer (fun (name, _, _) -> self#write ("$" ^ name)) args;
					self#write "])"
			);
			self#write ";\n";
			self#indent_less;
			self#write_line "}"
		(**
			Writes special methods for reflection
		*)
		method private write_reflection =
			(* __hx__list *)
			self#write_empty_lines;
			self#indent 1;
			self#write_line "/**";
			self#write_line " * Returns array of (constructorIndex => constructorName)";
			self#write_line " *";
			self#write_line " * @return string[]";
			self#write_line " */";
			self#write_line "static public function __hx__list ()";
			self#write_line "{";
			self#indent_more;
			self#write_line "return [";
			self#indent_more;
			PMap.iter
				(fun name field ->
					self#write_line ((string_of_int field.ef_index) ^ " => '" ^ name ^ "',")
				)
				enm.e_constrs;
			self#indent_less;
			self#write_statement "]";
			self#indent_less;
			self#write_line "}";
			(* __hx__paramsCount *)
			self#write_empty_lines;
			self#indent 1;
			self#write_line "/**";
			self#write_line " * Returns array of (constructorName => parametersCount)";
			self#write_line " *";
			self#write_line " * @return int[]";
			self#write_line " */";
			self#write_line "static public function __hx__paramsCount ()";
			self#write_line "{";
			self#indent_more;
			self#write_line "return [";
			self#indent_more;
			PMap.iter
				(fun name field ->
					let count = match follow field.ef_type with
						| TFun (params, _) -> List.length params
						| TEnum _ -> 0
						| _ -> fail field.ef_pos __POS__
					in
					self#write_line ("'" ^ name ^ "' => " ^ (string_of_int count) ^ ",")
				)
				enm.e_constrs;
			self#indent_less;
			self#write_statement "]";
			self#indent_less;
			self#write_line "}";
		(**
			Method `__hx__init` is not needed for enums
		**)
		method private write_hx_init_body = ()
		(**
			No need for additional initialization of enum instances
		*)
		method private write_instance_initialization = ()
		(**
			No need for additional type initialization for enums
		*)
		method private write_pre_hx_init = ()
	end

(**
	Builds class contents
*)
class class_builder ctx (cls:tclass) =
	object (self)
		inherit type_builder ctx (get_wrapper (TClassDecl cls)) as super
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
							cf_name_pos = cls.cl_pos;
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
							cf_expr_unoptimized = None;
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
			PMap.iter (write_if_method false) cls.cl_fields;
			(* Generate `__toString()` if not defined by user, but has `toString()` *)
			self#write_toString_if_required
		method private write_toString_if_required =
			if PMap.exists "toString" cls.cl_fields then
				if (not cls.cl_interface) && (not (PMap.exists "__toString" cls.cl_statics)) && (not (PMap.exists "__toString" cls.cl_fields)) then
					begin
						self#write_empty_lines;
						self#indent 1;
						self#write_line "public function __toString() {";
						self#indent_more;
						self#write_line "return $this->toString();";
						self#indent_less;
						self#write_line "}"
					end
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
			let prefix = String.concat "\\" (get_php_prefix ctx) in
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
				let field_access = "self::$" ^ (field_name field) in
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
				let write_assign expr =
					self#write_indentation;
					self#write ("self::$" ^ (field_name field) ^ " = ");
					self#write_expr expr
				in
				(* Do not generate fields for RTTI meta, because this generator uses another way to store it *)
				let is_auto_meta_var = field.cf_name = "__meta__" && (has_rtti_meta ctx wrapper#get_module_type) in
				if (is_var_with_nonconstant_expr field) && (not is_auto_meta_var) then begin
					(match field.cf_expr with
						| None -> ()
						(* There can be not-inlined blocks when compiling with `-debug` *)
						| Some { eexpr = TBlock exprs } ->
							let rec write_per_line exprs =
								match exprs with
									| [] -> ()
									| [expr] -> write_assign expr
									| expr :: rest ->
										self#write_indentation;
										self#write_expr expr;
										self#write ";\n";
										write_per_line rest
							in
							write_per_line exprs
						| Some expr -> write_assign expr
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
				| Var _ when is_real_var field ->
					(* Do not generate fields for RTTI meta, because this generator uses another way to store it *)
					let is_auto_meta_var = is_static && field.cf_name = "__meta__" && (has_rtti_meta ctx wrapper#get_module_type) in
					if not is_auto_meta_var then self#write_var field is_static;
				| Var _ -> ()
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
			self#write (visibility ^ " $" ^ (field_name field));
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
			self#write ("const " ^ (field_name field) ^ " = ");
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
					self#write ("function " ^ (field_name field) ^ " (");
					write_args buffer (self#write_arg true) args;
					self#write ")";
					self#write " ;\n"
				| Some { eexpr = TFunction fn } ->
					let name = if field.cf_name = "new" then "__construct" else (field_name field) in
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
			self#write ((get_visibility field.cf_meta) ^ " function " ^ (field_name field));
			(match field.cf_expr with
				| None -> (* interface *)
					self#write " (";
					write_args buffer (self#write_arg true) args;
					self#write ");\n";
				| Some { eexpr = TFunction fn } -> (* normal class *)
					self#write " (";
					write_args buffer self#write_function_arg fn.tf_args;
					self#write ")\n";
					self#write_line "{";
					self#indent_more;
					self#write_indentation;
					let field_access = "$this->" ^ (field_name field)
					and default_value = "$this->__hx__default__" ^ (field_name field) in
					self#write ("if (" ^ field_access ^ " !== " ^ default_value ^ ") return call_user_func_array(" ^ field_access ^ ", func_get_args());\n");
					self#write_fake_block fn.tf_expr;
					self#indent_less;
					self#write_line "}";
					(* Don't forget to create a field for default value *)
					self#write_statement ("protected $__hx__default__" ^ (field_name field))
				| _ -> fail field.cf_pos __POS__
			);
		(**
			Writes initialization code for instances of this class
		*)
		method private write_instance_initialization =
			let init_dynamic_method field =
				let field_name = field_name field in
				let default_field = "$this->__hx__default__" ^ field_name in
				self#write_line ("if (!" ^ default_field ^ ") {");
				self#indent_more;
				self#write_statement (default_field ^ " = new " ^ (self#use hxclosure_type_path) ^ "($this, '" ^ field_name ^ "')");
				self#write_statement ("if ($this->" ^ field_name ^ " === null) $this->" ^ field_name ^ " = " ^ default_field);
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
		(**
			Writes additional initialization code, which should be called before `__hx__init()`
		*)
		method private write_pre_hx_init =
			let getters = ref []
			and setters = ref [] in
			let collect field =
				match field.cf_kind with
					| Var { v_read = read; v_write = write } ->
						if read = AccCall then getters := field.cf_name :: !getters;
						if write = AccCall then setters := field.cf_name :: !setters;
					| _ -> ()
			in
			List.iter collect cls.cl_ordered_fields;
			List.iter collect cls.cl_ordered_statics;
			let rec write lst =
				match lst with
					| [] -> ()
					| [item] -> self#write_line ("'" ^ item ^ "' => true");
					| item :: rest ->
						self#write_line ("'" ^ item ^ "' => true,");
						write rest
			and type_name = get_full_type_name ~escape:true ~omit_first_slash:true (add_php_prefix ctx wrapper#get_type_path) in
			let write_register register_method lst =
				self#write_line ((self#use boot_type_path) ^ "::" ^ register_method ^ "('" ^ type_name ^ "', [");
				self#indent_more;
				write lst;
				self#indent_less;
				self#write_statement "])"
			in
			if List.length !getters > 0 then write_register "registerGetters" !getters;
			if List.length !setters > 0 then write_register "registerSetters" !setters;
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
			output_string channel ("\n//Haxe source file: " ^ builder#get_source_file ^ "\n");
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
	Hashtbl.iter
		(fun name data ->
			write_resource com.file name data
		)
		com.resources;
	clear_wrappers ();