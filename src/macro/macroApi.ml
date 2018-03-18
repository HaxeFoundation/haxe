open Ast
open Type
open Common

exception Invalid_expr
exception Abort

(**
	Our access to the compiler from the macro api
**)

type 'value compiler_api = {
	pos : Globals.pos;
	get_com : unit -> Common.context;
	get_type : string -> Type.t option;
	get_module : string -> Type.t list;
	after_typing : (module_type list -> unit) -> unit;
	on_generate : (Type.t list -> unit) -> unit;
	after_generate : (unit -> unit) -> unit;
	on_type_not_found : (string -> 'value) -> unit;
	parse_string : string -> Globals.pos -> bool -> Ast.expr;
	type_expr : Ast.expr -> Type.texpr;
	resolve_type  : Ast.complex_type -> Globals.pos -> t;
	type_macro_expr : Ast.expr -> Type.texpr;
	store_typed_expr : Type.texpr -> Ast.expr;
	allow_package : string -> unit;
	type_patch : string -> string -> bool -> string option -> unit;
	meta_patch : string -> string -> string option -> bool -> unit;
	set_js_generator : (Genjs.ctx -> unit) -> unit;
	get_local_type : unit -> t option;
	get_expected_type : unit -> t option;
	get_call_arguments : unit -> Ast.expr list option;
	get_local_method : unit -> string;
	get_local_imports : unit -> Ast.import list;
	get_local_using : unit -> tclass list;
	get_local_vars : unit -> (string, Type.tvar) PMap.t;
	get_build_fields : unit -> 'value;
	get_pattern_locals : Ast.expr -> Type.t -> (string,Type.tvar * Globals.pos) PMap.t;
	define_type : 'value -> string option -> unit;
	define_module : string -> 'value list -> ((string * Globals.pos) list * Ast.import_mode) list -> Ast.type_path list -> unit;
	module_dependency : string -> string -> unit;
	module_reuse_call : string -> string -> unit;
	current_module : unit -> module_def;
	on_reuse : (unit -> bool) -> unit;
	mutable current_macro_module : unit -> module_def;
	delayed_macro : int -> (unit -> (unit -> 'value));
	use_cache : unit -> bool;
	format_string : string -> Globals.pos -> Ast.expr;
	cast_or_unify : Type.t -> texpr -> Globals.pos -> bool;
	add_global_metadata : string -> string -> (bool * bool * bool) -> unit;
	add_module_check_policy : string list -> int list -> bool -> int -> unit;
	decode_expr : 'value -> Ast.expr;
	encode_expr : Ast.expr -> 'value;
	encode_ctype : Ast.type_hint -> 'value;
	decode_type : 'value -> t;
	flush_context : (unit -> t) -> t;
	typer_ctx : Typecore.typer;
}


type enum_type =
	| IExpr
	| IBinop
	| IUnop
	| IConst
	| ITParam
	| ICType
	| IField
	| IType
	| IFieldKind
	| IMethodKind
	| IVarAccess
	| IAccess
	| IClassKind
	| ITypedExpr
	| ITConstant
	| IModuleType
	| IFieldAccess
	| IAnonStatus
	| IQuoteStatus
	| IImportMode

type obj_type =
	(* make_const *)
	| O__Const
	(* Expr *)
	| OImportExpr
	| OImportExpr_path
	| OTypePath
	| OMetadataEntry
	| OField
	| OTypeParamDecl
	| OFunction
	| OFunctionArg
	| OExprDef_fields
	| OVar
	| OCase
	| OCatch
	| OExpr
	(* Type *)
	| OMetaAccess
	| OTypeParameter
	| OClassType
	| OAbstractType
	| OAnonType
	| ODefType
	| OEnumType
	| OClassField
	| OAbstractType_binops
	| OAbstractType_unops
	| OAbstractType_from
	| OAbstractType_to
	| OEnumField
	| OClassType_superClass
	| OClassType_interfaces
	| OType_args
	| OTVar
	| OTVar_extra
	| OTFunc
	| OTFunc_args
	| OFieldAccess_c
	| OTypedExprDef
	| OTypedExprDef_fields
	| OTypedExprDef_cases
	| OTypedExprDef_catches
	| OJSGenApi
	| OContext_getPosInfos
	| OCompiler_getDisplayPos
	| ORef
(* ---- ^^^^^ please exactly match the name of the typedef or use TypeName_field if it's a anonymous *)

(**
	Our access to the interpreter from the macro api
**)

module type InterpApi = sig
	type value

	val vnull : value
	val vint : int -> value
	val vfloat : float -> value
	val vint32 : int32 -> value
	val vbool : bool -> value

	val encode_array : value list -> value
	val encode_string  : string -> value
	val encode_obj : obj_type -> (string * value) list -> value

	val vfun0 : (unit -> value) -> value
	val vfun1 : (value -> value) -> value
	val vfun2 : (value -> value -> value) -> value
	val vfun3 : (value -> value -> value -> value) -> value
	val vfun4 : (value -> value -> value -> value -> value) -> value
	val vfun5 : (value -> value -> value -> value -> value -> value) -> value

	val encode_pos : Globals.pos -> value
	val encode_enum : enum_type -> Globals.pos option -> int -> value list -> value
	val encode_string_map : ('a -> value) -> (string, 'a) PMap.t -> value

	val encode_tdecl : Type.module_type -> value
	val encode_lazytype : tlazy ref -> (unit -> value) -> value
	val encode_unsafe : Obj.t -> value

	val field : value -> string -> value

	val decode_bool : value -> bool
	val decode_int : value -> int
	val decode_i32 : value -> int32
	val decode_string : value -> string
	val decode_array : value -> value list

	val decode_pos : value -> Globals.pos
	val decode_enum : value -> int * value list
	val decode_tdecl : value -> Type.module_type
	val decode_lazytype : value -> tlazy ref
	val decode_unsafe : value -> Obj.t

	val decode_enum_with_pos : value -> (int * value list) * Globals.pos

	val encode_ref : 'a -> ('a -> value) -> (unit -> string) -> value
	val decode_ref : value -> 'a

	val compiler_error : string -> Globals.pos -> 'a
	val error_message : string -> 'a
	val value_to_expr : value -> Globals.pos -> Ast.expr
	val value_signature : value -> string

	val encode_bytes : bytes -> value
	val decode_bytes : value -> bytes (* haxe.io.Bytes *)

	val prepare_callback : value -> int -> (value list -> value)

	val value_string : value -> string

	val flush_core_context : (unit -> t) -> t

	val handle_decoding_error : value -> Type.t -> (string * (string * int) list)

end

let enum_name = function
	| IExpr -> "ExprDef"
	| IBinop -> "Binop"
	| IUnop -> "Unop"
	| IConst -> "Constant"
	| ITParam -> "TypeParam"
	| ICType -> "ComplexType"
	| IField -> "FieldType"
	| IType -> "Type"
	| IFieldKind -> "FieldKind"
	| IMethodKind -> "MethodKind"
	| IVarAccess -> "VarAccess"
	| IAccess -> "Access"
	| IClassKind -> "ClassKind"
	| ITypedExpr -> "TypedExprDef"
	| ITConstant -> "TConstant"
	| IModuleType -> "ModuleType"
	| IFieldAccess -> "FieldAccess"
	| IAnonStatus -> "AnonStatus"
	| IImportMode -> "ImportMode"
	| IQuoteStatus -> "QuoteStatus"

let proto_name = function
	| O__Const -> assert false
	| OImportExpr -> "ImportExpr", None
	| OImportExpr_path -> "ImportExpr", Some "path"
	| OTypePath -> "TypePath", None
	| OMetadataEntry -> "MetadataEntry", None
	| OField -> "Field", None
	| OTypeParamDecl -> "TypeParamDecl", None
	| OFunction -> "Function", None
	| OFunctionArg -> "FunctionArg", None
	| OExprDef_fields -> "ExprDef", Some "fields"
	| OVar -> "Var", None
	| OCase -> "Case", None
	| OCatch -> "Catch", None
	| OExpr -> "Expr", None
	| OMetaAccess -> "MetaAccess", None
	| OTypeParameter -> "TypeParameter", None
	| OClassType -> "ClassType", None
	| OAbstractType -> "AbstracType", None
	| OAnonType -> "AnonType", None
	| ODefType -> "DefType", None
	| OEnumType -> "EnumType", None
	| OClassField -> "ClassField", None
	| OAbstractType_binops -> "AbstractType", Some "binops"
	| OAbstractType_unops -> "AbstractType", Some "unops"
	| OAbstractType_from -> "AbstractType", Some "from"
	| OAbstractType_to -> "AbstractType", Some "to"
	| OEnumField -> "EnumField", None
	| OClassType_superClass -> "ClassType", Some "superClass"
	| OClassType_interfaces -> "ClassType", Some "interfaces"
	| OType_args -> "Type", Some "args"
	| OTVar -> "TVar", None
	| OTVar_extra -> "TVar", Some "extra"
	| OTFunc -> "TFunc", None
	| OTFunc_args -> "TFunc", Some "args"
	| OFieldAccess_c -> "FieldAccess", Some "c"
	| OTypedExprDef -> "TypedExprDef", None
	| OTypedExprDef_fields -> "TypedExprDef", Some "fields"
	| OTypedExprDef_cases -> "TypedExprDef", Some "cases"
	| OTypedExprDef_catches -> "TypedExprDef", Some "catches"
	| OJSGenApi -> "JSGenApi", None
	| OContext_getPosInfos -> "Context", Some "getPosInfos"
	| OCompiler_getDisplayPos -> "Compiler", Some "getDisplayPos"
	| ORef -> "Ref", None

let all_enums =
	let last = IImportMode in
	let rec loop i =
		let e : enum_type = Obj.magic i in
		if e = last then [e] else e :: loop (i + 1)
	in
	loop 0


let s_type_path = Globals.s_type_path

(* convert float value to haxe expression, handling inf/-inf/nan *)
let haxe_float f p =
	let std = (Ast.EConst (Ast.Ident "std"), p) in
	let math = (Ast.EField (std, "Math"), p) in
	if (f = infinity) then
		(Ast.EField (math, "POSITIVE_INFINITY"), p)
	else if (f = neg_infinity) then
		(Ast.EField (math, "NEGATIVE_INFINITY"), p)
	else if (f <> f) then
		(Ast.EField (math, "NaN"), p)
	else
		(Ast.EConst (Ast.Float (Numeric.float_repres f)), p)

(* ------------------------------------------------------------------------------------------------------------- *)
(* Our macro api functor *)

module MacroApiImpl(InterpImpl:InterpApi) = struct

open InterpImpl

(*
	The whole encoding/decoding of compiler values from/to interpreter values based on interpreter api
*)

let null f = function
	| None -> vnull
	| Some v -> f v

let encode_enum ?(pos=None) k tag vl = encode_enum k pos tag vl

let encode_const c =
	let tag, pl = match c with
	| Int s -> 0, [encode_string s]
	| Float s -> 1, [encode_string s]
	| String s -> 2, [encode_string s]
	| Ident s -> 3, [encode_string s]
	| Regexp (s,opt) -> 4, [encode_string s;encode_string opt]
	in
	encode_enum IConst tag pl

let rec encode_binop op =
	let tag, pl = match op with
	| OpAdd -> 0, []
	| OpMult -> 1, []
	| OpDiv -> 2, []
	| OpSub -> 3, []
	| OpAssign -> 4, []
	| OpEq -> 5, []
	| OpNotEq -> 6, []
	| OpGt -> 7, []
	| OpGte -> 8, []
	| OpLt -> 9, []
	| OpLte -> 10, []
	| OpAnd -> 11, []
	| OpOr -> 12, []
	| OpXor -> 13, []
	| OpBoolAnd -> 14, []
	| OpBoolOr -> 15, []
	| OpShl -> 16, []
	| OpShr -> 17, []
	| OpUShr -> 18, []
	| OpMod -> 19, []
	| OpAssignOp op -> 20, [encode_binop op]
	| OpInterval -> 21, []
	| OpArrow -> 22, []
	| OpIn -> 23, []
	in
	encode_enum IBinop tag pl

let encode_unop op =
	let tag = match op with
	| Increment -> 0
	| Decrement -> 1
	| Not -> 2
	| Neg -> 3
	| NegBits -> 4
	in
	encode_enum IUnop tag []

let encode_import (path,mode) =
	let tag,pl = match mode with
		| INormal -> 0, []
		| IAsName s -> 1, [encode_string s]
		| IAll -> 2,[]
	in
	let mode = encode_enum IImportMode tag pl in
	encode_obj OImportExpr [
		"path", encode_array (List.map (fun (name,p) -> encode_obj OImportExpr_path [ "pos", encode_pos p; "name", encode_string name]) path);
		"mode", mode
	]

let encode_placed_name (s,p) =
	encode_string s

let rec encode_path (t,_) =
	let fields = [
		"pack", encode_array (List.map encode_string t.tpackage);
		"name", encode_string t.tname;
		"params", encode_array (List.map encode_tparam t.tparams);
	] in
	encode_obj OTypePath (match t.tsub with
		| None -> fields
		| Some s -> ("sub", encode_string s) :: fields)

and encode_tparam = function
	| TPType t -> encode_enum ITParam 0 [encode_ctype t]
	| TPExpr e -> encode_enum ITParam 1 [encode_expr e]

and encode_access a =
	let tag = match a with
		| APublic -> 0
		| APrivate -> 1
		| AStatic -> 2
		| AOverride -> 3
		| ADynamic -> 4
		| AInline -> 5
		| AMacro -> 6
		| AFinal -> 7
	in
	encode_enum IAccess tag []

and encode_meta_entry (m,ml,p) =
	encode_obj OMetadataEntry [
		"name", encode_string (Meta.to_string m);
		"params", encode_array (List.map encode_expr ml);
		"pos", encode_pos p;
	]

and encode_meta_content m =
	encode_array (List.map encode_meta_entry m)

and encode_field (f:class_field) =
	let tag, pl = match f.cff_kind with
		| FVar (t,e) -> 0, [null encode_ctype t; null encode_expr e]
		| FFun f -> 1, [encode_fun f]
		| FProp (get,set, t, e) -> 2, [encode_placed_name get; encode_placed_name set; null encode_ctype t; null encode_expr e]
	in
	encode_obj OField [
		"name",encode_placed_name f.cff_name;
		"name_pos", encode_pos (pos f.cff_name);
		"doc", null encode_string f.cff_doc;
		"pos", encode_pos f.cff_pos;
		"kind", encode_enum IField tag pl;
		"meta", encode_meta_content f.cff_meta;
		"access", encode_array (List.map encode_access f.cff_access);
	]

and encode_ctype t =
	let tag, pl = match fst t with
	| CTPath p ->
		0, [encode_path (p,Globals.null_pos)]
	| CTFunction (pl,r) ->
		1, [encode_array (List.map encode_ctype pl);encode_ctype r]
	| CTAnonymous fl ->
		2, [encode_array (List.map encode_field fl)]
	| CTParent t ->
		3, [encode_ctype t]
	| CTExtend (tl,fields) ->
		4, [encode_array (List.map encode_path tl); encode_array (List.map encode_field fields)]
	| CTOptional t ->
		5, [encode_ctype t]
	| CTNamed (n,t) ->
		6, [encode_placed_name n; encode_ctype t]
	in
	encode_enum ~pos:(Some (pos t)) ICType tag pl

and encode_tparam_decl tp =
	encode_obj OTypeParamDecl [
		"name", encode_placed_name tp.tp_name;
		"name_pos", encode_pos (pos tp.tp_name);
		"params", encode_array (List.map encode_tparam_decl tp.tp_params);
		"constraints", encode_array (List.map encode_ctype tp.tp_constraints);
		"meta", encode_meta_content tp.tp_meta;
	]

and encode_fun f =
	encode_obj OFunction [
		"params", encode_array (List.map encode_tparam_decl f.f_params);
		"args", encode_array (List.map (fun (n,opt,m,t,e) ->
			encode_obj OFunctionArg [
				"name", encode_placed_name n;
				"name_pos", encode_pos (pos n);
				"opt", vbool opt;
				"meta", encode_meta_content m;
				"type", null encode_ctype t;
				"value", null encode_expr e;
			]
		) f.f_args);
		"ret", null encode_ctype f.f_type;
		"expr", null encode_expr f.f_expr
	]

and encode_expr e =
	let rec loop (e,p) =
		let tag, pl = match e with
			| EConst c ->
				0, [encode_const c]
			| EArray (e1,e2) ->
				1, [loop e1;loop e2]
			| EBinop (op,e1,e2) ->
				2, [encode_binop op;loop e1;loop e2]
			| EField (e,f) ->
				3, [loop e;encode_string f]
			| EParenthesis e ->
				4, [loop e]
			| EObjectDecl fl ->
				5, [encode_array (List.map (fun ((f,p,qs),e) -> encode_obj OExprDef_fields [
					"field",encode_string f;
					"name_pos",encode_pos p;
					"quotes",encode_enum IQuoteStatus (match qs with NoQuotes -> 0 | DoubleQuotes -> 1) [];
					"expr",loop e;
				]) fl)]
			| EArrayDecl el ->
				6, [encode_array (List.map loop el)]
			| ECall (e,el) ->
				7, [loop e;encode_array (List.map loop el)]
			| ENew (p,el) ->
				8, [encode_path p; encode_array (List.map loop el)]
			| EUnop (op,flag,e) ->
				9, [encode_unop op; vbool (match flag with Prefix -> false | Postfix -> true); loop e]
			| EVars vl ->
				10, [encode_array (List.map (fun (v,t,eo) ->
					encode_obj OVar [
						"name",encode_placed_name v;
						"name_pos",encode_pos (pos v);
						"type",null encode_ctype t;
						"expr",null loop eo;
					]
				) vl)]
			| EFunction (name,f) ->
				11, [null encode_string name; encode_fun f]
			| EBlock el ->
				12, [encode_array (List.map loop el)]
			| EFor (e,eloop) ->
				13, [loop e;loop eloop]
			| EIf (econd,e,eelse) ->
				14, [loop econd;loop e;null loop eelse]
			| EWhile (econd,e,flag) ->
				15, [loop econd;loop e;vbool (match flag with NormalWhile -> true | DoWhile -> false)]
			| ESwitch (e,cases,eopt) ->
				16, [loop e;encode_array (List.map (fun (ecl,eg,e,p) ->
					encode_obj OCase [
						"values",encode_array (List.map loop ecl);
						"guard",null loop eg;
						"expr",null loop e;
						"pos",encode_pos p;
					]
				) cases);null (fun (e,_) -> encode_null_expr e) eopt]
			| ETry (e,catches) ->
				17, [loop e;encode_array (List.map (fun (v,t,e,p) ->
					encode_obj OCatch [
						"name",encode_placed_name v;
						"name_pos",encode_pos (pos v);
						"type",encode_ctype t;
						"expr",loop e;
						"pos",encode_pos p
					]
				) catches)]
			| EReturn eo ->
				18, [null loop eo]
			| EBreak ->
				19, []
			| EContinue ->
				20, []
			| EUntyped e ->
				21, [loop e]
			| EThrow e ->
				22, [loop e]
			| ECast (e,t) ->
				23, [loop e; null encode_ctype t]
			| EDisplay (e,flag) ->
				24, [loop e; vbool flag]
			| EDisplayNew t ->
				25, [encode_path t]
			| ETernary (econd,e1,e2) ->
				26, [loop econd;loop e1;loop e2]
			| ECheckType (e,t) ->
				27, [loop e; encode_ctype t]
			| EMeta (m,e) ->
				28, [encode_meta_entry m;loop e]
			| EComplexType t ->
				30, [encode_ctype (t,p)]
		in
		encode_obj OExpr [
			"pos", encode_pos p;
			"expr", encode_enum IExpr tag pl;
		]
	in
	loop e

and encode_null_expr e =
	match e with
	| None ->
		encode_obj OExpr ["pos", vnull;"expr",vnull]
	| Some e ->
		encode_expr e

(* ---------------------------------------------------------------------- *)
(* EXPR DECODING *)

let opt f v =
	if v = vnull then None else Some (f v)

let opt_list f v =
	if v = vnull then [] else f v

let decode_opt_bool v =
	if v = vnull then false else decode_bool v

let decode_const c =
	match decode_enum c with
	| 0, [s] -> Int (decode_string s)
	| 1, [s] -> Float (decode_string s)
	| 2, [s] -> String (decode_string s)
	| 3, [s] -> Ident (decode_string s)
	| 4, [s;opt] -> Regexp (decode_string s, decode_string opt)
	| 5, [s] -> Ident (decode_string s) (** deprecated CType, keep until 3.0 release **)
	| _ -> raise Invalid_expr

let rec decode_op op =
	match decode_enum op with
	| 0, [] -> OpAdd
	| 1, [] -> OpMult
	| 2, [] -> OpDiv
	| 3, [] -> OpSub
	| 4, [] -> OpAssign
	| 5, [] -> OpEq
	| 6, [] -> OpNotEq
	| 7, [] -> OpGt
	| 8, [] -> OpGte
	| 9, [] -> OpLt
	| 10, [] -> OpLte
	| 11, [] -> OpAnd
	| 12, [] -> OpOr
	| 13, [] -> OpXor
	| 14, [] -> OpBoolAnd
	| 15, [] -> OpBoolOr
	| 16, [] -> OpShl
	| 17, [] -> OpShr
	| 18, [] -> OpUShr
	| 19, [] -> OpMod
	| 20, [op] -> OpAssignOp (decode_op op)
	| 21, [] -> OpInterval
	| 22,[] -> OpArrow
	| 23,[] -> OpIn
	| _ -> raise Invalid_expr

let decode_unop op =
	match decode_enum op with
	| 0, [] -> Increment
	| 1, [] -> Decrement
	| 2, [] -> Not
	| 3, [] -> Neg
	| 4, [] -> NegBits
	| _ -> raise Invalid_expr

let decode_import_mode t =
	match decode_enum t with
	| 0, [] -> INormal
	| 1, [alias] -> IAsName (decode_string alias)
	| 2, [] -> IAll
	| _ -> raise Invalid_expr

let decode_import t = (List.map (fun o -> ((decode_string (field o "name")), (decode_pos (field o "pos")))) (decode_array (field t "path")), decode_import_mode (field t "mode"))

let maybe_decode_pos p = try decode_pos p with Invalid_expr -> Globals.null_pos

let decode_placed_name vp v =
	decode_string v,maybe_decode_pos vp

let decode_opt_array f v =
	if v = vnull then [] else List.map f (decode_array v)

let rec decode_path t =
	{
		tpackage = List.map decode_string (decode_array (field t "pack"));
		tname = decode_string (field t "name");
		tparams = decode_opt_array decode_tparam (field t "params");
		tsub = opt decode_string (field t "sub");
	},Globals.null_pos

and decode_tparam v =
	match decode_enum v with
	| 0,[t] -> TPType (decode_ctype t)
	| 1,[e] -> TPExpr (decode_expr e)
	| _ -> raise Invalid_expr

and decode_tparams v =
	decode_opt_array decode_tparam_decl v

and decode_tparam_decl v =
	{
		tp_name = decode_placed_name (field v "name_pos") (field v "name");
		tp_constraints = decode_opt_array decode_ctype (field v "constraints");
		tp_params = decode_tparams (field v "params");
		tp_meta = decode_meta_content (field v "meta");
	}

and decode_fun v =
	{
		f_params = decode_tparams (field v "params");
		f_args = List.map (fun o ->
			decode_placed_name (field o "name_pos") (field o "name"),
			decode_opt_bool (field o "opt"),
			decode_meta_content (field o "meta"),
			opt decode_ctype (field o "type"),
			opt decode_expr (field o "value")
		) (decode_array (field v "args"));
		f_type = opt decode_ctype (field v "ret");
		f_expr = opt decode_expr (field v "expr");
	}

and decode_access v =
	match decode_enum v with
	| 0, [] -> APublic
	| 1, [] -> APrivate
	| 2, [] -> AStatic
	| 3, [] -> AOverride
	| 4, [] -> ADynamic
	| 5, [] -> AInline
	| 6, [] -> AMacro
	| 7, [] -> AFinal
	| _ -> raise Invalid_expr

and decode_meta_entry v =
	Meta.from_string (decode_string (field v "name")), decode_opt_array decode_expr (field v "params"), decode_pos (field v "pos")

and decode_meta_content m = decode_opt_array decode_meta_entry m

and decode_field v =
	let fkind = match decode_enum (field v "kind") with
		| 0, [t;e] ->
			FVar (opt decode_ctype t, opt decode_expr e)
		| 1, [f] ->
			FFun (decode_fun f)
		| 2, [get;set; t; e] ->
			FProp (decode_placed_name vnull get, decode_placed_name vnull set, opt decode_ctype t, opt decode_expr e)
		| _ ->
			raise Invalid_expr
	in
	{
		cff_name = decode_placed_name (field v "name_pos") (field v "name");
		cff_doc = opt decode_string (field v "doc");
		cff_pos = decode_pos (field v "pos");
		cff_kind = fkind;
		cff_access = List.map decode_access (opt_list decode_array (field v "access"));
		cff_meta = opt_list decode_meta_content (field v "meta");
	}

and decode_ctype t =
	let (i,args),p = decode_enum_with_pos t in
	(match i,args with
	| 0, [p] ->
		CTPath (fst (decode_path p))
	| 1, [a;r] ->
		CTFunction (List.map decode_ctype (decode_array a), decode_ctype r)
	| 2, [fl] ->
		CTAnonymous (List.map decode_field (decode_array fl))
	| 3, [t] ->
		CTParent (decode_ctype t)
	| 4, [tl;fl] ->
		CTExtend (List.map decode_path (decode_array tl), List.map decode_field (decode_array fl))
	| 5, [t] ->
		CTOptional (decode_ctype t)
	| 6, [n;t] ->
		CTNamed ((decode_string n,p), decode_ctype t)
	| _ ->
		raise Invalid_expr),p

and decode_expr v =
	let rec loop v =
		let p = decode_pos (field v "pos") in
		(decode (field v "expr") p, p)
	and decode e p =
		match decode_enum e with
		| 0, [c] ->
			EConst (decode_const c)
		| 1, [e1;e2] ->
			EArray (loop e1, loop e2)
		| 2, [op;e1;e2] ->
			EBinop (decode_op op, loop e1, loop e2)
		| 3, [e;f] ->
			EField (loop e, decode_string f)
		| 4, [e] ->
			EParenthesis (loop e)
		| 5, [a] ->
			EObjectDecl (List.map (fun o ->
				let name,p = decode_placed_name (field o "name_pos") (field o "field") in
				let vquotes = field o "quotes" in
				let quotes = if vquotes = vnull then NoQuotes
				else match decode_enum vquotes with
					| 0,[] -> NoQuotes
					| 1,[] -> DoubleQuotes
					| _ -> raise Invalid_expr
				in
				(name,p,quotes),loop (field o "expr")
			) (decode_array a))
		| 6, [a] ->
			EArrayDecl (List.map loop (decode_array a))
		| 7, [e;el] ->
			ECall (loop e,List.map loop (decode_array el))
		| 8, [t;el] ->
			ENew (decode_path t,List.map loop (decode_array el))
		| 9, [op;f;e] ->
			EUnop (decode_unop op,(if decode_bool f then Postfix else Prefix),loop e)
		| 10, [vl] ->
			EVars (List.map (fun v ->
				((decode_placed_name (field v "name_pos") (field v "name")),opt decode_ctype (field v "type"),opt loop (field v "expr"))
			) (decode_array vl))
		| 11, [fname;f] ->
			EFunction (opt decode_string fname,decode_fun f)
		| 12, [el] ->
			EBlock (List.map loop (decode_array el))
		| 13, [e1;e2] ->
			EFor (loop e1, loop e2)
		| 14, [e1;e2;e3] ->
			EIf (loop e1, loop e2, opt loop e3)
		| 15, [e1;e2;flag] ->
			EWhile (loop e1,loop e2,if decode_bool flag then NormalWhile else DoWhile)
		| 16, [e;cases;eo] ->
			let cases = List.map (fun c ->
				(List.map loop (decode_array (field c "values")),opt loop (field c "guard"),opt loop (field c "expr"),maybe_decode_pos (field c "pos"))
			) (decode_array cases) in
			ESwitch (loop e,cases,opt (fun v -> (if field v "expr" = vnull then None else Some (decode_expr v)),Globals.null_pos) eo)
		| 17, [e;catches] ->
			let catches = List.map (fun c ->
				((decode_placed_name (field c "name_pos") (field c "name")),(decode_ctype (field c "type")),loop (field c "expr"),maybe_decode_pos (field c "pos"))
			) (decode_array catches) in
			ETry (loop e, catches)
		| 18, [e] ->
			EReturn (opt loop e)
		| 19, [] ->
			EBreak
		| 20, [] ->
			EContinue
		| 21, [e] ->
			EUntyped (loop e)
		| 22, [e] ->
			EThrow (loop e)
		| 23, [e;t] ->
			ECast (loop e,opt decode_ctype t)
		| 24, [e;f] ->
			EDisplay (loop e,decode_bool f)
		| 25, [t] ->
			EDisplayNew (decode_path t)
		| 26, [e1;e2;e3] ->
			ETernary (loop e1,loop e2,loop e3)
		| 27, [e;t] ->
			ECheckType (loop e, (decode_ctype t))
		| 28, [m;e] ->
			EMeta (decode_meta_entry m,loop e)
		| 29, [e;f] ->
			EField (loop e, decode_string f) (*** deprecated EType, keep until haxe 3 **)
		| 30, [t] ->
			let (t,_) = decode_ctype t in
			EComplexType t
		| _ ->
			raise Invalid_expr
	in
	try
		loop v
	with Stack_overflow ->
		raise Invalid_expr

(* ---------------------------------------------------------------------- *)
(* TYPE ENCODING *)

let encode_pmap_array convert m =
	let l = ref [] in
	PMap.iter (fun _ v -> l := !l @ [(convert v)]) m;
	encode_array !l

let encode_and_map_array convert l =
	encode_array (List.map convert l)

let vopt f v = match v with
	| None -> vnull
	| Some v -> f v

let encode_meta m set =
	let meta = ref m in
	encode_obj OMetaAccess [
		"get", vfun0 (fun() ->
			encode_meta_content (!meta)
		);
		"add", vfun3 (fun k vl p ->
			(try
				let el = List.map decode_expr (decode_array vl) in
				meta := (Meta.from_string (decode_string k), el, decode_pos p) :: !meta;
				set (!meta)
			with Invalid_expr ->
				failwith "Invalid expression");
			vnull
		);
		"extract", vfun1 (fun k ->
			let k = Meta.from_string (decode_string k) in
			encode_and_map_array encode_meta_entry (List.filter (fun (m,_,_) -> m = k) (!meta))
		);
		"remove", vfun1 (fun k ->
			let k = Meta.from_string (decode_string k) in
			meta := List.filter (fun (m,_,_) -> m <> k) (!meta);
			set (!meta);
			vnull
		);
		"has", vfun1 (fun k ->
			let k = Meta.from_string (decode_string k) in
			vbool (List.exists (fun (m,_,_) -> m = k) (!meta));
		);
	]

let rec encode_mtype ot t fields =
	let i = t_infos t in
	encode_obj ot ([
		"__t", 	encode_tdecl t;
		"pack", encode_array (List.map encode_string (fst i.mt_path));
		"name", encode_string (snd i.mt_path);
		"pos", encode_pos i.mt_pos;
		"module", encode_string (s_type_path i.mt_module.m_path);
		"isPrivate", vbool i.mt_private;
		"meta", encode_meta i.mt_meta (fun m -> i.mt_meta <- m);
		"doc", null encode_string i.mt_doc;
		"params", encode_type_params i.mt_params;
	] @ fields)

and encode_type_params tl =
	encode_array (List.map (fun (n,t) -> encode_obj OTypeParameter ["name",encode_string n;"t",encode_type t]) tl)

and encode_tenum e =
	encode_mtype OEnumType (TEnumDecl e) [
		"isExtern", vbool e.e_extern;
		"exclude", vfun0 (fun() -> e.e_extern <- true; vnull);
		"constructs", encode_string_map encode_efield e.e_constrs;
		"names", encode_array (List.map encode_string e.e_names);
	]

and encode_tabstract a =
	encode_mtype OAbstractType (TAbstractDecl a) [
		"type", encode_type a.a_this;
		"impl", (match a.a_impl with None -> vnull | Some c -> encode_clref c);
		"binops", encode_array (List.map (fun (op,cf) -> encode_obj OAbstractType_binops [ "op",encode_binop op; "field",encode_cfield cf]) a.a_ops);
		"unops", encode_array (List.map (fun (op,postfix,cf) -> encode_obj OAbstractType_unops [ "op",encode_unop op; "isPostfix",vbool (match postfix with Postfix -> true | Prefix -> false); "field",encode_cfield cf]) a.a_unops);
		"from", encode_array ((List.map (fun t -> encode_obj OAbstractType_from [ "t",encode_type t; "field",vnull]) a.a_from) @ (List.map (fun (t,cf) -> encode_obj OAbstractType_from [ "t",encode_type t; "field",encode_cfield cf]) a.a_from_field));
		"to", encode_array ((List.map (fun t -> encode_obj OAbstractType_to [ "t",encode_type t; "field",vnull]) a.a_to) @ (List.map (fun (t,cf) -> encode_obj OAbstractType_to [ "t",encode_type t; "field",encode_cfield cf]) a.a_to_field));
		"array", encode_array (List.map encode_cfield a.a_array);
		"resolve", (match a.a_resolve with None -> vnull | Some cf -> encode_cfref cf)
	]

and encode_efield f =
	encode_obj OEnumField [
		"name", encode_string f.ef_name;
		"type", encode_type f.ef_type;
		"pos", encode_pos f.ef_pos;
		"namePos", encode_pos f.ef_name_pos;
		"index", vint f.ef_index;
		"meta", encode_meta f.ef_meta (fun m -> f.ef_meta <- m);
		"doc", null encode_string f.ef_doc;
		"params", encode_type_params f.ef_params;
	]

and encode_cfield f =
	encode_obj OClassField [
		"name", encode_string f.cf_name;
		"type", encode_lazy_type f.cf_type;
		"isPublic", vbool f.cf_public;
		"params", encode_type_params f.cf_params;
		"meta", encode_meta f.cf_meta (fun m -> f.cf_meta <- m);
		"expr", vfun0 (fun() ->
			ignore (flush_core_context (fun() -> follow f.cf_type));
			(match f.cf_expr with None -> vnull | Some e -> encode_texpr e)
		);
		"kind", encode_field_kind f.cf_kind;
		"pos", encode_pos f.cf_pos;
		"namePos",encode_pos f.cf_name_pos;
		"doc", null encode_string f.cf_doc;
		"overloads", encode_ref f.cf_overloads (encode_and_map_array encode_cfield) (fun() -> "overloads");
	]

and encode_field_kind k =
	let tag, pl = (match k with
		| Type.Var v -> 0, [encode_var_access v.v_read; encode_var_access v.v_write]
		| Method m -> 1, [encode_method_kind m]
	) in
	encode_enum IFieldKind tag pl

and encode_var_access a =
	let tag, pl = (match a with
		| AccNormal -> 0, []
		| AccNo -> 1, []
		| AccNever -> 2, []
		| AccResolve -> 3, []
		| AccCall -> 4, []
		| AccInline	-> 5, []
		| AccRequire (s,msg) -> 6, [encode_string s; null encode_string msg]
		| AccCtor -> 7, []
	) in
	encode_enum IVarAccess tag pl

and encode_method_kind m =
	let tag, pl = (match m with
		| MethNormal -> 0, []
		| MethInline -> 1, []
		| MethDynamic -> 2, []
		| MethMacro -> 3, []
	) in
	encode_enum IMethodKind tag pl

and encode_class_kind k =
	let tag, pl = (match k with
		| KNormal -> 0, []
		| KTypeParameter pl -> 1, [encode_tparams pl]
		(* KExtension was here *)
		| KExpr e -> 3, [encode_expr e]
		| KGeneric -> 4, []
		| KGenericInstance (cl, params) -> 5, [encode_clref cl; encode_tparams params]
		| KMacroType -> 6, []
		| KAbstractImpl a -> 7, [encode_abref a]
		| KGenericBuild cfl -> 8, []
	) in
	encode_enum IClassKind tag pl

and encode_tclass c =
	ignore(c.cl_build());
	encode_mtype OClassType (TClassDecl c) [
		"kind", encode_class_kind c.cl_kind;
		"isExtern", vbool c.cl_extern;
		"exclude", vfun0 (fun() -> c.cl_extern <- true; c.cl_init <- None; vnull);
		"isInterface", vbool c.cl_interface;
		"superClass", (match c.cl_super with
			| None -> vnull
			| Some (c,pl) -> encode_obj OClassType_superClass ["t",encode_clref c;"params",encode_tparams pl]
		);
		"interfaces", encode_array (List.map (fun (c,pl) -> encode_obj OClassType_interfaces ["t",encode_clref c;"params",encode_tparams pl]) c.cl_implements);
		"fields", encode_ref c.cl_ordered_fields (encode_and_map_array encode_cfield) (fun() -> "class fields");
		"statics", encode_ref c.cl_ordered_statics (encode_and_map_array encode_cfield) (fun() -> "class fields");
		"constructor", (match c.cl_constructor with None -> vnull | Some cf -> encode_cfref cf);
		"init", (match c.cl_init with None -> vnull | Some e -> encode_texpr e);
		"overrides", (encode_array (List.map encode_cfref c.cl_overrides))
	]

and encode_ttype t =
	encode_mtype ODefType (TTypeDecl t) [
		"isExtern", vbool false;
		"exclude", vfun0 (fun() -> vnull);
		"type", encode_type t.t_type;
	]

and encode_tanon a =
	encode_obj OAnonType [
		"fields", encode_pmap_array encode_cfield a.a_fields;
		"status", encode_anon_status !(a.a_status);
	]

and encode_anon_status s =
	let tag, pl = (match s with
		| Closed -> 0, []
		| Opened -> 1, []
		| Type.Const -> 2, []
		| Extend tl -> 3, [encode_ref tl (fun tl -> encode_array (List.map encode_type tl)) (fun() -> "<extended types>")]
		| Statics cl -> 4, [encode_clref cl]
		| EnumStatics en -> 5, [encode_enref en]
		| AbstractStatics ab -> 6, [encode_abref ab]
	)
	in
	encode_enum IAnonStatus tag pl

and encode_tparams pl =
	encode_array (List.map encode_type pl)

and encode_clref c =
	encode_ref c encode_tclass (fun() -> s_type_path c.cl_path)

and encode_enref en =
	encode_ref en encode_tenum (fun() -> s_type_path en.e_path)

and encode_cfref cf =
	encode_ref cf encode_cfield (fun() -> cf.cf_name)

and encode_abref ab =
	encode_ref ab encode_tabstract (fun() -> s_type_path ab.a_path)

and encode_type t =
	let rec loop = function
		| TMono r ->
			(match !r with
			| None -> 0, [encode_ref r (fun r -> match !r with None -> vnull | Some t -> encode_type t) (fun() -> "<mono>")]
			| Some t -> loop t)
		| TEnum (e, pl) ->
			1 , [encode_ref e encode_tenum (fun() -> s_type_path e.e_path); encode_tparams pl]
		| TInst (c, pl) ->
			2 , [encode_clref c; encode_tparams pl]
		| TType (t,pl) ->
			3 , [encode_ref t encode_ttype (fun() -> s_type_path t.t_path); encode_tparams pl]
		| TFun (pl,ret) ->
			let pl = List.map (fun (n,o,t) ->
				encode_obj OType_args [
					"name",encode_string n;
					"opt",vbool o;
					"t",encode_type t
				]
			) pl in
			4 , [encode_array pl; encode_type ret]
		| TAnon a ->
			5, [encode_ref a encode_tanon (fun() -> "<anonymous>")]
		| TDynamic tsub as t ->
			if t == t_dynamic then
				6, [vnull]
			else
				6, [encode_type tsub]
		| TLazy f ->
			loop (lazy_type f)
		| TAbstract (a, pl) ->
			8, [encode_abref a; encode_tparams pl]
	in
	let tag, pl = loop t in
	encode_enum IType tag pl

and encode_lazy_type t =
	let rec loop = function
		| TMono r ->
			(match !r with
			| Some t -> loop t
			| _ -> encode_type t)
		| TLazy f ->
			(match !f with
			| LAvailable t ->
				encode_type t
			| _ ->
				encode_enum IType 7 [encode_lazytype f (fun() ->
					(match !f with
					| LAvailable t ->
						encode_type t
					| LWait _ ->
						(* we are doing some typing here, let's flush our context if it's not already *)
						encode_type (flush_core_context (fun() -> lazy_type f))
					| LProcessing _ ->
						(* our type in on the processing stack, error instead of returning most likely an unbound mono *)
						error_message "Accessing a type while it's being typed");
				)])
		| _ ->
			encode_type t
	in
	loop t

and decode_type t =
	match decode_enum t with
	| 0, [r] -> TMono (decode_ref r)
	| 1, [e; pl] -> TEnum (decode_ref e, List.map decode_type (decode_array pl))
	| 2, [c; pl] -> TInst (decode_ref c, List.map decode_type (decode_array pl))
	| 3, [t; pl] -> TType (decode_ref t, List.map decode_type (decode_array pl))
	| 4, [pl; r] -> TFun (List.map (fun p -> decode_string (field p "name"), decode_bool (field p "opt"), decode_type (field p "t")) (decode_array pl), decode_type r)
	| 5, [a] -> TAnon (decode_ref a)
	| 6, [t] -> if t = vnull then t_dynamic else TDynamic (decode_type t)
	| 7, [f] -> TLazy (decode_lazytype f)
	| 8, [a; pl] -> TAbstract (decode_ref a, List.map decode_type (decode_array pl))
	| _ -> raise Invalid_expr

and decode_type_decl t =
	decode_tdecl (field t "__t")

(* ---------------------------------------------------------------------- *)
(* TEXPR Encoding *)

and encode_tconst c =
	let tag, pl = match c with
		| TInt i -> 0,[vint32 i]
		| TFloat f -> 1,[encode_string f]
		| TString s -> 2,[encode_string s]
		| TBool b -> 3,[vbool b]
		| TNull -> 4,[]
		| TThis -> 5,[]
		| TSuper -> 6,[]
	in
	encode_enum ITConstant tag pl

and encode_tvar v =
	let f_extra (pl,e) =
		encode_obj OTVar_extra [
			"params",encode_type_params pl;
			"expr",vopt encode_texpr e
		]
	in
	encode_obj OTVar [
		"id", vint v.v_id;
		"name", encode_string v.v_name;
		"t", encode_type v.v_type;
		"capture", vbool v.v_capture;
		"extra", vopt f_extra v.v_extra;
		"meta", encode_meta v.v_meta (fun m -> v.v_meta <- m);
		"$", encode_unsafe (Obj.repr v);
	]

and encode_module_type mt =
	let tag,pl = match mt with
		| TClassDecl c -> 0,[encode_clref c]
		| TEnumDecl e -> 1,[encode_enref e]
		| TTypeDecl t -> 2,[encode_ref t encode_ttype (fun () -> s_type_path t.t_path)]
		| TAbstractDecl a -> 3,[encode_abref a]
	in
	encode_enum IModuleType tag pl

and encode_tfunc func =
	encode_obj OTFunc [
		"args",encode_array (List.map (fun (v,c) ->
			encode_obj OTFunc_args [
				"v",encode_tvar v;
				"value",match c with None -> vnull | Some c -> encode_tconst c
			]
		) func.tf_args);
		"t",encode_type func.tf_type;
		"expr",encode_texpr func.tf_expr
	]

and encode_field_access fa =
	let encode_instance c tl =
		encode_obj OFieldAccess_c [
			"c",encode_clref c;
			"params",encode_tparams tl
		]
	in
	let tag,pl = match fa with
		| FInstance(c,tl,cf) -> 0,[encode_clref c;encode_tparams tl;encode_cfref cf]
		| FStatic(c,cf) -> 1,[encode_clref c;encode_cfref cf]
		| FAnon(cf) -> 2,[encode_cfref cf]
		| FDynamic(s) -> 3,[encode_string s]
		| FClosure(co,cf) -> 4,[(match co with Some (c,tl) -> encode_instance c tl | None -> vnull);encode_cfref cf]
		| FEnum(en,ef) -> 5,[encode_enref en;encode_efield ef]
	in
	encode_enum IFieldAccess tag pl

and encode_texpr e =
	let rec loop e =
		let tag, pl = match e.eexpr with
			| TConst c -> 0,[encode_tconst c]
			| TLocal v -> 1,[encode_tvar v]
			| TArray(e1,e2) -> 2,[loop e1; loop e2]
			| TBinop(op,e1,e2) -> 3,[encode_binop op;loop e1;loop e2]
			| TField(e1,fa) -> 4,[loop e1;encode_field_access fa]
			| TTypeExpr mt -> 5,[encode_module_type mt]
			| TParenthesis e1 -> 6,[loop e1]
			| TObjectDecl fl -> 7, [encode_array (List.map (fun ((f,_,_),e) ->
				encode_obj OTypedExprDef_fields [
					"name",encode_string f;
					"expr",loop e;
				]) fl)]
			| TArrayDecl el -> 8,[encode_texpr_list el]
			| TCall(e1,el) -> 9,[loop e1;encode_texpr_list el]
			| TNew(c,pl,el) -> 10,[encode_clref c;encode_tparams pl;encode_texpr_list el]
			| TUnop(op,flag,e1) -> 11,[encode_unop op;vbool (flag = Postfix);loop e1]
			| TFunction func -> 12,[encode_tfunc func]
			| TVar (v,eo) -> 13,[encode_tvar v;vopt encode_texpr eo]
			| TBlock el -> 14,[encode_texpr_list el]
			| TFor(v,e1,e2) -> 15,[encode_tvar v;loop e1;loop e2]
			| TIf(eif,ethen,eelse) -> 16,[loop eif;loop ethen;vopt encode_texpr eelse]
			| TWhile(econd,e1,flag) -> 17,[loop econd;loop e1;vbool (flag = NormalWhile)]
			| TSwitch(e1,cases,edef) -> 18,[
				loop e1;
				encode_array (List.map (fun (el,e) -> encode_obj OTypedExprDef_cases ["values",encode_texpr_list el;"expr",loop e]) cases);
				vopt encode_texpr edef
				]
			| TTry(e1,catches) -> 19,[
				loop e1;
				encode_array (List.map (fun (v,e) ->
					encode_obj OTypedExprDef_catches [
						"v",encode_tvar v;
						"expr",loop e
					]) catches
				)]
			| TReturn e1 -> 20,[vopt encode_texpr e1]
			| TBreak -> 21,[]
			| TContinue -> 22,[]
			| TThrow e1 -> 23,[loop e1]
			| TCast(e1,mt) -> 24,[loop e1;match mt with None -> vnull | Some mt -> encode_module_type mt]
			| TMeta(m,e1) -> 25,[encode_meta_entry m;loop e1]
			| TEnumParameter(e1,ef,i) -> 26,[loop e1;encode_efield ef;vint i]
			| TEnumIndex e1 -> 27,[loop e1]
			| TIdent s -> 28,[encode_string s]
		in
		encode_obj OTypedExprDef [
			"pos", encode_pos e.epos;
			"expr", encode_enum ITypedExpr tag pl;
			"t", encode_type e.etype
		]
	in
	loop e

and encode_texpr_list el =
	encode_array (List.map encode_texpr el)

(* ---------------------------------------------------------------------- *)
(* TEXPR Decoding *)

let decode_tconst c =
	match decode_enum c with
	| 0, [s] -> TInt (decode_i32 s)
	| 1, [s] -> TFloat (decode_string s)
	| 2, [s] -> TString (decode_string s)
	| 3, [s] -> TBool (decode_bool s)
	| 4, [] -> TNull
	| 5, [] -> TThis
	| 6, [] -> TSuper
	| _ -> raise Invalid_expr

let decode_type_params v =
	List.map (fun v -> decode_string (field v "name"),decode_type (field v "t")) (decode_array v)

let decode_tvar v =
	(Obj.obj (decode_unsafe (field v "$")) : tvar)

let decode_var_access v =
	match decode_enum v with
	| 0, [] -> AccNormal
	| 1, [] -> AccNo
	| 2, [] -> AccNever
	| 3, [] -> AccResolve
	| 4, [] -> AccCall
	| 5, [] -> AccInline
	| 6, [s1;s2] -> AccRequire(decode_string s1, opt decode_string s2)
	| 7, [] -> AccCtor
	| _ -> raise Invalid_expr

let decode_method_kind v =
	match decode_enum v with
	| 0, [] -> MethNormal
	| 1, [] -> MethInline
	| 2, [] -> MethDynamic
	| 3, [] -> MethMacro
	| _ -> raise Invalid_expr

let decode_field_kind v =
	match decode_enum v with
	| 0, [vr;vw] -> Type.Var({v_read = decode_var_access vr; v_write = decode_var_access vw})
	| 1, [m] -> Method (decode_method_kind m)
	| _ -> raise Invalid_expr

let decode_cfield v =
	{
		cf_name = decode_string (field v "name");
		cf_type = decode_type (field v "type");
		cf_public = decode_bool (field v "isPublic");
		cf_pos = decode_pos (field v "pos");
		cf_name_pos = decode_pos (field v "namePos");
		cf_doc = opt decode_string (field v "doc");
		cf_meta = []; (* TODO *)
		cf_kind = decode_field_kind (field v "kind");
		cf_params = decode_type_params (field v "params");
		cf_expr = None;
		cf_expr_unoptimized = None;
		cf_overloads = decode_ref (field v "overloads");
	}

let decode_efield v =
	{
		ef_name = decode_string (field v "name");
		ef_type = decode_type (field v "type");
		ef_pos = decode_pos (field v "pos");
		ef_name_pos = decode_pos (field v "namePos");
		ef_index = decode_int (field v "index");
		ef_meta = []; (* TODO *)
		ef_doc = opt decode_string (field v "doc");
		ef_params = decode_type_params (field v "params")
	}

let decode_field_access v =
	match decode_enum v with
	| 0, [c;tl;cf] ->
		let c = decode_ref c in
		FInstance(c,List.map decode_type (decode_array tl),decode_ref cf)
	| 1, [c;cf] -> FStatic(decode_ref c,decode_ref cf)
	| 2, [cf] -> FAnon(decode_ref cf)
	| 3, [s] -> FDynamic(decode_string s)
	| 4, [co;cf] ->
		let co = if co = vnull then None else Some (decode_ref (field co "c"),List.map decode_type (decode_array (field co "params"))) in
		FClosure(co,decode_ref cf)
	| 5, [e;ef] -> FEnum(decode_ref e,decode_efield ef)
	| _ -> raise Invalid_expr

let decode_module_type v =
	match decode_enum v with
	| 0, [c] -> TClassDecl (decode_ref c)
	| 1, [en] -> TEnumDecl (decode_ref en)
	| 2, [t] -> TTypeDecl (decode_ref t)
	| 3, [a] -> TAbstractDecl (decode_ref a)
	| _ -> raise Invalid_expr

let rec decode_tfunc v =
	{
		tf_args = List.map (fun v -> decode_tvar (field v "v"),opt decode_tconst (field v "value")) (decode_array (field v "args"));
		tf_type = decode_type (field v "t");
		tf_expr = decode_texpr (field v "expr")
	}

and decode_texpr v =
	let rec loop v =
		mk (decode (field v "expr")) (decode_type (field v "t")) (decode_pos (field v "pos"))
	and decode e =
		match decode_enum e with
		| 0, [c] ->	TConst(decode_tconst c)
		| 1, [v] -> TLocal(decode_tvar v)
		| 2, [v1;v2] -> TArray(loop v1,loop v2)
		| 3, [op;v1;v2] -> TBinop(decode_op op,loop v1,loop v2)
		| 4, [v1;fa] -> TField(loop v1,decode_field_access fa)
		| 5, [mt] -> TTypeExpr(decode_module_type mt)
		| 6, [v1] -> TParenthesis(loop v1)
		| 7, [v] -> TObjectDecl(List.map (fun v -> (decode_string (field v "name"),Globals.null_pos,NoQuotes),loop (field v "expr")) (decode_array v))
		| 8, [vl] -> TArrayDecl(List.map loop (decode_array vl))
		| 9, [v1;vl] -> TCall(loop v1,List.map loop (decode_array vl))
		| 10, [c;tl;vl] -> TNew(decode_ref c,List.map decode_type (decode_array tl),List.map loop (decode_array vl))
		| 11, [op;pf;v1] -> TUnop(decode_unop op,(if decode_bool pf then Postfix else Prefix),loop v1)
		| 12, [f] -> TFunction(decode_tfunc f)
		| 13, [v;eo] -> TVar(decode_tvar v,opt loop eo)
		| 14, [vl] -> TBlock(List.map loop (decode_array vl))
		| 15, [v;v1;v2] -> TFor(decode_tvar v,loop v1,loop v2)
		| 16, [vif;vthen;velse] -> TIf(loop vif,loop vthen,opt loop velse)
		| 17, [vcond;v1;b] -> TWhile(loop vcond,loop v1,if decode_bool b then NormalWhile else DoWhile)
		| 18, [v1;cl;vdef] -> TSwitch(loop v1,List.map (fun v -> List.map loop (decode_array (field v "values")),loop (field v "expr")) (decode_array cl),opt loop vdef)
		| 19, [v1;cl] -> TTry(loop v1,List.map (fun v -> decode_tvar (field v "v"),loop (field v "expr")) (decode_array cl))
		| 20, [vo] -> TReturn(opt loop vo)
		| 21, [] -> TBreak
		| 22, [] -> TContinue
		| 23, [v1] -> TThrow(loop v1)
		| 24, [v1;mto] -> TCast(loop v1,opt decode_module_type mto)
		| 25, [m;v1] -> TMeta(decode_meta_entry m,loop v1)
		| 26, [v1;ef;i] -> TEnumParameter(loop v1,decode_efield ef,decode_int i)
		| 27, [v1] -> TEnumIndex(loop v1)
		| 28, [v1] -> TIdent(decode_string v1)
		| i,el -> Printf.printf "%i %i\n" i (List.length el); raise Invalid_expr
	in
	try
		loop v
	with Stack_overflow ->
		raise Invalid_expr

(* ---------------------------------------------------------------------- *)
(* TYPE DEFINITION *)

let decode_type_def v =
	let pack = List.map decode_string (decode_array (field v "pack")) in
	let name = decode_placed_name (field v "name_pos") (field v "name") in
	let meta = decode_meta_content (field v "meta") in
	let pos = decode_pos (field v "pos") in
	let isExtern = decode_opt_bool (field v "isExtern") in
	let fields = List.map decode_field (decode_array (field v "fields")) in
	let doc = opt decode_string (field v "doc") in
	let mk fl dl =
		{
			d_name = name;
			d_doc = doc;
			d_params = decode_tparams (field v "params");
			d_meta = meta;
			d_flags = fl;
			d_data = dl;
		}
	in
	let tdef = (match decode_enum (field v "kind") with
	| 0, [] ->
		let conv f =
			let loop ((n,_),opt,_,t,_) =
				match t with
				| None -> raise Invalid_expr
				| Some t -> n, opt, t
			in
			let args, params, t = (match f.cff_kind with
				| FVar (t,None) -> [], [], t
				| FFun f -> List.map loop f.f_args, f.f_params, f.f_type
				| _ -> raise Invalid_expr
			) in
			{
				ec_name = f.cff_name;
				ec_doc = f.cff_doc;
				ec_meta = f.cff_meta;
				ec_pos = f.cff_pos;
				ec_args = args;
				ec_params = params;
				ec_type = t;
			}
		in
		EEnum (mk (if isExtern then [EExtern] else []) (List.map conv fields))
	| 1, [] ->
		ETypedef (mk (if isExtern then [EExtern] else []) (CTAnonymous fields,Globals.null_pos))
	| 2, [ext;impl;interf] ->
		let flags = if isExtern then [HExtern] else [] in
		let is_interface = decode_opt_bool interf in
		let interfaces = (match opt (fun v -> List.map decode_path (decode_array v)) impl with Some l -> l | _ -> [] ) in
		let flags = (match opt decode_path ext with None -> flags | Some t -> HExtends t :: flags) in
		let flags = if is_interface then begin
				let flags = HInterface :: flags in
				List.map (fun t -> HExtends t) interfaces @ flags
			end else begin
				List.map (fun t -> HImplements t) interfaces @ flags
			end
		in
		EClass (mk flags fields)
	| 3, [t] ->
		ETypedef (mk (if isExtern then [EExtern] else []) (decode_ctype t))
	| 4, [tthis;tfrom;tto] ->
		let flags = match opt decode_array tfrom with None -> [] | Some ta -> List.map (fun t -> AFromType (decode_ctype t)) ta in
		let flags = match opt decode_array tto with None -> flags | Some ta -> (List.map (fun t -> AToType (decode_ctype t)) ta) @ flags in
		let flags = match opt decode_ctype tthis with None -> flags | Some t -> (AIsType t) :: flags in
		EAbstract(mk flags fields)
	| _ ->
		raise Invalid_expr
	) in
	(* if our package ends with an uppercase letter, then it's the module name *)
	let pack,name = (match List.rev pack with
		| last :: l when not (is_lower_ident last) -> List.rev l, last
		| _ -> pack, fst name
	) in
	(pack, name), tdef, pos

(* ---------------------------------------------------------------------- *)
(* VALUE-TO-CONSTANT *)

let rec make_const e =
	match e.eexpr with
	| TConst c ->
		(match c with
		| TInt i -> vint32 i
		| TFloat s -> vfloat (float_of_string s)
		| TString s -> encode_string s
		| TBool b -> vbool b
		| TNull -> vnull
		| TThis | TSuper -> raise Exit)
	| TParenthesis e | TMeta(_,e) | TCast(e,None) ->
		make_const e
	| TObjectDecl el ->
		encode_obj O__Const (List.map (fun ((f,_,_),e) -> f, make_const e) el)
	| TArrayDecl al ->
		encode_array (List.map make_const al)
	| _ ->
		raise Exit


(* ------------------------------------------------------------------------- *)
(* MACRO API *)

(**

	Our macro API implementation. It gets called by the interpreter and
	accesses the compiler with the compiler_api

**)

let macro_api ccom get_api =
	[
		"current_pos", vfun0 (fun() ->
			encode_pos (get_api()).pos
		);
		"error", vfun2 (fun msg p ->
			let msg = decode_string msg in
			let p = decode_pos p in
			(ccom()).error msg p;
			raise Abort
		);
		"fatal_error", vfun2 (fun msg p ->
			let msg = decode_string msg in
			let p = decode_pos p in
			raise (Error.Fatal_error (msg,p))
		);
		"warning", vfun2 (fun msg p ->
			let msg = decode_string msg in
			let p = decode_pos p in
			(ccom()).warning msg p;
			vnull
		);
		"class_path", vfun0 (fun() ->
			encode_array (List.map encode_string (ccom()).class_path);
		);
		"resolve_path", vfun1 (fun file ->
			let file = decode_string file in
			encode_string (try Common.find_file (ccom()) file with Not_found -> failwith ("File not found '" ^ file ^ "'"))
		);
		"define", vfun2 (fun s v ->
			let v = if v = vnull then "" else "=" ^ (decode_string v) in
			Common.raw_define (ccom()) ((decode_string s) ^ v);
			vnull
		);
		"defined", vfun1 (fun s ->
			vbool (Common.raw_defined (ccom()) (decode_string s))
		);
		"defined_value", vfun1 (fun s ->
			try encode_string (Common.raw_defined_value (ccom()) (decode_string s)) with Not_found -> vnull
		);
		"get_defines", vfun0 (fun() ->
			encode_string_map encode_string (ccom()).defines.Define.values
		);
		"get_type", vfun1 (fun s ->
			let tname = decode_string s in
			match (get_api()).get_type tname with
			| None -> failwith ("Type not found '" ^ tname ^ "'")
			| Some t -> encode_type t
		);
		"get_module", vfun1 (fun s ->
			encode_array (List.map encode_type ((get_api()).get_module (decode_string s)))
		);
		"on_after_typing", vfun1 (fun f ->
			let f = prepare_callback f 1 in
			(get_api()).after_typing (fun tl -> ignore(f [encode_array (List.map encode_module_type tl)]));
			vnull
		);
		"on_generate", vfun1 (fun f ->
			let f = prepare_callback f 1 in
			(get_api()).on_generate (fun tl -> ignore(f [encode_array (List.map encode_type tl)]));
			vnull
		);
		"on_after_generate", vfun1 (fun f ->
			let f = prepare_callback f 0 in
			(get_api()).after_generate (fun () -> ignore(f []));
			vnull
		);
		"on_type_not_found", vfun1 (fun f ->
			let f = prepare_callback f 1 in
			(get_api()).on_type_not_found (fun path -> f [encode_string path]);
			vnull
		);
		"do_parse", vfun3 (fun s p b ->
			let s = decode_string s in
			if s = "" then raise Invalid_expr;
			encode_expr ((get_api()).parse_string s (decode_pos p) (decode_bool b))
		);
		"make_expr", vfun2 (fun v p ->
			encode_expr (value_to_expr v (decode_pos p))
		);
		"signature", vfun1 (fun v ->
			encode_string (Digest.to_hex (value_signature v))
		);
		"to_complex_type", vfun1 (fun v ->
			try	encode_ctype (TExprToExpr.convert_type' (decode_type v))
			with Exit -> vnull
		);
		"unify", vfun2 (fun t1 t2 ->
			let e1 = mk (TObjectDecl []) (decode_type t1) Globals.null_pos in
			vbool (((get_api()).cast_or_unify) (decode_type t2) e1 Globals.null_pos)
		);
		"typeof", vfun1 (fun v ->
			encode_type ((get_api()).type_expr (decode_expr v)).etype
		);
		"type_expr", vfun1 (fun v ->
			encode_texpr ((get_api()).type_expr (decode_expr v))
		);
		"resolve_type", vfun2 (fun t p ->
			encode_type ((get_api()).resolve_type (fst (decode_ctype t)) (decode_pos p));
		);
		"s_type", vfun1 (fun v ->
			encode_string (Type.s_type (print_context()) (decode_type v))
		);
		"s_expr", vfun2 (fun v b ->
			let f = if decode_opt_bool b then Type.s_expr_pretty false "" false else Type.s_expr_ast true "" in
			encode_string (f (Type.s_type (print_context())) (decode_texpr v))
		);
		"is_fmt_string", vfun1 (fun p ->
			vbool (Lexer.is_fmt_string (decode_pos p))
		);
		"format_string", vfun2 (fun s p ->
			encode_expr ((get_api()).format_string (decode_string s) (decode_pos p))
		);
		"allow_package", vfun1 (fun s ->
			(get_api()).allow_package (decode_string s);
			vnull
		);
		"type_patch", vfun4 (fun t f s v ->
			(get_api()).type_patch (decode_string t) (decode_string f) (decode_bool s) (opt decode_string v);
			vnull
		);
		"meta_patch", vfun4 (fun m t f s ->
			(get_api()).meta_patch (decode_string m) (decode_string t) (opt decode_string f) (decode_bool s);
			vnull
		);
		"add_global_metadata_impl", vfun5 (fun s1 s2 b1 b2 b3 ->
			(get_api()).add_global_metadata (decode_string s1) (decode_string s2) (decode_bool b1,decode_bool b2,decode_bool b3);
			vnull
		);
		"set_custom_js_generator", vfun1 (fun f ->
			let f = prepare_callback f 1 in
			(get_api()).set_js_generator (fun js_ctx ->
				let com = ccom() in
				let api = encode_obj OJSGenApi [
					"outputFile", encode_string com.file;
					"types", encode_array (List.map (fun t -> encode_type (type_of_module_type t)) com.types);
					"main", (match com.main with None -> vnull | Some e -> encode_texpr e);
					"generateValue", vfun1 (fun v ->
						let e = decode_texpr v in
						let str = Genjs.gen_single_expr js_ctx e false in
						encode_string str
					);
					"isKeyword", vfun1 (fun v ->
						vbool (Hashtbl.mem Genjs.kwds (decode_string v))
					);
					"hasFeature", vfun1 (fun v ->
						vbool (Common.has_feature com (decode_string v))
					);
					"addFeature", vfun1 (fun v ->
						Common.add_feature com (decode_string v);
						vnull
					);
					"quoteString", vfun1 (fun v ->
						encode_string ("\"" ^ Ast.s_escape (decode_string v) ^ "\"")
					);
					"buildMetaData", vfun1 (fun t ->
						match Texpr.build_metadata com.basic (decode_type_decl t) with
						| None -> vnull
						| Some e -> encode_texpr e
					);
					"generateStatement", vfun1 (fun v ->
						let e = decode_texpr v in
						let str = Genjs.gen_single_expr js_ctx e true in
						encode_string str
					);
					"setTypeAccessor", vfun1 (fun callb ->
						let callb = prepare_callback callb 1 in
						js_ctx.Genjs.type_accessor <- (fun t ->
							decode_string (callb [encode_type (type_of_module_type t)])
						);
						vnull
					);
					"setCurrentClass", vfun1 (fun c ->
						Genjs.set_current_class js_ctx (match decode_type_decl c with TClassDecl c -> c | _ -> assert false);
						vnull
					);
				] in
				ignore(f [api]);
			);
			vnull
		);
		"get_pos_infos", vfun1 (fun p ->
			let p = decode_pos p in
			encode_obj OContext_getPosInfos ["min",vint p.Globals.pmin;"max",vint p.Globals.pmax;"file",encode_string p.Globals.pfile]
		);
		"make_position", vfun3 (fun min max file ->
			encode_pos { Globals.pmin = decode_int min; Globals.pmax = decode_int max; Globals.pfile = decode_string file }
		);
		"add_resource", vfun2 (fun name data ->
			let name = decode_string name in
			let data = decode_bytes data in
			let data = Bytes.unsafe_to_string data in
			if name = "" then failwith "Empty resource name";
			Hashtbl.replace (ccom()).resources name data;
			let m = if name.[0] = '$' then (get_api()).current_macro_module() else (get_api()).current_module() in
			m.m_extra.m_binded_res <- PMap.add name data m.m_extra.m_binded_res;
			vnull
		);
		"get_resources", vfun0 (fun() ->
			encode_string_map encode_string (Hashtbl.fold (fun k v acc -> PMap.add k v acc) (ccom()).resources PMap.empty)
		);
		"get_local_module", vfun0 (fun() ->
			let m = (get_api()).current_module() in
			encode_string (s_type_path m.m_path);
		);
		"get_local_type", vfun0 (fun() ->
			match (get_api()).get_local_type() with
			| None -> vnull
			| Some t -> encode_type t
		);
		"get_expected_type", vfun0 (fun() ->
			match (get_api()).get_expected_type() with
			| None -> vnull
			| Some t -> encode_type t
		);
		"get_call_arguments", vfun0 (fun() ->
			match (get_api()).get_call_arguments() with
			| None -> vnull
			| Some el -> encode_array (List.map encode_expr el)
		);
		"get_local_method", vfun0 (fun() ->
			encode_string ((get_api()).get_local_method())
		);
		"get_local_using", vfun0 (fun() ->
			encode_array (List.map encode_clref ((get_api()).get_local_using()))
		);
		"get_local_imports", vfun0 (fun() ->
			encode_array (List.map encode_import ((get_api()).get_local_imports()))
		);
		"local_vars", vfun1 (fun as_var ->
			let as_var = decode_opt_bool as_var in
			let vars = (get_api()).get_local_vars() in
			encode_string_map (if as_var then encode_tvar else (fun v -> encode_type v.v_type)) vars
		);
		"follow_with_abstracts", vfun2 (fun v once ->
			let t = decode_type v in
			let follow_once t =
				match t with
				| TMono r ->
					(match !r with
					| None -> t
					| Some t -> t)
				| TAbstract (a,tl) when not (Meta.has Meta.CoreType a.a_meta) ->
					Abstract.get_underlying_type a tl
				| TAbstract _ | TEnum _ | TInst _ | TFun _ | TAnon _ | TDynamic _ ->
					t
				| TType (t,tl) ->
					apply_params t.t_params tl t.t_type
				| TLazy f ->
					lazy_type f
			in
			encode_type (if decode_opt_bool once then follow_once t else Abstract.follow_with_abstracts t)
		);
		"follow", vfun2 (fun v once ->
			let t = decode_type v in
			let follow_once t =
				match t with
				| TMono r ->
					(match !r with
					| None -> t
					| Some t -> t)
				| TAbstract _ | TEnum _ | TInst _ | TFun _ | TAnon _ | TDynamic _ ->
					t
				| TType (t,tl) ->
					apply_params t.t_params tl t.t_type
				| TLazy f ->
					lazy_type f
			in
			encode_type (if decode_opt_bool once then follow_once t else follow t)
		);
		"get_build_fields", vfun0 (fun() ->
			(get_api()).get_build_fields()
		);
		"define_type", vfun2 (fun v m ->
			(get_api()).define_type v (opt decode_string m);
			vnull
		);
		"define_module", vfun4 (fun path vl ui ul ->
			(get_api()).define_module (decode_string path) (decode_array vl) (List.map decode_import (decode_array ui)) (List.map fst (List.map decode_path (decode_array ul)));
			vnull
		);
		"add_class_path", vfun1 (fun cp ->
			let com = ccom() in
			let cp = decode_string cp in
			let cp = Path.add_trailing_slash cp in
			com.class_path <- cp :: com.class_path;
			(match com.get_macros() with
			| Some(mcom) ->
				mcom.class_path <- cp :: mcom.class_path;
			| None ->
				());
			Hashtbl.clear com.file_lookup_cache;
			vnull
		);
		"add_native_lib", vfun1 (fun file ->
			let file = decode_string file in
			let com = ccom() in
			(match com.platform with
			| Globals.Flash -> SwfLoader.add_swf_lib com file false
			| Globals.Java -> Java.add_java_lib com file false
			| Globals.Cs ->
				let file, is_std = match ExtString.String.nsplit file "@" with
					| [file] ->
						file,false
					| [file;"std"] ->
						file,true
					| _ -> failwith ("unsupported file@`std` format: " ^ file)
				in
				Dotnet.add_net_lib com file is_std
			| _ -> failwith "Unsupported platform");
			vnull
		);
		"add_native_arg", vfun1 (fun arg ->
			let arg = decode_string arg in
			let com = ccom() in
			(match com.platform with
			| Globals.Java | Globals.Cs | Globals.Cpp ->
				com.c_args <- arg :: com.c_args
			| _ -> failwith "Unsupported platform");
			vnull
		);
		"register_module_dependency", vfun2 (fun m file ->
			(get_api()).module_dependency (decode_string m) (decode_string file);
			vnull
		);
		"register_module_reuse_call", vfun2 (fun m mcall ->
			(get_api()).module_reuse_call (decode_string m) (decode_string mcall);
			vnull
		);
		"get_typed_expr", vfun1 (fun e ->
			let e = decode_texpr e in
			encode_expr (TExprToExpr.convert_expr e)
		);
		"store_expr", vfun1 (fun e ->
			let api = get_api() in
			let te = (api.type_expr (decode_expr e)) in
			encode_expr (api.store_typed_expr te)
		);
		"store_typed_expr", vfun1 (fun e ->
			let e = decode_texpr e in
			encode_expr ((get_api()).store_typed_expr e)
		);
		"get_output", vfun0 (fun() ->
			encode_string (ccom()).file
		);
		"set_output", vfun1 (fun s ->
			(ccom()).file <- decode_string s;
			vnull
		);
		"get_display_pos", vfun0 (fun() ->
			let p = !Parser.resume_display in
			if p = Globals.null_pos then
				vnull
			else
				encode_obj OCompiler_getDisplayPos ["file",encode_string p.Globals.pfile;"pos",vint p.Globals.pmin]
		);
		"pattern_locals", vfun2 (fun e t ->
			let loc = (get_api()).get_pattern_locals (decode_expr e) (decode_type t) in
			encode_string_map (fun (v,_) -> encode_type v.v_type) loc
		);
		"on_macro_context_reused", vfun1 (fun c ->
			let c = prepare_callback c 0 in
			(get_api()).on_reuse (fun() -> decode_bool (c []));
			vnull
		);
		"apply_params", vfun3 (fun tpl tl t ->
			let tl = List.map decode_type (decode_array tl) in
			let tpl = List.map (fun v -> decode_string (field v "name"), decode_type (field v "t")) (decode_array tpl) in
			let rec map t = match t with
				| TInst({cl_kind = KTypeParameter _},_) ->
					begin try
						(* use non-physical equality check here to make apply_params work *)
						snd (List.find (fun (_,t2) -> type_iseq t t2) tpl)
					with Not_found ->
						Type.map map t
					end
				| _ -> Type.map map t
			in
			encode_type (apply_params tpl tl (map (decode_type t)))
		);
		"include_file", vfun2 (fun file position ->
			let file = decode_string file in
			let position = decode_string position in
			let file = if Sys.file_exists file then
				file
			else try Common.find_file (ccom()) file with
				| Not_found ->
					failwith ("unable to find file for inclusion: " ^ file)
			in
			(ccom()).include_files <- (file, position) :: (ccom()).include_files;
			vnull
		);
		(* Compilation server *)
		"server_add_module_check_policy", vfun4 (fun filter policy recursive context_options ->
			let filter = List.map decode_string (decode_array filter) in
			let policy = List.map decode_int (decode_array policy) in
			(get_api()).add_module_check_policy filter policy (decode_bool recursive) (decode_int context_options);
			vnull
		);
		"server_invalidate_files", vfun1 (fun a ->
			let cs = match CompilationServer.get() with Some cs -> cs | None -> failwith "compilation server not running" in
			List.iter (fun v ->
				let s = decode_string v in
				let s = Path.unique_full_path s in
				CompilationServer.taint_modules cs s;
				CompilationServer.remove_files cs s;
			) (decode_array a);
			vnull
		);
		"position_to_range", vfun1 (fun p ->
			let p = decode_pos p in
			let l1,c1,l2,c2 = Lexer.get_pos_coords p in
			let make_pos line character =
				encode_obj O__Const [
					"line",vint line;
					"character",vint character;
				]
			in
			let pos_start = make_pos l1 c1 in
			let pos_end = make_pos l2 c2 in
			let range = encode_obj O__Const [
				"start",pos_start;
				"end",pos_end;
			] in
			let location = encode_obj O__Const [
				"file",encode_string p.Globals.pfile;
				"range",range
			] in
			location
		);
	]


end
