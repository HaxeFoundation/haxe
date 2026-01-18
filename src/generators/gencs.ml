(*
	The Haxe Compiler
	Copyright (C) 2005-2019  Haxe Foundation

	This program is free software; you can redistribute it and/or
	modify it under the terms of the GNU General Public License
	as published by the Free Software Foundation; either version 2
	of the License, or (at your option) any later version.

	This program is distributed in the hope that it will be useful,
	but WITHOUT ANY WARRANTY; without even the implied warranty of
	MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
	GNU General Public License for more details.

	You should have received a copy of the GNU General Public License
	along with this program; if not, write to the Free Software
	Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA  02110-1301, USA.
 *)

(* C# code generator - main coordinator *)

open Globals
open Ast
open Type
open Gctx
open CsGlobals
open CsAst
open CsSignature
open CsPrinter

(* Get the native name of a class field (respects @:native metadata) *)
let get_native_field_name cf =
	if Meta.has Meta.Native cf.cf_meta then
		let _, args, _ = Meta.get Meta.Native cf.cf_meta in
		match args with
		| [(EConst (String (s, _)), _)] -> s
		| _ -> escape_identifier cf.cf_name
	else
		escape_identifier cf.cf_name

(* Get field name for a class, handling C# restriction where member names
   cannot be the same as the enclosing type name. *)
let get_cs_field_name c cf =
	let class_name = snd c.cl_path in
	let base_name = get_native_field_name cf in
	if base_name = class_name then base_name ^ "_" else base_name

(* Generation context *)
type gen_context = {
	com : Gctx.t;
	mutable generated_types : cs_type_def list;
	mutable closures_by_class : (path * cs_type_def list) list;  (* closures grouped by origin class path *)
	mutable closure_count : int;  (* counter for unique closure names *)
	invoke_signatures : (cs_type list * cs_type, unit) Hashtbl.t;  (* Track typed invoke signatures: (args, ret) *)
}

let create_context com = {
	com = com;
	generated_types = [];
	closures_by_class = [];
	closure_count = 0;
	invoke_signatures = Hashtbl.create 32;
}

(* Add a closure to the list for its origin class *)
let add_closure_for_class gctx origin_class_path closure_def =
	let existing = try List.assoc origin_class_path gctx.closures_by_class with Not_found -> [] in
	gctx.closures_by_class <- (origin_class_path, closure_def :: existing) ::
		List.filter (fun (p, _) -> p <> origin_class_path) gctx.closures_by_class

(* Get closures for a specific class path *)
let get_closures_for_class gctx class_path =
	try List.assoc class_path gctx.closures_by_class with Not_found -> []

(* Classify a cs_type for invoke signature purposes.
   We normalize types to avoid generating too many overloads:
   - Primitives (int, double, bool, etc.) stay as-is
   - Null<T> stays as-is (important for optional params)
   - Everything else becomes object
   This mirrors JVM's signature classification approach. *)
let rec classify_for_invoke t =
	match t with
	| CsTypeInt | CsTypeLong | CsTypeFloat | CsTypeDouble | CsTypeBool
	| CsTypeByte | CsTypeChar | CsTypeShort -> t
	| CsTypeString -> CsTypeObject  (* String is a reference type, use object *)
	| CsTypeClass ((["haxe"; "lang"], "Null"), [inner]) ->
		(* Keep Null<T> but classify inner type *)
		CsTypeClass ((["haxe"; "lang"], "Null"), [classify_for_invoke inner])
	| CsTypeVoid -> CsTypeVoid
	| _ -> CsTypeObject  (* All other types become object *)

(* Register an invoke signature for later generation on Function class *)
let register_invoke_signature gctx arg_types ret_type =
	(* Classify types to normalize the signature *)
	let classified_args = List.map classify_for_invoke arg_types in
	let classified_ret = classify_for_invoke ret_type in
	let key = (classified_args, classified_ret) in
	if not (Hashtbl.mem gctx.invoke_signatures key) then
		Hashtbl.add gctx.invoke_signatures key ()

(* Get the invoke method name for the given arity.
   invoke() for 0 args, invoke1 for 1 arg, invoke2 for 2 args, etc. *)
let invoke_method_name num_args =
	if num_args = 0 then "invoke"
	else "invoke" ^ string_of_int num_args

(* Convert Haxe binop to C# binop *)
let rec cs_binop_of_binop = function
	| OpAdd -> CsOpAdd
	| OpSub -> CsOpSub
	| OpMult -> CsOpMul
	| OpDiv -> CsOpDiv
	| OpMod -> CsOpMod
	| OpAnd -> CsOpAnd
	| OpOr -> CsOpOr
	| OpXor -> CsOpXor
	| OpShl -> CsOpShl
	| OpShr -> CsOpShr
	| OpUShr -> CsOpShr  (* C# doesn't have unsigned shift, handle separately *)
	| OpEq -> CsOpEq
	| OpNotEq -> CsOpNotEq
	| OpLt -> CsOpLt
	| OpLte -> CsOpLte
	| OpGt -> CsOpGt
	| OpGte -> CsOpGte
	| OpAssign -> CsOpAssign
	| OpBoolAnd -> CsOpBoolAnd
	| OpBoolOr -> CsOpBoolOr
	| OpInterval -> failwith "Interval operator not supported"
	| OpArrow -> failwith "Arrow operator not supported"
	| OpIn -> failwith "In operator not supported"
	| OpNullCoal -> CsOpNullCoalesce
	| OpAssignOp op -> CsOpAssignOp (cs_binop_of_binop op)

(* Convert Haxe unop to C# unop *)
let cs_unop_of_unop = function
	| Increment -> CsOpIncrement
	| Decrement -> CsOpDecrement
	| Not -> CsOpNot
	| Neg -> CsOpNeg
	| NegBits -> CsOpBitNot
	| Spread -> failwith "Spread operator not supported"

(* Expression generation context *)
type expr_context = {
	gctx : gen_context;
	mutable local_vars : (int * string) list;  (* tvar.v_id -> generated name *)
	mutable used_names : string list;  (* names already used in current scope *)
	mutable temp_count : int;
	mutable return_type : Type.t option;  (* expected return type for the current method *)
	mutable current_class_path : path option;  (* current class path for closure naming *)
	mutable current_method_name : string option;  (* current method name for closure naming *)
	mutable origin_class_path : path option;  (* original class path for grouping closures in same file *)
	mutable captured_vars : int list;  (* var IDs that are captured from outer scope (accessed via this.field) *)
	mutable captures_this : bool;  (* true if 'this' from outer scope is captured as _hx_this *)
}

let create_expr_context gctx = {
	gctx = gctx;
	local_vars = [];
	used_names = [];
	temp_count = 0;
	return_type = None;
	current_class_path = None;
	current_method_name = None;
	origin_class_path = None;
	captured_vars = [];
	captures_this = false;
}

(* Result type for expressions that may need prefix statements *)
type cs_expr_result = {
	er_stmts : cs_stmt list;  (* prefix statements to emit before the expression *)
	er_expr : cs_expr;        (* the actual expression value *)
}

(* Check if an expression contains a reference to 'this'.
   Used to determine if field initializers need to be moved to the constructor
   since C# field initializers cannot use 'this'. *)
let rec expr_contains_this e =
	match e.eexpr with
	| TConst TThis -> true
	| TLocal _ | TConst _ | TTypeExpr _ | TIdent _ -> false
	| TArray (e1, e2) ->
		expr_contains_this e1 || expr_contains_this e2
	| TBinop (_, e1, e2) ->
		expr_contains_this e1 || expr_contains_this e2
	| TUnop (_, _, e1) -> expr_contains_this e1
	| TField (e1, _) -> expr_contains_this e1
	| TParenthesis e1 -> expr_contains_this e1
	| TMeta (_, e1) -> expr_contains_this e1
	| TCast (e1, _) -> expr_contains_this e1
	| TEnumParameter (e1, _, _) -> expr_contains_this e1
	| TEnumIndex e1 -> expr_contains_this e1
	| TCall (e1, el) ->
		expr_contains_this e1 || List.exists expr_contains_this el
	| TNew (_, _, el) ->
		List.exists expr_contains_this el
	| TObjectDecl fields ->
		List.exists (fun (_, e1) -> expr_contains_this e1) fields
	| TArrayDecl el ->
		List.exists expr_contains_this el
	| TBlock el ->
		List.exists expr_contains_this el
	| TIf (e1, e2, e3_opt) ->
		expr_contains_this e1 || expr_contains_this e2 ||
		(match e3_opt with Some e3 -> expr_contains_this e3 | None -> false)
	| TWhile (e1, e2, _) ->
		expr_contains_this e1 || expr_contains_this e2
	| TSwitch sw ->
		expr_contains_this sw.switch_subject ||
		List.exists (fun case -> List.exists expr_contains_this case.case_patterns || expr_contains_this case.case_expr) sw.switch_cases ||
		(match sw.switch_default with Some e1 -> expr_contains_this e1 | None -> false)
	| TTry (e1, catches) ->
		expr_contains_this e1 ||
		List.exists (fun (_, e1) -> expr_contains_this e1) catches
	| TVar (_, init_opt) ->
		(match init_opt with Some e1 -> expr_contains_this e1 | None -> false)
	| TReturn e_opt ->
		(match e_opt with Some e1 -> expr_contains_this e1 | None -> false)
	| TThrow e1 -> expr_contains_this e1
	| TBreak | TContinue -> false
	| TFunction tf ->
		expr_contains_this tf.tf_expr

(* Check if an expression is "pure" (no side effects) and can be safely dropped
   when used as a statement. In C#, bare constants/locals can't be statements. *)
let rec is_pure_expr e =
	match e.eexpr with
	| TConst _ -> true
	| TLocal _ -> true
	| TTypeExpr _ -> true
	| TIdent _ -> true
	| TParenthesis e1 -> is_pure_expr e1
	| TCast (e1, None) -> is_pure_expr e1
	| TMeta (_, e1) -> is_pure_expr e1
	| TField (e1, _) -> is_pure_expr e1  (* Field access without call is pure *)
	| TEnumIndex e1 -> is_pure_expr e1
	| TEnumParameter (e1, _, _) -> is_pure_expr e1
	| _ -> false

(* Get or generate name for local variable *)
let get_local_name ectx v =
	try
		List.assoc v.v_id ectx.local_vars
	with Not_found ->
		(* Find a unique name - append suffix if already used *)
		let base_name = escape_identifier v.v_name in
		let rec find_unique_name name suffix =
			let candidate = if suffix = 0 then name else Printf.sprintf "%s_%d" name suffix in
			if List.mem candidate ectx.used_names then
				find_unique_name name (suffix + 1)
			else
				candidate
		in
		let name = find_unique_name base_name 0 in
		ectx.local_vars <- (v.v_id, name) :: ectx.local_vars;
		ectx.used_names <- name :: ectx.used_names;
		name

(* Generate fresh temp variable name *)
let fresh_temp ectx =
	ectx.temp_count <- ectx.temp_count + 1;
	Printf.sprintf "_hx_tmp%d" ectx.temp_count

(* Convert Haxe constant to C# constant *)
let cs_const_of_tconst = function
	| TInt i -> CsConstInt i
	| TFloat s -> CsConstDouble (float_of_string s)
	| TString s -> CsConstString s
	| TBool b -> CsConstBool b
	| TNull -> CsConstNull
	| TThis -> failwith "TThis is not a constant"
	| TSuper -> failwith "TSuper is not a constant"

(* Helper to check if a type is Null<T> wrapper - handles TMono, TType, TLazy *)
let is_null_wrapper_type t =
	let rec check t depth =
		if depth > 10 then false else
		match t with
		| TAbstract ({ a_path = ([], "Null") }, _) -> true
		| TType (_, _) -> check (Type.follow_once t) (depth + 1)
		| TLazy f -> check (lazy_type f) (depth + 1)
		| TMono r -> (match r.tm_type with Some t -> check t (depth + 1) | None -> false)
		| _ -> false
	in check t 0

(* Helper to check if a type is Null<Null<T>> (double wrapped) *)
let is_double_null_type t =
	let rec check t depth =
		if depth > 10 then false else
		match t with
		| TAbstract ({ a_path = ([], "Null") }, [inner]) ->
			(* Check if inner is also Null<_> *)
			let rec check_inner it depth =
				if depth > 10 then false else
				match it with
				| TAbstract ({ a_path = ([], "Null") }, _) -> true
				| TType (_, _) -> check_inner (Type.follow_once it) (depth + 1)
				| TLazy f -> check_inner (lazy_type f) (depth + 1)
				| TMono r -> (match r.tm_type with Some t -> check_inner t (depth + 1) | None -> false)
				| _ -> false
			in
			check_inner inner 0
		| TType (_, _) -> check (Type.follow_once t) (depth + 1)
		| TLazy f -> check (lazy_type f) (depth + 1)
		| TMono r -> (match r.tm_type with Some t -> check t (depth + 1) | None -> false)
		| _ -> false
	in check t 0

(* Helper to find if an expression involves a Null<T> wrapper - checks through TLocal, TCast, etc.
   Returns true if the GENERATED C# expression will have type Null<T> and needs .value unwrapping.
   CRITICAL: For TCast, we check the TARGET type (e.etype), not the inner expression type.
   This is because TCast changes the C# type - if we cast to non-Null, no .value needed. *)
let rec find_null_in_expr e =
	if is_null_wrapper_type e.etype then begin
		(* The expression type is Null<T>. But if this is a TCast with Null<Null<T>> target
		   and non-Null inner, we've flattened it to Null<T>, so only one level of Null. *)
		match e.eexpr with
		| TCast (inner, None) when is_double_null_type e.etype && not (is_null_wrapper_type inner.etype) ->
			(* Flattened from Null<Null<T>> to Null<T> - yes, it's a Null wrapper *)
			true
		| _ -> true
	end
	else match e.eexpr with
		| TLocal v -> is_null_wrapper_type v.v_type
		| TCast (_, None) ->
			(* TCast result type (e.etype) is NOT Null, so even if inner was Null,
			   the cast handles the conversion. No .value needed. *)
			false
		| TParenthesis inner -> find_null_in_expr inner
		| TMeta (_, inner) -> find_null_in_expr inner
		| _ -> false

(* Helper to get the inner type from Null<T>, if the expression needs .value unwrapping.
   Returns Some(inner) if expression is Null-wrapped and needs unwrapping, None otherwise.
   CRITICAL: For TCast, check the TARGET type, not the inner type. *)
let rec get_null_inner_if_needs_unwrap e =
	let get_inner t =
		let rec check t depth =
			if depth > 10 then None else
			match t with
			| TAbstract ({ a_path = ([], "Null") }, [inner]) -> Some inner
			| TType (_, _) -> check (Type.follow_once t) (depth + 1)
			| TLazy f -> check (lazy_type f) (depth + 1)
			| TMono r -> (match r.tm_type with Some t -> check t (depth + 1) | None -> None)
			| _ -> None
		in check t 0
	in
	if is_null_wrapper_type e.etype then
		get_inner e.etype
	else match e.eexpr with
		| TLocal v -> get_inner v.v_type
		| TCast (_, None) ->
			(* TCast result type (e.etype) is NOT Null - the cast handles it *)
			None
		| TParenthesis inner -> get_null_inner_if_needs_unwrap inner
		| TMeta (_, inner) -> get_null_inner_if_needs_unwrap inner
		| _ -> None

(* Check if a C# expression actually produces Null<Null<T>> type.
   Returns true only if the expression structure indicates double-Null wrapping.
   This is needed because expressions like ternary, default, cast may have been
   flattened to produce Null<T> even when the Haxe type says Null<Null<T>>. *)
let rec cs_expr_is_double_null cs_expr =
	match cs_expr with
	| CsCast (CsTypeClass ((["haxe"; "lang"], "Null"), [CsTypeClass ((["haxe"; "lang"], "Null"), _)]), _) ->
		(* Cast to Null<Null<T>> - actually double-wrapped *)
		true
	| CsCast (CsTypeClass ((["haxe"; "lang"], "Null"), [_]), _) ->
		(* Cast to Null<T> - flattened, not double-wrapped *)
		false
	| CsDefault (CsTypeClass ((["haxe"; "lang"], "Null"), [CsTypeClass ((["haxe"; "lang"], "Null"), _)])) ->
		(* default(Null<Null<T>>) - actually double-wrapped *)
		true
	| CsDefault (CsTypeClass ((["haxe"; "lang"], "Null"), [_])) ->
		(* default(Null<T>) - not double-wrapped *)
		false
	| CsTernary (_, then_e, else_e) ->
		(* For ternary, check if BOTH branches are double-wrapped *)
		cs_expr_is_double_null then_e && cs_expr_is_double_null else_e
	| CsParens e -> cs_expr_is_double_null e
	| CsField (_, "value") ->
		(* .value access unwraps one level, so even if inner was double-wrapped, result isn't *)
		false
	| _ ->
		(* For other cases (locals, calls, etc.), we can't easily determine,
		   so assume the Haxe type is accurate *)
		true

(* Generate a coerced argument expression - adds cast if needed for type mismatch *)
let coerce_arg gctx cs_arg arg_type expected_type =
	let arg_cs_type = cs_type_of_type gctx arg_type in
	let expected_cs_type = cs_type_of_type gctx expected_type in
	(* Check for various type conversions *)
	match expected_cs_type, arg_cs_type with
	(* Numeric conversions *)
	| CsTypeFloat, CsTypeDouble ->
		(* double -> float (Single) needs explicit cast *)
		CsCast (CsTypeFloat, cs_arg)
	| CsTypeFloat, CsTypeInt ->
		(* int -> float needs explicit cast *)
		CsCast (CsTypeFloat, cs_arg)
	| CsTypeByte, CsTypeInt ->
		(* int -> byte needs explicit cast *)
		CsCast (CsTypeByte, cs_arg)
	| CsTypeInt, CsTypeDouble ->
		(* double -> int needs explicit cast *)
		CsCast (CsTypeInt, cs_arg)
	(* object to basic types - need explicit cast *)
	| CsTypeInt, CsTypeObject -> CsCast (CsTypeInt, cs_arg)
	| CsTypeDouble, CsTypeObject -> CsCast (CsTypeDouble, cs_arg)
	| CsTypeBool, CsTypeObject -> CsCast (CsTypeBool, cs_arg)
	| CsTypeFloat, CsTypeObject -> CsCast (CsTypeFloat, cs_arg)
	| CsTypeString, CsTypeObject -> CsCast (CsTypeString, cs_arg)
	(* System.Type (Class<T>) from object needs explicit cast *)
	| CsTypeClass ((["System"], "Type"), []), CsTypeObject -> CsCast (expected_cs_type, cs_arg)
	(* object/Dynamic to generic type param T - need explicit cast (T)value *)
	| CsTypeGenericParam _, CsTypeObject -> CsCast (expected_cs_type, cs_arg)
	| CsTypeGenericParam _, CsTypeDynamic -> CsCast (expected_cs_type, cs_arg)
	(* Null<Null<T>> to T - need double unwrap via .value.value, but ONLY if expression actually has double-Null *)
	| target, CsTypeClass ((["haxe"; "lang"], "Null"), [CsTypeClass ((["haxe"; "lang"], "Null"), [inner])])
		when target = inner && cs_expr_is_double_null cs_arg ->
		CsField (CsField (cs_arg, "value"), "value")
	(* Null<Null<T>> to Null<T> - need single unwrap via .value, but ONLY if expression actually has double-Null *)
	| CsTypeClass ((["haxe"; "lang"], "Null"), [target]), CsTypeClass ((["haxe"; "lang"], "Null"), [CsTypeClass ((["haxe"; "lang"], "Null"), [inner])])
		when target = inner && cs_expr_is_double_null cs_arg ->
		CsField (cs_arg, "value")
	(* Haxe type says Null<Null<T>> to T, but C# expression was flattened - just single unwrap *)
	| target, CsTypeClass ((["haxe"; "lang"], "Null"), [CsTypeClass ((["haxe"; "lang"], "Null"), [inner])])
		when target = inner ->
		CsField (cs_arg, "value")
	(* Haxe type says Null<Null<T>> to Null<T>, but C# expression was flattened - no unwrap needed *)
	| CsTypeClass ((["haxe"; "lang"], "Null"), [target]), CsTypeClass ((["haxe"; "lang"], "Null"), [CsTypeClass ((["haxe"; "lang"], "Null"), [inner])])
		when target = inner ->
		cs_arg
	(* Don't cast object to arbitrary class types or generic params - they may not be in scope
	   and the type system should handle covariance through proper interfaces *)
	| _ -> cs_arg

(* Generate call arguments with type coercion based on expected parameter types *)
let generate_call_args ectx cs_expr_of_texpr args param_types =
	List.mapi (fun i arg ->
		if i < List.length param_types then
			let expected_type = List.nth param_types i in
			let expected_cs_type = cs_type_of_type ectx.gctx expected_type in
			(* Special case: null argument - generate the right default value directly based on expected type *)
			let is_null_arg = match arg.eexpr with TConst TNull -> true | _ -> false in
			if is_null_arg then begin
				(* For null arguments, generate appropriate default value based on expected type *)
				match expected_cs_type with
				| CsTypeClass ((["haxe"; "lang"], "Null"), _) ->
					(* Null<T> expected - generate default(Null<T>) with type params erased *)
					CsDefault (CsSignature.erase_type_params expected_cs_type)
				| CsTypeInt | CsTypeDouble | CsTypeBool | CsTypeLong | CsTypeFloat
				| CsTypeByte | CsTypeSByte | CsTypeChar | CsTypeShort | CsTypeUShort
				| CsTypeUInt | CsTypeULong | CsTypeDecimal ->
					(* Value type expected - generate default(T) *)
					CsDefault expected_cs_type
				| CsTypeGenericParam _ ->
					(* Generic param - generate default(T) *)
					CsDefault expected_cs_type
				| _ ->
					(* Reference type - generate null *)
					CsNull
			end
			else begin
				let cs_arg = cs_expr_of_texpr ectx arg in
				coerce_arg ectx.gctx cs_arg arg.etype expected_type
			end
		else
			cs_expr_of_texpr ectx arg
	) args

(* Generate a unique closure class name based on current context *)
let generate_closure_name gctx ectx =
	let count = gctx.closure_count in
	gctx.closure_count <- count + 1;
	let base_name = match ectx.current_class_path, ectx.current_method_name with
		| Some (ns, cname), Some mname -> Printf.sprintf "%s_%s" cname mname
		| Some (_, cname), None -> cname
		| None, Some mname -> mname
		| None, None -> "Closure"
	in
	Printf.sprintf "_hx_Closure_%s_%d" base_name count

(* Forward declaration for mutual recursion - implemented below *)
let generate_closure_class_ref : (expr_context -> tfunc -> Type.t -> cs_expr) ref = ref (fun _ _ _ -> failwith "Not initialized")

(* Forward declaration for method closure generation - implemented below *)
(* Parameters: ectx, obj_expr (CsThis or CsLocal), is_static, class_path, type_params, class_field, method_type *)
let generate_method_closure_ref : (expr_context -> cs_expr option -> bool -> path -> Type.t list -> tclass_field -> Type.t -> cs_expr) ref =
	ref (fun _ _ _ _ _ _ _ -> failwith "Not initialized")

(* Convert Haxe expression to C# expression - mutually recursive with cs_stmt_of_texpr *)
let rec cs_expr_of_texpr ectx e =
	match e.eexpr with
	| TConst TNull ->
		(* For Null<T> types and generic type params, generate default(T) instead of null.
		   However, avoid double-wrapping: if the C# type is already Null<Null<T>>, use the inner type. *)
		let cs_type = cs_type_of_type ectx.gctx e.etype in
		begin match cs_type with
		| CsTypeClass ((["haxe"; "lang"], "Null"), [CsTypeClass ((["haxe"; "lang"], "Null"), _) as inner]) ->
			(* Null<Null<T>> - avoid double wrapping, use Null<T> instead *)
			CsDefault inner
		| CsTypeClass ((["haxe"; "lang"], "Null"), _) -> CsDefault cs_type
		| CsTypeGenericParam _ -> CsDefault cs_type  (* C# requires default(T) for generic params *)
		| _ -> CsNull
		end
	| TConst c ->
		(* Handle captured 'this' - when captures_this is true, TThis becomes this._hx_this *)
		begin match c with
		| TThis when ectx.captures_this -> CsField (CsThis, "_hx_this")
		| TThis -> CsThis
		| TSuper -> CsBase
		| _ -> CsConst (cs_const_of_tconst c)
		end
	| TLocal v ->
		(* Check if this is a captured variable - if so, access via this.fieldName *)
		if List.mem v.v_id ectx.captured_vars then
			let name = get_local_name ectx v in
			CsField (CsThis, name)
		else
			CsLocal (get_local_name ectx v)
	| TArray (e1, e2) ->
		(* Check if this is array access on haxe.root.Array<T> - if so, access __a directly *)
		(* Also check for Null<Array<T>> - need to unwrap via .value first *)
		let rec is_haxe_array_type t =
			match follow t with
			| TInst ({ cl_path = ([], "Array") | (["haxe"; "root"], "Array") }, _) -> true
			| TAbstract ({ a_path = ([], "Null") }, [inner]) ->
				begin match follow inner with
				| TInst ({ cl_path = ([], "Array") | (["haxe"; "root"], "Array") }, _) -> true
				| _ -> is_haxe_array_type inner
				end
			| TAbstract (a, tl) when a.a_path <> ([], "Null") ->
				(* Check underlying type for abstracts that wrap Array *)
				let underlying = Abstract.get_underlying_type a tl in
				is_haxe_array_type underlying
			| _ -> false
		in
		(* Check if array expression is Dynamic - need runtime helper *)
		let is_dynamic = match follow e1.etype with
			| TDynamic _ -> true
			| _ -> false
		in
		if is_dynamic then
			(* Dynamic array access: use runtime helper cs.Cs.arrayGet *)
			CsStaticCall (CsTypeClass ((["cs"], "Cs"), []), "arrayGet", [cs_expr_of_texpr ectx e1; cs_expr_of_texpr ectx e2])
		else
			let is_haxe_array = is_haxe_array_type e1.etype in
			let is_null_wrapper = find_null_in_expr e1 in
			if is_haxe_array then
				(* arr[i] -> arr.__a[i] or arr.value.__a[i] for haxe Array *)
				let arr_expr = cs_expr_of_texpr ectx e1 in
				let arr_expr = if is_null_wrapper then CsField (arr_expr, "value") else arr_expr in
				CsArrayAccess (CsField (arr_expr, "__a"), cs_expr_of_texpr ectx e2)
			else
				CsArrayAccess (cs_expr_of_texpr ectx e1, cs_expr_of_texpr ectx e2)
	| TBinop (op, e1, e2) ->
		(* Special handling for Null<T> comparisons with null and generic type param equality *)
		let is_null_type t = match cs_type_of_type ectx.gctx t with
			| CsTypeClass ((["haxe"; "lang"], "Null"), _) -> true
			| _ -> false
		in
		let is_generic_param t = match cs_type_of_type ectx.gctx t with
			| CsTypeGenericParam _ -> true
			| _ -> false
		in
		let is_null_expr e = match e.eexpr with
			| TConst TNull -> true
			| _ -> false
		in
		(* Check if this is an assignment to a Haxe Array element *)
		(* Also detect Null<Array<T>> wrapper that needs unwrapping *)
		let rec is_haxe_array_type_binop t =
			match follow t with
			| TInst ({ cl_path = ([], "Array") | (["haxe"; "root"], "Array") }, _) -> true
			| TAbstract ({ a_path = ([], "Null") }, [inner]) ->
				begin match follow inner with
				| TInst ({ cl_path = ([], "Array") | (["haxe"; "root"], "Array") }, _) -> true
				| _ -> is_haxe_array_type_binop inner
				end
			| TAbstract (a, tl) when a.a_path <> ([], "Null") ->
				(* Check underlying type for abstracts that wrap Array *)
				let underlying = Abstract.get_underlying_type a tl in
				is_haxe_array_type_binop underlying
			| _ -> false
		in
		let is_haxe_array_assign, arr_needs_unwrap = match op, e1.eexpr with
			| OpAssign, TArray (arr, _) ->
				let is_arr = is_haxe_array_type_binop arr.etype in
				let is_null = find_null_in_expr arr in
				is_arr, is_null
			| _ -> false, false
		in
		(* Check for dynamic array assignment *)
		let is_dynamic_array_assign = match op, e1.eexpr with
			| OpAssign, TArray (arr, _) ->
				begin match follow arr.etype with
				| TDynamic _ -> true
				| _ -> false
				end
			| _ -> false
		in
		begin match op with
		| OpAssign when is_dynamic_array_assign ->
			(* Dynamic array assignment: arr[i] = v -> Cs.arraySet(arr, i, v) *)
			begin match e1.eexpr with
			| TArray (arr, idx) ->
				let arr_cs = cs_expr_of_texpr ectx arr in
				let idx_cs = cs_expr_of_texpr ectx idx in
				let val_cs = cs_expr_of_texpr ectx e2 in
				CsStaticCall (CsTypeClass ((["cs"], "Cs"), []), "arraySet", [arr_cs; idx_cs; val_cs])
			| _ -> CsBinop (cs_binop_of_binop op, cs_expr_of_texpr ectx e1, cs_expr_of_texpr ectx e2)
			end
		| OpAssign when is_haxe_array_assign ->
			(* Haxe Array assignment: arr[i] = v  ->  arr.__set(i, v) with return value v *)
			begin match e1.eexpr with
			| TArray (arr, idx) ->
				(* __set doesn't return a value, but assignment should evaluate to v *)
				(* We generate: (arr.__set(i, v), v) if we need the value, but for now just the call *)
				let arr_cs = cs_expr_of_texpr ectx arr in
				let arr_cs = if arr_needs_unwrap then CsField (arr_cs, "value") else arr_cs in
				let idx_cs = cs_expr_of_texpr ectx idx in
				let val_cs = cs_expr_of_texpr ectx e2 in
				CsCall (CsField (arr_cs, "__set"), [idx_cs; val_cs])
			| _ -> CsBinop (cs_binop_of_binop op, cs_expr_of_texpr ectx e1, cs_expr_of_texpr ectx e2)
			end
		| OpAssign ->
			(* Check for NativeArray byte element assignment - needs explicit cast to byte *)
			let need_byte_cast = match e1.eexpr with
				| TArray (arr, _) ->
					begin match follow arr.etype with
					| TInst ({ cl_path = (["cs"], "NativeArray") }, [elem_t]) ->
						(* Check if element type is byte/UInt8 *)
						begin match follow elem_t with
						| TAbstract ({ a_path = ([], "UInt8") | (["cs"], "UInt8") }, _) ->
							(* Check if value type is Int - needs cast *)
							begin match follow e2.etype with
							| TAbstract ({ a_path = ([], "Int") }, _) -> true
							| _ -> false
							end
						| _ -> false
						end
					| _ -> false
					end
				| _ -> false
			in
			(* Check for dynamic/anon field assignment: dynObj.field = value -> Runtime.SetField or _hx_setField *)
			begin match e1.eexpr with
			| TField (obj, FDynamic name) ->
				let obj_expr = cs_expr_of_texpr ectx obj in
				let raw_type = Type.follow_once obj.etype in
				let obj_expr = match raw_type with
					| TAbstract ({ a_path = ([], "Null") }, _) ->
						CsField (obj_expr, "value")
					| _ -> obj_expr
				in
				let val_cs = cs_expr_of_texpr ectx e2 in
				CsStaticCall (CsTypeClass ((["haxe"; "lang"], "Runtime"), []), "SetField", [obj_expr; CsConst (CsConstString name); val_cs])
			| TField (obj, FAnon cf) ->
				(* Anonymous field assignment - check if it's a truly dynamic object or a known type *)
				let obj_expr = cs_expr_of_texpr ectx obj in
				let raw_type = Type.follow_once obj.etype in
				let obj_expr = match raw_type with
					| TAbstract ({ a_path = ([], "Null") }, _) ->
						CsField (obj_expr, "value")
					| _ -> obj_expr
				in
				let val_cs = cs_expr_of_texpr ectx e2 in
				(* Check if the C# type is HaxeDynamicObject or object *)
				let cs_type = cs_type_of_type ectx.gctx obj.etype in
				begin match cs_type with
				| CsTypeClass ((["haxe"; "root"], "HaxeDynamicObject"), _) ->
					(* Dynamic object - use _hx_setField *)
					CsCall (CsField (obj_expr, "_hx_setField"), [CsConst (CsConstString cf.cf_name); val_cs])
				| CsTypeObject ->
					(* Object type - use Runtime.SetField *)
					CsStaticCall (CsTypeClass ((["haxe"; "lang"], "Runtime"), []), "SetField", [obj_expr; CsConst (CsConstString cf.cf_name); val_cs])
				| _ ->
					(* Known anonymous type - generate direct field assignment *)
					CsBinop (CsOpAssign, CsField (obj_expr, escape_identifier cf.cf_name), val_cs)
				end
			| _ ->
				let val_cs = cs_expr_of_texpr ectx e2 in
				let val_cs = if need_byte_cast then CsCast (CsTypeByte, val_cs) else val_cs in
				CsBinop (cs_binop_of_binop op, cs_expr_of_texpr ectx e1, val_cs)
			end
		| OpEq when is_null_type e1.etype && is_null_expr e2 ->
			(* x == null  ->  !x.hasValue *)
			CsUnop (CsOpNot, false, CsField (cs_expr_of_texpr ectx e1, "hasValue"))
		| OpEq when is_null_expr e1 && is_null_type e2.etype ->
			(* null == x  ->  !x.hasValue *)
			CsUnop (CsOpNot, false, CsField (cs_expr_of_texpr ectx e2, "hasValue"))
		| OpNotEq when is_null_type e1.etype && is_null_expr e2 ->
			(* x != null  ->  x.hasValue *)
			CsField (cs_expr_of_texpr ectx e1, "hasValue")
		| OpNotEq when is_null_expr e1 && is_null_type e2.etype ->
			(* null != x  ->  x.hasValue *)
			CsField (cs_expr_of_texpr ectx e2, "hasValue")
		| OpEq when is_generic_param e1.etype || is_generic_param e2.etype ->
			(* T == T  ->  object.Equals(a, b) for generic type params *)
			CsStaticCall (CsTypeObject, "Equals", [cs_expr_of_texpr ectx e1; cs_expr_of_texpr ectx e2])
		| OpNotEq when is_generic_param e1.etype || is_generic_param e2.etype ->
			(* T != T  ->  !object.Equals(a, b) for generic type params *)
			CsUnop (CsOpNot, false, CsStaticCall (CsTypeObject, "Equals", [cs_expr_of_texpr ectx e1; cs_expr_of_texpr ectx e2]))
		| _ ->
			(* Check if either operand is Dynamic - if so, use runtime helpers for non-comparison ops *)
			let is_dynamic t = match cs_type_of_type ectx.gctx (follow t) with
				| CsTypeObject -> true
				(* Also treat HaxeDynamicObject as dynamic for operator purposes *)
				| CsTypeClass ((["haxe"; "root"], "HaxeDynamicObject"), _) -> true
				| _ -> false
			in
			let either_dynamic = is_dynamic e1.etype || is_dynamic e2.etype in
			let cs_path = (["cs"], "Cs") in
			let is_string t = match cs_type_of_type ectx.gctx (follow t) with
				| CsTypeString -> true
				| _ -> false
			in
			begin match op with
			(* Arithmetic operators on Dynamic need runtime dispatch - use either_dynamic for most *)
			| OpAdd when either_dynamic ->
				CsStaticCall (CsTypeClass (cs_path, []), "opAdd", [cs_expr_of_texpr ectx e1; cs_expr_of_texpr ectx e2])
			| OpSub when either_dynamic ->
				CsStaticCall (CsTypeClass (cs_path, []), "opSub", [cs_expr_of_texpr ectx e1; cs_expr_of_texpr ectx e2])
			| OpMult when either_dynamic ->
				CsStaticCall (CsTypeClass (cs_path, []), "opMul", [cs_expr_of_texpr ectx e1; cs_expr_of_texpr ectx e2])
			| OpDiv when either_dynamic ->
				CsStaticCall (CsTypeClass (cs_path, []), "opDiv", [cs_expr_of_texpr ectx e1; cs_expr_of_texpr ectx e2])
			| OpMod when either_dynamic ->
				CsStaticCall (CsTypeClass (cs_path, []), "opMod", [cs_expr_of_texpr ectx e1; cs_expr_of_texpr ectx e2])
			| OpAnd when either_dynamic ->
				CsStaticCall (CsTypeClass (cs_path, []), "opAnd", [cs_expr_of_texpr ectx e1; cs_expr_of_texpr ectx e2])
			| OpOr when either_dynamic ->
				CsStaticCall (CsTypeClass (cs_path, []), "opOr", [cs_expr_of_texpr ectx e1; cs_expr_of_texpr ectx e2])
			| OpXor when either_dynamic ->
				CsStaticCall (CsTypeClass (cs_path, []), "opXor", [cs_expr_of_texpr ectx e1; cs_expr_of_texpr ectx e2])
			| OpShl when either_dynamic ->
				CsStaticCall (CsTypeClass (cs_path, []), "opShl", [cs_expr_of_texpr ectx e1; cs_expr_of_texpr ectx e2])
			| OpShr when either_dynamic ->
				CsStaticCall (CsTypeClass (cs_path, []), "opShr", [cs_expr_of_texpr ectx e1; cs_expr_of_texpr ectx e2])
			| OpUShr when either_dynamic ->
				CsStaticCall (CsTypeClass (cs_path, []), "opUshr", [cs_expr_of_texpr ectx e1; cs_expr_of_texpr ectx e2])
			(* Comparison operators on Dynamic also need runtime dispatch *)
			| OpLt when either_dynamic ->
				CsBinop (CsOpLt, CsStaticCall (CsTypeClass (cs_path, []), "compare", [cs_expr_of_texpr ectx e1; cs_expr_of_texpr ectx e2]), CsConst (CsConstInt 0l))
			| OpGt when either_dynamic ->
				CsBinop (CsOpGt, CsStaticCall (CsTypeClass (cs_path, []), "compare", [cs_expr_of_texpr ectx e1; cs_expr_of_texpr ectx e2]), CsConst (CsConstInt 0l))
			| OpLte when either_dynamic ->
				CsBinop (CsOpLte, CsStaticCall (CsTypeClass (cs_path, []), "compare", [cs_expr_of_texpr ectx e1; cs_expr_of_texpr ectx e2]), CsConst (CsConstInt 0l))
			| OpGte when either_dynamic ->
				CsBinop (CsOpGte, CsStaticCall (CsTypeClass (cs_path, []), "compare", [cs_expr_of_texpr ectx e1; cs_expr_of_texpr ectx e2]), CsConst (CsConstInt 0l))
			(* Equality comparisons with Dynamic - use object.Equals *)
			| OpEq when either_dynamic ->
				CsStaticCall (CsTypeObject, "Equals", [cs_expr_of_texpr ectx e1; cs_expr_of_texpr ectx e2])
			| OpNotEq when either_dynamic ->
				CsUnop (CsOpNot, false, CsStaticCall (CsTypeObject, "Equals", [cs_expr_of_texpr ectx e1; cs_expr_of_texpr ectx e2]))
			(* String comparison: use stringCompare for < > operators *)
			| OpLt when is_string e1.etype && is_string e2.etype ->
				CsBinop (CsOpLt, CsStaticCall (CsTypeClass (cs_path, []), "stringCompare", [cs_expr_of_texpr ectx e1; cs_expr_of_texpr ectx e2]), CsConst (CsConstInt 0l))
			| OpGt when is_string e1.etype && is_string e2.etype ->
				CsBinop (CsOpGt, CsStaticCall (CsTypeClass (cs_path, []), "stringCompare", [cs_expr_of_texpr ectx e1; cs_expr_of_texpr ectx e2]), CsConst (CsConstInt 0l))
			| OpLte when is_string e1.etype && is_string e2.etype ->
				CsBinop (CsOpLte, CsStaticCall (CsTypeClass (cs_path, []), "stringCompare", [cs_expr_of_texpr ectx e1; cs_expr_of_texpr ectx e2]), CsConst (CsConstInt 0l))
			| OpGte when is_string e1.etype && is_string e2.etype ->
				CsBinop (CsOpGte, CsStaticCall (CsTypeClass (cs_path, []), "stringCompare", [cs_expr_of_texpr ectx e1; cs_expr_of_texpr ectx e2]), CsConst (CsConstInt 0l))
			(* Boolean operators on Dynamic need conversion *)
			| OpBoolAnd when either_dynamic ->
				CsBinop (CsOpBoolAnd,
					CsStaticCall (CsTypeClass (cs_path, []), "dynamicToBool", [cs_expr_of_texpr ectx e1]),
					CsStaticCall (CsTypeClass (cs_path, []), "dynamicToBool", [cs_expr_of_texpr ectx e2]))
			| OpBoolOr when either_dynamic ->
				CsBinop (CsOpBoolOr,
					CsStaticCall (CsTypeClass (cs_path, []), "dynamicToBool", [cs_expr_of_texpr ectx e1]),
					CsStaticCall (CsTypeClass (cs_path, []), "dynamicToBool", [cs_expr_of_texpr ectx e2]))
			(* Compound assignment on Dynamic: v += e -> v = Cs.opAdd(v, e) *)
			| OpAssignOp inner_op when either_dynamic ->
				let helper = match inner_op with
					| OpAdd -> "opAdd" | OpSub -> "opSub" | OpMult -> "opMul" | OpDiv -> "opDiv"
					| OpMod -> "opMod" | OpAnd -> "opAnd" | OpOr -> "opOr" | OpXor -> "opXor"
					| OpShl -> "opShl" | OpShr -> "opShr" | OpUShr -> "opUshr"
					| _ -> ""  (* unsupported compound op *)
				in
				if helper = "" then
					CsBinop (cs_binop_of_binop op, cs_expr_of_texpr ectx e1, cs_expr_of_texpr ectx e2)
				else
					let e1_cs = cs_expr_of_texpr ectx e1 in
					let e2_cs = cs_expr_of_texpr ectx e2 in
					let op_result = CsStaticCall (CsTypeClass (cs_path, []), helper, [e1_cs; e2_cs]) in
					CsBinop (CsOpAssign, e1_cs, op_result)
			| _ ->
				CsBinop (cs_binop_of_binop op, cs_expr_of_texpr ectx e1, cs_expr_of_texpr ectx e2)
			end
		end
	| TUnop (Spread, _, e) ->
		(* Spread operator: in C#, this is used for Rest/params arguments.
		   The spread just unwraps the array - pass through the inner expression. *)
		cs_expr_of_texpr ectx e
	| TUnop (op, pos, e) ->
		(* Check if operand type is truly Dynamic (TDynamic) - only these need runtime helpers *)
		let is_truly_dynamic = match follow e.etype with
			| TDynamic _ -> true
			| _ -> false
		in
		let is_postfix = pos = Postfix in
		if is_truly_dynamic then
			(* Use runtime helpers for dynamic operations *)
			let helper_name = match op with
				| Not -> "opNot"
				| Neg -> "opNeg"
				| NegBits -> "opNegBits"
				| Increment -> "opIncrement"
				| Decrement -> "opDecrement"
				| Spread -> "/* spread on dynamic */"
			in
			if helper_name = "/* spread on dynamic */" then
				cs_expr_of_texpr ectx e
			else
				CsStaticCall (CsTypeClass ((["cs"], "Cs"), []), helper_name, [cs_expr_of_texpr ectx e])
		else
			CsUnop (cs_unop_of_unop op, is_postfix, cs_expr_of_texpr ectx e)
	| TField (e, FInstance ({ cl_path = (["cs"], "NativeArray") }, _, { cf_name = "length" })) ->
		(* NativeArray.length -> array.Length *)
		CsField (cs_expr_of_texpr ectx e, "Length")
	| TField (e, FInstance (_, _, { cf_name = "length" })) when (
		match cs_type_of_type ectx.gctx e.etype with
		| CsTypeString -> true
		| CsTypeClass ((["haxe"; "lang"], "Null"), [CsTypeString]) -> true
		| _ -> false) ->
		(* String.length -> string.Length (C# uses uppercase) *)
		(* Also handle Null<String>.value.length via .value accessor *)
		let obj_expr = cs_expr_of_texpr ectx e in
		let obj_expr = match cs_type_of_type ectx.gctx e.etype with
			| CsTypeClass ((["haxe"; "lang"], "Null"), _) -> CsField (obj_expr, "value")
			| _ -> obj_expr
		in
		CsField (obj_expr, "Length")
	| TField (e, FInstance (c, tl, cf)) ->
		(* Check if expression type is Null<T> - if so, access .value to unwrap *)
		let needs_unwrap = find_null_in_expr e in
		let obj_expr = cs_expr_of_texpr ectx e in
		let obj_expr = if needs_unwrap then CsField (obj_expr, "value") else obj_expr in
		CsField (obj_expr, escape_identifier cf.cf_name)
	| TField (e, FClosure (Some (c, tl), cf)) ->
		(* Instance method closure - generate a closure class that wraps the method call.
		   C# doesn't allow converting method groups to haxe.lang.Function directly. *)
		let needs_unwrap = find_null_in_expr e in
		let obj_expr = cs_expr_of_texpr ectx e in
		let obj_expr = if needs_unwrap then CsField (obj_expr, "value") else obj_expr in
		(* Generate closure class that captures 'this' and calls the method *)
		!generate_method_closure_ref ectx (Some obj_expr) false c.cl_path tl cf cf.cf_type
	| TField (e, FClosure (None, cf)) ->
		(* Static method closure - generate a closure class that wraps the static method call.
		   C# doesn't allow converting method groups to haxe.lang.Function directly. *)
		begin match e.eexpr with
		| TTypeExpr (TClassDecl c) ->
			(* Static closure - generate closure class with no captures *)
			!generate_method_closure_ref ectx None true c.cl_path [] cf cf.cf_type
		| TTypeExpr mt ->
			(* Other module type - fallback to field access (shouldn't happen for methods) *)
			let t = type_of_module_type mt in
			let cs_type = cs_type_of_type ectx.gctx t in
			let cs_type = CsSignature.erase_type_params cs_type in
			begin match cs_type with
			| CsTypeClass (path, params) -> CsStaticField (CsTypeClass (path, params), escape_identifier cf.cf_name)
			| _ -> CsField (cs_expr_of_texpr ectx e, escape_identifier cf.cf_name)
			end
		| _ ->
			CsField (cs_expr_of_texpr ectx e, escape_identifier cf.cf_name)
		end
	| TField ({ etype = field_type }, FStatic (c, cf)) ->
		(* Check if this is a static method reference (not a call).
		   If so, generate a closure class. Methods have TFun type. *)
		let is_method = match cf.cf_kind with
			| Method _ -> true
			| _ -> false
		in
		if is_method then
			(* Static method reference - generate closure class *)
			!generate_method_closure_ref ectx None true c.cl_path [] cf cf.cf_type
		else begin
			(* Static field access - normal field reference *)
			(* Special handling for String static methods - redirect to StringExt *)
			let actual_path, actual_params = match c.cl_path with
			| ([], ("String" | "string")) | (["haxe"; "root"], ("String" | "string")) ->
				(* String static methods like fromCharCode are in cs.StringExt *)
				((["cs"], "StringExt"), [])
			| _ ->
				let path = cs_path_of_path c.cl_path in
				(* For generic classes, try to infer type arguments from field type *)
				let type_params = match follow field_type with
					| TFun (_, ret) -> begin match follow ret with
						| TInst (ret_class, ret_params) when ret_class.cl_path = c.cl_path ->
							List.map (cs_type_of_type ectx.gctx) ret_params
						| _ ->
							List.map (fun _ -> CsTypeObject) c.cl_params
					end
					| _ ->
						List.map (fun _ -> CsTypeObject) c.cl_params
				in
				(path, type_params)
			in
			CsStaticField (CsTypeClass (actual_path, actual_params), get_cs_field_name c cf)
		end
	| TField (e, FAnon cf) ->
		(* Anonymous object field access *)
		(* First, check if expression type is Null<T> - if so, unwrap via .value.
		   Use get_null_inner_if_needs_unwrap which correctly handles TCast expressions. *)
		let obj_expr = cs_expr_of_texpr ectx e in
		let inner_type, obj_expr = match get_null_inner_if_needs_unwrap e with
			| Some inner -> (inner, CsField (obj_expr, "value"))
			| None -> (e.etype, obj_expr)
		in
		(* Check if the (unwrapped) C# type maps to a concrete class. If so, generate direct field access. *)
		let cs_type = cs_type_of_type ectx.gctx inner_type in
		begin match cs_type with
		| CsTypeClass ((["haxe"; "iterators"], "ArrayIterator"), _)
		| CsTypeClass ((["haxe"; "iterators"], "MapKeyValueIterator"), _) ->
			(* Known iterator class - generate direct field access *)
			CsField (obj_expr, escape_identifier cf.cf_name)
		| CsTypeClass ((["haxe"; "root"], "HaxeDynamicObject"), _) ->
			(* Truly anonymous - use _hx_getField *)
			let field_call = CsCall (CsField (obj_expr, "_hx_getField"), [CsConst (CsConstString cf.cf_name)]) in
			let target_type = cs_type_of_type ectx.gctx cf.cf_type in
			begin match target_type with
			| CsTypeObject -> field_call
			| _ -> CsCast (target_type, field_call)
			end
		| CsTypeString when cf.cf_name = "length" ->
			(* String.length -> string.Length (C# uses uppercase) *)
			CsField (obj_expr, "Length")
		| CsTypeClass (_, _) ->
			(* Some other concrete class - try direct access *)
			CsField (obj_expr, escape_identifier cf.cf_name)
		| CsTypeObject ->
			(* Object type (from TAnon/structural type) - use Reflect.field for dynamic access *)
			let reflect_path = (["haxe"; "root"], "Reflect") in
			let field_call = CsStaticCall (CsTypeClass (reflect_path, []), "field", [obj_expr; CsConst (CsConstString cf.cf_name)]) in
			let target_type = cs_type_of_type ectx.gctx cf.cf_type in
			begin match target_type with
			| CsTypeObject -> field_call
			| _ -> CsCast (target_type, field_call)
			end
		| _ ->
			(* Fallback to dynamic dispatch via _hx_getField *)
			let field_call = CsCall (CsField (obj_expr, "_hx_getField"), [CsConst (CsConstString cf.cf_name)]) in
			let target_type = cs_type_of_type ectx.gctx cf.cf_type in
			begin match target_type with
			| CsTypeObject -> field_call
			| _ -> CsCast (target_type, field_call)
			end
		end
	| TField (e, FDynamic name) ->
		(* Dynamic field access - need to use reflection since C# object doesn't have arbitrary fields *)
		(* NOTE: Use follow_once to peel through TMono but not unwrap Null<T> *)
		let obj_expr = cs_expr_of_texpr ectx e in
		let raw_type = Type.follow_once e.etype in
		let obj_expr = match raw_type with
			| TAbstract ({ a_path = ([], "Null") }, _) ->
				(* Null<T> -> access .value to unwrap *)
				CsField (obj_expr, "value")
			| _ -> obj_expr
		in
		(* Use haxe.lang.Runtime.GetField for dynamic field access *)
		CsStaticCall (CsTypeClass ((["haxe"; "lang"], "Runtime"), []), "GetField", [obj_expr; CsConst (CsConstString name)])
	| TField (_, FEnum (en, ef)) ->
		let path = cs_path_of_path en.e_path in
		(* Get type arguments from the expression type for generic enums like Option<T> *)
		let type_args = match follow e.etype with
			| TEnum (_, params) -> List.map (cs_type_of_type ectx.gctx) params
			| _ -> []
		in
		(* For parameterless enum constructors, access via: new EnumType<T>.ConstructorName()
		   For generic enums, nested class uses parent's type params (C# nested class inheritance) *)
		if type_args = [] && en.e_params = [] then
			(* Non-generic enum - use static field if ef has no params *)
			CsStaticField (CsTypeClass (path, []), escape_identifier ef.ef_name)
		else begin
			(* Generic enum - nested class inherits type params from parent: Parent<T>.Nested *)
			let ctor_name = escape_identifier ef.ef_name in
			let parent_type = CsTypeClass (path, type_args) in
			let nested_type = CsTypeNested (parent_type, ctor_name) in
			CsNew (nested_type, [])
		end
	| TCall (e_callee, args) when (
		(* Check if callee is a TFunction, possibly wrapped in TParenthesis *)
		let rec is_function e = match e.eexpr with
			| TFunction _ -> true
			| TParenthesis e1 -> is_function e1
			| TMeta (_, e1) -> is_function e1
			| _ -> false
		in
		is_function e_callee
	) ->
		(* Immediately-invoked function expression (IIFE): ((x) => ...)(args)
		   The TFunction becomes a closure class that extends haxe.lang.Function.
		   Call it using the typed invoke method. *)
		let closure = cs_expr_of_texpr ectx e_callee in
		(* Get the function type from the callee to extract param/return types *)
		let rec get_tf e = match e.eexpr with
			| TFunction tf -> Some tf
			| TParenthesis e1 | TMeta (_, e1) -> get_tf e1
			| _ -> None
		in
		let param_types_hx, ret_type_hx = match get_tf e_callee with
			| Some tf ->
				List.filter_map (fun (v, _) ->
					if ExtType.is_void (follow v.v_type) then None
					else Some v.v_type
				) tf.tf_args, tf.tf_type
			| None ->
				(* Fallback to TFun type *)
				match follow e_callee.etype with
				| TFun (params, ret) ->
					List.map (fun (_, opt, t) ->
						if opt then ectx.gctx.com.basic.tnull t else t
					) params, ret
				| _ -> [], ectx.gctx.com.basic.tvoid
		in
		let args_cs = List.mapi (fun i arg ->
			let expected_hx_type = if i < List.length param_types_hx then
				List.nth param_types_hx i
			else
				arg.etype
			in
			let cs_arg = cs_expr_of_texpr ectx arg in
			coerce_arg ectx.gctx cs_arg arg.etype expected_hx_type
		) args in
		let result_type = cs_type_of_type ectx.gctx ret_type_hx in
		let param_types_cs = List.map (cs_type_of_type ectx.gctx) param_types_hx in
		(* Register this signature for typed invoke generation *)
		register_invoke_signature ectx.gctx param_types_cs result_type;
		let call_expr = CsCall (CsField (closure, invoke_method_name (List.length args_cs)), args_cs) in
		(* The base class invokeN methods ALWAYS return object, so we need to cast
		   to the expected return type unless it's void or object *)
		begin match result_type with
		| CsTypeVoid -> call_expr
		| CsTypeObject | CsTypeDynamic -> call_expr
		| _ -> CsCast (result_type, call_expr)
		end
	| TCall ({ eexpr = TField (_, FEnum (en, ef)) }, args) ->
		(* Enum constructor with parameters -> new EnumType<T>.ConstructorName(...) *)
		let enum_path = cs_path_of_path en.e_path in
		let ctor_name = escape_identifier ef.ef_name in
		let args = List.map (cs_expr_of_texpr ectx) args in
		(* Get type arguments from the TCall's result type (e.etype) for generic enums *)
		let type_args = match follow e.etype with
			| TEnum (_, params) -> List.map (cs_type_of_type ectx.gctx) params
			| _ -> []
		in
		(* Nested class inherits type params from parent: Parent<T>.Nested *)
		let parent_type = CsTypeClass (enum_path, type_args) in
		let nested_type = CsTypeNested (parent_type, ctor_name) in
		CsNew (nested_type, args)
	| TCall ({ eexpr = TField (e_obj, FInstance (c, _, cf)) }, args)
		when (match c.cl_path with ([], ("String" | "string")) | (["haxe"; "root"], ("String" | "string")) -> true | _ -> false) ->
		(* String methods can be:
		   1. Native methods (@:native) like toUpperCase/toLowerCase -> use C# native name
		   2. Inline methods that redirect to StringExt -> should already be inlined, but handle fallback
		   Check for @:native metadata first, then check if method exists in StringExt. *)
		let obj = cs_expr_of_texpr ectx e_obj in
		let is_stringext_method = match cf.cf_name with
			| "charAt" | "charCodeAt" | "indexOf" | "lastIndexOf"
			| "split" | "substr" | "substring" -> true
			| _ -> false
		in
		if is_stringext_method then begin
			(* Inline method that should redirect to StringExt *)
			let cs_args = obj :: List.map (cs_expr_of_texpr ectx) args in
			CsStaticCall (CsTypeClass ((["cs"], "StringExt"), []), cf.cf_name, cs_args)
		end else begin
			(* Native method or toString - use direct method call with proper name *)
			let method_name = get_cs_field_name c cf in
			let cs_args = List.map (cs_expr_of_texpr ectx) args in
			CsCall (CsField (obj, method_name), cs_args)
		end
	| TCall ({ eexpr = TField (e_obj, FInstance (c, tl, cf)) }, args)
	| TCall ({ eexpr = TField (e_obj, FClosure (Some (c, tl), cf)) }, args) ->
		(* Check if this is a stored function field - we're calling a stored function,
		   not invoking a regular method. In that case, we need to use invokeN methods.
		   This includes:
		   - Var fields with function type (like `public var myFunc: Int->Void`)
		   - Method MethDynamic (like `public dynamic function onAbort(...)`) which are also stored functions *)
		let is_stored_function_field = match cf.cf_kind with
			| Var _ -> (match follow cf.cf_type with TFun _ -> true | _ -> false)
			| Method MethDynamic -> true  (* dynamic methods are stored as function fields *)
			| Method _ -> false
		in
		if is_stored_function_field then begin
			(* This is calling a function stored in a field - use typed invoke methods *)
			let needs_unwrap = find_null_in_expr e_obj in
			let obj = cs_expr_of_texpr ectx e_obj in
			let obj = if needs_unwrap then CsField (obj, "value") else obj in
			let func_expr = CsField (obj, get_native_field_name cf) in
			(* Get parameter and return types for typed invoke *)
			let param_types_hx, ret_type_hx = match follow cf.cf_type with
				| TFun (params, ret) ->
					List.map (fun (_, opt, t) ->
						let is_already_null = match follow t with
							| TAbstract ({ a_path = ([], "Null") }, _) -> true
							| _ -> false
						in
						if opt && not is_already_null then ectx.gctx.com.basic.tnull t else t
					) params, ret
				| _ -> [], ectx.gctx.com.basic.tvoid
			in
			let args_cs = List.mapi (fun i arg ->
				let expected_hx_type = if i < List.length param_types_hx then
					List.nth param_types_hx i
				else
					arg.etype
				in
				let cs_arg = cs_expr_of_texpr ectx arg in
				coerce_arg ectx.gctx cs_arg arg.etype expected_hx_type
			) args in
			let result_type = cs_type_of_type ectx.gctx ret_type_hx in
			let param_types_cs = List.map (cs_type_of_type ectx.gctx) param_types_hx in
			(* Register this signature for typed invoke generation *)
			register_invoke_signature ectx.gctx param_types_cs result_type;
			let call_expr = CsCall (CsField (func_expr, invoke_method_name (List.length args_cs)), args_cs) in
			(* The base class invokeN methods ALWAYS return object, so we need to cast
			   to the expected return type unless it's void or object *)
			begin match result_type with
			| CsTypeVoid -> call_expr
			| CsTypeObject | CsTypeDynamic -> call_expr
			| _ -> CsCast (result_type, call_expr)
			end
		end else begin
		(* Check if expression type is Null<T> - if so, access .value to unwrap.
		   Note: @:forward on Null<T> sets e.etype to the underlying type (after forward),
		   but for TLocal the v.v_type still has the Null wrapper.
		   CRITICAL: For TCast, check the TARGET type (e.etype), not the inner type.
		   This is because TCast changes the C# type - if we cast to non-Null, no .value needed. *)
		let needs_unwrap = find_null_in_expr e_obj in
		let obj = cs_expr_of_texpr ectx e_obj in
		let obj = if needs_unwrap then CsField (obj, "value") else obj in
		(* Get parameter types for argument coercion.
		   Apply class type params to get concrete types for generic methods like Array<T>.push(T).
		   IMPORTANT: When a parameter is optional (opt=true), wrap its type in Null<T>.
		   In Haxe's TFun, optional params have opt=true but the type itself is NOT wrapped.
		   We need to wrap it for C# where optional params use Null<T>.
		   BUT: Don't double-wrap if the type is already Null<T>. *)
		let param_types_base = match follow cf.cf_type with
			| TFun (params, _) ->
				let map_type = apply_params c.cl_params tl in
				List.map (fun (_, opt, t) ->
					let t = map_type t in
					let is_already_null = match follow t with
						| TAbstract ({ a_path = ([], "Null") }, _) -> true
						| _ -> false
					in
					if opt && not is_already_null then ectx.gctx.com.basic.tnull t else t
				) params
			| _ -> []
		in
		(* For generic methods, we need to provide explicit type arguments since C#
		   can't always infer them (especially with Null<T> implicit conversions).
		   Also, apply method type params to parameter types for proper argument generation. *)
		if cf.cf_params <> [] then begin
			(* Method has type params - infer from return type or arguments *)
			let return_type = e.etype in
			let infer_type_params_as_types () =
				(* Try to infer from arguments - match param types to arg types.
				   Returns Haxe Type.t list, not C# types, so we can apply them to param_types *)
				let param_type_pairs = match follow cf.cf_type with
					| TFun (params, _) when List.length params <= List.length args ->
						List.combine (List.map (fun (_, _, t) -> t) params) (List.map (fun a -> a.etype) (ExtList.List.take (List.length params) args))
					| _ -> []
				in
				(* Helper to recursively find matching type params inside generic types like Array<T> *)
				let rec find_type_param_in_type ttp_name param_t arg_t =
					match follow param_t, follow arg_t with
					| TInst ({ cl_kind = KTypeParameter ttp2 }, _), _ when ttp2.ttp_name = ttp_name ->
						(* Direct type parameter match - unwrap Null<T> if present
						   UNLESS the inner type is also Null (double-wrapped Null<Null<T>>),
						   in which case keep the inner Null as it's the actual stored type *)
						let unwrapped = match follow arg_t with
							| TAbstract ({ a_path = ([], "Null") }, [inner]) ->
								begin match follow inner with
								| TAbstract ({ a_path = ([], "Null") }, _) -> inner  (* Keep Null<X> when we have Null<Null<X>> *)
								| _ -> inner  (* Unwrap single Null<X> to X *)
								end
							| t -> t
						in
						Some unwrapped
					| TInst (c1, tp1_list), TInst (c2, tp2_list) when c1.cl_path = c2.cl_path && List.length tp1_list = List.length tp2_list ->
						(* Generic type like Array<T> matched with Array<int> - look inside type params *)
						List.fold_left2 (fun acc tp1 tp2 ->
							match acc with
							| Some _ -> acc
							| None -> find_type_param_in_type ttp_name tp1 tp2
						) None tp1_list tp2_list
					| TAbstract (a1, tp1_list), TAbstract (a2, tp2_list) when a1.a_path = a2.a_path && List.length tp1_list = List.length tp2_list ->
						(* Generic abstract like Null<T> matched with Null<int> - look inside type params *)
						List.fold_left2 (fun acc tp1 tp2 ->
							match acc with
							| Some _ -> acc
							| None -> find_type_param_in_type ttp_name tp1 tp2
						) None tp1_list tp2_list
					| TFun (p1_list, r1), TFun (p2_list, r2) when List.length p1_list = List.length p2_list ->
						(* Function types like (T)->Bool matched with (int)->Bool - look inside params and return *)
						let from_params = List.fold_left2 (fun acc (_, _, t1) (_, _, t2) ->
							match acc with
							| Some _ -> acc
							| None -> find_type_param_in_type ttp_name t1 t2
						) None p1_list p2_list in
						begin match from_params with
						| Some _ -> from_params
						| None -> find_type_param_in_type ttp_name r1 r2
						end
					| _ -> None
				in
				List.map (fun ttp ->
					let found_type = List.fold_left (fun acc (param_t, arg_t) ->
						match acc with
						| Some _ -> acc
						| None -> find_type_param_in_type ttp.ttp_name param_t arg_t
					) None param_type_pairs in
					match found_type with
					| Some t -> t
					| None -> t_dynamic  (* Fall back to Dynamic if not found *)
				) cf.cf_params
			in
			(* Get method type params as Haxe types for applying to param_types *)
			let method_type_params_hx = match follow return_type with
				| TInst (_, ret_params) when List.length ret_params = List.length cf.cf_params ->
					ret_params
				| _ -> infer_type_params_as_types ()
			in
			(* Apply method type params to parameter types *)
			let method_param_map = apply_params cf.cf_params method_type_params_hx in
			let param_types = List.map method_param_map param_types_base in
			let cs_args = generate_call_args ectx cs_expr_of_texpr args param_types in
			let method_type_params = List.map (cs_type_of_type ectx.gctx) method_type_params_hx in
			(* If any type param is void, don't provide explicit type args - C# can't use void as type arg *)
			let has_void = List.exists (fun t -> t = CsTypeVoid) method_type_params in
			if has_void then
				CsCall (CsField (obj, get_native_field_name cf), cs_args)
			else
				CsCallGeneric (CsField (obj, get_native_field_name cf), method_type_params, cs_args)
		end else begin
			let cs_args = generate_call_args ectx cs_expr_of_texpr args param_types_base in
			CsCall (CsField (obj, get_native_field_name cf), cs_args)
		end
		end  (* close the is_var_with_func_type else branch *)
	| TCall ({ eexpr = TField (e, FAnon cf) }, args) ->
		(* Method call on anonymous/structural type.
		   First, check if expression type is Null<T> - if so, unwrap via .value *)
		(* NOTE: Use follow_once to peel through TMono but not unwrap Null<T> *)
		let obj = cs_expr_of_texpr ectx e in
		let raw_type = Type.follow_once e.etype in
		let inner_type, obj = match raw_type with
			| TAbstract ({ a_path = ([], "Null") }, [inner]) ->
				(* Null<T> -> access .value to unwrap *)
				(inner, CsField (obj, "value"))
			| _ -> (e.etype, obj)
		in
		(* Get parameter types from field type for proper null handling.
		   IMPORTANT: Wrap optional params in Null<T> (opt=true means optional).
		   BUT: Don't double-wrap if the type is already Null<T>. *)
		let param_types = match follow cf.cf_type with
			| TFun (params, _) ->
				List.map (fun (_, opt, t) ->
					let is_already_null = match follow t with
						| TAbstract ({ a_path = ([], "Null") }, _) -> true
						| _ -> false
					in
					if opt && not is_already_null then ectx.gctx.com.basic.tnull t else t
				) params
			| _ -> []
		in
		let args = generate_call_args ectx cs_expr_of_texpr args param_types in
		(* Check if the (unwrapped) C# type maps to a concrete class (like ArrayIterator).
		   If so, we can generate direct method calls instead of dynamic dispatch. *)
		let cs_type = cs_type_of_type ectx.gctx inner_type in
		begin match cs_type with
		| CsTypeClass ((["haxe"; "iterators"], "ArrayIterator"), _)
		| CsTypeClass ((["haxe"; "iterators"], "MapKeyValueIterator"), _) ->
			(* Known iterator class - generate direct method call *)
			CsCall (CsField (obj, escape_identifier cf.cf_name), args)
		| CsTypeClass ((["haxe"; "root"], "HaxeDynamicObject"), _) ->
			(* Truly anonymous - use _hx_getField *)
			let field_call = CsCall (CsField (obj, "_hx_getField"), [CsConst (CsConstString cf.cf_name)]) in
			let func_type = CsSignature.erase_type_params (cs_type_of_type ectx.gctx cf.cf_type) in
			let casted = CsCast (func_type, field_call) in
			CsCall (casted, args)
		| CsTypeClass (path, _) ->
			(* Some other concrete class - try direct call *)
			CsCall (CsField (obj, escape_identifier cf.cf_name), args)
		| CsTypeObject ->
			(* Object type (from TAnon/structural type) - use Reflect.field then call *)
			let reflect_path = (["haxe"; "root"], "Reflect") in
			let field_call = CsStaticCall (CsTypeClass (reflect_path, []), "field", [obj; CsConst (CsConstString cf.cf_name)]) in
			let func_type = CsSignature.erase_type_params (cs_type_of_type ectx.gctx cf.cf_type) in
			let casted = CsCast (func_type, field_call) in
			CsCall (casted, args)
		| _ ->
			(* Fallback to dynamic dispatch via _hx_getField *)
			let field_call = CsCall (CsField (obj, "_hx_getField"), [CsConst (CsConstString cf.cf_name)]) in
			let func_type = CsSignature.erase_type_params (cs_type_of_type ectx.gctx cf.cf_type) in
			let casted = CsCast (func_type, field_call) in
			CsCall (casted, args)
		end
	| TCall ({ eexpr = TField (e_obj, FDynamic name) }, args) ->
		(* Dynamic method call: obj.dynamicMethod(args) -> Runtime.InvokeDelegate(Runtime.GetField(obj, "method"), args) *)
		let obj = cs_expr_of_texpr ectx e_obj in
		let raw_type = Type.follow_once e_obj.etype in
		let obj = match raw_type with
			| TAbstract ({ a_path = ([], "Null") }, _) ->
				CsField (obj, "value")
			| _ -> obj
		in
		let get_field = CsStaticCall (CsTypeClass ((["haxe"; "lang"], "Runtime"), []), "GetField", [obj; CsConst (CsConstString name)]) in
		let args_exprs = List.map (cs_expr_of_texpr ectx) args in
		(* Build an array of arguments: Array<object>.ofNative(new object[] { ... }) *)
		let args_array = if args_exprs = [] then
			CsNew (CsTypeClass (NativeTypes.haxe_array_path, [CsTypeObject]), [])
		else
			let native_array = CsNewArray (CsTypeObject, args_exprs) in
			CsStaticCall (CsTypeClass (NativeTypes.haxe_array_path, [CsTypeObject]), "ofNative", [native_array])
		in
		let call_expr = CsStaticCall (CsTypeClass ((["haxe"; "lang"], "Runtime"), []), "InvokeDelegate", [get_field; args_array]) in
		(* Cast the result to the expected return type - use e.etype (the TCall's type), not e_obj.etype *)
		let result_type = cs_type_of_type ectx.gctx e.etype in
		begin match result_type with
		| CsTypeObject | CsTypeDynamic -> call_expr  (* No cast needed for Dynamic/object *)
		| _ -> CsCast (result_type, call_expr)
		end
	| TCall ({ eexpr = TField (_, FStatic ({ cl_path = (["cs"], "Syntax") }, cf)) }, args) ->
		(* cs.Syntax.code or cs.Syntax.plainCode - inline C# code injection *)
		begin match cf.cf_name, args with
		| "code", { eexpr = TConst (TString template) } :: rest ->
			(* cs.Syntax.code("template {0} {1}", arg0, arg1) - interpolated *)
			let cs_args = List.map (cs_expr_of_texpr ectx) rest in
			CsInlineCode (template, cs_args)
		| "plainCode", [{ eexpr = TConst (TString code) }] ->
			(* cs.Syntax.plainCode("raw code") - no interpolation *)
			CsRaw code
		| "code", _ ->
			CsRaw "/* invalid cs.Syntax.code call - first arg must be string literal */"
		| "plainCode", _ ->
			CsRaw "/* invalid cs.Syntax.plainCode call - arg must be string literal */"
		| meth, _ ->
			CsRaw ("/* unknown cs.Syntax method: " ^ meth ^ " */")
		end
	| TCall ({ eexpr = TField (_, FStatic (c, cf)) }, orig_args) ->
		let return_type = e.etype in  (* Use the TCall's etype, not TField's *)
		(* Special handling for String static methods - redirect to StringExt *)
		let path, class_type_params = match c.cl_path with
		| ([], ("String" | "string")) | (["haxe"; "root"], ("String" | "string")) ->
			(* String static methods like fromCharCode are in cs.StringExt *)
			((["cs"], "StringExt"), [])
		| _ ->
			let path = cs_path_of_path c.cl_path in
			(* For generic classes, infer type arguments from return type if possible *)
			let class_type_params = match follow return_type with
				| TInst (_, ret_params) when List.length ret_params = List.length c.cl_params ->
					(* Return type is same generic class - use its type params *)
					List.map (cs_type_of_type ectx.gctx) ret_params
				| _ ->
					(* Fallback: use object for each type parameter *)
					List.map (fun _ -> CsTypeObject) c.cl_params
			in
			(path, class_type_params)
		in
		(* Get base parameter types from signature.
		   IMPORTANT: Wrap optional params in Null<T> (opt=true means optional).
		   BUT: Don't double-wrap if the type is already Null<T>. *)
		let param_types_base = match follow cf.cf_type with
			| TFun (params, _) ->
				List.map (fun (_, opt, t) ->
					let is_already_null = match follow t with
						| TAbstract ({ a_path = ([], "Null") }, _) -> true
						| _ -> false
					in
					if opt && not is_already_null then ectx.gctx.com.basic.tnull t else t
				) params
			| _ -> []
		in
		(* Check if method has its own type parameters *)
		if cf.cf_params <> [] then begin
			(* Method has type params - infer from return type, arguments, or method signature *)
			(* Check if the return type is a type parameter T (i.e., the method returns T directly) *)
			let returns_type_param = match cf.cf_type with
				| TFun (_, ret) ->
					begin match follow ret with
					| TInst ({ cl_kind = KTypeParameter _ }, _) -> true
					| _ -> false
					end
				| _ -> false
			in
			(* Try to infer method type params from arguments - returns Haxe Type.t for applying to params *)
			let infer_type_params_as_types () =
				(* For each method type parameter, try to find it in the parameter list and get the type from corresponding arg *)
				let param_type_pairs = match follow cf.cf_type with
					| TFun (params, _) when List.length params <= List.length orig_args ->
						List.combine (List.map (fun (_, _, t) -> t) params) (List.map (fun a -> a.etype) (ExtList.List.take (List.length params) orig_args))
					| _ -> []
				in
				(* Helper to recursively find matching type params inside generic types *)
				let rec find_type_param_in_type ttp_name param_t arg_t =
					match follow param_t, follow arg_t with
					| TInst ({ cl_kind = KTypeParameter ttp2 }, _), _ when ttp2.ttp_name = ttp_name ->
						(* Direct type parameter match *)
						let unwrapped = match follow arg_t with
							| TAbstract ({ a_path = ([], "Null") }, [inner]) ->
								begin match follow inner with
								| TAbstract ({ a_path = ([], "Null") }, _) -> inner
								| _ -> inner
								end
							| t -> t
						in
						Some unwrapped
					| TInst (c1, tp1_list), TInst (c2, tp2_list) when c1.cl_path = c2.cl_path && List.length tp1_list = List.length tp2_list ->
						(* Generic type SomeClass<K, V> matched with SomeClass<K2, V2> *)
						List.fold_left2 (fun acc tp1 tp2 ->
							match acc with
							| Some _ -> acc
							| None -> find_type_param_in_type ttp_name tp1 tp2
						) None tp1_list tp2_list
					| TAbstract (a1, tp1_list), TAbstract (a2, tp2_list) when a1.a_path = a2.a_path && List.length tp1_list = List.length tp2_list ->
						(* Generic abstract like Null<T> *)
						List.fold_left2 (fun acc tp1 tp2 ->
							match acc with
							| Some _ -> acc
							| None -> find_type_param_in_type ttp_name tp1 tp2
						) None tp1_list tp2_list
					| TFun (p1_list, r1), TFun (p2_list, r2) when List.length p1_list = List.length p2_list ->
						(* Function types like (T)->Bool matched with (int)->Bool *)
						let from_params = List.fold_left2 (fun acc (_, _, t1) (_, _, t2) ->
							match acc with
							| Some _ -> acc
							| None -> find_type_param_in_type ttp_name t1 t2
						) None p1_list p2_list in
						begin match from_params with
						| Some _ -> from_params
						| None -> find_type_param_in_type ttp_name r1 r2
						end
					| _ -> None
				in
				List.map (fun ttp ->
					let found_type = List.fold_left (fun acc (param_t, arg_t) ->
						match acc with
						| Some _ -> acc
						| None -> find_type_param_in_type ttp.ttp_name param_t arg_t
					) None param_type_pairs in
					match found_type with
					| Some t -> t
					| None -> t_dynamic  (* Fallback to Dynamic if can't infer *)
				) cf.cf_params
			in
			(* Get method type params as Haxe types for applying to param_types *)
			let method_type_params_hx = match follow return_type with
				| TInst (_, ret_params) when List.length ret_params = List.length cf.cf_params ->
					(* Generic return type with matching arity - use its type params *)
					ret_params
				| TInst _ | TEnum _ when returns_type_param && List.length cf.cf_params = 1 ->
					(* Method returns T directly (like createInstance<T>():T), and return type is concrete *)
					[return_type]
				| _ -> infer_type_params_as_types ()
			in
			(* Apply method type params to parameter types *)
			let method_param_map = apply_params cf.cf_params method_type_params_hx in
			let param_types = List.map method_param_map param_types_base in
			let args = generate_call_args ectx cs_expr_of_texpr orig_args param_types in
			let method_type_params = List.map (cs_type_of_type ectx.gctx) method_type_params_hx in
			(* If any type param is void, don't provide explicit type args - C# can't use void as type arg *)
			let has_void = List.exists (fun t -> t = CsTypeVoid) method_type_params in
			if has_void then
				CsStaticCall (CsTypeClass (path, class_type_params), escape_identifier cf.cf_name, args)
			else
				CsStaticCallGeneric (CsTypeClass (path, class_type_params), escape_identifier cf.cf_name, method_type_params, args)
		end else begin
			let args = generate_call_args ectx cs_expr_of_texpr orig_args param_types_base in
			CsStaticCall (CsTypeClass (path, class_type_params), escape_identifier cf.cf_name, args)
		end
	| TCall ({ eexpr = TIdent "__cs__" }, args) ->
		(* Inline C# code: untyped __cs__("code", arg1, arg2, ...) *)
		begin match args with
		| { eexpr = TConst (TString template) } :: rest ->
			let cs_args = List.map (cs_expr_of_texpr ectx) rest in
			CsInlineCode (template, cs_args)
		| _ ->
			CsRaw "/* invalid __cs__ call */"
		end
	| TCall ({ eexpr = TConst TSuper }, _) ->
		(* super() calls should be extracted and converted to : base() initializer *)
		(* If we get here, the super call is in an unexpected location *)
		CsRaw "/* ERROR: super() call in unexpected location */"
	| TCall (e_callee, args) ->
		(* Check if calling a Dynamic-typed expression *)
		let rec is_dynamic_type t = match follow t with
			| TDynamic _ -> true
			| TAbstract ({ a_path = (["haxe"], "Function") }, _) -> true  (* haxe.Constraints.Function *)
			| TAbstract (a, _) when Meta.has Meta.CoreType a.a_meta ->
				(* Abstract over Dynamic - check underlying type *)
				begin match Abstract.get_underlying_type a [] with
				| TDynamic _ -> true
				| _ -> false
				end
			| _ -> false
		in
		(* Check if callee is null - needs dynamic dispatch to throw properly *)
		let is_null_callee = match e_callee.eexpr with
			| TConst TNull -> true
			| _ -> false
		in
		let is_dynamic_call = is_null_callee || match follow e_callee.etype with
			| TDynamic _ -> true
			| TFun _ -> false  (* Typed function - will use invoke* method *)
			| _ -> is_dynamic_type e_callee.etype
		in
		if is_dynamic_call then begin
			(* Dynamic call: use haxe.lang.Runtime.InvokeDelegate(func, args) *)
			let func_expr = cs_expr_of_texpr ectx e_callee in
			(* For dynamic calls, convert arguments but use actual null for TConst TNull.
			   This ensures that invokeDynamic can detect null args with == null check.
			   If we used default(Null<T>), it would box to a struct with hasValue=false,
			   which != null and would fail when cast to the inner type. *)
			let args_exprs = List.map (fun arg ->
				match arg.eexpr with
				| TConst TNull -> CsNull  (* Use actual null for dynamic call args *)
				| _ -> cs_expr_of_texpr ectx arg
			) args in
			(* Build an array of arguments: new haxe.root.Array<object>(new object[] { ... }) *)
			let args_array = if args_exprs = [] then
				CsNew (CsTypeClass (NativeTypes.haxe_array_path, [CsTypeObject]), [])
			else
				let native_array = CsNewArray (CsTypeObject, args_exprs) in
				CsStaticCall (CsTypeClass (NativeTypes.haxe_array_path, [CsTypeObject]), "ofNative", [native_array])
			in
			let call_expr = CsStaticCall (CsTypeClass ((["haxe"; "lang"], "Runtime"), []), "InvokeDelegate", [func_expr; args_array]) in
			(* Cast the result to the expected return type *)
			let result_type = cs_type_of_type ectx.gctx e.etype in
			begin match result_type with
			| CsTypeVoid | CsTypeObject | CsTypeDynamic -> call_expr  (* No cast needed for void/Dynamic/object *)
			| _ -> CsCast (result_type, call_expr)
			end
		end else begin
			(* NOTE: Future optimization opportunity - if we could track which TLocal variables
			   have specific closure types (and are never reassigned), we could call their
			   typed invoke() method directly instead of going through invokeN with boxing.
			   For now, we always use the dynamic dispatch path.
			   See plan: "CRITICAL: Typed Closure Variables and Direct invoke() Calls" *)
			let raw_func = cs_expr_of_texpr ectx e_callee in
			(* Check if callee is Null<Function> - if so, access .value to unwrap *)
			let callee_type = Type.follow_once e_callee.etype in
			let func = match callee_type with
				| TAbstract ({ a_path = ([], "Null") }, [TFun _]) ->
					(* Null<Function> -> access .value to unwrap *)
					CsField (raw_func, "value")
				| _ -> raw_func
			in
			(* Get expected parameter types from the function type to handle coercion.
			   IMPORTANT: Wrap optional params in Null<T> (opt=true means optional).
			   BUT: Don't double-wrap if the type is already Null<T>. *)
			let param_types_hx = match follow e_callee.etype with
				| TFun (params, _) ->
					List.map (fun (_, opt, t) ->
						let is_already_null = match follow t with
							| TAbstract ({ a_path = ([], "Null") }, _) -> true
							| _ -> false
						in
						if opt && not is_already_null then ectx.gctx.com.basic.tnull t else t
					) params
				| _ -> []
			in
			(* Convert args, handling null -> default(Null<T>) and type coercion *)
			let args_cs = List.mapi (fun i arg ->
				let is_null = match arg.eexpr with TConst TNull -> true | _ -> false in
				let expected_hx_type = if i < List.length param_types_hx then
					List.nth param_types_hx i
				else
					arg.etype
				in
				let expected_cs_type = cs_type_of_type ectx.gctx expected_hx_type in
				if is_null then begin
					(* Handle null specially *)
					match expected_cs_type with
					| CsTypeClass ((["haxe"; "lang"], "Null"), _) -> CsDefault expected_cs_type
					| CsTypeGenericParam _ -> CsDefault expected_cs_type
					| _ -> CsNull
				end else begin
					let cs_arg = cs_expr_of_texpr ectx arg in
					coerce_arg ectx.gctx cs_arg arg.etype expected_hx_type
				end
			) args in
			(* Get the return type and register the typed invoke signature *)
			let result_type = cs_type_of_type ectx.gctx e.etype in
			let param_types_cs = List.map (cs_type_of_type ectx.gctx) param_types_hx in
			(* Register this signature for later generation on Function class *)
			register_invoke_signature ectx.gctx param_types_cs result_type;
			(* Call the typed invoke method - all signatures use "invoke" name
			   with different parameter types (C# method overloading) *)
			let call_expr = CsCall (CsField (func, invoke_method_name (List.length args_cs)), args_cs) in
			(* The base class invokeN methods ALWAYS return object, so we need to cast
			   to the expected return type unless it's void or object *)
			begin match result_type with
			| CsTypeVoid -> call_expr  (* No cast needed for void *)
			| CsTypeObject | CsTypeDynamic -> call_expr  (* No cast needed for object/dynamic *)
			| _ -> CsCast (result_type, call_expr)  (* Cast to expected type *)
			end
		end
	| TNew ({ cl_path = (["cs"], "NativeArray") }, [t], [size_expr]) ->
		(* cs.NativeArray<T>(size) -> new T[size] *)
		let elem_type = cs_type_of_type ectx.gctx t in
		let size = cs_expr_of_texpr ectx size_expr in
		CsNewArraySize (elem_type, size)
	| TNew (c, params, args) ->
		(* Special handling for String - C# doesn't have new string(str) syntax *)
		begin match c.cl_path with
		| ([], ("String" | "string")) | (["haxe"; "root"], ("String" | "string")) ->
			(* new String(s) in Haxe just returns s - string is immutable *)
			begin match args with
			| [arg] -> cs_expr_of_texpr ectx arg
			| _ -> CsRaw "/* ERROR: String constructor with unexpected args */"
			end
		| _ ->
			let path = cs_path_of_path c.cl_path in
			let type_params = List.map (cs_type_of_type ectx.gctx) params in
			let args = List.map (cs_expr_of_texpr ectx) args in
			CsNew (CsTypeClass (path, type_params), args)
		end
	| TObjectDecl fields ->
		(* Create HaxeDynamicObject with initial field values using _hx_create *)
		if fields = [] then
			(* Empty object: just new HaxeDynamicObject() *)
			CsNew (CsTypeClass (NativeTypes.haxe_dynamic_object_path, []), [])
		else begin
			(* Non-empty: HaxeDynamicObject._hx_create(new object[] { "name1", val1, ... }) *)
			let field_args = List.fold_left (fun acc ((name, _, _), e) ->
				let name_expr = CsConst (CsConstString name) in
				let val_expr = cs_expr_of_texpr ectx e in
				val_expr :: name_expr :: acc
			) [] fields in
			let field_args = List.rev field_args in
			(* Create native object array with the field name/value pairs *)
			let array_expr = CsNewArray (CsTypeObject, field_args) in
			(* Wrap in haxe.root.Array<object>.ofNative for the Haxe Array type *)
			let haxe_array = CsStaticCall (CsTypeClass (NativeTypes.haxe_array_path, [CsTypeObject]), "ofNative", [array_expr]) in
			CsStaticCall (CsTypeClass (NativeTypes.haxe_dynamic_object_path, []), "_hx_create", [haxe_array])
		end
	| TArrayDecl items ->
		(* Array literal [] creates a haxe.root.Array<T>, not a native C# array *)
		let array_type = cs_type_of_type ectx.gctx e.etype in
		if items = [] then
			(* Empty array: new haxe.root.Array<T>() *)
			CsNew (array_type, [])
		else begin
			(* Non-empty array: new haxe.root.Array<T>(new T[] { ... }) or use ofNative *)
			let elem_type = match follow e.etype with
				| TInst (_, [t]) -> cs_type_of_type ectx.gctx t
				| _ -> CsTypeObject
			in
			let cs_items = List.map (cs_expr_of_texpr ectx) items in
			(* Create native array and wrap with ofNative *)
			CsStaticCall (array_type, "ofNative", [CsNewArray (elem_type, cs_items)])
		end
	| TTypeExpr mt ->
		(* Convert module type to Haxe type, then to C# type *)
		let t = type_of_module_type mt in
		let cs_type = cs_type_of_type ectx.gctx t in
		(* Erase type parameters to object - C# doesn't allow typeof(SomeType<T>)
		   unless T is in scope. We convert to typeof(SomeType<object>) *)
		let cs_type = CsSignature.erase_type_params cs_type in
		CsTypeOf cs_type
	| TParenthesis e ->
		CsParens (cs_expr_of_texpr ectx e)
	| TCast (inner_e, _) ->
		(* The target type is the outer expression's type (e.etype), not the inner expression's type *)
		(* C# doesn't allow casting to void, so just emit the inner expression *)
		if ExtType.is_void (follow e.etype) then
			cs_expr_of_texpr ectx inner_e
		else begin
			let target_type_raw = cs_type_of_type ectx.gctx e.etype in
			(* Function types map to haxe.lang.Function - no special handling needed. *)
			(* Fix spurious Null<Null<T>> from safe cast pattern (e.g., Std.downcast).
			   When target is Null<Null<T>> but inner type is NOT Null<T>, flatten to Null<T>.
			   This happens because the Haxe type system infers an extra Null wrapper in ternaries
			   where one branch is `cast value` and the other is `null`. *)
			let inner_type_raw = cs_type_of_type ectx.gctx inner_e.etype in
			let target_type = match target_type_raw, inner_type_raw with
				| CsTypeClass ((["haxe"; "lang"], "Null"), [CsTypeClass ((["haxe"; "lang"], "Null"), [_]) as inner_null]),
				  t when t <> inner_null && not (match t with CsTypeClass ((["haxe"; "lang"], "Null"), _) -> true | _ -> false) ->
					(* Target is Null<Null<T>>, inner is not a Null type - use just Null<T> *)
					inner_null
				| _ -> target_type_raw
			in
			(* Special case: casting null to a value type should use default(T), not (T)(null)
			   This handles @:fromNull abstracts where null converts to the default value *)
			let is_null_inner = match inner_e.eexpr with TConst TNull -> true | _ -> false in
			let is_value_or_null_type = match target_type with
				| CsTypeBool | CsTypeByte | CsTypeSByte | CsTypeChar
				| CsTypeShort | CsTypeUShort | CsTypeInt | CsTypeUInt
				| CsTypeLong | CsTypeULong | CsTypeFloat | CsTypeDouble | CsTypeDecimal -> true
				| CsTypeClass ((["haxe"; "lang"], "Null"), _) -> true  (* Null<T> is a struct *)
				| CsTypeGenericParam _ -> true  (* Type params may be value types *)
				| _ -> false
			in
			if is_null_inner && is_value_or_null_type then
				CsDefault target_type
			else begin
				let inner_cs = cs_expr_of_texpr ectx inner_e in
				let inner_type = cs_type_of_type ectx.gctx inner_e.etype in
				(* Special case: casting to Null<T> should use implicit conversion, not explicit cast.
				   Null<T> has an implicit conversion operator from T, so no cast needed.
				   Explicit cast through object fails for value types at runtime. *)
				let is_target_null_wrapper = match target_type with
					| CsTypeClass ((["haxe"; "lang"], "Null"), _) -> true
					| _ -> false
				in
				(* Check if inner type is the wrapped type of target Null<T> *)
				let is_wrapped_type_match = match target_type, inner_type with
					| CsTypeClass ((["haxe"; "lang"], "Null"), [wrapped]), t when wrapped = t -> true
					| CsTypeClass ((["haxe"; "lang"], "Null"), [CsTypeGenericParam _]), CsTypeGenericParam _ -> true
					| _ -> false
				in
				if is_target_null_wrapper && is_wrapped_type_match then
					(* Use implicit conversion - just return the inner expression as-is.
					   C#'s implicit operator will handle the conversion. *)
					inner_cs
				else begin
					(* C# doesn't allow direct casts between unrelated type parameters.
					   Cast through object: (Target)(object)source *)
					let needs_double_cast = match target_type, inner_type with
						| CsTypeGenericParam _, CsTypeGenericParam _ -> true  (* T to O *)
						| CsTypeGenericParam _, CsTypeClass _ -> true  (* SomeClass to T - needs (T)(object)v *)
						| CsTypeClass _, CsTypeGenericParam _ when not is_target_null_wrapper -> true  (* T to SomeClass - needs (SomeClass)(object)v, but not for Null<T> *)
						| CsTypeClass (_, tparams1), CsTypeClass (_, tparams2) when tparams1 <> [] && tparams2 <> [] ->
							(* Generic class to generic class - may need double cast if type args differ *)
							(* Only if they're not the exact same type *)
							target_type <> inner_type
						| _ -> false
					in
					if needs_double_cast then
						CsCast (target_type, CsCast (CsTypeObject, inner_cs))
					else
						CsCast (target_type, inner_cs)
				end
			end
		end
	| TThrow e ->
		CsThrow (cs_expr_of_texpr ectx e)
	| TMeta (_, e) ->
		cs_expr_of_texpr ectx e
	| TIf (cond, then_expr, Some else_expr) ->
		(* Check if this is a void expression - can't use ternary for void in C# *)
		if ExtType.is_void (follow e.etype) then begin
			(* Void if/else in expression context: wrap in immediately invoked Action *)
			(* ((Action)(() => { if (cond) { then } else { else } }))() *)
			let cond_cs = cs_expr_of_texpr ectx cond in
			let then_stmt = cs_stmt_of_texpr ectx then_expr in
			let else_stmt = cs_stmt_of_texpr ectx else_expr in
			let if_stmt = CsIf (cond_cs, then_stmt, Some else_stmt) in
			let lambda = CsLambda ([], CsLambdaBlock [if_stmt]) in
			let action_type = CsTypeAction [] in
			CsCall (CsCast (action_type, lambda), [])
		end else begin
			(* Ternary expression: cond ? then : else *)
			let cond = cs_expr_of_texpr ectx cond in
			let then_e = cs_expr_of_texpr ectx then_expr in
			let else_e = cs_expr_of_texpr ectx else_expr in
			CsTernary (cond, then_e, else_e)
		end
	| TBlock [] ->
		(* Empty block as expression - return default value of expected type *)
		CsDefault (cs_type_of_type ectx.gctx e.etype)
	| TBlock [single] ->
		(* Single expression block - just unwrap *)
		cs_expr_of_texpr ectx single
	| TBlock [first; { eexpr = TLocal v }] ->
		(* Common pattern from inline abstract constructors: { temp = expr; temp; }
		   Optimize to just the expression if first assigns to the same variable *)
		begin match first.eexpr with
		| TVar (v2, Some init) when v.v_id = v2.v_id ->
			(* { var temp = expr; temp; } -> just expr *)
			cs_expr_of_texpr ectx init
		| TBinop (OpAssign, { eexpr = TLocal v2 }, rhs) when v.v_id = v2.v_id ->
			(* { temp = expr; temp; } -> just expr *)
			cs_expr_of_texpr ectx rhs
		| _ ->
			(* Fall through to general block handling *)
			cs_expr_of_texpr_block ectx e [first; { eexpr = TLocal v; etype = v.v_type; epos = e.epos }]
		end
	| TBlock [{ eexpr = TVar (v1, None) }; { eexpr = TBinop (OpAssign, { eexpr = TLocal v2 }, rhs) }; last]
		when v1.v_id = v2.v_id ->
		(* Common pattern: { var temp; temp = expr; temp; } or { var temp; temp = expr; (T)temp; } -> just expr *)
		let last_var_id = match last.eexpr with
			| TLocal v3 -> Some v3.v_id
			| TCast ({ eexpr = TLocal v3 }, _) -> Some v3.v_id
			| TParenthesis { eexpr = TLocal v3 } -> Some v3.v_id
			| TParenthesis { eexpr = TCast ({ eexpr = TLocal v3 }, _) } -> Some v3.v_id
			| _ -> None
		in
		begin match last_var_id with
		| Some vid when vid = v2.v_id ->
			(* Optimize: just the rhs expression, possibly with a cast *)
			begin match last.eexpr with
			| TCast (_, _) | TParenthesis { eexpr = TCast (_, _) } ->
				(* Preserve the cast type from the last expression *)
				let target_type = cs_type_of_type ectx.gctx last.etype in
				CsCast (target_type, cs_expr_of_texpr ectx rhs)
			| _ ->
				cs_expr_of_texpr ectx rhs
			end
		| _ ->
			(* Fall through to general block handling *)
			cs_expr_of_texpr_block ectx e [{ eexpr = TVar (v1, None); etype = v1.v_type; epos = e.epos }; { eexpr = TBinop (OpAssign, { eexpr = TLocal v2; etype = v2.v_type; epos = e.epos }, rhs); etype = rhs.etype; epos = e.epos }; last]
		end
	| TBlock exprs ->
		cs_expr_of_texpr_block ectx e exprs
	| TIf _  (* TIf without else, handled as statement-as-expression *)
	| TWhile _
	| TSwitch _
	| TTry _ ->
		(* Statement used as expression - wrap in immediately invoked lambda *)
		(* ((Func<T>)(() => { <statement>; return <value>; }))() *)
		cs_expr_of_stmt_as_expr ectx e
	| TReturn _
	| TBreak
	| TContinue
	| TVar _ ->
		(* These shouldn't appear as expression values in normal code *)
		CsRaw (Printf.sprintf "/* Unsupported: %s as expression */" (match e.eexpr with
			| TReturn _ -> "TReturn"
			| TBreak -> "TBreak"
			| TContinue -> "TContinue"
			| TVar _ -> "TVar"
			| _ -> "Unknown"
		))
	| TFunction tf ->
		(* Generate closure class and return instantiation expression *)
		(* Following JVM's approach: every local function becomes a closure class *)
		!generate_closure_class_ref ectx tf e.etype
	| TEnumParameter (e, ef, i) ->
		(* Access enum constructor parameter - need to cast to the proper subclass *)
		(* Check if expression is Null<EnumType> - need to unwrap via .value *)
		let is_null_wrapper = find_null_in_expr e in
		let obj = cs_expr_of_texpr ectx e in
		(* Unwrap Null<T> to get the value before accessing enum parameter *)
		let obj = if is_null_wrapper then CsField (obj, "value") else obj in
		let param_name = match ef.ef_type with
			| TFun (args, _) when i < List.length args ->
				let (name, _, _) = List.nth args i in
				name
			| _ -> Printf.sprintf "_hx_p%d" i
		in
		(* Get the enum path and type params, then cast to the constructor subclass.
		   In C# nested classes don't re-declare parent type params, so the nested class
		   reference should be: OuterClass<T>.NestedClass, not OuterClass.NestedClass<T> *)
		let enum_path, enum_type_params = match follow e.etype with
			| TEnum (en, params) ->
				cs_path_of_path en.e_path, List.map (cs_type_of_type ectx.gctx) params
			| TAbstract ({ a_path = ([], "Null") }, [t]) ->
				(* Null<EnumType> - unwrap to get inner enum *)
				begin match follow t with
				| TEnum (en, params) ->
					cs_path_of_path en.e_path, List.map (cs_type_of_type ectx.gctx) params
				| _ -> ([], "object"), []
				end
			| _ -> ([], "object"), []
		in
		let ctor_name = escape_identifier ef.ef_name in
		(* For nested enum constructors, create a CsTypeNested: Parent<T>.NestedClass *)
		let parent_type = CsTypeClass (enum_path, enum_type_params) in
		let nested_type = CsTypeNested (parent_type, ctor_name) in
		let cast_expr = CsCast (nested_type, obj) in
		CsField (cast_expr, escape_identifier param_name)
	| TEnumIndex e ->
		(* For extern enums (C# native enums) or simple enums (generated as C# enums),
		   cast to int; otherwise access _hx_index *)
		(* Also check if expression is Null<EnumType> - need to unwrap via .value *)
		let is_null_wrapper = find_null_in_expr e in
		let is_simple_or_extern_enum = match follow e.etype with
			| TEnum (en, _) ->
				if has_enum_flag en EnExtern then true
				else
					(* Check if it's a "simple" enum - no type params and no constructors with params *)
					en.e_params = [] && PMap.fold (fun ef acc ->
						acc && (match ef.ef_type with TFun _ -> false | _ -> true)
					) en.e_constrs true
			| TAbstract ({ a_path = ([], "Null") }, [t]) ->
				(* Null<EnumType> - check inner type *)
				begin match follow t with
				| TEnum (en, _) ->
					if has_enum_flag en EnExtern then true
					else
						en.e_params = [] && PMap.fold (fun ef acc ->
							acc && (match ef.ef_type with TFun _ -> false | _ -> true)
						) en.e_constrs true
				| _ -> false
				end
			| _ -> false
		in
		let obj = cs_expr_of_texpr ectx e in
		(* Unwrap Null<T> to get the value before accessing enum index *)
		let obj = if is_null_wrapper then CsField (obj, "value") else obj in
		if is_simple_or_extern_enum then
			CsCast (CsTypeInt, obj)
		else
			CsField (obj, "_hx_index")
	| TIdent s ->
		CsLocal (escape_identifier s)

(* Helper to wrap a statement-as-expression in an immediately invoked lambda.
   Used for TIf, TSwitch, TTry, TWhile when they appear in expression context.
   Generates: ((Func<T>)(() => { <stmt as return>; }))() *)
and cs_expr_of_stmt_as_expr ectx e =
	let return_type = cs_type_of_type ectx.gctx e.etype in
	let is_void = ExtType.is_void (follow e.etype) in
	(* Convert the statement, but we need to extract the "value" from it.
	   For TTry, TIf, TSwitch, etc., the value is the last expression in each branch. *)
	let stmt_with_return = cs_stmt_with_return_inner ectx is_void e in
	let lambda = CsLambda ([], CsLambdaBlock [stmt_with_return]) in
	(* For void expressions, use Action instead of Func<void> *)
	let func_type = if is_void then CsTypeAction [] else CsTypeFunc ([], return_type) in
	CsCall (CsCast (func_type, lambda), [])

(* Convert a statement to have explicit returns for expression-as-statement conversion.
   This makes the last expression in each branch into a return statement.
   is_void: if true, don't generate return statements with values (just emit the statement) *)
and cs_stmt_with_return_inner ectx is_void e =
	match e.eexpr with
	| TTry (e1, catches) ->
		let try_body = cs_stmt_with_return_inner ectx is_void e1 in
		let catch_clauses = List.map (fun (v, catch_expr) ->
			let catch_body = cs_stmt_with_return_inner ectx is_void catch_expr in
			{
				catch_type = Some (cs_type_of_type ectx.gctx v.v_type);
				catch_name = Some (get_local_name ectx v);
				catch_when = None;
				catch_body = catch_body;
			}
		) catches in
		CsTry (try_body, catch_clauses, None)
	| TIf (cond, e_then, e_else_opt) ->
		let cond_cs = cs_expr_of_texpr ectx cond in
		let then_body = cs_stmt_with_return_inner ectx is_void e_then in
		let else_body = Option.map (cs_stmt_with_return_inner ectx is_void) e_else_opt in
		CsIf (cond_cs, then_body, else_body)
	| TSwitch sw ->
		let switch_expr = cs_expr_of_texpr ectx sw.switch_subject in
		let cs_sections = List.map (fun case ->
			let cs_labels = List.map (fun v -> CsCaseConst (cs_expr_of_texpr ectx v)) case.case_patterns in
			let case_body_with_return = cs_stmt_with_return_inner ectx is_void case.case_expr in
			{ sw_labels = cs_labels; sw_body = [case_body_with_return] }
		) sw.switch_cases in
		let cs_sections = match sw.switch_default with
			| Some def_expr ->
				let def_body = cs_stmt_with_return_inner ectx is_void def_expr in
				cs_sections @ [{ sw_labels = [CsCaseDefault]; sw_body = [def_body] }]
			| None -> cs_sections
		in
		CsSwitch (switch_expr, cs_sections)
	| TBlock exprs ->
		let (init_exprs, last_opt) = split_last exprs in
		let init_stmts = List.map (cs_stmt_of_texpr ectx) init_exprs in
		let final_stmt = match last_opt with
			| Some last_expr -> cs_stmt_with_return_inner ectx is_void last_expr
			| None -> if is_void then CsEmpty else CsReturn None
		in
		CsBlock (init_stmts @ [final_stmt])
	| TWhile _ ->
		(* While loops as expressions are unusual - just emit the loop *)
		if is_void then
			cs_stmt_of_texpr ectx e
		else
			CsBlock [cs_stmt_of_texpr ectx e; CsReturn (Some (CsDefault (cs_type_of_type ectx.gctx e.etype)))]
	| _ ->
		(* For simple expressions, return them (or just emit as statement if void) *)
		if is_void then
			CsExprStmt (cs_expr_of_texpr ectx e)
		else
			CsReturn (Some (cs_expr_of_texpr ectx e))

(* Helper to split list into init elements and last element *)
and split_last = function
	| [] -> ([], None)
	| [x] -> ([], Some x)
	| x :: xs -> let (rest, last) = split_last xs in (x :: rest, last)

(* Convert block expression to prefix statements + final expression value.
   This is used in statement contexts where we can emit: { stmts...; var x = finalExpr; }
   instead of wrapping in a lambda. *)
and cs_expr_with_prefix ectx e : cs_expr_result =
	match e.eexpr with
	| TBlock [] ->
		{ er_stmts = []; er_expr = CsDefault (cs_type_of_type ectx.gctx e.etype) }
	| TBlock [single] ->
		cs_expr_with_prefix ectx single
	| TBlock exprs ->
		let (init_exprs, last_opt) = split_last exprs in
		let init_stmts = List.map (cs_stmt_of_texpr ectx) init_exprs in
		begin match last_opt with
		| Some last_expr ->
			(* Recursively handle the last expression - it might also be a block *)
			let last_result = cs_expr_with_prefix ectx last_expr in
			{ er_stmts = init_stmts @ last_result.er_stmts; er_expr = last_result.er_expr }
		| None ->
			{ er_stmts = init_stmts; er_expr = CsDefault (cs_type_of_type ectx.gctx e.etype) }
		end
	| _ ->
		(* Not a block - just return the expression with no prefix statements *)
		{ er_stmts = []; er_expr = cs_expr_of_texpr ectx e }

(* Helper for block expressions that need lambda wrapping (used in pure expression context) *)
and cs_expr_of_texpr_block ectx e exprs =
	(* Block expression with multiple statements - use immediately invoked lambda *)
	(* (() => { stmt1; stmt2; return lastValue; })() *)
	let (init_exprs, last_opt) = split_last exprs in
	let init_stmts = List.map (cs_stmt_of_texpr ectx) init_exprs in
	let return_type = cs_type_of_type ectx.gctx e.etype in
	let body_stmts = match last_opt with
		| Some last_expr ->
			let last_cs = cs_expr_of_texpr ectx last_expr in
			init_stmts @ [CsReturn (Some last_cs)]
		| None ->
			init_stmts @ [CsReturn None]
	in
	let lambda = CsLambda ([], CsLambdaBlock body_stmts) in
	(* Cast to Func<T> and invoke: ((Func<T>)(() => { ... }))() *)
	let func_type = CsTypeFunc ([], return_type) in
	CsCall (CsCast (func_type, lambda), [])

(* Convert Haxe expression to C# statement - mutually recursive with cs_expr_of_texpr *)
and cs_stmt_of_texpr ectx e =
	match e.eexpr with
	| TBlock exprs ->
		(* Filter out pure expressions that would become invalid C# statements.
		   In C#, bare constants/locals can't be statements (CS0201 error). *)
		let stmts = List.filter_map (fun expr ->
			if is_pure_expr expr then None
			else Some (cs_stmt_of_texpr ectx expr)
		) exprs in
		CsBlock stmts
	| TVar (v, init) ->
		let name = get_local_name ectx v in
		let var_type_raw = cs_type_of_type ectx.gctx v.v_type in
		begin match init with
		| None -> CsVarDecl (name, Some var_type_raw, None)
		| Some init_expr ->
			(* Use cs_expr_with_prefix to handle block expressions smartly *)
			let result = cs_expr_with_prefix ectx init_expr in
			let init_cs = result.er_expr in
			(* NOTE: We intentionally do NOT use specific closure types for variable declarations.
			   While it would enable direct typed invoke() calls, it breaks when the variable
			   is reassigned to a different lambda (different closure class).
			   Example: var fn = () -> 1; fn = () -> 2;  // Would fail with specific types

			   TODO: Future optimization - detect variables that are never reassigned (SSA analysis)
			   and use specific closure types for those.
			   See plan: "CRITICAL: Typed Closure Variables and Direct invoke() Calls" *)
			let var_type = var_type_raw in
			(* NOTE: Do NOT use C#'s 'var' type inference for Dynamic-typed variables,
			   even when the initializer is inline C# code. The problem is that Dynamic
			   access (array indexing, property access) happens throughout the code, not
			   just at the initialization point. Use Cs.* runtime helpers (arrayGet,
			   readField, etc.) for dynamic access instead - this works uniformly for
			   all cases where the variable is typed as 'object'. *)
			(* Handle type conversions *)
			let init_type = cs_type_of_type ectx.gctx init_expr.etype in
			let init_cs = match init_type, var_type with
				| (CsTypeObject | CsTypeDynamic), (CsTypeInt | CsTypeLong | CsTypeFloat | CsTypeDouble | CsTypeBool | CsTypeString | CsTypeClass _) ->
					(* Dynamic -> specific type: need runtime cast *)
					CsCast (var_type, init_cs)
				| CsTypeClass ((["haxe"; "lang"], "Null"), _), (CsTypeObject | CsTypeDynamic) ->
					(* Null<T> -> object/Dynamic: unwrap via .value to get the inner value *)
					CsField (init_cs, "value")
				| _ -> init_cs
			in
			let decl_type = Some var_type in
			if result.er_stmts = [] then
				(* No prefix statements - just emit the variable declaration *)
				CsVarDecl (name, decl_type, Some init_cs)
			else
				(* Has prefix statements - emit:
				   T x;
				   {
				       prefix_stmts;
				       x = finalExpr;
				   }
				   This ensures the variable is accessible after the block. *)
				CsStmtList [
					CsVarDecl (name, Some var_type, None);
					CsBlock (result.er_stmts @ [CsExprStmt (CsBinop (CsOpAssign, CsLocal name, init_cs))])
				]
		end
	| TIf (cond, then_expr, else_expr) ->
		let cond = cs_expr_of_texpr ectx cond in
		let then_stmt = cs_stmt_of_texpr ectx then_expr in
		let else_stmt = Option.map (cs_stmt_of_texpr ectx) else_expr in
		CsIf (cond, then_stmt, else_stmt)
	| TWhile (cond, body, NormalWhile) ->
		let cond = cs_expr_of_texpr ectx cond in
		let body = cs_stmt_of_texpr ectx body in
		CsWhile (cond, body)
	| TWhile (cond, body, DoWhile) ->
		let cond = cs_expr_of_texpr ectx cond in
		let body = cs_stmt_of_texpr ectx body in
		CsDoWhile (body, cond)
	| TSwitch sw ->
		(* Check if any case patterns are non-constant expressions - these can't be used in C# switch cases
		   Non-constant includes: typeof, static field access on non-enum classes, etc. *)
		let rec is_non_constant_pattern p =
			match p.eexpr with
			| TTypeExpr _ -> true  (* typeof() is not constant *)
			| TField (_, FStatic (c, cf)) ->
				(* Static field access - in C#, only const fields are valid in switch cases.
				   Haxe 'static final' generates properties, not const, so they're not valid. *)
				begin match c.cl_kind with
				| KAbstractImpl _ -> false  (* Abstract impls with primitives are usually constant *)
				| _ ->
					(* Check if the field type is a reference type (class) - these are never const in C# *)
					begin match follow cf.cf_type with
					| TInst _ -> true  (* Reference type - can't be const *)
					| TAbstract ({ a_path = ([], ("Int" | "Float" | "Bool" | "String")) }, _) -> false  (* Primitives can be const *)
					| _ -> true  (* Default to non-constant for safety *)
					end
				end
			| TField (_, FEnum _) -> false  (* Enum values can be constant in some cases *)
			| TParenthesis e -> is_non_constant_pattern e
			| TCast (e, None) -> is_non_constant_pattern e
			| _ -> false
		in
		let has_non_constant_pattern = List.exists (fun case ->
			List.exists is_non_constant_pattern case.case_patterns
		) sw.switch_cases in
		if has_non_constant_pattern then begin
			(* Generate if-else chain instead of switch *)
			let cond_expr = sw.switch_subject in
			let rec build_if_chain cases =
				match cases with
				| [] ->
					begin match sw.switch_default with
					| Some e -> cs_stmt_of_texpr ectx e
					| None -> CsThrowStmt (CsNew (CsTypeClass ((["System"], "InvalidOperationException"), []),
						[CsConst (CsConstString "Unexpected value")]))
					end
				| case :: rest ->
					(* Build condition: cond == pattern1 || cond == pattern2 || ... *)
					let cond_cs = cs_expr_of_texpr ectx cond_expr in
					let conditions = List.map (fun p ->
						CsBinop (CsOpEq, cond_cs, cs_expr_of_texpr ectx p)
					) case.case_patterns in
					let combined_cond = match conditions with
						| [c] -> c
						| c :: rest -> List.fold_left (fun acc c -> CsBinop (CsOpOr, acc, c)) c rest
						| [] -> CsConst (CsConstBool true)
					in
					CsIf (combined_cond, cs_stmt_of_texpr ectx case.case_expr, Some (build_if_chain rest))
			in
			build_if_chain sw.switch_cases
		end else begin
			let cond = cs_expr_of_texpr ectx sw.switch_subject in
			let sections = List.map (fun case ->
				let labels = List.map (fun p ->
					CsCaseConst (cs_expr_of_texpr ectx p)
				) case.case_patterns in
				let body_stmts = [cs_stmt_of_texpr ectx case.case_expr; CsBreak] in
				{ sw_labels = labels; sw_body = body_stmts }
			) sw.switch_cases in
			let sections = match sw.switch_default with
				| Some e ->
					let default_section = {
						sw_labels = [CsCaseDefault];
						sw_body = [cs_stmt_of_texpr ectx e; CsBreak]
					} in
					sections @ [default_section]
				| None ->
					(* Even for exhaustive switches, C# needs a default case for definite assignment.
					   Throw an exception to satisfy the compiler while preserving safety. *)
					let default_section = {
						sw_labels = [CsCaseDefault];
						sw_body = [CsThrowStmt (CsNew (CsTypeClass ((["System"], "InvalidOperationException"), []),
							[CsConst (CsConstString "Unexpected value")]))]
					} in
					sections @ [default_section]
			in
			CsSwitch (cond, sections)
		end
	| TTry (body, catches) ->
		let body = cs_stmt_of_texpr ectx body in
		let catches = List.map (fun (v, e) ->
			let name = get_local_name ectx v in
			let cs_type = cs_type_of_type ectx.gctx v.v_type in
			let body = cs_stmt_of_texpr ectx e in
			{
				catch_type = Some cs_type;
				catch_name = Some name;
				catch_when = None;
				catch_body = body;
			}
		) catches in
		CsTry (body, catches, None)
	| TReturn None ->
		CsReturn None
	| TReturn (Some e) ->
		(* Check if we need to cast the return value to the method's return type *)
		(* Special case: returning null where return type is Null<T> needs default(Null<T>) *)
		let is_null_expr = match e.eexpr with TConst TNull -> true | _ -> false in
		let ret_is_null_type = match ectx.return_type with
			| Some ret_t ->
				begin match cs_type_of_type ectx.gctx ret_t with
				| CsTypeClass ((["haxe"; "lang"], "Null"), _) -> true
				| _ -> false
				end
			| None -> false
		in
		if is_null_expr && ret_is_null_type then
			let ret_cs = cs_type_of_type ectx.gctx (Option.get ectx.return_type) in
			CsReturn (Some (CsDefault ret_cs))
		else begin
			let cs_e = cs_expr_of_texpr ectx e in
			let is_dynamic t = match follow t with TDynamic _ -> true | _ -> false in
			let is_type_param t = match follow t with TInst ({ cl_kind = KTypeParameter _ }, _) -> true | _ -> false in
			(* Check if return type is Null<T> and expression could use implicit conversion *)
			let is_ret_null_wrapper = match ectx.return_type with
				| Some ret_t ->
					begin match cs_type_of_type ectx.gctx ret_t with
					| CsTypeClass ((["haxe"; "lang"], "Null"), _) -> true
					| _ -> false
					end
				| None -> false
			in
			let return_expr = match ectx.return_type with
				| Some ret_t when is_dynamic e.etype && not (is_dynamic ret_t) && not (ExtType.is_void (follow ret_t)) ->
					(* Returning Dynamic but method returns a concrete type (non-void) - add cast *)
					CsCast (cs_type_of_type ectx.gctx ret_t, cs_e)
				| Some ret_t when is_type_param e.etype && is_ret_null_wrapper ->
					(* Expression is T (type param) and return is Null<T> - use implicit conversion, don't cast *)
					cs_e
				| Some ret_t when is_type_param e.etype && is_type_param ret_t ->
					(* Both are type parameters but may be different (e.g., O vs T) - cast through object *)
					let expr_cs = cs_type_of_type ectx.gctx e.etype in
					let ret_cs = cs_type_of_type ectx.gctx ret_t in
					if expr_cs <> ret_cs then
						(* Different type params - cast O to object then to T *)
						CsCast (ret_cs, CsCast (CsTypeObject, cs_e))
					else
						cs_e
				| _ ->
					cs_e
			in
			CsReturn (Some return_expr)
		end
	| TBreak ->
		CsBreak
	| TContinue ->
		CsContinue
	| TThrow e ->
		CsThrowStmt (cs_expr_of_texpr ectx e)
	| _ ->
		(* Expression statement *)
		CsExprStmt (cs_expr_of_texpr ectx e)

(* Generate a closure class for a TFunction and return a CsNew expression to instantiate it.
   Following JVM's approach: every local function becomes a closure class with:
   - Fields for captured variables
   - Constructor to initialize captures
   - invoke method with the function body
   - invokeDynamic for dynamic dispatch
*)
let generate_closure_class ectx tf func_type =
	let gctx = ectx.gctx in

	(* Collect captured variables - but exclude the function's own parameters *)
	(* We need to wrap tf in a TFunction node so that collect_captured_vars
	   properly declares the parameters before traversing the body *)
	let fake_tfunc_expr = { eexpr = TFunction tf; etype = func_type; epos = tf.tf_expr.epos } in
	let captured_vars, accesses_this = Texpr.collect_captured_vars fake_tfunc_expr in

	(* Generate unique class name and path *)
	(* Closures are placed outside the namespace block, so they have an empty namespace path *)
	let closure_name = generate_closure_name gctx ectx in
	let closure_path = ([], closure_name) in

	(* Build capture fields - include 'this' if accessed *)
	let this_capture = if accesses_this then
		match ectx.current_class_path with
		| Some class_path ->
			let this_cs_type = CsTypeClass (cs_path_of_path class_path, []) in
			[("_hx_this", this_cs_type)]
		| None -> []
	else []
	in
	let var_captures = List.map (fun v ->
		(* escape_identifier adds @ prefix for keywords, but _hx_ prefix makes it safe anyway.
		   So just use the raw name with _hx_ prefix for field names. *)
		let raw_name = v.v_name in
		let safe_name = if String.length raw_name > 0 && raw_name.[0] = '@' then
			String.sub raw_name 1 (String.length raw_name - 1)
		else raw_name
		in
		let field_name = "_hx_" ^ safe_name in
		let cs_type = cs_type_of_type gctx v.v_type in
		(field_name, cs_type)
	) captured_vars in
	let all_captures = this_capture @ var_captures in

	(* Collect all type parameters used in captured variables' types, function parameters,
	   and return type. These need to be added as class-level type parameters for C#
	   since C# has reified generics (unlike JVM's type erasure). *)
	let captured_types = List.map snd var_captures in
	let param_types = List.filter_map (fun (v, _) ->
		if ExtType.is_void (follow v.v_type) then None
		else Some (cs_type_of_type gctx v.v_type)
	) tf.tf_args in
	let return_cs_type = cs_type_of_type gctx tf.tf_type in
	let all_types = captured_types @ param_types @ [return_cs_type] in
	let closure_type_params = List.fold_left CsSignature.collect_type_params [] all_types in
	(* Reverse to maintain order of first appearance *)
	let closure_type_params = List.rev closure_type_params in

	(* Build field definitions for captures *)
	let capture_fields = List.map (fun (name, cs_type) ->
		CsMemberField {
			f_name = name;
			f_type = cs_type;
			f_access = AccessModifier.Public;
			f_modifiers = [];
			f_value = None;
		}
	) all_captures in

	(* Build constructor parameters and body *)
	let ctor_params = List.map (fun (name, cs_type) ->
		{ p_name = name; p_type = Some cs_type; p_default = None; p_modifier = None }
	) all_captures in
	let ctor_body = List.map (fun (name, _) ->
		CsExprStmt (CsBinop (CsOpAssign, CsField (CsThis, name), CsLocal name))
	) all_captures in
	let ctor = CsMemberConstructor {
		ctor_access = AccessModifier.Public;
		ctor_modifiers = [];
		ctor_params = ctor_params;
		ctor_base_call = None;
		ctor_this_call = None;
		ctor_body = ctor_body;
	} in

	(* Build invoke method parameters and return type.
	   For optional parameters (those with default values), we need to wrap primitive types
	   with Null<T> to match the expected delegate signature. This is similar to how JVM
	   boxes optional primitive parameters (see genjvm.ml transform_arg). *)
	let invoke_params = List.filter_map (fun (v, default_opt) ->
		if ExtType.is_void (follow v.v_type) then None
		else
			let base_type = cs_type_of_type gctx v.v_type in
			(* If parameter has default value, wrap with Null<T> unless already wrapped *)
			let param_type = match default_opt with
				| Some _ ->
					begin match base_type with
					| CsTypeClass ((["haxe"; "lang"], "Null"), _) ->
						(* Already Null<T>, don't double-wrap *)
						base_type
					| _ ->
						(* Wrap with Null<T> *)
						CsTypeClass ((["haxe"; "lang"], "Null"), [base_type])
					end
				| None -> base_type
			in
			Some {
				p_name = escape_identifier v.v_name;
				p_type = Some param_type;
				p_default = None;
				p_modifier = None;
			}
	) tf.tf_args in
	let return_type = cs_type_of_type gctx tf.tf_type in

	(* Create a new expression context for the closure body *)
	(* Captured variables will be accessed via this.fieldName *)
	(* Inherit origin_class_path from parent context for nested closures to be grouped in same file *)
	let closure_ectx = {
		gctx = gctx;
		local_vars = [];
		used_names = [];
		temp_count = 0;
		return_type = Some tf.tf_type;
		current_class_path = Some closure_path;
		current_method_name = Some "invoke";
		origin_class_path = ectx.origin_class_path;  (* Inherit from parent for nested closures *)
		captured_vars = List.map (fun v -> v.v_id) captured_vars;
		captures_this = accesses_this;
	} in

	(* Register function parameters as local vars *)
	List.iter (fun (v, _) ->
		if not (ExtType.is_void (follow v.v_type)) then begin
			let name = escape_identifier v.v_name in
			closure_ectx.local_vars <- (v.v_id, name) :: closure_ectx.local_vars;
			closure_ectx.used_names <- name :: closure_ectx.used_names
		end
	) tf.tf_args;

	(* Register captured variables - they're accessed via this.fieldName in the closure *)
	List.iter (fun v ->
		(* Use same naming as var_captures - raw name with _hx_ prefix *)
		let raw_name = v.v_name in
		let safe_name = if String.length raw_name > 0 && raw_name.[0] = '@' then
			String.sub raw_name 1 (String.length raw_name - 1)
		else raw_name
		in
		let field_name = "_hx_" ^ safe_name in
		closure_ectx.local_vars <- (v.v_id, field_name) :: closure_ectx.local_vars
	) captured_vars;

	(* Generate invoke method body *)
	let invoke_body = match tf.tf_expr.eexpr with
		| TBlock exprs -> List.map (cs_stmt_of_texpr closure_ectx) exprs
		| _ -> [cs_stmt_of_texpr closure_ectx tf.tf_expr]
	in

	(* If the invoke method has no parameters, it shadows the base class's invoke() method.
	   Add the 'new' modifier to suppress CS0114 warning. *)
	let num_params = List.length invoke_params in
	let invoke_modifiers = if num_params = 0 then [MemberModifier.New] else [] in
	let invoke_method = CsMemberMethod {
		m_name = invoke_method_name num_params;
		m_return_type = return_type;
		m_access = AccessModifier.Public;
		m_modifiers = invoke_modifiers;
		m_type_params = [];
		m_params = invoke_params;
		m_body = Some invoke_body;
		m_constraints = [];
		m_explicit_interface = None;
		m_attributes = [];
	} in

	(* Build invokeDynamic method for dynamic calls *)
	(* Override Function.invokeDynamic(haxe.root.Array<object> args) *)
	(* For optional parameters, check if the argument was provided before accessing.
	   tf.tf_args has (var, default_opt) pairs where Some(_) means optional. *)
	let invoke_dynamic_body =
		(* Generate: return invoke((T0)args.__a[0], (T1)args.__a[1], ...);
		   But for optional parameters, use: args.length > i ? (T)args.__a[i] : default(Null<T>)
		   IMPORTANT: For Null<T> types, we need special handling:
		   - If arg is null, use default(Null<T>) which has hasValue=false
		   - If arg is present, cast to inner T and let implicit conversion make Null<T>
		   This prevents InvalidCastException when casting null to a value type. *)
		let args_array = CsField (CsLocal "args", "__a") in
		let args_length = CsField (CsLocal "args", "length") in
		(* Build list with (param, is_optional) *)
		let params_with_opt = List.filter_map (fun (v, default_opt) ->
			if ExtType.is_void (follow v.v_type) then None
			else Some (v, default_opt <> None)
		) tf.tf_args in
		let call_args = List.mapi (fun i (param, param_type) ->
			let (_, is_optional) = List.nth params_with_opt i in
			let idx_const = CsConst (CsConstInt (Int32.of_int i)) in
			let arg_access = CsArrayAccess (args_array, idx_const) in
			match param_type with
			| Some (CsTypeClass ((["haxe"; "lang"], "Null"), [inner_type]) as null_type) ->
				(* Null<T> type - use _ofDynamic to properly handle:
				   1. null → Null<T> with hasValue=false
				   2. boxed Null<T> with hasValue=false → Null<T> with hasValue=false
				   3. value → Null<T> with the value
				   Generate: Null<inner_type>._ofDynamic(args.__a[i])
				   For optional params: args.length > i ? Null<T>._ofDynamic(args.__a[i]) : default(Null<T>) *)
				let default_val = CsDefault null_type in
				(* Call Null<T>._ofDynamic(args.__a[i]) - static method on the Null<T> type *)
				let of_dynamic_call = CsStaticCall (null_type, "_ofDynamic", [arg_access]) in
				if is_optional then
					let length_check = CsBinop (CsOpGt, args_length, idx_const) in
					CsTernary (length_check, of_dynamic_call, default_val)
				else
					of_dynamic_call
			| Some t ->
				let casted = CsCast (t, arg_access) in
				if is_optional then
					let length_check = CsBinop (CsOpGt, args_length, idx_const) in
					CsTernary (length_check, casted, CsDefault t)
				else
					casted
			| None ->
				if is_optional then
					let length_check = CsBinop (CsOpGt, args_length, idx_const) in
					CsTernary (length_check, arg_access, CsNull)
				else
					arg_access
		) (List.combine invoke_params (List.map (fun p -> p.p_type) invoke_params)) in
		let invoke_call = CsCall (CsLocal (invoke_method_name num_params), call_args) in
		if return_type = CsTypeVoid then
			[CsExprStmt invoke_call; CsReturn (Some CsNull)]
		else
			[CsReturn (Some invoke_call)]
	in
	let haxe_array_object = CsTypeClass ((["haxe"; "root"], "Array"), [CsTypeObject]) in
	let invoke_dynamic_method = CsMemberMethod {
		m_name = "invokeDynamic";
		m_return_type = CsTypeObject;
		m_access = AccessModifier.Public;
		m_modifiers = [MemberModifier.Override];
		m_type_params = [];
		m_params = [{ p_name = "args"; p_type = Some haxe_array_object; p_default = None; p_modifier = None }];
		m_body = Some invoke_dynamic_body;
		m_constraints = [];
		m_explicit_interface = None;
		m_attributes = [];
	} in

	(* Build the closure class *)
	let closure_class = CsClassDef {
		c_path = closure_path;
		c_access = AccessModifier.Internal;
		c_modifiers = [TypeModifier.Sealed];
		c_type_params = closure_type_params;
		c_base = Some (CsTypeClass ((["haxe"; "lang"], "Function"), []));
		c_interfaces = [];
		c_constraints = [];
		c_members = capture_fields @ [ctor; invoke_method; invoke_dynamic_method];
	} in

	(* Add closure to the origin class's closure list *)
	(* Use origin_class_path if set (for nested closures), otherwise use current_class_path *)
	(* Convert to C# path format so it matches when we look up during file generation *)
	let file_class_path = match ectx.origin_class_path with
		| Some p -> cs_path_of_path p
		| None -> match ectx.current_class_path with
			| Some p -> cs_path_of_path p
			| None -> (["haxe"; "root"], "Anonymous")
	in
	add_closure_for_class gctx file_class_path closure_class;

	(* Generate instantiation expression *)
	(* Include type arguments if the closure class has type parameters *)
	let closure_type_args = List.map (fun name -> CsTypeGenericParam name) closure_type_params in
	let closure_type = CsTypeClass (closure_path, closure_type_args) in
	let capture_args =
		(if accesses_this then [CsThis] else []) @
		List.map (fun v ->
			(* Use original local name from parent context *)
			match List.assoc_opt v.v_id ectx.local_vars with
			| Some name -> CsLocal name
			| None -> CsLocal (escape_identifier v.v_name)
		) captured_vars
	in

	(* Check if the expected type or closure type uses type parameters that aren't in scope.
	   Type parameters are in scope if they're:
	   1. Class-level type parameters (from the enclosing class)
	   2. Method-level type parameters (from the enclosing method)

	   For closures generated within other closures, type parameters from the inner function
	   signature may not be in scope in the outer closure context. In such cases, we need to
	   erase those type parameters to 'object'.

	   We detect this by checking if the closure's type params differ from what's available
	   in the expression context. If we're inside a closure (captured_vars is set on ectx),
	   any type parameters that aren't from captured variables need erasure. *)
	let closure_type_params_set = closure_type_params in
	let captured_var_type_params = List.fold_left (fun acc (_, cs_type) ->
		CsSignature.collect_type_params acc cs_type
	) [] var_captures in
	(* Type params available in the instantiation context are those from captured variables *)
	let available_type_params = List.rev captured_var_type_params in
	(* Check which closure type params are NOT available (come from function signature only) *)
	let needs_erasure = List.exists (fun tp ->
		not (List.mem tp available_type_params)
	) closure_type_params_set in

	(* If any type params need erasure, replace unavailable ones with 'object' *)
	let closure_type_erased = if needs_erasure then
		(* Erase type params that aren't available in scope - replace with object *)
		let erased_type_args = List.map (fun name ->
			if List.mem name available_type_params then
				CsTypeGenericParam name  (* Keep this type param, it's in scope *)
			else
				CsTypeObject  (* Erase to object *)
		) closure_type_params in
		CsTypeClass (closure_path, erased_type_args)
	else
		closure_type
	in
	(* Return the closure instance directly - it extends haxe.lang.Function *)
	CsNew (closure_type_erased, capture_args)

(* Initialize the forward reference *)
let () = generate_closure_class_ref := generate_closure_class

(* Generate a closure class for a method reference (FClosure) and return a CsNew expression.
   This is used when a method is referenced but not called, e.g., `this.add` or `Calculator.staticAdd`.
   C# doesn't allow converting method groups to haxe.lang.Function directly, so we generate a wrapper class.

   Parameters:
   - ectx: expression context
   - obj_expr: Optional CsExpr for the object (None for static methods)
   - is_static: true for static methods
   - class_path: path of the class containing the method
   - type_params: type parameters applied to the class
   - cf: the class field (method) being referenced
   - method_type: the type of the method (TFun)
*)
let generate_method_closure ectx obj_expr is_static class_path type_params cf method_type =
	let gctx = ectx.gctx in

	(* Extract parameter and return types from method_type *)
	let param_info, return_type = match follow method_type with
		| TFun (args, ret) -> args, ret
		| _ -> [], t_dynamic
	in

	(* Generate unique class name *)
	let closure_name = generate_closure_name gctx ectx in
	let closure_path = ([], closure_name) in

	(* For instance methods, we need to capture the object *)
	let captures = if is_static then [] else
		match obj_expr with
		| Some _ ->
			let obj_cs_type = CsTypeClass (cs_path_of_path class_path, List.map (cs_type_of_type gctx) type_params) in
			[("_hx_this", obj_cs_type)]
		| None -> []
	in

	(* Build capture fields *)
	let capture_fields = List.map (fun (name, cs_type) ->
		CsMemberField {
			f_name = name;
			f_type = cs_type;
			f_access = AccessModifier.Public;
			f_modifiers = [];
			f_value = None;
		}
	) captures in

	(* Build constructor *)
	let ctor_params = List.map (fun (name, cs_type) ->
		{ p_name = name; p_type = Some cs_type; p_default = None; p_modifier = None }
	) captures in
	let ctor_body = List.map (fun (name, _) ->
		CsExprStmt (CsBinop (CsOpAssign, CsField (CsThis, name), CsLocal name))
	) captures in
	let ctor = CsMemberConstructor {
		ctor_access = AccessModifier.Public;
		ctor_modifiers = [];
		ctor_params = ctor_params;
		ctor_base_call = None;
		ctor_this_call = None;
		ctor_body = ctor_body;
	} in

	(* Build invoke method parameters - filter out Void parameters.
	   Keep track of which parameters are optional for invokeDynamic bounds checking. *)
	let invoke_params_with_opt = List.filter_map (fun (name, opt, t) ->
		if ExtType.is_void (follow t) then None
		else begin
			let base_type = cs_type_of_type gctx t in
			(* If parameter is optional, wrap with Null<T> unless already wrapped *)
			let param_type = if opt then
				match base_type with
				| CsTypeClass ((["haxe"; "lang"], "Null"), _) -> base_type
				| _ -> CsTypeClass ((["haxe"; "lang"], "Null"), [base_type])
			else base_type
			in
			Some ({
				p_name = escape_identifier name;
				p_type = Some param_type;
				p_default = None;
				p_modifier = None;
			}, opt)
		end
	) param_info in
	let invoke_params = List.map fst invoke_params_with_opt in
	let return_cs_type = cs_type_of_type gctx return_type in

	(* Build invoke method body - call the actual method *)
	let call_args = List.map (fun param -> CsLocal param.p_name) invoke_params in
	let method_name = escape_identifier cf.cf_name in
	let method_call = if is_static then
		let static_type = CsTypeClass (cs_path_of_path class_path, List.map (cs_type_of_type gctx) type_params) in
		CsStaticCall (static_type, method_name, call_args)
	else
		CsCall (CsField (CsField (CsThis, "_hx_this"), method_name), call_args)
	in
	let invoke_body = if return_cs_type = CsTypeVoid then
		[CsExprStmt method_call]
	else
		[CsReturn (Some method_call)]
	in

	(* If the invoke method has no parameters, it shadows the base class's invoke() method.
	   Add the 'new' modifier to suppress CS0114 warning. *)
	let num_params = List.length invoke_params in
	let invoke_modifiers = if num_params = 0 then [MemberModifier.New] else [] in
	let invoke_method = CsMemberMethod {
		m_name = invoke_method_name num_params;
		m_return_type = return_cs_type;
		m_access = AccessModifier.Public;
		m_modifiers = invoke_modifiers;
		m_type_params = [];
		m_params = invoke_params;
		m_body = Some invoke_body;
		m_constraints = [];
		m_explicit_interface = None;
		m_attributes = [];
	} in

	(* Build invokeDynamic method.
	   For Null<T> parameters, we need special handling:
	   - Use Null<T>._ofDynamic(arg) which properly handles:
	     1. null → Null<T> with hasValue=false
	     2. boxed Null<T> with hasValue=false → Null<T> with hasValue=false
	     3. value → Null<T> with the value
	   For optional parameters, check bounds before accessing. *)
	let invoke_dynamic_body =
		let args_array = CsField (CsLocal "args", "__a") in
		let args_length = CsField (CsLocal "args", "length") in
		let dyn_call_args = List.mapi (fun i (param, is_optional) ->
			let idx_const = CsConst (CsConstInt (Int32.of_int i)) in
			let arg_access = CsArrayAccess (args_array, idx_const) in
			match param.p_type with
			| Some (CsTypeClass ((["haxe"; "lang"], "Null"), [_inner_type]) as null_type) ->
				(* Null<T> type - use _ofDynamic for proper handling *)
				let of_dynamic_call = CsStaticCall (null_type, "_ofDynamic", [arg_access]) in
				if is_optional then
					(* For optional params, check bounds first *)
					let length_check = CsBinop (CsOpGt, args_length, idx_const) in
					CsTernary (length_check, of_dynamic_call, CsDefault null_type)
				else
					of_dynamic_call
			| Some t ->
				let casted = CsCast (t, arg_access) in
				if is_optional then
					let length_check = CsBinop (CsOpGt, args_length, idx_const) in
					CsTernary (length_check, casted, CsDefault t)
				else
					casted
			| None ->
				if is_optional then
					let length_check = CsBinop (CsOpGt, args_length, idx_const) in
					CsTernary (length_check, arg_access, CsNull)
				else
					arg_access
		) invoke_params_with_opt in
		let invoke_call = CsCall (CsLocal (invoke_method_name num_params), dyn_call_args) in
		if return_cs_type = CsTypeVoid then
			[CsExprStmt invoke_call; CsReturn (Some CsNull)]
		else
			[CsReturn (Some invoke_call)]
	in

	let invoke_dynamic = CsMemberMethod {
		m_name = "invokeDynamic";
		m_return_type = CsTypeObject;
		m_access = AccessModifier.Public;
		m_modifiers = [Override];
		m_type_params = [];
		m_params = [{ p_name = "args"; p_type = Some (CsTypeClass ((["haxe"; "root"], "Array"), [CsTypeObject])); p_default = None; p_modifier = None }];
		m_body = Some invoke_dynamic_body;
		m_constraints = [];
		m_explicit_interface = None;
		m_attributes = [];
	} in

	(* Build class definition *)
	let members = capture_fields @ [ctor; invoke_method; invoke_dynamic] in
	let closure_class = CsClassDef {
		c_path = closure_path;
		c_access = AccessModifier.Internal;
		c_modifiers = [Sealed];
		c_type_params = [];
		c_base = Some (CsTypeClass ((["haxe"; "lang"], "Function"), []));
		c_interfaces = [];
		c_members = members;
		c_constraints = [];
	} in

	(* Add closure to the origin class's list *)
	let file_class_path = match ectx.origin_class_path with
		| Some p -> cs_path_of_path p
		| None -> match ectx.current_class_path with
			| Some p -> cs_path_of_path p
			| None -> (["haxe"; "root"], "Main")
	in
	add_closure_for_class gctx file_class_path closure_class;

	(* Build instantiation expression *)
	let closure_type = CsTypeClass (closure_path, []) in
	let capture_args = match obj_expr with
		| Some expr when not is_static -> [expr]
		| _ -> []
	in
	CsNew (closure_type, capture_args)

(* Initialize the method closure forward reference *)
let () = generate_method_closure_ref := generate_method_closure

(* Generate method body *)
(* param_cs_names: optional list of C# parameter names (in order) from the method signature.
   This ensures the body uses the same parameter names as the C# method signature.
   Without this, abstract @this parameters may be named differently (e.g., "this1" in AST
   but "@this" in the signature). The mapping is by position.
   return_type: optional return type for the method, used to cast Dynamic return values
   class_path: optional path of the enclosing class (for closure naming)
   method_name: optional name of the enclosing method (for closure naming) *)
let generate_method_body gctx ?(param_cs_names=[]) ?return_type ?class_path ?method_name e =
	let ectx = create_expr_context gctx in
	ectx.return_type <- return_type;
	ectx.current_class_path <- class_path;
	ectx.current_method_name <- method_name;
	ectx.origin_class_path <- class_path;  (* Set origin for closure grouping *)
	match e.eexpr with
	| TFunction tf ->
		(* Unwrap TFunction - this happens for dynamic function assignments *)
		(* Register parameter names in context, using provided names when available (by position) *)
		let param_cs_names_array = Array.of_list param_cs_names in
		List.iteri (fun i (v, _) ->
			(* Use the C# name from the signature if available at this position *)
			let param_name =
				if i < Array.length param_cs_names_array then
					param_cs_names_array.(i)
				else
					escape_identifier v.v_name
			in
			(* Directly register this name for this variable id *)
			ectx.local_vars <- (v.v_id, param_name) :: ectx.local_vars;
			ectx.used_names <- param_name :: ectx.used_names
		) tf.tf_args;
		(* Set return type from TFunction if not provided *)
		if ectx.return_type = None then ectx.return_type <- Some tf.tf_type;
		(* Generate the inner body *)
		begin match tf.tf_expr.eexpr with
		| TBlock exprs -> List.map (cs_stmt_of_texpr ectx) exprs
		| _ -> [cs_stmt_of_texpr ectx tf.tf_expr]
		end
	| TBlock exprs ->
		List.map (cs_stmt_of_texpr ectx) exprs
	| _ ->
		[cs_stmt_of_texpr ectx e]

(* Check if a method overrides a parent class method *)
let is_override cf =
	has_class_field_flag cf CfOverride

(* Check if a non-static method should be marked virtual *)
let should_be_virtual c cf =
	(* Can't have virtual in a sealed/final class *)
	if has_class_flag c CFinal then false
	(* Can't mark as virtual if it's already an override or final *)
	else if has_class_field_flag cf CfFinal then false
	else if has_class_field_flag cf CfOverride then false
	else true

(* Find ALL interface methods that this field implements with different types.
   Returns a list of (interface_type, interface_args, interface_return_type, method_type_params)
   for each interface where the return type OR parameter types differ from the implementation.
   This is needed because C# doesn't support:
   - Covariant return types in interface implementations
   - Contravariant parameter types in interface implementations
   We need to generate explicit interface implementations for each such interface. *)
let find_variant_interface_methods gctx c cf =
	let rec check_interface acc map_parent (c_int, params) =
		(* First apply any parent type mapping to the params.
		   This is needed when traversing parent interfaces:
		   e.g., B<T> extends A<T>, and C implements B<int>
		   When checking A, params is [T] but we need [int]. *)
		let params = List.map map_parent params in
		let map_type = apply_params c_int.cl_params params in
		let acc = try
			let cf_int = PMap.find cf.cf_name c_int.cl_fields in
			match cf_int.cf_kind with
			| Method _ ->
				(* Found interface method with same name *)
				let int_type = map_type cf_int.cf_type in
				begin match follow int_type, follow cf.cf_type with
				| TFun (int_args, int_ret), TFun (impl_args, impl_ret) ->
					(* Check if return types differ (covariant return) *)
					let ret_differs = not (Type.type_iseq int_ret impl_ret) in
					(* Check if any parameter types differ (contravariant params) *)
					let args_differ =
						try
							List.exists2 (fun (_, _, int_t) (_, _, impl_t) ->
								not (Type.type_iseq int_t impl_t)
							) int_args impl_args
						with Invalid_argument _ -> true (* Different arg counts *)
					in
					if ret_differs || args_differ then
						(* Build the interface type with applied params *)
						let iface_cs_type = cs_type_of_type gctx (TInst (c_int, params)) in
						(* Get interface method's type parameters *)
						let method_type_params = List.map (fun ttp -> ttp.ttp_name) cf_int.cf_params in
						(iface_cs_type, int_args, int_ret, method_type_params) :: acc
					else
						acc
				| _ -> acc
				end
			| _ -> acc
		with Not_found -> acc
		in
		(* Also check parent interfaces - they may require explicit implementations too.
		   Pass map_type so parent interface params get properly substituted. *)
		List.fold_left (fun acc iface -> check_interface acc map_type iface) acc c_int.cl_implements
	in
	(* Check all implemented interfaces, starting with identity mapping *)
	List.fold_left (fun acc iface -> check_interface acc (fun t -> t) iface) [] c.cl_implements

(* Find ALL interface properties that this field implements with different types.
   Returns a list of (interface_type, interface_field_type, has_getter, has_setter)
   for each interface where the property type differs from the implementation.
   This is needed because C# requires exact type match for interface property implementations. *)
let find_variant_interface_properties gctx c cf =
	let rec check_interface acc map_parent (c_int, params) =
		let params = List.map map_parent params in
		let map_type = apply_params c_int.cl_params params in
		let acc = try
			let cf_int = PMap.find cf.cf_name c_int.cl_fields in
			match cf_int.cf_kind with
			| Var { v_read = int_read; v_write = int_write } ->
				(* Found interface property with same name *)
				let int_type = map_type cf_int.cf_type in
				let impl_type = cf.cf_type in
				(* Compare C# types, not Haxe types, because:
				   - Haxe abstracts may resolve to same underlying type
				   - Null<T> wrapper makes types different in C# but may be seen as same in Haxe
				   This ensures we detect when explicit interface implementation is needed. *)
				let int_cs_type = cs_type_of_type gctx int_type in
				let impl_cs_type = cs_type_of_type gctx impl_type in
				if int_cs_type <> impl_cs_type then
					let iface_cs_type = cs_type_of_type gctx (TInst (c_int, params)) in
					let has_getter = int_read = AccNormal || int_read = AccCall in
					let has_setter = int_write = AccNormal || int_write = AccCall in
					(iface_cs_type, int_type, has_getter, has_setter) :: acc
				else
					acc
			| _ -> acc
		with Not_found -> acc
		in
		List.fold_left (fun acc iface -> check_interface acc map_type iface) acc c_int.cl_implements
	in
	List.fold_left (fun acc iface -> check_interface acc (fun t -> t) iface) [] c.cl_implements

(* Generate explicit interface implementations for variant return/param types.
   For each interface method with different types, we generate:
   ReturnType InterfaceName.MethodName(params) { return this.MethodName(params); }
   This delegates to the actual implementation method.
   For each interface property with different types, we generate:
   InterfaceType InterfaceName.PropertyName { get => (InterfaceType)this.PropertyName; set => this.PropertyName = (ImplType)value; } *)
let generate_explicit_interface_impls gctx c cf =
	match cf.cf_kind with
	| Method MethNormal | Method MethInline ->
		let variant_interfaces = find_variant_interface_methods gctx c cf in
		List.map (fun (iface_type, int_args, int_ret, method_type_params) ->
			(* Generate params for the explicit implementation (no defaults needed) *)
			let params = List.filter_map (fun (n, _, t) ->
				if ExtType.is_void (follow t) then None
				else Some {
					p_name = escape_identifier n;
					p_type = Some (cs_type_of_type gctx t);
					p_default = None;
					p_modifier = None;
				}
			) int_args in
			(* Body: call this.MethodName(args) *)
			let arg_exprs = List.map (fun p -> CsLocal p.p_name) params in
			let call_expr = CsCall (CsField (CsThis, escape_identifier cf.cf_name), arg_exprs) in
			(* Use return for non-void, expression statement for void *)
			let is_void = ExtType.is_void (follow int_ret) in
			let body_stmt = if is_void then
				CsExprStmt call_expr
			else
				CsReturn (Some call_expr)
			in
			CsMemberMethod {
				m_name = escape_identifier cf.cf_name;
				m_return_type = cs_type_of_type gctx int_ret;
				m_access = AccessModifier.Public; (* ignored for explicit interface impl *)
				m_modifiers = [];
				m_type_params = method_type_params;
				m_params = params;
				m_body = Some [body_stmt];
				m_constraints = [];
				m_explicit_interface = Some iface_type;
				m_attributes = [];
			}
		) variant_interfaces
	| Var { v_read = impl_read; v_write = impl_write } ->
		let variant_interfaces = find_variant_interface_properties gctx c cf in
		List.map (fun (iface_type, int_type, has_getter, has_setter) ->
			let name = escape_identifier cf.cf_name in
			let int_cs_type = cs_type_of_type gctx int_type in
			let impl_cs_type = cs_type_of_type gctx cf.cf_type in
			(* Generate getter body: return (InterfaceType)this.PropertyName *)
			let getter = if has_getter && (impl_read = AccNormal || impl_read = AccCall) then
				Some {
					acc_access = None;
					acc_body = Some [CsReturn (Some (CsCast (int_cs_type, CsField (CsThis, name))))];
				}
			else
				None
			in
			(* Generate setter body: this.PropertyName = (ImplType)value *)
			let setter = if has_setter && (impl_write = AccNormal || impl_write = AccCall) then
				Some {
					acc_access = None;
					acc_body = Some [CsExprStmt (CsBinop (CsOpAssign,
						CsField (CsThis, name),
						CsCast (impl_cs_type, CsLocal "value")))];
				}
			else
				None
			in
			CsMemberProperty {
				prop_name = name;
				prop_type = int_cs_type;
				prop_access = AccessModifier.Public; (* ignored for explicit interface impl *)
				prop_modifiers = [];
				prop_getter = getter;
				prop_setter = setter;
				prop_init = None;
				prop_explicit_interface = Some iface_type;
			}
		) variant_interfaces
	| _ -> []

(* Check if a field implements an interface property.
   In C#, fields cannot implement interface properties - must use properties. *)
let field_implements_interface_property c cf =
	if cf.cf_name = "" then false
	else
		(* Check all interfaces the class implements *)
		List.exists (fun (iface, _) ->
			(* Look for a field with the same name in the interface *)
			try
				let iface_cf = PMap.find cf.cf_name iface.cl_fields in
				(* Check if it's a property (var with AccNormal access) *)
				match iface_cf.cf_kind with
				| Type.Var { v_read = AccNormal; _ } -> true
				| Type.Var { v_write = AccNormal; _ } -> true
				| _ -> false
			with Not_found -> false
		) c.cl_implements

(* Generate class field as C# member *)
let generate_field gctx c cf is_static =
	(* Use get_cs_field_name which handles C# restriction where member names
	   cannot be the same as the enclosing type name. *)
	let name = get_cs_field_name c cf in
	let cs_type = cs_type_of_type gctx cf.cf_type in
	let modifiers = if is_static then [MemberModifier.Static] else [] in

	match cf.cf_kind with
	| Var { v_read = AccNormal; v_write = AccNormal } ->
		(* Simple read/write field.
		   Generate as property if implementing an interface (C# fields can't implement interface properties).
		   Otherwise generate as plain field for performance. *)
		let init = match cf.cf_expr with
			| Some e when not (expr_contains_this e) ->
				let ectx = create_expr_context gctx in
				ectx.current_class_path <- Some c.cl_path;
				ectx.current_method_name <- Some cf.cf_name;
				Some (cs_expr_of_texpr ectx e)
			| _ -> None
		in
		if field_implements_interface_property c cf then
			Some (CsMemberProperty {
				prop_name = name;
				prop_type = cs_type;
				prop_access = AccessModifier.Public;
				prop_modifiers = modifiers;
				prop_getter = Some { acc_access = None; acc_body = None };
				prop_setter = Some { acc_access = None; acc_body = None };
				prop_init = init;
				prop_explicit_interface = None;
			})
		else
			Some (CsMemberField {
				f_name = name;
				f_type = cs_type;
				f_access = AccessModifier.Public;
				f_modifiers = modifiers;
				f_value = init;
			})
	| Var { v_read = AccNormal; v_write = AccNever } ->
		(* Read-only field.
		   Generate as property if implementing an interface, otherwise as plain field. *)
		let init = match cf.cf_expr with
			| Some e when not (expr_contains_this e) ->
				let ectx = create_expr_context gctx in
				ectx.current_class_path <- Some c.cl_path;
				ectx.current_method_name <- Some cf.cf_name;
				Some (cs_expr_of_texpr ectx e)
			| _ -> None
		in
		if field_implements_interface_property c cf then
			Some (CsMemberProperty {
				prop_name = name;
				prop_type = cs_type;
				prop_access = AccessModifier.Public;
				prop_modifiers = modifiers;
				prop_getter = Some { acc_access = None; acc_body = None };
				prop_setter = None;  (* Read-only *)
				prop_init = init;
				prop_explicit_interface = None;
			})
		else
			Some (CsMemberField {
				f_name = name;
				f_type = cs_type;
				f_access = AccessModifier.Public;
				f_modifiers = modifiers;
				f_value = init;
			})
	| Var { v_read = AccCall; _ } | Var { v_write = AccCall; _ } ->
		(* Property with custom getter/setter.
		   The getter/setter methods (get_xxx, set_xxx) are generated separately
		   as methods. In C#, auto-properties reserve the get_/set_ method names,
		   so we can't have both an auto-property AND explicit getter/setter methods.
		   However, if this is a physical field (like var x(get,null) where the
		   backing field is writable inside the class), we need to generate it. *)
		if is_physical_var_field cf then
			(* C# field initializers cannot use 'this', so skip initializer if it contains 'this'.
			   The initialization will be moved to the constructor by generate_class. *)
			let value = match cf.cf_expr with
				| Some e when not (expr_contains_this e) ->
					let ectx = create_expr_context gctx in
					ectx.current_class_path <- Some c.cl_path;
					ectx.current_method_name <- Some cf.cf_name;
					Some (cs_expr_of_texpr ectx e)
				| _ -> None
			in
			Some (CsMemberField {
				f_name = name;
				f_type = cs_type;
				f_access = AccessModifier.Public;
				f_modifiers = modifiers;
				f_value = value;
			})
		else
			None
	| Var _ ->
		(* Simple property with no custom accessors *)
		Some (CsMemberProperty {
			prop_name = name;
			prop_type = cs_type;
			prop_access = AccessModifier.Public;
			prop_modifiers = modifiers;
			prop_getter = Some { acc_access = None; acc_body = None };
			prop_setter = Some { acc_access = None; acc_body = None };
			prop_init = None;
			prop_explicit_interface = None;
		})
	| Method MethNormal | Method MethInline ->
		(* Regular method *)
		let args, ret = match follow cf.cf_type with
			| TFun (args, ret) -> args, ret
			| _ -> [], cf.cf_type
		in
		(* For override methods, we need to use the parent's parameter types
		   to ensure C# compatibility. C# requires exact type match for overrides. *)
		let args, ret =
			if not is_static && is_override cf then
				let rec find_parent_types c_super tl =
					let map_type = apply_params c_super.cl_params tl in
					try
						let cf_super = PMap.find cf.cf_name c_super.cl_fields in
						match cf_super.cf_kind with
						| Method _ ->
							begin match follow (map_type cf_super.cf_type) with
							| TFun (parent_args, parent_ret) -> Some (parent_args, parent_ret)
							| _ -> None
							end
						| _ -> None
					with Not_found ->
						match c_super.cl_super with
						| Some (grandparent, tl2) -> find_parent_types grandparent (List.map map_type tl2)
						| None -> None
				in
				match c.cl_super with
				| Some (c_super, tl) ->
					begin match find_parent_types c_super tl with
					| Some (parent_args, parent_ret) -> parent_args, parent_ret
					| None -> args, ret
					end
				| None -> args, ret
			else
				args, ret
		in
		(* C# requires that all optional parameters come after all required ones.
		   Mark which optional params can have defaults (only trailing optionals).
		   If an optional param has a required param after it, it cannot have a default. *)
		let args_with_index = List.mapi (fun i arg -> (i, arg)) args in
		let last_required_index = List.fold_left (fun acc (i, (_, opt, _)) ->
			if not opt then i else acc
		) (-1) args_with_index in
		(* Filter out Void parameters - C# doesn't allow 'void' as a parameter type *)
		let filtered_args = List.filter (fun (_, _, t) -> not (ExtType.is_void (follow t))) args in
		(* Build list of C# parameter names in order.
		   This is needed because abstract methods have @this parameters where the
		   original name is "this" but gets escaped to "@this" in C#.
		   The body expression may use a renamed variable (e.g., "this1") which
		   needs to map to the escaped parameter name "@this". *)
		let param_cs_names = List.map (fun (n, _, _) ->
			escape_identifier n
		) filtered_args in
		let params = List.mapi (fun i (n, opt, t) ->
			let param_type = cs_type_of_type gctx t in
			(* Only add default for trailing optionals (after last required param) *)
			let default_val = if opt && i > last_required_index then
				(* For haxe.lang.Null<T> (struct), must use default instead of null *)
				Some (CsDefault param_type)
			else
				None
			in
			{
				p_name = escape_identifier n;
				p_type = Some param_type;
				p_default = default_val;
				p_modifier = None;
			}
		) filtered_args in
		let body = match cf.cf_expr with
			| Some e -> Some (generate_method_body gctx ~param_cs_names ~return_type:ret ~class_path:c.cl_path ~method_name:cf.cf_name e)
			| None -> None
		in
		(* Extract method-level type parameters from cf.cf_params *)
		let explicit_type_params = List.map (fun ttp -> ttp.ttp_name) cf.cf_params in
		(* Get class type parameters to exclude from method type params *)
		let class_type_params = List.map (fun ttp -> ttp.ttp_name) c.cl_params in
		(* Also collect type parameters used in the method signature.
		   This is needed for _Impl_ classes where the abstract's type params
		   become method-level params (like Rest<T>.append uses T but _Impl_ has no class params).
		   However, we must exclude type params that belong to the enclosing class. *)
		let return_cs_type = cs_type_of_type gctx ret in
		let param_cs_types = List.map (fun p -> p.p_type) params in
		let param_cs_types = List.filter_map (fun t -> t) param_cs_types in
		let inferred_type_params = CsSignature.get_method_type_params param_cs_types return_cs_type in
		(* Filter out class type params from inferred params *)
		let inferred_type_params = List.filter (fun p ->
			not (List.mem p class_type_params)
		) inferred_type_params in
		(* Combine explicit and inferred params, explicit first, avoiding duplicates *)
		let method_type_params = explicit_type_params @ (List.filter (fun p ->
			not (List.mem p explicit_type_params)
		) inferred_type_params) in
		(* Add virtual/override/abstract modifiers for instance methods *)
		let method_modifiers =
			if is_static then modifiers
			else if is_override cf then modifiers @ [MemberModifier.Override]
			(* Methods without a body should be abstract, not virtual *)
			else if body = None then modifiers @ [MemberModifier.Abstract]
			else if should_be_virtual c cf then modifiers @ [MemberModifier.Virtual]
			else modifiers
		in
		(* Extract @:csAttribute metadata for C# attributes *)
		let attributes = List.filter_map (fun (m, args, _) ->
			match m with
			| Meta.Custom ":csAttribute" ->
				begin match args with
				| [(EConst (String (name, _)), _)] ->
					Some { attr_name = name; attr_args = [] }
				| [(EConst (String (name, _)), _); (EArrayDecl arr, _)] ->
					let args = List.filter_map (fun e ->
						match e with
						| (EConst (String (s, _)), _) -> Some ("\"" ^ s ^ "\"")
						| _ -> None
					) arr in
					Some { attr_name = name; attr_args = args }
				| _ -> None
				end
			| _ -> None
		) cf.cf_meta in
		Some (CsMemberMethod {
			m_name = name;
			m_return_type = cs_type_of_type gctx ret;
			m_access = AccessModifier.Public;
			m_modifiers = method_modifiers;
			m_type_params = method_type_params;
			m_params = params;
			m_body = body;
			m_constraints = [];
			m_explicit_interface = None;
			m_attributes = attributes;
		})
	| Method MethDynamic ->
		(* Dynamic method - generate as haxe.lang.Function field
		   cs_type_of_type handles TFun -> haxe.lang.Function *)
		(* C# field initializers cannot use 'this', so skip initializer if it contains 'this'.
		   The initialization will be moved to the constructor by generate_cs_class. *)
		let value = match cf.cf_expr with
			| Some e when not (expr_contains_this e) ->
				let ectx = create_expr_context gctx in
				ectx.current_class_path <- Some c.cl_path;
				ectx.current_method_name <- Some cf.cf_name;
				Some (cs_expr_of_texpr ectx e)
			| _ -> None
		in
		Some (CsMemberField {
			f_name = name;
			f_type = cs_type;  (* cs_type_of_type handles TFun -> delegate type *)
			f_access = AccessModifier.Public;
			f_modifiers = modifiers;
			f_value = value;
		})
	| Method MethMacro ->
		(* Macro method - skip *)
		None

(* Extract super() call and remaining body from constructor expression *)
(* Returns (Some super_args, rest) if super call found, (None, body) otherwise *)
let rec extract_super_call e =
	match e.eexpr with
	| TCall ({ eexpr = TConst TSuper }, args) ->
		(* Found super call - return args and empty body *)
		(Some args, None)
	| TBlock el ->
		(* Look for super call in block - it should be the first statement *)
		extract_super_from_block el
	| TFunction tf ->
		(* Unwrap TFunction and look inside *)
		extract_super_call tf.tf_expr
	| _ ->
		(None, Some e)

and extract_super_from_block el =
	match el with
	| [] -> (None, None)
	| e :: rest ->
		begin match e.eexpr with
		| TCall ({ eexpr = TConst TSuper }, args) ->
			(* Found super call - return args and remaining statements *)
			let rest_expr = if rest = [] then None else Some { e with eexpr = TBlock rest } in
			(Some args, rest_expr)
		| TBlock inner ->
			(* Nested block - look inside *)
			let (super_args, inner_rest) = extract_super_from_block inner in
			begin match super_args with
			| Some args ->
				let rest_block = match inner_rest with
					| None -> rest
					| Some r -> r :: rest
				in
				let rest_expr = if rest_block = [] then None else Some { e with eexpr = TBlock rest_block } in
				(Some args, rest_expr)
			| None ->
				let rest_expr = Some { e with eexpr = TBlock el } in
				(None, rest_expr)
			end
		| _ ->
			(* Not a super call - return full block *)
			(None, Some { e with eexpr = TBlock el })
		end

(* Generate constructor - follows legacy C# target pattern *)
(* Constructor body goes directly in constructor, super() becomes : base() initializer *)
(* field_init_stmts: additional statements to prepend to constructor body,
   typically for field initializers that contain 'this' and cannot be in C# field declarations *)
let generate_constructor gctx c cf field_init_stmts =
	let args = match follow cf.cf_type with
		| TFun (args, _) -> args
		| _ -> []
	in
	(* C# requires that all optional parameters come after all required ones.
	   Mark which optional params can have defaults (only trailing optionals). *)
	let args_with_index = List.mapi (fun i arg -> (i, arg)) args in
	let last_required_index = List.fold_left (fun acc (i, (_, opt, _)) ->
		if not opt then i else acc
	) (-1) args_with_index in
	(* Parameters for the constructor - use actual types *)
	let ctor_params = List.mapi (fun i (n, opt, t) ->
		let param_type = cs_type_of_type gctx t in
		(* Only add default for trailing optionals (after last required param) *)
		let default_val = if opt && i > last_required_index then
			(* For haxe.lang.Null<T> (struct), must use default instead of null *)
			Some (CsDefault param_type)
		else
			None
		in
		{
			p_name = escape_identifier n;
			p_type = Some param_type;
			p_default = default_val;
			p_modifier = None;
		}
	) args in
	(* Extract super call and body from constructor expression *)
	let (super_args, body_expr) = match cf.cf_expr with
		| Some e -> extract_super_call e
		| None -> (None, None)
	in
	(* Get base class constructor types for casting super args *)
	let base_ctor_types = match c.cl_super with
		| Some (sc, _) ->
			begin match sc.cl_constructor with
			| Some ctor_cf ->
				begin match follow ctor_cf.cf_type with
				| TFun (base_args, _) -> List.map (fun (_, _, t) -> cs_type_of_type gctx t) base_args
				| _ -> []
				end
			| None -> []
			end
		| None -> []
	in
	(* Convert super args to base call, casting to expected types *)
	let base_call = match super_args with
		| Some args ->
			let ectx = create_expr_context gctx in
			ectx.current_class_path <- Some c.cl_path;
			ectx.current_method_name <- Some "new";
			let cs_args = List.mapi (fun i arg ->
				let cs_arg = cs_expr_of_texpr ectx arg in
				(* Cast to expected type if we know it *)
				if i < List.length base_ctor_types then
					let expected_type = List.nth base_ctor_types i in
					CsCast (expected_type, CsParens cs_arg)
				else
					cs_arg
			) args in
			Some cs_args
		| None -> None
	in
	(* Generate constructor body - prepend field initializations that contain 'this' *)
	let ctor_body = match body_expr with
		| Some e -> field_init_stmts @ generate_method_body gctx ~class_path:c.cl_path ~method_name:"new" e
		| None -> field_init_stmts
	in
	[
		CsMemberConstructor {
			ctor_access = AccessModifier.Public;
			ctor_modifiers = [];
			ctor_params = ctor_params;
			ctor_base_call = base_call;
			ctor_this_call = None;
			ctor_body = ctor_body;
		}
	]

(* Check if a class inherits from HaxeObject (directly or through Haxe superclass chain) *)
let rec extends_haxe_object c =
	match c.cl_super with
	| None -> true  (* No superclass means we add HaxeObject as base, so it inherits from HaxeObject *)
	| Some (sc, _) ->
		(* Check if the superclass is a native extern class or has @:native metadata *)
		if has_class_flag sc CExtern then
			false  (* Extern classes don't inherit from HaxeObject *)
		else if Meta.has Meta.Native sc.cl_meta then
			false  (* Classes with @:native map to native C# classes, not HaxeObject *)
		else
			extends_haxe_object sc  (* Check the superclass *)

(* Generate _hx_getField, _hx_setField, _hx_getFields override methods for AOT compatibility *)
let generate_field_accessors gctx c =
	(* Only generate field accessors if the class inherits from HaxeObject *)
	if not (extends_haxe_object c) then
		[]
	else
		(* Get list of instance fields with their native names *)
		let instance_fields = List.filter_map (fun cf ->
			match cf.cf_kind with
			| Var { v_read = AccNormal; v_write = AccNormal }
			| Var { v_read = AccNormal; v_write = AccNever } ->
				Some (get_native_field_name cf, cs_type_of_type gctx cf.cf_type)
			| _ -> None
		) c.cl_ordered_fields in

		if instance_fields = [] then
			[]  (* No fields, no need for accessors *)
		else
		let field_names = List.map fst instance_fields in

		(* Generate _hx_getField override:
		   public override object _hx_getField(string name) {
		       switch (name) {
		           case "field1": return this.field1;
		           ...
		           default: return base._hx_getField(name);
		       }
		   }
		*)
		let get_field_sections = List.map (fun (name, _) ->
			{
				sw_labels = [CsCaseConst (CsConst (CsConstString name))];
				sw_body = [CsReturn (Some (CsField (CsThis, name)))];
			}
		) instance_fields in
		let get_field_default = {
			sw_labels = [CsCaseDefault];
			sw_body = [CsReturn (Some (CsCall (CsField (CsBase, "_hx_getField"), [CsLocal "name"])))];
		} in
		let get_field_method = CsMemberMethod {
			m_name = "_hx_getField";
			m_return_type = CsTypeObject;
			m_access = AccessModifier.Public;
			m_modifiers = [MemberModifier.Override];
			m_type_params = [];
			m_params = [{ p_name = "name"; p_type = Some CsTypeString; p_default = None; p_modifier = None }];
			m_body = Some [CsSwitch (CsLocal "name", get_field_sections @ [get_field_default])];
			m_constraints = [];
			m_explicit_interface = None;
			m_attributes = [];
		} in

		(* Generate _hx_setField override:
		   public override void _hx_setField(string name, object value) {
		       switch (name) {
		           case "field1": this.field1 = (FieldType)value; return;
		           ...
		           default: base._hx_setField(name, value); return;
		       }
		   }
		*)
		let set_field_sections = List.map (fun (name, field_type) ->
			{
				sw_labels = [CsCaseConst (CsConst (CsConstString name))];
				sw_body = [
					CsExprStmt (CsBinop (CsOpAssign, CsField (CsThis, name), CsCast (field_type, CsLocal "value")));
					CsReturn None;
				];
			}
		) instance_fields in
		let set_field_default = {
			sw_labels = [CsCaseDefault];
			sw_body = [
				CsExprStmt (CsCall (CsField (CsBase, "_hx_setField"), [CsLocal "name"; CsLocal "value"]));
				CsReturn None;
			];
		} in
		let set_field_method = CsMemberMethod {
			m_name = "_hx_setField";
			m_return_type = CsTypeVoid;
			m_access = AccessModifier.Public;
			m_modifiers = [MemberModifier.Override];
			m_type_params = [];
			m_params = [
				{ p_name = "name"; p_type = Some CsTypeString; p_default = None; p_modifier = None };
				{ p_name = "value"; p_type = Some CsTypeObject; p_default = None; p_modifier = None };
			];
			m_body = Some [CsSwitch (CsLocal "name", set_field_sections @ [set_field_default])];
			m_constraints = [];
			m_explicit_interface = None;
			m_attributes = [];
		} in

		(* Generate _hx_getFields override:
		   public override Array<string> _hx_getFields() {
		       return new Array<string>(new string[] { "field1", "field2", ... });
		   }
		*)
		let field_name_exprs = List.map (fun name -> CsConst (CsConstString name)) field_names in
		let array_type = CsTypeClass ((["haxe"; "root"], "Array"), [CsTypeString]) in
		let get_fields_method = CsMemberMethod {
			m_name = "_hx_getFields";
			m_return_type = array_type;
			m_access = AccessModifier.Public;
			m_modifiers = [MemberModifier.Override];
			m_type_params = [];
			m_params = [];
			(* Create Array<string> from native array: Array<string>.ofNative(new string[] {...}) *)
			m_body = Some [CsReturn (Some (CsStaticCallGeneric (array_type, "ofNative", [CsTypeString], [CsNewArray (CsTypeString, field_name_exprs)])))];
			m_constraints = [];
			m_explicit_interface = None;
			m_attributes = [];
		} in

		[get_field_method; set_field_method; get_fields_method]

(* Generate C# class from Haxe class *)
let generate_class gctx c =
	let path = cs_path_of_path c.cl_path in

	(* Determine modifiers *)
	let modifiers =
		(if (has_class_flag c CFinal) then [TypeModifier.Sealed] else []) @
		(if (has_class_flag c CAbstract) then [TypeModifier.Abstract] else [])
	in

	(* Generate base class reference *)
	(* If no explicit superclass, use HaxeObject as the base class for dynamic field support *)
	let base_class = match c.cl_super with
		| Some (sc, params) ->
			let path = cs_path_of_path sc.cl_path in
			let params = List.map (cs_type_of_type gctx) params in
			Some (CsTypeClass (path, params))
		| None ->
			(* All Haxe classes extend HaxeObject for _hx_getField support *)
			Some (CsTypeClass ((["haxe"; "root"], "HaxeObject"), []))
	in

	(* Generate interface references *)
	let interfaces = List.map (fun (i, params) ->
		let path = cs_path_of_path i.cl_path in
		let params = List.map (cs_type_of_type gctx) params in
		CsTypeClass (path, params)
	) c.cl_implements in

	(* Generate members *)
	let members = ref [] in

	(* Collect field initializations that contain 'this' - these must go in constructor *)
	let field_init_stmts = List.filter_map (fun cf ->
		match cf.cf_kind with
		| Var { v_read = AccNormal; _ } | Var { v_write = AccNormal; _ }
		| Method MethDynamic ->
			begin match cf.cf_expr with
			| Some e when expr_contains_this e ->
				let ectx = create_expr_context gctx in
				ectx.current_class_path <- Some c.cl_path;
				ectx.current_method_name <- Some cf.cf_name;
				let name = get_cs_field_name c cf in
				let cs_expr = cs_expr_of_texpr ectx e in
				Some (CsExprStmt (CsBinop (CsOpAssign, CsField (CsThis, name), cs_expr)))
			| _ -> None
			end
		| _ -> None
	) c.cl_ordered_fields in

	(* Constructor - returns a list of members (constructor + _hx_ctor method) *)
	begin match c.cl_constructor with
	| Some cf -> members := generate_constructor gctx c cf field_init_stmts @ !members
	| None ->
		(* When a class doesn't have its own constructor but has a parent with a constructor,
		   we need to generate constructors that match the parent's signature so callers can
		   pass arguments (e.g., Exception(message, previous, native)). *)
		let parent_ctor_args = match c.cl_super with
			| Some (sc, _) ->
				begin match sc.cl_constructor with
				| Some ctor_cf ->
					begin match follow ctor_cf.cf_type with
					| TFun (args, _) -> args
					| _ -> []
					end
				| None -> []
				end
			| None -> []
		in
		let has_required_parent_args = List.exists (fun (_, opt, _) -> not opt) parent_ctor_args in
		(* Generate forwarding constructor that takes the same params as parent *)
		if parent_ctor_args <> [] then begin
			(* Build parameter list with defaults for optional params *)
			let args_with_index = List.mapi (fun i arg -> (i, arg)) parent_ctor_args in
			let last_required_index = List.fold_left (fun acc (i, (_, opt, _)) ->
				if not opt then i else acc
			) (-1) args_with_index in
			let ctor_params = List.mapi (fun i (n, opt, t) ->
				let param_type = cs_type_of_type gctx t in
				let default_val = if opt && i > last_required_index then
					Some (CsDefault param_type)
				else
					None
				in
				{
					p_name = escape_identifier n;
					p_type = Some param_type;
					p_default = default_val;
					p_modifier = None;
				}
			) parent_ctor_args in
			let base_args = List.map (fun (n, _, _) -> CsLocal (escape_identifier n)) parent_ctor_args in
			members := [CsMemberConstructor {
				ctor_access = AccessModifier.Public;
				ctor_modifiers = [];
				ctor_params = ctor_params;
				ctor_base_call = Some base_args;
				ctor_this_call = None;
				ctor_body = field_init_stmts;
			}] @ !members
		end
		(* Also generate a default parameterless constructor if:
		   1. There are field initializations with 'this', OR
		   2. The parent needs a base call with required args (but we only generate this
		      if the parent also has optional params, so parameterless call makes sense) *)
		else if field_init_stmts <> [] || has_required_parent_args then begin
			let base_call = if has_required_parent_args then
				Some (List.map (fun (_, _, t) ->
					CsDefault (cs_type_of_type gctx t)
				) parent_ctor_args)
			else
				None
			in
			members := [CsMemberConstructor {
				ctor_access = AccessModifier.Public;
				ctor_modifiers = [];
				ctor_params = [];
				ctor_base_call = base_call;
				ctor_this_call = None;
				ctor_body = field_init_stmts;
			}] @ !members
		end
	end;

	(* Fields *)
	List.iter (fun cf ->
		match generate_field gctx c cf false with
		| Some m ->
			members := m :: !members;
			(* Generate explicit interface implementations for covariant return types *)
			members := generate_explicit_interface_impls gctx c cf @ !members
		| None -> ()
	) c.cl_ordered_fields;

	(* Static fields *)
	List.iter (fun cf ->
		match generate_field gctx c cf true with
		| Some m -> members := m :: !members
		| None -> ()
	) c.cl_ordered_statics;

	(* Generate _hx_getField, _hx_setField, _hx_getFields for AOT-compatible dynamic field access *)
	members := generate_field_accessors gctx c @ !members;

	(* Generate static constructor if class has __init__ *)
	begin match TClass.get_cl_init c with
	| Some e ->
		let ectx = create_expr_context gctx in
		ectx.current_class_path <- Some c.cl_path;
		ectx.current_method_name <- Some "__init__";
		let stmts = match e.eexpr with
			| TBlock exprs -> List.map (cs_stmt_of_texpr ectx) exprs
			| _ -> [cs_stmt_of_texpr ectx e]
		in
		(* Filter out pure expressions that would become invalid statements *)
		let stmts = List.filter (function
			| CsExprStmt (CsConst _) -> false
			| CsExprStmt (CsLocal _) -> false
			| CsBlock [] -> false
			| _ -> true
		) stmts in
		if stmts <> [] then
			members := CsMemberStaticConstructor stmts :: !members
	| None -> ()
	end;

	(* Extract type parameter names *)
	let type_params = List.map (fun ttp -> ttp.ttp_name) c.cl_params in

	CsClassDef {
		c_path = path;
		c_access = AccessModifier.Public;
		c_modifiers = modifiers;
		c_type_params = type_params;
		c_base = base_class;
		c_interfaces = interfaces;
		c_constraints = [];
		c_members = List.rev !members;
	}

(* Generate C# interface from Haxe interface *)
let generate_interface gctx c =
	let path = cs_path_of_path c.cl_path in

	(* Generate base interfaces *)
	let base_interfaces = List.map (fun (i, params) ->
		let path = cs_path_of_path i.cl_path in
		let params = List.map (cs_type_of_type gctx) params in
		CsTypeClass (path, params)
	) c.cl_implements in

	(* Generate members *)
	let members = List.filter_map (fun cf ->
		match cf.cf_kind with
		| Method MethNormal ->
			let args, ret = match follow cf.cf_type with
				| TFun (args, ret) -> args, ret
				| _ -> [], cf.cf_type
			in
			let params = List.map (fun (n, _, t) ->
				{
					p_name = escape_identifier n;
					p_type = Some (cs_type_of_type gctx t);
					p_default = None;
					p_modifier = None;
				}
			) args in
			(* Get method-level type parameters *)
			let method_type_params = List.map (fun ttp -> ttp.ttp_name) cf.cf_params in
			Some (CsMemberMethod {
				m_name = escape_identifier cf.cf_name;
				m_return_type = cs_type_of_type gctx ret;
				m_access = AccessModifier.Public;
				m_modifiers = [];
				m_type_params = method_type_params;
				m_params = params;
				m_body = None;  (* Interface methods have no body *)
				m_constraints = [];
				m_explicit_interface = None;
				m_attributes = [];
			})
		| Var { v_read = AccNormal; v_write = AccNormal } ->
			(* Simple interface field - generate as property with auto get/set *)
			Some (CsMemberProperty {
				prop_name = escape_identifier cf.cf_name;
				prop_type = cs_type_of_type gctx cf.cf_type;
				prop_access = AccessModifier.Public;
				prop_modifiers = [];
				prop_getter = Some { acc_access = None; acc_body = None };  (* { get; } *)
				prop_setter = Some { acc_access = None; acc_body = None };  (* { set; } *)
				prop_init = None;
				prop_explicit_interface = None;
			})
		| Var { v_read = AccNormal; v_write = AccNo | AccNever } ->
			(* Read-only interface field - generate as property with only getter *)
			Some (CsMemberProperty {
				prop_name = escape_identifier cf.cf_name;
				prop_type = cs_type_of_type gctx cf.cf_type;
				prop_access = AccessModifier.Public;
				prop_modifiers = [];
				prop_getter = Some { acc_access = None; acc_body = None };  (* { get; } *)
				prop_setter = None;
				prop_init = None;
				prop_explicit_interface = None;
			})
		| Var { v_read = AccNo | AccNever; v_write = AccNormal } ->
			(* Write-only interface field - generate as property with only setter *)
			Some (CsMemberProperty {
				prop_name = escape_identifier cf.cf_name;
				prop_type = cs_type_of_type gctx cf.cf_type;
				prop_access = AccessModifier.Public;
				prop_modifiers = [];
				prop_getter = None;
				prop_setter = Some { acc_access = None; acc_body = None };  (* { set; } *)
				prop_init = None;
				prop_explicit_interface = None;
			})
		| Var _ ->
			(* Property with AccCall - getter/setter are methods, don't generate C# property.
			   The methods (get_x, set_x) are generated separately as Method MethNormal entries. *)
			None
		| _ -> None
	) c.cl_ordered_fields in

	(* Extract type parameter names *)
	let type_params = List.map (fun ttp -> ttp.ttp_name) c.cl_params in

	CsInterfaceDef {
		i_path = path;
		i_access = AccessModifier.Public;
		i_modifiers = [];
		i_type_params = type_params;
		i_base = base_interfaces;
		i_constraints = [];
		i_members = members;
	}

(* Generate C# enum from Haxe enum *)
let generate_enum gctx (e : tenum) =
	let path = cs_path_of_path e.e_path in

	(* Extract type parameter names from the enum *)
	let type_params = List.map (fun ttp -> ttp.ttp_name) e.e_params in
	(* Create C# type references for the type params (used when inheriting from parent) *)
	let type_param_refs = List.map (fun name -> CsTypeGenericParam name) type_params in

	(* Check if this is a simple enum (no constructors with parameters AND no type params) *)
	let is_simple = type_params = [] && PMap.fold (fun ef acc ->
		acc && (match ef.ef_type with TFun _ -> false | _ -> true)
	) e.e_constrs true in

	if is_simple then
		(* Generate as C# enum *)
		let members = PMap.fold (fun ef acc ->
			{ em_name = escape_identifier ef.ef_name; em_value = None } :: acc
		) e.e_constrs [] in
		CsEnumDef {
			e_path = path;
			e_access = AccessModifier.Public;
			e_underlying = None;
			e_members = List.rev members;
		}
	else
		(* Generate as abstract class with nested classes *)
		let members = PMap.fold (fun ef acc ->
			match ef.ef_type with
			| TFun (args, _) ->
				(* Nested class for constructor with parameters *)
				let class_name = escape_identifier ef.ef_name in
				(* GADT support: constructor may have its own type parameters (ef.ef_params) *)
				let ctor_type_params = List.map (fun ttp -> ttp.ttp_name) ef.ef_params in
				(* Filter out constructor params that shadow parent params (same name) *)
				let extra_type_params = List.filter (fun name ->
					not (List.mem name type_params)
				) ctor_type_params in
				(* Nested class type params: only constructor's EXTRA params (not parent's params,
				   since nested classes in C# can access outer class type params directly) *)
				let nested_type_params = extra_type_params in
				let fields = List.mapi (fun i (name, _, t) ->
					CsMemberField {
						f_name = escape_identifier name;
						f_type = cs_type_of_type gctx t;
						f_access = AccessModifier.Public;
						f_modifiers = [];
						f_value = None;
					}
				) args in
				let ctor_params = List.map (fun (name, _, t) ->
					{
						p_name = escape_identifier name;
						p_type = Some (cs_type_of_type gctx t);
						p_default = None;
						p_modifier = None;
					}
				) args in
				let ctor_body = List.map (fun (name, _, _) ->
					let esc_name = escape_identifier name in
					CsExprStmt (CsBinop (CsOpAssign,
						CsField (CsThis, esc_name),
						CsLocal esc_name))
				) args @ [
					CsExprStmt (CsBinop (CsOpAssign,
						CsField (CsThis, "_hx_index"),
						CsConst (CsConstInt (Int32.of_int ef.ef_index))))
				] in
				let ctor = CsMemberConstructor {
					ctor_access = AccessModifier.Public;
					ctor_modifiers = [];
					ctor_params = ctor_params;
					ctor_base_call = None;
					ctor_this_call = None;
					ctor_body = ctor_body;
				} in
				(* Nested class has parent's type params + constructor's own type params (GADT) *)
				let nested_class = CsClassDef {
					c_path = (fst path, class_name);
					c_access = AccessModifier.Public;
					c_modifiers = [];
					c_type_params = nested_type_params;  (* Parent + constructor's own type params *)
					c_base = Some (CsTypeClass (path, type_param_refs));  (* Reference parent with parent's type params only *)
					c_interfaces = [];
					c_constraints = [];
					c_members = fields @ [ctor];
				} in
				CsMemberNestedType nested_class :: acc
			| _ ->
				(* Simple constructor (no parameters) - need a nested class with singleton instance *)
				let class_name = escape_identifier ef.ef_name in
				(* For generic enums, simple constructors also need the type params *)
				if type_params <> [] then begin
					(* Generic enum - nested class inherits type params, no singleton possible *)
					let ctor_body = [
						CsExprStmt (CsBinop (CsOpAssign,
							CsField (CsThis, "_hx_index"),
							CsConst (CsConstInt (Int32.of_int ef.ef_index))))
					] in
					let ctor = CsMemberConstructor {
						ctor_access = AccessModifier.Public;
						ctor_modifiers = [];
						ctor_params = [];
						ctor_base_call = None;
						ctor_this_call = None;
						ctor_body = ctor_body;
					} in
					(* Nested class in C# can access outer class type params directly - don't re-declare *)
					let nested_class = CsClassDef {
						c_path = (fst path, class_name);
						c_access = AccessModifier.Public;
						c_modifiers = [];
						c_type_params = [];  (* No type params - use parent's *)
						c_base = Some (CsTypeClass (path, type_param_refs));  (* Reference parent with type params *)
						c_interfaces = [];
						c_constraints = [];
						c_members = [ctor];
					} in
					CsMemberNestedType nested_class :: acc
				end else begin
					(* Non-generic enum - use singleton pattern *)
					(* Suffix to avoid name collision between nested class and static field *)
					let nested_class_name = class_name ^ "_Impl_" in
					(* Constructor that sets _hx_index *)
					let ctor_body = [
						CsExprStmt (CsBinop (CsOpAssign,
							CsField (CsThis, "_hx_index"),
							CsConst (CsConstInt (Int32.of_int ef.ef_index))))
					] in
					let ctor = CsMemberConstructor {
						ctor_access = AccessModifier.Public;
						ctor_modifiers = [];
						ctor_params = [];
						ctor_base_call = None;
						ctor_this_call = None;
						ctor_body = ctor_body;
					} in
					(* Path for nested class: add parent class name to namespace *)
					let nested_path = (fst path @ [snd path], nested_class_name) in
					let instance = CsMemberField {
						f_name = "Instance";
						f_type = CsTypeClass (nested_path, []);
						f_access = AccessModifier.Public;
						f_modifiers = [MemberModifier.Static; MemberModifier.Readonly];
						f_value = Some (CsNew (CsTypeClass (nested_path, []), []));
					} in
					let nested_class = CsClassDef {
						c_path = (fst path, nested_class_name);  (* use short path for declaration *)
						c_access = AccessModifier.Public;
						c_modifiers = [];
						c_type_params = [];
						c_base = Some (CsTypeClass (path, []));
						c_interfaces = [];
						c_constraints = [];
						c_members = [ctor; instance];
					} in
					(* Static field on parent that returns the instance *)
					let field = CsMemberField {
						f_name = class_name;
						f_type = CsTypeClass (path, []);
						f_access = AccessModifier.Public;
						f_modifiers = [MemberModifier.Static; MemberModifier.Readonly];
						f_value = Some (CsStaticField (CsTypeClass (nested_path, []), "Instance"));
					} in
					CsMemberNestedType nested_class :: field :: acc
				end
		) e.e_constrs [] in

		(* Add _hx_index field *)
		let index_field = CsMemberField {
			f_name = "_hx_index";
			f_type = CsTypeInt;
			f_access = AccessModifier.Public;
			f_modifiers = [];
			f_value = None;
		} in

		CsClassDef {
			c_path = path;
			c_access = AccessModifier.Public;
			c_modifiers = [TypeModifier.Abstract];
			c_type_params = type_params;  (* Add type params to parent class *)
			c_base = None;
			c_interfaces = [];
			c_constraints = [];
			c_members = index_field :: List.rev members;
		}

(* Generate type *)
let generate_type gctx mt =
	match mt with
	| TClassDecl c when has_class_flag c CExtern ->
		(* Skip extern classes - they map to native types *)
		None
	| TClassDecl c when not (has_class_flag c CInterface) ->
		Some (generate_class gctx c)
	| TClassDecl c ->
		Some (generate_interface gctx c)
	| TEnumDecl e when has_enum_flag e EnExtern ->
		(* Skip extern enums *)
		None
	| TEnumDecl e ->
		Some (generate_enum gctx e)
	| TTypeDecl _ | TAbstractDecl _ ->
		None

(* Write file to disk *)
let write_file base_path rel_path content =
	let full_path = base_path ^ "/" ^ rel_path in
	Path.mkdir_from_path full_path;
	let ch = open_out_bin full_path in
	output_string ch content;
	close_out ch

(* Main generation entry point *)
let generate com =
	let gctx = create_context com in

	(* Generate all types *)
	List.iter (fun mt ->
		match generate_type gctx mt with
		| Some td -> gctx.generated_types <- td :: gctx.generated_types
		| None -> ()
	) com.types;

	(* Group types by namespace and write files, including closures with their origin class *)
	let files = List.map (fun td ->
		let path = match td with
			| CsClassDef c -> c.c_path
			| CsStructDef s -> s.s_path
			| CsInterfaceDef i -> i.i_path
			| CsEnumDef e -> e.e_path
			| CsDelegateDef d -> d.d_path
		in
		let namespace = fst path in
		let name = snd path in
		(* Get any closures that belong to this class *)
		let closures = get_closures_for_class gctx path in
		let file = {
			file_usings = [
				CsUsingNamespace ["System"];
				CsUsingNamespace ["System"; "Collections"; "Generic"];
			];
			file_namespace = if namespace = [] then None else Some namespace;
			file_types = [td];
			file_top_level_types = closures;  (* Closures outside namespace for unqualified access *)
		} in
		let rel_path = String.concat "/" namespace ^ "/" ^ name ^ ".cs" in
		(rel_path, generate_file file)
	) (List.rev gctx.generated_types) in

	(* Write all files *)
	List.iter (fun (rel_path, content) ->
		write_file com.file rel_path content
	) files;

	(* Generate Program.cs with Main entry point if we have an entry point *)
	begin match Gctx.get_entry_point com with
	| Some (_, entry_class, _) ->
		let main_class_path = cs_path_of_path entry_class.cl_path in
		let program_content = Printf.sprintf
"// Generated by Haxe C# target
using System;

public class Program
{
    public static void Main(string[] args)
    {
        %s.main();
    }
}
" (s_cs_path main_class_path) in
		write_file com.file "Program.cs" program_content
	| None -> ()
	end;

	(* Generate .csproj *)
	let proj = {
		proj_name = "HaxeProject";
		proj_target_framework = "net8.0";
		proj_output_type = "Exe";
	} in
	write_file com.file "Project.csproj" (generate_csproj proj);

	(* Copy runtime support files from std/cs/_cs/ *)
	let find_file f = (com.class_paths#find_file f).file in
	let copy_runtime_file src_path dest_path =
		let content = Std.input_file ~bin:true (find_file src_path) in
		write_file com.file dest_path content
	in
	copy_runtime_file "cs/_cs/haxe/root/HaxeObject.cs" "haxe/root/HaxeObject.cs";
	copy_runtime_file "cs/_cs/haxe/root/HaxeDynamicObject.cs" "haxe/root/HaxeDynamicObject.cs";
	copy_runtime_file "cs/_cs/haxe/lang/Null.cs" "haxe/lang/Null.cs";
	copy_runtime_file "cs/_cs/haxe/lang/Runtime.cs" "haxe/lang/Runtime.cs";
	copy_runtime_file "cs/_cs/haxe/lang/Function.cs" "haxe/lang/Function.cs";
	copy_runtime_file "cs/_cs/AssemblyAttributes.cs" "AssemblyAttributes.cs";

