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
open Genshared

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
	mutable preprocessor : cs_type preprocessor;  (* Preprocessor for this-before-super detection *)
}

let create_context com = {
	com = com;
	generated_types = [];
	closures_by_class = [];
	closure_count = 0;
	invoke_signatures = Hashtbl.create 32;
	preprocessor = Obj.magic ();  (* Initialized later after context is created *)
}

(* Check if expression needs unchecked context due to integer operations.
   This follows the legacy C# target approach: wrap method bodies in unchecked
   only when they contain non-zero integer constants that could overflow. *)
let needs_unchecked e =
	let rec loop e = match e.eexpr with
	(* A non-zero integer constant means we want unchecked context *)
	| TConst (TInt i) when i <> Int32.zero -> raise Exit
	(* Don't recurse into explicit __checked__ blocks *)
	| TCall ({ eexpr = TIdent "__checked__" }, _) -> ()
	(* Otherwise recurse into subexpressions *)
	| _ -> Type.iter loop e
	in
	try (loop e; false) with Exit -> true

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

(* Get the FunctionValue-based invoke method name: __hx_invoke0, __hx_invoke1, etc.
   These methods return FunctionValue to avoid boxing on return values. *)
let functionvalue_invoke_method_name num_args =
	"__hx_invoke" ^ string_of_int num_args

(* Generate FunctionValue arguments for closure/function invocation.
   Returns a list of FunctionValue.FromXxx(...) calls for each argument.
   Each argument type maps to a specific factory method:
   - int: FunctionValue.FromInt(arg)
   - double: FunctionValue.FromDouble(arg)
   - float: FunctionValue.FromFloat(arg)
   - bool: FunctionValue.FromBool(arg)
   - long: FunctionValue.FromLong(arg)
   - Null<int>: FunctionValue.FromNullInt(arg)
   - Null<double>: FunctionValue.FromNullDouble(arg)
   - other: FunctionValue.FromObject(arg)
   Note: arg_types may be shorter than args (e.g., if type info is missing);
   we default to FromObject for any args without type info. *)
let generate_functionvalue_args args arg_types =
	let functionvalue_type = CsTypeClass ((["haxe"; "lang"], "FunctionValue"), []) in
	let num_types = List.length arg_types in
	List.mapi (fun i arg ->
		let arg_type = if i < num_types then List.nth arg_types i else CsTypeObject in
		match arg_type with
		| CsTypeInt ->
			CsStaticCall (functionvalue_type, "FromInt", [arg])
		| CsTypeDouble ->
			CsStaticCall (functionvalue_type, "FromDouble", [arg])
		| CsTypeFloat ->
			CsStaticCall (functionvalue_type, "FromFloat", [arg])
		| CsTypeBool ->
			CsStaticCall (functionvalue_type, "FromBool", [arg])
		| CsTypeLong ->
			CsStaticCall (functionvalue_type, "FromLong", [arg])
		| CsTypeClass ((["haxe"; "lang"], "Null"), [inner]) ->
			(* Null<T>: use FromNullXxx methods to avoid boxing *)
			begin match inner with
			| CsTypeInt -> CsStaticCall (functionvalue_type, "FromNullInt", [arg])
			| CsTypeDouble -> CsStaticCall (functionvalue_type, "FromNullDouble", [arg])
			| CsTypeFloat -> CsStaticCall (functionvalue_type, "FromNullFloat", [arg])
			| CsTypeBool -> CsStaticCall (functionvalue_type, "FromNullBool", [arg])
			| CsTypeLong -> CsStaticCall (functionvalue_type, "FromNullLong", [arg])
			| _ -> CsStaticCall (functionvalue_type, "FromObject", [arg])
			end
		| _ ->
			(* References, strings, etc.: use FromObject *)
			CsStaticCall (functionvalue_type, "FromObject", [arg])
	) args

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
	mutable type_params_in_scope : string list;  (* type parameter names that are in scope (class + method) *)
	mutable type_param_constraints : (string * cs_type list) list;  (* type param name -> C# constraint types *)
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
	type_params_in_scope = [];
	type_param_constraints = [];
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

(* Check if a constructor needs two-phase construction (has Meta.HxGen from this-before-super) *)
let needs_two_phase_construction cf =
	Meta.has Meta.HxGen cf.cf_meta

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

(* Check if a C# type is valid as a generic constraint.
   C# only allows: interfaces, non-sealed classes, type parameters.
   Primitives, sealed classes (like string, Array<T>), structs are NOT allowed. *)
let is_valid_cs_constraint cs_t =
	match cs_t with
	(* Primitives - not valid *)
	| CsTypeInt | CsTypeLong | CsTypeFloat | CsTypeDouble | CsTypeBool
	| CsTypeByte | CsTypeSByte | CsTypeChar | CsTypeShort | CsTypeUShort
	| CsTypeUInt | CsTypeULong | CsTypeDecimal -> false
	(* object/dynamic - not valid (also C# doesn't allow 'object' as constraint) *)
	| CsTypeObject | CsTypeDynamic -> false
	(* string is sealed in C# - not valid *)
	| CsTypeString -> false
	(* Void is not valid *)
	| CsTypeVoid -> false
	(* Type parameters are valid constraints *)
	| CsTypeGenericParam _ -> true
	(* Arrays, Nullable - not valid (sealed/struct) *)
	| CsTypeArray _ | CsTypeNullable _ -> false
	(* For classes, we need to check if they're sealed. Common sealed: Array<T>, String *)
	| CsTypeClass ((["haxe"; "root"], "Array"), _) -> false  (* Array is sealed *)
	| CsTypeClass _ -> true  (* Assume other classes/interfaces are valid *)
	| CsTypeNested _ | CsTypeNestedGeneric _ -> true  (* Assume nested types are valid *)
	| CsTypeFunc _ | CsTypeAction _ -> false  (* Delegates are sealed *)
	| CsTypeVar -> false  (* var is not a real type *)

(* Extract type parameter constraints from typed_type_param list.
   Returns a list of (name, cs_type list) pairs for C# where clauses.
   Only includes type params that have non-empty valid C# constraints. *)
let extract_type_param_constraints gctx ttps =
	List.filter_map (fun ttp ->
		let constraints = TFunctions.get_constraints ttp in
		if constraints = [] then None
		else begin
			let cs_constraints = List.filter_map (fun t ->
				match follow t with
				| TInst (c, _) when c.cl_path = ([], "Class") -> None  (* Skip Class<T> constraint *)
				| TAbstract ({a_path = ["haxe"], "Constructible"}, _) -> None  (* Skip Constructible *)
				| TAnon _ -> None  (* Skip structural/anonymous type constraints *)
				| t ->
					let cs_t = cs_type_of_type gctx t in
					if is_valid_cs_constraint cs_t then Some cs_t
					else None
			) constraints in
			if cs_constraints = [] then None
			else Some (ttp.ttp_name, cs_constraints)
		end
	) ttps

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
		| TArray (arr, _) ->
			(* For array access, check the element type of the array.
			   Due to @:forward on Null<T>, e.etype might be T instead of Null<T>,
			   but the actual C# expression produces the element type which could be Null<...>.
			   Get element type from the array type.
			   IMPORTANT: Only do this check when the expression type is NOT already showing
			   as the element type - if e.etype matches the element type, it's not Null-wrapped. *)
			let rec get_element_type t = match follow t with
				| TInst ({ cl_path = ([], "Array") | (["haxe"; "root"], "Array") }, [elem_t]) -> Some elem_t
				| TAbstract ({ a_path = ([], "Null") }, [inner]) -> get_element_type inner
				| _ -> None
			in
			begin match get_element_type arr.etype with
			| Some elem_t ->
				(* Only treat as Null-wrapped if the element type IS Null<T> but e.etype is NOT.
				   This means @:forward has "forwarded" the type away from Null. *)
				if is_null_wrapper_type elem_t && not (is_null_wrapper_type e.etype) then
					true
				else
					false
			| None -> false
			end
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
	| CsTypeDouble, CsTypeInt ->
		(* int -> double: while implicit in C#, explicit cast avoids ambiguity
		   with methods like Math.Floor(decimal) vs Math.Floor(double) *)
		CsCast (CsTypeDouble, cs_arg)
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
	| CsTypeLong, CsTypeObject -> CsCast (CsTypeLong, cs_arg)
	| CsTypeByte, CsTypeObject -> CsCast (CsTypeByte, cs_arg)
	| CsTypeString, CsTypeObject -> CsCast (CsTypeString, cs_arg)
	(* Dynamic to basic types - need explicit cast (Dynamic is object in disguise) *)
	| CsTypeInt, CsTypeDynamic -> CsCast (CsTypeInt, cs_arg)
	| CsTypeDouble, CsTypeDynamic -> CsCast (CsTypeDouble, cs_arg)
	| CsTypeBool, CsTypeDynamic -> CsCast (CsTypeBool, cs_arg)
	| CsTypeFloat, CsTypeDynamic -> CsCast (CsTypeFloat, cs_arg)
	| CsTypeLong, CsTypeDynamic -> CsCast (CsTypeLong, cs_arg)
	| CsTypeByte, CsTypeDynamic -> CsCast (CsTypeByte, cs_arg)
	| CsTypeString, CsTypeDynamic -> CsCast (CsTypeString, cs_arg)
	(* Null<object> to basic types - unwrap .value then cast *)
	| CsTypeInt, CsTypeClass ((["haxe"; "lang"], "Null"), [CsTypeObject]) ->
		CsCast (CsTypeInt, CsField (cs_arg, "value"))
	| CsTypeDouble, CsTypeClass ((["haxe"; "lang"], "Null"), [CsTypeObject]) ->
		CsCast (CsTypeDouble, CsField (cs_arg, "value"))
	| CsTypeBool, CsTypeClass ((["haxe"; "lang"], "Null"), [CsTypeObject]) ->
		CsCast (CsTypeBool, CsField (cs_arg, "value"))
	| CsTypeFloat, CsTypeClass ((["haxe"; "lang"], "Null"), [CsTypeObject]) ->
		CsCast (CsTypeFloat, CsField (cs_arg, "value"))
	| CsTypeLong, CsTypeClass ((["haxe"; "lang"], "Null"), [CsTypeObject]) ->
		CsCast (CsTypeLong, CsField (cs_arg, "value"))
	| CsTypeString, CsTypeClass ((["haxe"; "lang"], "Null"), [CsTypeObject]) ->
		CsCast (CsTypeString, CsField (cs_arg, "value"))
	(* Null<object> to a class type - unwrap .value then cast *)
	| CsTypeClass (path, params), CsTypeClass ((["haxe"; "lang"], "Null"), [CsTypeObject])
		when path <> (["haxe"; "lang"], "Null") ->
		CsCast (CsTypeClass (path, params), CsField (cs_arg, "value"))
	(* object/Dynamic to Null<T> - need to create Null wrapper conditionally.
	   If the object is null, create a Null with hasValue=false.
	   If the object has a value, unbox it and create Null with hasValue=true. *)
	| CsTypeClass ((["haxe"; "lang"], "Null"), [inner]), (CsTypeObject | CsTypeDynamic) ->
		(* Generate: arg == null ? new Null<T>(default, false) : new Null<T>((T)arg, true) *)
		let null_check = CsBinop (CsOpEq, cs_arg, CsNull) in
		let cast_value = CsCast (inner, cs_arg) in
		let true_branch = CsNew (expected_cs_type, [CsDefault inner; CsConst (CsConstBool false)]) in
		let false_branch = CsNew (expected_cs_type, [cast_value; CsConst (CsConstBool true)]) in
		CsTernary (null_check, true_branch, false_branch)
	(* object to class type (except Null) - need explicit cast *)
	| CsTypeClass (path, params), CsTypeObject when path <> (["haxe"; "lang"], "Null") ->
		CsCast (CsTypeClass (path, params), cs_arg)
	(* Dynamic to class type (except Null) - need explicit cast *)
	| CsTypeClass (path, params), CsTypeDynamic when path <> (["haxe"; "lang"], "Null") ->
		CsCast (CsTypeClass (path, params), cs_arg)
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
	(* Null<numeric1> to Null<numeric2> - need to convert the inner value.
	   e.g., Null<int> to Null<double>: hasValue ? new Null<double>((double)value, true) : new Null<double>(0, false)
	   We use a ternary to handle the hasValue check. *)
	| CsTypeClass ((["haxe"; "lang"], "Null"), [inner_expected]), CsTypeClass ((["haxe"; "lang"], "Null"), [inner_arg])
		when inner_expected <> inner_arg ->
		(* Check if we need numeric conversion *)
		let needs_conversion = match inner_expected, inner_arg with
			| CsTypeDouble, CsTypeInt -> true
			| CsTypeDouble, CsTypeFloat -> true
			| CsTypeFloat, CsTypeInt -> true
			| CsTypeLong, CsTypeInt -> true
			| CsTypeInt, CsTypeLong -> true  (* narrowing *)
			| CsTypeInt, CsTypeDouble -> true  (* narrowing *)
			| _ -> false
		in
		if needs_conversion then
			(* Generate: arg.hasValue ? new Null<T>((T)arg.value, true) : new Null<T>(default(T), false) *)
			let has_value = CsField (cs_arg, "hasValue") in
			let converted_value = CsCast (inner_expected, CsField (cs_arg, "value")) in
			let true_branch = CsNew (expected_cs_type, [converted_value; CsConst (CsConstBool true)]) in
			let false_branch = CsNew (expected_cs_type, [CsDefault inner_expected; CsConst (CsConstBool false)]) in
			CsTernary (has_value, true_branch, false_branch)
		else
			cs_arg
	(* Generic covariance: SomeClass<SpecificType> to SomeClass<object> where we're widening.
	   C# generics are invariant, so we need to cast through object: (TargetType)(object)expr
	   This handles cases like Array<int> to Array<object>, etc.
	   IMPORTANT: Only apply when expected type params are MORE GENERAL (object/dynamic) than arg type params.
	   Do NOT apply when going from general to specific (e.g., Either<object,object> to Either<int,int>).
	   Also do NOT apply when arg has type params (they might be out of scope).
	   Note: CsTypeClass is used for both classes and interfaces in our AST. *)
	| CsTypeClass (path1, params1), CsTypeClass (path2, params2)
		when path1 = path2 && params1 <> params2 ->
		(* Check if expected params are all object/dynamic - only then is it safe to widen *)
		let is_general_type = function
			| CsTypeObject | CsTypeDynamic -> true
			| _ -> false
		in
		let is_type_param = function
			| CsTypeGenericParam _ -> true
			| _ -> false
		in
		let expected_is_general = List.for_all is_general_type params1 in
		let arg_has_type_params = List.exists is_type_param params2 in
		(* Only apply if expected is general AND arg has no type params (to avoid scope issues) *)
		if expected_is_general && not arg_has_type_params then
			CsCast (expected_cs_type, CsCast (CsTypeObject, cs_arg))
		else
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

(* Get the first valid class/interface constraint for a type parameter.
   Returns Some(constraint_type) if found, None otherwise.
   Used for field access on type params where C# can't express the constraint. *)
let get_type_param_constraint t =
	match follow t with
	| TInst ({ cl_kind = KTypeParameter ttp }, _) ->
		let constraints = TFunctions.get_constraints ttp in
		List.find_map (fun ct ->
			match follow ct with
			| TInst _ | TAbstract _ -> Some ct
			| _ -> None
		) constraints
	| _ -> None

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
		(* Check if type is a type parameter with Array<T> constraint.
		   Returns Some(element_type) if there's an Array constraint, None otherwise. *)
		let get_array_constraint_elem_type t =
			match follow t with
			| TInst ({ cl_kind = KTypeParameter ttp }, _) ->
				(* Check constraints for Array<T> *)
				let constraints = TFunctions.get_constraints ttp in
				List.find_map (fun ct ->
					match follow ct with
					| TInst ({ cl_path = ([], "Array") | (["haxe"; "root"], "Array") }, [elem]) -> Some elem
					| _ -> None
				) constraints
			| _ -> None
		in
		(* Check if array expression is Dynamic - need runtime helper *)
		let is_dynamic = match follow e1.etype with
			| TDynamic _ -> true
			| _ -> false
		in
		if is_dynamic then begin
			(* Dynamic array access: use runtime helper cs.Cs.arrayGet *)
			let call = CsStaticCall (CsTypeClass ((["cs"], "Cs"), []), "arrayGet", [cs_expr_of_texpr ectx e1; cs_expr_of_texpr ectx e2]) in
			(* Cast result to expected type if it's not Dynamic *)
			let result_type = cs_type_of_type ectx.gctx e.etype in
			match result_type with
			| CsTypeObject -> call  (* Already object, no cast needed *)
			| _ -> CsCast (result_type, call)
		end
		else
			let is_haxe_array = is_haxe_array_type e1.etype in
			let is_null_wrapper = find_null_in_expr e1 in
			if is_haxe_array then begin
				(* arr[i] -> arr.__a[i] or arr.value.__a[i] for haxe Array *)
				let arr_expr = cs_expr_of_texpr ectx e1 in
				let arr_expr = if is_null_wrapper then CsField (arr_expr, "value") else arr_expr in
				let access = CsArrayAccess (CsField (arr_expr, "__a"), cs_expr_of_texpr ectx e2) in
				(* If the expected element type differs from what C# infers (e.g., after casting to Array<object>),
				   cast the result to the expected type. e.etype tells us the Haxe-level element type.

				   The challenge: C# might have Array<object> due to anonymous type dispatch,
				   even when Haxe says Array<Int>. We detect this by:
				   1. Checking if e1 has a TCast that might widen types
				   2. Checking if the inner array expression is from a FAnon field call *)
				let expected_cs = cs_type_of_type ectx.gctx e.etype in
				begin match expected_cs with
				| CsTypeObject | CsTypeDynamic -> access  (* No cast needed for object/dynamic *)
				| _ ->
					(* Check if e1 contains a cast from something that returns object-typed arrays.
					   This includes TCast from anonymous type method returns. *)
					let rec has_widening_cast e = match e.eexpr with
						| TCast (inner, _) ->
							(* Check if inner is a call on anonymous type (returns object-parameterized types) *)
							begin match inner.eexpr with
							| TCall ({ eexpr = TField (_, FAnon _) }, _) -> true
							| _ -> has_widening_cast inner
							end
						| TParenthesis e1 | TMeta (_, e1) -> has_widening_cast e1
						| _ -> false
					in
					(* Also check for direct FAnon call without cast wrapper *)
					let is_fanon_call e = match e.eexpr with
						| TCall ({ eexpr = TField (_, FAnon _) }, _) -> true
						| _ -> false
					in
					if has_widening_cast e1 || is_fanon_call e1 then
						CsCast (expected_cs, access)
					else
						(* Get the C#-level array element type from e1's type *)
						let arr_cs_type = cs_type_of_type ectx.gctx e1.etype in
						let arr_element_type = match arr_cs_type with
							| CsTypeClass ((["haxe"; "root"], "Array"), [elem]) -> elem
							| CsTypeClass (([], "Array"), [elem]) -> elem
							| _ -> CsTypeObject
						in
						(* Cast if element type is object but we expect something more specific *)
						if arr_element_type = CsTypeObject && expected_cs <> CsTypeObject then
							CsCast (expected_cs, access)
						else
							access
				end
			end
			else begin
				(* Check if e1 is a type parameter with Array<T> constraint.
				   If so, cast to Array<T> before accessing - C# doesn't know about Haxe constraints. *)
				match get_array_constraint_elem_type e1.etype with
				| Some elem_type ->
					(* Cast type param to Array<T> through object: ((Array<T>)(object)b)[idx] *)
					let elem_cs = cs_type_of_type ectx.gctx elem_type in
					let array_cs_type = CsTypeClass ((["haxe"; "root"], "Array"), [elem_cs]) in
					let arr_cast = CsCast (array_cs_type, CsCast (CsTypeObject, cs_expr_of_texpr ectx e1)) in
					(* Access __a field of the cast array *)
					CsArrayAccess (CsField (arr_cast, "__a"), cs_expr_of_texpr ectx e2)
				| None ->
					CsArrayAccess (cs_expr_of_texpr ectx e1, cs_expr_of_texpr ectx e2)
			end
	| TBinop (op, e1, e2) ->
		(* Special handling for Null<T> comparisons with null and generic type param equality *)
		(* NOTE: We exclude Null<object> because it behaves like Dynamic and should use == null directly *)
		let is_null_type t = match cs_type_of_type ectx.gctx t with
			| CsTypeClass ((["haxe"; "lang"], "Null"), [CsTypeObject]) -> false  (* Null<object> uses == null *)
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
				(* Coerce value to target type - needed when assigning object/Dynamic to typed variable *)
				let val_cs = coerce_arg ectx.gctx val_cs e2.etype e1.etype in
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
			(* Helper: cast dynamic operation result to expected type if needed *)
			let cast_dynamic_result call_expr =
				let expected_cs_type = cs_type_of_type ectx.gctx e.etype in
				match expected_cs_type with
				| CsTypeObject | CsTypeDynamic -> call_expr
				| _ -> CsCast (expected_cs_type, call_expr)
			in
			begin match op with
			(* Arithmetic operators on Dynamic need runtime dispatch - use either_dynamic for most *)
			| OpAdd when either_dynamic ->
				cast_dynamic_result (CsStaticCall (CsTypeClass (cs_path, []), "opAdd", [cs_expr_of_texpr ectx e1; cs_expr_of_texpr ectx e2]))
			| OpSub when either_dynamic ->
				cast_dynamic_result (CsStaticCall (CsTypeClass (cs_path, []), "opSub", [cs_expr_of_texpr ectx e1; cs_expr_of_texpr ectx e2]))
			| OpMult when either_dynamic ->
				cast_dynamic_result (CsStaticCall (CsTypeClass (cs_path, []), "opMul", [cs_expr_of_texpr ectx e1; cs_expr_of_texpr ectx e2]))
			| OpDiv when either_dynamic ->
				cast_dynamic_result (CsStaticCall (CsTypeClass (cs_path, []), "opDiv", [cs_expr_of_texpr ectx e1; cs_expr_of_texpr ectx e2]))
			| OpMod when either_dynamic ->
				cast_dynamic_result (CsStaticCall (CsTypeClass (cs_path, []), "opMod", [cs_expr_of_texpr ectx e1; cs_expr_of_texpr ectx e2]))
			| OpAnd when either_dynamic ->
				cast_dynamic_result (CsStaticCall (CsTypeClass (cs_path, []), "opAnd", [cs_expr_of_texpr ectx e1; cs_expr_of_texpr ectx e2]))
			| OpOr when either_dynamic ->
				cast_dynamic_result (CsStaticCall (CsTypeClass (cs_path, []), "opOr", [cs_expr_of_texpr ectx e1; cs_expr_of_texpr ectx e2]))
			| OpXor when either_dynamic ->
				cast_dynamic_result (CsStaticCall (CsTypeClass (cs_path, []), "opXor", [cs_expr_of_texpr ectx e1; cs_expr_of_texpr ectx e2]))
			| OpShl when either_dynamic ->
				cast_dynamic_result (CsStaticCall (CsTypeClass (cs_path, []), "opShl", [cs_expr_of_texpr ectx e1; cs_expr_of_texpr ectx e2]))
			| OpShr when either_dynamic ->
				cast_dynamic_result (CsStaticCall (CsTypeClass (cs_path, []), "opShr", [cs_expr_of_texpr ectx e1; cs_expr_of_texpr ectx e2]))
			| OpUShr when either_dynamic ->
				cast_dynamic_result (CsStaticCall (CsTypeClass (cs_path, []), "opUshr", [cs_expr_of_texpr ectx e1; cs_expr_of_texpr ectx e2]))
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
			(* Compound assignment where e1 is an anonymous field access, Reflect.field() call,
			   or dynamic array access (possibly wrapped in casts). This must be checked FIRST
			   because after casting from Dynamic to int, the type is no longer Dynamic but we
			   still can't assign to the cast result. Use fieldXxxAssign or arrayXxxAssign helpers. *)
			| OpAssignOp inner_op ->
				(* Check if e1 is a dynamic access pattern (possibly wrapped in casts) *)
				let rec unwrap_casts expr = match expr.eexpr with
					| TCast (inner, _) -> unwrap_casts inner
					| TParenthesis inner -> unwrap_casts inner
					| _ -> expr
				in
				let inner_e1 = unwrap_casts e1 in
				(* Check for anonymous field access (FAnon), dynamic field access (FDynamic), explicit Reflect.field call, or Cs.arrayGet call *)
				let dynamic_access = match inner_e1.eexpr with
					| TField (obj_expr, FAnon cf) ->
						(* Anonymous object field access - check if it maps to Reflect.field *)
						let cs_type = cs_type_of_type ectx.gctx (follow obj_expr.etype) in
						begin match cs_type with
						| CsTypeObject ->
							(* This will be generated as Reflect.field - use helper *)
							`Field (obj_expr, cf.cf_name)
						| _ -> `None
						end
					| TField (obj_expr, FDynamic name) ->
						(* Dynamic field access - always needs runtime helper *)
						`Field (obj_expr, name)
					| TCall ({ eexpr = TField (_, FStatic ({ cl_path = ([], "Reflect") }, { cf_name = "field" })) }, [obj_expr; { eexpr = TConst (TString name) }]) ->
						`Field (obj_expr, name)
					| TCall ({ eexpr = TField (_, FStatic ({ cl_path = (["cs"], "Cs") }, { cf_name = "arrayGet" })) }, [arr_expr; idx_expr]) ->
						`Array (arr_expr, idx_expr)
					| TArray (arr_expr, idx_expr) when is_dynamic arr_expr.etype ->
						(* Dynamic array access - this will be generated as Cs.arrayGet *)
						`Array (arr_expr, idx_expr)
					| _ -> `None
				in
				begin match dynamic_access with
				| `Field (obj_expr, field_name) ->
					(* Use fieldXxxAssign helpers for dynamic field compound assignment *)
					let field_helper = match inner_op with
						| OpAdd -> Some "fieldAddAssign" | OpSub -> Some "fieldSubAssign"
						| OpMult -> Some "fieldMulAssign" | OpDiv -> Some "fieldDivAssign"
						| OpMod -> Some "fieldModAssign"
						| _ -> None
					in
					begin match field_helper with
					| Some helper_name ->
						(* These helpers return object, but the expression may have a specific type. Cast if needed. *)
						let call_expr = CsStaticCall (CsTypeClass (cs_path, []), helper_name,
							[cs_expr_of_texpr ectx obj_expr; CsConst (CsConstString field_name); cs_expr_of_texpr ectx e2]) in
						let expected_cs_type = cs_type_of_type ectx.gctx e.etype in
						begin match expected_cs_type with
						| CsTypeObject | CsTypeDynamic -> call_expr
						| _ -> CsCast (expected_cs_type, call_expr)
						end
					| None ->
						(* Unsupported compound op on dynamic field - fall back to regular handling *)
						let e1_cs = cs_expr_of_texpr ectx e1 in
						let e2_cs = cs_expr_of_texpr ectx e2 in
						CsBinop (cs_binop_of_binop op, e1_cs, e2_cs)
					end
				| `Array (arr_expr, idx_expr) ->
					(* Use arrayXxxAssign helpers for dynamic array compound assignment *)
					let array_helper = match inner_op with
						| OpAdd -> Some "arrayAddAssign" | OpSub -> Some "arraySubAssign"
						| OpMult -> Some "arrayMulAssign" | OpDiv -> Some "arrayDivAssign"
						| OpMod -> Some "arrayModAssign"
						| _ -> None
					in
					begin match array_helper with
					| Some helper_name ->
						(* These helpers return object, but the expression may have a specific type. Cast if needed. *)
						let call_expr = CsStaticCall (CsTypeClass (cs_path, []), helper_name,
							[cs_expr_of_texpr ectx arr_expr; cs_expr_of_texpr ectx idx_expr; cs_expr_of_texpr ectx e2]) in
						let expected_cs_type = cs_type_of_type ectx.gctx e.etype in
						begin match expected_cs_type with
						| CsTypeObject | CsTypeDynamic -> call_expr
						| _ -> CsCast (expected_cs_type, call_expr)
						end
					| None ->
						(* Unsupported compound op on dynamic array - fall back to regular handling *)
						let e1_cs = cs_expr_of_texpr ectx e1 in
						let e2_cs = cs_expr_of_texpr ectx e2 in
						CsBinop (cs_binop_of_binop op, e1_cs, e2_cs)
					end
				| `None when either_dynamic ->
					(* Regular dynamic compound assignment: v += e -> v = Cs.opAdd(v, e) *)
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
				| `None ->
					(* Non-dynamic compound assignment - use normal operator *)
					CsBinop (cs_binop_of_binop op, cs_expr_of_texpr ectx e1, cs_expr_of_texpr ectx e2)
				end
			| _ ->
				(* Handle special cases for arithmetic operations *)
				let is_int_type t = match follow t with
					| TAbstract ({ a_path = ([], "Int") }, _) -> true
					| _ -> false
				in
				let is_float_type t = match follow t with
					| TAbstract ({ a_path = ([], "Float") }, _) -> true
					| _ -> false
				in
				let cs_e1 = cs_expr_of_texpr ectx e1 in
				let cs_e2 = cs_expr_of_texpr ectx e2 in
				(* For integer division producing float (0/0 -> NaN), cast to double first.
				   C# doesn't allow integer 0/0, but double 0.0/0.0 produces NaN. *)
				let cs_e1, cs_e2 = match op with
					| OpDiv when is_int_type e1.etype && is_int_type e2.etype && is_float_type e.etype ->
						(* Cast both operands to double for float division semantics *)
						CsCast (CsTypeDouble, cs_e1), CsCast (CsTypeDouble, cs_e2)
					| _ -> cs_e1, cs_e2
				in
				CsBinop (cs_binop_of_binop op, cs_e1, cs_e2)
			end
		end
	| TUnop (Spread, _, e) ->
		(* Spread operator: in C#, this is used for Rest/params arguments.
		   The spread just unwraps the array - pass through the inner expression. *)
		cs_expr_of_texpr ectx e
	| TUnop (op, pos, unop_operand) ->
		(* Check if operand type is truly Dynamic (TDynamic) - only these need runtime helpers *)
		let is_truly_dynamic = match follow unop_operand.etype with
			| TDynamic _ -> true
			| _ -> false
		in
		(* Check for TCast from Dynamic - this pattern appears when doing x++ where x is a dynamic
		   field access cast to a specific type. We need to detect this and use runtime helpers
		   to avoid "cannot modify unboxing result" errors in C#. *)
		let rec unwrap_casts expr = match expr.eexpr with
			| TCast (inner, _) -> unwrap_casts inner
			| TParenthesis inner -> unwrap_casts inner
			| _ -> expr
		in
		let inner_expr = unwrap_casts unop_operand in
		let inner_is_dynamic = match follow inner_expr.etype with
			| TDynamic _ -> true
			| _ -> false
		in
		(* Check if the inner expression is an anonymous field access, dynamic field access, or Reflect.field call *)
		let is_dynamic_field = match inner_expr.eexpr with
			| TField (obj_expr, FAnon cf) ->
				(* Anonymous object field access - check if it maps to dynamic field access *)
				let cs_type = cs_type_of_type ectx.gctx (follow obj_expr.etype) in
				begin match cs_type with
				| CsTypeObject | CsTypeGenericParam _ ->
					(* This will be generated as Reflect.field or _hx_getField - use helper *)
					Some (obj_expr, cf.cf_name)
				| _ -> None
				end
			| TField (obj_expr, FDynamic name) ->
				(* Dynamic field access - always needs runtime helper *)
				Some (obj_expr, name)
			| TCall ({ eexpr = TField (_, FStatic ({ cl_path = ([], "Reflect") }, { cf_name = "field" })) }, [obj_expr; { eexpr = TConst (TString name) }]) ->
				Some (obj_expr, name)
			| _ -> None
		in
		let is_postfix = pos = Postfix in
		(* For dynamic field access with increment/decrement, use fieldPost/PreIncrement/Decrement helpers.
		   These helpers return object, so we need to cast to the expected type based on e.etype. *)
		begin match is_dynamic_field, op with
		| Some (obj_expr, field_name), Increment ->
			let helper_name = if is_postfix then "fieldPostIncrement" else "fieldPreIncrement" in
			let call_expr = CsStaticCall (CsTypeClass ((["cs"], "Cs"), []), helper_name, [cs_expr_of_texpr ectx obj_expr; CsConst (CsConstString field_name)]) in
			(* The helper returns object, but the Haxe expression has a specific type. Cast to it. *)
			let expected_cs_type = cs_type_of_type ectx.gctx e.etype in
			begin match expected_cs_type with
			| CsTypeObject | CsTypeDynamic -> call_expr
			| _ -> CsCast (expected_cs_type, call_expr)
			end
		| Some (obj_expr, field_name), Decrement ->
			let helper_name = if is_postfix then "fieldPostDecrement" else "fieldPreDecrement" in
			let call_expr = CsStaticCall (CsTypeClass ((["cs"], "Cs"), []), helper_name, [cs_expr_of_texpr ectx obj_expr; CsConst (CsConstString field_name)]) in
			(* The helper returns object, but the Haxe expression has a specific type. Cast to it. *)
			let expected_cs_type = cs_type_of_type ectx.gctx e.etype in
			begin match expected_cs_type with
			| CsTypeObject | CsTypeDynamic -> call_expr
			| _ -> CsCast (expected_cs_type, call_expr)
			end
		| _ ->
			(* Use dynamic helpers if either the outer type is Dynamic OR
			   the inner expression (under casts) is Dynamic and we're doing increment/decrement *)
			let use_dynamic_helpers = is_truly_dynamic || (inner_is_dynamic && (op = Increment || op = Decrement)) in
			if use_dynamic_helpers then
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
					cs_expr_of_texpr ectx unop_operand
				else begin
					(* For increment/decrement on cast from Dynamic, pass the inner Dynamic expression
					   to the helper, not the cast expression. The helper returns Dynamic,
					   so we cast to the expected type if needed. *)
					let arg_expr = if inner_is_dynamic && not is_truly_dynamic then
						cs_expr_of_texpr ectx inner_expr
					else
						cs_expr_of_texpr ectx unop_operand
					in
					let call_expr = CsStaticCall (CsTypeClass ((["cs"], "Cs"), []), helper_name, [arg_expr]) in
					(* Cast the result to expected type if it's not Dynamic - use outer expression type *)
					let expected_cs_type = cs_type_of_type ectx.gctx e.etype in
					match expected_cs_type with
					| CsTypeObject | CsTypeDynamic -> call_expr
					| _ -> CsCast (expected_cs_type, call_expr)
				end
			else
				CsUnop (cs_unop_of_unop op, is_postfix, cs_expr_of_texpr ectx unop_operand)
		end
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
		(* Check if the object is a type parameter - may need to cast to constraint type for field access.
		   This handles cases where C# can't express the constraint (e.g., T:String where String is sealed). *)
		let obj_expr, field_name = match get_type_param_constraint e.etype with
			| Some constraint_type ->
				(* Cast through object to constraint type for field access *)
				let cs_constraint = cs_type_of_type ectx.gctx constraint_type in
				let casted = CsCast (cs_constraint, CsCast (CsTypeObject, obj_expr)) in
				(* Translate field names for specific C# types (e.g., length -> Length for string) *)
				let field = match cs_constraint, cf.cf_name with
					| CsTypeString, "length" -> "Length"
					| _ -> escape_identifier cf.cf_name
				in
				(casted, field)
			| None -> (obj_expr, escape_identifier cf.cf_name)
		in
		CsField (obj_expr, field_name)
	| TField (e, FClosure (Some (c, tl), cf)) ->
		(* Check if this is a MethDynamic field - those are actually variable fields holding
		   functions, not real methods. They should be treated as field access, not closure generation.
		   This matches JVM's behavior at line 811 which handles MethDynamic as getfield. *)
		let is_dynamic_method = match cf.cf_kind with
			| Method MethDynamic -> true
			| _ -> false
		in
		let needs_unwrap = find_null_in_expr e in
		let obj_expr = cs_expr_of_texpr ectx e in
		let obj_expr = if needs_unwrap then CsField (obj_expr, "value") else obj_expr in
		if is_dynamic_method then
			(* Dynamic method - just field access, it's already a function value *)
			CsField (obj_expr, escape_identifier cf.cf_name)
		else begin
			(* Instance method closure - generate a closure class that wraps the method call.
			   C# doesn't allow converting method groups to haxe.lang.Function directly. *)
			(* Apply type parameters to method type: e.g., Array<Int>.push(T) becomes push(Int) *)
			let map_type = apply_params c.cl_params tl in
			let method_type = map_type cf.cf_type in
			(* Generate closure class that captures 'this' and calls the method *)
			!generate_method_closure_ref ectx (Some obj_expr) false c.cl_path tl cf method_type
		end
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
		   If so, generate a closure class. Methods have TFun type.
		   IMPORTANT: MethDynamic methods are actually assignable variables holding functions,
		   not true methods, so they should be treated as field access, not closure generation.
		   This matches JVM's behavior (line 799-802) which only matches MethNormal|MethInline. *)
		let is_real_method = match cf.cf_kind with
			| Method (MethNormal | MethInline) -> true
			| Method MethDynamic -> false  (* dynamic methods are variable fields *)
			| _ -> false
		in
		if is_real_method then
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
		| CsTypeGenericParam _ ->
			(* Type parameter - cast through object to HaxeObject to call _hx_getField *)
			let haxe_object_type = CsTypeClass ((["haxe"; "root"], "HaxeObject"), []) in
			let casted_obj = CsCast (haxe_object_type, CsCast (CsTypeObject, obj_expr)) in
			let field_call = CsCall (CsField (casted_obj, "_hx_getField"), [CsConst (CsConstString cf.cf_name)]) in
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
	| TField (e_obj, FDynamic name) ->
		(* Dynamic field access - need to use reflection since C# object doesn't have arbitrary fields *)
		(* NOTE: Use follow_once to peel through TMono but not unwrap Null<T> *)
		let obj_expr = cs_expr_of_texpr ectx e_obj in
		let raw_type = Type.follow_once e_obj.etype in
		let obj_expr = match raw_type with
			| TAbstract ({ a_path = ([], "Null") }, _) ->
				(* Null<T> -> access .value to unwrap *)
				CsField (obj_expr, "value")
			| _ -> obj_expr
		in
		(* Use haxe.lang.Runtime.GetField for dynamic field access *)
		let field_call = CsStaticCall (CsTypeClass ((["haxe"; "lang"], "Runtime"), []), "GetField", [obj_expr; CsConst (CsConstString name)]) in
		(* Runtime.GetField returns object, but Haxe knows the actual type.
		   Cast to the expected type if it's not Dynamic/object. *)
		let result_cs_type = cs_type_of_type ectx.gctx e.etype in
		begin match result_cs_type with
		| CsTypeObject | CsTypeDynamic -> field_call
		| _ -> CsCast (result_cs_type, field_call)
		end
	| TField (_, FEnum (en, ef)) ->
		let path = cs_path_of_path en.e_path in
		(* Get type arguments from the expression type for generic enums like Option<T> *)
		let type_args = match follow e.etype with
			| TEnum (_, params) -> List.map (cs_type_of_type ectx.gctx) params
			| TFun (_, ret) ->
				(* When an enum constructor with params is accessed as a value, e.etype is TFun.
				   Get type args from the return type (the enum type). *)
				begin match follow ret with
				| TEnum (_, params) -> List.map (cs_type_of_type ectx.gctx) params
				| _ -> []
				end
			| _ -> []
		in
		(* Check if this enum constructor takes parameters - if so, it's a function *)
		let has_params = match ef.ef_type with TFun (args, _) when args <> [] -> true | _ -> false in
		if has_params then begin
			(* Enum constructor with parameters accessed as a value - need to generate a closure.
			   For example, `Foo` where `enum E { Foo(v: Int); }` returns a function Int -> E.
			   We create a synthetic tfunc and use generate_closure_class. *)
			let args_with_types = match ef.ef_type with
				| TFun (args, _) -> args
				| _ -> []
			in
			let ret_type = match ef.ef_type with
				| TFun (_, ret) -> ret
				| _ -> e.etype
			in
			(* Build the nested type for the constructor *)
			let ctor_name = escape_identifier ef.ef_name in
			let parent_type = CsTypeClass (path, type_args) in
			let nested_type = CsTypeNested (parent_type, ctor_name) in
			(* Generate a closure class inline that wraps the enum constructor *)
			let gctx = ectx.gctx in
			let closure_name = generate_closure_name gctx ectx in
			let closure_path = ([], closure_name) in
			(* Build parameter types for the closure *)
			let param_types_cs = List.map (fun (_, _, t) -> cs_type_of_type gctx t) args_with_types in
			let param_names = List.map (fun (name, _, _) -> escape_identifier name) args_with_types in
			let ret_cs_type = cs_type_of_type gctx ret_type in
			let num_params = List.length param_types_cs in
			(* Register this signature for typed invoke generation *)
			register_invoke_signature gctx param_types_cs ret_cs_type;
			(* Build invoke method: return new E.Foo(arg0, arg1, ...) *)
			let invoke_params = List.map2 (fun name cs_type ->
				{ p_name = name; p_type = Some cs_type; p_default = None; p_modifier = None }
			) param_names param_types_cs in
			let ctor_call = CsNew (nested_type, List.map (fun n -> CsLocal n) param_names) in
			let invoke_method = CsMemberMethod {
				m_name = invoke_method_name num_params;
				m_return_type = ret_cs_type;
				m_access = AccessModifier.Public;
				m_modifiers = if num_params = 0 then [MemberModifier.New] else [];
				m_type_params = [];
				m_params = invoke_params;
				m_body = Some [CsReturn (Some ctor_call)];
				m_constraints = [];
				m_explicit_interface = None;
				m_attributes = [];
			} in
			(* Build invokeDynamic method *)
			let haxe_array_object = CsTypeClass ((["haxe"; "root"], "Array"), [CsTypeObject]) in
			let args_array = CsField (CsLocal "args", "__a") in
			let invoke_dynamic_args = List.mapi (fun i cs_type ->
				let idx_const = CsConst (CsConstInt (Int32.of_int i)) in
				let arg_access = CsArrayAccess (args_array, idx_const) in
				CsCast (cs_type, arg_access)
			) param_types_cs in
			let invoke_call_dyn = CsCall (CsLocal (invoke_method_name num_params), invoke_dynamic_args) in
			let invoke_dynamic_method = CsMemberMethod {
				m_name = "invokeDynamic";
				m_return_type = CsTypeObject;
				m_access = AccessModifier.Public;
				m_modifiers = [MemberModifier.Override];
				m_type_params = [];
				m_params = [{ p_name = "args"; p_type = Some haxe_array_object; p_default = None; p_modifier = None }];
				m_body = Some [CsReturn (Some invoke_call_dyn)];
				m_constraints = [];
				m_explicit_interface = None;
				m_attributes = [];
			} in
			(* Build FunctionValue-based __hx_invokeN method *)
			let functionvalue_type = CsTypeClass ((["haxe"; "lang"], "FunctionValue"), []) in
			let fv_params = List.mapi (fun i _ ->
				{ p_name = "a" ^ string_of_int (i + 1); p_type = Some functionvalue_type; p_default = None; p_modifier = None }
			) param_types_cs in
			(* Extract args from FunctionValue *)
			let fv_extract_args = List.mapi (fun i cs_type ->
				let a_var = CsLocal ("a" ^ string_of_int (i + 1)) in
				match cs_type with
				| CsTypeInt -> CsCall (CsField (a_var, "ToInt"), [])
				| CsTypeDouble -> CsCall (CsField (a_var, "ToDouble"), [])
				| CsTypeFloat -> CsCall (CsField (a_var, "ToFloat"), [])
				| CsTypeBool -> CsCall (CsField (a_var, "ToBool"), [])
				| CsTypeLong -> CsCall (CsField (a_var, "ToLong"), [])
				| CsTypeString -> CsCall (CsField (a_var, "ToStringValue"), [])
				| _ -> CsCast (cs_type, CsCall (CsField (a_var, "ToDynamic"), []))
			) param_types_cs in
			let fv_invoke_call = CsCall (CsLocal (invoke_method_name num_params), fv_extract_args) in
			let fv_return = CsStaticCall (functionvalue_type, "FromObject", [fv_invoke_call]) in
			let fv_invoke_method = CsMemberMethod {
				m_name = functionvalue_invoke_method_name num_params;
				m_return_type = functionvalue_type;
				m_access = AccessModifier.Public;
				m_modifiers = [MemberModifier.Override];
				m_type_params = [];
				m_params = fv_params;
				m_body = Some [CsReturn (Some fv_return)];
				m_constraints = [];
				m_explicit_interface = None;
				m_attributes = [];
			} in
			(* Build the closure class *)
			let closure_class = CsClassDef {
				c_path = closure_path;
				c_access = AccessModifier.Internal;
				c_modifiers = [TypeModifier.Sealed];
				c_type_params = [];
				c_base = Some (CsTypeClass ((["haxe"; "lang"], "Function"), []));
				c_interfaces = [];
				c_constraints = [];
				c_members = [invoke_method; invoke_dynamic_method; fv_invoke_method];
			} in
			(* Add closure class to the origin class's closure list *)
			let origin_path = match ectx.origin_class_path with
				| Some p -> p
				| None -> en.e_path  (* Fall back to enum path *)
			in
			add_closure_for_class gctx origin_path closure_class;
			(* Return instantiation of the closure *)
			CsNew (CsTypeClass (closure_path, []), [])
		end
		(* For parameterless enum constructors, access via: new EnumType<T>.ConstructorName()
		   For generic enums, nested class uses parent's type params (C# nested class inheritance) *)
		else if type_args = [] && en.e_params = [] then
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
		(* Use FunctionValue-based invoke to avoid boxing primitives *)
		let num_args = List.length args_cs in
		let functionvalue_args = generate_functionvalue_args args_cs param_types_cs in
		let call_expr = CsCall (CsField (closure, functionvalue_invoke_method_name num_args), functionvalue_args) in
		(* Extract the return value from FunctionValue using the appropriate ToXxx method *)
		begin match result_type with
		| CsTypeVoid -> call_expr  (* FunctionValue.Missing() returned, ignored *)
		| CsTypeInt -> CsCall (CsField (call_expr, "ToInt"), [])
		| CsTypeDouble -> CsCall (CsField (call_expr, "ToDouble"), [])
		| CsTypeFloat -> CsCall (CsField (call_expr, "ToFloat"), [])
		| CsTypeBool -> CsCall (CsField (call_expr, "ToBool"), [])
		| CsTypeLong -> CsCall (CsField (call_expr, "ToLong"), [])
		| CsTypeString -> CsCall (CsField (call_expr, "ToStringValue"), [])
		| CsTypeClass ((["haxe"; "lang"], "Null"), [inner]) ->
			(* Null<T>: use ToNullXxx() methods *)
			begin match inner with
			| CsTypeInt -> CsCall (CsField (call_expr, "ToNullInt"), [])
			| CsTypeDouble -> CsCall (CsField (call_expr, "ToNullDouble"), [])
			| CsTypeFloat -> CsCall (CsField (call_expr, "ToNullFloat"), [])
			| CsTypeBool -> CsCall (CsField (call_expr, "ToNullBool"), [])
			| CsTypeLong -> CsCall (CsField (call_expr, "ToNullLong"), [])
			| _ ->
				let null_type = CsTypeClass ((["haxe"; "lang"], "Null"), [inner]) in
				CsStaticCall (null_type, "_ofDynamic", [CsCall (CsField (call_expr, "ToDynamic"), [])])
			end
		| CsTypeObject | CsTypeDynamic -> CsCall (CsField (call_expr, "ToDynamic"), [])
		| _ ->
			(* For other reference types, get obj field and cast *)
			CsCast (result_type, CsField (call_expr, "obj"))
		end
	| TCall ({ eexpr = TField (_, FEnum (en, ef)) }, orig_args) ->
		(* Enum constructor with parameters -> new EnumType<T>.ConstructorName<C>(...) *)
		let enum_path = cs_path_of_path en.e_path in
		let ctor_name = escape_identifier ef.ef_name in
		(* Get type arguments from the TCall's result type (e.etype) for generic enums *)
		let enum_type_args = match follow e.etype with
			| TEnum (_, params) -> List.map (cs_type_of_type ectx.gctx) params
			| _ -> []
		in
		(* Check if constructor has its own type parameters (like EBinop<C>).
		   If so, we need to infer them from the arguments.
		   IMPORTANT: The generated nested class only has EXTRA type params - those that
		   don't shadow the parent enum's type params. So we filter ef.ef_params to match. *)
		let parent_type_param_names = List.map (fun ttp -> ttp.ttp_name) en.e_params in
		let extra_ctor_params = List.filter (fun ttp ->
			not (List.mem ttp.ttp_name parent_type_param_names)
		) ef.ef_params in
		let ctor_type_args = if extra_ctor_params = [] then [] else begin
			(* Get parameter types from constructor signature *)
			let param_types = match follow ef.ef_type with
				| TFun (params, _) -> List.map (fun (_, _, t) -> t) params
				| _ -> []
			in
			(* Match parameter types with argument types to infer constructor type params *)
			let param_type_pairs = if List.length param_types <= List.length orig_args then
				List.combine param_types (List.map (fun a -> a.etype) (ExtList.List.take (List.length param_types) orig_args))
			else []
			in
			(* For each EXTRA constructor type param, find its value from argument types *)
			List.map (fun ttp ->
				let rec find_type_param_in_type param_t arg_t =
					match follow param_t, follow arg_t with
					| TInst ({ cl_kind = KTypeParameter ttp2 }, _), _ when ttp2.ttp_name = ttp.ttp_name ->
						Some arg_t
					| TInst (c1, tp1_list), TInst (c2, tp2_list) when c1.cl_path = c2.cl_path && List.length tp1_list = List.length tp2_list ->
						List.fold_left2 (fun acc tp1 tp2 ->
							match acc with Some _ -> acc | None -> find_type_param_in_type tp1 tp2
						) None tp1_list tp2_list
					| TEnum (e1, tp1_list), TEnum (e2, tp2_list) when e1.e_path = e2.e_path && List.length tp1_list = List.length tp2_list ->
						List.fold_left2 (fun acc tp1 tp2 ->
							match acc with Some _ -> acc | None -> find_type_param_in_type tp1 tp2
						) None tp1_list tp2_list
					| TAbstract (a1, tp1_list), TAbstract (a2, tp2_list) when a1.a_path = a2.a_path && List.length tp1_list = List.length tp2_list ->
						List.fold_left2 (fun acc tp1 tp2 ->
							match acc with Some _ -> acc | None -> find_type_param_in_type tp1 tp2
						) None tp1_list tp2_list
					| _ -> None
				in
				let found = List.fold_left (fun acc (param_t, arg_t) ->
					match acc with Some _ -> acc | None -> find_type_param_in_type param_t arg_t
				) None param_type_pairs in
				match found with
				| Some t -> cs_type_of_type ectx.gctx t
				| None -> CsTypeObject  (* Fallback to object if not found *)
			) extra_ctor_params
		end in
		(* Nested class: Parent<T>.Nested or Parent<T>.Nested<C> *)
		let parent_type = CsTypeClass (enum_path, enum_type_args) in
		let nested_type = if ctor_type_args = [] then
			CsTypeNested (parent_type, ctor_name)
		else
			CsTypeNestedGeneric (parent_type, ctor_name, ctor_type_args)
		in
		(* Generate args with proper type coercion based on enum constructor's param types.
		   The param types need to be mapped through the enum's type params (e.g., T -> int for Option<int>). *)
		let enum_hx_params = match follow e.etype with
			| TEnum (_, params) -> params
			| _ -> []
		in
		let map_enum_type = apply_params en.e_params enum_hx_params in
		let param_types = match follow ef.ef_type with
			| TFun (params, _) -> List.map (fun (_, _, t) -> map_enum_type t) params
			| _ -> []
		in
		let args = generate_call_args ectx cs_expr_of_texpr orig_args param_types in
		CsNew (nested_type, args)
	| TCall ({ eexpr = TField (e_obj, FInstance (c, _, cf)) }, args)
		when (match c.cl_path with ([], ("String" | "string")) | (["haxe"; "root"], ("String" | "string")) -> true | _ -> false) ->
		(* String methods can be:
		   1. Native methods (@:native) like toUpperCase/toLowerCase -> use C# native name
		   2. Inline methods that redirect to StringExt -> should already be inlined, but handle fallback
		   Check for @:native metadata first, then check if method exists in StringExt. *)
		(* Check if expression type is Null<String> - if so, unwrap via .value *)
		let needs_unwrap = find_null_in_expr e_obj in
		let obj = cs_expr_of_texpr ectx e_obj in
		let obj = if needs_unwrap then CsField (obj, "value") else obj in
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
		   - Var fields with Dynamic type (like `public var fn: Dynamic` holding a function)
		   - Method MethDynamic (like `public dynamic function onAbort(...)`) which are also stored functions *)
		let is_stored_function_field = match cf.cf_kind with
			| Var _ -> (match follow cf.cf_type with TFun _ | TDynamic _ -> true | _ -> false)
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
			(* Use FunctionValue-based invoke to avoid boxing primitives *)
			let num_args = List.length args_cs in
			let functionvalue_args = generate_functionvalue_args args_cs param_types_cs in
			let call_expr = CsCall (CsField (func_expr, functionvalue_invoke_method_name num_args), functionvalue_args) in
			(* Extract the return value from FunctionValue using the appropriate ToXxx method *)
			begin match result_type with
			| CsTypeVoid -> call_expr  (* FunctionValue.Missing() returned, ignored *)
			| CsTypeInt -> CsCall (CsField (call_expr, "ToInt"), [])
			| CsTypeDouble -> CsCall (CsField (call_expr, "ToDouble"), [])
			| CsTypeFloat -> CsCall (CsField (call_expr, "ToFloat"), [])
			| CsTypeBool -> CsCall (CsField (call_expr, "ToBool"), [])
			| CsTypeLong -> CsCall (CsField (call_expr, "ToLong"), [])
			| CsTypeString -> CsCall (CsField (call_expr, "ToStringValue"), [])
			| CsTypeClass ((["haxe"; "lang"], "Null"), [inner]) ->
				(* Null<T>: use ToNullXxx() methods *)
				begin match inner with
				| CsTypeInt -> CsCall (CsField (call_expr, "ToNullInt"), [])
				| CsTypeDouble -> CsCall (CsField (call_expr, "ToNullDouble"), [])
				| CsTypeFloat -> CsCall (CsField (call_expr, "ToNullFloat"), [])
				| CsTypeBool -> CsCall (CsField (call_expr, "ToNullBool"), [])
				| CsTypeLong -> CsCall (CsField (call_expr, "ToNullLong"), [])
				| _ ->
					let null_type = CsTypeClass ((["haxe"; "lang"], "Null"), [inner]) in
					CsStaticCall (null_type, "_ofDynamic", [CsCall (CsField (call_expr, "ToDynamic"), [])])
				end
			| CsTypeObject | CsTypeDynamic -> CsCall (CsField (call_expr, "ToDynamic"), [])
			| _ ->
				(* For other reference types, get obj field and cast *)
				CsCast (result_type, CsField (call_expr, "obj"))
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
		   BUT: Don't double-wrap if the type is already Null<T>.

		   CRITICAL: For override methods, use the PARENT's parameter types to match the
		   generated C# method signature. C# override methods must have exact type match,
		   so we use parent types (e.g., K instead of EnumValue) in the signature.
		   The call coercion must use the same types. *)
		let get_parent_param_types c_class tl_class cf_method =
			let rec find_parent_types c_super tl =
				let map_type = apply_params c_super.cl_params tl in
				try
					let cf_super = PMap.find cf_method.cf_name c_super.cl_fields in
					match cf_super.cf_kind with
					| Method _ ->
						begin match follow (map_type cf_super.cf_type) with
						| TFun (parent_params, _) -> Some parent_params
						| _ -> None
						end
					| _ -> None
				with Not_found ->
					match c_super.cl_super with
					| Some (grandparent, tl2) -> find_parent_types grandparent (List.map map_type tl2)
					| None -> None
			in
			match c_class.cl_super with
			| Some (c_super, tl_super) ->
				let map_type = apply_params c_class.cl_params tl_class in
				find_parent_types c_super (List.map map_type tl_super)
			| None -> None
		in
		let param_types_base =
			(* For override methods, prefer parent's param types *)
			let use_parent_types = has_class_field_flag cf CfOverride in
			let parent_params = if use_parent_types then get_parent_param_types c tl cf else None in
			match parent_params with
			| Some params ->
				(* Use parent's param types - apply class type params mapping *)
				let map_type = apply_params c.cl_params tl in
				List.map (fun (_, opt, t) ->
					let t = map_type t in
					let is_already_null = match follow t with
						| TAbstract ({ a_path = ([], "Null") }, _) -> true
						| _ -> false
					in
					if opt && not is_already_null then ectx.gctx.com.basic.tnull t else t
				) params
			| None ->
				(* Not an override or parent not found - use method's own type *)
				match follow cf.cf_type with
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
				(* Helper to check if a type is Dynamic *)
				let is_dynamic_type t = match follow t with TDynamic _ -> true | _ -> false in
				List.map (fun ttp ->
					(* Try to find the best type for this type param, preferring non-Dynamic types.
					   We look through all param/arg pairs and prefer specific types over Dynamic. *)
					let found_type = List.fold_left (fun acc (param_t, arg_t) ->
						let this_match = find_type_param_in_type ttp.ttp_name param_t arg_t in
						match acc, this_match with
						| None, _ -> this_match  (* First match *)
						| Some prev, Some curr ->
							(* Prefer non-Dynamic over Dynamic *)
							if is_dynamic_type prev && not (is_dynamic_type curr) then Some curr
							else acc
						| _ -> acc
					) None param_type_pairs in
					match found_type with
					| Some t -> t
					| None -> t_dynamic  (* Fall back to Dynamic if not found *)
				) cf.cf_params
			in
			(* Get method type params as Haxe types for applying to param_types *)
			let method_type_params_hx =
				(* Check if the method's return type in the signature IS a type parameter.
				   For methods like copy<T>(v:T):T, the return type is T itself.
				   In that case, the entire actual return type is the value of that type param.
				   IMPORTANT: Don't use follow() on ret - it unwraps Null<T> to T via abstract semantics.
				   We only want to return true if the signature literally has T as return, not Null<T>. *)
				let sig_return_is_type_param = match follow cf.cf_type with
					| TFun (_, ret) -> begin match ret with
						| TInst ({ cl_kind = KTypeParameter _ }, _) -> true
						| TMono { tm_type = Some t } ->
							begin match t with
							| TInst ({ cl_kind = KTypeParameter _ }, _) -> true
							| _ -> false
							end
						| _ -> false
					end
					| _ -> false
				in
				(* Check if method's signature is Null<T> where T is directly a type param.
				   This is different from Null<Class<T>> where the inner type wraps the param. *)
				let method_returns_null_of_type_param = match cf.cf_type with
					| TFun (_, ret) ->
						begin match ret with
						| TAbstract ({ a_path = ([], "Null") }, [inner]) ->
							begin match inner with
							| TInst ({ cl_kind = KTypeParameter _ }, _) -> true
							| TMono { tm_type = Some t } ->
								begin match t with
								| TInst ({ cl_kind = KTypeParameter _ }, _) -> true
								| _ -> false
								end
							| _ -> false
							end
						| _ -> false
						end
					| _ -> false
				in
				if sig_return_is_type_param && List.length cf.cf_params = 1 then
					(* The whole return type is the type param value *)
					[return_type]
				else if method_returns_null_of_type_param && List.length cf.cf_params = 1 then
					(* Method signature is Null<T> where T is directly a type param (e.g., Lambda.find).
					   Extract T from the call-site expected type Null<inner>.
					   Use follow_once to peel TMono but keep Null<> intact. *)
					let return_type_once = Type.follow_once return_type in
					match return_type_once with
					| TAbstract ({ a_path = ([], "Null") }, [inner]) ->
						(* For Null<T> return types, extract T from the actual return type Null<inner>.
						   If inner is also Null<X>, extract X to avoid double-wrapping. *)
						let inner_followed = follow inner in
						let unwrapped_inner = match inner_followed with
							| TAbstract ({ a_path = ([], "Null") }, [x]) -> x
							| _ -> inner_followed
						in
						[unwrapped_inner]
					| _ -> infer_type_params_as_types ()
				else
						(* For other cases, use follow for proper type resolution *)
						match follow return_type with
						| TInst (_, ret_params) when List.length ret_params = List.length cf.cf_params ->
							ret_params
						| TAbstract (_, ret_params) when List.length ret_params = List.length cf.cf_params ->
							ret_params
						| _ -> infer_type_params_as_types ()
			in
			(* Apply method type params to parameter types *)
			let method_param_map = apply_params cf.cf_params method_type_params_hx in
			let param_types = List.map method_param_map param_types_base in
			let cs_args = generate_call_args ectx cs_expr_of_texpr args param_types in
			let method_type_params = List.map (cs_type_of_type ectx.gctx) method_type_params_hx in
			(* C# can't use void as a type argument, so substitute void with object.
			   This is safe because at runtime the type doesn't matter for these cases
			   (e.g., makeVarArgs<T> where T is the return type of a function). *)
			let method_type_params = List.map (fun t ->
				if t = CsTypeVoid then CsTypeObject else t
			) method_type_params in
			CsCallGeneric (CsField (obj, get_native_field_name cf), method_type_params, cs_args)
		end else begin
			let cs_args = generate_call_args ectx cs_expr_of_texpr args param_types_base in
			CsCall (CsField (obj, get_native_field_name cf), cs_args)
		end
		end  (* close the is_var_with_func_type else branch *)
	| TCall ({ eexpr = TField (e_obj, FAnon cf) }, args) ->
		(* Method call on anonymous/structural type.
		   First, check if expression type is Null<T> - if so, unwrap via .value *)
		(* NOTE: Use follow_once to peel through TMono but not unwrap Null<T> *)
		(* NOTE: e_obj is the object expression, e is the whole TCall expression (outer match var) *)
		let obj = cs_expr_of_texpr ectx e_obj in
		let raw_type = Type.follow_once e_obj.etype in
		let inner_type, obj = match raw_type with
			| TAbstract ({ a_path = ([], "Null") }, [inner]) ->
				(* Null<T> -> access .value to unwrap *)
				(inner, CsField (obj, "value"))
			| _ -> (e_obj.etype, obj)
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
			(* Truly anonymous - use _hx_getField -> Runtime.InvokeDelegate for function fields *)
			let is_function = match follow cf.cf_type with TFun _ -> true | _ -> false in
			let field_call = CsCall (CsField (obj, "_hx_getField"), [CsConst (CsConstString cf.cf_name)]) in
			if is_function then begin
				(* Build an array of arguments: Array<object>.ofNative(new object[] { ... }) *)
				let args_array = if args = [] then
					CsNew (CsTypeClass (NativeTypes.haxe_array_path, [CsTypeObject]), [])
				else
					let native_array = CsNewArray (CsTypeObject, args) in
					CsStaticCall (CsTypeClass (NativeTypes.haxe_array_path, [CsTypeObject]), "ofNative", [native_array])
				in
				let call_expr = CsStaticCall (CsTypeClass ((["haxe"; "lang"], "Runtime"), []), "InvokeDelegate", [field_call; args_array]) in
				(* Cast the result to the expected return type.
				   Use e.etype (the TCall's return type) which has type parameters resolved,
				   rather than cf.cf_type which might have unresolved type params. *)
				let result_type = cs_type_of_type ectx.gctx e.etype in
				begin match result_type with
				| CsTypeVoid | CsTypeObject | CsTypeDynamic -> call_expr
				| _ -> CsCast (result_type, call_expr)
				end
			end else begin
				(* Non-function field - just get and cast *)
				let func_type = CsSignature.erase_type_params (cs_type_of_type ectx.gctx cf.cf_type) in
				CsCast (func_type, field_call)
			end
		| CsTypeClass (path, _) ->
			(* Some other concrete class - try direct call *)
			CsCall (CsField (obj, escape_identifier cf.cf_name), args)
		| CsTypeObject ->
			(* Object type (from TAnon/structural type) - use Reflect.field then Runtime.InvokeDelegate *)
			let reflect_path = (["haxe"; "root"], "Reflect") in
			let field_call = CsStaticCall (CsTypeClass (reflect_path, []), "field", [obj; CsConst (CsConstString cf.cf_name)]) in
			(* Build an array of arguments: Array<object>.ofNative(new object[] { ... }) *)
			let args_array = if args = [] then
				CsNew (CsTypeClass (NativeTypes.haxe_array_path, [CsTypeObject]), [])
			else
				let native_array = CsNewArray (CsTypeObject, args) in
				CsStaticCall (CsTypeClass (NativeTypes.haxe_array_path, [CsTypeObject]), "ofNative", [native_array])
			in
			let call_expr = CsStaticCall (CsTypeClass ((["haxe"; "lang"], "Runtime"), []), "InvokeDelegate", [field_call; args_array]) in
			(* Cast the result to the expected return type.
			   Use e.etype (the TCall's return type) which has type parameters resolved. *)
			let result_type = cs_type_of_type ectx.gctx e.etype in
			begin match result_type with
			| CsTypeVoid | CsTypeObject | CsTypeDynamic -> call_expr
			| _ -> CsCast (result_type, call_expr)
			end
		| CsTypeGenericParam _ ->
			(* Type parameter - cast through object to HaxeObject to call _hx_getField *)
			let haxe_object_type = CsTypeClass ((["haxe"; "root"], "HaxeObject"), []) in
			let casted_obj = CsCast (haxe_object_type, CsCast (CsTypeObject, obj)) in
			let field_call = CsCall (CsField (casted_obj, "_hx_getField"), [CsConst (CsConstString cf.cf_name)]) in
			(* Build an array of arguments: Array<object>.ofNative(new object[] { ... }) *)
			let args_array = if args = [] then
				CsNew (CsTypeClass (NativeTypes.haxe_array_path, [CsTypeObject]), [])
			else
				let native_array = CsNewArray (CsTypeObject, args) in
				CsStaticCall (CsTypeClass (NativeTypes.haxe_array_path, [CsTypeObject]), "ofNative", [native_array])
			in
			let call_expr = CsStaticCall (CsTypeClass ((["haxe"; "lang"], "Runtime"), []), "InvokeDelegate", [field_call; args_array]) in
			let result_type = cs_type_of_type ectx.gctx e.etype in
			begin match result_type with
			| CsTypeVoid | CsTypeObject | CsTypeDynamic -> call_expr
			| _ -> CsCast (result_type, call_expr)
			end
		| _ ->
			(* Fallback to dynamic dispatch via _hx_getField -> Runtime.InvokeDelegate *)
			let field_call = CsCall (CsField (obj, "_hx_getField"), [CsConst (CsConstString cf.cf_name)]) in
			(* Build an array of arguments: Array<object>.ofNative(new object[] { ... }) *)
			let args_array = if args = [] then
				CsNew (CsTypeClass (NativeTypes.haxe_array_path, [CsTypeObject]), [])
			else
				let native_array = CsNewArray (CsTypeObject, args) in
				CsStaticCall (CsTypeClass (NativeTypes.haxe_array_path, [CsTypeObject]), "ofNative", [native_array])
			in
			let call_expr = CsStaticCall (CsTypeClass ((["haxe"; "lang"], "Runtime"), []), "InvokeDelegate", [field_call; args_array]) in
			(* Cast the result to the expected return type.
			   Use e.etype (the TCall's return type) which has type parameters resolved. *)
			let result_type = cs_type_of_type ectx.gctx e.etype in
			begin match result_type with
			| CsTypeVoid | CsTypeObject | CsTypeDynamic -> call_expr
			| _ -> CsCast (result_type, call_expr)
			end
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
		(* Also erase out-of-scope type params *)
		let result_type = cs_type_of_type ectx.gctx e.etype in
		let result_type = CsSignature.erase_out_of_scope_type_params ectx.type_params_in_scope result_type in
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
		(* Check if this is a stored function field (dynamic method, var with function type, or Dynamic type).
		   If so, we need to use Runtime.InvokeDelegate instead of direct call. *)
		let is_stored_function_field = match cf.cf_kind with
			| Var _ -> (match follow cf.cf_type with TFun _ | TDynamic _ -> true | _ -> false)
			| Method MethDynamic -> true
			| Method _ -> false
		in
		if is_stored_function_field then begin
			(* Stored function field - use Runtime.InvokeDelegate *)
			let path = cs_path_of_path c.cl_path in
			let class_type_params = List.map (fun _ -> CsTypeObject) c.cl_params in
			let func_expr = CsStaticField (CsTypeClass (path, class_type_params), escape_identifier cf.cf_name) in
			let args_exprs = List.map (cs_expr_of_texpr ectx) orig_args in
			let args_array = if args_exprs = [] then
				CsNew (CsTypeClass (NativeTypes.haxe_array_path, [CsTypeObject]), [])
			else
				let native_array = CsNewArray (CsTypeObject, args_exprs) in
				CsStaticCall (CsTypeClass (NativeTypes.haxe_array_path, [CsTypeObject]), "ofNative", [native_array])
			in
			let call_expr = CsStaticCall (CsTypeClass ((["haxe"; "lang"], "Runtime"), []), "InvokeDelegate", [func_expr; args_array]) in
			let result_type = cs_type_of_type ectx.gctx return_type in
			begin match result_type with
			| CsTypeVoid | CsTypeObject | CsTypeDynamic -> call_expr
			| _ -> CsCast (result_type, call_expr)
			end
		end else begin
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
		(* Get ALL method type params, including those inferred from the method's OWN signature.
		   For abstract _Impl_ classes, the abstract's type params become method-level params
		   but may not be in cf.cf_params. We need to include them for correct call generation.
		   IMPORTANT: We must use the METHOD's signature (cf.cf_type), not the instantiated types,
		   to avoid picking up type params from enclosing scopes. *)
		let explicit_type_params = List.map (fun ttp -> ttp.ttp_name) cf.cf_params in
		(* Get the method's own signature types (not instantiated) for inferring additional params *)
		let orig_param_types, orig_ret_type = match follow cf.cf_type with
			| TFun (params, ret) ->
				List.map (fun (_, _, t) -> cs_type_of_type ectx.gctx t) params,
				cs_type_of_type ectx.gctx ret
			| _ -> [], CsTypeObject
		in
		let all_signature_type_params = CsSignature.get_method_type_params orig_param_types orig_ret_type in
		(* Filter out class type params from inferred params *)
		let class_type_param_names = List.map (fun ttp -> ttp.ttp_name) c.cl_params in
		let inferred_type_params = List.filter (fun p ->
			not (List.mem p class_type_param_names) && not (List.mem p explicit_type_params)
		) all_signature_type_params in
		(* Combine explicit and inferred params *)
		let all_method_type_params = explicit_type_params @ inferred_type_params in
		(* Check if method has any type parameters (explicit or inferred) *)
		if all_method_type_params <> [] then begin
			(* Method has type params - infer from return type, arguments, or method signature *)
			(* Check if the return type is a type parameter T (i.e., the method returns T directly).
			   IMPORTANT: Don't use follow() here - it unwraps Null<T> to T via abstract semantics.
			   We only want to return true if the signature literally has T as return, not Null<T>. *)
			let returns_type_param = match cf.cf_type with
				| TFun (_, ret) ->
					begin match ret with
					| TInst ({ cl_kind = KTypeParameter _ }, _) -> true
					| TMono { tm_type = Some t } ->
						begin match t with
						| TInst ({ cl_kind = KTypeParameter _ }, _) -> true
						| _ -> false
						end
					| _ -> false
					end
				| _ -> false
			in
			(* Check if method's signature is Null<T> where T is directly a type param.
			   This is different from Null<Class<T>> where the inner type wraps the param. *)
			let method_returns_null_of_type_param = match cf.cf_type with
				| TFun (_, ret) ->
					begin match ret with
					| TAbstract ({ a_path = ([], "Null") }, [inner]) ->
						begin match inner with
						| TInst ({ cl_kind = KTypeParameter _ }, _) -> true
						| TMono { tm_type = Some t } ->
							begin match t with
							| TInst ({ cl_kind = KTypeParameter _ }, _) -> true
							| _ -> false
							end
						| _ -> false
						end
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
					(* Follow both types, and for arg_t also follow through abstracts to underlying type *)
					let param_t_f = follow param_t in
					let arg_t_f = follow arg_t in
					(* Also get the argument type with abstracts followed (for matching param like Array<T> with abstract wrapping Array) *)
					let arg_t_underlying = Abstract.follow_with_abstracts arg_t in
					match param_t_f, arg_t_f with
					| TInst ({ cl_kind = KTypeParameter ttp2 }, _), _ when ttp2.ttp_name = ttp_name ->
						(* Direct type parameter match *)
						let unwrapped = match arg_t_f with
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
					| TInst (c1, tp1_list), TAbstract (_, _) ->
						(* param_t is a class like Array<T>, arg_t is an abstract - try matching underlying type *)
						begin match arg_t_underlying with
						| TInst (c2, tp2_list) when c1.cl_path = c2.cl_path && List.length tp1_list = List.length tp2_list ->
							List.fold_left2 (fun acc tp1 tp2 ->
								match acc with
								| Some _ -> acc
								| None -> find_type_param_in_type ttp_name tp1 tp2
							) None tp1_list tp2_list
						| _ -> None
						end
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
				(* Helper to check if a type is Dynamic *)
				let is_dynamic_type t = match follow t with TDynamic _ -> true | _ -> false in
				(* Iterate over ALL type params (explicit + inferred), using names *)
				List.map (fun ttp_name ->
					(* Try to find the best type for this type param, preferring non-Dynamic types. *)
					let found_type = List.fold_left (fun acc (param_t, arg_t) ->
						let this_match = find_type_param_in_type ttp_name param_t arg_t in
						match acc, this_match with
						| None, _ -> this_match  (* First match *)
						| Some prev, Some curr ->
							(* Prefer non-Dynamic over Dynamic *)
							if is_dynamic_type prev && not (is_dynamic_type curr) then Some curr
							else acc
						| _ -> acc
					) None param_type_pairs in
					match found_type with
					| Some t -> t
					| None -> t_dynamic  (* Fallback to Dynamic if can't infer *)
				) all_method_type_params
			in
			(* Get method type params as Haxe types for applying to param_types *)
				let method_type_params_hx =
				(* FIRST check if method returns T directly (like copy<T>():T) *)
				if returns_type_param && List.length all_method_type_params = 1 then
					(* Method returns T directly - the whole return type is the type param value *)
					[return_type]
				else if method_returns_null_of_type_param && List.length all_method_type_params = 1 then
					(* Method signature is Null<T> where T is directly a type param (e.g., Lambda.find).
					   Extract T from the call-site expected type Null<inner>.
					   Use follow_once to peel TMono but keep Null<> intact. *)
					let return_type_once = Type.follow_once return_type in
					match return_type_once with
					| TAbstract ({ a_path = ([], "Null") }, [inner]) ->
						(* For Null<T> return types, extract T from the actual return type Null<inner>.
						   If inner is also Null<X>, extract X to avoid double-wrapping. *)
						let inner_followed = follow inner in
						let unwrapped_inner = match inner_followed with
							| TAbstract ({ a_path = ([], "Null") }, [x]) -> x
							| _ -> inner_followed
						in
						[unwrapped_inner]
					| _ -> infer_type_params_as_types ()
				else
						(* For other cases, use follow for proper type resolution *)
						match follow return_type with
						| TInst (_, ret_params) when List.length ret_params = List.length all_method_type_params ->
							(* Generic return type with matching arity - use its type params *)
							ret_params
						| TAbstract (_, ret_params) when List.length ret_params = List.length all_method_type_params ->
							ret_params
						| _ -> infer_type_params_as_types ()
			in
			(* Apply method type params to parameter types - only for explicit params that have bindings *)
			let method_param_map = apply_params cf.cf_params (ExtList.List.take (List.length cf.cf_params) method_type_params_hx) in
			let param_types = List.map method_param_map param_types_base in
			let args = generate_call_args ectx cs_expr_of_texpr orig_args param_types in
			let method_type_params = List.map (cs_type_of_type ectx.gctx) method_type_params_hx in
			(* C# can't use void as a type argument, so substitute void with object.
			   This is safe because at runtime the type doesn't matter for these cases
			   (e.g., makeVarArgs<T> where T is the return type of a function). *)
			let method_type_params = List.map (fun t ->
				if t = CsTypeVoid then CsTypeObject else t
			) method_type_params in
			CsStaticCallGeneric (CsTypeClass (path, class_type_params), escape_identifier cf.cf_name, method_type_params, args)
		end else begin
			let args = generate_call_args ectx cs_expr_of_texpr orig_args param_types_base in
			CsStaticCall (CsTypeClass (path, class_type_params), escape_identifier cf.cf_name, args)
		end
		end  (* close is_stored_function_field else branch *)
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
			| TInst ({ cl_path = (["haxe"; "lang"], "Function") }, _) -> true  (* haxe.lang.Function class *)
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
			(* Use FunctionValue-based invoke to avoid boxing primitives *)
			let num_args = List.length args_cs in
			let functionvalue_args = generate_functionvalue_args args_cs param_types_cs in
			let call_expr = CsCall (CsField (func, functionvalue_invoke_method_name num_args), functionvalue_args) in
			(* Extract the return value from FunctionValue using the appropriate ToXxx method *)
			begin match result_type with
			| CsTypeVoid -> call_expr  (* FunctionValue.Missing() returned, ignored *)
			| CsTypeInt -> CsCall (CsField (call_expr, "ToInt"), [])
			| CsTypeDouble -> CsCall (CsField (call_expr, "ToDouble"), [])
			| CsTypeFloat -> CsCall (CsField (call_expr, "ToFloat"), [])
			| CsTypeBool -> CsCall (CsField (call_expr, "ToBool"), [])
			| CsTypeLong -> CsCall (CsField (call_expr, "ToLong"), [])
			| CsTypeString -> CsCall (CsField (call_expr, "ToStringValue"), [])
			| CsTypeClass ((["haxe"; "lang"], "Null"), [inner]) ->
				(* Null<T>: use ToNullXxx() methods *)
				begin match inner with
				| CsTypeInt -> CsCall (CsField (call_expr, "ToNullInt"), [])
				| CsTypeDouble -> CsCall (CsField (call_expr, "ToNullDouble"), [])
				| CsTypeFloat -> CsCall (CsField (call_expr, "ToNullFloat"), [])
				| CsTypeBool -> CsCall (CsField (call_expr, "ToNullBool"), [])
				| CsTypeLong -> CsCall (CsField (call_expr, "ToNullLong"), [])
				| _ ->
					let null_type = CsTypeClass ((["haxe"; "lang"], "Null"), [inner]) in
					CsStaticCall (null_type, "_ofDynamic", [CsCall (CsField (call_expr, "ToDynamic"), [])])
				end
			| CsTypeObject | CsTypeDynamic -> CsCall (CsField (call_expr, "ToDynamic"), [])
			| _ ->
				(* For other reference types, get obj field and cast *)
				CsCast (result_type, CsField (call_expr, "obj"))
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
			let cs_args = List.map (cs_expr_of_texpr ectx) args in
			(* Check if this class needs two-phase construction *)
			let ctor_needs_two_phase = match c.cl_constructor with
				| Some cf -> needs_two_phase_construction cf
				| None -> false
			in
			if ctor_needs_two_phase then
				(* Two-phase: (new ClassName((EmptyConstructor)null))._hx_new_inline(args)
				   We need to use an inline pattern since C# expressions can't have statements.
				   Actually, we generate: (() => { var tmp = new C((EmptyConstructor)null); tmp._hx_new(args); return tmp; })()
				   But that's ugly. Instead, let's use the HaxeNewHelper approach or just inline the code.
				   For now, generate a helper call that we can handle specially. *)
				let class_type = CsTypeClass (path, type_params) in
				(* Generate: global::haxe.lang.Runtime.createInstance<T>(new T((EmptyConstructor)null), args...)
				   where createInstance calls _hx_new on the instance.
				   Actually, simpler: we can use object initializer with a factory method,
				   or generate the two-call pattern in statement context only.

				   For expression context, we need a lambda or helper.
				   Let's generate: ((Func<ClassName>)(() => { var t = new ClassName((EmptyConstructor)null); t._hx_new(args); return t; }))()
				*)
				let empty_ctor_type = CsTypeClass ((["haxe"; "lang"], "EmptyConstructor"), []) in
				let new_expr = CsNew (class_type, [CsCast (empty_ctor_type, CsNull)]) in
				(* Create a lambda that creates the object and calls _hx_new *)
				let hx_new_call = CsCall (CsField (CsLocal "_hx_tmp", "_hx_new"), cs_args) in
				let return_stmt = CsReturn (Some (CsLocal "_hx_tmp")) in
				let lambda_body = CsLambdaBlock [
					CsVarDecl ("_hx_tmp", Some class_type, Some new_expr);
					CsExprStmt hx_new_call;
					return_stmt
				] in
				let lambda = CsLambda ([], lambda_body) in
				(* Cast to Func<T> and invoke: ((Func<T>)(() => {...}))() *)
				let func_type = CsTypeClass ((["System"], "Func"), [class_type]) in
				CsCall (CsParens (CsCast (func_type, lambda)), [])
			else
				CsNew (CsTypeClass (path, type_params), cs_args)
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
			(* Erase type parameters that are not in scope - if we have type params from
			   a called method's signature, they won't be valid in the current context.
			   For example, calling a generic method via reflection returns T, but T
			   is not defined in the calling context. Replace with object. *)
			let target_type_raw = CsSignature.erase_out_of_scope_type_params ectx.type_params_in_scope target_type_raw in
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
					(* C# doesn't allow direct casts between unrelated type parameters or
				   primitives to type parameters. Cast through object: (Target)(object)source *)
					let is_primitive_type = function
						| CsTypeBool | CsTypeByte | CsTypeSByte | CsTypeChar
						| CsTypeShort | CsTypeUShort | CsTypeInt | CsTypeUInt
						| CsTypeLong | CsTypeULong | CsTypeFloat | CsTypeDouble | CsTypeDecimal -> true
						| _ -> false
					in
					let needs_double_cast = match target_type, inner_type with
						| CsTypeGenericParam _, CsTypeGenericParam _ -> true  (* T to O *)
						| CsTypeGenericParam _, CsTypeClass _ -> true  (* SomeClass to T - needs (T)(object)v *)
						| CsTypeGenericParam _, t when is_primitive_type t -> true  (* double/int/etc to T - needs (T)(object)v *)
						| CsTypeClass _, CsTypeGenericParam _ when not is_target_null_wrapper -> true  (* T to SomeClass - needs (SomeClass)(object)v, but not for Null<T> *)
						| t, CsTypeGenericParam _ when is_primitive_type t -> true  (* T to double/int/etc - needs cast through object *)
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
			(* Ternary expression: cond ? then : else
			   In C#, both branches must have compatible types.
			   If the branch types differ from the result type, coerce them.
			   Also, if condition is object/Dynamic, cast it to bool for C#. *)
			let cond_cs = cs_expr_of_texpr ectx cond in
			(* In C#, ternary condition must be bool. If it's object/Dynamic, cast to bool. *)
			let cond_cs = match cs_type_of_type ectx.gctx cond.etype with
				| CsTypeObject | CsTypeDynamic -> CsCast (CsTypeBool, cond_cs)
				| _ -> cond_cs
			in
			let result_type = e.etype in
			let then_e = cs_expr_of_texpr ectx then_expr in
			let else_e = cs_expr_of_texpr ectx else_expr in
			(* Coerce branches to result type if types differ *)
			let then_e = coerce_arg ectx.gctx then_e then_expr.etype result_type in
			let else_e = coerce_arg ectx.gctx else_e else_expr.etype result_type in
			(* Special case: if result type is a function type (TFun) and branches are closures,
			   C# can't determine common type between closure classes. Cast both to haxe.lang.Function. *)
			let then_e, else_e = match follow result_type with
				| TFun _ ->
					let func_type = CsTypeClass ((["haxe"; "lang"], "Function"), []) in
					CsCast (func_type, then_e), CsCast (func_type, else_e)
				| _ -> then_e, else_e
			in
			CsTernary (cond_cs, then_e, else_e)
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
		(* Get the enum and its info from the expression type *)
		let en, enum_params = match follow e.etype with
			| TEnum (en, params) -> en, params
			| TAbstract ({ a_path = ([], "Null") }, [t]) ->
				begin match follow t with
				| TEnum (en, params) -> en, params
				| _ -> failwith "TEnumParameter on non-enum type"
				end
			| _ -> failwith "TEnumParameter on non-enum type"
		in
		let enum_path = cs_path_of_path en.e_path in
		let enum_type_params = List.map (cs_type_of_type ectx.gctx) enum_params in
		let ctor_name = escape_identifier ef.ef_name in
		(* For GADT constructors with their own type params, we need to infer the nested class's
		   type arguments. The nested class only has EXTRA params (not in parent enum). *)
		let parent_type_param_names = List.map (fun ttp -> ttp.ttp_name) en.e_params in
		let extra_ctor_params = List.filter (fun ttp ->
			not (List.mem ttp.ttp_name parent_type_param_names)
		) ef.ef_params in
		(* Infer extra type params by matching the constructor's return type with the expression type.
		   e.g., Cons<X, L> returns Stack<TCons<X, L>>; if e.etype = Stack<TCons<S, T>>,
		   then X = S (the extra param we need). *)
		let ctor_type_args = if extra_ctor_params = [] then [] else begin
			(* Get the constructor's return type pattern *)
			let return_type = match follow ef.ef_type with
				| TFun (_, ret) -> ret
				| t -> t  (* Not a function - shouldn't happen for constructors with params *)
			in
			(* Match return type pattern against actual enum type to find param values *)
			let rec find_type_param_in_type ttp_name param_t arg_t =
				match follow param_t, follow arg_t with
				| TInst ({ cl_kind = KTypeParameter ttp2 }, _), _ when ttp2.ttp_name = ttp_name ->
					Some arg_t
				| TInst (c1, tp1_list), TInst (c2, tp2_list) when c1.cl_path = c2.cl_path && List.length tp1_list = List.length tp2_list ->
					List.fold_left2 (fun acc tp1 tp2 ->
						match acc with Some _ -> acc | None -> find_type_param_in_type ttp_name tp1 tp2
					) None tp1_list tp2_list
				| TEnum (e1, tp1_list), TEnum (e2, tp2_list) when e1.e_path = e2.e_path && List.length tp1_list = List.length tp2_list ->
					List.fold_left2 (fun acc tp1 tp2 ->
						match acc with Some _ -> acc | None -> find_type_param_in_type ttp_name tp1 tp2
					) None tp1_list tp2_list
				| TAbstract (a1, tp1_list), TAbstract (a2, tp2_list) when a1.a_path = a2.a_path && List.length tp1_list = List.length tp2_list ->
					List.fold_left2 (fun acc tp1 tp2 ->
						match acc with Some _ -> acc | None -> find_type_param_in_type ttp_name tp1 tp2
					) None tp1_list tp2_list
				| _ -> None
			in
			List.map (fun ttp ->
				match find_type_param_in_type ttp.ttp_name return_type e.etype with
				| Some t -> cs_type_of_type ectx.gctx t
				| None -> CsTypeObject  (* Fallback to object if inference fails *)
			) extra_ctor_params
		end in
		(* Create the nested type with appropriate type args *)
		let parent_type = CsTypeClass (enum_path, enum_type_params) in
		let nested_type = if ctor_type_args = [] then
			CsTypeNested (parent_type, ctor_name)
		else
			CsTypeNestedGeneric (parent_type, ctor_name, ctor_type_args)
		in
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
	| TThrow throw_e ->
		(* Throw as expression in return context - just emit throw statement, not "return throw" *)
		CsThrowStmt (cs_expr_of_texpr ectx throw_e)
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
		(* In C#, if condition must be bool. If it's object/Dynamic, cast to bool. *)
		let cond_cs = cs_expr_of_texpr ectx cond in
		let cond_cs = match cs_type_of_type ectx.gctx cond.etype with
			| CsTypeObject | CsTypeDynamic -> CsCast (CsTypeBool, cond_cs)
			| _ -> cond_cs
		in
		let then_stmt = cs_stmt_of_texpr ectx then_expr in
		let else_stmt = Option.map (cs_stmt_of_texpr ectx) else_expr in
		CsIf (cond_cs, then_stmt, else_stmt)
	| TWhile (cond, body, NormalWhile) ->
		(* In C#, while condition must be bool. If it's object/Dynamic, cast to bool. *)
		let cond_cs = cs_expr_of_texpr ectx cond in
		let cond_cs = match cs_type_of_type ectx.gctx cond.etype with
			| CsTypeObject | CsTypeDynamic -> CsCast (CsTypeBool, cond_cs)
			| _ -> cond_cs
		in
		let body = cs_stmt_of_texpr ectx body in
		CsWhile (cond_cs, body)
	| TWhile (cond, body, DoWhile) ->
		(* In C#, do-while condition must be bool. If it's object/Dynamic, cast to bool. *)
		let cond_cs = cs_expr_of_texpr ectx cond in
		let cond_cs = match cs_type_of_type ectx.gctx cond.etype with
			| CsTypeObject | CsTypeDynamic -> CsCast (CsTypeBool, cond_cs)
			| _ -> cond_cs
		in
		let body = cs_stmt_of_texpr ectx body in
		CsDoWhile (body, cond_cs)
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
		(* Special case: returning a throw expression should just be the throw statement.
		   "return throw ..." is not valid C# syntax. *)
		begin match e.eexpr with
		| TThrow throw_e ->
			CsThrowStmt (cs_expr_of_texpr ectx throw_e)
		| _ ->
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
				| Some ret_t when is_type_param ret_t && not (is_type_param e.etype) ->
					(* GADT pattern: returning concrete type (string, int, etc.) but method returns T.
					   Haxe typer knows the type is correct, but C# needs explicit cast through object. *)
					let ret_cs = cs_type_of_type ectx.gctx ret_t in
					CsCast (ret_cs, CsCast (CsTypeObject, cs_e))
				| Some ret_t when is_type_param e.etype && not (is_type_param ret_t) && not (is_dynamic ret_t) ->
					(* Constrained type param: expression is T but return type is concrete (e.g., T:(Float) -> Float).
					   Haxe knows T can be used as Float, but C# needs cast through object. *)
					let ret_cs = cs_type_of_type ectx.gctx ret_t in
					CsCast (ret_cs, CsCast (CsTypeObject, cs_e))
				| _ ->
					cs_e
			in
			CsReturn (Some return_expr)
		end
		end  (* close begin match for TThrow check *)
	| TBreak ->
		CsBreak
	| TContinue ->
		CsContinue
	| TThrow e ->
		CsThrowStmt (cs_expr_of_texpr ectx e)
	| _ ->
		(* Expression statement *)
		CsExprStmt (cs_expr_of_texpr ectx e)

(* Transform void returns (CsReturn None) to null returns (CsReturn (Some CsNull)).
   Used when a void-returning Haxe closure shadows a base class method that returns object. *)
let rec transform_void_returns_to_null stmt =
	match stmt with
	| CsReturn None -> CsReturn (Some CsNull)
	| CsBlock stmts -> CsBlock (List.map transform_void_returns_to_null stmts)
	| CsStmtList stmts -> CsStmtList (List.map transform_void_returns_to_null stmts)
	| CsIf (cond, then_stmt, else_opt) ->
		CsIf (cond, transform_void_returns_to_null then_stmt,
			Option.map transform_void_returns_to_null else_opt)
	| CsSwitch (expr, sections) ->
		let sections' = List.map (fun s ->
			{ s with sw_body = List.map transform_void_returns_to_null s.sw_body }
		) sections in
		CsSwitch (expr, sections')
	| CsWhile (cond, body) ->
		CsWhile (cond, transform_void_returns_to_null body)
	| CsDoWhile (body, cond) ->
		CsDoWhile (transform_void_returns_to_null body, cond)
	| CsFor (init, cond, iter, body) ->
		CsFor (init, cond, iter, transform_void_returns_to_null body)
	| CsForeach (t, name, expr, body) ->
		CsForeach (t, name, expr, transform_void_returns_to_null body)
	| CsTry (body, catches, finally_opt) ->
		let catches' = List.map (fun c ->
			{ c with catch_body = transform_void_returns_to_null c.catch_body }
		) catches in
		CsTry (transform_void_returns_to_null body, catches',
			Option.map transform_void_returns_to_null finally_opt)
	| CsUsing (decls, body) ->
		CsUsing (decls, transform_void_returns_to_null body)
	| CsLock (expr, body) ->
		CsLock (expr, transform_void_returns_to_null body)
	| _ -> stmt

(* Check if a statement definitely returns (ends with a return statement) *)
let rec stmt_has_return stmt =
	match stmt with
	| CsReturn _ -> true
	| CsThrowStmt _ -> true
	| CsBlock stmts -> (match List.rev stmts with [] -> false | last :: _ -> stmt_has_return last)
	| CsStmtList stmts -> (match List.rev stmts with [] -> false | last :: _ -> stmt_has_return last)
	| _ -> false

(* Check if a statement list ends with a return *)
let stmts_end_with_return stmts =
	match List.rev stmts with
	| [] -> false
	| last :: _ -> stmt_has_return last

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
		type_params_in_scope = ectx.type_params_in_scope;  (* Inherit type params from parent *)
		type_param_constraints = ectx.type_param_constraints;  (* Inherit constraints from parent *)
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
	(* Wrap in unchecked if the expression contains non-zero integer constants *)
	let invoke_body =
		if needs_unchecked tf.tf_expr then
			[CsUncheckedStmt (CsBlock invoke_body)]
		else
			invoke_body
	in

	(* The typed invoke method may shadow the base class's invokeN(object...) method.
	   For invoke() with 0 params, it matches exactly - add 'new' modifier.
	   For invokeN with all object params, it also matches - add 'new' modifier.
	   Otherwise, the parameter types differ so no 'new' needed.

	   IMPORTANT: When shadowing with 'new', the base class method returns 'object',
	   so if our return_type is void, we must:
	   1. Change return type to object to match the base signature
	   2. Append "return null;" to the body since C# requires explicit return *)
	let num_params = List.length invoke_params in
	let all_params_are_object = List.for_all (fun p ->
		match p.p_type with
		| Some CsTypeObject | Some CsTypeDynamic -> true
		| None -> true  (* untyped = object *)
		| _ -> false
	) invoke_params in
	let shadows_base = num_params = 0 || all_params_are_object in
	let invoke_modifiers = if shadows_base then [MemberModifier.New] else [] in
	(* When shadowing with 'new' modifier and return_type is void, change to object
	   since the base class invoke() returns object. *)
	let invoke_return_type = if shadows_base && return_type = CsTypeVoid then CsTypeObject else return_type in
	(* If the final return type is object (either because we changed from void, or it was already object),
	   AND the body might contain "return;" (CsReturn None), transform those to "return null;".
	   We only do this for object return type because CsReturn None is valid ONLY for void methods.
	   Only append return null if the body doesn't already end with a return. *)
	let invoke_body =
		if invoke_return_type = CsTypeObject then
			let transformed = List.map transform_void_returns_to_null invoke_body in
			(* Only append return null if body doesn't already end with a return *)
			if stmts_end_with_return transformed then transformed
			else transformed @ [CsReturn (Some CsNull)]
		else
			invoke_body
	in
	let invoke_method = CsMemberMethod {
		m_name = invoke_method_name num_params;
		m_return_type = invoke_return_type;
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

	(* Build __hx_invokeN method - FunctionValue-based invoke to avoid boxing.
	   Each argument is passed as a FunctionValue struct that holds primitives in prim field
	   and references in obj field. The kind field indicates which slot contains the value.
	   Extract values using ToInt(), ToDouble(), ToObject(), etc.
	   Returns FunctionValue to avoid boxing on return values too. *)
	let functionvalue_type = CsTypeClass ((["haxe"; "lang"], "FunctionValue"), []) in
	let functionvalue_invoke_method =
		if num_params = 0 then
			(* No params - just override __hx_invoke0 to call invoke() and wrap result *)
			let invoke_result = CsCall (CsLocal "invoke", []) in
			let body = if return_type = CsTypeVoid then
				[CsExprStmt invoke_result; CsReturn (Some (CsStaticCall (functionvalue_type, "Missing", [])))]
			else
				(* Wrap return value with appropriate FunctionValue.FromXxx *)
				let wrapped_result = match return_type with
					| CsTypeInt -> CsStaticCall (functionvalue_type, "FromInt", [invoke_result])
					| CsTypeDouble -> CsStaticCall (functionvalue_type, "FromDouble", [invoke_result])
					| CsTypeFloat -> CsStaticCall (functionvalue_type, "FromFloat", [invoke_result])
					| CsTypeBool -> CsStaticCall (functionvalue_type, "FromBool", [invoke_result])
					| CsTypeLong -> CsStaticCall (functionvalue_type, "FromLong", [invoke_result])
					| _ -> CsStaticCall (functionvalue_type, "FromObject", [invoke_result])
				in
				[CsReturn (Some wrapped_result)]
			in
			CsMemberMethod {
				m_name = "__hx_invoke0";
				m_return_type = functionvalue_type;
				m_access = AccessModifier.Public;
				m_modifiers = [MemberModifier.Override];
				m_type_params = [];
				m_params = [];
				m_body = Some body;
				m_constraints = [];
				m_explicit_interface = None;
				m_attributes = [];
			}
		else
			(* Build FunctionValue params: for each arg, FunctionValue aN *)
			let functionvalue_params = List.mapi (fun i _ ->
				{ p_name = "a" ^ string_of_int (i + 1); p_type = Some functionvalue_type; p_default = None; p_modifier = None }
			) invoke_params in
			(* Build extraction expressions for each argument.
			   Pattern: aN.ToInt(), aN.ToDouble(), aN.ToObject<T>(), etc.
			   For Null<T>: use aN.ToNullInt(), aN.ToNullDouble(), etc. *)
			let extract_args = List.mapi (fun i param ->
				let a_var = CsLocal ("a" ^ string_of_int (i + 1)) in
				match param.p_type with
				| Some CsTypeInt ->
					(* int: use ToInt() *)
					CsCall (CsField (a_var, "ToInt"), [])
				| Some CsTypeDouble ->
					(* double: use ToDouble() *)
					CsCall (CsField (a_var, "ToDouble"), [])
				| Some CsTypeFloat ->
					(* float (Single): use ToFloat() *)
					CsCall (CsField (a_var, "ToFloat"), [])
				| Some CsTypeBool ->
					(* bool: use ToBool() *)
					CsCall (CsField (a_var, "ToBool"), [])
				| Some CsTypeLong ->
					(* long: use ToLong() *)
					CsCall (CsField (a_var, "ToLong"), [])
				| Some CsTypeString ->
					(* string: use ToStringValue() *)
					CsCall (CsField (a_var, "ToStringValue"), [])
				| Some (CsTypeClass ((["haxe"; "lang"], "Null"), [inner])) ->
					(* Null<T>: use ToNullInt(), ToNullDouble(), etc. *)
					begin match inner with
						| CsTypeInt -> CsCall (CsField (a_var, "ToNullInt"), [])
						| CsTypeDouble -> CsCall (CsField (a_var, "ToNullDouble"), [])
						| CsTypeFloat -> CsCall (CsField (a_var, "ToNullFloat"), [])
						| CsTypeBool -> CsCall (CsField (a_var, "ToNullBool"), [])
						| CsTypeLong -> CsCall (CsField (a_var, "ToNullLong"), [])
						| _ ->
							(* For reference types, use ToNullObject<T>() - but C# needs explicit type *)
							(* We use ToDynamic() and wrap with Null<T>._ofDynamic for simplicity *)
							let null_type = CsTypeClass ((["haxe"; "lang"], "Null"), [inner]) in
							CsStaticCall (null_type, "_ofDynamic", [CsCall (CsField (a_var, "ToDynamic"), [])])
					end
				| Some t ->
					(* Other types (references): use obj field directly and cast *)
					CsCast (t, CsField (a_var, "obj"))
				| None ->
					(* Untyped: use ToDynamic() *)
					CsCall (CsField (a_var, "ToDynamic"), [])
			) invoke_params in
			let invoke_call = CsCall (CsLocal (invoke_method_name num_params), extract_args) in
			let body = if return_type = CsTypeVoid then
				[CsExprStmt invoke_call; CsReturn (Some (CsStaticCall (functionvalue_type, "Missing", [])))]
			else
				(* Wrap return value with appropriate FunctionValue.FromXxx *)
				let wrapped_result = match return_type with
					| CsTypeInt -> CsStaticCall (functionvalue_type, "FromInt", [invoke_call])
					| CsTypeDouble -> CsStaticCall (functionvalue_type, "FromDouble", [invoke_call])
					| CsTypeFloat -> CsStaticCall (functionvalue_type, "FromFloat", [invoke_call])
					| CsTypeBool -> CsStaticCall (functionvalue_type, "FromBool", [invoke_call])
					| CsTypeLong -> CsStaticCall (functionvalue_type, "FromLong", [invoke_call])
					| _ -> CsStaticCall (functionvalue_type, "FromObject", [invoke_call])
				in
				[CsReturn (Some wrapped_result)]
			in
			CsMemberMethod {
				m_name = "__hx_invoke" ^ string_of_int num_params;
				m_return_type = functionvalue_type;
				m_access = AccessModifier.Public;
				m_modifiers = [MemberModifier.Override];
				m_type_params = [];
				m_params = functionvalue_params;
				m_body = Some body;
				m_constraints = [];
				m_explicit_interface = None;
				m_attributes = [];
			}
	in

	(* Filter constraints to only include type params used by this closure *)
	let closure_constraints = List.filter (fun (name, _) ->
		List.mem name closure_type_params
	) ectx.type_param_constraints in

	(* Build the closure class *)
	let closure_class = CsClassDef {
		c_path = closure_path;
		c_access = AccessModifier.Internal;
		c_modifiers = [TypeModifier.Sealed];
		c_type_params = closure_type_params;
		c_base = Some (CsTypeClass ((["haxe"; "lang"], "Function"), []));
		c_interfaces = [];
		c_constraints = closure_constraints;
		c_members = capture_fields @ [ctor; invoke_method; invoke_dynamic_method; functionvalue_invoke_method];
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

	(* For instance methods, we need to capture the object.
	   We must erase type parameters from the captured type since the closure class
	   doesn't have access to the enclosing class's type parameters.
	   We also need to track the erased type to cast the capture expression when instantiating.
	   Special case: String is a C# built-in type, not a class.
	   Note: @:native("string") makes the path lowercase. *)
	let captures, erased_capture_type = if is_static then ([], None) else
		match obj_expr with
		| Some _ ->
			let obj_cs_type = match class_path with
				| ([], "String") | (["haxe"; "root"], "String")
				| ([], "string") | (["haxe"; "root"], "string") -> CsTypeString
				| _ -> CsTypeClass (cs_path_of_path class_path, List.map (cs_type_of_type gctx) type_params)
			in
			let erased_type = CsSignature.erase_type_params obj_cs_type in
			([("_hx_this", erased_type)], Some erased_type)
		| None -> ([], None)
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

	(* For methods with type parameters (like Reflect.compare<T>), erase type params to object
	   in the closure since we can't preserve them (C# closures can't have type params on invoke). *)
	let has_method_type_params = cf.cf_params <> [] in

	(* Build invoke method parameters - filter out Void parameters.
	   Keep track of which parameters are optional for invokeDynamic bounds checking. *)
	let invoke_params_with_opt = List.filter_map (fun (name, opt, t) ->
		if ExtType.is_void (follow t) then None
		else begin
			let base_type = cs_type_of_type gctx t in
			(* Erase method type parameters to object *)
			let base_type = if has_method_type_params then CsSignature.erase_type_params base_type else base_type in
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
	(* Erase method type parameters from return type too *)
	let return_cs_type = if has_method_type_params then CsSignature.erase_type_params return_cs_type else return_cs_type in

	(* Build invoke method body - call the actual method *)
	let call_args = List.map (fun param -> CsLocal param.p_name) invoke_params in
	let method_name = escape_identifier cf.cf_name in
	(* Check if this is a stored function field (dynamic method or var with function type).
	   If so, we need to use Runtime.InvokeDelegate instead of direct call. *)
	let is_stored_function = match cf.cf_kind with
		| Var _ -> (match follow cf.cf_type with TFun _ -> true | _ -> false)
		| Method MethDynamic -> true
		| Method _ -> false
	in
	let method_call = if is_static then
		(* Redirect String static methods to cs.StringExt *)
		let actual_path = match class_path with
			| ([], ("String" | "string")) | (["haxe"; "root"], ("String" | "string")) -> (["cs"], "StringExt")
			| _ -> cs_path_of_path class_path
		in
		let static_type = CsTypeClass (actual_path, List.map (cs_type_of_type gctx) type_params) in
		if is_stored_function then begin
			(* Static function field - use Runtime.InvokeDelegate *)
			let func_expr = CsStaticField (static_type, method_name) in
			let args_array = if call_args = [] then
				CsNew (CsTypeClass (NativeTypes.haxe_array_path, [CsTypeObject]), [])
			else
				let native_array = CsNewArray (CsTypeObject, call_args) in
				CsStaticCall (CsTypeClass (NativeTypes.haxe_array_path, [CsTypeObject]), "ofNative", [native_array])
			in
			CsStaticCall (CsTypeClass ((["haxe"; "lang"], "Runtime"), []), "InvokeDelegate", [func_expr; args_array])
		end else
			CsStaticCall (static_type, method_name, call_args)
	else if is_stored_function then begin
		(* Instance function field - use Runtime.InvokeDelegate *)
		let obj = CsField (CsThis, "_hx_this") in
		let func_expr = CsField (obj, method_name) in
		let args_array = if call_args = [] then
			CsNew (CsTypeClass (NativeTypes.haxe_array_path, [CsTypeObject]), [])
		else
			let native_array = CsNewArray (CsTypeObject, call_args) in
			CsStaticCall (CsTypeClass (NativeTypes.haxe_array_path, [CsTypeObject]), "ofNative", [native_array])
		in
		CsStaticCall (CsTypeClass ((["haxe"; "lang"], "Runtime"), []), "InvokeDelegate", [func_expr; args_array])
	end else
		CsCall (CsField (CsField (CsThis, "_hx_this"), method_name), call_args)
	in
	let invoke_body = if return_cs_type = CsTypeVoid then
		[CsExprStmt method_call]
	else
		[CsReturn (Some method_call)]
	in

	(* The typed invoke method may shadow the base class's invokeN(object...) method.
	   For invoke() with 0 params, it matches exactly - add 'new' modifier.
	   For invokeN with all object params, it also matches - add 'new' modifier.
	   Otherwise, the parameter types differ so no 'new' needed.

	   IMPORTANT: When shadowing with 'new', the base class method returns 'object',
	   so if our return_type is void, we must:
	   1. Change return type to object to match the base signature
	   2. Append "return null;" to the body since C# requires explicit return *)
	let num_params = List.length invoke_params in
	let all_params_are_object = List.for_all (fun p ->
		match p.p_type with
		| Some CsTypeObject | Some CsTypeDynamic -> true
		| None -> true  (* untyped = object *)
		| _ -> false
	) invoke_params in
	let shadows_base = num_params = 0 || all_params_are_object in
	let invoke_modifiers = if shadows_base then [MemberModifier.New] else [] in
	(* When shadowing with 'new' modifier and return_type is void, change to object
	   since the base class invoke() returns object. *)
	let invoke_return_type = if shadows_base && return_cs_type = CsTypeVoid then CsTypeObject else return_cs_type in
	(* If the final return type is object (either because we changed from void, or it was already object),
	   AND the body might contain "return;" (CsReturn None), transform those to "return null;".
	   We only do this for object return type because CsReturn None is valid ONLY for void methods.
	   Only append return null if the body doesn't already end with a return. *)
	let invoke_body =
		if invoke_return_type = CsTypeObject then
			let transformed = List.map transform_void_returns_to_null invoke_body in
			(* Only append return null if body doesn't already end with a return *)
			if stmts_end_with_return transformed then transformed
			else transformed @ [CsReturn (Some CsNull)]
		else
			invoke_body
	in
	let invoke_method = CsMemberMethod {
		m_name = invoke_method_name num_params;
		m_return_type = invoke_return_type;
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

	(* Build __hx_invokeN method - FunctionValue-based invoke to avoid boxing.
	   Returns FunctionValue to avoid boxing on return values too. *)
	let functionvalue_type = CsTypeClass ((["haxe"; "lang"], "FunctionValue"), []) in
	let functionvalue_invoke_method =
		if num_params = 0 then
			let invoke_result = CsCall (CsLocal "invoke", []) in
			let body = if return_cs_type = CsTypeVoid then
				[CsExprStmt invoke_result; CsReturn (Some (CsStaticCall (functionvalue_type, "Missing", [])))]
			else
				let wrapped_result = match return_cs_type with
					| CsTypeInt -> CsStaticCall (functionvalue_type, "FromInt", [invoke_result])
					| CsTypeDouble -> CsStaticCall (functionvalue_type, "FromDouble", [invoke_result])
					| CsTypeFloat -> CsStaticCall (functionvalue_type, "FromFloat", [invoke_result])
					| CsTypeBool -> CsStaticCall (functionvalue_type, "FromBool", [invoke_result])
					| CsTypeLong -> CsStaticCall (functionvalue_type, "FromLong", [invoke_result])
					| _ -> CsStaticCall (functionvalue_type, "FromObject", [invoke_result])
				in
				[CsReturn (Some wrapped_result)]
			in
			CsMemberMethod {
				m_name = "__hx_invoke0";
				m_return_type = functionvalue_type;
				m_access = AccessModifier.Public;
				m_modifiers = [Override];
				m_type_params = [];
				m_params = [];
				m_body = Some body;
				m_constraints = [];
				m_explicit_interface = None;
				m_attributes = [];
			}
		else
			let functionvalue_params = List.mapi (fun i _ ->
				{ p_name = "a" ^ string_of_int (i + 1); p_type = Some functionvalue_type; p_default = None; p_modifier = None }
			) invoke_params in
			let extract_args = List.mapi (fun i (param, _is_optional) ->
				let a_var = CsLocal ("a" ^ string_of_int (i + 1)) in
				match param.p_type with
				| Some CsTypeInt ->
					CsCall (CsField (a_var, "ToInt"), [])
				| Some CsTypeDouble ->
					CsCall (CsField (a_var, "ToDouble"), [])
				| Some CsTypeFloat ->
					CsCall (CsField (a_var, "ToFloat"), [])
				| Some CsTypeBool ->
					CsCall (CsField (a_var, "ToBool"), [])
				| Some CsTypeLong ->
					CsCall (CsField (a_var, "ToLong"), [])
				| Some CsTypeString ->
					CsCall (CsField (a_var, "ToStringValue"), [])
				| Some (CsTypeClass ((["haxe"; "lang"], "Null"), [inner])) ->
					begin match inner with
						| CsTypeInt -> CsCall (CsField (a_var, "ToNullInt"), [])
						| CsTypeDouble -> CsCall (CsField (a_var, "ToNullDouble"), [])
						| CsTypeFloat -> CsCall (CsField (a_var, "ToNullFloat"), [])
						| CsTypeBool -> CsCall (CsField (a_var, "ToNullBool"), [])
						| CsTypeLong -> CsCall (CsField (a_var, "ToNullLong"), [])
						| _ ->
							let null_type = CsTypeClass ((["haxe"; "lang"], "Null"), [inner]) in
							CsStaticCall (null_type, "_ofDynamic", [CsCall (CsField (a_var, "ToDynamic"), [])])
					end
				| Some t ->
					CsCast (t, CsField (a_var, "obj"))
				| None ->
					CsCall (CsField (a_var, "ToDynamic"), [])
			) invoke_params_with_opt in
			let invoke_call = CsCall (CsLocal (invoke_method_name num_params), extract_args) in
			let body = if return_cs_type = CsTypeVoid then
				[CsExprStmt invoke_call; CsReturn (Some (CsStaticCall (functionvalue_type, "Missing", [])))]
			else
				let wrapped_result = match return_cs_type with
					| CsTypeInt -> CsStaticCall (functionvalue_type, "FromInt", [invoke_call])
					| CsTypeDouble -> CsStaticCall (functionvalue_type, "FromDouble", [invoke_call])
					| CsTypeFloat -> CsStaticCall (functionvalue_type, "FromFloat", [invoke_call])
					| CsTypeBool -> CsStaticCall (functionvalue_type, "FromBool", [invoke_call])
					| CsTypeLong -> CsStaticCall (functionvalue_type, "FromLong", [invoke_call])
					| _ -> CsStaticCall (functionvalue_type, "FromObject", [invoke_call])
				in
				[CsReturn (Some wrapped_result)]
			in
			CsMemberMethod {
				m_name = "__hx_invoke" ^ string_of_int num_params;
				m_return_type = functionvalue_type;
				m_access = AccessModifier.Public;
				m_modifiers = [Override];
				m_type_params = [];
				m_params = functionvalue_params;
				m_body = Some body;
				m_constraints = [];
				m_explicit_interface = None;
				m_attributes = [];
			}
	in

	(* Build class definition *)
	let members = capture_fields @ [ctor; invoke_method; invoke_dynamic; functionvalue_invoke_method] in
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
	let capture_args = match obj_expr, erased_capture_type with
		| Some expr, Some erased_type when not is_static ->
			(* Cast the capture expression to the erased type to handle generic covariance.
			   e.g., TestHandler<T> needs to be cast to TestHandler<object> for the closure.
			   C# doesn't allow direct cast between generic types, so cast through object first. *)
			[CsCast (erased_type, CsCast (CsTypeObject, expr))]
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
   method_name: optional name of the enclosing method (for closure naming)
   type_params_in_scope: type parameter names from class and method that are valid in this context
   type_param_constraints: constraints for type parameters (name -> C# constraint types) *)
let generate_method_body gctx ?(param_cs_names=[]) ?(type_params_in_scope=[]) ?(type_param_constraints=[]) ?return_type ?class_path ?method_name e =
	let ectx = create_expr_context gctx in
	ectx.return_type <- return_type;
	ectx.current_class_path <- class_path;
	ectx.current_method_name <- method_name;
	ectx.origin_class_path <- class_path;  (* Set origin for closure grouping *)
	ectx.type_params_in_scope <- type_params_in_scope;
	ectx.type_param_constraints <- type_param_constraints;
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
				let init_cs = cs_expr_of_texpr ectx e in
				(* Coerce initializer to field type - needed for lambda returns, object->typed conversions *)
				let init_cs = coerce_arg gctx init_cs e.etype cf.cf_type in
				Some init_cs
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
				let init_cs = cs_expr_of_texpr ectx e in
				(* Coerce initializer to field type - needed for lambda returns, object->typed conversions *)
				let init_cs = coerce_arg gctx init_cs e.etype cf.cf_type in
				Some init_cs
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
		(* All type params in scope = class params + method params *)
		let all_type_params_in_scope = class_type_params @ method_type_params in
		(* Extract constraints from class and method type params *)
		let class_constraints = extract_type_param_constraints gctx c.cl_params in
		let method_constraints = extract_type_param_constraints gctx cf.cf_params in
		let all_type_param_constraints = class_constraints @ method_constraints in
		let body = match cf.cf_expr with
			| Some e ->
				let body_stmts = generate_method_body gctx ~param_cs_names ~type_params_in_scope:all_type_params_in_scope ~type_param_constraints:all_type_param_constraints ~return_type:ret ~class_path:c.cl_path ~method_name:cf.cf_name e in
				(* Wrap in unchecked if the expression contains non-zero integer constants *)
				if needs_unchecked e then
					Some [CsUncheckedStmt (CsBlock body_stmts)]
				else
					Some body_stmts
			| None -> None
		in
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
			m_constraints = method_constraints;
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

(* Search through block for super() call - it may not be the first statement.
   Returns (Some super_args, Some body_with_all_stmts) where body includes
   statements before super (which must be executed after base() in C#). *)
and extract_super_from_block el =
	(* Helper to find super call anywhere in the list *)
	let rec find_super before = function
		| [] -> None  (* No super found *)
		| e :: rest ->
			begin match e.eexpr with
			| TCall ({ eexpr = TConst TSuper }, args) ->
				(* Found super - before statements + rest statements become the body *)
				let body_stmts = List.rev before @ rest in
				let body_expr = if body_stmts = [] then None else Some { e with eexpr = TBlock body_stmts } in
				Some (args, body_expr)
			| TBlock inner ->
				(* Check inside nested block *)
				begin match extract_super_from_block inner with
				| (Some args, inner_body) ->
					(* Super found in nested block - combine with outer statements *)
					let body_stmts = List.rev before @ (match inner_body with Some b -> [b] | None -> []) @ rest in
					let body_expr = if body_stmts = [] then None else Some { e with eexpr = TBlock body_stmts } in
					Some (args, body_expr)
				| (None, _) ->
					(* No super in nested block - continue searching *)
					find_super (e :: before) rest
				end
			| _ ->
				(* Not super - continue searching *)
				find_super (e :: before) rest
			end
	in
	match find_super [] el with
	| Some (args, body) -> (Some args, body)
	| None -> (None, if el = [] then None else Some { (List.hd el) with eexpr = TBlock el })

(* Check if a parent class constructor needs two-phase construction *)
let parent_needs_two_phase_construction c =
	match c.cl_super with
	| Some (sc, _) ->
		begin match sc.cl_constructor with
		| Some ctor_cf -> needs_two_phase_construction ctor_cf
		| None -> false
		end
	| None -> false

(* EmptyConstructor type for two-phase construction marker *)
let empty_constructor_type = CsTypeClass ((["haxe"; "lang"], "EmptyConstructor"), [])

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

	(* Check if this constructor needs two-phase construction (this-before-super pattern) *)
	let is_two_phase = needs_two_phase_construction cf in
	let parent_is_two_phase = parent_needs_two_phase_construction c in

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
	(* Get class type parameters for constructor scope *)
	let class_type_params = List.map (fun ttp -> ttp.ttp_name) c.cl_params in
	let class_constraints = extract_type_param_constraints gctx c.cl_params in

	if is_two_phase then begin
		(* TWO-PHASE CONSTRUCTION: this constructor uses 'this' before calling super()
		   Generate:
		   1. Empty constructor: ClassName(EmptyConstructor _) : base((EmptyConstructor)null) { }
		   2. _hx_new() method with actual constructor logic (can use 'this' freely) *)

		(* 1. Empty constructor - just calls base's empty constructor *)
		let empty_ctor_param = {
			p_name = "_";
			p_type = Some empty_constructor_type;
			p_default = None;
			p_modifier = None;
		} in
		(* Base call for empty ctor: pass null cast to EmptyConstructor *)
		let empty_base_call =
			if c.cl_super <> None then
				Some [CsCast (empty_constructor_type, CsNull)]
			else
				None
		in
		let empty_ctor = CsMemberConstructor {
			ctor_access = AccessModifier.Public;
			ctor_modifiers = [];
			ctor_params = [empty_ctor_param];
			ctor_base_call = empty_base_call;
			ctor_this_call = None;
			ctor_body = [];
		} in

		(* 2. _hx_new method - contains actual constructor logic
		   Super call becomes base._hx_new(args) call in the body *)
		let hx_new_body =
			(* Generate base._hx_new(args) call if there was a super call *)
			let base_new_call = match super_args with
				| Some args when parent_is_two_phase ->
					let ectx = create_expr_context gctx in
					ectx.current_class_path <- Some c.cl_path;
					ectx.current_method_name <- Some "_hx_new";
					let cs_args = List.mapi (fun i arg ->
						let cs_arg = cs_expr_of_texpr ectx arg in
						if i < List.length base_ctor_types then
							let expected_type = List.nth base_ctor_types i in
							CsCast (expected_type, CsParens cs_arg)
						else
							cs_arg
					) args in
					[CsExprStmt (CsCall (CsField (CsBase, "_hx_new"), cs_args))]
				| Some args ->
					(* Parent doesn't need two-phase, but we do - need to call parent's regular constructor
					   This is tricky - in this case, the super() call args can use 'this'.
					   Since C# doesn't allow this, we need to call parent's empty ctor in the empty ctor above,
					   and here call parent's _hx_new if it exists, otherwise this is an error case.
					   For now, just call base._hx_new and hope parent has it. If parent is native, this will fail. *)
					let ectx = create_expr_context gctx in
					ectx.current_class_path <- Some c.cl_path;
					ectx.current_method_name <- Some "_hx_new";
					let cs_args = List.mapi (fun i arg ->
						let cs_arg = cs_expr_of_texpr ectx arg in
						if i < List.length base_ctor_types then
							let expected_type = List.nth base_ctor_types i in
							CsCast (expected_type, CsParens cs_arg)
						else
							cs_arg
					) args in
					[CsExprStmt (CsCall (CsField (CsBase, "_hx_new"), cs_args))]
				| None -> []
			in
			(* Rest of constructor body *)
			let body_stmts = match body_expr with
				| Some e -> generate_method_body gctx ~type_params_in_scope:class_type_params ~type_param_constraints:class_constraints ~class_path:c.cl_path ~method_name:"_hx_new" e
				| None -> []
			in
			field_init_stmts @ base_new_call @ body_stmts
		in
		(* Wrap in unchecked if the constructor expression contains non-zero integer constants *)
		let hx_new_body = match cf.cf_expr with
			| Some e when needs_unchecked e -> [CsUncheckedStmt (CsBlock hx_new_body)]
			| _ -> hx_new_body
		in
		(* Always use virtual for _hx_new - different constructor signatures result in method overloading,
		   not overriding. C# supports method overloading just like Java. *)
		let hx_new_modifiers = [MemberModifier.Virtual] in
		let hx_new_method = CsMemberMethod {
			m_name = "_hx_new";
			m_access = AccessModifier.Public;
			m_modifiers = hx_new_modifiers;
			m_type_params = [];
			m_params = ctor_params;
			m_return_type = CsTypeVoid;
			m_body = Some hx_new_body;
			m_constraints = [];
			m_explicit_interface = None;
			m_attributes = [];
		} in

		[empty_ctor; hx_new_method]
	end else begin
		(* NORMAL CONSTRUCTION: no this-before-super issue *)

		(* Convert super args to base call, casting to expected types *)
		let base_call = match super_args with
			| Some args when not parent_is_two_phase ->
				(* Normal case: parent has regular constructor *)
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
			| Some _ when parent_is_two_phase ->
				(* Parent needs two-phase but we don't use this before super.
				   We need to call parent's empty constructor and then call _hx_new in body. *)
				Some [CsCast (empty_constructor_type, CsNull)]
			| _ -> None
		in

		(* If parent needs two-phase, add base._hx_new call to body *)
		let extra_body_stmts = match super_args with
			| Some args when parent_is_two_phase ->
				let ectx = create_expr_context gctx in
				ectx.current_class_path <- Some c.cl_path;
				ectx.current_method_name <- Some "new";
				let cs_args = List.mapi (fun i arg ->
					let cs_arg = cs_expr_of_texpr ectx arg in
					if i < List.length base_ctor_types then
						let expected_type = List.nth base_ctor_types i in
						CsCast (expected_type, CsParens cs_arg)
					else
						cs_arg
				) args in
				[CsExprStmt (CsCall (CsField (CsBase, "_hx_new"), cs_args))]
			| _ -> []
		in

		(* Generate constructor body - prepend field initializations that contain 'this' *)
		let ctor_body = match body_expr with
			| Some e -> field_init_stmts @ extra_body_stmts @ generate_method_body gctx ~type_params_in_scope:class_type_params ~type_param_constraints:class_constraints ~class_path:c.cl_path ~method_name:"new" e
			| None -> field_init_stmts @ extra_body_stmts
		in
		(* Wrap in unchecked if the constructor expression contains non-zero integer constants *)
		let ctor_body = match cf.cf_expr with
			| Some e when needs_unchecked e -> [CsUncheckedStmt (CsBlock ctor_body)]
			| _ -> ctor_body
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
	end

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
		   pass arguments (e.g., Exception(message, previous, native)).
		   For extern classes with @:overload, we need to generate forwarding constructors
		   for ALL overloads, not just the main one. *)
		let get_ctor_args_from_cf cf =
			match follow cf.cf_type with
			| TFun (args, _) -> [args]
			| _ -> []
		in
		(* Check if parent needs two-phase construction *)
		let parent_is_two_phase = parent_needs_two_phase_construction c in
		let parent_ctor_signatures = match c.cl_super with
			| Some (sc, _) ->
				begin match sc.cl_constructor with
				| Some ctor_cf ->
					(* Get the main constructor signature *)
					let main_args = get_ctor_args_from_cf ctor_cf in
					(* Also get overload signatures *)
					let overload_args = List.flatten (List.map get_ctor_args_from_cf ctor_cf.cf_overloads) in
					main_args @ overload_args
				| None -> []
				end
			| None -> []
		in
		(* Generate a forwarding constructor for each parent constructor signature *)
		let generated_ctors = List.map (fun parent_ctor_args ->
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
			if parent_is_two_phase then begin
				(* Parent needs two-phase: call empty ctor, then base._hx_new in body *)
				let base_hx_new_args = List.map (fun (n, _, t) ->
					let param_type = cs_type_of_type gctx t in
					CsCast (param_type, CsParens (CsLocal (escape_identifier n)))
				) parent_ctor_args in
				let base_hx_new_call = CsExprStmt (CsCall (CsField (CsBase, "_hx_new"), base_hx_new_args)) in
				CsMemberConstructor {
					ctor_access = AccessModifier.Public;
					ctor_modifiers = [];
					ctor_params = ctor_params;
					ctor_base_call = Some [CsCast (empty_constructor_type, CsNull)];
					ctor_this_call = None;
					ctor_body = base_hx_new_call :: field_init_stmts;
				}
			end else begin
				(* Normal case: just forward to parent's constructor *)
				let base_args = List.map (fun (n, _, _) -> CsLocal (escape_identifier n)) parent_ctor_args in
				CsMemberConstructor {
					ctor_access = AccessModifier.Public;
					ctor_modifiers = [];
					ctor_params = ctor_params;
					ctor_base_call = Some base_args;
					ctor_this_call = None;
					ctor_body = field_init_stmts;
				}
			end
		) parent_ctor_signatures in
		if generated_ctors <> [] then
			members := generated_ctors @ !members
		(* Also generate a default parameterless constructor if there are field initializations *)
		else if field_init_stmts <> [] then begin
			members := [CsMemberConstructor {
				ctor_access = AccessModifier.Public;
				ctor_modifiers = [];
				ctor_params = [];
				ctor_base_call = None;
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

	(* Extract type parameter constraints.
	   Haxe constraints like T:SomeClass become C# where T : SomeClass.
	   C# constraints can only be: interfaces, non-sealed classes, or type parameters.
	   We filter out invalid constraints like value types, sealed classes, etc. *)
	let type_constraints = List.filter_map (fun ttp ->
		let constraints = TFunctions.get_constraints ttp in
		if constraints = [] then None
		else begin
			(* Convert Haxe constraint types to C# types, filtering out unusable ones *)
			let cs_constraints = List.filter_map (fun t ->
				match follow t with
				| TInst ({ cl_kind = KTypeParameter _ }, _) ->
					(* Another type parameter as constraint - skip for now (C# handles differently) *)
					None
				| TAnon _ ->
					(* Anonymous structural constraint - can't express in C# generics *)
					None
				| TDynamic _ ->
					(* Dynamic has no meaning as constraint *)
					None
				| TInst (c, params) ->
					(* Check if class is sealed - sealed classes can't be constraints in C# *)
					if has_class_flag c CFinal then None
					else begin
						let cs_t = cs_type_of_type gctx t in
						(* Also filter out special types that C# doesn't allow as constraints *)
						match cs_t with
						| CsTypeObject -> None  (* object can't be a constraint *)
						| CsTypeString -> None  (* string is sealed *)
						| _ -> Some cs_t
					end
				| TAbstract ({ a_path = ([], ("Int" | "Float" | "Single" | "Bool")) }, _) ->
					(* Value types can't be constraints *)
					None
				| _ ->
					let cs_t = cs_type_of_type gctx t in
					(* Filter out value types and special types *)
					match cs_t with
					| CsTypeObject | CsTypeString -> None
					| CsTypeInt | CsTypeUInt | CsTypeLong | CsTypeULong -> None
					| CsTypeByte | CsTypeSByte | CsTypeShort | CsTypeUShort -> None
					| CsTypeFloat | CsTypeDouble | CsTypeDecimal -> None
					| CsTypeBool | CsTypeChar -> None
					| _ -> Some cs_t
			) constraints in
			if cs_constraints = [] then None
			else Some (ttp.ttp_name, cs_constraints)
		end
	) c.cl_params in

	CsClassDef {
		c_path = path;
		c_access = AccessModifier.Public;
		c_modifiers = modifiers;
		c_type_params = type_params;
		c_base = base_class;
		c_interfaces = interfaces;
		c_constraints = type_constraints;
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

(* Wrapper for cs_type_of_type that doesn't need gctx - used for preprocessor *)
let cs_type_of_type_for_preprocessor t =
	(* The gctx parameter in cs_type_of_type is not actually used, so we can pass unit *)
	cs_type_of_type (Obj.magic ()) t

(* Main generation entry point *)
let generate com =
	let gctx = create_context com in

	(* Initialize the preprocessor for this-before-super detection *)
	gctx.preprocessor <- new preprocessor com.basic cs_type_of_type_for_preprocessor;

	(* Preprocess all types following JVM pattern:
	   - Classes: full preprocessing (this-before-super detection + optional param patching)
	   - Interfaces: only patch optional params to wrap in Null<T> *)
	List.iter (fun mt ->
		match mt with
		| TClassDecl c when not (has_class_flag c CInterface) ->
			(* Full preprocessing for classes *)
			gctx.preprocessor#preprocess_class c
		| TClassDecl c ->
			(* Interfaces: patch optional params only (same as JVM line 3130) *)
			List.iter (fun cf -> patch_optional com.basic cf) c.cl_ordered_fields;
			List.iter (fun cf -> patch_optional com.basic cf) c.cl_ordered_statics
		| _ -> ()
	) com.types;

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
	copy_runtime_file "cs/_cs/haxe/lang/FunctionValue.cs" "haxe/lang/FunctionValue.cs";
	copy_runtime_file "cs/_cs/haxe/lang/EmptyConstructor.cs" "haxe/lang/EmptyConstructor.cs";
	copy_runtime_file "cs/_cs/AssemblyAttributes.cs" "AssemblyAttributes.cs";

