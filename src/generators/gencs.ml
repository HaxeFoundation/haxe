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

(*
	C# CODE GENERATOR - MODULE STRUCTURE
	====================================

	The C# code generator is split across multiple modules for maintainability.
	This file [gencs.ml] contains the core expression/statement translation.

	MODULE OVERVIEW:
	----------------

	FOUNDATION:
	- csGlobals.ml      - Operators, modifiers, C# keywords, naming helpers
	- csAst.ml          - C# AST type definitions [cs_expr, cs_stmt, cs_member, ...]
	- csContext.ml      - Context types [gen_context, expr_context] shared across modules

	TYPE SYSTEM:
	- csTypeMapping.ml  - Haxe->C# type conversion, array helpers, NativeTypes module
	- csTypeCoercion.ml - Type coercion [coerce_cs_types, coerce_arg], type predicates

	ANALYSIS:
	- csExprAnalysis.ml - Expression analysis, control flow, side effects
	- csNullable.ml     - Null<T> pre-transform pass [AST rewriting before code gen]

	CODE GENERATION:
	- csPrinter.ml      - C# AST -> source code string output

	CORE [this file]:
	- gencs.ml          - Expression/statement translation, closure/constructor/field
	                      generation, type generation, main entry point

	FINDING THINGS:
	---------------

	C# AST types                      -> csAst.ml
	Context types [gen/expr_context]  -> csContext.ml
	Haxe type -> C# type conversion   -> csTypeMapping.ml [cs_type_of_type]
	Type coercion/casting             -> csTypeCoercion.ml [coerce_cs_types]
	Type predicates                   -> csTypeCoercion.ml [is_cs_null_wrapper, ...]
	Null<T> wrapper handling          -> csNullable.ml, csTypeCoercion.ml
	Expression side-effect analysis   -> csExprAnalysis.ml
	Control flow termination          -> csExprAnalysis.ml [stmt_terminates]
	Function invoke methods           -> gencs.ml [invoke system section]
	Closure/lambda generation         -> gencs.ml [generate_closure_class]
	Constructor generation            -> gencs.ml [generate_constructor]
	Field/method generation           -> gencs.ml [generate_field]
	Override/interface analysis       -> gencs.ml [is_override, find_variant_interface_...]
	AOT reflection/IMap               -> gencs.ml [generate_field_accessors, ...]
	Class/interface/enum generation   -> gencs.ml [generate_class, generate_interface, ...]
	C# source code output             -> csPrinter.ml [print_type_def]
	Haxe->C# expression translation   -> gencs.ml [cs_expr_of_texpr]
	Haxe->C# statement translation    -> gencs.ml [cs_stmt_of_texpr]
	Main entry point                  -> gencs.ml [generate]
	C# keywords and escaping          -> csGlobals.ml [escape_identifier]
	Binary/unary operators            -> csGlobals.ml [cs_binop_of_binop, ...]
 *)

(* C# code generator - main coordinator *)

open Globals
open Ast
open Type
open Gctx
open CsGlobals
open CsAst
open CsTypeMapping
open CsPrinter
open Genshared
open CsNullable
open CsExprAnalysis
open CsTypeCoercion
open CsContext

(* Common path tuples for pattern matching.
   Type constants (hxvalue_type, runtime_type, etc.) are defined in CsTypeMapping. *)
let null_path = NativeTypes.haxe_null_path
let runtime_path = NativeTypes.haxe_runtime_path
let haxeobject_path = NativeTypes.haxe_object_path

(* Context types gen_context, expr_context, cs_expr_result are defined in CsContext.ml *)
(* Also: create_context, create_expr_context, add_closure_for_class, get_closures_for_class, fresh_temp, generate_closure_name *)

(* ====== Invoke system ====== *)

(* Classify a type for invoke signature matching.
   Primitives stay as-is for typed dispatch, everything else becomes object.
   This approach mirrors the JVM generator's signature classification:
   - Primitives (int, double, bool, etc.) stay as-is
   - Null<T> stays as-is (important for optional params)
   - Everything else becomes object *)
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

(* Get the invoke method name for the given arity.
   invoke() for 0 args, invoke1 for 1 arg, invoke2 for 2 args, etc. *)
let invoke_method_name num_args =
	if num_args = 0 then "invoke"
	else "invoke" ^ string_of_int num_args

(* Get the Value-based invoke method name: __hx_invoke0, __hx_invoke1, etc.
   These methods return Value to avoid boxing on return values. *)
let hxvalue_invoke_method_name num_args =
	"__hx_invoke" ^ string_of_int num_args

(* Generate Value arguments for closure/function invocation.
   Returns a list of Value.FromXxx(...) calls for each argument.
   Each argument type maps to a specific factory method:
   - int: Value.FromInt(arg)
   - double: Value.FromDouble(arg)
   - float: Value.FromFloat(arg)
   - bool: Value.FromBool(arg)
   - long: Value.FromLong(arg)
   - Null<int>: Value.FromNullInt(arg)
   - Null<double>: Value.FromNullDouble(arg)
   - other: Value.FromObject(arg)
   Note: arg_types may be shorter than args (e.g., if type info is missing);
   we default to FromObject for any args without type info. *)
let generate_hxvalue_args args arg_types =
	let num_types = List.length arg_types in
	List.mapi (fun i arg ->
		let arg_type = if i < num_types then List.nth arg_types i else CsTypeObject in
		cast_type_to_value arg_type arg
	) args

(* Build an args array for InvokeDelegate calls.
   If args list is empty, creates an empty haxe.root.Array.
   Otherwise, wraps the args in a native object[] and converts to haxe.root.Array. *)
let make_invoke_args_array args =
	if args = [] then
		CsNew (haxe_array_type, [])
	else
		let native_array = CsNewArray (CsTypeObject, args) in
		make_array_from_native ArrayDynamic native_array haxe_array_type

(* Cast InvokeDelegate result to expected type.
   Delegates to cast_object_to_type for the actual conversion;
   adds Void pass-through since InvokeDelegate always returns object. *)
let cast_invoke_result result_type call_expr =
	match result_type with
	| CsTypeVoid -> call_expr
	| _ -> cast_object_to_type result_type call_expr

(* ====== Helper utilities ====== *)

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

(* Register an invoke signature for later generation on Function class.
   Uses classify_for_invoke from CsInvokeSystem to normalize the signature. *)
let register_invoke_signature gctx arg_types ret_type =
	(* Classify types to normalize the signature *)
	let classified_args = List.map classify_for_invoke arg_types in
	let classified_ret = classify_for_invoke ret_type in
	let key = (classified_args, classified_ret) in
	if not (Hashtbl.mem gctx.invoke_signatures key) then
		Hashtbl.add gctx.invoke_signatures key ()

(* Check if an expression contains a reference to 'this'.
   Used to determine if field initializers need to be moved to the constructor
   since C# field initializers cannot use 'this'. *)
(* Check if a constructor needs two-phase construction (has Meta.HxGen from this-before-super) *)
let needs_two_phase_construction cf =
	Meta.has Meta.HxGen cf.cf_meta

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

(* ====== Null & type wrapper detection ====== *)

(* Convert Haxe constant to C# constant *)
let cs_const_of_tconst = function
	| TInt i -> CsConstInt i
	| TFloat s -> CsConstDouble (float_of_string s)
	| TString s -> CsConstString s
	| TBool b -> CsConstBool b
	| TNull -> CsConstNull
	| TThis -> failwith "TThis is not a constant"
	| TSuper -> failwith "TSuper is not a constant"

(* Helper to check if a type is Null<T> wrapper that maps to haxe.lang.Null<T> in C#.
   Returns false for Null<T> where T is inherently nullable in C# (classes, arrays, etc.)
   because those get stripped to just T at the type level in cs_type_of_type.
   Handles TMono, TType, TLazy. *)
let is_null_wrapper_type t =
	let rec check t depth =
		if depth > 10 then false else
		match t with
		| TAbstract ({ a_path = ([], "Null") }, [inner]) ->
			(* Check if the inner type maps to an inherently nullable C# type.
			   If so, the Null<> wrapper is stripped at the type level and we don't need .value *)
			let inner_cs = CsTypeMapping.cs_type_of_type_without_gctx inner in
			not (CsTypeMapping.is_inherently_nullable inner_cs)
		| TAbstract ({ a_path = ([], "Null") }, _) -> true  (* Null with no/multiple params - treat as nullable *)
		| TType (_, _) -> check (Type.follow_once t) (depth + 1)
		| TLazy f -> check (lazy_type f) (depth + 1)
		| TMono r -> (match r.tm_type with Some t -> check t (depth + 1) | None -> false)
		| _ -> false
	in check t 0

(* Helper to find if an expression involves a Null<T> wrapper - checks through TLocal, TCast, etc.
   Returns true if the GENERATED C# expression will have type Null<T> and needs .value unwrapping.
   CRITICAL: For TCast, we check the TARGET type (e.etype), not the inner expression type.
   This is because TCast changes the C# type - if we cast to non-Null, no .value needed.
   NOTE: CsNullable handles Null<Null<T>> flattening at the AST level. *)
let rec find_null_in_expr e =
	if is_null_wrapper_type e.etype then
		true
	else match e.eexpr with
		| TLocal v -> is_null_wrapper_type v.v_type
		| TCast (_, None) ->
			(* TCast result type (e.etype) is NOT Null, so even if inner was Null,
			   the cast handles the conversion. No .value needed. *)
			false
		| TParenthesis inner -> find_null_in_expr inner
		| TMeta (_, inner) -> find_null_in_expr inner
		| TCall (_, _) -> false  (* Rely on is_null_wrapper_type e.etype check above *)
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

(* Check if a binop expression with Null<T> operands produces a non-Null result in C#.
   In C#, Null<T> has implicit conversion to T, so arithmetic like Null<int> + 1 produces int, not Null<int>.
   This is important because Haxe type may still say Null<Int> but the C# expression is actually int.
   Returns true if the expression is an arithmetic binop that will produce non-Null in C#. *)
let is_binop_with_implicit_null_conversion e =
	match e.eexpr with
	| TBinop (op, e1, e2) ->
		let is_arithmetic = match op with
			| OpAdd | OpSub | OpMult | OpDiv | OpMod
			| OpShl | OpShr | OpUShr | OpAnd | OpOr | OpXor -> true
			| _ -> false
		in
		if is_arithmetic then
			(* Check if either operand is Null<T> (with numeric inner type).
			   Use follow_once to peel through TMono but not unwrap abstract Null<T>.
			   Also check TLocal variable types since those preserve Null wrapper. *)
			let rec is_null_numeric_type t = match t with
				| TAbstract ({ a_path = ([], "Null") }, [inner]) ->
					begin match follow inner with
					| TAbstract ({ a_path = ([], ("Int" | "Float" | "Single")) }, _) -> true
					| _ -> false
					end
				| TType (_, _) -> is_null_numeric_type (Type.follow_once t)
				| TLazy f -> is_null_numeric_type (lazy_type f)
				| TMono r -> (match r.tm_type with Some t -> is_null_numeric_type t | None -> false)
				| _ -> false
			in
			let is_null_numeric_expr e = match e.eexpr with
				| TLocal v -> is_null_numeric_type v.v_type
				| _ -> is_null_numeric_type e.etype
			in
			is_null_numeric_expr e1 || is_null_numeric_expr e2
		else
			false
	| _ -> false

(* Check if type is a type parameter or Null<TypeParam> that gets erased to object *)
let rec is_erased_type_param t =
	match t with
	| TInst ({ cl_kind = KTypeParameter _ }, _) -> true
	| TAbstract ({ a_path = ([], "Null") }, [inner]) ->
		begin match follow inner with
		| TInst ({ cl_kind = KTypeParameter _ }, _) -> true
		| _ -> false
		end
	| TType (_, _) | TLazy _ -> is_erased_type_param (follow t)
	| TMono { tm_type = Some inner } -> is_erased_type_param inner
	| TMono { tm_type = None } -> false  (* Unresolved mono - not a type param *)
	| _ -> false

(* Check if a method call's DECLARED return type involves a type parameter that gets erased.
   This covers two cases:
   1. Null<T> where T is a type parameter - erased to just `object` (not `Null<object>`)
   2. T where T is a type parameter - erased to `object`
   In both cases, the C# method returns `object`, not the instantiated type. *)
let method_call_returns_erased_type_param e =
	match e.eexpr with
	| TCall (callee, _) ->
		let check_method c cf =
			(* Method returns erased type param if:
			   1. It's NOT a C# native generic class (those keep type params), AND
			   2. The method has type parameters (cf.cf_params <> []) OR
			      the return type is a type parameter from the class *)
			let is_native = CsTypeMapping.is_cs_native_generic_class c.cl_path in
			if is_native then false
			else begin
				(* Check if method has its own type parameters - those get erased *)
				if cf.cf_params <> [] then true
				else begin
					(* Check if return type involves a class type parameter *)
					match cf.cf_type with
					| TFun (_, ret) -> is_erased_type_param ret
					| TLazy f -> begin match lazy_type f with
						| TFun (_, ret) -> is_erased_type_param ret
						| _ -> false
						end
					| _ -> false
				end
			end
		in
		begin match callee.eexpr with
		| TField (_, FInstance (c, _, cf)) ->
			(* Array<T> becomes non-generic haxe.root.Array in C#, so its methods
			   like __popInt() return concrete types, not erased type params. *)
			begin match c.cl_path with
			| ([], "Array") | (["haxe"; "root"], "Array") -> false
			| _ -> check_method c cf
			end
		| TField (_, FStatic (c, cf)) ->
			check_method c cf
		| TField (_, FClosure (Some (c, _), cf)) ->
			check_method c cf
		| TField (_, FClosure (None, cf)) | TField (_, FAnon cf) ->
			(* No class context - check if method has type params *)
			cf.cf_params <> [] || begin
				match cf.cf_type with
				| TFun (_, ret) -> is_erased_type_param ret
				| TLazy f -> begin match lazy_type f with
					| TFun (_, ret) -> is_erased_type_param ret
					| _ -> false
					end
				| _ -> false
			end
		| _ -> false
		end
	| _ -> false

(* Check if a field access returns an erased type parameter.
   When a class field is declared as type T (a type parameter), it becomes object in C#. *)
let field_access_returns_erased_type_param e =
	match e.eexpr with
	| TField (_, FInstance (_, _, cf)) | TField (_, FStatic (_, cf)) | TField (_, FClosure (_, cf)) | TField (_, FAnon cf) ->
		(* Field type - if it's a type parameter, it gets erased to object *)
		is_erased_type_param cf.cf_type
	| _ -> false

(* Check if an expression returns an erased type parameter (method call or field access) *)
let expr_returns_erased_type_param e =
	method_call_returns_erased_type_param e || field_access_returns_erased_type_param e

(* SINGLE SOURCE OF TRUTH: This is the ONLY place that decides .value and .hasValue behavior.

   This helper determines if a Haxe expression produces a Null<T> struct in C#.
   Use it for BOTH decisions:
   - .value: if true → add .value to unwrap; if false → expression is already unwrapped
   - .hasValue: if true → use .hasValue; if false → use == null

   Returns false when Haxe type is Null<T> but C# type is NOT Null<T>:
   - Type param erased to `object` (method returns, local vars)
   - Null<object> (special case - uses == null)
   - Enum field access (generates plain EnumType)
   - TConst values (generated as bare primitives, not wrapped in Null<T>)

   DO NOT add separate helpers for .value or .hasValue decisions elsewhere.
   DO NOT add this logic to csNullable.ml - it doesn't have C# type information.
   Keep this as the single authoritative source. *)
let rec expr_produces_csharp_null_type gctx e =
	(* First check expression structure for cases that never produce Null<T> in C# *)
	match e.eexpr with
	| TConst _ ->
		(* Constants are generated as bare primitives (e.g., false, 0, "str", null).
		   They don't produce Null<T> structs even if Haxe type is Null<T>. *)
		false
	| TBinop (op, _, _) ->
		(* Binary operations that produce primitives via C#'s implicit Null<T> → T conversion.
		   Even if Haxe type is Null<T>, C# generates bare primitives for these.
		   Note: OpNullCoal (??) and OpAssign* can produce Null<T>, so they fall through to default. *)
		begin match op with
		| Ast.OpBoolAnd | Ast.OpBoolOr
		| Ast.OpEq | Ast.OpNotEq | Ast.OpLt | Ast.OpLte | Ast.OpGt | Ast.OpGte
		| Ast.OpAdd | Ast.OpSub | Ast.OpMult | Ast.OpDiv | Ast.OpMod
		| Ast.OpAnd | Ast.OpOr | Ast.OpXor | Ast.OpShl | Ast.OpShr | Ast.OpUShr ->
			false
		| _ ->
			(* OpNullCoal, OpAssign, OpAssignOp, etc. - check the actual C# type *)
			begin match CsTypeMapping.cs_type_of_type gctx e.etype with
			| CsTypeClass ((["haxe"; "lang"], "Null"), [CsTypeObject]) -> false
			| CsTypeClass ((["haxe"; "lang"], "Null"), _) -> true
			| _ -> false
			end
		end
	| TUnop _ ->
		(* Unary operations (!, -, ~, ++, --) use implicit conversion and produce primitives.
		   E.g., !Null<bool> produces bool, not Null<bool>. *)
		false
	| TParenthesis e1 | TMeta (_, e1) ->
		(* Unwrap parentheses/meta and check inner *)
		expr_produces_csharp_null_type gctx e1
	| TLocal v ->
		(* For locals, use the variable's declared type *)
		begin match CsTypeMapping.cs_type_of_type gctx v.v_type with
		| CsTypeClass ((["haxe"; "lang"], "Null"), [CsTypeObject]) -> false  (* Null<object> uses == null *)
		| CsTypeClass ((["haxe"; "lang"], "Null"), _) -> true  (* All other Null<T> *)
		| _ -> false
		end
	| _ ->
		(* For other expressions, check the expression's type *)
		begin match CsTypeMapping.cs_type_of_type gctx e.etype with
		| CsTypeClass ((["haxe"; "lang"], "Null"), [CsTypeObject]) -> false  (* Null<object> uses == null *)
		| CsTypeClass ((["haxe"; "lang"], "Null"), _) -> true  (* All other Null<T> *)
		| _ -> false
		end

(* Get the effective type of an expression for coercion purposes.
   For expressions that have Haxe type Null<T> but generate non-Null C# code (like enum field access),
   return the inner type T instead of Null<T>. This prevents incorrectly adding .value unwrapping.
   For method calls with erased Null<TypeParam>, return Dynamic (maps to object in C#).
   Uses expr_produces_csharp_null_type (the SINGLE SOURCE OF TRUTH) to determine actual C# type. *)
let get_effective_expr_type gctx e =
	(* Check if expression produces Null<T> in C# using the unified helper.
	   If NOT, we may need to strip the Haxe Null wrapper or return object type. *)
	if not (expr_produces_csharp_null_type gctx e) then begin
		(* Expression doesn't produce Null<T> in C#, even if Haxe type is Null<T> *)
		if expr_returns_erased_type_param e then
			(* Erased type param returns object in C# *)
			mk_mono ()
		else if is_null_wrapper_type e.etype then
			(* Strip Null wrapper - C# doesn't have Null<> here *)
			let rec get_inner t depth =
				if depth > 10 then e.etype else
				match t with
				| TAbstract ({ a_path = ([], "Null") }, [inner]) -> inner
				| TInst ({ cl_path = (["haxe"; "lang"], "Null") }, [inner]) -> inner
				| TType (_, _) -> get_inner (Type.follow_once t) (depth + 1)
				| TLazy f -> get_inner (lazy_type f) (depth + 1)
				| TMono r -> (match r.tm_type with Some t -> get_inner t (depth + 1) | None -> e.etype)
				| _ -> e.etype
			in
			get_inner e.etype 0
		else
			e.etype
	end
	else
		(* Expression produces Null<T> in C# - use the Haxe type as-is *)
		e.etype

(* Helper to get the inner type from Null<T>, if the expression needs .value unwrapping.
   Returns Some(inner) if expression is Null-wrapped and needs unwrapping, None otherwise.
   CRITICAL: For TCast, check the TARGET type, not the inner type.
   NOTE: Only returns Some when the inner type is NOT inherently nullable in C#
   (since inherently nullable types have Null<> stripped at the type level). *)
let rec get_null_inner_if_needs_unwrap e =
	let get_inner t =
		let rec check t depth =
			if depth > 10 then None else
			match t with
			| TAbstract ({ a_path = ([], "Null") }, [inner]) ->
				(* Only return Some if inner is NOT inherently nullable in C# *)
				let inner_cs = CsTypeMapping.cs_type_of_type_without_gctx inner in
				if CsTypeMapping.is_inherently_nullable inner_cs then None
				else Some inner
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

(* Debug helper to print C# expression structure *)
let rec debug_cs_expr_structure cs_expr =
	match cs_expr with
	| CsConst _ -> "CsConst"
	| CsLocal name -> Printf.sprintf "CsLocal(%s)" name
	| CsThis -> "CsThis"
	| CsBase -> "CsBase"
	| CsNull -> "CsNull"
	| CsDefault _ -> "CsDefault"
	| CsTypeOf _ -> "CsTypeOf"
	| CsNameOf _ -> "CsNameOf"
	| CsSizeOf _ -> "CsSizeOf"
	| CsBinop _ -> "CsBinop"
	| CsUnop _ -> "CsUnop"
	| CsTernary _ -> "CsTernary"
	| CsField (e, f) -> Printf.sprintf "CsField(%s, %s)" (debug_cs_expr_structure e) f
	| CsStaticField (_, f) -> Printf.sprintf "CsStaticField(_, %s)" f
	| CsArrayAccess _ -> "CsArrayAccess"
	| CsCall (e, _) -> Printf.sprintf "CsCall(%s, _)" (debug_cs_expr_structure e)
	| CsCallGeneric _ -> "CsCallGeneric"
	| CsStaticCall (t, m, _) ->
		let type_str = match t with
			| CsTypeClass ((ns, name), _) -> Printf.sprintf "CsTypeClass((%s, %s))" (String.concat "." ns) name
			| _ -> "other_type"
		in
		Printf.sprintf "CsStaticCall(%s, %s, _)" type_str m
	| CsStaticCallGeneric _ -> "CsStaticCallGeneric"
	| CsNew _ -> "CsNew"
	| CsNewArray _ -> "CsNewArray"
	| CsNewArraySize _ -> "CsNewArraySize"
	| CsCast (_, e) -> Printf.sprintf "CsCast(_, %s)" (debug_cs_expr_structure e)
	| CsAs _ -> "CsAs"
	| CsIs _ -> "CsIs"
	| CsIsPattern _ -> "CsIsPattern"
	| CsParens e -> Printf.sprintf "CsParens(%s)" (debug_cs_expr_structure e)
	| CsLambda _ -> "CsLambda"
	| CsUnchecked _ -> "CsUnchecked"
	| CsNullConditionalField _ -> "CsNullConditionalField"
	| CsNullConditionalCall _ -> "CsNullConditionalCall"
	| CsNullConditionalIndex _ -> "CsNullConditionalIndex"
	| CsAwait _ -> "CsAwait"
	| CsThrow _ -> "CsThrow"
	| CsInterpolatedString _ -> "CsInterpolatedString"
	| CsRaw _ -> "CsRaw"
	| CsInlineCode _ -> "CsInlineCode"

(* Check if a Haxe expression is null or a default value (for block expression optimization).
   This is used to detect patterns like { var x = null; call(args, x); } which can be
   optimized by inlining the null/default directly into the call. *)
let is_null_or_default_expr e =
	match e.eexpr with
	| TConst TNull -> true
	| TConst (TInt 0l) -> true
	| TConst (TFloat "0") -> true
	| TConst (TFloat "0.0") -> true
	| TConst (TBool false) -> true
	| _ -> false

(* Lookup table for dynamic binary operators that use cs.Cs.opXxx helpers.
   Maps Haxe operator to the C# helper method name. *)
let dynamic_binop_helpers = [
	(OpAdd, "opAdd"); (OpSub, "opSub"); (OpMult, "opMul"); (OpDiv, "opDiv");
	(OpMod, "opMod"); (OpAnd, "opAnd"); (OpOr, "opOr"); (OpXor, "opXor");
	(OpShl, "opShl"); (OpShr, "opShr"); (OpUShr, "opUshr")
]

(* Check if a type is Rest<T> and return the inner element type if so *)
let get_rest_element_type t =
	match Type.follow t with
	| TAbstract ({ a_path = (["haxe"], "Rest") }, [elem_type]) -> Some elem_type
	| _ -> None

(* ====== Call argument generation ====== *)

(* Generate a single argument with coercion based on expected type *)
let generate_single_arg ectx cs_expr_of_texpr arg expected_type =
	let expected_cs_type = cs_type_of_type ectx.gctx expected_type in
	(* Special case: null argument - generate the right default value directly based on expected type *)
	let is_null_arg = match arg.eexpr with TConst TNull -> true | _ -> false in
	if is_null_arg then begin
		(* For null arguments, generate appropriate default value based on expected type *)
		match expected_cs_type with
		| CsTypeClass ((["haxe"; "lang"], "Null"), _) ->
			(* Null<T> expected - generate default(Null<T>) using the expected type.
			   The expected type already has concrete type params from the method signature,
			   so we should NOT erase them. Only erase out-of-scope type params that would
			   cause CS0246 errors. *)
			let erased_expected = CsTypeMapping.erase_out_of_scope_type_params ectx.type_params_in_scope expected_cs_type in
			CsDefault erased_expected
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
		(* GADT argument coercion: For TLocal variables, check if the ORIGINAL declared type
		   (v.v_type) differs from the expected type. This handles GADT pattern matching where
		   the Haxe typer refines arg.etype but the C# variable declaration uses v.v_type.
		   Example: In evalBinop<T,C>(op:Binop<C,T>, e1:Expr<C>, e2:Expr<C>):T
		   When matching OpAdd (Binop<Float,Float>), e1.etype becomes Expr<Float> (refined),
		   but C# variable e1 is declared as Expr<C>. We need to cast Expr<C> -> Expr<double>. *)
		match arg.eexpr with
		| TLocal v ->
			let var_cs_type = cs_type_of_type ectx.gctx v.v_type in
			(* Check if original var type differs from expected and requires cast *)
			begin match var_cs_type, expected_cs_type with
			| CsTypeClass (path1, params1), CsTypeClass (path2, params2)
				when path1 = path2 && params1 <> params2 ->
				(* Check if all type params in expected are in scope.
				   If so, we can safely cast because the types are valid at this point. *)
				let rec all_type_params_in_scope scope cs_type = match cs_type with
					| CsTypeGenericParam name -> List.mem name scope
					| CsTypeClass (_, inner) | CsTypeNestedGeneric (_, _, inner) ->
						List.for_all (all_type_params_in_scope scope) inner
					| CsTypeNested (parent, _) -> all_type_params_in_scope scope parent
					| CsTypeArray (elem, _) -> all_type_params_in_scope scope elem
					| _ -> true
				in
				let expected_all_in_scope = List.for_all (all_type_params_in_scope ectx.type_params_in_scope) params2 in
				(* Cast if expected type params are all in scope - handles both:
				   1. GADT refinement: Expr<C> -> Expr<double> (expected has no type params)
				   2. Phantom types: Stack<S> -> Stack<TCons<Y, S>> (expected has in-scope type params) *)
				if expected_all_in_scope then
					CsCast (expected_cs_type, cs_arg)
				else
					(* Use effective type to handle non-null-generating expressions like enum field access *)
					coerce_arg ~in_scope:ectx.type_params_in_scope ectx.gctx cs_arg (get_effective_expr_type ectx.gctx arg) expected_type
			| _ ->
				(* Use effective type to handle non-null-generating expressions like enum field access *)
				coerce_arg ~in_scope:ectx.type_params_in_scope ectx.gctx cs_arg (get_effective_expr_type ectx.gctx arg) expected_type
			end
		| _ ->
			(* Use effective type to handle non-null-generating expressions like enum field access *)
			coerce_arg ~in_scope:ectx.type_params_in_scope ectx.gctx cs_arg (get_effective_expr_type ectx.gctx arg) expected_type
	end

(* Generate call arguments with type coercion based on expected parameter types.
   Handles Rest<T> parameters by wrapping remaining args into Array<T>. *)
let generate_call_args ectx cs_expr_of_texpr args param_types =
	let num_params = List.length param_types in
	(* Check if last param is Rest<T> *)
	let last_param_rest = if num_params > 0 then
		get_rest_element_type (List.nth param_types (num_params - 1))
	else
		None
	in
	match last_param_rest with
	| Some rest_elem_type ->
		(* Last param is Rest<T> - split args into regular and rest parts *)
		let regular_param_count = num_params - 1 in
		let regular_args = ExtList.List.take regular_param_count args in
		let rest_args = ExtList.List.drop regular_param_count args in
		(* Generate regular args *)
		let regular_cs_args = List.mapi (fun i arg ->
			let expected_type = List.nth param_types i in
			generate_single_arg ectx cs_expr_of_texpr arg expected_type
		) regular_args in
		(* Generate rest args - wrap into Array<T> *)
		let rest_cs_arg =
			if rest_args = [] then
				(* No rest args - create empty Array (non-generic) *)
				CsNew (haxe_array_type, [])
			else begin
				(* Check if first rest arg is a spread expression *)
				match (List.hd rest_args).eexpr with
				| TUnop (Spread, _, spread_expr) ->
					(* Spread operator: pass through the inner expression directly.
					   The inner expression should already be Array. *)
					cs_expr_of_texpr ectx spread_expr
				| _ ->
					(* Multiple args to wrap into Array using appropriate factory method *)
					let elem_cs_type = cs_type_of_type ectx.gctx rest_elem_type in
					let rest_cs_args = List.map (fun arg ->
						let cs_arg = cs_expr_of_texpr ectx arg in
						coerce_arg ~in_scope:ectx.type_params_in_scope ectx.gctx cs_arg arg.etype rest_elem_type
					) rest_args in
					let native_array = CsNewArray (elem_cs_type, rest_cs_args) in
					let storage_type = classify_cs_array_element_type elem_cs_type in
					make_array_from_native storage_type native_array haxe_array_type
			end
		in
		regular_cs_args @ [rest_cs_arg]
	| None ->
		(* No Rest parameter - process all args normally *)
		List.mapi (fun i arg ->
			if i < num_params then
				let expected_type = List.nth param_types i in
				generate_single_arg ectx cs_expr_of_texpr arg expected_type
			else
				cs_expr_of_texpr ectx arg
		) args

(* ====== Closure infrastructure ====== *)

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
(* Parameters: ectx, obj_expr (CsThis or CsLocal), is_static, tclass option, class_path, type_params, class_field, method_type *)
let generate_method_closure_ref : (expr_context -> cs_expr option -> bool -> tclass option -> path -> Type.t list -> tclass_field -> Type.t -> cs_expr) ref =
	ref (fun _ _ _ _ _ _ _ _ -> failwith "Not initialized")

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

(* ====== Expression translation ====== *)

(* Convert Haxe expression to C# expression - mutually recursive with cs_stmt_of_texpr *)
let rec cs_expr_of_texpr ectx e =
	match e.eexpr with
	| TConst TNull ->
		(* For Null<T> types, generic type params, and value types, generate default(T) instead of null.
		   Value types (primitives) can't be null in C#, so we use default(T) which is 0/false/etc.
		   NOTE: CsNullable handles Null<Null<T>> flattening at the AST level,
		   so we only need to handle single-level Null<T> here. *)
		let cs_type = cs_type_of_type ectx.gctx e.etype in
		begin match cs_type with
		| CsTypeClass ((["haxe"; "lang"], "Null"), _) -> CsDefault cs_type
		| CsTypeGenericParam _ -> CsDefault cs_type  (* C# requires default(T) for generic params *)
		| CsTypeBool | CsTypeByte | CsTypeSByte | CsTypeChar
		| CsTypeShort | CsTypeUShort | CsTypeInt | CsTypeUInt
		| CsTypeLong | CsTypeULong | CsTypeFloat | CsTypeDouble | CsTypeDecimal ->
			(* Value types can't be null - use default(T) which gives 0/false/etc. *)
			CsDefault cs_type
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
		(* Check if this is array access on haxe.root.Array<T> - if so, access __objectArray directly *)
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
			(* Check if index is a string - use Reflect.field for field access.
			   Note: The index expression might be a string constant wrapped in TCast to Dynamic,
			   or the type might be Dynamic because of how __getField works (cast f).
			   We need to look at the EXPRESSION type, not the followed type, and also
			   check if it's a string constant. *)
			let is_string_index =
				(* Check if the expression itself is a string constant *)
				match e2.eexpr with
				| TConst (TString _) -> true
				| TCast ({ eexpr = TConst (TString _) }, _) -> true
				| _ ->
					(* Check the expression type - might be String directly *)
					match follow e2.etype with
					| TInst ({ cl_path = ([], "String") | (["haxe"; "root"], "String") }, _) -> true
					| TAbstract ({ a_path = ([], "String") }, _) -> true
					| _ -> false
			in
			let call = if is_string_index then
				(* String index on Dynamic means field access: Reflect.field(obj, name).
				   If the index is a TCast(string_const, _), extract just the string constant. *)
				let string_expr = match e2.eexpr with
					| TCast ({ eexpr = TConst (TString _) } as inner, _) -> cs_expr_of_texpr ectx inner
					| _ -> cs_expr_of_texpr ectx e2
				in
				CsStaticCall (CsTypeClass ((["haxe"; "root"], "Reflect"), []), "field", [cs_expr_of_texpr ectx e1; string_expr])
			else
				(* Int/numeric index: use runtime helper cs.Cs.arrayGet *)
				CsStaticCall (CsTypeClass ((["cs"], "Cs"), []), "arrayGet", [cs_expr_of_texpr ectx e1; cs_expr_of_texpr ectx e2])
			in
			(* Cast result to expected type if it's not Dynamic.
			   Use Runtime.toXxx for primitives to handle boxed type mismatches. *)
			let result_type = cs_type_of_type ectx.gctx e.etype in
			(* Erase out-of-scope type params to avoid CS0246 errors *)
			let result_type = CsTypeMapping.erase_out_of_scope_type_params ectx.type_params_in_scope result_type in
			cast_object_to_type result_type call
		end
		else
			let is_haxe_array = is_haxe_array_type e1.etype in
			let is_null_wrapper = find_null_in_expr e1 in
			if is_haxe_array then begin
				(* Array access using typed backing arrays based on element type classification.
				   - ArrayInt/Float/Bool: direct access to typed backing array
				   - ArrayObject: access __objectArray with element cast
				   - ArrayDynamic: use __getDyn() method for runtime dispatch *)
				let arr_expr = cs_expr_of_texpr ectx e1 in
				let arr_expr = if is_null_wrapper then CsField (arr_expr, "value") else arr_expr in
				let storage_type = classify_array_element_type e1.etype in
				let idx_expr = cs_expr_of_texpr ectx e2 in
				match storage_type with
				| ArrayInt ->
					(* arr.__getInt(i) - bounds-checked access, returns 0 for out-of-bounds *)
					CsCall (CsField (arr_expr, "__getInt"), [idx_expr])
				| ArrayFloat ->
					(* arr.__getFloat(i) - bounds-checked access, returns 0.0 for out-of-bounds *)
					CsCall (CsField (arr_expr, "__getFloat"), [idx_expr])
				| ArrayBool ->
					(* arr.__getBool(i) - bounds-checked access, returns false for out-of-bounds *)
					CsCall (CsField (arr_expr, "__getBool"), [idx_expr])
				| ArrayDynamic ->
					(* arr.__getDyn(i) - runtime dispatch method with bounds checking *)
					let access = CsCall (CsField (arr_expr, "__getDyn"), [idx_expr]) in
					(* Cast result to expected type if not Dynamic *)
					let expected_cs = cs_type_of_type ectx.gctx e.etype in
					begin match expected_cs with
					| CsTypeObject | CsTypeDynamic -> access
					| _ -> CsCast (expected_cs, access)
					end
				| ArrayObject ->
					(* arr.__get(i) - bounds-checked access, returns null for out-of-bounds *)
					let access = CsCall (CsField (arr_expr, "__get"), [idx_expr]) in
					let expected_cs = cs_type_of_type ectx.gctx e.etype in
					begin match expected_cs with
					| CsTypeObject | CsTypeDynamic -> access  (* No cast needed for object/dynamic *)
					| CsTypeClass ((["haxe"; "lang"], "Null"), [_inner_type]) ->
						(* For Null<T> element types, use Null<T>._ofDynamic(value) instead of direct cast.
						   The object[] stores boxed values (e.g., boxed int), not Null<T> structs.
						   Direct cast would fail with "Specified cast is not valid". *)
						CsStaticCall (expected_cs, "_ofDynamic", [access])
					| _ ->
						(* Cast element from object[] to expected type.
						   Use cast_object_to_type for proper primitive unboxing. *)
						cast_object_to_type expected_cs access
					end
			end
			else begin
				(* Check if e1 is a type parameter with Array<T> constraint.
				   If so, cast to Array before accessing - C# doesn't know about Haxe constraints.
				   Array is non-generic in C# output. *)
				match get_array_constraint_elem_type e1.etype with
				| Some _ ->
					(* Cast type param to Array: ((Array)b)[idx] *)
					let arr_cast = CsCast (haxe_array_type, cs_expr_of_texpr ectx e1) in
					(* Access __objectArray field of the cast array *)
					CsArrayAccess (CsField (arr_cast, "__objectArray"), cs_expr_of_texpr ectx e2)
				| None ->
					CsArrayAccess (cs_expr_of_texpr ectx e1, cs_expr_of_texpr ectx e2)
			end
	| TBinop (op, e1, e2) ->
		(* Special handling for Null<T> comparisons with null and generic type param equality.
		   Uses expr_produces_csharp_null_type (the SINGLE SOURCE OF TRUTH) to determine
		   if we need .hasValue instead of == null. See that function's documentation. *)
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
			(* Haxe Array assignment using typed backing arrays based on element type classification.
			   - ArrayInt/Float/Bool: direct assignment to typed backing array
			   - ArrayObject: direct assignment to __objectArray
			   - ArrayDynamic: use __setDyn() method for runtime dispatch *)
			begin match e1.eexpr with
			| TArray (arr, idx) ->
				let arr_cs = cs_expr_of_texpr ectx arr in
				let arr_cs = if arr_needs_unwrap then CsField (arr_cs, "value") else arr_cs in
				let idx_cs = cs_expr_of_texpr ectx idx in
				let val_cs = cs_expr_of_texpr ectx e2 in
				(* Use typed setter methods that handle backing array initialization.
				   Direct backing array access (arr.__intArray[i] = v) would fail with
				   NullReferenceException if the array was created with "new Array()" and
				   the backing array wasn't allocated yet. The typed setters handle this.
				   NOTE: Coerce the value to the expected type to handle erased type params. *)
				let storage_type = classify_array_element_type arr.etype in
				begin match storage_type with
				| ArrayInt ->
					(* arr.__setInt(i, v) - handles initialization and returns the value *)
					let coerced_val = coerce_arg ~in_scope:ectx.type_params_in_scope ectx.gctx val_cs (get_effective_expr_type ectx.gctx e2) e2.etype in
					CsCall (CsField (arr_cs, "__setInt"), [idx_cs; coerced_val])
				| ArrayFloat ->
					(* arr.__setFloat(i, v) - handles initialization and returns the value *)
					let coerced_val = coerce_arg ~in_scope:ectx.type_params_in_scope ectx.gctx val_cs (get_effective_expr_type ectx.gctx e2) e2.etype in
					CsCall (CsField (arr_cs, "__setFloat"), [idx_cs; coerced_val])
				| ArrayBool ->
					(* arr.__setBool(i, v) - handles initialization and returns the value *)
					let coerced_val = coerce_arg ~in_scope:ectx.type_params_in_scope ectx.gctx val_cs (get_effective_expr_type ectx.gctx e2) e2.etype in
					CsCall (CsField (arr_cs, "__setBool"), [idx_cs; coerced_val])
				| ArrayDynamic ->
					(* arr.__setDyn(i, v) - runtime dispatch method, returns Dynamic/object *)
					CsCall (CsField (arr_cs, "__setDyn"), [idx_cs; val_cs])
				| ArrayObject ->
					(* arr.__setObject(i, v) - returns Dynamic/object but Haxe assignment has element type.
					   Cast the result to the expected element type for type safety when the result is used. *)
					let call = CsCall (CsField (arr_cs, "__setObject"), [idx_cs; val_cs]) in
					let expected_type = cs_type_of_type ectx.gctx e2.etype in
					if expected_type <> CsTypeObject && expected_type <> CsTypeDynamic then
						CsCast (expected_type, call)
					else
						call
				end
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
				CsStaticCall (runtime_type, "SetField", [obj_expr; CsConst (CsConstString name); val_cs])
			| TField (obj, FAnon cf) ->
				(* Anonymous field assignment - check if it's a truly dynamic object or a known type *)
				let obj_expr = cs_expr_of_texpr ectx obj in
				let raw_type = Type.follow_once obj.etype in
				(* Only unwrap .value if the inner type actually needs the Null wrapper in C#.
				   For reference types (classes, anonymous types/object), Null<T> is stripped to T,
				   so there's no .value to access - the variable holds the value directly. *)
				let obj_expr, inner_type = match raw_type with
					| TAbstract ({ a_path = ([], "Null") }, [inner_t]) ->
						let inner_cs = cs_type_of_type ectx.gctx inner_t in
						if CsTypeMapping.is_inherently_nullable inner_cs then
							(obj_expr, inner_t)  (* No .value - Null<T> stripped to T in C# *)
						else
							(CsField (obj_expr, "value"), inner_t)
					| _ -> (obj_expr, obj.etype)
				in
				let val_cs = cs_expr_of_texpr ectx e2 in
				(* Check if the C# type is HaxeDynamicObject or object - use inner_type after Null unwrap *)
				let cs_type = cs_type_of_type ectx.gctx inner_type in
				begin match cs_type with
				| CsTypeClass ((["haxe"; "root"], "HaxeDynamicObject"), _) ->
					(* Dynamic object - use _hx_setField *)
					CsCall (CsField (obj_expr, "_hx_setField"), [CsConst (CsConstString cf.cf_name); val_cs])
				| CsTypeObject ->
					(* Object type - use Runtime.SetField *)
					CsStaticCall (runtime_type, "SetField", [obj_expr; CsConst (CsConstString cf.cf_name); val_cs])
				| _ ->
					(* Known anonymous type - generate direct field assignment *)
					CsBinop (CsOpAssign, CsField (obj_expr, escape_identifier cf.cf_name), val_cs)
				end
			| TField (obj, FInstance (c, tl, cf)) ->
				(* Instance field assignment - check if object type is erased to object due to
				   generic interface with Dynamic type parameter. If so, use Runtime.SetField. *)
				let obj_expr = cs_expr_of_texpr ectx obj in
				let cs_type = cs_type_of_type ectx.gctx obj.etype in
				let val_cs = cs_expr_of_texpr ectx e2 in
				begin match cs_type with
				| CsTypeObject ->
					(* Type was erased to object - use Runtime.SetField *)
					CsStaticCall (runtime_type, "SetField", [obj_expr; CsConst (CsConstString cf.cf_name); val_cs])
				| _ ->
					(* Normal field assignment - generate field access WITHOUT the read-cast
					   that would be added by the general TField handler. The read-cast is for
					   when reading from an erased type param field; for writing, we don't need it. *)
					let val_cs = if need_byte_cast then CsCast (CsTypeByte, val_cs) else val_cs in
					let val_cs = coerce_arg ~in_scope:ectx.type_params_in_scope ectx.gctx val_cs e2.etype e1.etype in
					(* Generate field access directly, handling Null<T> unwrap but NOT the read-cast *)
					let needs_unwrap = find_null_in_expr obj in
					let obj_expr_for_field = if needs_unwrap then CsField (obj_expr, "value") else obj_expr in
					(* Handle type param constraint casting if needed *)
					let obj_expr_for_field, field_name = match get_type_param_constraint obj.etype with
						| Some constraint_type ->
							let cs_constraint = cs_type_of_type ectx.gctx constraint_type in
							let casted = CsCast (cs_constraint, obj_expr_for_field) in
							let field = match cs_constraint, cf.cf_name with
								| CsTypeString, "length" -> "Length"
								| _ -> escape_identifier cf.cf_name
							in
							(casted, field)
						| None -> (obj_expr_for_field, escape_identifier cf.cf_name)
					in
					CsBinop (cs_binop_of_binop op, CsField (obj_expr_for_field, field_name), val_cs)
				end
			| _ ->
				let val_cs = cs_expr_of_texpr ectx e2 in
				let val_cs = if need_byte_cast then CsCast (CsTypeByte, val_cs) else val_cs in
				(* Coerce value to target type - needed when assigning object/Dynamic to typed variable *)
				let val_cs = coerce_arg ~in_scope:ectx.type_params_in_scope ectx.gctx val_cs e2.etype e1.etype in
				CsBinop (cs_binop_of_binop op, cs_expr_of_texpr ectx e1, val_cs)
			end
		| OpEq when expr_produces_csharp_null_type ectx.gctx e1 && is_null_expr e2 ->
			(* x == null  ->  !x.hasValue *)
			CsUnop (CsOpNot, false, CsField (cs_expr_of_texpr ectx e1, "hasValue"))
		| OpEq when is_null_expr e1 && expr_produces_csharp_null_type ectx.gctx e2 ->
			(* null == x  ->  !x.hasValue *)
			CsUnop (CsOpNot, false, CsField (cs_expr_of_texpr ectx e2, "hasValue"))
		| OpNotEq when expr_produces_csharp_null_type ectx.gctx e1 && is_null_expr e2 ->
			(* x != null  ->  x.hasValue *)
			CsField (cs_expr_of_texpr ectx e1, "hasValue")
		| OpNotEq when is_null_expr e1 && expr_produces_csharp_null_type ectx.gctx e2 ->
			(* null != x  ->  x.hasValue *)
			CsField (cs_expr_of_texpr ectx e2, "hasValue")
		(* Null<T> comparison operators:
		   When either operand is Null<T> and the other is a non-null value,
		   the comparison should return false if the Null<T> has no value.
		   When both are Null<T>, check both have values before comparing.
		   This prevents the implicit conversion to T (which uses default value 0/0.0)
		   from producing wrong results for null comparisons. *)
		| (OpGt | OpGte | OpLt | OpLte) when expr_produces_csharp_null_type ectx.gctx e1 && not (is_null_expr e2) && not (expr_produces_csharp_null_type ectx.gctx e2) ->
			(* Null<T> op value → nullExpr.hasValue && (nullExpr.value op value) *)
			let cs_e1 = cs_expr_of_texpr ectx e1 in
			let cs_e2 = cs_expr_of_texpr ectx e2 in
			CsBinop (CsOpBoolAnd,
				CsField (cs_e1, "hasValue"),
				CsBinop (cs_binop_of_binop op, CsField (cs_e1, "value"), cs_e2))
		| (OpGt | OpGte | OpLt | OpLte) when not (is_null_expr e1) && not (expr_produces_csharp_null_type ectx.gctx e1) && expr_produces_csharp_null_type ectx.gctx e2 ->
			(* value op Null<T> → nullExpr.hasValue && (value op nullExpr.value) *)
			let cs_e1 = cs_expr_of_texpr ectx e1 in
			let cs_e2 = cs_expr_of_texpr ectx e2 in
			CsBinop (CsOpBoolAnd,
				CsField (cs_e2, "hasValue"),
				CsBinop (cs_binop_of_binop op, cs_e1, CsField (cs_e2, "value")))
		| (OpGt | OpLt) when expr_produces_csharp_null_type ectx.gctx e1 && expr_produces_csharp_null_type ectx.gctx e2 ->
			(* Null<T> op Null<T> for > and < → a.hasValue && b.hasValue && a.value op b.value *)
			let cs_e1 = cs_expr_of_texpr ectx e1 in
			let cs_e2 = cs_expr_of_texpr ectx e2 in
			CsBinop (CsOpBoolAnd,
				CsBinop (CsOpBoolAnd,
					CsField (cs_e1, "hasValue"),
					CsField (cs_e2, "hasValue")),
				CsBinop (cs_binop_of_binop op, CsField (cs_e1, "value"), CsField (cs_e2, "value")))
		| (OpGte | OpLte) when expr_produces_csharp_null_type ectx.gctx e1 && expr_produces_csharp_null_type ectx.gctx e2 ->
			(* Null<T> op Null<T> for >= and <= → (!a.hasValue && !b.hasValue) || (a.hasValue && b.hasValue && a.value op b.value) *)
			let cs_e1 = cs_expr_of_texpr ectx e1 in
			let cs_e2 = cs_expr_of_texpr ectx e2 in
			CsBinop (CsOpBoolOr,
				CsBinop (CsOpBoolAnd,
					CsUnop (CsOpNot, false, CsField (cs_e1, "hasValue")),
					CsUnop (CsOpNot, false, CsField (cs_e2, "hasValue"))),
				CsBinop (CsOpBoolAnd,
					CsBinop (CsOpBoolAnd,
						CsField (cs_e1, "hasValue"),
						CsField (cs_e2, "hasValue")),
					CsBinop (cs_binop_of_binop op, CsField (cs_e1, "value"), CsField (cs_e2, "value"))))
		| OpEq when expr_produces_csharp_null_type ectx.gctx e1 && not (is_null_expr e2) && not (expr_produces_csharp_null_type ectx.gctx e2) ->
			(* Null<T> == value → nullExpr.hasValue && (nullExpr.value == value) *)
			let cs_e1 = cs_expr_of_texpr ectx e1 in
			let cs_e2 = cs_expr_of_texpr ectx e2 in
			CsBinop (CsOpBoolAnd,
				CsField (cs_e1, "hasValue"),
				CsBinop (CsOpEq, CsField (cs_e1, "value"), cs_e2))
		| OpEq when not (is_null_expr e1) && not (expr_produces_csharp_null_type ectx.gctx e1) && expr_produces_csharp_null_type ectx.gctx e2 ->
			(* value == Null<T> → nullExpr.hasValue && (value == nullExpr.value) *)
			let cs_e1 = cs_expr_of_texpr ectx e1 in
			let cs_e2 = cs_expr_of_texpr ectx e2 in
			CsBinop (CsOpBoolAnd,
				CsField (cs_e2, "hasValue"),
				CsBinop (CsOpEq, cs_e1, CsField (cs_e2, "value")))
		| OpEq when expr_produces_csharp_null_type ectx.gctx e1 && expr_produces_csharp_null_type ectx.gctx e2 ->
			(* Null<T> == Null<T> → (a.hasValue == b.hasValue) && (!a.hasValue || a.value == b.value) *)
			let cs_e1 = cs_expr_of_texpr ectx e1 in
			let cs_e2 = cs_expr_of_texpr ectx e2 in
			CsBinop (CsOpBoolAnd,
				CsBinop (CsOpEq, CsField (cs_e1, "hasValue"), CsField (cs_e2, "hasValue")),
				CsParens (CsBinop (CsOpBoolOr,
					CsUnop (CsOpNot, false, CsField (cs_e1, "hasValue")),
					CsBinop (CsOpEq, CsField (cs_e1, "value"), CsField (cs_e2, "value")))))
		| OpNotEq when expr_produces_csharp_null_type ectx.gctx e1 && not (is_null_expr e2) && not (expr_produces_csharp_null_type ectx.gctx e2) ->
			(* Null<T> != value → !nullExpr.hasValue || (nullExpr.value != value) *)
			let cs_e1 = cs_expr_of_texpr ectx e1 in
			let cs_e2 = cs_expr_of_texpr ectx e2 in
			CsBinop (CsOpBoolOr,
				CsUnop (CsOpNot, false, CsField (cs_e1, "hasValue")),
				CsBinop (CsOpNotEq, CsField (cs_e1, "value"), cs_e2))
		| OpNotEq when not (is_null_expr e1) && not (expr_produces_csharp_null_type ectx.gctx e1) && expr_produces_csharp_null_type ectx.gctx e2 ->
			(* value != Null<T> → !nullExpr.hasValue || (value != nullExpr.value) *)
			let cs_e1 = cs_expr_of_texpr ectx e1 in
			let cs_e2 = cs_expr_of_texpr ectx e2 in
			CsBinop (CsOpBoolOr,
				CsUnop (CsOpNot, false, CsField (cs_e2, "hasValue")),
				CsBinop (CsOpNotEq, cs_e1, CsField (cs_e2, "value")))
		| OpNotEq when expr_produces_csharp_null_type ectx.gctx e1 && expr_produces_csharp_null_type ectx.gctx e2 ->
			(* Null<T> != Null<T> → (a.hasValue != b.hasValue) || (a.hasValue && a.value != b.value) *)
			let cs_e1 = cs_expr_of_texpr ectx e1 in
			let cs_e2 = cs_expr_of_texpr ectx e2 in
			CsBinop (CsOpBoolOr,
				CsBinop (CsOpNotEq, CsField (cs_e1, "hasValue"), CsField (cs_e2, "hasValue")),
				CsBinop (CsOpBoolAnd,
					CsField (cs_e1, "hasValue"),
					CsBinop (CsOpNotEq, CsField (cs_e1, "value"), CsField (cs_e2, "value"))))
		| OpEq when is_generic_param e1.etype || is_generic_param e2.etype ->
			(* T == T  ->  Runtime.valEq(a, b) for generic type params *)
			CsStaticCall (runtime_type, "valEq", [cs_expr_of_texpr ectx e1; cs_expr_of_texpr ectx e2])
		| OpNotEq when is_generic_param e1.etype || is_generic_param e2.etype ->
			(* T != T  ->  !Runtime.valEq(a, b) for generic type params *)
			CsUnop (CsOpNot, false, CsStaticCall (runtime_type, "valEq", [cs_expr_of_texpr ectx e1; cs_expr_of_texpr ectx e2]))
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
			(* Helper: cast dynamic operation result to expected type if needed. *)
			let cast_dynamic_result call_expr =
				let expected_cs_type = cs_type_of_type ectx.gctx e.etype in
				cast_object_to_type expected_cs_type call_expr
			in
			begin match op with
			(* Arithmetic/bitwise operators on Dynamic need runtime dispatch via cs.Cs helpers *)
			| op when either_dynamic && List.mem_assoc op dynamic_binop_helpers ->
				let helper = List.assoc op dynamic_binop_helpers in
				cast_dynamic_result (CsStaticCall (CsTypeClass (cs_path, []), helper, [cs_expr_of_texpr ectx e1; cs_expr_of_texpr ectx e2]))
			(* Comparison operators on Dynamic also need runtime dispatch *)
			| OpLt when either_dynamic ->
				CsBinop (CsOpLt, CsStaticCall (CsTypeClass (cs_path, []), "compare", [cs_expr_of_texpr ectx e1; cs_expr_of_texpr ectx e2]), CsConst (CsConstInt 0l))
			| OpGt when either_dynamic ->
				CsBinop (CsOpGt, CsStaticCall (CsTypeClass (cs_path, []), "compare", [cs_expr_of_texpr ectx e1; cs_expr_of_texpr ectx e2]), CsConst (CsConstInt 0l))
			| OpLte when either_dynamic ->
				CsBinop (CsOpLte, CsStaticCall (CsTypeClass (cs_path, []), "compare", [cs_expr_of_texpr ectx e1; cs_expr_of_texpr ectx e2]), CsConst (CsConstInt 0l))
			| OpGte when either_dynamic ->
				CsBinop (CsOpGte, CsStaticCall (CsTypeClass (cs_path, []), "compare", [cs_expr_of_texpr ectx e1; cs_expr_of_texpr ectx e2]), CsConst (CsConstInt 0l))
			(* Equality comparisons with Dynamic - use Runtime.valEq for value-based equality *)
			| OpEq when either_dynamic ->
				CsStaticCall (runtime_type, "valEq", [cs_expr_of_texpr ectx e1; cs_expr_of_texpr ectx e2])
			| OpNotEq when either_dynamic ->
				CsUnop (CsOpNot, false, CsStaticCall (runtime_type, "valEq", [cs_expr_of_texpr ectx e1; cs_expr_of_texpr ectx e2]))
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
						(* These helpers return object, but the expression may have a specific type. *)
						let call_expr = CsStaticCall (CsTypeClass (cs_path, []), helper_name,
							[cs_expr_of_texpr ectx obj_expr; CsConst (CsConstString field_name); cs_expr_of_texpr ectx e2]) in
						let expected_cs_type = cs_type_of_type ectx.gctx e.etype in
						cast_object_to_type expected_cs_type call_expr
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
						(* These helpers return object, but the expression may have a specific type. *)
						let call_expr = CsStaticCall (CsTypeClass (cs_path, []), helper_name,
							[cs_expr_of_texpr ectx arr_expr; cs_expr_of_texpr ectx idx_expr; cs_expr_of_texpr ectx e2]) in
						let expected_cs_type = cs_type_of_type ectx.gctx e.etype in
						cast_object_to_type expected_cs_type call_expr
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
					(* Check if e1 is a Haxe Array element access with object storage that needs a cast.
					   In C#, you can't do compound assignment on a cast expression:
					     ((string)(arr.__objectArray[i])) += "x"  // Invalid!
					   Expand to: arr.__objectArray[i] = ((string)(arr.__objectArray[i])) + "x" *)
					let rec is_haxe_array_type t =
						match follow t with
						| TInst ({ cl_path = ([], "Array") | (["haxe"; "root"], "Array") }, _) -> true
						| TAbstract ({ a_path = ([], "Null") }, [inner]) -> is_haxe_array_type inner
						| TAbstract (a, tl) when a.a_path <> ([], "Null") ->
							let underlying = Abstract.get_underlying_type a tl in
							is_haxe_array_type underlying
						| _ -> false
					in
					let is_null_wrapper e =
						match follow e.etype with
						| TAbstract ({ a_path = ([], "Null") }, _) -> true
						| _ -> false
					in
					let haxe_array_element_access = match inner_e1.eexpr with
						| TArray (arr_expr, idx_expr) when is_haxe_array_type arr_expr.etype ->
							let storage = classify_array_element_type arr_expr.etype in
							Some (arr_expr, idx_expr, storage)
						| _ -> None
					in
					begin match haxe_array_element_access with
					| Some (arr_expr, idx_expr, storage) ->
						(* Expand compound assignment on Haxe array elements.
						   We can't use method call results as lvalues in C#.
						   arr[i] += v  ->  arr.__backing[i] = arr.__backing[i] op v *)
						let arr_cs = cs_expr_of_texpr ectx arr_expr in
						let arr_cs = if is_null_wrapper arr_expr then CsField (arr_cs, "value") else arr_cs in
						let idx_cs = cs_expr_of_texpr ectx idx_expr in
						let e2_cs = cs_expr_of_texpr ectx e2 in
						let backing_field, needs_cast = match storage with
							| ArrayInt -> "__intArray", false
							| ArrayFloat -> "__floatArray", false
							| ArrayBool -> "__boolArray", false
							| ArrayObject | ArrayDynamic -> "__objectArray", true
						in
						let array_access = CsArrayAccess (CsField (arr_cs, backing_field), idx_cs) in
						if needs_cast then begin
							let elem_cs_type = cs_type_of_type ectx.gctx e1.etype in
							(* (T)(arr.__objectArray[i]) op v - use cast_object_to_type for proper unboxing *)
							let casted_read = cast_object_to_type elem_cs_type array_access in
							let op_result = CsBinop (cs_binop_of_binop inner_op, casted_read, e2_cs) in
							(* arr.__objectArray[i] = result *)
							CsBinop (CsOpAssign, array_access, op_result)
						end else begin
							(* arr.__intArray[i] op= v directly - no cast needed *)
							CsBinop (cs_binop_of_binop op, array_access, e2_cs)
						end
					| None ->
						(* Check if e1 is a field access with erased type param - also can't do compound assignment on cast *)
						let rec find_erased_field_for_assign expr = match expr.eexpr with
							| TField (obj_expr, FInstance (cl, tl, cf)) ->
								if is_erased_type_param cf.cf_type then
									Some (obj_expr, cl, tl, cf, expr.etype)
								else
									None
							| TParenthesis inner -> find_erased_field_for_assign inner
							| TCast (inner, _) -> find_erased_field_for_assign inner
							| _ -> None
						in
						begin match find_erased_field_for_assign e1 with
						| Some (obj_expr, _cl, _tl, cf, expr_type) ->
							(* Expand: ((T)obj.field) += v  ->  (T)(obj.field = (object)(((T)obj.field) + v)) *)
							let obj_cs = cs_expr_of_texpr ectx obj_expr in
							let obj_cs = if find_null_in_expr obj_expr then CsField (obj_cs, "value") else obj_cs in
							let field_name = escape_identifier cf.cf_name in
							let field_access = CsField (obj_cs, field_name) in
							let cast_type = cs_type_of_type ectx.gctx expr_type in
							let e2_cs = cs_expr_of_texpr ectx e2 in
							(* Cast obj.field to T - use cast_object_to_type for proper unboxing *)
							let casted_read = cast_object_to_type cast_type field_access in
							(* (T)obj.field op v *)
							let op_result = CsBinop (cs_binop_of_binop inner_op, casted_read, e2_cs) in
							(* (object)(op_result) for boxing *)
							let boxed = CsCast (CsTypeObject, op_result) in
							(* obj.field = boxed *)
							let assignment = CsBinop (CsOpAssign, field_access, boxed) in
							(* Cast assignment result to T - use cast_object_to_type for proper unboxing *)
							cast_object_to_type cast_type assignment
						| None ->
							(* Non-dynamic compound assignment - handle special cases *)
							begin match inner_op with
							| OpUShr ->
								(* Compound unsigned right shift: n >>>= k -> n = (int)((uint)n >> k) *)
								let e1_cs = cs_expr_of_texpr ectx e1 in
								let e2_cs = cs_expr_of_texpr ectx e2 in
								let inner_type = cs_type_of_type ectx.gctx e1.etype in
								let e1_as_unsigned =
									match inner_type with
									| CsTypeClass ((["haxe"; "lang"], "Null"), [CsTypeInt]) ->
										CsCast (CsTypeUInt, CsCast (CsTypeInt, e1_cs))
									| CsTypeClass ((["haxe"; "lang"], "Null"), [CsTypeLong]) ->
										CsCast (CsTypeULong, CsCast (CsTypeLong, e1_cs))
									| CsTypeLong ->
										CsCast (CsTypeULong, e1_cs)
									| _ ->
										CsCast (CsTypeUInt, e1_cs)
								in
								let result_type = match inner_type with
									| CsTypeClass ((["haxe"; "lang"], "Null"), [CsTypeLong]) | CsTypeLong -> CsTypeLong
									| _ -> CsTypeInt
								in
								let shifted = CsCast (result_type, CsBinop (CsOpShr, e1_as_unsigned, e2_cs)) in
								CsBinop (CsOpAssign, e1_cs, shifted)
							| _ ->
								(* Other compound ops - use normal operator *)
								CsBinop (cs_binop_of_binop op, cs_expr_of_texpr ectx e1, cs_expr_of_texpr ectx e2)
							end
						end
					end
				end
			| _ ->
				(* Handle special cases for arithmetic operations *)
				let is_int_type t = match follow t with
					| TAbstract ({ a_path = ([], "Int") }, _) -> true
					| _ -> false
				in
				let is_float_type t = match follow t with
					| TAbstract ({ a_path = ([], "Float") }, _) -> true
					| TAbstract ({ a_path = ([], "Single") }, _) -> true
					| _ -> false
				in
				let is_bool_type t = match follow t with
					| TAbstract ({ a_path = ([], "Bool") }, _) -> true
					| _ -> false
				in
				(* Check if type is a type parameter (generic). In C#, arithmetic on type params isn't allowed. *)
				let is_type_param t = match follow t with
					| TInst ({ cl_kind = KTypeParameter _ }, _) -> true
					| _ -> false
				in
				(* String concatenation with floats/doubles needs invariant culture to ensure '.' decimal separator.
				   C#'s implicit ToString() for string + float uses current culture, which may use comma.
				   Also: C# drops null in string concatenation, but Haxe expects null to become "null" string.
				   We use Runtime.toStr() for non-string operands that could be null to fix both issues. *)
				let is_string_concat = op = OpAdd && is_string e.etype in
				let needs_invariant_float_to_string e =
					is_string_concat && is_float_type e.etype
				in
				(* Check if an operand could be null at runtime and needs conversion to "null" string.
				   This covers: Object, Dynamic, class types, Null<T>, and explicitly nullable types.
				   Primitives (int, bool, float) cannot be null and don't need this. *)
				let could_be_null_reference e =
					if not is_string_concat then false
					else if is_string e.etype then false  (* Strings handled by + operator *)
					else if is_int_type e.etype then false
					else if is_float_type e.etype then false
					else if is_bool_type e.etype then false
					else true  (* Object, Dynamic, class types, Null<T> - all could be null *)
				in
				let cs_e1 = cs_expr_of_texpr ectx e1 in
				let cs_e2 = cs_expr_of_texpr ectx e2 in
				(* Wrap operands for string concatenation:
				   - float: cs.Cs.toString() for invariant culture
				   - nullable types: Runtime.toStrConcat() to convert null to "null" string *)
				let wrap_for_string_concat e cs_e =
					if needs_invariant_float_to_string e then
						CsStaticCall (CsTypeClass (cs_path, []), "toString", [cs_e])
					else if could_be_null_reference e then
						object_to_string_for_concat cs_e
					else cs_e
				in
				let cs_e1 = wrap_for_string_concat e1 cs_e1 in
				let cs_e2 = wrap_for_string_concat e2 cs_e2 in
				(* For type parameters, cast through object to int for arithmetic operations.
				   C# doesn't allow arithmetic on generic types even with constraints. *)
				let is_arithmetic_op = match op with
					| OpAdd | OpSub | OpMult | OpDiv | OpMod -> true
					| _ -> false
				in
				let cs_e1, cs_e2 =
					if is_arithmetic_op && (is_type_param e1.etype || is_type_param e2.etype) then
						(* Cast type params to int for arithmetic *)
						let cast_if_needed e cs_e =
							if is_type_param e.etype then
								CsCast (CsTypeInt, cs_e)
							else cs_e
						in
						(cast_if_needed e1 cs_e1, cast_if_needed e2 cs_e2)
					else match op with
						| OpDiv when is_int_type e1.etype && is_int_type e2.etype && is_float_type e.etype ->
							(* Cast both operands to double for float division semantics *)
							CsCast (CsTypeDouble, cs_e1), CsCast (CsTypeDouble, cs_e2)
						| _ -> cs_e1, cs_e2
				in
				(* Special case for unsigned right shift (>>>).
				   C# uses signed >> for int, so we need: (int)((uint)e1 >> e2)
				   This casts to unsigned, shifts, then casts back to signed.
				   If e1 is Null<int>, we need to first convert to int (via implicit conversion)
				   before casting to uint, since C# can't cast Null<T> directly to uint. *)
				begin match op with
				| OpUShr ->
					let inner_type = cs_type_of_type ectx.gctx e1.etype in
					let e1_as_unsigned =
						match inner_type with
						| CsTypeClass ((["haxe"; "lang"], "Null"), [CsTypeInt]) ->
							(* Null<int> -> int (implicit) -> uint *)
							CsCast (CsTypeUInt, CsCast (CsTypeInt, cs_e1))
						| CsTypeClass ((["haxe"; "lang"], "Null"), [CsTypeLong]) ->
							(* Null<long> -> long (implicit) -> ulong *)
							CsCast (CsTypeULong, CsCast (CsTypeLong, cs_e1))
						| CsTypeLong ->
							(* long -> ulong *)
							CsCast (CsTypeULong, cs_e1)
						| _ ->
							(* int or other -> uint *)
							CsCast (CsTypeUInt, cs_e1)
					in
					let result_type = match inner_type with
						| CsTypeClass ((["haxe"; "lang"], "Null"), [CsTypeLong]) | CsTypeLong -> CsTypeLong
						| _ -> CsTypeInt
					in
					CsCast (result_type, CsBinop (CsOpShr, e1_as_unsigned, cs_e2))
				| _ ->
					CsBinop (cs_binop_of_binop op, cs_e1, cs_e2)
				end
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
		(* Check if the inner expression is an array element access on a Haxe Array.
		   For ++/--, we need direct backing array access (not method call) to have an lvalue. *)
		let is_haxe_array_element = match op with
			| Increment | Decrement ->
				let rec find_array_access expr = match expr.eexpr with
					| TArray (arr_expr, idx_expr) ->
						let is_haxe_array = match follow arr_expr.etype with
							| TInst ({ cl_path = ([], "Array") | (["haxe"; "root"], "Array") }, _) -> true
							| _ -> false
						in
						if is_haxe_array then Some (arr_expr, idx_expr, unop_operand.etype) else None
					| TParenthesis inner | TCast (inner, _) -> find_array_access inner
					| _ -> None
				in
				find_array_access inner_expr
			| _ -> None
		in
		(* Handle array element increment/decrement with direct backing array access *)
		begin match is_haxe_array_element with
		| Some (arr_expr, idx_expr, elem_type) ->
			let arr_cs = cs_expr_of_texpr ectx arr_expr in
			let idx_cs = cs_expr_of_texpr ectx idx_expr in
			let storage_type = CsTypeMapping.classify_array_element_type arr_expr.etype in
			let backing_access = match storage_type with
				| CsTypeMapping.ArrayInt -> CsArrayAccess (CsField (arr_cs, "__intArray"), idx_cs)
				| CsTypeMapping.ArrayFloat -> CsArrayAccess (CsField (arr_cs, "__floatArray"), idx_cs)
				| CsTypeMapping.ArrayBool -> CsArrayAccess (CsField (arr_cs, "__boolArray"), idx_cs)
				| CsTypeMapping.ArrayObject | CsTypeMapping.ArrayDynamic ->
					CsArrayAccess (CsField (arr_cs, "__objectArray"), idx_cs)
			in
			let cs_op = if op = Increment then CsOpIncrement else CsOpDecrement in
			let is_postfix = pos = Postfix in
			let unop_result = CsUnop (cs_op, is_postfix, backing_access) in
			(* Cast result to expected type if needed (for object arrays) *)
			let expected_cs = cs_type_of_type ectx.gctx elem_type in
			begin match storage_type with
			| CsTypeMapping.ArrayObject | CsTypeMapping.ArrayDynamic ->
				CsCast (expected_cs, unop_result)
			| _ -> unop_result
			end
		| None ->
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
			(* The helper returns object, cast to expected type. *)
			let expected_cs_type = cs_type_of_type ectx.gctx e.etype in
			cast_object_to_type expected_cs_type call_expr
		| Some (obj_expr, field_name), Decrement ->
			let helper_name = if is_postfix then "fieldPostDecrement" else "fieldPreDecrement" in
			let call_expr = CsStaticCall (CsTypeClass ((["cs"], "Cs"), []), helper_name, [cs_expr_of_texpr ectx obj_expr; CsConst (CsConstString field_name)]) in
			(* The helper returns object, cast to expected type. *)
			let expected_cs_type = cs_type_of_type ectx.gctx e.etype in
			cast_object_to_type expected_cs_type call_expr
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
					(* Cast the result to expected type if it's not Dynamic *)
					let expected_cs_type = cs_type_of_type ectx.gctx e.etype in
					cast_object_to_type expected_cs_type call_expr
				end
			else
				(* Check if the operand is a cast of a field access where the field is object-typed.
				   In C#, you can't do ++/-- on an unboxing cast result (CS0445).
				   Expand: ((int)obj.field)++  ->  (int)(obj.field = (object)((int)obj.field + 1)) - 1
				   Expand: ++((int)obj.field)  ->  (int)(obj.field = (object)((int)obj.field + 1)) *)
				let needs_expansion = match op with
					| Increment | Decrement ->
						(* Check if operand is a field access where the field is object-typed due to erasure.
						   In gencs, such fields get cast to their substituted type, but C# doesn't
						   allow ++/-- on an unboxing cast result. We need to detect this BEFORE
						   the cs_expr_of_texpr call adds the cast. *)
						let rec find_erased_field expr = match expr.eexpr with
							| TField (obj_expr, FInstance (cl, tl, cf)) ->
								(* Check if field is an erased type param - these get casted during codegen *)
								if is_erased_type_param cf.cf_type then
									Some (obj_expr, cl, tl, cf, expr.etype)
								else
									None
							| TParenthesis inner -> find_erased_field inner
							| TCast (inner, _) -> find_erased_field inner
							| _ -> None
						in
						find_erased_field unop_operand
					| _ -> None
				in
				begin match needs_expansion with
				| Some (obj_expr, _cl, _tl, cf, cast_to_type) ->
					(* Expand the increment/decrement operation on a cast of an object-typed field *)
					let is_postfix = pos = Postfix in
					let obj_cs = cs_expr_of_texpr ectx obj_expr in
					(* Handle Null<T> unwrapping *)
					let obj_cs = if find_null_in_expr obj_expr then CsField (obj_cs, "value") else obj_cs in
					let field_name = escape_identifier cf.cf_name in
					let field_access = CsField (obj_cs, field_name) in
					let cast_type = cs_type_of_type ectx.gctx cast_to_type in
					(* Cast obj.field to cast_type - use cast_object_to_type for proper unboxing *)
					let casted_read = cast_object_to_type cast_type field_access in
					(* (cast_type)obj.field + 1 or - 1 *)
					let delta = if op = Increment then CsConst (CsConstInt 1l) else CsConst (CsConstInt (-1l)) in
					let new_value = CsBinop (CsOpAdd, casted_read, delta) in
					(* (object)new_value - box for storage *)
					let boxed_value = CsCast (CsTypeObject, new_value) in
					(* obj.field = boxed_value *)
					let assignment = CsBinop (CsOpAssign, field_access, boxed_value) in
					if is_postfix then
						(* For postfix: cast(assignment) - 1  (return old value) *)
						let result_minus_delta = CsBinop (CsOpSub, cast_object_to_type cast_type assignment, delta) in
						result_minus_delta
					else
						(* For prefix: cast(assignment)  (return new value) *)
						cast_object_to_type cast_type assignment
				| None ->
					CsUnop (cs_unop_of_unop op, is_postfix, cs_expr_of_texpr ectx unop_operand)
				end
		end
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
		(* Check if the C# type is object (due to erasure of generic interface with Dynamic),
		   OR if the expression is a method/field access that returns an erased type param.
		   In both cases, we can't do direct field access - use Reflect.field/setField. *)
		let cs_type = cs_type_of_type ectx.gctx e.etype in
		let expr_is_erased = expr_returns_erased_type_param e in
		begin match cs_type with
		| CsTypeObject ->
			(* Type was erased to object - use Reflect for field access *)
			let reflect_path = (["haxe"; "root"], "Reflect") in
			let field_call = CsStaticCall (CsTypeClass (reflect_path, []), "field", [obj_expr; CsConst (CsConstString cf.cf_name)]) in
			let target_type = cs_type_of_type ectx.gctx cf.cf_type in
			begin match target_type with
			| CsTypeObject -> field_call
			| _ -> CsCast (target_type, field_call)
			end
		| _ when expr_is_erased ->
			(* Expression returns erased type param (e.g., Type.createInstance returns object in C#) *)
			(* Need to cast the object to the expected type, then access the field *)
			let target_cs_type = cs_type_of_type ectx.gctx e.etype in
			let casted_obj = CsCast (target_cs_type, obj_expr) in
			let field_name = escape_identifier cf.cf_name in
			let field_access = CsField (casted_obj, field_name) in
			(* If the field itself returns an erased type param, cast the result too *)
			if is_erased_type_param cf.cf_type then
				let map_type = apply_params c.cl_params tl in
				let substituted_type = map_type cf.cf_type in
				let result_cs_type = cs_type_of_type ectx.gctx substituted_type in
				(* Use cast_object_to_type for proper unboxing when field returns object *)
				cast_object_to_type result_cs_type field_access
			else
				field_access
		| _ ->
			(* Check if the object is a type parameter - may need to cast to constraint type for field access.
			   This handles cases where C# can't express the constraint (e.g., T:String where String is sealed). *)
			let obj_expr, field_name = match get_type_param_constraint e.etype with
				| Some constraint_type ->
					(* Cast to constraint type for field access *)
					let cs_constraint = cs_type_of_type ectx.gctx constraint_type in
					let casted = CsCast (cs_constraint, obj_expr) in
					(* Translate field names for specific C# types (e.g., length -> Length for string) *)
					let field = match cs_constraint, cf.cf_name with
						| CsTypeString, "length" -> "Length"
						| _ -> escape_identifier cf.cf_name
					in
					(casted, field)
				| None -> (obj_expr, escape_identifier cf.cf_name)
			in
			let field_access = CsField (obj_expr, field_name) in
			(* Check if the field's declared type is an erased type param.
			   If so, the C# expression returns object, but we need to cast to the
			   substituted type for subsequent field/method access to work. *)
				if is_erased_type_param cf.cf_type then
				(* Apply type params to get the actual Haxe type after substitution *)
				let map_type = apply_params c.cl_params tl in
				let substituted_type = map_type cf.cf_type in
				let target_cs_type = cs_type_of_type ectx.gctx substituted_type in
				(* Use cast_object_to_type for proper unboxing when field returns object *)
				cast_object_to_type target_cs_type field_access
			else
				field_access
		end
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
			!generate_method_closure_ref ectx (Some obj_expr) false (Some c) c.cl_path tl cf method_type
		end
	| TField (e, FClosure (None, cf)) ->
		(* Static method closure - generate a closure class that wraps the static method call.
		   C# doesn't allow converting method groups to haxe.lang.Function directly. *)
		begin match e.eexpr with
		| TTypeExpr (TClassDecl c) ->
			(* Static closure - generate closure class with no captures *)
			!generate_method_closure_ref ectx None true (Some c) c.cl_path [] cf cf.cf_type
		| TTypeExpr mt ->
			(* Other module type - fallback to field access (shouldn't happen for methods) *)
			let t = type_of_module_type mt in
			let cs_type = cs_type_of_type ectx.gctx t in
			let cs_type = CsTypeMapping.erase_type_params cs_type in
			begin match cs_type with
			| CsTypeClass (path, params) -> CsStaticField (CsTypeClass (path, params), escape_identifier cf.cf_name)
			| _ -> CsField (cs_expr_of_texpr ectx e, escape_identifier cf.cf_name)
			end
		| _ ->
			(* Field access on non-class-type expression (e.g., local variable with structural type).
			   Check if the type is structural/anonymous and use Reflect.field for dynamic access. *)
			let obj_expr = cs_expr_of_texpr ectx e in
			let cs_type = cs_type_of_type ectx.gctx e.etype in
			begin match cs_type with
			| CsTypeObject ->
				(* Object/anonymous type - use Reflect.field for dynamic access *)
				let reflect_path = (["haxe"; "root"], "Reflect") in
				let field_call = CsStaticCall (CsTypeClass (reflect_path, []), "field", [obj_expr; CsConst (CsConstString cf.cf_name)]) in
				(* Cast to haxe.lang.Function since this is a method closure context *)
				CsCast (CsTypeClass ((["haxe"; "lang"], "Function"), []), field_call)
			| _ ->
				(* Known type - direct field access *)
				CsField (obj_expr, escape_identifier cf.cf_name)
			end
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
			!generate_method_closure_ref ectx None true (Some c) c.cl_path [] cf cf.cf_type
		else begin
			(* Static field access - normal field reference *)
			(* Special handling for String and Array *)
			let actual_path, actual_params = match c.cl_path with
			| ([], ("String" | "string")) | (["haxe"; "root"], ("String" | "string")) ->
				(* String static methods like fromCharCode are in cs.StringExt *)
				((["cs"], "StringExt"), [])
			| ([], "Array") | (["haxe"; "root"], "Array") ->
				(* Array is non-generic in C# output *)
				(cs_path_of_path c.cl_path, [])
			| _ ->
				let path = cs_path_of_path c.cl_path in
				(* Type erasure: Haxe generic classes become non-generic in C#.
				   Only C# native types keep their type parameters. *)
				if CsTypeMapping.is_cs_native_generic_class c.cl_path then begin
					(* C# native class - keep type parameters, try to infer from field type *)
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
				end else
					(* Haxe class - erase type parameters (class is non-generic in C#) *)
					(path, [])
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
			| CsTypeClass ((["haxe"; "lang"], "Null"), _) ->
				(* Object to Null<T> - use _ofDynamic for proper null handling *)
				CsStaticCall (target_type, "_ofDynamic", [field_call])
			(* Use cast_object_to_type for proper unboxing of primitives *)
			| _ -> cast_object_to_type target_type field_call
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
			| CsTypeClass ((["haxe"; "lang"], "Null"), _) ->
				(* Object to Null<T> - use _ofDynamic for proper null handling *)
				CsStaticCall (target_type, "_ofDynamic", [field_call])
			(* Use cast_object_to_type for proper unboxing of primitives *)
			| _ -> cast_object_to_type target_type field_call
			end
		| CsTypeGenericParam _ ->
			(* Type parameter - cast to HaxeObject to call _hx_getField *)
			let haxe_object_type = CsTypeClass ((["haxe"; "root"], "HaxeObject"), []) in
			let casted_obj = CsCast (haxe_object_type, obj_expr) in
			let field_call = CsCall (CsField (casted_obj, "_hx_getField"), [CsConst (CsConstString cf.cf_name)]) in
			let target_type = cs_type_of_type ectx.gctx cf.cf_type in
			begin match target_type with
			| CsTypeObject -> field_call
			| CsTypeClass ((["haxe"; "lang"], "Null"), _) ->
				(* Object to Null<T> - use _ofDynamic for proper null handling *)
				CsStaticCall (target_type, "_ofDynamic", [field_call])
			(* Use cast_object_to_type for proper unboxing of primitives *)
			| _ -> cast_object_to_type target_type field_call
			end
		| _ ->
			(* Fallback to dynamic dispatch via _hx_getField *)
			let field_call = CsCall (CsField (obj_expr, "_hx_getField"), [CsConst (CsConstString cf.cf_name)]) in
			let target_type = cs_type_of_type ectx.gctx cf.cf_type in
			begin match target_type with
			| CsTypeObject -> field_call
			| CsTypeClass ((["haxe"; "lang"], "Null"), _) ->
				(* Object to Null<T> - use _ofDynamic for proper null handling *)
				CsStaticCall (target_type, "_ofDynamic", [field_call])
			(* Use cast_object_to_type for proper unboxing of primitives *)
			| _ -> cast_object_to_type target_type field_call
			end
		end
	| TField (e_obj, FDynamic name) ->
		(* Dynamic field access - need to use reflection since C# object doesn't have arbitrary fields *)
		let obj_expr = cs_expr_of_texpr ectx e_obj in
		(* Check if the C# type is actually Null<T> (with type-level stripping, some Null<T>
		   become just T when the inner type is inherently nullable) *)
		let cs_type = cs_type_of_type ectx.gctx e_obj.etype in
		let is_cs_null_type = match cs_type with
			| CsTypeClass ((["haxe"; "lang"], "Null"), _) -> true
			| _ -> false
		in
		(* Special case: accessing .value or .hasValue on Null<T> - use direct field access, not reflection.
		   Only applies if the C# type is actually Null<T>. *)
		if is_cs_null_type && (name = "value" || name = "hasValue") then
			CsField (obj_expr, name)
		else begin
			(* Regular dynamic field access via reflection *)
			(* Use haxe.lang.Runtime.GetField for dynamic field access *)
			let field_call =
				CsStaticCall (runtime_type, "GetField", [obj_expr; CsConst (CsConstString name)])
			in
			(* Runtime.GetField returns object, but Haxe knows the actual type.
			   Cast to the expected type if it's not Dynamic/object.
			   Use cast_object_to_type for proper unboxing of primitives. *)
			let result_cs_type = cs_type_of_type ectx.gctx e.etype in
			begin match result_cs_type with
			| CsTypeObject | CsTypeDynamic -> field_call
			| CsTypeClass ((["haxe"; "lang"], "Null"), _) ->
				(* Object to Null<T> - use _ofDynamic for proper null handling *)
				CsStaticCall (result_cs_type, "_ofDynamic", [field_call])
			| _ -> cast_object_to_type result_cs_type field_call
			end
		end
	| TField (_, FEnum (en, ef)) ->
		let path = cs_path_of_path en.e_path in
		(* Enums are non-generic in C# after type erasure - no type arguments *)
		let type_args = [] in
		ignore (match follow e.etype with | _ -> ());
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
			(* Build invokeDynamic method - Array is non-generic in C# output *)
			let args_array = CsField (CsLocal "args", "__objectArray") in
			let invoke_dynamic_args = List.mapi (fun i cs_type ->
				let idx_const = CsConst (CsConstInt (Int32.of_int i)) in
				let arg_access = CsArrayAccess (args_array, idx_const) in
				(* Use cast_object_to_type for proper unboxing of primitives *)
				cast_object_to_type cs_type arg_access
			) param_types_cs in
			let invoke_call_dyn = CsCall (CsLocal (invoke_method_name num_params), invoke_dynamic_args) in
			let invoke_dynamic_method = CsMemberMethod {
				m_name = "invokeDynamic";
				m_return_type = CsTypeObject;
				m_access = AccessModifier.Public;
				m_modifiers = [MemberModifier.Override];
				m_type_params = [];
				m_params = [{ p_name = "args"; p_type = Some haxe_array_type; p_default = None; p_modifier = None }];
				m_body = Some [CsReturn (Some invoke_call_dyn)];
				m_constraints = [];
				m_explicit_interface = None;
				m_attributes = [];
			} in
			(* Build Value-based __hx_invokeN method *)
			let fv_params = List.mapi (fun i _ ->
				{ p_name = "a" ^ string_of_int (i + 1); p_type = Some hxvalue_type; p_default = None; p_modifier = None }
			) param_types_cs in
			(* Extract args from Value *)
			let fv_extract_args = List.mapi (fun i cs_type ->
				let a_var = CsLocal ("a" ^ string_of_int (i + 1)) in
				cast_value_to_type cs_type a_var
			) param_types_cs in
			let fv_invoke_call = CsCall (CsLocal (invoke_method_name num_params), fv_extract_args) in
			let fv_return = CsStaticCall (hxvalue_type, "FromObject", [fv_invoke_call]) in
			let fv_invoke_method = CsMemberMethod {
				m_name = hxvalue_invoke_method_name num_params;
				m_return_type = hxvalue_type;
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
		(* For parameterless enum constructors, access via static field.
		   After type erasure, all enums are non-generic in C#, so always use static field. *)
		else
			CsStaticField (CsTypeClass (path, []), get_cs_enum_ctor_name en ef)
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
		(* Handle Rest parameters - check if last param is Rest<T> *)
		let num_params = List.length param_types_hx in
		let last_param_rest = if num_params > 0 then
			get_rest_element_type (List.nth param_types_hx (num_params - 1))
		else
			None
		in
		let args_cs, param_types_cs = match last_param_rest with
			| Some rest_elem_type ->
				(* Last param is Rest<T> - split args into regular and rest parts *)
				let regular_param_count = num_params - 1 in
				let regular_args = ExtList.List.take regular_param_count args in
				let rest_args = ExtList.List.drop regular_param_count args in
				(* Generate regular args *)
				let regular_cs_args = List.mapi (fun i arg ->
					let expected_type = List.nth param_types_hx i in
					let cs_arg = cs_expr_of_texpr ectx arg in
					coerce_arg ~in_scope:ectx.type_params_in_scope ectx.gctx cs_arg arg.etype expected_type
				) regular_args in
				(* Generate rest args - wrap into Array<T> *)
				let rest_cs_arg =
					if rest_args = [] then
						(* No rest args - create empty Array *)
						CsNew (haxe_array_type, [])
					else begin
						(* Check if first rest arg is a spread expression *)
						match (List.hd rest_args).eexpr with
						| TUnop (Spread, _, spread_expr) ->
							(* Spread operator: pass through the inner expression directly *)
							cs_expr_of_texpr ectx spread_expr
						| _ ->
							(* Multiple args to wrap into Array<T> using appropriate factory method *)
							let elem_cs_type = cs_type_of_type ectx.gctx rest_elem_type in
							let rest_cs_args = List.map (fun arg ->
								let cs_arg = cs_expr_of_texpr ectx arg in
								coerce_arg ~in_scope:ectx.type_params_in_scope ectx.gctx cs_arg arg.etype rest_elem_type
							) rest_args in
							let native_array = CsNewArray (elem_cs_type, rest_cs_args) in
							let storage_type = classify_cs_array_element_type elem_cs_type in
							let target_array_type = haxe_array_type in
							make_array_from_native storage_type native_array target_array_type
					end
				in
				let all_args = regular_cs_args @ [rest_cs_arg] in
				(* For param types, use element type wrapped in Array for the rest param *)
				let regular_param_types = ExtList.List.take regular_param_count param_types_hx in
				let rest_array_type = ectx.gctx.com.basic.tarray rest_elem_type in
				let all_param_types_hx = regular_param_types @ [rest_array_type] in
				let param_types_cs = List.map (cs_type_of_type ectx.gctx) all_param_types_hx in
				(all_args, param_types_cs)
			| None ->
				(* No Rest parameter - process all args normally *)
				let args_cs = List.mapi (fun i arg ->
					let expected_hx_type = if i < List.length param_types_hx then
						List.nth param_types_hx i
					else
						arg.etype
					in
					let cs_arg = cs_expr_of_texpr ectx arg in
					coerce_arg ~in_scope:ectx.type_params_in_scope ectx.gctx cs_arg arg.etype expected_hx_type
				) args in
				let param_types_cs = List.map (cs_type_of_type ectx.gctx) param_types_hx in
				(args_cs, param_types_cs)
		in
		let result_type = cs_type_of_type ectx.gctx ret_type_hx in
		(* Register this signature for typed invoke generation *)
		register_invoke_signature ectx.gctx param_types_cs result_type;
		(* Use Value-based invoke to avoid boxing primitives *)
		let num_args = List.length args_cs in
		let hxvalue_args = generate_hxvalue_args args_cs param_types_cs in
		let call_expr = CsCall (CsField (closure, hxvalue_invoke_method_name num_args), hxvalue_args) in
		(* Extract the return value from Value *)
		cast_value_to_type result_type call_expr
	| TCall ({ eexpr = TField (_, FEnum (en, ef)) }, orig_args) ->
		(* Enum constructor with parameters -> new EnumType.ConstructorName(...)
		   After type erasure, both parent enum and constructor nested classes are non-generic.
		   Constructor parameters types are erased to object. *)
		let enum_path = cs_path_of_path en.e_path in
		let ctor_name = escape_identifier ef.ef_name in
		let parent_type = CsTypeClass (enum_path, []) in
		let nested_type = CsTypeNested (parent_type, ctor_name) in
		(* Get erased parameter types - with type erasure, type params become object *)
		let param_types = match follow ef.ef_type with
			| TFun (params, _) -> List.map (fun (_, _, t) -> t) params
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
		   - Method MethDynamic (like `public dynamic function onAbort(...)`) which are also stored functions
		   - Generic fields where the actual type (with applied type params) is a function *)
		let actual_field_type = apply_params c.cl_params tl cf.cf_type in
		let is_stored_function_field = match cf.cf_kind with
			| Var _ -> (match follow actual_field_type with TFun _ | TDynamic _ -> true | _ -> false)
			| Method MethDynamic -> true  (* dynamic methods are stored as function fields *)
			| Method _ -> false
		in
		if is_stored_function_field then begin
			(* This is calling a function stored in a field - use typed invoke methods *)
			let needs_unwrap = find_null_in_expr e_obj in
			let obj = cs_expr_of_texpr ectx e_obj in
			let obj = if needs_unwrap then CsField (obj, "value") else obj in
			(* Check if the object expression returns an erased type param (e.g., Type.createInstance) *)
			let obj = if expr_returns_erased_type_param e_obj then
				let target_cs_type = cs_type_of_type ectx.gctx e_obj.etype in
				CsCast (target_cs_type, obj)
			else obj in
			let func_expr = CsField (obj, get_native_field_name cf) in
			(* If the field's declared type is an erased type param, the field returns object in C#.
			   But we need to call __hx_invoke on it, so cast to haxe.lang.Function. *)
			let func_expr = if is_erased_type_param cf.cf_type then
				CsCast (CsTypeClass ((["haxe"; "lang"], "Function"), []), func_expr)
			else func_expr in
			(* Get parameter and return types for typed invoke.
			   Use actual_field_type (with type params substituted) to get concrete types. *)
			let param_types_hx, ret_type_hx = match follow actual_field_type with
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
			(* Handle Rest parameters - check if last param is Rest<T> *)
			let num_params = List.length param_types_hx in
			let last_param_rest = if num_params > 0 then
				get_rest_element_type (List.nth param_types_hx (num_params - 1))
			else
				None
			in
			let args_cs, param_types_cs = match last_param_rest with
				| Some rest_elem_type ->
					(* Last param is Rest<T> - split args into regular and rest parts *)
					let regular_param_count = num_params - 1 in
					let regular_args = ExtList.List.take regular_param_count args in
					let rest_args = ExtList.List.drop regular_param_count args in
					(* Generate regular args *)
					let regular_cs_args = List.mapi (fun i arg ->
						let expected_type = List.nth param_types_hx i in
						let cs_arg = cs_expr_of_texpr ectx arg in
						coerce_arg ~in_scope:ectx.type_params_in_scope ectx.gctx cs_arg arg.etype expected_type
					) regular_args in
					(* Generate rest args - wrap into Array *)
					let rest_cs_arg =
						if rest_args = [] then
							CsNew (haxe_array_type, [])
						else begin
							match (List.hd rest_args).eexpr with
							| TUnop (Spread, _, spread_expr) ->
								cs_expr_of_texpr ectx spread_expr
							| _ ->
								let elem_cs_type = cs_type_of_type ectx.gctx rest_elem_type in
								let rest_cs_args = List.map (fun arg ->
									let cs_arg = cs_expr_of_texpr ectx arg in
									coerce_arg ~in_scope:ectx.type_params_in_scope ectx.gctx cs_arg arg.etype rest_elem_type
								) rest_args in
								let native_array = CsNewArray (elem_cs_type, rest_cs_args) in
								let storage_type = classify_cs_array_element_type elem_cs_type in
								let target_array_type = haxe_array_type in
								make_array_from_native storage_type native_array target_array_type
						end
					in
					let all_args = regular_cs_args @ [rest_cs_arg] in
					let regular_param_types = ExtList.List.take regular_param_count param_types_hx in
					let rest_array_type = ectx.gctx.com.basic.tarray rest_elem_type in
					let all_param_types_hx = regular_param_types @ [rest_array_type] in
					let param_types_cs = List.map (cs_type_of_type ectx.gctx) all_param_types_hx in
					(all_args, param_types_cs)
				| None ->
					let args_cs = List.mapi (fun i arg ->
						let expected_hx_type = if i < List.length param_types_hx then
							List.nth param_types_hx i
						else
							arg.etype
						in
						let cs_arg = cs_expr_of_texpr ectx arg in
						coerce_arg ~in_scope:ectx.type_params_in_scope ectx.gctx cs_arg arg.etype expected_hx_type
					) args in
					let param_types_cs = List.map (cs_type_of_type ectx.gctx) param_types_hx in
					(args_cs, param_types_cs)
			in
			let result_type = cs_type_of_type ectx.gctx ret_type_hx in
			(* Register this signature for typed invoke generation *)
			register_invoke_signature ectx.gctx param_types_cs result_type;
			(* Use Value-based invoke to avoid boxing primitives *)
			let num_args = List.length args_cs in
			let hxvalue_args = generate_hxvalue_args args_cs param_types_cs in
			let call_expr = CsCall (CsField (func_expr, hxvalue_invoke_method_name num_args), hxvalue_args) in
			(* Extract the return value from Value *)
			cast_value_to_type result_type call_expr
		end else begin
		(* Check if expression type is Null<T> - if so, access .value to unwrap.
		   Note: @:forward on Null<T> sets e.etype to the underlying type (after forward),
		   but for TLocal the v.v_type still has the Null wrapper.
		   CRITICAL: For TCast, check the TARGET type (e.etype), not the inner type.
		   This is because TCast changes the C# type - if we cast to non-Null, no .value needed. *)
		let needs_unwrap = find_null_in_expr e_obj in
		let obj = cs_expr_of_texpr ectx e_obj in
		let obj = if needs_unwrap then CsField (obj, "value") else obj in
		(* Check if the C# type is object (due to erasure of generic interface with Dynamic).
		   In that case, we can't do direct method calls - use Reflect.field + Runtime.InvokeDelegate. *)
		let obj_cs_type = cs_type_of_type ectx.gctx e_obj.etype in
		if obj_cs_type = CsTypeObject then begin
			(* Type was erased to object - use reflection for method call *)
			let field_call = CsStaticCall (reflect_type, "field", [obj; CsConst (CsConstString cf.cf_name)]) in
			(* Build args array *)
			let cs_args = List.map (cs_expr_of_texpr ectx) args in
			let native_array = CsNewArray (CsTypeObject, cs_args) in
			let args_array = make_array_from_native ArrayDynamic native_array (haxe_array_type) in
			(* Call Runtime.InvokeDelegate(method, args) *)
			let invoke_call = CsStaticCall (runtime_type, "InvokeDelegate", [field_call; args_array]) in
			(* Cast result to expected type - use cast_object_to_type for proper unboxing *)
			let result_type = cs_type_of_type ectx.gctx e.etype in
			begin match result_type with
			| CsTypeVoid -> invoke_call
			| CsTypeObject -> invoke_call
			| _ -> cast_object_to_type result_type invoke_call
			end
		end else begin
		(* Get parameter types for argument coercion.
		   Apply class type params to get concrete types for generic methods like Array<T>.push(T).
		   IMPORTANT: When a parameter is optional (opt=true), wrap its type in Null<T>.
		   In Haxe's TFun, optional params have opt=true but the type itself is NOT wrapped.
		   We need to wrap it for C# where optional params use Null<T>.
		   BUT: Don't double-wrap if the type is already Null<T>.

		   CRITICAL: For override methods, use the PARENT's DECLARED parameter types (unmapped)
		   to match the generated C# method signature. With type erasure, type parameters
		   become object in C#. If we map K→String before converting to C# type, we get
		   string instead of object, causing signature mismatch.
		   Example: Parent<K>.put(K) → C# put(object); Child2 extends Parent<String>
		   should override with put(object), not put(string). *)
		let get_parent_param_types c_class tl_class cf_method =
			let rec find_parent_types c_super tl =
				let map_type = apply_params c_super.cl_params tl in
				try
					let cf_super = PMap.find cf_method.cf_name c_super.cl_fields in
					match cf_super.cf_kind with
					| Method _ ->
						(* Use DECLARED type without mapping - type params will be erased to object *)
						begin match follow cf_super.cf_type with
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
				(* Use parent's DECLARED param types directly - don't map type params.
				   Type params will be erased to object by cs_type_of_type. *)
				List.map (fun (_, opt, t) ->
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
		(* For generic methods on C# native classes, we need to provide explicit type arguments.
		   For Haxe classes, method type params are erased, so we don't need generic calls. *)
		if cf.cf_params <> [] && CsTypeMapping.is_cs_native_generic_class c.cl_path then begin
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
						(* Direct type parameter match - unwrap Null<T> if present.
						   Note: Null<Null<T>> is already flattened by csSignature.ml *)
						let unwrapped = match follow arg_t with
							| TAbstract ({ a_path = ([], "Null") }, [inner]) -> inner
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
					| TEnum (e1, tp1_list), TEnum (e2, tp2_list) when e1.e_path = e2.e_path && List.length tp1_list = List.length tp2_list ->
						(* Generic enum like GADT<A> matched with GADT<Int> - look inside type params.
						   This is critical for GADT inference where constructor's return type encodes the type param. *)
						List.fold_left2 (fun acc tp1 tp2 ->
							match acc with
							| Some _ -> acc
							| None -> find_type_param_in_type ttp_name tp1 tp2
						) None tp1_list tp2_list
					| TInst (c1, tp1_list), TInst (c2, _) when has_class_flag c1 CInterface && tp1_list <> [] ->
						(* Interface with type params (like IReport<T>) matched with a class (like PlainTextReport).
						   Check if the class implements this interface and extract the type params. *)
						let rec find_interface_impl c =
							(* Check direct interface implementations *)
							let from_direct = List.fold_left (fun acc (iface, iface_params) ->
								match acc with
								| Some _ -> acc
								| None when iface.cl_path = c1.cl_path && List.length iface_params = List.length tp1_list ->
									(* Found matching interface - match type params *)
									List.fold_left2 (fun acc tp1 tp2 ->
										match acc with
										| Some _ -> acc
										| None -> find_type_param_in_type ttp_name tp1 tp2
									) None tp1_list iface_params
								| None -> None
							) None c.cl_implements in
							begin match from_direct with
							| Some _ -> from_direct
							| None ->
								(* Check superclass *)
								match c.cl_super with
								| Some (sc, _) -> find_interface_impl sc
								| None -> None
							end
						in
						find_interface_impl c2
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
					(* Try to find the best type for this type param, preferring non-Dynamic types.
					   We look through all param/arg pairs and prefer specific types over Dynamic. *)
					let found_type = List.fold_left (fun acc (param_t, arg_t) ->
						let this_match = find_type_param_in_type ttp.ttp_name param_t arg_t in
						match acc, this_match with
						| None, _ -> this_match  (* First match *)
						| Some prev, Some curr ->
							(* Prefer non-Dynamic over Dynamic *)
							if is_haxe_dynamic_type prev && not (is_haxe_dynamic_type curr) then Some curr
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
						(* For Null<T> return types, extract T.
						   Note: Null<Null<T>> is already flattened by csSignature.ml *)
						[follow inner]
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
			(* Convert type params to C# types, respecting constraints.
			   If the inferred type would be object but the type param has a constraint,
			   use the constraint bound instead (C# requires type args satisfy constraints).
			   Also substitute void with object (C# doesn't allow void as type arg). *)
			(* Build a mapping from type param names to their inferred Haxe types first.
			   This is needed for constraints that reference other type params like O:T. *)
			let param_name_to_hx = List.map2 (fun hx_t ttp -> (ttp.ttp_name, hx_t)) method_type_params_hx cf.cf_params in
			let method_type_params = List.map2 (fun hx_type ttp ->
				let cs_type = cs_type_of_type ectx.gctx hx_type in
				match cs_type with
				| CsTypeObject ->
					(* Check if the type param has a constraint that object wouldn't satisfy.
					   For intersection constraints like O:{} & T, we get multiple constraint types.
					   We need to check ALL of them, not just the first one. *)
					let constraints = TFunctions.get_constraints ttp in
					(* Try to find a type param reference in any constraint *)
					let rec find_type_param_ref t = match follow t with
						| TInst ({ cl_kind = KTypeParameter _ } as c, _) ->
							let tp_name = snd c.cl_path in
							begin match List.assoc_opt tp_name param_name_to_hx with
							| Some hx_t -> Some (cs_type_of_type ectx.gctx hx_t)
							| None -> None
							end
						| TAnon _ -> None  (* Structural constraint like {} - skip *)
						| _ ->
							let constraint_cs = cs_type_of_type ectx.gctx t in
							if constraint_cs <> CsTypeObject then Some constraint_cs else None
					in
					(* Search through all constraints for a usable type *)
					let result = List.fold_left (fun acc constraint_t ->
						match acc with
						| Some _ -> acc  (* Already found a good type *)
						| None -> find_type_param_ref constraint_t
					) None constraints in
					begin match result with
					| Some cs_t -> cs_t
					| None -> cs_type
					end
				| CsTypeVoid -> CsTypeObject
				| _ -> cs_type
			) method_type_params_hx cf.cf_params in
			(* Erase type params that are not in scope at the C# level.
			   This handles GADT phantom types like C in EBinop<C> which are
			   introduced during pattern matching but don't exist as C# generic params. *)
			let method_type_params = List.map (CsTypeMapping.erase_out_of_scope_type_params ectx.type_params_in_scope) method_type_params in
			CsCallGeneric (CsField (obj, get_native_field_name cf), method_type_params, cs_args)
		end else begin
			let cs_args = generate_call_args ectx cs_expr_of_texpr args param_types_base in
			(* For Array methods, use typed accessor methods when the element type is known.
			   This avoids boxing/unboxing overhead. Methods like pop(), shift() become
			   __popInt(), __shiftInt() etc. based on element type.

			   IMPORTANT: Nullable types like Null<Int> must use ArrayObject because
			   primitive arrays cannot hold null values. *)
			begin match c.cl_path with
			| ([], "Array") | (["haxe"; "root"], "Array") ->
				(* Get the element type from the Array type parameters.
				   tl contains the applied type params, e.g., [Int] for Array<Int>.
				   Use follow_without_null to preserve Null<> wrappers. *)
				let storage_type = match tl with
					| [elem] ->
						begin match Type.follow_without_null elem with
						(* Nullable primitives must use object storage *)
						| TAbstract ({ a_path = ([], "Null") }, [inner]) ->
							begin match follow inner with
							| TAbstract ({ a_path = ([], "Int") }, _)
							| TAbstract ({ a_path = ([], "Float") }, _)
							| TAbstract ({ a_path = ([], "Bool") }, _) -> ArrayObject
							| _ -> ArrayObject
							end
						(* Non-nullable primitives get typed storage *)
						| TAbstract ({ a_path = ([], "Int") }, _) -> ArrayInt
						| TAbstract ({ a_path = ([], "Float") }, _) -> ArrayFloat
						| TAbstract ({ a_path = ([], "Bool") }, _) -> ArrayBool
						| TDynamic _ -> ArrayDynamic
						| TAbstract ({ a_path = ([], "Any") }, _) -> ArrayDynamic
						| TInst ({ cl_kind = KTypeParameter _ }, _) -> ArrayDynamic
						| _ -> ArrayObject
						end
					| _ -> ArrayDynamic
				in
				let method_name = get_native_field_name cf in
				(* Check if this method has a typed variant *)
				let typed_method_name = match method_name, storage_type with
					| "pop", ArrayInt -> Some "__popInt"
					| "pop", ArrayFloat -> Some "__popFloat"
					| "pop", ArrayBool -> Some "__popBool"
					| "shift", ArrayInt -> Some "__shiftInt"
					| "shift", ArrayFloat -> Some "__shiftFloat"
					| "shift", ArrayBool -> Some "__shiftBool"
					| "push", ArrayInt -> Some "__pushInt"
					| "push", ArrayFloat -> Some "__pushFloat"
					| "push", ArrayBool -> Some "__pushBool"
					| "push", ArrayDynamic -> Some "__pushDyn"
					| "unshift", ArrayInt -> Some "__unshiftInt"
					| "unshift", ArrayFloat -> Some "__unshiftFloat"
					| "unshift", ArrayBool -> Some "__unshiftBool"
					| "unshift", ArrayDynamic -> Some "__unshiftDyn"
					| "insert", ArrayInt -> Some "__insertInt"
					| "insert", ArrayFloat -> Some "__insertFloat"
					| "insert", ArrayBool -> Some "__insertBool"
					| "insert", ArrayDynamic -> Some "__insertDyn"
					| _ -> None
				in
				begin match typed_method_name with
				| Some typed_name ->
					(* Use typed method - no conversion needed *)
					CsCall (CsField (obj, typed_name), cs_args)
				| None ->
					(* Fall back to generic method with conversion if needed *)
					let call_expr = CsCall (CsField (obj, method_name), cs_args) in
					let expected_cs_type = cs_type_of_type ectx.gctx e.etype in
					begin match expected_cs_type with
					| CsTypeObject | CsTypeDynamic | CsTypeVoid ->
						(* No conversion needed *)
						call_expr
					| CsTypeClass ((["haxe"; "lang"], "Null"), [inner]) when not (CsTypeMapping.is_inherently_nullable inner) ->
						(* Null<T> where T is a value type - use Null<T>._ofDynamic(result) *)
						CsStaticCall (expected_cs_type, "_ofDynamic", [call_expr])
					| _ when CsTypeMapping.is_inherently_nullable expected_cs_type ->
						(* Reference type - simple cast *)
						CsCast (expected_cs_type, call_expr)
					| _ ->
						(* Value type - use cast_object_to_type for proper unboxing *)
						cast_object_to_type expected_cs_type call_expr
					end
				end
			| _ ->
				(* Generic Haxe class method call.
				   With type erasure, methods that return type parameters (like IntMap<T>.get():T)
				   now return object in C#, but Haxe expects the specific type.
				   Check if the method's declared return type involves type parameters and cast if needed. *)
				let call_expr = CsCall (CsField (obj, get_native_field_name cf), cs_args) in
				let method_returns_type_param = match follow cf.cf_type with
					| TFun (_, ret) ->
						(* Check if return type involves type parameters, tracking visited types to avoid cycles *)
						let visited = ref [] in
						let rec involves_type_param t =
							let t = follow t in
							(* Check if we've seen this type before (cycle detection) *)
							if List.memq t !visited then false
							else begin
								visited := t :: !visited;
								match t with
								| TInst ({ cl_kind = KTypeParameter _ }, _) -> true
								| TInst (_, tl) | TEnum (_, tl) | TAbstract (_, tl) | TType (_, tl) ->
									List.exists involves_type_param tl
								| TFun (args, ret) ->
									involves_type_param ret || List.exists (fun (_, _, t) -> involves_type_param t) args
								| TAnon a ->
									PMap.fold (fun f acc -> acc || involves_type_param f.cf_type) a.a_fields false
								| _ -> false
							end
						in
						involves_type_param ret
					| _ -> false
				in
				if method_returns_type_param && not (CsTypeMapping.is_cs_native_generic_class c.cl_path) then begin
					(* Method returns a type param that got erased to object - cast to expected type.
					   Use cast_object_to_type for primitives to handle boxed type mismatches. *)
					let expected_cs_type = cs_type_of_type ectx.gctx e.etype in
					match expected_cs_type with
					| CsTypeObject | CsTypeDynamic | CsTypeVoid -> call_expr
					| CsTypeClass ((["haxe"; "lang"], "Null"), [inner]) when not (CsTypeMapping.is_inherently_nullable inner) ->
						(* Null<T> where T is a value type - use Null<T>._ofDynamic(result) *)
						CsStaticCall (expected_cs_type, "_ofDynamic", [call_expr])
					| _ -> cast_object_to_type expected_cs_type call_expr
				end else
					call_expr
			end
		end
		end  (* close the obj_cs_type check *)
		end  (* close the is_var_with_func_type else branch *)
	| TCall ({ eexpr = TField (e_obj, FAnon cf) }, args) ->
		(* Method call on anonymous/structural type.
		   First, check if expression type is Null<T> - if so, unwrap via .value *)
		(* NOTE: Use follow_once to peel through TMono but not unwrap Null<T> *)
		(* NOTE: e_obj is the object expression, e is the whole TCall expression (outer match var) *)
		let obj = cs_expr_of_texpr ectx e_obj in
		let raw_type = Type.follow_once e_obj.etype in
		(* Only unwrap .value if the inner type actually needs the Null wrapper in C#.
		   For reference types (classes, anonymous types/object), Null<T> is stripped to T,
		   so there's no .value to access - the variable holds the value directly. *)
		let inner_type, obj = match raw_type with
			| TAbstract ({ a_path = ([], "Null") }, [inner]) ->
				let inner_cs = cs_type_of_type ectx.gctx inner in
				if CsTypeMapping.is_inherently_nullable inner_cs then
					(inner, obj)  (* No .value - Null<T> stripped to T in C# *)
				else
					(inner, CsField (obj, "value"))  (* Null<T> -> access .value to unwrap *)
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
				let args_array = make_invoke_args_array args in
				let call_expr = CsStaticCall (runtime_type, "InvokeDelegate", [field_call; args_array]) in
				(* Cast the result to the expected return type.
				   Use e.etype (the TCall's return type) which has type parameters resolved,
				   rather than cf.cf_type which might have unresolved type params. *)
				let result_type = cs_type_of_type ectx.gctx e.etype in
				(* Erase out-of-scope type params to avoid CS0246 errors *)
				let result_type = CsTypeMapping.erase_out_of_scope_type_params ectx.type_params_in_scope result_type in
				begin match result_type with
				| CsTypeVoid | CsTypeObject | CsTypeDynamic -> call_expr
				(* Use cast_object_to_type for proper unboxing of InvokeDelegate results *)
				| _ -> cast_object_to_type result_type call_expr
				end
			end else begin
				(* Non-function field - just get and cast *)
				let func_type = CsTypeMapping.erase_type_params (cs_type_of_type ectx.gctx cf.cf_type) in
				CsCast (func_type, field_call)
			end
		| CsTypeClass (path, _) ->
			(* Some other concrete class - try direct call *)
			CsCall (CsField (obj, escape_identifier cf.cf_name), args)
		| CsTypeObject ->
			(* Object type (from TAnon/structural type) - use Reflect.field then Runtime.InvokeDelegate *)
			let field_call = CsStaticCall (reflect_type, "field", [obj; CsConst (CsConstString cf.cf_name)]) in
			let args_array = make_invoke_args_array args in
			let call_expr = CsStaticCall (runtime_type, "InvokeDelegate", [field_call; args_array]) in
			(* Convert the result to the expected return type using Runtime helpers.
			   Use Runtime.toInt/toDouble/toBool for primitives (handles boxing/unboxing properly),
			   and direct cast for reference types.
			   Use e.etype (the TCall's return type) which has type parameters resolved. *)
			let result_type = cs_type_of_type ectx.gctx e.etype in
			(* Erase out-of-scope type params to avoid CS0246 errors *)
			let result_type = CsTypeMapping.erase_out_of_scope_type_params ectx.type_params_in_scope result_type in
			cast_invoke_result result_type call_expr
		| CsTypeGenericParam _ ->
			(* Type parameter - cast to HaxeObject to call _hx_getField *)
			let casted_obj = CsCast (haxe_object_type, obj) in
			let field_call = CsCall (CsField (casted_obj, "_hx_getField"), [CsConst (CsConstString cf.cf_name)]) in
			let args_array = make_invoke_args_array args in
			let call_expr = CsStaticCall (runtime_type, "InvokeDelegate", [field_call; args_array]) in
			let result_type = cs_type_of_type ectx.gctx e.etype in
			(* Erase out-of-scope type params to avoid CS0246 errors *)
			let result_type = CsTypeMapping.erase_out_of_scope_type_params ectx.type_params_in_scope result_type in
			cast_invoke_result result_type call_expr
		| _ ->
			(* Fallback to dynamic dispatch via _hx_getField -> Runtime.InvokeDelegate *)
			let field_call = CsCall (CsField (obj, "_hx_getField"), [CsConst (CsConstString cf.cf_name)]) in
			let args_array = make_invoke_args_array args in
			let call_expr = CsStaticCall (runtime_type, "InvokeDelegate", [field_call; args_array]) in
			(* Convert the result using Runtime helpers for proper type conversion *)
			let result_type = cs_type_of_type ectx.gctx e.etype in
			(* Erase out-of-scope type params to avoid CS0246 errors *)
			let result_type = CsTypeMapping.erase_out_of_scope_type_params ectx.type_params_in_scope result_type in
			cast_invoke_result result_type call_expr
		end
	| TCall ({ eexpr = TField (e_obj, FDynamic name) }, args) when name = "value" || name = "hasValue" ->
		(* Special case: calling .value or .hasValue on Null<T> - this is csNullable's unwrap pattern.
		   Generate direct field access and invoke, not Runtime.GetField. *)
		let obj = cs_expr_of_texpr ectx e_obj in
		let func_expr = CsField (obj, name) in
		let args_exprs = List.map (cs_expr_of_texpr ectx) args in
		let args_array = make_invoke_args_array args_exprs in
		let call_expr = CsStaticCall (runtime_type, "InvokeDelegate", [func_expr; args_array]) in
		let result_type = cs_type_of_type ectx.gctx e.etype in
		let result_type = CsTypeMapping.erase_out_of_scope_type_params ectx.type_params_in_scope result_type in
		begin match result_type with
		| CsTypeVoid -> call_expr  (* Don't cast void results *)
		| CsTypeObject | CsTypeDynamic -> call_expr
		(* Use cast_object_to_type for proper unboxing of InvokeDelegate results *)
		| _ -> cast_object_to_type result_type call_expr
		end
	| TCall ({ eexpr = TField (e_obj, FDynamic name) }, args) ->
		(* Dynamic method call: obj.dynamicMethod(args) -> Runtime.InvokeDelegate(Runtime.GetField(obj, "method"), args) *)
		let obj = cs_expr_of_texpr ectx e_obj in
		let raw_type = Type.follow_once e_obj.etype in
		(* Only unwrap .value if the inner type actually needs the Null wrapper in C#.
		   For reference types (classes, anonymous types/object), Null<T> is stripped to T,
		   so there's no .value to access - the variable holds the value directly. *)
		let obj = match raw_type with
			| TAbstract ({ a_path = ([], "Null") }, [inner_t]) ->
				let inner_cs = cs_type_of_type ectx.gctx inner_t in
				if CsTypeMapping.is_inherently_nullable inner_cs then
					obj  (* No .value - Null<T> stripped to T in C# *)
				else
					CsField (obj, "value")
			| TAbstract ({ a_path = ([], "Null") }, _) ->
				CsField (obj, "value")  (* Null with no/multiple params - treat as nullable *)
			| _ -> obj
		in
		let get_field = CsStaticCall (runtime_type, "GetField", [obj; CsConst (CsConstString name)]) in
		let args_exprs = List.map (cs_expr_of_texpr ectx) args in
		let args_array = make_invoke_args_array args_exprs in
		let call_expr = CsStaticCall (runtime_type, "InvokeDelegate", [get_field; args_array]) in
		(* Cast the result to the expected return type - use e.etype (the TCall's type), not e_obj.etype *)
		(* Also erase out-of-scope type params *)
		let result_type = cs_type_of_type ectx.gctx e.etype in
		let result_type = CsTypeMapping.erase_out_of_scope_type_params ectx.type_params_in_scope result_type in
		begin match result_type with
		| CsTypeObject | CsTypeDynamic -> call_expr  (* No cast needed for Dynamic/object *)
		(* Use cast_object_to_type for proper unboxing of InvokeDelegate results *)
		| _ -> cast_object_to_type result_type call_expr
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
		   If so, we need to use Runtime.InvokeDelegate instead of direct call.
		   Also check for @:callable abstracts - they wrap a function type but follow() doesn't unwrap them. *)
		let rec is_function_like_type t = match follow t with
			| TFun _ | TDynamic _ -> true
			| TAbstract (a, _) ->
				(* Check if abstract has @:callable and wraps a function type *)
				if Meta.has Meta.Callable a.a_meta then
					is_function_like_type a.a_this
				else
					false
			| _ -> false
		in
		let is_stored_function_field = match cf.cf_kind with
			| Var _ -> is_function_like_type cf.cf_type
			| Method MethDynamic -> true
			| Method _ -> false
		in
		if is_stored_function_field then begin
			(* Stored function field - use Runtime.InvokeDelegate *)
			let path = cs_path_of_path c.cl_path in
			(* Haxe classes are non-generic in C#, so no type params *)
			ignore c.cl_params;
			let func_expr = CsStaticField (CsTypeClass (path, []), escape_identifier cf.cf_name) in
			let args_exprs = List.map (cs_expr_of_texpr ectx) orig_args in
			let args_array = make_invoke_args_array args_exprs in
			let call_expr = CsStaticCall (runtime_type, "InvokeDelegate", [func_expr; args_array]) in
			let result_type = cs_type_of_type ectx.gctx return_type in
			begin match result_type with
			| CsTypeVoid | CsTypeObject | CsTypeDynamic -> call_expr
			(* Use cast_object_to_type for proper unboxing of InvokeDelegate results *)
			| _ -> cast_object_to_type result_type call_expr
			end
		end else begin
		(* Special handling for String static methods - redirect to StringExt *)
		let path, class_type_params = match c.cl_path with
		| ([], ("String" | "string")) | (["haxe"; "root"], ("String" | "string")) ->
			(* String static methods like fromCharCode are in cs.StringExt *)
			((["cs"], "StringExt"), [])
		| ([], "Array") | (["haxe"; "root"], "Array") ->
			(* Array is non-generic in C# output *)
			(cs_path_of_path c.cl_path, [])
		| _ ->
			let path = cs_path_of_path c.cl_path in
			(* Haxe classes are non-generic in C#, so no type params *)
			ignore c.cl_params;
			(path, [])
		in
		(* Get base parameter types from signature.
		   IMPORTANT: Wrap optional params in Null<T> (opt=true means optional).
		   BUT: Don't double-wrap if the type is already Null<T>.
		   NOTE: Don't use follow() on cf.cf_type - it can break physical equality
		   of type parameters that apply_params relies on. *)
		let param_types_base = match cf.cf_type with
			| TFun (params, _) ->
				List.map (fun (_, opt, t) ->
					let is_already_null = match follow t with
						| TAbstract ({ a_path = ([], "Null") }, _) -> true
						| _ -> false
					in
					if opt && not is_already_null then ectx.gctx.com.basic.tnull t else t
				) params
			| TLazy f ->
				(* Handle lazy types *)
				begin match lazy_type f with
				| TFun (params, _) ->
					List.map (fun (_, opt, t) ->
						let is_already_null = match follow t with
							| TAbstract ({ a_path = ([], "Null") }, _) -> true
							| _ -> false
						in
						if opt && not is_already_null then ectx.gctx.com.basic.tnull t else t
					) params
				| _ -> []
				end
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
		let all_signature_type_params = CsTypeMapping.get_method_type_params orig_param_types orig_ret_type in
		(* Filter out class type params from inferred params *)
		let class_type_param_names = List.map (fun ttp -> ttp.ttp_name) c.cl_params in
		let inferred_type_params = List.filter (fun p ->
			not (List.mem p class_type_param_names) && not (List.mem p explicit_type_params)
		) all_signature_type_params in
		(* Combine explicit and inferred params *)
		let all_method_type_params = explicit_type_params @ inferred_type_params in
		(* Check if method has any type parameters (explicit or inferred).
		   For Haxe classes, method type params are erased, so we don't need generic calls. *)
		if all_method_type_params <> [] && CsTypeMapping.is_cs_native_generic_class c.cl_path then begin
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
					(* Use follow_without_null for param_t to preserve Null<> wrappers.
					   The standard `follow` unwraps Null<T> to T, which breaks matching.
					   For arg_t, use regular follow since we want to match the actual argument type. *)
					let param_t_f = Type.follow_without_null param_t in
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
					| TEnum (e1, tp1_list), TEnum (e2, tp2_list) when e1.e_path = e2.e_path && List.length tp1_list = List.length tp2_list ->
						(* Generic enum like GADT<A> matched with GADT<Int> - look inside type params.
						   This is critical for GADT inference where constructor's return type encodes the type param. *)
						List.fold_left2 (fun acc tp1 tp2 ->
							match acc with
							| Some _ -> acc
							| None -> find_type_param_in_type ttp_name tp1 tp2
						) None tp1_list tp2_list
					| TInst (c1, tp1_list), TInst (c2, _) when has_class_flag c1 CInterface && tp1_list <> [] ->
						(* Interface with type params (like IReport<T>) matched with a class (like PlainTextReport).
						   Check if the class implements this interface and extract the type params. *)
						let rec find_interface_impl c =
							(* Check direct interface implementations *)
							let from_direct = List.fold_left (fun acc (iface, iface_params) ->
								match acc with
								| Some _ -> acc
								| None when iface.cl_path = c1.cl_path && List.length iface_params = List.length tp1_list ->
									(* Found matching interface - match type params *)
									List.fold_left2 (fun acc tp1 tp2 ->
										match acc with
										| Some _ -> acc
										| None -> find_type_param_in_type ttp_name tp1 tp2
									) None tp1_list iface_params
								| None -> None
							) None c.cl_implements in
							begin match from_direct with
							| Some _ -> from_direct
							| None ->
								(* Check superclass *)
								match c.cl_super with
								| Some (sc, _) -> find_interface_impl sc
								| None -> None
							end
						in
						find_interface_impl c2
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
					| TAbstract (a1, tp1_list), TAbstract (a2, _) when a1.a_path <> a2.a_path ->
						(* Different abstracts - try following arg_t through to underlying type.
						   This handles cases like param_t = Null<T> and arg_t = M<A> where M(Null<T>). *)
						let arg_underlying = Abstract.follow_with_abstracts_without_null arg_t in
						begin match arg_underlying with
						| TAbstract (a3, tp3_list) when a3.a_path = a1.a_path && List.length tp3_list = List.length tp1_list ->
							(* arg's underlying matches param's abstract - match type params *)
							List.fold_left2 (fun acc tp1 tp3 ->
								match acc with
								| Some _ -> acc
								| None -> find_type_param_in_type ttp_name tp1 tp3
							) None tp1_list tp3_list
						| _ -> None
						end
					| TAbstract (a1, tp1_list), TFun _ when tp1_list <> [] ->
						(* param is an abstract like LazyGenerator<Data, End>, arg followed to a function type.
						   This happens because follow() follows abstracts to their underlying types.
						   Match through the abstract's underlying type (a_this with type params applied). *)
						let param_underlying = apply_params a1.a_params tp1_list (Abstract.get_underlying_type a1 tp1_list) in
						begin match follow param_underlying with
						| TFun (p1_list, r1) ->
							(* Now both are function types - match their components *)
							let p2_list, r2 = match arg_t_f with TFun (p, r) -> p, r | _ -> [], t_dynamic in
							if List.length p1_list = List.length p2_list then begin
								let from_params = List.fold_left2 (fun acc (_, _, t1) (_, _, t2) ->
									match acc with
									| Some _ -> acc
									| None -> find_type_param_in_type ttp_name t1 t2
								) None p1_list p2_list in
								match from_params with
								| Some _ -> from_params
								| None -> find_type_param_in_type ttp_name r1 r2
							end else None
						| _ -> None
						end
					| TFun (p1_list, r1), TAbstract (a2, tp2_list) when tp2_list <> [] ->
						(* param is a function like ()->Either<Data, End>, arg is an abstract like LazyGenerator<Int, Int>.
						   Follow the abstract to its underlying function type and match.
						   This handles abstract _Impl_ method calls where this param has the underlying function type. *)
						let arg_underlying = apply_params a2.a_params tp2_list (Abstract.get_underlying_type a2 tp2_list) in
						begin match follow arg_underlying with
						| TFun (p2_list, r2) ->
							(* Now both are function types - match their components *)
							if List.length p1_list = List.length p2_list then begin
								let from_params = List.fold_left2 (fun acc (_, _, t1) (_, _, t2) ->
									match acc with
									| Some _ -> acc
									| None -> find_type_param_in_type ttp_name t1 t2
								) None p1_list p2_list in
								match from_params with
								| Some _ -> from_params
								| None -> find_type_param_in_type ttp_name r1 r2
							end else None
						| _ -> None
						end
					| _ -> None
				in
				(* Iterate over ALL type params (explicit + inferred), using names *)
				List.map (fun ttp_name ->
					(* Try to find the best type for this type param, preferring non-Dynamic types. *)
					let found_type = List.fold_left (fun acc (param_t, arg_t) ->
						let this_match = find_type_param_in_type ttp_name param_t arg_t in
						match acc, this_match with
						| None, _ -> this_match  (* First match *)
						| Some prev, Some curr ->
							(* Prefer non-Dynamic over Dynamic *)
							if is_haxe_dynamic_type prev && not (is_haxe_dynamic_type curr) then Some curr
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
						(* For Null<T> return types, extract T.
						   Note: Null<Null<T>> is already flattened by csSignature.ml *)
						[follow inner]
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
			(* Convert method type params to C# types FIRST, so we can use them for substitution.
			   This needs to happen before generate_call_args because apply_params may fail
			   with abstract impl methods due to physical identity issues with type param instances. *)
			(* Build a mapping from type param names to their inferred Haxe types.
			   This is needed for constraints that reference other type params like O:T. *)
			let param_name_to_hx = List.combine all_method_type_params method_type_params_hx in
			let method_type_params_cs = List.map2 (fun hx_type param_name ->
				let cs_type = cs_type_of_type ectx.gctx hx_type in
				match cs_type with
				| CsTypeObject ->
					(* Look up the ttp record by name to get constraints *)
					let ttp_opt = List.find_opt (fun ttp -> ttp.ttp_name = param_name) cf.cf_params in
					begin match ttp_opt with
					| Some ttp ->
						let constraints = TFunctions.get_constraints ttp in
						begin match constraints with
						| first_constraint :: _ ->
							(* Check if constraint is a reference to another type param in this method.
							   If so, use that type param's inferred value. This handles O:T constraints. *)
							let rec find_type_param_ref t = match t with
								| TInst ({ cl_kind = KTypeParameter _ }, _) ->
									let tp_name = match t with
										| TInst (c, _) -> snd c.cl_path
										| _ -> ""
									in
									(* Look up if this type param was already inferred *)
									begin match List.assoc_opt tp_name param_name_to_hx with
									| Some hx_t -> cs_type_of_type ectx.gctx hx_t
									| None -> CsTypeObject
									end
								| TAnon _ ->
									(* Structural constraint like {} - ignore for type inference *)
									CsTypeObject
								| _ ->
									let constraint_cs = cs_type_of_type ectx.gctx first_constraint in
									if constraint_cs <> CsTypeObject then constraint_cs else cs_type
							in
							let result = find_type_param_ref first_constraint in
							if result <> CsTypeObject then result else cs_type
						| [] -> cs_type
						end
					| None -> cs_type
					end
				| CsTypeVoid -> CsTypeObject
				| _ -> cs_type
			) method_type_params_hx all_method_type_params in
			(* Apply method type params to parameter types - only for explicit params that have bindings.
			   NOTE: apply_params may not work correctly with abstract impl methods because
			   the type param instances in cf.cf_type may differ from those in cf.cf_params.
			   We apply Haxe-level substitution first, then apply C#-level substitution as a fallback
			   to handle any remaining unsubstituted type params. *)
			let method_param_map = apply_params cf.cf_params (ExtList.List.take (List.length cf.cf_params) method_type_params_hx) in
			let param_types = List.map method_param_map param_types_base in
			(* Build C#-level substitution map: param_name -> cs_type *)
			let cs_subst = List.combine all_method_type_params method_type_params_cs in
			(* Convert param types to C# and apply substitution to handle any remaining type params
			   that weren't substituted by apply_params due to physical identity issues *)
			let param_types_cs = List.map (fun t ->
				let cs_t = cs_type_of_type ectx.gctx t in
				CsTypeMapping.substitute_type_params cs_subst cs_t
			) param_types in
			(* Generate args using a custom version of generate_single_arg that uses the C#-substituted types.
			   We need to handle null args specially because the substituted C# type may differ from
			   what apply_params produced at the Haxe level. *)
			let generate_arg_with_cs_subst ectx cs_expr_of_texpr arg expected_hx_type expected_cs_type =
				let is_null_arg = match arg.eexpr with TConst TNull -> true | _ -> false in
				if is_null_arg then begin
					(* For null args, use the substituted C# type directly *)
					match expected_cs_type with
					| CsTypeClass ((["haxe"; "lang"], "Null"), _) ->
						let erased = CsTypeMapping.erase_out_of_scope_type_params ectx.type_params_in_scope expected_cs_type in
						CsDefault erased
					| CsTypeInt | CsTypeDouble | CsTypeBool | CsTypeLong | CsTypeFloat
					| CsTypeByte | CsTypeSByte | CsTypeChar | CsTypeShort | CsTypeUShort
					| CsTypeUInt | CsTypeULong | CsTypeDecimal ->
						CsDefault expected_cs_type
					| CsTypeGenericParam _ ->
						CsDefault expected_cs_type
					| _ ->
						CsNull
				end
				else begin
					let cs_arg = cs_expr_of_texpr ectx arg in
					(* GADT argument coercion: For TLocal variables, check if the ORIGINAL declared type
					   (v.v_type) differs from the expected type. *)
					match arg.eexpr with
					| TLocal v ->
						let var_cs_type = cs_type_of_type ectx.gctx v.v_type in
						begin match var_cs_type, expected_cs_type with
						| CsTypeClass (path1, params1), CsTypeClass (path2, params2)
							when path1 = path2 && params1 <> params2 ->
							let is_type_param = function CsTypeGenericParam _ -> true | _ -> false in
							let original_has_in_scope_type_params = List.exists (fun p ->
								match p with
								| CsTypeGenericParam name -> List.mem name ectx.type_params_in_scope
								| _ -> false
							) params1 in
							let expected_has_no_type_params = not (List.exists is_type_param params2) in
							if original_has_in_scope_type_params && expected_has_no_type_params then
								CsCast (expected_cs_type, cs_arg)
							else
								coerce_arg ~in_scope:ectx.type_params_in_scope ectx.gctx cs_arg arg.etype expected_hx_type
						| _ ->
							coerce_arg ~in_scope:ectx.type_params_in_scope ectx.gctx cs_arg arg.etype expected_hx_type
						end
					| _ ->
						coerce_arg ~in_scope:ectx.type_params_in_scope ectx.gctx cs_arg arg.etype expected_hx_type
				end
			in
			let args = List.mapi (fun i arg ->
				if i < List.length param_types then begin
					let expected_cs_type = List.nth param_types_cs i in
					let expected_hx_type = List.nth param_types i in
					generate_arg_with_cs_subst ectx cs_expr_of_texpr arg expected_hx_type expected_cs_type
				end
				else
					cs_expr_of_texpr ectx arg
			) orig_args in
			(* Erase type params that are not in scope at the C# level.
			   This handles GADT phantom types like C in EBinop<C> which are
			   introduced during pattern matching but don't exist as C# generic params. *)
			let method_type_params = List.map (CsTypeMapping.erase_out_of_scope_type_params ectx.type_params_in_scope) method_type_params_cs in
			let call_expr = CsStaticCallGeneric (CsTypeClass (path, class_type_params), escape_identifier cf.cf_name, method_type_params, args) in
			(* GADT type erasure fix: when Haxe return type is a type param T but the generated
			   call uses object (due to type inference from erased arguments), we need to cast
			   the result back to T. Otherwise C# sees "object" return but expects "T".

			   Example: evalBinop<T,C>(...):T called as evalBinop<object,object>(...) returns object,
			   but the Haxe expression type is T, so we need (T)evalBinop<object,object>(...) *)
			let is_return_type_param = match follow return_type with
				| TInst ({ cl_kind = KTypeParameter _ }, _) -> true
				| _ -> false
			in
			if is_return_type_param then begin
				(* Find which type param position corresponds to the return type *)
				let ret_param_name = match follow return_type with
					| TInst ({ cl_kind = KTypeParameter ttp }, _) -> ttp.ttp_name
					| _ -> ""
				in
				(* Find the position of this type param in method params *)
				let rec find_pos name idx = function
					| [] -> -1
					| ttp :: rest ->
						if ttp.ttp_name = name then idx else find_pos name (idx + 1) rest
				in
				let pos = find_pos ret_param_name 0 cf.cf_params in
				(* Check if that position was instantiated to object *)
				let instantiated_to_object =
					pos >= 0 && pos < List.length method_type_params &&
					List.nth method_type_params pos = CsTypeObject
				in
				if instantiated_to_object then
					(* Cast result from object to T *)
					CsCast (CsTypeGenericParam ret_param_name, call_expr)
				else
					call_expr
			end else
				call_expr
		end else begin
			let args = generate_call_args ectx cs_expr_of_texpr orig_args param_types_base in
			let call_expr = CsStaticCall (CsTypeClass (path, class_type_params), escape_identifier cf.cf_name, args) in
			(* Check if the method's return type is an erased type param.
			   If so, the C# method returns object but call site expects a concrete type. *)
			let method_ret_type = match cf.cf_type with
				| TFun (_, ret) -> ret
				| _ -> return_type
			in
			if is_erased_type_param method_ret_type then
				let result_cs_type = cs_type_of_type ectx.gctx return_type in
				begin match result_cs_type with
				| CsTypeObject | CsTypeDynamic -> call_expr
				| _ -> cast_object_to_type result_cs_type call_expr
				end
			else
				call_expr
		end
		end  (* close is_stored_function_field else branch *)
	| TCall ({ eexpr = TIdent "__default__" }, []) ->
		(* Generated by CsNullable for default(Null<T>) *)
		let cs_type = cs_type_of_type ectx.gctx e.etype in
		CsDefault cs_type
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
			let args_array = make_invoke_args_array args_exprs in
			let call_expr = CsStaticCall (runtime_type, "InvokeDelegate", [func_expr; args_array]) in
			(* Cast the result to the expected return type *)
			let result_type = cs_type_of_type ectx.gctx e.etype in
			(* Erase out-of-scope type params to avoid CS0246 errors *)
			let result_type = CsTypeMapping.erase_out_of_scope_type_params ectx.type_params_in_scope result_type in
			begin match result_type with
			| CsTypeVoid | CsTypeObject | CsTypeDynamic -> call_expr  (* No cast needed for void/Dynamic/object *)
			(* Use cast_object_to_type for proper unboxing of InvokeDelegate results *)
			| _ -> cast_object_to_type result_type call_expr
			end
		end else begin
			(* NOTE: Future optimization opportunity - if we could track which TLocal variables
			   have specific closure types (and are never reassigned), we could call their
			   typed invoke() method directly instead of going through invokeN with boxing.
			   For now, we always use the dynamic dispatch path.
			   See plan: "CRITICAL: Typed Closure Variables and Direct invoke() Calls" *)
			let raw_func = cs_expr_of_texpr ectx e_callee in
			(* Check if callee is Null<T> that needs .value unwrap in C#.
			   Use find_null_in_expr which handles:
			   - Direct Null<T> types where T needs wrapper (value types, type params)
			   - Method calls that return Null<TypeParam> (like Array<Function>.pop() returning Null<T>)
			   CRITICAL: Even though Function is a reference type, Array<T>.pop() declares Null<T>
			   as its return type, so the C# method signature IS Null<Function> and needs .value. *)
			let func = if find_null_in_expr e_callee then
				CsField (raw_func, "value")
			else
				raw_func
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
			(* Handle Rest parameters - check if last param is Rest<T> *)
			let num_params = List.length param_types_hx in
			let last_param_rest = if num_params > 0 then
				get_rest_element_type (List.nth param_types_hx (num_params - 1))
			else
				None
			in
			(* Helper to convert a single arg with null handling *)
			let convert_arg arg expected_hx_type =
				let is_null = match arg.eexpr with TConst TNull -> true | _ -> false in
				let expected_cs_type = cs_type_of_type ectx.gctx expected_hx_type in
				if is_null then begin
					(* Handle null specially *)
					match expected_cs_type with
					| CsTypeClass ((["haxe"; "lang"], "Null"), _) -> CsDefault expected_cs_type
					| CsTypeGenericParam _ -> CsDefault expected_cs_type
					| _ -> CsNull
				end else begin
					let cs_arg = cs_expr_of_texpr ectx arg in
					coerce_arg ~in_scope:ectx.type_params_in_scope ectx.gctx cs_arg arg.etype expected_hx_type
				end
			in
			(* Convert args, handling null -> default(Null<T>) and type coercion *)
			let args_cs, param_types_cs = match last_param_rest with
				| Some rest_elem_type ->
					(* Last param is Rest<T> - split args into regular and rest parts *)
					let regular_param_count = num_params - 1 in
					let regular_args = ExtList.List.take regular_param_count args in
					let rest_args = ExtList.List.drop regular_param_count args in
					(* Generate regular args *)
					let regular_cs_args = List.mapi (fun i arg ->
						let expected_type = List.nth param_types_hx i in
						convert_arg arg expected_type
					) regular_args in
					(* Generate rest args - wrap into Array *)
					let rest_cs_arg =
						if rest_args = [] then
							CsNew (haxe_array_type, [])
						else begin
							match (List.hd rest_args).eexpr with
							| TUnop (Spread, _, spread_expr) ->
								cs_expr_of_texpr ectx spread_expr
							| _ ->
								let elem_cs_type = cs_type_of_type ectx.gctx rest_elem_type in
								let rest_cs_args = List.map (fun arg ->
									convert_arg arg rest_elem_type
								) rest_args in
								let native_array = CsNewArray (elem_cs_type, rest_cs_args) in
								let storage_type = classify_cs_array_element_type elem_cs_type in
								let target_array_type = haxe_array_type in
								make_array_from_native storage_type native_array target_array_type
						end
					in
					let all_args = regular_cs_args @ [rest_cs_arg] in
					let regular_param_types = ExtList.List.take regular_param_count param_types_hx in
					let rest_array_type = ectx.gctx.com.basic.tarray rest_elem_type in
					let all_param_types_hx = regular_param_types @ [rest_array_type] in
					let param_types_cs = List.map (cs_type_of_type ectx.gctx) all_param_types_hx in
					(all_args, param_types_cs)
				| None ->
					let args_cs = List.mapi (fun i arg ->
						let expected_hx_type = if i < List.length param_types_hx then
							List.nth param_types_hx i
						else
							arg.etype
						in
						convert_arg arg expected_hx_type
					) args in
					let param_types_cs = List.map (cs_type_of_type ectx.gctx) param_types_hx in
					(args_cs, param_types_cs)
			in
			(* Get the return type and register the typed invoke signature *)
			let result_type = cs_type_of_type ectx.gctx e.etype in
			(* Register this signature for later generation on Function class *)
			register_invoke_signature ectx.gctx param_types_cs result_type;
			(* Use Value-based invoke to avoid boxing primitives *)
			let num_args = List.length args_cs in
			let hxvalue_args = generate_hxvalue_args args_cs param_types_cs in
			let call_expr = CsCall (CsField (func, hxvalue_invoke_method_name num_args), hxvalue_args) in
			(* Extract the return value from Value *)
			cast_value_to_type result_type call_expr
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
		| ([], "Array") | (["haxe"; "root"], "Array") ->
			(* Array is non-generic in C# output - just new haxe.root.Array() *)
			let cs_args = List.map (cs_expr_of_texpr ectx) args in
			CsNew (haxe_array_type, cs_args)
		| _ ->
			(* Use cs_type_of_type to properly handle type parameters:
			   C# native types (System, cs namespaces) keep type params.
			   Haxe classes have type params erased. *)
			let class_type = cs_type_of_type ectx.gctx (TInst (c, params)) in
			let actual_params = params in
			ignore c.cl_params;
			(* Get constructor parameter types for proper type coercion and Rest handling *)
			let ctor_param_types = match c.cl_constructor with
				| Some cf -> begin match follow cf.cf_type with
					| TFun (ctor_params, _) ->
						(* Apply class type parameters to resolve generic types.
						   Use actual_params (from e.etype) for correct constructor param types. *)
						let map = apply_params c.cl_params actual_params in
						List.map (fun (_, _, t) -> map t) ctor_params
					| _ -> []
				end
				| None -> []
			in
			let cs_args = generate_call_args ectx cs_expr_of_texpr args ctor_param_types in
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
				CsNew (class_type, cs_args)
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
				(* Values get boxed to object when placed in object[]. For Null<T> structs,
				   this would box the struct, not convert hasValue=false to null.
				   The coercion logic in cs_expr_of_texpr handles Null<T> → object via toDynamic
				   when the target type expects object, so we don't need to add it here.
				   The values will be properly coerced during the expression generation. *)
				(* Note: Previously tried adding .toDynamic() here for Null<T>, but this caused
				   issues when the Haxe type is Null<T> but C# produces bare T (e.g., bool ops). *)
				val_expr :: name_expr :: acc
			) [] fields in
			let field_args = List.rev field_args in
			(* Create native object array with the field name/value pairs *)
			let array_expr = CsNewArray (CsTypeObject, field_args) in
			(* Wrap in haxe.root.Array<object> using appropriate factory method *)
			let haxe_array = make_array_from_native ArrayDynamic array_expr (haxe_array_type) in
			CsStaticCall (CsTypeClass (NativeTypes.haxe_dynamic_object_path, []), "_hx_create", [haxe_array])
		end
	| TArrayDecl items ->
		(* Array literal [] creates a haxe.root.Array<T>, not a native C# array.
		   Note: e.etype may be Null<Array<T>> if the array literal is in a context
		   expecting a nullable array. We need to unwrap Null to get the Array type. *)
		let array_etype = match follow e.etype with
			| TAbstract ({ a_path = (["haxe"; "lang"], "Null") }, [inner_t]) -> inner_t
			| t -> t
		in
		let array_type = cs_type_of_type ectx.gctx array_etype in
		if items = [] then
			(* Empty array: new haxe.root.Array<T>() *)
			CsNew (array_type, [])
		else begin
			(* Non-empty array: use appropriate factory method based on element type *)
			let elem_hx_type = match follow e.etype with
				| TInst (_, [t]) -> t
				| TAbstract ({ a_path = ([], "Null") }, [TInst (_, [t])]) -> t
				| _ -> mk_mono()
			in
			let elem_cs_type = cs_type_of_type ectx.gctx elem_hx_type in
			let cs_items = List.map (cs_expr_of_texpr ectx) items in
			let storage_type = classify_cs_array_element_type elem_cs_type in
			(* When using ArrayObject storage (which uses __ofObjectLiteral expecting object[]),
			   we need to create object[] if the element type is a value type (like Null<int>).
			   C# value type arrays are not covariant with object[]. *)
			let native_array_type, native_array_items = match storage_type with
				| ArrayObject when not (CsTypeMapping.is_inherently_nullable elem_cs_type) ->
					(* Value type - create object[] and box elements *)
					(CsTypeObject, cs_items)
				| _ ->
					(* Reference type or typed storage - use actual element type *)
					(elem_cs_type, cs_items)
			in
			let native_array = CsNewArray (native_array_type, native_array_items) in
			make_array_from_native storage_type native_array array_type
		end
	| TTypeExpr mt ->
		(* Convert module type to Haxe type, then to C# type *)
		let t = type_of_module_type mt in
		let cs_type = cs_type_of_type ectx.gctx t in
		(* Erase type parameters to object - C# doesn't allow typeof(SomeType<T>)
		   unless T is in scope. We convert to typeof(SomeType<object>) *)
		let cs_type = CsTypeMapping.erase_type_params cs_type in
		CsTypeOf cs_type
	| TParenthesis e ->
		CsParens (cs_expr_of_texpr ectx e)
	| TCast (inner_e, mt_opt) ->
		(* The target type is the outer expression's type (e.etype), not the inner expression's type *)
		(* C# doesn't allow casting to void, so just emit the inner expression *)
		if ExtType.is_void (follow e.etype) then
			cs_expr_of_texpr ectx inner_e
		else begin
			(* For explicit unsafe cast (cast(expr, Type)), use Runtime.checkedCast to get
			   proper runtime checking. This is needed because C# doesn't allow direct casts
			   between unrelated types (like string to int). *)
			let is_unsafe_cast = mt_opt <> None in
			let target_type_raw = cs_type_of_type ectx.gctx e.etype in
			(* Erase type parameters that are not in scope - if we have type params from
			   a called method's signature, they won't be valid in the current context.
			   For example, calling a generic method via reflection returns T, but T
			   is not defined in the calling context. Replace with object. *)
			let target_type = CsTypeMapping.erase_out_of_scope_type_params ectx.type_params_in_scope target_type_raw in
			(* NOTE: CsNullable handles Null<Null<T>> flattening at the AST level. *)
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
				let is_inner_null_wrapper = match inner_type with
					| CsTypeClass ((["haxe"; "lang"], "Null"), _) -> true
					| _ -> false
				in
				(* Check if inner is a ternary where one branch is Null<object> and the other is object.
				   In this case, C# unifies to 'object', but Haxe type says Null<object>.
				   We can't call .value on object, so we need to use Runtime.toInt/etc. directly. *)
				let is_ternary_with_mixed_null_branches = match inner_e.eexpr with
					| TIf (_, then_e, Some else_e) ->
						let then_cs_t = cs_type_of_type ectx.gctx then_e.etype in
						let else_cs_t = cs_type_of_type ectx.gctx else_e.etype in
						let is_null t = match t with CsTypeClass ((["haxe"; "lang"], "Null"), _) -> true | _ -> false in
						let is_obj t = match t with CsTypeObject -> true | _ -> false in
						(is_null then_cs_t && is_obj else_cs_t) || (is_obj then_cs_t && is_null else_cs_t) ||
						(is_null then_cs_t && is_null else_cs_t)  (* Both Null but C# might not see it that way *)
					| _ -> false
				in
				(* Check if the inner expression is an arithmetic binop on Null<T>.
				   In C#, arithmetic on Null<T> uses implicit conversion and produces T, not Null<T>.
				   So even though Haxe type is Null<T>, the C# expression is already T. *)
				let is_already_unwrapped_by_arithmetic = is_binop_with_implicit_null_conversion inner_e in
				(* Debug removed *)
				if is_target_null_wrapper && is_wrapped_type_match then
					(* Use implicit conversion - just return the inner expression as-is.
					   C#'s implicit operator will handle the conversion. *)
					inner_cs
				else if is_target_null_wrapper && is_inner_null_wrapper && not is_wrapped_type_match then begin
					(* Casting from Null<A> to Null<B> where A != B.
					   Need to convert the inner value and rewrap.
					   E.g., Null<int> to Null<double>: hasValue ? new Null<double>((double)value, true) : default(Null<double>)

					   IMPORTANT: The Haxe type says Null<A>, but the actual C# expression might just be
					   a primitive if the inner TCast used implicit conversion (is_wrapped_type_match).
					   In that case, we can't access .hasValue/.value on it - it's just a raw primitive.
					   Check the C# expression structure, not just the Haxe type. *)
					let inner_unwrapped = match inner_type with
						| CsTypeClass ((["haxe"; "lang"], "Null"), [t]) -> t
						| _ -> CsTypeObject
					in
					let outer_unwrapped = match target_type with
						| CsTypeClass ((["haxe"; "lang"], "Null"), [t]) -> t
						| _ -> CsTypeObject
					in
					(* Check if the generated C# expression is actually a Null<T> wrapper (CsNew/CsDefault/CsLocal of Null type)
					   or if it's just a raw primitive value due to implicit conversion optimizations.
					   A CsConst or a CsCast to primitive is NOT a Null wrapper. *)
					let rec is_actual_null_wrapper cs_e = match cs_e with
						| CsNew (CsTypeClass ((["haxe"; "lang"], "Null"), _), _) -> true
						| CsDefault (CsTypeClass ((["haxe"; "lang"], "Null"), _)) -> true
						| CsTernary (_, then_e, else_e) ->
							(* Both branches must be Null wrappers *)
							is_actual_null_wrapper then_e && is_actual_null_wrapper else_e
						| CsLocal _ -> true  (* Assume locals are correctly typed *)
						| CsField (_, _) -> true  (* Field access on Null type *)
						| CsCall (_, _) -> true  (* Method call returning Null *)
						| CsParens e -> is_actual_null_wrapper e
						| CsConst _ -> false  (* Raw constant - not a Null wrapper *)
						| CsCast (CsTypeClass ((["haxe"; "lang"], "Null"), _), _) -> true  (* Cast to Null is wrapper *)
						| CsCast (_, _) -> false  (* Cast to other type - not a Null wrapper *)
						| _ -> true  (* Default: assume it's a wrapper to be safe *)
					in
					let inner_is_null_wrapper = is_actual_null_wrapper inner_cs in
					(* Check if we need numeric conversion *)
					let is_numeric_conversion = match outer_unwrapped, inner_unwrapped with
						| CsTypeDouble, CsTypeInt -> true
						| CsTypeDouble, CsTypeFloat -> true
						| CsTypeFloat, CsTypeInt -> true
						| CsTypeLong, CsTypeInt -> true
						| CsTypeInt, CsTypeLong -> true  (* narrowing *)
						| CsTypeInt, CsTypeDouble -> true  (* narrowing *)
						| _ -> false
					in
					if not inner_is_null_wrapper && is_numeric_conversion then
						(* Inner is a raw primitive value (not actually wrapped) - just convert and wrap directly.
						   E.g., -1 (typed as Null<int> but generated as just -1) -> Null<double>:
						   new Null<double>((double)(-1), true) *)
						let converted_value = CsCast (outer_unwrapped, inner_cs) in
						CsNew (target_type, [converted_value; CsConst (CsConstBool true)])
					else if is_numeric_conversion then
						(* Inner is an actual Null<T> expression - use hasValue check.
						   E.g., someNullInt -> Null<double>: hasValue ? new Null<double>((double)value, true) : default *)
						let has_value = CsField (inner_cs, "hasValue") in
						let converted_value = CsCast (outer_unwrapped, CsField (inner_cs, "value")) in
						let true_branch = CsNew (target_type, [converted_value; CsConst (CsConstBool true)]) in
						let false_branch = CsDefault target_type in
						CsTernary (has_value, true_branch, false_branch)
					else
						(* For non-numeric conversion (e.g., Null<SomeClass<A>> to Null<SomeClass<B>>),
						   direct cast *)
						CsCast (target_type, inner_cs)
				end
				else if is_target_null_wrapper && not is_inner_null_wrapper then begin
					(* Casting TO Null<T> from a non-Null type that doesn't match T exactly.
					   E.g., int to Null<double>, or SomeClass to Null<SomeInterface>.
					   We need to convert to T first, then let C#'s implicit operator handle Null wrapping.
					   IMPORTANT: Only do this when inner is NOT already Null<_> - the existing logic
					   handles Null<A> to Null<B> conversions above. *)
					let wrapped_type = match target_type with
						| CsTypeClass ((["haxe"; "lang"], "Null"), [t]) -> t
						| _ -> CsTypeObject  (* Shouldn't happen *)
					in
					(* Check if inner is a primitive being converted to a different primitive in Null wrapper.
					   E.g., int to Null<double>. Direct cast (double)(-1) works in C#. *)
					let is_inner_primitive = match inner_type with
						| CsTypeBool | CsTypeByte | CsTypeSByte | CsTypeChar
						| CsTypeShort | CsTypeUShort | CsTypeInt | CsTypeUInt
						| CsTypeLong | CsTypeULong | CsTypeFloat | CsTypeDouble | CsTypeDecimal -> true
						| _ -> false
					in
					let is_wrapped_primitive = match wrapped_type with
						| CsTypeBool | CsTypeByte | CsTypeSByte | CsTypeChar
						| CsTypeShort | CsTypeUShort | CsTypeInt | CsTypeUInt
						| CsTypeLong | CsTypeULong | CsTypeFloat | CsTypeDouble | CsTypeDecimal -> true
						| _ -> false
					in
					if is_inner_primitive && is_wrapped_primitive then
						(* Primitive to primitive conversion - explicitly wrap in Null<T>.
						   We can't rely on implicit conversion because the result might be
						   used in expression context where .hasValue is called before any
						   assignment would trigger the implicit conversion.
						   E.g., ((1 : Null<Float>) ?? throw "").hasValue would fail without explicit wrap. *)
						CsNew (target_type, [CsCast (wrapped_type, inner_cs); CsConst (CsConstBool true)])
					else if inner_type = wrapped_type then
						(* Types already match - explicitly wrap in Null<T>.
						   Same reason as primitive-to-primitive: can't rely on implicit conversion
						   in expression context. *)
						CsNew (target_type, [inner_cs; CsConst (CsConstBool true)])
					else if is_inner_primitive then
						(* Inner is primitive but wrapped type is not - cast directly.
						   This handles weird cases like int to Null<object>. *)
						CsCast (wrapped_type, inner_cs)
					else if (inner_type = CsTypeObject || inner_type = CsTypeDynamic) then
						(* Object/Dynamic to Null<T> - use _ofDynamic for proper handling.
						   This correctly handles:
						   - null object → Null<T> with hasValue=false
						   - primitive value → Null<T> with proper Runtime.toXxx conversion
						   - nested Null<> types → unwrap and rewrap *)
						CsStaticCall (target_type, "_ofDynamic", [inner_cs])
					else
						(* Reference type conversion - explicitly wrap in Null<T> constructor.
						   E.g., SomeClass to Null<SomeInterface> -> new Null<IInterface>((IInterface)value, true)
						   We can't rely on implicit conversion since haxe.lang.Null is an extern class. *)
						let converted = CsCast (wrapped_type, inner_cs) in
						CsNew (target_type, [converted; CsConst (CsConstBool true)])
				end
				else if is_inner_null_wrapper && not is_target_null_wrapper && is_ternary_with_mixed_null_branches then begin
					(* Special case: ternary with mixed Null/object branches being cast to primitive.
					   C# unifies the ternary type to 'object', so we can't call .value on it.
					   Use Runtime.toInt/toDouble/etc. directly on the ternary result. *)
					cast_object_to_type target_type inner_cs
				end
				else if is_inner_null_wrapper && not is_target_null_wrapper && (target_type = CsTypeObject || target_type = CsTypeDynamic) && not is_already_unwrapped_by_arithmetic && not (cs_expr_is_object_cast inner_cs) && not (cs_expr_is_runtime_conversion inner_cs) && expr_produces_csharp_null_type ectx.gctx inner_e then begin
					(* Casting FROM Null<T> to object/Dynamic: use toDynamic() to preserve null semantics.
					   Unlike .value which returns default(T) when hasValue=false, toDynamic() returns null.
					   This ensures Null<T> is never boxed as-is into object (which would cause issues
					   with IConvertible, IComparable, etc.). *)
					CsCall (CsField (inner_cs, "toDynamic"), [])
				end
				else if is_inner_null_wrapper && not is_target_null_wrapper && not is_already_unwrapped_by_arithmetic && not (cs_expr_is_object_cast inner_cs) && not (cs_expr_is_runtime_conversion inner_cs) && expr_produces_csharp_null_type ectx.gctx inner_e then begin
					(* Casting FROM Null<T> to non-Null concrete type - use .value to unwrap, then cast if needed.
					   This handles cases like (SomeInterface)(map.get(...)) where get returns Null<SomeInterface>.
					   Uses expr_produces_csharp_null_type (SINGLE SOURCE OF TRUTH) to verify C# actually has Null<T>.
					   Also skip if arithmetic already unwrapped, or if cast/runtime conversion already handled it. *)
					let unwrapped = CsField (inner_cs, "value") in
					let inner_unwrapped_type = match inner_type with
						| CsTypeClass ((["haxe"; "lang"], "Null"), [t]) -> t
						| _ -> inner_type
					in
					if inner_unwrapped_type = target_type then
						unwrapped
					else
						CsCast (target_type, unwrapped)
				end
				else begin
					(* Special case: native array T[] to Haxe Array - use appropriate factory method.
				   Array is non-generic in C# output, so we use the actual array element type. *)
					match target_type, inner_type with
					| CsTypeClass ((["haxe"; "root"], "Array"), _), CsTypeArray (elem_type, _) ->
						let storage_type = classify_cs_array_element_type elem_type in
						make_array_from_native storage_type inner_cs target_type
					| CsTypeClass ((["haxe"; "root"], "Array"), _), CsTypeClass ((["haxe"; "root"], "Array"), _) ->
						(* Array to Array cast - may need to call __cast() to migrate storage.
						   Check Haxe types (not C# types) to determine source and target storage. *)
						let target_storage = classify_array_element_type e.etype in
						let source_storage = classify_array_element_type inner_e.etype in
						let type_code = array_cast_type_code target_storage in
						if type_code = 0 then
							(* Target is Dynamic - no __cast needed, just return inner *)
							inner_cs
						else if target_storage = source_storage then
							(* Same storage type - no migration needed *)
							inner_cs
						else
							(* Different storage types - call __cast(type_code) to migrate *)
							CsCall (CsField (inner_cs, "__cast"), [CsConst (CsConstInt (Int32.of_int type_code))])
					| _ ->
					(* C# doesn't allow direct casts between unrelated type parameters or
				   primitives to type parameters. Cast through object: (Target)(object)source *)
					let is_primitive_type = function
						| CsTypeBool | CsTypeByte | CsTypeSByte | CsTypeChar
						| CsTypeShort | CsTypeUShort | CsTypeInt | CsTypeUInt
						| CsTypeLong | CsTypeULong | CsTypeFloat | CsTypeDouble | CsTypeDecimal -> true
						| _ -> false
					in
					(* Check if inner expression is TObjectDecl - generates HaxeDynamicObject but types as object *)
					let is_object_decl = match inner_e.eexpr with TObjectDecl _ -> true | _ -> false in
					let needs_double_cast = match target_type, inner_type with
						| CsTypeGenericParam _, CsTypeGenericParam _ -> true  (* T to O *)
						| CsTypeGenericParam _, CsTypeClass _ -> true  (* SomeClass to T - needs (T)(object)v *)
						| CsTypeGenericParam _, CsTypeObject when is_object_decl -> true  (* {} to T - HaxeDynamicObject needs (T)(object)v *)
						| CsTypeGenericParam _, t when is_primitive_type t -> true  (* double/int/etc to T - needs (T)(object)v *)
						| CsTypeClass _, CsTypeGenericParam _ when not is_target_null_wrapper -> true  (* T to SomeClass - needs (SomeClass)(object)v, but not for Null<T> *)
						| t, CsTypeGenericParam _ when is_primitive_type t -> true  (* T to double/int/etc - needs cast through object *)
						| CsTypeClass (_, tparams1), CsTypeClass (_, tparams2) when tparams1 <> [] && tparams2 <> [] ->
							(* Generic class to generic class - may need double cast if type args differ *)
							(* Only if they're not the exact same type *)
							target_type <> inner_type
						| CsTypeClass ((["haxe"; "lang"], "Null"), _), CsTypeClass (_, _) ->
							(* Casting TO Null<T> from a different class type - need double cast.
							   Example: (Null<IMyChild>)parent where parent is IMyParent.
							   Direct cast fails because Null<T> is a struct and C# can't cast
							   directly from reference type to unrelated struct. *)
							true
						| CsTypeClass (path1, _), CsTypeClass (path2, _) when is_unsafe_cast && path1 <> path2 ->
							(* For explicit unsafe casts (Haxe's cast(expr, Type)) between different
							   non-generic class types, C# requires casting through object.
							   Direct cast (Object2)(new Object1()) fails at compile time. *)
							true
						| _ -> false
					in
					(* For unsafe cast (Haxe's cast(expr, Type)), some casts are impossible
					   in C# without going through object or using a runtime helper.
					   String to Int, for example, requires Convert or throwing. *)
					let is_impossible_cast = match target_type, inner_type with
						| CsTypeInt, CsTypeString -> true  (* string to int *)
						| CsTypeFloat, CsTypeString -> true  (* string to float *)
						| CsTypeDouble, CsTypeString -> true  (* string to double *)
						| CsTypeLong, CsTypeString -> true  (* string to long *)
						| t1, t2 when is_primitive_type t1 && is_primitive_type t2 && t1 <> t2 ->
							(* Primitive to different primitive - direct cast is fine in C# *)
							false
						| t, CsTypeString when is_primitive_type t -> true  (* string to any primitive *)
						| _ -> false
					in
					(* When casting from object/Dynamic/type-param to primitive, use Runtime.toXxx to handle
					   boxed type mismatches. Direct cast (double)(object)v fails if v is boxed int.
					   Type parameters are erased to object at runtime, so same issue applies. *)
					let is_object_to_primitive = match target_type, inner_type with
						| t, (CsTypeObject | CsTypeDynamic | CsTypeGenericParam _) when is_primitive_type t -> true
						| _ -> false
					in
					(* When casting from object/Dynamic/type-param to String, use Runtime.toStr to handle
					   boxed primitives. Direct cast (string)(object)v fails if v is a boxed int/bool/etc. *)
					let is_object_to_string = match target_type, inner_type with
						| CsTypeString, (CsTypeObject | CsTypeDynamic | CsTypeGenericParam _) -> true
						| _ -> false
					in
					if is_object_to_string || is_object_to_primitive then
						cast_object_to_type target_type inner_cs
					else if is_unsafe_cast && is_impossible_cast then
						(* Use Runtime.genericCast<T> which throws for impossible casts.
						   Generated as: haxe.lang.Runtime.genericCast<TargetType>(value) *)
						CsCallGeneric (CsStaticField (runtime_type, "genericCast"), [target_type], [inner_cs])
					else if needs_double_cast then
						(* Cast through object: (Target)(object)source
						   Needed for: unrelated class types, type parameter casts, generic coercion *)
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
				| CsTypeObject | CsTypeDynamic ->
					(* Use Runtime.toBool for proper unboxing from object *)
					CsStaticCall (runtime_type, "toBool", [cond_cs])
				| _ -> cond_cs
			in
			let result_type = e.etype in
			let then_e = cs_expr_of_texpr ectx then_expr in
			let else_e = cs_expr_of_texpr ectx else_expr in
			(* Coerce branches to result type if types differ *)
			let then_e = coerce_arg ~in_scope:ectx.type_params_in_scope ectx.gctx then_e then_expr.etype result_type in
			let else_e = coerce_arg ~in_scope:ectx.type_params_in_scope ectx.gctx else_e else_expr.etype result_type in
			(* Special case: if result type is a function type (TFun) and branches are closures,
			   C# can't determine common type between closure classes. Cast both to haxe.lang.Function. *)
			let then_e, else_e = match follow result_type with
				| TFun _ ->
					let func_type = CsTypeClass ((["haxe"; "lang"], "Function"), []) in
					CsCast (func_type, then_e), CsCast (func_type, else_e)
				| _ -> then_e, else_e
			in
			(* Special case: if result is Null<T>, C# can't determine ternary type when one branch
			   is T and other is Null<T> because both implicit conversions exist (T->Null<T> and Null<T>->T).
			   Use new Null<T>(value, true) for explicit wrapping since explicit casts don't work
			   for all types (e.g., interfaces can't be explicitly cast to Null<interface>).
			   NOTE: CsNullable handles Null<Null<T>> flattening at the AST level.
			   IMPORTANT: Only wrap if the branch isn't already the target Null type. *)
			let result_cs_type = cs_type_of_type ectx.gctx result_type in
			let then_cs_type = cs_type_of_type ectx.gctx then_expr.etype in
			let else_cs_type = cs_type_of_type ectx.gctx else_expr.etype in
			let wrap_in_null_if_needed null_type branch_type expr =
				(* Check if the C# expression actually produces a Null type.
				   We can't rely on Haxe types alone because the C# expression might be
				   a primitive constant even when Haxe type says Null<T>.
				   E.g., Haxe `2 : Null<Dynamic>` generates `CsConst(2)`, not `new Null<object>(2)`. *)
				let rec cs_expr_produces_null_type cs_expr = match cs_expr with
					| CsNew (CsTypeClass ((["haxe"; "lang"], "Null"), _), _) -> true
					| CsDefault (CsTypeClass ((["haxe"; "lang"], "Null"), _)) -> true
					| CsLocal _ ->
						(* For local variables, check if Haxe type is Null<T>.
						   The variable was declared with correct Null type. *)
						begin match branch_type with
						| CsTypeClass ((["haxe"; "lang"], "Null"), _) -> true
						| _ -> false
						end
					| CsField (_, "value") -> false  (* .value unwraps Null, so result is not Null *)
					| CsTernary (_, then_e, else_e) ->
						(* Ternary produces Null if BOTH branches produce Null *)
						cs_expr_produces_null_type then_e && cs_expr_produces_null_type else_e
					| CsParens e -> cs_expr_produces_null_type e
					| CsCast (CsTypeClass ((["haxe"; "lang"], "Null"), _), _) -> true
					(* Cast to Null's inner type (where Null<T> and branch says Null<T>) also counts as Null
					   because coerce_arg may have already extracted .value *)
					| CsCast _ ->
						begin match branch_type with
						| CsTypeClass ((["haxe"; "lang"], "Null"), _) -> false  (* Cast but branch says Null - it's unwrapped *)
						| _ -> false
						end
					| _ -> false
				in
				let expr_is_null_type = cs_expr_produces_null_type expr in
				if expr_is_null_type then
					expr  (* Already produces Null type in C# *)
				else
					(* Use new Null<T>(expr, true) instead of (Null<T>)expr to handle interfaces *)
					CsNew (null_type, [expr; CsConst (CsConstBool true)])
			in
			let then_e, else_e = match result_cs_type with
				| CsTypeClass ((["haxe"; "lang"], "Null"), _) as null_type ->
					(* Wrap both branches in Null<T> to ensure C# ternary type is unambiguous *)
					wrap_in_null_if_needed null_type then_cs_type then_e,
					wrap_in_null_if_needed null_type else_cs_type else_e
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
	| TBlock [{ eexpr = TVar (v, Some init) }; call_expr] when is_null_or_default_expr init ->
		(* Common pattern from optional parameters: { var startIndex = null; call(args, startIndex); }
		   Optimize by inlining the default value directly into the call.
		   This avoids creating a lambda IIFE for such simple patterns. *)
		let rec substitute_var_with_default expr =
			(* Substitute TLocal v with the default value expression *)
			match expr.eexpr with
			| TLocal v2 when v2.v_id = v.v_id ->
				(* Replace the local with the init expression *)
				init
			| _ -> Type.map_expr substitute_var_with_default expr
		in
		let optimized_call = substitute_var_with_default call_expr in
		cs_expr_of_texpr ectx optimized_call
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
	| TEnumParameter (enum_expr, ef, i) ->
		(* Access enum constructor parameter - need to cast to the proper subclass *)
		(* Check if expression is Null<EnumType> - need to unwrap via .value *)
		let is_null_wrapper = find_null_in_expr enum_expr in
		let obj = cs_expr_of_texpr ectx enum_expr in
		(* Unwrap Null<T> to get the value before accessing enum parameter *)
		let obj = if is_null_wrapper then CsField (obj, "value") else obj in
		let param_name = match ef.ef_type with
			| TFun (args, _) when i < List.length args ->
				let (name, _, _) = List.nth args i in
				name
			| _ -> Printf.sprintf "_hx_p%d" i
		in
		(* Get the enum and its info from the expression type *)
		let en, enum_params = match follow enum_expr.etype with
			| TEnum (en, params) -> en, params
			| TAbstract ({ a_path = ([], "Null") }, [t]) ->
				begin match follow t with
				| TEnum (en, params) -> en, params
				| _ -> failwith "TEnumParameter on non-enum type"
				end
			| _ -> failwith "TEnumParameter on non-enum type"
		in
		let enum_path = cs_path_of_path en.e_path in
		(* Haxe enums are non-generic in C#, so no type params *)
		ignore enum_params;
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
				match find_type_param_in_type ttp.ttp_name return_type enum_expr.etype with
				| Some t -> cs_type_of_type ectx.gctx t
				| None -> CsTypeObject  (* Fallback to object if inference fails *)
			) extra_ctor_params
		end in
		(* Create the nested type.
		   Type erasure: ALL Haxe enums (including GADT constructors) are non-generic in C#.
		   The GADT extra type params only existed at Haxe compile-time for type checking.
		   The C# nested class for the constructor has no type parameters. *)
		let parent_type = CsTypeClass (enum_path, []) in
		let nested_type = CsTypeNested (parent_type, ctor_name) in
		ignore ctor_type_args;  (* Type params erased - not used in C# *)
		let cast_expr = CsCast (nested_type, obj) in
		let field_access = CsField (cast_expr, escape_identifier param_name) in
		(* The enum constructor field is typed as 'object' in C# due to type erasure.
		   But the Haxe expression type is the substituted type (e.g., Int).
		   Cast the field access to the expected type if needed. *)
		let param_type = match ef.ef_type with
			| TFun (args, _) when i < List.length args ->
				let (_, _, t) = List.nth args i in
				(* Substitute enum type params with actual type arguments *)
				let map_type = apply_params en.e_params enum_params in
				map_type t
			| _ -> e.etype
		in
		let field_cs_type = cs_type_of_type ectx.gctx param_type in
		begin match field_cs_type with
		| CsTypeObject | CsTypeDynamic -> field_access
		(* Use cast_object_to_type for proper unboxing of enum fields *)
		| _ -> cast_object_to_type field_cs_type field_access
		end
	| TEnumIndex e ->
		(* For extern enums (C# native enums), cast to int; otherwise access _hx_index.
		   NOTE: All Haxe enums (including simple ones) are now generated as classes
		   to preserve null semantics, so only extern enums should cast to int. *)
		(* Special case: if expression is literal null, accessing ._hx_index would fail.
		   Generate -1 which won't match any valid enum case (indices start at 0). *)
		begin match e.eexpr with
		| TConst TNull ->
			CsConst (CsConstInt (-1l))
		| _ ->
			(* Also check if expression is Null<EnumType> - need to unwrap via .value *)
			let is_null_wrapper = find_null_in_expr e in
			let is_extern_enum, is_dynamic = match follow e.etype with
				| TEnum (en, _) -> has_enum_flag en EnExtern, false
				| TAbstract ({ a_path = ([], "Null") }, [t]) ->
					(* Null<EnumType> - check inner type *)
					begin match follow t with
					| TEnum (en, _) -> has_enum_flag en EnExtern, false
					| TDynamic _ -> false, true
					| _ -> false, false
					end
				| TDynamic _ -> false, true
				| _ -> false, false
			in
			let obj = cs_expr_of_texpr ectx e in
			(* Unwrap Null<T> to get the value before accessing enum index *)
			let obj = if is_null_wrapper then CsField (obj, "value") else obj in
			if is_extern_enum then
				CsCast (CsTypeInt, obj)
			else if is_dynamic then
				(* Dynamic type - use reflection to get _hx_index field.
				   Use Runtime.toInt for proper unboxing of the returned object. *)
				let reflect_path = (["haxe"; "root"], "Reflect") in
				let field_call = CsStaticCall (CsTypeClass (reflect_path, []), "field", [obj; CsConst (CsConstString "_hx_index")]) in
				cast_object_to_type CsTypeInt field_call
			else
				CsField (obj, "_hx_index")
		end
	| TIdent "__default__" ->
		(* Generated by CsNullable for default(Null<T>) *)
		let cs_type = cs_type_of_type ectx.gctx e.etype in
		CsDefault cs_type
	| TIdent s ->
		CsLocal (escape_identifier s)

(* Helper to wrap a statement-as-expression in an immediately invoked lambda.
   Used for TIf, TSwitch, TTry, TWhile when they appear in expression context.
   Generates: ((Func<T>)(() => { <stmt as return>; }))() *)
and cs_expr_of_stmt_as_expr ectx e =
	let declared_return_type = cs_type_of_type ectx.gctx e.etype in
	let is_declared_void = ExtType.is_void (follow e.etype) in
	(* Check if the statement contains returns with values.
	   If so, we need to use Func<T> even if the outer expression is void-typed.
	   This happens with while(true) { switch { case: return value; } } patterns. *)
	let return_type, is_void = match find_return_type e with
		| Some ret_t when is_declared_void ->
			(* Found a return with value inside a void-typed expression - use the return type *)
			cs_type_of_type ectx.gctx ret_t, false
		| _ ->
			declared_return_type, is_declared_void
	in
	(* Convert the statement, but we need to extract the "value" from it.
	   For TTry, TIf, TSwitch, etc., the value is the last expression in each branch. *)
	let stmt_with_return = cs_stmt_with_return_inner ectx is_void return_type e in
	let lambda = CsLambda ([], CsLambdaBlock [stmt_with_return]) in
	(* For void expressions, use Action instead of Func<void> *)
	let func_type = if is_void then CsTypeAction [] else CsTypeFunc ([], return_type) in
	CsCall (CsCast (func_type, lambda), [])

(* Convert a statement to have explicit returns for expression-as-statement conversion.
   This makes the last expression in each branch into a return statement.
   is_void: if true, don't generate return statements with values (just emit the statement)
   ret_cs_type: the C# return type to use for default returns (needed when inner returns
   have a different type than the expression's declared type) *)
and cs_stmt_with_return_inner ectx is_void ret_cs_type e =
	match e.eexpr with
	| TTry (e1, catches) ->
		let try_body = cs_stmt_with_return_inner ectx is_void ret_cs_type e1 in
		let catch_clauses = List.map (fun (v, catch_expr) ->
			let catch_body = cs_stmt_with_return_inner ectx is_void ret_cs_type catch_expr in
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
		let then_body = cs_stmt_with_return_inner ectx is_void ret_cs_type e_then in
		let else_body = Option.map (cs_stmt_with_return_inner ectx is_void ret_cs_type) e_else_opt in
		CsIf (cond_cs, then_body, else_body)
	| TSwitch sw ->
		let switch_expr = cs_expr_of_texpr ectx sw.switch_subject in
		let cs_sections = List.map (fun case ->
			let cs_labels = List.map (fun v -> CsCaseConst (cs_expr_of_texpr ectx v)) case.case_patterns in
			let case_body_with_return = cs_stmt_with_return_inner ectx is_void ret_cs_type case.case_expr in
			{ sw_labels = cs_labels; sw_body = [case_body_with_return] }
		) sw.switch_cases in
		let cs_sections = match sw.switch_default with
			| Some def_expr ->
				let def_body = cs_stmt_with_return_inner ectx is_void ret_cs_type def_expr in
				cs_sections @ [{ sw_labels = [CsCaseDefault]; sw_body = [def_body] }]
			| None -> cs_sections
		in
		CsSwitch (switch_expr, cs_sections)
	| TBlock exprs ->
		let (init_exprs, last_opt) = split_last exprs in
		let init_stmts = List.map (cs_stmt_of_texpr ectx) init_exprs in
		let final_stmt = match last_opt with
			| Some last_expr -> cs_stmt_with_return_inner ectx is_void ret_cs_type last_expr
			| None -> if is_void then CsEmpty else CsReturn None
		in
		CsBlock (init_stmts @ [final_stmt])
	| TWhile _ ->
		(* While loops as expressions are unusual - just emit the loop.
		   Use ret_cs_type for the default return, not e.etype, because the
		   return type may have been inferred from internal returns. *)
		if is_void then
			cs_stmt_of_texpr ectx e
		else
			CsBlock [cs_stmt_of_texpr ectx e; CsReturn (Some (CsDefault ret_cs_type))]
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

(* Convert a statement to assign its result to a variable instead of returning.
   Similar to cs_stmt_with_return_inner but uses assignment instead of return.
   This avoids lambda IIFEs for control flow expressions like if/switch/try. *)
and cs_stmt_with_result_assign ectx is_void result_var result_type e =
	match e.eexpr with
	| TTry (e1, catches) ->
		let try_body = cs_stmt_with_result_assign ectx is_void result_var result_type e1 in
		let catch_clauses = List.map (fun (v, catch_expr) ->
			let catch_body = cs_stmt_with_result_assign ectx is_void result_var result_type catch_expr in
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
		let then_body = cs_stmt_with_result_assign ectx is_void result_var result_type e_then in
		let else_body = Option.map (cs_stmt_with_result_assign ectx is_void result_var result_type) e_else_opt in
		CsIf (cond_cs, then_body, else_body)
	| TSwitch sw ->
		let switch_expr = cs_expr_of_texpr ectx sw.switch_subject in
		let cs_sections = List.map (fun case ->
			let cs_labels = List.map (fun v -> CsCaseConst (cs_expr_of_texpr ectx v)) case.case_patterns in
			let case_body_with_assign = cs_stmt_with_result_assign ectx is_void result_var result_type case.case_expr in
			{ sw_labels = cs_labels; sw_body = [case_body_with_assign] }
		) sw.switch_cases in
		let cs_sections = match sw.switch_default with
			| Some def_expr ->
				let def_body = cs_stmt_with_result_assign ectx is_void result_var result_type def_expr in
				cs_sections @ [{ sw_labels = [CsCaseDefault]; sw_body = [def_body] }]
			| None -> cs_sections
		in
		CsSwitch (switch_expr, cs_sections)
	| TBlock exprs ->
		let (init_exprs, last_opt) = split_last exprs in
		let init_stmts = List.map (cs_stmt_of_texpr ectx) init_exprs in
		let final_stmt = match last_opt with
			| Some last_expr -> cs_stmt_with_result_assign ectx is_void result_var result_type last_expr
			| None -> CsEmpty
		in
		CsBlock (init_stmts @ [final_stmt])
	| TWhile (cond, body, flag) ->
		(* While loop as expression - the loop body's last expression assigns to result_var.
		   For do-while loops that produce a value (like atomic operations), the result
		   is typically assigned inside the loop body. *)
		let body_with_assign = cs_stmt_with_result_assign ectx is_void result_var result_type body in
		let cond_cs = cs_expr_of_texpr ectx cond in
		let cond_cs = match cs_type_of_type ectx.gctx cond.etype with
			| CsTypeObject | CsTypeDynamic ->
					(* Use Runtime.toBool for proper unboxing from object *)
					CsStaticCall (runtime_type, "toBool", [cond_cs])
			| _ -> cond_cs
		in
		begin match flag with
		| NormalWhile -> CsWhile (cond_cs, body_with_assign)
		| DoWhile -> CsDoWhile (body_with_assign, cond_cs)
		end
	| TThrow throw_e ->
		(* Throw doesn't assign - just emit throw statement *)
		CsThrowStmt (cs_expr_of_texpr ectx throw_e)
	| _ ->
		(* For simple expressions, assign them to the result variable (or just emit as statement if void) *)
		if is_void then
			CsExprStmt (cs_expr_of_texpr ectx e)
		else begin
			(* Apply type coercion when assigning to the result variable.
			   This handles cases like object/Dynamic -> Null<T> where we need to wrap the value. *)
			let expr_cs = cs_expr_of_texpr ectx e in
			let expr_type = cs_type_of_type ectx.gctx e.etype in
			let coerced = coerce_cs_types ~in_scope:ectx.type_params_in_scope ectx.gctx expr_cs expr_type result_type in
			CsExprStmt (CsBinop (CsOpAssign, CsLocal result_var, coerced))
		end

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
	| TIf _ | TSwitch _ | TTry _ | TWhile _ ->
		(* For control flow statements used as expressions, generate a result variable
		   instead of wrapping in a lambda IIFE.
		   Pattern: var _hx_result = default(T); if (...) _hx_result = x; else _hx_result = y; use _hx_result *)
		(* NOTE: CsNullable filter handles Null<Null<T>> flattening at the AST level *)
		let result_type = cs_type_of_type ectx.gctx e.etype in
		let is_void = ExtType.is_void (follow e.etype) in
		(* Generate a unique temporary variable name *)
		let result_var = fresh_temp ectx in
		(* Declare the result variable with default value *)
		let decl_stmt = CsVarDecl (result_var, Some result_type, Some (CsDefault result_type)) in
		(* Convert the statement to assign to the result variable *)
		let assign_stmt = cs_stmt_with_result_assign ectx is_void result_var result_type e in
		{ er_stmts = [decl_stmt; assign_stmt]; er_expr = CsLocal result_var }
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

(* ====== Statement translation ====== *)

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
		(* Erase out-of-scope type params to avoid CS0246 errors *)
		let var_type_raw = CsTypeMapping.erase_out_of_scope_type_params ectx.type_params_in_scope var_type_raw in
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
			(* Handle type conversions.
			   Use get_effective_expr_type to handle erased type params - when a method
			   returns a type parameter, the Haxe type is the substituted type, but the
			   C# expression actually produces object due to type erasure. *)
			let init_type = cs_type_of_type ectx.gctx (get_effective_expr_type ectx.gctx init_expr) in
			let is_init_null_wrapper = match init_type with
				| CsTypeClass ((["haxe"; "lang"], "Null"), _) -> true
				| _ -> false
			in
			let is_var_null_wrapper = match var_type with
				| CsTypeClass ((["haxe"; "lang"], "Null"), _) -> true
				| _ -> false
			in
			(* Check if init expression is arithmetic on Null<T> - C# produces T, not Null<T> *)
			let is_init_already_unwrapped = is_binop_with_implicit_null_conversion init_expr in
			(* Check if init_cs is already cast to object - can't access .value on object type *)
			let is_init_object_cast = cs_expr_is_object_cast init_cs in
			(* Check if init_cs is a Runtime.toInt/toDouble/etc. call - returns primitive, not Null<primitive> *)
			let is_init_runtime_conversion = cs_expr_is_runtime_conversion init_cs in
			(* Check if init expr actually produces Null<T> in C# using the SINGLE SOURCE OF TRUTH.
			   This correctly handles cases where Haxe type is Null<T> but C# produces T directly
			   (e.g., enum field access, erased type params). *)
			let is_init_produces_null = expr_produces_csharp_null_type ectx.gctx init_expr in
			let is_var_null_type = match var_type with
				| CsTypeClass ((["haxe"; "lang"], "Null"), _) -> true
				| _ -> false
			in
			(* Track additional prefix statements for cases where we need temp vars *)
			let extra_prefix_stmts = ref [] in
			let init_cs = match init_type, var_type with
				| CsTypeClass ((["haxe"; "lang"], "Null"), [inner_init]), CsTypeClass ((["haxe"; "lang"], "Null"), [inner_var])
					when inner_init = inner_var ->
					(* Null<T> -> Null<T> with exact same inner type: no conversion needed *)
					init_cs
				| (CsTypeObject | CsTypeDynamic), CsTypeClass ((["haxe"; "lang"], "Null"), [inner_var]) ->
					(* object/Dynamic -> Null<T>: wrap in Null<T> constructor with runtime check.
					   Generate: val != null ? new Null<T>((T)val, true) : new Null<T>(default(T), false)
					   This handles cases like safe cast `Std.downcast(obj, SomeClass)` which returns Null<T>.
					   IMPORTANT: If init_cs has side effects (e.g., method call), we must use a temp var
					   to avoid evaluating it twice in the ternary expression. *)
					if cs_expr_has_side_effects init_cs then begin
						(* Generate: object _tmp = init_cs; _tmp != null ? new Null<T>((T)_tmp, true) : ... *)
						let tmp_name = "_tmp_" ^ (string_of_int ectx.gctx.temp_count) in
						ectx.gctx.temp_count <- ectx.gctx.temp_count + 1;
						extra_prefix_stmts := [CsVarDecl (tmp_name, Some CsTypeObject, Some init_cs)];
						let tmp_ref = CsLocal tmp_name in
						let null_check = CsBinop (CsOpNotEq, tmp_ref, CsNull) in
						let converted_value = cast_object_to_type inner_var tmp_ref in
						let true_branch = CsNew (var_type, [converted_value; CsConst (CsConstBool true)]) in
						let false_branch = CsNew (var_type, [CsDefault inner_var; CsConst (CsConstBool false)]) in
						CsTernary (null_check, true_branch, false_branch)
					end
					else begin
						let null_check = CsBinop (CsOpNotEq, init_cs, CsNull) in
						let converted_value = cast_object_to_type inner_var init_cs in
						let true_branch = CsNew (var_type, [converted_value; CsConst (CsConstBool true)]) in
						let false_branch = CsNew (var_type, [CsDefault inner_var; CsConst (CsConstBool false)]) in
						CsTernary (null_check, true_branch, false_branch)
					end
				| (CsTypeObject | CsTypeDynamic), (CsTypeInt | CsTypeLong | CsTypeFloat | CsTypeDouble | CsTypeBool) when not is_var_null_type ->
					(* Dynamic -> primitive: use Runtime.toXxx for proper unboxing *)
					cast_object_to_type var_type init_cs
				| (CsTypeObject | CsTypeDynamic), (CsTypeString | CsTypeClass _) when not is_var_null_type ->
					(* Dynamic -> string/reference type: direct cast is fine *)
					CsCast (var_type, init_cs)
				| CsTypeClass ((["haxe"; "lang"], "Null"), _), (CsTypeObject | CsTypeDynamic) when not is_init_already_unwrapped && not is_init_object_cast && not is_init_runtime_conversion && is_init_produces_null ->
					(* Null<T> -> object/Dynamic: use toDynamic() to get boxed value or null
					   This correctly returns null if hasValue=false, avoiding storing default(T) in object.
					   BUT: Skip if arithmetic already unwrapped in C#, or if already cast to object, or if Runtime conversion *)
					CsCall (CsField (init_cs, "toDynamic"), [])
				| CsTypeClass ((["haxe"; "lang"], "Null"), [inner_t]), var_t when inner_t = var_t && not is_var_null_wrapper && not is_init_already_unwrapped && not is_init_object_cast && not is_init_runtime_conversion && is_init_produces_null ->
					(* Null<T> -> T (exact match): use .value to unwrap
					   BUT: Skip if arithmetic already unwrapped in C#, or if already cast to object, or if Runtime conversion *)
					CsField (init_cs, "value")
				| CsTypeClass ((["haxe"; "lang"], "Null"), _), _ when not is_var_null_wrapper && not is_init_already_unwrapped && not is_init_object_cast && not is_init_runtime_conversion && is_init_produces_null ->
					(* Null<T> -> SomeType (not Null<_>): unwrap via .value and cast if needed
					   BUT: Skip if arithmetic already unwrapped in C#, or if already cast to object, or if Runtime conversion *)
					let unwrapped = CsField (init_cs, "value") in
					CsCast (var_type, unwrapped)
				| CsTypeClass ((["haxe"; "lang"], "Null"), [inner_init]), CsTypeClass ((["haxe"; "lang"], "Null"), [inner_var])
					when inner_init <> inner_var && not is_init_object_cast && not is_init_runtime_conversion && is_init_produces_null ->
					(* Null<A> -> Null<B> where A != B: need to convert inner value.
					   Generate: init.hasValue ? new Null<B>((B)init.value, true) : new Null<B>(default(B), false)
					   BUT: Skip if init is already cast to object (can't access .hasValue/.value) or if Runtime conversion *)
					let has_value = CsField (init_cs, "hasValue") in
					let converted_value = CsCast (inner_var, CsField (init_cs, "value")) in
					let true_branch = CsNew (var_type, [converted_value; CsConst (CsConstBool true)]) in
					let false_branch = CsNew (var_type, [CsDefault inner_var; CsConst (CsConstBool false)]) in
					CsTernary (has_value, true_branch, false_branch)
				| CsTypeClass (init_path, _), CsTypeClass ((["haxe"; "lang"], "Null"), [inner_var]) when init_path <> (["haxe"; "lang"], "Null") && not is_init_null_wrapper ->
					(* SomeType -> Null<T>: wrap in Null<T> constructor.
					   This happens with abstract types that can hold null values.
					   E.g., Variant (abstract over VariantType) -> Null<VariantType>.
					   We need to wrap explicitly since haxe.lang.Null doesn't have implicit conversion. *)
					let converted = CsCast (inner_var, init_cs) in
					CsNew (var_type, [converted; CsConst (CsConstBool true)])
				| CsTypeClass (init_path, _), CsTypeClass (var_path, _) when init_path <> var_path && not is_init_null_wrapper ->
					(* Different class types (not Null<T>) - need cast.
					   This happens with structural typing: e.g., IntIterator -> ArrayIterator<T>
					   where the Haxe type was Iterator<T> mapped to ArrayIterator<T>. *)
					CsCast (var_type, init_cs)
				| CsTypeClass (init_path, init_params), CsTypeClass (var_path, var_params)
					when init_path = var_path && init_params <> var_params && not is_init_null_wrapper ->
					(* Same class type but different type params (e.g., Array<object> -> Array<int>).
					   This can happen with abstract @:from casts that use generic type parameters. *)
					CsCast (var_type, init_cs)
				| _ ->
					(* Check if init_expr is a cs.Syntax.code call - inline C# code may return object
					   even when the Haxe type is concrete (e.g., when accessing erased generic fields).
					   In such cases, add a cast to ensure type safety. *)
					let is_cs_syntax_code_call = match init_expr.eexpr with
						| TCall ({ eexpr = TField (_, FStatic ({ cl_path = (["cs"], "Syntax") }, cf)) }, _)
							when cf.cf_name = "code" || cf.cf_name = "plainCode" -> true
						| _ -> false
					in
					if is_cs_syntax_code_call && var_type <> CsTypeObject && var_type <> CsTypeDynamic then
						CsCast (var_type, init_cs)
					else
						init_cs
			in
			let decl_type = Some var_type in
			let all_prefix_stmts = result.er_stmts @ !extra_prefix_stmts in
			if all_prefix_stmts = [] then
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
					CsBlock (all_prefix_stmts @ [CsExprStmt (CsBinop (CsOpAssign, CsLocal name, init_cs))])
				]
		end
	| TIf (cond, then_expr, else_expr) ->
		(* In C#, if condition must be bool. If it's object/Dynamic, cast to bool. *)
		let cond_cs = cs_expr_of_texpr ectx cond in
		let cond_cs = match cs_type_of_type ectx.gctx cond.etype with
			| CsTypeObject | CsTypeDynamic ->
					(* Use Runtime.toBool for proper unboxing from object *)
					CsStaticCall (runtime_type, "toBool", [cond_cs])
			| _ -> cond_cs
		in
		let then_stmt = cs_stmt_of_texpr ectx then_expr in
		let else_stmt = Option.map (cs_stmt_of_texpr ectx) else_expr in
		CsIf (cond_cs, then_stmt, else_stmt)
	| TWhile (cond, body, NormalWhile) ->
		(* In C#, while condition must be bool. If it's object/Dynamic, cast to bool. *)
		let cond_cs = cs_expr_of_texpr ectx cond in
		let cond_cs = match cs_type_of_type ectx.gctx cond.etype with
			| CsTypeObject | CsTypeDynamic ->
					(* Use Runtime.toBool for proper unboxing from object *)
					CsStaticCall (runtime_type, "toBool", [cond_cs])
			| _ -> cond_cs
		in
		(* Check if we need a break label for break-in-switch-in-loop pattern.
		   In Haxe, 'break' always breaks the loop. In C#, 'break' in a switch only breaks the switch.
		   We generate goto to a label after the loop when a break is inside a switch inside this loop. *)
		let needs_break_label = has_break_in_switch body in
		if needs_break_label then begin
			let break_label = Printf.sprintf "_hx_break_%d" ectx.temp_count in
			ectx.temp_count <- ectx.temp_count + 1;
			let old_loop_break_label = ectx.loop_break_label in
			ectx.loop_break_label <- Some break_label;
			let body_cs = cs_stmt_of_texpr ectx body in
			ectx.loop_break_label <- old_loop_break_label;
			CsStmtList [CsWhile (cond_cs, body_cs); CsLabel break_label]
		end else begin
			let body_cs = cs_stmt_of_texpr ectx body in
			CsWhile (cond_cs, body_cs)
		end
	| TWhile (cond, body, DoWhile) ->
		(* In C#, do-while condition must be bool. If it's object/Dynamic, cast to bool. *)
		let cond_cs = cs_expr_of_texpr ectx cond in
		let cond_cs = match cs_type_of_type ectx.gctx cond.etype with
			| CsTypeObject | CsTypeDynamic ->
					(* Use Runtime.toBool for proper unboxing from object *)
					CsStaticCall (runtime_type, "toBool", [cond_cs])
			| _ -> cond_cs
		in
		(* Check if we need a break label for break-in-switch-in-loop pattern *)
		let needs_break_label = has_break_in_switch body in
		if needs_break_label then begin
			let break_label = Printf.sprintf "_hx_break_%d" ectx.temp_count in
			ectx.temp_count <- ectx.temp_count + 1;
			let old_loop_break_label = ectx.loop_break_label in
			ectx.loop_break_label <- Some break_label;
			let body_cs = cs_stmt_of_texpr ectx body in
			ectx.loop_break_label <- old_loop_break_label;
			CsStmtList [CsDoWhile (body_cs, cond_cs); CsLabel break_label]
		end else begin
			let body_cs = cs_stmt_of_texpr ectx body in
			CsDoWhile (body_cs, cond_cs)
		end
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
					| None ->
						(* No explicit default. Check if:
						   1. The enclosing function returns void (or is a constructor with no return_type)
						   2. All branches terminate (return/throw)
						   If both are true, this is a partial switch where execution should continue after.
						   Otherwise, throw for definite assignment protection or value return. *)
						let enclosing_returns_void = match ectx.return_type with
							| None -> true  (* Constructor or unset - treat as void *)
							| Some t -> ExtType.is_void (follow t)
						in
						let all_terminate = List.for_all (fun case ->
							let body = cs_stmt_of_texpr ectx case.case_expr in
							stmt_terminates body
						) sw.switch_cases in
						if enclosing_returns_void && all_terminate then CsBlock []
						else CsThrowStmt (CsNew (CsTypeClass ((["System"], "InvalidOperationException"), []),
							[CsConst (CsConstString "Match failure")]))
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
			(* Set in_switch = true so TBreak knows to generate goto instead of break *)
			let old_in_switch = ectx.in_switch in
			ectx.in_switch <- true;
			let sections = List.map (fun case ->
				let labels = List.map (fun p ->
					CsCaseConst (cs_expr_of_texpr ectx p)
				) case.case_patterns in
				let body_stmt = cs_stmt_of_texpr ectx case.case_expr in
				(* Only add break if the body doesn't already exit the case (return, throw, break, goto, etc.)
				   to avoid CS0162 unreachable code warnings. Use stmt_exits_case which also checks for
				   break/goto (unlike stmt_terminates which only checks return/throw). *)
				let body_stmts = if stmt_exits_case body_stmt then [body_stmt] else [body_stmt; CsBreak] in
				{ sw_labels = labels; sw_body = body_stmts }
			) sw.switch_cases in
			let sections = match sw.switch_default with
				| Some e ->
					let body_stmt = cs_stmt_of_texpr ectx e in
					let default_section = {
						sw_labels = [CsCaseDefault];
						sw_body = if stmt_exits_case body_stmt then [body_stmt] else [body_stmt; CsBreak]
					} in
					sections @ [default_section]
				| None ->
					(* No explicit default case. Check if:
					   1. The enclosing function returns void (or is a constructor with no return_type)
					   2. All branches terminate (return/throw)
					   If both are true, this is a partial switch where execution should continue after.
					   Otherwise, throw for definite assignment protection or value return. *)
					let enclosing_returns_void = match ectx.return_type with
						| None -> true  (* Constructor or unset - treat as void *)
						| Some t -> ExtType.is_void (follow t)
					in
					let all_terminate = List.for_all (fun section ->
						match List.rev section.sw_body with
						| [] -> false
						| last :: _ -> stmt_terminates last
					) sections in
					let default_section = {
						sw_labels = [CsCaseDefault];
						sw_body = if enclosing_returns_void && all_terminate then [CsBreak]
						          else [CsThrowStmt (CsNew (CsTypeClass ((["System"], "InvalidOperationException"), []),
							[CsConst (CsConstString "Match failure")]))]
					} in
					sections @ [default_section]
			in
			ectx.in_switch <- old_in_switch;
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
			(* Check if expression type is a primitive and return type is Null<primitive> - need explicit wrapping *)
			let is_expr_primitive = match cs_type_of_type ectx.gctx e.etype with
				| CsTypeBool | CsTypeByte | CsTypeSByte | CsTypeChar
				| CsTypeShort | CsTypeUShort | CsTypeInt | CsTypeUInt
				| CsTypeLong | CsTypeULong | CsTypeFloat | CsTypeDouble | CsTypeDecimal -> true
				| _ -> false
			in
			let return_expr = match ectx.return_type with
				| Some ret_t when is_expr_primitive && is_ret_null_wrapper ->
					(* Primitive expression but return type is Null<T> - need explicit Null wrapper.
					   E.g., returning double when method returns Null<int>.
					   Generate: new Null<int>((int)v, true) *)
					let ret_cs = cs_type_of_type ectx.gctx ret_t in
					let inner_type = match ret_cs with
						| CsTypeClass ((["haxe"; "lang"], "Null"), [inner]) -> inner
						| _ -> CsTypeObject
					in
					let converted = CsCast (inner_type, cs_e) in
					CsNew (ret_cs, [converted; CsConst (CsConstBool true)])
				| Some ret_t when is_haxe_dynamic_type e.etype && is_ret_null_wrapper ->
					(* Returning Dynamic but method returns Null<T> - need to construct Null wrapper.
					   Generate: v == null ? default(Null<T>) : new Null<T>((T)v, true)
					   We use Runtime.toInt/toDouble/toBool for proper numeric conversion. *)
					let ret_cs = cs_type_of_type ectx.gctx ret_t in
					let inner_type = match ret_cs with
						| CsTypeClass ((["haxe"; "lang"], "Null"), [inner]) -> inner
						| _ -> CsTypeObject
					in
					let null_check = CsBinop (CsOpEq, cs_e, CsNull) in
					let converted_value = cast_object_to_type inner_type cs_e in
					let true_branch = CsDefault ret_cs in
					let false_branch = CsNew (ret_cs, [converted_value; CsConst (CsConstBool true)]) in
					CsTernary (null_check, true_branch, false_branch)
				| Some ret_t when is_haxe_dynamic_type e.etype && not (is_haxe_dynamic_type ret_t) && not (ExtType.is_void (follow ret_t)) ->
					(* Returning Dynamic but method returns a concrete type (non-void, non-Null).
					   For primitives, use Runtime.toXxx to handle boxed type mismatches. *)
					let ret_cs = cs_type_of_type ectx.gctx ret_t in
					cast_object_to_type ret_cs cs_e
				| Some ret_t when is_type_param e.etype && is_ret_null_wrapper ->
					(* Expression is T (type param) and return is Null<T> - use implicit conversion, don't cast *)
					cs_e
				| Some ret_t when is_type_param e.etype && is_type_param ret_t ->
					(* Both are type parameters but may be different (e.g., O vs T) - direct cast *)
					let expr_cs = cs_type_of_type ectx.gctx e.etype in
					let ret_cs = cs_type_of_type ectx.gctx ret_t in
					if expr_cs <> ret_cs then
						(* Different type params - cast O to T *)
						CsCast (ret_cs, cs_e)
					else
						cs_e
				| Some ret_t when is_type_param ret_t && not (is_type_param e.etype) ->
					(* GADT pattern: returning concrete type (string, int, etc.) but method returns T.
					   Haxe typer knows the type is correct, but C# needs explicit cast. *)
					let ret_cs = cs_type_of_type ectx.gctx ret_t in
					CsCast (ret_cs, cs_e)
				| Some ret_t when is_type_param e.etype && not (is_type_param ret_t) && not (is_haxe_dynamic_type ret_t) ->
					(* Constrained type param: expression is T but return type is concrete (e.g., T:(Float) -> Float).
					   Haxe knows T can be used as Float, but C# needs proper unboxing for primitives. *)
					let ret_cs = cs_type_of_type ectx.gctx ret_t in
					cast_object_to_type ret_cs cs_e
				| Some ret_t when (match cs_type_of_type ectx.gctx ret_t with CsTypeObject | CsTypeDynamic -> true | _ -> false)
						&& expr_produces_csharp_null_type ectx.gctx e ->
					(* Returning Null<T> to object/Dynamic: call toDynamic() to properly box.
					   Null<T> is a custom struct that does NOT unwrap when boxed (unlike C#'s Nullable<T>),
					   so we must explicitly call toDynamic() to get a boxed T value or null. *)
					CsCall (CsField (cs_e, "toDynamic"), [])
				| Some ret_t ->
					(* Check for GADT covariance: returning SomeClass<ConcreteType> where method returns SomeClass<A>.
					   C# generics are invariant, so we need to cast through object.
					   Example: returning Option<int>.Some when method returns Option<A>. *)
					let ret_cs = cs_type_of_type ectx.gctx ret_t in
					let expr_cs = cs_type_of_type ectx.gctx e.etype in
					let needs_gadt_cast = match ret_cs, expr_cs with
						| CsTypeClass (ret_path, ret_params), CsTypeClass (expr_path, expr_params)
							when ret_path = expr_path && ret_params <> expr_params ->
							(* Same class but different type params - check if return has type params *)
							let has_type_param = function CsTypeGenericParam _ -> true | _ -> false in
							List.exists has_type_param ret_params
						| _ -> false
					in
					if needs_gadt_cast then
						CsCast (ret_cs, cs_e)
					else begin
						(* Check for TObjectDecl -> class/interface coercion.
						   Structural typing in Haxe allows { hasNext: ..., next: ... } to satisfy Iterator<T>,
						   but C# requires explicit cast through object because we generate HaxeDynamicObject.
						   Note: We check the expression kind, not the type, because type inference may have
						   unified the anonymous type with the target class type.
						   We also check the C# type, not the Haxe type, because typedefs like Iterator<T>
						   are mapped to concrete classes like ArrayIterator<T> in C#. *)
						let is_object_decl = match e.eexpr with TObjectDecl _ -> true | _ -> false in
						let is_cs_class t = match cs_type_of_type ectx.gctx t with CsTypeClass _ -> true | _ -> false in
						if is_object_decl && is_cs_class ret_t then
							CsCast (ret_cs, cs_e)
						(* Check for object -> T coercion.
						   When expression maps to object but return type is a type param T,
						   we need to cast (T)expression. This happens with GADT method calls
						   where type erasure produces object but we need T. *)
						else if expr_cs = CsTypeObject && (match ret_cs with CsTypeGenericParam _ -> true | _ -> false) then
							CsCast (ret_cs, cs_e)
						else
							cs_e
					end
				| _ ->
					cs_e
			in
			CsReturn (Some return_expr)
		end
		end  (* close begin match for TThrow check *)
	| TBreak ->
		(* In Haxe, 'break' always breaks the enclosing loop.
		   In C#, 'break' inside a switch only breaks the switch, not the enclosing loop.
		   When we're inside a switch that's inside a loop, generate goto to the loop's break label. *)
		begin match ectx.in_switch, ectx.loop_break_label with
		| true, Some label -> CsGoto label
		| _ -> CsBreak
		end
	| TContinue ->
		CsContinue
	| TThrow e ->
		CsThrowStmt (cs_expr_of_texpr ectx e)
	| TMeta (_, inner) ->
		(* Unwrap TMeta and process the inner expression as a statement *)
		cs_stmt_of_texpr ectx inner
	| TParenthesis inner ->
		(* Unwrap TParenthesis and process the inner expression as a statement *)
		cs_stmt_of_texpr ectx inner
	| TCast (inner, None) ->
		(* Unwrap unnecessary casts and process the inner expression as a statement *)
		cs_stmt_of_texpr ectx inner
	| TBinop (OpAssign, e1, e2) ->
		(* Assignment statement - use prefix handling for RHS to avoid lambda IIFE.
		   This optimizes: `x = { ... do-while ... }` to use prefix statements instead of wrapping in lambda. *)
		let result = cs_expr_with_prefix ectx e2 in
		if result.er_stmts = [] then
			(* Simple case - no prefix statements needed, fall through to general handler *)
			CsExprStmt (cs_expr_of_texpr ectx e)
		else begin
			(* RHS needed prefix statements - emit them, then the assignment.
			   NOTE: CsNullable filter handles Null<Null<T>> flattening at the AST level.

			   NOTE: We use cs_expr_of_texpr for LHS but delegate to the assignment handling
			   in the general TBinop OpAssign case for correct field handling. *)
			let arg_cs_type = cs_type_of_type ectx.gctx e2.etype in
			let expected_cs_type = cs_type_of_type ectx.gctx e1.etype in
			let val_cs = coerce_cs_types ~in_scope:ectx.type_params_in_scope ectx.gctx result.er_expr arg_cs_type expected_cs_type in
			(* Create a fake expression for just the LHS assignment with pre-computed RHS *)
			let lhs_expr = { e with eexpr = TBinop (OpAssign, e1, { e2 with eexpr = TConst TNull; etype = e1.etype }) } in
			let lhs_assign_cs = cs_expr_of_texpr ectx lhs_expr in
			(* Replace the null RHS with our actual value *)
			let assign_cs = match lhs_assign_cs with
				| CsBinop (CsOpAssign, lhs_cs, _) -> CsBinop (CsOpAssign, lhs_cs, val_cs)
				| CsStaticCall (t, "SetField", [obj; name; _]) -> CsStaticCall (t, "SetField", [obj; name; val_cs])
				| CsCall (CsField (obj, "_hx_setField"), [name; _]) -> CsCall (CsField (obj, "_hx_setField"), [name; val_cs])
				| other -> other  (* Fallback - shouldn't happen *)
			in
			CsBlock (result.er_stmts @ [CsExprStmt assign_cs])
		end
	| _ ->
		(* Expression statement - check if it's a void-typed control flow expression that can be emitted directly *)
		let is_void = ExtType.is_void (follow e.etype) in
		begin match e.eexpr with
		| TIf _ | TSwitch _ | TTry _ when is_void ->
			(* Void control flow - use cs_stmt_with_return_inner with void mode to emit direct statements *)
			cs_stmt_with_return_inner ectx true CsTypeVoid e
		| _ ->
			(* Other expressions - wrap in CsExprStmt *)
			CsExprStmt (cs_expr_of_texpr ectx e)
		end

(* Generate preamble statements for optional parameters with default values.
   In Haxe, optional parameters have default values (e.g., function f(a = 2, b = 4.25)).
   In C#, Null<T> parameters use default(Null<T>) which has hasValue=false.
   This generates: if (!param.hasValue) param = <default>;  for Null<T> types
                   if (param == null) param = <default>;     for reference types *)
let generate_optional_param_defaults ectx gctx tf_args =
	List.filter_map (fun (v, default_opt) ->
		match default_opt with
		| None -> None
		| Some default_expr ->
			(match default_expr.eexpr with
			| TConst TNull -> None
			| _ ->
				let param_name = get_local_name ectx v in
				let param_cs_type = cs_type_of_type gctx v.v_type in
				let cs_default = cs_expr_of_texpr ectx default_expr in
				(match param_cs_type with
				| CsTypeClass ((["haxe"; "lang"], "Null"), _) ->
					let condition = CsUnop (CsOpNot, false, CsField (CsLocal param_name, "hasValue")) in
					let assign = CsExprStmt (CsBinop (CsOpAssign, CsLocal param_name, cs_default)) in
					Some (CsIf (condition, assign, None))
				| _ ->
					let condition = CsBinop (CsOpEq, CsLocal param_name, CsNull) in
					let assign = CsExprStmt (CsBinop (CsOpAssign, CsLocal param_name, cs_default)) in
					Some (CsIf (condition, assign, None))))
	) tf_args

(* ====== Closure class generation ====== *)

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
	let closure_type_params = List.fold_left CsTypeMapping.collect_type_params [] all_types in
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
			(* If parameter has default value, wrap with Null<T> unless already wrapped
			   or unless the type is inherently nullable (classes, arrays, etc.) *)
			let param_type = match default_opt with
				| Some _ ->
					begin match base_type with
					| CsTypeClass ((["haxe"; "lang"], "Null"), _) ->
						(* Already Null<T>, don't double-wrap *)
						base_type
					| _ when CsTypeMapping.is_inherently_nullable base_type ->
						(* Type is inherently nullable (class, array, etc.) - no wrapper needed *)
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
	(* IMPORTANT: Add the closure's own type params to scope - they are class-level params
	   like B in _hx_Closure<B>, which are valid inside the closure's methods. *)
	let closure_type_params_in_scope = closure_type_params @ ectx.type_params_in_scope in
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
		type_params_in_scope = closure_type_params_in_scope;  (* Closure's own params + inherited *)
		type_param_constraints = ectx.type_param_constraints;  (* Inherit constraints from parent *)
		in_switch = false;
		loop_break_label = None;
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

	(* Generate invoke method body with optional parameter default preamble *)
	let default_preamble = generate_optional_param_defaults closure_ectx gctx tf.tf_args in
	let invoke_body = match tf.tf_expr.eexpr with
		| TBlock exprs -> default_preamble @ List.map (cs_stmt_of_texpr closure_ectx) exprs
		| _ -> default_preamble @ [cs_stmt_of_texpr closure_ectx tf.tf_expr]
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
		(* Generate: return invoke((T0)args.__objectArray[0], (T1)args.__objectArray[1], ...);
		   But for optional parameters, use: args.length > i ? (T)args.__objectArray[i] : default(Null<T>)
		   IMPORTANT: For Null<T> types, we need special handling:
		   - If arg is null, use default(Null<T>) which has hasValue=false
		   - If arg is present, cast to inner T and let implicit conversion make Null<T>
		   This prevents InvalidCastException when casting null to a value type. *)
		let args_array = CsField (CsLocal "args", "__objectArray") in
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
				   Generate: Null<inner_type>._ofDynamic(args.__objectArray[i])
				   For optional params: args.length > i ? Null<T>._ofDynamic(args.__objectArray[i]) : default(Null<T>) *)
				let default_val = CsDefault null_type in
				(* Call Null<T>._ofDynamic(args.__objectArray[i]) - static method on the Null<T> type *)
				let of_dynamic_call = CsStaticCall (null_type, "_ofDynamic", [arg_access]) in
				if is_optional then
					let length_check = CsBinop (CsOpGt, args_length, idx_const) in
					CsTernary (length_check, of_dynamic_call, default_val)
				else
					of_dynamic_call
			| Some t ->
				(* Use cast_object_to_type to handle boxed type mismatches.
				   E.g., when args.__objectArray[i] contains boxed long but we need int,
				   Runtime.toInt() handles the conversion correctly. *)
				let casted = cast_object_to_type t arg_access in
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
	(* Array is non-generic in C# output *)
	let invoke_dynamic_method = CsMemberMethod {
		m_name = "invokeDynamic";
		m_return_type = CsTypeObject;
		m_access = AccessModifier.Public;
		m_modifiers = [MemberModifier.Override];
		m_type_params = [];
		m_params = [{ p_name = "args"; p_type = Some haxe_array_type; p_default = None; p_modifier = None }];
		m_body = Some invoke_dynamic_body;
		m_constraints = [];
		m_explicit_interface = None;
		m_attributes = [];
	} in

	(* Build __hx_invokeN method - Value-based invoke to avoid boxing.
	   Each argument is passed as a Value struct that holds primitives in prim field
	   and references in obj field. The kind field indicates which slot contains the value.
	   Extract values using ToInt(), ToDouble(), ToObject(), etc.
	   Returns Value to avoid boxing on return values too. *)
	let hxvalue_invoke_method =
		if num_params = 0 then
			(* No params - just override __hx_invoke0 to call invoke() and wrap result *)
			let invoke_result = CsCall (CsLocal "invoke", []) in
			let body = if return_type = CsTypeVoid then
				[CsExprStmt invoke_result; CsReturn (Some (CsStaticCall (hxvalue_type, "Missing", [])))]
			else
				(* Wrap return value with appropriate Value.FromXxx *)
				let wrapped_result = match return_type with
					| CsTypeInt -> CsStaticCall (hxvalue_type, "FromInt", [invoke_result])
					| CsTypeDouble -> CsStaticCall (hxvalue_type, "FromDouble", [invoke_result])
					| CsTypeFloat -> CsStaticCall (hxvalue_type, "FromFloat", [invoke_result])
					| CsTypeBool -> CsStaticCall (hxvalue_type, "FromBool", [invoke_result])
					| CsTypeLong -> CsStaticCall (hxvalue_type, "FromLong", [invoke_result])
					| _ -> CsStaticCall (hxvalue_type, "FromObject", [invoke_result])
				in
				[CsReturn (Some wrapped_result)]
			in
			CsMemberMethod {
				m_name = "__hx_invoke0";
				m_return_type = hxvalue_type;
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
			(* Build Value params: for each arg, Value aN *)
			let hxvalue_params = List.mapi (fun i _ ->
				{ p_name = "a" ^ string_of_int (i + 1); p_type = Some hxvalue_type; p_default = None; p_modifier = None }
			) invoke_params in
			(* Extract typed values from Value params using cast_value_to_type *)
			let extract_args = List.mapi (fun i param ->
				let a_var = CsLocal ("a" ^ string_of_int (i + 1)) in
				let t = match param.p_type with Some t -> t | None -> CsTypeDynamic in
				cast_value_to_type t a_var
			) invoke_params in
			let invoke_call = CsCall (CsLocal (invoke_method_name num_params), extract_args) in
			let body = if return_type = CsTypeVoid then
				[CsExprStmt invoke_call; CsReturn (Some (CsStaticCall (hxvalue_type, "Missing", [])))]
			else
				(* Wrap return value with appropriate Value.FromXxx *)
				let wrapped_result = cast_type_to_value return_type invoke_call in
				[CsReturn (Some wrapped_result)]
			in
			CsMemberMethod {
				m_name = "__hx_invoke" ^ string_of_int num_params;
				m_return_type = hxvalue_type;
				m_access = AccessModifier.Public;
				m_modifiers = [MemberModifier.Override];
				m_type_params = [];
				m_params = hxvalue_params;
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
		c_members = capture_fields @ [ctor; invoke_method; invoke_dynamic_method; hxvalue_invoke_method];
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
		CsTypeMapping.collect_type_params acc cs_type
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

(* Compute the total number of indexable instance methods across all ancestor classes.
   Used to offset method indices so they don't collide in the inheritance hierarchy.
   Each class's method indices start after the parent's highest index, ensuring that
   virtual dispatch of _hx_invokeMethodN never hits the wrong class's case. *)
let rec compute_ancestor_method_count c =
	match c.cl_super with
	| None -> 0
	| Some (parent, _) ->
		let parent_methods = List.filter (fun cf ->
			match cf.cf_kind with
			| Method (MethNormal | MethInline) when not (has_class_field_flag cf CfStatic) && cf.cf_params = [] -> true
			| _ -> false
		) parent.cl_ordered_fields in
		List.length parent_methods + compute_ancestor_method_count parent

(* Compute the method index for an instance method within its class.
   Returns a hierarchically-unique index (offset by ancestor method count)
   so indices don't collide across the inheritance chain.
   Returns Some(index) if the method is indexable, None for generic/non-indexable methods. *)
let compute_instance_method_index c cf =
	let ancestor_count = compute_ancestor_method_count c in
	let methods = List.filter (fun cf2 ->
		match cf2.cf_kind with
		| Method (MethNormal | MethInline) when not (has_class_field_flag cf2 CfStatic) && cf2.cf_params = [] -> true
		| _ -> false
	) c.cl_ordered_fields in
	let rec find i = function
		| [] -> None
		| cf2 :: rest -> if cf2.cf_name = cf.cf_name then Some (i + ancestor_count) else find (i + 1) rest
	in
	find 0 methods

(* Compute the static method index for a static method within its class.
   Mirrors the filtering logic in generate_static_field_accessors (line 8076). *)
let compute_static_method_index c cf =
	let methods = List.filter (fun cf2 ->
		match cf2.cf_kind with
		| Method (MethNormal | MethInline) when cf2.cf_params = [] -> true
		| _ -> false
	) c.cl_ordered_statics in
	let rec find i = function
		| [] -> None
		| cf2 :: rest -> if cf2.cf_name = cf.cf_name then Some i else find (i + 1) rest
	in
	find 0 methods

(* Generate a closure class for a method reference (FClosure) and return a CsNew expression.
   This is used when a method is referenced but not called, e.g., `this.add` or `Calculator.staticAdd`.
   C# doesn't allow converting method groups to haxe.lang.Function directly, so we generate a wrapper class.

   Parameters:
   - ectx: expression context
   - obj_expr: Optional CsExpr for the object (None for static methods)
   - is_static: true for static methods
   - c_opt: optional tclass for cached closure fast path
   - class_path: path of the class containing the method
   - type_params: type parameters applied to the class
   - cf: the class field (method) being referenced
   - method_type: the type of the method (TFun)
*)
let rec generate_method_closure ectx obj_expr is_static c_opt class_path type_params cf method_type =
	(* Fast path: use cached InstanceMethodFunction/ClassMethodFunction when possible.
	   This reuses the same infrastructure as _hx_getField, ensuring that method closures
	   for the same method on the same object are reference-equal (important for == checks).
	   Only available for concrete Haxe classes (not interfaces, not externs). *)
	match c_opt with
	| Some c when not is_static && not (has_class_flag c CInterface) && not (has_class_flag c CExtern) ->
		begin match compute_instance_method_index c cf with
		| Some idx ->
			let arity = match follow cf.cf_type with TFun(args, _) -> List.length args | _ -> 0 in
			let obj = match obj_expr with Some e -> e | None -> CsThis in
			CsCall (CsField (obj, "_hx_getMethodClosure"), [CsConst (CsConstInt (Int32.of_int idx)); CsConst (CsConstInt (Int32.of_int arity))])
		| None ->
			generate_method_closure_fallback ectx obj_expr is_static class_path type_params cf method_type
		end
	| Some c when is_static && not (has_class_flag c CInterface) && not (has_class_flag c CExtern) ->
		begin match compute_static_method_index c cf with
		| Some idx ->
			let cs_type = CsTypeClass (cs_path_of_path class_path, []) in
			CsCall (CsStaticField (cs_type, "_hx_getStaticMethodClosure"), [CsConst (CsConstInt (Int32.of_int idx))])
		| None ->
			generate_method_closure_fallback ectx obj_expr is_static class_path type_params cf method_type
		end
	| _ ->
		generate_method_closure_fallback ectx obj_expr is_static class_path type_params cf method_type

and generate_method_closure_fallback ectx obj_expr is_static class_path type_params cf method_type =
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
	   Since all Haxe classes are now non-generic in C#, we don't need type params.
	   Special case: String is a C# built-in type, not a class.
	   Note: @:native("string") makes the path lowercase. *)
	let captures, erased_capture_type = if is_static then ([], None) else
		match obj_expr with
		| Some _ ->
			let obj_cs_type = match class_path with
				| ([], "String") | (["haxe"; "root"], "String")
				| ([], "string") | (["haxe"; "root"], "string") -> CsTypeString
				| _ ->
					(* Haxe classes are non-generic in C#, so no type params *)
					ignore type_params;
					CsTypeClass (cs_path_of_path class_path, [])
			in
			([("_hx_this", obj_cs_type)], Some obj_cs_type)
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
			let base_type = if has_method_type_params then CsTypeMapping.erase_type_params base_type else base_type in
			(* If parameter is optional, wrap with Null<T> unless already wrapped
			   or unless the type is inherently nullable (classes, arrays, etc.) *)
			let param_type = if opt then
				match base_type with
				| CsTypeClass ((["haxe"; "lang"], "Null"), _) -> base_type
				| _ when CsTypeMapping.is_inherently_nullable base_type -> base_type
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
	let return_cs_type = if has_method_type_params then CsTypeMapping.erase_type_params return_cs_type else return_cs_type in

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
				CsNew (haxe_array_type, [])
			else
				let native_array = CsNewArray (CsTypeObject, call_args) in
				make_array_from_native ArrayDynamic native_array (haxe_array_type)
			in
			CsStaticCall (runtime_type, "InvokeDelegate", [func_expr; args_array])
		end else
			CsStaticCall (static_type, method_name, call_args)
	else if is_stored_function then begin
		(* Instance function field - use Runtime.InvokeDelegate *)
		let obj = CsField (CsThis, "_hx_this") in
		let func_expr = CsField (obj, method_name) in
		let args_array = if call_args = [] then
			CsNew (haxe_array_type, [])
		else
			let native_array = CsNewArray (CsTypeObject, call_args) in
			make_array_from_native ArrayDynamic native_array (haxe_array_type)
		in
		CsStaticCall (runtime_type, "InvokeDelegate", [func_expr; args_array])
	end else
		CsCall (CsField (CsField (CsThis, "_hx_this"), method_name), call_args)
	in
	let invoke_body = if return_cs_type = CsTypeVoid then
		[CsExprStmt method_call]
	else begin
		(* For type-erased classes like Array, methods return object in C# but the
		   closure's typed invoke() should return the expected type.
		   Add conversion when return type is Null<T> where T is a value type. *)
		let return_value = match return_cs_type with
			| CsTypeClass ((["haxe"; "lang"], "Null"), [inner])
				when not (CsTypeMapping.is_cs_native_generic_class class_path)
				  && not (CsTypeMapping.is_inherently_nullable inner) ->
				CsStaticCall (return_cs_type, "_ofDynamic", [method_call])
			| _ -> method_call
		in
		[CsReturn (Some return_value)]
	end
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
		let args_array = CsField (CsLocal "args", "__objectArray") in
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
				(* Use cast_object_to_type to handle boxed type mismatches.
				   E.g., when args.__objectArray[i] contains boxed long but we need int,
				   Runtime.toInt() handles the conversion correctly. *)
				let casted = cast_object_to_type t arg_access in
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

	(* Array is non-generic in C# output *)
	let invoke_dynamic = CsMemberMethod {
		m_name = "invokeDynamic";
		m_return_type = CsTypeObject;
		m_access = AccessModifier.Public;
		m_modifiers = [Override];
		m_type_params = [];
		m_params = [{ p_name = "args"; p_type = Some haxe_array_type; p_default = None; p_modifier = None }];
		m_body = Some invoke_dynamic_body;
		m_constraints = [];
		m_explicit_interface = None;
		m_attributes = [];
	} in

	(* Build __hx_invokeN method - Value-based invoke to avoid boxing.
	   Returns Value to avoid boxing on return values too. *)
	let hxvalue_invoke_method =
		if num_params = 0 then
			let invoke_result = CsCall (CsLocal "invoke", []) in
			let body = if return_cs_type = CsTypeVoid then
				[CsExprStmt invoke_result; CsReturn (Some (CsStaticCall (hxvalue_type, "Missing", [])))]
			else
				let wrapped_result = match return_cs_type with
					| CsTypeInt -> CsStaticCall (hxvalue_type, "FromInt", [invoke_result])
					| CsTypeDouble -> CsStaticCall (hxvalue_type, "FromDouble", [invoke_result])
					| CsTypeFloat -> CsStaticCall (hxvalue_type, "FromFloat", [invoke_result])
					| CsTypeBool -> CsStaticCall (hxvalue_type, "FromBool", [invoke_result])
					| CsTypeLong -> CsStaticCall (hxvalue_type, "FromLong", [invoke_result])
					| _ -> CsStaticCall (hxvalue_type, "FromObject", [invoke_result])
				in
				[CsReturn (Some wrapped_result)]
			in
			CsMemberMethod {
				m_name = "__hx_invoke0";
				m_return_type = hxvalue_type;
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
			let hxvalue_params = List.mapi (fun i _ ->
				{ p_name = "a" ^ string_of_int (i + 1); p_type = Some hxvalue_type; p_default = None; p_modifier = None }
			) invoke_params in
			let extract_args = List.mapi (fun i (param, _is_optional) ->
				let a_var = CsLocal ("a" ^ string_of_int (i + 1)) in
				let t = match param.p_type with Some t -> t | None -> CsTypeDynamic in
				cast_value_to_type t a_var
			) invoke_params_with_opt in
			let invoke_call = CsCall (CsLocal (invoke_method_name num_params), extract_args) in
			let body = if return_cs_type = CsTypeVoid then
				[CsExprStmt invoke_call; CsReturn (Some (CsStaticCall (hxvalue_type, "Missing", [])))]
			else
				let wrapped_result = cast_type_to_value return_cs_type invoke_call in
				[CsReturn (Some wrapped_result)]
			in
			CsMemberMethod {
				m_name = "__hx_invoke" ^ string_of_int num_params;
				m_return_type = hxvalue_type;
				m_access = AccessModifier.Public;
				m_modifiers = [Override];
				m_type_params = [];
				m_params = hxvalue_params;
				m_body = Some body;
				m_constraints = [];
				m_explicit_interface = None;
				m_attributes = [];
			}
	in

	(* Build class definition *)
	let members = capture_fields @ [ctor; invoke_method; invoke_dynamic; hxvalue_invoke_method] in
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
			   e.g., TestHandler<T> needs to be cast to TestHandler<object> for the closure. *)
			[CsCast (erased_type, expr)]
		| _ -> []
	in
	CsNew (closure_type, capture_args)

(* Initialize the method closure forward reference *)
let () = generate_method_closure_ref := generate_method_closure

(* Generate method body *)
(* ====== Method body & override handling ====== *)

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
	(* Apply Null<T> syntax filter to the expression first *)
	let e = CsNullable.filter gctx.com e in
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
		(* Generate preamble for optional parameters with default values *)
		let default_preamble = generate_optional_param_defaults ectx gctx tf.tf_args in
		(* Generate the inner body *)
		let body_stmts = match tf.tf_expr.eexpr with
			| TBlock exprs -> List.map (cs_stmt_of_texpr ectx) exprs
			| _ -> [cs_stmt_of_texpr ectx tf.tf_expr]
		in
		default_preamble @ body_stmts
	| TBlock exprs ->
		List.map (cs_stmt_of_texpr ectx) exprs
	| _ ->
		[cs_stmt_of_texpr ectx e]

(* Check if a method overrides a parent class method *)
let is_override cf =
	has_class_field_flag cf CfOverride

(* Check if this is a contravariant override (method accepts wider types than parent).
   Returns Some (haxe_args, parent_args) if the override has wider parameter types,
   None otherwise. This is needed because C# doesn't allow contravariant overrides. *)
let get_contravariant_override_info gctx c cf =
	if not (is_override cf) then None
	else
		let args, _ = match follow cf.cf_type with
			| TFun (args, ret) -> args, ret
			| _ -> [], t_dynamic
		in
		let rec find_parent_types c_super tl =
			let map_type = apply_params c_super.cl_params tl in
			try
				let cf_super = PMap.find cf.cf_name c_super.cl_fields in
				match cf_super.cf_kind with
				| Method _ ->
					begin match follow (map_type cf_super.cf_type) with
					| TFun (parent_args, _) -> Some parent_args
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
			| Some parent_args ->
				(* Check if any parameter type is wider in the override than in parent *)
				let is_contravariant = List.exists2 (fun (_, _, hx_t) (_, _, parent_t) ->
					let hx_cs = cs_type_of_type gctx hx_t in
					let parent_cs = cs_type_of_type gctx parent_t in
					(* Contravariant if Haxe type is wider (parent type is subtype of Haxe type) *)
					hx_cs <> parent_cs
				) args parent_args in
				if is_contravariant then Some (args, parent_args) else None
			| None -> None
			end
		| None -> None

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
   We need to generate explicit interface implementations for each such interface.

   IMPORTANT: With type erasure, we compare C# types (not Haxe types) because:
   - Interface method with param type T becomes object in C#
   - Implementation with param type Int becomes int in C#
   - These don't match even though Haxe sees both as "Int" after type substitution *)
let find_variant_interface_methods gctx c cf =
	(* Track seen interfaces to avoid duplicates when same interface is reached
	   via multiple paths (e.g., C implements A and B, where B extends A) *)
	let seen = ref [] in
	let rec check_interface acc map_parent (c_int, params) =
		(* Skip if we've already processed this interface *)
		if List.mem c_int.cl_path !seen then acc
		else begin
			seen := c_int.cl_path :: !seen;
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
					(* Found interface method with same name.
					   Use the DECLARED interface method type (cf_int.cf_type) for C# signature,
					   because type parameters get erased to object in C#. *)
					begin match follow cf_int.cf_type, follow cf.cf_type with
					| TFun (int_args, int_ret), TFun (impl_args, impl_ret) ->
						(* Compare C# types, not Haxe types.
						   Interface uses declared types (type params → object).
						   Implementation uses concrete types (Int → int). *)
						let int_ret_cs = cs_type_of_type gctx int_ret in
						let impl_ret_cs = cs_type_of_type gctx impl_ret in
						let ret_differs = int_ret_cs <> impl_ret_cs in
						let args_differ =
							try
								List.exists2 (fun (_, _, int_t) (_, _, impl_t) ->
									cs_type_of_type gctx int_t <> cs_type_of_type gctx impl_t
								) int_args impl_args
							with Invalid_argument _ -> true (* Different arg counts *)
						in
						if ret_differs || args_differ then
							(* Build the interface type with applied params *)
							let iface_cs_type = cs_type_of_type gctx (TInst (c_int, params)) in
							(* Get interface method's type parameters *)
							let method_type_params = List.map (fun ttp -> ttp.ttp_name) cf_int.cf_params in
							(* Use declared interface args/ret for explicit implementation signature *)
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
		end
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
		(* Get implementation argument types for casting *)
		let impl_args = match follow cf.cf_type with
			| TFun (args, _) -> args
			| _ -> []
		in
		List.map (fun (iface_type, int_args, int_ret, method_type_params) ->
			(* Generate params for the explicit implementation using interface's erased types *)
			let params = List.filter_map (fun (n, _, t) ->
				if ExtType.is_void (follow t) then None
				else Some {
					p_name = escape_identifier n;
					p_type = Some (cs_type_of_type gctx t);
					p_default = None;
					p_modifier = None;
				}
			) int_args in
			(* Generate argument expressions with casts from interface type to impl type.
			   Interface params use erased types (object), impl uses concrete types (string, int).
			   We need to cast: (ImplType)interfaceParam *)
			let arg_exprs = List.map2 (fun (int_n, _, int_t) (_, _, impl_t) ->
				let int_cs_type = cs_type_of_type gctx int_t in
				let impl_cs_type = cs_type_of_type gctx impl_t in
				let arg_ref = CsLocal (escape_identifier int_n) in
				if int_cs_type = impl_cs_type then
					arg_ref  (* Same type, no cast needed *)
				else
					CsCast (impl_cs_type, arg_ref)  (* Cast from interface type to impl type *)
			) int_args impl_args in
			let callee = CsField (CsThis, escape_identifier cf.cf_name) in
			let call_expr =
				if method_type_params = [] then
					CsCall (callee, arg_exprs)
				else
					(* Add explicit type arguments for generic methods *)
					let type_args = List.map (fun tp_name -> CsTypeClass (([], tp_name), [])) method_type_params in
					CsCallGeneric (callee, type_args, arg_exprs)
			in
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
			(* Check for Null<T> wrapper on impl type *)
			let impl_is_null = match impl_cs_type with
				| CsTypeClass ((["haxe"; "lang"], "Null"), _) -> true
				| _ -> false
			in
			let int_is_null = match int_cs_type with
				| CsTypeClass ((["haxe"; "lang"], "Null"), _) -> true
				| _ -> false
			in
			(* Generate getter body *)
			let getter = if has_getter && (impl_read = AccNormal || impl_read = AccCall) then
				let prop_access = CsField (CsThis, name) in
				let return_expr = match impl_is_null, int_is_null with
					| true, false ->
						(* Impl is Null<T>, interface wants T - use .value to unwrap *)
						CsField (prop_access, "value")
					| false, true ->
						(* Impl is T, interface wants Null<T> - implicit conversion works *)
						prop_access
					| _ ->
						(* Same wrapper status - just cast *)
						CsCast (int_cs_type, prop_access)
				in
				Some {
					acc_access = None;
					acc_body = Some [CsReturn (Some return_expr)];
				}
			else
				None
			in
			(* Generate setter body *)
			let setter = if has_setter && (impl_write = AccNormal || impl_write = AccCall) then
				let value_expr = match impl_is_null, int_is_null with
					| true, false ->
						(* Impl is Null<T>, interface passes T - use new Null<T>(value, true) *)
						CsNew (impl_cs_type, [CsLocal "value"; CsConst (CsConstBool true)])
					| false, true ->
						(* Impl is T, interface passes Null<T> - use .value to unwrap *)
						CsField (CsLocal "value", "value")
					| _ ->
						(* Same wrapper status - just cast *)
						CsCast (impl_cs_type, CsLocal "value")
				in
				Some {
					acc_access = None;
					acc_body = Some [CsExprStmt (CsBinop (CsOpAssign,
						CsField (CsThis, name),
						value_expr))];
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

(* Generate an overload method for contravariant overrides.
   When a Haxe override method accepts a wider type than the parent,
   we need to generate an additional overload that accepts the wider type.
   Example:
     Parent: doSomething(Child child)
     Override: doSomething(Base base)  // Haxe allows this, C# doesn't
   We generate:
     override doSomething(Child child) { return doSomething_impl(child); }
     public int doSomething(Base base) { return doSomething_impl(base); }
   But actually it's simpler to just have the override call the implementation directly
   and generate the wider-param method that also calls the same implementation. *)
let generate_contravariant_override_overload gctx c cf =
	match cf.cf_kind with
	| Method MethNormal | Method MethInline when not (has_class_field_flag cf CfStatic) && is_override cf ->
		let haxe_args, haxe_ret = match follow cf.cf_type with
			| TFun (args, ret) -> args, ret
			| _ -> [], t_dynamic
		in
		begin match get_contravariant_override_info gctx c cf with
		| Some (_, parent_args) ->
			(* Check if any param type differs *)
			let has_wider_param = List.exists2 (fun (_, _, hx_t) (_, _, parent_t) ->
				cs_type_of_type gctx hx_t <> cs_type_of_type gctx parent_t
			) haxe_args parent_args in
			if has_wider_param then begin
				(* Generate an overload with the wider (Haxe) parameter types.
				   This method has the actual implementation body. *)
				let name = escape_identifier cf.cf_name in
				let filtered_args = List.filter (fun (_, _, t) -> not (ExtType.is_void (follow t))) haxe_args in
				let params = List.map (fun (n, _, t) -> {
					p_name = escape_identifier n;
					p_type = Some (cs_type_of_type gctx t);
					p_default = None;  (* No defaults for overloads *)
					p_modifier = None;
				}) filtered_args in
				let param_cs_names = List.map (fun (n, _, _) -> escape_identifier n) filtered_args in
				let class_type_params = List.map (fun ttp -> ttp.ttp_name) c.cl_params in
				let method_type_params = List.map (fun ttp -> ttp.ttp_name) cf.cf_params in
				let all_type_params_in_scope = class_type_params @ method_type_params in
				let class_constraints = extract_type_param_constraints gctx c.cl_params in
				let method_constraints = extract_type_param_constraints gctx cf.cf_params in
				let all_type_param_constraints = class_constraints @ method_constraints in
				let body = match cf.cf_expr with
					| Some e ->
						let body_stmts = generate_method_body gctx ~param_cs_names ~type_params_in_scope:all_type_params_in_scope ~type_param_constraints:all_type_param_constraints ~return_type:haxe_ret ~class_path:c.cl_path ~method_name:cf.cf_name e in
						if needs_unchecked e then
							Some [CsUncheckedStmt (CsBlock body_stmts)]
						else
							Some body_stmts
					| None -> None
				in
				let return_cs_type = cs_type_of_type gctx haxe_ret in
				[CsMemberMethod {
					m_name = name;
					m_return_type = return_cs_type;
					m_access = AccessModifier.Public;
					m_modifiers = [MemberModifier.Virtual];  (* Virtual so it can be overridden *)
					m_type_params = method_type_params;
					m_params = params;
					m_body = body;
					m_constraints = [];
					m_explicit_interface = None;
					m_attributes = [];
				}]
			end else []
		| None -> []
		end
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
				(* Check if it's a property (var with AccNormal access) or dynamic method (becomes property) *)
				match iface_cf.cf_kind with
				| Type.Var { v_read = AccNormal; _ } -> true
				| Type.Var { v_write = AccNormal; _ } -> true
				| Type.Method MethDynamic -> true  (* Dynamic methods are properties in C# interfaces *)
				| _ -> false
			with Not_found -> false
		) c.cl_implements

(* ====== Field & property generation ====== *)

(* Generate class field as C# member *)
let generate_field gctx c cf is_static =
	(* Use get_cs_field_name which handles C# restriction where member names
	   cannot be the same as the enclosing type name. *)
	let name = get_cs_field_name c cf in
	let cs_type = cs_type_of_type gctx cf.cf_type in
	(* Special case: __rtti is generated as const for AOT compatibility.
	   Const fields are implicitly static in C#, so we don't add Static modifier.
	   If superclass also has __rtti, add 'new' modifier to avoid CS0108 warning. *)
	let is_rtti = cf.cf_name = "__rtti" && is_static in
	let super_has_rtti =
		if not is_rtti then false
		else
			let rec check_super c =
				match c.cl_super with
				| None -> false
				| Some (super_class, _) ->
					List.exists (fun scf -> scf.cf_name = "__rtti") super_class.cl_ordered_statics
					|| check_super super_class
			in
			check_super c
	in
	let modifiers = if is_rtti then
			if super_has_rtti then [MemberModifier.New; MemberModifier.Const]
			else [MemberModifier.Const]
		else if is_static then [MemberModifier.Static]
		else [] in

	match cf.cf_kind with
	| Var { v_read = AccNormal; v_write = AccNormal } ->
		(* Simple read/write field.
		   Generate as property if implementing an interface (C# fields can't implement interface properties).
		   Otherwise generate as plain field for performance. *)
		let init = match cf.cf_expr with
			| Some e when not (expr_contains_this e) ->
				(* Apply Null<T> syntax filter to field initializer *)
				let e = CsNullable.filter gctx.com e in
				let ectx = create_expr_context gctx in
				ectx.current_class_path <- Some c.cl_path;
				ectx.current_method_name <- Some cf.cf_name;
				let init_cs = cs_expr_of_texpr ectx e in
				(* Coerce initializer to field type - needed for lambda returns, object->typed conversions *)
				let init_cs = coerce_arg ~in_scope:ectx.type_params_in_scope gctx init_cs e.etype cf.cf_type in
				(* Fix: if result is CsNull but field type is Null<T>, use default(Null<T>) instead.
				   This handles cases where the null constant's type doesn't match the field type due to abstracts. *)
				let init_cs = match init_cs with
					| CsNull ->
						begin match cs_type with
						| CsTypeClass ((["haxe"; "lang"], "Null"), _) -> CsDefault cs_type
						| _ -> init_cs
						end
					| _ -> init_cs
				in
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
				(* Apply Null<T> syntax filter to field initializer *)
				let e = CsNullable.filter gctx.com e in
				let ectx = create_expr_context gctx in
				ectx.current_class_path <- Some c.cl_path;
				ectx.current_method_name <- Some cf.cf_name;
				let init_cs = cs_expr_of_texpr ectx e in
				(* Coerce initializer to field type - needed for lambda returns, object->typed conversions *)
				let init_cs = coerce_arg ~in_scope:ectx.type_params_in_scope gctx init_cs e.etype cf.cf_type in
				(* Fix: if result is CsNull but field type is Null<T>, use default(Null<T>) instead *)
				let init_cs = match init_cs with
					| CsNull ->
						begin match cs_type with
						| CsTypeClass ((["haxe"; "lang"], "Null"), _) -> CsDefault cs_type
						| _ -> init_cs
						end
					| _ -> init_cs
				in
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
					(* Apply Null<T> syntax filter to field initializer *)
					let e = CsNullable.filter gctx.com e in
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
		(* Simple property with no custom accessors.
		   Includes initializer if present (e.g., Math.NaN = 0.0 / 0.0) *)
		let init = match cf.cf_expr with
			| Some e when not (expr_contains_this e) ->
				let e = CsNullable.filter gctx.com e in
				let ectx = create_expr_context gctx in
				ectx.current_class_path <- Some c.cl_path;
				ectx.current_method_name <- Some cf.cf_name;
				let init_cs = cs_expr_of_texpr ectx e in
				let init_cs = coerce_arg ~in_scope:ectx.type_params_in_scope gctx init_cs e.etype cf.cf_type in
				let init_cs = match init_cs with
					| CsNull ->
						begin match cs_type with
						| CsTypeClass ((["haxe"; "lang"], "Null"), _) -> CsDefault cs_type
						| _ -> init_cs
						end
					| _ -> init_cs
				in
				Some init_cs
			| _ -> None
		in
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
	| Method MethNormal | Method MethInline ->
		(* Regular method *)
		let haxe_args, haxe_ret = match follow cf.cf_type with
			| TFun (args, ret) -> args, ret
			| _ -> [], cf.cf_type
		in
		(* For override methods, we need to use the parent's DECLARED parameter types
		   (unmapped) to ensure C# compatibility. With type erasure, type parameters
		   become object in C#. If we map K→String before converting to C# type, we get
		   string instead of object, causing signature mismatch with the parent's method. *)
		let args, ret, override_param_casts =
			if not is_static && is_override cf then
				let rec find_parent_types c_super tl =
					let map_type = apply_params c_super.cl_params tl in
					try
						let cf_super = PMap.find cf.cf_name c_super.cl_fields in
						match cf_super.cf_kind with
						| Method _ ->
							(* Use DECLARED type without mapping - type params get erased to object *)
							begin match follow cf_super.cf_type with
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
					| Some (parent_args, parent_ret) ->
						(* Generate casts for parameters where parent type (erased) differs from child type.
						   This happens when parent has type param T that becomes object, but child has concrete type. *)
						let casts =
							let pairs = try List.combine parent_args haxe_args with Invalid_argument _ -> [] in
							List.filter_map (fun ((pname, _, ptype), (_, _, htype)) ->
								let parent_cs = cs_type_of_type gctx ptype in
								let haxe_cs = cs_type_of_type gctx htype in
								(* If parent type is object but haxe type is not, need a cast *)
								match parent_cs, haxe_cs with
								| CsTypeObject, t when t <> CsTypeObject && t <> CsTypeDynamic ->
									(* Generate: var _hx_pname = (HaxeType)pname; *)
									let param_name = escape_identifier pname in
									let shadow_name = "_hx_" ^ param_name in
									Some (shadow_name, param_name, haxe_cs)
								| _ -> None
							) pairs
						in
						(parent_args, parent_ret, casts)
					| None -> (haxe_args, haxe_ret, [])
					end
				| None -> (haxe_args, haxe_ret, [])
			else
				(haxe_args, haxe_ret, [])
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
		(* Get class type parameters *)
		let class_type_params = List.map (fun ttp -> ttp.ttp_name) c.cl_params in
		(* Method type parameters: For Haxe classes, type params are erased to object,
		   so don't include them in C# output. Only C# native classes keep type params. *)
		let method_type_params =
			if CsTypeMapping.is_cs_native_generic_class c.cl_path then begin
				(* C# native class - keep method type params *)
				let explicit_type_params = List.map (fun ttp -> ttp.ttp_name) cf.cf_params in
				let return_cs_type = cs_type_of_type gctx ret in
				let param_cs_types = List.map (fun p -> p.p_type) params in
				let param_cs_types = List.filter_map (fun t -> t) param_cs_types in
				let inferred_type_params = CsTypeMapping.get_method_type_params param_cs_types return_cs_type in
				let inferred_type_params = List.filter (fun p ->
					not (List.mem p class_type_params)
				) inferred_type_params in
				explicit_type_params @ (List.filter (fun p ->
					not (List.mem p explicit_type_params)
				) inferred_type_params)
			end else
				(* Haxe class - erase method type params (they become object) *)
				[]
		in
		(* All type params in scope = class params + method params *)
		let all_type_params_in_scope = class_type_params @ method_type_params in
		(* Extract constraints from class and method type params.
		   For Haxe classes, method type params are erased, so no constraints. *)
		let class_constraints = extract_type_param_constraints gctx c.cl_params in
		let method_constraints = if method_type_params = [] then [] else extract_type_param_constraints gctx cf.cf_params in
		let all_type_param_constraints = class_constraints @ method_constraints in
		(* Check for contravariant override - if so, generate a bridge that calls the overload *)
		let is_contravariant_override = match get_contravariant_override_info gctx c cf with
			| Some _ -> true
			| None -> false
		in
		let body =
			if is_contravariant_override then begin
				(* Generate bridge: just call the overloaded method with same args.
				   The overload has wider param types and contains the actual implementation.
				   IMPORTANT: Use the override's parameter names (from 'args'), not Haxe's (from cf.cf_type).
				   The override signature uses parent's param names. *)
				let filtered_override_args = List.filter (fun (_, _, t) -> not (ExtType.is_void (follow t))) args in
				let arg_exprs = List.map (fun (n, _, _) -> CsLocal (escape_identifier n)) filtered_override_args in
				let call_expr = CsCall (CsField (CsThis, name), arg_exprs) in
				let is_void = ExtType.is_void (follow ret) in
				let body_stmt = if is_void then CsExprStmt call_expr else CsReturn (Some call_expr) in
				Some [body_stmt]
			end else begin
				match cf.cf_expr with
				| Some e ->
					(* For override methods with erased parameters, we need to:
					   1. Use shadow names in the body (so TLocal accesses the casted variable)
					   2. Prepend cast statements: var _hx_param = (HaxeType)param; *)
					let param_cs_names_with_shadows = List.map (fun name ->
						(* Check if this param needs shadowing *)
						match List.find_opt (fun (shadow, orig, _) -> orig = name) override_param_casts with
						| Some (shadow, _, _) -> shadow
						| None -> name
					) param_cs_names in
					let cast_stmts = List.map (fun (shadow_name, param_name, haxe_cs) ->
						CsVarDecl (shadow_name, Some haxe_cs, Some (CsCast (haxe_cs, CsLocal param_name)))
					) override_param_casts in
					let body_stmts = generate_method_body gctx ~param_cs_names:param_cs_names_with_shadows ~type_params_in_scope:all_type_params_in_scope ~type_param_constraints:all_type_param_constraints ~return_type:ret ~class_path:c.cl_path ~method_name:cf.cf_name e in
					let all_stmts = cast_stmts @ body_stmts in
					(* Wrap in unchecked if the expression contains non-zero integer constants *)
					if needs_unchecked e then
						Some [CsUncheckedStmt (CsBlock all_stmts)]
					else
						Some all_stmts
				| None -> None
			end
		in
		(* Add virtual/override/abstract modifiers for instance methods *)
		(* toString method needs override because HaxeObject defines a virtual toString().
		   But only for classes that inherit from HaxeObject (not native C# classes like Exception). *)
		let inherits_from_haxe_object =
			(* If no explicit superclass, it inherits from HaxeObject implicitly *)
			match c.cl_super with
			| None -> not (has_class_flag c CExtern)
			| Some (super_class, _) -> not (has_class_flag super_class CExtern)
		in
		let is_tostring_override = cf.cf_name = "toString" && not is_static && inherits_from_haxe_object in
		let method_modifiers =
			if is_static then modifiers
			else if is_override cf || is_tostring_override then modifiers @ [MemberModifier.Override]
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
		(* Dynamic method - generate as haxe.lang.Function field (or property if implementing interface)
		   cs_type_of_type handles TFun -> haxe.lang.Function *)
		(* C# field initializers cannot use 'this', so skip initializer if it contains 'this'.
		   The initialization will be moved to the constructor by generate_cs_class. *)
		let value = match cf.cf_expr with
			| Some e when not (expr_contains_this e) ->
				(* Apply Null<T> syntax filter to field initializer *)
				let e = CsNullable.filter gctx.com e in
				let ectx = create_expr_context gctx in
				ectx.current_class_path <- Some c.cl_path;
				ectx.current_method_name <- Some cf.cf_name;
				Some (cs_expr_of_texpr ectx e)
			| _ -> None
		in
		if field_implements_interface_property c cf then
			(* Generate as property to implement interface *)
			Some (CsMemberProperty {
				prop_name = name;
				prop_type = cs_type;
				prop_access = AccessModifier.Public;
				prop_modifiers = modifiers;
				prop_getter = Some { acc_access = None; acc_body = None };
				prop_setter = Some { acc_access = None; acc_body = None };
				prop_init = value;
				prop_explicit_interface = None;
			})
		else
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

(* ====== Constructor generation ====== *)

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

	(* Extract tf_args for optional parameter defaults.
	   Must be done before extract_super_call strips the TFunction wrapper. *)
	let tf_args = match cf.cf_expr with
		| Some { eexpr = TFunction tf } -> tf.tf_args
		| _ -> []
	in

	(* Extract super call and body from constructor expression *)
	let (super_args, body_expr) = match cf.cf_expr with
		| Some e -> extract_super_call e
		| None -> (None, None)
	in

	(* Check if this constructor needs two-phase construction (this-before-super pattern).
	   Note: Super args referencing body locals is handled separately with IIFE lambdas. *)
	let is_two_phase = needs_two_phase_construction cf in
	let parent_is_two_phase = parent_needs_two_phase_construction c in
	(* Get base class constructor types for casting super args.
	   We need to apply type parameter substitution when extending generic classes. *)
	let base_ctor_types = match c.cl_super with
		| Some (sc, tl) ->
			begin match sc.cl_constructor with
			| Some ctor_cf ->
				begin match follow ctor_cf.cf_type with
				| TFun (base_args, _) ->
					(* Apply type parameter substitution: sc.cl_params -> tl *)
					let param_map = List.map2 (fun ttp t -> (ttp.ttp_type, t)) sc.cl_params tl in
					List.map (fun (_, _, t) ->
						let substituted_t = List.fold_left (fun t (from_tp, to_t) ->
							(* Replace type parameter with actual type *)
							let rec subst t = match t with
								| _ when Type.fast_eq t from_tp -> to_t
								| _ -> Type.map subst t
							in
							subst t
						) t param_map in
						cs_type_of_type gctx substituted_t
					) base_args
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
			(* Generate preamble for optional parameters with default values *)
			let default_preamble = if tf_args = [] then [] else begin
				let ectx = create_expr_context gctx in
				ectx.current_class_path <- Some c.cl_path;
				ectx.current_method_name <- Some "_hx_new";
				List.iter (fun (v, _) ->
					let param_name = escape_identifier v.v_name in
					ectx.local_vars <- (v.v_id, param_name) :: ectx.local_vars;
					ectx.used_names <- param_name :: ectx.used_names
				) tf_args;
				generate_optional_param_defaults ectx gctx tf_args
			end in
			field_init_stmts @ base_new_call @ default_preamble @ body_stmts
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

		(* Check if super args reference body-defined locals - if so, we need special handling *)
		let args_ref_body_locals = match super_args with
			| Some args -> super_args_reference_body_locals args body_expr
			| None -> false
		in

		(* If args reference body locals, extract those statements and adjust body_expr *)
		let (dependency_stmts, adjusted_body_expr) =
			if args_ref_body_locals then
				match super_args with
				| Some args ->
					let local_ids = collect_locals_used args in
					extract_local_dependencies body_expr local_ids
				| None -> ([], body_expr)
			else
				([], body_expr)
		in

		(* Convert super args to base call, casting to expected types.
		   If args reference body locals, wrap in IIFE to define locals first. *)
		let base_call = match super_args with
			| Some args when not parent_is_two_phase ->
				(* Normal case: parent has regular constructor *)
				let ectx = create_expr_context gctx in
				ectx.current_class_path <- Some c.cl_path;
				ectx.current_method_name <- Some "new";
				let cs_args = List.mapi (fun i arg ->
					let arg_uses_body_locals =
						let arg_local_ids = collect_locals_used [arg] in
						let body_local_ids = match body_expr with
							| Some body -> collect_locals_defined body
							| None -> []
						in
						List.exists (fun id -> List.mem id body_local_ids) arg_local_ids
					in
					if arg_uses_body_locals && dependency_stmts <> [] then begin
						(* This arg uses body-defined locals - wrap in IIFE:
						   ((Func<T>)(() => { setup_stmts; return value; }))()
						*)
						let expected_type = if i < List.length base_ctor_types then
							List.nth base_ctor_types i
						else
							cs_type_of_type gctx arg.etype
						in
						(* Generate the statements and return expression *)
						let cs_stmts = List.map (fun stmt ->
							cs_stmt_of_texpr ectx stmt
						) dependency_stmts in
						let cs_return_value = cs_expr_of_texpr ectx arg in
						let cs_return = CsReturn (Some (CsCast (expected_type, CsParens cs_return_value))) in
						let lambda_body = CsLambdaBlock (cs_stmts @ [cs_return]) in
						(* Func<T> type for the lambda *)
						let func_type = CsTypeClass ((["System"], "Func"), [expected_type]) in
						(* Build: ((Func<T>)(() => { body }))() *)
						let lambda = CsLambda ([], lambda_body) in
						let cast_lambda = CsCast (func_type, CsParens lambda) in
						CsCall (CsParens cast_lambda, [])
					end else begin
						let cs_arg = cs_expr_of_texpr ectx arg in
						(* Cast to expected type if we know it *)
						if i < List.length base_ctor_types then
							let expected_type = List.nth base_ctor_types i in
							CsCast (expected_type, CsParens cs_arg)
						else
							cs_arg
					end
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

		(* Generate preamble for optional parameters with default values *)
		let default_preamble = if tf_args = [] then [] else begin
			let ectx = create_expr_context gctx in
			ectx.current_class_path <- Some c.cl_path;
			ectx.current_method_name <- Some "new";
			List.iter (fun (v, _) ->
				let param_name = escape_identifier v.v_name in
				ectx.local_vars <- (v.v_id, param_name) :: ectx.local_vars;
				ectx.used_names <- param_name :: ectx.used_names
			) tf_args;
			generate_optional_param_defaults ectx gctx tf_args
		end in

		(* Generate constructor body - prepend field initializations that contain 'this'.
		   Use adjusted_body_expr which has dependency statements removed (they're in the IIFE). *)
		let ctor_body = match adjusted_body_expr with
			| Some e -> field_init_stmts @ extra_body_stmts @ default_preamble @ generate_method_body gctx ~type_params_in_scope:class_type_params ~type_param_constraints:class_constraints ~class_path:c.cl_path ~method_name:"new" e
			| None -> field_init_stmts @ extra_body_stmts @ default_preamble
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

(* ====== Reflection & AOT accessors ====== *)

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

(* Value type for method closure dispatchers *)
let function_value_type = CsTypeClass ((["haxe"; "lang"], "Value"), [])

(* InstanceMethodFunction type for AOT-safe method closures *)
let instance_method_func_type = CsTypeClass ((["haxe"; "lang"], "InstanceMethodFunction"), [])

(* Generate _hx_getField, _hx_setField, _hx_getFields, method closure infrastructure for AOT compatibility *)
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

		(* Get list of instance methods (MethNormal and MethInline only, not MethDynamic)
		   IMPORTANT: Exclude generic methods (cf.cf_params <> []) because MethodClosure
		   cannot handle type parameters - they're only known at the call site. *)

		(* Helper to get parent method's return type for override methods.
		   For overrides, C# requires the return type to match the parent exactly. *)
		let get_parent_return_type cf =
			if not (has_class_field_flag cf CfOverride) then None
			else
				let rec find_parent_type c_super =
					try
						let cf_super = PMap.find cf.cf_name c_super.cl_fields in
						match cf_super.cf_kind with
						| Method _ ->
							begin match follow cf_super.cf_type with
							| TFun (_, ret) -> Some ret
							| _ -> None
							end
						| _ -> None
					with Not_found ->
						match c_super.cl_super with
						| Some (grandparent, _) -> find_parent_type grandparent
						| None -> None
				in
				match c.cl_super with
				| Some (c_super, _) -> find_parent_type c_super
				| None -> None
		in

		let instance_methods = List.filter_map (fun cf ->
			match cf.cf_kind with
			| Method (MethNormal | MethInline) when not (has_class_field_flag cf CfStatic) && cf.cf_params = [] ->
				let args, ret = match follow cf.cf_type with
					| TFun (args, ret) -> args, ret
					| _ -> [], t_dynamic
				in
				(* For override methods, use parent's return type to match the C# method signature *)
				let ret = match get_parent_return_type cf with
					| Some parent_ret -> parent_ret
					| None -> ret
				in
				let arity = List.length args in
				Some (cf.cf_name, get_native_field_name cf, arity, args, ret)
			| _ -> None
		) c.cl_ordered_fields in

		(* Compute ancestor method count for hierarchical index offset.
		   Each class's method indices start after all ancestor indices,
		   preventing collisions when InstanceMethodFunction uses virtual _hx_invokeMethodN dispatch. *)
		let ancestor_method_count = compute_ancestor_method_count c in

		(* Assign sequential indexes to methods, offset by ancestor method count *)
		let indexed_methods = List.mapi (fun idx (name, native_name, arity, args, ret) ->
			(idx + ancestor_method_count, name, native_name, arity, args, ret)
		) instance_methods in

		let method_count = List.length instance_methods in
		let field_names = List.map fst instance_fields in

		(* If no fields and no methods, return empty *)
		if instance_fields = [] && instance_methods = [] then
			[]
		else

		(* Generate _hx_closureCache field (nullable array of InstanceMethodFunction) *)
		let closure_cache_members = if method_count = 0 then [] else [
			CsMemberField {
				f_name = "_hx_closureCache";
				f_type = CsTypeArray (instance_method_func_type, None);
				f_access = AccessModifier.Private;
				f_modifiers = [];
				f_value = None;
			};
		] in

		(* Generate _hx_getMethodClosure helper method.
		   Cache uses local indices (0..method_count-1) but InstanceMethodFunction stores the
		   global index (offset by ancestor_method_count) for _hx_invokeMethodN dispatch. *)

		(* Expression to convert global index to local cache index *)
		let cache_index_expr = if ancestor_method_count = 0 then
			CsLocal "index"
		else
			CsBinop (CsOpSub, CsLocal "index", CsConst (CsConstInt (Int32.of_int ancestor_method_count)))
		in

		let get_method_closure_members = if method_count = 0 then [] else [
			CsMemberMethod {
				m_name = "_hx_getMethodClosure";
				m_return_type = instance_method_func_type;
				m_access = AccessModifier.Internal;
				m_modifiers = [];
				m_type_params = [];
				m_params = [
					{ p_name = "index"; p_type = Some CsTypeInt; p_default = None; p_modifier = None };
					{ p_name = "arity"; p_type = Some CsTypeInt; p_default = None; p_modifier = None };
				];
				m_body = Some [
					(* if (_hx_closureCache == null) _hx_closureCache = new InstanceMethodFunction[method_count]; *)
					CsIf (
						CsBinop (CsOpEq, CsField (CsThis, "_hx_closureCache"), CsConst CsConstNull),
						CsExprStmt (CsBinop (CsOpAssign,
							CsField (CsThis, "_hx_closureCache"),
							CsNewArray (instance_method_func_type, List.init method_count (fun _ -> CsConst CsConstNull))
						)),
						None
					);
					(* if (_hx_closureCache[localIdx] == null) _hx_closureCache[localIdx] = new InstanceMethodFunction(this, index, arity); *)
					CsIf (
						CsBinop (CsOpEq, CsArrayAccess (CsField (CsThis, "_hx_closureCache"), cache_index_expr), CsConst CsConstNull),
						CsExprStmt (CsBinop (CsOpAssign,
							CsArrayAccess (CsField (CsThis, "_hx_closureCache"), cache_index_expr),
							CsNew (instance_method_func_type, [CsThis; CsLocal "index"; CsLocal "arity"])
						)),
						None
					);
					(* return _hx_closureCache[localIdx]; *)
					CsReturn (Some (CsArrayAccess (CsField (CsThis, "_hx_closureCache"), cache_index_expr)));
				];
				m_constraints = [];
				m_explicit_interface = None;
				m_attributes = [];
			};
		] in

		(* Generate _hx_getField override:
		   public override object _hx_getField(string name) {
		       switch (name) {
		           case "field1": return this.field1;
		           case "method1": return _hx_getMethodClosure(0);
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
		let get_method_sections = List.map (fun (idx, name, _, arity, _, _) ->
			{
				sw_labels = [CsCaseConst (CsConst (CsConstString name))];
				sw_body = [CsReturn (Some (CsCall (CsField (CsThis, "_hx_getMethodClosure"), [CsConst (CsConstInt (Int32.of_int idx)); CsConst (CsConstInt (Int32.of_int arity))])))];
			}
		) indexed_methods in
		let get_field_default = {
			sw_labels = [CsCaseDefault];
			sw_body = [CsReturn (Some (CsCall (CsField (CsBase, "_hx_getField"), [CsLocal "name"])))];
		} in
		let all_get_sections = get_field_sections @ get_method_sections @ [get_field_default] in
		let get_field_method = if all_get_sections = [get_field_default] then None else Some (CsMemberMethod {
			m_name = "_hx_getField";
			m_return_type = CsTypeObject;
			m_access = AccessModifier.Public;
			m_modifiers = [MemberModifier.Override];
			m_type_params = [];
			m_params = [{ p_name = "name"; p_type = Some CsTypeString; p_default = None; p_modifier = None }];
			m_body = Some [CsSwitch (CsLocal "name", all_get_sections)];
			m_constraints = [];
			m_explicit_interface = None;
			m_attributes = [];
		}) in

		(* Generate _hx_setField override - only for data fields, not methods *)
		let set_field_sections = List.map (fun (name, field_type) ->
			{
				sw_labels = [CsCaseConst (CsConst (CsConstString name))];
				sw_body = [
					(* Use cast_object_to_type for proper handling of primitives (Runtime.toInt, etc.) *)
					CsExprStmt (CsBinop (CsOpAssign, CsField (CsThis, name), cast_object_to_type field_type (CsLocal "value")));
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
		let set_field_method = if instance_fields = [] then None else Some (CsMemberMethod {
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
		}) in

		(* Generate _hx_getFields override - only data fields, not methods *)
		let get_fields_method = if field_names = [] then None else
			let field_name_exprs = List.map (fun name -> CsConst (CsConstString name)) field_names in
			let native_array = CsNewArray (CsTypeObject, field_name_exprs) in
			let array_expr = make_array_from_native ArrayObject native_array haxe_array_type in
			Some (CsMemberMethod {
				m_name = "_hx_getFields";
				m_return_type = haxe_array_type;
				m_access = AccessModifier.Public;
				m_modifiers = [MemberModifier.Override];
				m_type_params = [];
				m_params = [];
				m_body = Some [CsReturn (Some array_expr)];
				m_constraints = [];
				m_explicit_interface = None;
				m_attributes = [];
			})
		in

		(* Generate _hx_invokeMethodN dispatchers for each arity used by this class's methods *)
		let methods_by_arity = Hashtbl.create 10 in
		List.iter (fun (idx, name, native_name, arity, args, ret) ->
			let current = try Hashtbl.find methods_by_arity arity with Not_found -> [] in
			Hashtbl.replace methods_by_arity arity ((idx, name, native_name, args, ret) :: current)
		) indexed_methods;

		let invoke_method_dispatchers = Hashtbl.fold (fun arity methods acc ->
			if arity > 9 then acc (* Methods with 10+ args use _hx_invokeMethodDynamic *)
			else
				let method_name = Printf.sprintf "_hx_invokeMethod%d" arity in
				(* Build switch cases for each method of this arity *)
				let cases = List.map (fun (idx, _, native_name, args, ret) ->
					(* Generate the method call with proper argument extraction from Value *)
					let call_args = List.filter_map (fun (i, (arg_name, _, t)) ->
						let fv_local = CsLocal (Printf.sprintf "a%d" (i + 1)) in
						let cs_arg_type = cs_type_of_type gctx t in
						match cs_arg_type with
						| CsTypeVoid -> None
						| _ -> Some (cast_value_to_type cs_arg_type fv_local)
					) (List.mapi (fun i arg -> (i, arg)) args) in
					let method_call = CsCall (CsField (CsThis, native_name), call_args) in
					(* Wrap result in Value *)
					let cs_ret_type = cs_type_of_type gctx ret in
					let result_expr = match cs_ret_type with
						| CsTypeVoid ->
							[CsExprStmt method_call; CsReturn (Some (CsStaticCall (function_value_type, "Missing", [])))]
						| _ ->
							[CsReturn (Some (cast_type_to_value cs_ret_type method_call))]
					in
					{
						sw_labels = [CsCaseConst (CsConst (CsConstInt (Int32.of_int idx)))];
						sw_body = result_expr;
					}
				) (List.rev methods) in (* Reverse to maintain original order *)
				let default_case = {
					sw_labels = [CsCaseDefault];
					sw_body = [CsReturn (Some (CsCall (CsField (CsBase, method_name),
						CsLocal "index" :: List.mapi (fun i _ -> CsLocal (Printf.sprintf "a%d" (i + 1))) (List.init arity (fun _ -> ()))
					)))];
				} in
				(* Build parameter list: int index, Value a1, Value a2, ... *)
				let params =
					{ p_name = "index"; p_type = Some CsTypeInt; p_default = None; p_modifier = None } ::
					List.mapi (fun i _ -> {
						p_name = Printf.sprintf "a%d" (i + 1);
						p_type = Some function_value_type;
						p_default = None;
						p_modifier = None;
					}) (List.init arity (fun _ -> ()))
				in
				let dispatcher = CsMemberMethod {
					m_name = method_name;
					m_return_type = function_value_type;
					m_access = AccessModifier.Public;
					m_modifiers = [MemberModifier.Override];
					m_type_params = [];
					m_params = params;
					m_body = Some [CsSwitch (CsLocal "index", cases @ [default_case])];
					m_constraints = [];
					m_explicit_interface = None;
					m_attributes = [];
				} in
				dispatcher :: acc
		) methods_by_arity [] in

		(* Combine all generated members *)
		let optional_members = List.filter_map (fun x -> x) [get_field_method; set_field_method; get_fields_method] in
		closure_cache_members @ get_method_closure_members @ optional_members @ invoke_method_dispatchers

(* Type for ClassMethodFunction *)
let class_method_func_type = CsTypeClass ((["haxe"; "lang"], "ClassMethodFunction"), [])

(* Type for haxe.lang.HaxeStaticFields *)
let haxe_static_fields_type = CsTypeClass ((["haxe"; "lang"], "HaxeStaticFields"), [])

(* Type for haxe.lang.StaticAccessors *)
let static_accessors_type = CsTypeClass ((["haxe"; "lang"], "StaticAccessors"), [])

(* Generate static field accessors for AOT-compatible static field/method access.
   This includes:
   - _hx_bind: registers static field accessors with HaxeStaticFields (called from Program.cs)
   - _hx_getStaticField: returns static field/method by name
   - _hx_hasStaticField: checks if static field/method exists
   - _hx_staticClosureCache: array cache for static method closures
   - _hx_getStaticMethodClosure: gets/creates cached static method closure
   Returns list of members to add to the class.
   Also records the class path in gctx.all_haxe_classes for Program.cs bind calls. *)
let generate_static_field_accessors gctx c =
	(* Get list of static fields with their C# names (handles class/member name conflicts) *)
	let static_fields = List.filter_map (fun cf ->
		match cf.cf_kind with
		| Var { v_read = AccNormal; v_write = AccNormal }
		| Var { v_read = AccNormal; v_write = AccNever } ->
			Some (cf.cf_name, get_cs_field_name c cf, cs_type_of_type gctx cf.cf_type)
		| _ -> None
	) c.cl_ordered_statics in

	(* Get list of static physical property fields (only those with backing fields, i.e. @:isVar).
	   Excludes AccNormal vars which are already in static_fields. *)
	let static_property_fields = List.filter_map (fun cf ->
		match cf.cf_kind with
		| Var { v_read = AccNormal; v_write = AccNormal }
		| Var { v_read = AccNormal; v_write = AccNever } -> None (* already in static_fields *)
		| Var _ when is_physical_var_field cf ->
			Some (cf.cf_name, get_cs_field_name c cf, cs_type_of_type gctx cf.cf_type)
		| _ -> None
	) c.cl_ordered_statics in

	(* Get list of static methods (MethNormal and MethInline only)
	   Exclude generic methods since they can't be invoked dynamically. *)
	let static_methods = List.filter_map (fun cf ->
		match cf.cf_kind with
		| Method (MethNormal | MethInline) when cf.cf_params = [] ->
			let args, ret = match follow cf.cf_type with
				| TFun (args, ret) -> args, ret
				| _ -> [], t_dynamic
			in
			let arity = List.length args in
			Some (cf.cf_name, get_cs_field_name c cf, arity, args, ret)
		| _ -> None
	) c.cl_ordered_statics in

	(* Assign sequential indexes to methods *)
	let indexed_methods = List.mapi (fun idx (name, native_name, arity, args, ret) ->
		(idx, name, native_name, arity, args, ret)
	) static_methods in

	let method_count = List.length static_methods in
	let all_data_field_names = List.map (fun (name, _, _) -> name) static_fields in
	let all_property_field_names = List.map (fun (name, _, _) -> name) static_property_fields in
	let all_method_names = List.map (fun (name, _, _, _, _) -> name) static_methods in

	(* Compute instance field names for Type.getInstanceFields() registry *)
	let all_instance_field_names =
		let instance_data_names = List.filter_map (fun cf ->
			match cf.cf_kind with
			| Var { v_read = AccNormal; v_write = AccNormal }
			| Var { v_read = AccNormal; v_write = AccNever } -> Some cf.cf_name
			| _ -> None
		) c.cl_ordered_fields in
		let instance_property_names = List.filter_map (fun cf ->
			match cf.cf_kind with
			| Var { v_read = AccNormal; v_write = AccNormal }
			| Var { v_read = AccNormal; v_write = AccNever } -> None (* already in data_names *)
			| Var _ when is_physical_var_field cf -> Some cf.cf_name
			| _ -> None
		) c.cl_ordered_fields in
		let instance_method_names = List.filter_map (fun cf ->
			match cf.cf_kind with
			| Method (MethNormal | MethInline) when not (has_class_field_flag cf CfStatic) && cf.cf_params = [] -> Some cf.cf_name
			| _ -> None
		) c.cl_ordered_fields in
		instance_data_names @ instance_property_names @ instance_method_names
	in

	(* Get C# path and type for this class *)
	let cs_path = cs_path_of_path c.cl_path in
	let cs_class_type = CsTypeClass (cs_path, []) in

	(* Record this class for Program.cs _hx_bind() calls *)
	gctx.all_haxe_classes <- cs_path :: gctx.all_haxe_classes;

	(* Determine if we need 'new' modifier on _hx_bind:
	   - Classes without explicit superclass get HaxeObject as base (which defines _hx_bind) -> need 'new'
	   - Classes with explicit Haxe superclass also have _hx_bind in the chain -> need 'new'
	   - Classes with native/extern superclass (like System.Exception) don't have _hx_bind -> no 'new' *)
	let bind_needs_new = match c.cl_super with
		| None -> true  (* No explicit parent -> gets HaxeObject as base *)
		| Some (super_class, _) -> not (has_class_flag super_class CExtern)  (* Extern parent has no _hx_bind *)
	in
	let bind_modifiers = if bind_needs_new then [MemberModifier.New; MemberModifier.Static] else [MemberModifier.Static] in

	(* --- Factory ConstructorFunctions for AOT-safe Type.createInstance / Type.createEmptyInstance --- *)
	let constructor_function_type = CsTypeClass ((["haxe"; "lang"], "ConstructorFunction"), []) in
	let is_haxe_object = extends_haxe_object c in
	let is_abstract_class = has_class_flag c CAbstract in
	(* Get constructor args and two-phase status.
	   If the class has its own constructor, use that.
	   If not, check the parent constructor (for forwarding constructors). *)
	let ctor_args, ctor_is_two_phase = match c.cl_constructor with
		| Some cf ->
			let args = (match follow cf.cf_type with TFun (args, _) -> args | _ -> []) in
			let is_two_phase = needs_two_phase_construction cf in
			(args, is_two_phase)
		| None ->
			(* No explicit constructor — check parent for forwarding constructor args *)
			let parent_args = match c.cl_super with
				| Some (sc, _) ->
					begin match sc.cl_constructor with
					| Some ctor_cf -> (match follow ctor_cf.cf_type with TFun (args, _) -> args | _ -> [])
					| None -> []
					end
				| None -> []
			in
			(parent_args, false)  (* Forwarding ctors are never two-phase from the factory's perspective *)
	in
	let factory_methods, factory_bind_stmts =
		if not is_haxe_object || is_abstract_class then ([], [])
		else
			let args_param = { p_name = "args"; p_type = Some (CsTypeArray (CsTypeObject, None)); p_default = None; p_modifier = None } in
			(* _hx_emptyFactory: return new MyClass((EmptyConstructor)null); *)
			let empty_factory_method = CsMemberMethod {
				m_name = "_hx_emptyFactory";
				m_return_type = CsTypeObject;
				m_access = AccessModifier.Private;
				m_modifiers = [MemberModifier.Static];
				m_type_params = [];
				m_params = [args_param];
				m_body = Some [CsReturn (Some (CsNew (cs_class_type, [CsCast (empty_constructor_type, CsNull)])))];
				m_constraints = [];
				m_explicit_interface = None;
				m_attributes = [];
			} in
			(* _hx_factory body depends on constructor args and two-phase status.
			   Arg casts use cast_object_to_type from csTypeCoercion.ml which handles:
			   - Numeric boxing mismatches via Runtime.toInt/toDouble/toLong/toBool
			   - Null<T> via _ofDynamic (handles null + boxing correctly)
			   - String via Runtime.toStr
			   - Other types via direct cast *)
			let factory_body =
				if ctor_is_two_phase then
					(* Two-phase: create empty, call _hx_new, return *)
					let cast_args = List.mapi (fun i (_, _, t) ->
						let arg_type = cs_type_of_type gctx t in
						cast_object_to_type arg_type (CsArrayAccess (CsLocal "args", CsConst (CsConstInt (Int32.of_int i))))
					) ctor_args in
					[
						CsVarDecl ("_hx_tmp", Some cs_class_type,
							Some (CsNew (cs_class_type, [CsCast (empty_constructor_type, CsNull)])));
						CsExprStmt (CsCall (CsField (CsLocal "_hx_tmp", "_hx_new"), cast_args));
						CsReturn (Some (CsLocal "_hx_tmp"))
					]
				else
					(* Normal: return new MyClass((Type1)args[0], ...); *)
					let cast_args = List.mapi (fun i (_, _, t) ->
						let arg_type = cs_type_of_type gctx t in
						cast_object_to_type arg_type (CsArrayAccess (CsLocal "args", CsConst (CsConstInt (Int32.of_int i))))
					) ctor_args in
					[CsReturn (Some (CsNew (cs_class_type, cast_args)))]
			in
			let factory_method = CsMemberMethod {
				m_name = "_hx_factory";
				m_return_type = CsTypeObject;
				m_access = AccessModifier.Private;
				m_modifiers = [MemberModifier.Static];
				m_type_params = [];
				m_params = [args_param];
				m_body = Some factory_body;
				m_constraints = [];
				m_explicit_interface = None;
				m_attributes = [];
			} in
			(* Bind statements: acc.emptyFactory = new ConstructorFunction(_hx_emptyFactory); etc. *)
			let set_empty_factory = CsExprStmt (CsBinop (CsOpAssign,
				CsField (CsLocal "acc", "emptyFactory"),
				CsNew (constructor_function_type, [CsStaticField (cs_class_type, "_hx_emptyFactory")])
			)) in
			let set_factory = CsExprStmt (CsBinop (CsOpAssign,
				CsField (CsLocal "acc", "factory"),
				CsNew (constructor_function_type, [CsStaticField (cs_class_type, "_hx_factory")])
			)) in
			([empty_factory_method; factory_method], [set_empty_factory; set_factory])
	in

	(* If no static data fields and no static methods,
	   generate a minimal _hx_bind that only registers field name arrays (no getter/checker) *)
	if static_fields = [] && static_methods = [] then
		let bind_body =
			(* var acc = haxe.lang.HaxeStaticFields.getOrCreate(typeof(MyClass).FullName); *)
			let get_or_create = CsVarDecl (
				"acc",
				Some static_accessors_type,
				Some (CsStaticCall (haxe_static_fields_type, "getOrCreate", [
					CsField (CsTypeOf cs_class_type, "FullName")
				]))
			) in
			(* acc.classFieldNames = new string[] { ... }; (property names only, no data fields/methods) *)
			let set_class_field_names = CsExprStmt (CsBinop (CsOpAssign,
				CsField (CsLocal "acc", "classFieldNames"),
				CsNewArray (CsTypeString,
					List.map (fun name -> CsConst (CsConstString name)) all_property_field_names)
			)) in
			(* acc.instanceFieldNames = new string[] { ... }; *)
			let set_instance_field_names = CsExprStmt (CsBinop (CsOpAssign,
				CsField (CsLocal "acc", "instanceFieldNames"),
				CsNewArray (CsTypeString,
					List.map (fun name -> CsConst (CsConstString name)) all_instance_field_names)
			)) in
			[get_or_create; set_class_field_names; set_instance_field_names] @ factory_bind_stmts
		in
		factory_methods @
		[CsMemberMethod {
			m_name = "_hx_bind";
			m_return_type = CsTypeVoid;
			m_access = AccessModifier.Public;
			m_modifiers = bind_modifiers;
			m_type_params = [];
			m_params = [];
			m_body = Some bind_body;
			m_constraints = [];
			m_explicit_interface = None;
			m_attributes = [];
		}]
	else

	(* Check if any superclass has static fields/methods that would generate static accessors.
	   Only then do we need the 'new' modifier for _hx_getStaticField/_hx_hasStaticField,
	   since static methods hide (not override) in C#. *)
	let rec super_has_static_fields c =
		match c.cl_super with
		| None -> false
		| Some (super_class, _) ->
			let super_has = List.exists (fun cf ->
				match cf.cf_kind with
				| Var { v_read = AccNormal; v_write = AccNormal }
				| Var { v_read = AccNormal; v_write = AccNever } -> true
				| Method (MethNormal | MethInline) when cf.cf_params = [] -> true
				| _ -> false
			) super_class.cl_ordered_statics in
			super_has || super_has_static_fields super_class
	in
	let accessor_needs_new = super_has_static_fields c in
	let static_accessor_modifiers = if accessor_needs_new then [MemberModifier.New; MemberModifier.Static] else [MemberModifier.Static] in

	(* Generate _hx_staticClosureCache field (nullable array of ClassMethodFunction) *)
	let closure_cache_members = if method_count = 0 then [] else [
		CsMemberField {
			f_name = "_hx_staticClosureCache";
			f_type = CsTypeArray (class_method_func_type, None);
			f_access = AccessModifier.Private;
			f_modifiers = [MemberModifier.Static];
			f_value = None;
		};
	] in

	(* Generate _hx_getStaticMethodClosure helper method with direct lambdas:
	   private static haxe.lang.ClassMethodFunction _hx_getStaticMethodClosure(int index) {
	       if (_hx_staticClosureCache == null)
	           _hx_staticClosureCache = new haxe.lang.ClassMethodFunction[N];
	       if (_hx_staticClosureCache[index] == null) {
	           switch (index) {
	               case 0: _hx_staticClosureCache[0] = new ClassMethodFunction(() => Value.FromInt(method0())); break;
	               case 1: _hx_staticClosureCache[1] = new ClassMethodFunction((a1, a2) => Value.FromObject(method1(...))); break;
	               ...
	           }
	       }
	       return _hx_staticClosureCache[index];
	   }
	*)
	let get_static_method_closure_members = if method_count = 0 then [] else
		(* Build switch cases for closure creation - one case per method *)
		let creation_cases = List.map (fun (idx, _, native_name, arity, args, ret) ->
			(* Generate lambda parameters: a1, a2, ... (all of type Value) *)
			let lambda_params = List.mapi (fun i _ -> {
				p_name = Printf.sprintf "a%d" (i + 1);
				p_type = None;  (* Inferred *)
				p_default = None;
				p_modifier = None;
			}) (List.init arity (fun _ -> ())) in
			(* Generate argument extraction and method call *)
			let call_args = List.filter_map (fun (i, (_, _, t)) ->
				let fv_local = CsLocal (Printf.sprintf "a%d" (i + 1)) in
				let cs_arg_type = cs_type_of_type gctx t in
				match cs_arg_type with
				| CsTypeVoid -> None
				| _ -> Some (cast_value_to_type cs_arg_type fv_local)
			) (List.mapi (fun i arg -> (i, arg)) args) in
			let method_call = CsCall (CsStaticField (cs_class_type, native_name), call_args) in
			(* Wrap result in Value *)
			let cs_ret_type = cs_type_of_type gctx ret in
			let result_expr = match cs_ret_type with
				| CsTypeVoid ->
					CsCall (CsStaticField (function_value_type, "Missing"), [])
				| _ ->
					cast_type_to_value cs_ret_type method_call
			in
			(* For void methods, we need a block lambda that calls the method then returns *)
			let lambda_body = match cs_ret_type with
				| CsTypeVoid -> CsLambdaBlock [CsExprStmt method_call; CsReturn (Some result_expr)]
				| _ -> CsLambdaExpr result_expr
			in
			let lambda = CsLambda (lambda_params, lambda_body) in
			(* Create the closure assignment *)
			let closure_creation = CsExprStmt (CsBinop (CsOpAssign,
				CsArrayAccess (CsStaticField (cs_class_type, "_hx_staticClosureCache"), CsConst (CsConstInt (Int32.of_int idx))),
				CsNew (class_method_func_type, [lambda])
			)) in
			{
				sw_labels = [CsCaseConst (CsConst (CsConstInt (Int32.of_int idx)))];
				sw_body = [closure_creation; CsBreak];
			}
		) indexed_methods in
		[CsMemberMethod {
			m_name = "_hx_getStaticMethodClosure";
			m_return_type = class_method_func_type;
			m_access = AccessModifier.Internal;
			m_modifiers = [MemberModifier.Static];
			m_type_params = [];
			m_params = [{ p_name = "index"; p_type = Some CsTypeInt; p_default = None; p_modifier = None }];
			m_body = Some [
				(* if (_hx_staticClosureCache == null) _hx_staticClosureCache = new ClassMethodFunction[method_count]; *)
				CsIf (
					CsBinop (CsOpEq, CsStaticField (cs_class_type, "_hx_staticClosureCache"), CsConst CsConstNull),
					CsExprStmt (CsBinop (CsOpAssign,
						CsStaticField (cs_class_type, "_hx_staticClosureCache"),
						CsNewArray (class_method_func_type, List.init method_count (fun _ -> CsConst CsConstNull))
					)),
					None
				);
				(* if (_hx_staticClosureCache[index] == null) { switch ... } *)
				CsIf (
					CsBinop (CsOpEq, CsArrayAccess (CsStaticField (cs_class_type, "_hx_staticClosureCache"), CsLocal "index"), CsConst CsConstNull),
					CsSwitch (CsLocal "index", creation_cases),
					None
				);
				(* return _hx_staticClosureCache[index]; *)
				CsReturn (Some (CsArrayAccess (CsStaticField (cs_class_type, "_hx_staticClosureCache"), CsLocal "index")));
			];
			m_constraints = [];
			m_explicit_interface = None;
			m_attributes = [];
		}]
	in

	(* Generate _hx_getStaticField:
	   public static object _hx_getStaticField(string name) {
	       switch (name) {
	           case "field1": return field1;
	           case "method1": return _hx_getStaticMethodClosure(0);
	           ...
	           default: return null;
	       }
	   }
	*)
	let get_field_sections = List.map (fun (name, native_name, _) ->
		{
			sw_labels = [CsCaseConst (CsConst (CsConstString name))];
			sw_body = [CsReturn (Some (CsStaticField (cs_class_type, native_name)))];
		}
	) static_fields in
	let get_method_sections = List.map (fun (idx, name, _, _, _, _) ->
		{
			sw_labels = [CsCaseConst (CsConst (CsConstString name))];
			sw_body = [CsReturn (Some (CsCall (CsStaticField (cs_class_type, "_hx_getStaticMethodClosure"), [CsConst (CsConstInt (Int32.of_int idx))])))];
		}
	) indexed_methods in
	let get_field_default = {
		sw_labels = [CsCaseDefault];
		sw_body = [CsReturn (Some (CsConst CsConstNull))];
	} in
	let all_get_sections = get_field_sections @ get_method_sections @ [get_field_default] in
	let get_static_field_method = CsMemberMethod {
		m_name = "_hx_getStaticField";
		m_return_type = CsTypeObject;
		m_access = AccessModifier.Public;
		m_modifiers = static_accessor_modifiers;
		m_type_params = [];
		m_params = [{ p_name = "name"; p_type = Some CsTypeString; p_default = None; p_modifier = None }];
		m_body = Some [CsSwitch (CsLocal "name", all_get_sections)];
		m_constraints = [];
		m_explicit_interface = None;
		m_attributes = [];
	} in

	(* Generate _hx_hasStaticField:
	   public static bool _hx_hasStaticField(string name) {
	       switch (name) {
	           case "field1": case "method1": ... return true;
	           default: return false;
	       }
	   }
	*)
	let all_names = all_data_field_names @ all_method_names in
	let has_field_sections = if all_names = [] then [] else [{
		sw_labels = List.map (fun name -> CsCaseConst (CsConst (CsConstString name))) all_names;
		sw_body = [CsReturn (Some (CsConst (CsConstBool true)))];
	}] in
	let has_field_default = {
		sw_labels = [CsCaseDefault];
		sw_body = [CsReturn (Some (CsConst (CsConstBool false)))];
	} in
	let has_static_field_method = CsMemberMethod {
		m_name = "_hx_hasStaticField";
		m_return_type = CsTypeBool;
		m_access = AccessModifier.Public;
		m_modifiers = static_accessor_modifiers;
		m_type_params = [];
		m_params = [{ p_name = "name"; p_type = Some CsTypeString; p_default = None; p_modifier = None }];
		m_body = Some [CsSwitch (CsLocal "name", has_field_sections @ [has_field_default])];
		m_constraints = [];
		m_explicit_interface = None;
		m_attributes = [];
	} in

	(* Generate _hx_bind method for HaxeStaticFields registration.
	   This is called from Program.cs before main() to ensure all static accessors are registered.
	   public static void _hx_bind() {
	       var acc = haxe.lang.HaxeStaticFields.getOrCreate(typeof(MyClass).FullName);
	       acc.getter = _hx_getStaticField;
	       acc.checker = _hx_hasStaticField;
	       acc.fieldNames = new string[] { ... };
	   }
	*)
	let bind_body =
		(* var acc = haxe.lang.HaxeStaticFields.getOrCreate(typeof(MyClass).FullName); *)
		let get_or_create = CsVarDecl (
			"acc",
			Some static_accessors_type,
			Some (CsStaticCall (haxe_static_fields_type, "getOrCreate", [
				CsField (CsTypeOf cs_class_type, "FullName")
			]))
		) in
		(* acc.getter = _hx_getStaticField; *)
		let set_getter = CsExprStmt (CsBinop (CsOpAssign,
			CsField (CsLocal "acc", "getter"),
			CsStaticField (cs_class_type, "_hx_getStaticField")
		)) in
		(* acc.checker = _hx_hasStaticField; *)
		let set_checker = CsExprStmt (CsBinop (CsOpAssign,
			CsField (CsLocal "acc", "checker"),
			CsStaticField (cs_class_type, "_hx_hasStaticField")
		)) in
		(* acc.classFieldNames = new string[] { ... }; *)
		let set_class_field_names = CsExprStmt (CsBinop (CsOpAssign,
			CsField (CsLocal "acc", "classFieldNames"),
			CsNewArray (CsTypeString,
				List.map (fun name -> CsConst (CsConstString name)) (all_data_field_names @ all_property_field_names @ all_method_names))
		)) in
		(* acc.instanceFieldNames = new string[] { ... }; *)
		let set_instance_field_names = CsExprStmt (CsBinop (CsOpAssign,
			CsField (CsLocal "acc", "instanceFieldNames"),
			CsNewArray (CsTypeString,
				List.map (fun name -> CsConst (CsConstString name)) all_instance_field_names)
		)) in
		[get_or_create; set_getter; set_checker; set_class_field_names; set_instance_field_names] @ factory_bind_stmts
	in
	let bind_method = CsMemberMethod {
		m_name = "_hx_bind";
		m_return_type = CsTypeVoid;
		m_access = AccessModifier.Public;
		m_modifiers = bind_modifiers;
		m_type_params = [];
		m_params = [];
		m_body = Some bind_body;
		m_constraints = [];
		m_explicit_interface = None;
		m_attributes = [];
	} in

	(* Combine all generated members *)
	let members = closure_cache_members @ get_static_method_closure_members @
		[get_static_field_method; has_static_field_method; bind_method] @ factory_methods in
	members

(* =============================================================================
   Specialized Map Explicit Interface Implementations
   =============================================================================
   StringMap, IntMap, ObjectMap have typed public APIs (e.g., get(string key))
   but implement the non-generic IMap interface which has object parameters.
   We generate explicit interface implementations as bridge methods.
   Only methods that exist in IMap (after DCE) get bridge implementations.
   ============================================================================= *)

(* Returns the C# key type if this is a specialized map that needs explicit IMap implementations *)
let get_imap_key_type_for_class path =
	match path with
	| (["haxe"; "ds"], "StringMap") -> Some CsTypeString
	| (["haxe"; "ds"], "IntMap") -> Some CsTypeInt
	| (["haxe"; "ds"], "ObjectMap") -> Some CsTypeObject
	| _ -> None

(* Get the IMap interface from the class's implements list, if present *)
let get_imap_interface c =
	List.find_map (fun (iface, _) ->
		if iface.cl_path = (["haxe"], "IMap") || iface.cl_path = (["haxe"; "Constraints"], "IMap") then
			Some iface
		else
			None
	) c.cl_implements

(* Check if the IMap interface has a method with the given name *)
let imap_has_method imap method_name =
	try
		let cf = PMap.find method_name imap.cl_fields in
		match cf.cf_kind with Method _ -> true | _ -> false
	with Not_found -> false

(* Generate explicit IMap interface implementations for specialized maps.
   These bridge methods cast the object parameter to the typed key and delegate.
   Only generates bridges for methods that exist in the IMap interface (after DCE). *)
let generate_explicit_imap_implementations key_type imap =
	let imap_type = CsTypeClass ((["haxe"], "IMap"), []) in
	(* Cast expression for key - identity if key_type is object *)
	let cast_key expr =
		if key_type = CsTypeObject then expr
		else CsCast (key_type, expr)
	in
	(* Define all possible bridge methods *)
	let all_bridges = [
		(* object IMap.get(object k) => this.get((KeyType)k) *)
		("get", CsMemberMethod {
			m_name = "get";
			m_return_type = CsTypeObject;
			m_access = AccessModifier.Public;
			m_modifiers = [];
			m_type_params = [];
			m_params = [{ p_name = "k"; p_type = Some CsTypeObject; p_default = None; p_modifier = None }];
			m_body = Some [CsReturn (Some (CsCall (CsField (CsThis, "get"), [cast_key (CsLocal "k")])))];
			m_constraints = [];
			m_explicit_interface = Some imap_type;
			m_attributes = [];
		});
		(* void IMap.set(object k, object v) => this.set((KeyType)k, v) *)
		("set", CsMemberMethod {
			m_name = "set";
			m_return_type = CsTypeVoid;
			m_access = AccessModifier.Public;
			m_modifiers = [];
			m_type_params = [];
			m_params = [
				{ p_name = "k"; p_type = Some CsTypeObject; p_default = None; p_modifier = None };
				{ p_name = "v"; p_type = Some CsTypeObject; p_default = None; p_modifier = None }
			];
			m_body = Some [CsExprStmt (CsCall (CsField (CsThis, "set"), [cast_key (CsLocal "k"); CsLocal "v"]))];
			m_constraints = [];
			m_explicit_interface = Some imap_type;
			m_attributes = [];
		});
		(* bool IMap.exists(object k) => this.exists((KeyType)k) *)
		("exists", CsMemberMethod {
			m_name = "exists";
			m_return_type = CsTypeBool;
			m_access = AccessModifier.Public;
			m_modifiers = [];
			m_type_params = [];
			m_params = [{ p_name = "k"; p_type = Some CsTypeObject; p_default = None; p_modifier = None }];
			m_body = Some [CsReturn (Some (CsCall (CsField (CsThis, "exists"), [cast_key (CsLocal "k")])))];
			m_constraints = [];
			m_explicit_interface = Some imap_type;
			m_attributes = [];
		});
		(* bool IMap.remove(object k) => this.remove((KeyType)k) *)
		("remove", CsMemberMethod {
			m_name = "remove";
			m_return_type = CsTypeBool;
			m_access = AccessModifier.Public;
			m_modifiers = [];
			m_type_params = [];
			m_params = [{ p_name = "k"; p_type = Some CsTypeObject; p_default = None; p_modifier = None }];
			m_body = Some [CsReturn (Some (CsCall (CsField (CsThis, "remove"), [cast_key (CsLocal "k")])))];
			m_constraints = [];
			m_explicit_interface = Some imap_type;
			m_attributes = [];
		});
		(* object IMap.keys() => this.keys() *)
		("keys", CsMemberMethod {
			m_name = "keys";
			m_return_type = CsTypeObject;
			m_access = AccessModifier.Public;
			m_modifiers = [];
			m_type_params = [];
			m_params = [];
			m_body = Some [CsReturn (Some (CsCall (CsField (CsThis, "keys"), [])))];
			m_constraints = [];
			m_explicit_interface = Some imap_type;
			m_attributes = [];
		});
		(* object IMap.iterator() => this.iterator() *)
		("iterator", CsMemberMethod {
			m_name = "iterator";
			m_return_type = CsTypeObject;
			m_access = AccessModifier.Public;
			m_modifiers = [];
			m_type_params = [];
			m_params = [];
			m_body = Some [CsReturn (Some (CsCall (CsField (CsThis, "iterator"), [])))];
			m_constraints = [];
			m_explicit_interface = Some imap_type;
			m_attributes = [];
		});
	] in
	(* Filter to only methods that exist in IMap interface *)
	List.filter_map (fun (name, member) ->
		if imap_has_method imap name then Some member else None
	) all_bridges

(* ====== Type generation ====== *)

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
	(* Use cs_type_of_type on reconstructed TInst to get proper type parameter erasure *)
	let base_class = match c.cl_super with
		| Some (sc, params) ->
			Some (cs_type_of_type gctx (TInst (sc, params)))
		| None ->
			(* All Haxe classes extend HaxeObject for _hx_getField support *)
			Some (CsTypeClass ((["haxe"; "root"], "HaxeObject"), []))
	in

	(* Generate interface references *)
	(* Use cs_type_of_type on reconstructed TInst to get proper type parameter erasure *)
	let interfaces = List.map (fun (i, params) ->
		cs_type_of_type gctx (TInst (i, params))
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
				(* Apply Null<T> syntax filter to field initializer *)
				let e = CsNullable.filter gctx.com e in
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
		else begin
			(* No forwarding ctors from parent. Generate a default parameterless constructor.
			   This is always needed because we also generate an EmptyConstructor which
			   prevents C# from creating an implicit parameterless constructor.
			   field_init_stmts may be empty, in which case the body is just []. *)
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

	(* Add EmptyConstructor for AOT factory support (createEmptyInstance / createInstance).
	   Two-phase classes already have an EmptyConstructor, so skip those. *)
	let ctor_is_two_phase = match c.cl_constructor with
		| Some cf -> needs_two_phase_construction cf
		| None -> false
	in
	if extends_haxe_object c && not ctor_is_two_phase then begin
		let empty_ctor_param = {
			p_name = "_";
			p_type = Some empty_constructor_type;
			p_default = None;
			p_modifier = None;
		} in
		let empty_base_call =
			if c.cl_super <> None then
				Some [CsCast (empty_constructor_type, CsNull)]
			else
				None
		in
		(* Use Private for sealed classes to avoid CS0628 warning *)
		let empty_ctor_access = if has_class_flag c CFinal then AccessModifier.Private else AccessModifier.Protected in
		members := CsMemberConstructor {
			ctor_access = empty_ctor_access;
			ctor_modifiers = [];
			ctor_params = [empty_ctor_param];
			ctor_base_call = empty_base_call;
			ctor_this_call = None;
			ctor_body = [];
		} :: !members
	end;

	(* Fields *)
	List.iter (fun cf ->
		match generate_field gctx c cf false with
		| Some m ->
			members := m :: !members;
			(* Generate explicit interface implementations for covariant return types *)
			members := generate_explicit_interface_impls gctx c cf @ !members;
			(* Generate overload for contravariant overrides (method accepts wider type than parent) *)
			members := generate_contravariant_override_overload gctx c cf @ !members
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

	(* Generate static field accessors for AOT-compatible static field/method access *)
	let static_accessor_members = generate_static_field_accessors gctx c in
	members := static_accessor_members @ !members;

	(* NOTE: Explicit IMap implementations for specialized maps (StringMap, IntMap, ObjectMap)
	   are now generated by the general find_variant_interface_methods / generate_explicit_interface_impls
	   mechanism, which handles all interface method signature mismatches uniformly. *)

	(* Generate static constructor if there's __init__ code *)
	let class_init_stmts = match TClass.get_cl_init c with
		| Some e ->
			let ectx = create_expr_context gctx in
			ectx.current_class_path <- Some c.cl_path;
			ectx.current_method_name <- Some "__init__";
			let stmts = match e.eexpr with
				| TBlock exprs -> List.map (cs_stmt_of_texpr ectx) exprs
				| _ -> [cs_stmt_of_texpr ectx e]
			in
			(* Filter out pure expressions that would become invalid statements *)
			List.filter (function
				| CsExprStmt (CsConst _) -> false
				| CsExprStmt (CsLocal _) -> false
				| CsBlock [] -> false
				| _ -> true
			) stmts
		| None -> []
	in
	if class_init_stmts <> [] then
		members := CsMemberStaticConstructor class_init_stmts :: !members;

	(* Type parameter erasure: ALL Haxe generic classes become non-generic in C# output.
	   This is the universal type erasure strategy (like Java's type erasure).
	   Type parameters only exist at Haxe compile-time for type checking.
	   In C#, fields/parameters of type T become object, and casts are inserted when needed. *)
	let type_params = [] in
	let type_constraints = [] in
	ignore c.cl_params; (* Suppress unused warning - params are intentionally erased *)

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

	(* Generate base interfaces - use cs_type_of_type which handles type erasure correctly *)
	let base_interfaces = List.map (fun (i, params) ->
		cs_type_of_type gctx (TInst (i, params))
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
			(* Get method-level type parameters - only for C# native interfaces.
			   For Haxe interfaces, method type params are erased. *)
			let method_type_params =
				if CsTypeMapping.is_cs_native_generic_class c.cl_path then
					List.map (fun ttp -> ttp.ttp_name) cf.cf_params
				else
					[]
			in
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
		| Method MethDynamic ->
			(* Dynamic method is a variable holding a function - generate as property *)
			Some (CsMemberProperty {
				prop_name = escape_identifier cf.cf_name;
				prop_type = CsTypeClass ((["haxe"; "lang"], "Function"), []);
				prop_access = AccessModifier.Public;
				prop_modifiers = [];
				prop_getter = Some { acc_access = None; acc_body = None };
				prop_setter = Some { acc_access = None; acc_body = None };
				prop_init = None;
				prop_explicit_interface = None;
			})
		| _ -> None
	) c.cl_ordered_fields in

	(* Type parameter erasure: ALL Haxe generic interfaces become non-generic in C# output.
	   This is the universal type erasure strategy (like Java's type erasure). *)
	let type_params = [] in
	ignore c.cl_params; (* Suppress unused warning - params are intentionally erased *)

	(* Collect interface instance field names for the HaxeStaticFields registry.
	   Since C# interfaces can't have static methods, we can't add _hx_bind() directly.
	   Instead, we store the names here and emit registration code in Program.cs. *)
	let interface_field_names = List.filter_map (fun cf ->
		match cf.cf_kind with
		| Var { v_read = AccNormal; v_write = AccNormal }
		| Var { v_read = AccNormal; v_write = AccNever }
		| Var { v_read = AccNormal; v_write = AccNo } -> Some cf.cf_name
		| Var { v_read = AccNo | AccNever; v_write = AccNormal } -> Some cf.cf_name
		| Method MethDynamic -> Some cf.cf_name
		| Method (MethNormal | MethInline) when cf.cf_params = [] -> Some cf.cf_name
		| _ -> None
	) c.cl_ordered_fields in
	gctx.all_haxe_interfaces <- (path, interface_field_names) :: gctx.all_haxe_interfaces;

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
	let enum_name = snd path in
	(* Helper: escape enum constructor name, adding suffix if it matches enum type name *)
	let escape_ctor_name name =
		let esc = escape_identifier name in
		if esc = enum_name then esc ^ "_" else esc
	in

	(* Type parameter erasure: ALL Haxe generic enums become non-generic in C# output.
	   This is the universal type erasure strategy (like Java's type erasure). *)
	let type_params = [] in
	let type_param_refs = [] in
	ignore e.e_params; (* Suppress unused warning - params are intentionally erased *)

	(* NOTE: We always generate Haxe enums as classes (not C# enums) to preserve
	   null semantics. In Haxe, all enums are reference types and can be null.
	   C# enums are value types and cannot be null.

	   TODO: Consider generating C# enums for simple cases when @:native or
	   similar metadata is present for explicit C# interop. *)
	let _ (* is_simple *) = PMap.fold (fun ef acc ->
		acc && (match ef.ef_type with TFun _ -> false | _ -> true)
	) e.e_constrs true in

	(* Always generate as abstract class with nested classes or static fields *)
		(* Generate as abstract class with nested classes *)
		let members = PMap.fold (fun ef acc ->
			match ef.ef_type with
			| TFun (args, _) ->
				(* Nested class for constructor with parameters *)
				let class_name = escape_ctor_name ef.ef_name in
				(* GADT support: constructor may have its own type parameters (ef.ef_params)
				   With type erasure, these also become non-generic. *)
				let nested_type_params = [] in
				ignore ef.ef_params; (* Suppress unused warning - params are intentionally erased *)
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
				(* Generate ToString() method for proper enum string representation *)
				let tostring_method =
					(* Build: return "ConstructorName(" + Std.string(param1) + ", " + Std.string(param2) + ... + ")" *)
					let std_path = (["haxe"; "root"], "Std") in
					let param_names = List.map (fun (name, _, _) -> escape_identifier name) args in
					let tostring_body = match param_names with
						| [] ->
							(* No params: return "ConstructorName" *)
							[CsReturn (Some (CsConst (CsConstString ef.ef_name)))]
						| _ ->
							(* With params: return "ConstructorName(param1, param2, ...)" *)
							let param_strings = List.map (fun pname ->
								CsStaticCall (CsTypeClass (std_path, []), "string", [CsField (CsThis, pname)])
							) param_names in
							(* Join with "," *)
							let joined = List.fold_left (fun acc ps ->
								CsBinop (CsOpAdd, CsBinop (CsOpAdd, acc, CsConst (CsConstString ",")), ps)
							) (List.hd param_strings) (List.tl param_strings) in
							(* Wrap with constructor name and parentheses *)
							let result = CsBinop (CsOpAdd,
								CsBinop (CsOpAdd,
									CsConst (CsConstString (ef.ef_name ^ "(")),
									joined),
								CsConst (CsConstString ")")) in
							[CsReturn (Some result)]
					in
					CsMemberMethod {
						m_name = "ToString";
						m_return_type = CsTypeString;
						m_access = AccessModifier.Public;
						m_modifiers = [MemberModifier.Override];
						m_type_params = [];
						m_params = [];
						m_body = Some tostring_body;
						m_constraints = [];
						m_explicit_interface = None;
						m_attributes = [];
					}
				in
				(* Nested class has parent's type params + constructor's own type params (GADT) *)
				let nested_class = CsClassDef {
					c_path = (fst path, class_name);
					c_access = AccessModifier.Public;
					c_modifiers = [];
					c_type_params = nested_type_params;  (* Parent + constructor's own type params *)
					c_base = Some (CsTypeClass (path, type_param_refs));  (* Reference parent with parent's type params only *)
					c_interfaces = [];
					c_constraints = [];
					c_members = fields @ [ctor; tostring_method];
				} in
				CsMemberNestedType nested_class :: acc
			| _ ->
				(* Simple constructor (no parameters) - need a nested class with singleton instance *)
				let class_name = escape_ctor_name ef.ef_name in
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
					(* Generate ToString() method for proper enum string representation *)
					let tostring_method = CsMemberMethod {
						m_name = "ToString";
						m_return_type = CsTypeString;
						m_access = AccessModifier.Public;
						m_modifiers = [MemberModifier.Override];
						m_type_params = [];
						m_params = [];
						m_body = Some [CsReturn (Some (CsConst (CsConstString ef.ef_name)))];
						m_constraints = [];
						m_explicit_interface = None;
						m_attributes = [];
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
						c_members = [ctor; tostring_method];
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
					(* Generate ToString() method for proper enum string representation *)
					let tostring_method = CsMemberMethod {
						m_name = "ToString";
						m_return_type = CsTypeString;
						m_access = AccessModifier.Public;
						m_modifiers = [MemberModifier.Override];
						m_type_params = [];
						m_params = [];
						m_body = Some [CsReturn (Some (CsConst (CsConstString ef.ef_name)))];
						m_constraints = [];
						m_explicit_interface = None;
						m_attributes = [];
					} in
					let nested_class = CsClassDef {
						c_path = (fst path, nested_class_name);  (* use short path for declaration *)
						c_access = AccessModifier.Public;
						c_modifiers = [];
						c_type_params = [];
						c_base = Some (CsTypeClass (path, []));
						c_interfaces = [];
						c_constraints = [];
						c_members = [ctor; instance; tostring_method];
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

		(* Record this enum for Program.cs _hx_bind() calls *)
		gctx.all_haxe_classes <- path :: gctx.all_haxe_classes;

		(* Get enum constructor names sorted by ef_index for correct declaration order *)
		let sorted_constrs = List.sort (fun ef1 ef2 -> compare ef1.ef_index ef2.ef_index)
			(PMap.fold (fun ef acc -> ef :: acc) e.e_constrs []) in
		let constr_names = List.map (fun ef -> ef.ef_name) sorted_constrs in

		(* Generate _hx_bind method for HaxeStaticFields registration.
		   This registers the enum constructor names in declaration order.
		   public static void _hx_bind() {
		       var acc = haxe.lang.HaxeStaticFields.getOrCreate(typeof(MyEnum).FullName);
		       acc.enumConstructs = new string[] { "A", "B", "C" };
		   }
		*)
		let cs_enum_type = CsTypeClass (path, []) in
		let bind_body =
			(* var acc = haxe.lang.HaxeStaticFields.getOrCreate(typeof(MyEnum).FullName); *)
			let get_or_create = CsVarDecl (
				"acc",
				Some static_accessors_type,
				Some (CsStaticCall (haxe_static_fields_type, "getOrCreate", [
					CsField (CsTypeOf cs_enum_type, "FullName")
				]))
			) in
			(* acc.enumConstructs = new string[] { "A", "B", "C" }; *)
			let set_enum_constructs = CsExprStmt (CsBinop (CsOpAssign,
				CsField (CsLocal "acc", "enumConstructs"),
				CsNewArray (CsTypeString,
					List.map (fun name -> CsConst (CsConstString name)) constr_names)
			)) in
			[get_or_create; set_enum_constructs]
		in
		let bind_method = CsMemberMethod {
			m_name = "_hx_bind";
			m_return_type = CsTypeVoid;
			m_access = AccessModifier.Public;
			m_modifiers = [MemberModifier.Static];
			m_type_params = [];
			m_params = [];
			m_body = Some bind_body;
			m_constraints = [];
			m_explicit_interface = None;
			m_attributes = [];
		} in

		(* Generate _hx_getIndex() method for IHaxeEnum interface - AOT-safe enum index access *)
		let get_index_method = CsMemberMethod {
			m_name = "_hx_getIndex";
			m_return_type = CsTypeInt;
			m_access = AccessModifier.Public;
			m_modifiers = [];
			m_type_params = [];
			m_params = [];
			m_body = Some [CsReturn (Some (CsField (CsThis, "_hx_index")))];
			m_constraints = [];
			m_explicit_interface = None;
			m_attributes = [];
		} in

		(* IHaxeEnum interface type *)
		let ihaxe_enum_type = CsTypeClass ((["haxe"; "lang"], "IHaxeEnum"), []) in

		CsClassDef {
			c_path = path;
			c_access = AccessModifier.Public;
			c_modifiers = [TypeModifier.Abstract];
			c_type_params = type_params;  (* Add type params to parent class *)
			c_base = None;
			c_interfaces = [ihaxe_enum_type];
			c_constraints = [];
			c_members = index_field :: get_index_method :: bind_method :: List.rev members;
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

(* ====== Output & main entry point ====== *)

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
		(* Generate _hx_bind() calls for all Haxe classes *)
		let bind_calls = List.rev_map (fun path ->
			Printf.sprintf "        global::%s._hx_bind();" (s_cs_path path)
		) gctx.all_haxe_classes in
		let bind_calls_str = String.concat "\n" bind_calls in
		(* Generate interface field name registrations (interfaces can't have _hx_bind) *)
		let interface_registrations = List.rev_map (fun (ipath, field_names) ->
			let field_names_str = String.concat ", " (List.map (Printf.sprintf "\"%s\"") field_names) in
			Printf.sprintf "        {\n            var acc = global::haxe.lang.HaxeStaticFields.getOrCreate(typeof(global::%s).FullName);\n            acc.instanceFieldNames = new string[] { %s };\n        }" (s_cs_path ipath) field_names_str
		) gctx.all_haxe_interfaces in
		let interface_registrations_str = String.concat "\n" interface_registrations in
		let program_content = Printf.sprintf
"// Generated by Haxe C# target
using System;

public class Program
{
    public static void Main(string[] args)
    {
        // Initialize all Haxe static field accessors
%s
%s

        %s.main();
    }
}
" bind_calls_str interface_registrations_str (s_cs_path main_class_path) in
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
	copy_runtime_file "cs/_cs/haxe/lang/Value.cs" "haxe/lang/Value.cs";
	copy_runtime_file "cs/_cs/haxe/lang/InstanceMethodFunction.cs" "haxe/lang/InstanceMethodFunction.cs";
	copy_runtime_file "cs/_cs/haxe/lang/ClassMethodFunction.cs" "haxe/lang/ClassMethodFunction.cs";
	copy_runtime_file "cs/_cs/haxe/lang/EmptyConstructor.cs" "haxe/lang/EmptyConstructor.cs";
	copy_runtime_file "cs/_cs/haxe/lang/ConstructorFunction.cs" "haxe/lang/ConstructorFunction.cs";
	copy_runtime_file "cs/_cs/haxe/lang/StaticAccessors.cs" "haxe/lang/StaticAccessors.cs";
	copy_runtime_file "cs/_cs/haxe/lang/HaxeStaticFields.cs" "haxe/lang/HaxeStaticFields.cs";
	copy_runtime_file "cs/_cs/haxe/lang/IHaxeEnum.cs" "haxe/lang/IHaxeEnum.cs";
	copy_runtime_file "cs/_cs/AssemblyAttributes.cs" "AssemblyAttributes.cs";

