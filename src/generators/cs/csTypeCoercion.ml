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
	C# Type Coercion Module
	=======================

	This module handles type coercion and classification for the C# code generator.
	It provides:
	- Type classification predicates (is_cs_object_or_dynamic, is_cs_null_wrapper, etc.)
	- Type casting helpers (cast_object_to_type)
	- C# expression analysis helpers for coercion decisions
	- Coercion functions for converting between C# types
*)

open Type
open CsAst
open CsTypeMapping

(* ============================================================
   Type classification predicates
   ============================================================ *)

(* Check if a Haxe type is Dynamic (after following aliases) *)
let is_haxe_dynamic_type t = match Type.follow t with TDynamic _ -> true | _ -> false

(* Check if C# type is object or Dynamic *)
let is_cs_object_or_dynamic = function
	| CsTypeObject | CsTypeDynamic -> true
	| _ -> false

(* Check if C# type is a Null<T> wrapper *)
let is_cs_null_wrapper = function
	| CsTypeClass ((["haxe"; "lang"], "Null"), _) -> true
	| _ -> false

(* Get inner type from Null<T>, if it's a Null wrapper *)
let get_cs_null_inner = function
	| CsTypeClass ((["haxe"; "lang"], "Null"), [inner]) -> Some inner
	| _ -> None

(* Check if C# type is a basic numeric type *)
let is_cs_numeric_type = function
	| CsTypeInt | CsTypeDouble | CsTypeFloat | CsTypeLong | CsTypeByte -> true
	| _ -> false

(* Check if C# type is a primitive (numeric or bool) *)
let is_cs_primitive_type = function
	| CsTypeInt | CsTypeDouble | CsTypeFloat | CsTypeLong | CsTypeByte
	| CsTypeBool | CsTypeSByte | CsTypeChar | CsTypeShort | CsTypeUShort
	| CsTypeUInt | CsTypeULong | CsTypeDecimal -> true
	| _ -> false

(* ============================================================
   C# expression analysis helpers for coercion
   ============================================================ *)

(* Check if a C# expression is a cast to object/Dynamic type.
   Used to avoid double unwrapping of nullable types. *)
let rec cs_expr_is_object_cast cs_expr =
	match cs_expr with
	| CsCast ((CsTypeObject | CsTypeDynamic), _) -> true
	| CsParens e -> cs_expr_is_object_cast e
	| _ -> false

(* Check if a C# expression is a Runtime conversion call (toInt, toDouble, etc.)
   that returns a primitive type, not Null<primitive>.
   This is important because the Haxe type might be Null<Int> but the C# expression
   returns int directly. *)
let rec cs_expr_is_runtime_conversion cs_expr =
	match cs_expr with
	| CsStaticCall (CsTypeClass ((["haxe"; "lang"], "Runtime"), _), method_name, _)
		when method_name = "toInt" || method_name = "toDouble" || method_name = "toLong"
		  || method_name = "toFloat" || method_name = "toBool" ->
		true
	| CsParens e -> cs_expr_is_runtime_conversion e
	| CsCast (_, e) -> cs_expr_is_runtime_conversion e  (* Look through casts *)
	| _ -> false

(* If cs_expr is a Null<T>._ofDynamic(...) call, return Some(inner_type) where T is inner_type.
   This detects when earlier code added _ofDynamic, meaning the actual result type is
   Null<T> (not object as arg_cs_type might indicate). Used to avoid double conversion. *)
let rec get_null_inner_type_if_of_dynamic_call cs_expr =
	match cs_expr with
	| CsStaticCall (CsTypeClass ((["haxe"; "lang"], "Null"), [inner_type]), "_ofDynamic", _) ->
		Some inner_type
	| CsParens e -> get_null_inner_type_if_of_dynamic_call e
	| _ -> None

(* Check if a C# expression is a ternary with mixed Null<T>/object branches.
   This pattern occurs with Haxe's ?? operator when one branch is Null<T> and
   the other is cast to object. C# cannot directly cast such ternaries to primitives.
   Returns true if the expression is a ternary with branches of incompatible types. *)
let rec is_ternary_with_mixed_types cs_expr =
	let is_null_type = function
		| CsTypeClass ((["haxe"; "lang"], "Null"), _) -> true
		| _ -> false
	in
	let get_branch_type cs_expr = match cs_expr with
		| CsLocal _ -> None  (* Can't determine type from local alone *)
		| CsCast (t, _) -> Some t
		| CsDefault t -> Some t
		| CsNew (t, _) -> Some t
		| CsParens e ->
			(* Recursively get branch type through parentheses *)
			let rec inner e = match e with
				| CsLocal _ -> None
				| CsCast (t, _) -> Some t
				| CsDefault t -> Some t
				| CsNew (t, _) -> Some t
				| CsParens inner_e -> inner inner_e
				| _ -> None
			in inner e
		| _ -> None
	in
	match cs_expr with
	| CsTernary (_, then_e, else_e) ->
		let then_type = get_branch_type then_e in
		let else_type = get_branch_type else_e in
		begin match then_type, else_type with
		| Some t1, Some t2 when is_null_type t1 <> is_null_type t2 ->
			(* One branch is Null<T>, the other is not - mixed types *)
			true
		| _, _ ->
			(* Also check if one branch is a local (Null<T> variable) and other is object cast *)
			let then_is_local = match then_e with CsLocal _ -> true | _ -> false in
			let else_is_object = match else_type with Some CsTypeObject -> true | _ -> false in
			let else_is_local = match else_e with CsLocal _ -> true | _ -> false in
			let then_is_object = match then_type with Some CsTypeObject -> true | _ -> false in
			(then_is_local && else_is_object) || (else_is_local && then_is_object)
		end
	| CsParens e -> is_ternary_with_mixed_types e
	| _ -> false

(* ============================================================
   Type casting helpers
   ============================================================ *)

(* Convert object to string safely - handles boxed primitives.
   Direct cast (string)obj fails when obj is a boxed int/long/bool.
   This generates: Runtime.toStr(obj)
   The Runtime.toStr method handles null, strings, and calls ToString() for other types. *)
let object_to_string object_expr =
	CsStaticCall (runtime_type, "toStr", [object_expr])

(* Cast object/Dynamic to target C# type, using Runtime.toXxx for primitives.
   This handles boxed type mismatches (e.g., boxed int to double).
   Used when dynamic operation results need to be cast to specific types. *)
let cast_object_to_type target_cs_type object_expr =
	match target_cs_type with
	| CsTypeObject | CsTypeDynamic -> object_expr
	| CsTypeInt -> CsStaticCall (runtime_type, "toInt", [object_expr])
	| CsTypeDouble -> CsStaticCall (runtime_type, "toDouble", [object_expr])
	| CsTypeLong -> CsStaticCall (runtime_type, "toLong", [object_expr])
	| CsTypeBool -> CsStaticCall (runtime_type, "toBool", [object_expr])
	| CsTypeFloat -> CsCast (CsTypeFloat, CsStaticCall (runtime_type, "toDouble", [object_expr]))
	| CsTypeString -> object_to_string object_expr
	| CsTypeClass ((["haxe"; "lang"], "Null"), _) ->
		(* Object to Null<T> - use _ofDynamic for proper handling of null and boxed values *)
		CsStaticCall (target_cs_type, "_ofDynamic", [object_expr])
	| _ -> CsCast (target_cs_type, object_expr)

(* Cast haxe.lang.Value → typed C# expression.
   Primitives: expr.ToInt(), expr.ToDouble(), etc.
   Null<T>: expr.ToNullInt(), expr.ToNullDouble(), etc.
   Other: falls through to cast_object_to_type via expr.ToDynamic(). *)
let cast_value_to_type target_cs_type value_expr =
	match target_cs_type with
	| CsTypeInt -> CsCall (CsField (value_expr, "ToInt"), [])
	| CsTypeDouble -> CsCall (CsField (value_expr, "ToDouble"), [])
	| CsTypeFloat -> CsCall (CsField (value_expr, "ToFloat"), [])
	| CsTypeBool -> CsCall (CsField (value_expr, "ToBool"), [])
	| CsTypeLong -> CsCall (CsField (value_expr, "ToLong"), [])
	| CsTypeString -> CsCall (CsField (value_expr, "ToStringValue"), [])
	| CsTypeClass ((["haxe"; "lang"], "Null"), [inner]) ->
		begin match inner with
		| CsTypeInt -> CsCall (CsField (value_expr, "ToNullInt"), [])
		| CsTypeDouble -> CsCall (CsField (value_expr, "ToNullDouble"), [])
		| CsTypeFloat -> CsCall (CsField (value_expr, "ToNullFloat"), [])
		| CsTypeBool -> CsCall (CsField (value_expr, "ToNullBool"), [])
		| CsTypeLong -> CsCall (CsField (value_expr, "ToNullLong"), [])
		| _ when CsTypeMapping.is_inherently_nullable inner ->
			CsCast (inner, CsCall (CsField (value_expr, "ToDynamic"), []))
		| _ ->
			let null_type = CsTypeClass ((["haxe"; "lang"], "Null"), [inner]) in
			CsStaticCall (null_type, "_ofDynamic", [CsCall (CsField (value_expr, "ToDynamic"), [])])
		end
	| CsTypeVoid -> value_expr
	| CsTypeObject | CsTypeDynamic -> CsCall (CsField (value_expr, "ToDynamic"), [])
	| _ -> cast_object_to_type target_cs_type (CsCall (CsField (value_expr, "ToDynamic"), []))

(* Cast typed C# expression → haxe.lang.Value.
   Primitives: Value.FromInt(expr), Value.FromDouble(expr), etc.
   Null<T>: Value.FromNullInt(expr), Value.FromNullDouble(expr), etc.
   Other: Value.FromObject(expr). *)
let cast_type_to_value source_cs_type expr =
	match source_cs_type with
	| CsTypeInt -> CsStaticCall (hxvalue_type, "FromInt", [expr])
	| CsTypeDouble -> CsStaticCall (hxvalue_type, "FromDouble", [expr])
	| CsTypeFloat -> CsStaticCall (hxvalue_type, "FromFloat", [expr])
	| CsTypeBool -> CsStaticCall (hxvalue_type, "FromBool", [expr])
	| CsTypeLong -> CsStaticCall (hxvalue_type, "FromLong", [expr])
	| CsTypeClass ((["haxe"; "lang"], "Null"), [inner]) ->
		begin match inner with
		| CsTypeInt -> CsStaticCall (hxvalue_type, "FromNullInt", [expr])
		| CsTypeDouble -> CsStaticCall (hxvalue_type, "FromNullDouble", [expr])
		| CsTypeFloat -> CsStaticCall (hxvalue_type, "FromNullFloat", [expr])
		| CsTypeBool -> CsStaticCall (hxvalue_type, "FromNullBool", [expr])
		| CsTypeLong -> CsStaticCall (hxvalue_type, "FromNullLong", [expr])
		| _ -> CsStaticCall (hxvalue_type, "FromObject", [expr])
		end
	| _ -> CsStaticCall (hxvalue_type, "FromObject", [expr])

(* ============================================================
   Coercion helper functions
   Each returns Some(result) if it handles the conversion,
   or None if the conversion doesn't apply.
   ============================================================ *)

(* Handle simple numeric conversions: int↔float↔double↔long↔byte *)
let coerce_numeric cs_arg arg_type expected_type =
	match expected_type, arg_type with
	| CsTypeFloat, CsTypeDouble -> Some (CsCast (CsTypeFloat, cs_arg))
	| CsTypeFloat, CsTypeInt -> Some (CsCast (CsTypeFloat, cs_arg))
	| CsTypeDouble, CsTypeInt -> Some (CsCast (CsTypeDouble, cs_arg))
	| CsTypeByte, CsTypeInt -> Some (CsCast (CsTypeByte, cs_arg))
	| CsTypeInt, CsTypeDouble -> Some (CsCast (CsTypeInt, cs_arg))
	| _ -> None

(* Handle object/Dynamic to primitive with _ofDynamic awareness.
   Uses Runtime.toXxx for proper boxed type conversion. *)
let coerce_object_to_primitive cs_arg arg_type expected_type =
	if not (is_cs_object_or_dynamic arg_type) then None
	else match expected_type with
	| CsTypeInt ->
		begin match get_null_inner_type_if_of_dynamic_call cs_arg with
		| Some CsTypeInt -> Some (CsField (cs_arg, "value"))
		| Some _ -> Some (CsStaticCall (runtime_type, "toInt", [CsField (cs_arg, "value")]))
		| None -> Some (CsStaticCall (runtime_type, "toInt", [cs_arg]))
		end
	| CsTypeDouble ->
		begin match get_null_inner_type_if_of_dynamic_call cs_arg with
		| Some CsTypeDouble -> Some (CsField (cs_arg, "value"))
		| Some _ -> Some (CsStaticCall (runtime_type, "toDouble", [CsField (cs_arg, "value")]))
		| None -> Some (CsStaticCall (runtime_type, "toDouble", [cs_arg]))
		end
	| CsTypeBool ->
		begin match get_null_inner_type_if_of_dynamic_call cs_arg with
		| Some CsTypeBool -> Some (CsField (cs_arg, "value"))
		| Some _ -> Some (CsStaticCall (runtime_type, "toBool", [CsField (cs_arg, "value")]))
		| None -> Some (CsStaticCall (runtime_type, "toBool", [cs_arg]))
		end
	| CsTypeFloat ->
		begin match get_null_inner_type_if_of_dynamic_call cs_arg with
		| Some CsTypeFloat -> Some (CsField (cs_arg, "value"))
		| Some _ -> Some (CsCast (CsTypeFloat, CsStaticCall (runtime_type, "toDouble", [CsField (cs_arg, "value")])))
		| None -> Some (CsCast (CsTypeFloat, CsStaticCall (runtime_type, "toDouble", [cs_arg])))
		end
	| CsTypeLong ->
		begin match get_null_inner_type_if_of_dynamic_call cs_arg with
		| Some CsTypeLong -> Some (CsField (cs_arg, "value"))
		| Some _ -> Some (CsStaticCall (runtime_type, "toLong", [CsField (cs_arg, "value")]))
		| None -> Some (CsStaticCall (runtime_type, "toLong", [cs_arg]))
		end
	| CsTypeByte -> Some (CsCast (CsTypeByte, cs_arg))
	| CsTypeString -> Some (object_to_string cs_arg)
	| _ -> None

(* Handle Null<T> to T unwrapping for exact type match.
   Returns Some if it's a simple unwrap case, None for more complex conversions. *)
let coerce_null_unwrap_exact cs_arg arg_type expected_type =
	if cs_expr_is_object_cast cs_arg then None  (* Can't unwrap object cast *)
	else match get_cs_null_inner arg_type with
	| None -> None  (* arg is not Null<T> *)
	| Some inner_type ->
		if inner_type = expected_type then
			(* Exact match: Null<int> → int, just unwrap *)
			Some (CsField (cs_arg, "value"))
		else
			None  (* Not an exact match, need more complex handling *)

(* Handle Null<object> to primitive with Runtime conversion.
   These need Runtime.toXxx after unwrapping .value. *)
let coerce_null_object_to_primitive cs_arg arg_type expected_type =
	match get_cs_null_inner arg_type with
	| Some CsTypeObject when not (cs_expr_is_object_cast cs_arg) ->
		begin match expected_type with
		| CsTypeInt -> Some (CsStaticCall (runtime_type, "toInt", [CsField (cs_arg, "value")]))
		| CsTypeDouble -> Some (CsStaticCall (runtime_type, "toDouble", [CsField (cs_arg, "value")]))
		| CsTypeBool -> Some (CsStaticCall (runtime_type, "toBool", [CsField (cs_arg, "value")]))
		| CsTypeFloat -> Some (CsCast (CsTypeFloat, CsStaticCall (runtime_type, "toDouble", [CsField (cs_arg, "value")])))
		| CsTypeLong -> Some (CsStaticCall (runtime_type, "toLong", [CsField (cs_arg, "value")]))
		| CsTypeString -> Some (object_to_string (CsField (cs_arg, "value")))
		| CsTypeClass (path, params) when path <> (["haxe"; "lang"], "Null") ->
			Some (CsCast (CsTypeClass (path, params), CsField (cs_arg, "value")))
		| _ -> None
		end
	| Some CsTypeObject (* when cs_expr_is_object_cast cs_arg *) ->
		(* Expression is already cast to object - use Runtime conversion directly *)
		begin match expected_type with
		| CsTypeInt -> Some (CsStaticCall (runtime_type, "toInt", [cs_arg]))
		| CsTypeDouble -> Some (CsStaticCall (runtime_type, "toDouble", [cs_arg]))
		| CsTypeBool -> Some (CsStaticCall (runtime_type, "toBool", [cs_arg]))
		| CsTypeFloat -> Some (CsCast (CsTypeFloat, CsStaticCall (runtime_type, "toDouble", [cs_arg])))
		| CsTypeLong -> Some (CsStaticCall (runtime_type, "toLong", [cs_arg]))
		| CsTypeString -> Some (object_to_string cs_arg)
		| CsTypeClass (path, params) when path <> (["haxe"; "lang"], "Null") ->
			Some (CsCast (CsTypeClass (path, params), cs_arg))
		| _ -> None
		end
	| _ -> None

(* Handle Null<SomeClass> to SomeClass for reference types.
   Reference types don't need .value unwrap - C# variable is declared as the type directly. *)
let coerce_null_ref_to_ref cs_arg arg_type expected_type =
	if cs_expr_is_object_cast cs_arg then None
	else match arg_type, expected_type with
	| CsTypeClass ((["haxe"; "lang"], "Null"), [CsTypeClass (inner_path, inner_params)]),
	  CsTypeClass (path, params)
		when path = inner_path && path <> (["haxe"; "lang"], "Null") ->
		if params = inner_params then Some cs_arg
		else Some (CsCast (CsTypeClass (path, params), cs_arg))
	| CsTypeClass ((["haxe"; "lang"], "Null"), [CsTypeNestedGeneric (inner_parent, inner_name, inner_params)]),
	  CsTypeNestedGeneric (parent, name, params)
		when parent = inner_parent && name = inner_name ->
		if params = inner_params then Some cs_arg
		else Some (CsCast (CsTypeNestedGeneric (parent, name, params), cs_arg))
	| _ -> None

(* Handle conversions TO Null<T>: null literal, object/Dynamic to Null<T> *)
let coerce_to_null cs_arg arg_type expected_type =
	match get_cs_null_inner expected_type with
	| None -> None  (* target is not Null<T> *)
	| Some _inner_expected ->
		if cs_arg = CsNull then
			(* null literal → Null<T>: generate default *)
			Some (CsDefault expected_type)
		else if is_cs_object_or_dynamic arg_type then
			(* object/Dynamic → Null<T>: use _ofDynamic *)
			Some (CsStaticCall (expected_type, "_ofDynamic", [cs_arg]))
		else
			None  (* More complex conversion needed *)

(* Check if a C# expression is a call to an abstract implementation method.
   These return the underlying type, not Null<T>, even when Haxe type says Null<T>. *)
let rec is_abstract_impl_call cs_expr =
	match cs_expr with
	| CsStaticCall (CsTypeClass (path, _), _, _) ->
		let class_name = snd path in
		String.length class_name >= 6 &&
		String.sub class_name (String.length class_name - 6) 6 = "_Impl_"
	| CsParens e -> is_abstract_impl_call e
	| _ -> false

(* Check if a C# expression is a cast to Null<T> type *)
let rec is_cast_to_null_type cs_expr =
	match cs_expr with
	| CsCast (CsTypeClass ((["haxe"; "lang"], "Null"), _), _) -> true
	| CsParens e -> is_cast_to_null_type e
	| _ -> false

(* Handle Null<T> -> object/Dynamic: use toDynamic() to get boxed value or null.
   Excludes expressions that return primitives:
   - Runtime conversions (toInt, toDouble, etc.)
   - Abstract implementation methods (*_Impl_.*) when not explicitly wrapped in Null<T> *)
let coerce_null_to_object cs_arg arg_type expected_type =
	if not (is_cs_object_or_dynamic expected_type) then None
	else if cs_expr_is_runtime_conversion cs_arg then None
	else
		(* Check if the C# expression is explicitly a Null<T> struct *)
		let is_null_from_expr = get_null_inner_type_if_of_dynamic_call cs_arg <> None in
		let is_null_from_cast = is_cast_to_null_type cs_arg in
		if is_null_from_expr || is_null_from_cast then
			Some (CsCall (CsField (cs_arg, "toDynamic"), []))
		(* For _Impl_ calls without explicit Null wrapper, don't trust Haxe type
		   because it may include implicit conversions not in C# code *)
		else if is_abstract_impl_call cs_arg then
			None
		(* For non-_Impl_ calls, trust the Haxe type *)
		else if get_cs_null_inner arg_type <> None then
			Some (CsCall (CsField (cs_arg, "toDynamic"), []))
		else
			None

(* Handle SomeClass<A> to Null<SomeClass<B>> with generic coercion *)
let coerce_class_to_null_class cs_arg arg_type expected_type =
	match expected_type, arg_type with
	| CsTypeClass ((["haxe"; "lang"], "Null"), [CsTypeClass (inner_path, inner_params)]),
	  CsTypeClass (arg_path, arg_params)
		when inner_path = arg_path && inner_params <> arg_params && arg_path <> (["haxe"; "lang"], "Null") ->
		let inner_type = CsTypeClass (inner_path, inner_params) in
		let casted = CsCast (inner_type, cs_arg) in
		Some (CsNew (expected_type, [casted; CsConst (CsConstBool true)]))
	| _ -> None

(* Handle object/Dynamic to class/array types with explicit cast *)
let coerce_object_to_class cs_arg arg_type expected_type =
	if not (is_cs_object_or_dynamic arg_type) then None
	else match expected_type with
	| CsTypeClass (path, params) when path <> (["haxe"; "lang"], "Null") ->
		Some (CsCast (CsTypeClass (path, params), cs_arg))
	| CsTypeArray (_, _) ->
		Some (CsCast (expected_type, cs_arg))
	| CsTypeGenericParam _ ->
		Some (CsCast (expected_type, cs_arg))
	| _ -> None

(* Handle Null<T> to T (generic) with exact inner match *)
let coerce_null_to_inner cs_arg arg_type expected_type =
	if cs_expr_is_object_cast cs_arg then None
	else match get_cs_null_inner arg_type with
	| Some inner when inner = expected_type -> Some (CsField (cs_arg, "value"))
	| _ -> None

(* Handle Null<A> to Null<B> conversions (numeric or generic) *)
let coerce_null_to_null cs_arg arg_type expected_type =
	match get_cs_null_inner arg_type, get_cs_null_inner expected_type with
	| Some inner_arg, Some inner_expected when inner_arg <> inner_expected ->
		(* Check if numeric conversion needed *)
		let needs_numeric = match inner_expected, inner_arg with
			| CsTypeDouble, CsTypeInt -> true
			| CsTypeDouble, CsTypeFloat -> true
			| CsTypeFloat, CsTypeInt -> true
			| CsTypeLong, CsTypeInt -> true
			| CsTypeInt, CsTypeLong -> true
			| CsTypeInt, CsTypeDouble -> true
			| _ -> false
		in
		(* Check if generic coercion needed *)
		let needs_generic = match inner_expected, inner_arg with
			| CsTypeClass (path1, params1), CsTypeClass (path2, params2)
				when path1 = path2 && params1 <> params2 ->
				let is_type_param = function CsTypeGenericParam _ -> true | _ -> false in
				not (List.exists is_type_param params1)
			| _ -> false
		in
		if needs_numeric then begin
			let has_value = CsField (cs_arg, "hasValue") in
			let converted_value = CsCast (inner_expected, CsField (cs_arg, "value")) in
			let true_branch = CsNew (expected_type, [converted_value; CsConst (CsConstBool true)]) in
			let false_branch = CsNew (expected_type, [CsDefault inner_expected; CsConst (CsConstBool false)]) in
			Some (CsTernary (has_value, true_branch, false_branch))
		end
		else if needs_generic then
			Some (CsCast (expected_type, cs_arg))
		else
			None
	| _ -> None

(* Handle generic class coercion: same class with different type params.
   Only casts if expected type params are in scope. *)
let coerce_generic_class ?in_scope cs_arg arg_type expected_type =
	match arg_type, expected_type with
	| CsTypeClass (path1, params1), CsTypeClass (path2, params2)
		when path1 = path2 && params1 <> params2 ->
		(* Check if any type param in expected type is out of scope *)
		let rec has_out_of_scope_param in_scope_opt cs_type = match cs_type with
			| CsTypeGenericParam name ->
				begin match in_scope_opt with
				| Some scope -> not (List.mem name scope)
				| None -> true
				end
			| CsTypeClass (_, inner_params) | CsTypeNestedGeneric (_, _, inner_params) ->
				List.exists (has_out_of_scope_param in_scope_opt) inner_params
			| CsTypeNested (parent, _) ->
				has_out_of_scope_param in_scope_opt parent
			| CsTypeArray (elem, _) ->
				has_out_of_scope_param in_scope_opt elem
			| _ -> false
		in
		let expected_has_out_of_scope = List.exists (has_out_of_scope_param in_scope) params2 in
		if not expected_has_out_of_scope then
			Some (CsCast (expected_type, cs_arg))
		else
			Some cs_arg  (* Can't cast to out-of-scope params *)
	| _ -> None

(* ============================================================
   Main coercion functions
   ============================================================ *)

(* Core coercion logic that works with C# types directly.
   This is the inner function used by coerce_arg and can also be called directly
   when you already have C# types.
   NOTE: Null<Null<T>> flattening is handled by CsNullable at the AST level. *)
let coerce_cs_types ?in_scope _gctx cs_arg arg_cs_type expected_cs_type_raw =
	(* Only erase when the expected type is purely a generic param that's out of scope.
	   For complex types like Expr<double>, we should NOT erase - the type params were
	   correctly inferred. Erasing Expr<C> to Expr<object> would break valid code. *)
	let expected_cs_type = match expected_cs_type_raw, in_scope with
		| CsTypeGenericParam name, Some scope when not (List.mem name scope) ->
			CsTypeObject  (* Erase out-of-scope bare generic param to object *)
		| _ -> expected_cs_type_raw  (* Keep all other types as-is *)
	in
	(* FIRST: Check for ternary with mixed Null<T>/object branches targeting primitive type.
	   C# cannot cast such ternaries directly to int/double/etc. because the branches have
	   incompatible types. We must use Runtime.toInt/toDouble/etc. instead.
	   This handles the ?? operator pattern: (!obj.Equals(v, default)) ? v : (object)2 *)
	if is_ternary_with_mixed_types cs_arg then
		match expected_cs_type with
		| CsTypeInt | CsTypeDouble | CsTypeBool | CsTypeFloat | CsTypeLong ->
			(* Use cast_object_to_type for primitives - handles Runtime.toXxx *)
			cast_object_to_type expected_cs_type (CsCast (CsTypeObject, cs_arg))
		| CsTypeString -> object_to_string cs_arg
		| _ -> cs_arg  (* Non-primitive target types can use normal flow *)
	else
	(* Try numeric conversions first *)
	match coerce_numeric cs_arg arg_cs_type expected_cs_type with
	| Some result -> result
	| None ->
	(* Try object/Dynamic to primitive conversions *)
	match coerce_object_to_primitive cs_arg arg_cs_type expected_cs_type with
	| Some result -> result
	| None ->
	(* Try Null<T> to T exact match unwrap *)
	match coerce_null_unwrap_exact cs_arg arg_cs_type expected_cs_type with
	| Some result -> result
	| None ->
	(* Try Null<object> to primitive/class conversions *)
	match coerce_null_object_to_primitive cs_arg arg_cs_type expected_cs_type with
	| Some result -> result
	| None ->
	(* Try Null<T> -> object/Dynamic conversion *)
	match coerce_null_to_object cs_arg arg_cs_type expected_cs_type with
	| Some result -> result
	| None ->
	(* Try Null<SomeClass> to SomeClass for reference types *)
	match coerce_null_ref_to_ref cs_arg arg_cs_type expected_cs_type with
	| Some result -> result
	| None ->
	(* Try conversions TO Null<T> (null literal, object/Dynamic) *)
	match coerce_to_null cs_arg arg_cs_type expected_cs_type with
	| Some result -> result
	| None ->
	(* Try SomeClass<A> to Null<SomeClass<B>> generic coercion *)
	match coerce_class_to_null_class cs_arg arg_cs_type expected_cs_type with
	| Some result -> result
	| None ->
	(* Try object/Dynamic to class/array conversions *)
	match coerce_object_to_class cs_arg arg_cs_type expected_cs_type with
	| Some result -> result
	| None ->
	(* Try Null<T> to T generic unwrap *)
	match coerce_null_to_inner cs_arg arg_cs_type expected_cs_type with
	| Some result -> result
	| None ->
	(* Try Null<A> to Null<B> conversions (numeric or generic) *)
	match coerce_null_to_null cs_arg arg_cs_type expected_cs_type with
	| Some result -> result
	| None ->
	(* Try generic class coercion (same class, different type params) *)
	match coerce_generic_class ?in_scope cs_arg arg_cs_type expected_cs_type with
	| Some result -> result
	| None ->
	(* Check for various type conversions *)
	match expected_cs_type, arg_cs_type with
	(* Native array T[] to Haxe Array - use appropriate factory method.
	   Array is non-generic in C# output, so we use the actual array element type. *)
	| CsTypeClass ((["haxe"; "root"], "Array"), _), CsTypeArray (elem_type, _) ->
		let storage_type = classify_cs_array_element_type elem_type in
		make_array_from_native storage_type cs_arg expected_cs_type
	(* System.Type (Class<T>) from object needs explicit cast *)
	| CsTypeClass ((["System"], "Type"), []), CsTypeObject -> CsCast (expected_cs_type, cs_arg)
	(* object/Dynamic to generic type param T - need explicit cast (T)value *)
	| CsTypeGenericParam _, CsTypeObject -> CsCast (expected_cs_type, cs_arg)
	| CsTypeGenericParam _, CsTypeDynamic -> CsCast (expected_cs_type, cs_arg)
	(* Don't cast object to arbitrary class types or generic params - they may not be in scope
	   and the type system should handle covariance through proper interfaces *)
	| _ -> cs_arg

(* Wrapper that converts Haxe types to C# types and calls coerce_cs_types *)
let coerce_arg ?in_scope gctx cs_arg arg_type expected_type =
	let arg_cs_type = cs_type_of_type gctx arg_type in
	let expected_cs_type = cs_type_of_type gctx expected_type in
	coerce_cs_types ?in_scope gctx cs_arg arg_cs_type expected_cs_type
