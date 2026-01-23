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
	C# Invoke System Module
	=======================

	This module handles Function invoke() method infrastructure for the C# code generator.
	It provides:
	- Invoke signature classification (normalize types for dispatch)
	- Invoke method naming (invoke0, invoke1, etc.)
	- Value-based invoke argument generation
	- Invoke result casting
*)

open CsAst
open CsSignature

(* ============================================================
   Invoke Signature Classification
   ============================================================ *)

(* Classify a type for invoke signature matching.
   Primitives stay as-is for typed dispatch, everything else becomes object.
   This approach mirrors the JVM generator's signature classification:
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

(* ============================================================
   Invoke Method Naming
   ============================================================ *)

(* Get the invoke method name for the given arity.
   invoke() for 0 args, invoke1 for 1 arg, invoke2 for 2 args, etc. *)
let invoke_method_name num_args =
	if num_args = 0 then "invoke"
	else "invoke" ^ string_of_int num_args

(* Get the Value-based invoke method name: __hx_invoke0, __hx_invoke1, etc.
   These methods return Value to avoid boxing on return values. *)
let hxvalue_invoke_method_name num_args =
	"__hx_invoke" ^ string_of_int num_args

(* ============================================================
   Value Argument Generation
   ============================================================ *)

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
		match arg_type with
		| CsTypeInt ->
			CsStaticCall (hxvalue_type, "FromInt", [arg])
		| CsTypeDouble ->
			CsStaticCall (hxvalue_type, "FromDouble", [arg])
		| CsTypeFloat ->
			CsStaticCall (hxvalue_type, "FromFloat", [arg])
		| CsTypeBool ->
			CsStaticCall (hxvalue_type, "FromBool", [arg])
		| CsTypeLong ->
			CsStaticCall (hxvalue_type, "FromLong", [arg])
		| CsTypeClass ((["haxe"; "lang"], "Null"), [inner]) ->
			(* Null<T>: use FromNullXxx methods to avoid boxing *)
			begin match inner with
			| CsTypeInt -> CsStaticCall (hxvalue_type, "FromNullInt", [arg])
			| CsTypeDouble -> CsStaticCall (hxvalue_type, "FromNullDouble", [arg])
			| CsTypeFloat -> CsStaticCall (hxvalue_type, "FromNullFloat", [arg])
			| CsTypeBool -> CsStaticCall (hxvalue_type, "FromNullBool", [arg])
			| CsTypeLong -> CsStaticCall (hxvalue_type, "FromNullLong", [arg])
			| _ -> CsStaticCall (hxvalue_type, "FromObject", [arg])
			end
		| _ ->
			(* References, strings, etc.: use FromObject *)
			CsStaticCall (hxvalue_type, "FromObject", [arg])
	) args

(* ============================================================
   Invoke Argument Array Building
   ============================================================ *)

(* Build an args array for InvokeDelegate calls.
   If args list is empty, creates an empty haxe.root.Array.
   Otherwise, wraps the args in a native object[] and converts to haxe.root.Array. *)
let make_invoke_args_array args =
	if args = [] then
		CsNew (haxe_array_type, [])
	else
		let native_array = CsNewArray (CsTypeObject, args) in
		make_array_from_native ArrayDynamic native_array haxe_array_type

(* ============================================================
   Invoke Result Casting
   ============================================================ *)

(* Cast InvokeDelegate result to expected type.
   For primitives, uses Runtime.toXxx to handle boxed type mismatches.
   For void/object/dynamic, returns expression unchanged.
   For other types, uses direct C# cast. *)
let cast_invoke_result result_type call_expr =
	match result_type with
	| CsTypeVoid | CsTypeObject | CsTypeDynamic -> call_expr
	| CsTypeInt -> CsStaticCall (runtime_type, "toInt", [call_expr])
	| CsTypeLong -> CsStaticCall (runtime_type, "toLong", [call_expr])
	| CsTypeFloat | CsTypeDouble -> CsStaticCall (runtime_type, "toDouble", [call_expr])
	| CsTypeBool -> CsStaticCall (runtime_type, "toBool", [call_expr])
	| CsTypeString -> CsCast (CsTypeString, call_expr)
	| _ -> CsCast (result_type, call_expr)
