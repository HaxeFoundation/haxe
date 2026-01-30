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
	Null<T> syntax filter for C# target.
	Based on Haxe4's HardNullableSynf from gencommon.

	This module handles Null<T> types for languages that use stack-allocated
	structs for nullable types. On C#, Null<T> is a struct with a value field
	and hasValue property for null checking.

	IMPORTANT ARCHITECTURAL NOTE:
	This module does NOT add .value/.hasValue accesses. Those decisions are made
	in gencs.ml where we have access to actual C# type information via cs_type_of_type.

	Why? Because Haxe type Null<T> doesn't always map to C# Null<T>:
	- Method returning Null<TypeParam> → C# returns `object` (type param erased)
	- Local var with Null<TypeParam> → C# declares as `object`
	- Enum field access → C# generates plain EnumType, not Null<EnumType>

	This module only:
	1. Flattens Null<Null<T>> to Null<T> (via is_null_t recursion)
	2. Strips Null from enum field access (FEnum) - C# generates plain EnumType
	3. Strips Null from TConst TNull when inner type is inherently nullable (reference types)

	gencs.ml handles (via expr_produces_csharp_null_type helper):
	- Deciding when to add .value (checks actual C# type)
	- Deciding when to use .hasValue vs == null
	- All coercion logic that knows the actual C# types
*)

open Type

(* NOTE: No configuration needed anymore. All .value/.hasValue decisions
   are made by gencs.ml which has access to actual C# type information. *)

(* Check if a type is a basic value type (int, float, bool, etc.) *)
let is_cs_basic_type t =
	match follow t with
	| TAbstract({ a_path = ([], "Int") }, _)
	| TAbstract({ a_path = ([], "Float") }, _)
	| TAbstract({ a_path = ([], "Bool") }, _)
	| TAbstract({ a_path = ([], "Single") }, _)
	| TAbstract({ a_path = (["haxe"], "Int64") }, _)
	| TAbstract({ a_path = (["haxe"], "Int32") }, _)
	| TAbstract({ a_path = (["cs"], "Int64") }, _)
	| TAbstract({ a_path = (["cs"], "UInt64") }, _)
	| TAbstract({ a_path = (["cs"], "Int8") }, _)
	| TAbstract({ a_path = (["cs"], "Int16") }, _)
	| TAbstract({ a_path = (["cs"], "UInt8") }, _)
	| TAbstract({ a_path = (["cs"], "UInt16") }, _)
	| TAbstract({ a_path = (["cs"], "UInt") }, _)
	| TAbstract({ a_path = (["cs"], "Char16") }, _) ->
		true
	| _ -> false

(* Check if type is a type parameter *)
let is_type_param t =
	match follow t with
	| TInst({ cl_kind = KTypeParameter _ }, _) -> true
	| _ -> false

(* Check if a type should be wrapped in Null<> (basic types and type params need wrapping).
   Reference types (classes, enums, etc.) can be null directly in C# and don't need Null<T>. *)
let needs_null_wrapper t =
	is_cs_basic_type t || is_type_param t

(* Custom follow that doesn't unwrap abstracts - only follows TType, TLazy, TMono *)
let rec shallow_follow t depth =
	if depth > 10 then t else
	match t with
	| TType (_, _) -> shallow_follow (Type.follow_once t) (depth + 1)
	| TLazy f -> shallow_follow (lazy_type f) (depth + 1)
	| TMono r -> (match r.tm_type with Some t -> shallow_follow t (depth + 1) | None -> t)
	| _ -> t

(* Check if type is Null<T> wrapper, return inner type WITHOUT filtering by needs_null_wrapper.
   This is used for field/call/array access where we need to add .value regardless of
   whether the inner type is a value type or reference type, because the C# expression
   type IS Null<T> (e.g., from a generic method like IntMap<T>.get() returning Null<T>). *)
let rec is_null_wrapper_type t =
	let rec take_off_null t =
		match is_null_wrapper_type t with
		| None -> t
		| Some inner -> take_off_null inner
	in
	match shallow_follow t 0 with
	| TInst({ cl_path = (["haxe";"lang"], "Null") }, [of_t]) ->
		Some (take_off_null of_t)
	| TAbstract({ a_path = ([], "Null") }, [of_t]) ->
		Some (take_off_null of_t)
	| _ -> None

(* Check if type is Null<T>, return inner type (with nested Null stripped).
   IMPORTANT: Only return Some when the inner type actually needs a Null wrapper in C#.
   Reference types (classes, enums, abstracts over them) can be null directly in C#.

   This is used for TCast and TBinop where we only want to wrap/unwrap when necessary.

   IMPORTANT: Don't use follow() on the outer type - it may follow through abstract to inner type.
   Only follow TType, TLazy, TMono like gencs.ml's is_null_wrapper_type does. *)
let rec is_null_t t =
	let rec take_off_null t =
		match is_null_t t with
		| None -> t
		| Some inner -> take_off_null inner
	in
	match shallow_follow t 0 with
	| TInst({ cl_path = (["haxe";"lang"], "Null") }, [of_t]) ->
		(* haxe.lang.Null<T> extern class - only treat as Null if inner type needs wrapper *)
		let inner = take_off_null of_t in
		if needs_null_wrapper inner then Some inner else None
	| TAbstract({ a_path = ([], "Null") }, [of_t]) ->
		(* Standard library Null<T> abstract - only treat as Null if inner type needs wrapper *)
		let inner = take_off_null of_t in
		if needs_null_wrapper inner then Some inner else None
	| _ -> None

(* Main transformation function *)
let rec transform e =
	match e.eexpr with
	(* TCast: gencs.ml handles all Null<T> conversions based on actual C# types.
	   We just transform recursively here. The coerce_cs_types function in gencs.ml
	   will add .value unwrap or Null<T> constructor wrap as needed based on C# types. *)
	| TCast(v, md) ->
		{ e with eexpr = TCast(transform v, md) }

	(* TField with FEnum: enum constructors don't generate Null<> in C#, so strip the Null from etype.
	   This ensures downstream coercion doesn't think this is a Null-wrapped value. *)
	| TField(ef, FEnum(en, ef_field)) ->
		begin match is_null_t e.etype with
		| Some inner_t ->
			(* Strip the Null<> wrapper from the type - the C# code generates plain EnumType, not Null<EnumType> *)
			{ e with eexpr = TField(transform ef, FEnum(en, ef_field)); etype = inner_t }
		| None ->
			(* Not Null-wrapped, transform normally *)
			{ e with eexpr = TField(transform ef, FEnum(en, ef_field)) }
		end

	(* NOTE: TField/TCall/TArray/TBinop on Null<T> do NOT auto-unwrap here.
	   gencs.ml handles all .value/.hasValue decisions based on actual C# types
	   via expr_produces_csharp_null_type. These cases fall through to the default handler. *)

	(* TBlock: process contents *)
	| TBlock bl ->
		{ e with eexpr = TBlock(List.map transform bl) }

	(* TConst TNull with Null<Abstract>: strip Null wrapper for inherently nullable types.

	   DESIGN PRINCIPLE: When Null<T> wraps a type that is already nullable in C#,
	   the Null<> wrapper should be stripped. Ideally this stripping happens at the
	   TYPE DEFINITION level (when determining how Null<SomeAbstract> maps to C#),
	   so it applies consistently everywhere that type is used.

	   Current behavior: Strip Null<NonCoreAbstract> because for most abstracts,
	   the C# variable is declared as the underlying type (e.g., VariantType), not
	   Null<VariantType>. The underlying type is typically a class/interface which
	   is inherently nullable in C#, so the Null<> struct wrapper is unnecessary.

	   IMPORTANT: Do NOT remove this stripping - it is correct for inherently nullable
	   types. Edge cases where the field IS declared as Null<T> (e.g., recursive
	   abstracts due to cycle-breaking) are handled by gencs.ml's coerce_cs_types
	   fallback pattern which generates default(Null<T>) when assigning CsNull to Null<T>. *)
	| TConst TNull ->
		begin match e.etype with
		| TAbstract ({ a_path = ([], "Null") }, [inner]) ->
			(* Check if the inner type needs a Null wrapper in C#.
			   If it does (value types), keep the Null wrapper.
			   If it doesn't (reference types), strip it. *)
			if needs_null_wrapper inner then
				(* Value type or type param - C# variable IS Null<T>, needs default(Null<T>) *)
				e
			else
				(* Reference type - C# variable is just the type, use plain null *)
				{ e with etype = inner }
		| _ -> e
		end

	(* Default: recurse into children *)
	| _ -> Type.map_expr transform e

(* Entry point: run the filter on an expression.
   Note: com parameter kept for API compatibility but not used. *)
let filter _com e =
	transform e
