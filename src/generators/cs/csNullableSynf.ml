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

	Transforms the AST before code generation to:
	1. Flatten Null<Null<T>> to Null<T>
	2. Insert explicit .value unwrap calls
	3. Insert explicit Null<T> constructor wrap calls
	4. Handle == null comparisons via != default check

	This module handles Null<T> types for languages that use stack-allocated
	structs for nullable types. On C#, Null<T> is a struct with a value field
	and implicit hasValue semantics (checked via != default).
*)

open Ast
open Type
open Globals
open Gctx

(* Configuration context for the filter *)
type null_config = {
	null_class : tclass option;    (* haxe.lang.Null class, if found *)
	basic : basic_types;           (* Basic types from compiler context *)
}

(* Check if type is Null<T>, return inner type (with nested Null stripped).
   IMPORTANT: Don't use follow() on the outer type - it may follow through abstract to inner type.
   Only follow TType, TLazy, TMono like gencs.ml's is_null_wrapper_type does. *)
let rec is_null_t t =
	let rec take_off_null t =
		match is_null_t t with
		| None -> t
		| Some inner -> take_off_null inner
	in
	(* Custom follow that doesn't unwrap abstracts *)
	let rec shallow_follow t depth =
		if depth > 10 then t else
		match t with
		| TType (_, _) -> shallow_follow (Type.follow_once t) (depth + 1)
		| TLazy f -> shallow_follow (lazy_type f) (depth + 1)
		| TMono r -> (match r.tm_type with Some t -> shallow_follow t (depth + 1) | None -> t)
		| _ -> t
	in
	match shallow_follow t 0 with
	| TInst({ cl_path = (["haxe";"lang"], "Null") }, [of_t]) ->
		(* haxe.lang.Null<T> extern class *)
		Some (take_off_null of_t)
	| TAbstract({ a_path = ([], "Null") }, [of_t]) ->
		(* Standard library Null<T> abstract *)
		Some (take_off_null of_t)
	| _ -> None

(* Check if a type is a basic value type (int, float, bool, etc.) *)
let is_cs_basic_type t =
	match follow t with
	| TAbstract({ a_path = ([], "Int") }, _)
	| TAbstract({ a_path = ([], "Float") }, _)
	| TAbstract({ a_path = ([], "Bool") }, _)
	| TAbstract({ a_path = ([], "Single") }, _)
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

(* Check if a type should be wrapped in Null<> (basic types and type params need wrapping) *)
let needs_null_wrapper t =
	is_cs_basic_type t || is_type_param t

(* Check if an expression is an enum field access (FEnum).
   In C#, enum constructors don't generate Null-wrapped code even though
   their Haxe type is Null<EnumType>. *)
let rec is_enum_field_access e =
	match e.eexpr with
	| TField(_, FEnum _) -> true
	| TParenthesis e1 | TMeta (_, e1) | TCast(e1, _) -> is_enum_field_access e1
	| _ -> false

(* Check if an expression generates C# code that is NOT Null-wrapped,
   even though its Haxe type might be Null<T>.
   These expressions should NOT have .value added. *)
let is_non_null_generating_expr e =
	is_enum_field_access e

(* Generate: expr.value field access.
   Uses FDynamic if null_class not available - gencs.ml handles this correctly. *)
let unwrap_null cfg expr inner_type =
	match cfg.null_class with
	| Some null_class ->
		(* Find the 'value' field in Null class *)
		let value_field = try
			PMap.find "value" null_class.cl_fields
		with Not_found ->
			(* Fallback: create a synthetic field *)
			let cf = {
				(mk_field "value" inner_type expr.epos null_pos) with
				cf_kind = Var { v_read = AccNormal; v_write = AccNormal };
			} in
			cf
		in
		{
			eexpr = TField(expr, FInstance(null_class, [inner_type], value_field));
			etype = inner_type;
			epos = expr.epos
		}
	| None ->
		(* No null_class found - use FDynamic which gencs.ml will handle *)
		{
			eexpr = TField(expr, FDynamic "value");
			etype = inner_type;
			epos = expr.epos
		}

(* Generate: new Null<T>(value, hasValue) constructor call *)
let wrap_null cfg expr inner_type has_value =
	match cfg.null_class with
	| Some null_class ->
		let null_type = TInst(null_class, [inner_type]) in
		let bool_expr = { eexpr = TConst(TBool has_value); etype = cfg.basic.tbool; epos = expr.epos } in
		{
			eexpr = TNew(null_class, [inner_type], [expr; bool_expr]);
			etype = null_type;
			epos = expr.epos
		}
	| None ->
		(* No null_class - can't generate proper TNew, return expr unchanged.
		   This is a fallback that shouldn't happen in practice. *)
		expr

(* Generate: expr != default(Null<T>) - checks if Null struct has a value *)
let has_value cfg expr =
	let null_type = expr.etype in
	let default_expr = {
		eexpr = TCall(
			{ eexpr = TIdent "__default__"; etype = TFun([], null_type); epos = expr.epos },
			[]
		);
		etype = null_type;
		epos = expr.epos
	} in
	{
		eexpr = TBinop(OpNotEq, expr, default_expr);
		etype = cfg.basic.tbool;
		epos = expr.epos
	}

(* Handle unwrapping from Null<T> to target type *)
let handle_unwrap cfg to_t e =
	match is_null_t e.etype with
	| Some inner_t ->
		(* Unwrap .value, then cast if needed *)
		let unwrapped = unwrap_null cfg e inner_t in
		(* If target type differs from inner type, add cast.
		   BUT: Don't add cast to Void - C# doesn't allow (void) casts. *)
		let is_void_target = ExtType.is_void (follow to_t) in
		if not is_void_target && not (type_iseq (follow to_t) (follow inner_t)) then
			{ eexpr = TCast(unwrapped, None); etype = to_t; epos = e.epos }
		else
			{ unwrapped with etype = to_t }
	| None ->
		(* Not a Null type, just cast *)
		{ eexpr = TCast(e, None); etype = to_t; epos = e.epos }

(* Handle wrapping value into Null<T> *)
let handle_wrap cfg e inner_type =
	match e.eexpr with
	| TConst TNull ->
		(* Wrapping null - hasValue = false *)
		wrap_null cfg { e with etype = inner_type } inner_type false
	| _ ->
		(* Wrapping a value - hasValue = true *)
		wrap_null cfg e inner_type true

(* Main transformation function *)
let run cfg e =
	let rec transform e =
		match e.eexpr with
		(* TCast: detect Null<T> conversions *)
		| TCast(v, md) ->
			(* For TLocal, use v_type to get the declared type *)
			let v = match v.eexpr with
				| TLocal l -> { v with etype = l.v_type }
				| _ -> v
			in
			let null_et = is_null_t e.etype in    (* Target: is it Null<T>? *)
			let null_vt = is_null_t v.etype in    (* Source: is it Null<T>? *)
			begin match null_vt, null_et with
			| Some inner_vt, None ->
				(* Null<T> -> T: unwrap .value *)
				(* BUT: Skip unwrap if the source expression doesn't actually generate
				   Null-wrapped C# code (e.g., enum constructors have Haxe type Null<Enum>
				   but generate plain Enum in C#) *)
				let is_non_null = is_non_null_generating_expr v in
				if is_non_null then
					(* Just transform and cast, no .value unwrap *)
					{ e with eexpr = TCast(transform v, md) }
				else begin match v.eexpr with
				| TCast(v2, _) ->
					(* Unnecessary nested cast to Nullable, skip *)
					transform { v with etype = e.etype }
				| _ ->
					handle_unwrap cfg e.etype (transform v)
				end
			| None, Some inner_et ->
				(* T -> Null<T>: wrap in constructor *)
				handle_wrap cfg (transform v) inner_et
			| Some inner_vt, Some inner_et when not (type_iseq (follow inner_vt) (follow inner_et)) ->
				(* Null<A> -> Null<B>: check hasValue, unwrap, convert, rewrap *)
				let v_transformed = transform v in
				(* Generate: v.hasValue ? new Null<B>((B)v.value, true) : new Null<B>(default, false) *)
				let has_val = has_value cfg v_transformed in
				let unwrapped = unwrap_null cfg v_transformed inner_vt in
				let converted = { eexpr = TCast(unwrapped, None); etype = inner_et; epos = e.epos } in
				let wrapped_true = wrap_null cfg converted inner_et true in
				let default_val = { eexpr = TConst TNull; etype = inner_et; epos = e.epos } in
				let wrapped_false = wrap_null cfg default_val inner_et false in
				{
					eexpr = TIf(has_val, wrapped_true, Some wrapped_false);
					etype = e.etype;
					epos = e.epos
				}
			| _ ->
				(* Same types or no Null involved, keep cast *)
				{ e with eexpr = TCast(transform v, md) }
			end

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

		(* TField on Null<T>: auto-unwrap before field access *)
		| TField(ef, field) when Option.is_some (is_null_t ef.etype) ->
			let inner_t = Option.get (is_null_t ef.etype) in
			let unwrapped = handle_unwrap cfg inner_t (transform ef) in
			{ e with eexpr = TField(unwrapped, field) }

		(* TCall on Null<T>: auto-unwrap before call *)
		| TCall(ecall, params) when Option.is_some (is_null_t ecall.etype) ->
			let inner_t = Option.get (is_null_t ecall.etype) in
			let unwrapped = handle_unwrap cfg inner_t (transform ecall) in
			{ e with eexpr = TCall(unwrapped, List.map transform params) }

		(* TArray on Null<T>: auto-unwrap before array access *)
		| TArray(earray, idx) when Option.is_some (is_null_t earray.etype) ->
			let inner_t = Option.get (is_null_t earray.etype) in
			let unwrapped = handle_unwrap cfg inner_t (transform earray) in
			{ e with eexpr = TArray(unwrapped, transform idx) }

		(* TBinop: special handling for equality and arithmetic *)
		| TBinop(op, e1, e2) ->
			let e1_null_t = is_null_t e1.etype in
			let e2_null_t = is_null_t e2.etype in
			begin match op with
			(* Assignment operators *)
			| OpAssign | OpAssignOp _ ->
				begin match e1_null_t, e2_null_t with
				| Some t1, Some t2 ->
					begin match op with
					| OpAssign ->
						(* Simple assignment between Null types - transform both sides *)
						Type.map_expr transform e
					| OpAssignOp binop ->
						(* Compound assignment: x += y becomes x = wrap(unwrap(x) + unwrap(y)) *)
						let e1' = transform e1 in
						let e2' = transform e2 in
						let unwrapped1 = unwrap_null cfg e1' t1 in
						let unwrapped2 = unwrap_null cfg e2' t2 in
						let result = { e with eexpr = TBinop(binop, unwrapped1, unwrapped2); etype = t1 } in
						let wrapped = wrap_null cfg result t1 true in
						{ e with eexpr = TBinop(OpAssign, e1', wrapped) }
					| _ -> die "" __LOC__
					end
				| _ ->
					(* Not both Null, normal processing *)
					Type.map_expr transform e
				end

			(* Equality comparison *)
			| OpEq | OpNotEq ->
				begin match e1.eexpr, e2.eexpr with
				| TConst TNull, _ when Option.is_some e2_null_t ->
					(* null == Null<T> becomes !hasValue *)
					let hv = has_value cfg (transform e2) in
					if op = OpEq then
						{ hv with eexpr = TUnop(Not, Prefix, hv) }
					else
						hv
				| _, TConst TNull when Option.is_some e1_null_t ->
					(* Null<T> == null becomes !hasValue *)
					let hv = has_value cfg (transform e1) in
					if op = OpEq then
						{ hv with eexpr = TUnop(Not, Prefix, hv) }
					else
						hv
				| _ when Option.is_some e1_null_t || Option.is_some e2_null_t ->
					(* Comparing Null types with non-null values: unwrap Null operands and compare directly.
					   This generates simple C# equality like: a.value == 5 *)
					let e1' = match e1_null_t with
						| Some inner -> handle_unwrap cfg inner (transform e1)
						| None -> transform e1
					in
					let e2' = match e2_null_t with
						| Some inner -> handle_unwrap cfg inner (transform e2)
						| None -> transform e2
					in
					{ e with eexpr = TBinop(op, e1', e2') }
				| _ ->
					(* No Null types involved *)
					Type.map_expr transform e
				end

			(* Other binary operators: unwrap Null operands *)
			| _ ->
				let e1' = match e1_null_t with
					| Some inner -> handle_unwrap cfg inner (transform e1)
					| None -> transform e1
				in
				let e2' = match e2_null_t with
					| Some inner -> handle_unwrap cfg inner (transform e2)
					| None -> transform e2
				in
				(* If result type is Null<T>, rewrap the result *)
				let result = { e with eexpr = TBinop(op, e1', e2') } in
				begin match is_null_t e.etype with
				| Some inner -> wrap_null cfg { result with etype = inner } inner true
				| None -> result
				end
			end

		(* TBlock: process contents and prepend any temp vars *)
		| TBlock bl ->
			{ e with eexpr = TBlock(List.map transform bl) }

		(* Default: recurse into children *)
		| _ -> Type.map_expr transform e
	in
	transform e

(* Create configuration from compilation context.
   Note: We always create a config now - if the Null class isn't in com.types,
   we use a None placeholder and generate FDynamic field access instead. *)
let create_config com =
	(* Try to find haxe.lang.Null class from types *)
	let null_class = ref None in
	List.iter (fun t ->
		match t with
		| TClassDecl c when c.cl_path = (["haxe"; "lang"], "Null") ->
			null_class := Some c
		| _ -> ()
	) com.types;
	(* Return config - null_class may be None, which is handled in unwrap_null *)
	{ null_class = !null_class; basic = com.basic }

(* Entry point: run the filter on an expression *)
let filter com e =
	let cfg = create_config com in
	run cfg e
