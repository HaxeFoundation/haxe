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
open Common
open Ast
open Type
open Texpr.Builder
open Gencommon

(* ******************************************* *)
(* Dynamic Binop/Unop handler *)
(* ******************************************* *)
(*
	On some languages there is limited support for operations on
	dynamic variables, so those operations must be changed.

	There are 5 types of binary operators:
		1 - can take any variable and returns a bool (== and !=)
		2 - can take either a string, or a number and returns either a bool or the underlying type ( >, < for bool and + for returning its type)
		3 - take numbers and return a number ( *, /, ...)
		4 - take ints and return an int (bit manipulation)
		5 - take a bool and returns a bool ( &&, || ...)

	On the default implementation, type 1 and the plus function will be handled with a function call;
	Type 2 will be handled with the parameter "compare_handler", which will do something like Reflect.compare(x1, x2);
	Types 3, 4 and 5 will perform a cast to double, int and bool, which will then be handled normally by the platform

	Unary operators are the most difficult to handle correctly.
	With unary operators, there are 2 types:

		1 - can take a number, changes and returns the result (++, --, ~)
		2 - can take a number (-) or bool (!), and returns the result

	The first case is much trickier, because it doesn't seem a good idea to change any variable to double just because it is dynamic,
	but this is how we will handle right now.
	something like that:

	var x:Dynamic = 10;
	x++;

	will be:
	object x = 10;
	x = ((IConvertible)x).ToDouble(null) + 1;

	depends on:
		(syntax) must run before expression/statment normalization because it may generate complex expressions
		must run before OverloadingConstructor due to later priority conflicts. Since ExpressionUnwrap is only
		defined afterwards, we will set this value with absolute values
*)
let init com handle_strings (should_change:texpr->bool) (equals_handler:texpr->texpr->texpr) (dyn_plus_handler:texpr->texpr->texpr->texpr) (compare_handler:Ast.binop->texpr->texpr->texpr->texpr) =
	let get_etype_one e =
		if like_int e.etype then
			make_int com.basic 1 e.epos
		else
			make_float com.basic "1.0" e.epos
	in
	let rec run e =
		match e.eexpr with
		| TBinop (OpAssignOp op, e1, e2) when should_change e -> (* e1 will never contain another TBinop *)
			(match e1.eexpr with
			| TLocal _ ->
				mk_paren { e with eexpr = TBinop(OpAssign, e1, run { e with eexpr = TBinop(op, e1, e2) }) }
			| TField _ | TArray _ ->
				let eleft, rest =
					match e1.eexpr with
					| TField(ef, f) ->
						let v = mk_temp "dynop" ef.etype in
						{ e1 with eexpr = TField (mk_local v ef.epos, f) }, [mk (TVar (v, Some (run ef))) com.basic.tvoid ef.epos]
					| TArray(e1a, e2a) ->
						let v = mk_temp "dynop" e1a.etype in
						let v2 = mk_temp "dynopi" e2a.etype in
						{ e1 with eexpr = TArray(mk_local v e1a.epos, mk_local v2 e2a.epos) }, [
							(mk (TVar (v, Some (run e1a))) com.basic.tvoid e1.epos);
							(mk (TVar (v2, Some (run e2a))) com.basic.tvoid e1.epos)
						]
					| _ -> Globals.die "" __LOC__
				in
				{ e with eexpr = TBlock (rest @ [{ e with eexpr = TBinop (OpAssign, eleft, run { e with eexpr = TBinop (op, eleft, e2) }) }]) }
			| _ ->
				Globals.die "" __LOC__)

		| TBinop (OpAssign, e1, e2)
		| TBinop (OpInterval, e1, e2) ->
			Type.map_expr run e

		| TBinop (op, e1, e2) when should_change e ->
			(match op with
			| OpEq -> (* type 1 *)
				equals_handler (run e1) (run e2)
			| OpNotEq -> (* != -> !equals() *)
				mk_parent (mk (TUnop (Not, Prefix, (equals_handler (run e1) (run e2)))) com.basic.tbool e.epos)
			| OpAdd  ->
				if handle_strings && (is_string e.etype || is_string e1.etype || is_string e2.etype) then
					{ e with eexpr = TBinop (op, mk_cast com.basic.tstring (run e1), mk_cast com.basic.tstring (run e2)) }
				else
					dyn_plus_handler e (run e1) (run e2)
			| OpGt | OpGte | OpLt | OpLte  -> (* type 2 *)
				compare_handler op e (run e1) (run e2)
			| OpMult | OpDiv | OpSub | OpMod -> (* always cast everything to double *)
				let etype = (get_etype_one e).etype in
				{ e with eexpr = TBinop (op, mk_cast etype (run e1), mk_cast etype (run e2)) }
			| OpBoolAnd | OpBoolOr ->
				{ e with eexpr = TBinop (op, mk_cast com.basic.tbool (run e1), mk_cast com.basic.tbool (run e2)) }
			| OpAnd | OpOr | OpXor | OpShl | OpShr | OpUShr ->
				{ e with eexpr = TBinop (op, mk_cast com.basic.tint (run e1), mk_cast com.basic.tint (run e2)) }
			| OpAssign | OpAssignOp _ | OpInterval | OpArrow | OpIn | OpNullCoal ->
				Globals.die "" __LOC__)

		| TUnop (Increment as op, flag, e1)
		| TUnop (Decrement as op, flag, e1) when should_change e ->
			(*
				some naming definitions:
				* ret => the returning variable
				* _g => the get body
				* getvar => the get variable expr

				This will work like this:
					- if e1 is a TField, set _g = get body, getvar = (get body).varname
					- if Prefix, return getvar = getvar + 1.0
					- if Postfix, set ret = getvar; getvar = getvar + 1.0; ret;
			*)
			let one = get_etype_one e in
			let etype = one.etype in
			let op = (match op with Increment -> OpAdd | Decrement -> OpSub | _ -> Globals.die "" __LOC__) in

			let block =
				let vars, getvar =
					match e1.eexpr with
					| TField (fexpr, field) ->
						let tmp = mk_temp "getvar" fexpr.etype in
						let var = mk (TVar (tmp, Some (run fexpr))) com.basic.tvoid e.epos in
						([var], mk (TField (make_local tmp fexpr.epos, field)) etype e1.epos)
					| _ ->
						([], e1)
				in
				match flag with
				| Prefix ->
					vars @ [
						mk_cast etype { e with eexpr = TBinop(OpAssign, getvar, binop op (mk_cast etype getvar) one etype e.epos); etype = getvar.etype }
					]
				| Postfix ->
					let ret = mk_temp "ret" etype in
					let retlocal = make_local ret e.epos in
					vars @ [
						mk (TVar (ret, Some (mk_cast etype getvar))) com.basic.tvoid e.epos;
						{ e with eexpr = TBinop (OpAssign, getvar, binop op retlocal one getvar.etype e.epos) };
						retlocal
					]
			in
			mk (TBlock block) etype e.epos

	| TUnop (op, flag, e1) when should_change e ->
		let etype = match op with
			| Not -> com.basic.tbool
			| Neg ->
				if like_float e.etype || like_i64 e.etype then
					e.etype
				else
					com.basic.tfloat
			| _ -> com.basic.tint
		in
		mk_parent (mk (TUnop (op, flag, mk_cast etype (run e1))) etype e.epos)

	| _ ->
		Type.map_expr run e
	in
	run

let name = "dyn_ops"
let priority = 0.0

let configure gen ~handle_strings should_change equals_handler dyn_plus_handler compare_handler =
	let run = init gen.gcon handle_strings should_change equals_handler dyn_plus_handler compare_handler in
	gen.gexpr_filters#add name (PCustom priority) run
