(*
	The Haxe Compiler
	Copyright (C) 2005-2017  Haxe Foundation

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
open Codegen
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
let name = "dyn_ops"
let priority = 0.0

let configure gen ?(handle_strings = true) (should_change:texpr->bool) (equals_handler:texpr->texpr->texpr) (dyn_plus_handler:texpr->texpr->texpr->texpr) (compare_handler:texpr->texpr->texpr) =
	let get_etype_one e =
		if like_int e.etype then
			ExprBuilder.make_int gen.gcon 1 e.epos
		else
			ExprBuilder.make_float gen.gcon "1.0" e.epos
	in

	let basic = gen.gcon.basic in

	let rec run e =
		match e.eexpr with
			| TBinop (OpAssignOp op, e1, e2) when should_change e -> (* e1 will never contain another TBinop *)
				(match e1.eexpr with
					| TLocal _ ->
						mk_paren { e with eexpr = TBinop(OpAssign, e1, run { e with eexpr = TBinop(op, e1, e2) }) }
					| TField _ | TArray _ ->
						let eleft, rest = match e1.eexpr with
							| TField(ef, f) ->
								let v = mk_temp "dynop" ef.etype in
								{ e1 with eexpr = TField(mk_local v ef.epos, f) }, [ { eexpr = TVar(v,Some (run ef)); etype = basic.tvoid; epos = ef.epos } ]
							| TArray(e1a, e2a) ->
								let v = mk_temp "dynop" e1a.etype in
								let v2 = mk_temp "dynopi" e2a.etype in
								{ e1 with eexpr = TArray(mk_local v e1a.epos, mk_local v2 e2a.epos) }, [
									{ eexpr = TVar(v,Some (run e1a)); etype = basic.tvoid; epos = e1.epos };
									{ eexpr = TVar(v2, Some (run e2a)); etype = basic.tvoid; epos = e1.epos }
								]
							| _ -> assert false
						in
						{ e with
							eexpr = TBlock (rest @ [ { e with eexpr = TBinop(OpAssign, eleft, run { e with eexpr = TBinop(op, eleft, e2) }) } ]);
						}
					| _ ->
						assert false
				)

			| TBinop (OpAssign, e1, e2)
			| TBinop (OpInterval, e1, e2) -> Type.map_expr run e
			| TBinop (op, e1, e2) when should_change e->
				(match op with
					| OpEq -> (* type 1 *)
						equals_handler (run e1) (run e2)
					| OpNotEq -> (* != -> !equals() *)
						mk_paren { eexpr = TUnop(Ast.Not, Prefix, (equals_handler (run e1) (run e2))); etype = gen.gcon.basic.tbool; epos = e.epos }
					| OpAdd  ->
						if handle_strings && (is_string e.etype || is_string e1.etype || is_string e2.etype) then
							{ e with eexpr = TBinop(op, mk_cast gen.gcon.basic.tstring (run e1), mk_cast gen.gcon.basic.tstring (run e2)) }
						else
							dyn_plus_handler e (run e1) (run e2)
					| OpGt | OpGte | OpLt | OpLte  -> (* type 2 *)
						{ eexpr = TBinop(op, compare_handler (run e1) (run e2), { eexpr = TConst(TInt(Int32.zero)); etype = gen.gcon.basic.tint; epos = e.epos} ); etype = gen.gcon.basic.tbool; epos = e.epos }
					| OpMult | OpDiv | OpSub | OpMod -> (* always cast everything to double *)
						let etype = (get_etype_one e).etype in
						{ e with eexpr = TBinop(op, mk_cast etype (run e1), mk_cast etype (run e2)) }
					| OpBoolAnd | OpBoolOr ->
						{ e with eexpr = TBinop(op, mk_cast gen.gcon.basic.tbool (run e1), mk_cast gen.gcon.basic.tbool (run e2)) }
					| OpAnd | OpOr | OpXor | OpShl | OpShr | OpUShr ->
						{ e with eexpr = TBinop(op, mk_cast gen.gcon.basic.tint (run e1), mk_cast gen.gcon.basic.tint (run e2)) }
					| OpAssign | OpAssignOp _ | OpInterval | OpArrow -> assert false)
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
				let op = (match op with Increment -> OpAdd | Decrement -> OpSub | _ -> assert false) in

				let var, getvar =
					match e1.eexpr with
						| TField(fexpr, field) ->
							let tmp = mk_temp "getvar" fexpr.etype in
							let var = { eexpr = TVar(tmp, Some(run fexpr)); etype = gen.gcon.basic.tvoid; epos = e.epos } in
							(Some var, { eexpr = TField( { fexpr with eexpr = TLocal(tmp) }, field); etype = etype; epos = e1.epos })
						| _ ->
							(None, e1)
				in

				(match flag with
					| Prefix ->
						let block = (match var with | Some e -> [e] | None -> []) @
						[
							mk_cast etype { e with eexpr = TBinop(OpAssign, getvar,{ eexpr = TBinop(op, mk_cast etype getvar, one); etype = etype; epos = e.epos }); etype = getvar.etype; }
						]
						in
						{ eexpr = TBlock(block); etype = etype; epos = e.epos }
					| Postfix ->
						let ret = mk_temp "ret" etype in
						let vars = (match var with Some e -> [e] | None -> []) @ [{ eexpr = TVar(ret, Some (mk_cast etype getvar)); etype = gen.gcon.basic.tvoid; epos = e.epos }] in
						let retlocal = { eexpr = TLocal(ret); etype = etype; epos = e.epos } in
						let block = vars @
						[
						{ e with eexpr = TBinop(OpAssign, getvar, { eexpr = TBinop(op, retlocal, one); etype = getvar.etype; epos = e.epos }) };
						retlocal
					] in
					{ eexpr = TBlock(block); etype = etype; epos = e.epos }
			)
		| TUnop (op, flag, e1) when should_change e ->
			let etype = match op with | Not -> gen.gcon.basic.tbool | _ -> gen.gcon.basic.tint in
			mk_paren { eexpr = TUnop(op, flag, mk_cast etype (run e1)); etype = etype; epos = e.epos }
		| _ -> Type.map_expr run e
	in
	let map e = Some(run e) in
	gen.gexpr_filters#add ~name:"dyn_ops" ~priority:(PCustom priority) map
