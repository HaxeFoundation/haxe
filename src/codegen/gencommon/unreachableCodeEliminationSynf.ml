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
open Gencommon

(*
	In some source code platforms, the code won't compile if there is Unreachable code, so this filter will take off any unreachable code.
		If the parameter "handle_switch_break" is set to true, it will already add a "break" statement on switch cases when suitable;
			in order to not confuse with while break, it will be a special expression __sbreak__
		If the parameter "handle_not_final_returns" is set to true, it will also add final returns when functions are detected to be lacking of them.
			(Will respect __fallback__ expressions)
		If the parameter "java_mode" is set to true, some additional checks following the java unreachable specs
			(http://docs.oracle.com/javase/specs/jls/se7/html/jls-14.html#jls-14.21) will be added

	dependencies:
		This must run before SwitchBreakSynf (see SwitchBreakSynf dependecy value)
		This must be the LAST syntax filter to run. It expects ExpressionUnwrap to have run correctly, since this will only work for source-code based targets
*)
type uexpr_kind =
	| Normal
	| BreaksLoop
	| BreaksFunction

let aggregate_kind e1 e2 =
	match e1, e2 with
		| Normal, _
		| _, Normal -> Normal
		| BreaksLoop, _
		| _, BreaksLoop -> BreaksLoop
		| BreaksFunction, BreaksFunction -> BreaksFunction

let aggregate_constant op c1 c2=
	match op, c1, c2 with
		| OpEq, Some v1, Some v2 -> Some (TBool (v1 = v2))
		| OpNotEq, Some v1, Some v2 -> Some (TBool (v1 <> v2))
		| OpBoolOr, Some (TBool v1) , Some (TBool v2) -> Some (TBool (v1 || v2))
		| OpBoolAnd, Some (TBool v1) , Some (TBool v2) -> Some (TBool (v1 && v2))
		| OpAssign, _, Some v2 -> Some v2
		| _ -> None

let rec get_constant_expr e =
	match e.eexpr with
		| TConst (v) -> Some v
		| TBinop(op, v1, v2) -> aggregate_constant op (get_constant_expr v1) (get_constant_expr v2)
		| TParenthesis(e) | TMeta(_,e) -> get_constant_expr e
		| _ -> None

let init gen java_mode =
	let should_warn = false in

	let do_warn =
		if should_warn then gen.gwarning WGenerator "Unreachable code" else (fun pos -> ())
	in

	let return_loop expr kind =
		match kind with
			| Normal | BreaksLoop -> expr, Normal
			| _ -> expr, kind
	in

	let mk_sbreak = mk (TIdent "__sbreak__") t_dynamic in

	let rec has_fallback expr = match expr.eexpr with
		| TBlock(bl) -> (match List.rev bl with
			| { eexpr = TIdent "__fallback__" } :: _ -> true
			| ({ eexpr = TBlock(_) } as bl) :: _ -> has_fallback bl
			| _ -> false)
		| TIdent "__fallback__" -> true
		| _ -> false
	in

	let handle_case = fun (expr,kind) ->
		match kind with
		| Normal when has_fallback expr -> expr
		| Normal -> Type.concat expr (mk_sbreak expr.epos)
		| BreaksLoop | BreaksFunction -> expr
	in

	let has_break = ref false in

	let rec process_expr expr =
		match expr.eexpr with
			| TMeta (m,expr) ->
				let expr,kind = process_expr expr in
				{ expr with eexpr = TMeta (m, expr) }, kind
			| TReturn _ | TThrow _ -> expr, BreaksFunction
			| TContinue -> expr, BreaksLoop
			| TBreak -> has_break := true; expr, BreaksLoop
			| TCall( { eexpr = TIdent "__goto__" }, _ ) -> expr, BreaksLoop

			| TBlock bl ->
				let new_block = ref [] in
				let is_unreachable = ref false in
				let ret_kind = ref Normal in

				List.iter (fun e ->
					if !is_unreachable then
						do_warn e.epos
					else begin
						let changed_e, kind = process_expr e in
						new_block := changed_e :: !new_block;
						match kind with
							| BreaksLoop | BreaksFunction ->
								ret_kind := kind;
								is_unreachable := true
							| _ -> ()
					end
				) bl;

				{ expr with eexpr = TBlock(List.rev !new_block) }, !ret_kind
			| TFunction tf ->
				let changed, kind = process_expr tf.tf_expr in
				let changed = if not (ExtType.is_void tf.tf_type) && kind <> BreaksFunction then
					Type.concat changed (Texpr.Builder.mk_return (null tf.tf_type expr.epos))
				else
					changed
				in

				{ expr with eexpr = TFunction({ tf with tf_expr = changed }) }, Normal
			| TFor(var, cond, block) ->
				let last_has_break = !has_break in
				has_break := false;

				let changed_block, _ = process_expr block in
				has_break := last_has_break;
				let expr = { expr with eexpr = TFor(var, cond, changed_block) } in
				return_loop expr Normal
			| TIf(cond, eif, None) ->
				if java_mode then
					match get_constant_expr cond with
						| Some (TBool true) ->
							process_expr eif
						| _ ->
							{ expr with eexpr = TIf(cond, fst (process_expr eif), None) }, Normal
				else
					{ expr with eexpr = TIf(cond, fst (process_expr eif), None) }, Normal
			| TIf(cond, eif, Some eelse) ->
				let eif, eif_k = process_expr eif in
				let eelse, eelse_k = process_expr eelse in
				let k = aggregate_kind eif_k eelse_k in
				{ expr with eexpr = TIf(cond, eif, Some eelse) }, k
			| TWhile(cond, block, flag) ->
				let last_has_break = !has_break in
				has_break := false;

				let block, k = process_expr block in
				if java_mode then
					match get_constant_expr cond, flag, !has_break with
						| Some (TBool true), _, false ->
							has_break := last_has_break;
							{ expr with eexpr = TWhile(cond, block, flag) }, BreaksFunction
						| Some (TBool false), NormalWhile, _ ->
							has_break := last_has_break;
							do_warn expr.epos;
							null expr.etype expr.epos, Normal
						| _ ->
							has_break := last_has_break;
							return_loop { expr with eexpr = TWhile(cond,block,flag) } Normal
				else begin
					has_break := last_has_break;
					return_loop { expr with eexpr = TWhile(cond,block,flag) } Normal
				end
			| TSwitch(cond, el_e_l, None) ->
				{ expr with eexpr = TSwitch(cond, List.map (fun (el, e) -> (el, handle_case (process_expr e))) el_e_l, None) }, Normal
			| TSwitch(cond, el_e_l, Some def) ->
				let def, k = process_expr def in
				let def = handle_case (def, k) in
				let k = ref k in
				let ret = { expr with eexpr = TSwitch(cond, List.map (fun (el, e) ->
					let e, ek = process_expr e in
					k := aggregate_kind !k ek;
					(el, handle_case (e, ek))
				) el_e_l, Some def) } in
				ret, !k
			| TTry (e, catches) ->
				let e, k = process_expr e in
				let k = ref k in
				let ret = { expr with eexpr = TTry(e, List.map (fun (v, e) ->
					let e, ek = process_expr e in
					k := aggregate_kind !k ek;
					(v, e)
				) catches) } in
				ret, !k
			| _ -> expr, Normal
	in

	let run e = fst (process_expr e) in
	run

let priority = min_dep -. 100.0

let configure gen java_mode =
	let run = init gen java_mode in
	gen.gsyntax_filters#add "unreachable_synf" (PCustom priority) run
