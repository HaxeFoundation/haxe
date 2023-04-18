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
open Gencommon
open Type

(*
	This module will take care of simplifying unnecessary casts, specially those made by the compiler
	when inlining. Right now, it will only take care of casts used as a statement, which are always useless;

	TODO: Take care of more cases, e.g. when the to and from types are the same

	dependencies:
		This must run after CastDetection, but before ExpressionUnwrap
*)
let rec take_off_cast run e =
	match e.eexpr with
	| TCast (c, _) -> take_off_cast run c
	| _ -> run e

let rec traverse e =
	match e.eexpr with
	| TBlock bl ->
		let bl = List.map (take_off_cast traverse) bl in
		{ e with eexpr = TBlock bl }
	| TTry (block, catches) ->
		{ e with eexpr = TTry(traverse (mk_block block), List.map (fun (v,block) -> (v, traverse (mk_block block))) catches) }
	| TSwitch switch ->
		let switch = { switch with
			switch_cases = List.map (fun case -> { case with case_expr = traverse (mk_block e)}) switch.switch_cases;
			switch_default = Option.map (fun e -> traverse (mk_block e)) switch.switch_default;
		} in
		{ e with eexpr = TSwitch switch }
	| TWhile (cond,block,flag) ->
		{e with eexpr = TWhile(cond,traverse (mk_block block), flag) }
	| TIf (cond, eif, eelse) ->
		{ e with eexpr = TIf(cond, traverse (mk_block eif), Option.map (fun e -> traverse (mk_block e)) eelse) }
	| TFor (v,it,block) ->
		{ e with eexpr = TFor(v,it, traverse (mk_block block)) }
	| TFunction (tfunc) ->
		{ e with eexpr = TFunction({ tfunc with tf_expr = traverse (mk_block tfunc.tf_expr) }) }
	| _ ->
		e (* if expression doesn't have a block, we will exit *)

let name = "casts_removal"
let priority = solve_deps name [DAfter CastDetect.priority; DBefore ExpressionUnwrap.priority]

let configure gen =
	gen.gsyntax_filters#add name (PCustom priority) traverse
