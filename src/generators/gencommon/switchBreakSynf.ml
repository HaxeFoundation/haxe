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
open Type
open Gencommon

(* ******************************************* *)
(* SwitchBreakSynf *)
(* ******************************************* *)
(*
	In most languages, 'break' is used as a statement also to break from switch statements.
	This generates an incompatibility with haxe code, as we can use break to break from loops from inside a switch

	This script will detect 'breaks' inside switch statements, and will offer the opportunity to change both
	when this pattern is found.

	Some options are possible:
		On languages that support goto, 'break' may mean goto " after the loop ". There also can be special labels for
			loops, so you can write "break label" (javascript, java, d)
		On languages that do not support goto, a custom solution must be enforced

	dependencies:
		Since UnreachableCodeElimination must run before it, and Unreachable should be one of the
		very last filters to run, we will make a fixed value which runs after UnreachableCodeElimination
		(meaning: it's the very last filter)
*)
let name = "switch_break_synf"
let priority = min_dep -. 150.0

type add_to_block_api = texpr->bool->unit

let configure gen (change_loop:texpr->int->add_to_block_api->texpr) (change_break:texpr->int->add_to_block_api->texpr) =
	let in_switch = ref false in
	let cur_block = ref [] in
	let to_add = ref [] in
	let did_found = ref (-1) in

	let api expr before =
		if before then cur_block := expr :: !cur_block else to_add := expr :: !to_add
	in
	let num = ref 0 in
	let cur_num = ref 0 in

	let rec run e =
		match e.eexpr with
		| TFunction _ ->
			let old_num = !num in
			num := 0;
				let ret = Type.map_expr run e in
			num := old_num;
			ret
		| TFor _
		| TWhile _ ->
			let last_switch = !in_switch in
			let last_found = !did_found in
			let last_num = !cur_num in
			in_switch := false;
			incr num;
			cur_num := !num;
			did_found := -1;
				let new_e = Type.map_expr run e in (* assuming that no loop will be found in the condition *)
				let new_e = if !did_found <> -1 then change_loop new_e !did_found api else new_e in
			did_found := last_found;
			in_switch := last_switch;
			cur_num := last_num;

			new_e
		| TSwitch _ ->
			let last_switch = !in_switch in
			in_switch := true;

				let new_e = Type.map_expr run e in

			in_switch := last_switch;
			new_e
		| TBlock bl ->
			let last_block = !cur_block in
			let last_toadd = !to_add in
			to_add := [];
			cur_block := [];

				List.iter (fun e ->
					let new_e = run e in
					cur_block := new_e :: !cur_block;
					match !to_add with
						| [] -> ()
						| _ -> cur_block := !to_add @ !cur_block; to_add := []
				) bl;

			let ret = List.rev !cur_block in
			cur_block := last_block;
			to_add := last_toadd;

			{ e with eexpr = TBlock(ret) }
		| TBreak ->
			if !in_switch then (did_found := !cur_num; change_break e !cur_num api) else e
		| _ -> Type.map_expr run e
	in
	let map e = Some(run e) in
	gen.gsyntax_filters#add ~name:name ~priority:(PCustom priority) map
