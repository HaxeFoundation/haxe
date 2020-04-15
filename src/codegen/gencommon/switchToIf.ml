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
open Type
open Codegen
open Gencommon

(* ******************************************* *)
(* SwitchToIf *)
(* ******************************************* *)
(*
	A syntax filter which changes switch expressions to if() else if() else if() ...

	Also it handles switches on native enums (which are not converted to classes) by
	rewriting the switch expression to what's supported directly by the targets.
*)
let name = "switch_to_if"
let priority = solve_deps name []

let rec simplify_expr e =
	match e.eexpr with
	| TParenthesis e | TMeta (_, e) -> simplify_expr e
	| _ -> e

let configure gen (should_convert:texpr->bool) =
	let basic = gen.gcon.basic in
	let rec run e =
		match e.eexpr with
		| TSwitch (cond, cases, default) when should_convert e ->
			let cond_etype, should_cache =
				match gen.gfollow#run_f cond.etype with
				| TAbstract ({ a_path = [], "Null" }, [t]) ->
					let rec take_off_nullable t =
						match gen.gfollow#run_f t with
						| TAbstract ({ a_path = [], "Null" }, [t]) -> take_off_nullable t
						| _ -> t
					in
					take_off_nullable t, true
				| _ ->
					cond.etype, false
			in

			if should_cache && not (should_convert { e with eexpr = TSwitch ({ cond with etype = cond_etype }, cases, default) }) then begin
				{ e with eexpr = TSwitch (mk_cast cond_etype (run cond), List.map (fun (cs,e) -> (List.map run cs, run e)) cases, Option.map run default) }
			end else begin
				let local, fst_block =
					match cond.eexpr, should_cache with
					| TLocal _, false ->
						cond, []
					| _ ->
						let var = mk_temp "switch" cond_etype in
						let cond = run cond in
						let cond = if should_cache then mk_cast cond_etype cond else cond in
						mk_local var cond.epos, [ mk (TVar (var,Some cond)) basic.tvoid cond.epos ]
				in

				let mk_eq cond =
					mk (TBinop (Ast.OpEq, local, cond)) basic.tbool cond.epos
				in

				let rec mk_many_cond conds =
					match conds with
					| cond :: [] ->
						mk_eq cond
					| cond :: tl ->
						mk (TBinop (Ast.OpBoolOr, mk_eq (run cond), mk_many_cond tl)) basic.tbool cond.epos
					| [] ->
						Globals.die ""
				in

				let mk_many_cond conds =
					let ret = mk_many_cond conds in
					(*
						this might be considered a hack. But since we're on a syntax filter and
						the condition is guaranteed to not have run twice, we can really run the
						expr filters again for it (to change e.g. OpEq accordingly)
					*)
					gen.gexpr_filters#run ret
				in

				let rec loop cases =
					match cases with
					| (conds, e) :: [] ->
						mk (TIf (mk_many_cond conds, run e, Option.map run default)) e.etype e.epos
					| (conds, e) :: tl ->
						mk (TIf (mk_many_cond conds, run e, Some (loop tl))) e.etype e.epos
					| [] ->
						match default with
						| None ->
							raise Exit
						| Some d ->
							run d
				in

				try
					{ e with eexpr = TBlock (fst_block @ [loop cases]) }
				with Exit ->
					{ e with eexpr = TBlock [] }
			end

		(*
			Convert a switch on a non-class enum (e.g. native enums) to the native switch,
			effectively chancing `switch enumIndex(e) { case 1: ...; case 2: ...; }` to
			`switch e { case MyEnum.A: ...; case MyEnum.B: ...; }`, which is supported natively
			by some target languages like Java and C#.
		*)
		| TSwitch (cond, cases, default) ->
			begin
				try
					match (simplify_expr cond).eexpr with
					| TEnumIndex enum
					| TCall  ({ eexpr = TField (_, FStatic ({ cl_path = [],"Type" }, { cf_name = "enumIndex" })) }, [enum]) ->
						let real_enum =
							match enum.etype with
							| TEnum (e, _) -> e
							| _ -> raise Not_found
						in
						if Meta.has Meta.Class real_enum.e_meta then
							raise Not_found;

						let fields = Hashtbl.create (List.length real_enum.e_names) in
						PMap.iter (fun _ ef -> Hashtbl.add fields ef.ef_index ef) real_enum.e_constrs;

						let enum_expr = Texpr.Builder.make_typeexpr (TEnumDecl real_enum) e.epos in
						let cases = List.map (fun (patterns, body) ->
							let patterns = List.map (fun e ->
								match e.eexpr with
								| TConst (TInt i) ->
									let ef = Hashtbl.find fields (Int32.to_int i) in
									{ e with eexpr = TField (enum_expr, FEnum (real_enum, ef)); etype = TEnum (real_enum, List.map (fun _ -> t_dynamic) real_enum.e_params) }
								| _ ->
									raise Not_found
							) patterns in
							let body = run body in
							patterns, body
						) cases in
						{ e with eexpr = TSwitch (enum, cases, Option.map run default) }
					| _ ->
						raise Not_found
				with Not_found ->
					Type.map_expr run e
			end
		| _ ->
			Type.map_expr run e
	in
	gen.gsyntax_filters#add name (PCustom priority) run
