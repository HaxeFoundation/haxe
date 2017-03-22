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
open Gencommon

(* ******************************************* *)
(* Dynamic TArray Handling *)
(* ******************************************* *)
(*
	In some languages you cannot overload the [] operator,
	so we need to decide what is kept as TArray and what gets mapped.

	depends on:
		(syntax) must run before expression/statment normalization because it may generate complex expressions
		(ok) must run before binop transformations because it may generate some untreated binop ops
		(ok) must run before dynamic field access is transformed into reflection
*)
let name = "dyn_tarray"
let priority = solve_deps name [DBefore DynamicOperators.priority; DBefore DynamicFieldAccess.priority]

(* should change signature: tarray expr -> binop operation -> should change? *)
let configure gen (should_change:texpr->Ast.binop option->bool) (get_fun:string) (set_fun:string) =
	let basic = gen.gcon.basic in
	let mk_get e e1 e2 =
		let efield = mk_field_access gen e1 get_fun e.epos in
		{ e with eexpr = TCall(efield, [e2]) }
	in
	let mk_set e e1 e2 evalue =
		let efield = mk_field_access gen e1 set_fun e.epos in
		{ e with eexpr = TCall(efield, [e2; evalue]) }
	in
	let rec run e =
		match e.eexpr with
			| TArray(e1, e2) ->
				(* e1 should always be a var; no need to map there *)
				if should_change e None then mk_get e (run e1) (run e2) else Type.map_expr run e
			| TBinop (Ast.OpAssign, ({ eexpr = TArray(e1a,e2a) } as earray), evalue) when should_change earray (Some Ast.OpAssign) ->
				mk_set e (run e1a) (run e2a) (run evalue)
			| TBinop (Ast.OpAssignOp op,({ eexpr = TArray(e1a,e2a) } as earray) , evalue) when should_change earray (Some (Ast.OpAssignOp op)) ->
				(* cache all arguments in vars so they don't get executed twice *)
				(* let ensure_local gen block name e = *)
				let block = ref [] in

				let arr_local = ensure_local gen block "array" (run e1a) in
				let idx_local = ensure_local gen block "index" (run e2a) in
				block := (mk_set e arr_local idx_local ( { e with eexpr=TBinop(op, mk_get earray arr_local idx_local, run evalue) } )) :: !block;

				{ e with eexpr = TBlock (List.rev !block) }
			| TUnop(op, flag, ({ eexpr = TArray(e1a, e2a) } as earray)) ->
				if should_change earray None && match op with | Not | Neg -> false | _ -> true then begin

					let block = ref [] in

					let actual_t = match op with
						| Ast.Increment | Ast.Decrement -> (match follow earray.etype with
							| TInst _ | TAbstract _ | TEnum _ -> earray.etype
							| _ -> basic.tfloat)
						| Ast.Not -> basic.tbool
						| _ -> basic.tint
					in

					let val_v = mk_temp "arrVal" actual_t in
					let ret_v = mk_temp "arrRet" actual_t in

					let arr_local = ensure_local gen block "arr" (run e1a) in
					let idx_local = ensure_local gen block "arrIndex" (run e2a) in

					let val_local = { earray with eexpr = TLocal(val_v) } in
					let ret_local = { earray with eexpr = TLocal(ret_v) } in
					(* var idx = 1; var val = x._get(idx); var ret = val++; x._set(idx, val); ret; *)
					block := { eexpr = TVar(val_v, Some(mk_get earray arr_local idx_local)); (* var val = x._get(idx) *)
											etype = gen.gcon.basic.tvoid;
											epos = e2a.epos
										} :: !block;
					block := { eexpr = TVar(ret_v, Some { e with eexpr = TUnop(op, flag, val_local) }); (* var ret = val++ *)
											etype = gen.gcon.basic.tvoid;
											epos = e2a.epos
										} :: !block;
					block := (mk_set e arr_local idx_local val_local) (*x._set(idx,val)*) :: !block;
					block := ret_local :: !block;
					{ e with eexpr = TBlock (List.rev !block) }
				end else
					Type.map_expr run e
			| _ -> Type.map_expr run e
	in
	let map e = Some(run e) in
	gen.gexpr_filters#add ~name:"dyn_tarray" ~priority:(PCustom priority) map
