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
open Type
open Gencommon

(* ******************************************* *)
(* Closure Detection *)
(* ******************************************* *)
(*
	Just a small utility filter that detects when a closure must be created.
	On the default implementation, this means when a function field is being accessed
	not via reflection and not to be called instantly

	dependencies:
		must run after DynamicFieldAccess, so any TAnon { ClassStatics / EnumStatics } will be changed to the corresponding TTypeExpr
*)
let name = "filter_closures"
let priority = solve_deps name [DAfter DynamicFieldAccess.priority]

let configure gen (should_change:texpr->string->bool) (filter:texpr->texpr->string->bool->texpr) =
	let rec run e =
		match e.eexpr with
			(*(* this is precisely the only case where we won't even ask if we should change, because it is a direct use of TClosure *)
			| TCall ( {eexpr = TClosure(e1,s)} as clos, args ) ->
				{ e with eexpr = TCall({ clos with eexpr = TClosure(run e1, s) }, List.map run args ) }
			| TCall ( clos, args ) ->
				let rec loop clos = match clos.eexpr with
					| TClosure(e1,s) -> Some (clos, e1, s)
					| TParenthesis p -> loop p
					| _ -> None
				in
				let clos = loop clos in
				(match clos with
					| Some (clos, e1, s) -> { e with eexpr = TCall({ clos with eexpr = TClosure(run e1, s) }, List.map run args ) }
					| None -> Type.map_expr run e)*)
				| TCall({ eexpr = TIdent "__delegate__" } as local, [del]) ->
					{ e with eexpr = TCall(local, [Type.map_expr run del]) }
				| TCall(({ eexpr = TField(_, _) } as ef), params) ->
					{ e with eexpr = TCall(Type.map_expr run ef, List.map run params) }
				| TField(ef, FEnum(en, field)) ->
						(* FIXME replace t_dynamic with actual enum Anon field *)
						let ef = run ef in
						(match follow field.ef_type with
							| TFun _ when should_change ef field.ef_name ->
								filter e ef field.ef_name true
							| _ ->
									{ e with eexpr = TField(ef, FEnum(en,field)) }
						)
				| TField(({ eexpr = TTypeExpr _ } as tf), f) ->
					(match field_access_esp gen tf.etype (f) with
						| FClassField(_,_,_,cf,_,_,_) ->
							(match cf.cf_kind with
								| Method(MethDynamic)
								| Var _ ->
									e
								| _ when should_change tf cf.cf_name ->
									filter e tf cf.cf_name true
								| _ ->
									e
							)
						| _ -> e)
				| TField(e1, FClosure (Some _, cf)) when should_change e1 cf.cf_name ->
					(match cf.cf_kind with
					| Method MethDynamic | Var _ ->
						Type.map_expr run e
					| _ ->
						filter e (run e1) cf.cf_name false)
				| _ -> Type.map_expr run e
	in
	gen.gexpr_filters#add name (PCustom priority) run
