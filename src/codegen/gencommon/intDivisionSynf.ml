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
	On targets that support int division, this module will force a float division to be performed.
	It will also look for casts to int or use of Std.int() to optimize this kind of operation.

	dependencies:
		since it depends on nothing, but many modules might generate division expressions,
		it will be one of the last modules to run
*)
let init com =
	let rec is_int e =
		let rec is_int_type t =
			match follow t with
				| TInst ({ cl_path = (["haxe";"lang"],"Null") }, [t]) ->
					is_int_type t
				| t ->
					like_int t && not (like_i64 t)
		in
		is_int_type e.etype || begin
			match e.eexpr with
			| TUnop (_, _, e) -> is_int e
			| _ -> false
		end
	in
	let rec is_exactly_int e =
		match follow e.etype with
		| TAbstract ({ a_path = ([],"Int") }, []) -> true
		| _ ->
			match e.eexpr with
			| TUnop (_, _, e) -> is_exactly_int e
			| _ -> false
	in
	let rec run e =
		match e.eexpr with
		| TBinop ((OpDiv as op), e1, e2) when is_int e1 && is_int e2 ->
			{ e with eexpr = TBinop (op, mk_cast com.basic.tfloat (run e1), run e2) }
		| TCall (
				{ eexpr = TField (_, FStatic ({ cl_path = ([], "Std") }, { cf_name = "int" })) },
				[ { eexpr = TBinop ((OpDiv as op), e1, e2) } as ebinop ]
			) when is_int e1 && is_int e2 ->
			let e = { ebinop with eexpr = TBinop (op, run e1, run e2); etype = com.basic.tint } in
			if not (is_exactly_int e1 && is_exactly_int e2) then
				mk_cast com.basic.tint e
			else
				e
		| TCast ({ eexpr = TBinop((OpDiv as op), e1, e2) } as ebinop, _ )
		| TCast ({ eexpr = TBinop(((OpAssignOp OpDiv) as op), e1, e2) } as ebinop, _ ) when is_int e1 && is_int e2 && is_int e ->
			let ret = { ebinop with eexpr = TBinop (op, run e1, run e2); etype = e.etype } in
			if not (is_exactly_int e1 && is_exactly_int e2) then
				mk_cast e.etype ret
			else
				Type.map_expr run e

		| _ ->
			Type.map_expr run e
	in
	run

let name = "int_division_synf"
let priority = solve_deps name [ DAfter ExpressionUnwrap.priority; DAfter ObjectDeclMap.priority; DAfter ArrayDeclSynf.priority ]

let configure gen =
	let run = init gen.gcon in
	gen.gsyntax_filters#add name (PCustom priority) run
