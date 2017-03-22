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
(* ArrayDeclSynf *)
(* ******************************************* *)
(*
	A syntax filter that will change array declarations to the actual native array declarations plus
	the haxe array initialization

	dependencies:
		Must run after ObjectDeclMap since it can add TArrayDecl expressions
*)
let name = "array_decl_synf"
let priority = solve_deps name [DAfter ObjectDeclMap.priority]

let configure gen native_array_cl =
	let rec run e =
		match e.eexpr with
		| TArrayDecl el ->
			let cl, params = match follow e.etype with
				| TInst(({ cl_path = ([], "Array") } as cl), ( _ :: _  as params)) -> cl, params
				| TInst(({ cl_path = ([], "Array") } as cl), []) -> cl, [t_dynamic]
				| _ -> assert false
			in
			let changed_params = gen.greal_type_param (TClassDecl cl) params in
			{ e with eexpr = TNew(cl, changed_params, [ { e with eexpr = TArrayDecl(List.map run el); etype = TInst(native_array_cl, changed_params) } ]	); }
		| _ -> Type.map_expr run e
	in
	let map e = Some(run e) in
	gen.gsyntax_filters#add name (PCustom priority) map
