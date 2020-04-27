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

(*
	A syntax filter that will change array declarations to the actual native array declarations plus
	the haxe array initialization

	dependencies:
		Must run after ObjectDeclMap since it can add TArrayDecl expressions
*)
let init (native_array_cl : tclass) (change_type_params : module_type -> t list -> t list) =
	let rec run e =
		match e.eexpr with
		| TArrayDecl el ->
			let cl, params = match follow e.etype with
				| TInst(({ cl_path = ([], "Array") } as cl), ( _ :: _  as params)) -> cl, params
				| TInst(({ cl_path = ([], "Array") } as cl), []) -> cl, [t_dynamic]
				| _ -> Globals.die ""
			in
			let params = change_type_params (TClassDecl cl) params in
			let e_inner_decl = mk (TArrayDecl (List.map run el)) (TInst (native_array_cl, params)) e.epos in
			mk (TNew (cl, params, [e_inner_decl])) e.etype e.epos
		| _ ->
			Type.map_expr run e
	in
	run

let name = "array_decl_synf"
let priority = solve_deps name [DAfter ObjectDeclMap.priority]

let configure gen native_array_cl change_type_params =
	let run = init native_array_cl change_type_params in
	gen.gsyntax_filters#add name (PCustom priority) run
