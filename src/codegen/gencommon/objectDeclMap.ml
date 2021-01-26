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
(* Object Declaration Mapper *)
(* ******************************************* *)
let name = "object_decl_map"
let priority = solve_deps name []

let configure gen map_fn =
	let rec run e =
		match e.eexpr with
		| TObjectDecl odecl ->
			let e = Type.map_expr run e in
			(match e.eexpr with TObjectDecl odecl -> map_fn e odecl | _ -> Globals.die "" __LOC__)
		| _ ->
			Type.map_expr run e
	in
	gen.gsyntax_filters#add name (PCustom priority) run
