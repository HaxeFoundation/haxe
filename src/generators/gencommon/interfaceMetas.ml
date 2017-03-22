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
(* InterfaceMetas *)
(* ******************************************* *)
(*
	Deal with metadata on interfaces by taking it off from interface, and adding a new class with `_HxMeta` suffix

	dependencies:
		Must run before InitFunction
*)
let name = "interface_metas"
let priority = solve_deps name [ DBefore InitFunction.priority ]

let configure gen =
	let run md =
		match md with
		| TClassDecl ({ cl_interface = true; cl_ordered_statics = (_ :: _) } as cl) ->
			cl.cl_ordered_statics <- [];
			let path = fst cl.cl_path,snd cl.cl_path ^ "_HxMeta" in
			(match Codegen.build_metadata gen.gcon (TClassDecl cl) with
			| Some expr ->
				let ncls = mk_class cl.cl_module path cl.cl_pos in
				let cf = mk_class_field "__meta__" expr.etype false expr.epos (Var { v_read = AccNormal; v_write = AccNormal }) [] in
				cf.cf_expr <- Some expr;
				ncls.cl_statics <- PMap.add "__meta__" cf ncls.cl_statics;
				ncls.cl_ordered_statics <- cf :: ncls.cl_ordered_statics;
				gen.gadd_to_module (TClassDecl(ncls)) priority;
			| _ -> ())
		| _ -> ()
	in
	let map md = run md; Some(md) in
	gen.gmodule_filters#add ~name:name ~priority:(PCustom priority) map
