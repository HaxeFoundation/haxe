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
open Globals
open Type
open Gencommon

(* ******************************************* *)
(* Interface Variables Removal Modf *)
(* ******************************************* *)
(*
	This module filter will take care of sanitizing interfaces for targets that do not support
	variables declaration in interfaces. By now this will mean that if anything is typed as the interface,
	and a variable access is made, a FNotFound will be returned for the field_access, so
	the field will be only accessible by reflection.
	Speed-wise, ideally it would be best to create getProp/setProp functions in this case and change
	the AST to call them when accessing by interface. (TODO)
	But right now it will be accessed by reflection.
*)
let name = "interface_vars"
let priority = solve_deps name []

let configure gen =
	let run md =
		match md with
		| TClassDecl cl when (has_class_flag cl CInterface) ->
			let to_add = ref [] in
			let fields = List.filter (fun cf ->
				match cf.cf_kind with
				| Var _ when gen.gcon.platform = Cs && Meta.has Meta.Event cf.cf_meta ->
					true
				| Var vkind when Type.is_physical_field cf || not (Meta.has Meta.Property cf.cf_meta) ->
					(match vkind.v_read with
						| AccCall ->
							let newcf = mk_class_field ("get_" ^ cf.cf_name) (TFun([],cf.cf_type)) true cf.cf_pos (Method MethNormal) [] in
							to_add := newcf :: !to_add;
						| _ -> ()
					);
					(match vkind.v_write with
						| AccCall ->
							let newcf = mk_class_field ("set_" ^ cf.cf_name) (TFun(["val",false,cf.cf_type],cf.cf_type)) true cf.cf_pos (Method MethNormal) [] in
							to_add := newcf :: !to_add;
						| _ -> ()
					);
					cl.cl_fields <- PMap.remove cf.cf_name cl.cl_fields;
					false
				| Method MethDynamic ->
					(* TODO OPTIMIZATION - add a `_dispatch` method to the interface which will call the dynamic function itself *)
					cl.cl_fields <- PMap.remove cf.cf_name cl.cl_fields;
					false
				| _ ->
					true
			) cl.cl_ordered_fields in

			cl.cl_ordered_fields <- fields;

			List.iter (fun cf ->
				match field_access gen (TInst(cl,extract_param_types cl.cl_params)) cf.cf_name with
				| FNotFound | FDynamicField _ ->
					cl.cl_ordered_fields <- cf :: cl.cl_ordered_fields;
					cl.cl_fields <- PMap.add cf.cf_name cf cl.cl_fields
				| _ ->
					()
			) !to_add
		| _ -> ()
	in
	let map md = run md; md in
	gen.gmodule_filters#add name (PCustom priority) map
