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

(** add abstract type parameters to abstract implementation methods *)
let add_abstract_params = function
	| TClassDecl ({ cl_kind = KAbstractImpl a } as c) ->
		List.iter (
			function
			| ({ cf_name = "_new" } as cf) ->
				cf.cf_params <- cf.cf_params @ a.a_params
			| cf when has_class_field_flag cf CfImpl ->
				(match cf.cf_expr with
				| Some({ eexpr = TFunction({ tf_args = (v, _) :: _ }) }) when Meta.has Meta.This v.v_meta ->
					cf.cf_params <- cf.cf_params @ a.a_params
				| _ -> ())
			| _ -> ()
		) c.cl_ordered_statics
	| _ -> ()

let name = "abstract_implementation_fix"
let priority = solve_deps name []

let configure gen =
	let run md = (add_abstract_params md; md) in
	gen.gmodule_filters#add name (PCustom priority) run
