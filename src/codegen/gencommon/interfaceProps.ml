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
open Globals
open Type

(*
	This module filter will go through all declared properties, and see if they are conforming to a native interface.
	If they are, it will add Meta.Property to it.
*)
let run = function
	| TClassDecl cl when (has_class_flag cl CInterface) && not (has_class_flag cl CExtern) ->
		let vars = List.fold_left (fun acc (iface,_) ->
			if Meta.has Meta.CsNative iface.cl_meta then
				let props = List.filter (fun cf -> match cf.cf_kind with Var { v_read = AccCall } | Var { v_write = AccCall } -> true | _ -> false) iface.cl_ordered_fields in
				props @ acc
			else
				acc
		) [] cl.cl_implements in
		if vars <> [] then
			let vars = List.map (fun cf -> cf.cf_name) vars in
			List.iter (fun cf -> match cf.cf_kind with
				| Var { v_read = AccCall } | Var { v_write = AccCall } when List.mem cf.cf_name vars ->
					cf.cf_meta <- (Meta.Property, [], null_pos) :: cf.cf_meta
				| _ -> ()
			) cl.cl_ordered_fields
	| _ ->
		()
