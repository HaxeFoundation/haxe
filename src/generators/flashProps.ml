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

let is_getter_name name = ExtString.String.starts_with name "get_"
let is_setter_name name = ExtString.String.starts_with name "set_"
let get_property_name accessor_name = String.sub accessor_name 4 (String.length accessor_name - 4)
let is_flash_property cf = Meta.has Meta.FlashProperty cf.cf_meta

let find_property_for_accessor ~isget cl tl accessor_name =
	let prop_name = get_property_name accessor_name in
	try
		match Type.class_field cl tl prop_name with
		| Some (prop_cl, prop_tl), _, prop_cf ->
			(match prop_cf.cf_kind with
			| Var { v_read = AccCall; v_write = AccCall | AccNever } when isget && is_flash_property prop_cf -> Some (prop_cl, prop_tl, prop_cf)
			| Var { v_read = AccCall | AccNever; v_write = AccCall } when not isget && is_flash_property prop_cf -> Some (prop_cl, prop_tl, prop_cf)
			| _ -> None)
		| _ -> None
	with Not_found ->
		None

let is_extern_instance_accessor ~isget cl tl cf =
	if (has_class_flag cl CExtern) && (if isget then is_getter_name cf.cf_name else is_setter_name cf.cf_name) then
		find_property_for_accessor ~isget cl tl cf.cf_name
	else
		None

let find_static_property_for_accessor ~isget cl accessor_name =
	let prop_name = get_property_name accessor_name in
	try
		let prop_cf = PMap.find prop_name cl.cl_statics in
		(match prop_cf.cf_kind with
		| Var { v_read = AccCall; v_write = AccCall | AccNever } when isget && is_flash_property prop_cf -> Some prop_cf
		| Var { v_read = AccCall | AccNever; v_write = AccCall } when not isget && is_flash_property prop_cf -> Some prop_cf
		| _ -> None)
	with Not_found ->
		None

let is_extern_static_accessor ~isget cl cf =
	if (has_class_flag cl CExtern) && (if isget then is_getter_name cf.cf_name else is_setter_name cf.cf_name) then
		find_static_property_for_accessor ~isget cl cf.cf_name
	else
		None
