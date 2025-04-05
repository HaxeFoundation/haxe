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
open EvalValue
open EvalContext
open EvalHash

let no_field =
	vnull

let proto_field_direct proto name =
	proto.pfields.(get_proto_field_index_raise proto name)

let rec proto_field_raise proto name =
	try proto_field_direct proto name
	with Not_found -> match proto.pparent with
		| Some proto -> proto_field_raise proto name
		| _ -> raise Not_found

let instance_field vi name =
	vi.ifields.(get_instance_field_index_raise vi.iproto name)

let object_field_raise o name = match o.oproto with
	| OProto proto ->
		o.ofields.(get_instance_field_index_raise proto name)
	| ODictionary l ->
		IntMap.find name l

let object_field o name =
	try object_field_raise o name with Not_found -> vnull

let field_raise v f =
	match vresolve v with
	| VObject o -> object_field_raise o f
	| VInstance {ikind = IBytes s} when f = key_length -> vint (Bytes.length s)
	| VPrototype proto -> proto_field_raise proto f
	| VArray va ->
		if f = key_length then vint (va.alength)
		else proto_field_direct (get_ctx()).array_prototype f
	| VVector vv ->
		if f = key_length then vint (Array.length vv)
		else proto_field_direct (get_ctx()).vector_prototype f
	| VString s ->
		if f = key_length then vint (s.slength)
		else proto_field_direct (get_ctx()).string_prototype f
	| VInstance vi -> (try instance_field vi f with Not_found -> proto_field_raise vi.iproto f)
	| _ -> raise Not_found

let field v f =
	try field_raise v f with Not_found -> no_field

let dynamic_field v name = match field v name with
	| VFunction(f,false) -> vfield_closure v f
	| v -> v