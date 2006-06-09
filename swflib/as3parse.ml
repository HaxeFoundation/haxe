(*
 *  This file is part of SwfLib
 *  Copyright (c)2004-2006 Nicolas Cannasse
 *
 *  This program is free software; you can redistribute it and/or modify
 *  it under the terms of the GNU General Public License as published by
 *  the Free Software Foundation; either version 2 of the License, or
 *  (at your option) any later version.
 *
 *  This program is distributed in the hope that it will be useful,
 *  but WITHOUT ANY WARRANTY; without even the implied warranty of
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 *  GNU General Public License for more details.
 *
 *  You should have received a copy of the GNU General Public License
 *  along with this program; if not, write to the Free Software
 *  Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
 *)
open As3

(* ************************************************************************ *)
(* PARSING *)

let read_as3_int ch =
	let a = IO.read_byte ch in
	if a < 128 then
		Int32.of_int a
	else
	let a = a land 127 in
	let b = IO.read_byte ch in
	if b < 128 then
		Int32.of_int ((b lsl 7) lor a)
	else
	let b = b land 127 in
	let c = IO.read_byte ch in
	if c < 128 then
		Int32.of_int ((c lsl 14) lor (b lsl 7) lor a)
	else
	let c = c land 127 in
	let d = IO.read_byte ch in
	if d < 128 then
		Int32.of_int ((d lsl 21) lor (c lsl 14) lor (b lsl 7) lor a)
	else
	let d = d land 127 in
	let e = IO.read_byte ch in
	(* check byte order *)
	assert false

let read_int ch =
	Int32.to_int (read_as3_int ch)

let read_ident ch =
	IO.nread ch (read_int ch)

let read_base_right idents ch =
	let k = IO.read_byte ch in
	let p = read_int ch - 1 in
	if p >= Array.length idents then assert false;
	let has_namespace = p <> -1 && String.length idents.(p) <> 0 in
	match k with
	| 0x05 ->
		if has_namespace then assert false;
		A3RPrivate
	| 0x16 ->
		A3RPublic (if has_namespace then Some p else None)
	| 0x17 ->
		A3RInternal (if has_namespace then Some p else None)
	| 0x18 ->
		if not has_namespace then assert false;
		A3RProtected p
	| _ ->
		assert false

let read_rights base_rights ch =
	let rec loop n =
		if n = 0 then
			[]
		else
			let r = read_int ch in
			if r >= Array.length base_rights then assert false;
			r :: loop (n - 1)
	in
	loop (IO.read_byte ch)

let read_type ctx ch =
	match IO.read_byte ch with
	| 0x09 ->
		let id = read_int ch - 1 in
		if id < 0 || id >= Array.length ctx.as3_idents then assert false;
		let rights = read_int ch in
		if rights >= Array.length ctx.as3_rights then assert false;
		A3TClassInterface (id,rights)
	| 0x07 ->
		let rights = read_int ch - 1in
		if rights < 0 || rights >= Array.length ctx.as3_base_rights then assert false;
		let id = read_int ch - 1 in
		if id < 0 || id >= Array.length ctx.as3_idents then assert false;
		A3TMethodVar (id,rights)
	| _ ->
		assert false

let read_list ch f =	
	match read_int ch with
	| 0 -> [||]
	| n -> Array.init (n - 1) (fun _ -> f ch)

let as3_parse ch =
	if IO.read_i32 ch <> 0x00000001 then assert false;
	let frame = IO.read_string ch in
	if IO.read_i32 ch <> 0x002E0010 then assert false;
	let ints = read_list ch read_as3_int in
	if IO.read_byte ch <> 0 then assert false;
	let floats = read_list ch IO.read_double in
	let idents = read_list ch read_ident in
	let base_rights = read_list ch (read_base_right idents) in
	let rights = read_list ch (read_rights base_rights) in
	let ctx = {
		as3_frame = frame;
		as3_ints = ints;
		as3_floats = floats;
		as3_idents = idents;
		as3_base_rights = base_rights;
		as3_rights = rights;
		as3_types = [||];
		as3_method_types = [||];
		as3_classes = [||];
		as3_statics = [||];
		as3_unknown = "";
	} in
	ctx.as3_types <- read_list ch (read_type ctx);
(*
	ctx.as3_method_types <- read_list ch (read_method_type ctx);
	ctx.as3_classes <- read_list ch (read_class ctx);
	ctx.as3_static <- read_list ch (read_static ctx);
*)
	ctx.as3_unknown <- IO.read_all ch;
	ctx

(* ************************************************************************ *)
(* WRITING *)

let as3_write ch ctx =
	()

