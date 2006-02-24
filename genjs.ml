(*
 *  Haxe Compiler
 *  Copyright (c)2005 Nicolas Cannasse
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
open Type

type ctx = {
	buf : Buffer.t;
}

let generate_class ctx c = 
	()

let generate_enum ctx e =
	()

let generate_type ctx = function
	| TClassDecl c -> generate_class ctx c
	| TEnumDecl e -> generate_enum ctx e

let generate file types =
	let ctx = {
		buf = Buffer.create 16000;
	} in
	List.iter (generate_type ctx) types;
	let ch = open_out file in
	output_string ch (Buffer.contents ctx.buf);
	close_out ch
