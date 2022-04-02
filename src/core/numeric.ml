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

(*  Taken from OCaml source typing/oprint.ml

	This is a better version of string_of_float which prints without loss of precision
	so that float_of_string (float_repres x) = x for all floats x
*)
let valid_float_lexeme s =
	let l = String.length s in
	let rec loop i =
		if i >= l then s ^ "." else
		match s.[i] with
		| '0' .. '9' | '-' -> loop (i+1)
		| _ -> s
	in loop 0

let float_repres f =
	match classify_float f with
	| FP_nan -> "nan"
	| FP_infinite ->
		if f < 0.0 then "neg_infinity" else "infinity"
	| _ ->
		let float_val =
			let s1 = Printf.sprintf "%.12g" f in
			if f = float_of_string s1 then s1 else
			let s2 = Printf.sprintf "%.15g" f in
			if f = float_of_string s2 then s2 else
			Printf.sprintf "%.18g" f
		in valid_float_lexeme float_val

let is_whitespace code =
	code > 8 && code < 14

let parse_float s =
	let rec loop sp i =
		if i = String.length s then (if sp = 0 then s else String.sub s sp (i - sp)) else
		match String.unsafe_get s i with
		| ' ' when sp = i -> loop (sp + 1) (i + 1)
		| c when sp = i && is_whitespace (Char.code c) -> loop (sp + 1) (i + 1)
		| '0'..'9' | '-' | '+' | 'e' | 'E' | '.' -> loop sp (i + 1)
		| _ -> String.sub s sp (i - sp)
	in
	float_of_string (loop 0 0)

let parse_int s =
	let rec loop_hex sp i =
		if i = String.length s then
			String.sub s sp (i - sp)
		else
			match String.unsafe_get s i with
			| '0'..'9' | 'a'..'f' | 'A'..'F' -> loop_hex sp (i + 1)
			| _ -> String.sub s sp (i - sp)
	in
	let rec loop_dec sp i =
		if i = String.length s then (if sp = 0 then s else String.sub s sp (i - sp)) else
		match String.unsafe_get s i with
		| '0'..'9' -> loop_dec sp (i + 1)
		| _ -> String.sub s sp (i - sp)
	in
	let handle_digits sp i =
		if i + 1 < String.length s && String.get s i = '0' &&
			(String.get s (i + 1) = 'x' || String.get s (i + 1) = 'X')
		then loop_hex sp (i + 2)
		else loop_dec sp i
	in
	let rec loop sp =
		if sp = String.length s then "" else
		match String.unsafe_get s sp with
		| ' ' -> loop (sp + 1)
		| '+' -> handle_digits (sp + 1) (sp + 1)
		| '-' -> handle_digits sp (sp + 1)
		| c when is_whitespace (Char.code c) -> loop (sp + 1)
		| _ -> handle_digits sp sp
	in
	Int32.of_string (loop 0)
