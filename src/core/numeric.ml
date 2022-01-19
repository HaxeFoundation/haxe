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

let parse_float s =
	let rec loop sp i =
		if i = String.length s then (if sp = 0 then s else String.sub s sp (i - sp)) else
		match String.unsafe_get s i with
		| ' ' | '\t' when sp = i -> loop (sp + 1) (i + 1)
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
	let rec loop sp i digits_count =
		if i = String.length s then (if sp = 0 then s else String.sub s sp (i - sp)) else
		match String.unsafe_get s i with
		| '0'..'9' -> loop sp (i + 1) (digits_count + 1)
		| ' ' | '+' when sp = i -> loop (sp + 1) (i + 1) digits_count
		| c when sp = i && Char.code c > 8 && Char.code c < 14 -> loop (sp + 1) (i + 1) digits_count
		| '-' when i = sp -> loop sp (i + 1) digits_count
		| ('x' | 'X') when digits_count = 1 && String.get s (i - 1) = '0' -> loop_hex sp (i + 1)
		| _ -> String.sub s sp (i - sp)
	in
	Int32.of_string (loop 0 0 0)