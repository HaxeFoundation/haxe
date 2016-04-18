(*
	The Haxe Compiler
	Copyright (C) 2005-2016  Haxe Foundation

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

type t =
	| JString of string
	| JFloat of float
	| JInt of int
	| JObject of (string * t) list
	| JArray of t list
	| JBool of bool
	| JNull

let write_iter f_el f_sep l =
	let rec rest = function
		| [] -> ()
		| v :: l ->
			f_sep();
			f_el v;
			rest l
	in
	match l with
	| [] -> ()
	| v :: l ->
		f_el v;
		rest l

let write_sep w =
	w ","

let rec write_json w v =
	match v with
	| JNull -> write_null w
	| JBool b -> write_bool w b
	| JString s -> write_string w s
	| JFloat f -> write_float w f
	| JInt i -> write_int w i
	| JObject o -> write_object w o
	| JArray a -> write_array w a

and write_null w =
	w "null"

and write_bool w b =
	w (if b then "true" else "false")

and write_string w s =
	w "\"";
	let b = Buffer.create (String.length s) in
	for i = 0 to String.length s - 1 do
		match String.unsafe_get s i with
		| '"' -> Buffer.add_string b "\\\""
		| '\t' -> Buffer.add_string b "\\t"
		| '\r' -> Buffer.add_string b "\\r"
		| '\b' -> Buffer.add_string b "\\b"
		| '\n' -> Buffer.add_string b "\\n"
		| '\012' -> Buffer.add_string b "\\f"
		| '\\' -> Buffer.add_string b "\\\\"
		| '\x00'..'\x1F' | '\x7F' as c -> Buffer.add_string b (Printf.sprintf "\\u%04X" (int_of_char c))
		| c -> Buffer.add_char b c
	done;
	w (Buffer.contents b);
	w "\""

and write_int w i =
	w (string_of_int i)

and write_float w f =
	match classify_float f with
	| FP_nan | FP_infinite -> failwith "NaN and infinity floats are unsupported in JSON"
	| _ ->
		let s = Printf.sprintf "%.16g" f in
		let s = if float_of_string s = f then s else Printf.sprintf "%.17g" f in
		w s

and write_array w a =
	w "[";
	write_iter (write_json w) (fun() -> write_sep w) a;
	w "]"

and write_object w o =
	let write_el (k, v) =
		write_string w k;
		w ":";
		write_json w v
	in
	w "{";
	write_iter write_el (fun() -> write_sep w) o;
	w "}"
