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

let string_of_json json =
	let b = Buffer.create 0 in
	write_json (Buffer.add_string b) json;
	Buffer.contents b;

module Reader = struct
	(*
		The following code is basically stripped down yojson (https://github.com/mjambon/yojson),
		adapted to our data structures and using sedlex instad of ocamllex.

		TODO: we could probably re-use utf-8 stuff from our extlib, but I don't know enough about it.
	*)
	open Sedlexing
	open Sedlexing.Utf8

	exception Json_error of string
	exception Int_overflow

	let dec c =
		Char.code c - 48

	let hex c =
		match (char_of_int c) with
		| '0'..'9' -> c - int_of_char '0'
		| 'a'..'f' -> c - int_of_char 'a' + 10
		| 'A'..'F' -> c - int_of_char 'A' + 10
		| _ -> assert false

	let min10 = min_int / 10 - (if min_int mod 10 = 0 then 0 else 1)
	let max10 = max_int / 10 + (if max_int mod 10 = 0 then 0 else 1)

	let json_error s = raise (Json_error s)

	let extract_positive_int lexbuf =
		let s = Sedlexing.Utf8.lexeme lexbuf in
		let n = ref 0 in
		for i = 0 to (lexeme_length lexbuf) - 1 do
			if !n >= max10 then
				raise Int_overflow
			else
				n := 10 * !n + dec s.[i]
		done;
		if !n < 0 then
			raise Int_overflow
		else
			!n

	let make_positive_int lexbuf =
		try JInt (extract_positive_int lexbuf)
		with Int_overflow -> JFloat (float_of_string (lexeme lexbuf))

	let extract_negative_int lexbuf =
		let s = Sedlexing.Utf8.lexeme lexbuf in
		let n = ref 0 in
		for i = 1 to (lexeme_length lexbuf) - 1 do
			if !n <= min10 then
				raise Int_overflow
			else
				n := 10 * !n - dec s.[i]
		done;
		if !n > 0 then
			raise Int_overflow
		else
			!n

	let make_negative_int lexbuf =
		try JInt (extract_negative_int lexbuf)
		with Int_overflow -> JFloat (float_of_string (lexeme lexbuf))

	let utf8_of_code buf x =
		let add = Buffer.add_char in

		(* Straight <= doesn't work with signed 31-bit ints *)
		let maxbits n x = x lsr n = 0 in

		if maxbits 7 x then
			(* 7 *)
			add buf (Char.chr x)
		else if maxbits 11 x then (
			(* 5 + 6 *)
			add buf (Char.chr (0b11000000 lor ((x lsr 6) land 0b00011111)));
			add buf (Char.chr (0b10000000 lor (x         land 0b00111111)))
		)
		else if maxbits 16 x then (
			(* 4 + 6 + 6 *)
			add buf (Char.chr (0b11100000 lor ((x lsr 12) land 0b00001111)));
			add buf (Char.chr (0b10000000 lor ((x lsr  6) land 0b00111111)));
			add buf (Char.chr (0b10000000 lor (x          land 0b00111111)))
		)
		else if maxbits 21 x then (
			(* 3 + 6 + 6 + 6 *)
			add buf (Char.chr (0b11110000 lor ((x lsr 18) land 0b00000111)));
			add buf (Char.chr (0b10000000 lor ((x lsr 12) land 0b00111111)));
			add buf (Char.chr (0b10000000 lor ((x lsr  6) land 0b00111111)));
			add buf (Char.chr (0b10000000 lor (x          land 0b00111111)));
		)
		else if maxbits 26 x then (
			(* 2 + 6 + 6 + 6 + 6 *)
			add buf (Char.chr (0b11111000 lor ((x lsr 24) land 0b00000011)));
			add buf (Char.chr (0b10000000 lor ((x lsr 18) land 0b00111111)));
			add buf (Char.chr (0b10000000 lor ((x lsr 12) land 0b00111111)));
			add buf (Char.chr (0b10000000 lor ((x lsr  6) land 0b00111111)));
			add buf (Char.chr (0b10000000 lor (x          land 0b00111111)));
		)
		else (
			assert (maxbits 31 x);
			(* 1 + 6 + 6 + 6 + 6 + 6 *)
			add buf (Char.chr (0b11111100 lor ((x lsr 30) land 0b00000001)));
			add buf (Char.chr (0b10000000 lor ((x lsr 24) land 0b00111111)));
			add buf (Char.chr (0b10000000 lor ((x lsr 18) land 0b00111111)));
			add buf (Char.chr (0b10000000 lor ((x lsr 12) land 0b00111111)));
			add buf (Char.chr (0b10000000 lor ((x lsr  6) land 0b00111111)));
			add buf (Char.chr (0b10000000 lor (x          land 0b00111111)));
		)

	let code_of_surrogate_pair i j =
		let high10 = i - 0xD800 in
		let low10 = j - 0xDC00 in
		0x10000 + ((high10 lsl 10) lor low10)

	let utf8_of_surrogate_pair buf i j =
		utf8_of_code buf (code_of_surrogate_pair i j)

	let space = [%sedlex.regexp? Plus (Chars " \t\r\n")]
	let digit = [%sedlex.regexp? '0' .. '9']
	let nonzero = [%sedlex.regexp? '1' .. '9']
	let digits = [%sedlex.regexp? Plus digit]
	let frac = [%sedlex.regexp? '.', digits]
	let e = [%sedlex.regexp? (Chars "eE"),(Opt (Chars "+-"))]
	let exp = [%sedlex.regexp? e, digits]
	let positive_int = [%sedlex.regexp? digit | (nonzero, digits)]
	let float = [%sedlex.regexp? (Opt '-'), positive_int, (frac | exp | (frac, exp))]
	let hex = [%sedlex.regexp? '0'..'9' | 'a'..'f' | 'A'..'F' ]

	let rec read_json lexbuf =
		match%sedlex lexbuf with
		| "true" ->
			JBool true
		| "false" ->
			JBool false
		| "null" ->
			JNull
		| '"' ->
			JString (finish_string (Buffer.create 0) lexbuf)
		| positive_int ->
			make_positive_int lexbuf
		| '-', positive_int ->
			make_negative_int lexbuf
		| float ->
			JFloat (float_of_string (lexeme lexbuf))
		| '{' ->
			let acc = ref [] in
			begin try
				skip_space lexbuf;
				read_object_end lexbuf;
				let field_name = read_string lexbuf in
				skip_space lexbuf;
				read_colon lexbuf;
				skip_space lexbuf;
				acc := (field_name, read_json lexbuf) :: !acc;
				while true do
					skip_space lexbuf;
					read_object_sep lexbuf;
					skip_space lexbuf;
					let field_name = read_string lexbuf in
					skip_space lexbuf;
					read_colon lexbuf;
					skip_space lexbuf;
					acc := (field_name, read_json lexbuf) :: !acc;
				done;
				assert false
			with Exit ->
				JObject (List.rev !acc)
			end
		| '[' ->
			let acc = ref [] in
			begin try
				skip_space lexbuf;
				read_array_end lexbuf;
				acc := read_json lexbuf :: !acc;
				while true do
					skip_space lexbuf;
					read_array_sep lexbuf;
					skip_space lexbuf;
					acc := read_json lexbuf :: !acc;
				done;
				assert false
			with Exit ->
				JArray (List.rev !acc)
			end
		| space ->
			read_json lexbuf
		| eof ->
			json_error "Unexpected end of input"
		| _ ->
			json_error "Invalid token"

	and finish_string buf lexbuf =
		match%sedlex lexbuf with
		| '"' -> Buffer.contents buf
		| '\\' ->
			finish_escaped_char buf lexbuf;
			finish_string buf lexbuf
		| Plus (Compl ('"' | '\\')) ->
			Buffer.add_string buf (Sedlexing.Utf8.lexeme lexbuf);
			finish_string buf lexbuf
		| eof -> json_error "Unexpected end of input"
		| _ -> assert false

	and finish_escaped_char buf lexbuf =
		match%sedlex lexbuf with
		| '"' | '\\' | '/' ->
			Buffer.add_char buf (Uchar.to_char (Sedlexing.lexeme_char lexbuf 0))
		| 'b' ->
			Buffer.add_char buf '\b'
		| 'f' ->
			Buffer.add_char buf '\012'
		| 'n' ->
			Buffer.add_char buf '\n'
		| 'r' ->
			Buffer.add_char buf '\r'
		| 't' ->
			Buffer.add_char buf '\t'
		| 'u', hex, hex, hex, hex ->
			let a,b,c,d =
				match Sedlexing.lexeme lexbuf with
				| [|_; a; b; c; d|] -> Uchar.to_int a, Uchar.to_int b, Uchar.to_int c, Uchar.to_int d
				| _ -> assert false
			in
			let x =
				(hex a lsl 12) lor (hex b lsl 8) lor (hex c lsl 4) lor hex d
			in
			if x >= 0xD800 && x <= 0xDBFF then
				finish_surrogate_pair buf x lexbuf
			else
				utf8_of_code buf x
		| _ ->
			json_error "Invalid escape sequence"

	and finish_surrogate_pair buf x lexbuf =
		match%sedlex lexbuf with
		| "\\u", hex, hex, hex, hex ->
			let a,b,c,d =
				match Sedlexing.lexeme lexbuf with
				| [|_;_ ; a; b; c; d|] -> Uchar.to_int a, Uchar.to_int b, Uchar.to_int c, Uchar.to_int d
				| _ -> assert false
			in
			let y =
				(hex a lsl 12) lor (hex b lsl 8) lor (hex c lsl 4) lor hex d
			in
			if y >= 0xDC00 && y <= 0xDFFF then
				utf8_of_surrogate_pair buf x y
			else
				json_error "Invalid low surrogate for code point beyond U+FFFF"
		| _ ->
			json_error "Missing escape sequence representing low surrogate for code point beyond U+FFFF"

	and skip_space lexbuf =
		match%sedlex lexbuf with
		| space | "" -> ()
		| _ -> assert false

	and read_string lexbuf =
		match%sedlex lexbuf with
		| '"' -> finish_string (Buffer.create 0) lexbuf
		| _ -> json_error "Expected string"

	and read_array_end lexbuf =
		match%sedlex lexbuf with
		| ']' -> raise Exit
		| "" -> ()
		| _ -> assert false

	and read_array_sep lexbuf =
		match%sedlex lexbuf with
		| ',' -> ()
		| ']' -> raise Exit
		| _ -> json_error "Expected ',' or ']'"

	and read_object_end lexbuf =
		match%sedlex lexbuf with
		| '}' -> raise Exit
		| "" -> ()
		| _ -> assert false

	and read_object_sep lexbuf =
		match%sedlex lexbuf with
		| ',' -> ()
		| '}' -> raise Exit
		| _ -> json_error "Expected ',' or '}'"

	and read_colon lexbuf =
		match%sedlex lexbuf with
		| ':' -> ()
		| _ -> json_error "Expected ':'"
end
