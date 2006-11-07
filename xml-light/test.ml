(*
 * Xml Light, an small Xml parser/printer with DTD support.
 * Copyright (C) 2003 Nicolas Cannasse (ncannasse@motion-twin.com)
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Lesser General Public
 * License as published by the Free Software Foundation; either
 * version 2.1 of the License, or (at your option) any later version.
 *
 * This library has the special exception on linking described in file
 * README.
 *
 * This library is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 * Lesser General Public License for more details.
 *
 * You should have received a copy of the GNU Lesser General Public
 * License along with this library; if not, write to the Free Software
 * Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston,
 * MA 02110-1301 USA
 *)

open Xml
open Dtd

let parse data =
	match data.[0] with
	| '#' -> Xml.parse_file (String.sub data 1 ((String.length data)-2))
	| _ -> Xml.parse_string data

;;
let buf = ref "" in
print_endline "Please enter some XML data followed (press return twice to parse) :";
try
	while true do
		match read_line() with
		| "" when !buf <> "" ->
			let data = !buf in
			buf := "";
			(try
				let x = parse data in
				print_endline "Parsing...";
				print_endline (Xml.to_string_fmt x);
			with
				| Xml.Error msg as e ->
					Printf.printf "Xml error : %s\n" (Xml.error msg)
				| Dtd.Parse_error msg as e ->
					Printf.printf "Dtd parse error : %s\n" (Dtd.parse_error msg)
				| Dtd.Check_error msg as e ->
					Printf.printf "Dtd check error : %s\n" (Dtd.check_error msg)
				| Dtd.Prove_error msg as e ->
					Printf.printf "Dtd prove error : %s\n" (Dtd.prove_error msg))
		| s -> 
			buf := !buf ^ s ^ "\n"
	done
with
	End_of_file -> print_endline "Exit."