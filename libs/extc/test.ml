(*
 *  Extc : C common OCaml bindings
 *  Copyright (c)2004 Nicolas Cannasse
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
 *  Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301, USA
 *)

if Array.length Sys.argv > 1 then begin
	print_string Sys.argv.(1);
	flush stdout;
	prerr_string "ERROR";
	flush stderr;
	let input = Std.input_all stdin in
	print_string input;
	exit 66;
end;
 
prerr_endline "Start";

prerr_endline (Extc.executable_path());
let contents = Std.input_file "test.ml" in
let s = Extc.unzip (Extc.zip contents) in
if s <> contents then failwith "zip + unzip failed";

let p = Process.run "test" [|"Hello"|] in
let tmp = String.create 100 in
let out = String.sub tmp 0 (Process.read_stdout p tmp 0 100) in
if out <> "Hello" then failwith ("OUT=" ^ out ^ "#");
let err = String.sub tmp 0 (Process.read_stderr p tmp 0 100) in
if err <> "ERROR" then failwith ("ERR= " ^ err ^ "#");
ignore(Process.write_stdin p "INPUT" 0 5);
Process.close_stdin p;
let out = String.sub tmp 0 (Process.read_stdout p tmp 0 100) in
if out <> "INPUT" then failwith ("IN-OUT=" ^ out ^ "#");
let code = Process.exit p in
if code <> 66 then failwith ("EXIT=" ^ string_of_int code);
Process.close p;

prerr_endline "End";