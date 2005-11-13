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
open Printf

let normalize_path p =
	let l = String.length p in
	if l = 0 then
		"./"
	else match p.[l-1] with 
		| '\\' | '/' -> p
		| _ -> p ^ "/"

let warn msg p =
	let error_printer file line = sprintf "%s:%d:" file line in
	let epos = Lexer.get_error_pos error_printer p in
	prerr_endline (sprintf "%s : %s" epos msg)

let report msg p =
	warn msg p;
	exit 1

let compile classes =
	let ctx = Typer.context warn in
	List.iter (fun f ->
		let cl = ExtString.String.nsplit f "." in
		let error() = failwith ("Invalid class name " ^ f) in
		let invalid_char x =
			for i = 1 to String.length x - 1 do
				match x.[i] with
				| 'A'..'Z' | 'a'..'z' | '0'..'9' | '_' -> ()
				| _ -> error()
			done;
			false
		in
		let rec loop = function
			| [] -> error()
			| [x] -> if String.length x = 0 || x.[0] < 'A' || x.[0] > 'Z' || invalid_char x then error() else [] , x
			| x :: l ->
				if String.length x = 0 || x.[0] < 'a' || x.[0] > 'z' || invalid_char x then error() else
					let path , name = loop l in
					x :: path , name
		in
		let cpath = loop cl in
		ignore(Typer.load ctx cpath Ast.null_pos);
	) classes;
	Typer.finalize ctx

;;
try	
	let usage = "Haxe Compiler Alpha - (c)2005 Motion-Twin\n Usage : haxe.exe [options] <class names...>\n Options :" in
	let base_path = normalize_path (try Extc.executable_path() with _ -> ".") in
	let classes = ref [] in
	let time = Sys.time() in
	Plugin.class_path := [base_path;"";"/"];
	let args_spec = [
		("-cp",Arg.String (fun path -> Plugin.class_path := normalize_path path :: !Plugin.class_path),"<path> : add a directory to find source files");
		("-v",Arg.Unit (fun () -> Plugin.verbose := true),": turn on verbose mode");
	] @ !Plugin.options in
	Arg.parse args_spec (fun cl -> classes := cl :: !classes) usage;
	if !classes = [] then begin
		Arg.usage args_spec usage
	end else begin
		if !Plugin.verbose then print_endline ("Classpath : " ^ (String.concat ";" !Plugin.class_path));
		compile (List.rev !classes);
		if !Plugin.verbose then print_endline ("Time spent : " ^ string_of_float (Sys.time() -. time));
	end;
with
	| Lexer.Error (m,p) -> report (Lexer.error_msg m) p
	| Parser.Error (m,p) -> report (Parser.error_msg m) p
	| Typer.Error (m,p) -> report (Typer.error_msg m) p
	| Failure msg ->
		prerr_endline msg;
		exit 1;
