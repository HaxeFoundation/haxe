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

let report kind msg p =
	let error_printer file line = sprintf "%s:%d:" file line in
	let epos = Lexer.get_error_pos error_printer p in
	prerr_endline (sprintf "%s : %s %s" epos kind msg);
	exit 1

let compile files =
	List.iter (fun f ->
		let file = (try Plugin.find_file f with Not_found -> failwith ("File not found " ^ f)) in
		let ch = open_in file in
		let ast = Parser.parse (Lexing.from_channel ch) file in
		()
	) files

;;
try	
	let usage = "Haxe Compiler Alpha - (c)2005 Motion-Twin\n Usage : haxe.exe [options] <files...>\n Options :" in
	let base_path = normalize_path (try Extc.executable_path() with _ -> ".") in
	let files = ref [] in
	let time = Sys.time() in
	Plugin.class_path := [base_path;"";"/"];
	let args_spec = [
		("-cp",Arg.String (fun path -> Plugin.class_path := normalize_path path :: !Plugin.class_path),"<path> : add a directory to find source files");
		("-v",Arg.Unit (fun () -> Plugin.verbose := true),": turn on verbose mode");
	] @ !Plugin.options in
	Arg.parse args_spec (fun file -> files := file :: !files) usage;
	if !files = [] then begin
		Arg.usage args_spec usage
	end else begin
		if !Plugin.verbose then print_endline ("Classpath : " ^ (String.concat ";" !Plugin.class_path));
		compile (List.rev !files);
		if !Plugin.verbose then print_endline ("Time spent : " ^ string_of_float (Sys.time() -. time));
	end;
with
	| Lexer.Error (m,p) -> report "syntax error" (Lexer.error_msg m) p
	| Parser.Error (m,p) -> report "parse error" (Parser.error_msg m) p
	| Failure msg ->
		prerr_endline msg;
		exit 1;
