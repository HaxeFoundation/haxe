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

let compile ctx f =
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
	ignore(Typer.load ctx cpath Ast.null_pos)

;;
try	
	let usage = "Haxe Compiler Alpha - (c)2005 Motion-Twin\n Usage : haxe.exe [options] <class names...>\n Options :" in
	let base_path = normalize_path (try Extc.executable_path() with _ -> "./") in
	let classes = ref ["Std"] in
	let swf_out = ref None in
	let neko_out = ref None in
	let swf_version = ref 8 in
	let time = Sys.time() in
	Plugin.class_path := [base_path ^ "std/";"";"/"];
	let check_targets() =
		if !swf_out <> None || !neko_out <> None then raise (Arg.Bad "Multiple targets");
	in
	let args_spec = [
		("-cp",Arg.String (fun path ->
			Plugin.class_path := normalize_path path :: !Plugin.class_path
		),"<path> : add a directory to find source files");
		("-swf",Arg.String (fun file ->
			check_targets();
			swf_out := Some file
		),"<file> : compile code to SWF file");
		("-neko",Arg.String (fun file ->
			check_targets();
			neko_out := Some file
		),"<file> : compile code to Neko Binary");
		("-D",Arg.String (fun def ->
			Hashtbl.add Parser.defines def ();
		),"<var> : define the macro variable");
		("-fplayer",Arg.Int (fun v ->
			swf_version := v;
		),"<version> : flash player version (8 by default)");
		("-v",Arg.Unit (fun () -> Plugin.verbose := true),": turn on verbose mode");
	] @ !Plugin.options in
	Arg.parse args_spec (fun cl -> classes := cl :: !classes) usage;
	(match !swf_out with
	| None -> ()
	| Some _ ->
		Hashtbl.add Parser.defines "flash" ();
		Plugin.class_path := (base_path ^ "flash/") :: !Plugin.class_path;
		Hashtbl.add Parser.defines ("flash" ^ string_of_int !swf_version) ());
	(match !neko_out with
	| None -> ()
	| Some _ ->
		Hashtbl.add Parser.defines "neko" ();
		Plugin.class_path := (base_path ^ "neko/") :: !Plugin.class_path);
	if !classes = ["Std"] then begin
		Arg.usage args_spec usage
	end else begin
		if !Plugin.verbose then print_endline ("Classpath : " ^ (String.concat ";" !Plugin.class_path));
		let ctx = Typer.context warn in
		List.iter (compile ctx) (List.rev !classes);
		Typer.finalize ctx;
		let types = Typer.types ctx in
		(match !swf_out with
		| None -> ()
		| Some file ->
			if !Plugin.verbose then print_endline ("Generating swf : " ^ file);
			Genswf.generate file (!swf_version) types
		);
		(match !neko_out with
		| None -> ()
		| Some file ->
			if !Plugin.verbose then print_endline ("Generating neko : " ^ file);
			Genneko.generate file types);
		if !Plugin.verbose then print_endline ("Time spent : " ^ string_of_float (Sys.time() -. time));
	end;
with
	| Lexer.Error (m,p) -> report (Lexer.error_msg m) p
	| Parser.Error (m,p) -> report (Parser.error_msg m) p
	| Typer.Error (m,p) -> report (Typer.error_msg m) p
	| Failure msg ->
		prerr_endline msg;
		exit 1;
