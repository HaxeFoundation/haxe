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

let prompt = ref false

let normalize_path p =
	let l = String.length p in
	if l = 0 then
		"./"
	else match p.[l-1] with 
		| '\\' | '/' -> p
		| _ -> p ^ "/"

let warn msg p =
	if p = Ast.null_pos then
		prerr_endline msg
	else begin
		let error_printer file line = sprintf "%s:%d:" file line in
		let epos = Lexer.get_error_pos error_printer p in
		prerr_endline (sprintf "%s : %s" epos msg)
	end

let report msg p =
	warn msg p;
	if !prompt then begin
		print_endline "Press enter to exit...";
		ignore(read_line());
	end;
	exit 1

let make_path f =
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
	loop cl

;;
try	
	let usage = "Haxe Compiler Alpha - (c)2005 Motion-Twin\n Usage : haxe.exe [options] <class names...>\n Options :" in
	let base_path = normalize_path (try Extc.executable_path() with _ -> "./") in
	let classes = ref [([],"Std")] in
	let swf_out = ref None in
	let neko_out = ref None in
	let xml_out = ref None in
	let main_class = ref None in
	let swf_version = ref 8 in
	let time = Sys.time() in
	Plugin.class_path := [base_path;base_path ^ "std/";"";"/"];
	let check_targets() =
		if !swf_out <> None || !neko_out <> None then raise (Arg.Bad "Multiple targets");
	in
	let args_spec = [
		("-cp",Arg.String (fun path ->
			Plugin.class_path := normalize_path path :: !Plugin.class_path
		),"<path> : add a directory to find source files");
		("-swf",Arg.String (fun file ->
			check_targets();
			Typer.forbidden_packages := ["js"; "neko"];
			swf_out := Some file
		),"<file> : compile code to SWF file");
		("-neko",Arg.String (fun file ->
			check_targets();
			Typer.forbidden_packages := ["js"; "flash"];
			neko_out := Some file
		),"<file> : compile code to Neko Binary");
		("-xml",Arg.String (fun file ->
			xml_out := Some file
		),"<file> : generate XML types description");
		("-main",Arg.String (fun cl ->
			if !main_class <> None then raise (Arg.Bad "Multiple -main");
			let cpath = make_path cl in
			main_class := Some cpath;
			classes := cpath :: !classes
		),"<class> : select startup class");
		("-D",Arg.String (fun def ->
			Hashtbl.add Parser.defines def ();
		),"<var> : define the macro variable");
		("-fplayer",Arg.Int (fun v ->
			swf_version := v;
		),"<version> : flash player version (8 by default)");
		("-v",Arg.Unit (fun () -> Plugin.verbose := true),": turn on verbose mode");
		("-prompt", Arg.Unit (fun() -> prompt := true),": prompt on error");
	] @ !Plugin.options in
	let rec args_callback cl =
		match List.rev (ExtString.String.nsplit cl ".") with
		| x :: _ when String.lowercase x = "hxml" ->
			let ch = (try open_in cl with _ -> failwith ("File not found " ^ cl)) in
			let lines = Std.input_list ch in
			let args = List.concat (List.map (fun l -> 
				if l = "" || l.[0] = '#' then
					[]
				else if l.[0] = '-' then
					try
						let a, b = ExtString.String.split l " " in
						[a; b]
					with
						_ -> [l]
				else
					[l]
			) lines) in
			Arg.parse_argv ~current:(ref (-1)) (Array.of_list args) args_spec args_callback usage;
		| _ -> classes := make_path cl :: !classes
	in
	Arg.parse args_spec args_callback usage;
	(match !swf_out with
	| None -> ()
	| Some _ ->
		Hashtbl.add Parser.defines "flash" ();
		Hashtbl.add Parser.defines ("flash" ^ string_of_int !swf_version) ());
	(match !neko_out with
	| None -> ()
	| Some _ ->
		Hashtbl.add Parser.defines "neko" ();
	);
	if !classes = [([],"Std")] then begin
		Arg.usage args_spec usage
	end else begin
		if !Plugin.verbose then print_endline ("Classpath : " ^ (String.concat ";" !Plugin.class_path));
		let ctx = Typer.context warn in
		List.iter (fun cpath -> ignore(Typer.load ctx cpath Ast.null_pos)) (List.rev !classes);
		Typer.finalize ctx;
		let types = Typer.types ctx (!main_class) in
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
		(match !xml_out with
		| None -> ()
		| Some file ->
			if !Plugin.verbose then print_endline ("Generating xml : " ^ file);
			Genxml.generate file types);
		if !Plugin.verbose then print_endline ("Time spent : " ^ string_of_float (Sys.time() -. time));
	end;
with
	| Lexer.Error (m,p) -> report (Lexer.error_msg m) p
	| Parser.Error (m,p) -> report (Parser.error_msg m) p
	| Typer.Error (m,p) -> report (Typer.error_msg m) p
	| Failure msg -> report msg Ast.null_pos
	| e -> report (Printexc.to_string e) Ast.null_pos
