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

type target = 
	| No 
	| Js of string
	| Swf of string
	| Neko of string

let prompt = ref false
let alt_format = ref false
let has_error = ref false

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
		let error_printer file line = 
			if !alt_format then
				sprintf "%s(%d):" file line
			else
				sprintf "%s:%d: :" file line
		in
		let epos = Lexer.get_error_pos error_printer p in
		prerr_endline (sprintf "%s %s" epos msg)
	end

let do_exit() =
	if !prompt then begin
		print_endline "Press enter to exit...";
		ignore(read_line());
	end;
	exit 1

let report msg p =
	warn msg p;
	do_exit()

let type_error e p =
	warn (Typer.error_msg e) p;
	has_error := true

let parse_error e p =
	Lexer.save_lines();
	warn (Parser.error_msg e) p;
	has_error := true

let file_extension f = 
	let cl = ExtString.String.nsplit f "." in
	match List.rev cl with
	| [] -> ""
	| x :: _ -> x

let make_path f =
	let cl = ExtString.String.nsplit f "." in
	let cl = (match List.rev cl with
		| "hx" :: l -> List.rev l
		| _ -> cl
	) in
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


let delete_file f = try Sys.remove f with _ -> ()

let base_defines = !Plugin.defines

exception Hxml_found

let rec process_params acc = function
	| [] ->	
		init (List.rev acc)
	| "--next" :: l ->
		init (List.rev acc);
		process_params [] l
	| x :: l ->
		process_params (x :: acc) l

and init params =
try	
	let usage = "Haxe Compiler 1.03 - (c)2005-2006 Motion-Twin\n Usage : haxe.exe [options] <class names...>\n Options :" in
	let base_path = normalize_path (try Extc.executable_path() with _ -> "./") in
	let classes = ref [([],"Std")] in
	let target = ref No in
	let swf_in = ref None in
	let xml_out = ref None in
	let main_class = ref None in
	let swf_version = ref 8 in
	let swf_header = ref None in
	let hres = Hashtbl.create 0 in
	let cmds = ref [] in
	let excludes = ref [] in
	Plugin.defines := base_defines;
	Typer.forbidden_packages := ["js"; "neko"; "flash"];
	Parser.display_error := parse_error;
	(try
		let p = Sys.getenv "HAXE_LIBRARY_PATH" in
		let rec loop = function
			| drive :: path :: l ->	
				if String.length drive = 1 && ((drive.[0] >= 'a' && drive.[0] <= 'z') || (drive.[0] >= 'A' && drive.[0] <= 'Z')) then
					(drive ^ ":" ^ path) :: loop l
				else
					drive :: loop (path :: l)
			| l ->
				l
		in
		Plugin.class_path := List.map normalize_path (loop (ExtString.String.nsplit p ":"))
	with
		Not_found -> 
			Plugin.class_path := [base_path ^ "std/";"";"/"]);
	let check_targets() =
		if !target <> No then failwith "Multiple targets";
	in
	let define f = Arg.Unit (fun () -> Plugin.define f) in
	let args_spec = [
		("-cp",Arg.String (fun path ->
			Plugin.class_path := normalize_path path :: !Plugin.class_path
		),"<path> : add a directory to find source files");
		("-js",Arg.String (fun file ->
			check_targets();
			Typer.forbidden_packages := ["neko"; "flash"];
			target := Js file
		),"<file> : compile code to JavaScript file");
		("-swf",Arg.String (fun file ->
			check_targets();
			Typer.forbidden_packages := ["js"; "neko"];
			target := Swf file
		),"<file> : compile code to Flash SWF file");
		("-swf-version",Arg.Int (fun v -> 
			swf_version := v;
		),"<version> : change the SWF version (6,7,8,9)");
		("-swf-header",Arg.String (fun h ->
			try
				swf_header := Some (match ExtString.String.nsplit h ":" with
				| [width; height; fps] ->
					(int_of_string width,int_of_string height,float_of_string fps,0xFFFFFF)
				| [width; height; fps; color] ->
					(int_of_string width, int_of_string height, float_of_string fps, int_of_string ("0x" ^ color))
				| _ -> raise Exit)
			with
				_ -> raise (Arg.Bad "Invalid SWF header format")
		),"<header> : define SWF header (width:height:fps:color)");
		("-swf-lib",Arg.String (fun file ->
			swf_in := Some file
		),"<file> : add the SWF library to the compiled SWF");
		("-neko",Arg.String (fun file ->
			check_targets();
			Typer.forbidden_packages := ["js"; "flash"];
			target := Neko file
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
			Plugin.define def;
		),"<var> : define a conditional compilation flag");
		("-resource",Arg.String (fun res ->
			match ExtString.String.nsplit res "@" with
			| [file; name] ->
				let file = (try Plugin.find_file file with Not_found -> file) in
				let data = Std.input_file ~bin:true file in
				if Hashtbl.mem hres name then failwith ("Duplicate resource name " ^ name);
				Hashtbl.add hres name data
			| _ ->
				raise (Arg.Bad "Invalid Resource format : should be file@name")
		),"<file@name> : add a named resource file");
		("-exclude",Arg.String (fun file ->
			let file = (try Plugin.find_file file with Not_found -> file) in
			let ch = open_in file in
			let lines = Std.input_list ch in
			close_in ch;
			excludes := (List.map (fun l -> 
				match List.rev (ExtString.String.nsplit l ".") with
				| [] -> ([],"")
				| x :: l -> (List.rev l,x)
			) lines) @ !excludes;
		),"<filename> : don't generate code for classes listed in this file");
		("-v",Arg.Unit (fun () -> Plugin.verbose := true),": turn on verbose mode");
		("-prompt", Arg.Unit (fun() -> prompt := true),": prompt on error");
		("-cmd", Arg.String (fun cmd ->
			cmds := cmd :: !cmds
		),": run the specified command after successful compilation");
		("--flash-strict", define "flash_strict", ": more type strict flash API");
		("--no-traces", define "no_traces", ": don't compile trace calls in the program");
		("--flash-use-stage", define "flash_use_stage", ": place objects found on the stage of the SWF lib");
		("--next", Arg.Unit (fun() -> assert false), ": separate several haxe compilations");
		("--altfmt", Arg.Unit (fun() -> alt_format := true),": use alternative error output format");
	] in
	let current = ref 0 in
	let args = Array.of_list ("" :: params) in
	let rec args_callback cl =
		match List.rev (ExtString.String.nsplit cl ".") with
		| x :: _ when String.lowercase x = "hxml" ->
			let ch = (try open_in cl with _ -> failwith ("File not found " ^ cl)) in
			let lines = Std.input_list ch in
			let hxml_args = List.concat (List.map (fun l -> 
				let len = String.length l in
				let l = (if len != 0 && l.[len - 1] = '\r' then String.sub l 0 (len-1) else l) in
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
			let p1 = Array.to_list (Array.sub args 1 (!current - 1)) in
			let p2 = Array.to_list (Array.sub args (!current + 1) (Array.length args - !current - 1)) in
			if !Plugin.verbose then print_endline ("Processing HXML : " ^ cl);
			process_params [] (p1 @ hxml_args @ p2);
			raise Hxml_found
		| _ ->
			classes := make_path cl :: !classes
	in
	Arg.parse_argv ~current args args_spec args_callback usage;
	(match !target with
	| No ->
		()
	| Swf file ->
		(* check file extension. In case of wrong commandline, we don't want
		   to accidentaly delete a source file. *)
		if file_extension file = "swf" then delete_file file;	
		Plugin.define "flash";
		Plugin.define ("flash"  ^ string_of_int !swf_version);
	| Neko file ->
		if file_extension file = "n" then delete_file file;
		Plugin.define "neko";
	| Js file ->
		if file_extension file = "js" then delete_file file;
		Plugin.define "js";
	);
	if !classes = [([],"Std")] then begin
		if !cmds = [] then Arg.usage args_spec usage;
	end else begin
		if !Plugin.verbose then print_endline ("Classpath : " ^ (String.concat ";" !Plugin.class_path));
		let ctx = Typer.context type_error warn in
		List.iter (fun cpath -> ignore(Typer.load ctx cpath Ast.null_pos)) (List.rev !classes);
		Typer.finalize ctx;
		if !has_error then do_exit();
		let types = Typer.types ctx (!main_class) (!excludes) in
		(match !target with
		| No -> ()
		| Swf file ->
			if !Plugin.verbose then print_endline ("Generating swf : " ^ file);
			Genswf.generate file (!swf_version) (!swf_header) (!swf_in) types hres
		| Neko file ->
			if !Plugin.verbose then print_endline ("Generating neko : " ^ file);
			Genneko.generate file types hres
		| Js file ->
			if !Plugin.verbose then print_endline ("Generating js : " ^ file);
			Genjs.generate file types hres
		);
		(match !xml_out with
		| None -> ()
		| Some file ->
			if !Plugin.verbose then print_endline ("Generating xml : " ^ file);
			Genxml.generate file ctx types);
	end;
	List.iter (fun cmd ->
		let len = String.length cmd in
		if len > 3 && String.sub cmd 0 3 = "cd " then
			Sys.chdir (String.sub cmd 3 (len - 3))
		else
			if Sys.command cmd <> 0 then failwith "Command failed"
	) (List.rev !cmds)
with	
	| Lexer.Error (m,p) -> report (Lexer.error_msg m) p
	| Parser.Error (m,p) -> report (Parser.error_msg m) p
	| Typer.Error (m,p) -> report (Typer.error_msg m) p
	| Failure msg | Arg.Bad msg -> report ("Error : " ^ msg) Ast.null_pos
	| Arg.Help msg -> print_string msg
	| Hxml_found -> ()
	| e -> report (Printexc.to_string e) Ast.null_pos

;;
let time = Sys.time() in
process_params [] (List.tl (Array.to_list Sys.argv));
if !Plugin.verbose then print_endline ("Time spent : " ^ string_of_float (Sys.time() -. time));
