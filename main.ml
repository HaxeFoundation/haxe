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
	| As3 of string

let prompt = ref false
let alt_format = ref false
let has_error = ref false
let auto_xml = ref false
let display = ref false

let executable_path() =
	Extc.executable_path()

let get_full_path file =
	Extc.get_full_path file

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
				sprintf "%s:%d:" file line
		in
		let epos = Lexer.get_error_pos error_printer p in
		let msg = String.concat ("\n" ^ epos ^ " : ") (ExtString.String.nsplit msg "\n") in
		prerr_endline (sprintf "%s : %s" epos msg)
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

let htmlescape s =
	let s = String.concat "&lt;" (ExtString.String.nsplit s "<") in
	let s = String.concat "&gt;" (ExtString.String.nsplit s ">") in
	s

let report_list l =
	prerr_endline "<list>";
	List.iter (fun (n,t,d) ->
		prerr_endline (Printf.sprintf "<i n=\"%s\"><t>%s</t><d>%s</d></i>" n (htmlescape t) (htmlescape d));
	) (List.sort (fun (a,_,_) (b,_,_) -> compare a b) l);
	prerr_endline "</list>"

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
		| ["hx";path] -> ExtString.String.nsplit path "/"
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

let read_type_path p cp =
	let classes = ref [] in
	let packages = ref [] in
	let p = (match p with "flash" :: l when Plugin.defined "flash9" -> "flash9" :: l | _ -> p) in
	List.iter (fun path ->
		let dir = path ^ String.concat "/" p in
		let r = (try Sys.readdir dir with _ -> [||]) in
		Array.iter (fun f ->
			if (Unix.stat (dir ^ "/" ^ f)).Unix.st_kind = Unix.S_DIR then begin
				if f.[0] > 'a' && f.[0] < 'z' then packages := f :: !packages
			end else if file_extension f = "hx" then begin
				let c = Filename.chop_extension f in
				if String.length c < 2 || String.sub c (String.length c - 2) 2 <> "__" then classes := c :: !classes;
			end;
		) r;
	) cp;
	List.sort compare (!packages), List.sort compare (!classes)

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
	let version = 116 in
	let version_str = Printf.sprintf "%d.%.2d" (version / 100) (version mod 100) in
	let usage = "Haxe Compiler " ^ version_str ^ " - (c)2005-2007 Motion-Twin\n Usage : haxe.exe [options] <class names...>\n Options :" in
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
	let libs = ref [] in
	let gen_hx = ref false in
	let no_output = ref false in
	Plugin.defines := base_defines;
	Plugin.define ("haxe_" ^ string_of_int version);
	Typer.check_override := false;
	Typer.forbidden_packages := ["js"; "neko"; "flash"];
	Parser.display_error := parse_error;
	Parser.use_doc := false;
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
			if Sys.os_type = "Unix" then
				Plugin.class_path := ["/usr/lib/haxe/std/";"/usr/local/lib/haxe/std/";"";"/"]
			else
				let base_path = normalize_path (try executable_path() with _ -> "./") in
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
		("-as3",Arg.String (fun dir ->
			check_targets();
			swf_version := 9;
			Plugin.define "as3gen";
			Typer.forbidden_packages := ["js"; "neko"];
			target := As3 dir;
		),"<directory> : generate AS3 code into target directory");
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
		("-x", Arg.String (fun file ->
			check_targets();
			Typer.forbidden_packages := ["js"; "flash"];
			let neko_file = file ^ ".n" in
			target := Neko neko_file;
			if !main_class = None then begin
				let cpath = make_path file in
				main_class := Some cpath;
				classes := cpath :: !classes
			end;
			cmds := ("neko " ^ neko_file) :: !cmds;
		),"<file> : shortcut for compiling and executing a neko file");
		("-xml",Arg.String (fun file ->
			Parser.use_doc := true;
			xml_out := Some file
		),"<file> : generate XML types description");
		("-main",Arg.String (fun cl ->
			if !main_class <> None then raise (Arg.Bad "Multiple -main");
			let cpath = make_path cl in
			main_class := Some cpath;
			classes := cpath :: !classes
		),"<class> : select startup class");
		("-lib",Arg.String (fun l -> libs := l :: !libs),"<library[:version]> : use an haxelib library");
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
			| [file] ->
				let file = (try Plugin.find_file file with Not_found -> file) in
				let data = Std.input_file ~bin:true file in
				Hashtbl.replace hres file data
			| _ ->
				raise (Arg.Bad "Invalid Resource format : should be file@name")
		),"<file@name> : add a named resource file");
		("-exclude",Arg.String (fun file ->
			let file = (try Plugin.find_file file with Not_found -> file) in
			let ch = open_in file in
			let lines = Std.input_list ch in
			close_in ch;
			excludes := (List.map (fun l ->
				let len = String.length l in
				let l = (if len > 0 && l.[len-1] = '\r' then String.sub l 0 (len - 1) else l) in
				match List.rev (ExtString.String.nsplit l ".") with
				| [] -> ([],"")
				| x :: l -> (List.rev l,x)
			) lines) @ !excludes;
		),"<filename> : don't generate code for classes listed in this file");
		("-v",Arg.Unit (fun () -> Plugin.verbose := true),": turn on verbose mode");
		("-debug", define "debug", ": add debug informations to the compiled code");
		("-prompt", Arg.Unit (fun() -> prompt := true),": prompt on error");
		("-cmd", Arg.String (fun cmd ->
			cmds := cmd :: !cmds
		),": run the specified command after successful compilation");
		("--flash-strict", define "flash_strict", ": more type strict flash API");
		("--override", Arg.Unit (fun() ->
			Typer.check_override := true
		),": ensure that overriden methods are declared with 'override'");
		("--no-traces", define "no_traces", ": don't compile trace calls in the program");
		("--flash-use-stage", define "flash_use_stage", ": place objects found on the stage of the SWF lib");
		("--neko-source", define "neko_source", ": keep generated neko source");
		("--gen-hx-classes", Arg.String (fun file ->
			gen_hx := true;
			Genas3.genhx file
		),"<file> : generate hx headers from SWF9 file");
		("--next", Arg.Unit (fun() -> assert false), ": separate several haxe compilations");
		("--altfmt", Arg.Unit (fun() -> alt_format := true),": use alternative error output format");
		("--auto-xml", Arg.Unit (fun() -> auto_xml := true),": automatically create an XML for each target");
		("--display", Arg.String (fun file_pos ->
			let file, pos = try ExtString.String.split file_pos "@" with _ -> failwith ("Invalid format : " ^ file_pos) in
			let pos = try int_of_string pos with _ -> failwith ("Invalid format : "  ^ pos) in
			display := true;
			no_output := true;
			Parser.resume_display := {
				Ast.pfile = (!Plugin.get_full_path) file;
				Ast.pmin = pos;
				Ast.pmax = pos;
			};
		),": display code tips");
		("--no-output", Arg.Unit (fun() -> no_output := true),": compiles but does not generate any file");
		("--times", Arg.Unit (fun() -> Plugin.times := true),": mesure compilation times");
	] in
	let current = ref 0 in
	let args = Array.of_list ("" :: params) in
	let rec args_callback cl =
		match List.rev (ExtString.String.nsplit cl ".") with
		| x :: _ when String.lowercase x = "hxml" ->
			let ch = (try open_in cl with _ -> failwith ("File not found " ^ cl)) in
			let lines = Std.input_list ch in
			let hxml_args = List.concat (List.map (fun l ->				
				let l = ExtString.String.strip l in
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
	(match !libs with
	| [] -> ()
	| l ->
		libs := [];
		let cmd = "haxelib path " ^ String.concat " " l in
		let p = Unix.open_process_in cmd in
		let lines = Std.input_list p in
		let ret = Unix.close_process_in p in
		let lines = List.fold_left (fun acc l ->
			let p = String.length l - 1 in
			let l = (if l.[p] = '\r' then String.sub l 0 p else l) in
			if p > 3 && String.sub l 0 3 = "-L " then begin
				libs := String.sub l 3 (String.length l - 3) :: !libs;
				acc
			end else
				l :: acc
		) [] lines in
		if ret <> Unix.WEXITED 0 then failwith (String.concat "\n" lines);
		Plugin.class_path := lines @ !Plugin.class_path;
	);
	(match !target with
	| No ->
		()
	| Swf file | As3 file ->
		(* check file extension. In case of wrong commandline, we don't want
		   to accidentaly delete a source file. *)
		if not !no_output && file_extension file = "swf" then delete_file file;
		Plugin.define "flash";
		Plugin.define ("flash"  ^ string_of_int !swf_version);
	| Neko file ->
		if not !no_output && file_extension file = "n" then delete_file file;
		Plugin.define "neko";
	| Js file ->
		if not !no_output && file_extension file = "js" then delete_file file;
		Plugin.define "js";
	);
	if !classes = [([],"Std")] then begin
		if !cmds = [] && not !gen_hx then Arg.usage args_spec usage;
	end else begin
		if !Plugin.verbose then print_endline ("Classpath : " ^ (String.concat ";" !Plugin.class_path));
		let t = Plugin.timer "typing" in
		let ctx = Typer.context type_error warn in
		List.iter (fun cpath -> ignore(Typer.load ctx cpath Ast.null_pos)) (List.rev !classes);
		Typer.finalize ctx;
		t();
		if !has_error then do_exit();
		if !display then begin
			xml_out := None;
			auto_xml := false;
		end;
		if !no_output then target := No;
		let do_auto_xml file = if !auto_xml then xml_out := Some (file ^ ".xml") in
		let types = Typer.types ctx (!main_class) (!excludes) in
		(match !target with
		| No -> ()
		| Swf file ->
			do_auto_xml file;
			if !Plugin.verbose then print_endline ("Generating swf : " ^ file);
			Genswf.generate file (!swf_version) (!swf_header) (!swf_in) types hres
		| Neko file ->
			do_auto_xml file;
			if !Plugin.verbose then print_endline ("Generating neko : " ^ file);
			Genneko.generate file types hres !libs
		| Js file ->
			do_auto_xml file;
			if !Plugin.verbose then print_endline ("Generating js : " ^ file);
			Genjs.generate file types hres
		| As3 dir ->
			if !Plugin.verbose then print_endline ("Generating AS3 in : " ^ dir);
			Genas3.generate dir types
		);
		(match !xml_out with
		| None -> ()
		| Some file ->
			if !Plugin.verbose then print_endline ("Generating xml : " ^ file);
			Genxml.generate file ctx types);
	end;
	if not !no_output then List.iter (fun cmd ->
		let t = Plugin.timer "command" in
		let len = String.length cmd in
		if len > 3 && String.sub cmd 0 3 = "cd " then
			Sys.chdir (String.sub cmd 3 (len - 3))
		else
			if Sys.command cmd <> 0 then failwith "Command failed";
		t();
	) (List.rev !cmds)
with
	| Lexer.Error (m,p) -> report (Lexer.error_msg m) p
	| Parser.Error (m,p) -> report (Parser.error_msg m) p
	| Typer.Error (m,p) -> report (Typer.error_msg m) p
	| Failure msg | Arg.Bad msg -> report ("Error : " ^ msg) Ast.null_pos
	| Arg.Help msg -> print_string msg
	| Hxml_found -> ()
	| Typer.Display t ->
		let ctx = Type.print_context() in
		(match Type.follow t with
		| Type.TAnon a ->
			report_list (PMap.fold (fun f acc ->
				if not f.Type.cf_public then
					acc
				else
					(f.Type.cf_name,Type.s_type ctx f.Type.cf_type,match f.Type.cf_doc with None -> "" | Some d -> d) :: acc
			) a.Type.a_fields []);
		| _ ->
			prerr_endline "<type>";
			prerr_endline (htmlescape (Type.s_type ctx t));
			prerr_endline "</type>");
		exit 0;
	| Parser.TypePath p ->
		let packs, classes = read_type_path p (!Plugin.class_path) in
		if packs = [] && classes = [] then report ("No classes found in " ^ String.concat "." p) Ast.null_pos;
		report_list (List.map (fun f -> f,"","") (packs @ classes));
		exit 0;
	| e -> report (Printexc.to_string e) Ast.null_pos

;;
let all = Plugin.timer "other" in
Plugin.times := false;
Plugin.get_full_path := get_full_path;
process_params [] (List.tl (Array.to_list Sys.argv));
all();
if !Plugin.times then begin
	let tot = ref 0. in
	Hashtbl.iter (fun _ t -> tot := !tot +. t.Plugin.total) Plugin.htimers;
	Printf.eprintf "Total time : %.3fs\n" !tot;
	Printf.eprintf "------------------------------------\n";
	Hashtbl.iter (fun _ t ->
		Printf.eprintf "  %s : %.3fs, %.0f%%\n" t.Plugin.name t.Plugin.total (t.Plugin.total *. 100. /. !tot);
	) Plugin.htimers;
end;