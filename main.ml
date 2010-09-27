(*
 *  Haxe Compiler
 *  Copyright (c)2005-2008 Nicolas Cannasse
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
open Genswf
open Common

let version = 206

let prompt = ref false
let measure_times = ref false
let start = get_time()

let executable_path() =
	Extc.executable_path()

let normalize_path p =
	let l = String.length p in
	if l = 0 then
		"./"
	else match p.[l-1] with
		| '\\' | '/' -> p
		| _ -> p ^ "/"

let format msg p =
	if p = Ast.null_pos then
		msg
	else begin
		let error_printer file line = sprintf "%s:%d:" file line in
		let epos = Lexer.get_error_pos error_printer p in
		let msg = String.concat ("\n" ^ epos ^ " : ") (ExtString.String.nsplit msg "\n") in
		sprintf "%s : %s" epos msg
	end

let message msg p =
	prerr_endline (format msg p)

let messages = ref []

let store_message msg p =
	messages := format msg p :: !messages

let do_exit() =
	List.iter prerr_endline (List.rev (!messages));
	if !prompt then begin
		print_endline "Press enter to exit...";
		ignore(read_line());
	end;
	exit 1

let report msg p =
	messages := format msg p :: !messages;
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

let rec read_type_path com p =
	let classes = ref [] in
	let packages = ref [] in
	let p = (match p with
		| x :: l ->
			(try
				match PMap.find x com.package_rules with
				| Directory d -> d :: l
				| Remap s -> s :: l
				| _ -> p
			with
				Not_found -> p)
		| _ -> p
	) in
	List.iter (fun path ->
		let dir = path ^ String.concat "/" p in
		let r = (try Sys.readdir dir with _ -> [||]) in
		Array.iter (fun f ->
			if (try (Unix.stat (dir ^ "/" ^ f)).Unix.st_kind = Unix.S_DIR with _ -> false) then begin
				if f.[0] >= 'a' && f.[0] <= 'z' then begin
					if p = ["."] then
						match read_type_path com [f] with
						| [] , [] -> ()
						| _ ->
							try
								match PMap.find f com.package_rules with
								| Forbidden -> ()
								| Remap f -> packages := f :: !packages
								| Directory _ -> raise Not_found
							with Not_found ->
								packages := f :: !packages
					else
						packages := f :: !packages
				end;
			end else if file_extension f = "hx" then begin
				let c = Filename.chop_extension f in
				if String.length c < 2 || String.sub c (String.length c - 2) 2 <> "__" then classes := c :: !classes;
			end;
		) r;
	) com.class_path;
	List.iter (fun (_,_,extract) ->
		Hashtbl.iter (fun (path,name) _ ->
			if path = p then classes := name :: !classes else
			let rec loop p1 p2 =
				match p1, p2 with
				| [], _ -> ()
				| x :: _, [] -> packages := x :: !packages
				| a :: p1, b :: p2 -> if a = b then loop p1 p2
			in
			loop path p
		) (extract());
	) com.swf_libs;
	let rec unique = function
		| [] -> []
		| x1 :: x2 :: l when x1 = x2 -> unique (x2 :: l)
		| x :: l -> x :: unique l
	in
	unique (List.sort compare (!packages)), unique (List.sort compare (!classes))

let delete_file f = try Sys.remove f with _ -> ()

let expand_env path =
	let r = Str.regexp "%\\([^%]+\\)%" in
	Str.global_substitute r (fun s -> try Sys.getenv (Str.matched_group 1 s) with Not_found -> "") path

let parse_hxml file =
	let ch = IO.input_channel (try open_in_bin file with _ -> failwith ("File not found " ^ file)) in
	let lines = Str.split (Str.regexp "[\r\n]+") (IO.read_all ch) in
	IO.close_in ch;
	List.concat (List.map (fun l ->
		let l = ExtString.String.strip l in
		let renv = Str.regexp "%\\([A-Za-z0-9_]+\\)%" in
		let l = Str.global_substitute renv (fun _ ->
			let e = Str.matched_group 1 l in
			try Sys.getenv e with Not_found -> "%" ^ e ^ "%"
		) l in
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
	) lines)

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
	let usage = Printf.sprintf
		"haXe Compiler %d.%.2d - (c)2005-2010 Motion-Twin\n Usage : haxe%s -main <class> [-swf9|-swf|-js|-neko|-php|-cpp|-as3] <output> [options]\n Options :"
		(version / 100) (version mod 100) (if Sys.os_type = "Win32" then ".exe" else "")
	in
	let classes = ref [([],"Std")] in
	let com = Common.create version in
try
	let xml_out = ref None in
	let swf_header = ref None in
	let cmds = ref [] in
	let excludes = ref [] in
	let libs = ref [] in
	let has_error = ref false in
	let gen_as3 = ref false in
	let no_output = ref false in
	let did_something = ref false in
	let pre_compilation = ref [] in
	let interp = ref false in
	Common.define com ("haxe_" ^ string_of_int version);
	com.warning <- message;
	com.error <- (fun msg p ->
		message msg p;
		has_error := true;
	);
	Parser.display_error := (fun e p ->
		Lexer.save_lines();
		com.error (Parser.error_msg e) p;
	);
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
		let parts = "" :: Str.split_delim (Str.regexp "[;:]") p in
		com.class_path <- List.map normalize_path (loop parts)
	with
		Not_found ->
			if Sys.os_type = "Unix" then
				com.class_path <- ["/usr/lib/haxe/std/";"/usr/local/lib/haxe/std/";"";"/"]
			else
				let base_path = normalize_path (try executable_path() with _ -> "./") in
				com.class_path <- [base_path ^ "std/";"";"/"]);
	com.std_path <- List.filter (fun p -> ExtString.String.ends_with p "std/" || ExtString.String.ends_with p "std\\") com.class_path;
	let set_platform pf file =
		if com.platform <> Cross then failwith "Multiple targets";
		Common.init_platform com pf;
		com.file <- file;
		Unix.putenv "__file__" file;
		Unix.putenv "__platform__" file;
		if (pf = Flash || pf = Flash9) && file_extension file = "swc" then Common.define com "swc";
	in
	let define f = Arg.Unit (fun () -> Common.define com f) in
	let basic_args_spec = [
		("-cp",Arg.String (fun path ->
			com.class_path <- normalize_path path :: com.class_path
		),"<path> : add a directory to find source files");
		("-js",Arg.String (set_platform Js),"<file> : compile code to JavaScript file");
		("-swf",Arg.String (set_platform Flash),"<file> : compile code to Flash SWF file");
		("-swf9",Arg.String (fun file ->
			set_platform Flash file;
			if com.flash_version < 9 then com.flash_version <- 9;
		),"<file> : compile code to Flash9 SWF file");
		("-as3",Arg.String (fun dir ->
			set_platform Flash dir;
			if com.flash_version < 9 then com.flash_version <- 9;
			gen_as3 := true;
			Common.define com "as3";
			Common.define com "no_inline";
		),"<directory> : generate AS3 code into target directory");
		("-neko",Arg.String (set_platform Neko),"<file> : compile code to Neko Binary");
		("-php",Arg.String (fun dir ->
			classes := (["php"],"Boot") :: !classes;
			set_platform Php dir;
		),"<directory> : generate PHP code into target directory");
		("-cpp",Arg.String (fun dir ->
			set_platform Cpp dir;
		),"<directory> : generate C++ code into target directory");
		("-xml",Arg.String (fun file ->
			Parser.use_doc := true;
			xml_out := Some file
		),"<file> : generate XML types description");
		("-main",Arg.String (fun cl ->
			if com.main_class <> None then raise (Arg.Bad "Multiple -main");
			let cpath = make_path cl in
			com.main_class <- Some cpath;
			classes := cpath :: !classes
		),"<class> : select startup class");
		("-lib",Arg.String (fun l ->
			libs := l :: !libs;
			Common.define com l;
		),"<library[:version]> : use a haxelib library");
		("-D",Arg.String (fun var ->
			(match var with
			| "use_rtti_doc" -> Parser.use_doc := true
			| "no_opt" -> com.foptimize <- false
			| _ -> ());
			Common.define com var
		),"<var> : define a conditional compilation flag");
		("-v",Arg.Unit (fun () ->
			com.verbose <- true
		),": turn on verbose mode");
		("-debug", Arg.Unit (fun() ->
			Common.define com "debug"; com.debug <- true
		), ": add debug informations to the compiled code");
	] in
	let adv_args_spec = [
		("-swf-version",Arg.Int (fun v ->
			com.flash_version <- v;
		),"<version> : change the SWF version (6 to 10)");
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
			let getSWF = Genswf.parse_swf com file in
			let extract = Genswf.extract_data getSWF in
			let build cl p = 
				match (try Some (Hashtbl.find (extract()) cl) with Not_found -> None) with
				| None -> None
				| Some c -> Some (Genswf.build_class com c file)
			in
			com.load_extern_type <- com.load_extern_type @ [build];
			com.swf_libs <- (file,getSWF,extract) :: com.swf_libs
		),"<file> : add the SWF library to the compiled SWF");
		("-x", Arg.String (fun file ->
			let neko_file = file ^ ".n" in
			set_platform Neko neko_file;
			if com.main_class = None then begin
				let cpath = make_path file in
				com.main_class <- Some cpath;
				classes := cpath :: !classes
			end;
			cmds := ("neko " ^ neko_file) :: !cmds;
		),"<file> : shortcut for compiling and executing a neko file");
		("-resource",Arg.String (fun res ->
			let file, name = (match ExtString.String.nsplit res "@" with
				| [file; name] -> file, name
				| [file] -> file, file
				| _ -> raise (Arg.Bad "Invalid Resource format : should be file@name")
			) in
			let file = (try Common.find_file com file with Not_found -> file) in
			let data = (try
				let s = Std.input_file ~bin:true file in
				if String.length s > 12000000 then raise Exit;
				s;
			with
				| Sys_error _ -> failwith ("Resource file not found : " ^ file)
				| _ -> failwith ("Resource '" ^ file ^ "' excess the maximum size of 12MB")
			) in
			if Hashtbl.mem com.resources name then failwith ("Duplicate resource name " ^ name);
			Hashtbl.add com.resources name data
		),"<file>[@name] : add a named resource file");
		("-exclude",Arg.String (fun file ->
			let file = (try Common.find_file com file with Not_found -> file) in
			let ch = open_in file in
			let lines = Std.input_list ch in
			close_in ch;
			excludes := (List.map (fun l ->
				let l = ExtString.String.strip l in
				if l = "" then ([],"") else Ast.parse_path l
			) lines) @ !excludes;
		),"<filename> : don't generate code for classes listed in this file");
		("-prompt", Arg.Unit (fun() -> prompt := true),": prompt on error");
		("-cmd", Arg.String (fun cmd ->
			cmds := expand_env cmd :: !cmds
		),": run the specified command after successful compilation");
		("--flash-strict", define "flash_strict", ": more type strict flash API");
		("--no-traces", define "no_traces", ": don't compile trace calls in the program");
		("--flash-use-stage", define "flash_use_stage", ": place objects found on the stage of the SWF lib");
		("--neko-source", define "neko_source", ": keep generated neko source");
		("--gen-hx-classes", Arg.String (fun file ->
			com.file <- file;
			Genas3.genhx com;
			did_something := true;
		),"<file> : generate hx headers from SWF9 file");
		("--next", Arg.Unit (fun() -> assert false), ": separate several haxe compilations");
		("--display", Arg.String (fun file_pos ->
			match file_pos with
			| "classes" ->
				pre_compilation := (fun() -> raise (Parser.TypePath (["."],None))) :: !pre_compilation;
			| "keywords" ->
				report_list (Hashtbl.fold (fun k _ acc -> (k,"","") :: acc) Lexer.keywords []);
				exit 0;
			| _ ->
				let file, pos = try ExtString.String.split file_pos "@" with _ -> failwith ("Invalid format : " ^ file_pos) in
				let pos = try int_of_string pos with _ -> failwith ("Invalid format : "  ^ pos) in
				Common.display := true;
				Common.define com "display";
				Parser.resume_display := {
					Ast.pfile = String.lowercase (Common.get_full_path file);
					Ast.pmin = pos;
					Ast.pmax = pos;
				};
		),": display code tips");
		("--no-output", Arg.Unit (fun() -> no_output := true),": compiles but does not generate any file");
		("--times", Arg.Unit (fun() -> measure_times := true),": measure compilation times");
		("--no-inline", define "no_inline", ": disable inlining");
		("--no-opt", Arg.Unit (fun() ->
			com.foptimize <- false;
			Common.define com "no_opt";
		), ": disable code optimizations");
		("--php-front",Arg.String (fun f ->
			if com.php_front <> None then raise (Arg.Bad "Multiple --php-front");
			com.php_front <- Some f;
		),"<filename> : select the name for the php front file");
		("--js-namespace",Arg.String (fun f ->
			if com.js_namespace <> None then raise (Arg.Bad "Multiple --js-namespace");
			com.js_namespace <- Some f;
			Common.define com "js_namespace";
		),"<namespace> : create a namespace where root types are defined");
		("--remap", Arg.String (fun s ->
			let pack, target = (try ExtString.String.split s ":" with _ -> raise (Arg.Bad "Invalid format")) in
			com.package_rules <- PMap.add pack (Remap target) com.package_rules;
		),"<package:target> : remap a package to another one");
		("--interp", Arg.Unit (fun() ->
			Common.define com "macro";
			set_platform Neko "";
			no_output := true;
			interp := true;
		),": interpret the program using internal macro system");
	] in
	let current = ref 0 in
	let args = Array.of_list ("" :: params) in
	let rec args_callback cl =
		match List.rev (ExtString.String.nsplit cl ".") with
		| x :: _ when String.lowercase x = "hxml" ->
			let hxml_args = parse_hxml cl in
			let p1 = Array.to_list (Array.sub args 1 (!current - 1)) in
			let p2 = Array.to_list (Array.sub args (!current + 1) (Array.length args - !current - 1)) in
			if com.verbose then print_endline ("Processing HXML : " ^ cl);
			process_params [] (p1 @ hxml_args @ p2);
			raise Hxml_found
		| _ ->
			classes := make_path cl :: !classes
	in
	Arg.parse_argv ~current args (basic_args_spec @ adv_args_spec) args_callback usage;
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
		com.class_path <- lines @ com.class_path;
	);
	if !Common.display then begin
		com.verbose <- false;
		xml_out := None;
		no_output := true;
		com.warning <- store_message;
		com.error <- (fun msg p ->
			store_message msg p;
			has_error := true;
		);
	end;
	let add_std dir =
		com.class_path <- List.map (fun p -> p ^ dir ^ "/_std/") com.std_path @ com.class_path
	in
	let ext = (match com.platform with
		| Cross ->
			(* no platform selected *)
			set_platform Cross "";
			"?"
		| Flash | Flash9 ->
			Common.define com ("flash" ^ string_of_int com.flash_version);
			if com.flash_version >= 9 then begin
				Common.define com "flash9"; (* always define flash9, even for flash10+ *)
				com.package_rules <- PMap.add "flash" (Directory "flash9") com.package_rules;
				com.package_rules <- PMap.add "flash9" Forbidden com.package_rules;
				com.platform <- Flash9;
				add_std "flash9";
			end else
				add_std "flash";
			"swf"
		| Neko -> add_std "neko"; "n"
		| Js -> add_std "js"; "js"
		| Php -> add_std "php"; "php"
		| Cpp -> add_std "cpp"; "cpp"
	) in
	(* check file extension. In case of wrong commandline, we don't want
		to accidentaly delete a source file. *)
	if not !no_output && file_extension com.file = ext then delete_file com.file;
	List.iter (fun f -> f()) (List.rev (!pre_compilation));
	if !classes = [([],"Std")] then begin
		if !cmds = [] && not !did_something then Arg.usage basic_args_spec usage;
	end else begin
		if com.verbose then print_endline ("Classpath : " ^ (String.concat ";" com.class_path));
		let t = Common.timer "typing" in
		Typecore.type_expr_ref := (fun ctx e need_val -> Typer.type_expr ~need_val ctx e);
		let ctx = Typer.create com in
		List.iter (fun cpath -> ignore(ctx.Typecore.g.Typecore.do_load_module ctx cpath Ast.null_pos)) (List.rev !classes);
		Typer.finalize ctx;
		t();
		if !has_error then do_exit();
		if !no_output then com.platform <- Cross;
		let types, modules = Typer.generate ctx com.main_class (!excludes) in
		com.types <- types;
		com.modules <- modules;
		com.lines <- Lexer.build_line_index();
		let filters = [
			Codegen.check_local_vars_init;
			Codegen.block_vars com;
		] in
		let tfilters = [
			Codegen.fix_overrides com;
		] in
		let filters = (match com.platform with Js | Php | Cpp -> Optimizer.sanitize :: filters | _ -> filters) in
		let filters = (if not com.foptimize then filters else Optimizer.reduce_expression ctx :: filters) in
		Codegen.post_process com filters tfilters;
		if Common.defined com "dump" then Codegen.dump_types com;
		(match com.platform with
		| Cross ->
			if !interp then begin
				let ctx = Interp.create com in
				Interp.add_types ctx com.types;
			end;
		| Flash | Flash9 when !gen_as3 ->
			if com.verbose then print_endline ("Generating AS3 in : " ^ com.file);
			Genas3.generate com;
		| Flash | Flash9 ->
			if com.verbose then print_endline ("Generating swf : " ^ com.file);
			Genswf.generate com !swf_header;
		| Neko ->
			if com.verbose then print_endline ("Generating neko : " ^ com.file);
			Genneko.generate com !libs;
		| Js ->
			if com.verbose then print_endline ("Generating js : " ^ com.file);
			Genjs.generate com
		| Php ->
			if com.verbose then print_endline ("Generating PHP in : " ^ com.file);
			Genphp.generate com;
		| Cpp ->
			if com.verbose then print_endline ("Generating Cpp in : " ^ com.file);
			Gencpp.generate com;
		);
		(match !xml_out with
		| None -> ()
		| Some file ->
			if com.verbose then print_endline ("Generating xml : " ^ com.file);
			Genxml.generate com file);
	end;
	if not !no_output then List.iter (fun cmd ->
		let t = Common.timer "command" in
		let len = String.length cmd in
		if len > 3 && String.sub cmd 0 3 = "cd " then
			Sys.chdir (String.sub cmd 3 (len - 3))
		else
			if Sys.command cmd <> 0 then failwith "Command failed";
		t();
	) (List.rev !cmds)
with
	| Common.Abort (m,p) -> report m p
	| Lexer.Error (m,p) -> report (Lexer.error_msg m) p
	| Parser.Error (m,p) -> report (Parser.error_msg m) p
	| Typecore.Error (m,p) -> report (Typecore.error_msg m) p
	| Interp.Error (msg,p :: l) ->
		store_message msg p;
		List.iter (store_message "Called from") l;
		report "Aborted" Ast.null_pos;
	| Failure msg | Arg.Bad msg -> report ("Error : " ^ msg) Ast.null_pos
	| Arg.Help msg -> print_string msg
	| Hxml_found -> ()
	| Typer.Display t ->
		(*
			documentation is currently not output even when activated
			because the parse 'eats' it when used in "resume" mode
		*)
		let ctx = Type.print_context() in
		(match Type.follow t with
		| Type.TAnon a ->
			let fields = PMap.fold (fun f acc ->
				if not f.Type.cf_public then
					acc
				else
					(f.Type.cf_name,Type.s_type ctx f.Type.cf_type,match f.Type.cf_doc with None -> "" | Some d -> d) :: acc
			) a.Type.a_fields [] in
			let fields = if !measure_times then begin
				close_time();
				let tot = ref 0. in
				Hashtbl.iter (fun _ t -> tot := !tot +. t.total) Common.htimers;
				let fields = ("@TOTAL", Printf.sprintf "%.3fs" (get_time() -. start), "") :: fields in
				Hashtbl.fold (fun _ t acc ->
					("@TIME " ^ t.name, Printf.sprintf "%.3fs (%.0f%%)" t.total (t.total *. 100. /. !tot), "") :: acc
				) Common.htimers fields;
			end else
				fields
			in
			report_list fields;
		| _ ->
			prerr_endline "<type>";
			prerr_endline (htmlescape (Type.s_type ctx t));
			prerr_endline "</type>");
		exit 0;
	| Parser.TypePath (p,c) ->
		(match c with
		| None ->
			let packs, classes = read_type_path com p in
			if packs = [] && classes = [] then report ("No classes found in " ^ String.concat "." p) Ast.null_pos;
			report_list (List.map (fun f -> f,"","") (packs @ classes))
		| Some c ->
			try
				let ctx = Typer.create com in
				let m = Typeload.load_module ctx (p,c) Ast.null_pos in
				report_list (List.map (fun t -> snd (Type.t_path t),"","") (List.filter (fun t -> not (Type.t_private t)) m.Type.mtypes))
			with _ ->
				report ("Could not load module " ^ (Ast.s_type_path (p,c))) Ast.null_pos
		);
		exit 0;
	| e when (try Sys.getenv "OCAMLRUNPARAM" <> "b" with _ -> true) ->
		report (Printexc.to_string e) Ast.null_pos

;;
let all = Common.timer "other" in
process_params [] (List.tl (Array.to_list Sys.argv));
all();
if !measure_times then begin
	let tot = ref 0. in
	Hashtbl.iter (fun _ t -> tot := !tot +. t.total) Common.htimers;
	Printf.eprintf "Total time : %.3fs\n" !tot;
	Printf.eprintf "------------------------------------\n";
	Hashtbl.iter (fun _ t ->
		Printf.eprintf "  %s : %.3fs, %.0f%%\n" t.name t.total (t.total *. 100. /. !tot);
	) Common.htimers;
end;