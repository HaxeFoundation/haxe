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

type context = {
	com : Common.context;
	mutable messages : string list;
	mutable params : string list;
	mutable has_next : bool;
	mutable has_error : bool;
}

type cache = {
	mutable c_haxelib : (string list, string list) Hashtbl.t;
	mutable c_files : (string, float * Ast.package) Hashtbl.t;
}

exception Abort
exception Completion of string

let version = 208

let measure_times = ref false
let prompt = ref false
let start_time = ref (get_time())
let global_cache = ref None

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

let message ctx msg p =
	ctx.messages <- format msg p :: ctx.messages

let error ctx msg p =
	message ctx msg p;
	ctx.has_error <- true

let htmlescape s =
	let s = String.concat "&lt;" (ExtString.String.nsplit s "<") in
	let s = String.concat "&gt;" (ExtString.String.nsplit s ">") in
	s

let complete_fields fields =
	let b = Buffer.create 0 in
	Buffer.add_string b "<list>\n";
	List.iter (fun (n,t,d) ->
		Buffer.add_string b (Printf.sprintf "<i n=\"%s\"><t>%s</t><d>%s</d></i>\n" n (htmlescape t) (htmlescape d))
	) (List.sort (fun (a,_,_) (b,_,_) -> compare a b) fields);
	Buffer.add_string b "</list>\n";
	raise (Completion (Buffer.contents b))

let file_extension f =
	let cl = ExtString.String.nsplit f "." in
	match List.rev cl with
	| [] -> ""
	| x :: _ -> x

let make_path f =
	let f = String.concat "/" (ExtString.String.nsplit f "\\") in
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
		| [x] -> if String.length x = 0 || not (x.[0] = '_' || (x.[0] >= 'A' && x.[0] <= 'Z')) || invalid_char x then error() else [] , x
		| x :: l ->
			if String.length x = 0 || x.[0] < 'a' || x.[0] > 'z' || invalid_char x then error() else
				let path , name = loop l in
				x :: path , name
	in
	loop cl

let unique l =
	let rec _unique = function
		| [] -> []
		| x1 :: x2 :: l when x1 = x2 -> _unique (x2 :: l)
		| x :: l -> x :: _unique l
	in
	_unique (List.sort compare l)

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
	unique !packages, unique !classes

let delete_file f = try Sys.remove f with _ -> ()

let expand_env path =
	let r = Str.regexp "%\\([A-Za-z0-9_]+\\)%" in
	Str.global_substitute r (fun s -> try Sys.getenv (Str.matched_group 1 s) with Not_found -> "") path

let unquote v =
	let len = String.length v in
	if len > 0 && v.[0] = '"' && v.[len - 1] = '"' then String.sub v 1 (len - 2) else v

let parse_hxml_data data =
	let lines = Str.split (Str.regexp "[\r\n]+") data in
	List.concat (List.map (fun l ->
		let l = unquote (expand_env (ExtString.String.strip l)) in
		if l = "" || l.[0] = '#' then
			[]
		else if l.[0] = '-' then
			try
				let a, b = ExtString.String.split l " " in
				[unquote a; unquote (ExtString.String.strip b)]
			with
				_ -> [l]
		else
			[l]
	) lines)

let parse_hxml file =
	let ch = IO.input_channel (try open_in_bin file with _ -> failwith ("File not found " ^ file)) in
	let data = IO.read_all ch in
	IO.close_in ch;
	parse_hxml_data data

let lookup_classes com fpath =
	let spath = String.lowercase fpath in
	let rec loop = function
		| [] -> []
		| cp :: l ->
			let cp = (if cp = "" then "./" else cp) in
			let c = normalize_path (Common.get_full_path cp) in
			let clen = String.length c in
			if clen < String.length fpath && String.sub spath 0 clen = String.lowercase c then begin
				let path = String.sub fpath clen (String.length fpath - clen) in
				(try [make_path path] with _ -> loop l)
			end else
				loop l
	in
	loop com.class_path

let add_swf_lib com file =
	let swf_data = ref None in
	let swf_classes = ref None in
	let getSWF = (fun() ->
		match !swf_data with
		| None ->
			let d = Genswf.parse_swf com file in
			swf_data := Some d;
			d
		| Some d -> d
	) in
	let extract = (fun() ->
		match !swf_classes with
		| None ->
			let d = Genswf.extract_data (getSWF()) in
			swf_classes := Some d;
			d
		| Some d -> d
	) in
	let build cl p =
		match (try Some (Hashtbl.find (extract()) cl) with Not_found -> None) with
		| None -> None
		| Some c -> Some (file, Genswf.build_class com c file)
	in
	com.load_extern_type <- com.load_extern_type @ [build];
	com.swf_libs <- (file,getSWF,extract) :: com.swf_libs

let add_libs com libs =
	let call_haxelib() =
		let t = Common.timer "haxelib" in
		let cmd = "haxelib path " ^ String.concat " " libs in
		let p = Unix.open_process_in cmd in
		let lines = Std.input_list p in
		let ret = Unix.close_process_in p in
		if ret <> Unix.WEXITED 0 then failwith (String.concat "\n" lines);
		t();
		lines
	in
	match libs with
	| [] -> ()
	| _ ->
		let lines = match !global_cache with
			| Some cache ->
				(try
					(* if we are compiling, really call haxelib since library path might have changed *)
					if not com.display then raise Not_found;
					Hashtbl.find cache.c_haxelib libs
				with Not_found ->
					let lines = call_haxelib() in
					Hashtbl.replace cache.c_haxelib libs lines;
					lines)
			| _ -> call_haxelib()
		in
		let lines = List.fold_left (fun acc l ->
			let p = String.length l - 1 in
			let l = (if l.[p] = '\r' then String.sub l 0 p else l) in
			match (if p > 3 then String.sub l 0 3 else "") with
			| "-D " ->
				Common.define com (String.sub l 3 (String.length l - 3));
				acc
			| "-L " ->
				com.neko_libs <- String.sub l 3 (String.length l - 3) :: com.neko_libs;
				acc
			| _ ->
				l :: acc
		) [] lines in
		com.class_path <- lines @ com.class_path

let create_context params =
	{
		com = Common.create version;
		params = params;
		messages = [];
		has_next = false;
		has_error = false;
	}

let default_flush ctx =
	List.iter prerr_endline (List.rev ctx.messages);
	if ctx.has_error && !prompt then begin
		print_endline "Press enter to exit...";
		ignore(read_line());
	end;
	if ctx.has_error then exit 1

let rec process_params flush acc = function
	| [] ->
		let ctx = create_context (List.rev acc) in
		init flush ctx;
		flush ctx
	| "--next" :: l ->
		let ctx = create_context (List.rev acc) in
		ctx.has_next <- true;
		init flush ctx;
		flush ctx;
		process_params flush [] l
	| "--cwd" :: dir :: l ->
		(* we need to change it immediately since it will affect hxml loading *)
		(try Unix.chdir dir with _ -> ());
		process_params flush (dir :: "--cwd" :: acc) l
	| arg :: l ->
		match List.rev (ExtString.String.nsplit arg ".") with
		| "hxml" :: _ -> process_params flush acc (parse_hxml arg @ l)
		| _ -> process_params flush (arg :: acc) l

and wait_loop boot_com host port =
	let sock = Unix.socket Unix.PF_INET Unix.SOCK_STREAM 0 in
	(try Unix.bind sock (Unix.ADDR_INET (Unix.inet_addr_of_string host,port)) with _ -> failwith ("Couldn't wait on " ^ host ^ ":" ^ string_of_int port));
	Unix.listen sock 10;
	Sys.catch_break false;
	let verbose = boot_com.verbose in
	if verbose then print_endline ("Waiting on " ^ host ^ ":" ^ string_of_int port);
	let bufsize = 1024 in
	let tmp = String.create bufsize in
	let cache = {
		c_haxelib = Hashtbl.create 0;
		c_files = Hashtbl.create 0;
	} in
	global_cache := Some cache;
	let get_signature com = 
		match com.defines_signature with
		| Some s -> s
		| None ->
			let s = Digest.string (String.concat "@" (PMap.foldi (fun k _ acc -> k :: acc) com.defines [])) in
			com.defines_signature <- Some s;
			s
	in
	let file_time file =
		try (Unix.stat file).Unix.st_mtime with _ -> 0.
	in
	Typeload.parse_hook := (fun com2 file p ->
		let sign = get_signature com2 in
		let ffile = Common.get_full_path file in
		let ftime = file_time ffile in
		let fkey = ffile ^ "!" ^ sign in
		try
			let time, data = Hashtbl.find cache.c_files fkey in
			if time <> ftime then raise Not_found;
			data
		with Not_found ->
			let data = Typeload.parse_file com2 file p in
			if verbose && not com2.verbose then print_endline ("Parsed " ^ ffile);
			Hashtbl.replace cache.c_files fkey (ftime,data);
			data
	);
	while true do
		let sin, _ = Unix.accept sock in
		let t0 = get_time() in
		Unix.set_nonblock sin;
		if verbose then print_endline "Client connected";
		let b = Buffer.create 0 in
		let rec read_loop() =
			try
				let r = Unix.recv sin tmp 0 bufsize [] in
				if verbose then Printf.printf "Reading %d bytes\n" r;
				Buffer.add_substring b tmp 0 r;
				if r > 0 && tmp.[r-1] = '\000' then Buffer.sub b 0 (Buffer.length b - 1) else read_loop();
			with Unix.Unix_error((Unix.EWOULDBLOCK|Unix.EAGAIN),_,_) ->
				if verbose then print_endline "Waiting for data...";
				ignore(Unix.select [] [] [] 0.1);
				read_loop()
		in
		let send str =
			let rec loop pos len =
				if len = 0 then
					()
				else
					let s = Unix.send sin str pos len [] in
					loop (pos + s) (len - s)
			in
			loop 0 (String.length str)
		in
		let flush ctx =
			List.iter (fun s -> send (s ^ "\n")) (List.rev ctx.messages)
		in
		(try
			let data = parse_hxml_data (read_loop()) in
			Unix.clear_nonblock sin;
			if verbose then print_endline ("Processing Arguments [" ^ String.concat "," data ^ "]");
			(try
				Common.display_default := false;
				Parser.resume_display := Ast.null_pos;
				measure_times := false;
				start_time := get_time();
				process_params flush [] data
			with Completion str ->
				if verbose then print_endline ("Completion Response =\n" ^ str);
				send str
			);
			if verbose then Printf.printf "Time spent : %.3fs\n" (get_time() -. t0);
		with Unix.Unix_error _ ->
			if verbose then print_endline "Connection Aborted");
		if verbose then print_endline "Closing connection";
		Unix.close sin;
	done

and init flush ctx =
	let usage = Printf.sprintf
		"haXe Compiler %d.%.2d - (c)2005-2011 Motion-Twin\n Usage : haxe%s -main <class> [-swf|-js|-neko|-php|-cpp|-as3] <output> [options]\n Options :"
		(version / 100) (version mod 100) (if Sys.os_type = "Win32" then ".exe" else "")
	in
	let com = ctx.com in
	let classes = ref [([],"Std")] in
try
	let xml_out = ref None in
	let swf_header = ref None in
	let cmds = ref [] in
	let config_macros = ref [] in
	let cp_libs = ref [] in
	let gen_as3 = ref false in
	let no_output = ref false in
	let did_something = ref false in
	let force_typing = ref false in
	let pre_compilation = ref [] in
	let interp = ref false in
	Common.define com ("haxe_" ^ string_of_int version);
	com.warning <- (fun msg p -> message ctx ("Warning : " ^ msg) p);
	com.error <- error ctx;
	Parser.display_error := (fun e p -> com.error (Parser.error_msg e) p);
	Parser.use_doc := !Common.display_default;
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
				com.class_path <- [base_path ^ "std/";""]);
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
			add_libs com (!cp_libs);
			cp_libs := [];
			com.class_path <- normalize_path (expand_env path) :: com.class_path
		),"<path> : add a directory to find source files");
		("-js",Arg.String (set_platform Js),"<file> : compile code to JavaScript file");
		("-swf",Arg.String (set_platform Flash),"<file> : compile code to Flash SWF file");
		("-as3",Arg.String (fun dir ->
			set_platform Flash dir;
			if com.flash_version < 9. then com.flash_version <- 9.;
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
			cp_libs := l :: !cp_libs;
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
		("-swf-version",Arg.Float (fun v ->
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
			add_swf_lib com file
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
		("-prompt", Arg.Unit (fun() -> prompt := true),": prompt on error");
		("-cmd", Arg.String (fun cmd ->
			cmds := expand_env (unquote cmd) :: !cmds
		),": run the specified command after successful compilation");
		("--flash-strict", define "flash_strict", ": more type strict flash API");
		("--no-traces", define "no_traces", ": don't compile trace calls in the program");
		("--flash-use-stage", define "flash_use_stage", ": place objects found on the stage of the SWF lib");
		("--gen-hx-classes", Arg.Unit (fun() ->
			force_typing := true;
			pre_compilation := (fun() ->
				List.iter (fun (_,_,extract) ->
					Hashtbl.iter (fun n _ -> classes := n :: !classes) (extract())
				) com.swf_libs;
			) :: !pre_compilation;
			xml_out := Some "hx"
		),": generate hx headers for all input classes");
		("--next", Arg.Unit (fun() -> assert false), ": separate several haxe compilations");
		("--display", Arg.String (fun file_pos ->
			match file_pos with
			| "classes" ->
				pre_compilation := (fun() -> raise (Parser.TypePath (["."],None))) :: !pre_compilation;
			| "keywords" ->
				complete_fields (Hashtbl.fold (fun k _ acc -> (k,"","") :: acc) Lexer.keywords [])
			| _ ->
				let file, pos = try ExtString.String.split file_pos "@" with _ -> failwith ("Invalid format : " ^ file_pos) in
				let pos = try int_of_string pos with _ -> failwith ("Invalid format : "  ^ pos) in
				com.display <- true;
				Common.display_default := true;
				Common.define com "display";
				Parser.use_doc := true;
				Parser.resume_display := {
					Ast.pfile = Common.get_full_path file;
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
		("--php-lib",Arg.String (fun f ->
 			if com.php_lib <> None then raise (Arg.Bad "Multiple --php-lib");
 			com.php_lib <- Some f;
 		),"<filename> : select the name for the php lib folder");
		("--php-prefix", Arg.String (fun f ->
			if com.php_prefix <> None then raise (Arg.Bad "Multiple --php-prefix");
			com.php_prefix <- Some f;
			Common.define com "php_prefix";
		),"<name> : prefix all classes with given name");
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
		("--macro", Arg.String (fun e ->
			force_typing := true;
			config_macros := e :: !config_macros
		)," : call the given macro before typing anything else");
		("--dead-code-elimination", Arg.Unit (fun () ->
			com.dead_code_elimination <- true;
			Common.add_filter com (fun() -> Optimizer.filter_dead_code com);
		)," : remove unused methods");
		("--wait", Arg.String (fun hp ->
			let host, port = (try ExtString.String.split hp ":" with _ -> "127.0.0.1", hp) in
			wait_loop com host (try int_of_string port with _ -> raise (Arg.Bad "Invalid port"))
		),"<[host:]port> : wait on the given port for commands to run)");
		("--cwd", Arg.String (fun dir ->
			(try Unix.chdir dir with _ -> raise (Arg.Bad "Invalid directory"))
		),"<dir> : set current working directory");
		("-swf9",Arg.String (fun file ->
			set_platform Flash file;
			if com.flash_version < 9. then com.flash_version <- 9.;
		),"<file> : [deprecated] compile code to Flash9 SWF file");
	] in
	let current = ref 0 in
	let args = Array.of_list ("" :: ctx.params) in
	let args_callback cl = classes := make_path cl :: !classes in
	Arg.parse_argv ~current args (basic_args_spec @ adv_args_spec) args_callback usage;
	add_libs com (!cp_libs);
	(try ignore(Common.find_file com "mt/Include.hx"); Common.define com "mt"; with Not_found -> ());
	if com.display then begin
		xml_out := None;
		no_output := true;
		com.warning <- message ctx;
		com.error <- error ctx;
		com.main_class <- None;
		classes := lookup_classes com (!Parser.resume_display).Ast.pfile;
	end;
	let add_std dir =
		com.class_path <- List.filter (fun s -> not (List.mem s com.std_path)) com.class_path @ List.map (fun p -> p ^ dir ^ "/_std/") com.std_path @ com.std_path
	in
	let ext = (match com.platform with
		| Cross ->
			(* no platform selected *)
			set_platform Cross "";
			"?"
		| Flash | Flash9 ->
			if com.flash_version >= 9. then begin
				let rec loop = function
					| [] -> ()
					| (v,_) :: _ when v > com.flash_version -> ()
					| (v,def) :: l ->
						Common.define com ("flash" ^ def);
						loop l
				in
				loop Common.flash_versions;
				com.package_rules <- PMap.add "flash" (Directory "flash9") com.package_rules;
				com.package_rules <- PMap.add "flash9" Forbidden com.package_rules;
				com.platform <- Flash9;
				add_std "flash9";
			end else begin
				Common.define com ("flash" ^ string_of_int (int_of_float com.flash_version));
				add_std "flash";
			end;
			"swf"
		| Neko -> add_std "neko"; "n"
		| Js -> add_std "js"; "js"
		| Php -> add_std "php"; "php"
		| Cpp -> add_std "cpp"; "cpp"
	) in
	(* if we are at the last compilation step, allow all packages accesses - in case of macros or opening another project file *)
	if com.display && not ctx.has_next then com.package_rules <- PMap.foldi (fun p r acc -> match r with Forbidden -> acc | _ -> PMap.add p r acc) com.package_rules PMap.empty;

	(* check file extension. In case of wrong commandline, we don't want
		to accidentaly delete a source file. *)
	if not !no_output && file_extension com.file = ext then delete_file com.file;
	List.iter (fun f -> f()) (List.rev (!pre_compilation));
	if !classes = [([],"Std")] && not !force_typing then begin
		if !cmds = [] && not !did_something then Arg.usage basic_args_spec usage;
	end else begin
		if com.verbose then print_endline ("Classpath : " ^ (String.concat ";" com.class_path));
		let t = Common.timer "typing" in
		Typecore.type_expr_ref := (fun ctx e need_val -> Typer.type_expr ~need_val ctx e);
		let tctx = Typer.create com in
		List.iter (Typer.call_init_macro tctx) (List.rev !config_macros);
		List.iter (fun cpath -> ignore(tctx.Typecore.g.Typecore.do_load_module tctx cpath Ast.null_pos)) (List.rev !classes);
		Typer.finalize tctx;
		t();
		if ctx.has_error then raise Abort;
		let t = Common.timer "filters" in
		let main, types, modules = Typer.generate tctx com.main_class in
		com.main <- main;
		com.types <- types;
		com.modules <- modules;
		let filters = [
			if com.foptimize then Optimizer.reduce_expression tctx else Optimizer.sanitize tctx;
			Codegen.check_local_vars_init;
			Codegen.captured_vars com;
			Codegen.rename_local_vars com;
		] in
		Codegen.post_process com.types filters;
		Common.add_filter com (fun() -> List.iter (Codegen.on_generate tctx) com.types);
		List.iter (fun f -> f()) (List.rev com.filters);
		(match !xml_out with
		| None -> ()
		| Some "hx" ->
			Genxml.generate_hx com
		| Some file ->
			if com.verbose then print_endline ("Generating xml : " ^ com.file);
			Genxml.generate com file);
		if com.platform = Flash9 || com.platform = Cpp then List.iter (Codegen.fix_overrides com) com.types;
		if Common.defined com "dump" then Codegen.dump_types com;
		t();
		(match com.platform with
		| _ when !no_output ->
			if !interp then begin
				let ctx = Interp.create com (Typer.make_macro_api tctx Ast.null_pos) in
				Interp.add_types ctx com.types;
				(match com.main with
				| None -> ()
				| Some e -> ignore(Interp.eval_expr ctx e));
			end;
		| Cross ->
			()
		| Flash | Flash9 when !gen_as3 ->
			if com.verbose then print_endline ("Generating AS3 in : " ^ com.file);
			Genas3.generate com;
		| Flash | Flash9 ->
			if com.verbose then print_endline ("Generating swf : " ^ com.file);
			Genswf.generate com !swf_header;
		| Neko ->
			if com.verbose then print_endline ("Generating neko : " ^ com.file);
			Genneko.generate com;
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
	| Abort ->
		()
	| Common.Abort (m,p) ->
		error ctx m p
	| Lexer.Error (m,p) ->
		error ctx (Lexer.error_msg m) p
	| Parser.Error (m,p) ->
		error ctx (Parser.error_msg m) p
	| Typecore.Error (Typecore.Forbid_package _,_) when !Common.display_default && ctx.has_next ->
		()
	| Typecore.Error (m,p) ->
		error ctx (Typecore.error_msg m) p
	| Interp.Error (msg,p :: l) ->
		message ctx msg p;
		List.iter (message ctx "Called from") l;
		error ctx "Aborted" Ast.null_pos;
	| Failure msg | Arg.Bad msg ->
		error ctx ("Error : " ^ msg) Ast.null_pos
	| Arg.Help msg ->
		print_string msg
	| Typer.DisplayFields fields ->
		let ctx = Type.print_context() in
		let fields = List.map (fun (name,t,doc) -> name, Type.s_type ctx t, (match doc with None -> "" | Some d -> d)) fields in
		let fields = if !measure_times then begin
			let rec loop() =
				match !curtime with
				| [] -> ()
				| _ -> close_time(); loop();
			in
			loop();
			let tot = ref 0. in
			Hashtbl.iter (fun _ t -> tot := !tot +. t.total) Common.htimers;
			let fields = ("@TOTAL", Printf.sprintf "%.3fs" (get_time() -. !start_time), "") :: fields in
			Hashtbl.fold (fun _ t acc ->
				("@TIME " ^ t.name, Printf.sprintf "%.3fs (%.0f%%)" t.total (t.total *. 100. /. !tot), "") :: acc
			) Common.htimers fields;
		end else
			fields
		in
		complete_fields fields
	| Typer.DisplayTypes tl ->
		let ctx = Type.print_context() in
		let b = Buffer.create 0 in
		List.iter (fun t ->
			Buffer.add_string b "<type>\n";
			Buffer.add_string b (htmlescape (Type.s_type ctx t));
			Buffer.add_string b "\n</type>\n";
		) tl;
		raise (Completion (Buffer.contents b))
	| Parser.TypePath (p,c) ->
		(match c with
		| None ->
			let packs, classes = read_type_path com p in
			if packs = [] && classes = [] then
				error ctx ("No classes found in " ^ String.concat "." p) Ast.null_pos
			else
				complete_fields (List.map (fun f -> f,"","") (packs @ classes))
		| Some c ->
			try
				let ctx = Typer.create com in
				let m = Typeload.load_module ctx (p,c) Ast.null_pos in
				complete_fields (List.map (fun t -> snd (Type.t_path t),"","") (List.filter (fun t -> not (Type.t_infos t).Type.mt_private) m.Type.mtypes))
			with _ ->
				error ctx ("Could not load module " ^ (Ast.s_type_path (p,c))) Ast.null_pos)
	| e when (try Sys.getenv "OCAMLRUNPARAM" <> "b" with _ -> true) ->
		error ctx (Printexc.to_string e) Ast.null_pos

;;
let all = Common.timer "other" in
Sys.catch_break true;
(try
	process_params default_flush [] (List.tl (Array.to_list Sys.argv));
with Completion c ->
	prerr_endline c;
	exit 0
);
all();
if !measure_times then begin
	let tot = ref 0. in
	Hashtbl.iter (fun _ t -> tot := !tot +. t.total) Common.htimers;
	Printf.eprintf "Total time : %.3fs\n" !tot;
	Printf.eprintf "------------------------------------\n";
	let timers = List.sort (fun t1 t2 -> compare t1.name t2.name) (Hashtbl.fold (fun _ t acc -> t :: acc) Common.htimers []) in
	List.iter (fun t ->
		Printf.eprintf "  %s : %.3fs, %.0f%%\n" t.name t.total (t.total *. 100. /. !tot);
	) timers;
end;
