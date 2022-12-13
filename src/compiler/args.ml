open Globals
open Common
open CompilationContext

let limit_string s offset =
	let rest = 80 - offset in
	let words = ExtString.String.nsplit s " " in
	let rec loop i words = match words with
		| word :: words ->
			if String.length word + i + 1 > rest then (Printf.sprintf "\n%*s" offset "") :: word :: loop (String.length word) words
			else (if i = 0 then "" else " ") :: word :: loop (i + 1 + String.length word) words
		| [] ->
			[]
	in
	String.concat "" (loop 0 words)

let usage_string ?(print_cat=true) arg_spec usage =
	let make_label = fun names hint -> Printf.sprintf "%s %s" (String.concat ", " names) hint in
	let args = (List.filter (fun (cat, ok, dep, spec, hint, doc) -> (List.length ok) > 0) arg_spec) in
	let cat_order = ["Target";"Compilation";"Optimization";"Debug";"Batch";"Services";"Compilation Server";"Target-specific";"Miscellaneous"] in
	let cats = List.filter (fun x -> List.mem x (List.map (fun (cat, _, _, _, _, _) -> cat) args)) cat_order in
	let max_length = List.fold_left max 0 (List.map String.length (List.map (fun (_, ok, _, _, hint, _) -> make_label ok hint) args)) in
	usage ^ (String.concat "\n" (List.flatten (List.map (fun cat -> (if print_cat then ["\n"^cat^":"] else []) @ (List.map (fun (cat, ok, dep, spec, hint, doc) ->
		let label = make_label ok hint in
		Printf.sprintf "  %s%s  %s" label (String.make (max_length - (String.length label)) ' ') doc
	) (List.filter (fun (cat', _, _, _, _, _) -> (if List.mem cat' cat_order then cat' else "Miscellaneous") = cat) args))) cats)))

let process_args arg_spec =
	List.flatten(List.map (fun (cat, ok, dep, spec, hint, doc) ->
		(* official argument names *)
		(List.map (fun (arg) -> (arg, spec, doc)) ok) @
		let dep_fun arg spec = () in
		let dep_spec arg spec = match spec with
			| Arg.String f -> Arg.String (fun x -> dep_fun arg spec; f x)
			| Arg.Unit f -> Arg.Unit (fun x -> dep_fun arg spec; f x)
			| Arg.Bool f -> Arg.Bool (fun x -> dep_fun arg spec; f x)
			| _ -> spec in
		(List.map (fun (arg) -> (arg, dep_spec arg spec, doc)) dep)
	) arg_spec)

let parse_args com =
	let usage = Printf.sprintf
		"Haxe Compiler %s - (C)2005-2022 Haxe Foundation\nUsage: haxe%s <target> [options] [hxml files and dot paths...]\n"
		s_version_full (if Sys.os_type = "Win32" then ".exe" else "")
	in
	let actx = {
		classes = [([],"Std")];
		xml_out = None;
		json_out = None;
		cmds = [];
		config_macros = [];
		no_output = false;
		did_something = false;
		force_typing = false;
		pre_compilation = [];
		interp = false;
		jvm_flag = false;
		swf_version = false;
		native_libs = [];
		raise_usage = (fun () -> ());
		display_arg = None;
		deprecations = [];
	} in
	let add_deprecation s =
		actx.deprecations <- s :: actx.deprecations
	in
	let add_native_lib file extern = actx.native_libs <- (file,extern) :: actx.native_libs in
	let basic_args_spec = [
		("Target",["--js"],["-js"],Arg.String (set_platform com Js),"<file>","generate JavaScript code into target file");
		("Target",["--lua"],["-lua"],Arg.String (set_platform com Lua),"<file>","generate Lua code into target file");
		("Target",["--swf"],["-swf"],Arg.String (set_platform com Flash),"<file>","generate Flash SWF bytecode into target file");
		("Target",["--neko"],["-neko"],Arg.String (set_platform com Neko),"<file>","generate Neko bytecode into target file");
		("Target",["--php"],["-php"],Arg.String (fun dir ->
			actx.classes <- (["php"],"Boot") :: actx.classes;
			set_platform com Php dir;
		),"<directory>","generate PHP code into target directory");
		("Target",["--cpp"],["-cpp"],Arg.String (fun dir ->
			set_platform com Cpp dir;
		),"<directory>","generate C++ code into target directory");
		("Target",["--cppia"],["-cppia"],Arg.String (fun file ->
			Common.define com Define.Cppia;
			set_platform com Cpp file;
		),"<file>","generate Cppia bytecode into target file");
		("Target",["--cs"],["-cs"],Arg.String (fun dir ->
			set_platform com Cs dir;
		),"<directory>","generate C# code into target directory");
		("Target",["--java"],["-java"],Arg.String (fun dir ->
			set_platform com Java dir;
		),"<directory>","generate Java code into target directory");
		("Target",["--jvm"],[],Arg.String (fun dir ->
			Common.define com Define.Jvm;
			actx.jvm_flag <- true;
			set_platform com Java dir;
		),"<file>","generate JVM bytecode into target file");
		("Target",["--python"],["-python"],Arg.String (fun dir ->
			set_platform com Python dir;
		),"<file>","generate Python code into target file");
		("Target",["--hl"],["-hl"],Arg.String (fun file ->
			set_platform com Hl file;
		),"<file>","generate HashLink .hl bytecode or .c code into target file");
		("Target",[],["-x"], Arg.String (fun cl ->
			let cpath = Path.parse_type_path cl in
			(match com.main_class with
				| Some c -> if cpath <> c then raise (Arg.Bad "Multiple --main classes specified")
				| None -> com.main_class <- Some cpath);
			actx.classes <- cpath :: actx.classes;
			Common.define com Define.Interp;
			set_platform com (!Globals.macro_platform) "";
			actx.interp <- true;
		),"<class>","interpret the program using internal macro system");
		("Target",["--interp"],[], Arg.Unit (fun() ->
			Common.define com Define.Interp;
			set_platform com (!Globals.macro_platform) "";
			actx.interp <- true;
		),"","interpret the program using internal macro system");
		("Target",["--run"],[], Arg.Unit (fun() ->
			raise (Arg.Bad "--run requires an argument: a Haxe module name")
		), "<module> [args...]","interpret a Haxe module with command line arguments");
		("Compilation",["-p";"--class-path"],["-cp"],Arg.String (fun path ->
			com.class_path <- Path.add_trailing_slash path :: com.class_path
		),"<path>","add a directory to find source files");
		("Compilation",["-m";"--main"],["-main"],Arg.String (fun cl ->
			if com.main_class <> None then raise (Arg.Bad "Multiple --main classes specified");
			let cpath = Path.parse_type_path cl in
			com.main_class <- Some cpath;
			actx.classes <- cpath :: actx.classes
		),"<class>","select startup class");
		("Compilation",["-D";"--define"],[],Arg.String (fun var ->
			let flag, value = try let split = ExtString.String.split var "=" in (fst split, Some (snd split)) with _ -> var, None in
			match value with
				| Some value -> Common.external_define_value com flag value
				| None -> Common.external_define com flag;
		),"<var[=value]>","define a conditional compilation flag");
		("Debug",["-v";"--verbose"],[],Arg.Unit (fun () ->
			com.verbose <- true
		),"","turn on verbose mode");
		("Debug",["--debug"],["-debug"], Arg.Unit (fun() ->
			Common.define com Define.Debug;
			com.debug <- true;
		),"","add debug information to the compiled code");
		("Miscellaneous",["--version"],["-version"],Arg.Unit (fun() ->
			com.info s_version_full null_pos;
			actx.did_something <- true;
		),"","print version and exit");
		("Miscellaneous", ["-h";"--help"], ["-help"], Arg.Unit (fun () ->
			raise (Arg.Help "")
		),"","show extended help information");
		("Miscellaneous",["--help-defines"],[], Arg.Unit (fun() ->
			let all,max_length = Define.get_documentation_list com.user_defines in
			let all = List.map (fun (n,doc) -> Printf.sprintf " %-*s: %s" max_length n (limit_string doc (max_length + 3))) all in
			List.iter (fun msg -> com.print (msg ^ "\n")) all;
			actx.did_something <- true
		),"","print help for all compiler specific defines");
		("Miscellaneous",["--help-user-defines"],[], Arg.Unit (fun() ->
			actx.did_something <- true;
			com.callbacks#add_after_init_macros (fun() ->
				let all,max_length = Define.get_user_documentation_list com.user_defines in
				let all = List.map (fun (n,doc) -> Printf.sprintf " %-*s: %s" max_length n (limit_string doc (max_length + 3))) all in
				List.iter (fun msg -> com.print (msg ^ "\n")) all;
				exit 0
			)
		),"","print help for all user defines");
		("Miscellaneous",["--help-metas"],[], Arg.Unit (fun() ->
			let all,max_length = Meta.get_documentation_list com.user_metas in
			let all = List.map (fun (n,doc) -> Printf.sprintf " %-*s: %s" max_length n (limit_string doc (max_length + 3))) all in
			List.iter (fun msg -> com.print (msg ^ "\n")) all;
			actx.did_something <- true
		),"","print help for all compiler metadatas");
		("Miscellaneous",["--help-user-metas"],[], Arg.Unit (fun() ->
			actx.did_something <- true;
			com.callbacks#add_after_init_macros (fun() ->
				let all,max_length = Meta.get_user_documentation_list com.user_metas in
				let all = List.map (fun (n,doc) -> Printf.sprintf " %-*s: %s" max_length n (limit_string doc (max_length + 3))) all in
				List.iter (fun msg -> com.print (msg ^ "\n")) all;
				exit 0
			)
		),"","print help for all user metadatas");
	] in
	let adv_args_spec = [
		("Optimization",["--dce"],["-dce"],Arg.String (fun mode ->
			(match mode with
			| "std" | "full" | "no" -> ()
			| _ -> raise (Arg.Bad "Invalid DCE mode, expected std | full | no"));
			Common.define_value com Define.Dce mode
		),"[std|full|no]","set the dead code elimination mode (default std)");
		("Target-specific",["--swf-version"],["-swf-version"],Arg.Float (fun v ->
			if not actx.swf_version || com.flash_version < v then com.flash_version <- v;
			actx.swf_version <- true;
		),"<version>","change the SWF version");
		("Target-specific",["--swf-header"],["-swf-header"],Arg.String (fun h ->
			add_deprecation "-swf-header has been deprecated, use -D swf-header instead";
			define_value com Define.SwfHeader h
		),"<header>","define SWF header (width:height:fps:color)");
		("Target-specific",["--flash-strict"],[],Arg.Unit (fun () ->
			add_deprecation "--flash-strict has been deprecated, use -D flash-strict instead";
			Common.define com Define.FlashStrict
		), "","more type strict flash API");
		("Target-specific",["--swf-lib"],["-swf-lib"],Arg.String (fun file ->
			add_native_lib file false;
		),"<file>","add the SWF library to the compiled SWF");
		("Target-specific",["--neko-lib"],[],Arg.String (fun file ->
			com.neko_libs <- file :: com.neko_libs
		),"<file>","add the neko library");
		("Target-specific",["--swf-lib-extern"],["-swf-lib-extern"],Arg.String (fun file ->
			add_native_lib file true;
		),"<file>","use the SWF library for type checking");
		("Target-specific",["--java-lib"],["-java-lib"],Arg.String (fun file ->
			add_native_lib file false;
		),"<file>","add an external JAR or directory of JAR files");
		("Target-specific",["--java-lib-extern"],[],Arg.String (fun file ->
			add_native_lib file true;
		),"<file>","use an external JAR or directory of JAR files for type checking");
		("Target-specific",["--net-lib"],["-net-lib"],Arg.String (fun file ->
			add_native_lib file false;
		),"<file>[@std]","add an external .NET DLL file");
		("Target-specific",["--net-std"],["-net-std"],Arg.String (fun file ->
			Dotnet.add_net_std com file
		),"<file>","add a root std .NET DLL search path");
		("Target-specific",["--c-arg"],["-c-arg"],Arg.String (fun arg ->
			com.c_args <- arg :: com.c_args
		),"<arg>","pass option <arg> to the native Java/C# compiler");
		("Compilation",["-r";"--resource"],["-resource"],Arg.String (fun res ->
			let file, name = (match ExtString.String.nsplit res "@" with
				| [file; name] -> file, name
				| [file] -> file, file
				| _ -> raise (Arg.Bad "Invalid Resource format, expected file@name")
			) in
			let file = (try Common.find_file com file with Not_found -> file) in
			let data = (try
				let s = Std.input_file ~bin:true file in
				if String.length s > 12000000 then raise Exit;
				s;
			with
				| Sys_error _ -> failwith ("Resource file not found: " ^ file)
				| _ -> failwith ("Resource '" ^ file ^ "' excess the maximum size of 12MB")
			) in
			if Hashtbl.mem com.resources name then failwith ("Duplicate resource name " ^ name);
			Hashtbl.add com.resources name data
		),"<file>[@name]","add a named resource file");
		("Debug",["--prompt"],["-prompt"], Arg.Unit (fun() -> Helper.prompt := true),"","prompt on error");
		("Compilation",["--cmd"],["-cmd"], Arg.String (fun cmd ->
			actx.cmds <- Helper.unquote cmd :: actx.cmds
		),"<command>","run the specified command after successful compilation");
		("Optimization",["--no-traces"],[], Arg.Unit (fun () ->
			add_deprecation "--no-traces has been deprecated, use -D no-traces instead";
			Common.define com Define.NoTraces
		), "","don't compile trace calls in the program");
		("Batch",["--next"],[], Arg.Unit (fun() -> die "" __LOC__), "","separate several haxe compilations");
		("Batch",["--each"],[], Arg.Unit (fun() -> die "" __LOC__), "","append preceding parameters to all Haxe compilations separated by --next");
		("Services",["--display"],[], Arg.String (fun input ->
			actx.display_arg <- Some input;
		),"","display code tips");
		("Services",["--xml"],["-xml"],Arg.String (fun file ->
			actx.xml_out <- Some file
		),"<file>","generate XML types description");
		("Services",["--json"],[],Arg.String (fun file ->
			actx.json_out <- Some file
		),"<file>","generate JSON types description");
		("Optimization",["--no-output"],[], Arg.Unit (fun() -> actx.no_output <- true),"","compiles but does not generate any file");
		("Debug",["--times"],[], Arg.Unit (fun() -> Timer.measure_times := true),"","measure compilation times");
		("Optimization",["--no-inline"],[],Arg.Unit (fun () ->
			add_deprecation "--no-inline has been deprecated, use -D no-inline instead";
			Common.define com Define.NoInline
		), "","disable inlining");
		("Optimization",["--no-opt"],[], Arg.Unit (fun() ->
			com.foptimize <- false;
			Common.define com Define.NoOpt;
		), "","disable code optimizations");
		("Compilation",["--remap"],[], Arg.String (fun s ->
			let pack, target = (try ExtString.String.split s ":" with _ -> raise (Arg.Bad "Invalid remap format, expected source:target")) in
			com.package_rules <- PMap.add pack (Remap target) com.package_rules;
		),"<package:target>","remap a package to another one");
		("Compilation",["--macro"],[], Arg.String (fun e ->
			actx.force_typing <- true;
			actx.config_macros <- e :: actx.config_macros
		),"<macro>","call the given macro before typing anything else");
		("Compilation Server",["--server-listen"],["--wait"], Arg.String (fun hp ->
			die "" __LOC__
		),"[[host:]port]|stdio]","wait on the given port (or use standard i/o) for commands to run");
		("Compilation Server",["--server-connect"],[], Arg.String (fun hp ->
			die "" __LOC__
		),"[host:]port]","connect to the given port and wait for commands to run");
		("Compilation Server",["--connect"],[],Arg.String (fun _ ->
			die "" __LOC__
		),"<[host:]port>","connect on the given port and run commands there");
		("Compilation",["-C";"--cwd"],[], Arg.String (fun dir ->
			(try Unix.chdir dir with _ -> raise (Arg.Bad ("Invalid directory: " ^ dir)));
			actx.did_something <- true;
		),"<directory>","set current working directory");
		("Compilation",["--haxelib-global"],[], Arg.Unit (fun () -> ()),"","pass --global argument to haxelib");
		("Compilation",["-w"],[], Arg.String (fun s ->
			let p = { pfile = "-w " ^ s; pmin = 0; pmax = 0 } in
			let l = Warning.parse_options s p in
			com.warning_options <- l :: com.warning_options
		),"<warning list>","enable or disable specific warnings");
	] in
	let args_callback cl =
		begin try
			let path,name = Path.parse_path cl in
			if StringHelper.starts_uppercase_identifier name then
				actx.classes <- (path,name) :: actx.classes
			else begin
				actx.force_typing <- true;
				actx.config_macros <- (Printf.sprintf "include('%s', true, null, null, true)" cl) :: actx.config_macros;
			end
		with Failure _ when ignore_error com ->
			()
		end
	in
	let all_args = (basic_args_spec @ adv_args_spec) in
	let all_args_spec = process_args all_args in
	let process args =
		let current = ref 0 in
		(try
			let rec loop acc args = match args with
				| "--display" :: arg :: args ->
					loop (arg :: "--display" :: acc) args
				| arg :: args ->
					loop (Helper.expand_env arg :: acc) args
				| [] ->
					List.rev acc
			in
			let args = loop [] args in
			Arg.parse_argv ~current (Array.of_list ("" :: args)) all_args_spec args_callback "";
		with
		| Arg.Help _ ->
			raise (Helper.HelpMessage (usage_string all_args usage))
		| Arg.Bad msg ->
			let first_line = List.nth (Str.split (Str.regexp "\n") msg) 0 in
			let new_msg = (Printf.sprintf "%s" first_line) in
			let r = Str.regexp "unknown option [`']?\\([-A-Za-z]+\\)[`']?" in
			try
				ignore(Str.search_forward r msg 0);
				let s = Str.matched_group 1 msg in
				let sl = List.map (fun (s,_,_) -> s) all_args_spec in
				let sl = StringError.get_similar s sl in
				begin match sl with
				| [] -> raise Not_found
				| _ ->
					let spec = List.filter (fun (_,sl',sl'',_,_,_) ->
						List.exists (fun s -> List.mem s sl) (sl' @ sl'')
					) all_args in
					let new_msg = (Printf.sprintf "%s\nDid you mean:\n%s" first_line (usage_string ~print_cat:false spec "")) in
					raise (Arg.Bad new_msg)
				end;
			with Not_found ->
				raise (Arg.Bad new_msg));
		if com.platform = Globals.Cpp && not (Define.defined com.defines DisableUnicodeStrings) && not (Define.defined com.defines HxcppSmartStings) then begin
			Define.define com.defines HxcppSmartStings;
		end;
		if Define.raw_defined com.defines "gen_hx_classes" then begin
			(* TODO: this is something we're gonna remove once we have something nicer for generating flash externs *)
			actx.force_typing <- true;
			actx.pre_compilation <- (fun() ->
				let process_lib lib =
					if not (lib#has_flag NativeLibraries.FlagIsStd) then
						List.iter (fun path -> if path <> (["java";"lang"],"String") then actx.classes <- path :: actx.classes) lib#list_modules
				in
				List.iter process_lib com.native_libs.net_libs;
				List.iter process_lib com.native_libs.swf_libs;
				List.iter process_lib com.native_libs.java_libs;
			) :: actx.pre_compilation;
			actx.xml_out <- Some "hx"
		end;
	in
	actx.raise_usage <- (fun () -> raise (Helper.HelpMessage (usage_string basic_args_spec usage)));
	(* Handle CLI arguments *)
	process com.args;
	actx