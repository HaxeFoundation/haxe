open Globals
open Common
open ParsedArg

type server_mode =
	| SMNone
	| SMListen of string
	| SMConnect of string

let columns = AtomicLazy.from_fun (fun () -> match Terminal_size.get_columns () with None -> 80 | Some c -> c)

let limit_string s offset =
	let rest = (AtomicLazy.force columns) - offset in
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

(** Static arg descriptions used for generating help strings via [usage_string].
    [usage_string] only reads the category, primary-flag names, hint, and doc
    fields — the [Arg.t] value is never invoked, so [Arg.Unit ignore] is used
    everywhere.  Entries with an empty primary-flag list ([ok = []]) are already
    filtered out by [usage_string], so they are omitted here. *)
let basic_args_descriptions = [
	("Target",          ["--js"],               ["-js"],              Arg.Unit ignore, "<file>",            "generate JavaScript code into target file");
	("Target",          ["--lua"],              ["-lua"],             Arg.Unit ignore, "<file>",            "generate Lua code into target file");
	("Target",          ["--swf"],              ["-swf"],             Arg.Unit ignore, "<file>",            "generate Flash SWF bytecode into target file");
	("Target",          ["--neko"],             ["-neko"],            Arg.Unit ignore, "<file>",            "generate Neko bytecode into target file");
	("Target",          ["--php"],              ["-php"],             Arg.Unit ignore, "<directory>",       "generate PHP code into target directory");
	("Target",          ["--cpp"],              ["-cpp"],             Arg.Unit ignore, "<directory>",       "generate C++ code into target directory");
	("Target",          ["--cppia"],            ["-cppia"],           Arg.Unit ignore, "<file>",            "generate Cppia bytecode into target file");
	("Target",          ["--jvm"],              ["-jvm"],             Arg.Unit ignore, "<file>",            "generate JVM bytecode into target file");
	("Target",          ["--python"],           ["-python"],          Arg.Unit ignore, "<file>",            "generate Python code into target file");
	("Target",          ["--hl"],               ["-hl"],              Arg.Unit ignore, "<file>",            "generate HashLink .hl bytecode or .c code into target file");
	("Target",          ["--custom-target"],    ["-custom"],          Arg.Unit ignore, "<name[=path]>",     "generate code for a custom target");
	("Target",          ["--interp"],           [],                   Arg.Unit ignore, "",                  "interpret the program using internal macro system");
	("Target",          ["--run"],              [],                   Arg.Unit ignore, "<module> [args...]","interpret a Haxe module with command line arguments");
	("Compilation",     ["-p";"--class-path"],  ["-cp"],              Arg.Unit ignore, "<path>",            "add a directory to find source files");
	("Compilation",     ["--hxb-lib"],          ["-hxb-lib"],         Arg.Unit ignore, "<path>",            "add a hxb library");
	("Compilation",     ["-m";"--main"],        ["-main"],            Arg.Unit ignore, "<class>",           "select startup class");
	("Compilation",     ["-L";"--library"],     ["-lib"],             Arg.Unit ignore, "<name[:ver]>",      "use a haxelib library");
	("Compilation",     ["-D";"--define"],      [],                   Arg.Unit ignore, "<var[=value]>",     "define a conditional compilation flag");
	("Compilation",     ["--undefine"],         [],                   Arg.Unit ignore, "<var>",             "remove a conditional compilation flag");
	("Debug",           ["-v";"--verbose"],     [],                   Arg.Unit ignore, "",                  "turn on verbose mode");
	("Debug",           ["--debug"],            ["-debug"],           Arg.Unit ignore, "",                  "add debug information to the compiled code");
	("Miscellaneous",   ["--version"],          ["-version"],         Arg.Unit ignore, "",                  "print version and exit");
	("Miscellaneous",   ["-h";"--help"],        ["-help"],            Arg.Unit ignore, "",                  "show extended help information");
	("Miscellaneous",   ["--help-defines"],     [],                   Arg.Unit ignore, "",                  "print help for all compiler specific defines");
	("Miscellaneous",   ["--help-user-defines"],[],                   Arg.Unit ignore, "",                  "print help for all user defines");
	("Miscellaneous",   ["--help-metas"],       [],                   Arg.Unit ignore, "",                  "print help for all compiler metadatas");
	("Miscellaneous",   ["--help-user-metas"],  [],                   Arg.Unit ignore, "",                  "print help for all user metadatas");
]

let adv_args_descriptions = [
	("Optimization",          ["--dce"],              ["-dce"],           Arg.Unit ignore, "[std|full|no]",      "set the dead code elimination mode (default std)");
	("Target-specific",       ["--swf-version"],      ["-swf-version"],   Arg.Unit ignore, "<version>",          "change the SWF version");
	("Target-specific",       ["--swf-lib"],          ["-swf-lib"],       Arg.Unit ignore, "<file>",             "add the SWF library to the compiled SWF");
	("Target-specific",       ["--swf-lib-extern"],   ["-swf-lib-extern"],Arg.Unit ignore, "<file>",             "use the SWF library for type checking");
	("Target-specific",       ["--java-lib"],         ["-java-lib"],      Arg.Unit ignore, "<file>",             "add an external JAR or directory of JAR files");
	("Target-specific",       ["--java-lib-extern"],  [],                 Arg.Unit ignore, "<file>",             "use an external JAR or directory of JAR files for type checking");
	("Compilation",           ["-r";"--resource"],    ["-resource"],      Arg.Unit ignore, "<file>[@name]",      "add a named resource file");
	("Debug",                 ["--prompt"],            ["-prompt"],        Arg.Unit ignore, "",                   "prompt on error");
	("Compilation",           ["--cmd"],               ["-cmd"],           Arg.Unit ignore, "<command>",          "run the specified command after successful compilation");
	("Batch",                 ["--next"],              [],                 Arg.Unit ignore, "",                   "separate several haxe compilations");
	("Batch",                 ["--each"],              [],                 Arg.Unit ignore, "",                   "append preceding parameters to all Haxe compilations separated by --next");
	("Services",              ["--display"],           [],                 Arg.Unit ignore, "",                   "display code tips");
	("Services",              ["--xml"],               ["-xml"],           Arg.Unit ignore, "<file>",             "generate XML types description");
	("Services",              ["--json"],              [],                 Arg.Unit ignore, "<file>",             "generate JSON types description");
	("Services",              ["--hxb"],               [],                 Arg.Unit ignore, "<file>",             "generate haxe binary representation to target archive");
	("Optimization",          ["--no-output"],         [],                 Arg.Unit ignore, "",                   "compiles but does not generate any file");
	("Debug",                 ["--times"],             [],                 Arg.Unit ignore, "",                   "measure compilation times");
	("Compilation",           ["--remap"],             [],                 Arg.Unit ignore, "<package:target>",   "remap a package to another one");
	("Compilation",           ["--custom-extension"],  [],                 Arg.Unit ignore, "<extension>",        "custom extension used to shadow modules with Module.custom.hx files");
	("Compilation",           ["--macro"],             [],                 Arg.Unit ignore, "<macro>",            "call the given macro before typing anything else");
	("Compilation Server",    ["--server-listen"],     ["--wait"],         Arg.Unit ignore, "[[host:]port]|stdio]","wait on the given port (or use standard i/o) for commands to run");
	("Compilation Server",    ["--server-connect"],    [],                 Arg.Unit ignore, "[host:]port]",       "connect to the given port and wait for commands to run");
	("Compilation Server",    ["--connect"],           [],                 Arg.Unit ignore, "<[host:]port>",      "connect on the given port and run commands there");
	("Compilation",           ["-C";"--cwd"],          [],                 Arg.Unit ignore, "<directory>",        "set current working directory");
	("Compilation",           ["--haxelib-global"],    [],                 Arg.Unit ignore, "",                   "pass --global argument to haxelib");
	("Compilation",           ["-w"],                  [],                 Arg.Unit ignore, "<warning list>",     "enable or disable specific warnings");
]

(** Pre-parse a flat string list into a [parsed_arg list].
    This does NOT expand hxml files (they become [HxmlFile] markers) and does
    NOT call out to haxelib (libraries become [AddLib] markers). Both are
    handled lazily when the request is later processed.

    The translation is mostly 1-to-1: one CLI flag group → one [parsed_arg].
    Compound expansions (e.g. adding [php.Boot] for [--php], setting
    [actx.interp] for [--interp]) are deferred to [process_args]. *)
let parse_args args =
	let parsed = DynArray.create () in
	let add a = DynArray.add parsed a in
	let rec loop = function
		| [] -> ()
		| ("--next" | "-next") :: rest ->
			add Next; loop rest
		| ("--each" | "-each") :: rest ->
			add Each; loop rest
		| ("--cwd" | "-C") :: dir :: rest ->
			add (Cwd dir); loop rest
		| ("--js" | "-js") :: file :: rest ->
			add (SetPlatform (Js, file)); loop rest
		| ("--lua" | "-lua") :: file :: rest ->
			add (SetPlatform (Lua, file)); loop rest
		| ("--swf" | "-swf") :: file :: rest ->
			add (SetPlatform (Flash, file)); loop rest
		| ("--neko" | "-neko") :: file :: rest ->
			add (SetPlatform (Neko, file)); loop rest
		| ("--php" | "-php") :: dir :: rest ->
			add (SetPlatform (Php, dir)); loop rest
		| ("--cpp" | "-cpp") :: dir :: rest ->
			add (SetPlatform (Cpp, dir)); loop rest
		| ("--cppia" | "-cppia") :: file :: rest ->
			add (SetCppiaTarget file); loop rest
		| ("--jvm" | "-jvm") :: file :: rest ->
			add (SetPlatform (Jvm, file));
			add (AddLib "hxjava");
			loop rest
		| ("--python" | "-python") :: dir :: rest ->
			add (SetPlatform (Python, dir)); loop rest
		| ("--hl" | "-hl") :: file :: rest ->
			add (SetPlatform (Hl, file)); loop rest
		| ("--custom-target" | "-custom") :: target :: rest ->
			let name, path = try ExtString.String.split target "=" with _ -> target, "" in
			add (SetCustomTarget (name, path)); loop rest
		| "-x" :: cl :: rest ->
			(* -x is non-terminal: subsequent args remain build args *)
			add (RunX cl); loop rest
		| "--interp" :: rest ->
			add Interp; loop rest
		| "--run" :: cl :: rest ->
			add (Run (cl, rest))
			(* --run consumes all remaining tokens as runtime args *)
		| ("--class-path" | "-p" | "-cp") :: path :: rest ->
			add (AddClassPath path); loop rest
		| "-libcp" :: path :: rest ->
			add (AddLibClassPath path); loop rest
		| ("--hxb-lib" | "-hxb-lib") :: file :: rest ->
			add (AddHxbLib file); loop rest
		| ("--main" | "-m" | "-main") :: cl :: rest ->
			let cpath = Path.parse_type_path cl in
			add (SetMain cpath); loop rest
		| ("--library" | "-L" | "-lib") :: name :: rest ->
			add (AddLib name); loop rest
		| ("--define" | "-D") :: var :: rest ->
			let flag, value = try let split = ExtString.String.split var "=" in (fst split, Some (snd split)) with _ -> var, None in
			add (Define (flag, value)); loop rest
		| "--undefine" :: var :: rest ->
			add (Undefine var); loop rest
		| ("--verbose" | "-v") :: rest ->
			add SetVerbose; loop rest
		| ("--debug" | "-debug") :: rest ->
			add SetDebug; loop rest
		| ("--version" | "-version") :: _ ->
			add ShowVersion
			(* consume remaining args - ShowVersion raises immediately in process_args *)
		| ("--help" | "-h" | "-help") :: _ ->
			add ShowHelp
		| "--help-defines" :: _ ->
			add ShowHelpDefines
		| "--help-metas" :: _ ->
			add ShowHelpMetas
		| "--help-user-defines" :: _ ->
			add ShowHelpUserDefines
		| "--help-user-metas" :: _ ->
			add ShowHelpUserMetas
		| ("--dce" | "-dce") :: mode :: rest ->
			add (SetDce mode); loop rest
		| ("--swf-version" | "-swf-version") :: v :: rest ->
			(try add (SetSwfVersion (float_of_string v)) with _ -> ());
			loop rest
		| ("--swf-lib" | "-swf-lib") :: file :: rest ->
			add (AddNativeLib (create_native_lib file false SwfLib)); loop rest
		| "--neko-lib-path" :: dir :: rest ->
			add (AddNekoLibPath dir); loop rest
		| ("--swf-lib-extern" | "-swf-lib-extern") :: file :: rest ->
			add (AddNativeLib (create_native_lib file true SwfLib)); loop rest
		| ("--java-lib" | "-java-lib") :: file :: rest ->
			add (AddNativeLib (create_native_lib file false JavaLib)); loop rest
		| "--java-lib-extern" :: file :: rest ->
			add (AddNativeLib (create_native_lib file true JavaLib)); loop rest
		| ("--resource" | "-r" | "-resource") :: res :: rest ->
			(match ExtString.String.nsplit res "@" with
			| [file; name] -> add (AddResource (file, name))
			| [file] -> add (AddResource (file, file))
			| _ -> ());
			loop rest
		| ("--prompt" | "-prompt") :: rest ->
			add SetPrompt; loop rest
		| ("--cmd" | "-cmd") :: cmd :: rest ->
			add (RunCmd (Helper.unquote cmd)); loop rest
		| "--display" :: input :: rest ->
			add (SetDisplayArg input); loop rest
		| ("--xml" | "-xml") :: file :: rest ->
			add (SetXmlOut file); loop rest
		| "--json" :: file :: rest ->
			add (SetJsonOut file); loop rest
		| "--hxb" :: file :: rest ->
			add (SetHxbOut file); loop rest
		| "--no-output" :: rest ->
			add SetNoOutput; loop rest
		| "--times" :: rest ->
			add SetMeasureTimes; loop rest
		| ("--remap" | "-remap") :: s :: rest ->
			(try
				let pack, target = ExtString.String.split s ":" in
				add (Remap (pack, target))
			with _ -> ());
			loop rest
		| "--custom-extension" :: ext :: rest ->
			add (SetCustomExtension ext); loop rest
		| ("--macro" | "-macro") :: e :: rest ->
			add (AddMacro e); loop rest
		| ("--server-listen" | "--wait") :: hp :: rest ->
			add (ServerListen hp); loop rest
		| "--server-connect" :: hp :: rest ->
			add (ServerConnect hp); loop rest
		| "--connect" :: hp :: rest ->
			add (Connect hp); loop rest
		| "--haxelib-global" :: rest ->
			add HaxelibGlobal; loop rest
		| "-w" :: s :: rest ->
			add (AddWarning s); loop rest
		| arg :: rest ->
			(match List.rev (ExtString.String.nsplit arg ".") with
			| "hxml" :: _ :: _ ->
				add (HxmlFile arg)
			| _ ->
				(try
					let path, name = Path.parse_path arg in
					if StringHelper.starts_uppercase_identifier name then
						add (AddClass (path, name))
					else
						add (IncludeModule arg)
				with Failure _ ->
					add (IncludeModule arg)));
			loop rest
	in
	loop args;
	DynArray.to_list parsed

(** Apply a single-part [parsed_arg list] to [com], returning the populated
    [arg_context].  Higher-level concerns ([Next], [Each], [AddLib] expansion,
    hxml expansion) are handled by [Compiler.HighLevel.process_params]. *)
let process_args (com : Common.context) =
	let actx = {
		classes = [([],"Std")];
		xml_out = None;
		hxb_out = None;
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
		hxb_libs = [];
		native_libs = [];
		raise_usage = (fun () -> ());
		display_arg = None;
		measure_times = false;
	} in
	let usage = Printf.sprintf
		"Haxe Compiler %s - (C)2005-2025 Haxe Foundation\nUsage: haxe%s <target> [options] [hxml files and dot paths...]\n"
		(s_version_full com.sctx.version) (if Sys.os_type = "Win32" then ".exe" else "")
	in
	let process_one arg = match arg with
		| SetPlatform (platform, file) ->
			(match platform with
			| Php ->
				(* --php also adds the php.Boot class *)
				actx.classes <- (["php"],"Boot") :: actx.classes
			| Jvm ->
				(* --jvm also sets the jvm_flag *)
				actx.jvm_flag <- true
			| _ -> ());
			set_platform com platform file
		| SetCppiaTarget file ->
			(* --cppia: enable cppia define and target cpp *)
			Common.define com Define.Cppia;
			set_platform com Cpp file
		| SetCustomTarget (name, path) ->
			set_custom_target com name path
		| AddClassPath path ->
			com.class_paths#add (new ClassPath.directory_class_path (Path.add_trailing_slash path) User)
		| AddLibClassPath path ->
			com.class_paths#add (new ClassPath.directory_class_path (Path.add_trailing_slash path) Lib)
		| AddHxbLib file ->
			actx.hxb_libs <- create_native_lib file false HxbLib :: actx.hxb_libs
		| SetMain cpath ->
			if com.main.main_path <> None then raise (Arg.Bad "Multiple --main classes specified");
			com.main.main_path <- Some cpath;
			(* --main also adds the class for compilation (matches old parse_args behaviour) *)
			actx.classes <- cpath :: actx.classes
		| AddLib _ | HaxelibGlobal ->
			(* handled at the process_params level *)
			()
		| Define (flag, value) ->
			(match value with
			| Some v -> Common.external_define_value com flag v
			| None -> Common.external_define com flag);
			(* --no-opt also disables the foptimize flag (mirroring parse_args behavior) *)
			if flag = "no-opt" then com.foptimize <- false
		| Undefine var ->
			Common.external_undefine com var
		| SetVerbose ->
			com.verbose <- true
		| SetDebug ->
			Common.define com Define.Debug;
			com.debug <- true
		| Interp ->
			(* --interp: define interp, set eval platform, mark actx.interp *)
			Common.define com Define.Interp;
			set_platform com Eval "";
			actx.interp <- true
		| Run _ | RunX _ ->
			(* --run / -x: handled at process_params level *)
			()
		| AddResource (file, name) ->
			let file = (try Common.find_file com file with Not_found -> file) in
			let data = (try
				let s = Std.input_file ~bin:true file in
				if String.length s > 12000000 then raise Exit;
				s
			with
			| Sys_error _ -> failwith ("Resource file not found: " ^ file)
			| _ -> failwith ("Resource '" ^ file ^ "' excess the maximum size of 12MB"))
			in
			if Hashtbl.mem com.resources name then failwith ("Duplicate resource name " ^ name);
			Hashtbl.add com.resources name data
		| RunCmd cmd ->
			actx.cmds <- cmd :: actx.cmds
		| SetSwfVersion v ->
			if not actx.swf_version || com.flash_version < v then com.flash_version <- v;
			actx.swf_version <- true
		| SetDce mode ->
			(match mode with
			| "std" | "full" | "no" -> ()
			| _ -> raise (Arg.Bad "Invalid DCE mode, expected std | full | no"));
			Common.define_value com Define.Dce mode
		| AddNativeLib lib ->
			actx.native_libs <- lib :: actx.native_libs
		| AddNekoLibPath dir ->
			com.neko_lib_paths <- dir :: com.neko_lib_paths
		| Remap (pack, target) ->
			com.package_rules <- PMap.add pack (Common.Remap target) com.package_rules
		| SetCustomExtension ext ->
			com.custom_ext <- Some ext
		| AddMacro e ->
			actx.force_typing <- true;
			actx.config_macros <- e :: actx.config_macros
		| SetDisplayArg input ->
			actx.display_arg <- Some input
		| SetXmlOut file ->
			actx.xml_out <- Some file
		| SetJsonOut file ->
			actx.json_out <- Some file
		| SetHxbOut file ->
			actx.hxb_out <- Some file
		| SetNoOutput ->
			actx.no_output <- true
		| SetMeasureTimes ->
			actx.measure_times <- true
		| AddWarning s ->
			let p = fake_pos ("-w " ^ s) in
			let l = Warning.parse_options s p in
			com.warning_options <- l :: com.warning_options
		| AddClass cpath ->
			actx.classes <- cpath :: actx.classes
		| IncludeModule cl ->
			actx.force_typing <- true;
			actx.config_macros <- (Printf.sprintf "include('%s', true, null, null, true)" cl) :: actx.config_macros
		| SetPrompt ->
			Helper.prompt := true
		| Cwd dir ->
			(* Re-apply chdir: process_params applies it eagerly for hxml resolution,
			   but entry restores the original cwd before execute_ctx, so we must
			   re-apply here to compile in the right directory. *)
			(try Unix.chdir dir with _ -> raise (Arg.Bad ("Invalid directory: " ^ dir)));
			actx.did_something <- true
		| HxmlFile _ ->
			(* hxml files should have been expanded before reaching process_args *)
			()
		| Next | Each | Expand _ ->
			(* batch directives handled at process_params level *)
			()
		| ServerListen _ | ServerConnect _ | Connect _ ->
			(* server modes handled at process_params level *)
			()
		| ShowVersion ->
			raise (Helper.HelpMessage (s_version_full com.sctx.version))
		| ShowHelp ->
			raise (Helper.HelpMessage (usage_string (basic_args_descriptions @ adv_args_descriptions) usage))
		| ShowHelpDefines ->
			let all, max_length = Define.get_documentation_list com.user_defines in
			let all = List.map (fun (n,doc) -> Printf.sprintf " %-*s: %s" max_length n (limit_string doc (max_length + 3))) all in
			raise (Helper.HelpMessage (ExtLib.String.join "\n" all))
		| ShowHelpMetas ->
			let all, max_length = Meta.get_documentation_list com.user_metas in
			let all = List.map (fun (n,doc) -> Printf.sprintf " %-*s: %s" max_length n (limit_string doc (max_length + 3))) all in
			raise (Helper.HelpMessage (ExtLib.String.join "\n" all))
		| ShowHelpUserDefines ->
			actx.did_something <- true;
			com.callbacks#add_after_init_macros (fun () ->
				let all, max_length = Define.get_user_documentation_list com.user_defines in
				let all = List.map (fun (n,doc) -> Printf.sprintf " %-*s: %s" max_length n (limit_string doc (max_length + 3))) all in
				raise (Helper.HelpMessage (ExtLib.String.join "\n" all))
			)
		| ShowHelpUserMetas ->
			actx.did_something <- true;
			com.callbacks#add_after_init_macros (fun () ->
				let all, max_length = Meta.get_user_documentation_list com.user_metas in
				let all = List.map (fun (n,doc) -> Printf.sprintf " %-*s: %s" max_length n (limit_string doc (max_length + 3))) all in
				raise (Helper.HelpMessage (ExtLib.String.join "\n" all))
			)
	in
	List.iter process_one com.parsed_args;
	if com.platform = Globals.Cpp && not (Define.defined com.defines DisableUnicodeStrings) && not (Define.defined com.defines HxcppSmartStings) then
		Define.define com.defines HxcppSmartStings;
	if Define.raw_defined com.defines "gen_hx_classes" then begin
		actx.force_typing <- true;
		actx.pre_compilation <- (fun() ->
			let process_lib lib =
				if not (lib#has_flag NativeLibraries.FlagIsStd) then
					List.iter (fun path -> if path <> (["java";"lang"],"String") then actx.classes <- path :: actx.classes) lib#list_modules
			in
			List.iter process_lib com.native_libs.swf_libs;
			List.iter process_lib com.native_libs.java_libs;
		) :: actx.pre_compilation;
		actx.xml_out <- Some "hx"
	end;
	actx.raise_usage <- (fun () ->
		raise (Helper.HelpMessage (usage_string basic_args_descriptions usage))
	);
	actx

(** Serialize a [parsed_arg list] back to CLI tokens for the [--connect] wire
    protocol.  Each variant is mapped to the string(s) a user would type.
    This is used to forward a client's arguments to a remote compilation server. *)
let to_raw_args (parsed_args : parsed_arg list) =
	let s_platform = function
		| Cross -> "cross" | Js -> "js" | Lua -> "lua" | Neko -> "neko"
		| Flash -> "swf" | Php -> "php" | Cpp -> "cpp"
		| Jvm -> "jvm" | Python -> "python" | Hl -> "hl" | Eval -> "eval"
		| CustomTarget name -> name
	in
	let s_path (p, n) = String.concat "." (p @ [n]) in
	let rec to_raw arg = match arg with
		| SetPlatform (platform, file) -> ["--" ^ s_platform platform; file]
		| SetCppiaTarget file -> ["--cppia"; file]
		| SetCustomTarget (name, path) ->
			["--custom-target"; if path = "" then name else name ^ "=" ^ path]
		| AddClassPath path -> ["-cp"; path]
		| AddLibClassPath path -> ["-libcp"; path]
		| AddHxbLib file -> ["--hxb-lib"; file]
		| SetMain p -> ["--main"; s_path p]
		| AddLib name -> ["-lib"; name]
		| HaxelibGlobal -> ["--haxelib-global"]
		| Define (flag, None) -> ["-D"; flag]
		| Define (flag, Some value) -> ["-D"; flag ^ "=" ^ value]
		| Undefine var -> ["--undefine"; var]
		| SetVerbose -> ["-v"]
		| SetDebug -> ["--debug"]
		| Interp -> ["--interp"]
		| Run (cl, _) -> ["--run"; cl]
		| RunX cl -> ["-x"; cl]
		| AddResource (file, name) ->
			if file = name then ["-r"; file] else ["-r"; file ^ "@" ^ name]
		| RunCmd cmd -> ["--cmd"; cmd]
		| SetSwfVersion v -> ["--swf-version"; string_of_float v]
		| SetDce mode -> ["--dce"; mode]
		| AddNativeLib {lib_file; lib_extern = false; lib_kind = SwfLib} -> ["--swf-lib"; lib_file]
		| AddNativeLib {lib_file; lib_extern = true; lib_kind = SwfLib} -> ["--swf-lib-extern"; lib_file]
		| AddNativeLib {lib_file; lib_extern = false; lib_kind = JavaLib} -> ["--java-lib"; lib_file]
		| AddNativeLib {lib_file; lib_extern = true; lib_kind = JavaLib} -> ["--java-lib-extern"; lib_file]
		| AddNativeLib _ -> []
		| AddNekoLibPath dir -> ["--neko-lib-path"; dir]
		| Remap (pack, target) -> ["--remap"; pack ^ ":" ^ target]
		| SetCustomExtension ext -> ["--custom-extension"; ext]
		| AddMacro e -> ["--macro"; e]
		| SetDisplayArg input -> ["--display"; input]
		| SetXmlOut file -> ["--xml"; file]
		| SetJsonOut file -> ["--json"; file]
		| SetHxbOut file -> ["--hxb"; file]
		| SetNoOutput -> ["--no-output"]
		| SetMeasureTimes -> ["--times"]
		| AddWarning s -> ["-w"; s]
		| AddClass p -> [s_path p]
		| IncludeModule cl -> [cl]
		| SetPrompt -> ["--prompt"]
		| Next -> ["--next"]
		| Each -> ["--each"]
		| Expand ex -> List.concat_map to_raw ex.original
		| ServerListen hp -> ["--wait"; hp]
		| ServerConnect hp -> ["--server-connect"; hp]
		| Connect hp -> ["--connect"; hp]
		| Cwd dir -> ["--cwd"; dir]
		| HxmlFile path -> [path]
		| ShowVersion -> ["--version"]
		| ShowHelp -> ["--help"]
		| ShowHelpDefines -> ["--help-defines"]
		| ShowHelpMetas -> ["--help-metas"]
		| ShowHelpUserDefines -> ["--help-user-defines"]
		| ShowHelpUserMetas -> ["--help-user-metas"]
	in
	List.concat_map to_raw parsed_args

type part_args = {
	args : parsed_arg list;
	runtime_args : string list;
}

type request_args = {
	parts : part_args list;
	server_mode : server_mode;
	connect_arg : string option;
	display_arg : string option;
}

let expand_args args =
	let each = ref [] in
	let parts = DynArray.create () in
	let server_mode = ref SMNone in
	let connect_arg = ref None in
	let display_arg = ref None in
	let hxml_stack = ref [] in
	let add_part args runtime_args =
		DynArray.add parts {args = !each @ args;runtime_args}
	in
	let rec find_subsequent_libs acc args = match args with
		| AddLib name :: args ->
			find_subsequent_libs (name :: acc) args
		| _ ->
			List.rev acc, args
	in
	let rec loop current = function
		| [] ->
			add_part (List.rev current) []
		| Next :: rest when current = [] ->
			(* reset hxml_stack so the same hxml can be included in a subsequent section *)
			hxml_stack := [];
			loop [] rest
		| Next :: rest ->
			add_part (List.rev current) [];
			(* reset hxml_stack so the same hxml can be included in a subsequent section *)
			hxml_stack := [];
			loop [] rest
		| Each :: rest ->
			each := List.rev current;
			loop [] rest
		| Cwd dir :: rest ->
			(try Unix.chdir dir with _ -> ());
			loop (Cwd dir :: current) rest
		| Connect hp :: rest ->
			connect_arg := Some hp;
			loop current rest
		| ServerConnect hp :: rest ->
			server_mode := SMConnect hp;
			loop current rest
		| ServerListen hp :: rest ->
			server_mode := SMListen hp;
			loop current rest
		| Run (cl, runtime_args) :: rest ->
			let cpath = Path.parse_type_path cl in
			let acc = Interp :: SetMain cpath :: current in
			let runtime_args = runtime_args @ (to_raw_args rest) in
			add_part (List.rev acc) runtime_args;
		| RunX cl :: rest ->
			let cpath = Path.parse_type_path cl in
			loop (Interp :: SetMain cpath :: current) rest
		| AddLib name :: rest ->
			let libs, rest = find_subsequent_libs [name] rest in
			let ex = {
				original = List.map (fun name -> AddLib name) libs;
				state = NotYetExpanded (AddLibs libs)
			} in
			loop (Expand ex :: current) rest
		| HxmlFile path :: rest ->
			let full_path = try Extc.get_full_path path with Failure(_) -> raise (Arg.Bad (Printf.sprintf "File not found: %s" path)) in
			if List.mem full_path !hxml_stack then
				raise (Arg.Bad (Printf.sprintf "Duplicate hxml inclusion: %s" full_path))
			else
				hxml_stack := full_path :: !hxml_stack;
			let expanded =
				try
					let raw = Helper.parse_hxml path in
					parse_args raw
				with Not_found ->
					[IncludeModule (path ^ " (file not found)")]
			in
			loop current (expanded @ rest)
		| SetDisplayArg s :: rest ->
			display_arg := Some s;
			loop (SetDisplayArg s :: current) rest
		| arg :: rest ->
			loop (arg :: current) rest
	in
	loop [] args;
	{
		parts = DynArray.to_list parts;
		server_mode = !server_mode;
		connect_arg = !connect_arg;
		display_arg = !display_arg;
	}

let string_of_request_args (req : request_args) : string =
	let quote s =
		(* quote arguments that contain whitespace for clearer output *)
		if String.contains s ' ' || String.contains s '\t' then Printf.sprintf "%S" s else s
	in
	let string_of_server_mode = function
		| SMNone -> "None"
		| SMListen hp -> Printf.sprintf "Listen %s" hp
		| SMConnect hp -> Printf.sprintf "Connect %s" hp
	in
	let format_part { args; runtime_args } =
		let raw = to_raw_args args in
		let args_str = String.concat " " (List.map quote raw) in
		let runtime_str = String.concat " " (List.map quote runtime_args) in
		match runtime_str with
		| "" -> args_str
		| _ -> args_str ^ " -- " ^ runtime_str
	in
	let parts_str =
		req.parts
		|> List.mapi (fun i p -> Printf.sprintf "  Part %d: %s" (i + 1) (format_part p))
		|> String.concat "\n"
	in
	String.concat "\n" [
		Printf.sprintf "server_mode: %s" (string_of_server_mode req.server_mode);
		Printf.sprintf "connect_arg: %s" (match req.connect_arg with Some s -> s | None -> "<none>");
		Printf.sprintf "display_arg: %s" (match req.display_arg with Some s -> s | None -> "<none>");
		"parts:";
		parts_str;
	]