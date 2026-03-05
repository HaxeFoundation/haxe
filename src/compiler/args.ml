open Globals
open Common
open CompilationContext

let columns = lazy (match Terminal_size.get_columns () with None -> 80 | Some c -> c)

let limit_string s offset =
	let rest = (Lazy.force columns) - offset in
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

(** Pre-parse a flat string list into a [parsed_arg list].
    This does NOT expand hxml files (they become [HxmlFile] markers) and does
    NOT call out to haxelib (libraries become [AddLib] markers). Both are
    handled lazily when the request is later processed.

    Each logical CLI argument group is preceded by a [RawArgs] marker carrying
    the original tokens, so that [to_raw_args] can reconstruct [com.args]. *)
let parse_args_new (_sctx : ServerCompilationContext.t) args =
	let parsed = DynArray.create () in
	let add a = DynArray.add parsed a in
	(* Emit raw token(s) then the parsed arg(s) for one CLI argument group. *)
	let raw raw = add (RawArgs raw) in
	let rec loop = function
		| [] -> ()
		| ("--next" | "-next") :: rest ->
			add Next; loop rest
		| ("--each" | "-each") :: rest ->
			add Each; loop rest
		| (("--cwd" | "-C") as f) :: dir :: rest ->
			raw [f; dir]; add (Cwd dir); loop rest
		| (("--js" | "-js") as f) :: file :: rest ->
			raw [f; file]; add (SetPlatform (Js, file)); loop rest
		| (("--lua" | "-lua") as f) :: file :: rest ->
			raw [f; file]; add (SetPlatform (Lua, file)); loop rest
		| (("--swf" | "-swf") as f) :: file :: rest ->
			raw [f; file]; add (SetPlatform (Flash, file)); loop rest
		| (("--neko" | "-neko") as f) :: file :: rest ->
			raw [f; file]; add (SetPlatform (Neko, file)); loop rest
		| (("--php" | "-php") as f) :: dir :: rest ->
			raw [f; dir]; add (AddClass (["php"], "Boot")); add (SetPlatform (Php, dir)); loop rest
		| (("--cpp" | "-cpp") as f) :: dir :: rest ->
			raw [f; dir]; add (SetPlatform (Cpp, dir)); loop rest
		| (("--cppia" | "-cppia") as f) :: file :: rest ->
			raw [f; file]; add (Define ("cppia", None)); add (SetPlatform (Cpp, file)); loop rest
		| (("--jvm" | "-jvm") as f) :: file :: rest ->
			raw [f; file]; add SetJvmFlag; add (SetPlatform (Jvm, file)); add (AddLib "hxjava"); loop rest
		| (("--python" | "-python") as f) :: dir :: rest ->
			raw [f; dir]; add (SetPlatform (Python, dir)); loop rest
		| (("--hl" | "-hl") as f) :: file :: rest ->
			raw [f; file]; add (SetPlatform (Hl, file)); loop rest
		| (("--custom-target" | "-custom") as f) :: target :: rest ->
			raw [f; target];
			let name, path = try ExtString.String.split target "=" with _ -> target, "" in
			add (SetCustomTarget (name, path)); loop rest
		| "-x" :: cl :: rest ->
			let cpath = Path.parse_type_path cl in
			raw ["-x"; cl];
			add (SetMain cpath); add (AddClass cpath); add (Define ("interp", None));
			add (SetPlatform (Eval, "")); add SetInterp;
			loop rest
		| "--interp" :: rest ->
			raw ["--interp"];
			add (Define ("interp", None)); add (SetPlatform (Eval, "")); add SetInterp;
			loop rest
		| "--run" :: cl :: rest ->
			let cpath = Path.parse_type_path cl in
			(* Use -x format for backward compat with Compiler.getArguments() *)
			raw ["-x"; cl];
			add (SetMain cpath); add (AddClass cpath); add (Define ("interp", None));
			add (SetPlatform (Eval, "")); add SetInterp; add (AddRuntimeArgs rest);
			(* --run consumes remaining args as runtime args *)
		| (("--class-path" | "-p" | "-cp") as f) :: path :: rest ->
			raw [f; path]; add (AddClassPath path); loop rest
		| "-libcp" :: path :: rest ->
			raw ["-libcp"; path]; add (AddLibClassPath path); loop rest
		| (("--hxb-lib" | "-hxb-lib") as f) :: file :: rest ->
			raw [f; file]; add (AddHxbLib file); loop rest
		| (("--main" | "-m" | "-main") as f) :: cl :: rest ->
			let cpath = Path.parse_type_path cl in
			raw [f; cl]; add (SetMain cpath); add (AddClass cpath); loop rest
		| (("--library" | "-L" | "-lib") as f) :: name :: rest ->
			raw [f; name]; add (AddLib name); loop rest
		| (("--define" | "-D") as f) :: var :: rest ->
			let flag, value = try let split = ExtString.String.split var "=" in (fst split, Some (snd split)) with _ -> var, None in
			raw [f; var]; add (Define (flag, value)); loop rest
		| "--undefine" :: var :: rest ->
			raw ["--undefine"; var]; add (Undefine var); loop rest
		| (("--verbose" | "-v") as f) :: rest ->
			raw [f]; add SetVerbose; loop rest
		| (("--debug" | "-debug") as f) :: rest ->
			raw [f]; add (Define ("debug", None)); add SetDebug; loop rest
		| ("--version" | "-version") :: _ ->
			add ShowVersion
			(* consume remaining args - ShowVersion raises immediately in process_args_new *)
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
		| (("--dce" | "-dce") as f) :: mode :: rest ->
			raw [f; mode]; add (SetDce mode); loop rest
		| (("--swf-version" | "-swf-version") as f) :: v :: rest ->
			raw [f; v]; (try add (SetSwfVersion (float_of_string v)) with _ -> ()); loop rest
		| ("--swf-header" | "-swf-header") :: h :: rest ->
			add (AddDeprecation "-swf-header has been deprecated, use -D swf-header instead");
			add (Define ("swf-header", Some h)); loop rest
		| "--flash-strict" :: rest ->
			add (AddDeprecation "--flash-strict has been deprecated, use -D flash-strict instead");
			add (Define ("flash-strict", None)); loop rest
		| (("--swf-lib" | "-swf-lib") as f) :: file :: rest ->
			raw [f; file]; add (AddNativeLib (create_native_lib file false SwfLib)); loop rest
		| "--neko-lib-path" :: dir :: rest ->
			raw ["--neko-lib-path"; dir]; add (AddNekoLibPath dir); loop rest
		| (("--swf-lib-extern" | "-swf-lib-extern") as f) :: file :: rest ->
			raw [f; file]; add (AddNativeLib (create_native_lib file true SwfLib)); loop rest
		| (("--java-lib" | "-java-lib") as f) :: file :: rest ->
			raw [f; file]; add (AddNativeLib (create_native_lib file false JavaLib)); loop rest
		| "--java-lib-extern" :: file :: rest ->
			raw ["--java-lib-extern"; file]; add (AddNativeLib (create_native_lib file true JavaLib)); loop rest
		| (("--resource" | "-r" | "-resource") as f) :: res :: rest ->
			raw [f; res];
			(match ExtString.String.nsplit res "@" with
			| [file; name] -> add (AddResource (file, name))
			| [file] -> add (AddResource (file, file))
			| _ -> ());
			loop rest
		| ("--prompt" | "-prompt") :: rest ->
			add SetPrompt; loop rest
		| (("--cmd" | "-cmd") as f) :: cmd :: rest ->
			raw [f; cmd]; add (RunCmd (Helper.unquote cmd)); loop rest
		| "--no-traces" :: rest ->
			add (AddDeprecation "--no-traces has been deprecated, use -D no-traces instead");
			add (Define ("no-traces", None)); loop rest
		| "--display" :: input :: rest ->
			raw ["--display"; input]; add (SetDisplayArg input); loop rest
		| (("--xml" | "-xml") as f) :: file :: rest ->
			raw [f; file]; add (SetXmlOut file); loop rest
		| "--json" :: file :: rest ->
			raw ["--json"; file]; add (SetJsonOut file); loop rest
		| "--hxb" :: file :: rest ->
			raw ["--hxb"; file]; add (SetHxbOut file); loop rest
		| "--no-output" :: rest ->
			raw ["--no-output"]; add SetNoOutput; loop rest
		| "--times" :: rest ->
			raw ["--times"]; add SetMeasureTimes; loop rest
		| "--no-inline" :: rest ->
			add (AddDeprecation "--no-inline has been deprecated, use -D no-inline instead");
			add (Define ("no-inline", None)); loop rest
		| "--no-opt" :: rest ->
			raw ["--no-opt"];
			add (AddDeprecation "--no-opt has been deprecated, use -D no-opt instead");
			add (Define ("no-opt", None)); add (Define ("no-opt-2", None)); loop rest
		| (("--remap" | "-remap") as f) :: s :: rest ->
			raw [f; s];
			(try
				let pack, target = ExtString.String.split s ":" in
				add (Remap (pack, target))
			with _ -> ());
			loop rest
		| "--custom-extension" :: ext :: rest ->
			raw ["--custom-extension"; ext]; add (SetCustomExtension ext); loop rest
		| (("--macro" | "-macro") as f) :: e :: rest ->
			raw [f; e]; add (AddMacro e); loop rest
		| (("--server-listen" | "--wait") as f) :: hp :: rest ->
			raw [f; hp]; add (ServerListen hp); loop rest
		| "--server-connect" :: hp :: rest ->
			raw ["--server-connect"; hp]; add (ServerConnect hp); loop rest
		| "--connect" :: hp :: rest ->
			raw ["--connect"; hp]; add (Connect hp); loop rest
		| "--haxelib-global" :: rest ->
			raw ["--haxelib-global"]; add HaxelibGlobal; loop rest
		| "-w" :: s :: rest ->
			raw ["-w"; s]; add (AddWarning s); loop rest
		| arg :: rest ->
			(match List.rev (ExtString.String.nsplit arg ".") with
			| "hxml" :: _ :: _ ->
				raw [arg]; add (HxmlFile arg)
			| _ ->
				raw [arg];
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
	let args = match List.rev args with
		| file :: "--display" :: pl when file <> "memory" ->
			"--display" :: file :: List.rev pl
		| _ ->
			args
	in
	loop args;
	DynArray.to_list parsed

(** Apply a single-part [parsed_arg list] to [com], returning the populated
    [arg_context].  Higher-level concerns ([Next], [Each], [AddLib] expansion,
    hxml expansion) are handled by [Compiler.HighLevel.process_params]. *)
let process_args_new (com : Common.context) (parsed_args : parsed_arg list) =
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
		deprecations = [];
		measure_times = false;
	} in
	let usage = Printf.sprintf
		"Haxe Compiler %s - (C)2005-2025 Haxe Foundation\nUsage: haxe%s <target> [options] [hxml files and dot paths...]\n"
		(s_version_full com.sctx.version) (if Sys.os_type = "Win32" then ".exe" else "")
	in
	let process_one arg = match arg with
		| SetPlatform (platform, file) ->
			set_platform com platform file
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
			com.main.main_path <- Some cpath
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
		| SetInterp ->
			actx.interp <- true
		| SetJvmFlag ->
			actx.jvm_flag <- true
		| AddRuntimeArgs _ ->
			(* runtime args are set on compilation_context in process_params *)
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
		| AddDeprecation s ->
			actx.deprecations <- s :: actx.deprecations
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
			(* hxml files should have been expanded before reaching process_args_new *)
			()
		| Next | Each ->
			(* batch directives handled at process_params level *)
			()
		| ServerListen _ | ServerConnect _ | Connect _ ->
			(* server modes handled at process_params level *)
			()
		| RawArgs _ ->
			(* raw token carrier for com.args reconstruction - no action needed *)
			()
		| ShowVersion ->
			raise (Helper.HelpMessage (s_version_full com.sctx.version))
		| ShowHelp ->
			raise (Helper.HelpMessage usage)
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
	List.iter process_one parsed_args;
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
		raise (Helper.HelpMessage usage)
	);
	actx

(** Convert a [parsed_arg list] back to the original CLI tokens.
    Each [RawArgs] marker in the list carries the original tokens for its group;
    all other variants are ignored.  This gives a faithful reconstruction of
    [com.args] from the per-batch parsed_arg list. *)
let to_raw_args (parsed_args : parsed_arg list) =
	List.concat_map (fun arg -> match arg with RawArgs raw -> raw | _ -> []) parsed_args

let parse_args (com : Common.context) =
	let usage = Printf.sprintf
		"Haxe Compiler %s - (C)2005-2025 Haxe Foundation\nUsage: haxe%s <target> [options] [hxml files and dot paths...]\n"
		(s_version_full com.sctx.version) (if Sys.os_type = "Win32" then ".exe" else "")
	in
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
		deprecations = [];
		measure_times = false;
	} in
	let add_deprecation s =
		actx.deprecations <- s :: actx.deprecations
	in
	let add_native_lib file extern kind =
		let lib = create_native_lib file extern kind in
		actx.native_libs <- lib :: actx.native_libs
	in
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
		("Target",["--jvm"],["-jvm"],Arg.String (fun dir ->
			actx.jvm_flag <- true;
			set_platform com Jvm dir;
		),"<file>","generate JVM bytecode into target file");
		("Target",["--python"],["-python"],Arg.String (fun dir ->
			set_platform com Python dir;
		),"<file>","generate Python code into target file");
		("Target",["--hl"],["-hl"],Arg.String (fun file ->
			set_platform com Hl file;
		),"<file>","generate HashLink .hl bytecode or .c code into target file");
		("Target",["--custom-target"],["-custom"],Arg.String (fun target ->
			let name, path = try let split = ExtString.String.split target "=" in split with _ -> target, "" in
			set_custom_target com name path;
		),"<name[=path]>","generate code for a custom target");
		("Target",[],["-x"], Arg.String (fun cl ->
			let cpath = Path.parse_type_path cl in
			(match com.main.main_path with
				| Some c -> if cpath <> c then raise (Arg.Bad "Multiple --main classes specified")
				| None -> com.main.main_path <- Some cpath);
			actx.classes <- cpath :: actx.classes;
			Common.define com Define.Interp;
			set_platform com Eval "";
			actx.interp <- true;
		),"<class>","interpret the program using internal macro system");
		("Target",["--interp"],[], Arg.Unit (fun() ->
			Common.define com Define.Interp;
			set_platform com Eval "";
			actx.interp <- true;
		),"","interpret the program using internal macro system");
		("Target",["--run"],[], Arg.Unit (fun() ->
			raise (Arg.Bad "--run requires an argument: a Haxe module name")
		), "<module> [args...]","interpret a Haxe module with command line arguments");
		("Compilation",["-p";"--class-path"],["-cp"],Arg.String (fun path ->
			com.class_paths#add (new ClassPath.directory_class_path (Path.add_trailing_slash path) User);
		),"<path>","add a directory to find source files");
		("Compilation",[],["-libcp"],Arg.String (fun path ->
			com.class_paths#add (new ClassPath.directory_class_path (Path.add_trailing_slash path) Lib);
		),"<path>","add a directory to find source files");
		("Compilation",["--hxb-lib"],["-hxb-lib"],Arg.String (fun file ->
			let lib = create_native_lib file false HxbLib in
			actx.hxb_libs <- lib :: actx.hxb_libs
		),"<path>","add a hxb library");
		("Compilation",["-m";"--main"],["-main"],Arg.String (fun cl ->
			if com.main.main_path <> None then raise (Arg.Bad "Multiple --main classes specified");
			let cpath = Path.parse_type_path cl in
			com.main.main_path <- Some cpath;
			actx.classes <- cpath :: actx.classes
		),"<class>","select startup class");
		("Compilation",["-L";"--library"],["-lib"],Arg.String (fun _ -> ()),"<name[:ver]>","use a haxelib library");
		("Compilation",["-D";"--define"],[],Arg.String (fun var ->
			let flag, value = try let split = ExtString.String.split var "=" in (fst split, Some (snd split)) with _ -> var, None in
			match value with
				| Some value -> Common.external_define_value com flag value
				| None -> Common.external_define com flag;
		),"<var[=value]>","define a conditional compilation flag");
		("Compilation",["--undefine"],[],Arg.String (fun var ->
			Common.external_undefine com var
		),"","remove a conditional compilation flag");
		("Debug",["-v";"--verbose"],[],Arg.Unit (fun () ->
			com.verbose <- true
		),"","turn on verbose mode");
		("Debug",["--debug"],["-debug"], Arg.Unit (fun() ->
			Common.define com Define.Debug;
			com.debug <- true;
		),"","add debug information to the compiled code");
		("Miscellaneous",["--version"],["-version"],Arg.Unit (fun() ->
			raise (Helper.HelpMessage (s_version_full com.sctx.version));
		),"","print version and exit");
		("Miscellaneous", ["-h";"--help"], ["-help"], Arg.Unit (fun () ->
			raise (Arg.Help "")
		),"","show extended help information");
		("Miscellaneous",["--help-defines"],[], Arg.Unit (fun() ->
			let all,max_length = Define.get_documentation_list com.user_defines in
			let all = List.map (fun (n,doc) -> Printf.sprintf " %-*s: %s" max_length n (limit_string doc (max_length + 3))) all in
			raise (Helper.HelpMessage (ExtLib.String.join "\n" all));
		),"","print help for all compiler specific defines");
		("Miscellaneous",["--help-user-defines"],[], Arg.Unit (fun() ->
			actx.did_something <- true;
			com.callbacks#add_after_init_macros (fun() ->
				let all,max_length = Define.get_user_documentation_list com.user_defines in
				let all = List.map (fun (n,doc) -> Printf.sprintf " %-*s: %s" max_length n (limit_string doc (max_length + 3))) all in
				raise (Helper.HelpMessage (ExtLib.String.join "\n" all));
			)
		),"","print help for all user defines");
		("Miscellaneous",["--help-metas"],[], Arg.Unit (fun() ->
			let all,max_length = Meta.get_documentation_list com.user_metas in
			let all = List.map (fun (n,doc) -> Printf.sprintf " %-*s: %s" max_length n (limit_string doc (max_length + 3))) all in
			raise (Helper.HelpMessage (ExtLib.String.join "\n" all));
		),"","print help for all compiler metadatas");
		("Miscellaneous",["--help-user-metas"],[], Arg.Unit (fun() ->
			actx.did_something <- true;
			com.callbacks#add_after_init_macros (fun() ->
				let all,max_length = Meta.get_user_documentation_list com.user_metas in
				let all = List.map (fun (n,doc) -> Printf.sprintf " %-*s: %s" max_length n (limit_string doc (max_length + 3))) all in
				raise (Helper.HelpMessage (ExtLib.String.join "\n" all));
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
			add_native_lib file false SwfLib;
		),"<file>","add the SWF library to the compiled SWF");
		("Target-specific",[],["--neko-lib-path"],Arg.String (fun dir ->
			com.neko_lib_paths <- dir :: com.neko_lib_paths
		),"<directory>","add the neko library path");
		("Target-specific",["--swf-lib-extern"],["-swf-lib-extern"],Arg.String (fun file ->
			add_native_lib file true SwfLib;
		),"<file>","use the SWF library for type checking");
		("Target-specific",["--java-lib"],["-java-lib"],Arg.String (fun file ->
			add_native_lib file false JavaLib;
		),"<file>","add an external JAR or directory of JAR files");
		("Target-specific",["--java-lib-extern"],[],Arg.String (fun file ->
			add_native_lib file true JavaLib;
		),"<file>","use an external JAR or directory of JAR files for type checking");
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
		("Services",["--hxb"],[], Arg.String (fun file ->
			actx.hxb_out <- Some file;
		),"<file>", "generate haxe binary representation to target archive");
		("Optimization",["--no-output"],[], Arg.Unit (fun() -> actx.no_output <- true),"","compiles but does not generate any file");
		("Debug",["--times"],[], Arg.Unit (fun() ->
			actx.measure_times <- true
		),"","measure compilation times");
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
			com.package_rules <- PMap.add pack (Common.Remap target) com.package_rules;
		),"<package:target>","remap a package to another one");
		("Compilation",["--custom-extension"],[],Arg.String (fun ext ->
			com.custom_ext <- Some ext;
		),"<extension>","custom extension used to shadow modules with Module.custom.hx files");
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
			let p = fake_pos ("-w " ^ s) in
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
			Arg.parse_argv ~current (Array.of_list ("Haxe" :: args)) all_args_spec args_callback "";
		with
		| Arg.Help _ ->
			raise (Helper.HelpMessage (usage_string all_args usage))
		| Arg.Bad msg ->
			(* Strip error prefix added by ocaml's arg parser *)
			let msg = if ExtLib.String.starts_with msg "Haxe: " then (String.sub msg 6 ((String.length msg) - 6)) else msg in
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