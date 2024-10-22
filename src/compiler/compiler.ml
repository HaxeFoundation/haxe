open Globals
open Common
open CompilationContext

let run_or_diagnose ctx f =
	let com = ctx.com in
	let handle_diagnostics msg p kind =
		ctx.has_error <- true;
		add_diagnostics_message com msg p kind Error;
		match com.report_mode with
		| RMLegacyDiagnostics _ -> DisplayOutput.emit_legacy_diagnostics ctx.com
		| RMDiagnostics _ -> DisplayOutput.emit_diagnostics ctx.com
		| _ -> die "" __LOC__
	in
	if is_diagnostics com then begin try
			f ()
		with
		| Error.Error err ->
			ctx.has_error <- true;
			Error.recurse_error (fun depth err ->
				add_diagnostics_message ~depth com (Error.error_msg err.err_message) err.err_pos DKCompilerMessage Error
			) err;
			(match com.report_mode with
			| RMLegacyDiagnostics _ -> DisplayOutput.emit_legacy_diagnostics ctx.com
			| RMDiagnostics _ -> DisplayOutput.emit_diagnostics ctx.com
			| _ -> die "" __LOC__)
		| Parser.Error(msg,p) ->
			handle_diagnostics (Parser.error_msg msg) p DKParserError
		| Lexer.Error(msg,p) ->
			handle_diagnostics (Lexer.error_msg msg) p DKParserError
		end
	else
		f ()

let run_command ctx cmd =
	let t = Timer.timer ["command";cmd] in
	(* TODO: this is a hack *)
	let cmd = if ctx.comm.is_server then begin
		let h = Hashtbl.create 0 in
		Hashtbl.add h "__file__" ctx.com.file;
		Hashtbl.add h "__platform__" (platform_name ctx.com.platform);
		Helper.expand_env ~h:(Some h) cmd
	end else
		cmd
	in
	let len = String.length cmd in
	let result =
		if len > 3 && String.sub cmd 0 3 = "cd " then begin
			Sys.chdir (String.sub cmd 3 (len - 3));
			0
		(* Emit stderr as a server message in server mode *)
		end else begin
			let pout, pin, perr = Unix.open_process_full cmd (Unix.environment()) in
			let bout = Bytes.create 1024 in
			let berr = Bytes.create 1024 in
			let rec read_content channel buf f =
				begin try
					let i = input channel buf 0 1024 in
					if i > 0 then begin
						f (Bytes.unsafe_to_string (Bytes.sub buf 0 i));
						read_content channel buf f
					end
				with Unix.Unix_error _ ->
					()
				end
			in
			let tout = Thread.create (fun() -> read_content pout bout ctx.comm.write_out) () in
			let terr = Thread.create (fun() -> read_content perr berr ctx.comm.write_err) () in
			Thread.join tout;
			Thread.join terr;
			let result = (match Unix.close_process_full (pout,pin,perr) with Unix.WEXITED c | Unix.WSIGNALED c | Unix.WSTOPPED c -> c) in
			result
		end
	in
	t();
	result

module Setup = struct
	let initialize_target ctx com actx =
		init_platform com;
		com.class_paths#lock_context (platform_name com.platform) false;
		let add_std dir =
			com.class_paths#modify_inplace (fun cp -> match cp#scope with
				| Std ->
					let cp' = new ClassPath.directory_class_path (cp#path ^ dir ^ "/_std/") StdTarget in
					cp :: [cp']
				| _ ->
					[cp]
			);
		in
		match com.platform with
			| Cross ->
				"?"
			| CustomTarget name ->
				name
			| Flash ->
				let rec loop = function
					| [] -> ()
					| (v,_) :: _ when v > com.flash_version -> ()
					| (v,def) :: l ->
						Common.raw_define com ("flash" ^ def);
						loop l
				in
				loop Common.flash_versions;
				com.package_rules <- PMap.remove "flash" com.package_rules;
				add_std "flash";
				"swf"
			| Neko ->
				add_std "neko";
				"n"
			| Js ->
				let es_version =
					try
						int_of_string (Common.defined_value com Define.JsEs)
					with
					| Not_found ->
						(Common.define_value com Define.JsEs "5"; 5)
					| _ ->
						0
				in

				if es_version < 3 || es_version = 4 then (* we don't support ancient and there's no 4th *)
					failwith "Invalid -D js-es value";

				if es_version >= 5 then Common.raw_define com "js_es5"; (* backward-compatibility *)

				add_std "js";
				"js"
			| Lua ->
				add_std "lua";
				"lua"
			| Php ->
				add_std "php";
				"php"
			| Cpp ->
				Common.define_value com Define.HxcppApiLevel "430";
				add_std "cpp";
				if Common.defined com Define.Cppia then
					actx.classes <- (Path.parse_path "cpp.cppia.HostClasses" ) :: actx.classes;
				"cpp"
			| Jvm ->
				add_std "jvm";
				com.package_rules <- PMap.remove "java" com.package_rules;
				add_std "java";
				"java"
			| Python ->
				add_std "python";
				if not (Common.defined com Define.PythonVersion) then
					Common.define_value com Define.PythonVersion "3.3";
				"python"
			| Hl ->
				add_std "hl";
				if not (Common.defined com Define.HlVer) then begin
					let hl_ver = try
						Std.input_file (Common.find_file com "hl/hl_version")
					with Not_found ->
						failwith "The file hl_version could not be found. Please make sure HAXE_STD_PATH is set to the standard library corresponding to the used compiler version."
					in
					Define.define_value com.defines Define.HlVer hl_ver
				end;
				"hl"
			| Eval ->
				add_std "eval";
				"eval"

	let init_native_libs com native_libs =
		(* Native lib pass 1: Register *)
		let fl = List.map (fun lib -> NativeLibraryHandler.add_native_lib com lib) (List.rev native_libs) in
		(* Native lib pass 2: Initialize *)
		List.iter (fun f -> f()) fl

	let create_typer_context ctx macros =
		let com = ctx.com in
		let buffer = Buffer.create 64 in
		Buffer.add_string buffer "Defines: ";
		PMap.iter (fun k v -> match v with
			| "1" -> Printf.bprintf buffer "%s;" k
			| _ -> Printf.bprintf buffer "%s=%s;" k v
		) com.defines.values;
		Buffer.truncate buffer (Buffer.length buffer - 1);
		Common.log com (Buffer.contents buffer);
		com.callbacks#run com.error_ext com.callbacks#get_before_typer_create;
		TyperEntry.create com macros

	let executable_path() =
		Extc.executable_path()

	open ClassPath

	let get_std_class_paths () =
		try
			let p = Sys.getenv "HAXE_STD_PATH" in
			let p = Path.remove_trailing_slash p in
			let rec loop = function
				| drive :: path :: l ->
					if String.length drive = 1 && ((drive.[0] >= 'a' && drive.[0] <= 'z') || (drive.[0] >= 'A' && drive.[0] <= 'Z')) then
						(drive ^ ":" ^ path) :: loop l
					else
						drive :: loop (path :: l)
				| l ->
					l
			in
			let parts = Str.split_delim (Str.regexp "[;:]") p in
			List.map (fun s -> s,Std) (loop parts)
		with Not_found ->
			let base_path = Path.get_real_path (try executable_path() with _ -> "./") in
			if Sys.os_type = "Unix" then
				let prefix_path = Filename.dirname base_path in
				let lib_path = Filename.concat prefix_path "lib" in
				let share_path = Filename.concat prefix_path "share" in
				[
					(Filename.concat share_path "haxe/std"),Std;
					(Filename.concat lib_path "haxe/std"),Std;
					(Filename.concat base_path "std"),Std;
				]
			else
				[
					(Filename.concat base_path "std"),Std;
				]

	let init_std_class_paths com =
		List.iter (fun (s,scope) ->
			try if Sys.is_directory s then
				let cp = new ClassPath.directory_class_path (Path.add_trailing_slash s) scope in
				com.class_paths#add cp
			with Sys_error _ -> ()
		) (List.rev (get_std_class_paths ()));
		com.class_paths#add com.empty_class_path

	let setup_common_context ctx =
		let com = ctx.com in
		ctx.com.print <- ctx.comm.write_out;
		Common.define_value com Define.HaxeVer (Printf.sprintf "%.3f" (float_of_int Globals.version /. 1000.));
		Common.raw_define com "haxe3";
		Common.raw_define com "haxe4";
		Common.raw_define com "haxe5";
		Common.define_value com Define.Haxe s_version;
		Common.raw_define com "true";
		Common.define_value com Define.Dce "std";
		com.info <- (fun ?(depth=0) ?(from_macro=false) msg p ->
			message ctx (make_compiler_message ~from_macro msg p depth DKCompilerMessage Information)
		);
		com.warning <- (fun ?(depth=0) ?(from_macro=false) w options msg p ->
			match Warning.get_mode w (com.warning_options @ options) with
			| WMEnable ->
				let wobj = Warning.warning_obj w in
				let msg = if wobj.w_generic then
					msg
				else
					Printf.sprintf "(%s) %s" wobj.w_name msg
				in
				message ctx (make_compiler_message ~from_macro msg p depth DKCompilerMessage Warning)
			| WMDisable ->
				()
		);
		com.error_ext <- error_ext ctx;
		com.error <- (fun ?(depth = 0) msg p -> com.error_ext (Error.make_error ~depth (Custom msg) p));
		let filter_messages = (fun keep_errors predicate -> (List.filter (fun cm ->
			(match cm.cm_severity with
			| MessageSeverity.Error -> keep_errors;
			| Information | Warning | Hint -> predicate cm;)
		) (List.rev ctx.messages))) in
		com.get_messages <- (fun () -> (List.map (fun cm ->
			(match cm.cm_severity with
			| MessageSeverity.Error -> die "" __LOC__;
			| Information | Warning | Hint -> cm;)
		) (filter_messages false (fun _ -> true))));
		com.filter_messages <- (fun predicate -> (ctx.messages <- (List.rev (filter_messages true predicate))));
		com.run_command <- run_command ctx;
		init_std_class_paths com

end

let check_defines com =
	if is_next com then begin
		PMap.iter (fun k _ ->
			try
				let reason = Hashtbl.find Define.deprecation_lut k in
				let p = fake_pos ("-D " ^ k) in
				com.warning WDeprecatedDefine [] reason p
			with Not_found ->
				()
		) com.defines.values
	end

(** Creates the typer context and types [classes] into it. *)
let do_type ctx mctx actx display_file_dot_path =
	let com = ctx.com in
	let t = Timer.timer ["typing"] in
	let cs = com.cs in
	CommonCache.maybe_add_context_sign cs com "before_init_macros";
	enter_stage com CInitMacrosStart;
	ServerMessage.compiler_stage com;
	Setup.init_native_libs com actx.hxb_libs;
	let mctx = List.fold_left (fun mctx path ->
		Some (MacroContext.call_init_macro ctx.com mctx path)
	) mctx (List.rev actx.config_macros) in
	enter_stage com CInitMacrosDone;
	ServerMessage.compiler_stage com;

	let macros = match mctx with None -> None | Some mctx -> mctx.g.macros in
	Setup.init_native_libs com actx.native_libs;
	let tctx = Setup.create_typer_context ctx macros in
	let display_file_dot_path = DisplayProcessing.maybe_load_display_file_before_typing tctx display_file_dot_path in
	check_defines ctx.com;
	CommonCache.lock_signature com "after_init_macros";
	Option.may (fun mctx -> MacroContext.finalize_macro_api tctx mctx) mctx;
	(try begin
		com.callbacks#run com.error_ext com.callbacks#get_after_init_macros;
		run_or_diagnose ctx (fun () ->
			if com.display.dms_kind <> DMNone then DisplayTexpr.check_display_file tctx cs;
			List.iter (fun cpath -> ignore(tctx.Typecore.g.Typecore.do_load_module tctx cpath null_pos)) (List.rev actx.classes);
			Finalization.finalize tctx;
		);
	end with TypeloadParse.DisplayInMacroBlock ->
		ignore(DisplayProcessing.load_display_module_in_macro tctx display_file_dot_path true)
	);
	enter_stage com CTypingDone;
	ServerMessage.compiler_stage com;
	(* If we are trying to find references, let's syntax-explore everything we know to check for the
		identifier we are interested in. We then type only those modules that contain the identifier. *)
	begin match com.display.dms_kind with
		| (DMUsage _ | DMImplementation) -> FindReferences.find_possible_references tctx cs;
		| _ -> ()
	end;
	t();
	(tctx, display_file_dot_path)

let finalize_typing ctx tctx =
	let t = Timer.timer ["finalize"] in
	let com = ctx.com in
	enter_stage com CFilteringStart;
	ServerMessage.compiler_stage com;
	let main, types, modules = run_or_diagnose ctx (fun () -> Finalization.generate tctx) in
	com.main.main_expr <- main;
	com.types <- types;
	com.modules <- modules;
	t()

let filter ctx tctx before_destruction =
	let t = Timer.timer ["filters"] in
	DeprecationCheck.run ctx.com;
	run_or_diagnose ctx (fun () -> Filters.run tctx ctx.com.main.main_expr before_destruction);
	t()

let compile ctx actx callbacks =
	let com = ctx.com in
	(* Set up display configuration *)
	DisplayProcessing.process_display_configuration ctx;
	let restore = disable_report_mode com in
	let display_file_dot_path = DisplayProcessing.process_display_file com actx in
	restore ();
	let mctx = match com.platform with
		| CustomTarget name ->
			begin try
				Some (MacroContext.call_init_macro com None (Printf.sprintf "%s.Init.init()" name))
			with (Error.Error { err_message = Module_not_found ([pack],"Init") }) when pack = name ->
				(* ignore if <target_name>.Init doesn't exist *)
				None
			end
		| _ ->
			None
		in
	(* Initialize target: This allows access to the appropriate std packages and sets the -D defines. *)
	let ext = Setup.initialize_target ctx com actx in
	update_platform_config com; (* make sure to adapt all flags changes defined after platform *)
	callbacks.after_target_init ctx;
	let t = Timer.timer ["init"] in
	List.iter (fun f -> f()) (List.rev (actx.pre_compilation));
	begin match actx.hxb_out with
		| None ->
			()
		| Some file ->
			com.hxb_writer_config <- HxbWriterConfig.process_argument file
	end;
	t();
	enter_stage com CInitialized;
	ServerMessage.compiler_stage com;
	if actx.classes = [([],"Std")] && not actx.force_typing then begin
		if actx.cmds = [] && not actx.did_something then actx.raise_usage();
	end else begin
		(* Actual compilation starts here *)
		let (tctx,display_file_dot_path) = do_type ctx mctx actx display_file_dot_path in
		DisplayProcessing.handle_display_after_typing ctx tctx display_file_dot_path;
		finalize_typing ctx tctx;
		let is_compilation = is_compilation com in
		com.callbacks#add_after_save (fun () ->
			callbacks.after_save ctx;
			if is_compilation then match com.hxb_writer_config with
				| Some config ->
					Generate.check_hxb_output ctx config;
				| None ->
					()
		);
		if is_diagnostics com then
			filter ctx tctx (fun () -> DisplayProcessing.handle_display_after_finalization ctx tctx display_file_dot_path)
		else begin
			DisplayProcessing.handle_display_after_finalization ctx tctx display_file_dot_path;
			filter ctx tctx (fun () -> ());
		end;
		if ctx.has_error then raise Abort;
		if is_compilation then Generate.check_auxiliary_output com actx;
		enter_stage com CGenerationStart;
		ServerMessage.compiler_stage com;
		Generate.maybe_generate_dump ctx tctx;
		if not actx.no_output then Generate.generate ctx tctx ext actx;
		enter_stage com CGenerationDone;
		ServerMessage.compiler_stage com;
	end;
	Sys.catch_break false;
	com.callbacks#run com.error_ext com.callbacks#get_after_generation;
	if not actx.no_output then begin
		List.iter (fun c ->
			let r = run_command ctx c in
			if r <> 0 then failwith ("Command failed with error " ^ string_of_int r)
		) (List.rev actx.cmds)
	end

let compile_safe ctx f =
	let com = ctx.com in
try
	f ()
with
	| Error.Fatal_error err ->
		error_ext ctx err
	| Lexer.Error (m,p) ->
		error ctx (Lexer.error_msg m) p
	| Parser.Error (m,p) ->
		error ctx (Parser.error_msg m) p
	| Typecore.Forbid_package ((pack,m,p),pl,pf)  ->
		if !Parser.display_mode <> DMNone && ctx.has_next then begin
			ctx.has_error <- false;
			ctx.messages <- [];
		end else begin
			let sub = List.map (fun p -> Error.make_error ~depth:1 (Error.Custom (Error.compl_msg "referenced here")) p) pl in
			error_ext ctx (Error.make_error (Error.Custom (Printf.sprintf "You cannot access the %s package while %s (for %s)" pack (if pf = "macro" then "in a macro" else "targeting " ^ pf) (s_type_path m))) ~sub p)
		end
	| Error.Error err ->
		error_ext ctx err
	| Arg.Bad msg ->
		error ctx ("Error: " ^ msg) null_pos
	| Failure msg when not Helper.is_debug_run ->
		error ctx ("Error: " ^ msg) null_pos
	| Helper.HelpMessage msg ->
		print_endline msg
	| Parser.TypePath (p,c,is_import,pos) ->
		DisplayOutput.handle_type_path_exception ctx p c is_import pos
	| Parser.SyntaxCompletion(kind,subj) ->
		DisplayOutput.handle_syntax_completion com kind subj;
		error ctx ("Error: No completion point was found") null_pos
	| DisplayException.DisplayException dex ->
		DisplayOutput.handle_display_exception ctx dex
	| Abort | Out_of_memory | EvalTypes.Sys_exit _ | Hlinterp.Sys_exit _ | DisplayProcessingGlobals.Completion _ as exc ->
		(* We don't want these to be caught by the catchall below *)
		raise exc
	| e when (try Sys.getenv "OCAMLRUNPARAM" <> "b" with _ -> true) && not Helper.is_debug_run ->
		error ctx (Printexc.to_string e) null_pos

let compile_safe ctx f =
	try compile_safe ctx f with Abort -> ()

let finalize ctx =
	ctx.comm.flush ctx;
	List.iter (fun lib -> lib#close) ctx.com.hxb_libs;
	(* In server mode any open libs are closed by the lib_build_task. In offline mode
		we should do it here to be safe. *)
	if not ctx.comm.is_server then begin
		List.iter (fun lib -> lib#close) ctx.com.native_libs.java_libs;
		List.iter (fun lib -> lib#close) ctx.com.native_libs.swf_libs;
	end

let catch_completion_and_exit ctx callbacks run =
	try
		run ctx;
		if ctx.has_error then 1 else 0
	with
		| DisplayProcessingGlobals.Completion str ->
			callbacks.after_compilation ctx;
			ServerMessage.completion str;
			ctx.comm.write_err str;
			0
		| EvalTypes.Sys_exit i | Hlinterp.Sys_exit i ->
			if i <> 0 then ctx.has_error <- true;
			finalize ctx;
			i

let process_actx ctx actx =
	DisplayProcessing.process_display_arg ctx actx;
	List.iter (fun s ->
		ctx.com.warning WDeprecated [] s null_pos
	) actx.deprecations;
	if defined ctx.com NoDeprecationWarnings then begin
		ctx.com.warning_options <- [{wo_warning = WDeprecated; wo_mode = WMDisable}] :: ctx.com.warning_options
	end

let compile_ctx callbacks ctx =
	let run ctx =
		callbacks.before_anything ctx;
		Setup.setup_common_context ctx;
		compile_safe ctx (fun () ->
			let actx = Args.parse_args ctx.com in
			process_actx ctx actx;
			compile ctx actx callbacks;
		);
		finalize ctx;
		callbacks.after_compilation ctx;
	in
	if ctx.has_error then begin
		finalize ctx;
		1 (* can happen if process_params fails already *)
	end else
		catch_completion_and_exit ctx callbacks run

let create_context comm cs compilation_step params = {
	com = Common.create compilation_step cs version params (DisplayTypes.DisplayMode.create !Parser.display_mode);
	messages = [];
	has_next = false;
	has_error = false;
	comm = comm;
	runtime_args = [];
}

module HighLevel = struct
	let add_libs libs args cs has_display =
		let global_repo = List.exists (fun a -> a = "--haxelib-global") args in
		let fail msg =
			raise (Arg.Bad msg)
		in
		let call_haxelib() =
			let t = Timer.timer ["haxelib"] in
			let cmd = "haxelib" ^ (if global_repo then " --global" else "") ^ " path " ^ String.concat " " libs in
			let pin, pout, perr = Unix.open_process_full cmd (Unix.environment()) in
			let lines = Std.input_list pin in
			let err = Std.input_list perr in
			let ret = Unix.close_process_full (pin,pout,perr) in
			if ret <> Unix.WEXITED 0 then fail (match lines, err with
				| [], [] -> "Failed to call haxelib (command not found ?)"
				| [], [s] when ExtString.String.ends_with (ExtString.String.strip s) "Module not found: path" -> "The haxelib command has been strip'ed, please install it again"
				| _ -> String.concat "\n" (lines@err));
			t();
			lines
		in
		match libs with
		| [] ->
			[]
		| _ ->
			let lines =
				try
					(* if we are compiling, really call haxelib since library path might have changed *)
					if not has_display then raise Not_found;
					cs#find_haxelib libs
				with Not_found -> try
					let lines = call_haxelib() in
					cs#cache_haxelib libs lines;
					lines
				with Unix.Unix_error(code,msg,arg) ->
					fail ((Printf.sprintf "%s (%s)" (Unix.error_message code) arg))
			in
			let lines = List.fold_left (fun acc l ->
				let l = ExtString.String.strip l in
				if l = "" then
					acc
				else if l.[0] <> '-' then
					"-libcp" :: l :: acc
				else match (try ExtString.String.split l " " with _ -> l, "") with
				| ("-L",dir) ->
					"--neko-lib-path" :: (String.sub l 3 (String.length l - 3)) :: acc
				| param, value ->
					let acc = if value <> "" then value :: acc else acc in
					let acc = param :: acc in
					acc
			) [] (List.rev lines) in
			lines

	(* Returns a list of contexts, but doesn't do anything yet *)
	let process_params server_api create each_args has_display is_server args =
		(* We want the loop below to actually see all the --each params, so let's prepend them *)
		let args = !each_args @ args in
		let added_libs = Hashtbl.create 0 in
		let server_mode = ref SMNone in
		let hxml_stack = ref [] in
		let create_context args =
			let ctx = create (server_api.on_context_create()) args in
			ctx
		in
		let rec find_subsequent_libs acc args = match args with
		| ("-L" | "--library" | "-lib") :: name :: args ->
			find_subsequent_libs (name :: acc) args
		| _ ->
			List.rev acc,args
		in
		let rec loop acc = function
			| [] ->
				[],Some (create_context (List.rev acc))
			| "--next" :: l when acc = [] -> (* skip empty --next *)
				loop [] l
			| "--next" :: l ->
				let ctx = create_context (List.rev acc) in
				ctx.has_next <- true;
				l,Some ctx
			| "--each" :: l ->
				each_args := List.rev acc;
				loop acc l
			| "--cwd" :: dir :: l | "-C" :: dir :: l ->
				(* we need to change it immediately since it will affect hxml loading *)
				(* Exceptions are ignored there to let arg parsing do the error handling in expected order *)
				(try Unix.chdir dir with _ -> ());
				(* Push the --cwd arg so the arg processor know we did something. *)
				loop (dir :: "--cwd" :: acc) l
			| "--connect" :: hp :: l ->
				if is_server then
					(* If we are already connected, ignore (issue #10813) *)
					loop acc l
				else begin
					let host, port = Helper.parse_host_port hp in
					server_api.do_connect host port ((List.rev acc) @ l);
					[],None
				end
			| "--server-connect" :: hp :: l ->
				server_mode := SMConnect hp;
				loop acc l
			| ("--server-listen" | "--wait") :: hp :: l ->
				server_mode := SMListen hp;
				loop acc l
			| "--run" :: cl :: args ->
				let acc = cl :: "-x" :: acc in
				let ctx = create_context (List.rev acc) in
				ctx.runtime_args <- args;
				[],Some ctx
			| ("-L" | "--library" | "-lib") :: name :: args ->
				let libs,args = find_subsequent_libs [name] args in
				let libs = List.filter (fun l -> not (Hashtbl.mem added_libs l)) libs in
				List.iter (fun l -> Hashtbl.add added_libs l ()) libs;
				let lines = add_libs libs args server_api.cache has_display in
				loop acc (lines @ args)
			| ("--jvm" | "-jvm" as arg) :: dir :: args ->
				loop_lib arg dir "hxjava" acc args
			| arg :: l ->
				match List.rev (ExtString.String.nsplit arg ".") with
				| "hxml" :: _ :: _ when (match acc with "-cmd" :: _ | "--cmd" :: _ -> false | _ -> true) ->
					let full_path = Extc.get_full_path arg in
					if List.mem full_path !hxml_stack then
						raise (Arg.Bad (Printf.sprintf "Duplicate hxml inclusion: %s" full_path))
					else
						hxml_stack := full_path :: !hxml_stack;
					let acc, l = (try acc, Helper.parse_hxml arg @ l with Not_found -> (arg ^ " (file not found)") :: acc, l) in
					loop acc l
				| _ ->
					loop (arg :: acc) l
		and loop_lib arg dir lib acc args =
			loop (dir :: arg :: acc) ("-lib" :: lib :: args)
		in
		let args,ctx = loop [] args in
		args,!server_mode,ctx

	let execute_ctx server_api ctx server_mode =
		begin match server_mode with
		| SMListen hp ->
			(* parse for com.verbose *)
			ignore(Args.parse_args ctx.com);
			let accept = match hp with
			| "stdio" ->
				server_api.init_wait_stdio()
			| _ ->
				let host, port = Helper.parse_host_port hp in
				server_api.init_wait_socket host port
			in
			server_api.wait_loop ctx.com.verbose accept
		| SMConnect hp ->
			ignore(Args.parse_args ctx.com);
			let host, port = Helper.parse_host_port hp in
			let accept = server_api.init_wait_connect host port in
			server_api.wait_loop ctx.com.verbose accept
		| SMNone ->
			compile_ctx server_api.callbacks ctx
		end

	let entry server_api comm args =
		let create = create_context comm server_api.cache in
		let each_args = ref [] in
		let curdir = Unix.getcwd () in
		let has_display = ref false in
		(* put --display in front if it was last parameter *)
		let args = match List.rev args with
			| file :: "--display" :: pl when file <> "memory" ->
				has_display := true;
				"--display" :: file :: List.rev pl
			| _ ->
				args
		in
		let rec loop args =
			let args,server_mode,ctx = try
				process_params server_api create each_args !has_display comm.is_server args
			with Arg.Bad msg ->
				let ctx = create 0 args in
				error ctx ("Error: " ^ msg) null_pos;
				[],SMNone,Some ctx
			in
			let code = match ctx with
				| Some ctx ->
					(* Need chdir here because --cwd is eagerly applied in process_params *)
					Unix.chdir curdir;
					execute_ctx server_api ctx server_mode
				| None ->
					(* caused by --connect *)
					0
			in
			if code = 0 && args <> [] && not !has_display then begin
				(* We have to chdir here again because any --cwd also takes effect in execute_ctx *)
				Unix.chdir curdir;
				loop args
			end else
				code
		in
		let code = loop args in
		comm.exit code
end
