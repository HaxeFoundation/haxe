open Globals
open Common
open CompilationContext
open ParsedArg

let handle_diagnostics ctx msg p kind =
	ctx.has_error <- true;
	add_diagnostics_message ctx.com msg p kind Error;
	match ctx.com.report_mode with
	| RMDiagnostics _ -> DisplayOutput.emit_diagnostics ctx.com
	| _ -> die "" __LOC__

let run_or_diagnose ctx f =
	let com = ctx.com in
	if is_diagnostics com then begin try
			f ()
		with
		| Error.Error err ->
			ctx.has_error <- true;
			Error.recurse_error (fun depth err ->
				add_diagnostics_message ~depth com (Error.error_msg err.err_message) err.err_pos DKCompilerMessage Error
			) err;
			(match com.report_mode with
			| RMDiagnostics _ -> DisplayOutput.emit_diagnostics ctx.com
			| _ -> die "" __LOC__)
		| Parser.Error(msg,p) ->
			handle_diagnostics ctx (Parser.error_msg msg) p DKParserError
		| Lexer.Error(msg,p) ->
			handle_diagnostics ctx (Lexer.error_msg msg) p DKParserError
		end
	else
		f ()

let run_command ctx cmd =
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
		end else if not ctx.comm.is_server then
			(* In non-server mode, inherit stdin/stdout/stderr so that interactive commands work *)
			Sys.command cmd
		else begin
			(* In server mode, capture stdout/stderr and forward stdin through the communication channel.
			   We use create_process instead of open_process_full so that we can
			   properly forward the client's stdin and close it to signal EOF. *)
			PipeThings.run_command ctx.comm cmd
		end
	in
	result

let run_command ctx cmd =
	Timer.time ctx.com.timer_ctx ["command";cmd] (run_command ctx) cmd

module Setup = struct
	let initialize_target ctx com actx =
		init_platform com;
		com.class_paths#lock_context com.custom_ext (platform_name com.platform) false;
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

				if es_version < 5 then
					failwith "Invalid -D js-es value, minimal supported version is 5";

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
				Common.define_value com Define.HxcppApiLevel "500";
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
		Common.define_value com Define.HaxeVer (Printf.sprintf "%.3f" (float_of_int version /. 1000.));
		Common.define_value com Define.Haxe s_version;
		Common.raw_define com "true";
		List.iter (fun (k,v) -> Define.raw_define_value com.defines k v) DefineList.default_values;
		com.info <- (fun ?(depth=0) ?(from_macro=false) msg p ->
			message ctx (make_compiler_message ~from_macro msg p depth DKCompilerMessage Information)
		);
		com.warning <- (fun ?(depth=0) ?(from_macro=false) w options msg p ->
			match Warning.get_mode w (options @ com.warning_options) with
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
		com.error <- (fun msg p -> com.error_ext (Error.make_error (Custom msg) p));
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
	if defined com Define.DisableParallelism then Parallel.enable := false;
	PMap.iter (fun k v ->
		try
			let reason = Hashtbl.find Define.deprecation_lut k in
			let p = fake_pos ("-D " ^ k) in
			begin match reason with
			| DueTo reason ->
				com.warning WDeprecatedDefine [] reason p
			| InFavorOf d ->
				Define.raw_define_value com.defines d v;
				com.warning WDeprecatedDefine [] (Printf.sprintf "-D %s has been deprecated in favor of -D %s" k d) p
			end;
		with Not_found ->
			()
	) com.defines.values

(** Creates the typer context and types [classes] into it. *)
let do_type ctx mctx actx display_file_dot_path =
	let com = ctx.com in
	let cs = com.cs in
	CommonCache.maybe_add_context_sign cs com "before_init_macros";
	enter_stage com CInitMacrosStart;
	ServerMessage.compiler_stage com;
	Setup.init_native_libs com actx.hxb_libs;
	let mctx = List.fold_left (fun mctx path ->
		Some (MacroContext.call_init_macro ctx.com mctx path)
	) mctx (List.rev actx.config_macros) in
	enter_stage com CInitMacrosDone;
	check_defines ctx.com;
	update_platform_config com; (* make sure to adapt all flags changes defined during init macros *)
	ServerMessage.compiler_stage com;

	let macros = match mctx with None -> None | Some mctx -> mctx.g.macros in
	Setup.init_native_libs com actx.native_libs;
	let tctx = Setup.create_typer_context ctx macros in
	let display_file_dot_path = DisplayProcessing.maybe_load_display_file_before_typing tctx display_file_dot_path in
	(* Make sure display module is being typed *)
	Option.may (fun cpath -> actx.classes <- cpath :: actx.classes) display_file_dot_path;
	DumpConfig.update_from_defines com.dump_config com.defines;
	CommonCache.lock_signature com "after_init_macros";
	Option.may (fun mctx -> MacroContext.finalize_macro_api tctx mctx) mctx;
	(try begin
		com.callbacks#run com.error_ext com.callbacks#get_after_init_macros;
		run_or_diagnose ctx (fun () ->
			if com.display.dms_kind <> DMNone then DisplayTexpr.check_display_file tctx cs;
			List.iter (fun cpath ->
				ignore(tctx.Typecore.g.Typecore.do_load_module tctx cpath null_pos);
				Typecore.flush_pass tctx.g PBuildClass "actx.classes"
			) (List.rev actx.classes);
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
	(tctx, display_file_dot_path)

let finalize_typing ctx tctx =
	let com = ctx.com in
	let main_module = Finalization.maybe_load_main tctx in
	enter_stage com CFilteringStart;
	ServerMessage.compiler_stage com;
	let (main_expr,main_file),types,modules = run_or_diagnose ctx (fun () -> Finalization.generate tctx main_module) in
	com.main.main_expr <- main_expr;
	com.main.main_file <- main_file;
	com.types <- types;
	com.modules <- modules

let finalize_typing ctx tctx =
	Timer.time ctx.com.timer_ctx ["finalize"] (finalize_typing ctx) tctx

let filter ctx tctx ectx before_destruction =
	Timer.time ctx.com.timer_ctx ["filters"] (fun () ->
		run_or_diagnose ctx (fun () -> Filters.run tctx ectx before_destruction)
	) ()

let compile ctx actx sctx =
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
	ServerCache.after_target_init sctx ctx;
	Timer.time ctx.com.timer_ctx ["init"] (fun () ->
		List.iter (fun f -> f()) (List.rev (actx.pre_compilation));
		begin match actx.hxb_out with
			| None ->
				()
			| Some file ->
				com.hxb_writer_config <- HxbWriterConfig.process_argument file
		end;
	) ();
	enter_stage com CInitialized;
	ServerMessage.compiler_stage com;
	if actx.classes = [([],"Std")] && not actx.force_typing then begin
		if actx.cmds = [] && not actx.did_something then actx.raise_usage();
	end else begin
		(* Actual compilation starts here *)
		let (tctx,display_file_dot_path) = Timer.time ctx.com.timer_ctx ["typing"] (do_type ctx mctx actx) display_file_dot_path in
		DisplayProcessing.handle_display_after_typing ctx tctx display_file_dot_path;
		let ectx = ExceptionInit.create_exception_context tctx in
		finalize_typing ctx tctx;
		Dump.maybe_generate_dump ctx.com AfterTyping;
		let is_compilation = is_compilation com in
		com.callbacks#add_after_save (fun () ->
			ServerCache.after_save sctx ctx;
			if is_compilation then match com.hxb_writer_config with
				| Some config ->
					Generate.check_hxb_output ctx config;
				| None ->
					()
		);
		if is_diagnostics com then
			filter ctx com ectx (fun () -> DisplayProcessing.handle_display_after_finalization ctx tctx display_file_dot_path)
		else begin
			DisplayProcessing.handle_display_after_finalization ctx tctx display_file_dot_path;
			filter ctx com ectx (fun () -> ());
		end;
		if ctx.has_error then raise Abort;
		if is_compilation then Generate.check_auxiliary_output com actx;
		enter_stage com CGenerationStart;
		ServerMessage.compiler_stage com;
		Dump.maybe_generate_dump ctx.com AfterDce;
		Generate.maybe_generate_dump_dependencies ctx tctx;
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

let make_ice_message (com : Common.context) msg backtrace =
		let ver = (s_version_full com.sctx.version) in
		let os_type = if Sys.unix then "unix" else "windows" in
		Printf.sprintf "%s\nHaxe: %s; OS type: %s;\n%s" msg ver os_type backtrace
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
		if ctx.com.display.dms_kind <> DMNone && ctx.has_next then begin
			ctx.has_error <- false;
			ctx.messages <- [];
		end else begin
			let sub = List.map (fun p -> Error.make_error (Error.Custom (Error.compl_msg "referenced here")) p) pl in
			error_ext ctx (Error.make_error (Error.Custom (Printf.sprintf "You cannot access the %s package while %s (for %s)" pack (if pf = "macro" then "in a macro" else "targeting " ^ pf) (s_type_path m))) ~sub p)
		end
	| Error.Error err ->
		error_ext ctx err
	| Arg.Bad msg ->
		error ctx ("Error: " ^ msg) null_pos
	| Failure msg when is_diagnostics com ->
		handle_diagnostics ctx msg null_pos DKCompilerMessage;
	| Failure msg when not Helper.is_debug_run ->
		error ctx ("Error: " ^ msg) null_pos
	| Globals.Ice (msg,backtrace) when is_diagnostics com ->
		let s = make_ice_message com msg backtrace in
		handle_diagnostics ctx s null_pos DKCompilerMessage
	| Globals.Ice (msg,backtrace) when not Helper.is_debug_run ->
		let s = make_ice_message com msg backtrace in
		error ctx ("Error: " ^ s) null_pos
	| Helper.HelpMessage msg ->
		print_endline msg
	| Parser.TypePath (p,c,is_import,pos) ->
		DisplayOutput.handle_type_path_exception ctx p c is_import pos
	| Parser.SyntaxCompletion(kind,subj) ->
		DisplayOutput.handle_syntax_completion com kind subj;
		error ctx ("Error: No completion point was found") null_pos
	| DisplayException.DisplayException dex ->
		DisplayOutput.handle_display_exception ctx dex
	| Abort | Out_of_memory | EvalTypes.Sys_exit _ | Hlinterp.Sys_exit _ | DisplayProcessingGlobals.Completion _ | DisplayJson.JsonCompleted as exc ->
		(* We don't want these to be caught by the catchall below *)
		raise exc
	| e when (try Sys.getenv "OCAMLRUNPARAM" <> "b" with _ -> true) && not Helper.is_debug_run ->
		error ctx (Printexc.to_string e) null_pos

let compile_safe ctx f =
	try compile_safe ctx f with Abort -> ()

let finalize ctx =
	ctx.com.part_scope.io.close ();
	List.iter (fun lib -> lib#close) ctx.com.hxb_libs;
	(* In server mode any open libs are closed by the lib_build_task. In offline mode
		we should do it here to be safe. *)
	if not ctx.comm.is_server then begin
		List.iter (fun lib -> lib#close) ctx.com.native_libs.java_libs;
		List.iter (fun lib -> lib#close) ctx.com.native_libs.swf_libs;
	end

let emit_completion ctx str =
	ServerMessage.completion str;
	ctx.comm.write_err str

let catch_completion_and_exit ctx sctx run =
	try
		run ctx;
		if ctx.has_error then 1 else 0
	with
		| DisplayProcessingGlobals.Completion str ->
			ServerCache.after_compilation sctx ctx;
			emit_completion ctx str;
			finalize ctx;
			0
		| DisplayJson.JsonCompleted ->
			ServerCache.after_compilation sctx ctx;
			finalize ctx;
			0
		| EvalTypes.Sys_exit i | Hlinterp.Sys_exit i ->
			if i <> 0 then ctx.has_error <- true;
			ctx.comm.flush ctx;
			finalize ctx;
			i

let process_actx ctx actx =
	ctx.com.doinline <- ctx.com.display.dms_inline && not (Common.defined ctx.com Define.NoInline);
	ctx.com.timer_ctx.measure_times <- (if actx.measure_times then Yes else No);
	match DisplayProcessing.process_display_arg ctx actx with
	| Completed ->
		raise DisplayJson.JsonCompleted
	| NotCompleted ->
		if defined ctx.com NoDeprecationWarnings then begin
			ctx.com.warning_options <- [{wo_warning = WDeprecated; wo_mode = WMDisable}] :: ctx.com.warning_options
		end

let compile_ctx sctx ctx =
	let run ctx =
		ServerCache.before_anything sctx ctx;
		Setup.setup_common_context ctx;
		compile_safe ctx (fun () ->
			let actx = Args.process_args ctx.com ctx.parsed_args in
			process_actx ctx actx;
			compile ctx actx sctx;
		);
		ctx.comm.flush ctx;
		ServerCache.after_compilation sctx ctx;
		finalize ctx;
	in
	if ctx.has_error then begin
		ctx.comm.flush ctx;
		finalize ctx;
		1 (* can happen if process_params fails already *)
	end else
		catch_completion_and_exit ctx sctx run

let create_context comm sctx request_scope compilation_step (parsed_args : parsed_arg list) =
	let io = PipeThings.create_io comm in
	let part_scope = {
		warned_positions = Hashtbl.create 0;
		diagnostics_messages = [];
		io;
	} in
	let com = Common.create sctx request_scope part_scope compilation_step (Args.to_raw_args parsed_args) (DisplayTypes.DisplayMode.create DMNone) in
	{
		com;
		messages = [];
		has_next = false;
		has_error = false;
		comm = comm;
		runtime_args = [];
		parsed_args;
	}

module HighLevel = struct
	let add_libs timer_ctx libs args cs has_display =
		let global_repo = List.exists (fun a -> a = "--haxelib-global") args in
		let fail msg =
			raise (Arg.Bad msg)
		in
		let call_haxelib() =
			let cmd = "haxelib" ^ (if global_repo then " --global" else "") ^ " path " ^ String.concat " " libs in
			let pin, pout, perr = Unix.open_process_full cmd (Unix.environment()) in
			let lines = Std.input_list pin in
			let err = Std.input_list perr in
			let ret = Unix.close_process_full (pin,pout,perr) in
			if ret <> Unix.WEXITED 0 then fail (match lines, err with
				| [], [] -> "Failed to call haxelib (command not found ?)"
				| [], [s] when ExtString.String.ends_with (ExtString.String.strip s) "Module not found: path" -> "The haxelib command has been strip'ed, please install it again"
				| _ -> String.concat "\n" (lines@err));
			lines
		in
		let call_haxelib () =
			Timer.time timer_ctx ["haxelib"] call_haxelib ()
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

	let create_context_from_part (sctx : ServerCompilationContext.t) comm (request_scope : request_scope) has_display part =
		(* Expand Expand markers by calling haxelib, caching the result in the marker state *)
		let expand_part_libs has_global (part_args : parsed_arg list) =
			let expand_one arg = match arg with
				| Expand ex ->
					(match ex.state with
					| AlreadyExpanded expanded ->
						expanded
					| NotYetExpanded (AddLibs libs) ->
						let raw_lines = add_libs request_scope.timer_ctx libs (if has_global then ["--haxelib-global"] else []) sctx.cs has_display in
						let expanded = Args.parse_args raw_lines in
						ex.state <- AlreadyExpanded expanded;
						expanded)
				| arg -> [arg]
			in
			List.concat_map expand_one part_args
		in
		let has_global = List.exists (fun a -> a = HaxelibGlobal) part.Args.args in
		let expanded_args = expand_part_libs has_global part.Args.args in
		sctx.compilation_step <- sctx.compilation_step + 1;
		create_context comm sctx request_scope sctx.compilation_step expanded_args

	let entry sctx request_scope comm (args : parsed_arg list) =
		let curdir = Unix.getcwd () in
		try
			let request_args = Args.expand_args args in
			let has_display = request_args.display_arg <> None in
			let rec loop = function
				| [] -> 0
				| part :: rest ->
					(* Re-apply original dir in case --cwd was used in a previous part *)
					Unix.chdir curdir;
					let ctx = create_context_from_part sctx comm request_scope has_display part in
					if rest <> [] then ctx.has_next <- true;
					ctx.runtime_args <- part.Args.runtime_args;
					let code = compile_ctx sctx ctx in
					if code = 0 && rest <> [] && not has_display then
						loop rest
					else
						code
			in
			let code = loop request_args.parts in
			Unix.chdir curdir;
			code
		with Arg.Bad msg ->
			Unix.chdir curdir;
			(* TODO: this is silly *)
			let ctx = create_context comm sctx request_scope 0 args in
			error ctx ("Error: " ^ msg) null_pos;
			compile_ctx sctx ctx
end
