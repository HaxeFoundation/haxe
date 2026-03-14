open Globals
open Message
open Common
open ParsedArg

let error_ext com (err : Error.error) =
	com.error_ext err

let error com msg p =
	error_ext com (Error.make_error (Custom msg) p)

let has_error com =
	com.part_scope.has_error

let handle_diagnostics com ?(diagnostics_kind = MessageKind.DKCompilerMessage) msg p message_kind =
	com.part_scope.has_error <- true;
	add_diagnostics_message ~diagnostics_kind com msg p message_kind;
	match com.report_mode with
	| RMDiagnostics _ -> DisplayOutput.emit_diagnostics com
	| _ -> die "" __LOC__

let run_or_diagnose com f =
	if is_diagnostics com then begin try
			f ()
		with
		| Error.Error err ->
			com.part_scope.has_error <- true;
			Error.recurse_error (fun depth err ->
				add_diagnostics_message ~depth com (Error.error_msg err.err_message) err.err_pos MKError
			) err;
			(match com.report_mode with
			| RMDiagnostics _ -> DisplayOutput.emit_diagnostics com
			| _ -> die "" __LOC__)
		| Parser.Error(msg,p) ->
			handle_diagnostics com ~diagnostics_kind:DKParserError (Parser.error_msg msg) p MKError
		| Lexer.Error(msg,p) ->
			handle_diagnostics com ~diagnostics_kind:DKParserError (Lexer.error_msg msg) p MKError
		end
	else
		f ()

let run_command com cmd =
	let io = com.request_scope.io in
	(* TODO: this is a hack *)
	let cmd = if com.sctx.is_server then begin
		let h = Hashtbl.create 0 in
		Hashtbl.add h "__file__" com.file;
		Hashtbl.add h "__platform__" (platform_name com.platform);
		Helper.expand_env ~h:(Some h) cmd
	end else
		cmd
	in
	let len = String.length cmd in
	let result =
		if len > 3 && String.sub cmd 0 3 = "cd " then begin
			Sys.chdir (String.sub cmd 3 (len - 3));
			0
		end else if not com.sctx.is_server then
			(* In non-server mode, inherit stdin/stdout/stderr so that interactive commands work *)
			Sys.command cmd
		else begin
			(* In server mode, capture stdout/stderr through the output target and
			   forward the client's stdin from request_scope. *)
			PipeThings.run_command io cmd
		end
	in
	result

let run_command com cmd =
	Timer.time com.timer_ctx ["command";cmd] (run_command com) cmd

module Setup = struct
	let initialize_target com actx =
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

	let create_typer_context com macros =
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

	let setup_common_context com =
		Common.define_value com Define.HaxeVer (Printf.sprintf "%.3f" (float_of_int version /. 1000.));
		Common.define_value com Define.Haxe s_version;
		Common.raw_define com "true";
		List.iter (fun (k,v) -> Define.raw_define_value com.defines k v) DefineList.default_values;
		com.info <- CompilerMessage.default_info_handler com;
		com.warning <- CompilerMessage.default_warning_handler com;
		com.error_ext <- CompilerMessage.default_error_handler com;
		com.error <- (fun msg p -> com.error_ext (Error.make_error (Custom msg) p));
		let filter_messages = (fun keep_errors predicate -> (List.filter (fun cm ->
			(match cm_severity cm with
			| MessageSeverity.Error -> keep_errors;
			| Information | Warning | Hint -> predicate cm;)
		) (List.rev com.part_scope.messages))) in
		com.get_messages <- (fun () -> (List.map (fun cm ->
			(match cm_severity cm with
			| MessageSeverity.Error -> die "" __LOC__;
			| Information | Warning | Hint -> cm;)
		) (filter_messages false (fun _ -> true))));
		com.filter_messages <- (fun predicate -> (com.part_scope.messages <- (List.rev (filter_messages true predicate))));
		com.run_command <- run_command com;
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
let do_type com mctx actx display_file_dot_path =
	let cs = com.cs in
	CommonCache.maybe_add_context_sign cs com "before_init_macros";
	enter_stage com CInitMacrosStart;
	ServerMessage.compiler_stage com;
	Setup.init_native_libs com actx.hxb_libs;
	let mctx = List.fold_left (fun mctx path ->
		Some (MacroContext.call_init_macro com mctx path)
	) mctx (List.rev actx.config_macros) in
	enter_stage com CInitMacrosDone;
	check_defines com;
	update_platform_config com; (* make sure to adapt all flags changes defined during init macros *)
	ServerMessage.compiler_stage com;

	let macros = match mctx with None -> None | Some mctx -> mctx.g.macros in
	Setup.init_native_libs com actx.native_libs;
	let tctx = Setup.create_typer_context com macros in
	let display_file_dot_path = DisplayProcessing.maybe_load_display_file_before_typing tctx display_file_dot_path in
	(* Make sure display module is being typed *)
	Option.may (fun cpath -> actx.classes <- cpath :: actx.classes) display_file_dot_path;
	DumpConfig.update_from_defines com.dump_config com.defines;
	CommonCache.lock_signature com "after_init_macros";
	Option.may (fun mctx -> MacroContext.finalize_macro_api tctx mctx) mctx;
	(try begin
		com.callbacks#run com.error_ext com.callbacks#get_after_init_macros;
		run_or_diagnose com (fun () ->
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

let finalize_typing com tctx =
	let main_module = Finalization.maybe_load_main tctx in
	enter_stage com CFilteringStart;
	ServerMessage.compiler_stage com;
	let (main_expr,main_file),types,modules = run_or_diagnose com (fun () -> Finalization.generate tctx main_module) in
	com.main.main_expr <- main_expr;
	com.main.main_file <- main_file;
	com.types <- types;
	com.modules <- modules

let finalize_typing com tctx =
	Timer.time com.timer_ctx ["finalize"] (finalize_typing com) tctx

let filter com tctx ectx before_destruction =
	Timer.time com.timer_ctx ["filters"] (fun () ->
		run_or_diagnose com (fun () -> Filters.run tctx ectx before_destruction)
	) ()

let compile com actx sctx =
	(* Set up display configuration *)
	DisplayProcessing.process_display_configuration com;
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
	let ext = Setup.initialize_target com actx in
	update_platform_config com; (* make sure to adapt all flags changes defined after platform *)
	ServerCache.after_target_init sctx com;
	Timer.time com.timer_ctx ["init"] (fun () ->
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
		let (tctx,display_file_dot_path) = Timer.time com.timer_ctx ["typing"] (do_type com mctx actx) display_file_dot_path in
		if DisplayProcessing.handle_display_after_typing com tctx display_file_dot_path then raise CompilerMessage.Abort;
		let ectx = ExceptionInit.create_exception_context tctx in
		finalize_typing com tctx;
		Dump.maybe_generate_dump com AfterTyping;
		let is_compilation = is_compilation com in
		com.callbacks#add_after_save (fun () ->
			ServerCache.after_save sctx com;
			if is_compilation then match com.hxb_writer_config with
				| Some config ->
					Generate.check_hxb_output com config;
				| None ->
					()
		);
		if is_diagnostics com then
			filter com com ectx (fun () -> DisplayProcessing.handle_display_after_finalization com tctx display_file_dot_path)
		else begin
			DisplayProcessing.handle_display_after_finalization com tctx display_file_dot_path;
			filter com com ectx (fun () -> ());
		end;
		if has_error com && is_compilation then raise CompilerMessage.Abort;
		if is_compilation then Generate.check_auxiliary_output com actx;
		enter_stage com CGenerationStart;
		ServerMessage.compiler_stage com;
		Dump.maybe_generate_dump com AfterDce;
		Generate.maybe_generate_dump_dependencies com tctx;
		if not actx.no_output then Generate.generate com tctx ext actx;
		enter_stage com CGenerationDone;
		ServerMessage.compiler_stage com;
	end;
	Sys.catch_break false;
	com.callbacks#run com.error_ext com.callbacks#get_after_generation;
	if not actx.no_output then begin
		List.iter (fun c ->
			let r = run_command com c in
			if r <> 0 then failwith ("Command failed with error " ^ string_of_int r)
		) (List.rev actx.cmds)
	end

let make_ice_message (com : Common.context) msg backtrace =
		let ver = (s_version_full com.sctx.version) in
		let os_type = if Sys.unix then "unix" else "windows" in
		Printf.sprintf "%s\nHaxe: %s; OS type: %s;\n%s" msg ver os_type backtrace
let compile_safe com f =
try
	f ()
with
	| Error.Fatal_error err ->
		error_ext com err
	| Lexer.Error (m,p) ->
		error com (Lexer.error_msg m) p
	| Parser.Error (m,p) ->
		error com (Parser.error_msg m) p
	| Typecore.Forbid_package ((pack,m,p),pl,pf)  ->
		if com.display.dms_kind <> DMNone && com.part_scope.has_next then begin
			com.part_scope.has_error <- false;
			com.part_scope.messages <- [];
		end else begin
			let sub = List.map (fun p -> Error.make_error (Error.Custom (Error.compl_msg "referenced here")) p) pl in
			error_ext com (Error.make_error (Error.Custom (Printf.sprintf "You cannot access the %s package while %s (for %s)" pack (if pf = "macro" then "in a macro" else "targeting " ^ pf) (s_type_path m))) ~sub p)
		end
	| Error.Error err ->
		error_ext com err
	| Arg.Bad msg ->
		error com ("Error: " ^ msg) null_pos
	| Failure msg when is_diagnostics com ->
		handle_diagnostics com msg null_pos MKError;
	| Failure msg when not Helper.is_debug_run ->
		error com ("Error: " ^ msg) null_pos
	| Globals.Ice (msg,backtrace) when is_diagnostics com ->
		let s = make_ice_message com msg backtrace in
		handle_diagnostics com s null_pos MKError
	| Globals.Ice (msg,backtrace) when not Helper.is_debug_run ->
		let s = make_ice_message com msg backtrace in
		error com ("Error: " ^ s) null_pos
	| Helper.HelpMessage msg ->
		CompilerIo.write_out com.request_scope.io (msg ^ "\n")
	| Parser.TypePath (p,c,is_import,pos) ->
		DisplayOutput.handle_type_path_exception com p c is_import pos
	| Parser.SyntaxCompletion(kind,subj) ->
		DisplayOutput.handle_syntax_completion com kind subj;
		error com ("Error: No completion point was found") null_pos
	| DisplayException.DisplayException dex ->
		DisplayOutput.handle_display_exception com dex
	| CompilerMessage.Abort | Out_of_memory | EvalTypes.Sys_exit _ | Hlinterp.Sys_exit _ | DisplayJson.JsonCompleted as exc ->
		(* We don't want these to be caught by the catchall below *)
		raise exc
	| e when (try Sys.getenv "OCAMLRUNPARAM" <> "b" with _ -> true) && not Helper.is_debug_run ->
		error com (Printexc.to_string e) null_pos

let compile_safe com f =
	try compile_safe com f with CompilerMessage.Abort -> ()

let finalize com =
	CompilerIo.flush com.request_scope.io;
	List.iter (fun lib -> lib#close) com.hxb_libs;
	(* In server mode any open libs are closed by the lib_build_task. In offline mode
		we should do it here to be safe. *)
	if not com.sctx.is_server then begin
		List.iter (fun lib -> lib#close) com.native_libs.java_libs;
		List.iter (fun lib -> lib#close) com.native_libs.swf_libs;
	end

module ContextFlush = struct
	let flush_context com =
		match com.report_mode with
		| RMDiagnostics _ ->
			(* In diagnostics mode, messages are already in the unified buffer.
			   Output happens via DisplayOutput.emit_diagnostics, not flush_messages. *)
			()
		| _ ->
			let rh = com.request_scope.result_handler in
			CompilerOutput.flush_messages rh (Common.has_error_to_report com) com
end

let catch_completion_and_exit com sctx run =
	try
		run com;
		if has_error com then 1 else 0
	with
		| DisplayJson.JsonCompleted ->
			finalize com;
			0
		| EvalTypes.Sys_exit i | Hlinterp.Sys_exit i ->
			if i <> 0 then com.part_scope.has_error <- true;
			ContextFlush.flush_context com;
			finalize com;
			i

let process_actx com actx =
	com.doinline <- com.display.dms_inline && not (Common.defined com Define.NoInline);
	com.timer_ctx.measure_times <- (if actx.measure_times then Yes else No);
	let check_deprecation_settings () =
		if defined com NoDeprecationWarnings then begin
			com.warning_options <- [{wo_warning = WDeprecated; wo_mode = WMDisable}] :: com.warning_options
		end
	in
	match DisplayProcessing.process_display_arg com actx with
	| Completed ->
		raise DisplayJson.JsonCompleted
	| NeedsTyping ->
		actx.did_something <- true;
		actx.force_typing <- true;
		check_deprecation_settings ()
	| NoCompletionPointFound ->
		check_deprecation_settings ()

let compile_ctx sctx com =
	let run com =
		ServerCache.before_anything sctx com;
		Setup.setup_common_context com;
		compile_safe com (fun () ->
			let actx = Args.process_args com in
			process_actx com actx;
			compile com actx sctx;
		);
		ContextFlush.flush_context com;
		finalize com;
	in
	catch_completion_and_exit com sctx run

let create_context sctx request_scope runtime_args has_next =
	let part_scope = {
		runtime_args;
		warned_positions = Hashtbl.create 0;
		has_next;
		has_error = false;
		messages = [];
	} in
	Common.create sctx request_scope part_scope sctx.compilation_step (DisplayTypes.DisplayMode.create DMNone)

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

	let create_context_from_part (sctx : ServerCompilationContext.t) (request_scope : request_scope) has_display has_next (part : Args.part_args) =
		sctx.compilation_step <- sctx.compilation_step + 1;
		let com = create_context sctx request_scope part.runtime_args has_next in
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
		com.parsed_args <- expanded_args;
		com

	let entry sctx request_scope (request_args : Args.request_args) =
		let curdir = Unix.getcwd () in
		let has_display = request_args.display_arg <> None in
		let rec loop = function
			| [] ->
				0
			| part :: rest ->
				let com = create_context_from_part sctx request_scope has_display (rest <> []) part in
				let code = compile_ctx sctx com in
				Unix.chdir curdir;
				if code = 0 && rest <> [] && not has_display then
					loop rest
				else
					code
		in
		let code = loop request_args.parts in
		Unix.chdir curdir;
		code
end


