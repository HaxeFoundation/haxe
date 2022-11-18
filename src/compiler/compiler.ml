open Globals
open Common
open CompilationContext

let run_or_diagnose ctx f arg =
	let com = ctx.com in
	let handle_diagnostics msg p kind =
		ctx.has_error <- true;
		add_diagnostics_message com msg p kind Error;
		DisplayOutput.emit_diagnostics ctx.com
	in
	if is_diagnostics com then begin try
			f arg
		with
		| Error.Error(msg,p) ->
			handle_diagnostics (Error.error_msg msg) p DKCompilerMessage
		| Parser.Error(msg,p) ->
			handle_diagnostics (Parser.error_msg msg) p DKParserError
		| Lexer.Error(msg,p) ->
			handle_diagnostics (Lexer.error_msg msg) p DKParserError
		end
	else
		f arg

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
		let add_std dir =
			com.class_path <- List.filter (fun s -> not (List.mem s com.std_path)) com.class_path @ List.map (fun p -> p ^ dir ^ "/_std/") com.std_path @ com.std_path
		in
		match com.platform with
			| Cross ->
				(* no platform selected *)
				set_platform com Cross "";
				"?"
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
			| Cs ->
				Dotnet.before_generate com;
				add_std "cs"; "cs"
			| Java ->
				Java.before_generate com;
				if defined com Define.Jvm then begin
					add_std "jvm";
					com.package_rules <- PMap.remove "jvm" com.package_rules;
				end;
				add_std "java";
				"java"
			| Python ->
				add_std "python";
				if not (Common.defined com Define.PythonVersion) then
					Common.define_value com Define.PythonVersion "3.3";
				"python"
			| Hl ->
				add_std "hl";
				if not (Common.defined com Define.HlVer) then Define.define_value com.defines Define.HlVer (try Std.input_file (Common.find_file com "hl/hl_version") with Not_found -> die "" __LOC__);
				"hl"
			| Eval ->
				add_std "eval";
				"eval"

	let create_typer_context ctx native_libs =
		let com = ctx.com in
		Common.log com ("Classpath: " ^ (String.concat ";" com.class_path));
		let buffer = Buffer.create 64 in
		Buffer.add_string buffer "Defines: ";
		PMap.iter (fun k v -> match v with
			| "1" -> Printf.bprintf buffer "%s;" k
			| _ -> Printf.bprintf buffer "%s=%s;" k v
		) com.defines.values;
		Buffer.truncate buffer (Buffer.length buffer - 1);
		Common.log com (Buffer.contents buffer);
		Typecore.type_expr_ref := (fun ?(mode=MGet) ctx e with_type -> Typer.type_expr ~mode ctx e with_type);
		List.iter (fun f -> f ()) (List.rev com.callbacks#get_before_typer_create);
		(* Native lib pass 1: Register *)
		let fl = List.map (fun (file,extern) -> NativeLibraryHandler.add_native_lib com file extern) (List.rev native_libs) in
		(* Native lib pass 2: Initialize *)
		List.iter (fun f -> f()) fl;
		Typer.create com

	let executable_path() =
		Extc.executable_path()

	let get_std_class_paths () =
		try
			let p = Sys.getenv "HAXE_STD_PATH" in
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
			"" :: List.map Path.add_trailing_slash (loop parts)
		with Not_found ->
			let base_path = Path.get_real_path (try executable_path() with _ -> "./") in
			if Sys.os_type = "Unix" then
				let prefix_path = Filename.dirname base_path in
				let lib_path = Filename.concat prefix_path "lib" in
				let share_path = Filename.concat prefix_path "share" in
				[
					"";
					Path.add_trailing_slash (Filename.concat lib_path "haxe/std");
					Path.add_trailing_slash (Filename.concat lib_path "haxe/extraLibs");
					Path.add_trailing_slash (Filename.concat share_path "haxe/std");
					Path.add_trailing_slash (Filename.concat share_path "haxe/extraLibs");
					Path.add_trailing_slash (Filename.concat base_path "std");
					Path.add_trailing_slash (Filename.concat base_path "extraLibs")
				]
			else
				[
					"";
					Path.add_trailing_slash (Filename.concat base_path "std");
					Path.add_trailing_slash (Filename.concat base_path "extraLibs")
				]

	let setup_common_context ctx =
		let com = ctx.com in
		ctx.com.print <- ctx.comm.write_out;
		Common.define_value com Define.HaxeVer (Printf.sprintf "%.3f" (float_of_int Globals.version /. 1000.));
		Common.raw_define com "haxe3";
		Common.raw_define com "haxe4";
		Common.define_value com Define.Haxe s_version;
		Common.raw_define com "true";
		Common.define_value com Define.Dce "std";
		com.info <- (fun msg p -> message ctx (msg,p,DKCompilerMessage,Information));
		com.warning <- (fun w options msg p ->
			match Warning.get_mode w (com.warning_options @ options) with
			| WMEnable ->
				let wobj = Warning.warning_obj w in
				let msg = if wobj.w_generic then
					msg
				else
					Printf.sprintf "(%s) %s" wobj.w_name msg
				in
				message ctx (msg,p,DKCompilerMessage,Warning)
			| WMDisable ->
				()
		);
		com.error <- error ctx;
		let filter_messages = (fun keep_errors predicate -> (List.filter (fun ((_,_,_,sev) as cm) ->
			(match sev with
			| MessageSeverity.Error -> keep_errors;
			| Information | Warning | Hint -> predicate cm;)
		) (List.rev ctx.messages))) in
		com.get_messages <- (fun () -> (List.map (fun ((_,_,_,sev) as cm) ->
			(match sev with
			| MessageSeverity.Error -> die "" __LOC__;
			| Information | Warning | Hint -> cm;)
		) (filter_messages false (fun _ -> true))));
		com.filter_messages <- (fun predicate -> (ctx.messages <- (List.rev (filter_messages true predicate))));
		com.run_command <- run_command ctx;
		com.class_path <- get_std_class_paths ();
		com.std_path <- List.filter (fun p -> ExtString.String.ends_with p "std/" || ExtString.String.ends_with p "std\\") com.class_path

end

(** Creates the typer context and types [classes] into it. *)
let do_type ctx tctx actx =
	let com = tctx.Typecore.com in
	let t = Timer.timer ["typing"] in
	let cs = com.cs in
	CommonCache.maybe_add_context_sign cs com "before_init_macros";
	com.stage <- CInitMacrosStart;
	List.iter (MacroContext.call_init_macro tctx) (List.rev actx.config_macros);
	com.stage <- CInitMacrosDone;
	CommonCache.lock_signature com "after_init_macros";
	List.iter (fun f -> f ()) (List.rev com.callbacks#get_after_init_macros);
	run_or_diagnose ctx (fun () ->
		if com.display.dms_kind <> DMNone then DisplayTexpr.check_display_file tctx cs;
		List.iter (fun cpath -> ignore(tctx.Typecore.g.Typecore.do_load_module tctx cpath null_pos)) (List.rev actx.classes);
		Finalization.finalize tctx;
	) ();
	com.stage <- CTypingDone;
	(* If we are trying to find references, let's syntax-explore everything we know to check for the
		identifier we are interested in. We then type only those modules that contain the identifier. *)
	begin match com.display.dms_kind with
		| (DMUsage _ | DMImplementation) -> FindReferences.find_possible_references tctx cs;
		| _ -> ()
	end;
	t()

let finalize_typing ctx tctx =
	let t = Timer.timer ["finalize"] in
	let com = ctx.com in
	com.stage <- CFilteringStart;
	let main, types, modules = run_or_diagnose ctx Finalization.generate tctx in
	com.main <- main;
	com.types <- types;
	com.modules <- modules;
	t()

let filter ctx tctx =
	let t = Timer.timer ["filters"] in
	DeprecationCheck.run ctx.com;
	Filters.run ctx.com tctx ctx.com.main;
	t()

let compile ctx actx =
	let com = ctx.com in
	(* Set up display configuration *)
	DisplayProcessing.process_display_configuration ctx;
	let display_file_dot_path = DisplayProcessing.process_display_file com actx in
	(* Initialize target: This allows access to the appropriate std packages and sets the -D defines. *)
	let ext = Setup.initialize_target ctx com actx in
	com.config <- get_config com; (* make sure to adapt all flags changes defined after platform *)
	let t = Timer.timer ["init"] in
	List.iter (fun f -> f()) (List.rev (actx.pre_compilation));
	t();
	com.stage <- CInitialized;
	if actx.classes = [([],"Std")] && not actx.force_typing then begin
		if actx.cmds = [] && not actx.did_something then actx.raise_usage();
	end else begin
		(* Actual compilation starts here *)
		let tctx = Setup.create_typer_context ctx actx.native_libs in
		com.stage <- CTyperCreated;
		let display_file_dot_path = DisplayProcessing.maybe_load_display_file_before_typing tctx display_file_dot_path in
		begin try
			do_type ctx tctx actx
		with TypeloadParse.DisplayInMacroBlock ->
			ignore(DisplayProcessing.load_display_module_in_macro tctx display_file_dot_path true);
		end;
		DisplayProcessing.handle_display_after_typing ctx tctx display_file_dot_path;
		finalize_typing ctx tctx;
		DisplayProcessing.handle_display_after_finalization ctx tctx display_file_dot_path;
		filter ctx tctx;
		if ctx.has_error then raise Abort;
		Generate.check_auxiliary_output com actx;
		com.stage <- CGenerationStart;
		if not actx.no_output then Generate.generate ctx tctx ext actx;
		com.stage <- CGenerationDone;
	end;
	Sys.catch_break false;
	List.iter (fun f -> f()) (List.rev com.callbacks#get_after_generation);
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
	| Abort ->
		()
	| Error.Fatal_error (m,p) ->
		error ctx m p
	| Common.Abort (m,p) ->
		error ctx m p
	| Lexer.Error (m,p) ->
		error ctx (Lexer.error_msg m) p
	| Parser.Error (m,p) ->
		error ctx (Parser.error_msg m) p
	| Typecore.Forbid_package ((pack,m,p),pl,pf)  ->
		if !Parser.display_mode <> DMNone && ctx.has_next then begin
			ctx.has_error <- false;
			ctx.messages <- [];
		end else begin
			error ctx (Printf.sprintf "You cannot access the %s package while %s (for %s)" pack (if pf = "macro" then "in a macro" else "targeting " ^ pf) (s_type_path m) ) p;
			List.iter (error ctx (Error.compl_msg "referenced here")) (List.rev pl);
		end
	| Error.Error (m,p) ->
		error ctx (Error.error_msg m) p
	| Generic.Generic_Exception(m,p) ->
		error ctx m p
	| Arg.Bad msg ->
		error ctx ("Error: " ^ msg) null_pos
	| Failure msg when not Helper.is_debug_run ->
		error ctx ("Error: " ^ msg) null_pos
	| Helper.HelpMessage msg ->
		com.info msg null_pos
	| Parser.TypePath (p,c,is_import,pos) ->
		DisplayOutput.handle_type_path_exception ctx p c is_import pos
	| Parser.SyntaxCompletion(kind,subj) ->
		DisplayOutput.handle_syntax_completion com kind subj;
		error ctx ("Error: No completion point was found") null_pos
	| DisplayException.DisplayException dex ->
		DisplayOutput.handle_display_exception ctx dex
	| Out_of_memory | EvalExceptions.Sys_exit _ | Hlinterp.Sys_exit _ | DisplayProcessingGlobals.Completion _ as exc ->
		(* We don't want these to be caught by the catchall below *)
		raise exc
	| e when (try Sys.getenv "OCAMLRUNPARAM" <> "b" with _ -> true) && not Helper.is_debug_run ->
		error ctx (Printexc.to_string e) null_pos

let finalize ctx =
	ctx.comm.flush ctx;
	(* In server mode any open libs are closed by the lib_build_task. In offline mode
		we should do it here to be safe. *)
	if not ctx.comm.is_server then begin
		List.iter (fun lib -> lib#close) ctx.com.native_libs.java_libs;
		List.iter (fun lib -> lib#close) ctx.com.native_libs.net_libs;
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
		| EvalExceptions.Sys_exit i | Hlinterp.Sys_exit i ->
			if i <> 0 then ctx.has_error <- true;
			finalize ctx;
			i

let process_actx ctx actx =
	DisplayProcessing.process_display_arg ctx actx;
	List.iter (fun s ->
		ctx.com.warning WDeprecated [] s null_pos
	) actx.deprecations

let compile_ctx callbacks ctx =
	let run ctx =
		callbacks.before_anything ctx;
		Setup.setup_common_context ctx;
		compile_safe ctx (fun () ->
			let actx = Args.parse_args ctx.com in
			process_actx ctx actx;
			callbacks.after_arg_parsing ctx;
			compile ctx actx;
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
	com = Common.create compilation_step cs version params;
	messages = [];
	has_next = false;
	has_error = false;
	comm = comm;
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
					"-cp" :: l :: acc
				else match (try ExtString.String.split l " " with _ -> l, "") with
				| ("-L",dir) ->
					"--neko-lib" :: (String.sub l 3 (String.length l - 3)) :: acc
				| param, value ->
					let acc = if value <> "" then value :: acc else acc in
					let acc = param :: acc in
					acc
			) [] (List.rev lines) in
			lines

	(* Returns a list of contexts, but doesn't do anything yet *)
	let process_params server_api create each_params has_display is_server pl =
		let curdir = Unix.getcwd () in
		let added_libs = Hashtbl.create 0 in
		let server_mode = ref SMNone in
		let create_context args =
			let ctx = create (server_api.on_context_create()) args in
			(* --cwd triggers immediately, so let's reset *)
			Unix.chdir curdir;
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
				[],Some (create_context (!each_params @ (List.rev acc)))
			| "--next" :: l when acc = [] -> (* skip empty --next *)
				loop [] l
			| "--next" :: l ->
				let ctx = create_context (!each_params @ (List.rev acc)) in
				ctx.has_next <- true;
				l,Some ctx
			| "--each" :: l ->
				each_params := List.rev acc;
				loop [] l
			| "--cwd" :: dir :: l | "-C" :: dir :: l ->
				(* we need to change it immediately since it will affect hxml loading *)
				(try Unix.chdir dir with _ -> raise (Arg.Bad ("Invalid directory: " ^ dir)));
				(* Push the --cwd arg so the arg processor know we did something. *)
				loop (dir :: "--cwd" :: acc) l
			| "--connect" :: hp :: l ->
				if is_server then
					(* If we are already connected, ignore (issue #10813) *)
					loop acc l
				else begin
					let host, port = (try ExtString.String.split hp ":" with _ -> "127.0.0.1", hp) in
					server_api.do_connect host (try int_of_string port with _ -> raise (Arg.Bad "Invalid port")) ((List.rev acc) @ l);
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
				let ctx = create_context (!each_params @ (List.rev acc)) in
				ctx.com.sys_args <- args;
				[],Some ctx
			| ("-L" | "--library" | "-lib") :: name :: args ->
				let libs,args = find_subsequent_libs [name] args in
				let libs = List.filter (fun l -> not (Hashtbl.mem added_libs l)) libs in
				List.iter (fun l -> Hashtbl.add added_libs l ()) libs;
				let lines = add_libs libs pl server_api.cache has_display in
				loop acc (lines @ args)
			| ("--jvm" | "--java" | "-java" as arg) :: dir :: args ->
				loop_lib arg dir "hxjava" acc args
			| ("--cs" | "-cs" as arg) :: dir :: args ->
				loop_lib arg dir "hxcs" acc args
			| arg :: l ->
				match List.rev (ExtString.String.nsplit arg ".") with
				| "hxml" :: _ :: _ when (match acc with "-cmd" :: _ | "--cmd" :: _ -> false | _ -> true) ->
					let acc, l = (try acc, Helper.parse_hxml arg @ l with Not_found -> (arg ^ " (file not found)") :: acc, l) in
					loop acc l
				| _ ->
					loop (arg :: acc) l
		and loop_lib arg dir lib acc args =
			loop (dir :: arg :: acc) ("-lib" :: lib :: args)
		in
		let args,ctx = loop [] pl in
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
					execute_ctx server_api ctx server_mode
				| None ->
					(* caused by --connect *)
					0
			in
			if code = 0 && args <> [] then
				loop args
			else
				code
		in
		let code = loop args in
		comm.exit code
end