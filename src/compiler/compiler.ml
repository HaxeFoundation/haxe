open Extlib_leftovers
open Globals
open Common
open CompilationContext
open Type
open DisplayException
open DisplayTypes.CompletionResultKind

exception Abort

let message ctx msg =
	ctx.messages <- msg :: ctx.messages

let error ctx msg p =
	message ctx (CMError(msg,p));
	ctx.has_error <- true

let delete_file f = try Sys.remove f with _ -> ()

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
			ctx.on_exit <- (fun () ->
				com.native_libs.net_libs <- [];
			) :: ctx.on_exit;
			Dotnet.before_generate com;
			add_std "cs"; "cs"
		| Java ->
			ctx.on_exit <- (fun () ->
				List.iter (fun java_lib -> java_lib#close) com.native_libs.java_libs;
				com.native_libs.java_libs <- [];
			) :: ctx.on_exit;
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

let process_display_configuration ctx =
	let com = ctx.com in
	if com.display.dms_kind <> DMNone then begin
		com.warning <-
			if com.diagnostics <> None then
				(fun w options s p ->
					match Warning.get_mode w (com.warning_options @ options) with
					| WMEnable ->
						add_diagnostics_message com s p DKCompilerError DisplayTypes.DiagnosticsSeverity.Warning
					| WMDisable ->
						()
				)
			else
				(fun w options msg p ->
					match Warning.get_mode w (com.warning_options @ options) with
					| WMEnable ->
						message ctx (CMWarning(msg,p))
					| WMDisable ->
						()
				);
		com.error <- error ctx;
	end;
	Lexer.old_format := Common.defined com Define.OldErrorFormat;
	if !Lexer.old_format && !Parser.in_display then begin
		let p = DisplayPosition.display_position#get in
		(* convert byte position to utf8 position *)
		try
			let content = Std.input_file ~bin:true (Path.get_real_path p.pfile) in
			let pos = UTF8.length (String.sub content 0 p.pmin) in
			DisplayPosition.display_position#set { p with pmin = pos; pmax = pos }
		with _ ->
			() (* ignore *)
	end

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

let load_display_module_in_macro tctx display_file_dot_path clear = match display_file_dot_path with
	| Some cpath ->
		let p = null_pos in
		begin try
			let open Typecore in
			let _, mctx = MacroContext.get_macro_context tctx p in
			(* Tricky stuff: We want to remove the module from our lookups and load it again in
				display mode. This covers some cases like --macro typing it in non-display mode (issue #7017). *)
			if clear then begin
				begin try
					let m = Hashtbl.find mctx.g.modules cpath in
					Hashtbl.remove mctx.g.modules cpath;
					Hashtbl.remove mctx.g.types_module cpath;
					List.iter (fun mt ->
						let ti = t_infos mt in
						Hashtbl.remove mctx.g.modules ti.mt_path;
						Hashtbl.remove mctx.g.types_module ti.mt_path;
					) m.m_types
				with Not_found ->
					()
				end;
			end;
			let _ = MacroContext.load_macro_module tctx cpath true p in
			Finalization.finalize mctx;
			Some mctx
		with DisplayException _ | Parser.TypePath _ as exc ->
			raise exc
		| _ ->
			None
		end
	| None ->
		None

let emit_diagnostics ctx =
	let dctx = Diagnostics.run ctx.com in
	let s = Json.string_of_json (DiagnosticsPrinter.json_of_diagnostics ctx.com dctx) in
	DisplayPosition.display_position#reset;
	raise (DisplayOutput.Completion s)

let run_or_diagnose ctx f arg =
	let com = ctx.com in
	let handle_diagnostics msg p kind =
		ctx.has_error <- true;
		add_diagnostics_message com msg p kind DisplayTypes.DiagnosticsSeverity.Error;
		emit_diagnostics ctx
	in
	if com.diagnostics <> None then begin try
			f arg
		with
		| Error.Error(msg,p) ->
			handle_diagnostics (Error.error_msg msg) p DisplayTypes.DiagnosticsKind.DKCompilerError
		| Parser.Error(msg,p) ->
			handle_diagnostics (Parser.error_msg msg) p DisplayTypes.DiagnosticsKind.DKParserError
		| Lexer.Error(msg,p) ->
			handle_diagnostics (Lexer.error_msg msg) p DisplayTypes.DiagnosticsKind.DKParserError
		end
	else
		f arg

(** Creates the typer context and types [classes] into it. *)
let do_type ctx tctx actx =
	let com = tctx.Typecore.com in
	let t = Timer.timer ["typing"] in
	Option.may (fun cs -> CommonCache.maybe_add_context_sign cs com "before_init_macros") (CompilationServer.get ());
	com.stage <- CInitMacrosStart;
	List.iter (MacroContext.call_init_macro tctx) (List.rev actx.config_macros);
	com.stage <- CInitMacrosDone;
	CommonCache.lock_signature com "after_init_macros";
	List.iter (fun f -> f ()) (List.rev com.callbacks#get_after_init_macros);
	run_or_diagnose ctx (fun () ->
		if com.display.dms_kind <> DMNone then Option.may (DisplayTexpr.check_display_file tctx) (CompilationServer.get ());
		List.iter (fun cpath -> ignore(tctx.Typecore.g.Typecore.do_load_module tctx cpath null_pos)) (List.rev actx.classes);
		Finalization.finalize tctx;
	) ();
	com.stage <- CTypingDone;
	(* If we are trying to find references, let's syntax-explore everything we know to check for the
		identifier we are interested in. We then type only those modules that contain the identifier. *)
	begin match !CompilationServer.instance,com.display.dms_kind with
		| Some cs,(DMUsage _ | DMImplementation) -> FindReferences.find_possible_references tctx cs;
		| _ -> ()
	end;
	t()

let handle_display ctx tctx display_file_dot_path =
	let com = ctx.com in
	if not ctx.com.display.dms_display && ctx.has_error then raise Abort;
	begin match ctx.com.display.dms_kind,!Parser.delayed_syntax_completion with
		| DMDefault,Some(kind,subj) -> DisplayOutput.handle_syntax_completion com kind subj
		| _ -> ()
	end;
	if ctx.com.display.dms_exit_during_typing then begin
		if ctx.has_next || ctx.has_error then raise Abort;
		(* If we didn't find a completion point, load the display file in macro mode. *)
		if com.display_information.display_module_has_macro_defines then
			ignore(load_display_module_in_macro tctx display_file_dot_path true);
		let no_completion_point_found = "No completion point was found" in
		match com.json_out with
		| Some _ -> (match ctx.com.display.dms_kind with
			| DMDefault -> raise (DisplayException(DisplayFields None))
			| DMSignature -> raise (DisplayException(DisplaySignatures None))
			| DMHover -> raise (DisplayException(DisplayHover None))
			| DMDefinition | DMTypeDefinition -> raise_positions []
			| _ -> failwith no_completion_point_found)
		| None ->
			failwith no_completion_point_found;
	end

let filter ctx tctx display_file_dot_path =
	let com = ctx.com in
	com.stage <- CFilteringStart;
	let t = Timer.timer ["filters"] in
	let main, types, modules = run_or_diagnose ctx Finalization.generate tctx in
	com.main <- main;
	com.types <- types;
	com.modules <- modules;
	(* Special case for diagnostics: We don't want to load the display file in macro mode because there's a chance it might not be
		macro-compatible. This means that we might some macro-specific diagnostics, but I don't see what we could do about that. *)
	let should_load_in_macro =
		(* Special case for the special case: If the display file has a block which becomes active if `macro` is defined, we can safely
		   type the module in macro context. (#8682). *)
		ctx.com.diagnostics = None || com.display_information.display_module_has_macro_defines
	in
	if ctx.com.display.dms_force_macro_typing && should_load_in_macro then begin
		match load_display_module_in_macro  tctx display_file_dot_path false with
		| None -> ()
		| Some mctx ->
			(* We don't need a full macro flush here because we're not going to run any macros. *)
			let _, types, modules = Finalization.generate mctx in
			mctx.Typecore.com.types <- types;
			mctx.Typecore.com.Common.modules <- modules
	end;
	DisplayOutput.process_global_display_mode com tctx;
	if com.diagnostics <> None then emit_diagnostics ctx;
	DeprecationCheck.run com;
	Filters.run com tctx main;
	t()

let check_auxiliary_output com actx =
	begin match actx.xml_out with
		| None -> ()
		| Some "hx" ->
			Genhxold.generate com
		| Some file ->
			Common.log com ("Generating xml: " ^ file);
			Path.mkdir_from_path file;
			Genxml.generate com file
	end;
	begin match actx.json_out with
		| None -> ()
		| Some file ->
			Common.log com ("Generating json : " ^ file);
			Path.mkdir_from_path file;
			Genjson.generate com.types file
	end

let generate tctx ext actx =
	let com = tctx.Typecore.com in
	(* check file extension. In case of wrong commandline, we don't want
		to accidentaly delete a source file. *)
	if file_extension com.file = ext then delete_file com.file;
	if com.platform = Flash || com.platform = Cpp || com.platform = Hl then List.iter (Codegen.fix_overrides com) com.types;
	if Common.defined com Define.Dump then begin
		Codegen.Dump.dump_types com;
		Option.may Codegen.Dump.dump_types (com.get_macros())
	end;
	if Common.defined com Define.DumpDependencies then begin
		Codegen.Dump.dump_dependencies com;
		if not tctx.Typecore.in_macro then match tctx.Typecore.g.Typecore.macros with
			| None -> ()
			| Some(_,ctx) -> Codegen.Dump.dump_dependencies ~target_override:(Some "macro") ctx.Typecore.com
	end;
	begin match com.platform with
		| Neko | Hl | Eval when actx.interp -> ()
		| Cpp when Common.defined com Define.Cppia -> ()
		| Cpp | Cs | Php -> Path.mkdir_from_path (com.file ^ "/.")
		| Java when not actx.jvm_flag -> Path.mkdir_from_path (com.file ^ "/.")
		| _ -> Path.mkdir_from_path com.file
	end;
	if actx.interp then
		Std.finally (Timer.timer ["interp"]) MacroContext.interpret tctx
	else if com.platform = Cross then
		()
	else begin
		let generate,name = match com.platform with
		| Flash ->
			Genswf.generate actx.swf_header,"swf"
		| Neko ->
			Genneko.generate,"neko"
		| Js ->
			Genjs.generate,"js"
		| Lua ->
			Genlua.generate,"lua"
		| Php ->
			Genphp7.generate,"php"
		| Cpp ->
			Gencpp.generate,"cpp"
		| Cs ->
			Gencs.generate,"cs"
		| Java ->
			if Common.defined com Jvm then
				Genjvm.generate actx.jvm_flag,"java"
			else
				Genjava.generate,"java"
		| Python ->
			Genpy.generate,"python"
		| Hl ->
			Genhl.generate,"hl"
		| Eval ->
			(fun _ -> MacroContext.interpret tctx),"eval"
		| Cross ->
			die "" __LOC__
		in
		Common.log com ("Generating " ^ name ^ ": " ^ com.file);
		let t = Timer.timer ["generate";name] in
		generate com;
		t()
	end

let run_command ctx cmd =
	let t = Timer.timer ["command"] in
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
	com.info <- (fun msg p -> message ctx (CMInfo(msg,p)));
	com.warning <- (fun w options msg p ->
		match Warning.get_mode w (com.warning_options @ options) with
		| WMEnable ->
			message ctx (CMWarning(msg,p))
		| WMDisable ->
			()
	);
	com.error <- error ctx;
	let filter_messages = (fun keep_errors predicate -> (List.filter (fun msg ->
		(match msg with
		| CMError(_,_) -> keep_errors;
		| CMInfo(_,_) | CMWarning(_,_) -> predicate msg;)
	) (List.rev ctx.messages))) in
	com.get_messages <- (fun () -> (List.map (fun msg ->
		(match msg with
		| CMError(_,_) -> die "" __LOC__;
		| CMInfo(_,_) | CMWarning(_,_) -> msg;)
	) (filter_messages false (fun _ -> true))));
	com.filter_messages <- (fun predicate -> (ctx.messages <- (List.rev (filter_messages true predicate))));
	if CompilationServer.runs() then com.run_command <- run_command ctx;
	com.class_path <- get_std_class_paths ();
	com.std_path <- List.filter (fun p -> ExtString.String.ends_with p "std/" || ExtString.String.ends_with p "std\\") com.class_path

let compile ctx actx =
	let com = ctx.com in
	(* Set up display configuration *)
	process_display_configuration ctx;
	let display_file_dot_path = DisplayOutput.process_display_file com actx in
	(* Initialize target: This allows access to the appropriate std packages and sets the -D defines. *)
	let ext = initialize_target ctx com actx in
	(* if we are at the last compilation step, allow all packages accesses - in case of macros or opening another project file *)
	if com.display.dms_display then begin match com.display.dms_kind with
		| DMDefault | DMUsage _ -> ()
		| _ -> if not ctx.has_next then com.package_rules <- PMap.foldi (fun p r acc -> match r with Forbidden -> acc | _ -> PMap.add p r acc) com.package_rules PMap.empty;
	end;
	com.config <- get_config com; (* make sure to adapt all flags changes defined after platform *)
	let t = Timer.timer ["init"] in
	List.iter (fun f -> f()) (List.rev (actx.pre_compilation));
	t();
	com.stage <- CInitialized;
	if actx.classes = [([],"Std")] && not actx.force_typing then begin
		if actx.cmds = [] && not actx.did_something then actx.raise_usage();
	end else begin
		(* Actual compilation starts here *)
		let tctx = create_typer_context ctx actx.native_libs in
		com.stage <- CTyperCreated;
		let display_file_dot_path = match display_file_dot_path with
			| DPKMacro path ->
				ignore(load_display_module_in_macro tctx (Some path) true);
				Some path
			| DPKNormal path ->
				Some path
			| DPKNone ->
				None
			| DPKDirect file ->
				DisplayOutput.load_display_file_standalone tctx file;
				None
			| DPKInput input ->
				DisplayOutput.load_display_content_standalone tctx input;
				None
		in
		begin try
			do_type ctx tctx actx
		with TypeloadParse.DisplayInMacroBlock ->
			ignore(load_display_module_in_macro tctx display_file_dot_path true);
		end;
		handle_display ctx tctx display_file_dot_path;
		filter ctx tctx display_file_dot_path;
		if ctx.has_error then raise Abort;
		check_auxiliary_output com actx;
		com.stage <- CGenerationStart;
		if not actx.no_output then generate tctx ext actx;
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

let finalize ctx =
	List.iter (fun f ->
		f();
	) ctx.on_exit;
	ctx.comm.flush ctx

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
	| DisplayException(DisplayHover _ | DisplayPositions _ | DisplayFields _ | DisplayPackage _  | DisplaySignatures _ as de) when ctx.com.json_out <> None ->
		begin
			DisplayPosition.display_position#reset;
			match ctx.com.json_out with
			| Some api ->
				let ctx = DisplayJson.create_json_context api.jsonrpc (match de with DisplayFields _ -> true | _ -> false) in
				api.send_result (DisplayException.to_json ctx de)
			| _ -> die "" __LOC__
		end
	(* | Parser.TypePath (_,_,_,p) when ctx.com.json_out <> None ->
		begin match com.json_out with
		| Some (f,_) ->
			let tctx = Typer.create ctx.com in
			let fields = DisplayToplevel.collect tctx true Typecore.NoValue in
			let jctx = Genjson.create_context Genjson.GMMinimum in
			f (DisplayException.fields_to_json jctx fields CRImport (Some (Parser.cut_pos_at_display p)) false)
		| _ -> die "" __LOC__
		end *)
	| DisplayException(DisplayPackage pack) ->
		DisplayPosition.display_position#reset;
		raise (DisplayOutput.Completion (String.concat "." pack))
	| DisplayException(DisplayFields Some r) ->
		DisplayPosition.display_position#reset;
		let fields = if !Timer.measure_times then begin
			Timer.close_times();
			(List.map (fun (name,value) ->
				CompletionItem.make_ci_timer ("@TIME " ^ name) value
			) (DisplayOutput.get_timer_fields !Helper.start_time)) @ r.fitems
		end else
			r.fitems
		in
		let s = match r.fkind with
			| CRToplevel _
			| CRTypeHint
			| CRExtends
			| CRImplements
			| CRStructExtension _
			| CRImport
			| CRUsing
			| CRNew
			| CRPattern _
			| CRTypeRelation
			| CRTypeDecl ->
				DisplayOutput.print_toplevel fields
			| CRField _
			| CRStructureField
			| CRMetadata
			| CROverride ->
				DisplayOutput.print_fields fields
		in
		raise (DisplayOutput.Completion s)
	| DisplayException(DisplayHover Some ({hitem = {CompletionItem.ci_type = Some (t,_)}} as hover)) ->
		DisplayPosition.display_position#reset;
		let doc = CompletionItem.get_documentation hover.hitem in
		raise (DisplayOutput.Completion (DisplayOutput.print_type t hover.hpos doc))
	| DisplayException(DisplaySignatures Some (signatures,_,display_arg,_)) ->
		DisplayPosition.display_position#reset;
		if ctx.com.display.dms_kind = DMSignature then
			raise (DisplayOutput.Completion (DisplayOutput.print_signature signatures display_arg))
		else
			raise (DisplayOutput.Completion (DisplayOutput.print_signatures signatures))
	| DisplayException(DisplayPositions pl) ->
		DisplayPosition.display_position#reset;
		raise (DisplayOutput.Completion (DisplayOutput.print_positions pl))
	| Parser.TypePath (p,c,is_import,pos) ->
		let fields =
			try begin match c with
				| None ->
					DisplayPath.TypePathHandler.complete_type_path com p
				| Some (c,cur_package) ->
					let ctx = Typer.create com in
					DisplayPath.TypePathHandler.complete_type_path_inner ctx p c cur_package is_import
			end with Common.Abort(msg,p) ->
				error ctx msg p;
				None
		in
		begin match ctx.com.json_out,fields with
		| None,None ->
			()
		| None,Some fields ->
			raise (DisplayOutput.Completion (DisplayOutput.print_fields fields))
		| Some api,None when is_legacy_completion com ->
			api.send_result JNull
		| Some api,fields ->
			let fields = Option.default [] fields in
			let ctx = DisplayJson.create_json_context api.jsonrpc false in
			let path = match List.rev p with
				| name :: pack -> List.rev pack,name
				| [] -> [],""
			in
			let kind = CRField ((CompletionItem.make_ci_module path,pos,None,None)) in
			api.send_result (DisplayException.fields_to_json ctx fields kind (DisplayTypes.make_subject None pos));
		end
	| Parser.SyntaxCompletion(kind,subj) ->
		DisplayOutput.handle_syntax_completion com kind subj;
		error ctx ("Error: No completion point was found") null_pos
	| DisplayException(ModuleSymbols s | Statistics s | Metadata s) ->
		DisplayPosition.display_position#reset;
		raise (DisplayOutput.Completion s)
	| EvalExceptions.Sys_exit i | Hlinterp.Sys_exit i ->
		finalize ctx;
		if !Timer.measure_times then Timer.report_times prerr_endline;
		exit i
	| DisplayOutput.Completion _ as exc ->
		raise exc
	| Out_of_memory as exc ->
		raise exc
	| e when (try Sys.getenv "OCAMLRUNPARAM" <> "b" || CompilationServer.runs() with _ -> true) && not Helper.is_debug_run ->
		error ctx (Printexc.to_string e) null_pos

let compile_ctx server_api comm ctx =
	let run ctx =
		server_api.before_anything ctx;
		setup_common_context ctx;
		compile_safe ctx (fun () ->
			let actx = Args.parse_args ctx.com in
			begin match actx.server_mode with
			| SMListen hp ->
				let accept = match hp with
				| "stdio" ->
					server_api.init_wait_stdio()
				| _ ->
					let host, port = Helper.parse_host_port hp in
					server_api.init_wait_socket host port
				in
				server_api.wait_loop ctx.com.verbose accept
			| SMConnect hp ->
				let host, port = Helper.parse_host_port hp in
				let accept = server_api.init_wait_connect host port in
				server_api.wait_loop ctx.com.verbose accept
			| SMNone ->
				()
			end;
			server_api.after_arg_parsing ctx;
			compile ctx actx;
		);
		finalize ctx;
		server_api.after_compilation ctx;
	in
	try
		if ctx.has_error then begin
			finalize ctx;
			false (* can happen if process_params above fails already *)
		end else begin
			run ctx;
			true (* reads as "continue?" *)
		end
	with
		| DisplayOutput.Completion str ->
			server_api.after_compilation ctx;
			ServerMessage.completion str;
			comm.write_err str;
			false
		| Arg.Bad msg ->
			error ctx ("Error: " ^ msg) null_pos;
			false

let create_context comm params = {
	com = Common.create version params;
	on_exit = [];
	messages = [];
	has_next = false;
	has_error = false;
	comm = comm;
}

module HighLevel = struct
	(* Returns a list of contexts, but doesn't do anything yet *)
	let process_params server_api create pl =
		let each_params = ref [] in
		let compilations = DynArray.create () in
		let curdir = Unix.getcwd () in
		let add_context args =
			let ctx = create args in
			(* --cwd triggers immediately, so let's reset *)
			Unix.chdir curdir;
			DynArray.add compilations ctx;
			ctx;
		in
		let rec loop acc = function
			| [] ->
				ignore(add_context (!each_params @ (List.rev acc)));
			| "--next" :: l when acc = [] -> (* skip empty --next *)
				loop [] l
			| "--next" :: l ->
				let ctx = add_context (!each_params @ (List.rev acc)) in
				ctx.has_next <- true;
				loop [] l
			| "--each" :: l ->
				each_params := List.rev acc;
				loop [] l
			| "--cwd" :: dir :: l | "-C" :: dir :: l ->
				(* we need to change it immediately since it will affect hxml loading *)
				(try Unix.chdir dir with _ -> raise (Arg.Bad ("Invalid directory: " ^ dir)));
				(* Push the --cwd arg so the arg processor know we did something. *)
				loop (dir :: "--cwd" :: acc) l
			| "--connect" :: hp :: l ->
				let host, port = (try ExtString.String.split hp ":" with _ -> "127.0.0.1", hp) in
				server_api.do_connect host (try int_of_string port with _ -> raise (Arg.Bad "Invalid port")) ((List.rev acc) @ l)
			| "--run" :: cl :: args ->
				let acc = cl :: "-x" :: acc in
				let ctx = add_context (!each_params @ (List.rev acc)) in
				ctx.com.sys_args <- args;
			| arg :: l ->
				match List.rev (ExtString.String.nsplit arg ".") with
				| "hxml" :: _ when (match acc with "-cmd" :: _ | "--cmd" :: _ -> false | _ -> true) ->
					let acc, l = (try acc, Helper.parse_hxml arg @ l with Not_found -> (arg ^ " (file not found)") :: acc, l) in
					loop acc l
				| _ -> loop (arg :: acc) l
		in
		(* put --display in front if it was last parameter *)
		let pl = (match List.rev pl with
			| file :: "--display" :: pl when file <> "memory" -> "--display" :: file :: List.rev pl
			| _ -> pl
		) in
		loop [] pl;
		DynArray.to_list compilations

	let entry server_api comm args =
		let ctxs = try
			process_params server_api (create_context comm) args
		with Arg.Bad msg ->
			let ctx = create_context comm args in
			error ctx ("Error: " ^ msg) null_pos;
			[ctx]
		in
		let success = List.fold_left (fun b ctx -> b && compile_ctx server_api comm ctx) true ctxs in
		if success then begin
			Timer.close_times();
			if !Timer.measure_times then Timer.report_times (fun s -> comm.write_err (s ^ "\n"));
		end;
end