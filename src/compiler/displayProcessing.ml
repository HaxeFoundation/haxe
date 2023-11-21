open Globals
open Common
open CompilationContext
open DisplayProcessingGlobals

type display_path_kind =
	| DPKNormal of path
	| DPKMacro of path
	| DPKDirect of string
	| DPKInput of string
	| DPKNone

(* 1. Argument processing from --display *)

let handle_display_argument_old com file_pos actx =
	match file_pos with
	| "classes" ->
		actx.pre_compilation <- (fun() -> raise (Parser.TypePath (["."],None,true,null_pos))) :: actx.pre_compilation;
	| "keywords" ->
		raise (Completion (DisplayOutput.print_keywords ()))
	| "memory" ->
		actx.did_something <- true;
		(try Memory.display_memory com with e -> prerr_endline (Printexc.get_backtrace ()));
	| "diagnostics" ->
		com.report_mode <- RMDiagnostics []
	| _ ->
		let file, pos = try ExtString.String.split file_pos "@" with _ -> failwith ("Invalid format: " ^ file_pos) in
		let file = Helper.unquote file in
		let file_unique = com.file_keys#get file in
		let pos, smode = try ExtString.String.split pos "@" with _ -> pos,"" in
		let create mode =
			Parser.display_mode := mode;
			DisplayTypes.DisplayMode.create mode
		in
		let dm = match smode with
			| "position" ->
				create DMDefinition
			| "usage" ->
				create (DMUsage (false,false,false))
			| "package" ->
				create DMPackage
			| "type" ->
				create DMHover
			| "toplevel" ->
				create DMDefault
			| "module-symbols" ->
				create (DMModuleSymbols None)
			| "diagnostics" ->
				com.report_mode <- RMDiagnostics [file_unique];
				let dm = create DMNone in
				{dm with dms_display_file_policy = DFPAlso; dms_per_file = true; dms_populate_cache = !ServerConfig.populate_cache_from_display}
			| "statistics" ->
				com.report_mode <- RMStatistics;
				let dm = create DMNone in
				{dm with dms_display_file_policy = DFPAlso; dms_error_policy = EPIgnore; dms_per_file = true}
			| "signature" ->
				create DMSignature
			| "" ->
				create DMDefault
			| _ ->
				let smode,arg = try ExtString.String.split smode "@" with _ -> pos,"" in
				match smode with
					| "workspace-symbols" ->
						create (DMModuleSymbols (Some arg))
					| _ ->
						create DMDefault
		in
		let pos = try int_of_string pos with _ -> failwith ("Invalid format: "  ^ pos) in
		com.display <- dm;
		if not com.display.dms_full_typing then Common.define_value com Define.Display (if smode <> "" then smode else "1");
		DisplayPosition.display_position#set {
			pfile = Path.get_full_path file;
			pmin = pos;
			pmax = pos;
		}

let process_display_arg ctx actx =
	match actx.display_arg with
	| Some input ->
		let input = String.trim input in
		if String.length input > 0 && (input.[0] = '[' || input.[0] = '{') then begin
			actx.did_something <- true;
			actx.force_typing <- true;
			DisplayJson.parse_input ctx.com input Timer.measure_times
		end else
			handle_display_argument_old ctx.com input actx;
	| None ->
		()

(* 2. Compilation start, setup display configuration in context *)

let process_display_configuration ctx =
	let com = ctx.com in
	if is_diagnostics com then begin
		com.info <- (fun ?depth ?from_macro s p ->
			add_diagnostics_message ?depth com s p DKCompilerMessage Information
		);
		com.warning <- (fun ?(depth = 0) ?from_macro w options s p ->
			match Warning.get_mode w (com.warning_options @ options) with
			| WMEnable ->
				let wobj = Warning.warning_obj w in
				add_diagnostics_message ~depth ~code:(Some wobj.w_name) com s p DKCompilerMessage Warning
			| WMDisable ->
				()
		);
	end;
	Lexer.old_format := Common.defined com Define.OldErrorFormat;
	if !Lexer.old_format && !Parser.in_display then begin
		let p = DisplayPosition.display_position#get in
		(* convert byte position to utf8 position *)
		try
			let content = Std.input_file ~bin:true (Path.get_real_path p.pfile) in
			let pos = Extlib_leftovers.UTF8.length (String.sub content 0 p.pmin) in
			DisplayPosition.display_position#set { p with pmin = pos; pmax = pos }
		with _ ->
			() (* ignore *)
	end

let process_display_file com actx =
	let get_module_path_from_file_path com spath =
		let rec loop = function
			| [] -> None
			| cp :: l ->
				let cp = (if cp = "" then "./" else cp) in
				let c = Path.add_trailing_slash (Path.get_real_path cp) in
				let clen = String.length c in
				if clen < String.length spath && String.sub spath 0 clen = c then begin
					let path = String.sub spath clen (String.length spath - clen) in
					(try
						let path = Path.parse_path path in
						(match loop l with
						| Some x as r when String.length (s_type_path x) < String.length (s_type_path path) -> r
						| _ -> Some path)
					with _ -> loop l)
				end else
					loop l
		in
		loop com.class_path
	in
	match com.display.dms_display_file_policy with
		| DFPNo ->
			DPKNone
		| DFPOnly when (DisplayPosition.display_position#get).pfile = file_input_marker ->
			actx.classes <- [];
			com.main_class <- None;
			begin match !TypeloadParse.current_stdin with
			| Some input ->
				TypeloadParse.current_stdin := None;
				DPKInput input
			| None ->
				DPKNone
			end
		| dfp ->
			if dfp = DFPOnly then begin
				actx.classes <- [];
				com.main_class <- None;
			end;
			let real = Path.get_real_path (DisplayPosition.display_position#get).pfile in
			let path = match get_module_path_from_file_path com real with
			| Some path ->
				if com.display.dms_kind = DMPackage then DisplayException.raise_package (fst path);
				let path = match ExtString.String.nsplit (snd path) "." with
					| [name;"macro"] ->
						(* If we have a .macro.hx path, don't add the file to classes because the compiler won't find it.
						   This can happen if we're completing in such a file. *)
						DPKMacro (fst path,name)
					| [name] ->
						actx.classes <- path :: actx.classes;
						DPKNormal path
					| [name;target] ->
						let path = fst path, name in
						actx.classes <- path :: actx.classes;
						DPKNormal path
					| e ->
						die "" __LOC__
				in
				path
			| None ->
				if not (Sys.file_exists real) then failwith "Display file does not exist";
				(match List.rev (ExtString.String.nsplit real Path.path_sep) with
				| file :: _ when file.[0] >= 'a' && file.[0] <= 'z' -> failwith ("Display file '" ^ file ^ "' should not start with a lowercase letter")
				| _ -> ());
				DPKDirect real
			in
			Common.log com ("Display file : " ^ real);
			Common.log com ("Classes found : ["  ^ (String.concat "," (List.map s_type_path actx.classes)) ^ "]");
			path

(* 3. Loaders for display file that might be called *)

let load_display_module_in_macro tctx display_file_dot_path clear = match display_file_dot_path with
	| Some cpath ->
		let p = null_pos in
		begin try
			let open Typecore in
			let mctx = MacroContext.get_macro_context tctx in
			(* Tricky stuff: We want to remove the module from our lookups and load it again in
				display mode. This covers some cases like --macro typing it in non-display mode (issue #7017). *)
			if clear then begin
				begin try
					let m = mctx.com.module_lut#find cpath in
					mctx.com.module_lut#remove cpath;
					mctx.com.type_to_module#remove cpath;
					List.iter (fun mt ->
						let ti = Type.t_infos mt in
						mctx.com.module_lut#remove ti.mt_path;
						mctx.com.type_to_module#remove ti.mt_path;
					) m.m_types
				with Not_found ->
					()
				end;
			end;
			let _ = MacroContext.load_macro_module (MacroContext.get_macro_context tctx) tctx.com cpath true p in
			Finalization.finalize mctx;
			Some mctx
		with DisplayException.DisplayException _ | Parser.TypePath _ as exc ->
			raise exc
		| _ ->
			None
		end
	| None ->
		None

let load_display_file_standalone (ctx : Typecore.typer) file =
	let com = ctx.com in
	let pack,decls = TypeloadParse.parse_module_file com file null_pos in
	let path = Path.FilePath.parse file in
	let name = match path.file_name with
		| None -> "?DISPLAY"
		| Some name -> name
	in
	begin match path.directory with
		| None -> ()
		| Some dir ->
			(* Chop off number of package parts from the dir and use that as class path. *)
			let parts = ExtString.String.nsplit dir (if path.backslash then "\\" else "/") in
			let parts = List.rev (ExtList.List.drop (List.length pack) (List.rev parts)) in
			let dir = ExtString.String.join (if path.backslash then "\\" else "/") parts in
			com.class_path <- dir :: com.class_path
	end;
	ignore(TypeloadModule.type_module ctx (pack,name) file ~dont_check_path:true decls null_pos)

let load_display_content_standalone (ctx : Typecore.typer) input =
	let com = ctx.com in
	let file = file_input_marker in
	let p = {pfile = file; pmin = 0; pmax = 0} in
	let parsed = TypeloadParse.parse_file_from_string com file p input in
	let pack,decls = TypeloadParse.handle_parser_result com p parsed in
	ignore(TypeloadModule.type_module ctx (pack,"?DISPLAY") file ~dont_check_path:true decls p)

(* 4. Display processing before typing *)

let maybe_load_display_file_before_typing tctx display_file_dot_path = match display_file_dot_path with
	| DPKMacro path ->
		ignore(load_display_module_in_macro tctx (Some path) true);
		Some path
	| DPKNormal path ->
		Some path
	| DPKNone ->
		None
	| DPKDirect file ->
		load_display_file_standalone tctx file;
		None
	| DPKInput input ->
		load_display_content_standalone tctx input;
		None

(* 5. Display processing after typing *)

let handle_display_after_typing ctx tctx display_file_dot_path =
	let com = ctx.com in
	if ctx.com.display.dms_kind = DMNone && ctx.has_error then raise Abort;
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
		| Some _ ->
			raise (DisplayException.DisplayException DisplayNoResult)
		| None ->
			failwith no_completion_point_found;
	end

(* 6. Display processing after finalization *)

let promote_type_hints (tctx : Typecore.typer) =
	let open Type in
	let rec explore_type_hint (md,p,t) =
		match t with
		| TMono r -> (match r.tm_type with None -> () | Some t -> explore_type_hint (md,p,t))
		| TLazy f -> explore_type_hint (md,p,lazy_type f)
		| TInst(({cl_name_pos = pn;cl_path = (_,name)}),_)
		| TEnum(({e_name_pos = pn;e_path = (_,name)}),_)
		| TType(({t_name_pos = pn;t_path = (_,name)}),_)
		| TAbstract(({a_name_pos = pn;a_path = (_,name)}),_) ->
			md.m_type_hints <- (p,pn) :: md.m_type_hints;
		| TDynamic _ -> ()
		| TFun _ | TAnon _ -> ()
	in
	List.iter explore_type_hint tctx.g.type_hints

let process_global_display_mode com tctx =
	promote_type_hints tctx;
	match com.display.dms_kind with
	| DMUsage (with_definition,_,_) ->
		FindReferences.find_references tctx com with_definition
	| DMImplementation ->
		FindReferences.find_implementations tctx com
	| DMModuleSymbols filter ->
		let open CompilationCache in
		let cs = com.cs in
		let symbols =
			let l = cs#get_context_files ((Define.get_signature com.defines) :: (match com.get_macros() with None -> [] | Some com -> [Define.get_signature com.defines])) in
			List.fold_left (fun acc (file_key,cfile) ->
				let file = cfile.c_file_path in
				if (filter <> None || DisplayPosition.display_position#is_in_file (com.file_keys#get file)) then
					(file,DocumentSymbols.collect_module_symbols (Some (file,get_module_name_of_cfile file cfile)) (filter = None) (cfile.c_package,cfile.c_decls)) :: acc
				else
					acc
			) [] l
		in
		DisplayException.raise_module_symbols (DocumentSymbols.Printer.print_module_symbols com symbols filter)
	| _ -> ()

let handle_display_after_finalization ctx tctx display_file_dot_path =
	let com = ctx.com in
	(* Special case for diagnostics: We don't want to load the display file in macro mode because there's a chance it might not be
		macro-compatible. This means that we might some macro-specific diagnostics, but I don't see what we could do about that. *)
	let should_load_in_macro =
		(* Special case for the special case: If the display file has a block which becomes active if `macro` is defined, we can safely
			type the module in macro context. (#8682). *)
		not (is_diagnostics com) || com.display_information.display_module_has_macro_defines
	in
	if ctx.com.display.dms_force_macro_typing && should_load_in_macro then begin
		match load_display_module_in_macro tctx display_file_dot_path false with
		| None -> ()
		| Some mctx ->
			(* We don't need a full macro flush here because we're not going to run any macros. *)
			let _, types, modules = Finalization.generate mctx in
			mctx.Typecore.com.types <- types;
			mctx.Typecore.com.Common.modules <- modules
	end;
	process_global_display_mode com tctx;
	begin match com.report_mode with
	| RMDiagnostics _ ->
		DisplayOutput.emit_diagnostics com
	| RMStatistics ->
		DisplayOutput.emit_statistics tctx
	| RMNone ->
		()
	end
