open Globals
open Common
open ParsedArg
open DisplayProcessingGlobals

type display_path_kind =
	| DPKNormal of path
	| DPKMacro of path
	| DPKDirect of string
	| DPKInput of string
	| DPKNone

(* 1. Argument processing from --display *)

let process_display_arg com actx =
	let result_handler = com.request_scope.result_handler in
	result_handler.set_com com

(* 2. Compilation start, setup display configuration in context *)

let process_display_configuration com =
	if is_diagnostics com then begin
		com.info <- (fun ?(depth = 0) s p ->
			add_diagnostics_message ~depth com s p MKInfo
		);
		com.warning <- (fun ?(depth = 0) w options s p ->
			match Warning.get_mode w (options @ com.warning_options) with
			| WMEnable ->
				add_diagnostics_message ~depth com s p (MKWarning(w,options))
			| WMDisable ->
				()
		);
	end

let process_display_file com actx =
	let get_module_path_from_file_path com spath =
		let rec loop = function
			| [] -> None
			| cp :: l ->
				let cp = cp#path in
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
		loop com.class_paths#as_list
	in
	match com.display.dms_display_file_policy with
		| DFPNo ->
			DPKNone
		| DFPOnly when (DisplayPosition.display_position#get).pfile = file_input_marker ->
			actx.classes <- [];
			com.main.main_path <- None;
			begin match com.file_contents with
			| [_, Some input] ->
				com.file_contents <- [];
				DPKInput input
			| _ ->
				DPKNone
			end
		| dfp ->
			if dfp = DFPOnly then begin
				actx.classes <- [];
				com.main.main_path <- None;
			end;
			let dpk = List.map (fun file_key ->
				let real = Path.get_real_path (Path.UniqueKey.to_string file_key) in
				let dpk = match get_module_path_from_file_path com real with
				| Some path ->
					if com.display.dms_kind = DMPackage then DisplayException.raise_package (fst path);
					let dpk = match ExtString.String.nsplit (snd path) "." with
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
						| _ ->
							failwith ("Invalid display file '" ^ real ^ "'")
					in
					dpk
				| None ->
					if not (Sys.file_exists real) then failwith "Display file does not exist";
					(match List.rev (ExtString.String.nsplit real Path.path_sep) with
					| file :: _ when file.[0] >= 'a' && file.[0] <= 'z' -> failwith ("Display file '" ^ file ^ "' should not start with a lowercase letter")
					| _ -> ());
					DPKDirect real
				in
				Common.log com ("Display file : " ^ real);
				dpk
			) DisplayPosition.display_position#get_files in
			Common.log com ("Classes found : ["  ^ (String.concat "," (List.map s_type_path actx.classes)) ^ "]");
			match dpk with
				| [dfile] -> dfile
				| _ -> DPKNone

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
					mctx.com.module_lut#get_type_lut#remove cpath;
					List.iter (fun mt ->
						let ti = Type.t_infos mt in
						mctx.com.module_lut#remove ti.mt_path;
						mctx.com.module_lut#get_type_lut#remove ti.mt_path;
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
	let (pack,decls),_ = TypeloadParse.parse_module_file com (ClassPaths.create_resolved_file file ctx.com.empty_class_path) null_pos in
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
			com.class_paths#add (new ClassPath.directory_class_path dir User)
	end;
	ignore(TypeloadModule.type_module ctx.com ctx.g (pack,name) file ~dont_check_path:true decls null_pos)

let load_display_content_standalone (ctx : Typecore.typer) input =
	let com = ctx.com in
	let file = file_input_marker in
	let p = file_pos file in
	let parsed = TypeloadParse.parse_file_from_string com file p input in
	let (pack,decls),_ = TypeloadParse.handle_parser_result com p parsed in
	ignore(TypeloadModule.type_module ctx.com ctx.g (pack,"?DISPLAY") file ~dont_check_path:true decls p)

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

let handle_display_after_typing com tctx display_file_dot_path =
	if com.display.dms_kind = DMNone && com.part_scope.has_error && Common.is_compilation com then
		true
	else begin
	begin match com.display.dms_kind,Atomic.get com.parser_state.delayed_syntax_completion with
		| DMDefault,Some(kind,subj) -> DisplayOutput.handle_syntax_completion com kind subj
		| _ -> ()
	end;
	if com.display.dms_exit_during_typing then begin
		if com.part_scope.has_next || Common.has_error_to_report com then
			true
		else begin
		(* If we didn't find a completion point, load the display file in macro mode. *)
		if com.parser_state.display_module_has_macro_defines then
			ignore(load_display_module_in_macro tctx display_file_dot_path true);
		raise (DisplayException.DisplayException DisplayNoResult)
		end
	end else
		false
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
		FindReferences.find_references com with_definition
	| DMImplementation ->
		FindReferences.find_implementations com
	| DMModuleSymbols filter ->
		let open CompilationCache in
		let cs = com.cs in
		let symbols =
			let l = cs#get_context_files ((Define.get_signature com.defines) :: (match com.get_macros() with None -> [] | Some com -> [Define.get_signature com.defines])) in
			List.fold_left (fun acc (file_key,cfile) ->
				let file = cfile.c_file_path.file in
				if (filter <> None || DisplayPosition.display_position#is_in_file (com.file_keys#get file)) then
					(file,DocumentSymbols.collect_module_symbols (Some (file,get_module_name_of_cfile file cfile)) (filter = None) (cfile.c_package,cfile.c_decls)) :: acc
				else
					acc
			) [] l
		in
		DisplayException.raise_module_symbols (DocumentSymbols.Printer.json_of_module_symbols com symbols filter)
	| _ -> ()

let handle_display_after_finalization com tctx display_file_dot_path =
	(* Special case for diagnostics: We don't want to load the display file in macro mode because there's a chance it might not be
		macro-compatible. This means that we might some macro-specific diagnostics, but I don't see what we could do about that. *)
	let should_load_in_macro =
		(* Special case for the special case: If the display file has a block which becomes active if `macro` is defined, we can safely
			type the module in macro context. (#8682). *)
		not (is_diagnostics com) || com.parser_state.display_module_has_macro_defines
	in
	if com.display.dms_force_macro_typing && should_load_in_macro then begin
		match load_display_module_in_macro tctx display_file_dot_path false with
		| None -> ()
		| Some mctx ->
			(* We don't need a full macro flush here because we're not going to run any macros. *)
			let _, types, modules = Finalization.generate mctx (Finalization.maybe_load_main mctx) in
			mctx.Typecore.com.types <- types;
			mctx.Typecore.com.Common.modules <- modules
	end;
	process_global_display_mode com tctx;
	begin match com.report_mode with
	| RMDiagnostics _ ->
		DisplayOutput.emit_diagnostics com
	| RMStatistics ->
		DisplayOutput.emit_statistics com
	| RMNone ->
		()
	end
