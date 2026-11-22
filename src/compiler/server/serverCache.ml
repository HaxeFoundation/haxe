open Globals
open Common
open CompilationCache
open Type
open TypeloadCacheHook

exception Dirty of module_skip_reason
exception ServerError of string

let parse_string com file p s =
	match TypeloadParse.parse_file_from_string com file p s with
	| ParseSuccess(data,pdi) -> Parser.ParseSuccess(data,{pdi with pd_digest = Some (Digest.string s)})
	| result -> result

let parse_file sctx com (rfile : ClassPaths.resolved_file) p =
	let cc = CommonCache.get_cache com in
	let file = rfile.file in
	let ffile = Path.get_full_path rfile.file
	and fkey = com.part_scope.file_keys#get file in
	let is_display_file = DisplayPosition.display_position#is_in_file (com.part_scope.file_keys#get ffile) in
	match is_display_file, sctx.ServerCompilationContext.current_stdin with
	| true, Some stdin when (com.file_contents <> [] || Common.defined com Define.DisplayStdin) ->
		parse_string com file p stdin
	| _ ->
		let ftime = file_time ffile in
		let data = Std.finally (Timer.start_timer com.timer_ctx ["server";"parser cache"]) (fun () ->
			try
				let cfile = cc#find_file fkey in
				if cfile.c_time <> ftime || (is_display_file && com.display.dms_kind = DMSignature) then raise Not_found;
				Parser.ParseSuccess((cfile.c_package,cfile.c_decls),cfile.c_pdi)
			with Not_found ->
				let parse_result = parse_string com file p (TypeloadParse.read_file com rfile p) in
				let info,is_unusual = match parse_result with
					| ParseError(_,_,_) -> "not cached, has parse error",true
					| ParseSuccess(data,pdi) ->
						if is_display_file then begin
							if pdi.pd_errors <> [] then
								"not cached, is display file with parse errors",true
							else if com.display.dms_per_file then begin
								cc#cache_file fkey rfile ftime data pdi;
								"cached, is intact display file",true
							end else
								"not cached, is display file",true
						end else begin try
							(* We assume that when not in display mode it's okay to cache stuff that has #if display
							checks. The reasoning is that non-display mode has more information than display mode. *)
							if com.display.dms_full_typing then raise Not_found;
							let ident = ThreadSafeHashtbl.find com.part_scope.parser_state.special_identifier_files fkey in
							Printf.sprintf "not cached, using \"%s\" define" ident,true
						with Not_found ->
							cc#cache_file fkey (ClassPaths.create_resolved_file ffile rfile.class_path) ftime data pdi;
							"cached",false
						end
				in
				if is_unusual then ServerMessage.parsed com "" (ffile,info);
				parse_result
		) () in
		data

open ServerCompilationContext

let stat dir =
	(Unix.stat (Path.remove_trailing_slash dir)).Unix.st_mtime

let absolute_class_paths (com : Common.context) =
	List.map (fun cp -> cp#absolute_path) com.class_paths#as_list

(* The empty class path is the working directory, which usually holds much more
   than modules: its directories are watched by package, see [package_directory_changed]. *)
let watched_class_paths (com : Common.context) =
	List.filter_map (fun cp -> if cp#path = "" then None else Some cp#absolute_path) com.class_paths#as_list

(* Gets a list of changed directories for the current compilation. *)
let get_changed_directories sctx com =
	let cs = sctx.cs in
	let sign = Define.get_signature com.defines in
	let dirs = try
		(* First, check if we already have determined changed directories for current compilation. *)
		Hashtbl.find sctx.changed_directories sign
	with Not_found ->
		let dirs = try
			(* Next, get all directories from the cache and filter the ones that haven't changed. *)
			let all_dirs = cs#find_directories sign in
			let sub_dirs dir =
				List.map Path.add_trailing_slash (Path.find_directories (platform_name com.platform) false [dir.c_path])
			in
			let rec add_new acc dir =
				if not (cs#has_directory sign dir) then begin try
					let time = stat dir in
					ServerMessage.added_directory com "" dir;
					let cdir = CompilationCache.create_directory dir time in
					cs#add_directory sign cdir;
					List.fold_left add_new (cdir :: acc) (sub_dirs cdir)
				with Unix.Unix_error _ ->
					acc
				end else
					acc
			in
			let dirs = List.fold_left (fun acc dir ->
				try
					let time' = stat dir.c_path in
					if dir.c_mtime < time' then begin
						dir.c_mtime <- time';
						let acc = List.fold_left add_new acc (sub_dirs dir) in
						(CompilationCache.create_directory dir.c_path time') :: acc
					end else
						acc
				with Unix.Unix_error _ ->
					cs#remove_directory sign dir.c_path;
					ServerMessage.removed_directory com "" dir.c_path;
					acc
			) [] all_dirs in
			(* A class path directory created since is added like a new subdirectory. *)
			let dirs = List.fold_left add_new dirs (watched_class_paths com) in
			ServerMessage.changed_directories com "" dirs;
			dirs
		with Not_found ->
			(* There were no directories in the cache, so this must be a new context. Let's add
				an empty list to make sure no crazy recursion happens. *)
			cs#add_directories sign [];
			(* Register the delay that is going to populate the cache dirs. *)
			sctx.delays <- (fun () ->
				let dirs = ref [] in
				let add_dir path =
					try
						let time = stat path in
						dirs := CompilationCache.create_directory path time :: !dirs
					with Unix.Unix_error _ ->
						()
				in
				let class_path_strings = watched_class_paths com in
				List.iter add_dir class_path_strings;
				List.iter add_dir (List.map Path.add_trailing_slash (Path.find_directories (platform_name com.platform) true class_path_strings));
				ServerMessage.found_directories com "" !dirs;
				cs#add_directories sign !dirs
			) :: sctx.delays;
			(* Returning [] should be fine here because it's a new context, so we won't do any
				shadowing checks anyway. *)
			[]
		in
		Hashtbl.add sctx.changed_directories sign dirs;
		dirs
	in
	dirs

let get_changed_directories sctx com =
	Timer.time com.Common.timer_ctx ["server";"module cache";"changed dirs"] (get_changed_directories sctx) com

(* A directory of the working directory is watched from the first time a module
   of its package is checked, and counts as changed then. *)
let package_directory_changed sctx com dir =
	let sign = Define.get_signature com.defines in
	try
		Hashtbl.find sctx.package_directories (sign,dir)
	with Not_found ->
		let cs = sctx.cs in
		let changed = try
			let time = stat dir in
			let changed = try
				cs#find_package_directory sign dir < time
			with Not_found ->
				ServerMessage.added_directory com "" dir;
				true
			in
			cs#add_package_directory sign dir time;
			changed
		with Unix.Unix_error _ ->
			cs#remove_package_directory sign dir;
			false
		in
		Hashtbl.add sctx.package_directories (sign,dir) changed;
		changed

(* Checks if module [m] can be reused from the cache and returns None in that case. Otherwise, returns
   [Some m'] where [m'] is the module responsible for [m] not being reusable. *)
let check_module sctx com m_path m_extra p =
	let cc = CommonCache.get_cache com in
	let content_changed m_path file =
		let fkey = com.part_scope.file_keys#get file in
		try
			let cfile = cc#find_file fkey in
			(* We must use the module path here because the file path is absolute and would cause
				positions in the parsed declarations to differ. *)
			let _,decls,_,_ = TypeloadParse.parse_module com m_path p in
			cfile.c_decls <> decls
		with Not_found ->
			true
	in
	(* A file written again with the same content is not a change. Contents sent with the request
	   take precedence over the file, so they are never compared. *)
	let unchanged_content m_path m_extra file time =
		match m_extra.m_digest with
		| Some digest when not (List.mem_assoc (Path.UniqueKey.lazy_key m_extra.m_file) com.file_contents)
			&& (try Digest.file file = digest with Sys_error _ -> false) ->
			ServerMessage.unchanged_content com "" file;
			m_extra.m_time <- time;
			true
		| _ ->
			false
	in
	(* Only the directory of the package of the module in a class path can hold a file shadowing it. *)
	let check_module_shadowing com m_path m_extra =
		let own_file = Path.UniqueKey.lazy_key m_extra.m_file in
		let changed = get_changed_directories sctx com in
		let pack_dir = String.concat "" (List.map (fun s -> s ^ "/") (fst m_path)) in
		com.class_paths#iter (fun cp ->
			let dir = cp#absolute_path ^ pack_dir in
			let changed = if cp#path = "" then package_directory_changed sctx com dir else List.exists (fun cdir -> cdir.c_path = dir) changed in
			let file = dir ^ (snd m_path) ^ ".hx" in
			if changed && Sys.file_exists file then begin
				let time = file_time file in
				(* The file of the module does not shadow it, unless its content changed. *)
				if time > m_extra.m_time && not (com.part_scope.file_keys#get file = own_file && unchanged_content m_path m_extra file time) then begin
					ServerMessage.module_path_changed com "" (m_path,m_extra,time,file);
					raise (Dirty (Shadowed file))
				end
			end
		)
	in
	let has_policy m_extra policy = List.mem policy m_extra.m_check_policy || match policy with
		| NoFileSystemCheck when !ServerConfig.do_not_check_modules && com.display.dms_kind <> DMNone -> true
		| _ -> false
	in
	(* With the watched file system view, the path of the module is resolved
	   again: it must still give the file of the module. *)
	let check_module_resolution com m_path m_extra =
		match m_path with
		| _,name when String.length name > 0 && name.[0] = '_' ->
			()
		| _ ->
			try
				let rfile = TypeloadParse.resolve_module_file com m_path (ref []) p in
				let file = rfile.ClassPaths.file in
				let time = file_time file in
				if com.part_scope.file_keys#get file <> Path.UniqueKey.lazy_key m_extra.m_file
					|| (has_policy m_extra NoFileSystemCheck && time > m_extra.m_time && not (unchanged_content m_path m_extra file time)) then begin
					ServerMessage.module_path_changed com "" (m_path,m_extra,time,file);
					raise (Dirty (Shadowed file))
				end
			with Not_found ->
				()
	in
	let check_module_shadowing com m_path m_extra =
		if !Vfs.mode = Vfs.Watched then
			check_module_resolution com m_path m_extra
		else
			check_module_shadowing com m_path m_extra
	in
	let start_mark = sctx.compilation_step in
	let unknown_state_modules = ref [] in
	let rec check m_path m_extra =
		let check_module_path () =
			match m_extra.m_kind with
			| MFake | MImport -> () (* don't get classpath *)
			| MExtern ->
				(* if we have a file then this will override our extern type *)
				check_module_shadowing com m_path m_extra;
				let rec loop = function
					| [] ->
						if sctx.verbose then print_endline ("No library file was found for " ^ s_type_path m_path); (* TODO *)
						raise (Dirty LibraryChanged)
					| (file,load) :: l ->
						match load m_path p with
						| None ->
							loop l
						| Some _ ->
							if com.part_scope.file_keys#get file <> (Path.UniqueKey.lazy_key m_extra.m_file) then begin
								if sctx.verbose then print_endline ("Library file was changed for " ^ s_type_path m_path); (* TODO *)
								raise (Dirty LibraryChanged)
							end
				in
				loop com.load_extern_type
			| MCode ->
				check_module_shadowing com m_path m_extra
			| MMacro when com.is_macro_context ->
				check_module_shadowing com m_path m_extra
			| MMacro ->
				begin match com.get_macros() with
					| None ->
						()
					| Some mcom ->
						check_module_shadowing mcom m_path m_extra
				end
		in
		let has_policy = has_policy m_extra in
		let check_file () =
			let file = Path.UniqueKey.lazy_path m_extra.m_file in
			let time = file_time file in
			if time <> m_extra.m_time && not (unchanged_content m_path m_extra file time) then begin
				if has_policy CheckFileContentModification && not (content_changed m_path file) then begin
					ServerMessage.unchanged_content com "" file;
				end else begin
					ServerMessage.not_cached com "" m_path;
					if m_extra.m_kind = MFake then (com.cs#get_context m_extra.m_sign)#remove_fake_module (Path.UniqueKey.lazy_key m_extra.m_file);
					raise (Dirty (FileChanged file))
				end
			end
		in
		let find_module_extra sign mpath =
			(com.cs#get_context sign)#find_module_extra mpath
		in
		let check_dependencies () =
			PMap.iter (fun _ mdep ->
				let sign = mdep.md_sign in
				let mpath = mdep.md_path in
				let m2_extra = try
					find_module_extra sign mpath
				with Not_found ->
					raise (Dirty (DependencyNotCached mpath))
				in
				match check mpath m2_extra with
				| None -> ()
				| Some reason -> raise (Dirty (DependencyDirty(mpath,reason)))
			) m_extra.m_deps
		in
		let check () =
			try
				check_module_path();
				if not (has_policy NoFileSystemCheck) || Path.file_extension (Path.UniqueKey.lazy_path m_extra.m_file) <> "hx" then check_file();
				check_dependencies();
				None
			with
			| Dirty reason ->
				Some reason
		in
		(* If the module mark matches our compilation mark, we are done *)
		if m_extra.m_checked = start_mark then begin match m_extra.m_cache_state with
			| MSGood | MSUnknown ->
				None
			| MSBad reason ->
				Some reason
		end else begin
			(* Otherwise, set to current compilation mark for recursion *)
			m_extra.m_checked <- start_mark;
			let dirty = match m_extra.m_cache_state with
				| MSBad reason ->
					(* If we are already dirty, stick to it. *)
					Some reason
				| MSUnknown	->
					(* This should not happen because any MSUnknown module is supposed to have the current m_checked. *)
					die "" __LOC__
				| MSGood ->
					(* Otherwise, run the checks *)
					m_extra.m_cache_state <- MSUnknown;
					begin try check () with e ->
						m_extra.m_cache_state <- MSGood;
						m_extra.m_checked <- start_mark - 1;
						raise e
					end
			in
			(* Update the module now. It will use this dirty status for the remainder of this compilation. *)
			begin match dirty with
			| Some reason ->
				(* Update the state if we're dirty. *)
				m_extra.m_cache_state <- MSBad reason;
			| None ->
				(* We cannot update if we're clean because at this point it might just be an assumption.
				   Instead We add the module to a list which is updated at the end of handling this subgraph. *)
				unknown_state_modules := m_extra :: !unknown_state_modules;
			end;
			dirty
		end
	in
	let reset_unknown () =
		List.iter (fun m_extra -> match m_extra.m_cache_state with
			| MSUnknown ->
				m_extra.m_checked <- start_mark - 1;
				m_extra.m_cache_state <- MSGood;
			| MSGood | MSBad _ ->
				()
		) !unknown_state_modules
	in
	(* A check interrupted by an exception (a syntax error met when comparing the content of a file)
	   leaves the modules it visited to be checked again. *)
	let state = try check m_path m_extra with e -> reset_unknown (); raise e in
	begin match state with
	| None ->
		(* If the entire subgraph is clean, we can set all modules to good state *)
		List.iter (fun m_extra -> m_extra.m_cache_state <- MSGood) !unknown_state_modules;
	| Some _ ->
		(* Otherwise, unknown state module may or may not be dirty. We didn't check everything eagerly, so we have
		   to make sure that the module is checked again if it appears in a different check. This is achieved by
		   setting m_checked to a lower value and assuming Good state again. *)
		reset_unknown ()
	end;
	state

let handle_cache_bound_objects com cbol =
	DynArray.iter (function
		| Resource(name,data) ->
			Hashtbl.replace com.resources name data
		| IncludeFile(file,position) ->
			com.include_files <- (file,position) :: com.include_files
		| Message(cm) ->
			CompilerMessage.replay_message com cm
	) cbol

(* Adds module [m] and all its dependencies (recursively) from the cache to the current compilation
   context. *)
let rec add_modules sctx com delay (m : module_def) (p : pos) =
	let own_sign = CommonCache.get_cache_sign com in
	let rec add_modules tabs m0 m =
		if m.m_extra.m_cache_state <> MSGood then begin
			(match m.m_extra.m_cache_state with
				| MSBad reason when com.display.dms_full_typing ->
					failwith (Printf.sprintf "Unexpected bad module %s (%s)" (s_type_path m.m_path) (Printer.s_module_skip_reason reason))
				| MSBad reason ->
					com.warning WIgnoredBadModule com.warning_options (Printf.sprintf "Ignored bad module %s (%s)" (s_type_path m.m_path) (Printer.s_module_skip_reason reason)) p
				| _ -> ()
			);
			com.module_lut#remove m.m_path
		end else if m.m_extra.m_added < com.part_scope.compilation_step then begin
			m.m_extra.m_added <- com.part_scope.compilation_step;
			(match m0.m_extra.m_kind, m.m_extra.m_kind with
			| MCode, MMacro | MMacro, MCode ->
				(* this was just a dependency to check : do not add to the context *)
				handle_cache_bound_objects com m.m_extra.m_cache_bound_objects;
			| _ ->
				ServerMessage.reusing com tabs m;
				List.iter (fun t ->
					(t_infos t).mt_restore()
				) m.m_types;
				com.module_lut#add m.m_path m;
				handle_cache_bound_objects com m.m_extra.m_cache_bound_objects;
				PMap.iter (fun _ mdep ->
					let mpath = mdep.md_path in
					if mdep.md_sign = own_sign then begin
						let m2 = try
							Some (com.module_lut#find mpath)
						with Not_found ->
							match type_module sctx com delay mpath p with
							| GoodModule m ->
								Some m
							| NoModule ->
								failwith (Printf.sprintf "Unexpectedly could not find module %s as a dependency of %s" (s_type_path mpath) (s_type_path m0.m_path))
							| BadModule reason ->
								failwith (Printf.sprintf "Unexpected bad module %s (%s) as a dependency of %s" (s_type_path mpath) (Printer.s_module_skip_reason reason) (s_type_path m0.m_path))
						in
						Option.may (fun m2 -> add_modules (tabs ^ "  ") m0 m2) m2
					end
				) m.m_extra.m_deps
			)
		end
	in
	add_modules "" m m

(* Looks up the module referred to by [mpath] in the cache. If it exists, a check is made to
   determine if it's still valid. If this function returns None, the module is re-typed. *)
and type_module sctx com delay mpath p =
	let t = Timer.start_timer com.timer_ctx ["server";"module cache"] in
	let cc = CommonCache.get_cache com in
	let skip m_path reason =
		ServerMessage.skipping_dep com "" (m_path,(fun () -> Printer.s_module_skip_reason reason));
		BadModule reason
	in
	let add_modules m =
		Timer.time com.timer_ctx ["server";"module cache";"add modules"] (add_modules sctx com delay m) p;
		GoodModule m
	in
	let check_module sctx m_path m_extra p =
		Timer.time com.timer_ctx ["server";"module cache";"check"] (check_module sctx com mpath m_extra) p
	in
	let find_module_in_cache cc m_path p =
		try
			let m = cc#find_module m_path in
			begin match m.m_extra.m_cache_state with
				| MSBad reason -> BadModule reason
				| _ -> GoodModule m
			end;
		with Not_found -> NoModule
	in
	(* Should not raise anything! *)
	let m = match find_module_in_cache cc mpath p with
		| GoodModule m ->
			(* "Good" here is an assumption, it only means that the module wasn't explicitly invalidated
			   in the cache. The true cache state will be known after check_module. *)
			begin match check_module sctx mpath m.m_extra p with
				| None ->
					add_modules m;
				| Some reason ->
					skip m.m_path reason
			end
		| BadModule reason ->
			(* A BadModule state here means that the module is already invalidated in the cache, e.g. from server/invalidate. *)
			skip mpath reason
		| NoModule as mr ->
			mr
	in
	t();
	m

(* The directories of a context are listed from its first compilation, and
   again when its class paths change. The watched view resolves the modules
   again instead. *)
let watch_directories sctx com =
	let cs = sctx.cs in
	let sign = Define.get_signature com.defines in
	let class_path_strings = absolute_class_paths com in
	(try
		if (Hashtbl.find sctx.class_paths sign) <> class_path_strings then begin
			ServerMessage.class_paths_changed com "";
			Hashtbl.replace sctx.class_paths sign class_path_strings;
			cs#clear_directories sign;
			cs#clear_package_directories sign;
			(cs#get_context sign)#set_initialized false;
		end;
	with Not_found ->
		Hashtbl.add sctx.class_paths sign class_path_strings);
	if !Vfs.mode <> Vfs.Watched then begin try
		ignore (cs#find_directories sign)
	with Not_found ->
		ignore (get_changed_directories sctx com)
	end

let ensure_macro_setup sctx =
	if not sctx.macro_context_setup then begin
		sctx.macro_context_setup <- true;
		MacroContext.setup();
		MacroContext.on_macro_context_created := (fun mcom -> if sctx.is_server then watch_directories sctx mcom);
	end

let cleanup sctx =
	begin match !MacroContext.macro_interp_cache with
	| Some interp ->
		(* curapi holds a reference to the typing context which we don't want to persist. Replace it with a
		   null API so all references to the compilation context are released and it can be garbage collected. *)
		interp.curapi <- MacroApi.null_api ()
	| None ->
		()
	end;
	(* Remove context caches that haven't been accessed within the max age window.
	   This prevents unbounded accumulation of stale contexts when compilation defines
	   change between requests, generating new cache signatures each time. *)
	if !ServerConfig.stale_context_max_age_seconds > -1 then begin
		let removed = sctx.cs#remove_stale_contexts !ServerConfig.stale_context_max_age_seconds in
		if removed > 0 then
			ServerMessage.message (Printf.sprintf "Removed %d stale context cache(s)" removed)
	end

let before_anything sctx ctx =
	ensure_macro_setup sctx

let after_target_init sctx com =
	let sign = Define.get_signature com.defines in
	ServerMessage.defines com "";
	ServerMessage.signature com "" sign;
	ServerMessage.display_position com "" (DisplayPosition.display_position#get);
	watch_directories sctx com

(* The directories of the class paths are listed once the server is back in
   its own working directory: the absolute paths of the class paths added after
   [after_target_init], like the one of a standalone display file, are resolved
   before, also before a --cmd "cd dir" changes the working directory. *)
let resolve_class_paths sctx com =
	if sctx.is_server then begin
		ignore (absolute_class_paths com);
		Option.may (fun mcom -> ignore (absolute_class_paths mcom)) (com.get_macros())
	end

let after_save sctx com =
	if sctx.is_server && not (Common.has_error_to_report com) then
		CommonCache.maybe_cache_context com

let enable_cache_mode sctx =
	type_module_hook := type_module sctx;
	ensure_macro_setup sctx;
	TypeloadParse.parse_hook := parse_file sctx