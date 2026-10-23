open Globals
open Common
open CompilationCache
open Type
open HxbData
open TypeloadCacheHook

exception Dirty of module_skip_reason
exception ServerError of string

(* Files are parsed in parallel ([TypeloadParse.preparse]): the caches are
   read and written under the leaf lock, the parsing itself runs outside. *)
let parse_file sctx com (rfile : ClassPaths.resolved_file) p =
	let cc = TypingLock.protect_leaf (fun () -> CommonCache.get_cache com) in
	let file = rfile.file in
	let ffile = Path.get_full_path rfile.file
	and fkey = com.part_scope.file_keys#get file in
	let is_display_file = DisplayPosition.display_position#is_in_file (com.part_scope.file_keys#get ffile) in
	match is_display_file, sctx.ServerCompilationContext.current_stdin with
	| true, Some stdin when (com.file_contents <> [] || Common.defined com Define.DisplayStdin) ->
		TypeloadParse.parse_file_from_string com file p stdin
	| _ ->
		let ftime = file_time ffile in
		let data = Std.finally (Timer.start_timer com.timer_ctx ["server";"parser cache"]) (fun () ->
			let cached = TypingLock.protect_leaf (fun () ->
				try
					let cfile = cc#find_file fkey in
					if cfile.c_time <> ftime then raise Not_found;
					Some (Parser.ParseSuccess((cfile.c_package,cfile.c_decls),cfile.c_pdi))
				with Not_found -> match (if is_display_file then None else sctx.cs#find_shared_parse fkey ftime com.defines) with
				| Some sp ->
					(* parsed by another context where the defines read have the same values *)
					cc#cache_file fkey (ClassPaths.create_resolved_file ffile rfile.class_path) ftime sp.sp_data sp.sp_pdi;
					ServerMessage.parsed com "" (ffile,"shared with another context");
					Some (Parser.ParseSuccess(sp.sp_data,sp.sp_pdi))
				| None ->
					None
			) in
			match cached with
			| Some r -> r
			| None ->
				let parse_result = TypeloadParse.parse_file com rfile p in
				let info,is_unusual = match parse_result with
					| ParseError(_,_,_) -> "not cached, has parse error",true
					| ParseSuccess(data,pdi) ->
						if is_display_file then begin
							if pdi.pd_errors <> [] then
								"not cached, is display file with parse errors",true
							else if com.display.dms_per_file then begin
								TypingLock.protect_leaf (fun () -> cc#cache_file fkey rfile ftime data pdi);
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
							TypingLock.protect_leaf (fun () ->
								cc#cache_file fkey (ClassPaths.create_resolved_file ffile rfile.class_path) ftime data pdi;
								sctx.cs#add_shared_parse fkey ftime data pdi
							);
							"cached",false
						end
				in
				if is_unusual then ServerMessage.parsed com "" (ffile,info);
				parse_result
		) () in
		data

open ServerCompilationContext

let stat dir =
	match Vfs.stat (Path.remove_trailing_slash dir) with
	| { Vfs.kind = Vfs.Missing } -> raise (Unix.Unix_error (Unix.ENOENT,"stat",dir))
	| info -> info.Vfs.mtime

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
			let dirs = List.fold_left (fun acc dir ->
				try
					let time' = stat dir.c_path in
					if dir.c_mtime < time' then begin
						dir.c_mtime <- time';
						let sub_dirs = Path.find_directories (platform_name com.platform) false [dir.c_path] in
						List.iter (fun dir ->
							if not (cs#has_directory sign dir) then begin
								let time = stat dir in
								ServerMessage.added_directory com "" dir;
								cs#add_directory sign (CompilationCache.create_directory dir time)
							end;
						) sub_dirs;
						(CompilationCache.create_directory dir.c_path time') :: acc
					end else
						acc
				with Unix.Unix_error _ ->
					cs#remove_directory sign dir.c_path;
					ServerMessage.removed_directory com "" dir.c_path;
					acc
			) [] all_dirs in
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
				let class_path_strings = com.class_paths#as_string_list in
				List.iter add_dir class_path_strings;
				List.iter add_dir (Path.find_directories (platform_name com.platform) true class_path_strings);
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

(* Modules are kept in memory, fully typed: partial typing only applied to
   modules decoded from the hxb cache, which the server no longer uses. *)
let get_typing_mode com m_extra =
	FullTyping

(* The file a lookup of module [path] finds, "" if none (query graph, phase 6). *)
(* Phase 6: a body that the previous compilation typed, of a module typed
   again, is kept when its computation is still valid. *)
let () = DepRecord.reuse_body := (fun c cf ->
	match !NodeGraph.current with
	| Some (sign,v,memo) when c.cl_module.m_extra.m_sign = sign && not (TFingerprint.has_inline_body c cf) ->
		(* the positions of the expression are still right: in the field, which
		   did not move, or in a file that did not change *)
		let positions_kept e old_pos =
			let file = Pos.file old_pos and pmin = Pos.min old_pos and pmax = Pos.max old_pos in
			Pos.file cf.cf_pos = file && Pos.min cf.cf_pos = pmin && Pos.max cf.cf_pos = pmax &&
			let same = Hashtbl.create 4 in
			let rec loop e =
				let p = e.epos in
				let f = Pos.file p in
				if f = file then begin
					if Pos.min p < pmin || Pos.max p > pmax then raise Exit
				end else if not (Hashtbl.mem same f) then begin
					match Hashtbl.find_opt NodeGraph.lexer_files f, (try Some (ThreadSafeHashtbl.find Lexer.all_files f) with Not_found -> None) with
					| Some lf, Some lf' when lf == lf' -> Hashtbl.add same f ()
					| _ -> if f <> Pos.file null_pos then raise Exit
				end;
				Type.iter loop e
			in
			try loop e; true with Exit -> false
		in
		begin match Graft.old_body c cf with
		| Some (e,old_pos) when not (positions_kept e old_pos) -> ignore e; None
		| None -> None
		| Some (e,_) ->
			let key = DepRecord.field_key c cf ^ "\tbody" in
			match Hashtbl.find_opt v.NodeGraph.v_graph.NodeGraph.nodes key with
			| None -> None
			| Some n ->
				match Mutex.protect NodeGraph.reuse_mutex (fun () -> NodeGraph.reusable v memo n) with
				| None -> None
				| Some paths ->
					cf.cf_expr <- Some e;
					add_class_field_flag cf CfPostProcessed;
					Mutex.protect DepRecord.mutex (fun () -> DepRecord.reused := key :: !DepRecord.reused);
					Some paths
		end
	| _ ->
		None
)

let resolve_for_graph com path =
	try Path.get_full_path (TypeloadParse.resolve_module_file com path (ref (fst path)) null_pos).ClassPaths.file
	with _ -> ""

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
			let _,decls,_ = TypeloadParse.parse_module com m_path p in
			cfile.c_decls <> decls
		with Not_found ->
			true
	in
	let check_module_shadowing paths m_path m_extra =
		List.iter (fun dir ->
			let file = (dir.c_path ^ (snd m_path)) ^ ".hx" in
			if Vfs.file_exists file then begin
				let time = file_time file in
				if time > m_extra.m_time then begin
					ServerMessage.module_path_changed com "" (m_path,m_extra,time,file);
					raise (Dirty (Shadowed file))
				end
			end
		) paths
	in
	(* The module's path must still resolve to the module's file: a new file in a
	   class path with a higher priority, or a more specific file (`Foo.js.hx`
	   next to `Foo.hx`), shadows it. Resolution goes through the virtual file
	   system, so this makes no system call for known directories. *)
	let check_module_resolution com m_path m_extra =
		match m_path with
		| _,name when String.length name > 0 && name.[0] = '_' ->
			(* Private module paths (e.g. `_Foo`) are not resolved from files. *)
			()
		| _ ->
			try
				let rfile = TypeloadParse.resolve_module_file com m_path (ref []) p in
				let key = com.part_scope.file_keys#get rfile.ClassPaths.file in
				if key <> Path.UniqueKey.lazy_key m_extra.m_file then begin
					ServerMessage.module_path_changed com "" (m_path,m_extra,file_time rfile.file,rfile.file);
					raise (Dirty (Shadowed rfile.file))
				end
			with Not_found ->
				()
	in
	let start_mark = sctx.compilation_step in
	let unknown_state_modules = ref [] in
	(* the query graph of this context (phase 6), for the modules it traces *)
	let own_sign = CommonCache.get_cache_sign com in
	let graph = if NodeGraph.disabled then None else NodeGraph.find_opt own_sign in
	let module_valid = ref (fun _ -> true) in
	let macro_module_valid = ref (fun _ _ -> true) in
	let validation = Option.map (fun g ->
		NodeGraph.begin_validation g start_mark (fun () -> get_changed_directories sctx com <> []) (fun () -> com.class_paths#as_string_list);
		let current_decls f =
			if file_time f.NodeGraph.f_path = f.f_time then
				f.f_decls
			else
				(* changed: parsed again, the parse is cached for typing *)
				try
					let rfile,_,_,parsed,_ = TypeloadParse.parse_module' com f.f_module p in
					Some (NodeGraph.decl_table rfile.ClassPaths.file parsed)
				with _ ->
					None
		in
		{
			NodeGraph.v_graph = g;
			v_decls = current_decls;
			v_resolve = resolve_for_graph com;
			v_module_valid = (fun path -> !module_valid path);
			v_macro_module_valid = (fun sign path -> !macro_module_valid sign path);
			v_provisional = [];
			v_failed = None;
			v_assume = not !NodeGraph.no_assumptions && !NodeGraph.attempts < 3 && com.display.dms_kind = DMNone;
		}
	) graph in
	(* for the bodies kept while typing (phase 6): a module is valid when this
	   compilation found it valid, without checking it again from there *)
	Option.may (fun v ->
		let checked sign path =
			try
				let m_extra = (com.cs#get_context sign)#find_module_extra path in
				m_extra.m_checked = start_mark && m_extra.m_cache_state = MSGood
			with Not_found ->
				false
		in
		NodeGraph.current := Some (own_sign,{ v with NodeGraph.v_module_valid = checked own_sign; v_macro_module_valid = checked },Hashtbl.create 0)
	) validation;
	let rec check m_path m_extra =
		let check_module_path () =
			let directories = get_changed_directories sctx com in
			match m_extra.m_kind with
			| MFake | MImport -> () (* don't get classpath *)
			| MExtern ->
				(* if we have a file then this will override our extern type *)
				check_module_shadowing directories m_path m_extra;
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
				check_module_resolution com m_path m_extra
			| MMacro when com.is_macro_context ->
				check_module_resolution com m_path m_extra
			| MMacro ->
				begin match com.get_macros() with
					| None ->
						()
					| Some mcom ->
						check_module_resolution mcom m_path m_extra
				end
		in
		let has_policy policy = List.mem policy m_extra.m_check_policy || match policy with
			| NoFileSystemCheck when !ServerConfig.do_not_check_modules && com.display.dms_kind <> DMNone -> true
			| _ -> false
		in
		let check_file () =
			let file = Path.UniqueKey.lazy_path m_extra.m_file in
			if file_time file <> m_extra.m_time then begin
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
			(* A module traced by the graph is valid when what it read did not
			   change: its dependencies on the code of its context are covered. *)
			let traced = match validation with
				| Some v when m_extra.m_sign = own_sign && m_extra.m_kind = MCode && NodeGraph.is_traced v.v_graph m_path ->
					v.v_failed <- None;
					if not (Timer.time com.timer_ctx ["server";"module cache";"graph"] (NodeGraph.module_valid v) m_path) then
						raise (Dirty (ReadChanged (Option.default "?" v.v_failed)));
					true
				| _ ->
					false
			in
			PMap.iter (fun _ mdep ->
				let sign = mdep.md_sign in
				let mpath = mdep.md_path in
				let m2_extra = try
					find_module_extra sign mpath
				with Not_found ->
					die (Printf.sprintf "Could not find dependency %s of %s in the cache" (s_type_path mpath) (s_type_path m_path)) __LOC__;
				in
				if traced && sign = own_sign && m2_extra.m_kind = MCode then
					()
				else match check mpath m2_extra with
				| None -> ()
				| Some reason -> raise (Dirty (DependencyDirty(mpath,reason)))
			) m_extra.m_deps
		in
		let check () =
			try
				check_module_path();
				if List.mem VolatileMacro m_extra.m_check_policy && Common.volatile_changed m_path m_extra then raise (Dirty (ReadChanged "volatile macro"));
				if not (has_policy NoFileSystemCheck) || Path.file_extension (Path.UniqueKey.lazy_path m_extra.m_file) <> "hx" then check_file();
				if (get_typing_mode com m_extra) = FullTyping && not (Define.defined com.defines Define.IncrementalRetypeChangedOnly) then check_dependencies();
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
					check ()
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
	module_valid := (fun path ->
		try check path ((com.cs#get_context own_sign)#find_module_extra path) = None with Not_found -> false
	);
	macro_module_valid := (fun sign path ->
		try check path ((com.cs#get_context sign)#find_module_extra path) = None with Not_found -> false
	);
	let assumptions = Option.map (fun v -> v.NodeGraph.v_graph.assumptions) validation in
	let state = check m_path m_extra in
	Option.may (fun v ->
		if state = None then v.NodeGraph.v_provisional <- []
		else begin
			NodeGraph.forget_provisional v;
			(* the assumptions made for modules that are typed again anyway *)
			Option.may (fun l -> v.v_graph.assumptions <- l) assumptions
		end
	) validation;
	begin match state with
	| None ->
		(* If the entire subgraph is clean, we can set all modules to good state *)
		List.iter (fun m_extra -> m_extra.m_cache_state <- MSGood) !unknown_state_modules;
	| Some _ ->
		(* Otherwise, unknown state module may or may not be dirty. We didn't check everything eagerly, so we have
		   to make sure that the module is checked again if it appears in a different check. This is achieved by
		   setting m_checked to a lower value and assuming Good state again. *)
		List.iter (fun m_extra -> match m_extra.m_cache_state with
			| MSUnknown ->
				m_extra.m_checked <- start_mark - 1;
				m_extra.m_cache_state <- MSGood;
			| MSGood | MSBad _ ->
				()
		) !unknown_state_modules
	end;
	state

let get_hxb_module com cc path typing_mode =
	try
		let mc = cc#get_hxb_module path in
		match get_typing_mode com mc.mc_extra with
			| AllowPartialTyping ->
				mc.mc_extra.m_cache_state <- MSGood;
				BinaryModule mc
			| FullTyping ->
				begin match mc.mc_extra.m_cache_state with
					| MSBad reason when typing_mode = AllowPartialTyping -> BadBinaryModule (mc, reason)
					| MSBad reason -> BadModule reason
					| _ -> BinaryModule mc
				end
	with Not_found ->
		NoModule

class hxb_reader_api_server
	(com : Common.context)
	(cc : context_cache)
	(delay : ProgramStage.stage -> (unit -> unit) -> unit)
= object(self)

	method make_module (path : path) (file : string) =
		let mc = cc#get_hxb_module path in
		{
			m_id = mc.mc_id;
			m_path = path;
			m_types = [];
			m_statics = None;
			(* Creating a new m_extra because if we keep the same reference, display requests *)
			(* can alter it with bad data (for example adding dependencies that are not cached) *)
			m_extra = { mc.mc_extra with m_deps = mc.mc_extra.m_deps; m_display_deps = None }
		}

	method add_module (m : module_def) =
		com.module_lut#add m.m_path m

	method resolve_type (pack : string list) (mname : string) (tname : string) full_restore =
		let path = (pack,mname) in
		let m = self#resolve_module path full_restore in
		List.find (fun t -> snd (t_path t) = tname) m.m_types

	method resolve_module (path : path) full_restore =
		match self#find_module path full_restore with
		| GoodModule m ->
			m
		| BinaryModule mc ->
			let reader = new HxbReader.hxb_reader path com.hxb_reader_stats (if Common.defined com Define.HxbTimes then Some com.timer_ctx else None) in
			let typing_mode = get_typing_mode com mc.mc_extra in
			let f_next chunks until =
				let macro = if com.is_macro_context then " (macro)" else "" in
				let f  = reader#read_chunks_until (self :> HxbReaderApi.hxb_reader_api) chunks until in
				Timer.time com.timer_ctx ["server";"module cache";"hxb read" ^ macro;"until " ^ (string_of_chunk_kind until)] f typing_mode
			in

			let m,chunks = f_next mc.mc_chunks EOT in

			(* We try to avoid reading expressions as much as possible, so we only do this for
				 our current display file if we're in display mode. *)
			(match typing_mode with
			| FullTyping -> ignore(f_next chunks EOM)
			| AllowPartialTyping -> delay StageMembers (fun () -> ignore(f_next chunks EOF)));
			incr com.request_scope.stats.s_modules_restored;
			m
		| BadBinaryModule (mc, reason) ->
			let reader = new HxbReader.hxb_reader path com.hxb_reader_stats (if Common.defined com Define.HxbTimes then Some com.timer_ctx else None) in
			let typing_mode = get_typing_mode com mc.mc_extra in
			let f_next chunks until =
				let macro = if com.is_macro_context then " (macro)" else "" in
				let f  = reader#read_chunks_until (self :> HxbReaderApi.hxb_reader_api) chunks until in
				Timer.time com.timer_ctx ["server";"module cache";"hxb read" ^ macro;"until " ^ (string_of_chunk_kind until)] f full_restore
			in

			let m,chunks = f_next mc.mc_chunks EOT in
			m.m_extra.m_cache_state <- MSBad reason;

			(* We try to avoid reading expressions as much as possible, so we only do this for
				 our current display file if we're in display mode. *)
			(match typing_mode with
			| FullTyping -> ignore(f_next chunks EOM)
			| AllowPartialTyping -> delay StageMembers (fun () -> ignore(f_next chunks EOF)));
			incr com.request_scope.stats.s_modules_restored;
			m
		| BadModule reason ->
			die (Printf.sprintf "Unexpected BadModule %s (%s)" (s_type_path path) (Printer.s_module_skip_reason reason)) __LOC__
		| NoModule ->
			die (Printf.sprintf "Unexpected NoModule %s" (s_type_path path)) __LOC__

	method find_module (m_path : path) typing_mode =
		try
			GoodModule (com.module_lut#find m_path)
		with Not_found -> get_hxb_module com cc m_path typing_mode

	method basic_types =
		com.basic

	method get_var_id (i : int) =
		i

	method read_expression_eagerly (cf : tclass_field) =
		true

	method make_lazy_type t f =
		let r = make_unforced_lazy t f "server-api" in
		delay StageChecks (fun () -> ignore(lazy_type r));
		TLazy r
end

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
let rec add_modules sctx com delay (m : module_def) (from_binary : bool) (p : pos) =
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
				(* The main module gets added when reading hxb already, so let's not add it again. Note that we
				   can't set its m_added ahead of time because we want the rest of the logic here to run. *)
				if not from_binary then
					com.module_lut#add m.m_path m;
				handle_cache_bound_objects com m.m_extra.m_cache_bound_objects;
				let typing_mode = get_typing_mode com m.m_extra in
				PMap.iter (fun _ mdep ->
					let mpath = mdep.md_path in
					if mdep.md_sign = own_sign then begin
						let m2 = try
							Some (com.module_lut#find mpath)
						with Not_found ->
							match type_module sctx com delay mpath p with
							| GoodModule m ->
								Some m
							| BinaryModule mc ->
								failwith (Printf.sprintf "Unexpectedly found unresolved binary module %s as a dependency of %s" (s_type_path mpath) (s_type_path m0.m_path))
							| NoModule ->
								failwith (Printf.sprintf "Unexpectedly could not find module %s as a dependency of %s" (s_type_path mpath) (s_type_path m0.m_path))
							| BadBinaryModule (_, reason) | BadModule reason when typing_mode = AllowPartialTyping ->
								com.warning WIgnoredBadModule com.warning_options (Printf.sprintf "Ignored bad dependency %s (%s) of %s" (s_type_path m.m_path) (Printer.s_module_skip_reason reason) (s_type_path m0.m_path)) p;
								None
							| BadBinaryModule (_, reason) ->
								failwith (Printf.sprintf "Unexpected bad hxb module %s (%s) as a dependency of %s" (s_type_path mpath) (Printer.s_module_skip_reason reason) (s_type_path m0.m_path))
							| BadModule _ ->
								(* a module can be valid although a module it depends on is not,
								   when what it read from it did not change (phase 6): that one
								   is typed again after it *)
								TypeloadCacheHook.dirty_dependencies := (com,mpath) :: !TypeloadCacheHook.dirty_dependencies;
								None
						in
						Option.may (fun m2 -> add_modules (tabs ^ "  ") m0 m2) m2
					end
				) (if typing_mode = FullTyping then m.m_extra.m_deps else Option.default m.m_extra.m_deps m.m_extra.m_display_deps)
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
	let add_modules from_binary m =
		Timer.time com.timer_ctx ["server";"module cache";"add modules"] (add_modules sctx com delay m from_binary) p;
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
		with Not_found -> get_hxb_module com cc m_path FullTyping
	in
	(* Should not raise anything! *)
	let m = match find_module_in_cache cc mpath p with
		| GoodModule m ->
			(* "Good" here is an assumption, it only means that the module wasn't explicitly invalidated
			   in the cache. The true cache state will be known after check_module. *)
			begin match check_module sctx mpath m.m_extra p with
				| None ->
					add_modules false m;
				| Some reason ->
					skip m.m_path reason
			end
		| BinaryModule mc ->
			(* Similarly, we only know that a binary module wasn't explicitly tainted. Decode it only after
			   checking dependencies. This means that the actual decoding never has any reason to fail. *)
			begin match check_module sctx mpath mc.mc_extra p with
				| None ->
					let reader = new HxbReader.hxb_reader mpath com.hxb_reader_stats (if Common.defined com Define.HxbTimes then Some com.timer_ctx else None) in
					let typing_mode = get_typing_mode com mc.mc_extra in
					let api = match com.hxb_reader_api with
						| Some api ->
							api
						| None ->
							let api = (new hxb_reader_api_server com cc delay :> HxbReaderApi.hxb_reader_api) in
							com.hxb_reader_api <- Some api;
							api
					in
					let f_next chunks until =
						let macro = if com.is_macro_context then " (macro)" else "" in
						Timer.time com.timer_ctx ["server";"module cache";"hxb read" ^ macro;"until " ^ (string_of_chunk_kind until)] (reader#read_chunks_until api chunks until) typing_mode
					in

					let m,chunks = f_next mc.mc_chunks EOT in

					(* We try to avoid reading expressions as much as possible, so we only do this for
					   our current display file if we're in display mode. *)
					(match typing_mode with
					| FullTyping -> ignore(f_next chunks EOM)
					| AllowPartialTyping -> delay StageMembers (fun () -> ignore(f_next chunks EOF)));
					incr com.request_scope.stats.s_modules_restored;
					add_modules true m;
				| Some reason ->
					skip mpath reason
			end
		| BadBinaryModule (_, reason) ->
			(* A BadModule state here means that the module is already invalidated in the cache, e.g. from server/invalidate. *)
			skip mpath reason
		| BadModule reason ->
			(* A BadModule state here means that the module is already invalidated in the cache, e.g. from server/invalidate. *)
			skip mpath reason
		| NoModule as mr ->
			mr
	in
	t();
	m

let ensure_macro_setup sctx =
	if not sctx.macro_context_setup then begin
		sctx.macro_context_setup <- true;
		MacroContext.setup();
	end

let cleanup sctx =
	begin match !MacroContext.macro_interp_cache with
	| Some interp ->
		(* curapi holds a reference to the typing context which we don't want to persist. Replace it with a
		   null API so all references to the compilation context are released and it can be garbage collected. *)
		EvalContext.clear_curapi interp (MacroApi.null_api ())
	| None ->
		()
	end;
	sctx.cs#clear_temp_cache;
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
	let cs = sctx.cs in
	let sign = Define.get_signature com.defines in
	ServerMessage.defines com "";
	ServerMessage.signature com "" sign;
	ServerMessage.display_position com "" (DisplayPosition.display_position#get);
	let class_path_strings = com.class_paths#as_string_list in
	try
		if (Hashtbl.find sctx.class_paths sign) <> class_path_strings then begin
			ServerMessage.class_paths_changed com "";
			Hashtbl.replace sctx.class_paths sign class_path_strings;
			cs#clear_directories sign;
			(cs#get_context sign)#set_initialized false;
		end;
	with Not_found ->
		Hashtbl.add sctx.class_paths sign class_path_strings;
		()

(* Stores the query graph of the modules typed in this compilation (phase 6),
   and checks the assumptions of early cutoff: if one does not hold, the
   compilation is done again (raises [NodeGraph.Retry]). *)
let assumed_graphs com =
	List.filter (fun (g : NodeGraph.graph) -> g.assumptions <> []) (List.filter_map NodeGraph.find_opt
		(CommonCache.get_cache_sign com :: (match com.get_macros () with Some mcom -> [CommonCache.get_cache_sign mcom] | None -> [])))

(* Typing stopped on an error: it can come from a false assumption, which one
   is not known. *)
let retry_if_assumed com =
	let assumed = assumed_graphs com in
	if assumed <> [] then begin
		List.iter (fun g -> g.NodeGraph.assumptions <- []) assumed;
		NodeGraph.no_assumptions := true;
		raise NodeGraph.Retry
	end

let store_graph com =
	if Common.has_error_to_report com then retry_if_assumed com;
	let assumed = assumed_graphs com in
	if com.sctx.is_server && com.display.dms_full_typing && com.display.dms_populate_cache && not (Common.has_error_to_report com) then begin
		let store com =
			let cc = CommonCache.get_cache com in
			let decls_of m =
				try Some (cc#find_file (com.part_scope.file_keys#get (Path.UniqueKey.lazy_path m.m_extra.m_file))).c_decls
				with Not_found -> None
			in
			(* the version in the cache is the previous one, until the end of the compilation *)
			let is_typed m = match cc#find_module_opt m.m_path with Some m' -> m' != m | None -> true in
			NodeGraph.prepare_store com (resolve_for_graph com) decls_of is_typed
		in
		DepRecord.last_typed := [];
		Timer.time com.timer_ctx ["server";"query graph"] (fun () ->
			let prepared = store com :: (match com.get_macros() with Some mcom -> [store mcom] | None -> []) in
			(* the graph is stored only if the assumptions held: otherwise the
			   compilation is done again, knowing the new fingerprints of the
			   nodes that changed, with the graph of the previous compilation *)
			let failed = List.concat_map (fun (failed,_) -> failed ()) prepared in
			if failed <> [] then begin
				List.iter (fun (key,fp) -> Hashtbl.replace NodeGraph.known_changed key fp) failed;
				List.iter (fun g -> g.NodeGraph.assumptions <- []) assumed;
				incr NodeGraph.attempts;
				raise NodeGraph.Retry
			end;
			List.iter (fun (_,apply) -> apply ()) prepared
		) ();
		(* the edges, once the server answered (before the maintenance tasks,
		   which empty the expressions of cached fields) *)
		com.cs#add_task (new arbitrary_task ["query graph"] 10 NodeGraph.flush)
	end else
		List.iter (fun g -> g.NodeGraph.assumptions <- []) assumed

let after_save sctx com =
	if sctx.is_server && not (Common.has_error_to_report com) then
		CommonCache.maybe_cache_context com

let enable_cache_mode sctx =
	type_module_hook := type_module sctx;
	server_active := true;
	ensure_macro_setup sctx;
	TypeloadParse.parse_hook := parse_file sctx