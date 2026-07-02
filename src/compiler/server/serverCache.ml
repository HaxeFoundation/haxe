open Globals
open Common
open CompilationCache
open Type
open HxbData
open TypeloadCacheHook

exception Dirty of module_skip_reason
exception ServerError of string

let parse_file sctx com (rfile : ClassPaths.resolved_file) p =
	let cc = CommonCache.get_cache com in
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
			try
				let cfile = cc#find_file fkey in
				if cfile.c_time <> ftime then raise Not_found;
				Parser.ParseSuccess((cfile.c_package,cfile.c_decls),cfile.c_pdi)
			with Not_found ->
				let parse_result = TypeloadParse.parse_file com rfile p in
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

let get_typing_mode com m_extra =
	let full_typing = com.is_macro_context
		|| com.display.dms_full_typing
		|| Define.defined com.defines Define.DisableHxbCache
		|| Define.defined com.defines Define.DisableHxbOptimizations
		|| DisplayPosition.display_position#is_in_file (Path.UniqueKey.lazy_key m_extra.m_file)
	in
	if full_typing then FullTyping else AllowPartialTyping

(* Diagnostic logs (idsplit / decode / resident-hits — all stripped before merge). Use a FIXED dir with
   NO session id, created on demand, so they never embed a dead per-session /tmp path and a missing dir
   (e.g. after a reboot wipes /tmp) never Sys_error-crashes the compiler. Override with HXB_DIAG_DIR. *)
let diag_dir =
	try Sys.getenv "HXB_DIAG_DIR" with Not_found -> "/tmp/haxe-hxb-diag"
let diag_open name =
	(try Unix.mkdir diag_dir 0o755 with _ -> ());
	open_out_gen [Open_append;Open_creat] 0o644 (Filename.concat diag_dir name)

(* hxb.decode_log diagnostic: record every hxb decode with its trigger site (TOP = top-level type_module load,
   CASC = cascade cross-ref resolution), to diff the decoded module SET between resident on/off. *)
let decode_log com tag path =
	if Define.raw_defined com.defines "hxb.decode_log" then begin
		let oc = diag_open "decoded.log" in
		output_string oc (Printf.sprintf "%s %s\n" tag (s_type_path path)); close_out oc
	end

(* hxb.idsplit diagnostic (resident_modules display identity-split bug): assign a stable physical serial to
   each target tclass/tabstract object so we can see, per resolution, whether the SAME path yields the SAME
   physical object across the resident top-level serve path and the cascade cross-ref path. If the cursor's
   Item (resolved via cascade while typing ItemSlot) gets a different serial than the Item frozen inside the
   served Tooltip.fromItem param, the corruption is an identity split; if the serial is stable but typing still
   fails, it is in-place mutation. Physical-eq assoc list (target objects are few). *)
let idsplit_serials : (Obj.t * int) list ref = ref []
let idsplit_next = ref 0
let idsplit_serial (o:Obj.t) =
	let rec find = function
		| [] -> let s = !idsplit_next in incr idsplit_next; idsplit_serials := (o,s) :: !idsplit_serials; s
		| (o',s) :: _ when o' == o -> s
		| _ :: tl -> find tl
	in find !idsplit_serials

let idsplit_enabled com = Define.raw_defined com.defines "hxb.idsplit"

let idsplit_log com tag s =
	if idsplit_enabled com then begin
		let oc = diag_open "idsplit.log" in
		output_string oc (Printf.sprintf "%s %s\n" tag s); close_out oc
	end

let idsplit_is_target name = match name with
	| "Item" | "Tooltip" | "TipItemCompare" -> true
	| _ -> false

(* Log the physical serial of every target decl in a module (its decode/serve identity). *)
let idsplit_module com tag m =
	if idsplit_enabled com then
		List.iter (fun mt -> match mt with
			| TClassDecl c when idsplit_is_target (snd c.cl_path) ->
				idsplit_log com tag (Printf.sprintf "class %s #%d" (s_type_path c.cl_path) (idsplit_serial (Obj.repr c)))
			| TAbstractDecl a when idsplit_is_target (snd a.a_path) ->
				idsplit_log com tag (Printf.sprintf "abstract %s #%d" (s_type_path a.a_path) (idsplit_serial (Obj.repr a)))
			| _ -> ()
		) m.m_types

(* Non-forcing description (with serial) of the class/abstract object embedded in a (param) type. *)
let rec idsplit_type_obj t = match t with
	| TAbstract(a, [p]) when snd a.a_path = "Null" -> idsplit_type_obj p
	| TAbstract(a, _) -> Printf.sprintf "abstract %s #%d" (s_type_path a.a_path) (idsplit_serial (Obj.repr a))
	| TInst(c, _) -> Printf.sprintf "class %s #%d" (s_type_path c.cl_path) (idsplit_serial (Obj.repr c))
	| TType(td, _) -> Printf.sprintf "typedef %s" (s_type_path td.t_path)
	| TLazy _ -> "lazy" | TMono { tm_type = Some t } -> "mono->" ^ idsplit_type_obj t | TMono _ -> "mono" | _ -> "other"

(* For the served Tooltip module, log the physical identity of the Item/st.Item objects frozen inside
   fromItem's parameter types (the receiver type the cursor must unify its argument against). *)
let idsplit_tooltip com tag m =
	if idsplit_enabled com then
		List.iter (function
			| TClassDecl c when snd c.cl_path = "Tooltip" ->
				(try match (PMap.find "fromItem" c.cl_statics).cf_type with
					| TFun(args,_) -> List.iter (fun (_,_,at) -> idsplit_log com tag ("fromItem-param " ^ idsplit_type_obj at)) args
					| _ -> ()
				with Not_found -> ())
			| _ -> ()
		) m.m_types

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
			if Sys.file_exists file then begin
				let time = file_time file in
				if time > m_extra.m_time then begin
					ServerMessage.module_path_changed com "" (m_path,m_extra,time,file);
					raise (Dirty (Shadowed file))
				end
			end
		) paths
	in
	let start_mark = sctx.compilation_step in
	let unknown_state_modules = ref [] in
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
				check_module_shadowing directories m_path m_extra
			| MMacro when com.is_macro_context ->
				check_module_shadowing directories m_path m_extra
			| MMacro ->
				begin match com.get_macros() with
					| None ->
						()
					| Some mcom ->
						check_module_shadowing (get_changed_directories sctx mcom) m_path m_extra
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
					if m_extra.m_kind = MFake then Hashtbl.remove com.fake_modules (Path.UniqueKey.lazy_key m_extra.m_file);
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
					die (Printf.sprintf "Could not find dependency %s of %s in the cache" (s_type_path mpath) (s_type_path m_path)) __LOC__;
				in
				(* Header invalidation: if this dependency was re-typed as a seed in the pre-phase, it
				   carries a step-tagged signature delta. The dependent is then invalidated only if it
				   observes one of the changed entries (field-granular); otherwise it is spared (the dep
				   is already re-typed and good). *)
				let spared_via_delta = (match m2_extra.m_sig_delta with
					| Some (step,delta) when step = start_mark ->
						let edges = PMap.foldi (fun _ e acc ->
							if e.dep_tgt_path = mpath then e :: acc else acc
						) m_extra.m_field_deps [] in
						let observes = ModuleSignature.dependent_observes_changes delta edges in
						(if Define.defined com.defines Define.HxbHeaderInvalidationVerbose then
							print_endline (Printf.sprintf "[hi-probe] %s -> seed %s observes=%b | changes=[%s] | edges=[%s]"
								(s_type_path m_path) (s_type_path mpath) observes
								(String.concat "; " (List.map ModuleSignature.s_sig_change delta))
								(String.concat "; " (List.map (fun e -> match e.dep_tgt with
									| None -> "MODULE(" ^ (match e.dep_tgt_origin with MDepFromMacro|MDepFromMacroDefine -> "macro" | _ -> "plain") ^ ")"
									| Some df -> Printf.sprintf "%s.%s" (s_type_path df.dfd_path) df.dfd_field) edges))));
						if observes then
							raise (Dirty (DependencyDirty(mpath,Tainted ServerInvalidate)))
						else
							true
					| _ ->
						false
				) in
				if not spared_via_delta then
				match check mpath m2_extra with
				| None -> ()
				(* Header invalidation: a seed currently being re-typed in the pre-phase (MSBad
				   Reprocessing) is dirty for itself but CLEAN as a dependency — it must not cascade
				   dirtiness to its dependents. *)
				| Some Reprocessing -> ()
				| Some reason -> raise (Dirty (DependencyDirty(mpath,reason)))
			) m_extra.m_deps
		in
		let check () =
			try
				check_module_path();
				if not (has_policy NoFileSystemCheck) || Path.file_extension (Path.UniqueKey.lazy_path m_extra.m_file) <> "hx" then check_file();
				if (get_typing_mode com m_extra) = FullTyping then check_dependencies();
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
	let state = check m_path m_extra in
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
				begin match mc.mc_extra.m_cache_state with
					(* The macro context EXECUTES field bodies at compile time, so it must not run against
					   a stale cache: an invalidated module (MSBad, other than a seed currently being re-typed)
					   is re-typed from source rather than served from the binary cache. A display request never
					   executes bodies, so for it the header-stable cache stays valid and is served as before. *)
					| MSBad reason when com.is_macro_context && (match reason with Reprocessing -> false | _ -> true) ->
						BadModule reason
					| _ ->
						mc.mc_extra.m_cache_state <- MSGood;
						BinaryModule mc
				end
			| FullTyping ->
				begin match mc.mc_extra.m_cache_state with
					| MSBad reason when typing_mode = AllowPartialTyping -> BadBinaryModule (mc, reason)
					(* Header invalidation: a seed being re-typed is referenced by a peer's restore;
					   restore it (its cached header resolves the reference) rather than dying, so
					   cycles through the seed close. *)
					| MSBad Reprocessing -> BadBinaryModule (mc, Reprocessing)
					| MSBad reason -> BadModule reason
					| _ -> BinaryModule mc
				end
	with Not_found ->
		NoModule

(* hxb.resident_modules: shared resident-serve logic used by BOTH resolution paths — the typer's free top-level
   load (find_module_in_cache) and the reader's cascade cross-ref load (api#find_module). Serving on a SINGLE
   path (top-level only) was the identity-split bug: a resident module's frozen cross-module refs (decode-gen
   objects) never matched what the full-typed display file resolved through the un-served cascade path, so unify
   collapsed to TMono. Returning the SAME resident m_types on both paths gives one canonical object per path.
   Hands out a fresh m_extra shell (this request's dep mutations must not pollute the resident object) while
   preserving the resident module's own m_display_deps (nulling it forces add_modules to walk the full m_deps).
   Reuse is validated by module id; a stale/missing binary entry purges the resident. Returns the served wrapper
   or None (caller falls back to a fresh decode). resident_enabled is request-level (display, not full-typing). *)
let try_serve_resident com cc m_path =
	let trace tag = if idsplit_is_target (snd m_path) then idsplit_log com "LIFECYCLE" (Printf.sprintf "%s %s" tag (s_type_path m_path)) in
	let resident_enabled = Define.defined com.defines Define.HxbResidentModules && not com.display.dms_full_typing in
	match (if resident_enabled then cc#find_resident_module m_path else None) with
	| None -> trace (if resident_enabled then "MISS-notresident" else "DISABLED"); None
	| Some m ->
		match (try Some (cc#get_hxb_module m_path) with Not_found -> None) with
		| None -> trace "PURGE-nochunk"; cc#remove_resident_module m_path; None
		| Some mc when mc.mc_id <> m.m_id -> trace (Printf.sprintf "PURGE-stale(rid=%d bid=%d)" m.m_id mc.mc_id); cc#remove_resident_module m_path; None
		| Some mc when get_typing_mode com mc.mc_extra <> AllowPartialTyping -> trace "SKIP-fulltyping"; None
		| Some mc ->
			trace "SERVE"; Some { m with m_extra = { mc.mc_extra with m_deps = mc.mc_extra.m_deps; m_display_deps = m.m_extra.m_display_deps } }

class hxb_reader_api_server
	(init_com : Common.context)
	(cc : context_cache)
	(init_delay : TyperPass.typer_pass -> (unit -> unit) -> unit)
= object(self)

	(* The per-request context (com + delay) is held in mutable fields rather than captured, so a single
	   shared api instance can be re-pointed at the current request via #set_request. This is what lets a
	   resident module (hxb.resident_modules) whose lazy closures captured THIS api be forced soundly by
	   a later request: the closures resolve through whatever com is current, not the originating one.
	   Within a single request the fields are fixed, so behaviour is identical to direct binding. *)
	val mutable com = init_com
	val mutable delay = init_delay

	method set_request (com' : Common.context) (delay' : TyperPass.typer_pass -> (unit -> unit) -> unit) =
		com <- com';
		delay <- delay'

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
			idsplit_module com "CASC-GOOD" m; idsplit_tooltip com "CASC-GOOD" m;
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
			decode_log com "CASC" path;

			(* We try to avoid reading expressions as much as possible, so we only do this for
				 our current display file if we're in display mode. *)
			(match typing_mode with
			| FullTyping -> ignore(f_next chunks EOM)
			(* The macro context executes and inlines field bodies, so it cannot defer them: an inline
			   field restored body-lazy has cf_expr = None at expansion time (calls.ml "Recursive inline").
			   Read to EOM eagerly like FullTyping; deferral stays a display-only optimization. *)
			| AllowPartialTyping when com.is_macro_context -> ignore(f_next chunks EOM)
			| AllowPartialTyping -> delay PConnectField (fun () -> ignore(f_next chunks EOF)));
			incr com.request_scope.stats.s_modules_restored;
			(* hxb.resident_modules canonical registry: register on EVERY first decode, cascade INCLUDED
			   (the top-level path stored only top-level decodes — the gap that forked st.Item in cont.8).
			   Same object the deferred EOF-connect (above) fills in-place this request; cross-request its
			   lazies resolve through the shared api repoint. *)
			if Define.defined com.defines Define.HxbResidentModules && not com.display.dms_full_typing
				&& typing_mode = AllowPartialTyping then begin
				cc#cache_resident_module path m;
				if idsplit_is_target (snd path) then idsplit_log com "LIFECYCLE" (Printf.sprintf "CASC-FRESH-DECODE %s store=true" (s_type_path path))
			end;
			idsplit_module com "CASC-DECODE" m; idsplit_tooltip com "CASC-DECODE" m;
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
			(* The macro context executes and inlines field bodies, so it cannot defer them: an inline
			   field restored body-lazy has cf_expr = None at expansion time (calls.ml "Recursive inline").
			   Read to EOM eagerly like FullTyping; deferral stays a display-only optimization. *)
			| AllowPartialTyping when com.is_macro_context -> ignore(f_next chunks EOM)
			| AllowPartialTyping -> delay PConnectField (fun () -> ignore(f_next chunks EOF)));
			incr com.request_scope.stats.s_modules_restored;
			m
		| BadModule reason ->
			die (Printf.sprintf "Unexpected BadModule %s (%s)" (s_type_path path) (Printer.s_module_skip_reason reason)) __LOC__
		| NoModule ->
			die (Printf.sprintf "Unexpected NoModule %s" (s_type_path path)) __LOC__

	method find_module (m_path : path) typing_mode =
		try
			GoodModule (com.module_lut#find m_path)
		with Not_found ->
			(* hxb.resident_modules canonical registry: the cascade cross-ref path must consult the SAME
			   registry as the top-level load, and SEED module_lut from it, so a path resolves to ONE
			   canonical object across both paths (the cont.8 identity-split fix). Without this the cascade
			   re-decodes a separate generation that a resident module's frozen ref will not unify with. *)
			let resident_enabled = Define.defined com.defines Define.HxbResidentModules && not com.display.dms_full_typing in
			match (if resident_enabled then try_serve_resident com cc m_path else None) with
			| Some m ->
				com.module_lut#add m_path m;
				idsplit_module com "CASCSERVE" m; idsplit_tooltip com "CASCSERVE" m;
				GoodModule m
			| None -> get_hxb_module com cc m_path typing_mode

	method basic_types =
		com.basic

	method get_var_id (i : int) =
		i

	method read_expression_eagerly (cf : tclass_field) =
		(* Header invalidation: never read bodies eagerly, so a restore in the seed re-typing cascade does
		   not force a body type-ref to a cyclic peer that is not loaded yet. Bodies are deferred (TLazy)
		   and forced later, once the peers exist. *)
		not (Define.defined com.defines Define.HxbHeaderInvalidation) &&
		(com.is_macro_context || com.display.dms_full_typing || Define.defined com.defines Define.DisableHxbOptimizations)

	method make_lazy_type t f =
		let r = make_unforced_lazy t f "server-api" in
		delay PForce (fun () -> ignore(lazy_type r));
		TLazy r

	method forward_classes = com.hxb_forward_classes
	(* Inheritance forwarding mints identity-only placeholder tclasses; that is safe for the display
	   context (inspect-only) but breaks TInst identity in the macro/eval path, where build macros unify
	   freshly-built fields against std macro types (e.g. Array<haxe.macro.Field>). Keep it off for the
	   macro context. *)
	method forwarding_enabled =
		Define.defined com.defines Define.HxbLazyInheritance && not com.is_macro_context
end

(* hxb.resident_modules: one shared reader api per (context, is_macro), reused across requests. Resident
   modules' lazy closures capture THIS api; re-pointing it at the current request's com (set_request) when we
   decode AND when we serve a resident module makes those closures resolve through the live request instead of
   the dead one that originally decoded them. *)
let shared_reader_apis : (int * bool, hxb_reader_api_server) Hashtbl.t = Hashtbl.create 0

let ensure_shared_reader_api com cc delay =
	let key = (cc#get_index, com.is_macro_context) in
	let api =
		try Hashtbl.find shared_reader_apis key
		with Not_found ->
			let api = new hxb_reader_api_server com cc delay in
			Hashtbl.replace shared_reader_apis key api;
			api
	in
	api#set_request com delay;
	api

(* hxb.resident_verbose diagnostic: re-serialize a resident module and diff its chunks against the original
   cached chunks, to pin which module/chunk was mutated in-place since it was decoded. *)
let resident_mutation_diff com cc m mc =
	try
		let anon_identification = new Tanon_identification.tanon_identification in
		let config = match com.hxb_writer_config with
			| None -> HxbWriterConfig.create_target_config ()
			| Some config -> if com.is_macro_context then config.macro_config else config.target_config
		in
		let writer = HxbWriter.create config (fun _ _ _ -> ()) anon_identification in
		HxbWriter.write_module writer m;
		let fresh = HxbWriter.get_chunks writer in
		CommonCache.unexpected_chunk_diffs mc.HxbData.mc_chunks fresh
	with e ->
		Some (Printf.sprintf "diff-exn: %s" (Printexc.to_string e))

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
		if m.m_extra.m_cache_state = MSBad Reprocessing then
			(* Header invalidation: a seed being re-typed; leave it (it is handled by the pre-phase /
			   resolved from cache), do not treat it as a bad module. *)
			()
		else if m.m_extra.m_cache_state <> MSGood then begin
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
				if not from_binary || m != m then
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
							| BadModule reason ->
								failwith (Printf.sprintf "Unexpected bad module %s (%s) as a dependency of %s" (s_type_path mpath) (Printer.s_module_skip_reason reason) (s_type_path m0.m_path))
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
		ServerMessage.skipping_dep com "" (m_path,(Printer.s_module_skip_reason reason));
		BadModule reason
	in
	let add_modules from_binary m =
		Timer.time com.timer_ctx ["server";"module cache";"add modules"] (add_modules sctx com delay m from_binary) p;
		GoodModule m
	in
	let check_module sctx m_path m_extra p =
		Timer.time com.timer_ctx ["server";"module cache";"check"] (check_module sctx com mpath m_extra) p
	in
	(* hxb.resident_modules is a DISPLAY-request optimization only: it serves/stores AllowPartialTyping (EOT,
	   body-lazy) modules and routes decoding through a shared cross-request reader api. A full-typing request
	   (a normal build, or the macro context, both of which set dms_full_typing) must NOT touch the tier — its
	   modules are FullTyping (resident EOT copies can't satisfy them) and, critically, it must not decode
	   through the shared api, whose state carries over from the previous display request and would corrupt the
	   full compile (e.g. type redefinition). Gating the whole tier on a genuine display request keeps resident
	   state from ever leaking into a full compile. *)
	let resident_enabled =
		Define.defined com.defines Define.HxbResidentModules && not com.display.dms_full_typing
	in
	let find_module_in_cache cc m_path p =
		let from_cc_or_binary () =
			try
				let m = cc#find_module m_path in
				idsplit_module com "TOP-GOOD" m; idsplit_tooltip com "TOP-GOOD" m;
				begin match m.m_extra.m_cache_state with
					| MSBad reason -> BadModule reason
					| _ -> GoodModule m
				end;
			with Not_found -> get_hxb_module com cc m_path FullTyping
		in
		(* hxb.resident_modules: reuse a restored module kept resident from a previous request instead of
		   re-decoding it. Hand out a fresh m_extra so this request's dep/state mutations don't pollute the
		   resident object; check_module still validates source freshness afterwards. Only reuse for
		   AllowPartialTyping (we only ever store those — an EOT resident can't satisfy a FullTyping load).
		   A changed binary entry (mc_id mismatch) or missing entry purges the stale resident. *)
		let rlog tag = if Define.raw_defined com.defines "hxb.resident_verbose" then begin
			let oc = diag_open "resident_hits.log" in
			output_string oc (Printf.sprintf "%s %s\n" tag (s_type_path m_path)); close_out oc
		end in
		match (if resident_enabled then try_serve_resident com cc m_path else None) with
		| None ->
			rlog "MISS"; from_cc_or_binary ()
		| Some m ->
			rlog "HIT";
			idsplit_module com "TOPSERVE" m; idsplit_tooltip com "TOPSERVE" m;
			(* Re-point the shared api (whose closures this resident module captured) at the current request
			   before its body lazies can be forced during typing. The top-level load may be the first thing
			   this request does, so set_request has not necessarily run yet (unlike the cascade path, where
			   self is already active). *)
			ignore(ensure_shared_reader_api com cc delay);
			begin match m.m_extra.m_cache_state with
				| MSBad reason -> BadModule reason
				| _ -> GoodModule m
			end
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
					let api =
						if resident_enabled then
							(ensure_shared_reader_api com cc delay :> HxbReaderApi.hxb_reader_api)
						else match com.hxb_reader_api with
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
					decode_log com "TOP" mpath;

					(* We try to avoid reading expressions as much as possible, so we only do this for
					   our current display file if we're in display mode. *)
					(match typing_mode with
					| FullTyping -> ignore(f_next chunks EOM)
					(* The macro context executes and inlines field bodies, so it cannot defer them: an inline
					   field restored body-lazy has cf_expr = None at expansion time (calls.ml "Recursive inline").
					   Read to EOM eagerly like FullTyping; deferral stays a display-only optimization. *)
					| AllowPartialTyping when com.is_macro_context -> ignore(f_next chunks EOM)
					| AllowPartialTyping -> delay PConnectField (fun () -> ignore(f_next chunks EOF)));
					incr com.request_scope.stats.s_modules_restored;
					(* hxb.resident_modules: keep this restored module resident so the next request reuses it
					   instead of re-decoding. Only AllowPartialTyping (EOT) modules; the deferred EOF connect
					   runs on this same object before the request ends, so the resident copy is the full
					   restored module. *)
					idsplit_module com "TOP-DECODE" m; idsplit_tooltip com "TOP-DECODE" m;
					if idsplit_is_target (snd mpath) then idsplit_log com "LIFECYCLE" (Printf.sprintf "FRESH-DECODE %s mode=%s store=%b" (s_type_path mpath) (match typing_mode with FullTyping -> "FULL" | AllowPartialTyping -> "PARTIAL") (resident_enabled && typing_mode = AllowPartialTyping));
					if resident_enabled && typing_mode = AllowPartialTyping then
						cc#cache_resident_module mpath m;
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
		interp.curapi <- MacroApi.null_api ()
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

let after_save sctx com =
	if sctx.is_server && not (Common.has_error_to_report com) then
		CommonCache.maybe_cache_context com

let enable_cache_mode sctx =
	type_module_hook := type_module sctx;
	ensure_macro_setup sctx;
	TypeloadParse.parse_hook := parse_file sctx