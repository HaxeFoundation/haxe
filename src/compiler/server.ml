open Printf
open Globals
open Ast
open Common
open CompilationCache
open Timer
open Type
open DisplayProcessingGlobals
open Json
open Compiler
open CompilationContext

exception Dirty of module_skip_reason
exception ServerError of string

let has_error ctx =
	ctx.has_error || ctx.com.Common.has_error

let check_display_flush ctx f_otherwise = match ctx.com.json_out with
	| None ->
		if is_diagnostics ctx.com then begin
			List.iter (fun (msg,p,kind,sev) ->
				add_diagnostics_message ctx.com msg p kind sev
			) (List.rev ctx.messages);
			raise (Completion (Diagnostics.print ctx.com))
		end else
			f_otherwise ()
	| Some api ->
		if has_error ctx then begin
			let errors = List.map (fun (msg,p,_,sev) ->
				JObject [
					"severity",JInt (MessageSeverity.to_int sev);
					"location",Genjson.generate_pos_as_location p;
					"message",JString msg;
				]
			) (List.rev ctx.messages) in
			api.send_error errors
		end

let current_stdin = ref None

let parse_file cs com file p =
	let cc = CommonCache.get_cache com in
	let ffile = Path.get_full_path file
	and fkey = com.file_keys#get file in
	let is_display_file = DisplayPosition.display_position#is_in_file (com.file_keys#get ffile) in
	match is_display_file, !current_stdin with
	| true, Some stdin when Common.defined com Define.DisplayStdin ->
		TypeloadParse.parse_file_from_string com file p stdin
	| _ ->
		let ftime = file_time ffile in
		let data = Std.finally (Timer.timer ["server";"parser cache"]) (fun () ->
			try
				let cfile = cc#find_file fkey in
				if cfile.c_time <> ftime then raise Not_found;
				Parser.ParseSuccess((cfile.c_package,cfile.c_decls),false,cfile.c_pdi)
			with Not_found ->
				let parse_result = TypeloadParse.parse_file com file p in
				let info,is_unusual = match parse_result with
					| ParseError(_,_,_) -> "not cached, has parse error",true
					| ParseSuccess(data,is_display_file,pdi) ->
						if is_display_file then begin
							if pdi.pd_errors <> [] then
								"not cached, is display file with parse errors",true
							else if com.display.dms_per_file then begin
								cc#cache_file fkey ffile ftime data pdi;
								"cached, is intact display file",true
							end else
								"not cached, is display file",true
						end else begin try
							(* We assume that when not in display mode it's okay to cache stuff that has #if display
							checks. The reasoning is that non-display mode has more information than display mode. *)
							if com.display.dms_full_typing then raise Not_found;
							let ident = Hashtbl.find Parser.special_identifier_files fkey in
							Printf.sprintf "not cached, using \"%s\" define" ident,true
						with Not_found ->
							cc#cache_file fkey ffile ftime data pdi;
							"cached",false
						end
				in
				if is_unusual then ServerMessage.parsed com "" (ffile,info);
				parse_result
		) () in
		data

open ServerCompilationContext

module Communication = struct

	let compiler_message_string (str,p,_,sev) =
		let str = match sev with
			| MessageSeverity.Warning -> "Warning : " ^ str
			| Information | Error | Hint -> str
		in
		if p = null_pos then
			str
		else begin
			let error_printer file line = Printf.sprintf "%s:%d:" file line in
			let epos = Lexer.get_error_pos error_printer p in
			let str =
				let lines =
					match (ExtString.String.nsplit str "\n") with
					| first :: rest -> first :: List.map Error.compl_msg rest
					| l -> l
				in
				String.concat ("\n" ^ epos ^ " : ") lines
			in
			Printf.sprintf "%s : %s" epos str
		end

	let create_stdio () =
		let rec self = {
			write_out = (fun s ->
				print_string s;
				flush stdout;
			);
			write_err = (fun s ->
				prerr_string s;
			);
			flush = (fun ctx ->
				List.iter (fun ((_,_,_,sev) as cm) -> match sev with
					| MessageSeverity.Information -> print_endline (compiler_message_string cm)
					| Warning | Error | Hint -> prerr_endline (compiler_message_string cm)
				) (List.rev ctx.messages);
				if has_error ctx && !Helper.prompt then begin
					print_endline "Press enter to exit...";
					ignore(read_line());
				end;
				flush stdout;
			);
			exit = (fun code ->
				if code = 0 then begin
					Timer.close_times();
					if !Timer.measure_times then Timer.report_times (fun s -> self.write_err (s ^ "\n"));
				end;
				exit code;
			);
			is_server = false;
		} in
		self

	let create_pipe sctx write = {
		write_out = (fun s ->
			write ("\x01" ^ String.concat "\x01" (ExtString.String.nsplit s "\n") ^ "\n")
		);
		write_err = (fun s ->
			write s
		);
		flush = (fun ctx ->
			check_display_flush ctx (fun () ->
				List.iter
					(fun msg ->
						let s = compiler_message_string msg in
						write (s ^ "\n");
						ServerMessage.message s;
					)
					(List.rev ctx.messages);
				sctx.was_compilation <- ctx.com.display.dms_full_typing;
				if has_error ctx then begin
					measure_times := false;
					write "\x02\n"
				end
			)
		);
		exit = (fun i ->
			()
		);
		is_server = true;
	}
end

let stat dir =
	(Unix.stat (Path.remove_trailing_slash dir)).Unix.st_mtime

(* Gets a list of changed directories for the current compilation. *)
let get_changed_directories sctx (ctx : Typecore.typer) =
	let t = Timer.timer ["server";"module cache";"changed dirs"] in
	let cs = sctx.cs in
	let com = ctx.Typecore.com in
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
				List.iter add_dir com.class_path;
				List.iter add_dir (Path.find_directories (platform_name com.platform) true com.class_path);
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
	t();
	dirs

(* Checks if module [m] can be reused from the cache and returns None in that case. Otherwise, returns
   [Some m'] where [m'] is the module responsible for [m] not being reusable. *)
let check_module sctx ctx m p =
	let com = ctx.Typecore.com in
	let cc = CommonCache.get_cache com in
	let content_changed m file =
		let fkey = ctx.com.file_keys#get file in
		try
			let cfile = cc#find_file fkey in
			(* We must use the module path here because the file path is absolute and would cause
				positions in the parsed declarations to differ. *)
			let new_data = TypeloadParse.parse_module ctx m.m_path p in
			cfile.c_decls <> snd new_data
		with Not_found ->
			true
	in
	let check_module_shadowing paths m =
		List.iter (fun dir ->
			let file = (dir.c_path ^ (snd m.m_path)) ^ ".hx" in
			if Sys.file_exists file then begin
				let time = file_time file in
				if time > m.m_extra.m_time then begin
					ServerMessage.module_path_changed com "" (m,time,file);
					raise (Dirty (Shadowed file))
				end
			end
		) paths
	in
	let start_mark = sctx.compilation_step in
	let unknown_state_modules = ref [] in
	let rec check m =
		let check_module_path () =
			let directories = get_changed_directories sctx ctx in
			match m.m_extra.m_kind with
			| MFake | MImport -> () (* don't get classpath *)
			| MExtern ->
				(* if we have a file then this will override our extern type *)
				check_module_shadowing directories m;
				let rec loop = function
					| [] ->
						if sctx.verbose then print_endline ("No library file was found for " ^ s_type_path m.m_path); (* TODO *)
						raise (Dirty LibraryChanged)
					| (file,load) :: l ->
						match load m.m_path p with
						| None ->
							loop l
						| Some _ ->
							if com.file_keys#get file <> (Path.UniqueKey.lazy_key m.m_extra.m_file) then begin
								if sctx.verbose then print_endline ("Library file was changed for " ^ s_type_path m.m_path); (* TODO *)
								raise (Dirty LibraryChanged)
							end
				in
				loop com.load_extern_type
			| MCode ->
				check_module_shadowing directories m
			| MMacro when com.is_macro_context ->
				check_module_shadowing directories m
			| MMacro ->
				(*
					Creating another context while the previous one is incomplete means we have an infinite loop in the compiler.
					Most likely because of circular dependencies in base modules (e.g. `StdTypes` or `String`)
					Prevents spending another 5 hours for debugging.
					@see https://github.com/HaxeFoundation/haxe/issues/8174
				*)
				if not ctx.g.complete && ctx.com.is_macro_context then
					raise (ServerError ("Infinite loop in Haxe server detected. "
						^ "Probably caused by shadowing a module of the standard library. "
						^ "Make sure shadowed module does not pull macro context."));
				let _, mctx = MacroContext.get_macro_context ctx p in
				check_module_shadowing (get_changed_directories sctx mctx) m
		in
		let has_policy policy = List.mem policy m.m_extra.m_check_policy || match policy with
			| NoCheckShadowing | NoCheckFileTimeModification when !ServerConfig.do_not_check_modules && !Parser.display_mode <> DMNone -> true
			| _ -> false
		in
		let check_file () =
			let file = Path.UniqueKey.lazy_path m.m_extra.m_file in
			if file_time file <> m.m_extra.m_time then begin
				if has_policy CheckFileContentModification && not (content_changed m file) then begin
					ServerMessage.unchanged_content com "" file;
				end else begin
					ServerMessage.not_cached com "" m;
					if m.m_extra.m_kind = MFake then Hashtbl.remove Typecore.fake_modules (Path.UniqueKey.lazy_key m.m_extra.m_file);
					raise (Dirty (FileChanged file))
				end
			end
		in
		let check_dependencies () =
			PMap.iter (fun _ m2 -> match check m2 with
				| None -> ()
				| Some reason -> raise (Dirty (DependencyDirty(m2.m_path,reason)))
			) m.m_extra.m_deps;
		in
		let check () =
			try
				if not (has_policy NoCheckShadowing) then check_module_path();
				if not (has_policy NoCheckFileTimeModification) || Path.file_extension (Path.UniqueKey.lazy_path m.m_extra.m_file) <> "hx" then check_file();
				if not (has_policy NoCheckDependencies) then check_dependencies();
				None
			with
			| Dirty reason ->
				Some reason
		in
		(* If the module mark matches our compilation mark, we are done *)
		if m.m_extra.m_checked = start_mark then begin match m.m_extra.m_cache_state with
			| MSGood | MSUnknown ->
				None
			| MSBad reason ->
				Some reason
		end else begin
			(* Otherwise, set to current compilation mark for recursion *)
			m.m_extra.m_checked <- start_mark;
			let dirty = match m.m_extra.m_cache_state with
				| MSBad reason ->
					(* If we are already dirty, stick to it. *)
					Some reason
				| MSUnknown	->
					(* This should not happen because any MSUnknown module is supposed to have the current m_checked. *)
					die "" __LOC__
				| MSGood ->
					(* Otherwise, run the checks *)
					m.m_extra.m_cache_state <- MSUnknown;
					check ()
			in
			let dirty = match dirty with
				| Some (DependencyDirty _) when has_policy Retype ->
					let result = Retyper.attempt_retyping ctx m p in
					begin match result with
					| None ->
						ServerMessage.retyper_ok com "" m;
						None
					| Some reason ->
						ServerMessage.retyper_fail com "" m reason;
						dirty
					end
				| _ ->
					dirty
			in
			(* Update the module now. It will use this dirty status for the remainder of this compilation. *)
			begin match dirty with
			| Some reason ->
				(* Update the state if we're dirty. *)
				m.m_extra.m_cache_state <- MSBad reason;
			| None ->
				(* We cannot update if we're clean because at this point it might just be an assumption.
				   Instead We add the module to a list which is updated at the end of handling this subgraph. *)
				unknown_state_modules := m :: !unknown_state_modules;
			end;
			dirty
		end
	in
	let state = check m in
	begin match state with
	| None ->
		(* If the entire subgraph is clean, we can set all modules to good state *)
		List.iter (fun m -> m.m_extra.m_cache_state <- MSGood) !unknown_state_modules;
	| Some _ ->
		(* Otherwise, unknown state module may or may not be dirty. We didn't check everything eagerly, so we have
		   to make sure that the module is checked again if it appears in a different check. This is achieved by
		   setting m_checked to a lower value and assuming Good state again. *)
		List.iter (fun m -> match m.m_extra.m_cache_state with
			| MSUnknown ->
				m.m_extra.m_checked <- start_mark - 1;
				m.m_extra.m_cache_state <- MSGood;
			| MSGood | MSBad _ ->
				()
		) !unknown_state_modules
	end;
	state

(* Adds module [m] and all its dependencies (recursively) from the cache to the current compilation
   context. *)
let add_modules sctx ctx m p =
	let com = ctx.Typecore.com in
	let rec add_modules tabs m0 m =
		if m.m_extra.m_added < ctx.com.compilation_step then begin
			(match m0.m_extra.m_kind, m.m_extra.m_kind with
			| MCode, MMacro | MMacro, MCode ->
				(* this was just a dependency to check : do not add to the context *)
				PMap.iter (Hashtbl.replace com.resources) m.m_extra.m_binded_res;
			| _ ->
				m.m_extra.m_added <- ctx.com.compilation_step;
				ServerMessage.reusing com tabs m;
				List.iter (fun t ->
					match t with
					| TClassDecl c -> c.cl_restore()
					| TEnumDecl e ->
						let rec loop acc = function
							| [] -> ()
							| (Meta.RealPath,[Ast.EConst (Ast.String(path,_)),_],_) :: l ->
								e.e_path <- Ast.parse_path path;
								e.e_meta <- (List.rev acc) @ l;
							| x :: l -> loop (x::acc) l
						in
						loop [] e.e_meta
					| TAbstractDecl a ->
						a.a_meta <- List.filter (fun (m,_,_) -> m <> Meta.ValueUsed) a.a_meta
					| _ -> ()
				) m.m_types;
				TypeloadModule.ModuleLevel.add_module ctx m p;
				PMap.iter (Hashtbl.replace com.resources) m.m_extra.m_binded_res;
				PMap.iter (fun _ m2 -> add_modules (tabs ^ "  ") m0 m2) m.m_extra.m_deps
			)
		end
	in
	add_modules "" m m

(* Looks up the module referred to by [mpath] in the cache. If it exists, a check is made to
   determine if it's still valid. If this function returns None, the module is re-typed. *)
let type_module sctx (ctx:Typecore.typer) mpath p =
	let t = Timer.timer ["server";"module cache"] in
	let com = ctx.Typecore.com in
	let cc = CommonCache.get_cache com in
	try
		let m = cc#find_module mpath in
		let tcheck = Timer.timer ["server";"module cache";"check"] in
		begin match check_module sctx ctx m p with
		| None -> ()
		| Some reason ->
			ServerMessage.skipping_dep com "" (m,(Printer.s_module_skip_reason reason));
			tcheck();
			raise Not_found;
		end;
		tcheck();
		let tadd = Timer.timer ["server";"module cache";"add modules"] in
		add_modules sctx ctx m p;
		tadd();
		t();
		Some m
	with Not_found ->
		t();
		None

let before_anything sctx ctx =
	ensure_macro_setup sctx

let after_arg_parsing sctx ctx =
	let com = ctx.com in
	let cs = sctx.cs in
	let sign = Define.get_signature com.defines in
	ServerMessage.defines com "";
	ServerMessage.signature com "" sign;
	ServerMessage.display_position com "" (DisplayPosition.display_position#get);
	try
		if (Hashtbl.find sctx.class_paths sign) <> com.class_path then begin
			ServerMessage.class_paths_changed com "";
			Hashtbl.replace sctx.class_paths sign com.class_path;
			cs#clear_directories sign;
			(cs#get_context sign)#set_initialized false;
		end;
	with Not_found ->
		Hashtbl.add sctx.class_paths sign com.class_path;
		()

let after_compilation sctx ctx =
	if not (has_error ctx) then
		maybe_cache_context sctx ctx.com

let mk_length_prefixed_communication allow_nonblock chin chout =
	let sin = Unix.descr_of_in_channel chin in
	let chin = IO.input_channel chin in
	let chout = IO.output_channel chout in

	let bout = Buffer.create 0 in

	let block () = Unix.clear_nonblock sin in
	let unblock () = Unix.set_nonblock sin in

	let read_nonblock _ =
        let len = IO.read_i32 chin in
        Some (IO.really_nread_string chin len)
	in
	let read = if allow_nonblock then fun do_block ->
		if do_block then begin
			block();
			read_nonblock true;
		end else begin
			let c0 =
				unblock();
				try
					Some (IO.read_byte chin)
				with
				| Sys_blocked_io
				(* TODO: We're supposed to catch Sys_blocked_io only, but that doesn't work on my PC... *)
				| Sys_error _ ->
					None
			in
			begin match c0 with
			| Some c0 ->
				block(); (* We got something, make sure we block until we're done. *)
				let c1 = IO.read_byte chin in
				let c2 = IO.read_byte chin in
				let c3 = IO.read_byte chin in
				let len = c3 lsl 24 + c2 lsl 16 + c1 lsl 8 + c0 in
				Some (IO.really_nread_string chin len)
			| None ->
				None
			end
		end
	else read_nonblock in

	let write = Buffer.add_string bout in

	let close = fun() ->
		flush stdout;
		IO.write_i32 chout (Buffer.length bout);
		IO.nwrite_string chout (Buffer.contents bout);
		IO.flush chout
	in

	fun () ->
		Buffer.clear bout;
		allow_nonblock, read, write, close

let ssend sock str =
	let rec loop pos len =
		if len = 0 then
			()
		else
			let s = Unix.send sock str pos len [] in
			loop (pos + s) (len - s)
	in
	loop 0 (Bytes.length str)

(* The accept-function to wait for a stdio connection. *)
let init_wait_stdio() =
	set_binary_mode_in stdin true;
	set_binary_mode_out stderr true;
	mk_length_prefixed_communication false stdin stderr

(* The connect function to connect to [host] at [port] and send arguments [args]. *)
let do_connect host port args =
	let sock = Unix.socket Unix.PF_INET Unix.SOCK_STREAM 0 in
	(try Unix.connect sock (Unix.ADDR_INET (Unix.inet_addr_of_string host,port)) with _ -> failwith ("Couldn't connect on " ^ host ^ ":" ^ string_of_int port));
	let rec display_stdin args =
		match args with
		| [] -> ""
		| "-D" :: ("display_stdin" | "display-stdin") :: _ ->
			let accept = init_wait_stdio() in
			let _, read, _, _ = accept() in
			Option.default "" (read true)
		| _ :: args ->
			display_stdin args
	in
	let args = ("--cwd " ^ Unix.getcwd()) :: args in
	let s = (String.concat "" (List.map (fun a -> a ^ "\n") args)) ^ (display_stdin args) in
	ssend sock (Bytes.of_string (s ^ "\000"));
	let has_error = ref false in
	let rec print line =
		match (if line = "" then '\x00' else line.[0]) with
		| '\x01' ->
			print_string (String.concat "\n" (List.tl (ExtString.String.nsplit line "\x01")));
			flush stdout
		| '\x02' ->
			has_error := true;
		| _ ->
			prerr_endline line;
	in
	let buf = Buffer.create 0 in
	let process() =
		let lines = ExtString.String.nsplit (Buffer.contents buf) "\n" in
		(* the last line ends with \n *)
		let lines = (match List.rev lines with "" :: l -> List.rev l | _ -> lines) in
		List.iter print lines;
	in
	let tmp = Bytes.create 1024 in
	let rec loop() =
		let b = Unix.recv sock tmp 0 1024 [] in
		Buffer.add_subbytes buf tmp 0 b;
		if b > 0 then begin
			if Bytes.get tmp (b - 1) = '\n' then begin
				process();
				Buffer.reset buf;
			end;
			loop();
		end
	in
	loop();
	process();
	if !has_error then exit 1

let enable_cache_mode sctx =
	TypeloadModule.type_module_hook := type_module sctx;
	MacroContext.macro_enable_cache := true;
	ServerCompilationContext.ensure_macro_setup sctx;
	TypeloadParse.parse_hook := parse_file sctx.cs

let rec process sctx comm args =
	let t0 = get_time() in
	ServerMessage.arguments args;
	reset sctx;
	let api = {
		on_context_create = (fun () ->
			sctx.compilation_step <- sctx.compilation_step + 1;
			sctx.compilation_step;
		);
		cache = sctx.cs;
		callbacks = {
			before_anything = before_anything sctx;
			after_arg_parsing = after_arg_parsing sctx;
			after_compilation = after_compilation sctx;
		};
		init_wait_socket = init_wait_socket;
		init_wait_connect = init_wait_connect;
		init_wait_stdio = init_wait_stdio;
		wait_loop = wait_loop;
		do_connect = do_connect;
	} in
	Compiler.HighLevel.entry api comm args;
	run_delays sctx;
	ServerMessage.stats stats (get_time() -. t0)

(* The server main loop. Waits for the [accept] call to then process the sent compilation
   parameters through [process_params]. *)
and wait_loop verbose accept =
	if verbose then ServerMessage.enable_all ();
	Sys.catch_break false; (* Sys can never catch a break *)
	(* Create server context and set up hooks for parsing and typing *)
	let sctx = ServerCompilationContext.create verbose in
	let cs = sctx.cs in
	enable_cache_mode sctx;
	let ring = Ring.create 10 0. in
	let gc_heap_stats () =
		let stats = Gc.quick_stat() in
		stats.major_words,stats.heap_words
	in
	let heap_stats_start = ref (gc_heap_stats()) in
	let update_heap () =
		(* On every compilation: Track how many words were allocated for this compilation (working memory). *)
		let heap_stats_now = gc_heap_stats() in
		let words_allocated = (fst heap_stats_now) -. (fst !heap_stats_start) in
		let heap_size = float_of_int (snd heap_stats_now) in
		Ring.push ring words_allocated;
		if Ring.is_filled ring then begin
			Ring.reset_filled ring;
			 (* Maximum working memory for the last X compilations. *)
			let max = Ring.fold ring 0. (fun m i -> if i > m then i else m) in
			cs#add_task (new Tasks.gc_task max heap_size)
		end;
		heap_stats_start := heap_stats_now;
	in
	(* Main loop: accept connections and process arguments *)
	while true do
		let support_nonblock, read, write, close = accept() in
		begin try
			(* Read arguments *)
			let rec loop block =
				match read block with
				| Some s ->
					let hxml =
						try
							let idx = String.index s '\001' in
							current_stdin := Some (String.sub s (idx + 1) ((String.length s) - idx - 1));
							(String.sub s 0 idx)
						with Not_found ->
							s
					in
					let data = Helper.parse_hxml_data hxml in
					process sctx (Communication.create_pipe sctx write) data
				| None ->
					if not cs#has_task then
						(* If there is no pending task, turn into blocking mode. *)
						loop true
					else begin
						(* Otherwise run the task and loop to check if there are more or if there's a request now. *)
						cs#get_task#run;
						loop false
					end;
			in
			loop (not support_nonblock)
		with Unix.Unix_error _ ->
			ServerMessage.socket_message "Connection Aborted"
		| e ->
			let estr = Printexc.to_string e in
			ServerMessage.uncaught_error estr;
			(try write ("\x02\n" ^ estr); with _ -> ());
			if Helper.is_debug_run then print_endline (estr ^ "\n" ^ Printexc.get_backtrace());
			if e = Out_of_memory then begin
				close();
				exit (-1);
			end;
		end;
		(* Close connection and perform some cleanup *)
		close();
		current_stdin := None;
		cleanup();
		update_heap();
		(* If our connection always blocks, we have to execute all pending tasks now. *)
		if not support_nonblock then
			while cs#has_task do cs#get_task#run done
		else if sctx.was_compilation then
			cs#add_task (new Tasks.server_exploration_task cs)
	done;
	0

(* Connect to given host/port and return accept function for communication *)
and init_wait_connect host port =
	let host = Unix.inet_addr_of_string host in
	let chin, chout = Unix.open_connection (Unix.ADDR_INET (host,port)) in
	mk_length_prefixed_communication true chin chout

(* The accept-function to wait for a socket connection. *)
and init_wait_socket host port =
	let sock = Unix.socket Unix.PF_INET Unix.SOCK_STREAM 0 in
	(try Unix.setsockopt sock Unix.SO_REUSEADDR true with _ -> ());
	(try Unix.bind sock (Unix.ADDR_INET (Unix.inet_addr_of_string host,port)) with _ -> failwith ("Couldn't wait on " ^ host ^ ":" ^ string_of_int port));
	ServerMessage.socket_message ("Waiting on " ^ host ^ ":" ^ string_of_int port);
	Unix.listen sock 10;
	let bufsize = 1024 in
	let tmp = Bytes.create bufsize in
	let accept() = (
		let sin, _ = Unix.accept sock in
		Unix.set_nonblock sin;
		ServerMessage.socket_message "Client connected";
		let b = Buffer.create 0 in
		let rec read_loop count =
			try
				let r = Unix.recv sin tmp 0 bufsize [] in
				if r = 0 then
					failwith "Incomplete request"
				else begin
					ServerMessage.socket_message (Printf.sprintf "Reading %d bytes\n" r);
					Buffer.add_subbytes b tmp 0 r;
					if Bytes.get tmp (r-1) = '\000' then
						Buffer.sub b 0 (Buffer.length b - 1)
					else
						read_loop 0
				end
			with Unix.Unix_error((Unix.EWOULDBLOCK|Unix.EAGAIN),_,_) ->
				if count = 100 then
					failwith "Aborting inactive connection"
				else begin
					ServerMessage.socket_message "Waiting for data...";
					ignore(Unix.select [] [] [] 0.05); (* wait a bit *)
					read_loop (count + 1);
				end
		in
		let read = fun _ -> (let s = read_loop 0 in Unix.clear_nonblock sin; Some s) in
		let write s = ssend sin (Bytes.unsafe_of_string s) in
		let close() = Unix.close sin in
		false, read, write, close
	) in
	accept