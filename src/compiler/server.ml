open Printf
open Globals
open Ast
open Common
open CompilationServer
open Timer
open Type
open DisplayOutput
open Json
open Compiler
open CompilationContext

exception Dirty of path
exception ServerError of string

let check_display_flush ctx f_otherwise = match ctx.com.json_out with
	| None ->
		begin match ctx.com.display.dms_kind with
		| DMDiagnostics _->
			List.iter (fun msg ->
				let msg,p,kind = match msg with
					| CMInfo(msg,p) -> msg,p,DisplayTypes.DiagnosticsSeverity.Information
					| CMWarning(msg,p) -> msg,p,DisplayTypes.DiagnosticsSeverity.Warning
					| CMError(msg,p) -> msg,p,DisplayTypes.DiagnosticsSeverity.Error
				in
				add_diagnostics_message ctx.com msg p DisplayTypes.DiagnosticsKind.DKCompilerError kind
			) (List.rev ctx.messages);
			raise (Completion (Diagnostics.print ctx.com))
		| _ ->
			f_otherwise ()
		end
	| Some api ->
		if ctx.has_error then begin
			let errors = List.map (fun msg ->
				let msg,p,i = match msg with
					| CMInfo(msg,p) -> msg,p,3
					| CMWarning(msg,p) -> msg,p,2
					| CMError(msg,p) -> msg,p,1
				in
				JObject [
					"severity",JInt i;
					"location",Genjson.generate_pos_as_location p;
					"message",JString msg;
				]
			) (List.rev ctx.messages) in
			api.send_error errors
		end

let parse_hxml_data data =
	let lines = Str.split (Str.regexp "[\r\n]+") data in
	List.concat (List.map (fun l ->
		let l = unquote (ExtString.String.strip l) in
		if l = "" || l.[0] = '#' then
			[]
		else if l.[0] = '-' then
			try
				let a, b = ExtString.String.split l " " in
				[unquote a; unquote (ExtString.String.strip b)]
			with
				_ -> [l]
		else
			[l]
	) lines)

let parse_hxml file =
	let ch = IO.input_channel (try open_in_bin file with _ -> raise Not_found) in
	let data = IO.read_all ch in
	IO.close_in ch;
	parse_hxml_data data

let current_stdin = ref None

let parse_file cs com file p =
	let cc = CommonCache.get_cache cs com in
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
							if not com.display.dms_display then raise Not_found;
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

module ServerCompilationContext = struct
	type t = {
		(* If true, prints some debug information *)
		verbose : bool;
		(* The list of changed directories per-signature *)
		changed_directories : (Digest.t,cached_directory list) Hashtbl.t;
		(* A reference to the compilation server instance *)
		cs : CompilationServer.t;
		(* A list of class paths per-signature *)
		class_paths : (Digest.t,string list) Hashtbl.t;
		(* Increased for each typed module *)
		mutable mark_loop : int;
		(* Increased for each compilation *)
		mutable compilation_step : int;
		(* The [mark_loop] value at which we started the current compilation *)
		mutable compilation_mark : int;
		(* A list of delays which are run after compilation *)
		mutable delays : (unit -> unit) list;
		(* True if it's an actual compilation, false if it's a display operation *)
		mutable was_compilation : bool;
	}

	let create verbose cs = {
		verbose = verbose;
		cs = cs;
		class_paths = Hashtbl.create 0;
		changed_directories = Hashtbl.create 0;
		compilation_step = 0;
		compilation_mark = 0;
		mark_loop = 0;
		delays = [];
		was_compilation = false;
	}

	let add_delay sctx f =
		sctx.delays <- f :: sctx.delays

	let run_delays sctx =
		let fl = sctx.delays in
		sctx.delays <- [];
		List.iter (fun f -> f()) fl

	let reset sctx =
		Hashtbl.clear sctx.changed_directories;
		sctx.was_compilation <- false
end

open ServerCompilationContext

class virtual server_communication
	(sctx : ServerCompilationContext.t)
= object(self)
	method virtual out : string -> unit (* like stdout *)
	method virtual err : string -> unit (* like stderr *)
	method virtual do_finish : compilation_context -> unit

	method maybe_cache_context com =
		if com.display.dms_full_typing then begin
			CommonCache.cache_context sctx.cs com;
			ServerMessage.cached_modules com "" (List.length com.modules);
		end

	method finish (ctx : compilation_context) =
		ctx.com.client_stdout#close self#out;
		ctx.com.client_stderr#close self#err;
		self#do_finish ctx
end

class stdio_communication
	(sctx : ServerCompilationContext.t)
= object(self)
	inherit server_communication sctx

	method out (s : string) =
		print_string s;
		flush stdout;

	method err (s : string) =
		prerr_string s

	method do_finish (ctx : compilation_context) =
		List.iter (fun msg -> match msg with
			| CMInfo _ -> print_endline (compiler_message_string msg)
			| CMWarning _ | CMError _ -> prerr_endline (compiler_message_string msg)
		) (List.rev ctx.messages);
		if ctx.has_error && !Helper.prompt then begin
			print_endline "Press enter to exit...";
			ignore(read_line());
		end;
		flush stdout;
		if ctx.has_error then exit 1
end

class other_communication
	(sctx : ServerCompilationContext.t)
	(write : string -> unit)
= object(self)
	inherit server_communication sctx

	method out (s : string) =
		write ("\x01" ^ String.concat "\x01" (ExtString.String.nsplit s "\n") ^ "\n")

	method err (s : string) =
		write s

	method do_finish (ctx : compilation_context) =
		sctx.compilation_step <- sctx.compilation_step + 1;
		sctx.compilation_mark <- sctx.mark_loop;
		check_display_flush ctx (fun () ->
			List.iter
				(fun msg ->
					let s = compiler_message_string msg in
					write (s ^ "\n");
					ServerMessage.message s;
				)
				(List.rev ctx.messages);
			sctx.was_compilation <- ctx.com.display.dms_full_typing;
			if ctx.has_error then begin
				measure_times := false;
				write "\x02\n"
			end else
				self#maybe_cache_context ctx.com;
		)
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
								cs#add_directory sign (CompilationServer.create_directory dir time)
							end;
						) sub_dirs;
						(CompilationServer.create_directory dir.c_path time') :: acc
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
						dirs := CompilationServer.create_directory path time :: !dirs
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
	let cc = CommonCache.get_cache sctx.cs com in
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
					raise Not_found
				end
			end
		) paths
	in
	let mark = sctx.mark_loop in
	let start_mark = sctx.compilation_mark in
	let rec check m =
		let check_module_path () =
			let directories = get_changed_directories sctx ctx in
			match m.m_extra.m_kind with
			| MFake | MImport -> () (* don't get classpath *)
			| MExtern ->
				(* if we have a file then this will override our extern type *)
				let has_file = (try check_module_shadowing directories m; false with Not_found -> true) in
				if has_file then begin
					if sctx.verbose then print_endline ("A file is masking the library file " ^ s_type_path m.m_path); (* TODO *)
					raise Not_found;
				end;
				let rec loop = function
					| [] ->
						if sctx.verbose then print_endline ("No library file was found for " ^ s_type_path m.m_path); (* TODO *)
						raise Not_found (* no extern registration *)
					| (file,load) :: l ->
						match load m.m_path p with
						| None -> loop l
						| Some _ ->
							if com.file_keys#get file <> (Path.UniqueKey.lazy_key m.m_extra.m_file) then begin
								if sctx.verbose then print_endline ("Library file was changed for " ^ s_type_path m.m_path); (* TODO *)
								raise Not_found;
							end
				in
				loop com.load_extern_type
			| MCode -> check_module_shadowing directories m
			| MMacro when ctx.Typecore.in_macro -> check_module_shadowing directories m
			| MMacro ->
				(*
					Creating another context while the previous one is incomplete means we have an infinite loop in the compiler.
					Most likely because of circular dependencies in base modules (e.g. `StdTypes` or `String`)
					Prevents spending another 5 hours for debugging.
					@see https://github.com/HaxeFoundation/haxe/issues/8174
				*)
				if not ctx.g.complete && ctx.in_macro then
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
					raise Not_found;
				end
			end
		in
		let check_dependencies () =
			PMap.iter (fun _ m2 -> match check m2 with
				| None -> ()
				| Some path -> raise (Dirty path)
			) m.m_extra.m_deps;
		in
		begin match m.m_extra.m_dirty with
		| Some path ->
			Some path
		| None ->
			if m.m_extra.m_mark = mark then
				None
			else try
				let old_mark = m.m_extra.m_mark in
				m.m_extra.m_mark <- mark;
				if old_mark <= start_mark then begin
					if not (has_policy NoCheckShadowing) then check_module_path();
					if not (has_policy NoCheckFileTimeModification) || file_extension (Path.UniqueKey.lazy_path m.m_extra.m_file) <> "hx" then check_file();
				end;
				if not (has_policy NoCheckDependencies) then check_dependencies();
				None
			with
			| Not_found ->
				m.m_extra.m_dirty <- Some m.m_path;
				Some m.m_path
			| Dirty path ->
				m.m_extra.m_dirty <- Some path;
				Some path
			end
	in
	check m

(* Adds module [m] and all its dependencies (recursively) from the cache to the current compilation
   context. *)
let add_modules sctx ctx m p =
	let com = ctx.Typecore.com in
	let rec add_modules tabs m0 m =
		if m.m_extra.m_added < sctx.compilation_step then begin
			(match m0.m_extra.m_kind, m.m_extra.m_kind with
			| MCode, MMacro | MMacro, MCode ->
				(* this was just a dependency to check : do not add to the context *)
				PMap.iter (Hashtbl.replace com.resources) m.m_extra.m_binded_res;
			| _ ->
				ServerMessage.reusing com tabs m;
				m.m_extra.m_added <- sctx.compilation_step;
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
				TypeloadModule.add_module ctx m p;
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
	let cc = CommonCache.get_cache sctx.cs com in
	sctx.mark_loop <- sctx.mark_loop + 1;
	try
		let m = cc#find_module mpath in
		let tcheck = Timer.timer ["server";"module cache";"check"] in
		begin match check_module sctx ctx m p with
		| None -> ()
		| Some path ->
			ServerMessage.skipping_dep com "" (m,path);
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

(* Sets up the per-compilation context. *)
let create sctx (comm : server_communication) params =
	let cs = sctx.cs in
	let rec ctx = {
		com = Common.create version params;
		finish = (fun()-> comm#finish ctx);
		setup = (fun() ->
			let sign = Define.get_signature ctx.com.defines in
			ServerMessage.defines ctx.com "";
			ServerMessage.signature ctx.com "" sign;
			ServerMessage.display_position ctx.com "" (DisplayPosition.display_position#get);
			try
				if (Hashtbl.find sctx.class_paths sign) <> ctx.com.class_path then begin
					ServerMessage.class_paths_changed ctx.com "";
					Hashtbl.replace sctx.class_paths sign ctx.com.class_path;
					cs#clear_directories sign;
					(cs#get_context sign)#set_initialized false;
				end;
			with Not_found ->
				Hashtbl.add sctx.class_paths sign ctx.com.class_path;
				()
		);
		messages = [];
		has_next = false;
		has_error = false;
		server_mode = SMNone;
	} in
	ctx

(* Resets the state for a new compilation *)
let init_new_compilation sctx =
	ServerCompilationContext.reset sctx;
	Parser.reset_state();
	return_partial_type := false;
	measure_times := false;
	Hashtbl.clear DeprecationCheck.warned_positions;
	close_times();
	stats.s_files_parsed := 0;
	stats.s_classes_built := 0;
	stats.s_methods_typed := 0;
	stats.s_macros_called := 0;
	Hashtbl.clear Timer.htimers;
	sctx.compilation_step <- sctx.compilation_step + 1;
	sctx.compilation_mark <- sctx.mark_loop;
	Helper.start_time := get_time()

let cleanup () =
	begin match !MacroContext.macro_interp_cache with
	| Some interp -> EvalContext.GlobalState.cleanup interp
	| None -> ()
	end

let gc_heap_stats () =
	let stats = Gc.quick_stat() in
	stats.major_words,stats.heap_words

(* Returns a list of contexts, but doesn't do anything yet *)
let process_params create pl =
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
			Server_old.do_connect host (try int_of_string port with _ -> raise (Arg.Bad "Invalid port")) ((List.rev acc) @ l)
		| "--run" :: cl :: args ->
			let acc = cl :: "-x" :: acc in
			let ctx = add_context (!each_params @ (List.rev acc)) in
			ctx.com.sys_args <- args;
		| arg :: l ->
			match List.rev (ExtString.String.nsplit arg ".") with
			| "hxml" :: _ when (match acc with "-cmd" :: _ | "--cmd" :: _ -> false | _ -> true) ->
				let acc, l = (try acc, parse_hxml arg @ l with Not_found -> (arg ^ " (file not found)") :: acc, l) in
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

let parse_host_port hp =
	let host, port = (try ExtString.String.split hp ":" with _ -> "127.0.0.1", hp) in
	let port = try int_of_string port with _ -> raise (Arg.Bad "Invalid port") in
	host, port

let setup_common_context ctx comm =
	let com = ctx.com in
	let out = new server_pipe (Unix.pipe()) in
	ctx.com.client_stdout <- out;
	ctx.com.client_stderr <- new server_pipe (Unix.pipe());
	ctx.com.print <- (fun s ->
		out#write s;
		out#read comm#out;
	);
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

let rec process sctx comm args =
	let t0 = get_time() in
	ServerMessage.arguments args;
	init_new_compilation sctx;
	let create = create sctx comm in
	let ctxs = try
		process_params create args
	with Arg.Bad msg ->
		let ctx = create args in
		error ctx ("Error: " ^ msg) null_pos;
		[ctx]
	in
	let run ctx =
		(* Close any leftover descriptors from previous compilation *)
		ctx.com.client_stdout#close (fun _ -> ());
		ctx.com.client_stderr#close (fun _ -> ());
		setup_common_context ctx comm;
		Compiler.compile_safe ctx (fun () ->
			let actx = Args.parse_args ctx in
			begin match ctx.server_mode with
			| SMListen hp ->
				let accept = match hp with
				| "stdio" ->
					Server_old.init_wait_stdio()
				| _ ->
					let host, port = parse_host_port hp in
					init_wait_socket host port
				in
				wait_loop ctx.com.verbose accept
			| SMConnect hp ->
				let host, port = parse_host_port hp in
				let accept = init_wait_connect host port in
				wait_loop ctx.com.verbose accept
			| SMNone ->
				()
			end;
			Compiler.compile ctx actx;
		);
		ctx.finish();
	in
	let run ctx = try
		if ctx.has_error then begin
			ctx.finish();
			false (* can happen if process_params above fails already *)
		end else begin
			run ctx;
			true (* reads as "continue?" *)
		end
	with
		| Completion str ->
			ServerMessage.completion str;
			comm#err str;
			false
		| Arg.Bad msg ->
			error ctx ("Error: " ^ msg) null_pos;
			false
	in
	let success = List.fold_left (fun b ctx -> b && run ctx) true ctxs in
	if success then begin
		close_times();
		if !measure_times then report_times (fun s -> comm#err (s ^ "\n"));
	end;
	run_delays sctx;
	ServerMessage.stats stats (get_time() -. t0)

(* The server main loop. Waits for the [accept] call to then process the sent compilation
   parameters through [process_params]. *)
and wait_loop verbose accept =
	if verbose then ServerMessage.enable_all ();
	Sys.catch_break false; (* Sys can never catch a break *)
	(* Create server context and set up hooks for parsing and typing *)
	let cs = CompilationServer.create () in
	let sctx = ServerCompilationContext.create verbose cs in
	TypeloadModule.type_module_hook := type_module sctx;
	MacroContext.macro_enable_cache := true;
	TypeloadParse.parse_hook := parse_file cs;
	let ring = Ring.create 10 0. in
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
					let data = parse_hxml_data hxml in
					process sctx (new other_communication sctx write) data
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
	done

(* Connect to given host/port and return accept function for communication *)
and init_wait_connect host port =
	let host = Unix.inet_addr_of_string host in
	let chin, chout = Unix.open_connection (Unix.ADDR_INET (host,port)) in
	Server_old.mk_length_prefixed_communication true chin chout

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
		let write s = Server_old.ssend sin (Bytes.unsafe_of_string s) in
		let close() = Unix.close sin in
		false, read, write, close
	) in
	accept