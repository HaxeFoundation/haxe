open Printf
open Globals
open Ast
open Common
open CompilationServer
open DisplayTypes.DisplayMode
open Timer
open Type
open DisplayOutput
open Json

exception Dirty of module_def
exception ServerError of string

let measure_times = ref false
let prompt = ref false
let start_time = ref (Timer.get_time())

let is_debug_run() =
	try Sys.getenv "HAXEDEBUG" = "1" with _ -> false

type context = {
	com : Common.context;
	mutable flush : unit -> unit;
	mutable setup : unit -> unit;
	mutable messages : compiler_message list;
	mutable has_next : bool;
	mutable has_error : bool;
}

let s_version =
	let pre = Option.map_default (fun pre -> "-" ^ pre) "" version_pre in
	let build = Option.map_default (fun (_,build) -> "+" ^ build) "" Version.version_extra in
	Printf.sprintf "%d.%d.%d%s%s" version_major version_minor version_revision pre build

let check_display_flush ctx f_otherwise = match ctx.com.json_out with
	| None ->
		begin match ctx.com.display.dms_kind with
		| DMDiagnostics global->
			List.iter (fun msg ->
				let msg,p,kind = match msg with
					| CMInfo(msg,p) -> msg,p,DisplayTypes.DiagnosticsSeverity.Information
					| CMWarning(msg,p) -> msg,p,DisplayTypes.DiagnosticsSeverity.Warning
					| CMError(msg,p) -> msg,p,DisplayTypes.DiagnosticsSeverity.Error
				in
				add_diagnostics_message ctx.com msg p DisplayTypes.DiagnosticsKind.DKCompilerError kind
			) (List.rev ctx.messages);
			raise (Completion (Diagnostics.print ctx.com global))
		| _ ->
			f_otherwise ()
		end
	| Some(_,f,_) ->
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
			f errors
		end

let default_flush ctx =
	check_display_flush ctx (fun () ->
		List.iter
			(fun msg -> match msg with
				| CMInfo _ -> print_endline (compiler_message_string msg)
				| CMWarning _ | CMError _ -> prerr_endline (compiler_message_string msg)
			)
			(List.rev ctx.messages);
		if ctx.has_error && !prompt then begin
			print_endline "Press enter to exit...";
			ignore(read_line());
		end;
		if ctx.has_error then exit 1
	)

let create_context params =
	let ctx = {
		com = Common.create version s_version params;
		flush = (fun()->());
		setup = (fun()->());
		messages = [];
		has_next = false;
		has_error = false;
	} in
	ctx.flush <- (fun() -> default_flush ctx);
	ctx

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

let ssend sock str =
	let rec loop pos len =
		if len = 0 then
			()
		else
			let s = Unix.send sock str pos len [] in
			loop (pos + s) (len - s)
	in
	loop 0 (Bytes.length str)

let rec wait_loop process_params verbose accept =
	if verbose then ServerMessage.enable_all ();
	Sys.catch_break false; (* Sys can never catch a break *)
	let cs = CompilationServer.create () in
	MacroContext.macro_enable_cache := true;
	let current_stdin = ref None in
	TypeloadParse.parse_hook := (fun com2 file p ->
		let ffile = Path.unique_full_path file in
		let is_display_file = ffile = (DisplayPosition.display_position#get).pfile in

		match is_display_file, !current_stdin with
		| true, Some stdin when Common.defined com2 Define.DisplayStdin ->
			TypeloadParse.parse_file_from_string com2 file p stdin
		| _ ->
			let sign = Define.get_signature com2.defines in
			let ftime = file_time ffile in
			let fkey = (ffile,sign) in
			let data = Std.finally (Timer.timer ["server";"parser cache"]) (fun () ->
				try
					let cfile = CompilationServer.find_file cs fkey in
					if cfile.c_time <> ftime then raise Not_found;
					Parser.ParseSuccess(cfile.c_package,cfile.c_decls)
				with Not_found ->
					let parse_result = TypeloadParse.parse_file com2 file p in
					let info,is_unusual = match parse_result with
						| ParseError(_,_,_) -> "not cached, has parse error",true
						| ParseDisplayFile _ -> "not cached, is display file",true
						| ParseSuccess data ->
							begin try
								(* We assume that when not in display mode it's okay to cache stuff that has #if display
								checks. The reasoning is that non-display mode has more information than display mode. *)
								if not com2.display.dms_display then raise Not_found;
								let ident = Hashtbl.find Parser.special_identifier_files ffile in
								Printf.sprintf "not cached, using \"%s\" define" ident,true
							with Not_found ->
								CompilationServer.cache_file cs fkey ftime data;
								"cached",false
							end
					in
					if is_unusual then ServerMessage.parsed com2 "" (ffile,info);
					parse_result
			) () in
			data
	);
	let check_module_shadowing com paths m =
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
	let delays = ref [] in
	let changed_directories = Hashtbl.create 0 in
	let arguments = Hashtbl.create 0 in
	let stat dir =
		(Unix.stat (Path.remove_trailing_slash dir)).Unix.st_mtime
	in
	let get_changed_directories (ctx : Typecore.typer) =
		let t = Timer.timer ["server";"module cache";"changed dirs"] in
		let com = ctx.Typecore.com in
		let sign = Define.get_signature com.defines in
		let dirs = try
			(* First, check if we already have determined changed directories for current compilation. *)
			Hashtbl.find changed_directories sign
		with Not_found ->
			let dirs = try
				(* Next, get all directories from the cache and filter the ones that haven't changed. *)
				let all_dirs = CompilationServer.find_directories cs sign in
				let dirs = List.fold_left (fun acc dir ->
					try
						let time' = stat dir.c_path in
						if dir.c_mtime < time' then begin
							dir.c_mtime <- time';
							let sub_dirs = Path.find_directories (platform_name com.platform) false [dir.c_path] in
							List.iter (fun dir ->
								if not (CompilationServer.has_directory cs sign dir) then begin
									let time = stat dir in
									ServerMessage.added_directory com "" dir;
									CompilationServer.add_directory cs sign (CompilationServer.create_directory dir time)
								end;
							) sub_dirs;
							(CompilationServer.create_directory dir.c_path time') :: acc
						end else
							acc
					with Unix.Unix_error _ ->
						CompilationServer.remove_directory cs sign dir.c_path;
						ServerMessage.removed_directory com "" dir.c_path;
						acc
				) [] all_dirs in
				ServerMessage.changed_directories com "" dirs;
				dirs
			with Not_found ->
				(* There were no directories in the cache, so this must be a new context. Let's add
				   an empty list to make sure no crazy recursion happens. *)
				CompilationServer.add_directories cs sign [];
				(* Register the delay that is going to populate the cache dirs. *)
				delays := (fun () ->
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
					CompilationServer.add_directories cs sign !dirs
				) :: !delays;
				(* Returning [] should be fine here because it's a new context, so we won't do any
				   shadowing checks anyway. *)
				[]
			in
			Hashtbl.add changed_directories sign dirs;
			dirs
		in
		t();
		dirs
	in
	let compilation_step = ref 0 in
	let compilation_mark = ref 0 in
	let mark_loop = ref 0 in
	TypeloadModule.type_module_hook := (fun (ctx:Typecore.typer) mpath p ->
		let t = Timer.timer ["server";"module cache"] in
		let com2 = ctx.Typecore.com in
		let sign = Define.get_signature com2.defines in
		let content_changed m file =
			let ffile = Path.unique_full_path file in
			let fkey = (ffile,sign) in
			try
				let cfile = CompilationServer.find_file cs fkey in
				(* We must use the module path here because the file path is absolute and would cause
				   positions in the parsed declarations to differ. *)
				let new_data = TypeloadParse.parse_module ctx m.m_path p in
				cfile.c_decls <> snd new_data
			with Not_found ->
				true
		in
		incr mark_loop;
		let mark = !mark_loop in
		let start_mark = !compilation_mark in
		let rec check m =
			let check_module_path () =
				let directories = get_changed_directories ctx in
				match m.m_extra.m_kind with
				| MFake | MImport -> () (* don't get classpath *)
				| MExtern ->
					(* if we have a file then this will override our extern type *)
					let has_file = (try check_module_shadowing com2 directories m; false with Not_found -> true) in
					if has_file then begin
						if verbose then print_endline ("A file is masking the library file " ^ s_type_path m.m_path); (* TODO *)
						raise Not_found;
					end;
					let rec loop = function
						| [] ->
							if verbose then print_endline ("No library file was found for " ^ s_type_path m.m_path); (* TODO *)
							raise Not_found (* no extern registration *)
						| load :: l ->
							match load m.m_path p with
							| None -> loop l
							| Some (file,_) ->
								if Path.unique_full_path file <> m.m_extra.m_file then begin
									if verbose then print_endline ("Library file was changed for " ^ s_type_path m.m_path); (* TODO *)
									raise Not_found;
								end
					in
					loop com2.load_extern_type
				| MCode -> check_module_shadowing com2 directories m
				| MMacro when ctx.Typecore.in_macro -> check_module_shadowing com2 directories m
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
					check_module_shadowing mctx.Typecore.com (get_changed_directories mctx) m
			in
			let has_policy policy = List.mem policy m.m_extra.m_check_policy || match policy with
				| NoCheckShadowing | NoCheckFileTimeModification when !ServerConfig.do_not_check_modules && !Parser.display_mode <> DMNone -> true
				| _ -> false
			in
			let check_file () =
				if file_time m.m_extra.m_file <> m.m_extra.m_time then begin
					if has_policy CheckFileContentModification && not (content_changed m m.m_extra.m_file) then begin
						ServerMessage.unchanged_content com2 "" m.m_extra.m_file;
					end else begin
						ServerMessage.not_cached com2 "" m;
						if m.m_extra.m_kind = MFake then Hashtbl.remove Typecore.fake_modules m.m_extra.m_file;
						raise Not_found;
					end
				end
			in
			let check_dependencies () =
				PMap.iter (fun _ m2 -> match check m2 with
					| None -> ()
					| Some m -> raise (Dirty m)
				) m.m_extra.m_deps;
			in
			begin match m.m_extra.m_dirty with
			| Some m ->
				Some m
			| None ->
				if m.m_extra.m_mark = mark then
					None
				else try
					let old_mark = m.m_extra.m_mark in
					m.m_extra.m_mark <- mark;
					if old_mark <= start_mark then begin
						(* Workaround for preview.4 Java issue *)
						begin match m.m_extra.m_kind with
							| MExtern -> check_module_path()
							| _ -> if not (has_policy NoCheckShadowing) then check_module_path();
						end;
						if not (has_policy NoCheckFileTimeModification) then check_file();
					end;
					if not (has_policy NoCheckDependencies) then check_dependencies();
					None
				with
				| Not_found ->
					m.m_extra.m_dirty <- Some m;
					Some m
				| Dirty m' ->
					m.m_extra.m_dirty <- Some m';
					Some m'
				end
		in
		let rec add_modules tabs m0 m =
			if m.m_extra.m_added < !compilation_step then begin
				(match m0.m_extra.m_kind, m.m_extra.m_kind with
				| MCode, MMacro | MMacro, MCode ->
					(* this was just a dependency to check : do not add to the context *)
					PMap.iter (Hashtbl.replace com2.resources) m.m_extra.m_binded_res;
				| _ ->
					ServerMessage.reusing com2 tabs m;
					m.m_extra.m_added <- !compilation_step;
					List.iter (fun t ->
						match t with
						| TClassDecl c -> c.cl_restore()
						| TEnumDecl e ->
							let rec loop acc = function
								| [] -> ()
								| (Meta.RealPath,[Ast.EConst (Ast.String path),_],_) :: l ->
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
					PMap.iter (Hashtbl.replace com2.resources) m.m_extra.m_binded_res;
					PMap.iter (fun _ m2 -> add_modules (tabs ^ "  ") m0 m2) m.m_extra.m_deps
				)
			end
		in
		try
			let m = CompilationServer.find_module cs (mpath,sign) in
			let tcheck = Timer.timer ["server";"module cache";"check"] in
			begin match check m with
			| None -> ()
			| Some m' ->
				ServerMessage.skipping_dep com2 "" (m,m');
				tcheck();
				raise Not_found;
			end;
			tcheck();
			let tadd = Timer.timer ["server";"module cache";"add modules"] in
			add_modules "" m m;
			tadd();
			t();
			Some m
		with Not_found ->
			t();
			None
	);
	let run_count = ref 0 in
	while true do
		let read, write, close = accept() in
		let was_compilation = ref false in
		let maybe_cache_context com =
			if com.display.dms_full_typing then begin
				CompilationServer.cache_context cs com;
				ServerMessage.cached_modules com "" (List.length com.modules);
			end;
		in
		let create params =
			let ctx = create_context params in
			ctx.flush <- (fun() ->
				incr compilation_step;
				compilation_mark := !mark_loop;
				check_display_flush ctx (fun () ->
					List.iter
						(fun msg ->
							let s = compiler_message_string msg in
							write (s ^ "\n");
							ServerMessage.message s;
						)
						(List.rev ctx.messages);
					was_compilation := ctx.com.display.dms_full_typing;
					if ctx.has_error then begin
						measure_times := false;
						write "\x02\n"
					end else maybe_cache_context ctx.com;
				)
			);
			ctx.setup <- (fun() ->
				let sign = Define.get_signature ctx.com.defines in
				ServerMessage.defines ctx.com "";
				ServerMessage.signature ctx.com "" sign;
				ServerMessage.display_position ctx.com "" (DisplayPosition.display_position#get);
				(* Special case for diagnostics: It's not treated as a display mode, but we still want to invalidate the
				   current file in order to run diagnostics on it again. *)
				if ctx.com.display.dms_display || (match ctx.com.display.dms_kind with DMDiagnostics _ -> true | _ -> false) then begin
					let file = (DisplayPosition.display_position#get).pfile in
					let fkey = (file,sign) in
					(* force parsing again : if the completion point have been changed *)
					CompilationServer.remove_file cs fkey;
					CompilationServer.taint_modules cs file;
				end;
				try
					if (Hashtbl.find arguments sign) <> ctx.com.class_path then begin
						ServerMessage.class_paths_changed ctx.com "";
						Hashtbl.replace arguments sign ctx.com.class_path;
						CompilationServer.clear_directories cs sign;
					end;
				with Not_found ->
					Hashtbl.add arguments sign ctx.com.class_path;
					()
			);
			ctx.com.print <- (fun str -> write ("\x01" ^ String.concat "\x01" (ExtString.String.nsplit str "\n") ^ "\n"));
			ctx
		in
		(try
			let s = read() in
			let t0 = get_time() in
			let hxml =
				try
					let idx = String.index s '\001' in
					current_stdin := Some (String.sub s (idx + 1) ((String.length s) - idx - 1));
					(String.sub s 0 idx)
				with Not_found ->
					s
			in
			let data = parse_hxml_data hxml in
			ServerMessage.arguments data;
			(try
				Hashtbl.clear changed_directories;
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
				let _ = Timer.timer ["other"] in
				incr compilation_step;
				compilation_mark := !mark_loop;
				start_time := get_time();
				process_params create data;
				close_times();
				if !measure_times then report_times (fun s -> write (s ^ "\n"))
			with
			| Completion str ->
				ServerMessage.completion str;
				write str
			| Arg.Bad msg ->
				print_endline ("Error: " ^ msg);
			);
			let fl = !delays in
			delays := [];
			List.iter (fun f -> f()) fl;
			ServerMessage.stats stats (get_time() -. t0);
		with Unix.Unix_error _ ->
			ServerMessage.socket_message "Connection Aborted"
		| e ->
			let estr = Printexc.to_string e in
			ServerMessage.uncaught_error estr;
			(try write ("\x02\n" ^ estr); with _ -> ());
			if is_debug_run() then print_endline (estr ^ "\n" ^ Printexc.get_backtrace());
			if e = Out_of_memory then begin
				close();
				exit (-1);
			end;
		);
		close();
		current_stdin := None;
		(* prevent too much fragmentation by doing some compactions every X run *)
		if !was_compilation then incr run_count;
		if !run_count mod 10 = 0 then begin
			run_count := 1;
			let t0 = get_time() in
			Gc.compact();
			ServerMessage.gc_stats (get_time() -. t0);
		end else Gc.minor();
	done

and init_wait_stdio() =
	set_binary_mode_in stdin true;
	set_binary_mode_out stderr true;

	let chin = IO.input_channel stdin in
	let cherr = IO.output_channel stderr in

	let berr = Buffer.create 0 in
	let read = fun () ->
		let len = IO.read_i32 chin in
		IO.really_nread_string chin len
	in
	let write = Buffer.add_string berr in
	let close = fun() ->
		IO.write_i32 cherr (Buffer.length berr);
		IO.nwrite_string cherr (Buffer.contents berr);
		IO.flush cherr
	in
	fun() ->
		Buffer.clear berr;
		read, write, close

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
		let read = fun() -> (let s = read_loop 0 in Unix.clear_nonblock sin; s) in
		let write s = ssend sin (Bytes.unsafe_of_string s) in
		let close() = Unix.close sin in
		read, write, close
	) in
	accept

and do_connect host port args =
	let sock = Unix.socket Unix.PF_INET Unix.SOCK_STREAM 0 in
	(try Unix.connect sock (Unix.ADDR_INET (Unix.inet_addr_of_string host,port)) with _ -> failwith ("Couldn't connect on " ^ host ^ ":" ^ string_of_int port));
	let args = ("--cwd " ^ Unix.getcwd()) :: args in
	ssend sock (Bytes.of_string (String.concat "" (List.map (fun a -> a ^ "\n") args) ^ "\000"));
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
