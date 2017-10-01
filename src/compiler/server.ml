open Printf
open Globals
open Ast
open Common
open Common.DisplayMode
open Type
open DisplayOutput
open Json

exception Dirty of module_def

let measure_times = ref false
let prompt = ref false
let start_time = ref (get_time())

let is_debug_run() =
	try Sys.getenv "HAXEDEBUG" = "1" with _ -> false

type context = {
	com : Common.context;
	mutable flush : unit -> unit;
	mutable setup : unit -> unit;
	mutable messages : string list;
	mutable has_next : bool;
	mutable has_error : bool;
}

type server_message =
	| AddedDirectory of string
	| FoundDirectories of (string * float ref) list
	| ChangedDirectories of (string * float) list
	| ModulePathChanged of (module_def * float * string)
	| NotCached of module_def
	| Parsed of (string * string)
	| RemovedDirectory of string
	| Reusing of module_def
	| SkippingDep of (module_def * module_def)

let s_version =
	Printf.sprintf "%d.%d.%d%s" version_major version_minor version_revision (match Version.version_extra with None -> "" | Some v -> " " ^ v)

type timer_node = {
	name : string;
	path : string;
	parent : timer_node;
	info : string;
	mutable time : float;
	mutable num_calls : int;
	mutable children : timer_node list;
}

let report_times print =
	let nodes = Hashtbl.create 0 in
	let rec root = {
		name = "";
		path = "";
		parent = root;
		info = "";
		time = 0.;
		num_calls = 0;
		children = [];
	} in
	Hashtbl.iter (fun _ timer ->
		let rec loop parent sl = match sl with
			| [] -> assert false
			| s :: sl ->
				let path = (match parent.path with "" -> "" | _ -> parent.path ^ ".") ^ s in
				let node = try
					let node = Hashtbl.find nodes path in
					node.num_calls <- node.num_calls + timer.calls;
					node.time <- node.time +. timer.total;
					node
				with Not_found ->
					let name,info = try
						let i = String.rindex s '.' in
						String.sub s (i + 1) (String.length s - i - 1),String.sub s 0 i
					with Not_found ->
						s,""
					in
					let node = {
						name = name;
						path = path;
						parent = parent;
						info = info;
						time = timer.total;
						num_calls = timer.calls;
						children = [];
					} in
					Hashtbl.add nodes path node;
					node
				in
				begin match sl with
					| [] -> ()
					| _ ->
						let child = loop node sl in
						if not (List.memq child node.children) then
							node.children <- child :: node.children;
				end;
				node
		in
		let node = loop root timer.id in
		if not (List.memq node root.children) then
			root.children <- node :: root.children
	) Common.htimers;
	let max_name = ref 0 in
	let max_calls = ref 0 in
	let rec loop depth node =
		let l = (String.length node.name) + 2 * depth in
		List.iter (fun child ->
			if depth = 0 then begin
				node.num_calls <- node.num_calls + child.num_calls;
				node.time <- node.time +. child.time;
			end;
			loop (depth + 1) child;
		) node.children;
		node.children <- List.sort (fun node1 node2 -> compare node2.time node1.time) node.children;
		if node.num_calls > !max_calls then max_calls := node.num_calls;
		if node.time > 0.0009 && l > !max_name then max_name := l;
	in
	loop 0 root;
	let max_calls = String.length (string_of_int !max_calls) in
	print (Printf.sprintf "%-*s | %7s |   %% |  p%% | %*s | info" !max_name "name" "time(s)" max_calls "#");
	let sep = String.make (!max_name + max_calls + 27) '-' in
	print sep;
	let print_time name node =
		if node.time > 0.0009 then
			print (Printf.sprintf "%-*s | %7.3f | %3.0f | %3.0f | %*i | %s" !max_name name node.time (node.time *. 100. /. root.time) (node.time *. 100. /. node.parent.time) max_calls node.num_calls node.info)
	in
	let rec loop depth node =
		let name = (String.make (depth * 2) ' ') ^ node.name in
		print_time name node;
		List.iter (loop (depth + 1)) node.children
	in
	List.iter (loop 0) root.children;
	print sep;
	print_time "total" root

let default_flush ctx =
	List.iter prerr_endline (List.rev ctx.messages);
	if ctx.has_error && !prompt then begin
		print_endline "Press enter to exit...";
		ignore(read_line());
	end;
	if ctx.has_error then exit 1

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
	Sys.catch_break false;
	let has_parse_error = ref false in
	let test_server_messages = DynArray.create () in
	let cs = CompilationServer.create () in
	let sign_string com =
		let sign = Define.get_signature com.defines in
		let	sign_id =
			try
				CompilationServer.get_sign cs sign;
			with Not_found ->
				let i = CompilationServer.add_sign cs sign in
				print_endline (Printf.sprintf "Found context %s:\n%s" i (dump_context com));
				i
		in
		Printf.sprintf "%2s,%3s: " sign_id (short_platform_name com.platform)
	in
	let process_server_message com tabs =
		if Common.raw_defined com "compilation-server-test" then (fun message ->
			let module_path m = JString (s_type_path m.m_path) in
			let kind,data = match message with
				| AddedDirectory dir -> "addedDirectory",JString dir
				| FoundDirectories dirs -> "foundDirectories",JInt (List.length dirs)
				| ChangedDirectories dirs -> "changedDirectories",JArray (List.map (fun (s,_) -> JString s) dirs)
				| ModulePathChanged(m,time,file) -> "modulePathChanged",module_path m
				| NotCached m -> "notCached",module_path m
				| Parsed(ffile,_) -> "parsed",JString ffile
				| RemovedDirectory dir -> "removedDirectory",JString dir
				| Reusing m -> "reusing",module_path m
				| SkippingDep(m,m') -> "skipping",JObject ["skipped",module_path m;"dependency",module_path m']
			in
			let js = JObject [("kind",JString kind);("data",data)] in
			DynArray.add test_server_messages js;
		) else (fun message -> match message with
			| AddedDirectory dir -> print_endline (Printf.sprintf "%sadded directory %s" (sign_string com) dir)
			| FoundDirectories dirs -> print_endline (Printf.sprintf "%sfound %i directories" (sign_string com) (List.length dirs));
			| ChangedDirectories dirs ->
				print_endline (Printf.sprintf "%schanged directories: [%s]" (sign_string com) (String.concat ", " (List.map (fun (s,_) -> "\"" ^ s ^ "\"") dirs)))
			| ModulePathChanged(m,time,file) ->
				print_endline (Printf.sprintf "%smodule path might have changed: %s\n\twas: %2.0f %s\n\tnow: %2.0f %s"
					(sign_string com) (s_type_path m.m_path) m.m_extra.m_time m.m_extra.m_file time file);
			| NotCached m -> print_endline (Printf.sprintf "%s%s not cached (%s)" (sign_string com) (s_type_path m.m_path) (if m.m_extra.m_time = -1. then "macro-in-macro" else "modified"));
			| Parsed(ffile,info) -> print_endline (Printf.sprintf "%sparsed %s (%s)" (sign_string com) ffile info)
			| RemovedDirectory dir -> print_endline (Printf.sprintf "%sremoved directory %s" (sign_string com) dir);
			| Reusing m -> print_endline (Printf.sprintf "%s%sreusing %s" (sign_string com) tabs (s_type_path m.m_path));
			| SkippingDep(m,m') -> print_endline (Printf.sprintf "%sskipping %s%s" (sign_string com) (s_type_path m.m_path) (if m == m' then "" else Printf.sprintf "(%s)" (s_type_path m'.m_path)));
		)
	in
	MacroContext.macro_enable_cache := true;
	let current_stdin = ref None in
	Typeload.parse_hook := (fun com2 file p ->
		let ffile = Path.unique_full_path file in
		let is_display_file = ffile = (!Parser.resume_display).pfile in

		match is_display_file, !current_stdin with
		| true, Some stdin when Common.defined com2 Define.DisplayStdin ->
			Typeload.parse_file_from_string com2 file p stdin
		| _ ->
			let sign = Define.get_signature com2.defines in
			let ftime = file_time ffile in
			let fkey = (ffile,sign) in
			try
				let time, data = CompilationServer.find_file cs fkey in
				if time <> ftime then raise Not_found;
				data
			with Not_found ->
				has_parse_error := false;
				let data = Typeload.parse_file com2 file p in
				let info,is_unusual = if !has_parse_error then "not cached, has parse error",true
					else if is_display_file then "not cached, is display file",true
					else begin try
						(* We assume that when not in display mode it's okay to cache stuff that has #if display
						   checks. The reasoning is that non-display mode has more information than display mode. *)
						if not com2.display.dms_display then raise Not_found;
						let ident = Hashtbl.find Parser.special_identifier_files ffile in
						Printf.sprintf "not cached, using \"%s\" define" ident,true
					with Not_found ->
						CompilationServer.cache_file cs fkey (ftime,data);
						"cached",false
				end in
				if verbose && is_unusual then process_server_message com2 "" (Parsed(ffile,info));
				data
	);
	let check_module_shadowing com paths m =
		List.iter (fun (path,_) ->
			let file = (path ^ (snd m.m_path)) ^ ".hx" in
			if Sys.file_exists file then begin
				let time = file_time file in
				if time > m.m_extra.m_time then begin
					if verbose then process_server_message com "" (ModulePathChanged(m,time,file));
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
		let t = Common.timer ["server";"module cache";"changed dirs"] in
		let com = ctx.Typecore.com in
		let sign = Define.get_signature com.defines in
		let dirs = try
			(* First, check if we already have determined changed directories for current compilation. *)
			Hashtbl.find changed_directories sign
		with Not_found ->
			let dirs = try
				(* Next, get all directories from the cache and filter the ones that haven't changed. *)
				let all_dirs = CompilationServer.find_directories cs sign in
				let dirs = List.fold_left (fun acc (dir,time) ->
					try
						let time' = stat dir in
						if !time < time' then begin
							time := time';
							let sub_dirs = Path.find_directories (platform_name com.platform) false [dir] in
							List.iter (fun dir ->
								if not (CompilationServer.has_directory cs sign dir) then begin
									let time = stat dir in
									if verbose then process_server_message com "" (AddedDirectory dir);
									CompilationServer.add_directory cs sign (dir,ref time)
								end;
							) sub_dirs;
							(dir,time') :: acc
						end else
							acc
					with Unix.Unix_error _ ->
						CompilationServer.remove_directory cs sign dir;
						if verbose then process_server_message com "" (RemovedDirectory dir);
						acc
				) [] all_dirs in
				if verbose then process_server_message com "" (ChangedDirectories dirs);
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
							dirs := (path,ref time) :: !dirs
						with Unix.Unix_error _ ->
							()
					in
					List.iter add_dir com.class_path;
					List.iter add_dir (Path.find_directories (platform_name com.platform) true com.class_path);
					if verbose then process_server_message com "" (FoundDirectories !dirs);
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
	Typeload.type_module_hook := (fun (ctx:Typecore.typer) mpath p ->
		let t = Common.timer ["server";"module cache"] in
		let com2 = ctx.Typecore.com in
		let sign = Define.get_signature com2.defines in
		let content_changed m file =
			let ffile = Path.unique_full_path file in
			let fkey = (ffile,sign) in
			try
				let _, old_data = CompilationServer.find_file cs fkey in
				(* We must use the module path here because the file path is absolute and would cause
				   positions in the parsed declarations to differ. *)
				let new_data = Typeload.parse_module ctx m.m_path p in
				snd old_data <> snd new_data
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
					let has_file = (try check_module_shadowing com2 directories m; true with Not_found -> false) in
					if has_file then begin
						if verbose then print_endline ("A file is masking the library file " ^ s_type_path m.m_path);
						raise Not_found;
					end;
					let rec loop = function
						| [] ->
							if verbose then print_endline ("No library file was found for " ^ s_type_path m.m_path);
							raise Not_found (* no extern registration *)
						| load :: l ->
							match load m.m_path p with
							| None -> loop l
							| Some (file,_) ->
								if Path.unique_full_path file <> m.m_extra.m_file then begin
									if verbose then print_endline ("Library file was changed for " ^ s_type_path m.m_path);
									raise Not_found;
								end
					in
					loop com2.load_extern_type
				| MCode -> check_module_shadowing com2 directories m
				| MMacro when ctx.Typecore.in_macro -> check_module_shadowing com2 directories m
				| MMacro ->
					let _, mctx = MacroContext.get_macro_context ctx p in
					check_module_shadowing mctx.Typecore.com (get_changed_directories mctx) m
			in
			let has_policy policy = List.mem policy m.m_extra.m_check_policy in
			let check_file () =
				if file_time m.m_extra.m_file <> m.m_extra.m_time then begin
					if has_policy CheckFileContentModification && not (content_changed m m.m_extra.m_file) then begin
						if verbose then print_endline (Printf.sprintf "%s%s changed time not but content, reusing" (sign_string com2) m.m_extra.m_file)
					end else begin
						if verbose then process_server_message com2 "" (NotCached m);
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
					if m.m_extra.m_mark <= start_mark then begin
						if not (has_policy NoCheckShadowing) then check_module_path();
						if not (has_policy NoCheckFileTimeModification) then check_file();
					end;
					m.m_extra.m_mark <- mark;
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
					if verbose then process_server_message com2 tabs (Reusing m);
					m.m_extra.m_added <- !compilation_step;
					List.iter (fun t ->
						match t with
						| TClassDecl c -> c.cl_restore()
						| TEnumDecl e ->
							let rec loop acc = function
								| [] -> ()
								| (Meta.RealPath,[Ast.EConst (Ast.String (path,_)),_],_) :: l ->
									e.e_path <- Ast.parse_path path;
									e.e_meta <- (List.rev acc) @ l;
								| x :: l -> loop (x::acc) l
							in
							loop [] e.e_meta
						| TAbstractDecl a ->
							a.a_meta <- List.filter (fun (m,_,_) -> m <> Meta.ValueUsed) a.a_meta
						| _ -> ()
					) m.m_types;
					Typeload.add_module ctx m p;
					PMap.iter (Hashtbl.replace com2.resources) m.m_extra.m_binded_res;
					if ctx.Typecore.in_macro || com2.display.dms_full_typing then
						PMap.iter (fun _ m2 -> add_modules (tabs ^ "  ") m0 m2) m.m_extra.m_deps;
					List.iter (MacroContext.call_init_macro ctx) m.m_extra.m_macro_calls
				)
			end
		in
		try
			let m = CompilationServer.find_module cs (mpath,sign) in
			let tcheck = Common.timer ["server";"module cache";"check"] in
			begin match check m with
			| None -> ()
			| Some m' ->
				if verbose then process_server_message com2 "" (SkippingDep(m,m'));
				tcheck();
				raise Not_found;
			end;
			tcheck();
			let tadd = Common.timer ["server";"module cache";"add modules"] in
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
		let rec cache_context com =
			let cache_module m =
				CompilationServer.cache_module cs (m.m_path,m.m_extra.m_sign) m;
				(*if verbose then print_endline (Printf.sprintf "%scached %s" (sign_string com) (s_type_path m.m_path));*)
			in
			if com.display.dms_full_typing then begin
				List.iter cache_module com.modules;
				if verbose then print_endline ("Cached " ^ string_of_int (List.length com.modules) ^ " modules");
			end;
			match com.get_macros() with
			| None -> ()
			| Some com -> cache_context com
		in
		let create params =
			let ctx = create_context params in
			ctx.flush <- (fun() ->
				incr compilation_step;
				compilation_mark := !mark_loop;
				List.iter (fun s -> write (s ^ "\n"); if verbose then print_endline ("> " ^ s)) (List.rev ctx.messages);
				if ctx.has_error then begin
					measure_times := false;
					write "\x02\n"
				end else cache_context ctx.com;
			);
			ctx.setup <- (fun() ->
				let sign = Define.get_signature ctx.com.defines in
				if verbose then begin
					let defines = PMap.foldi (fun k v acc -> (k ^ "=" ^ v) :: acc) ctx.com.defines.Define.values [] in
					print_endline ("Defines " ^ (String.concat "," (List.sort compare defines)));
					print_endline ("Using signature " ^ Digest.to_hex sign);
					print_endline ("Display position: " ^ (Printer.s_pos !Parser.resume_display));
				end;
				Parser.display_error := (fun e p -> has_parse_error := true; ctx.com.error (Parser.error_msg e) p);
				if ctx.com.display.dms_display then begin
					let file = (!Parser.resume_display).pfile in
					let fkey = (file,sign) in
					(* force parsing again : if the completion point have been changed *)
					CompilationServer.remove_file cs fkey;
					CompilationServer.taint_modules cs file;
				end;
				try
					if (Hashtbl.find arguments sign) <> ctx.com.class_path then begin
						if verbose then print_endline (Printf.sprintf "%sclass paths changed, resetting directories" (sign_string ctx.com));
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
			if verbose then print_endline ("Processing Arguments [" ^ String.concat "," data ^ "]");
			(try
				DynArray.clear test_server_messages;
				Hashtbl.clear changed_directories;
				Common.display_default := DMNone;
				Parser.resume_display := null_pos;
				Typeload.return_partial_type := false;
				measure_times := false;
				close_times();
				stats.s_files_parsed := 0;
				stats.s_classes_built := 0;
				stats.s_methods_typed := 0;
				stats.s_macros_called := 0;
				Hashtbl.clear Common.htimers;
				let _ = Common.timer ["other"] in
				incr compilation_step;
				compilation_mark := !mark_loop;
				start_time := get_time();
				process_params create data;
				close_times();
				if !measure_times then report_times (fun s -> write (s ^ "\n"))
			with
			| Completion str ->
				if verbose then print_endline ("Completion Response =\n" ^ str);
				write str
			| Arg.Bad msg ->
				print_endline ("Error: " ^ msg);
			);
			if DynArray.length test_server_messages > 0 then begin
				let b = Buffer.create 0 in
				write_json (Buffer.add_string b) (JArray (DynArray.to_list test_server_messages));
				write (Buffer.contents b)
			end;
			let fl = !delays in
			delays := [];
			List.iter (fun f -> f()) fl;
			if verbose then begin
				print_endline (Printf.sprintf "Stats = %d files, %d classes, %d methods, %d macros" !(stats.s_files_parsed) !(stats.s_classes_built) !(stats.s_methods_typed) !(stats.s_macros_called));
				print_endline (Printf.sprintf "Time spent : %.3fs" (get_time() -. t0));
			end;
		with Unix.Unix_error _ ->
			if verbose then print_endline "Connection Aborted"
		| e ->
			let estr = Printexc.to_string e in
			if verbose then print_endline ("Uncaught Error : " ^ estr);
			(try write estr with _ -> ());
			if is_debug_run() then print_endline (Printexc.get_backtrace());
		);
		close();
		current_stdin := None;
		(* prevent too much fragmentation by doing some compactions every X run *)
		incr run_count;
		if !run_count mod 10 = 0 then begin
			let t0 = get_time() in
			Gc.compact();
			if verbose then begin
				let stat = Gc.quick_stat() in
				let size = (float_of_int stat.Gc.heap_words) *. 4. in
				print_endline (Printf.sprintf "Compacted memory %.3fs %.1fMB" (get_time() -. t0) (size /. (1024. *. 1024.)));
			end
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

and init_wait_socket verbose host port =
	let sock = Unix.socket Unix.PF_INET Unix.SOCK_STREAM 0 in
	(try Unix.setsockopt sock Unix.SO_REUSEADDR true with _ -> ());
	(try Unix.bind sock (Unix.ADDR_INET (Unix.inet_addr_of_string host,port)) with _ -> failwith ("Couldn't wait on " ^ host ^ ":" ^ string_of_int port));
	if verbose then print_endline ("Waiting on " ^ host ^ ":" ^ string_of_int port);
	Unix.listen sock 10;
	let bufsize = 1024 in
	let tmp = Bytes.create bufsize in
	let accept() = (
		let sin, _ = Unix.accept sock in
		Unix.set_nonblock sin;
		if verbose then print_endline "Client connected";
		let b = Buffer.create 0 in
		let rec read_loop count =
			try
				let r = Unix.recv sin tmp 0 bufsize [] in
				if r = 0 then
					failwith "Incomplete request"
				else begin
					if verbose then Printf.printf "Reading %d bytes\n" r;
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
					if verbose then print_endline "Waiting for data...";
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
