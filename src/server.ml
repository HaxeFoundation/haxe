open Printf
open Ast
open Common
open Common.DisplayMode
open Type
open DisplayOutput

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

let report_times print =
	let tot = ref 0. in
	Hashtbl.iter (fun _ t -> tot := !tot +. t.total) Common.htimers;
	if !tot > 0. then begin
		let buckets = Hashtbl.create 0 in
		let add id time calls =
			try
				let time',calls' = Hashtbl.find buckets id in
				Hashtbl.replace buckets id (time' +. time,calls' + calls)
			with Not_found ->
				Hashtbl.add buckets id (time,calls)
		in
		Hashtbl.iter (fun _ t ->
			let rec loop acc ids = match ids with
				| id :: ids ->
					add (List.rev (id :: acc)) t.total t.calls;
					loop (id :: acc) ids
				| [] ->
					()
			in
			loop [] t.id
		) Common.htimers;
		let max_name = ref 0 in
		let max_calls = ref 0 in
		let timers = Hashtbl.fold (fun id t acc ->
			let name,indent = match List.rev id with
				| [] -> assert false
				| name :: l -> name,(String.make (List.length l * 2) ' ')
			in
			let name,info = try
				let i = String.rindex name '.' in
				String.sub name (i + 1) (String.length name - i - 1),String.sub name 0 i
			with Not_found ->
				name,""
			in
			let name = indent ^ name in
			if String.length name > !max_name then max_name := String.length name;
			if snd t > !max_calls then max_calls := snd t;
			(id,name,info,t) :: acc
		) buckets [] in
		let max_calls = String.length (string_of_int !max_calls) in
		print (Printf.sprintf "%-*s | %7s |   %% | %*s | info" !max_name "name" "time(s)" max_calls "#");
		let sep = String.make (!max_name + max_calls + 21) '-' in
		print sep;
		let timers = List.sort (fun (id1,_,_,_) (id2,_,_,_) -> compare id1 id2) timers in
		let print_timer id name info (time,calls) =
			print (Printf.sprintf "%-*s | %7.3f | %3.0f | %*i | %s" !max_name name time (time *. 100. /. !tot) max_calls calls info)
		in
		List.iter (fun (id,name,info,t) -> print_timer id name info t) timers;
		print sep;
		print_timer ["total"] "total" "" (!tot,0)
	end

let default_flush ctx =
	List.iter prerr_endline (List.rev ctx.messages);
	if ctx.has_error && !prompt then begin
		print_endline "Press enter to exit...";
		ignore(read_line());
	end;
	if ctx.has_error then exit 1

let create_context params =
	let ctx = {
		com = Common.create Globals.version Globals.s_version params;
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
	loop 0 (String.length str)

let rec wait_loop process_params verbose accept =
	Sys.catch_break false;
	let has_parse_error = ref false in

	let signs = ref [] in
	let sign_string com =
		let sign = get_signature com in
		let	sign_id =
			try
				List.assoc sign !signs
			with Not_found ->
				let i = string_of_int (List.length !signs) in
				signs := (sign,i) :: !signs;
				print_endline (Printf.sprintf "Found context %s:\n%s" i (dump_context com));
				i
		in
		Printf.sprintf "%2s,%3s: " sign_id (short_platform_name com.platform)
	in
	let cs = CompilationServer.create () in
	Typer.macro_enable_cache := true;
	let current_stdin = ref None in
	Typeload.parse_hook := (fun com2 file p ->
		let ffile = Path.unique_full_path file in
		let is_display_file = ffile = (!Parser.resume_display).Ast.pfile in

		match is_display_file, !current_stdin with
		| true, Some stdin when Common.defined com2 Define.DisplayStdin ->
			Typeload.parse_file_from_string com2 file p stdin
		| _ ->
			let sign = get_signature com2 in
			let ftime = file_time ffile in
			let fkey = (ffile,sign) in
			try
				let time, data = CompilationServer.find_file cs fkey in
				if time <> ftime then raise Not_found;
				data
			with Not_found ->
				has_parse_error := false;
				let data = Typeload.parse_file com2 file p in
				let info = if !has_parse_error then "not cached, has parse error"
					else if is_display_file then "not cached, is display file"
					else begin try
						(* We assume that when not in display mode it's okay to cache stuff that has #if display
						   checks. The reasoning is that non-display mode has more information than display mode. *)
						if not com2.display.dms_display then raise Not_found;
						let ident = Hashtbl.find Parser.special_identifier_files ffile in
						Printf.sprintf "not cached, using \"%s\" define" ident;
					with Not_found ->
						CompilationServer.cache_file cs fkey (ftime,data);
						"cached"
				end in
				if verbose then print_endline (Printf.sprintf "%sparsed %s (%s)" (sign_string com2) ffile info);
				data
	);
	let check_module_path com m p =
		if m.m_extra.m_file <> Path.unique_full_path (Typeload.resolve_module_file com m.m_path (ref[]) p) then begin
			if verbose then print_endline ("Module path " ^ s_type_path m.m_path ^ " has been changed");
			raise Not_found;
		end
	in
	let compilation_step = ref 0 in
	let compilation_mark = ref 0 in
	let mark_loop = ref 0 in
	Typeload.type_module_hook := (fun (ctx:Typecore.typer) mpath p ->
		let t = Common.timer ["server";"module cache"] in
		let com2 = ctx.Typecore.com in
		let sign = get_signature com2 in
		let dep = ref None in
		incr mark_loop;
		let mark = !mark_loop in
		let start_mark = !compilation_mark in
		let rec check m =
			if m.m_extra.m_dirty then begin
				dep := Some m;
				false
			end else if m.m_extra.m_mark = mark then
				true
			else try
				if m.m_extra.m_mark <= start_mark then begin
					(match m.m_extra.m_kind with
					| MFake | MSub | MImport -> () (* don't get classpath *)
					| MExtern ->
						(* if we have a file then this will override our extern type *)
						let has_file = (try ignore(Typeload.resolve_module_file com2 m.m_path (ref[]) p); true with Not_found -> false) in
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
					| MCode -> check_module_path com2 m p
					| MMacro when ctx.Typecore.in_macro -> check_module_path com2 m p
					| MMacro ->
						let _, mctx = Typer.get_macro_context ctx p in
						check_module_path mctx.Typecore.com m p
					);
					if file_time m.m_extra.m_file <> m.m_extra.m_time then begin
						if verbose then print_endline (Printf.sprintf "%s%s not cached (%s)" (sign_string com2) (s_type_path m.m_path) (if m.m_extra.m_time = -1. then "macro-in-macro" else "modified"));
						if m.m_extra.m_kind = MFake then Hashtbl.remove Typecore.fake_modules m.m_extra.m_file;
						raise Not_found;
					end;
				end;
				m.m_extra.m_mark <- mark;
				PMap.iter (fun _ m2 -> if not (check m2) then begin dep := Some m2; raise Not_found end) m.m_extra.m_deps;
				true
			with Not_found ->
				m.m_extra.m_dirty <- true;
				false
		in
		let rec add_modules tabs m0 m =
			if m.m_extra.m_added < !compilation_step then begin
				(match m0.m_extra.m_kind, m.m_extra.m_kind with
				| MCode, MMacro | MMacro, MCode ->
					(* this was just a dependency to check : do not add to the context *)
					PMap.iter (Hashtbl.replace com2.resources) m.m_extra.m_binded_res;
				| _ ->
					if verbose then print_endline (Printf.sprintf "%s%sreusing %s" (sign_string com2) tabs (s_type_path m.m_path));
					m.m_extra.m_added <- !compilation_step;
					List.iter (fun t ->
						match t with
						| TClassDecl c -> c.cl_restore()
						| TEnumDecl e ->
							let rec loop acc = function
								| [] -> ()
								| (Ast.Meta.RealPath,[Ast.EConst (Ast.String path),_],_) :: l ->
									e.e_path <- Ast.parse_path path;
									e.e_meta <- (List.rev acc) @ l;
								| x :: l -> loop (x::acc) l
							in
							loop [] e.e_meta
						| TAbstractDecl a ->
							a.a_meta <- List.filter (fun (m,_,_) -> m <> Ast.Meta.ValueUsed) a.a_meta
						| _ -> ()
					) m.m_types;
					if m.m_extra.m_kind <> MSub then Typeload.add_module ctx m p;
					PMap.iter (Hashtbl.replace com2.resources) m.m_extra.m_binded_res;
					PMap.iter (fun _ m2 -> add_modules (tabs ^ "  ") m0 m2) m.m_extra.m_deps);
					List.iter (Typer.call_init_macro ctx) m.m_extra.m_macro_calls
			end
		in
		try
			let m = CompilationServer.find_module cs (mpath,sign) in
			let tcheck = Common.timer ["server";"module cache";"check"] in
			if not (check m) then begin
				if verbose then print_endline (Printf.sprintf "%sskipping %s%s" (sign_string com2) (s_type_path m.m_path) (Option.map_default (fun m -> Printf.sprintf " (via %s)" (s_type_path m.m_path)) "" !dep));
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
				if verbose then print_endline (Printf.sprintf "%scached %s" (sign_string com) (s_type_path m.m_path));
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
				if ctx.has_error then write "\x02\n" else cache_context ctx.com;
			);
			ctx.setup <- (fun() ->
				if verbose then begin
					let defines = PMap.foldi (fun k v acc -> (k ^ "=" ^ v) :: acc) ctx.com.defines [] in
					print_endline ("Defines " ^ (String.concat "," (List.sort compare defines)));
					print_endline ("Using signature " ^ Digest.to_hex (get_signature ctx.com));
					print_endline ("Display position: " ^ (Printer.s_pos !Parser.resume_display));
				end;
				Parser.display_error := (fun e p -> has_parse_error := true; ctx.com.error (Parser.error_msg e) p);
				if ctx.com.display.dms_display then begin
					let file = (!Parser.resume_display).Ast.pfile in
					let fkey = (file,get_signature ctx.com) in
					(* force parsing again : if the completion point have been changed *)
					CompilationServer.remove_file cs fkey;
					(* force module reloading (if cached) *)
					CompilationServer.taint_modules cs file;
				end
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
				Common.display_default := DMNone;
				Parser.resume_display := Ast.null_pos;
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
				prerr_endline ("Error: " ^ msg);
			);
			if verbose then begin
				print_endline (Printf.sprintf "Stats = %d files, %d classes, %d methods, %d macros" !(stats.s_files_parsed) !(stats.s_classes_built) !(stats.s_methods_typed) !(stats.s_macros_called));
				print_endline (Printf.sprintf "Time spent : %.3fs" (get_time() -. t0));
			end
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
		IO.really_nread chin len
	in
	let write = Buffer.add_string berr in
	let close = fun() ->
		IO.write_i32 cherr (Buffer.length berr);
		IO.nwrite cherr (Buffer.contents berr);
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
	let tmp = String.create bufsize in
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
					Buffer.add_substring b tmp 0 r;
					if tmp.[r-1] = '\000' then
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
		let write = ssend sin in
		let close() = Unix.close sin in
		read, write, close
	) in
	accept

and do_connect host port args =
	let sock = Unix.socket Unix.PF_INET Unix.SOCK_STREAM 0 in
	(try Unix.connect sock (Unix.ADDR_INET (Unix.inet_addr_of_string host,port)) with _ -> failwith ("Couldn't connect on " ^ host ^ ":" ^ string_of_int port));
	let args = ("--cwd " ^ Unix.getcwd()) :: args in
	ssend sock (String.concat "" (List.map (fun a -> a ^ "\n") args) ^ "\000");
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
	let tmp = String.create 1024 in
	let rec loop() =
		let b = Unix.recv sock tmp 0 1024 [] in
		Buffer.add_substring buf tmp 0 b;
		if b > 0 then begin
			if String.get tmp (b - 1) = '\n' then begin
				process();
				Buffer.reset buf;
			end;
			loop();
		end
	in
	loop();
	process();
	if !has_error then exit 1