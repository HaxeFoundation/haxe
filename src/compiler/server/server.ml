open Globals
open Common
open CompilationCache
open Type
open DisplayProcessingGlobals
open Ipaddr
open Json
open CompilationContext
open MessageReporting
open HxbData
open TypeloadCacheHook

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
		{ support_nonblock = allow_nonblock; read; write; close; get_stdin = (fun () -> None) }

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

module Connect = struct

	let poll sock print =
		let response_buf = Buffer.create 0 in
		let process_response () =
			let lines = ExtString.String.nsplit (Buffer.contents response_buf) "\n" in
			let lines = (match List.rev lines with "" :: l -> List.rev l | _ -> lines) in
			List.iter print lines;
		in
		(* Use Unix.select to multiplex reading from both server socket and local stdin,
		avoiding the need for a separate forwarding thread. *)
		let stdin_fd = Unix.descr_of_in_channel Stdlib.stdin in
		let stdin_buf = Bytes.create 1024 in
		let sock_buf = Bytes.create 1024 in
		let stdin_active = ref true in
		let sock_open = ref true in
		let rec loop () =
			let read_fds = (if !sock_open then [sock] else []) @ (if !stdin_active then [stdin_fd] else []) in
			if read_fds = [] then ()
			else begin
				let readable, _, _ = Unix.select read_fds [] [] (-1.0) in
				List.iter (fun fd ->
					if fd = stdin_fd then begin
						let n = Unix.read fd stdin_buf 0 1024 in
						if n = 0 then begin
							stdin_active := false;
							(try Unix.shutdown sock Unix.SHUTDOWN_SEND with _ -> ())
						end else
							ssend sock (Bytes.sub stdin_buf 0 n)
					end else begin
						let b = Unix.recv sock sock_buf 0 1024 [] in
						Buffer.add_subbytes response_buf sock_buf 0 b;
						if b > 0 then begin
							if Bytes.get sock_buf (b - 1) = '\n' then begin
								process_response ();
								Buffer.reset response_buf;
							end
						end else
							sock_open := false
					end
				) readable;
				if !sock_open then loop ()
			end
		in
		loop ();
		process_response ()

	(* The connect function to connect to [host] at [port] and send arguments [args]. *)
	let do_connect ip port args =
		let (domain, host) = match ip with
			| V4 ip -> (Unix.PF_INET, V4.to_string ip)
			| V6 ip -> (Unix.PF_INET6, V6.to_string ip)
		in
		let sock = Unix.socket domain Unix.SOCK_STREAM 0 in
		(try Unix.connect sock (Unix.ADDR_INET (Unix.inet_addr_of_string host,port)) with
			| Unix.Unix_error(code,_,_) -> failwith("Couldn't connect on " ^ host ^ ":" ^ string_of_int port ^ " (" ^ (Unix.error_message code) ^ ")");
			| _ -> failwith ("Couldn't connect on " ^ host ^ ":" ^ string_of_int port)
		);
		let rec display_stdin args =
			match args with
			| [] -> ""
			| "-D" :: ("display_stdin" | "display-stdin") :: _ ->
				let accept = init_wait_stdio() in
				let conn = accept() in
				Option.default "" (conn.read true)
			| _ :: args ->
				display_stdin args
		in
		let args = ("--cwd " ^ Unix.getcwd()) :: args in
		let s = (String.concat "" (List.map (fun a -> a ^ "\n") args)) ^ (display_stdin args) in
		ssend sock (Bytes.of_string (s ^ "\000"));
		let has_error = ref false in
		let print line =
			match (if line = "" then '\x00' else line.[0]) with
			| '\x01' ->
				print_string (String.concat "\n" (List.tl (ExtString.String.nsplit line "\x01")));
				flush stdout
			| '\x02' ->
				has_error := true;
			| _ ->
				prerr_endline line;
		in
		poll sock print;
		if !has_error then exit 1 else exit 0
end

module SocketRequest = struct
	type t = {
		data : string;
		stdin : in_channel;
	}

	let setup_client_stdin_forward overflow sin =
		(* Set up stdin forwarding: create a pipe and a thread that reads
		   from the client socket and writes to the pipe. *)
		let (stdin_r_fd, stdin_w_fd) = Unix.pipe ~cloexec:true () in
		let _stdin_thread = Thread.create (fun () ->
			let write_all fd data pos len =
				let rec loop pos len =
					if len > 0 then begin
						let w = Unix.write fd data pos len in
						loop (pos + w) (len - w)
					end
				in
				loop pos len
			in
			let buf = Bytes.create 1024 in
			(try
				(* Write any overflow data read past the null terminator *)
				if Bytes.length overflow > 0 then
					write_all stdin_w_fd overflow 0 (Bytes.length overflow);
				(* Forward data from client socket to stdin pipe *)
				while true do
					let n = Unix.recv sin buf 0 1024 [] in
					if n = 0 then raise Exit;
					write_all stdin_w_fd buf 0 n
				done
			with _ -> ());
			(try Unix.close stdin_w_fd with _ -> ())
		) () in
		Unix.in_channel_of_descr stdin_r_fd

	(* Reads a null-terminated request from a non-blocking socket, tracking any
	   overflow data received past the null terminator (e.g. stdin data from the client). *)
	let read sin bufsize =
		let tmp = Bytes.create bufsize in
		let b = Buffer.create 0 in
		let overflow = ref Bytes.empty in
		let rec read_loop count =
			try
				let r = Unix.recv sin tmp 0 bufsize [] in
				if r = 0 then
					failwith "Incomplete request"
				else begin
					ServerMessage.socket_message (Printf.sprintf "Reading %d bytes\n" r);
					let rec find_null i = if i >= r then -1 else if Bytes.get tmp i = '\000' then i else find_null (i + 1) in
					let null_pos = find_null 0 in
					if null_pos >= 0 then begin
						Buffer.add_subbytes b tmp 0 null_pos;
						let remaining = r - null_pos - 1 in
						if remaining > 0 then
							overflow := Bytes.sub tmp (null_pos + 1) remaining;
						Buffer.contents b
					end else begin
						Buffer.add_subbytes b tmp 0 r;
						read_loop 0
					end
				end
			with Unix.Unix_error((Unix.EWOULDBLOCK|Unix.EAGAIN),_,_) ->
				if count = 100 then
					failwith "Aborting inactive connection"
				else begin
					ServerMessage.socket_message "Waiting for data...";
					ignore(Unix.select [] [] [] 0.05);
					read_loop (count + 1);
				end
		in
		let data = read_loop 0 in
		let stdin = setup_client_stdin_forward !overflow sin in
		{ data; stdin }
end

let process sctx entry comm args =
	let t0 = Extc.time() in
	ServerMessage.arguments args;
	ServerCompilationContext.reset sctx;
	let api = {
		on_context_create = (fun () ->
			sctx.compilation_step <- sctx.compilation_step + 1;
			sctx.compilation_step;
		);
		cache = sctx.cs;
		callbacks = {
			before_anything = ServerCache.before_anything sctx;
			after_target_init = ServerCache.after_target_init sctx;
			after_save = ServerCache.after_save sctx;
			after_compilation = ServerCache.after_compilation sctx;
		};
	} in
	entry api comm args;
	ServerCompilationContext.run_delays sctx;
	ServerMessage.stats stats (Extc.time() -. t0)

(* The server main loop. Waits for the [accept] call to then process the sent compilation
   parameters through [process_params]. *)
let wait_loop entry verbose accept =
	if verbose then ServerMessage.enable_all ();
	Sys.catch_break false; (* Sys can never catch a break *)
	(* Ignore SIGPIPE to prevent process termination when stdin pipe is closed.
	   Sys.sigpipe may not map to the real signal number, so use 13 directly. *)
	(try Sys.set_signal 13 Sys.Signal_ignore with _ -> ());
	(* Create server context and set up hooks for parsing and typing *)
	let sctx = ServerCompilationContext.create verbose in
	let cs = sctx.cs in
	ServerCache.enable_cache_mode sctx;
	(* Main loop: accept connections and process arguments *)
	while true do
		let conn = accept() in
		begin try
			(* Read arguments *)
			let rec loop block =
				match conn.read block with
				| Some s ->
					let hxml =
						try
							let idx = String.index s '\001' in
							sctx.current_stdin <- Some (String.sub s (idx + 1) ((String.length s) - idx - 1));
							(String.sub s 0 idx)
						with Not_found ->
							s
					in
					sctx.current_stdin_pipe <- conn.get_stdin ();
					let data = Helper.parse_hxml_data hxml in
					process sctx entry (ServerCommunication.Communication.create_pipe sctx conn.write sctx.current_stdin_pipe) data
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
			loop (not conn.support_nonblock)
		with Unix.Unix_error _ ->
			ServerMessage.socket_message "Connection Aborted"
		| e ->
			let estr = Printexc.to_string e in
			ServerMessage.uncaught_error estr;
			(try conn.write ("\x02\n" ^ estr); with _ -> ());
			if Helper.is_debug_run then print_endline (estr ^ "\n" ^ Printexc.get_backtrace());
			if e = Out_of_memory then begin
				conn.close();
				exit (-1);
			end;
		end;
		(* Close connection and perform some cleanup *)
		conn.close();
		sctx.current_stdin <- None;
		sctx.current_stdin_pipe <- None;
		ServerCompilationContext.cleanup();
		(* If our connection always blocks, we have to execute all pending tasks now. *)
		if not conn.support_nonblock then
			while cs#has_task do cs#get_task#run done
		else if sctx.was_compilation then
			cs#add_task (new Tasks.server_exploration_task cs)
	done;
	0

(* Connect to given host/port and return accept function for communication *)
let init_wait_connect ip port =
	let host = match ip with
		| V4 ip -> V4.to_string ip
		| V6 ip -> V6.to_string ip
	in
	let host = Unix.inet_addr_of_string host in
	let chin, chout = Unix.open_connection (Unix.ADDR_INET (host,port)) in
	mk_length_prefixed_communication true chin chout

(* The accept-function to wait for a socket connection. *)
let init_wait_socket ip port =
	let (domain, host) = match ip with
		| V4 ip -> (Unix.PF_INET, V4.to_string ip)
		| V6 ip -> (Unix.PF_INET6, V6.to_string ip)
	in
	let sock = Unix.socket domain Unix.SOCK_STREAM 0 in
	(try Unix.setsockopt sock Unix.SO_REUSEADDR true with _ -> ());
	(try Unix.bind sock (Unix.ADDR_INET (Unix.inet_addr_of_string host,port)) with _ -> failwith ("Couldn't wait on " ^ host ^ ":" ^ string_of_int port));
	ServerMessage.socket_message ("Waiting on " ^ host ^ ":" ^ string_of_int port);
	Unix.listen sock 10;
	let bufsize = 1024 in
	let accept() = (
		let sin, _ = Unix.accept sock in
		Unix.set_nonblock sin;
		ServerMessage.socket_message "Client connected";
		let stdin_pipe = ref None in
		let read = fun _ ->
			let req = SocketRequest.read sin bufsize in
			Unix.clear_nonblock sin;
			stdin_pipe := Some (req.stdin);
			Some req.data
		in
		let get_stdin () = !stdin_pipe in
		let closed = ref false in
		let close() =
			if not !closed then begin
				try Unix.close sin with Unix.Unix_error _ -> trace "Error while closing socket.";
				closed := true;
			end
		in
		let write s =
			if not !closed then
				match Unix.getsockopt_error sin with
				| Some _ -> close()
				| None -> ssend sin (Bytes.unsafe_of_string s);
		in
		{ support_nonblock = false; read; write; close; get_stdin }
	) in
	accept
