open Globals
open Common
open CompilationCache
open Type
open DisplayProcessingGlobals
open Ipaddr
open Json
open CompilationContext
open ParsedArg
open MessageReporting
open HxbData
open TypeloadCacheHook

let mk_length_prefixed_communication allow_nonblock chin chout =
	let sin = Unix.descr_of_in_channel chin in
	Unix.clear_nonblock sin;
	let chin = IO.input_channel chin in
	let chout = IO.output_channel chout in

	let bout = Buffer.create 0 in

	let read () =
        let len = IO.read_i32 chin in
        IO.really_nread_string chin len
	in

	let write = Buffer.add_string bout in

	let close = fun() ->
		flush stdout;
		IO.write_i32 chout (Buffer.length bout);
		IO.nwrite_string chout (Buffer.contents bout);
		IO.flush chout;
		Buffer.clear bout
	in

	fun () ->
		{ read; write; close; get_stdin = (fun () -> None) }

module Connect = struct
	(* The connect function to connect to [host] at [port] and send arguments [args]. *)
	let do_connect ip port (args : parsed_arg list) =
		let (domain, host) = match ip with
			| V4 ip -> (Unix.PF_INET, V4.to_string ip)
			| V6 ip -> (Unix.PF_INET6, V6.to_string ip)
		in
		let sock = Unix.socket domain Unix.SOCK_STREAM 0 in
		(try Unix.connect sock (Unix.ADDR_INET (Unix.inet_addr_of_string host,port)) with
			| Unix.Unix_error(code,_,_) -> failwith("Couldn't connect on " ^ host ^ ":" ^ string_of_int port ^ " (" ^ (Unix.error_message code) ^ ")");
			| _ -> failwith ("Couldn't connect on " ^ host ^ ":" ^ string_of_int port)
		);
		let raw_args = ("--cwd " ^ Unix.getcwd()) :: Args.to_raw_args args in
		let s = (String.concat "" (List.map (fun a -> a ^ "\n") raw_args)) in
		PipeThings.ssend sock (Bytes.of_string (s ^ "\000"));
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
		PipeThings.poll sock print;
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

let create_request_scope () =
	{
		stats = Stats.create ();
		timer_ctx = Timer.make_context (Timer.make ["other"]);
		cancellation_requested = false;
	}

let process sctx request_scope entry comm (args : parsed_arg list) =
	let t0 = Extc.time() in
	ServerMessage.arguments ["<" ^ string_of_int (List.length args) ^ " pre-parsed args>"];
	ServerCompilationContext.reset sctx;
	entry sctx request_scope comm args;
	ServerCompilationContext.run_delays sctx;
	ServerMessage.stats request_scope.stats (Extc.time() -. t0)

module RequestQueue = struct
	type request = {
		args : parsed_arg list;
		stdin : string option;
		stdin_pipe : in_channel option;
		conn : server_connection;
	}

	type t = {
		mutex : Mutex.t;
		semaphore : Semaphore.Counting.t;
		mutable requests : request list;
		mutable current_request : request_scope option;
		shutdown_flag : bool Atomic.t;
		cancel_token : bool Atomic.t;
	}

	let create () =
		{
			mutex = Mutex.create ();
			semaphore = Semaphore.Counting.make 0;
			requests = [];
			current_request = None;
			shutdown_flag = Atomic.make false;
			cancel_token = Atomic.make false;
		}

	let wake_up rq =
		Semaphore.Counting.release rq.semaphore

	let add rq args stdin stdin_pipe conn =
		Mutex.lock rq.mutex;
		rq.requests <- { args; stdin; stdin_pipe; conn } :: rq.requests;
		Mutex.unlock rq.mutex;
		wake_up rq

	let shutdown rq =
		Atomic.set rq.cancel_token true;
		Atomic.set rq.shutdown_flag true;
		wake_up rq
end

module WorkerDomain = struct
	open RequestQueue
	open ServerCompilationContext

	type t = {
		domain : unit Domain.t;
	}

	let shutdown rq =
		(* Drain remaining requests by closing their connections, then return
		   without recursing to exit the loop gracefully. *)
		Mutex.lock rq.mutex;
		let pending = rq.requests in
		rq.requests <- [];
		Mutex.unlock rq.mutex;
		List.iter (fun req ->
			(try req.conn.write "\x02\nServer shutdown\n"; with _ -> ());
			req.conn.close()
		) pending

	let run_request sctx request_scope entry {conn; stdin; stdin_pipe; args} =
		try
			process sctx request_scope entry (ServerCommunication.Communication.create_pipe sctx conn.write stdin_pipe) args;
		with
		| Cancelled ->
			ServerMessage.uncaught_error "Compilation cancelled";
			(try conn.write "\x02\nCancelled\n"; with _ -> ());
		| e ->
			let estr = Printexc.to_string e in
			ServerMessage.uncaught_error estr;
			(try conn.write ("\x02\n" ^ estr); with _ -> ());
			if Helper.is_debug_run then print_endline (estr ^ "\n" ^ Printexc.get_backtrace());
			if e = Out_of_memory then begin
				conn.close();
				exit (-1);
			end

	let create sctx entry rq =
		let domain = Domain.spawn (fun () ->
			let cs = sctx.cs in
			let rec loop () =
				Semaphore.Counting.acquire rq.semaphore;
				(* Check for shutdown before doing any work *)
				if Atomic.get rq.shutdown_flag then begin
					shutdown rq
				end else begin
					Mutex.lock rq.mutex;
					match rq.requests with
					| [] ->
						Mutex.unlock rq.mutex;
						if cs#has_task then begin
							cs#get_task#run;
							RequestQueue.wake_up rq;
						end;
						loop()
					| request :: l ->
						rq.requests <- l;
						Mutex.unlock rq.mutex;
						sctx.current_stdin <- request.stdin;
						Atomic.set rq.cancel_token false;
						let request_scope = create_request_scope() in
						rq.current_request <- Some request_scope;
						run_request sctx request_scope entry request;
						request.conn.close();
						sctx.current_stdin <- None;
						ServerCache.cleanup();
						if sctx.was_compilation then
							cs#add_task (new Tasks.server_exploration_task cs);
						RequestQueue.wake_up rq;
						loop()
				end
			in
			loop ()
		) in
		{
			domain;
		}
end

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
	ServerCache.enable_cache_mode sctx;
	let rq = RequestQueue.create () in
	let worker = WorkerDomain.create sctx entry rq in
	(* Main loop: accept connections and enqueue requests for the worker.
	   The loop exits if the accept function raises an exception (e.g. socket closed). *)
	begin try
		while true do
			let conn = accept() in
			begin try
				let s = conn.read () in
				let stdin,hxml =
					try
						let idx = String.index s '\001' in
						let stdin = (String.sub s (idx + 1) ((String.length s) - idx - 1)) in
						Some stdin,(String.sub s 0 idx)
					with Not_found ->
						None,s
				in
				let stdin_pipe = conn.get_stdin () in
				let data = Helper.parse_hxml_data hxml in
				let parsed_args = Args.parse_args sctx data in
				RequestQueue.add rq parsed_args stdin stdin_pipe conn;
			with Unix.Unix_error _ ->
				ServerMessage.socket_message "Connection Aborted";
				conn.close()
			end;
		done
	with _ ->
		()
	end;
	(* Signal the worker to shut down and wait for it to finish *)
	RequestQueue.shutdown rq;
	Domain.join worker.domain;
	ServerCompilationContext.dispose sctx;
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
		let read () =
			let req = SocketRequest.read sin bufsize in
			Unix.clear_nonblock sin;
			stdin_pipe := Some (req.stdin);
			req.data
		in
		let get_stdin () = !stdin_pipe in
		let closed = ref false in
		let close() =
			if not !closed then begin
				closed := true;
				(* Shutdown before close to ensure FIN is sent to the client even if
				   the stdin-forwarding thread has a pending recv on the same fd.
				   Unix.close alone may not send FIN while another thread blocks on recv. *)
				(try Unix.shutdown sin Unix.SHUTDOWN_ALL with Unix.Unix_error _ -> ());
				(try Unix.close sin with Unix.Unix_error _ -> trace "Error while closing socket.");
			end
		in
		let write s =
			if not !closed then
				match Unix.getsockopt_error sin with
				| Some _ -> close()
				| None -> PipeThings.ssend sin (Bytes.unsafe_of_string s);
		in
		{ read; write; close; get_stdin }
	) in
	accept
