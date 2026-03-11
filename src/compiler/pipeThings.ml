open ServerCommunication
open CompilationContext

(** Handles IO piping between the compilation server and its clients.

    In server mode (--connect), the compiler runs as a long-lived process.
    Client requests arrive over a socket, and we need to redirect the
    compilation's stdin/stdout/stderr through the socket protocol rather
    than using the server process's own file descriptors.

    The socket protocol uses newline-framed messages with prefix bytes:
    - [\x01]: stdout data (newlines within the data are encoded as [\x01] separators)
    - [\x02]: error flag
    - other: stderr (written verbatim)

    Stdin data from the client is forwarded as raw bytes after the null-terminated
    argument string, so newlines in stdin require no special encoding. *)

(** Reads all available data from [channel] in 1024-byte chunks,
	passing each chunk to [f]. Stops on EOF or Unix error. *)
let rec read_content channel buf f =
	begin try
		let i = input channel buf 0 1024 in
		if i > 0 then begin
			f (Bytes.unsafe_to_string (Bytes.sub buf 0 i));
			read_content channel buf f
		end
	with Unix.Unix_error _ ->
		()
	end

(** Creates a pipe where the write end is an [out_channel] and a background
	thread reads from the read end, forwarding chunks to [write_fn].
	Returns [(out_channel, thread)] — the caller writes to [out_channel],
	and [write_fn] receives the data asynchronously. Used to bridge
	OCaml channel writes (e.g. [Sys.println]) to the socket protocol. *)
let make_output_pipe write_fn =
	let (r_fd, w_fd) = Unix.pipe ~cloexec:true () in
	let out_ch = Unix.out_channel_of_descr w_fd in
	let in_ch = Unix.in_channel_of_descr r_fd in
	let thread = Thread.create (fun () ->
		let buf = Bytes.create 1024 in
		(try while true do
			let n = input in_ch buf 0 1024 in
			if n = 0 then raise Exit;
			write_fn (Bytes.sub_string buf 0 n)
		done with
		| End_of_file | Exit -> ()
		| Unix.Unix_error _ -> ());
		close_in_noerr in_ch
	) () in
	(out_ch, thread)

(** Returns the stdin [in_channel] for this compilation context.
	In server mode, [comm.stdin] is [Some ch] when the client forwarded
	stdin data over the socket (see {!SocketRequest.setup_client_stdin_forward}).
	When [None] (no stdin data), creates a pipe with the write end immediately
	closed so that reads return EOF. *)
let get_stdin_channel comm =
	match comm.stdin with
	| Some ch -> ch
	| None ->
		let (stdin_r_fd, stdin_w_fd) = Unix.pipe ~cloexec:true () in
		Unix.close stdin_w_fd;
		Unix.in_channel_of_descr stdin_r_fd

(** Pipe-based implementation of [Sys.getChar] for server mode.
	Reads a single byte from [stdin_ch] and optionally echoes it to [stdout_ch].
	Returns -1 on EOF, matching the convention of the native [Extc.getch]. *)
let getch_from_channel stdin_ch stdout_ch echo =
	let c = try
		int_of_char (input_char stdin_ch)
	with End_of_file ->
		-1
	in
	if echo && c >= 0 then begin
		output_char stdout_ch (char_of_int c);
		flush stdout_ch
	end;
	c

(** Creates the {!Gctx.compilation_io} record for this compilation.

	In server mode ([comm.is_server = true]):
	- stdout/stderr are pipe-backed channels with background threads that
		forward writes through [comm.write_out]/[comm.write_err] (the socket protocol)
	- stdin comes from the client's forwarded data (or an immediately-closed pipe)
	- [getch] reads from the stdin pipe instead of the terminal
	- [close] flushes and joins all background threads

	In non-server mode:
	- channels are the process's real stdin/stdout/stderr
	- [getch] uses [Extc.getch] for native terminal raw-mode reading *)
let create_io comm =
	if comm.is_server then begin
		let (stdout_ch, stdout_thread) = make_output_pipe comm.write_out in
		let (stderr_ch, stderr_thread) = make_output_pipe comm.write_err in
		let stdin_ch = get_stdin_channel comm in
		let closed = ref false in
		{
			Gctx.print = comm.write_out;
			print_err = comm.write_err;
			stdout = stdout_ch;
			stderr = stderr_ch;
			stdin = stdin_ch;
			getch = getch_from_channel stdin_ch stdout_ch;
			close = (fun () ->
				if not !closed then begin
					closed := true;
					flush stdout_ch; close_out_noerr stdout_ch; Thread.join stdout_thread;
					flush stderr_ch; close_out_noerr stderr_ch; Thread.join stderr_thread;
					close_in_noerr stdin_ch;
				end
			);
		}
	end else
		{
			Gctx.print = comm.write_out;
			print_err = comm.write_err;
			stdout = Stdlib.stdout;
			stderr = Stdlib.stderr;
			stdin = Stdlib.stdin;
			getch = Extc.getch;
			close = (fun () -> ());
		}

(** Runs a shell command in server mode, forwarding stdin from the client
	and capturing stdout/stderr through the socket protocol.
	Uses {!Process.run} to create the child process so we can connect
	the child's stdin to the client's forwarded data and properly signal
	EOF when the client closes its end. *)
let run_command comm cmd =
	let proc = Process.run cmd None in
	let pout = Unix.in_channel_of_descr proc.Process.stdout_fd in
	let pin = Unix.out_channel_of_descr proc.Process.stdin_fd in
	let perr = Unix.in_channel_of_descr proc.Process.stderr_fd in
	let bout = Bytes.create 1024 in
	let berr = Bytes.create 1024 in
	(* Use a flag to signal the stdin-forwarding thread to stop.
		The thread uses Unix.select with a timeout so it can check this flag
		periodically, avoiding a hang when the child exits but the client
		hasn't closed its stdin (e.g. interactive use or partial writes). *)
	let stop_stdin = ref false in
	let tin = match comm.stdin with
		| Some stdin_pipe ->
			let stdin_fd = Unix.descr_of_in_channel stdin_pipe in
			Some (Thread.create (fun () ->
				let buf = Bytes.create 1024 in
				(try while not !stop_stdin do
					let readable, _, _ = Unix.select [stdin_fd] [] [] 0.05 in
					if readable <> [] then begin
						let i = Unix.read stdin_fd buf 0 1024 in
						if i = 0 then raise Exit;
						output pin buf 0 i;
						flush pin
					end
				done with _ -> ());
				close_out_noerr pin
			) ())
		| None ->
			close_out_noerr pin;
			None
	in
	let tout = Thread.create (fun() -> read_content pout bout comm.write_out) () in
	let terr = Thread.create (fun() -> read_content perr berr comm.write_err) () in
	(* Join stdout/stderr threads first — they complete when the child closes
		its output fds (typically on exit). Then reap the child process, signal
		the stdin thread to stop, and join it. *)
	Thread.join tout;
	Thread.join terr;
	close_in_noerr pout;
	close_in_noerr perr;
	let code = Process.exit proc in
	stop_stdin := true;
	(match tin with Some t -> Thread.join t | None -> ());
	code

let ssend sock str =
	let rec loop pos len =
		if len = 0 then
			()
		else
			let s = Unix.send sock str pos len [] in
			loop (pos + s) (len - s)
	in
	loop 0 (Bytes.length str)

let poll sock print =
	let response_buf = Buffer.create 0 in
	(* Process all complete lines (up to the last newline) in the buffer,
	   keeping any partial unflushed line for the next read. *)
	let flush_complete_lines () =
		let s = Buffer.contents response_buf in
		match String.rindex_opt s '\n' with
		| None -> ()
		| Some last_nl ->
			let complete = String.sub s 0 (last_nl + 1) in
			let remaining = String.sub s (last_nl + 1) (String.length s - last_nl - 1) in
			let lines = ExtString.String.nsplit complete "\n" in
			let lines = (match List.rev lines with "" :: l -> List.rev l | _ -> lines) in
			List.iter print lines;
			Buffer.reset response_buf;
			if remaining <> "" then Buffer.add_string response_buf remaining
	in
	(* Forward stdin to the server socket in a background thread.
	   Using a dedicated thread avoids mixing socket and non-socket file
	   descriptors in Unix.select, which has known issues on Windows. *)
	let stdin_buf = Bytes.create 1024 in
	let _ = Thread.create (fun () ->
		(try
			let rec loop () =
				let n = Unix.read (Unix.descr_of_in_channel Stdlib.stdin) stdin_buf 0 1024 in
				if n = 0 then
					(try Unix.shutdown sock Unix.SHUTDOWN_SEND with _ -> ())
				else begin
					ssend sock (Bytes.sub stdin_buf 0 n);
					loop ()
				end
			in
			loop ()
		with _ -> ())
	) () in
	(* Read server output until the connection closes, printing lines immediately
	   as they arrive rather than waiting for the server to disconnect. *)
	let sock_buf = Bytes.create 1024 in
	let sock_open = ref true in
	while !sock_open do
		let b = Unix.recv sock sock_buf 0 1024 [] in
		Buffer.add_subbytes response_buf sock_buf 0 b;
		if b <= 0 then
			sock_open := false
		else
			flush_complete_lines ()
	done;
	(* Flush any remaining partial line after the server closes the connection *)
	let s = Buffer.contents response_buf in
	if s <> "" then begin
		let lines = ExtString.String.nsplit s "\n" in
		let lines = (match List.rev lines with "" :: l -> List.rev l | _ -> lines) in
		List.iter print lines
	end