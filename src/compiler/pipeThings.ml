(** Handles IO piping between the compilation server and its clients.

    In server mode, the compiler runs as a long-lived process.  Client
    requests arrive over a socket, and we need to redirect the compilation's
    stdin/stdout/stderr through the socket rather than using the server
    process's own file descriptors.

    Protocol encoding (how stdout/stderr/errors are multiplexed) is handled
    by {!CompilerIo} via its opaque {!CompilerIo.protocol} type.  This module
    provides the client-side socket communication ([poll], [ssend]) and the
    server-side subprocess runner ({!run_command}).

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

(** Runs a shell command in server mode, forwarding stdin from the client
	and capturing stdout/stderr through the socket protocol.
	Uses {!Process.run} to create the child process so we can connect
	the child's stdin to the client's forwarded data and properly signal
	EOF when the client closes its end. *)
let run_command io cmd =
	let write_out = CompilerIo.write_out io in
	let write_err = CompilerIo.write_err io in
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
	let tin = match Some (CompilerIo.get_stdin io) with
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
	let tout = Thread.create (fun() -> read_content pout bout write_out) () in
	let terr = Thread.create (fun() -> read_content perr berr write_err) () in
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
	let stop_stdin = ref false in
	let stdin_fd = Unix.descr_of_in_channel Stdlib.stdin in
	let stdin_thread = Thread.create (fun () ->
		(try while not !stop_stdin do
			let readable, _, _ = Unix.select [stdin_fd] [] [] 0.05 in
			if readable <> [] then begin
				let n = Unix.read stdin_fd stdin_buf 0 1024 in
				if n = 0 then begin
					(try Unix.shutdown sock Unix.SHUTDOWN_SEND with _ -> ());
					raise Exit
				end;
				ssend sock (Bytes.sub stdin_buf 0 n)
			end
		done with _ -> ())
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
	stop_stdin := true;
	Thread.join stdin_thread;
	(* Flush any remaining partial line after the server closes the connection *)
	let s = Buffer.contents response_buf in
	if s <> "" then begin
		let lines = ExtString.String.nsplit s "\n" in
		let lines = (match List.rev lines with "" :: l -> List.rev l | _ -> lines) in
		List.iter print lines
	end