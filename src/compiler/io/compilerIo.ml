type output_target =
	| Stdio
	| Pipe of (string -> unit)

type t = {
	stdout : out_channel;
	stderr : out_channel;
	stdin : in_channel;
	getch : bool -> int;
		(** Reads a single character from stdin. The [bool] parameter controls echo.
		    In non-server mode, uses [Extc.getch] for native terminal raw-mode input.
		    In server mode, reads from the client's forwarded stdin pipe. Returns -1 on EOF. *)
	flush : unit -> unit;
	close : unit -> unit;
	output : output_target;
}

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

let write_out' output s = match output with
	| Stdio -> print_string s; flush stdout
	| Pipe write -> write ("\x01" ^ String.concat "\x01" (ExtString.String.nsplit s "\n") ^ "\n")

let write_err' output s = match output with
	| Stdio -> prerr_string s
	| Pipe write -> write s

let write_out io s = write_out' io.output s
let write_err io s = write_err' io.output s

let get_stdout io = io.stdout
let get_stderr io = io.stderr
let get_stdin io = io.stdin

let getch io echo = io.getch echo

let flush io = io.flush ()
let close io = io.close ()

let is_server io = match io.output with
	| Stdio -> false
	| Pipe _ -> true

(** Creates the {!Gctx.compilation_io} record for this compilation.

	In server mode ([Pipe]):
	- stdout/stderr are pipe-backed channels with background threads that
		forward writes through [CompilerOutput.write_out]/[write_err] (the socket protocol)
	- stdin comes from the client's forwarded data (or an immediately-closed pipe)
	- [getch] reads from the stdin pipe instead of the terminal
	- [close] flushes and joins all background threads

	In non-server mode ([Stdio]):
	- channels are the process's real stdin/stdout/stderr
	- [getch] uses [Extc.getch] for native terminal raw-mode reading *)
let create_pipe_io output stdin_ch =
	let write_out = write_out' output in
	let write_err = write_err' output in
	let (stdout_ch, stdout_thread) = make_output_pipe write_out in
	let (stderr_ch, stderr_thread) = make_output_pipe write_err in
	let closed = ref false in
	{
		stdout = stdout_ch;
		stderr = stderr_ch;
		stdin = stdin_ch;
		getch = getch_from_channel stdin_ch stdout_ch;
		flush = (fun () ->
			Stdlib.flush stdout_ch;
			Stdlib.flush stderr_ch;
		);
		close = (fun () ->
			if not !closed then begin
				closed := true;
				Stdlib.flush stdout_ch; close_out_noerr stdout_ch; Thread.join stdout_thread;
				Stdlib.flush stderr_ch; close_out_noerr stderr_ch; Thread.join stderr_thread;
				close_in_noerr stdin_ch;
			end
		);
		output;
	}

let create_stdio_io output stdin_ch =
	{
		stdout = Stdlib.stdout;
		stderr = Stdlib.stderr;
		stdin = Stdlib.stdin;
		getch = Extc.getch;
		flush = (fun () -> Stdlib.flush Stdlib.stdout);
		close = (fun () -> ());
		output;
	}