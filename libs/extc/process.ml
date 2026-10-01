(** Pure OCaml implementation of subprocess management.

	Replaces the former C stubs (process_stubs.c) which were unsafe under
	OCaml 5 multi-domain execution (missing [caml_enter_blocking_section],
	direct [fork()] in multi-threaded processes).

	Uses [Unix.create_process] which is domain-safe and handles
	[posix_spawn] on modern systems.

	When the command is not found, [run] returns a process whose pipes
	immediately return EOF and whose [exit] returns code 127 (matching
	the POSIX shell convention and the old fork+exec behavior where
	fork always succeeded). *)

type process = {
	pid : int;
	stdin_fd : Unix.file_descr;
	stdout_fd : Unix.file_descr;
	stderr_fd : Unix.file_descr;
	mutable exit_code : int option;
}

(** Returns a readable file_descr that immediately yields EOF. *)
let make_eof_fd () =
	let (r, w) = Unix.pipe ~cloexec:true () in
	Unix.close w;
	r

(** Returns a writable file_descr where writes fail with EPIPE. *)
let make_null_fd () =
	let (r, w) = Unix.pipe ~cloexec:true () in
	Unix.close r;
	w

let unix_error_msg err fn arg =
	Printf.sprintf "%s(%s): %s" fn arg (Unix.error_message err)

let run cmd args =
	let (child_stdin_r, child_stdin_w) = Unix.pipe ~cloexec:true () in
	let (child_stdout_r, child_stdout_w) = Unix.pipe ~cloexec:true () in
	let (child_stderr_r, child_stderr_w) = Unix.pipe ~cloexec:true () in
	let shell, argv = match args with
		| None ->
			if Sys.win32 then
				let comspec = try Sys.getenv "COMSPEC" with Not_found -> "cmd.exe" in
				comspec, [|comspec; "/C"; cmd|]
			else
				"/bin/sh", [|"/bin/sh"; "-c"; cmd|]
		| Some a ->
			cmd, Array.append [|cmd|] a
	in
	match
		try Ok (Unix.create_process shell argv child_stdin_r child_stdout_w child_stderr_w)
		with Unix.Unix_error _ as e -> Error e
	with
	| Ok pid ->
		Unix.close child_stdin_r;
		Unix.close child_stdout_w;
		Unix.close child_stderr_w;
		{ pid; stdin_fd = child_stdin_w; stdout_fd = child_stdout_r; stderr_fd = child_stderr_r; exit_code = None }
	| Error err  ->
		Unix.close child_stdin_r;
		Unix.close child_stdin_w;
		Unix.close child_stdout_r;
		Unix.close child_stdout_w;
		Unix.close child_stderr_r;
		let stderr_r = match err with
			| (Unix.Unix_error (err, fn, arg)) ->
				(* Process creation failed (e.g. command not found).
					Match the old fork+exec behavior: return a process whose pipes
					immediately return EOF and whose exit code is 127.
					Write the error message to the stderr pipe so callers can read it. *)
				let errmsg = unix_error_msg err fn arg ^ "\n" in
				let (stderr_r, stderr_w) = Unix.pipe ~cloexec:true () in
				begin try
					ignore (Unix.write_substring stderr_w errmsg 0 (String.length errmsg))
				with Unix.Unix_error _ ->
					()
				end;
				Unix.close stderr_w;
				stderr_r
			| _ ->
				Unix.close child_stderr_w;
				make_eof_fd ()
		in
		{ pid = 0; stdin_fd = make_null_fd (); stdout_fd = make_eof_fd (); stderr_fd = stderr_r; exit_code = Some 127 }

let read_stdout p buf pos len =
	let n = try
		Unix.read p.stdout_fd (Bytes.unsafe_of_string buf) pos len
	with Unix.Unix_error (err, fn, arg) ->
		failwith (unix_error_msg err fn arg)
	in
	if n = 0 then failwith "process_stdout_read";
	n

let read_stderr p buf pos len =
	let n = try
		Unix.read p.stderr_fd (Bytes.unsafe_of_string buf) pos len
	with Unix.Unix_error (err, fn, arg) ->
		failwith (unix_error_msg err fn arg)
	in
	if n = 0 then failwith "process_stderr_read";
	n

let write_stdin p buf pos len =
	try Unix.write_substring p.stdin_fd buf pos len
	with Unix.Unix_error (err, fn, arg) -> failwith (unix_error_msg err fn arg)

let close_stdin p =
	try Unix.close p.stdin_fd
	with Unix.Unix_error (err, fn, arg) -> failwith (unix_error_msg err fn arg)

let exit p =
	match p.exit_code with
	| Some c -> c
	| None ->
		let _, status = Unix.waitpid [] p.pid in
		let c = match status with
			| Unix.WEXITED c -> c
			| Unix.WSIGNALED c -> c
			| Unix.WSTOPPED c -> c
		in
		p.exit_code <- Some c;
		c

let pid p = p.pid

let close p =
	(try Unix.close p.stdout_fd with Unix.Unix_error _ -> ());
	(try Unix.close p.stderr_fd with Unix.Unix_error _ -> ());
	(try Unix.close p.stdin_fd with Unix.Unix_error _ -> ())

let kill p =
	if p.exit_code = None && p.pid > 0 then
		(try Unix.kill p.pid Sys.sigkill with Unix.Unix_error _ -> ())
