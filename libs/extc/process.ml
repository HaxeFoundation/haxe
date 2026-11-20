(** Pure OCaml implementation of subprocess management.

	Replaces the former C stubs (process_stubs.c) which were unsafe under
	OCaml 5 multi-domain execution (missing [caml_enter_blocking_section],
	direct [fork()] in multi-threaded processes).

	Uses [Unix.create_process] which is domain-safe and handles
	[posix_spawn] on modern systems.

	The one exception is starting a process in another directory on Windows
	(see [spawn_in]): [Unix.create_process] cannot do it, and there is no
	shell that could reliably do it for us as /bin/sh does elsewhere.

	When the command is not found, [run] returns a process whose pipes
	immediately return EOF and whose [exit] returns code 127 (matching
	the POSIX shell convention and the old fork+exec behavior where
	fork always succeeded). *)

type process_handle =
	| Pid of int (* Unix.create_process, reaped with waitpid *)
	| Popen of int * (in_channel * out_channel * in_channel) (* Unix.open_process_full, reaped with close_process_full *)

type process = {
	handle : process_handle;
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

(* Windows only (spawn_stubs.c): as Unix.create_process, starts [prog] with
   the command line [cmdline] and the given standard handles, but in directory
   [cwd]; returns its process handle. *)
external spawn_in : string -> string -> string -> Unix.file_descr -> Unix.file_descr -> Unix.file_descr -> int
	= "process_spawn_in_bytecode" "process_spawn_in_native"

(* The /bin/sh command that runs the shell command [cmd] in directory [cwd]. *)
let sh_command_in ~cwd cmd =
	Printf.sprintf "cd %s || exit\n%s" (Filename.quote cwd) cmd

(* The /bin/sh arguments that run [prog] with [args] in directory [cwd]. *)
let sh_args_in ~cwd prog args =
	Array.append [|"-c"; "cd \"$1\" && shift && exec \"$@\""; "sh"; cwd; prog|] args

(* The command line of [argv], quoted as by Unix.create_process on Windows. *)
let windows_command_line argv =
	let quote s = if s = "" || String.exists (fun c -> c = ' ' || c = '\t' || c = '"') s then Filename.quote s else s in
	String.concat " " (List.map quote (Array.to_list argv))

(* The Windows program and command line that run the shell command [cmd]. *)
let windows_shell cmd =
	let shell = try Sys.getenv "COMSPEC" with Not_found -> "cmd.exe" in
	shell, windows_command_line [|shell|] ^ " /c " ^ cmd

(* On Windows, a program path relative to [cwd] is found there, as it would be
   by a process started from [cwd]. *)
let windows_program_in ~cwd prog =
	let has_dir = String.contains prog '/' || String.contains prog '\\' in
	let local = Filename.concat cwd prog in
	if has_dir then
		(if Filename.is_relative prog then local else prog)
	else if Sys.file_exists (local ^ ".exe") then local ^ ".exe"
	else if Filename.extension prog <> "" && Sys.file_exists local then local
	else prog

let windows_spawn ~cwd prog cmdline fd0 fd1 fd2 =
	spawn_in cwd (windows_program_in ~cwd prog) cmdline fd0 fd1 fd2

(* A process whose standard descriptors are new pipes, started by [create]. *)
let with_pipes create =
	let (child_stdin_r, child_stdin_w) = Unix.pipe ~cloexec:true () in
	let (child_stdout_r, child_stdout_w) = Unix.pipe ~cloexec:true () in
	let (child_stderr_r, child_stderr_w) = Unix.pipe ~cloexec:true () in
	match
		try Ok (create child_stdin_r child_stdout_w child_stderr_w)
		with Unix.Unix_error _ as e -> Error e
	with
	| Ok pid ->
		Unix.close child_stdin_r;
		Unix.close child_stdout_w;
		Unix.close child_stderr_w;
		{ handle = Pid pid; stdin_fd = child_stdin_w; stdout_fd = child_stdout_r; stderr_fd = child_stderr_r; exit_code = None }
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
		{ handle = Pid 0; stdin_fd = make_null_fd (); stdout_fd = make_eof_fd (); stderr_fd = stderr_r; exit_code = Some 127 }

let run_in_current_dir cmd args =
	match args with
	| None when Sys.win32 ->
		(* cmd.exe must get the command line as is, not re-quoted as an argv entry.
		   The pipes are duplicated so they stay readable after close_process_full. *)
		let (pout, pin, perr) as popen = Unix.open_process_full cmd (Unix.environment ()) in
		let dup fd = Unix.dup ~cloexec:true fd in
		let stdin_fd = dup (Unix.descr_of_out_channel pin) in
		close_out pin; (* so that close_stdin signals EOF *)
		{
			handle = Popen (Unix.process_full_pid popen, popen);
			stdin_fd;
			stdout_fd = dup (Unix.descr_of_in_channel pout);
			stderr_fd = dup (Unix.descr_of_in_channel perr);
			exit_code = None;
		}
	| _ ->
		let prog, argv = match args with
			| None -> "/bin/sh", [|"/bin/sh"; "-c"; cmd|]
			| Some a -> cmd, Array.append [|cmd|] a
		in
		with_pipes (Unix.create_process prog argv)

(** Runs [cmd] with [args], or the shell command [cmd] if [args] is [None], in
	directory [cwd] if given, without changing the directory of this process. *)
let run ?cwd cmd args =
	match cwd, args with
	| None, _ ->
		run_in_current_dir cmd args
	| Some cwd, None when not Sys.win32 ->
		run_in_current_dir (sh_command_in ~cwd cmd) None
	| Some cwd, Some args when not Sys.win32 ->
		run_in_current_dir "/bin/sh" (Some (sh_args_in ~cwd cmd args))
	| Some cwd, None ->
		let prog, cmdline = windows_shell cmd in
		with_pipes (windows_spawn ~cwd prog cmdline)
	| Some cwd, Some args ->
		with_pipes (windows_spawn ~cwd cmd (windows_command_line (Array.append [|cmd|] args)))

let rec waitpid_no_eintr pid =
	try snd (Unix.waitpid [] pid)
	with Unix.Unix_error (Unix.EINTR,_,_) -> waitpid_no_eintr pid

(** As Sys.command, the shell command [cmd] with the standard descriptors of
	this process, in directory [cwd] if given. *)
let command ?cwd cmd =
	match cwd with
	| None ->
		Sys.command cmd
	| Some cwd when not Sys.win32 ->
		Sys.command (sh_command_in ~cwd cmd)
	| Some cwd ->
		let prog, cmdline = windows_shell cmd in
		match windows_spawn ~cwd prog cmdline Unix.stdin Unix.stdout Unix.stderr with
		| pid ->
			(match waitpid_no_eintr pid with
			| Unix.WEXITED c -> c
			| Unix.WSIGNALED _ | Unix.WSTOPPED _ -> 255)
		| exception Unix.Unix_error _ -> 127

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
		let status = match p.handle with
			| Pid pid -> snd (Unix.waitpid [] pid)
			| Popen (_, popen) -> Unix.close_process_full popen
		in
		let c = match status with
			| Unix.WEXITED c -> c
			| Unix.WSIGNALED c -> c
			| Unix.WSTOPPED c -> c
		in
		p.exit_code <- Some c;
		c

let pid p = match p.handle with
	| Pid pid | Popen (pid, _) -> pid

let close p =
	(try Unix.close p.stdout_fd with Unix.Unix_error _ -> ());
	(try Unix.close p.stderr_fd with Unix.Unix_error _ -> ());
	(try Unix.close p.stdin_fd with Unix.Unix_error _ -> ());
	match p.handle with
	| Pid _ -> ()
	| Popen (_, (pout, _, perr)) ->
		(* not reaped by exit: release the original pipes too *)
		if p.exit_code = None then begin
			close_in_noerr pout;
			close_in_noerr perr
		end

let kill p =
	if p.exit_code = None && pid p > 0 then
		(try Unix.kill (pid p) Sys.sigkill with Unix.Unix_error _ -> ())
