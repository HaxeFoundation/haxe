(** Pure OCaml implementation of subprocess management.

	Replaces the former C stubs (process_stubs.c) which were unsafe under
	OCaml 5 multi-domain execution (missing [caml_enter_blocking_section],
	direct [fork()] in multi-threaded processes).

	Uses [Unix.create_process] which is domain-safe and handles
	[posix_spawn] on modern systems.

	[Unix.create_process] cannot start a process in another directory, and
	the directory of this process is shared by all its threads and domains,
	so such a process is started by libuv ({!Luv.Process.spawn}) instead.

	When the command is not found, [run] returns a process whose pipes
	immediately return EOF and whose [exit] returns code 127 (matching
	the POSIX shell convention and the old fork+exec behavior where
	fork always succeeded). *)

(* A process started by libuv on a loop of its own, which runs until the
   process has exited. *)
type luv_process = {
	loop : Luv.Loop.t;
	mutable luv_pid : int;
	mutable luv_handle : Luv.Process.t option; (* None once closed *)
	mutable luv_exit : int option;
}

type process_handle =
	| Pid of int (* Unix.create_process, reaped with waitpid *)
	| Luv of luv_process
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

exception Spawn_error of string

(* The descriptor number of [fd] for libuv. It is a copy, to close with
   Luv.File.Sync.close: on Windows it owns its handle. *)
let luv_file fd =
	let fd = Unix.dup ~cloexec:true fd in
	let file = match Luv_unix.Os_fd.Fd.from_unix fd with
		| Ok os_fd -> Luv.File.open_osfhandle os_fd
		| Error _ as e -> e
	in
	match file with
	| Ok file -> file
	| Error e -> Unix.close fd; raise (Spawn_error (Luv.Error.strerror e))

(* As Unix.create_process on Windows: without a console, the process gets a
   hidden one. *)
let windows_hide () =
	Sys.win32 && match Unix.openfile "CONOUT$" [Unix.O_WRONLY] 0 with
		| fd -> Unix.close fd; false
		| exception Unix.Unix_error _ -> true

(* Closes the handle of [lp], then its loop once nothing is left on it. *)
let luv_release lp =
	Option.iter (fun h -> Luv.Handle.close h ignore) lp.luv_handle;
	lp.luv_handle <- None;
	ignore (Luv.Loop.run ~loop:lp.loop ());
	ignore (Luv.Loop.close lp.loop)

(* Starts [prog] with arguments [argv] in directory [cwd], with the given
   standard descriptors. With [verbatim], Windows gets [argv] as is, separated
   by spaces. *)
let luv_spawn ~cwd ?(verbatim=false) prog argv fd0 fd1 fd2 =
	let loop = match Luv.Loop.init () with
		| Ok loop -> loop
		| Error e -> raise (Spawn_error (Luv.Error.strerror e))
	in
	let lp = { loop; luv_pid = 0; luv_handle = None; luv_exit = None } in
	let files = ref [] in
	let redirect n fd =
		let file = luv_file fd in
		files := file :: !files;
		Luv.Process.inherit_fd ~fd:n ~from_parent_fd:(Luv.File.to_int file) ()
	in
	let on_exit _ ~exit_status ~term_signal =
		(* as Sys.command: 255 for a process ended by a signal *)
		lp.luv_exit <- Some (if term_signal <> 0 && not Sys.win32 then 255 else Int64.to_int exit_status)
	in
	let result = Fun.protect ~finally:(fun () -> List.iter (fun f -> ignore (Luv.File.Sync.close f)) !files) (fun () ->
		let redirect = [redirect 0 fd0; redirect 1 fd1; redirect 2 fd2] in
		Luv.Process.spawn ~loop ~on_exit ~working_directory:cwd ~redirect
			~windows_verbatim_arguments:verbatim ~windows_hide:(windows_hide ()) prog argv
	) in
	match result with
	| Ok h ->
		lp.luv_handle <- Some h;
		lp.luv_pid <- Luv.Process.pid h;
		lp
	| Error e ->
		luv_release lp;
		raise (Spawn_error (Printf.sprintf "%s: %s" prog (Luv.Error.strerror e)))

(* The exit code of [lp], once it has exited. *)
let luv_wait lp =
	while lp.luv_exit = None && lp.luv_handle <> None do
		ignore (Luv.Loop.run ~loop:lp.loop ~mode:`ONCE ())
	done;
	luv_release lp;
	Option.value lp.luv_exit ~default:255

(* The program and arguments that run the shell command [cmd], with whether
   Windows must get them as they are. *)
let shell cmd =
	if Sys.win32 then begin
		(* cmd.exe must get the command line as is, not re-quoted as an argv entry *)
		let shell = try Sys.getenv "COMSPEC" with Not_found -> "cmd.exe" in
		shell, [Filename.quote shell; "/c"; cmd], true
	end else
		"/bin/sh", ["/bin/sh"; "-c"; cmd], false

(* The shell command [cmd] started in directory [cwd]. *)
let luv_spawn_shell ~cwd cmd fd0 fd1 fd2 =
	let prog, argv, verbatim = shell cmd in
	luv_spawn ~cwd ~verbatim prog argv fd0 fd1 fd2

(* A process whose standard descriptors are new pipes, started by [create]. *)
let with_pipes create =
	let (child_stdin_r, child_stdin_w) = Unix.pipe ~cloexec:true () in
	let (child_stdout_r, child_stdout_w) = Unix.pipe ~cloexec:true () in
	let (child_stderr_r, child_stderr_w) = Unix.pipe ~cloexec:true () in
	match
		try Ok (create child_stdin_r child_stdout_w child_stderr_w)
		with Unix.Unix_error _ | Spawn_error _ as e -> Error e
	with
	| Ok handle ->
		Unix.close child_stdin_r;
		Unix.close child_stdout_w;
		Unix.close child_stderr_w;
		{ handle; stdin_fd = child_stdin_w; stdout_fd = child_stdout_r; stderr_fd = child_stderr_r; exit_code = None }
	| Error err  ->
		Unix.close child_stdin_r;
		Unix.close child_stdin_w;
		Unix.close child_stdout_r;
		Unix.close child_stdout_w;
		Unix.close child_stderr_r;
		let errmsg = match err with
			| Unix.Unix_error (err, fn, arg) -> Some (unix_error_msg err fn arg)
			| Spawn_error msg -> Some msg
			| _ -> None
		in
		let stderr_r = match errmsg with
			| Some errmsg ->
				(* Process creation failed (e.g. command not found).
					Match the old fork+exec behavior: return a process whose pipes
					immediately return EOF and whose exit code is 127.
					Write the error message to the stderr pipe so callers can read it. *)
				let errmsg = errmsg ^ "\n" in
				let (stderr_r, stderr_w) = Unix.pipe ~cloexec:true () in
				begin try
					ignore (Unix.write_substring stderr_w errmsg 0 (String.length errmsg))
				with Unix.Unix_error _ ->
					()
				end;
				Unix.close stderr_w;
				stderr_r
			| None ->
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
		with_pipes (fun fd0 fd1 fd2 -> Pid (Unix.create_process prog argv fd0 fd1 fd2))

(** Runs [cmd] with [args], or the shell command [cmd] if [args] is [None], in
	directory [cwd] if given, without changing the directory of this process. *)
let run ?cwd cmd args =
	match cwd, args with
	| None, _ ->
		run_in_current_dir cmd args
	| Some cwd, None ->
		with_pipes (fun fd0 fd1 fd2 -> Luv (luv_spawn_shell ~cwd cmd fd0 fd1 fd2))
	| Some cwd, Some args ->
		with_pipes (fun fd0 fd1 fd2 -> Luv (luv_spawn ~cwd cmd (cmd :: Array.to_list args) fd0 fd1 fd2))

(* As system(), the interrupt and quit signals are ignored while waiting for
   the child: they go to the child from the terminal. *)
let ignoring_signals f =
	if Sys.win32 then
		f ()
	else begin
		let old_int = Sys.signal Sys.sigint Sys.Signal_ignore in
		let old_quit = Sys.signal Sys.sigquit Sys.Signal_ignore in
		Fun.protect ~finally:(fun () ->
			Sys.set_signal Sys.sigint old_int;
			Sys.set_signal Sys.sigquit old_quit
		) f
	end

(** As Sys.command, the shell command [cmd] with the standard descriptors of
	this process, in directory [cwd] if given. *)
let command ?cwd cmd =
	match cwd with
	| None ->
		Sys.command cmd
	| Some cwd ->
		match luv_spawn_shell ~cwd cmd Unix.stdin Unix.stdout Unix.stderr with
		| lp -> ignoring_signals (fun () -> luv_wait lp)
		| exception Spawn_error _ -> 127

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
		let of_status = function
			| Unix.WEXITED c -> c
			| Unix.WSIGNALED c -> c
			| Unix.WSTOPPED c -> c
		in
		let c = match p.handle with
			| Pid pid -> of_status (snd (Unix.waitpid [] pid))
			| Luv lp -> luv_wait lp
			| Popen (_, popen) -> of_status (Unix.close_process_full popen)
		in
		p.exit_code <- Some c;
		c

let pid p = match p.handle with
	| Pid pid | Popen (pid, _) -> pid
	| Luv lp -> lp.luv_pid

let close p =
	(try Unix.close p.stdout_fd with Unix.Unix_error _ -> ());
	(try Unix.close p.stderr_fd with Unix.Unix_error _ -> ());
	(try Unix.close p.stdin_fd with Unix.Unix_error _ -> ());
	match p.handle with
	| Pid _ -> ()
	| Luv lp -> if lp.luv_handle <> None then luv_release lp
	| Popen (_, (pout, _, perr)) ->
		(* not reaped by exit: release the original pipes too *)
		if p.exit_code = None then begin
			close_in_noerr pout;
			close_in_noerr perr
		end

let kill p =
	if p.exit_code = None && pid p > 0 then match p.handle with
		| Luv lp -> Option.iter (fun h -> ignore (Luv.Process.kill h Luv.Signal.sigkill)) lp.luv_handle
		| Pid _ | Popen _ -> (try Unix.kill (pid p) Sys.sigkill with Unix.Unix_error _ -> ())
