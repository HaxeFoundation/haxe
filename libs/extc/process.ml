(** Subprocess management on libuv ({!Luv.Process}), without C stubs of our own.

	Replaces the former C stubs (process_stubs.c) which were unsafe under
	OCaml 5 multi-domain execution (missing [caml_enter_blocking_section],
	direct [fork()] in multi-threaded processes).

	[Unix.create_process] cannot start a process in another directory, and
	the directory of this process is shared by all its threads and domains,
	which is why processes are started by libuv. The pipes are libuv's too,
	read and written with blocking calls.

	When the command is not found, [run] returns a process whose pipes
	immediately return EOF and whose [exit] returns code 127 (matching
	the POSIX shell convention and the old fork+exec behavior where
	fork always succeeded). *)

(* A process runs on a loop that is its own until it has exited. *)
type process = {
	loop : Luv.Loop.t;
	mutable handle : Luv.Process.t option; (* None if not started, or closed *)
	mutable pid : int;
	mutable stdin : Luv.File.t option; (* None once closed *)
	mutable stdout : Luv.File.t option;
	mutable stderr : Luv.File.t option;
	mutable exit_code : int option;
}

exception Spawn_error of string

let luv_ok = function
	| Ok v -> v
	| Error e -> raise (Spawn_error (Luv.Error.strerror e))

let close_file file =
	ignore (Luv.File.Sync.close file)

(* As Unix.create_process on Windows: without a console, the process gets a
   hidden one. *)
let windows_hide () =
	Sys.win32 && match Unix.openfile "CONOUT$" [Unix.O_WRONLY] 0 with
		| fd -> Unix.close fd; false
		| exception Unix.Unix_error _ -> true

(* The program and arguments that run the shell command [cmd], with whether
   Windows must get them as they are. *)
let shell cmd =
	if Sys.win32 then begin
		(* cmd.exe must get the command line as is, not re-quoted as an argv entry *)
		let shell = try Sys.getenv "COMSPEC" with Not_found -> "cmd.exe" in
		shell, [Filename.quote shell; "/c"; cmd], true
	end else
		"/bin/sh", ["/bin/sh"; "-c"; cmd], false

(* Loops are kept for the next process instead of being closed. On Linux,
   closing one ends its io_uring instance, and the kernel then interrupts a
   later blocking system call of this thread: a connect or a read on a socket
   with a timeout fails with EINTR. *)
let free_loops = ref []
let free_loops_mutex = Mutex.create ()

let take_loop () =
	let loop = Mutex.protect free_loops_mutex (fun () ->
		match !free_loops with
		| loop :: l -> free_loops := l; Some loop
		| [] -> None
	) in
	match loop with
	| Some loop -> loop
	| None -> luv_ok (Luv.Loop.init ())

let new_process () =
	{ loop = take_loop (); handle = None; pid = 0; stdin = None; stdout = None; stderr = None; exit_code = None }

(* Closes the handle of [p] and gives its loop back once nothing is left on
   it. To call once per process. *)
let release p =
	Option.iter (fun h -> Luv.Handle.close h ignore) p.handle;
	p.handle <- None;
	ignore (Luv.Loop.run ~loop:p.loop ());
	Mutex.protect free_loops_mutex (fun () -> free_loops := p.loop :: !free_loops)

(* Starts [cmd] with [args], or the shell command [cmd] if [args] is [None],
   as process [p] with the descriptors [fd0], [fd1] and [fd2] of this process
   as its standard ones. *)
let spawn p ?cwd cmd args fd0 fd1 fd2 =
	let prog, argv, verbatim = match args with
		| None -> shell cmd
		| Some args -> cmd, cmd :: Array.to_list args, false
	in
	let on_exit _ ~exit_status ~term_signal =
		(* as Sys.command: 255 for a process ended by a signal *)
		p.exit_code <- Some (if term_signal <> 0 && not Sys.win32 then 255 else Int64.to_int exit_status)
	in
	let redirect = List.mapi (fun fd from_parent_fd -> Luv.Process.inherit_fd ~fd ~from_parent_fd ()) [fd0; fd1; fd2] in
	match
		Luv.Process.spawn ~loop:p.loop ~on_exit ?working_directory:cwd ~redirect
			~windows_verbatim_arguments:verbatim ~windows_hide:(windows_hide ()) prog argv
	with
	| Ok h ->
		p.handle <- Some h;
		p.pid <- Luv.Process.pid h
	| Error e ->
		raise (Spawn_error (Printf.sprintf "%s: %s" prog (Luv.Error.strerror e)))

(* Waits for [p] to exit; its exit code. *)
let wait p =
	while p.exit_code = None && p.handle <> None do
		ignore (Luv.Loop.run ~loop:p.loop ~mode:`ONCE ())
	done;
	if p.handle <> None then release p;
	let c = Option.value p.exit_code ~default:255 in
	p.exit_code <- Some c;
	c

let write_file file s pos len =
	Luv.File.Sync.write file [Luv.Buffer.from_string (String.sub s pos len)]

(** Runs [cmd] with [args], or the shell command [cmd] if [args] is [None], in
	directory [cwd] if given, without changing the directory of this process.
	Its standard descriptors are new pipes. *)
let run ?cwd cmd args =
	let p = new_process () in
	let files = ref [] in
	let pipe () =
		let r, w = luv_ok (Luv.Pipe.pipe ~read_flags:[] ~write_flags:[] ()) in
		files := r :: w :: !files;
		r, w
	in
	match
		let child_stdin, stdin = pipe () in
		let stdout, child_stdout = pipe () in
		let stderr, child_stderr = pipe () in
		(stdin, stdout, stderr), (child_stdin, child_stdout, child_stderr)
	with
	| exception (Spawn_error _ as e) ->
		List.iter close_file !files;
		release p;
		raise e
	| (stdin, stdout, stderr), (child_stdin, child_stdout, child_stderr) ->
		p.stdin <- Some stdin;
		p.stdout <- Some stdout;
		p.stderr <- Some stderr;
		begin try
			spawn p ?cwd cmd args (Luv.File.to_int child_stdin) (Luv.File.to_int child_stdout) (Luv.File.to_int child_stderr)
		with Spawn_error msg ->
			(* Process creation failed (e.g. command not found).
				Match the old fork+exec behavior: the pipes immediately return EOF
				and the exit code is 127. The error message is written to the
				stderr pipe so callers can read it. *)
			let msg = msg ^ "\n" in
			ignore (write_file child_stderr msg 0 (String.length msg));
			release p;
			p.exit_code <- Some 127
		end;
		close_file child_stdin;
		close_file child_stdout;
		close_file child_stderr;
		p

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
		match
			let p = new_process () in
			(try spawn p ~cwd cmd None Luv.Process.stdin Luv.Process.stdout Luv.Process.stderr
			with Spawn_error _ as e -> release p; raise e);
			p
		with
		| p -> ignoring_signals (fun () -> wait p)
		| exception Spawn_error _ -> 127

let read name file buf pos len =
	let fail () = failwith ("process_" ^ name ^ "_read") in
	match file with
	| None -> fail ()
	| Some file ->
		let b = Luv.Buffer.create len in
		match Luv.File.Sync.read file [b] with
		| Error e -> failwith (Luv.Error.strerror e)
		| Ok n ->
			let n = Unsigned.Size_t.to_int n in
			if n = 0 then fail ();
			Luv.Buffer.blit_to_bytes (Luv.Buffer.sub b ~offset:0 ~length:n) (Bytes.unsafe_of_string buf) ~destination_offset:pos;
			n

let read_stdout p buf pos len =
	read "stdout" p.stdout buf pos len

let read_stderr p buf pos len =
	read "stderr" p.stderr buf pos len

let write_stdin p buf pos len =
	match p.stdin with
	| None -> failwith "process_stdin_write"
	| Some file ->
		match write_file file buf pos len with
		| Ok n -> Unsigned.Size_t.to_int n
		| Error e -> failwith (Luv.Error.strerror e)

let close_stdin p =
	Option.iter close_file p.stdin;
	p.stdin <- None

let exit p =
	match p.exit_code with
	| Some c -> c
	| None -> wait p

let pid p =
	p.pid

let close p =
	close_stdin p;
	Option.iter close_file p.stdout;
	p.stdout <- None;
	Option.iter close_file p.stderr;
	p.stderr <- None;
	if p.handle <> None then release p

let kill p =
	if p.exit_code = None then
		Option.iter (fun h -> ignore (Luv.Process.kill h Luv.Signal.sigkill)) p.handle
