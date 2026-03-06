(** Pure OCaml implementation of subprocess management.

    Replaces the former C stubs (process_stubs.c) which were unsafe under
    OCaml 5 multi-domain execution (missing [caml_enter_blocking_section],
    direct [fork()] in multi-threaded processes).

    Uses [Unix.create_process] which is domain-safe and handles
    [posix_spawn] on modern systems. *)

type process = {
  pid : int;
  stdin_fd : Unix.file_descr;
  stdout_fd : Unix.file_descr;
  stderr_fd : Unix.file_descr;
}

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
  let pid =
    try Unix.create_process shell argv child_stdin_r child_stdout_w child_stderr_w
    with e ->
      Unix.close child_stdin_r;
      Unix.close child_stdin_w;
      Unix.close child_stdout_r;
      Unix.close child_stdout_w;
      Unix.close child_stderr_r;
      Unix.close child_stderr_w;
      raise e
  in
  Unix.close child_stdin_r;
  Unix.close child_stdout_w;
  Unix.close child_stderr_w;
  { pid; stdin_fd = child_stdin_w; stdout_fd = child_stdout_r; stderr_fd = child_stderr_r }

let read_stdout p buf pos len =
  let n = Unix.read p.stdout_fd (Bytes.unsafe_of_string buf) pos len in
  if n = 0 then failwith "process_stdout_read";
  n

let read_stderr p buf pos len =
  let n = Unix.read p.stderr_fd (Bytes.unsafe_of_string buf) pos len in
  if n = 0 then failwith "process_stderr_read";
  n

let write_stdin p buf pos len =
  Unix.write_substring p.stdin_fd buf pos len

let close_stdin p =
  Unix.close p.stdin_fd

let exit p =
  let _, status = Unix.waitpid [] p.pid in
  match status with
  | Unix.WEXITED c -> c
  | Unix.WSIGNALED c -> c
  | Unix.WSTOPPED c -> c

let pid p = p.pid

let close p =
  (try Unix.close p.stdout_fd with Unix.Unix_error _ -> ());
  (try Unix.close p.stderr_fd with Unix.Unix_error _ -> ());
  (try Unix.close p.stdin_fd with Unix.Unix_error _ -> ())

let kill p =
  (try Unix.kill p.pid Sys.sigkill with Unix.Unix_error _ -> ())

