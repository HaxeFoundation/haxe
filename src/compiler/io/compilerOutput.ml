open CompilerIo

(** Higher-level compiler output helpers.

    Builds on {!CompilerIo} to provide convenience functions for output
    that needs formatting or buffering beyond simple string writes. *)

(** Collect timer report output and write it to stderr / the connection.
    Writes are wrapped in [try ... with] because in server mode the
    client connection may have been closed by the time we try to send. *)
let send_timer_report io timer_ctx =
	let buf = Buffer.create 4096 in
	Timer.report_times timer_ctx (fun s -> Buffer.add_string buf (s ^ "\n"));
	try (CompilerIo.write_err io) (Buffer.contents buf) with _ -> ()