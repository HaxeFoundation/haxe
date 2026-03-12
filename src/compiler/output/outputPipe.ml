open CompilerOutput

(** Pipe/server output handler for --connect mode.

    Routes output through the server communication channel:
    - [OTimerData]: written to the error stream (same as stderr in --connect protocol)
    - Other kinds: not yet handled (will be added during migration)

    All writes are wrapped in [try ... with] because the client connection
    may have been closed. *)

let create ~(write_err : string -> unit) : output_handler = fun kind ->
	match kind with
	| OTimerData s -> (try write_err s with _ -> ())
	| _ -> ()
