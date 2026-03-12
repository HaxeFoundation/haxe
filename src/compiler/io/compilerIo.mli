type output_target =
	| Stdio
	| Pipe of (string -> unit)

type t

val write_out : t -> string -> unit
val write_err : t -> string -> unit

val get_stdout : t -> out_channel
val get_stderr : t -> out_channel
val get_stdin : t -> in_channel

val getch : t -> bool -> int

(** Flush the stdout/stderr channels. Call per compilation part to ensure
    output from each part is delivered before the next part starts. *)
val flush : t -> unit

(** Close the io channels and join background threads. Call once at
    request level to clean up resources. *)
val close : t -> unit

(* TODO: IMO this shouldn't exist *)
val is_server : t -> bool

val create_pipe_io : output_target -> in_channel -> t

val create_stdio_io : output_target -> in_channel -> t