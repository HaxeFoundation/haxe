(** Compiler IO abstraction.

    This module provides the IO channels and protocol-aware output routing
    for a single compilation request.

    The protocol encoding is fully encapsulated via generic callbacks. *)

(** Abstract IO handle for a compilation request. *)
type t

val write_out : t -> string -> unit
val write_err : t -> string -> unit
val write_result : t -> string -> unit
val signal_error : t -> unit
val get_stdout : t -> out_channel
val get_stderr : t -> out_channel
val get_stdin : t -> in_channel
val getch : t -> bool -> int
val flush : t -> unit
val close : t -> unit

(** Create a pipe-backed IO handle with explicit callbacks. *)
val create :
	write_out:(string -> unit) ->
	write_err:(string -> unit) ->
	write_result:(string -> unit) ->
	signal_error:(unit -> unit) ->
	in_channel ->
	t

(** Create a stdio-based IO handle for non-server (CLI) mode. *)
val create_stdio_io : unit -> t