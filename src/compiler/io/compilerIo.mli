(** Compiler IO abstraction.

    This module provides the IO channels and protocol-aware output routing
    for a single compilation request.

    The protocol is determined internally by the create function used:
    {!create_stdio_io} for CLI mode, {!create_pipe_io} for server mode.
    The protocol encoding (how stdout/stderr/errors are multiplexed) is
    fully encapsulated — callers use {!write_out}, {!write_err},
    {!signal_error} without knowing the wire format.

    In server mode, [stdout] and [stderr] are pipe-backed channels with
    background threads that forward writes through the protocol layer.
    In non-server mode, the process's real stdin/stdout/stderr are used. *)

(** Abstract IO handle for a compilation request. *)
type t

(** Write to stdout using the protocol encoding.  In server mode, newlines
    in the string are encoded as [\x01] separators per the v1 protocol.
    In CLI mode, writes directly to [Stdlib.stdout]. *)
val write_out : t -> string -> unit

(** Write to stderr using the protocol encoding.  In server mode, the
    string is sent as-is through the connection's write function (stderr
    lines are plain text in v1).  In CLI mode, writes to [Stdlib.stderr]. *)
val write_err : t -> string -> unit

(** Signal that the current request ended with an error.  In server mode,
    writes the [\x02\n] error sentinel per the v1 protocol.  In CLI
    mode, this is a no-op (error status is communicated via the exit code). *)
val signal_error : t -> unit

(** Whether this IO handle is in server mode (i.e. using a socket protocol). *)
val is_server : t -> bool

(** The [out_channel] that compilation code should use for stdout.
    In server mode this is one end of a pipe; a background thread reads
    the other end and forwards chunks through the protocol layer. *)
val get_stdout : t -> out_channel

(** The [out_channel] for stderr, analogous to {!get_stdout}. *)
val get_stderr : t -> out_channel

(** The [in_channel] for stdin.  In server mode this is one end of a pipe
    fed by the client's forwarded stdin data.  In CLI mode, [Stdlib.stdin]. *)
val get_stdin : t -> in_channel

(** Read a single character from stdin. The [bool] parameter controls echo:
    when [true], the character is echoed to stdout.
    In CLI mode uses [Extc.getch] for native terminal raw-mode input.
    In server mode reads from the client's forwarded stdin pipe.
    Returns [-1] on EOF. *)
val getch : t -> bool -> int

(** Flush the stdout/stderr pipe channels without closing them.
    Call per compilation part to ensure output from each part is
    delivered before the next part starts. *)
val flush : t -> unit

(** Close the IO channels and join background threads.  Call once at
    request level (not per part) to clean up resources.
    In server mode, flushes and closes both output pipes, joins their
    background threads, and closes the stdin channel.
    In CLI mode, this is a no-op. *)
val close : t -> unit

(** Create a pipe-backed IO handle for server mode.

    Uses the v1 socket protocol: [\x01]-separated stdout, verbatim stderr,
    [\x02] error sentinel.

    [stdout] and [stderr] are pipe-backed channels; background threads
    read from the pipes and forward chunks through the protocol encoder.

    @param write The connection's raw write function.
    @param stdin_ch The stdin channel forwarded from the client. *)
val create_pipe_io : (string -> unit) -> in_channel -> t

(** Create a stdio-based IO handle for non-server (CLI) mode.

    Uses the process's real [stdin]/[stdout]/[stderr].  [getch] uses
    [Extc.getch] for native terminal raw-mode reading. *)
val create_stdio_io : unit -> t