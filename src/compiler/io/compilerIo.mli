(** Compiler IO abstraction.

    This module provides the IO channels and output routing for a single
    compilation request. It bridges the gap between OCaml's standard channel
    API (used by code generators, [Printf], etc.) and the socket protocol
    used in server mode.

    In server mode, [stdout] and [stderr] are pipe-backed channels with
    background threads that forward writes through the socket protocol.
    In non-server mode, the process's real stdin/stdout/stderr are used. *)

(** The output target determines where compiler output goes.

    - [Stdio]: direct writes to the process's stdout/stderr.
    - [Pipe write]: server mode — writes go through the connection's
      write function, which encodes and sends them to the client. *)
type output_target =
	| Stdio
	| Pipe of (string -> unit)

(** Abstract IO handle for a compilation request. *)
type t

(** Write to stdout using the socket protocol encoding in server mode,
    or directly to stdout in CLI mode.  In [Pipe] mode, newlines in the
    string are encoded as [\x01] separators per the legacy protocol. *)
val write_out : t -> string -> unit

(** Write to stderr.  In [Pipe] mode, the string is sent as-is through
    the connection's write function (stderr lines are plain text in the
    legacy protocol).  In [Stdio] mode, writes to [Stdlib.stderr]. *)
val write_err : t -> string -> unit

(** The [out_channel] that compilation code should use for stdout.
    In server mode this is one end of a pipe; a background thread reads
    the other end and forwards chunks through [write_out]. *)
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

(** Whether we're in server mode (i.e. the output target is [Pipe]). *)
val is_server : t -> bool

(** Create a pipe-backed IO handle for server mode.

    [stdout] and [stderr] are pipe-backed channels; background threads
    read from the pipes and forward chunks through the socket protocol
    via [write_out] / [write_err].

    @param output The output target (should be [Pipe _]).
    @param stdin_ch The stdin channel forwarded from the client. *)
val create_pipe_io : output_target -> in_channel -> t

(** Create a stdio-based IO handle for non-server (CLI) mode.

    Uses the process's real [stdin]/[stdout]/[stderr].  [getch] uses
    [Extc.getch] for native terminal raw-mode reading.

    @param output The output target (should be [Stdio]).
    @param stdin_ch Unused in practice ([Stdlib.stdin] is used directly). *)
val create_stdio_io : output_target -> in_channel -> t