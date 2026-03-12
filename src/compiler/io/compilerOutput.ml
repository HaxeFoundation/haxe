open CompilerIo

(** Unified compiler output.

    This module defines the output target type and API functions for all
    user-facing compiler output. The [output_target] is a data-only variant
    that lives in [request_scope] and determines how output is delivered.

    API functions (like [write_err], [send_timer_report]) pattern-match on
    the target to route output appropriately. This separates the "what kind
    of output are we producing" from "where does it go."

    Currently handles:
    - Timer reporting
    - stdout/stderr routing (via [write_out] / [write_err])

    Future migration:
    - Compiler messages (errors, warnings, hints)
    - Diagnostics (per-file IDE diagnostics)
    - JSON-RPC display results
    - Eventually replaces [Communication] and [json_out] entirely *)

(** The output target determines where compiler output goes.
    This is a data-only type: the API functions below handle formatting
    and delivery based on the variant.

    - [Stdio]: direct writes to the process's stdout/stderr
    - [Pipe write]: server mode — writes go through the connection's
      write function (which handles the socket protocol) *)

(** Whether we're in server mode. *)

(** Collect timer report output and write it to stderr / the connection.
    Writes are wrapped in [try ... with] because in server mode the
    client connection may have been closed. *)
let send_timer_report io timer_ctx =
	let buf = Buffer.create 4096 in
	Timer.report_times timer_ctx (fun s -> Buffer.add_string buf (s ^ "\n"));
	try (CompilerIo.write_err io) (Buffer.contents buf) with _ -> ()