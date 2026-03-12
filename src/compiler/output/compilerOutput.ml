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
type output_target =
	| Stdio
	| Pipe of (string -> unit)

(** Write a string to stdout (CLI) or through the pipe protocol (server).
    In server mode, lines are separated by [\x01] markers. *)
let write_out target s = match target with
	| Stdio -> print_string s; flush stdout
	| Pipe write -> write ("\x01" ^ String.concat "\x01" (ExtString.String.nsplit s "\n") ^ "\n")

(** Write a string to stderr (CLI) or through the connection (server). *)
let write_err target s = match target with
	| Stdio -> prerr_string s
	| Pipe write -> write s

(** Whether we're in server mode. *)
let is_server target = match target with
	| Stdio -> false
	| Pipe _ -> true

(** Collect timer report output and write it to stderr / the connection.
    Writes are wrapped in [try ... with] because in server mode the
    client connection may have been closed. *)
let send_timer_report target timer_ctx =
	let buf = Buffer.create 4096 in
	Timer.report_times timer_ctx (fun s -> Buffer.add_string buf (s ^ "\n"));
	try write_err target (Buffer.contents buf) with _ -> ()