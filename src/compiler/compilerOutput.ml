open Globals

(** Unified compiler output protocol.

    This module defines the types for all user-facing compiler output.
    All output from the compiler should eventually pass through a single
    [output_handler] function that routes messages to the appropriate
    printer (JSON or CLI).

    The design follows patterns from rustc (--error-format=json) and LSP
    diagnostics: a single message type with a kind discriminator, carrying
    structured payload. Printers switch on the kind and format accordingly.

    Current output mechanisms being unified:
    - [json_out] (JSON-RPC responses for display/IDE features)
    - [Communication] (CLI stdio/pipe output)
    - [compiler_message] / [display_messages] (formatted error/warning output)
    - [diagnostics] (per-file diagnostic collection)
    - Timer reporting

    Migration strategy:
    1. Keep [json_out] working alongside this new system.
    2. Incrementally port output sites to use [send_output].
    3. Eventually remove [json_out] once all sites are ported. *)

(** The kind of output being produced. Each variant represents a distinct
    category of compiler output that printers must handle. *)
type output_kind =
	(** Compiler messages (errors, warnings, information, hints) produced
	    during compilation. These are the primary user-facing messages.
	    Corresponds to the current [display_messages] / [flush_context] paths. *)
	| OMessages of compiler_message list
	(** Diagnostic results collected across files, used for IDE integration.
	    Carries per-file diagnostic information with codes and severity.
	    Corresponds to the current [DiagnosticsPrinter.json_of_diagnostics] path. *)
	| ODiagnostics of diagnostic list
	(** A successful result as JSON. Used for display/IDE features
	    like hover, completion, signature help, etc.
	    Corresponds to the current [json_out.send_result] path. *)
	| OResult of Json.t
	(** An error result as a list of JSON error objects.
	    Corresponds to the current [json_out.send_error] path. *)
	| OError of Json.t list
	(** Performance timer data as a pre-formatted string.
	    Corresponds to the current [Timer.report_times] output. *)
	| OTimerData of string

(** An output handler processes compiler output of any kind.
    Printers implement this type by switching on [output_kind]
    to format and deliver the output appropriately.

    Two printer implementations are planned:
    - CLI printer: formats messages using pretty/classic/indent formatters
      and writes to stdout/stderr (replaces [Communication.create_stdio])
    - JSON printer: wraps output in JSON-RPC envelopes and sends via
      the IO channel (replaces [json_out] and [Communication.create_pipe]) *)
type output_handler = output_kind -> unit

(** An output handler that discards all output.
    Used as the default before a proper handler is configured. *)
let noop_handler : output_handler = fun _ -> ()

(** Convenience: send compiler messages through a handler. *)
let send_messages handler messages =
	handler (OMessages messages)

(** Convenience: send a display result through a handler. *)
let send_result handler json =
	handler (OResult json)

(** Convenience: send a display error through a handler. *)
let send_error handler errors =
	handler (OError errors)

