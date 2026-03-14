(** Unified compiler message API.

    This module provides a single entry point for adding compiler messages
    (errors, warnings, info) to the compilation context. It replaces the
    scattered message-adding logic that was spread across [compiler.ml],
    [displayProcessing.ml], and [common.ml].

    Messages are stored in a single buffer ([com.part_scope.messages]) and
    the output layer determines how to present them (formatted text for CLI,
    JSON diagnostics for IDE, etc.).

    Cache-bound messages are stored on module extras via
    [m_cache_bound_objects] so they survive across server compilations. *)

open Globals
open Common
open Type

(** Add a compiler message to the message buffer.

    This is the primary entry point for recording any compiler output
    (errors, warnings, info messages). In diagnostics mode, messages go
    to the diagnostics buffer; otherwise they go to the regular message
    buffer.

    Sets [com.has_error] when severity is [Error]. *)
let add_message ?(depth = 0) ?(from_macro = false) ?(code = None) com msg p kind sev =
	if sev = MessageSeverity.Error then com.has_error <- true;
	let cm = make_compiler_message ~from_macro ~code msg p depth kind sev in
	if is_diagnostics com then
		com.part_scope.diagnostics_messages <- cm :: com.part_scope.diagnostics_messages
	else
		com.part_scope.messages <- cm :: com.part_scope.messages

(** Add a compiler message that is bound to a specific module's cache.

    When [dms_full_typing] is active (i.e. during a full compilation, not
    a display request), the message is also recorded in
    [m.m_extra.m_cache_bound_objects] so that it gets replayed when the
    module is loaded from cache in subsequent server compilations.

    Use this instead of {!add_message} when the message originates from
    processing a specific module and should be preserved across
    compilations. *)
let add_module_message ?(depth = 0) ?(from_macro = false) ?(code = None) com (m : module_def) msg p kind sev =
	if sev = MessageSeverity.Error then com.has_error <- true;
	let cm = make_compiler_message ~from_macro ~code msg p depth kind sev in
	if com.display.dms_full_typing then
		DynArray.add m.m_extra.m_cache_bound_objects (Message cm);
	if is_diagnostics com then
		com.part_scope.diagnostics_messages <- cm :: com.part_scope.diagnostics_messages
	else
		com.part_scope.messages <- cm :: com.part_scope.messages

(** Replay a cache-bound message into the current compilation context.

    Called from {!ServerCache.handle_cache_bound_objects} when loading
    modules from cache. The message is added to the appropriate buffer
    based on whether we're in diagnostics mode. *)
let replay_message com cm =
	if is_diagnostics com then
		com.part_scope.diagnostics_messages <- cm :: com.part_scope.diagnostics_messages
	else
		com.part_scope.messages <- cm :: com.part_scope.messages
