(** Unified compiler message API.

    This module provides a single entry point for adding compiler messages
    (errors, warnings, info) to the compilation context.

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
    (errors, warnings, info messages).

    Sets [com.has_error] when severity is [Error]. *)
let add_message ?(depth = 0) ?(from_macro = false) com msg p message_kind =
	if message_kind_severity message_kind = MessageSeverity.Error then com.has_error <- true;
	let cm = make_compiler_message ~from_macro msg p depth message_kind in
	com.part_scope.messages <- cm :: com.part_scope.messages

(** Add a compiler message that is bound to a specific module's cache.

    When [dms_full_typing] is active (i.e. during a full compilation, not
    a display request), the message is also recorded in
    [m.m_extra.m_cache_bound_objects] so that it gets replayed when the
    module is loaded from cache in subsequent server compilations.

    Use this instead of {!add_message} when the message originates from
    processing a specific module and should be preserved across
    compilations. *)
let add_module_message ?(depth = 0) ?(from_macro = false) com (m : module_def) msg p message_kind =
	if message_kind_severity message_kind = MessageSeverity.Error then com.has_error <- true;
	let cm = make_compiler_message ~from_macro msg p depth message_kind in
	if com.display.dms_full_typing then
		DynArray.add m.m_extra.m_cache_bound_objects (Message cm);
	com.part_scope.messages <- cm :: com.part_scope.messages

(** Replay a cache-bound message into the current compilation context.

    Called from {!ServerCache.handle_cache_bound_objects} when loading
    modules from cache. [MKWarning] messages are re-evaluated through
    [com.warning] to respect current warning options. *)
let replay_message com cm =
	match cm.cm_message_kind with
	| MKWarning(w, options) ->
		com.warning ~depth:cm.cm_depth ~from_macro:cm.cm_from_macro w options cm.cm_message cm.cm_pos
	| _ ->
		com.part_scope.messages <- cm :: com.part_scope.messages
