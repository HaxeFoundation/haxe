(** Unified compiler message API.

    This module provides a single entry point for adding compiler messages
    (errors, warnings, info) to the compilation context.

    Messages are stored in a single buffer ([com.part_scope.messages]) and
    the output layer determines how to present them (formatted text for CLI,
    JSON diagnostics for IDE, etc.).

    Cache-bound messages are stored on module extras via
    [m_cache_bound_objects] so they survive across server compilations. *)

open Message
open Common
open Type

(** Add a compiler message to the message buffer.

    This is the primary entry point for recording any compiler output
    (errors, warnings, info messages).

    Sets [com.part_scope.has_error] when severity is [Error]. *)
let add_message com msg p depth message_kind =
	let cm = make_message com.is_macro_context msg p depth message_kind in
	match com.part_scope.message_capture with
	| Some buf ->
		buf := cm :: !buf
	| None ->
		if message_kind_severity message_kind = MessageSeverity.Error then com.part_scope.has_error <- true;
		com.part_scope.messages <- cm :: com.part_scope.messages

(** Add a compiler message that is bound to a specific module's cache.

    When [dms_full_typing] is active (i.e. during a full compilation, not
    a display request), the message is also recorded in
    [m.m_extra.m_cache_bound_objects] so that it gets replayed when the
    module is loaded from cache in subsequent server compilations.

    Use this instead of {!add_message} when the message originates from
    processing a specific module and should be preserved across
    compilations. *)
let add_module_message com (m : module_def) msg p depth message_kind =
	if message_kind_severity message_kind = MessageSeverity.Error then com.part_scope.has_error <- true;
	let cm = make_message com.is_macro_context msg p depth message_kind in
	if com.display.dms_full_typing then
		DynArray.add m.m_extra.m_cache_bound_objects (Message cm);
	com.part_scope.messages <- cm :: com.part_scope.messages

(** Add a pre-built diagnostic message bound to a specific module's cache.

    Like {!add_module_message} but takes a pre-built [Message.t]
    (e.g. from {!DiagnosticsPrinter.make_missing_fields_message}).

    During [dms_full_typing], the message is cached in
    [m.m_extra.m_cache_bound_objects] unconditionally — the
    [RMDiagnostics] filter is checked at replay time, not at store time.

    The message is only added to the current message buffer when in
    diagnostics mode ([RMDiagnostics]), matching the old behavior where
    [module_diagnostics] data was stored separately and only processed
    by the diagnostics printer.

    Use this for diagnostics-specific messages (MissingFields,
    UnresolvedIdentifier) that originate from module typing and should
    survive across server compilations. *)
let add_module_diagnostic com (m : module_def) cm =
	if com.display.dms_full_typing then
		DynArray.add m.m_extra.m_cache_bound_objects (Message cm);
	if is_diagnostics com then
		com.part_scope.messages <- cm :: com.part_scope.messages

(** Replay a cache-bound message into the current compilation context.

    Called from {!ServerCache.handle_cache_bound_objects} when loading
    modules from cache.

    - [MKWarning] messages are re-evaluated through [com.warning] to
      respect current warning options.
    - Diagnostics-specific messages ([DKMissingFields],
      [DKUnresolvedIdentifier]) are only replayed when in diagnostics
      mode ([RMDiagnostics]), matching the filter-on-replay pattern
      used for warning options. *)
let replay_message com cm =
	match cm.cm_message_kind with
	| MKWarning(w, options) ->
		com.warning ~depth:cm.cm_depth w options cm.cm_message cm.cm_pos
	| _ ->
		let is_diagnostics_only = match cm.cm_diagnostics_kind with
			| MessageKind.DKMissingFields | MessageKind.DKUnresolvedIdentifier -> true
			| _ -> false
		in
		if not is_diagnostics_only || is_diagnostics com then
			com.part_scope.messages <- cm :: com.part_scope.messages

(* Default handlers *)

exception Abort

let default_error_handler com =
	(fun (err : Error.error) ->
		Error.recurse_error (fun depth err ->
			add_message com (Error.error_msg err.err_message) err.err_pos depth MKError
		) err;
		if com.part_scope.message_capture = None then begin
			com.part_scope.has_error <- true;
			if Common.fail_fast com then raise Abort
		end
	)

let default_warning_handler com =
	(fun ?(depth=0) w options msg p ->
		match Warning.get_mode w (options @ com.warning_options) with
		| WMEnable ->
			let wobj = Warning.warning_obj w in
			let msg = if wobj.w_generic then
				msg
			else
				Printf.sprintf "(%s) %s" wobj.w_name msg
			in
			add_message com msg p depth (MKWarning(w,options))
		| WMDisable ->
			()
	)

let default_info_handler com =
	(fun ?(depth=0) msg p ->
		add_message com msg p depth MKInfo
	)
