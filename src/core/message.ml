open Globals

module MessageSeverity = struct
	type t =
		| Error
		| Warning
		| Information
		| Hint

	let to_int = function
		| Error -> 1
		| Warning -> 2
		| Information -> 3
		| Hint -> 4
end

module MessageKind = struct
	type t =
		| DKUnusedImport
		| DKUnresolvedIdentifier
		| DKCompilerMessage
		| DKReplacableCode
		| DKParserError
		| DKDeprecationWarning
		| DKInactiveBlock
		| DKMissingFields

	let to_int = function
		| DKUnusedImport -> 0
		| DKUnresolvedIdentifier -> 1
		| DKCompilerMessage -> 2
		| DKReplacableCode -> 3
		| DKParserError -> 4
		| DKDeprecationWarning -> 5
		| DKInactiveBlock -> 6
		| DKMissingFields -> 7
end

type warning_mode =
	| WMEnable
	| WMDisable

type warning_option = {
	wo_warning : WarningList.warning;
	wo_mode : warning_mode;
}

type message_kind =
	| MKError
	| MKWarning of WarningList.warning * (warning_option list list)
	| MKInfo

let message_kind_severity = function
	| MKError -> MessageSeverity.Error
	| MKWarning _ -> MessageSeverity.Warning
	| MKInfo -> MessageSeverity.Information

type t = {
	cm_message : string;
	cm_pos : pos;
	cm_depth : int;
	cm_from_macro : bool;
	cm_message_kind : message_kind;
	cm_diagnostics_kind : MessageKind.t;
	cm_json : Json.t;
}

let cm_severity cm = message_kind_severity cm.cm_message_kind

let cm_code cm = match cm.cm_message_kind with
	| MKWarning(w,_) ->
		let wobj = WarningList.warning_obj w in
		Some wobj.w_name
	| _ -> None

let make ?(from_macro = false) ?(diagnostics_kind = MessageKind.DKCompilerMessage) ?json msg p depth message_kind =
	let cm_json = match json with Some j -> j | None -> Json.JString msg in
	{
		cm_message = msg;
		cm_pos = p;
		cm_depth = depth;
		cm_from_macro = from_macro;
		cm_message_kind = message_kind;
		cm_diagnostics_kind = diagnostics_kind;
		cm_json;
	}
