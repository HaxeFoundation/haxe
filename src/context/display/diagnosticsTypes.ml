open Globals
open Ast
open Type
open Common

type diagnostics_context = {
	mutable removable_code : (string * pos * pos) list;
	mutable import_positions : (pos,bool ref) PMap.t;
	mutable dead_blocks : (Path.UniqueKey.t,(pos * expr) list) Hashtbl.t;
	mutable unresolved_identifiers : (string * pos * (string * CompletionItem.t * int) list) list;
	mutable diagnostics_messages : (string * pos * DisplayTypes.DiagnosticsKind.t * DisplayTypes.DiagnosticsSeverity.t) list;
	mutable missing_fields : (pos,(tclass * (missing_fields_diagnostics list ref))) PMap.t;
}