open Globals
open Json
open DisplayTypes
open DiagnosticsKind
open DisplayTypes
open DiagnosticsTypes

type t = DiagnosticsKind.t * pos

let is_diagnostics_file file =
	let file = Path.unique_full_path file in
	match (!Parser.display_mode) with
	| DMDiagnostics [] -> true
	| DMDiagnostics files -> List.exists (fun file' -> file = file') files
	| _ -> false

module UnresolvedIdentifierSuggestion = struct
	type t =
		| UISImport
		| UISTypo

	let to_int = function
		| UISImport -> 0
		| UISTypo -> 1
end

open UnresolvedIdentifierSuggestion
open CompletionItem
open CompletionModuleType

let json_of_diagnostics dctx =
	let diag = Hashtbl.create 0 in
	let add dk p sev args =
		let file = if p = null_pos then p.pfile else Path.get_real_path p.pfile in
		let diag = try
			Hashtbl.find diag file
		with Not_found ->
			let d = Hashtbl.create 0 in
			Hashtbl.add diag file d;
			d
		in
		if not (Hashtbl.mem diag p) then
			Hashtbl.add diag p (dk,p,sev,args)
	in
	let add dk p sev args =
		if p = null_pos || is_diagnostics_file p.pfile then add dk p sev args
	in
	List.iter (fun (s,p,suggestions) ->
		let suggestions = ExtList.List.filter_map (fun (s,item,r) ->
			match item.ci_kind with
			| ITType(t,_) when r = 0 ->
				let path = if t.module_name = t.name then (t.pack,t.name) else (t.pack @ [t.module_name],t.name) in
				Some (JObject [
					"kind",JInt (to_int UISImport);
					"name",JString (s_type_path path);
				])
			| _ when r = 0 ->
				(* TODO !!! *)
				None
			| _ ->
				Some (JObject [
					"kind",JInt (to_int UISTypo);
					"name",JString s;
				])
		) suggestions in
		add DKUnresolvedIdentifier p DiagnosticsSeverity.Error (JArray suggestions);
	) dctx.unresolved_identifiers;
	PMap.iter (fun p r ->
		if not !r then add DKUnusedImport p DiagnosticsSeverity.Warning (JArray [])
	) dctx.import_positions;
	List.iter (fun (s,p,kind,sev) ->
		add kind p sev (JString s)
	) (List.rev dctx.diagnostics_messages);
	List.iter (fun (s,p,prange) ->
		add DKRemovableCode p DiagnosticsSeverity.Warning (JObject ["description",JString s;"range",if prange = null_pos then JNull else Genjson.generate_pos_as_range prange])
	) dctx.removable_code;
	Hashtbl.iter (fun _ (s,p) ->
		add DKDeprecationWarning p DiagnosticsSeverity.Warning (JString s);
	) DeprecationCheck.warned_positions;
	Hashtbl.iter (fun file ranges ->
		List.iter (fun (p,e) ->
			let jo = JObject [
				"expr",JObject [
					"string",JString (Ast.Printer.s_expr e)
				]
			] in
			add DKInactiveBlock p DiagnosticsSeverity.Hint jo
		) ranges
	) dctx.dead_blocks;
	let jl = Hashtbl.fold (fun file diag acc ->
		let jl = Hashtbl.fold (fun _ (dk,p,sev,jargs) acc ->
			(JObject [
				"kind",JInt (DiagnosticsKind.to_int dk);
				"severity",JInt (DiagnosticsSeverity.to_int sev);
				"range",Genjson.generate_pos_as_range p;
				"args",jargs
			]) :: acc
		) diag [] in
		(JObject [
			"file",if file = "?" then JNull else JString file;
			"diagnostics",JArray jl
		]) :: acc
	) diag [] in
	let js = JArray jl in
	js