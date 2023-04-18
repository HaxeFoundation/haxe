open Globals
open Common
open Json
open DisplayTypes
open DisplayTypes
open Type
open Genjson
open MessageKind

type t = {
	diag_kind : MessageKind.t;
	diag_pos : pos;
	diag_severity : MessageSeverity.t;
	diag_args : Json.t;
	mutable diag_related_information : (pos * int * string) list;
}

let make_diagnostic kd p sev args = {
	diag_kind = kd;
	diag_pos = p;
	diag_severity = sev;
	diag_args = args;
	diag_related_information = [];
}

let is_diagnostics_file com file_key =
	match com.report_mode with
	| RMDiagnostics [] -> true
	| RMDiagnostics file_keys -> List.exists (fun key' -> file_key = key') file_keys
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

let json_of_diagnostics com dctx =
	let diagnostics = Hashtbl.create 0 in
	let current = ref None in
	let add append diag =
		let p = diag.diag_pos in
		let file = if p = null_pos then p.pfile else Path.get_real_path p.pfile in
		let fdiag = try
			Hashtbl.find diagnostics file
		with Not_found ->
			let d = [] in
			Hashtbl.add diagnostics file d;
			d
		in
		if append || (List.find_opt (fun diag -> diag.diag_pos = p) fdiag) = None then
			Hashtbl.replace diagnostics file (diag :: fdiag)
	in
	let file_keys = new Common.file_keys in
	let add dk p sev args =
		let append = match dk with
			| DKUnusedImport
			| DKRemovableCode
			| DKDeprecationWarning
			| DKInactiveBlock ->
				false
			| DKUnresolvedIdentifier
			| DKCompilerMessage
			| DKParserError
			| DKMissingFields ->
				true
		in
		if p = null_pos || is_diagnostics_file com (file_keys#get p.pfile) then begin
			let diag = make_diagnostic dk p sev args in
			current := Some diag;
			add append diag
		end else current := None
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
		add DKUnresolvedIdentifier p MessageSeverity.Error (JArray suggestions);
	) dctx.unresolved_identifiers;
	List.iter (fun (s,p,kind,sev,depth) -> match (depth, !current) with
		| d, Some diag when d > 0 ->
			let lines = ExtString.String.nsplit s "\n" in
			(match lines with
				| [] -> ()
				| s :: sub ->
					let related = List.fold_left (fun acc s -> (p,d,Error.compl_msg s) :: acc) diag.diag_related_information sub in
					diag.diag_related_information <- (p,d,s) :: related;
			)
		| 0, _ ->
			add kind p sev (JString s)
		| _ ->
			(* Do not add errors with depth greater than one as top level diagnostic. *)
			(* This could happen when running diagnostics for a file that is wentioned in *)
			(* sub errors of a file not included for diagnostics. *)
			()
	) (List.rev dctx.diagnostics_messages);
	PMap.iter (fun p (mt,mfl) ->
		let jctx = create_context GMMinimum in
		let all_fields = ref [] in
		let scope cf =
			if has_class_field_flag cf CfStatic then CFSStatic else CFSMember
		in
		let create mf =
			let kind,args = match mf.mf_cause with
				| AbstractParent(csup,tl) ->
					"AbstractParent",jobject [
						"parent",generate_type_path_with_params jctx csup.cl_module.m_path csup.cl_path tl csup.cl_meta;
					]
				| ImplementedInterface(ci,tl) ->
					"ImplementedInterface",jobject [
						"parent",generate_type_path_with_params jctx ci.cl_module.m_path ci.cl_path tl ci.cl_meta;
					]
				| PropertyAccessor(cf,is_getter) ->
					"PropertyAccessor",jobject [
						"property",generate_class_field jctx (scope cf) cf;
						"isGetter",jbool is_getter;
					]
				| FieldAccess ->
					"FieldAccess",jobject []
				| FinalFields cfl ->
					"FinalFields",jobject [
						"fields",jarray (List.map (fun cf -> generate_class_field jctx (scope cf) cf) cfl)
					]
			in
			let current_fields = ref [] in
			let map_field (cf,t,ct) =
				let cf = {cf with cf_overloads = []} in
				if List.exists (fun (t2,cf2) -> cf.cf_name = cf2.cf_name && Overloads.same_overload_args t t2 cf cf2) !current_fields then
					None
				else begin
					(* With multiple interfaces there can be duplicates, which would be bad for the "Implement all" code action. *)
					let unique = not (List.exists (fun (t2,cf2) -> cf.cf_name = cf2.cf_name && Overloads.same_overload_args t t2 cf cf2) !all_fields) in
					current_fields := (t,cf) :: !current_fields;
					all_fields := (t,cf) :: !all_fields;
					Some (jobject [
						"field",generate_class_field jctx (scope cf) cf;
						"type",CompletionType.generate_type jctx ct;
						"unique",jbool unique;
					])
				end
			in
			let fields = ExtList.List.filter_map map_field mf.mf_fields in
			jobject [
				"fields",jarray fields;
				"cause",jobject [
					"kind",jstring kind;
					"args",args
				]
			]
		in
		(* cl_interfaces is reversed, let's reverse the order again here *)
		let l = List.map create (List.rev !mfl) in
		let j = jobject [
			"moduleType",generate_module_type jctx mt;
			"moduleFile",jstring (Path.UniqueKey.lazy_path (t_infos mt).mt_module.m_extra.m_file);
			"entries",jarray l
		] in
		add DKMissingFields p MessageSeverity.Error j
	) dctx.missing_fields;
	(* non-append from here *)
	begin match Warning.get_mode WDeprecated com.warning_options with
	| WMEnable ->
		Hashtbl.iter (fun _ (s,p) ->
			add DKDeprecationWarning p MessageSeverity.Warning (JString s);
		) DeprecationCheck.warned_positions;
	| WMDisable ->
		()
	end;
	PMap.iter (fun p r ->
		if not !r then add DKUnusedImport p MessageSeverity.Warning (JArray [])
	) dctx.import_positions;
	List.iter (fun (s,p,prange) ->
		add DKRemovableCode p MessageSeverity.Warning (JObject ["description",JString s;"range",if prange = null_pos then JNull else Genjson.generate_pos_as_range prange])
	) dctx.removable_code;
	Hashtbl.iter (fun file ranges ->
		List.iter (fun (p,e) ->
			let jo = JObject [
				"expr",JObject [
					"string",JString (Ast.Printer.s_expr e)
				]
			] in
			add DKInactiveBlock p MessageSeverity.Hint jo
		) ranges
	) dctx.dead_blocks;
	let jl = Hashtbl.fold (fun file diag acc ->
		let jl = List.rev_map (fun diag ->
			(JObject [
				"kind",JInt (MessageKind.to_int diag.diag_kind);
				"severity",JInt (MessageSeverity.to_int diag.diag_severity);
				"range",Genjson.generate_pos_as_range diag.diag_pos;
				"args",diag.diag_args;
				"relatedInformation",JArray (
					List.rev_map (fun (pos,depth,msg) -> (JObject [
						"location",Genjson.generate_pos_as_location pos;
						"depth",JInt depth;
						"message",JString msg;
					])) diag.diag_related_information
				)
			])
		) diag in
		(JObject [
			"file",if file = "?" then JNull else JString file;
			"diagnostics",JArray jl
		]) :: acc
	) diagnostics [] in
	let js = JArray jl in
	js
