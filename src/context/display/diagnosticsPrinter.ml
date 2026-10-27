open Globals
open Common
open Json
open DisplayTypes
open Type
open Genjson
open Message
open MessageKind

type t = {
	diag_kind : MessageKind.t;
	diag_pos : pos;
	diag_severity : MessageSeverity.t;
	diag_code : string option;
	diag_args : Json.t;
	mutable diag_related_information : (pos * int * string) list;
}

let make_diagnostic kd p sev code args = {
	diag_kind = kd;
	diag_pos = p;
	diag_severity = sev;
	diag_code = code;
	diag_args = args;
	diag_related_information = [];
}

let is_diagnostics_file com file_key =
	match com.part_scope.report_mode with
	| RMDiagnostics [] -> true
	| RMDiagnostics file_keys -> List.mem file_key file_keys
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

(** Convert a single missing_fields_diagnostics to its JSON form and
    store it as a [Message.t] with [DKMissingFields].

    The JSON carries: moduleType, moduleFile, and a single entry
    (with "fields" and "cause"). The printer groups entries by
    position, handles cross-entry field deduplication, and wraps
    them in the final "entries" array. *)
let make_missing_fields_message mf =
	let jctx = create_minimal_config () in
	let scope cf =
		if has_class_field_flag cf CfStatic then CFSStatic else CFSMember
	in
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
			current_fields := (t,cf) :: !current_fields;
			Some (jobject [
				"name",jstring cf.cf_name;
				"field",generate_class_field jctx (scope cf) cf;
				"type",CompletionType.generate_type jctx ct;
			])
		end
	in
	let fields = ExtList.List.filter_map map_field mf.mf_fields in
	let entry = jobject [
		"fields",jarray fields;
		"cause",jobject [
			"kind",jstring kind;
			"args",args
		]
	] in
	let j = jobject [
		"moduleType",generate_module_type { jctx with generate_member_bodies = false } mf.mf_on;
		"moduleFile",jstring (Path.UniqueKey.lazy_path (t_infos mf.mf_on).mt_module.m_extra.m_file);
		"entry",entry
	] in
	Message.make_diagnostic false DKMissingFields j mf.mf_pos 0 MKError

(** Create a [Message.t] for an UnresolvedIdentifier diagnostic. *)
let make_unresolved_identifier_message i p suggestions =
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
	Message.make_diagnostic false DKUnresolvedIdentifier (JArray suggestions) p 0 MKError

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
	let add dk p sev code args =
		let append = match dk with
			| DKUnusedImport
			| DKReplacableCode
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
			let diag = make_diagnostic dk p sev code args in
			current := Some diag;
			add append diag
		end else current := None
	in
	(* Collect DKMissingFields messages for grouping by position *)
	let missing_fields_by_pos = Hashtbl.create 0 in
	List.iter (fun d -> match (d.cm_depth, d.cm_diagnostics_kind) with
		| 0, DKMissingFields ->
			let entries = try Hashtbl.find missing_fields_by_pos d.cm_pos with Not_found -> [] in
			Hashtbl.replace missing_fields_by_pos d.cm_pos (d :: entries)
		| _ -> ()
	) (List.rev dctx.messages);
	List.iter (fun d -> match (d.cm_depth, !current) with
		| _, _ when d.cm_diagnostics_kind = DKMissingFields ->
			(* Handled separately via missing_fields_by_pos *)
			()
		| depth, Some diag when depth > 0 ->
			let lines = ExtString.String.nsplit d.cm_message "\n" in
			(match lines with
				| [] -> ()
				| s :: sub ->
					let related = List.fold_left (fun acc s -> (d.cm_pos,depth,Error.compl_msg s) :: acc) [] (List.rev sub) in
					diag.diag_related_information <- List.append diag.diag_related_information ((d.cm_pos,depth,s) :: related);
			)
		| 0, _ ->
			add d.cm_diagnostics_kind d.cm_pos (cm_severity d) (cm_code d) d.cm_json
		| _ ->
			(* Do not add errors with depth greater than one as top level diagnostic. *)
			(* This could happen when running diagnostics for a file that is wentioned in *)
			(* sub errors of a file not included for diagnostics. *)
			()
	) (List.rev dctx.messages);
	(* Group MissingFields entries by position and emit combined diagnostics.
	   Cross-entry field deduplication adds "unique" flags. *)
	Hashtbl.iter (fun p entries ->
		let module_type = ref JNull in
		let module_file = ref JNull in
		let all_field_names = Hashtbl.create 0 in
		let process_entry d =
			match d.cm_json with
			| JObject fields ->
				(match List.assoc_opt "moduleType" fields with Some j -> module_type := j | None -> ());
				(match List.assoc_opt "moduleFile" fields with Some j -> module_file := j | None -> ());
				begin match List.assoc_opt "entry" fields with
				| Some (JObject entry_fields) ->
					let entry_field_jsons = match List.assoc_opt "fields" entry_fields with
						| Some (JArray fl) ->
							List.map (fun fj ->
								let name = match fj with
									| JObject l -> (match List.assoc_opt "name" l with Some (JString n) -> n | _ -> "")
									| _ -> ""
								in
								let unique = not (Hashtbl.mem all_field_names name) in
								Hashtbl.replace all_field_names name true;
								match fj with
								| JObject l -> JObject (l @ ["unique", jbool unique])
								| _ -> fj
							) fl
						| _ -> []
					in
					let cause = match List.assoc_opt "cause" entry_fields with Some j -> j | None -> JNull in
					Some (jobject [
						"fields",jarray entry_field_jsons;
						"cause",cause
					])
				| _ -> None
				end
			| _ -> None
		in
		(* cl_interfaces is reversed, reverse again to restore order *)
		let grouped_entries = ExtList.List.filter_map process_entry (List.rev entries) in
		let j = jobject [
			"moduleType",!module_type;
			"moduleFile",!module_file;
			"entries",jarray grouped_entries
		] in
		add DKMissingFields p MessageSeverity.Error None j
	) missing_fields_by_pos;
	(* non-append from here *)
	begin match Warning.get_mode WDeprecated com.warning_options with
	| WMEnable ->
		Hashtbl.iter (fun _ (s,p,options) ->
			begin match Warning.get_mode WDeprecated (com.warning_options @ options) with
			| WMEnable ->
				let wobj = Warning.warning_obj WDeprecated in
				add DKDeprecationWarning p MessageSeverity.Warning (Some wobj.w_name) (JString s);
			| WMDisable -> ()
			end
		) com.part_scope.warned_positions;
	| WMDisable ->
		()
	end;
	PMap.iter (fun p r ->
		if not !r then add DKUnusedImport p MessageSeverity.Warning None (JArray [])
	) dctx.import_positions;
	List.iter (fun rc ->
		add DKReplacableCode rc.display_range MessageSeverity.Warning None (JObject [
			"description",JString rc.reason;
			"newCode",JString rc.replacement;
			"range",if rc.replace_range = null_pos then JNull else Genjson.generate_pos_as_range rc.replace_range
		])
	) dctx.replaceable_code;
	Hashtbl.iter (fun file ranges ->
		List.iter (fun (p,e) ->
			let jo = JObject [
				"expr",JObject [
					"string",JString (Ast.Printer.s_expr e)
				]
			] in
			add DKInactiveBlock p MessageSeverity.Hint None jo
		) ranges
	) dctx.dead_blocks;
	let jl = Hashtbl.fold (fun file diag acc ->
		let jl = List.rev_map (fun diag ->
			(JObject [
				"kind",JInt (MessageKind.to_int diag.diag_kind);
				"severity",JInt (MessageSeverity.to_int diag.diag_severity);
				"range",Genjson.generate_pos_as_range diag.diag_pos;
				"args",diag.diag_args;
				"code",(match diag.diag_code with None -> JNull | Some c -> JString c);
				"relatedInformation",JArray (
					List.map (fun (pos,depth,msg) -> (JObject [
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
