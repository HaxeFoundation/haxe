open Globals
open Ast
open DisplayTypes
open CompletionItem
open Type
open Genjson

type hover_result = {
	hitem : CompletionItem.t;
	hpos : pos;
	hexpected : WithType.t option;
}

type fields_result = {
	fitems : CompletionItem.t list;
	fkind : CompletionResultKind.t;
	fsubject : completion_subject;
}

type signature_kind =
	| SKCall
	| SKArrayAccess

type kind =
	| DisplayDiagnostics of DiagnosticsTypes.diagnostics_context
	| Statistics of string
	| ModuleSymbols of string
	| Metadata of string
	| DisplaySignatures of (((tsignature * CompletionType.ct_function) * documentation) list * int * int * signature_kind) option
	| DisplayHover of hover_result option
	| DisplayPositions of pos list
	| DisplayFields of fields_result option
	| DisplayPackage of string list

exception DisplayException of kind

let raise_diagnostics s = raise (DisplayException(DisplayDiagnostics s))
let raise_statistics s = raise (DisplayException(Statistics s))
let raise_module_symbols s = raise (DisplayException(ModuleSymbols s))
let raise_metadata s = raise (DisplayException(Metadata s))
let raise_signatures l isig iarg kind = raise (DisplayException(DisplaySignatures(Some(l,isig,iarg,kind))))
let raise_hover item expected p = raise (DisplayException(DisplayHover(Some {hitem = item;hpos = p;hexpected = expected})))
let raise_positions pl = raise (DisplayException(DisplayPositions pl))
let raise_fields ckl cr subj = raise (DisplayException(DisplayFields(Some({fitems = ckl;fkind = cr;fsubject = subj}))))
let raise_package sl = raise (DisplayException(DisplayPackage sl))

(* global state *)
let last_completion_result = ref (Array.make 0 (CompletionItem.make (ITModule ([],"")) None))
let last_completion_pos = ref None
let max_completion_items = ref 0

let filter_somehow ctx items kind subj =
	let subject = match subj.s_name with
		| None -> ""
		| Some name-> String.lowercase name
	in
	let subject_length = String.length subject in
	let determine_cost s =
		let get_initial_cost o =
			if o = 0 then
				0 (* Term starts with subject - perfect *)
			else begin
				(* Consider `.` as anchors and determine distance from closest one. Penalize starting distance by factor 2. *)
				try
					let last_anchor = String.rindex_from s o '.' in
					(o - (last_anchor + 1)) * 2
				with Not_found ->
					o * 2
			end
		in
		let index_from o c =
			let rec loop o cost =
				let c' = s.[o] in
				if c' = c then
					o,cost
				else
					loop (o + 1) (cost + 3) (* Holes are bad, penalize by 3. *)
			in
			loop o 0
		in
		let rec loop i o cost =
			if i < subject_length then begin
				let o',new_cost = index_from o subject.[i] in
				loop (i + 1) o' (cost + new_cost)
			end else
				cost + (if o = String.length s - 1 then 0 else 1) (* Slightly penalize for not-exact matches. *)
		in
		if subject_length = 0 then
			0
		else try
			let o = String.index s subject.[0] in
			loop 1 o (get_initial_cost o);
		with Not_found | Invalid_argument _ ->
			-1
	in
	let rec loop acc items index =
		match items with
		| item :: items ->
			let name = String.lowercase (get_filter_name item) in
			let cost = determine_cost name in
			let acc = if cost >= 0 then
				(item,index,cost) :: acc
			else
				acc
			in
			loop acc items (index + 1)
		| [] ->
			acc
	in
	let acc = loop [] items 0 in
	let acc = if subject_length = 0 then
		List.rev acc
	else
		List.sort (fun (_,_,cost1) (_,_,cost2) ->
			compare cost1 cost2
		) acc
	in
	let ret = DynArray.create () in
	let rec loop acc_types = match acc_types with
		| (item,index,_) :: acc_types when DynArray.length ret < !max_completion_items ->
			DynArray.add ret (CompletionItem.to_json ctx (Some index) item);
			loop acc_types
		| _ ->
			()
	in
	loop acc;
	DynArray.to_list ret,DynArray.length ret

let patch_completion_subject subj =
	let p = Parser.cut_pos_at_display subj.s_insert_pos in
	match subj.s_name with
	| Some name ->
		let delta = p.pmax - p.pmin in
		let name = if delta > 0 && delta < String.length name then
			String.sub name 0 delta
		else
			name
		in
		{subj with s_name = Some name;s_insert_pos = p}
	| None ->
		{subj with s_insert_pos = p}

let fields_to_json ctx fields kind subj =
	last_completion_result := Array.of_list fields;
	let needs_filtering = !max_completion_items > 0 && Array.length !last_completion_result > !max_completion_items in
	(* let p_before = subj.s_insert_pos in *)
	let subj = patch_completion_subject subj in
	let ja,num_items = if needs_filtering then
		filter_somehow ctx fields kind subj
	else
		List.mapi (fun i item -> CompletionItem.to_json ctx (Some i) item) fields,Array.length !last_completion_result
 	in
	let did_filter = num_items = !max_completion_items in
	last_completion_pos := if did_filter then Some subj.s_start_pos else None;
	let filter_string = (match subj.s_name with None -> "" | Some name -> name) in
	(* print_endline (Printf.sprintf "FIELDS OUTPUT:\n\tfilter_string: %s\n\t    num items: %i\n\t        start: %s\n\t     position: %s\n\t   before cut: %s"
		filter_string
		num_items
		(Printer.s_pos subj.s_start_pos)
		(Printer.s_pos subj.s_insert_pos)
		(Printer.s_pos p_before)
	); *)
	let fl =
		("items",jarray ja) ::
		("isIncomplete",jbool did_filter) ::
		("mode",CompletionResultKind.to_json ctx kind) ::
		("filterString",jstring filter_string) ::
		("replaceRange",generate_pos_as_range subj.s_insert_pos) ::
		[]
	in
	jobject fl

let to_json ctx de =
	match de with
	| Statistics _
	| ModuleSymbols _
	| Metadata _ -> die "" __LOC__
	| DisplaySignatures None ->
		jnull
	| DisplayDiagnostics dctx ->
		DiagnosticsPrinter.json_of_diagnostics dctx
	| DisplaySignatures Some(sigs,isig,iarg,kind) ->
		(* We always want full info for signatures *)
		let ctx = Genjson.create_context GMFull in
		let fsig ((_,signature),doc) =
			let fl = CompletionType.generate_function' ctx signature in
			let fl = (match doc with None -> fl | Some d -> ("documentation",jstring (gen_doc_text d)) :: fl) in
			jobject fl
		in
		let sigkind = match kind with
			| SKCall -> 0
			| SKArrayAccess -> 1
		in
		jobject [
			"activeSignature",jint isig;
			"activeParameter",jint iarg;
			"signatures",jlist fsig sigs;
			"kind",jint sigkind;
		]
	| DisplayHover None ->
		jnull
	| DisplayHover (Some hover) ->
		let named_source_kind = function
			| WithType.FunctionArgument name -> (0, name)
			| WithType.StructureField name -> (1, name)
			| _ -> die "" __LOC__
		in
		let ctx = Genjson.create_context GMFull in
		let generate_name kind =
			let i, name = named_source_kind kind in
			jobject [
				"name",jstring name;
				"kind",jint i;
			]
		in
		let expected = match hover.hexpected with
			| Some(WithType.WithType(t,src)) ->
				jobject (("type",generate_type ctx t)
				:: (match src with
					| None -> []
					| Some ImplicitReturn -> []
					| Some src -> ["name",generate_name src])
				)
			| Some(Value(Some ((FunctionArgument name | StructureField name) as src))) ->
				jobject ["name",generate_name src]
			| _ -> jnull
		in
		jobject [
			"documentation",jopt jstring (gen_doc_text_opt (CompletionItem.get_documentation hover.hitem));
			"range",generate_pos_as_range hover.hpos;
			"item",CompletionItem.to_json ctx None hover.hitem;
			"expected",expected;
		]
	| DisplayPositions pl ->
		jarray (List.map generate_pos_as_location pl)
	| DisplayFields None ->
		jnull
	| DisplayFields Some r ->
		fields_to_json ctx r.fitems r.fkind r.fsubject
	| DisplayPackage pack ->
		jarray (List.map jstring pack)