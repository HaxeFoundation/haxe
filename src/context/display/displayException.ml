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
	finsert_pos : pos option;
	fsubject : string option;
}

type signature_kind =
	| SKCall
	| SKArrayAccess

type kind =
	| Diagnostics of string
	| Statistics of string
	| ModuleSymbols of string
	| Metadata of string
	| DisplaySignatures of (((tsignature * CompletionType.ct_function) * documentation) list * int * int * signature_kind) option
	| DisplayHover of hover_result option
	| DisplayPositions of pos list
	| DisplayFields of fields_result option
	| DisplayPackage of string list

exception DisplayException of kind

let raise_diagnostics s = raise (DisplayException(Diagnostics s))
let raise_statistics s = raise (DisplayException(Statistics s))
let raise_module_symbols s = raise (DisplayException(ModuleSymbols s))
let raise_metadata s = raise (DisplayException(Metadata s))
let raise_signatures l isig iarg kind = raise (DisplayException(DisplaySignatures(Some(l,isig,iarg,kind))))
let raise_hover item expected p = raise (DisplayException(DisplayHover(Some {hitem = item;hpos = p;hexpected = expected})))
let raise_positions pl = raise (DisplayException(DisplayPositions pl))
let raise_fields ckl cr po = raise (DisplayException(DisplayFields(Some({fitems = ckl;fkind = cr;finsert_pos = po;fsubject = None}))))
let raise_fields2 ckl cr po subject = raise (DisplayException(DisplayFields(Some({fitems = ckl;fkind = cr;finsert_pos = Some po;fsubject = Some subject}))))
let raise_package sl = raise (DisplayException(DisplayPackage sl))

(* global state *)
let last_completion_result = ref (Array.make 0 (CompletionItem.make (ITModule ([],"")) None))
let last_completion_pos = ref None
let max_completion_items = ref 0

let filter_somehow ctx items subject kind po =
	let ret = DynArray.create () in
	let acc_types = DynArray.create () in
	let subject = match subject with
		| None -> []
		| Some subject -> List.map String.lowercase (ExtString.String.nsplit subject ".")
	in
	let rec subject_matches subject sl = match subject with
		| [] -> true
		| s' :: sl' ->
			let rec loop sl = match sl with
				| [] -> false
				| s :: sl when ExtString.String.exists s s' -> subject_matches sl' sl
				| _ :: sl -> loop sl
			in
			loop sl
	in
	let rec loop items index =
		match items with
		| _ when DynArray.length ret > !max_completion_items ->
			()
		| item :: items ->
			let parts = List.map String.lowercase (get_filter_parts item) in
			if subject_matches subject parts then begin
				(* Treat types with lowest priority. The assumption is that they are the only kind
				   which actually causes the limit to be hit, so we show everything else and then
				   fill in types. *)
				match item.ci_kind with
				| ITType _ ->
					if DynArray.length ret + DynArray.length acc_types < !max_completion_items then
						DynArray.add acc_types (item,index);
				| _ ->
					DynArray.add ret (CompletionItem.to_json ctx (Some index) item);
			end;
			loop items (index + 1)
		| [] ->
			()
	in
	loop items 0;
	DynArray.iter (fun (item,index) ->
		if DynArray.length ret < !max_completion_items then
			DynArray.add ret (CompletionItem.to_json ctx (Some index) item);
	) acc_types;
	DynArray.to_list ret

let fields_to_json ctx fields kind po subject =
	last_completion_result := Array.of_list fields;
	last_completion_pos := po;
	let needs_filtering = !max_completion_items > 0 && Array.length !last_completion_result > !max_completion_items in
	let ja = if needs_filtering then
		filter_somehow ctx fields subject kind po
	else
		List.mapi (fun i item -> CompletionItem.to_json ctx (Some i) item) fields
 	in
	let fl =
		("items",jarray ja) ::
		("isIncomplete",jbool needs_filtering) ::
		("mode",CompletionResultKind.to_json ctx kind) ::
		(match po with None -> [] | Some p -> ["replaceRange",generate_pos_as_range (Parser.cut_pos_at_display p)]) in
	jobject fl

let to_json ctx de =
	match de with
	| Diagnostics _
	| Statistics _
	| ModuleSymbols _
	| Metadata _ -> assert false
	| DisplaySignatures None ->
		jnull
	| DisplaySignatures Some(sigs,isig,iarg,kind) ->
		(* We always want full info for signatures *)
		let ctx = Genjson.create_context GMFull in
		let fsig ((_,signature),doc) =
			let fl = CompletionType.generate_function' ctx signature in
			let fl = (match doc with None -> fl | Some s -> ("documentation",jstring s) :: fl) in
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
			| _ -> assert false
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
			"documentation",jopt jstring (CompletionItem.get_documentation hover.hitem);
			"range",generate_pos_as_range hover.hpos;
			"item",CompletionItem.to_json ctx None hover.hitem;
			"expected",expected;
		]
	| DisplayPositions pl ->
		jarray (List.map generate_pos_as_location pl)
	| DisplayFields None ->
		jnull
	| DisplayFields Some r ->
		fields_to_json ctx r.fitems r.fkind r.finsert_pos r.fsubject
	| DisplayPackage pack ->
		jarray (List.map jstring pack)