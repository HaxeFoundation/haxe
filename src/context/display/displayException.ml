open Globals
open Ast
open DisplayTypes
open CompletionItem
open Type
open Genjson

type hover_result = {
	hitem : CompletionItem.t;
	hpos : pos;
}

type kind =
	| Diagnostics of string
	| Statistics of string
	| ModuleSymbols of string
	| Metadata of string
	| DisplaySignatures of (tsignature * documentation) list * int * int
	| DisplayHover of hover_result
	| DisplayPosition of pos list
	| DisplayFields of CompletionItem.t list * CompletionResultKind.t * pos option (* insert pos *)
	| DisplayPackage of string list

exception DisplayException of kind

let raise_diagnostics s = raise (DisplayException(Diagnostics s))
let raise_statistics s = raise (DisplayException(Statistics s))
let raise_module_symbols s = raise (DisplayException(ModuleSymbols s))
let raise_metadata s = raise (DisplayException(Metadata s))
let raise_signatures l isig iarg = raise (DisplayException(DisplaySignatures(l,isig,iarg)))
let raise_hover item p = raise (DisplayException(DisplayHover({hitem = item;hpos = p})))
let raise_position pl = raise (DisplayException(DisplayPosition pl))
let raise_fields ckl cr po = raise (DisplayException(DisplayFields(ckl,cr,po)))
let raise_package sl = raise (DisplayException(DisplayPackage sl))

(* global state *)
let last_completion_result = ref (Array.make 0 (CompletionItem.make (ITModule ([],"")) None))

let fields_to_json ctx fields kind po =
	let ja = List.map (CompletionItem.to_json ctx) fields in
	last_completion_result := Array.of_list fields;
	let fl =
		("items",jarray ja) ::
		("mode",CompletionResultKind.to_json ctx kind) ::
		(match po with None -> [] | Some p -> ["replaceRange",generate_pos_as_range p]) in
	jobject fl

let to_json ctx de =
	match de with
	| Diagnostics _
	| Statistics _
	| ModuleSymbols _
	| Metadata _ -> assert false
	| DisplaySignatures(sigs,isig,iarg) ->
		(* We always want full info for signatures *)
		let ctx = Genjson.create_context GMFull in
		let fsig ((tl,tr),doc) =
			let fl = generate_function_signature ctx tl tr in
			let fl = (match doc with None -> fl | Some s -> ("documentation",jstring s) :: fl) in
			jobject fl
		in
		jobject [
			"activeSignature",jint isig;
			"activeParameter",jint iarg;
			"signatures",jlist fsig sigs;
		]
	| DisplayHover hover ->
		jobject [
			"documentation",jopt jstring (CompletionItem.get_documentation hover.hitem);
			"range",generate_pos_as_range hover.hpos;
			"type",jopt (generate_type ctx) hover.hitem.ci_type; (* TODO: remove *)
			"item",CompletionItem.to_json ctx hover.hitem;
		]
	| DisplayPosition pl ->
		jarray (List.map generate_pos_as_location pl)
	| DisplayFields(fields,kind,po) ->
		fields_to_json ctx fields kind po
	| DisplayPackage pack ->
		jarray (List.map jstring pack)