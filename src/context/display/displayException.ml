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

type signature_kind =
	| SKCall
	| SKArrayAccess

type kind =
	| Diagnostics of string
	| Statistics of string
	| ModuleSymbols of string
	| Metadata of string
	| DisplaySignatures of ((tsignature * CompletionType.ct_function) * documentation) list * int * int * signature_kind
	| DisplayHover of hover_result
	| DisplayPositions of pos list
	| DisplayFields of CompletionItem.t list * CompletionResultKind.t * pos option (* insert pos *)
	| DisplayPackage of string list

exception DisplayException of kind

let raise_diagnostics s = raise (DisplayException(Diagnostics s))
let raise_statistics s = raise (DisplayException(Statistics s))
let raise_module_symbols s = raise (DisplayException(ModuleSymbols s))
let raise_metadata s = raise (DisplayException(Metadata s))
let raise_signatures l isig iarg kind = raise (DisplayException(DisplaySignatures(l,isig,iarg,kind)))
let raise_hover item expected p = raise (DisplayException(DisplayHover({hitem = item;hpos = p;hexpected = expected})))
let raise_positions pl = raise (DisplayException(DisplayPositions pl))
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
		(match po with None -> [] | Some p -> ["replaceRange",generate_pos_as_range (Parser.cut_pos_at_display p)]) in
	jobject fl

let to_json ctx de =
	match de with
	| Diagnostics _
	| Statistics _
	| ModuleSymbols _
	| Metadata _ -> assert false
	| DisplaySignatures(sigs,isig,iarg,kind) ->
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
	| DisplayHover hover ->
		let name_source_kind_to_int = function
			| WithType.FunctionArgument -> 0
			| WithType.StructureField -> 1
		in
		let ctx = Genjson.create_context GMFull in
		let generate_name (name,kind) = jobject [
			"name",jstring name;
			"kind",jint (name_source_kind_to_int kind);
		] in
		let expected = match hover.hexpected with
			| Some(WithType.WithType(t,name)) ->
				jobject (("type",generate_type ctx t) :: (match name with None -> [] | Some name -> ["name",generate_name name]))
			| Some(Value(Some name)) ->
				jobject ["name",generate_name name]
			| _ -> jnull
		in
		jobject [
			"documentation",jopt jstring (CompletionItem.get_documentation hover.hitem);
			"range",generate_pos_as_range hover.hpos;
			"item",CompletionItem.to_json ctx hover.hitem;
			"expected",expected;
		]
	| DisplayPositions pl ->
		jarray (List.map generate_pos_as_location pl)
	| DisplayFields(fields,kind,po) ->
		fields_to_json ctx fields kind po
	| DisplayPackage pack ->
		jarray (List.map jstring pack)