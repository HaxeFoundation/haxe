open Globals
open Ast
open DisplayTypes
open CompletionItem
open Type
open Genjson

type kind =
	| Diagnostics of string
	| Statistics of string
	| ModuleSymbols of string
	| Metadata of string
	| DisplaySignatures of (tsignature * documentation) list * int * int
	| DisplayHover of t option * pos * string option
	| DisplayPosition of pos list
	| DisplayFields of CompletionItem.t list * CompletionResultKind.t * pos option (* insert pos *) * bool (* sorted? *)
	| DisplayPackage of string list

exception DisplayException of kind

let raise_diagnostics s = raise (DisplayException(Diagnostics s))
let raise_statistics s = raise (DisplayException(Statistics s))
let raise_module_symbols s = raise (DisplayException(ModuleSymbols s))
let raise_metadata s = raise (DisplayException(Metadata s))
let raise_signatures l isig iarg = raise (DisplayException(DisplaySignatures(l,isig,iarg)))
let raise_hover t p so = raise (DisplayException(DisplayHover(t,p,so)))
let raise_position pl = raise (DisplayException(DisplayPosition pl))
let raise_fields ckl cr po b = raise (DisplayException(DisplayFields(ckl,cr,po,b)))
let raise_package sl = raise (DisplayException(DisplayPackage sl))

(* global state *)
let last_completion_result = ref (Array.make 0 (ITModule ""))

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
	| DisplayHover(t,p,doc) ->
		jobject [
			"documentation",jopt jstring doc;
			"range",generate_pos_as_range p;
			"type",jopt (generate_type ctx) t;
		]
	| DisplayPosition pl ->
		jarray (List.map generate_pos_as_location pl)
	| DisplayFields(fields,kind,po,sorted) ->
		let ja = List.map (CompletionItem.to_json ctx) fields in
		last_completion_result := Array.of_list fields;
		let fl =
			("items",jarray ja) ::
			("kind",jint (Obj.magic kind)) ::
			("sorted",jbool sorted) ::
			(match po with None -> [] | Some p -> ["replaceRange",generate_pos_as_range p]) in
		jobject fl
	| DisplayPackage pack ->
		jarray (List.map jstring pack)