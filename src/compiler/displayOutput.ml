open Globals
open Ast
open Common
open DisplayTypes.DisplayMode
open DisplayTypes.CompletionResultKind
open CompletionItem
open CompletionClassField
open CompletionEnumField
open DisplayException
open Type
open DisplayTypes
open CompletionModuleType
open Genjson
open CompilationContext
open DisplayProcessingGlobals

(* New JSON stuff *)

open Json

let print_signature tl display_arg =
	let st = s_type (print_context()) in
	let s_arg (n,o,t) = Printf.sprintf "%s%s:%s" (if o then "?" else "") n (st t) in
	let s_fun args ret = Printf.sprintf "(%s):%s" (String.concat ", " (List.map s_arg args)) (st ret) in
	let siginf = List.map (fun (((args,ret),_),doc) ->
		let label = s_fun args ret in
		let parameters =
			List.map (fun arg ->
					let label = s_arg arg in
					JObject [
						"label",JString label
					]
			) args
		in
		let js = [
			"label",JString label;
			"parameters",JArray parameters;
		] in
		JObject (match doc with None -> js | Some d -> ("documentation",JString (gen_doc_text d)) :: js)
	) tl in
	let jo = JObject [
		"signatures",JArray siginf;
		"activeParameter",JInt (arg_index tl 0 display_arg);
		"activeSignature",JInt 0;
	] in
	string_of_json jo

(* Mode processing *)

let find_doc t =
	let doc = match follow t with
		| TAnon an ->
			begin match !(an.a_status) with
				| ClassStatics c -> c.cl_doc
				| EnumStatics en -> en.e_doc
				| AbstractStatics a -> a.a_doc
				| _ -> None
			end
		| _ ->
			None
	in
	doc

let handle_syntax_completion com kind subj =
	let open Parser in
	let l,kind = match kind with
		| SCClassRelation ->
			[Extends;Implements],CRTypeRelation
		| SCInterfaceRelation ->
			[Extends],CRTypeRelation
		| SCComment ->
			[],CRTypeRelation
		| SCTypeDecl mode ->
			let in_import_hx = Filename.basename subj.s_insert_pos.pfile = "import.hx" in
			let l = if in_import_hx then [] else [Private;Extern;Class;Interface;Enum;Abstract;Typedef;Final] in
			let l = match mode with
				| TCBeforePackage -> Package :: Import :: Using :: l
				| TCAfterImport -> Import :: Using :: l
				| TCAfterType -> l
			in
			l,CRTypeDecl
		| SCAfterTypeFlag flags ->
			let l = [Class;Interface] in
			let l = if List.mem DPrivate flags then l else Private :: l in
			let l = if List.mem DExtern flags then l else Extern :: l in
			let l = if List.mem DFinal flags then l else
				Final :: Enum :: Abstract :: Typedef :: l
			in
			l,CRTypeDecl
	in
	match l with
	| [] ->
		()
	| _ ->
		let l = List.map make_ci_keyword l in
		let api = Option.get com.Common.json_out in
		let ctx = Genjson.create_context ~jsonrpc:api.jsonrpc GMFull in
		api.send_result_raise (fields_to_json ctx l kind subj)

let handle_display_exception_json ctx dex api =
	match dex with
	| DisplayHover _ | DisplayPositions _ | DisplayFields _ | DisplayPackage _  | DisplaySignatures _ ->
		DisplayPosition.display_position#reset;
		let ctx = DisplayJson.create_json_context api.jsonrpc (match dex with DisplayFields _ -> true | _ -> false) in
		api.send_result_raise (DisplayException.to_json ctx dex)
	| DisplayNoResult ->
		(match ctx.com.display.dms_kind with
			| DMDefault -> api.send_error_raise [jstring "No completion point"]
			| _ -> api.send_result_raise JNull
		)
	| ModuleSymbols json ->
		DisplayPosition.display_position#reset;
		api.send_result_raise json
	| Metadata _ ->
		die "Unexpected Metadata display exception" __LOC__

let handle_display_exception ctx dex =
	handle_display_exception_json ctx dex (Option.get ctx.com.json_out)

let handle_type_path_exception ctx p c is_import pos =
	let open DisplayTypes.CompletionResultKind in
	let com = ctx.com in
	let fields =
		try begin match c with
			| None ->
				DisplayPath.TypePathHandler.complete_type_path com p
			| Some (c,cur_package) ->
				let ctx = TyperEntry.create com None in
				DisplayPath.TypePathHandler.complete_type_path_inner ctx p c cur_package is_import
		end with Error.Fatal_error err ->
			error_ext ctx err;
			None
	in
	let api = Option.get ctx.com.json_out in
	begin match fields with
	| None ->
		()
	| Some fields ->
		let ctx = DisplayJson.create_json_context api.jsonrpc false in
		let path = match List.rev p with
			| name :: pack -> List.rev pack,name
			| [] -> [],""
		in
		let kind = CRField ((CompletionItem.make_ci_module path,pos,None,None)) in
		api.send_result_raise (DisplayException.fields_to_json ctx fields kind (DisplayTypes.make_subject None pos));
	end

let emit_diagnostics com =
	let api = Option.get com.Common.json_out in
	let dctx = Diagnostics.run com in
	let diagnostics = DiagnosticsPrinter.json_of_diagnostics com dctx in
	DisplayPosition.display_position#reset;
	api.send_result_raise diagnostics

let emit_statistics tctx =
	let api = Option.get tctx.Common.json_out in
	let stats = Statistics.collect_statistics tctx [SFFile (DisplayPosition.display_position#get).pfile] true in
	let json = Statistics.Printer.json_of_statistics stats in
	DisplayPosition.display_position#reset;
	api.send_result_raise json
