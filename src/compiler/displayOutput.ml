open Globals
open Ast
open Common
open DisplayTypes.CompletionResultKind
open CompletionItem
open DisplayException
open DisplayTypes
open Genjson

(* Mode processing *)

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
		let rh = com.Common.request_scope.result_handler in
		let ctx = Genjson.create_full_config () in
		CompilerOutput.send_result_raise rh (fields_to_json ctx l kind subj)

let handle_display_exception_json com dex rh =
	match dex with
	| DisplayHover _ | DisplayFields _ ->
		DisplayPosition.display_position#reset;
		let ctx = match dex with
			| DisplayFields _ when !DisplayJson.supports_resolve -> Genjson.create_minimal_config ()
			| _ -> Genjson.create_full_config ()
		in
		CompilerOutput.send_result_raise rh (DisplayException.to_json ctx dex)

let handle_display_exception com dex =
	handle_display_exception_json com dex com.request_scope.result_handler

let handle_type_path_exception com p c is_import pos =
	let open DisplayTypes.CompletionResultKind in
	let fields =
		try begin match c with
			| None ->
				DisplayPath.TypePathHandler.complete_type_path com p
			| Some (c,cur_package) ->
				let ctx = TyperEntry.create com None in
				DisplayPath.TypePathHandler.complete_type_path_inner ctx p c cur_package is_import
		end with Error.Fatal_error err ->
			com.error_ext err;
			None
	in
	let rh = com.request_scope.result_handler in
	begin match fields with
	| None ->
		()
	| Some fields ->
		let ctx = Genjson.create_full_config () in
		let path = match List.rev p with
			| name :: pack -> List.rev pack,name
			| [] -> [],""
		in
		let kind = CRField ((CompletionItem.make_ci_module path,pos,None,None)) in
		CompilerOutput.send_result_raise rh (DisplayException.fields_to_json ctx fields kind (DisplayTypes.make_subject None pos));
	end

let emit_diagnostics com =
	let rh = com.Common.request_scope.result_handler in
	let dctx = Diagnostics.run com in
	let diagnostics = DiagnosticsPrinter.json_of_diagnostics com dctx in
	DisplayPosition.display_position#reset;
	CompilerOutput.send_result_raise rh diagnostics

let emit_statistics tctx =
	let rh = tctx.Common.request_scope.result_handler in
	let stats = Statistics.collect_statistics tctx [SFFile (DisplayPosition.display_position#get).pfile] true in
	let json = Statistics.Printer.json_of_statistics stats in
	DisplayPosition.display_position#reset;
	CompilerOutput.send_result_raise rh json
