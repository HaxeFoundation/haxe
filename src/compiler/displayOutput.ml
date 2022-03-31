open Globals
open Ast
open Common
open Filename
open Timer
open DisplayTypes.DisplayMode
open DisplayTypes.CompletionResultKind
open CompletionItem
open CompletionClassField
open CompletionEnumField
open ClassFieldOrigin
open DisplayException
open Type
open Display
open DisplayTypes
open CompletionModuleType
open Typecore
open Genjson
open CompilationContext
open DisplayProcessingGlobals

(* Old XML stuff *)

let htmlescape s =
	let s = String.concat "&amp;" (ExtString.String.nsplit s "&") in
	let s = String.concat "&lt;" (ExtString.String.nsplit s "<") in
	let s = String.concat "&gt;" (ExtString.String.nsplit s ">") in
	let s = String.concat "&quot;" (ExtString.String.nsplit s "\"") in
	s

let get_timer_fields start_time =
	let tot = ref 0. in
	Hashtbl.iter (fun _ t -> tot := !tot +. t.total) Timer.htimers;
	let fields = [("@TOTAL", Printf.sprintf "%.3fs" (get_time() -. start_time))] in
	if !tot > 0. then
		Hashtbl.fold (fun _ t acc ->
			((String.concat "." t.id),(Printf.sprintf "%.3fs (%.0f%%)" t.total (t.total *. 100. /. !tot))) :: acc
		) Timer.htimers fields
	else
		fields

let print_keywords () =
	let b = Buffer.create 0 in
	Buffer.add_string b "<list>\n";
	Hashtbl.iter (fun k _ ->
		Buffer.add_string b (Printf.sprintf "<i n=\"%s\"></i>\n" k)
	) Lexer.keywords;
	Buffer.add_string b "</list>\n";
	Buffer.contents b

let print_fields fields =
	let b = Buffer.create 0 in
	Buffer.add_string b "<list>\n";
	let convert k = match k.ci_kind with
		| ITClassField({field = cf}) | ITEnumAbstractField(_,{field = cf}) ->
			let kind = match cf.cf_kind with
				| Method _ -> "method"
				| Var _ -> "var"
			in
			kind,cf.cf_name,s_type (print_context()) cf.cf_type,cf.cf_doc
		| ITEnumField ef ->
			let ef = ef.efield in
			let kind = match follow ef.ef_type with
				| TFun _ -> "method"
				| _ -> "var"
			in
			kind,ef.ef_name,s_type (print_context()) ef.ef_type,ef.ef_doc
		| ITType(cm,_) ->
			let path = CompletionItem.CompletionModuleType.get_path cm in
			"type",snd path,s_type_path path,None
		| ITPackage(path,_) -> "package",snd path,"",None
		| ITModule path -> "type",snd path,"",None
		| ITMetadata  meta ->
			let s,(doc,_) = Meta.get_info meta in
			"metadata","@" ^ s,"",doc_from_string doc
		| ITTimer(name,value) -> "timer",name,"",doc_from_string value
		| ITLiteral s ->
			let t = match k.ci_type with None -> t_dynamic | Some (t,_) -> t in
			"literal",s,s_type (print_context()) t,None
		| ITLocal v -> "local",v.v_name,s_type (print_context()) v.v_type,None
		| ITKeyword kwd -> "keyword",Ast.s_keyword kwd,"",None
		| ITExpression _ | ITAnonymous _ | ITTypeParameter _ | ITDefine _ -> die "" __LOC__
	in
	let fields = List.sort (fun k1 k2 -> compare (legacy_sort k1) (legacy_sort k2)) fields in
	let fields = List.map convert fields in
	List.iter (fun(k,n,t,d) ->
		let d = match d with None -> "" | Some d -> gen_doc_text d in
		Buffer.add_string b (Printf.sprintf "<i n=\"%s\" k=\"%s\"><t>%s</t><d>%s</d></i>\n" n k (htmlescape t) (htmlescape d))
	) fields;
	Buffer.add_string b "</list>\n";
	Buffer.contents b

let maybe_print_doc d_opt =
	Option.map_default (fun d -> Printf.sprintf " d=\"%s\"" (htmlescape (gen_doc_text d))) "" d_opt

let print_toplevel il =
	let b = Buffer.create 0 in
	Buffer.add_string b "<il>\n";
	let s_type t = htmlescape (s_type (print_context()) t) in
	let s_doc d = maybe_print_doc d in
	let identifiers = Hashtbl.create 0 in
	let check_ident s =
		if Hashtbl.mem identifiers s then false
		else begin
			Hashtbl.add identifiers s true;
			true
		end
	in
	List.iter (fun id -> match id.ci_kind with
		| ITLocal v ->
			if check_ident v.v_name then Buffer.add_string b (Printf.sprintf "<i k=\"local\" t=\"%s\">%s</i>\n" (s_type v.v_type) v.v_name);
		| ITClassField({field = cf;scope = CFSMember}) ->
			if check_ident cf.cf_name then Buffer.add_string b (Printf.sprintf "<i k=\"member\" t=\"%s\"%s>%s</i>\n" (s_type cf.cf_type) (s_doc cf.cf_doc) cf.cf_name);
		| ITClassField({field = cf;scope = (CFSStatic | CFSConstructor)}) ->
			if check_ident cf.cf_name then Buffer.add_string b (Printf.sprintf "<i k=\"static\" t=\"%s\"%s>%s</i>\n" (s_type cf.cf_type) (s_doc cf.cf_doc) cf.cf_name);
		| ITEnumField ef ->
			let ef = ef.efield in
			if check_ident ef.ef_name then Buffer.add_string b (Printf.sprintf "<i k=\"enum\" t=\"%s\"%s>%s</i>\n" (s_type ef.ef_type) (s_doc ef.ef_doc) ef.ef_name);
		| ITEnumAbstractField(a,cf) ->
			let cf = cf.field in
			if check_ident cf.cf_name then Buffer.add_string b (Printf.sprintf "<i k=\"enumabstract\" t=\"%s\"%s>%s</i>\n" (s_type cf.cf_type) (s_doc cf.cf_doc) cf.cf_name);
		| ITType(cm,_) ->
			let path = CompletionItem.CompletionModuleType.get_path cm in
			Buffer.add_string b (Printf.sprintf "<i k=\"type\" p=\"%s\"%s>%s</i>\n" (s_type_path path) ("") cm.name);
		| ITPackage(path,_) ->
			Buffer.add_string b (Printf.sprintf "<i k=\"package\">%s</i>\n" (snd path))
		| ITLiteral s ->
			Buffer.add_string b (Printf.sprintf "<i k=\"literal\">%s</i>\n" s)
		| ITTimer(s,_) ->
			Buffer.add_string b (Printf.sprintf "<i k=\"timer\">%s</i>\n" s)
		| ITTypeParameter c ->
			Buffer.add_string b (Printf.sprintf "<i k=\"type\" p=\"%s\"%s>%s</i>\n" (s_type_path c.cl_path) ("") (snd c.cl_path));
		| ITMetadata _ | ITModule _ | ITKeyword _ | ITAnonymous _ | ITExpression _ | ITDefine _ ->
			(* compat: don't add *)
			()
	) il;
	Buffer.add_string b "</il>";
	Buffer.contents b

let print_type t p doc =
	let b = Buffer.create 0 in
	if p = null_pos then
		Buffer.add_string b "<type"
	else begin
		let error_printer file line = Printf.sprintf "%s:%d:" (Path.get_full_path file) line in
		let epos = Lexer.get_error_pos error_printer p in
		Buffer.add_string b ("<type p=\"" ^ (htmlescape epos) ^ "\"")
	end;
	Buffer.add_string b (maybe_print_doc doc);
	Buffer.add_string b ">\n";
	Buffer.add_string b (htmlescape (s_type (print_context()) t));
	Buffer.add_string b "\n</type>\n";
	Buffer.contents b

let print_signatures tl =
	let b = Buffer.create 0 in
	List.iter (fun (((args,ret),_),doc) ->
		Buffer.add_string b "<type";
		Option.may (fun d -> Buffer.add_string b (Printf.sprintf " d=\"%s\"" (htmlescape (gen_doc_text d)))) doc;
		Buffer.add_string b ">\n";
		Buffer.add_string b (htmlescape (s_type (print_context()) (TFun(args,ret))));
		Buffer.add_string b "\n</type>\n";
	) tl;
	Buffer.contents b

let print_positions pl =
	let b = Buffer.create 0 in
	let error_printer file line = Printf.sprintf "%s:%d:" (Path.get_real_path file) line in
	Buffer.add_string b "<list>\n";
	List.iter (fun p ->
		let epos = Lexer.get_error_pos error_printer p in
		Buffer.add_string b "<pos>";
		Buffer.add_string b epos;
		Buffer.add_string b "</pos>\n";
	) pl;
	Buffer.add_string b "</list>";
	Buffer.contents b

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
				| Statics c -> c.cl_doc
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
		match com.Common.json_out with
		| None ->
			let b = Buffer.create 0 in
			Buffer.add_string b "<il>\n";
			List.iter (fun item -> match item.ci_kind with
				| ITKeyword kwd -> Buffer.add_string b (Printf.sprintf "<i k=\"keyword\">%s</i>" (s_keyword kwd));
				| _ -> die "" __LOC__
			) l;
			Buffer.add_string b "</il>";
			let s = Buffer.contents b in
			raise (Completion s)
		| Some api ->
			let ctx = Genjson.create_context ~jsonrpc:api.jsonrpc GMFull in
			api.send_result(fields_to_json ctx l kind subj)

let handle_display_exception_old ctx dex = match dex with
	| DisplayPackage pack ->
		DisplayPosition.display_position#reset;
		raise (Completion (String.concat "." pack))
	| DisplayFields r ->
		DisplayPosition.display_position#reset;
		let fields = if !Timer.measure_times then begin
			Timer.close_times();
			(List.map (fun (name,value) ->
				CompletionItem.make_ci_timer ("@TIME " ^ name) value
			) (get_timer_fields !Helper.start_time)) @ r.fitems
		end else
			r.fitems
		in
		let s = match r.fkind with
			| CRToplevel _
			| CRTypeHint
			| CRExtends
			| CRImplements
			| CRStructExtension _
			| CRImport
			| CRUsing
			| CRNew
			| CRPattern _
			| CRTypeRelation
			| CRTypeDecl ->
				print_toplevel fields
			| CRField _
			| CRStructureField
			| CRMetadata
			| CROverride ->
				print_fields fields
		in
		raise (Completion s)
	| DisplayHover ({hitem = {CompletionItem.ci_type = Some (t,_)}} as hover) ->
		DisplayPosition.display_position#reset;
		let doc = CompletionItem.get_documentation hover.hitem in
		raise (Completion (print_type t hover.hpos doc))
	| DisplaySignatures (signatures,_,display_arg,_) ->
		DisplayPosition.display_position#reset;
		if ctx.com.display.dms_kind = DMSignature then
			raise (Completion (print_signature signatures display_arg))
		else
			raise (Completion (print_signatures signatures))
	| DisplayPositions pl ->
		DisplayPosition.display_position#reset;
		raise (Completion (print_positions pl))
	| ModuleSymbols s | Metadata s ->
		DisplayPosition.display_position#reset;
		raise (Completion s)
	| DisplayHover _ | DisplayNoResult ->
		raise (Completion "")

let handle_display_exception_json ctx dex api =
	match dex with
	| DisplayHover _ | DisplayPositions _ | DisplayFields _ | DisplayPackage _  | DisplaySignatures _ ->
		DisplayPosition.display_position#reset;
		let ctx = DisplayJson.create_json_context api.jsonrpc (match dex with DisplayFields _ -> true | _ -> false) in
		api.send_result (DisplayException.to_json ctx dex)
	| DisplayNoResult ->
		api.send_result JNull
	| _ ->
		handle_display_exception_old ctx dex

let handle_display_exception ctx dex = match ctx.com.json_out with
	| Some api ->
		handle_display_exception_json ctx dex api
	| None ->
		handle_display_exception_old ctx dex

let handle_type_path_exception ctx p c is_import pos =
	let open DisplayTypes.CompletionResultKind in
	let com = ctx.com in
	let fields =
		try begin match c with
			| None ->
				DisplayPath.TypePathHandler.complete_type_path com p
			| Some (c,cur_package) ->
				let ctx = Typer.create com in
				DisplayPath.TypePathHandler.complete_type_path_inner ctx p c cur_package is_import
		end with Common.Abort(msg,p) ->
			error ctx msg p;
			None
	in
	begin match ctx.com.json_out,fields with
	| None,None ->
		()
	| None,Some fields ->
		raise (Completion (print_fields fields))
	| Some api,None when is_legacy_completion com ->
		api.send_result JNull
	| Some api,fields ->
		let fields = Option.default [] fields in
		let ctx = DisplayJson.create_json_context api.jsonrpc false in
		let path = match List.rev p with
			| name :: pack -> List.rev pack,name
			| [] -> [],""
		in
		let kind = CRField ((CompletionItem.make_ci_module path,pos,None,None)) in
		api.send_result (DisplayException.fields_to_json ctx fields kind (DisplayTypes.make_subject None pos));
	end

let emit_diagnostics com =
	let dctx = Diagnostics.run com in
	let s = Json.string_of_json (DiagnosticsPrinter.json_of_diagnostics com dctx) in
	DisplayPosition.display_position#reset;
	raise (Completion s)

let emit_statistics tctx =
	let stats = Statistics.collect_statistics tctx [SFFile (DisplayPosition.display_position#get).pfile] true in
	let s = Statistics.Printer.print_statistics stats in
	raise (Completion s)