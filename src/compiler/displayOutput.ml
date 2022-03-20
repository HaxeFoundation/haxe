open Globals
open Ast
open Common
open Filename
open CompilationContext
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

exception Completion of string

let unquote v =
	let len = String.length v in
	if len > 0 then
		match v.[0], v.[len - 1] with
			| '"', '"'
			| '\'', '\'' -> String.sub v 1 (len - 2)
			| _ -> v
	else v

let handle_display_argument com file_pos actx =
	match file_pos with
	| "classes" ->
		actx.pre_compilation <- (fun() -> raise (Parser.TypePath (["."],None,true,null_pos))) :: actx.pre_compilation;
	| "keywords" ->
		raise (Completion (print_keywords ()))
	| "memory" ->
		actx.did_something <- true;
		(try Memory.display_memory com with e -> prerr_endline (Printexc.get_backtrace ()));
	| "diagnostics" ->
		com.report_mode <- RMDiagnostics []
	| _ ->
		let file, pos = try ExtString.String.split file_pos "@" with _ -> failwith ("Invalid format: " ^ file_pos) in
		let file = unquote file in
		let file_unique = com.file_keys#get file in
		let pos, smode = try ExtString.String.split pos "@" with _ -> pos,"" in
		let create mode =
			Parser.display_mode := mode;
			DisplayMode.create mode
		in
		let dm = match smode with
			| "position" ->
				create DMDefinition
			| "usage" ->
				create (DMUsage (false,false,false))
			| "package" ->
				create DMPackage
			| "type" ->
				create DMHover
			| "toplevel" ->
				create DMDefault
			| "module-symbols" ->
				create (DMModuleSymbols None)
			| "diagnostics" ->
				com.report_mode <- RMDiagnostics [file_unique];
				let dm = create DMNone in
				{dm with dms_display_file_policy = DFPAlso}
			| "statistics" ->
				com.report_mode <- RMStatistics;
				let dm = create DMNone in
				{dm with dms_display_file_policy = DFPAlso; dms_error_policy = EPIgnore}
			| "signature" ->
				create DMSignature
			| "" ->
				create DMDefault
			| _ ->
				let smode,arg = try ExtString.String.split smode "@" with _ -> pos,"" in
				match smode with
					| "workspace-symbols" ->
						create (DMModuleSymbols (Some arg))
					| _ ->
						create DMDefault
		in
		let pos = try int_of_string pos with _ -> failwith ("Invalid format: "  ^ pos) in
		com.display <- dm;
		if not com.display.dms_full_typing then Common.define_value com Define.Display (if smode <> "" then smode else "1");
		DisplayPosition.display_position#set {
			pfile = Path.get_full_path file;
			pmin = pos;
			pmax = pos;
		}

let file_input_marker = Path.get_full_path "? input"

type display_path_kind =
	| DPKNormal of path
	| DPKMacro of path
	| DPKDirect of string
	| DPKInput of string
	| DPKNone

let process_display_file com actx =
	let get_module_path_from_file_path com spath =
		let rec loop = function
			| [] -> None
			| cp :: l ->
				let cp = (if cp = "" then "./" else cp) in
				let c = Path.add_trailing_slash (Path.get_real_path cp) in
				let clen = String.length c in
				if clen < String.length spath && String.sub spath 0 clen = c then begin
					let path = String.sub spath clen (String.length spath - clen) in
					(try
						let path = Path.parse_path path in
						(match loop l with
						| Some x as r when String.length (s_type_path x) < String.length (s_type_path path) -> r
						| _ -> Some path)
					with _ -> loop l)
				end else
					loop l
		in
		loop com.class_path
	in
	match com.display.dms_display_file_policy with
		| DFPNo ->
			DPKNone
		| DFPOnly when (DisplayPosition.display_position#get).pfile = file_input_marker ->
			actx.classes <- [];
			com.main_class <- None;
			begin match !TypeloadParse.current_stdin with
			| Some input ->
				TypeloadParse.current_stdin := None;
				DPKInput input
			| None ->
				DPKNone
			end
		| dfp ->
			if dfp = DFPOnly then begin
				actx.classes <- [];
				com.main_class <- None;
			end;
			let real = Path.get_real_path (DisplayPosition.display_position#get).pfile in
			let path = match get_module_path_from_file_path com real with
			| Some path ->
				if com.display.dms_kind = DMPackage then raise_package (fst path);
				let path = match ExtString.String.nsplit (snd path) "." with
					| [name;"macro"] ->
						(* If we have a .macro.hx path, don't add the file to classes because the compiler won't find it.
						   This can happen if we're completing in such a file. *)
						DPKMacro (fst path,name)
					| [name] ->
						actx.classes <- path :: actx.classes;
						DPKNormal path
					| [name;target] ->
						let path = fst path, name in
						actx.classes <- path :: actx.classes;
						DPKNormal path
					| e ->
						die "" __LOC__
				in
				path
			| None ->
				if not (Sys.file_exists real) then failwith "Display file does not exist";
				(match List.rev (ExtString.String.nsplit real Path.path_sep) with
				| file :: _ when file.[0] >= 'a' && file.[0] <= 'z' -> failwith ("Display file '" ^ file ^ "' should not start with a lowercase letter")
				| _ -> ());
				DPKDirect real
			in
			Common.log com ("Display file : " ^ real);
			Common.log com ("Classes found : ["  ^ (String.concat "," (List.map s_type_path actx.classes)) ^ "]");
			path

let load_display_file_standalone ctx file =
	let com = ctx.com in
	let pack,decls = TypeloadParse.parse_module_file com file null_pos in
	let path = Path.FilePath.parse file in
	let name = match path.file_name with
		| None -> "?DISPLAY"
		| Some name -> name
	in
	begin match path.directory with
		| None -> ()
		| Some dir ->
			(* Chop off number of package parts from the dir and use that as class path. *)
			let parts = ExtString.String.nsplit dir (if path.backslash then "\\" else "/") in
			let parts = List.rev (ExtList.List.drop (List.length pack) (List.rev parts)) in
			let dir = ExtString.String.join (if path.backslash then "\\" else "/") parts in
			com.class_path <- dir :: com.class_path
	end;
	ignore(TypeloadModule.type_module ctx (pack,name) file ~dont_check_path:true decls null_pos)

let load_display_content_standalone ctx input =
	let com = ctx.com in
	let file = file_input_marker in
	let p = {pfile = file; pmin = 0; pmax = 0} in
	let parsed = TypeloadParse.parse_file_from_string com file p input in
	let pack,decls = TypeloadParse.handle_parser_result com p parsed in
	ignore(TypeloadModule.type_module ctx (pack,"?DISPLAY") file ~dont_check_path:true decls p)

let promote_type_hints tctx =
	let rec explore_type_hint (md,p,t) =
		match t with
		| TMono r -> (match r.tm_type with None -> () | Some t -> explore_type_hint (md,p,t))
		| TLazy f -> explore_type_hint (md,p,lazy_type f)
		| TInst(({cl_name_pos = pn;cl_path = (_,name)}),_)
		| TEnum(({e_name_pos = pn;e_path = (_,name)}),_)
		| TType(({t_name_pos = pn;t_path = (_,name)}),_)
		| TAbstract(({a_name_pos = pn;a_path = (_,name)}),_) ->
			md.m_type_hints <- (p,pn) :: md.m_type_hints;
		| TDynamic _ -> ()
		| TFun _ | TAnon _ -> ()
	in
	List.iter explore_type_hint tctx.g.type_hints

let process_global_display_mode com tctx =
	promote_type_hints tctx;
	match com.display.dms_kind with
	| DMUsage (with_definition,_,_) ->
		FindReferences.find_references tctx com with_definition
	| DMImplementation ->
		FindReferences.find_implementations tctx com
	| DMModuleSymbols (Some "") -> ()
	| DMModuleSymbols filter ->
		let open CompilationCache in
		let cs = com.cs in
		let symbols =
			let l = cs#get_context_files ((Define.get_signature com.defines) :: (match com.get_macros() with None -> [] | Some com -> [Define.get_signature com.defines])) in
			List.fold_left (fun acc (file_key,cfile) ->
				let file = cfile.c_file_path in
				if (filter <> None || DisplayPosition.display_position#is_in_file (com.file_keys#get file)) then
					(file,DocumentSymbols.collect_module_symbols (Some (file,get_module_name_of_cfile file cfile)) (filter = None) (cfile.c_package,cfile.c_decls)) :: acc
				else
					acc
			) [] l
		in
		raise_module_symbols (DocumentSymbols.Printer.print_module_symbols com symbols filter)
	| _ -> ()

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