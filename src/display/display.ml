open Ast
open Common
open Common.DisplayMode
open Type
open Typecore

type display_field_kind =
	| FKVar of t
	| FKMethod of t
	| FKType of t
	| FKModule
	| FKPackage
	| FKMetadata
	| FKTimer of string

let display_field_kind_index = function
	| FKVar _ -> 0
	| FKMethod _ -> 1
	| FKType _ -> 2
	| FKModule -> 3
	| FKPackage -> 4
	| FKMetadata -> 5
	| FKTimer _ -> 6

exception Diagnostics of string
exception Statistics of string
exception ModuleSymbols of string
exception Metadata of string
exception DisplaySignatures of (t * documentation) list
exception DisplayType of t * pos * string option
exception DisplayPosition of Ast.pos list
exception DisplayFields of (string * display_field_kind * documentation) list
exception DisplayToplevel of IdentifierType.t list
exception DisplayPackage of string list
exception DisplaySignature of string

let is_display_file file =
	file <> "?" && Path.unique_full_path file = (!Parser.resume_display).pfile

let encloses_position p_target p =
	p.pmin <= p_target.pmin && p.pmax >= p_target.pmax

let is_display_position p =
	encloses_position !Parser.resume_display p

let find_enclosing com e =
	let display_pos = ref (!Parser.resume_display) in
	let mk_null p = (EDisplay(((EConst(Ident "null")),p),false),p) in
	let encloses_display_pos p =
		if encloses_position !display_pos p then begin
			let p = !display_pos in
			display_pos := { pfile = ""; pmin = -2; pmax = -2 };
			Some p
		end else
			None
	in
	let rec loop e = match fst e with
		| EBlock el ->
			let p = pos e in
			(* We want to find the innermost block which contains the display position. *)
			let el = List.map loop el in
			let el = match encloses_display_pos p with
				| None ->
					el
				| Some p2 ->
					let b,el = List.fold_left (fun (b,el) e ->
						let p = pos e in
						if b || p.pmax <= p2.pmin then begin
							(b,e :: el)
						end else begin
							let e_d = (EDisplay(mk_null p,false)),p in
							(true,e :: e_d :: el)
						end
					) (false,[]) el in
					let el = if b then
						el
					else begin
						mk_null p :: el
					end in
					List.rev el
			in
			(EBlock el),(pos e)
		| _ ->
			Ast.map_expr loop e
	in
	loop e

let find_before_pos com e =
	let display_pos = ref (!Parser.resume_display) in
	let is_annotated p =
		if p.pmin <= !display_pos.pmin && p.pmax >= !display_pos.pmax then begin
			display_pos := { pfile = ""; pmin = -2; pmax = -2 };
			true
		end else
			false
	in
	let loop e =
		if is_annotated (pos e) then
			(EDisplay(e,false),(pos e))
		else
			e
	in
	let rec map e =
		loop (Ast.map_expr map e)
	in
	map e

let find_display_call e =
	let found = ref false in
	let loop e = if !found then e else match fst e with
		| ECall _ | ENew _ when is_display_position (pos e) ->
			found := true;
			(EDisplay(e,true),(pos e))
		| _ ->
			e
	in
	let rec map e = match fst e with
		| EDisplay(_,true) ->
			found := true;
			e
		| EDisplay(e1,false) -> map e1
		| _ -> loop (Ast.map_expr map e)
	in
	map e

let display_module_type dm mt p = match dm.dms_kind with
	| DMPosition -> raise (DisplayPosition [(t_infos mt).mt_pos]);
	| DMUsage _ ->
		let ti = t_infos mt in
		ti.mt_meta <- (Meta.Usage,[],ti.mt_pos) :: ti.mt_meta
	| DMType -> raise (DisplayType (type_of_module_type mt,p,None))
	| _ -> ()

let rec display_type dm t p = match dm.dms_kind with
	| DMType -> raise (DisplayType (t,p,None))
	| _ ->
		try display_module_type dm (module_type_of_type t) p
		with Exit -> match follow t,follow !t_dynamic_def with
			| _,TDynamic _ -> () (* sanity check in case it's still t_dynamic *)
			| TDynamic _,_ -> display_type dm !t_dynamic_def p
			| _ -> ()

let check_display_type ctx t p =
	let add_type_hint () =
		Hashtbl.replace ctx.com.shared.shared_display_information.type_hints p t;
	in
	let maybe_display_type () =
		if ctx.is_display_file && is_display_position p then
			display_type ctx.com.display t p
	in
	match ctx.com.display.dms_kind with
	| DMStatistics -> add_type_hint()
	| DMUsage _ -> add_type_hint(); maybe_display_type()
	| _ -> maybe_display_type()

let display_variable dm v p = match dm.dms_kind with
	| DMPosition -> raise (DisplayPosition [v.v_pos])
	| DMUsage _ -> v.v_meta <- (Meta.Usage,[],v.v_pos) :: v.v_meta;
	| DMType -> raise (DisplayType (v.v_type,p,None))
	| _ -> ()

let display_field dm cf p = match dm.dms_kind with
	| DMPosition -> raise (DisplayPosition [cf.cf_pos]);
	| DMUsage _ -> cf.cf_meta <- (Meta.Usage,[],cf.cf_pos) :: cf.cf_meta;
	| DMType -> raise (DisplayType (cf.cf_type,p,cf.cf_doc))
	| _ -> ()

let display_enum_field dm ef p = match dm.dms_kind with
	| DMPosition -> raise (DisplayPosition [p]);
	| DMUsage _ -> ef.ef_meta <- (Meta.Usage,[],p) :: ef.ef_meta;
	| DMType -> raise (DisplayType (ef.ef_type,p,ef.ef_doc))
	| _ -> ()

let get_timer_fields start_time =
	let tot = ref 0. in
	Hashtbl.iter (fun _ t -> tot := !tot +. t.total) Common.htimers;
	let fields = [("@TOTAL", FKTimer (Printf.sprintf "%.3fs" (get_time() -. start_time)), "")] in
	if !tot > 0. then
		Hashtbl.fold (fun _ t acc ->
			("@TIME " ^ t.name, FKTimer (Printf.sprintf "%.3fs (%.0f%%)" t.total (t.total *. 100. /. !tot)), "") :: acc
		) Common.htimers fields
	else
		fields

open Json

let htmlescape s =
	let s = String.concat "&amp;" (ExtString.String.nsplit s "&") in
	let s = String.concat "&lt;" (ExtString.String.nsplit s "<") in
	let s = String.concat "&gt;" (ExtString.String.nsplit s ">") in
	let s = String.concat "&quot;" (ExtString.String.nsplit s "\"") in
	s

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
	List.iter (fun (n,k,d) ->
		let s_kind, t = match k with
			| FKVar t -> "var", s_type (print_context()) t
			| FKMethod t -> "method", s_type (print_context()) t
			| FKType t -> "type", s_type (print_context()) t
			| FKPackage -> "package", ""
			| FKModule -> "type", ""
			| FKMetadata -> "metadata", ""
			| FKTimer s -> "", s
		in
		Buffer.add_string b (Printf.sprintf "<i n=\"%s\" k=\"%s\"><t>%s</t><d>%s</d></i>\n" n s_kind (htmlescape t) (htmlescape d))
	) (List.sort (fun (a,ak,_) (b,bk,_) -> compare (display_field_kind_index ak,a) (display_field_kind_index bk,b)) fields);
	Buffer.add_string b "</list>\n";
	Buffer.contents b

let maybe_print_doc d =
	Option.map_default (fun s -> Printf.sprintf " d=\"%s\"" (htmlescape s)) "" d

let print_toplevel il =
	let b = Buffer.create 0 in
	Buffer.add_string b "<il>\n";
	let s_type t = htmlescape (s_type (print_context()) t) in
	let s_doc d = maybe_print_doc d in
	List.iter (fun id -> match id with
		| IdentifierType.ITLocal v ->
			Buffer.add_string b (Printf.sprintf "<i k=\"local\" t=\"%s\">%s</i>\n" (s_type v.v_type) v.v_name);
		| IdentifierType.ITMember(c,cf) ->
			Buffer.add_string b (Printf.sprintf "<i k=\"member\" t=\"%s\"%s>%s</i>\n" (s_type cf.cf_type) (s_doc cf.cf_doc) cf.cf_name);
		| IdentifierType.ITStatic(c,cf) ->
			Buffer.add_string b (Printf.sprintf "<i k=\"static\" t=\"%s\"%s>%s</i>\n" (s_type cf.cf_type) (s_doc cf.cf_doc) cf.cf_name);
		| IdentifierType.ITEnum(en,ef) ->
			Buffer.add_string b (Printf.sprintf "<i k=\"enum\" t=\"%s\"%s>%s</i>\n" (s_type ef.ef_type) (s_doc ef.ef_doc) ef.ef_name);
		| IdentifierType.ITEnumAbstract(a,cf) ->
			Buffer.add_string b (Printf.sprintf "<i k=\"enumabstract\" t=\"%s\"%s>%s</i>\n" (s_type cf.cf_type) (s_doc cf.cf_doc) cf.cf_name);
		| IdentifierType.ITGlobal(mt,s,t) ->
			Buffer.add_string b (Printf.sprintf "<i k=\"global\" p=\"%s\" t=\"%s\">%s</i>\n" (s_type_path (t_infos mt).mt_path) (s_type t) s);
		| IdentifierType.ITType(mt) ->
			let infos = t_infos mt in
			Buffer.add_string b (Printf.sprintf "<i k=\"type\" p=\"%s\"%s>%s</i>\n" (s_type_path infos.mt_path) (s_doc infos.mt_doc) (snd infos.mt_path));
		| IdentifierType.ITPackage s ->
			Buffer.add_string b (Printf.sprintf "<i k=\"package\">%s</i>\n" s)
	) il;
	Buffer.add_string b "</il>";
	Buffer.contents b

let print_type t p doc =
	let b = Buffer.create 0 in
	if p = null_pos then
		Buffer.add_string b "<type"
	else begin
		let error_printer file line = Printf.sprintf "%s:%d:" (Path.unique_full_path file) line in
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
	List.iter (fun (t,doc) ->
		Buffer.add_string b "<type";
		Option.may (fun s -> Buffer.add_string b (Printf.sprintf " d=\"%s\"" (htmlescape s))) doc;
		Buffer.add_string b ">\n";
		Buffer.add_string b (htmlescape (s_type (print_context()) (follow t)));
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

(** return a range JSON structure for given position
    positions are 0-based and the result object looks like this:
    {
        start: {line: 0, character: 0},
        end: {line: 3, character: 42},
    }
*)
let pos_to_json_range p =
	if p.pmin = -1 then
		JNull
	else
		let l1, p1, l2, p2 = Lexer.get_pos_coords p in
		let to_json l c = JObject [("line", JInt (l - 1)); ("character", JInt c)] in
		JObject [
			("start", to_json l1 p1);
			("end", to_json l2 p2);
		]

module DocumentSymbols = struct
	open DisplayTypes.SymbolKind
	open DisplayTypes.SymbolInformation
	open Json

	let collect_module_symbols (pack,decls) =
		let l = DynArray.create() in
		let add name kind location parent =
			let si = make name kind location (if parent = "" then None else Some parent) in
			DynArray.add l si;
		in
		let rec expr parent (e,p) =
			let add name kind location = add name kind location parent in
			begin match e with
			| EVars vl ->
				List.iter (fun ((s,p),_,eo) ->
					add s Variable p;
					expr_opt parent eo
				) vl
			| ETry(e1,catches) ->
				expr parent e1;
				List.iter (fun ((s,p),_,e,_) ->
					add s Variable p;
					expr parent e
				) catches;
			| EFunction(Some s,f) ->
				add s Function p;
				func parent f
			| EIn((EConst(Ident s),p),e2) ->
				add s Variable p;
				expr parent e2;
			| _ ->
				iter_expr (expr parent) (e,p)
			end
		and expr_opt parent eo = match eo with
			| None -> ()
			| Some e -> expr parent e
		and func parent f =
			List.iter (fun ((s,p),_,_,_,eo) ->
				add s Variable p parent;
				expr_opt parent eo
			) f.f_args;
			expr_opt parent f.f_expr
		in
		let field parent cff =
			let field_parent = parent ^ "." ^ (fst cff.cff_name) in
			match cff.cff_kind with
			| FVar(_,eo) ->
				add (fst cff.cff_name) Field cff.cff_pos parent;
				expr_opt field_parent eo
			| FFun f ->
				add (fst cff.cff_name) (if fst cff.cff_name = "new" then Constructor else Method) cff.cff_pos parent;
				func field_parent f
			| FProp(_,_,_,eo) ->
				add (fst cff.cff_name) Property cff.cff_pos parent;
				expr_opt field_parent eo
		in
		List.iter (fun (td,p) -> match td with
			| EImport _ | EUsing _ ->
				() (* TODO: Can we do anything with these? *)
			| EClass d ->
				add (fst d.d_name) (if List.mem HInterface d.d_flags then Interface else Class) p "";
				List.iter (field (fst d.d_name)) d.d_data
			| EEnum d ->
				add (fst d.d_name) Enum p "";
				List.iter (fun ef ->
					add (fst ef.ec_name) Method ef.ec_pos (fst d.d_name)
				) d.d_data
			| ETypedef d ->
				add (fst d.d_name) Typedef p "";
				(match d.d_data with
				| CTAnonymous fields,_ ->
					List.iter (field (fst d.d_name)) fields
				| _ -> ())
			| EAbstract d ->
				add (fst d.d_name) Abstract p "";
				List.iter (field (fst d.d_name)) d.d_data
		) decls;
		l

	let print_module_symbols com symbols filter =
		let regex = Option.map Str.regexp_case_fold filter in
		let reported = Hashtbl.create 0 in
		let add si =
			if Hashtbl.mem reported si.pos then false
			else begin
				let b = match regex with
					| None -> true
					| Some regex -> (try ignore(Str.search_forward regex si.name 0); true with Not_found -> false)
				in
				Hashtbl.replace reported si.pos true;
				b
			end
		in
		let ja = List.fold_left (fun acc (file,l) ->
			let jl = ExtList.List.filter_map (fun si ->
				if not (add si) then
					None
				else begin
					let l =
						("name",JString si.name) ::
						("kind",JInt (to_int si.kind)) ::
						("range", pos_to_json_range si.pos) ::
						(match si.container_name with None -> [] | Some s -> ["containerName",JString s])
					in
					Some (JObject l)
				end
			) (DynArray.to_list l) in
			if jl = [] then
				acc
			else
				(JObject [
					"file",JString file;
					"symbols",JArray jl
				]) :: acc
		) [] symbols in
		let js = JArray ja in
		let b = Buffer.create 0 in
		write_json (Buffer.add_string b) js;
		Buffer.contents b
end

type import_display_kind =
	| IDKPackage of string list
	| IDKModule of string list * string
	| IDKSubType of string list * string * string
	| IDKModuleField of string list * string * string
	| IDKSubTypeField of string list * string * string * string
	| IDK

type import_display = import_display_kind * pos

let convert_import_to_something_usable pt path =
	let rec loop pack m t = function
		| (s,p) :: l ->
			let is_lower = is_lower_ident s in
			let is_display_pos = encloses_position pt p in
			begin match is_lower,m,t with
				| _,None,Some _ ->
					assert false (* impossible, I think *)
				| true,Some m,None ->
					if is_display_pos then (IDKModuleField(List.rev pack,m,s),p)
					else (IDK,p) (* assume that we're done *)
				| _,Some m,Some t ->
					if is_display_pos then (IDKSubTypeField(List.rev pack,m,t,s),p)
					else (IDK,p)
				| true,None,None ->
					if is_display_pos then (IDKPackage (List.rev (s :: pack)),p)
					else loop (s :: pack) m t l
				| false,Some sm,None ->
					if is_display_pos then (IDKSubType (List.rev pack,sm,s),p)
					else loop pack m (Some s) l
				| false,None,None ->
					if is_display_pos then (IDKModule (List.rev pack,s),p)
					else loop pack (Some s) None l
			end
		| [] ->
			(IDK,null_pos)
	in
	loop [] None None path

let process_expr com e = match com.display.dms_kind with
	| DMToplevel -> find_enclosing com e
	| DMPosition | DMUsage _ | DMType -> find_before_pos com e
	| DMSignature -> find_display_call e
	| _ -> e

let add_import_position com p path =
	com.shared.shared_display_information.import_positions <- PMap.add p (ref false,path) com.shared.shared_display_information.import_positions

let mark_import_position com p =
	try
		let r = fst (PMap.find p com.shared.shared_display_information.import_positions) in
		r := true
	with Not_found ->
		()

module Diagnostics = struct
	module DiagnosticsKind = struct
		type t =
			| DKUnusedImport
			| DKUnresolvedIdentifier
			| DKCompilerError

		let to_int = function
			| DKUnusedImport -> 0
			| DKUnresolvedIdentifier -> 1
			| DKCompilerError -> 2
	end

	type t = DiagnosticsKind.t * pos

	module UnresolvedIdentifierSuggestion = struct
		type t =
			| UISImport
			| UISTypo

		let to_int = function
			| UISImport -> 0
			| UISTypo -> 1
	end

	open UnresolvedIdentifierSuggestion
	open DiagnosticsKind
	open DisplayTypes

	let find_unused_variables com e =
		let vars = Hashtbl.create 0 in
		let rec loop e = match e.eexpr with
			| TVar(v,eo) when Meta.has Meta.UserVariable v.v_meta ->
				Hashtbl.replace vars v.v_id v;
				(match eo with None -> () | Some e -> loop e)
			| TLocal v when Meta.has Meta.UserVariable v.v_meta ->
				Hashtbl.remove vars v.v_id;
			| _ ->
				Type.iter loop e
		in
		loop e;
		Hashtbl.iter (fun _ v ->
			add_diagnostics_message com "Unused variable" v.v_pos DiagnosticsSeverity.Warning
		) vars

	let check_other_things com e =
		let had_effect = ref false in
		let no_effect p =
			add_diagnostics_message com "This code has no effect" p DiagnosticsSeverity.Warning;
		in
		let pointless_compound s p =
			add_diagnostics_message com (Printf.sprintf "This %s has no effect, but some of its sub-expressions do" s) p DiagnosticsSeverity.Warning;
		in
		let rec compound s el p =
			let old = !had_effect in
			had_effect := false;
			List.iter (loop true) el;
			if not !had_effect then no_effect p else pointless_compound s p;
			had_effect := old;
		and loop in_value e = match e.eexpr with
			| TBlock el ->
				let rec loop2 el = match el with
					| [] -> ()
					| [e] -> loop in_value e
					| e :: el -> loop false e; loop2 el
				in
				loop2 el
			| TMeta((Meta.Extern,_,_),_) ->
				(* This is so something like `[inlineFunc()]` is not reported. *)
				had_effect := true;
			| TLocal v when not (Meta.has Meta.UserVariable v.v_meta) ->
				()
			| TConst _ | TLocal _ | TTypeExpr _ | TFunction _ when not in_value ->
				no_effect e.epos;
			| TConst _ | TLocal _ | TTypeExpr _ | TEnumParameter _ | TVar _ ->
				()
			| TFunction tf ->
				loop false tf.tf_expr
			| TNew _ | TCall _ | TBinop ((Ast.OpAssignOp _ | Ast.OpAssign),_,_) | TUnop ((Ast.Increment | Ast.Decrement),_,_)
			| TReturn _ | TBreak | TContinue | TThrow _ | TCast (_,Some _)
			| TIf _ | TTry _ | TSwitch _ | TWhile _ | TFor _ ->
				had_effect := true;
				Type.iter (loop true) e
			| TParenthesis e1 | TMeta(_,e1) ->
				loop in_value e1
			| TArray _ | TCast (_,None) | TBinop _ | TUnop _
			| TField _ | TArrayDecl _ | TObjectDecl _ when in_value ->
				Type.iter (loop true) e;
			| TArray(e1,e2) -> compound "array access" [e1;e2] e.epos
			| TCast(e1,None) -> compound "cast" [e1] e.epos
			| TBinop(op,e1,e2) -> compound (Printf.sprintf "'%s' operator" (s_binop op)) [e1;e2] e.epos
			| TUnop(op,_,e1) -> compound (Printf.sprintf "'%s' operator" (s_unop op)) [e1] e.epos
			| TField(e1,_) -> compound "field access" [e1] e.epos
			| TArrayDecl el -> compound "array declaration" el e.epos
			| TObjectDecl fl -> compound "object declaration" (List.map snd fl) e.epos
		in
		loop true e

	let prepare_field com cf = match cf.cf_expr with
		| None -> ()
		| Some e ->
			find_unused_variables com e;
			check_other_things com e

	let prepare com global =
		List.iter (function
			| TClassDecl c when global || is_display_file c.cl_pos.pfile ->
				List.iter (prepare_field com) c.cl_ordered_fields;
				List.iter (prepare_field com) c.cl_ordered_statics;
				(match c.cl_constructor with None -> () | Some cf -> prepare_field com cf);
			| _ ->
				()
		) com.types

	let print_diagnostics ctx global =
		let com = ctx.com in
		let diag = Hashtbl.create 0 in
		let add dk p sev args =
			let file = Path.get_real_path p.pfile in
			let diag = try
				Hashtbl.find diag file
			with Not_found ->
				let d = DynArray.create() in
				Hashtbl.add diag file d;
				d
			in
			DynArray.add diag (dk,p,sev,args)
		in
		let add dk p sev args =
			if global || is_display_file p.pfile then add dk p sev args
		in
		let find_type i =
			let types = ref [] in
			Hashtbl.iter (fun _ m ->
				List.iter (fun mt ->
					let s_full_type_path (p,s) n = s_type_path (p,s) ^ if (s <> n) then "." ^ n else "" in
					let tinfos = t_infos mt in
					if snd tinfos.mt_path = i then
						types := JObject [
							"kind",JInt (UnresolvedIdentifierSuggestion.to_int UISImport);
							"name",JString (s_full_type_path m.m_path i)
						] :: !types
				) m.m_types;
			) ctx.g.modules;
			!types
		in
		List.iter (fun (s,p,suggestions) ->
			let suggestions = List.map (fun (s,_) ->
				JObject [
					"kind",JInt (UnresolvedIdentifierSuggestion.to_int UISTypo);
					"name",JString s
				]
			) suggestions in
			add DKUnresolvedIdentifier p DiagnosticsSeverity.Error (suggestions @ (find_type s));
		) com.display_information.unresolved_identifiers;
		PMap.iter (fun p (r,_) ->
			if not !r then add DKUnusedImport p DiagnosticsSeverity.Warning []
		) com.shared.shared_display_information.import_positions;
		List.iter (fun (s,p,sev) ->
			add DKCompilerError p sev [JString s]
		) com.shared.shared_display_information.diagnostics_messages;
		let jl = Hashtbl.fold (fun file diag acc ->
			let jl = DynArray.fold_left (fun acc (dk,p,sev,args) ->
				(JObject [
					"kind",JInt (to_int dk);
					"severity",JInt (DiagnosticsSeverity.to_int sev);
					"range",pos_to_json_range p;
					"args",JArray args
				]) :: acc
			) [] diag in
			(JObject [
				"file",JString file;
				"diagnostics",JArray jl
			]) :: acc
		) diag [] in
		let js = JArray jl in
		let b = Buffer.create 0 in
		write_json (Buffer.add_string b) js;
		Buffer.contents b

	let is_diagnostics_run ctx = match ctx.com.display.dms_kind with
		| DMDiagnostics true -> true
		| DMDiagnostics false -> ctx.is_display_file
		| _ -> false

	let secure_generated_code ctx e =
		if is_diagnostics_run ctx then mk (TMeta((Meta.Extern,[],e.epos),e)) e.etype e.epos else e
end

let maybe_mark_import_position ctx p =
	if Diagnostics.is_diagnostics_run ctx then mark_import_position ctx.com p

module Statistics = struct
	type relation =
		| Implemented
		| Extended
		| Overridden
		| Referenced

	type symbol =
		| SKClass of tclass
		| SKInterface of tclass
		| SKEnum of tenum
		| SKField of tclass_field
		| SKEnumField of tenum_field
		| SKVariable of tvar

	let is_usage_symbol symbol =
		let meta = match symbol with
			| SKClass c | SKInterface c -> c.cl_meta
			| SKEnum en -> en.e_meta
			| SKField cf -> cf.cf_meta
			| SKEnumField ef -> ef.ef_meta
			| SKVariable v -> v.v_meta
		in
		Meta.has Meta.Usage meta

	let collect_statistics ctx =
		let relations = Hashtbl.create 0 in
		let symbols = Hashtbl.create 0 in
		let add_relation pos r =
			if is_display_file pos.pfile then try
				Hashtbl.replace relations pos (r :: Hashtbl.find relations pos)
			with Not_found ->
				Hashtbl.add relations pos [r]
		in
		let declare kind p =
			if is_display_file p.pfile then begin
				if not (Hashtbl.mem relations p) then Hashtbl.add relations p [];
				Hashtbl.replace symbols p kind;
			end
		in
		let collect_overrides c =
			List.iter (fun cf ->
				let rec loop c = match c.cl_super with
					| Some (c,_) ->
						begin try
							let cf' = PMap.find cf.cf_name c.cl_fields in
							add_relation cf'.cf_name_pos (Overridden,cf.cf_pos)
						with Not_found ->
							loop c
						end
					| _ ->
						()
				in
				loop c
			) c.cl_overrides
		in
		let rec find_real_constructor c = match c.cl_constructor,c.cl_super with
			(* The pos comparison might be a bit weak, not sure... *)
			| Some cf,_ when not (Meta.has Meta.CompilerGenerated cf.cf_meta) && c.cl_pos <> cf.cf_pos -> cf
			| _,Some(c,_) -> find_real_constructor c
			| _,None -> raise Not_found
		in
		let var_decl v = declare (SKVariable v) v.v_pos in
		let patch_string_pos p s = { p with pmin = p.pmax - String.length s } in
		let patch_string_pos_front p s  = { p with pmax = p.pmin + String.length s } in
		let field_reference cf p =
			add_relation cf.cf_name_pos (Referenced,patch_string_pos p cf.cf_name)
		in
		let collect_references c e =
			let rec loop e = match e.eexpr with
				| TField(e1,fa) ->
					(* Check if the sub-expression is actually shorter than the whole one. This should
					   detect cases where it was automatically generated. *)
					if e1.epos.pmin = e.epos.pmin && e1.epos.pmax <> e.epos.pmax then
						loop e1;
					begin match fa with
						| FStatic(_,cf) | FInstance(_,_,cf) | FClosure(_,cf) ->
							field_reference cf e.epos
						| FAnon cf ->
							declare  (SKField cf) cf.cf_name_pos;
							field_reference cf e.epos
						| FEnum(_,ef) ->
							add_relation ef.ef_name_pos (Referenced,patch_string_pos e.epos ef.ef_name)
						| FDynamic _ ->
							()
					end
				| TTypeExpr mt ->
					let tinfos = t_infos mt in
					add_relation tinfos.mt_name_pos (Referenced,patch_string_pos e.epos (snd tinfos.mt_path))
				| TNew(c,_,el) ->
					List.iter loop el;
					(try add_relation (find_real_constructor c).cf_name_pos (Referenced,e.epos) with Not_found -> ());
				| TCall({eexpr = TConst TSuper},el) ->
					List.iter loop el;
					begin match c.cl_super with
						| Some(c,_) -> (try add_relation (find_real_constructor c).cf_name_pos (Referenced,e.epos) with Not_found -> ())
						| None -> ()
					end
				| TVar(v,eo) ->
					Option.may loop eo;
					var_decl v;
				| TFor(v,e1,e2) ->
					var_decl v;
					loop e1;
					loop e2;
				| TFunction tf ->
					List.iter (fun (v,_) -> var_decl v) tf.tf_args;
					loop tf.tf_expr;
				| TLocal v when e.epos.pmax - e.epos.pmin = String.length v.v_name ->
					add_relation v.v_pos (Referenced,e.epos)
				| _ ->
					Type.iter loop e
			in
			loop e
		in
		List.iter (function
			| TClassDecl c ->
				declare (if c.cl_interface then (SKInterface c) else (SKClass c)) c.cl_name_pos;
				List.iter (fun (c',_) -> add_relation c'.cl_name_pos ((if c.cl_interface then Extended else Implemented),c.cl_name_pos)) c.cl_implements;
				begin match c.cl_super with
					| None -> ()
					| Some (c',_) -> add_relation c'.cl_name_pos (Extended,c.cl_name_pos);
				end;
				collect_overrides c;
				let field cf =
					if cf.cf_pos.pmin > c.cl_name_pos.pmin then declare (SKField cf) cf.cf_name_pos;
					let _ = follow cf.cf_type in
					match cf.cf_expr with None -> () | Some e -> collect_references c e
				in
				Option.may field c.cl_constructor;
				List.iter field c.cl_ordered_fields;
				List.iter field c.cl_ordered_statics;
			| TEnumDecl en ->
				declare (SKEnum en) en.e_name_pos;
				PMap.iter (fun _ ef -> declare (SKEnumField ef) ef.ef_name_pos) en.e_constrs
			| _ ->
				()
		) ctx.com.types;
		let explore_type_hint p t = match follow t with
			| TInst(c,_) -> add_relation c.cl_name_pos (Referenced,(patch_string_pos_front p (snd c.cl_path)))
			| _ -> ()
		in
		Hashtbl.iter (fun p t ->
			explore_type_hint p t
		) ctx.com.shared.shared_display_information.type_hints;
		let l = List.fold_left (fun acc (_,cfi,_,cfo) -> match cfo with
			| Some cf -> if List.mem_assoc cf.cf_name_pos acc then acc else (cf.cf_name_pos,cfi.cf_name_pos) :: acc
			| None -> acc
		) [] ctx.com.display_information.interface_field_implementations in
		List.iter (fun (p,p') -> add_relation p' (Implemented,p)) l;
		let deal_with_imports paths =
			let check_subtype m s p =
				try
					let mt = List.find (fun mt -> snd (t_infos mt).mt_path = s) m.m_types in
					add_relation (t_infos mt).mt_name_pos (Referenced,p);
					Some mt
				with Not_found ->
					None
			in
			let check_module path p =
				let m = ctx.g.do_load_module ctx path p in
				m
			in
			let check_field c s p =
				let cf = PMap.find s c.cl_statics in
				add_relation cf.cf_name_pos (Referenced,p)
			in
			let check_subtype_field m ssub psub sfield pfield = match check_subtype m ssub psub with
				| Some (TClassDecl c) -> check_field c sfield pfield
				| _ -> ()
			in
			PMap.iter (fun p (_,path) ->
				match convert_import_to_something_usable { p with pmin = p.pmax - 1; pmax = p.pmax - 1 } path,List.rev path with
				| (IDKSubType(sl,s1,s2),_),(_,psubtype) :: (_,pmodule) :: _ ->
					let m = check_module (sl,s1) pmodule in
					(*ignore(check_subtype m s1 pmodule);*)
					ignore(check_subtype m s2 psubtype)
				| (IDKModuleField(sl,s1,s2),_),(_,pfield) :: (_,pmodule) :: _ ->
					let m = check_module (sl,s1) pmodule in
					check_subtype_field m s1 pmodule s2 pfield
				| (IDKSubTypeField(sl,s1,s2,s3),_),(_,pfield) :: (_,psubtype) :: (_,pmodule) :: _ ->
					let m = check_module (sl,s1) pmodule in
					check_subtype_field m s2 psubtype s3 pfield
				| (IDKModule(sl,s),_),(_,pmodule) :: _ ->
					let m = check_module (sl,s) pmodule in
					ignore(check_subtype m s pmodule);
				| _ ->
					()
			) paths
		in
		if false then deal_with_imports ctx.com.shared.shared_display_information.import_positions;
		symbols,relations
end

module StatisticsPrinter = struct
	open Statistics

	let relation_to_string = function
		| Implemented -> "implementers"
		| Extended -> "subclasses"
		| Overridden -> "overrides"
		| Referenced -> "references"

	let symbol_to_string = function
		| SKClass _ -> "class type"
		| SKInterface _ -> "interface type"
		| SKEnum _ -> "enum type"
		| SKField _ -> "class field"
		| SKEnumField _ -> "enum field"
		| SKVariable _ -> "variable"

	let print_statistics (kinds,relations) =
		let files = Hashtbl.create 0 in
		Hashtbl.iter (fun p rl ->
			let file = Path.get_real_path p.pfile in
			try
				Hashtbl.replace files file ((p,rl) :: Hashtbl.find files file)
			with Not_found ->
				Hashtbl.add files file [p,rl]
		) relations;
		let ja = Hashtbl.fold (fun file relations acc ->
			let l = List.map (fun (p,rl) ->
				let h = Hashtbl.create 0 in
				List.iter (fun (r,p) ->
					let s = relation_to_string r in
					let jo = JObject [
						"range",pos_to_json_range p;
						"file",JString (Path.get_real_path p.pfile);
					] in
					try Hashtbl.replace h s (jo :: Hashtbl.find h s)
					with Not_found -> Hashtbl.add h s [jo]
				) rl;
				let l = Hashtbl.fold (fun s js acc -> (s,JArray js) :: acc) h [] in
				let l = ("range",pos_to_json_range p) :: l in
				let l = try ("kind",JString (symbol_to_string (Hashtbl.find kinds p))) :: l with Not_found -> l in
				JObject l
			) relations in
			(JObject [
				"file",JString file;
				"statistics",JArray l
			]) :: acc
		) files [] in
		let b = Buffer.create 0 in
		write_json (Buffer.add_string b) (JArray ja);
		Buffer.contents b
end

let process_display_file com classes =
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
						let path = Path.parse_type_path path in
						(match loop l with
						| Some x as r when String.length (Ast.s_type_path x) < String.length (Ast.s_type_path path) -> r
						| _ -> Some path)
					with _ -> loop l)
				end else
					loop l
		in
		loop com.class_path
	in
	match com.display.dms_display_file_policy with
		| DFPNo ->
			()
		| dfp ->
			if dfp = DFPOnly then begin
				classes := [];
				com.main_class <- None;
			end;
			let real = Path.get_real_path (!Parser.resume_display).Ast.pfile in
			(match get_module_path_from_file_path com real with
			| Some path ->
				if com.display.dms_kind = DMPackage then raise (DisplayPackage (fst path));
				classes := path :: !classes
			| None ->
				if not (Sys.file_exists real) then failwith "Display file does not exist";
				(match List.rev (ExtString.String.nsplit real Path.path_sep) with
				| file :: _ when file.[0] >= 'a' && file.[1] <= 'z' -> failwith ("Display file '" ^ file ^ "' should not start with a lowercase letter")
				| _ -> ());
				failwith "Display file was not found in class path"
			);
			Common.log com ("Display file : " ^ real);
			Common.log com ("Classes found : ["  ^ (String.concat "," (List.map Ast.s_type_path !classes)) ^ "]")

let process_global_display_mode com tctx = match com.display.dms_kind with
	| DMUsage with_definition ->
		let symbols,relations = Statistics.collect_statistics tctx in
		let rec loop acc relations = match relations with
			| (Statistics.Referenced,p) :: relations -> loop (p :: acc) relations
			| _ :: relations -> loop acc relations
			| [] -> acc
		in
		let usages = Hashtbl.fold (fun p sym acc ->
			if Statistics.is_usage_symbol sym then begin
				let acc = if with_definition then p :: acc else acc in
				(try loop acc (Hashtbl.find relations p)
				with Not_found -> acc)
			end else
				acc
		) symbols [] in
		let usages = List.sort (fun p1 p2 ->
			let c = compare p1.pfile p2.pfile in
			if c <> 0 then c else compare p1.pmin p2.pmin
		) usages in
		raise (DisplayPosition usages)
	| DMDiagnostics global ->
		Diagnostics.prepare com global;
		raise (Diagnostics (Diagnostics.print_diagnostics tctx global))
	| DMStatistics ->
		let stats = Statistics.collect_statistics tctx in
		raise (Statistics (StatisticsPrinter.print_statistics stats))
	| DMModuleSymbols filter ->
		let symbols = com.shared.shared_display_information.document_symbols in
		let symbols = match !global_cache with
			| None -> symbols
			| Some cache ->
				let rec loop acc com =
					let com_sign = get_signature com in
					let acc = Hashtbl.fold (fun (file,sign) (_,data) acc ->
						if (filter <> None || is_display_file file) && com_sign = sign then
							(file,DocumentSymbols.collect_module_symbols data) :: acc
						else
							acc
					) cache.c_files acc in
					match com.get_macros() with None -> acc | Some com -> loop acc com
				in
				loop symbols com
		in
		raise (ModuleSymbols(DocumentSymbols.print_module_symbols com symbols filter))
	| _ -> ()

module ToplevelCollector = struct
	open IdentifierType

	let run ctx only_types =
		let acc = DynArray.create () in

		if not only_types then begin
			(* locals *)
			PMap.iter (fun _ v ->
				if not (is_gen_local v) then
					DynArray.add acc (ITLocal v)
			) ctx.locals;

			(* member vars *)
			if ctx.curfun <> FunStatic then begin
				let rec loop c =
					List.iter (fun cf ->
						DynArray.add acc (ITMember(ctx.curclass,cf))
					) c.cl_ordered_fields;
					match c.cl_super with
						| None ->
							()
						| Some (csup,tl) ->
							loop csup; (* TODO: type parameters *)
				in
				loop ctx.curclass;
				(* TODO: local using? *)
			end;

			(* statics *)
			List.iter (fun cf ->
				DynArray.add acc (ITStatic(ctx.curclass,cf))
			) ctx.curclass.cl_ordered_statics;

			(* enum constructors *)
			let rec enum_ctors t =
				match t with
				| TAbstractDecl ({a_impl = Some c} as a) when Meta.has Meta.Enum a.a_meta ->
					List.iter (fun cf ->
						if (Meta.has Meta.Enum cf.cf_meta) then DynArray.add acc (ITEnumAbstract(a,cf));
					) c.cl_ordered_statics
				| TClassDecl _ | TAbstractDecl _ ->
					()
				| TTypeDecl t ->
					begin match follow t.t_type with
						| TEnum (e,_) -> enum_ctors (TEnumDecl e)
						| _ -> ()
					end
				| TEnumDecl e ->
					PMap.iter (fun _ ef ->
						DynArray.add acc (ITEnum(e,ef))
					) e.e_constrs;
			in
			List.iter enum_ctors ctx.m.curmod.m_types;
			List.iter enum_ctors (List.map fst ctx.m.module_types);

			(* imported globals *)
			PMap.iter (fun _ (mt,s,_) ->
				try
					let t = match resolve_typedef mt with
						| TClassDecl c -> (PMap.find s c.cl_statics).cf_type
						| TEnumDecl en -> (PMap.find s en.e_constrs).ef_type
						| TAbstractDecl {a_impl = Some c} -> (PMap.find s c.cl_statics).cf_type
						| _ -> raise Not_found
					in
					DynArray.add acc (ITGlobal(mt,s,t))
				with Not_found ->
					()
			) ctx.m.module_globals;
		end;

		let module_types = ref [] in

		let add_type mt =
			match mt with
			| TClassDecl {cl_kind = KAbstractImpl _} -> ()
			| _ ->
				let path = (t_infos mt).mt_path in
				if not (List.exists (fun mt2 -> (t_infos mt2).mt_path = path) !module_types) then begin
					(match mt with
					| TClassDecl c | TAbstractDecl { a_impl = Some c } when Meta.has Meta.CoreApi c.cl_meta ->
						!merge_core_doc_ref ctx c
					| _ -> ());
					module_types := mt :: !module_types
				end
		in

		(* module types *)
		List.iter add_type ctx.m.curmod.m_types;

		(* module imports *)
		List.iter add_type (List.map fst ctx.m.module_types);

		(* module using *)
		List.iter (fun (c,_) ->
			add_type (TClassDecl c)
		) ctx.m.module_using;

		(* TODO: wildcard packages. How? *)

		(* packages and toplevel types *)
		let class_paths = ctx.com.class_path in
		let class_paths = List.filter (fun s -> s <> "") class_paths in

		let packages = ref [] in
		let add_package pack =
			try
				begin match PMap.find pack ctx.com.package_rules with
					| Forbidden ->
						()
					| _ ->
						raise Not_found
				end
			with Not_found ->
				if not (List.mem pack !packages) then packages := pack :: !packages
		in

		List.iter (fun dir ->
			try
				let entries = Sys.readdir dir in
				Array.iter (fun file ->
					match file with
						| "." | ".." ->
							()
						| _ when Sys.is_directory (dir ^ file) && file.[0] >= 'a' && file.[0] <= 'z' ->
							add_package file
						| _ ->
							let l = String.length file in
							if l > 3 && String.sub file (l - 3) 3 = ".hx" then begin
								try
									let name = String.sub file 0 (l - 3) in
									let md = ctx.g.do_load_module ctx ([],name) Ast.null_pos in
									List.iter (fun mt ->
										if (t_infos mt).mt_path = md.m_path then add_type mt
									) md.m_types
								with _ ->
									()
							end
				) entries;
			with Sys_error _ ->
				()
		) class_paths;

		List.iter (fun pack ->
			DynArray.add acc (ITPackage pack)
		) !packages;

		List.iter (fun mt ->
			DynArray.add acc (ITType mt)
		) !module_types;
		DynArray.to_list acc

	let handle_unresolved_identifier ctx i p only_types =
		let l = run ctx only_types in
		let cl = List.map (fun it ->
			let s = IdentifierType.get_name it in
			(s,it),StringError.levenshtein i s
		) l in
		let cl = List.sort (fun (_,c1) (_,c2) -> compare c1 c2) cl in
		let cl = StringError.filter_similar (fun (s,_) r -> r > 0 && r <= (min (String.length s) (String.length i)) / 3) cl in
		ctx.com.display_information.unresolved_identifiers <- (i,p,cl) :: ctx.com.display_information.unresolved_identifiers
end

let display_signature tl display_arg =
	let st = s_type (print_context()) in
	let s_arg (n,o,t) = Printf.sprintf "%s%s:%s" (if o then "?" else "") n (st t) in
	let s_fun args ret = Printf.sprintf "(%s):%s" (String.concat ", " (List.map s_arg args)) (st ret) in
	let siginf = List.map (fun (t,doc) ->
		let label = match follow t with TFun(args,ret) -> s_fun args ret | _ -> st t in
		let parameters = match follow t with
			| TFun(args,_) ->
				List.map (fun arg ->
					let label = s_arg arg in
					JObject [
						"label",JString label
					]
				) args
			| _ -> []
		in
		let js = [
			"label",JString label;
			"parameters",JArray parameters;
		] in
		JObject (match doc with None -> js | Some s -> ("documentation",JString s) :: js)
	) tl in
	let jo = JObject [
		"signatures",JArray siginf;
		"activeParameter",JInt display_arg;
		"activeSignature",JInt 0;
	] in
	let b = Buffer.create 0 in
	write_json (Buffer.add_string b) jo;
	Buffer.contents b