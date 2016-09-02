open Ast
open Common
open Type
open Typecore

(* order of these variants affects output sorting *)
type display_field_kind =
	| FKVar
	| FKMethod
	| FKType
	| FKPackage

exception Diagnostics of string
exception Statistics of string
exception ModuleSymbols of string
exception DisplaySignatures of (t * documentation) list
exception DisplayType of t * pos
exception DisplayPosition of Ast.pos list
exception DisplaySubExpression of Ast.expr
exception DisplayFields of (string * t * display_field_kind option * documentation) list
exception DisplayToplevel of IdentifierType.t list
exception DisplayPackage of string list

let is_display_file file =
	file <> "?" && Common.unique_full_path file = (!Parser.resume_display).pfile

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
	let rec loop e =
		if is_annotated (pos e) then
			(EDisplay(e,false),(pos e))
		else
			e
	in
	let rec map e =
		loop (Ast.map_expr map e)
	in
	map e

let display_type dm t p =
	try
		let mt = module_type_of_type t in
		begin match dm with
			| DMPosition -> raise (DisplayPosition [(t_infos mt).mt_pos]);
			| DMUsage ->
				let ti = t_infos mt in
				ti.mt_meta <- (Meta.Usage,[],ti.mt_pos) :: ti.mt_meta
			| DMType -> raise (DisplayType (t,p))
			| _ -> ()
		end
	with Exit ->
		()

let display_module_type dm mt =
	display_type dm (type_of_module_type mt)

let display_variable dm v p = match dm with
	| DMPosition -> raise (DisplayPosition [v.v_pos])
	| DMUsage -> v.v_meta <- (Meta.Usage,[],v.v_pos) :: v.v_meta;
	| DMType -> raise (DisplayType (v.v_type,p))
	| _ -> ()

let display_field dm cf p = match dm with
	| DMPosition -> raise (DisplayPosition [cf.cf_pos]);
	| DMUsage -> cf.cf_meta <- (Meta.Usage,[],cf.cf_pos) :: cf.cf_meta;
	| DMType -> raise (DisplayType (cf.cf_type,p))
	| _ -> ()

let display_enum_field dm ef p = match dm with
	| DMPosition -> raise (DisplayPosition [p]);
	| DMUsage -> ef.ef_meta <- (Meta.Usage,[],p) :: ef.ef_meta;
	| DMType -> raise (DisplayType (ef.ef_type,p))
	| _ -> ()

module SymbolKind = struct
	type t =
		| Class
		| Interface
		| Enum
		| Typedef
		| Abstract
		| Field
		| Property
		| Method
		| Constructor
		| Function
		| Variable

	let to_int = function
		| Class -> 1
		| Interface -> 2
		| Enum -> 3
		| Typedef -> 4
		| Abstract -> 5
		| Field -> 6
		| Property -> 7
		| Method -> 8
		| Constructor -> 9
		| Function -> 10
		| Variable -> 11
end

module SymbolInformation = struct
	type t = {
		name : string;
		kind : SymbolKind.t;
		pos : pos;
		container_name : string option;
	}

	let make name kind pos container_name = {
		name = name;
		kind = kind;
		pos = pos;
		container_name = container_name;
	}
end

open SymbolKind
open SymbolInformation
open Json

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

let print_module_symbols (pack,decls) =
	let l = DynArray.create() in
	let add name kind location parent =
		let si = SymbolInformation.make name kind location (match parent with None -> None | Some si -> Some si.name) in
		DynArray.add l si;
		si
	in
(* 		let si_pack = match pack with
		| [] -> None
		| _ -> Some (add (String.concat "." pack) Package null_pos None) (* TODO: we don't have the position *)
	in *)
	let si_pack = None in (* TODO: no position, no point *)
	let rec expr si (e,p) =
		let add name kind location = add name kind location si in
		let add_ignore name kind location = ignore(add name kind location) in
		begin match e with
			(* TODO: disabled for now because it's too spammy *)
(* 			| EConst ct ->
			begin match ct with
				| Int i | Float i -> add_ignore i Number p
				| Ast.String s -> add_ignore s String p
				| Ident ("true" | "false" as s) -> add_ignore s Boolean p
				| Ident _ -> (* Hmm... *) ()
				| _ -> ()
			end *)
		| EVars vl ->
			List.iter (fun ((s,p),_,eo) ->
				add_ignore s Variable p;
				expr_opt si eo
			) vl
		| ETry(e1,catches) ->
			expr si e1;
			List.iter (fun ((s,p),_,e) ->
				add_ignore s Variable p;
				expr si e
			) catches;
		| EFunction(Some s,f) ->
			let si_function = add s Function p in
			func si_function f
		| EIn((EConst(Ident s),p),e2) ->
			add_ignore s Variable p;
			expr si e2;
		| _ ->
			iter_expr (expr si) (e,p)
		end
	and expr_opt si eo = match eo with
		| None -> ()
		| Some e -> expr si e
	and func si f =
		List.iter (fun ((s,p),_,_,_,eo) ->
			let si_arg = add s Variable p (Some si) in
			expr_opt (Some si_arg) eo
		) f.f_args;
		expr_opt (Some si) f.f_expr
	in
	let field si_type cff = match cff.cff_kind with
		| FVar(_,eo) ->
			let si_field = add (fst cff.cff_name) Field cff.cff_pos (Some si_type) in
			expr_opt (Some si_field) eo
		| FFun f ->
			let si_method = add (fst cff.cff_name) (if fst cff.cff_name = "new" then Constructor else Method) cff.cff_pos (Some si_type) in
			func si_method f
		| FProp(_,_,_,eo) ->
			let si_property = add (fst cff.cff_name) Property cff.cff_pos (Some si_type) in
			expr_opt (Some si_property) eo
	in
	List.iter (fun (td,p) -> match td with
		| EImport _ | EUsing _ ->
			() (* TODO: Can we do anything with these? *)
		| EClass d ->
			let si_type = add (fst d.d_name) (if List.mem HInterface d.d_flags then Interface else Class) p si_pack in
			List.iter (field si_type) d.d_data
		| EEnum d ->
			let si_type = add (fst d.d_name) Enum p si_pack in
			List.iter (fun ef ->
				ignore (add (fst ef.ec_name) Method ef.ec_pos (Some si_type))
			) d.d_data
		| ETypedef d ->
			let si_type = add (fst d.d_name) Typedef p si_pack in
			(match d.d_data with
			| CTAnonymous fields,_ ->
				List.iter (field si_type) fields
			| _ -> ())
		| EAbstract d ->
			let si_type = add (fst d.d_name) Abstract p si_pack in
			List.iter (field si_type) d.d_data
	) decls;
	let jl = List.map (fun si ->
		let l =
			("name",JString si.name) ::
			("kind",JInt (to_int si.kind)) ::
			("range", pos_to_json_range si.pos) ::
			(match si.container_name with None -> [] | Some s -> ["containerName",JString s])
		in
		JObject l
	) (DynArray.to_list l) in
	let js = JArray jl in
	let b = Buffer.create 0 in
	write_json (Buffer.add_string b) js;
	Buffer.contents b

type import_display_kind =
	| IDKPackage of string list
	| IDKModule of string list * string
	| IDKSubType of string list * string * string
	| IDKModuleField of string list * string * string
	| IDKSubTypeField of string list * string * string * string
	| IDK

type import_display = import_display_kind * pos

let convert_import_to_something_usable path =
	let rec loop pack m t = function
		| (s,p) :: l ->
			let is_lower = is_lower_ident s in
			let is_display_pos = encloses_position !Parser.resume_display p in
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

let process_expr com e = match com.display with
	| DMToplevel -> find_enclosing com e
	| DMPosition | DMUsage | DMType -> find_before_pos com e
	| _ -> e

let add_import_position com p =
	com.shared.shared_display_information.import_positions <- PMap.add p (ref false) com.shared.shared_display_information.import_positions

let mark_import_position com p =
	try
		let r = PMap.find p com.shared.shared_display_information.import_positions in
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

	let prepare com =
		List.iter (function
			| TClassDecl c ->
				List.iter (prepare_field com) c.cl_ordered_fields;
				List.iter (prepare_field com) c.cl_ordered_statics;
				(match c.cl_constructor with None -> () | Some cf -> prepare_field com cf);
			| _ ->
				()
		) com.types

	let print_diagnostics ctx =
		let com = ctx.com in
		let diag = Hashtbl.create 0 in
		let add dk p sev args =
			let file = get_real_path p.pfile in
			let diag = try
				Hashtbl.find diag file
			with Not_found ->
				let d = DynArray.create() in
				Hashtbl.add diag file d;
				d
			in
			DynArray.add diag (dk,p,sev,args)
		in
		let find_type i =
			let types = ref [] in
			Hashtbl.iter (fun _ m ->
				List.iter (fun mt ->
					let tinfos = t_infos mt in
					if snd tinfos.mt_path = i then
						types := JObject [
							"kind",JInt (UnresolvedIdentifierSuggestion.to_int UISImport);
							"name",JString (s_type_path m.m_path)
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
		PMap.iter (fun p r ->
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

	let is_diagnostics_run ctx = match ctx.com.display with
		| DMDiagnostics true -> true
		| DMDiagnostics false -> ctx.is_display_file
		| _ -> false
end

let maybe_mark_import_position ctx p =
	if Diagnostics.is_diagnostics_run ctx then mark_import_position ctx.com p


module Statistics = struct
	type relation =
		| Implemented
		| Extended
		| Overridden
		| Referenced

	type source_kind =
		| SKClass
		| SKInterface
		| SKEnum
		| SKField
		| SKEnumField

	let relation_to_string = function
		| Implemented -> "implementers"
		| Extended -> "subclasses"
		| Overridden -> "overrides"
		| Referenced -> "references"

	let source_kind_to_string = function
		| SKClass -> "class type"
		| SKInterface -> "interface type"
		| SKEnum -> "enum type"
		| SKField -> "class field"
		| SKEnumField -> "enum field"

	let print_statistics kinds relations =
		let files = Hashtbl.create 0 in
		Hashtbl.iter (fun p rl ->
			let file = get_real_path p.pfile in
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
						"file",JString (get_real_path p.pfile);
					] in
					try Hashtbl.replace h s (jo :: Hashtbl.find h s)
					with Not_found -> Hashtbl.add h s [jo]
				) rl;
				let l = Hashtbl.fold (fun s js acc -> (s,JArray js) :: acc) h [] in
				let l = ("range",pos_to_json_range p) :: l in
				let l = try ("kind",JString (source_kind_to_string (Hashtbl.find kinds p))) :: l with Not_found -> l in
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

	let collect_statistics ctx =
		let relations = Hashtbl.create 0 in
		let kinds = Hashtbl.create 0 in
		let add_relation pos r =
			if is_display_file pos.pfile then try
				Hashtbl.replace relations pos (r :: Hashtbl.find relations pos)
			with Not_found ->
				Hashtbl.add relations pos [r]
		in
		let declare kind p =
			if is_display_file p.pfile then begin
				if not (Hashtbl.mem relations p) then Hashtbl.add relations p [];
				Hashtbl.replace kinds p kind;
			end
		in
		let collect_overrides c =
			List.iter (fun cf ->
				let rec loop c = match c.cl_super with
					| Some (c,_) ->
						begin try
							let cf' = PMap.find cf.cf_name c.cl_fields in
							add_relation cf'.cf_pos (Overridden,cf.cf_pos)
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
			(* The pos comparison might be a bit week, not sure... *)
			| Some cf,_ when not (Meta.has Meta.CompilerGenerated cf.cf_meta) && c.cl_pos <> cf.cf_pos -> cf
			| _,Some(c,_) -> find_real_constructor c
			| _,None -> raise Not_found
		in
		let collect_references c e =
			let rec loop e = match e.eexpr with
				| TField(e1,FEnum(_,ef)) ->
					loop e1;
					add_relation ef.ef_pos (Referenced,e.epos);
				| TField(e1,fa) ->
					loop e1;
					begin match extract_field fa with
						| Some cf when is_display_file cf.cf_pos.pfile ->
							add_relation cf.cf_pos (Referenced,e.epos)
						| _ ->
							()
					end
				| TNew(c,_,el) ->
					List.iter loop el;
					(try add_relation (find_real_constructor c).cf_pos (Referenced,e.epos) with Not_found -> ());
				| TCall({eexpr = TConst TSuper},el) ->
					List.iter loop el;
					begin match c.cl_super with
						| Some(c,_) -> (try add_relation (find_real_constructor c).cf_pos (Referenced,e.epos) with Not_found -> ())
						| None -> ()
					end

				| _ ->
					Type.iter loop e
			in
			loop e
		in
		List.iter (function
			| TClassDecl c ->
				declare (if c.cl_interface then SKInterface else SKClass) c.cl_pos;
				List.iter (fun (c',_) -> add_relation c'.cl_pos ((if c.cl_interface then Extended else Implemented),c.cl_pos)) c.cl_implements;
				(match c.cl_super with None -> () | Some (c',_) -> add_relation c'.cl_pos (Extended,c.cl_pos));
				collect_overrides c;
				let field cf =
					declare SKField cf.cf_pos;
					let _ = follow cf.cf_type in
					match cf.cf_expr with None -> () | Some e -> collect_references c e
				in
				let _ = Option.map field c.cl_constructor in
				List.iter field c.cl_ordered_fields;
				List.iter field c.cl_ordered_statics;
			| TEnumDecl en ->
				declare SKEnum en.e_pos;
				PMap.iter (fun _ ef -> declare SKEnumField ef.ef_pos) en.e_constrs
			| _ ->
				()
		) ctx.com.types;
		print_statistics kinds relations
end