open Ast
open Common
open Common.DisplayMode
open Type
open Typecore
open Globals

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
exception DisplaySignatures of (tsignature * documentation) list * int
exception DisplayType of t * pos * string option
exception DisplayPosition of pos list
exception DisplayFields of (string * display_field_kind * documentation) list
exception DisplayToplevel of IdentifierType.t list
exception DisplayPackage of string list

let is_display_file file =
	file <> "?" && Path.unique_full_path file = (!Parser.resume_display).pfile

let encloses_position p_target p =
	p.pmin <= p_target.pmin && p.pmax >= p_target.pmax

let is_display_position p =
	encloses_position !Parser.resume_display p

module ExprPreprocessing = struct
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


	let process_expr com e = match com.display.dms_kind with
		| DMToplevel -> find_enclosing com e
		| DMPosition | DMUsage _ | DMType -> find_before_pos com e
		| DMSignature -> find_display_call e
		| _ -> e
end

module DisplayEmitter = struct
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

	let maybe_display_field ctx p cf =
		if is_display_position p then display_field ctx.com.display cf p

	let display_enum_field dm ef p = match dm.dms_kind with
		| DMPosition -> raise (DisplayPosition [p]);
		| DMUsage _ -> ef.ef_meta <- (Meta.Usage,[],p) :: ef.ef_meta;
		| DMType -> raise (DisplayType (ef.ef_type,p,ef.ef_doc))
		| _ -> ()

	let display_meta dm meta = match dm.dms_kind with
		| DMType ->
			begin match meta with
			| Meta.Custom _ | Meta.Dollar _ -> ()
			| _ -> match Meta.get_documentation meta with
				| None -> ()
				| Some (_,s) ->
					(* TODO: hack until we support proper output for hover display mode *)
					raise (Metadata ("<metadata>" ^ s ^ "</metadata>"));
			end
		| DMField ->
			let all,_ = Meta.get_documentation_list() in
			let all = List.map (fun (s,doc) -> (s,FKMetadata,Some doc)) all in
			raise (DisplayFields all)
		| _ ->
			()

	let check_display_metadata ctx meta =
		List.iter (fun (meta,args,p) ->
			if is_display_position p then display_meta ctx.com.display meta;
			List.iter (fun e ->
				if is_display_position (pos e) then begin
					let e = ExprPreprocessing.process_expr ctx.com e in
					delay ctx PTypeField (fun _ -> ignore(type_expr ctx e Value));
				end
			) args
		) meta
end

module DocumentSymbols = struct
	open DisplayTypes.SymbolKind

	let collect_module_symbols (pack,decls) =
		let l = DynArray.create() in
		let add name kind location parent =
			let si = DisplayTypes.SymbolInformation.make name kind location (if parent = "" then None else Some parent) in
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
			| EBinop(OpIn,(EConst(Ident s),p),e2) ->
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
end

module DeprecationCheck = struct

	let curclass = ref null_class

	let warned_positions = Hashtbl.create 0

	let print_deprecation_message com meta s p_usage =
		let s = match meta with
			| _,[EConst(String (s,_)),_],_ -> s
			| _ -> Printf.sprintf "Usage of this %s is deprecated" s
		in
		if not (Hashtbl.mem warned_positions p_usage) then begin
			Hashtbl.replace warned_positions p_usage true;
			com.warning s p_usage;
		end

	let check_meta com meta s p_usage =
		try
			print_deprecation_message com (Meta.get Meta.Deprecated meta) s p_usage;
		with Not_found ->
			()

	let check_cf com cf p = check_meta com cf.cf_meta "field" p

	let check_class com c p = if c != !curclass then check_meta com c.cl_meta "class" p

	let check_enum com en p = check_meta com en.e_meta "enum" p

	let check_ef com ef p = check_meta com ef.ef_meta "enum field" p

	let check_typedef com t p = check_meta com t.t_meta "typedef" p

	let check_module_type com mt p = match mt with
		| TClassDecl c -> check_class com c p
		| TEnumDecl en -> check_enum com en p
		| _ -> ()

	let run_on_expr com e =
		let rec expr e = match e.eexpr with
			| TField(e1,fa) ->
				expr e1;
				begin match fa with
					| FStatic(c,cf) | FInstance(c,_,cf) ->
						check_class com c e.epos;
						check_cf com cf e.epos
					| FAnon cf ->
						check_cf com cf e.epos
					| FClosure(co,cf) ->
						(match co with None -> () | Some (c,_) -> check_class com c e.epos);
						check_cf com cf e.epos
					| FEnum(en,ef) ->
						check_enum com en e.epos;
						check_ef com ef e.epos;
					| _ ->
						()
				end
			| TNew(c,_,el) ->
				List.iter expr el;
				check_class com c e.epos;
				(match c.cl_constructor with None -> () | Some cf -> check_cf com cf e.epos)
			| TTypeExpr(mt) | TCast(_,Some mt) ->
				check_module_type com mt e.epos
			| TMeta((Meta.Deprecated,_,_) as meta,e1) ->
				print_deprecation_message com meta "field" e1.epos;
				expr e1;
			| _ ->
				Type.iter expr e
		in
		expr e

	let run_on_field com cf = match cf.cf_expr with None -> () | Some e -> run_on_expr com e

	let run com =
		List.iter (fun t -> match t with
			| TClassDecl c ->
				curclass := c;
				(match c.cl_constructor with None -> () | Some cf -> run_on_field com cf);
				(match c.cl_init with None -> () | Some e -> run_on_expr com e);
				List.iter (run_on_field com) c.cl_ordered_statics;
				List.iter (run_on_field com) c.cl_ordered_fields;
			| _ ->
				()
		) com.types
end

module Diagnostics = struct
	module DiagnosticsKind = struct
		type t =
			| DKUnusedImport
			| DKUnresolvedIdentifier
			| DKCompilerError
			| DKRemovableCode

		let to_int = function
			| DKUnusedImport -> 0
			| DKUnresolvedIdentifier -> 1
			| DKCompilerError -> 2
			| DKRemovableCode -> 3
	end

	open DiagnosticsKind
	open DisplayTypes

	let add_removable_code com s p prange =
		let di = com.shared.shared_display_information in
		di.removable_code <- (s,p,prange) :: di.removable_code

	let find_unused_variables com e =
		let vars = Hashtbl.create 0 in
		let pmin_map = Hashtbl.create 0 in
		let rec loop e = match e.eexpr with
			| TVar(v,eo) when Meta.has Meta.UserVariable v.v_meta ->
				Hashtbl.add pmin_map e.epos.pmin v;
				let p = match eo with
					| None -> e.epos
					| Some e1 ->
						loop e1;
						{ e.epos with pmax = e1.epos.pmin }
				in
				Hashtbl.replace vars v.v_id (v,p);
			| TLocal v when Meta.has Meta.UserVariable v.v_meta ->
				Hashtbl.remove vars v.v_id;
			| _ ->
				Type.iter loop e
		in
		loop e;
		Hashtbl.iter (fun _ (v,p) ->
			let p = match (Hashtbl.find_all pmin_map p.pmin) with [_] -> p | _ -> null_pos in
			add_removable_code com "Unused variable" v.v_pos p
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
			| TConst _ | TLocal _ | TTypeExpr _ | TFunction _ | TIdent _ when not in_value ->
				no_effect e.epos;
			| TConst _ | TLocal _ | TTypeExpr _ | TEnumParameter _ | TEnumIndex _ | TVar _ | TIdent _ ->
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
			check_other_things com e;
			DeprecationCheck.run_on_expr com e

	let prepare com global =
		List.iter (function
			| TClassDecl c when global || is_display_file c.cl_pos.pfile ->
				List.iter (prepare_field com) c.cl_ordered_fields;
				List.iter (prepare_field com) c.cl_ordered_statics;
				(match c.cl_constructor with None -> () | Some cf -> prepare_field com cf);
			| _ ->
				()
		) com.types

	let is_diagnostics_run ctx = match ctx.com.display.dms_kind with
		| DMDiagnostics true -> true
		| DMDiagnostics false -> ctx.is_display_file
		| _ -> false

	let secure_generated_code ctx e =
		if is_diagnostics_run ctx then mk (TMeta((Meta.Extern,[],e.epos),e)) e.etype e.epos else e
end

module ImportHandling = struct
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

	let add_import_position com p path =
		com.shared.shared_display_information.import_positions <- PMap.add p (ref false,path) com.shared.shared_display_information.import_positions

	let mark_import_position com p =
		try
			let r = fst (PMap.find p com.shared.shared_display_information.import_positions) in
			r := true
		with Not_found ->
			()

	let maybe_mark_import_position ctx p =
		if Diagnostics.is_diagnostics_run ctx then mark_import_position ctx.com p
end

module Statistics = struct
	open ImportHandling

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
				match ImportHandling.convert_import_to_something_usable { p with pmin = p.pmax - 1; pmax = p.pmax - 1 } path,List.rev path with
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

let explore_class_paths ctx class_paths recusive f_pack f_module f_type =
	let rec loop dir pack =
		try
			let entries = Sys.readdir dir in
			Array.iter (fun file ->
				match file with
					| "." | ".." ->
						()
					| _ when Sys.is_directory (dir ^ file) && file.[0] >= 'a' && file.[0] <= 'z' ->
						f_pack file;
						if recusive then loop (dir ^ file ^ "/") (file :: pack)
					| _ ->
						let l = String.length file in
						if l > 3 && String.sub file (l - 3) 3 = ".hx" then begin
							try
								let name = String.sub file 0 (l - 3) in
								let path = (List.rev pack,name) in
								let md = ctx.g.do_load_module ctx path null_pos in
								f_module md;
								List.iter (fun mt -> f_type mt) md.m_types
							with _ ->
								()
						end
			) entries;
		with Sys_error _ ->
			()
	in
	List.iter (fun dir -> loop dir []) class_paths

module ToplevelCollector = struct
	open IdentifierType

	let run ctx only_types =
		let acc = DynArray.create () in
		let add x = DynArray.add acc x in

		if not only_types then begin
			(* locals *)
			PMap.iter (fun _ v ->
				if not (is_gen_local v) then
					add (ITLocal v)
			) ctx.locals;

			(* member vars *)
			if ctx.curfun <> FunStatic then begin
				let rec loop c =
					List.iter (fun cf ->
						if not (Meta.has Meta.NoCompletion cf.cf_meta) then add (ITMember(ctx.curclass,cf))
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
				if not (Meta.has Meta.NoCompletion cf.cf_meta) then add (ITStatic(ctx.curclass,cf))
			) ctx.curclass.cl_ordered_statics;

			(* enum constructors *)
			let rec enum_ctors t =
				match t with
				| TAbstractDecl ({a_impl = Some c} as a) when Meta.has Meta.Enum a.a_meta ->
					List.iter (fun cf ->
						if (Meta.has Meta.Enum cf.cf_meta) && not (Meta.has Meta.NoCompletion cf.cf_meta) then add (ITEnumAbstract(a,cf));
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
						add (ITEnum(e,ef))
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
					add (ITGlobal(mt,s,t))
				with Not_found ->
					()
			) ctx.m.module_globals;

			(* literals *)
			add (ITLiteral "null");
			add (ITLiteral "true");
			add (ITLiteral "false");
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

		let maybe_add_type mt = if not (t_infos mt).mt_private then add_type mt in

		explore_class_paths ctx class_paths false add_package (fun _ -> ()) maybe_add_type;

		List.iter (fun pack ->
			add (ITPackage pack)
		) !packages;

		List.iter (fun mt ->
			add (ITType mt)
		) !module_types;

		(* type params *)
		List.iter (fun (_,t) ->
			add (ITType (module_type_of_type t))
		) ctx.type_params;

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