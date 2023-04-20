open Globals
open Ast
open Common
open Type
open Typecore
open Constructor
open Pattern
open Error

type pattern_context = {
	ctx : typer;
	or_locals : (string, tvar * pos) PMap.t option;
	ctx_locals : (string, tvar) PMap.t;
	mutable current_locals : (string, tvar * pos) PMap.t;
	mutable in_reification : bool;
	is_postfix_match : bool;
	unapply_type_parameters : unit -> (tmono * Type.t option) list;
}

exception Bad_pattern of string

let fake_tuple_type = TInst(mk_class null_module ([],"-Tuple") null_pos null_pos, [])

let tuple_type tl =
	tfun tl fake_tuple_type

let type_field_access ctx ?(resume=false) e name =
	Calls.acc_get ctx (Fields.type_field (Fields.TypeFieldConfig.create resume) ctx e name e.epos MGet WithType.value)

let unapply_type_parameters params monos =
	let unapplied = ref [] in
	List.iter2 (fun tp1 t2 ->
		match t2,follow t2 with
		| TMono m1,TMono m2 ->
			unapplied := (m1,m1.tm_type) :: !unapplied;
			Monomorph.bind m1 tp1.ttp_type;
		| _ -> ()
	) params monos;
	!unapplied

let reapply_type_parameters unapplied =
	List.iter (fun (m,o) -> match o with
		| None -> Monomorph.unbind m
		| Some t -> Monomorph.bind m t
	) unapplied

let get_general_module_type ctx mt p =
	let rec loop = function
		| TClassDecl _ -> "Class"
		| TEnumDecl _ -> "Enum"
		| TAbstractDecl a when Meta.has Meta.RuntimeValue a.a_meta -> "Class"
		| TTypeDecl t ->
			begin match follow (monomorphs t.t_params t.t_type) with
				| TInst(c,_) -> loop (TClassDecl c)
				| TEnum(en,_) -> loop (TEnumDecl en)
				| TAbstract(a,_) -> loop (TAbstractDecl a)
				| _ -> raise_typing_error "Cannot use this type as a value" p
			end
		| _ -> raise_typing_error "Cannot use this type as a value" p
	in
	Typeload.load_instance ctx ({tname=loop mt;tpackage=[];tsub=None;tparams=[]},p) true

let unify_type_pattern ctx mt t p =
	let tcl = get_general_module_type ctx mt p in
	match tcl with
		| TAbstract(a,_) -> unify ctx (TAbstract(a,[spawn_monomorph ctx p])) t p
		| _ -> die "" __LOC__

let rec make pctx toplevel t e =
	let ctx = pctx.ctx in
	let p = pos e in
	let fail () =
		raise_typing_error ("Unrecognized pattern: " ^ (Ast.Printer.s_expr e)) p
	in
	let unify_expected t' =
		unify ctx t' t p
	in
	let verror name p =
		raise_typing_error (Printf.sprintf "Variable %s must appear exactly once in each sub-pattern" name) p
	in
	let add_local final name p =
		let is_wildcard_local = name = "_" in
		if not is_wildcard_local && pctx.is_postfix_match then raise_typing_error "Pattern variables are not allowed in .match patterns" p;
		if not is_wildcard_local && PMap.mem name pctx.current_locals then raise_typing_error (Printf.sprintf "Variable %s is bound multiple times" name) p;
		match pctx.or_locals with
		| Some map when not is_wildcard_local ->
			let v,p = try PMap.find name map with Not_found -> verror name p in
			unify ctx t v.v_type p;
			if final then add_var_flag v VFinal;
			pctx.current_locals <- PMap.add name (v,p) pctx.current_locals;
			v
		| _ ->
			let v = alloc_var (VUser TVOPatternVariable) name t p in
			if final then add_var_flag v VFinal;
			pctx.current_locals <- PMap.add name (v,p) pctx.current_locals;
			ctx.locals <- PMap.add name v ctx.locals;
			v
	in
	let con_enum en ef p =
		let dctx = create_deprecation_context pctx.ctx in
		DeprecationCheck.check_enum dctx en p;
		DeprecationCheck.check_ef dctx ef p;
		ConEnum(en,ef),p
	in
	let con_static c cf p = ConStatic(c,cf),p in
	let con_const ct p = ConConst ct,p in
	let con_type_expr mt p = ConTypeExpr mt,p in
	let con_array i p = ConArray i,p in
	let con_fields fl p = ConFields fl,p in
	let get_enumerable_idents () = match follow t with
		| TEnum(en,_) ->
			en.e_names
		| TAbstract({a_impl = Some c} as a,pl) when a.a_enum ->
			ExtList.List.filter_map (fun cf ->
				if has_class_field_flag cf CfImpl && has_class_field_flag cf CfEnum then Some cf.cf_name else None
			) c.cl_ordered_statics
		| _ ->
			[]
	in
	let check_expr e =
		let rec loop e = match e.eexpr with
			| TField(_,FEnum(en,ef)) ->
				(* Let the unification afterwards fail so we don't recover. *)
				(* (match follow ef.ef_type with TFun _ -> raise Exit | _ -> ()); *)
				PatConstructor(con_enum en ef e.epos,[])
			| TField(_,FStatic(c,({cf_kind = Var {v_write = AccNever}} as cf))) ->
				PatConstructor(con_static c cf e.epos,[])
			| TConst ct ->
				PatConstructor(con_const ct e.epos,[])
			| TCast(e1,None) ->
				loop e1
			| TField (ef,f) ->
				let s = field_name f in
				begin match StringError.get_similar s (get_enumerable_idents()) with
					| [] -> ()
					| l ->
						let tpath = match follow t with
							| TEnum (e,tl) -> s_type_path e.e_path ^ "."
							| TAbstract (a,tl) -> s_type_path a.a_path ^ "."
							| _ -> ""
						in
						let fields = List.map (fun (el) -> tpath ^ el) l in
						warning pctx.ctx WTyper ("Potential typo detected (expected similar values are " ^ (String.concat ", " fields) ^ ")") p
				end;
				raise (Bad_pattern "Only inline or read-only (default, never) fields can be used as a pattern")
			| TTypeExpr mt ->
				PatConstructor(con_type_expr mt e.epos,[])
			| TMeta((Meta.Deprecated,_,_) as m, e1) ->
				DeprecationCheck.check_meta (create_deprecation_context pctx.ctx) [m] "field" e1.epos;
				loop e1
			| _ ->
				raise Exit
		in
		loop e
	in
	let display_mode () =
		if pctx.is_postfix_match then DKMarked else DKPattern toplevel
	in
	let catch_errors () =
		let old = ctx.com.error_ext in
		let restore_report_mode = disable_report_mode ctx.com in
		ctx.com.error_ext <- (fun _ -> raise Exit);
		(fun () ->
			restore_report_mode();
			ctx.com.error_ext <- old
		)
	in
	let try_typing e =
		let old = ctx.untyped in
		ctx.untyped <- true;
		let restore = catch_errors () in
		let e = try
			type_expr ctx e (WithType.with_type t)
		with exc ->
			restore();
			ctx.untyped <- old;
			raise exc
		in
		restore();
		ctx.untyped <- old;
		let pat = check_expr e in
		begin match pat with
			| PatConstructor((ConTypeExpr mt,_),_) -> unify_type_pattern ctx mt t e.epos;
			| _ -> unify ctx e.etype t p;
		end;
		pat
	in
	let handle_ident s p =
		try
			try_typing (EConst (Ident s),p)
		with
		| Exit | Bad_pattern _ ->
			let restore = catch_errors () in
			begin try
				let mt = module_type_of_type t in
				let e_mt = TyperBase.type_module_type ctx mt p in
				let e = type_field_access ctx ~resume:true e_mt s in
				restore();
				check_expr e
			with _ ->
				restore();
				if not (is_lower_ident s) && (match s.[0] with '`' | '_' -> false | _ -> true) then begin
					display_error ctx.com ("Unknown identifier : " ^ s ^ ", pattern variables must be lower-case or with `var ` prefix") p;
				end;
				begin match StringError.get_similar s (get_enumerable_idents()) with
					| [] ->
						()
						(* if toplevel then
							warning pctx.ctx (Printf.sprintf "`case %s` has been deprecated, use `case var %s` instead" s s) p *)
					| l -> warning pctx.ctx WTyper ("Potential typo detected (expected similar values are " ^ (String.concat ", " l) ^ "). Consider using `var " ^ s ^ "` instead") p
				end;
				let v = add_local false s p in
				PatBind(v, (PatAny,null_pos))
			end
	in
	let rec loop e = match fst e with
		| EParenthesis e1 | ECast(e1,None) ->
			loop e1
		| ECheckType(e, (CTPath({tpackage=["haxe";"macro"]; tname="Expr"}),_)) ->
			let old = pctx.in_reification in
			pctx.in_reification <- true;
			let e = loop e in
			pctx.in_reification <- old;
			e
		| EConst((Ident ("false" | "true") | Int (_,_) | String _ | Float (_,_)) as ct) ->
			begin match ct with
				| String (value,kind) when kind = Ast.SSingleQuotes ->
					let e = ctx.g.do_format_string ctx value p in
					begin match e with
						| EBinop _, p -> raise_typing_error "String interpolation is not allowed in case patterns" p;
						| _ -> ()
					end;
				| _ -> ()
			end;
			let p = pos e in
			let e = Texpr.type_constant ctx.com.basic ct p in
			unify_expected e.etype;
			let ct = match e.eexpr with TConst ct -> ct | _ -> die "" __LOC__ in
			PatConstructor(con_const ct p,[])
		| EConst (Ident i) ->
			begin match follow t with
				| TFun(ta,tr) when tr == fake_tuple_type ->
					if i = "_" then PatTuple(List.map (fun (_,_,t) -> (PatAny,pos e)) ta)
					else raise_typing_error "Cannot bind matched tuple to variable, use _ instead" p
				| _ ->
					if i = "_" then PatAny
					else handle_ident i (pos e)
			end
		| EVars([{ ev_name = (s,p); ev_final = final; ev_type = None; ev_expr = None; }]) ->
			let v = add_local final s p in
			PatBind(v,(PatAny,null_pos))
		| ECall(e1,el) ->
			let e1 = type_expr ctx e1 (WithType.with_type t) in
			begin match e1.eexpr,follow e1.etype with
				| TField(_, FEnum(en,ef)),TFun(_,TEnum(_,tl)) ->
					let map = apply_params en.e_params tl in
					let monos = Monomorph.spawn_constrained_monos map ef.ef_params in
					let map t = map (apply_params ef.ef_params monos t) in
					unify ctx (map ef.ef_type) e1.etype e1.epos;
					let args = match follow e1.etype with
						| TFun(args,r) ->
							unify_expected r;
							args
						| _ -> die "" __LOC__
					in
					let rec loop el tl = match el,tl with
						| [EConst (Ident "_"),p],(_,_,t) :: tl ->
							(* Allow using final _ to match "multiple" arguments *)
							(PatAny,p) :: (match tl with [] -> [] | _ -> loop el tl)
						| e :: el,(_,_,t) :: tl ->
							make pctx false t e :: loop el tl
						| [],(_,true,t) :: tl ->
							(PatAny,pos e) :: loop [] tl
						| [],[] ->
							[]
						| [],_ ->
							raise_typing_error "Not enough arguments" p
						| _,[] ->
							raise_typing_error "Too many arguments" p
					in
					let patterns = loop el args in
					ignore(unapply_type_parameters ef.ef_params monos);
					PatConstructor(con_enum en ef e1.epos,patterns)
				| _ ->
					fail()
			end
		| EField _ ->
			begin try
				try_typing e
			with
				| Exit -> fail()
				| Bad_pattern s -> raise_typing_error s p
			end
		| EArrayDecl el ->
			let rec pattern seen t = match follow t with
				| TFun(tl,tr) when tr == fake_tuple_type ->
					let rec loop el tl = match el,tl with
						| e :: el,(_,_,t) :: tl ->
							let pat = make pctx false t e in
							pat :: loop el tl
						| [],[] -> []
						| [],_ -> raise_typing_error "Not enough arguments" p
						| (_,p) :: _,[] -> raise_typing_error "Too many arguments" p
					in
					let patterns = loop el tl in
					PatTuple patterns
				| TInst({cl_path=[],"Array"},[t2]) | (TDynamic _ as t2) ->
					let patterns = ExtList.List.mapi (fun i e ->
						make pctx false t2 e
					) el in
					PatConstructor(con_array (List.length patterns) (pos e),patterns)
				| TAbstract(a,tl) as t when not (List.exists (fun t' -> shallow_eq t t') seen) ->
					begin match TyperBase.get_abstract_froms ctx a tl with
						| [t2] -> pattern (t :: seen) t2
						| _ -> fail()
					end
				| _ ->
					fail()
			in
			pattern [] t
		| EObjectDecl fl ->
			let known_fields = ref [] in
			let collect_field cf t filter =	match filter with
				| Some sl when not (List.mem cf.cf_name sl) -> ()
				| _ -> known_fields := (cf,t) :: (List.filter (fun (cf',_) -> cf'.cf_name <> cf.cf_name) !known_fields)
			in
			let rec collect_fields t filter = match follow t with
				| TAnon an ->
					PMap.iter (fun _ cf -> collect_field cf cf.cf_type filter) an.a_fields
				| TInst(c,tl) ->
					let rec loop c tl =
						(match c.cl_super with
							| Some (csup,tlsup) -> loop csup (List.map (apply_params c.cl_params tl) tlsup)
							| _ -> ());
						List.iter (fun cf ->
							if Typecore.can_access ctx c cf false then
								collect_field cf (apply_params c.cl_params tl cf.cf_type) filter
						) c.cl_ordered_fields
					in
					loop c tl
				| TAbstract({a_impl = Some c} as a,tl) ->
					(if Meta.has Meta.Forward a.a_meta then
						let _,el,_ = Meta.get Meta.Forward a.a_meta in
						let sl = ExtList.List.filter_map (fun e -> match fst e with
							| EConst(Ident s) -> Some s
							| _ -> None
						) el in
						let filter = if sl = [] then filter else Some (match filter with
							| Some fsl -> List.filter (fun s -> List.mem s fsl) sl
							| None -> sl
						) in
						collect_fields (Abstract.get_underlying_type a tl) filter);
					List.iter (fun cf ->
						if has_class_field_flag cf CfImpl then
							collect_field cf (apply_params a.a_params tl cf.cf_type) filter
					) c.cl_ordered_statics;
				| _ ->
					raise_typing_error (Printf.sprintf "Cannot field-match against %s" (s_type (print_context()) t)) (pos e)
			in
			collect_fields t None;
			let is_matchable cf =
				match cf.cf_kind with Method _ -> false | _ -> true
			in
			let patterns,fields = List.fold_left (fun (patterns,fields) (cf,t) ->
				try
					if pctx.in_reification && cf.cf_name = "pos" then raise Not_found;
					let e1 = Expr.field_assoc cf.cf_name fl in
					make pctx false t e1 :: patterns,cf.cf_name :: fields
				with Not_found ->
					if is_matchable cf then
						(PatAny,cf.cf_pos) :: patterns,cf.cf_name :: fields
					else
						patterns,fields
			) ([],[]) !known_fields in
			List.iter (fun ((s,_,_),e) -> if not (List.mem s fields) then raise_typing_error (Printf.sprintf "%s has no field %s" (s_type (print_context()) t) s) (pos e)) fl;
			PatConstructor(con_fields fields (pos e),patterns)
		| EBinop(OpOr,e1,e2) ->
			let pctx1 = {pctx with current_locals = PMap.empty} in
			let pat1 = make pctx1 toplevel t e1 in
			let pctx2 = {pctx with current_locals = PMap.empty; or_locals = Some (pctx1.current_locals)} in
			let pat2 = make pctx2 toplevel t e2 in
			PMap.iter (fun name (v,p) ->
				if not (PMap.mem name pctx2.current_locals) && name <> "_" then verror name p;
				pctx.current_locals <- PMap.add name (v,p) pctx.current_locals
			) pctx1.current_locals;
			PatOr(pat1,pat2)
		| EBinop(OpAssign,e1,e2) ->
			let rec loop dko e = match e with
				| (EConst (Ident s),p) ->
					let v = add_local false s p in
					begin match dko with
					| None -> ()
					| Some dk -> ignore(TyperDisplay.display_expr ctx e (mk (TLocal v) v.v_type p) dk (MSet None) (WithType.with_type t) p);
					end;
					let pat = make pctx false t e2 in
					PatBind(v,pat)
				| (EParenthesis e1,_) -> loop dko e1
				| (EDisplay(e1,dk),_) -> loop (Some dk) e1
				| _ -> fail()
			in
			loop None e1
		| EBinop(OpArrow,e1,e2) ->
			let restore = save_locals ctx in
			ctx.locals <- pctx.ctx_locals;
			let v = add_local false "_" null_pos in
			(* Tricky stuff: Extractor expressions are like normal expressions, so we don't want to deal with GADT-applied types here.
			   Let's unapply, then reapply after we're done with the extractor (#5952). *)
			let unapplied = pctx.unapply_type_parameters () in
			let e1 = type_expr ctx e1 WithType.value in
			reapply_type_parameters unapplied;
			v.v_name <- "tmp";
			restore();
			let pat = make pctx toplevel e1.etype e2 in
			PatExtractor {ex_var = v; ex_expr = e1; ex_pattern = pat}
		(* Special case for completion on a pattern local: We don't want to add the local to the context
		   while displaying (#7319) *)
		| EDisplay((EConst (Ident _),_ as e),dk) when pctx.ctx.com.display.dms_kind = DMDefault ->
			let locals = ctx.locals in
			let pat = loop e in
			let locals' = ctx.locals in
			ctx.locals <- locals;
			ignore(TyperDisplay.handle_edisplay ctx e (display_mode()) MGet (WithType.with_type t));
			ctx.locals <- locals';
			pat
		(* For signature completion, we don't want to recurse into the inner pattern because there's probably
		   a EDisplay(_,DMMarked) in there. We can handle display immediately because inner patterns should not
		   matter (#7326) *)
		| EDisplay(e1,DKCall) ->
			ignore(TyperDisplay.handle_edisplay ctx e (display_mode()) MGet (WithType.with_type t));
			loop e1
		| EDisplay(e,dk) ->
			let pat = loop e in
			ignore(TyperDisplay.handle_edisplay ctx e (display_mode()) MGet (WithType.with_type t));
			pat
		| EMeta((Meta.StoredTypedExpr,_,_),e1) ->
			let e1 = MacroContext.type_stored_expr ctx e1 in
			loop (TExprToExpr.convert_expr e1)
		| _ ->
			fail()
	in
	let pat = loop e in
	pat,p