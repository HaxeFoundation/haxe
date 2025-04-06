open Globals
open Ast
open Type
open Typecore
open Common
open Error
open MatcherGlobals
open DecisionTree
open Constructor
open Case
open Texpr.Builder

type match_kind =
	| SKValue
	| SKEnum
	| SKLength

let constructor_to_texpr ctx con =
	let open Typecore in
	let open Constructor in
	let p = pos con in
	match fst con with
	| ConEnum(en,ef) -> mk (TConst (TInt (Int32.of_int ef.ef_index))) ctx.t.tint p
	| ConConst ct -> make_const_texpr ctx.com.basic ct p
	| ConArray i -> make_int ctx.com.basic i p
	| ConTypeExpr mt -> TyperBase.type_module_type ctx mt p
	| ConStatic(c,cf) -> make_static_field c cf p
	| ConFields _ -> raise_typing_error "Unexpected matching on ConFields, please report this" p

let s_subject v_lookup s e =
	let rec loop top s e = match e.eexpr with
		| TField(_,FEnum(en,ef)) ->
			s
		| TField(e1,fa) ->
			if top then loop false s e1
			else loop false (Printf.sprintf "{ %s: %s }" (field_name fa) s) e1
		| TEnumParameter(e1,ef,i) ->
			let arity = match follow ef.ef_type with TFun(args,_) -> List.length args | _ -> die "" __LOC__ in
			let l = make_offset_list i (arity - i - 1) s "_" in
			loop false (Printf.sprintf "%s(%s)" ef.ef_name (String.concat ", " l)) e1
		| TLocal v ->
			begin try
				loop top s (IntMap.find v.v_id v_lookup)
			with Not_found ->
				s
			end
		| _ ->
			s
	in
	loop true s e

let s_match_kind = function
	| SKValue -> "value"
	| SKEnum -> "enum"
	| SKLength -> "length"

let unify_constructor ctx params t con =
	match fst con with
	| ConEnum(en,ef) ->
		let t_ef = match follow ef.ef_type with TFun(_,t) -> t | _ -> ef.ef_type in
		let t_ef = apply_params ctx.type_params params (monomorphs en.e_params (monomorphs ef.ef_params t_ef)) in
		let monos = List.map (fun t -> match follow t with
			| TInst({cl_kind = KTypeParameter _},_) | TMono _ -> mk_mono()
			| _ -> t
		) params in
		let rec duplicate_monos t = match follow t with
			| TMono _ -> mk_mono()
			| _ -> Type.map duplicate_monos t
		in
		let t_e = apply_params ctx.type_params monos (duplicate_monos t) in
		begin try
			Type.unify t_ef t_e;
			Some(con,monos)
		with Unify_error _ ->
			None
		end
	| _ ->
		Some(con,params)

let rec extract_ctor e = match e.eexpr with
	| TConst ct -> Some (ConConst ct)
	| TField(_,FEnum(en,ef)) -> Some (ConEnum(en,ef))
	| TCast(e1,None) -> extract_ctor e1
	| _ -> None

let all_ctors ctx e cases =
	let infer_type() = match cases with
		| [] -> e,e.etype,false
		| sc :: _ ->
			let fail() =
				(* error "Could not determine switch kind, make sure the type is known" e.epos; *)
				t_dynamic
			in
			let t = match fst sc.sc_con with
				| ConEnum(en,_) -> TEnum(en,extract_param_types en.e_params)
				| ConArray _ -> ctx.t.tarray t_dynamic
				| ConConst ct ->
					begin match ct with
						| TString _ -> ctx.t.tstring
						| TInt _ -> ctx.t.tint
						| TFloat _ -> ctx.t.tfloat
						| TBool _ -> ctx.t.tbool
						| _ -> fail()
					end
				| ConStatic({cl_kind = KAbstractImpl a},_) -> (TAbstract(a,extract_param_types a.a_params))
				| ConTypeExpr mt -> ExprToPattern.get_general_module_type ctx mt e.epos
				| ConFields _ | ConStatic _ -> fail()
			in
			e,t,true
	in
	let e,t,inferred = match follow e.etype with
		| TDynamic _ | TMono _ ->
			infer_type()
		| _ ->
			e,e.etype,false
	in
	let h = Compile.ConTable.create 0 in
	let add constructor =
		Compile.ConTable.replace h constructor true
	in
	let rec loop deep t = match follow t with
		| TAbstract({a_path = [],"Bool"},_) ->
			if not deep then begin
				add (ConConst(TBool true),null_pos);
				add (ConConst(TBool false),null_pos);
			end;
			SKValue,RunTimeFinite
		| TAbstract({a_impl = Some c} as a,pl) when a.a_enum ->
			if not deep then List.iter (fun cf ->
				ignore(follow cf.cf_type);
				if has_class_field_flag cf CfImpl && has_class_field_flag cf CfEnum then match cf.cf_expr with
					| Some e ->
						begin match extract_ctor e with
						| Some (ConConst TNull) -> ()
						| Some ctor -> add (ctor,null_pos)
						| None -> add (ConStatic(c,cf),null_pos)
						end;
					| _ -> add (ConStatic(c,cf),null_pos)
			) c.cl_ordered_statics;
			let real_kind,_ = loop true (Abstract.get_underlying_type a pl) in
			real_kind,CompileTimeFinite
		| TAbstract(a,pl) when not (Meta.has Meta.CoreType a.a_meta) ->
			loop deep (Abstract.get_underlying_type a pl)
		| TInst({cl_path=[],"String"},_)
		| TInst({cl_kind = KTypeParameter _ },_) ->
			SKValue,Infinite
		| TInst({cl_path=[],"Array"},_) ->
			SKLength,Infinite
		| TEnum(en,pl) ->
			if not deep then
				PMap.iter (fun _ ef -> add (ConEnum(en,ef),null_pos)) en.e_constrs;
			SKEnum,RunTimeFinite
		| TAnon _ ->
			SKValue,Infinite
		| TInst(_,_) ->
			SKValue,Infinite
		| _ ->
			SKValue,Infinite
	in
	let kind,finiteness = loop false t in
	let compatible_kind con = match fst con with
		| ConEnum _ -> kind = SKEnum
		| ConArray _ -> kind = SKLength
		| _ -> kind = SKValue
	in
	List.iter (fun sc ->
		if not (compatible_kind sc.sc_con) then raise_typing_error "Incompatible pattern" sc.sc_dt.dt_pos;
		if sc.sc_unguarded then Compile.ConTable.remove h sc.sc_con
	) cases;
	let unmatched = Compile.ConTable.fold (fun con _ acc -> con :: acc) h [] in
	e,unmatched,kind,finiteness

let report_not_exhaustive v_lookup e_subject unmatched =
	let sl = match follow e_subject.etype with
		| TAbstract({a_impl = Some c} as a,tl) when a.a_enum ->
			List.map (fun (con,_) -> match fst con with
				| ConConst _ | ConEnum _ ->
					let cf = List.find (fun cf ->
						match cf.cf_expr with
						| Some e ->
							begin match extract_ctor e with
							| Some ctor -> Constructor.equal (ctor,null_pos) con
							| None -> false
							end
						| _ -> false
					) c.cl_ordered_statics in
					cf.cf_name
				| _ ->
					Constructor.to_string con
			) unmatched
		| _ ->
			List.map (fun (con,_) -> Constructor.to_string con) unmatched
	in
	let s = match unmatched with
		| [] -> "_"
		| _ -> String.concat " | " (List.sort Stdlib.compare sl)
	in
	raise_typing_error (Printf.sprintf "Unmatched patterns: %s" (s_subject v_lookup s e_subject)) e_subject.epos

type dt_recursion =
	| Toplevel
	| AfterSwitch
	| Deep

let to_texpr ctx t_switch with_type dt =
	let v_lookup = ref IntMap.empty in
	let com = ctx.com in
	let p = dt.dt_pos in
	let mk_index_call e =
		mk (TEnumIndex e) com.basic.tint e.epos
	in
	let rec loop dt_rec params dt = match dt.dt_texpr with
		| Some e ->
			Some e
		| None ->
			let e = match dt.dt_t with
				| Leaf case ->
					begin match case.case_expr with
						| Some e -> Some e
						| None -> Some (mk (TBlock []) ctx.t.tvoid case.case_pos)
					end
				| Switch(e_subject,[{sc_con = (ConFields _,_)} as sc],_) -> (* TODO: Can we improve this by making it more general? *)
					begin match loop dt_rec params sc.sc_dt with
						| None ->
							None
						| Some e ->
							Some (concat e_subject e)
					end
				| Switch(e_subject,cases,default) ->
					let dt_rec',toplevel = match dt_rec with
						| Toplevel -> AfterSwitch,true
						| AfterSwitch | Deep -> Deep,false
					in
					let e_subject,unmatched,kind,finiteness = all_ctors ctx e_subject cases in
					let unmatched = ExtList.List.filter_map (unify_constructor ctx params e_subject.etype) unmatched in
					let loop params dt = match loop dt_rec' params dt with
						| None ->
							begin match with_type,finiteness with
							| WithType.NoValue,Infinite when toplevel -> None
							| _,CompileTimeFinite when unmatched = [] -> None
							| _ when ignore_error ctx.com -> None
							| _ -> report_not_exhaustive !v_lookup e_subject unmatched
							end
						| Some e ->
							Some e
					in
					let cases = ExtList.List.filter_map (fun sc -> match unify_constructor ctx params e_subject.etype sc.sc_con with
						| Some(_,params) -> Some (sc.sc_con,sc.sc_dt,params)
						| None -> None
					) cases in
					let group cases =
						let h = Compile.DtTable.create 0 in
						List.iter (fun (con,dt,params) ->
							let l,_,_ = try Compile.DtTable.find h dt.dt_t with Not_found -> [],dt,params in
							Compile.DtTable.replace h dt.dt_t (con :: l,dt,params)
						) cases;
						Compile.DtTable.fold (fun _ (cons,dt,params) acc -> (cons,dt,params) :: acc) h []
					in
					let cases = group cases in
					let cases = List.sort (fun (cons1,_,_) (cons2,_,_) -> match cons1,cons2 with
						| (con1 :: _),con2 :: _ -> Constructor.compare con1 con2
						| _ -> -1
					) cases in
					let e_default = match unmatched,finiteness with
						| [],RunTimeFinite ->
							None
						| _ ->
							loop params default
					in
					let cases = ExtList.List.filter_map (fun (cons,dt,params) ->
						let eo = loop params dt in
						begin match eo with
							| None -> None
							| Some e -> Some {case_patterns = List.map (constructor_to_texpr ctx) (List.sort Constructor.compare cons);case_expr = e}
						end
					) cases in
					let is_nullable_subject = is_explicit_null_or_abstract_over_that e_subject.etype in
					let e_subject = match kind with
						| SKValue -> e_subject
						| SKEnum -> mk_index_call e_subject
						| SKLength -> ExprToPattern.type_field_access ctx e_subject "length"
					in
					let e = match cases,e_default,with_type with
						| [case],None,_ when (match finiteness with RunTimeFinite -> true | _ -> false) && not is_nullable_subject ->
							{case.case_expr with etype = t_switch}
						| [{case_patterns = [e1];case_expr = e2}],Some _,_
						| [{case_patterns = [e1];case_expr = e2}],None,NoValue ->
							let e_op = mk (TBinop(OpEq,e_subject,e1)) ctx.t.tbool e_subject.epos in
							begin match e2.eexpr with
								| TIf(e_op2,e3,e_default2) when (match e_default,e_default2 with Some(e1),Some(e2) when e1 == e2 -> true | _ -> false) ->
									let eand = binop OpBoolAnd e_op e_op2 ctx.t.tbool (punion e_op.epos e_op2.epos) in
									mk (TIf(eand,e3,e_default)) t_switch dt.dt_pos
								| _ ->
									mk (TIf(e_op,e2,e_default)) t_switch dt.dt_pos
							end
						| [{case_patterns = [{eexpr = TConst (TBool true)}];case_expr = e2};{case_patterns = [{eexpr = TConst (TBool false)}];case_expr = e1}],None,_
						| [{case_patterns = [{eexpr = TConst (TBool false)}];case_expr = e2};{case_patterns = [{eexpr = TConst (TBool true)}];case_expr = e1}],None,_ ->
							mk (TIf(e_subject,e1,Some e2)) t_switch dt.dt_pos
						| _ ->
							let is_exhaustive = e_default <> None || match finiteness with
								| RunTimeFinite | CompileTimeFinite when e_default = None ->
									true
								| _ ->
									false
							in
							let switch = mk_switch e_subject cases e_default is_exhaustive in
							mk (TSwitch switch) t_switch dt.dt_pos
					in
					Some e
				| Guard(e,dt1,dt2) ->
					(* Normal guards are considered toplevel if we're in the toplevel switch. *)
					let toplevel = match dt_rec with
						| Toplevel | AfterSwitch -> true
						| Deep -> false
					in
					let e_then = loop dt_rec params dt1 in
					begin match e_then with
					| None ->
						None
					| Some e_then ->
						let e_else = loop dt_rec params dt2 in
						begin match e_else with
						| Some e_else ->
							Some (mk (TIf(e,e_then,Some e_else)) t_switch (punion e_then.epos e_else.epos))
						| None ->
							if with_type = NoValue && toplevel then
								Some (mk (TIf(e,e_then,None)) ctx.t.tvoid (punion e.epos e_then.epos))
							else
								None
						end
					end
				| GuardNull(e,dt1,dt2) ->
					let toplevel = match dt_rec with
						| Toplevel -> true
						| Deep | AfterSwitch -> false
					in
					let e_null = make_null e.etype e.epos in
					let f_op e = mk (TBinop(OpEq,e,e_null)) ctx.t.tbool e.epos in
					let rec loop2 acc dt = match dt.dt_t with
						| GuardNull(e,dt1,dt3) when DecisionTree.equal_dt dt2 dt3 ->
							loop2 ((f_op e) :: acc) dt1
						| Guard(e,dt1,dt3) when DecisionTree.equal_dt dt2 dt3 ->
							loop2 (e :: acc) dt1
						| _ ->
							List.rev acc,dt
					in
					let conds,dt1 = loop2 [] dt1 in
					let e_cond = List.fold_left (fun e1 e2 -> binop OpBoolAnd e1 e2 ctx.t.tbool (punion e1.epos e2.epos)) (f_op e) conds in
					let e_then = loop dt_rec params dt1 in
					begin match e_then with
					| None ->
						if toplevel then begin match loop dt_rec params dt2 with
							| None ->
								None
							| Some e_else ->
								(* In some cases like extractors, the original e expression might be significant for the
								   output, so let's make sure it appears there (issue #11738). *)
								let e = mk (TBlock [e;e_else]) e_else.etype e_else.epos in
								Some e
						end else if ignore_error ctx.com then
							Some (mk (TConst TNull) (mk_mono()) dt2.dt_pos)
						else
							report_not_exhaustive !v_lookup e [(ConConst TNull,dt.dt_pos),dt.dt_pos]
					| Some e_then ->
						let e_else = loop dt_rec params dt2 in
						begin match e_else with
						| None ->
							if toplevel && with_type = NoValue then
								Some (mk (TIf(e_cond,e_then,None)) t_switch e_then.epos)
							else
								report_not_exhaustive !v_lookup e []
						| Some e_else ->
							Some (mk (TIf(e_cond,e_then,Some e_else)) t_switch (punion e_then.epos e_else.epos))
						end
					end
				| Bind(bl,dt) ->
					let el = ExtList.List.filter_map (fun bind ->
						begin match bind.Bind.b_status with
							| BindUsed ->
								v_lookup := IntMap.add bind.b_var.v_id bind.b_expr !v_lookup;
								Some (mk (TVar(bind.b_var,Some bind.b_expr)) com.basic.tvoid bind.b_pos)
							| BindDeferred ->
								Some (mk (TVar(bind.b_var,None)) com.basic.tvoid bind.b_pos)
							| BindUnused ->
								None
						end
					) bl in
					let e = loop dt_rec params dt in
					Option.map (fun e -> mk (TBlock (el @ [e])) e.etype dt.dt_pos) e;
				| Fail ->
					None
			in
			dt.dt_texpr <- e;
			e
	in
	let params = extract_param_types ctx.type_params in
	let e = loop Toplevel params dt in
	match e with
	| None ->
		raise_typing_error "Unmatched patterns: _" p;
	| Some e ->
		Texpr.duplicate_tvars e_identity e
