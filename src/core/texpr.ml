open Globals
open Ast
open TType
open TFunctions
open TUnification
open TPrinting
open Error

let iter f e =
	match e.eexpr with
	| TConst _
	| TLocal _
	| TBreak
	| TContinue
	| TTypeExpr _
	| TIdent _ ->
		()
	| TArray (e1,e2)
	| TBinop (_,e1,e2)
	| TFor (_,e1,e2)
	| TWhile (e1,e2,_) ->
		f e1;
		f e2;
	| TThrow e
	| TField (e,_)
	| TEnumParameter (e,_,_)
	| TEnumIndex e
	| TParenthesis e
	| TCast (e,_)
	| TUnop (_,_,e)
	| TMeta(_,e) ->
		f e
	| TArrayDecl el
	| TNew (_,_,el)
	| TBlock el ->
		List.iter f el
	| TObjectDecl fl ->
		List.iter (fun (_,e) -> f e) fl
	| TCall (e,el) ->
		f e;
		List.iter f el
	| TVar (v,eo) ->
		(match eo with None -> () | Some e -> f e)
	| TFunction fu ->
		f fu.tf_expr
	| TIf (e,e1,e2) ->
		f e;
		f e1;
		(match e2 with None -> () | Some e -> f e)
	| TSwitch switch ->
		f switch.switch_subject;
		List.iter (fun case -> List.iter f case.case_patterns; f case.case_expr) switch.switch_cases;
		(match switch.switch_default with None -> () | Some e -> f e)
	| TTry (e,catches) ->
		f e;
		List.iter (fun (_,e) -> f e) catches
	| TReturn eo ->
		(match eo with None -> () | Some e -> f e)

(**
	Returns `true` if `predicate` is evaluated to `true` for at least one of sub-expressions.
	Returns `false` otherwise.
	Does not evaluate `predicate` for the `e` expression.
*)
let check_expr predicate e =
	match e.eexpr with
		| TConst _ | TLocal _ | TBreak | TContinue | TTypeExpr _ | TIdent _ ->
			false
		| TArray (e1,e2) | TBinop (_,e1,e2) | TFor (_,e1,e2) | TWhile (e1,e2,_) ->
			predicate e1 || predicate e2;
		| TThrow e | TField (e,_) | TEnumParameter (e,_,_) | TEnumIndex e | TParenthesis e
		| TCast (e,_) | TUnop (_,_,e) | TMeta(_,e) ->
			predicate e
		| TArrayDecl el | TNew (_,_,el) | TBlock el ->
			List.exists predicate el
		| TObjectDecl fl ->
			List.exists (fun (_,e) -> predicate e) fl
		| TCall (e,el) ->
			predicate e ||  List.exists predicate el
		| TVar (_,eo) | TReturn eo ->
			(match eo with None -> false | Some e -> predicate e)
		| TFunction fu ->
			predicate fu.tf_expr
		| TIf (e,e1,e2) ->
			predicate e || predicate e1 || (match e2 with None -> false | Some e -> predicate e)
		| TSwitch switch ->
			predicate switch.switch_subject
			|| List.exists (fun case -> List.exists predicate case.case_patterns || predicate case.case_expr) switch.switch_cases
			|| (match switch.switch_default with None -> false | Some e -> predicate e)
		| TTry (e,catches) ->
			predicate e || List.exists (fun (_,e) -> predicate e) catches

let map_expr f e =
	match e.eexpr with
	| TConst _
	| TLocal _
	| TBreak
	| TContinue
	| TTypeExpr _
	| TIdent _ ->
		e
	| TArray (e1,e2) ->
		let e1 = f e1 in
		{ e with eexpr = TArray (e1,f e2) }
	| TBinop (op,e1,e2) ->
		let e1 = f e1 in
		{ e with eexpr = TBinop (op,e1,f e2) }
	| TFor (v,e1,e2) ->
		let e1 = f e1 in
		{ e with eexpr = TFor (v,e1,f e2) }
	| TWhile (e1,e2,flag) ->
		let e1 = f e1 in
		{ e with eexpr = TWhile (e1,f e2,flag) }
	| TThrow e1 ->
		{ e with eexpr = TThrow (f e1) }
	| TEnumParameter (e1,ef,i) ->
		{ e with eexpr = TEnumParameter(f e1,ef,i) }
	| TEnumIndex e1 ->
		{ e with eexpr = TEnumIndex (f e1) }
	| TField (e1,v) ->
		{ e with eexpr = TField (f e1,v) }
	| TParenthesis e1 ->
		{ e with eexpr = TParenthesis (f e1) }
	| TUnop (op,pre,e1) ->
		{ e with eexpr = TUnop (op,pre,f e1) }
	| TArrayDecl el ->
		{ e with eexpr = TArrayDecl (List.map f el) }
	| TNew (t,pl,el) ->
		{ e with eexpr = TNew (t,pl,List.map f el) }
	| TBlock el ->
		{ e with eexpr = TBlock (List.map f el) }
	| TObjectDecl el ->
		{ e with eexpr = TObjectDecl (List.map (fun (v,e) -> v, f e) el) }
	| TCall (e1,el) ->
		let e1 = f e1 in
		{ e with eexpr = TCall (e1, List.map f el) }
	| TVar (v,eo) ->
		{ e with eexpr = TVar (v, match eo with None -> None | Some e -> Some (f e)) }
	| TFunction fu ->
		{ e with eexpr = TFunction { fu with tf_expr = f fu.tf_expr } }
	| TIf (ec,e1,e2) ->
		let ec = f ec in
		let e1 = f e1 in
		{ e with eexpr = TIf (ec,e1,match e2 with None -> None | Some e -> Some (f e)) }
	| TSwitch switch ->
		let e1 = f switch.switch_subject in
		let cases = List.map (fun case -> {
			case_patterns = List.map f case.case_patterns;
			case_expr = f case.case_expr
		}) switch.switch_cases in
		let def = Option.map f switch.switch_default in
		{ e with eexpr = TSwitch {switch with switch_subject = e1;switch_cases = cases;switch_default = def} }
	| TTry (e1,catches) ->
		let e1 = f e1 in
		{ e with eexpr = TTry (e1, List.map (fun (v,e) -> v, f e) catches) }
	| TReturn eo ->
		{ e with eexpr = TReturn (match eo with None -> None | Some e -> Some (f e)) }
	| TCast (e1,t) ->
		{ e with eexpr = TCast (f e1,t) }
	| TMeta (m,e1) ->
		 {e with eexpr = TMeta(m,f e1)}

let map_expr_type f ft fv e =
	match e.eexpr with
	| TConst _
	| TBreak
	| TContinue
	| TTypeExpr _
	| TIdent _ ->
		{ e with etype = ft e.etype }
	| TLocal v ->
		{ e with eexpr = TLocal (fv v); etype = ft e.etype }
	| TArray (e1,e2) ->
		let e1 = f e1 in
		{ e with eexpr = TArray (e1,f e2); etype = ft e.etype }
	| TBinop (op,e1,e2) ->
		let e1 = f e1 in
		{ e with eexpr = TBinop (op,e1,f e2); etype = ft e.etype }
	| TFor (v,e1,e2) ->
		let v = fv v in
		let e1 = f e1 in
		{ e with eexpr = TFor (v,e1,f e2); etype = ft e.etype }
	| TWhile (e1,e2,flag) ->
		let e1 = f e1 in
		{ e with eexpr = TWhile (e1,f e2,flag); etype = ft e.etype }
	| TThrow e1 ->
		{ e with eexpr = TThrow (f e1); etype = ft e.etype }
	| TEnumParameter (e1,ef,i) ->
		{ e with eexpr = TEnumParameter (f e1,ef,i); etype = ft e.etype }
	| TEnumIndex e1 ->
		{ e with eexpr = TEnumIndex (f e1); etype = ft e.etype }
	| TField (e1,(FClosure(None,cf) as fa)) ->
		let e1 = f e1 in
		let fa = try
			begin match quick_field e1.etype cf.cf_name with
				| FInstance(c,tl,cf) ->
					FClosure(Some(c,tl),cf)
				| _ ->
					raise Not_found
			end
		with Not_found ->
			fa
		in
		{ e with eexpr = TField (e1,fa); etype = ft e.etype }
	| TField (e1,v) ->
		let e1 = f e1 in
		let v = try
			let n = match v with
				| FClosure _ -> raise Not_found
				| FAnon f | FInstance (_,_,f) | FStatic (_,f) -> f.cf_name
				| FEnum (_,f) -> f.ef_name
				| FDynamic n -> n
			in
			quick_field e1.etype n
		with Not_found ->
			v
		in
		{ e with eexpr = TField (e1,v); etype = ft e.etype }
	| TParenthesis e1 ->
		{ e with eexpr = TParenthesis (f e1); etype = ft e.etype }
	| TUnop (op,pre,e1) ->
		{ e with eexpr = TUnop (op,pre,f e1); etype = ft e.etype }
	| TArrayDecl el ->
		{ e with eexpr = TArrayDecl (List.map f el); etype = ft e.etype }
	| TNew (c,pl,el) ->
		let et = ft e.etype in
		(* make sure that we use the class corresponding to the replaced type *)
		let t = match c.cl_kind with
			| KTypeParameter _ | KGeneric ->
				et
			| _ ->
				ft (TInst(c,pl))
		in
		let c, pl = (match follow t with TInst (c,pl) -> (c,pl) | TAbstract({a_impl = Some c},pl) -> c,pl | t -> TUnification.error [has_no_field t "new"]) in
		{ e with eexpr = TNew (c,pl,List.map f el); etype = et }
	| TBlock el ->
		{ e with eexpr = TBlock (List.map f el); etype = ft e.etype }
	| TObjectDecl el ->
		{ e with eexpr = TObjectDecl (List.map (fun (v,e) -> v, f e) el); etype = ft e.etype }
	| TCall (e1,el) ->
		let e1 = f e1 in
		{ e with eexpr = TCall (e1, List.map f el); etype = ft e.etype }
	| TVar (v,eo) ->
		{ e with eexpr = TVar (fv v, match eo with None -> None | Some e -> Some (f e)); etype = ft e.etype }
	| TFunction fu ->
		let fu = {
			tf_expr = f fu.tf_expr;
			tf_args = List.map (fun (v,o) -> fv v, (Option.map f o)) fu.tf_args;
			tf_type = ft fu.tf_type;
		} in
		{ e with eexpr = TFunction fu; etype = ft e.etype }
	| TIf (ec,e1,e2) ->
		let ec = f ec in
		let e1 = f e1 in
		{ e with eexpr = TIf (ec,e1,match e2 with None -> None | Some e -> Some (f e)); etype = ft e.etype }
	| TSwitch switch ->
		let e1 = f switch.switch_subject in
		let cases = List.map (fun case -> {
			case_patterns = List.map f case.case_patterns;
			case_expr = f case.case_expr
		}) switch.switch_cases in
		let def = Option.map f switch.switch_default in
		{ e with eexpr = TSwitch {switch with switch_subject = e1;switch_cases = cases;switch_default = def}; etype = ft e.etype }
	| TTry (e1,catches) ->
		let e1 = f e1 in
		{ e with eexpr = TTry (e1, List.map (fun (v,e) -> fv v, f e) catches); etype = ft e.etype }
	| TReturn eo ->
		{ e with eexpr = TReturn (match eo with None -> None | Some e -> Some (f e)); etype = ft e.etype }
	| TCast (e1,t) ->
		{ e with eexpr = TCast (f e1,t); etype = ft e.etype }
	| TMeta (m,e1) ->
		{e with eexpr = TMeta(m, f e1); etype = ft e.etype }

let equal_fa fa1 fa2 = match fa1,fa2 with
	| FStatic(c1,cf1),FStatic(c2,cf2) -> c1 == c2 && cf1.cf_name == cf2.cf_name
	| FInstance(c1,tl1,cf1),FInstance(c2,tl2,cf2) -> c1 == c2 && safe_for_all2 type_iseq tl1 tl2 && cf1.cf_name == cf2.cf_name
	| FAnon cf1,FAnon cf2 -> cf1.cf_name = cf2.cf_name
	| FDynamic s1,FDynamic s2 -> s1 = s2
	| FClosure(None,cf1),FClosure(None,cf2) -> cf1.cf_name == cf2.cf_name
	| FClosure(Some(c1,tl1),cf1),FClosure(Some(c2,tl2),cf2) -> c1 == c2 && safe_for_all2 type_iseq tl1 tl2 && cf1.cf_name == cf2.cf_name
	| FEnum(en1,ef1),FEnum(en2,ef2) -> en1 == en2 && ef1.ef_name == ef2.ef_name
	| _ -> false

let rec equal e1 e2 = match e1.eexpr,e2.eexpr with
	| TConst ct1,TConst ct2 -> ct1 = ct2
	| TLocal v1,TLocal v2 -> v1 == v2
	| TArray(eb1,ei1),TArray(eb2,ei2) -> equal eb1 eb2 && equal ei1 ei2
	| TBinop(op1,lhs1,rhs1),TBinop(op2,lhs2,rhs2) -> op1 = op2 && equal lhs1 lhs2 && equal rhs1 rhs2
	| TField(e1,fa1),TField(e2,fa2) -> equal e1 e2 && equal_fa fa1 fa2
	| TTypeExpr (TClassDecl c1),TTypeExpr (TClassDecl c2) -> c1 == c2
	| TTypeExpr (TEnumDecl e1),TTypeExpr (TEnumDecl e2) -> e1 == e2
	| TTypeExpr (TTypeDecl t1),TTypeExpr (TTypeDecl t2) -> t1 == t2
	| TTypeExpr (TAbstractDecl a1),TTypeExpr (TAbstractDecl a2) -> a1 == a2
	| TTypeExpr _,TTypeExpr _ -> false
	| TParenthesis e1,TParenthesis e2 -> equal e1 e2
	| TObjectDecl fl1,TObjectDecl fl2 -> safe_for_all2 (fun (s1,e1) (s2,e2) -> s1 = s2 && equal e1 e2) fl1 fl2
	| (TArrayDecl el1,TArrayDecl el2) | (TBlock el1,TBlock el2) -> safe_for_all2 equal el1 el2
	| TCall(e1,el1),TCall(e2,el2) -> equal e1 e2 && safe_for_all2 equal el1 el2
	| TNew(c1,tl1,el1),TNew(c2,tl2,el2) -> c1 == c2 && safe_for_all2 type_iseq tl1 tl2 && safe_for_all2 equal el1 el2
	| TUnop(op1,flag1,e1),TUnop(op2,flag2,e2) -> op1 = op2 && flag1 = flag2 && equal e1 e2
	| TFunction tf1,TFunction tf2 -> tf1 == tf2
	| TVar(v1,None),TVar(v2,None) -> v1 == v2
	| TVar(v1,Some e1),TVar(v2,Some e2) -> v1 == v2 && equal e1 e2
	| TFor(v1,ec1,eb1),TFor(v2,ec2,eb2) -> v1 == v2 && equal ec1 ec2 && equal eb1 eb2
	| TIf(e1,ethen1,None),TIf(e2,ethen2,None) -> equal e1 e2 && equal ethen1 ethen2
	| TIf(e1,ethen1,Some eelse1),TIf(e2,ethen2,Some eelse2) -> equal e1 e2 && equal ethen1 ethen2 && equal eelse1 eelse2
	| TWhile(e1,eb1,flag1),TWhile(e2,eb2,flag2) -> equal e1 e2 && equal eb2 eb2 && flag1 = flag2
	| TSwitch switch1,TSwitch switch2 ->
		equal switch1.switch_subject switch2.switch_subject &&
		safe_for_all2 (fun case1 case2 -> safe_for_all2 equal case1.case_patterns case2.case_patterns && equal case1.case_expr case2.case_expr) switch1.switch_cases switch2.switch_cases &&
		(match switch1.switch_default,switch2.switch_default with None,None -> true | Some e1,Some e2 -> equal e1 e2 | _ -> false)
	| TTry(e1,catches1),TTry(e2,catches2) -> equal e1 e2 && safe_for_all2 (fun (v1,e1) (v2,e2) -> v1 == v2 && equal e1 e2) catches1 catches2
	| TReturn None,TReturn None -> true
	| TReturn(Some e1),TReturn(Some e2) -> equal e1 e2
	| TThrow e1,TThrow e2 -> equal e1 e2
	| TCast(e1,None),TCast(e2,None) -> equal e1 e2
	| TCast(e1,Some mt1),TCast(e2,Some mt2) -> equal e1 e2 && mt1 == mt2
	| TMeta((m1,el1,_),e1),TMeta((m2,el2,_),e2) -> m1 = m2 && safe_for_all2 (fun e1 e2 -> (* TODO: cheating? *) (Ast.Printer.s_expr e1) = (Ast.Printer.s_expr e2)) el1 el2 && equal e1 e2
	| (TBreak,TBreak) | (TContinue,TContinue) -> true
	| TEnumParameter(e1,ef1,i1),TEnumParameter(e2,ef2,i2) -> equal e1 e2 && ef1 == ef2 && i1 = i2
	| _ -> false

let e_identity e = e

let duplicate_tvars f_this e =
	let vars = Hashtbl.create 0 in
	let copy_var v =
		let v2 = alloc_var v.v_kind v.v_name v.v_type v.v_pos in
		v2.v_meta <- v.v_meta;
		v2.v_extra <- v.v_extra;
		v2.v_flags <- v.v_flags;
		Hashtbl.add vars v.v_id v2;
		v2;
	in
	let rec build_expr e =
		match e.eexpr with
		| TVar (v,eo) ->
			let v2 = copy_var v in
			{e with eexpr = TVar(v2, Option.map build_expr eo)}
		| TFor (v,e1,e2) ->
			let v2 = copy_var v in
			{e with eexpr = TFor(v2, build_expr e1, build_expr e2)}
		| TTry (e1,cl) ->
			let cl = List.map (fun (v,e) ->
				let v2 = copy_var v in
				v2, build_expr e
			) cl in
			{e with eexpr = TTry(build_expr e1, cl)}
		| TFunction f ->
			let args = List.map (fun (v,c) -> copy_var v, c) f.tf_args in
			let f = {
				tf_args = args;
				tf_type = f.tf_type;
				tf_expr = build_expr f.tf_expr;
			} in
			{e with eexpr = TFunction f}
		| TLocal v ->
			(try
				let v2 = Hashtbl.find vars v.v_id in
				{e with eexpr = TLocal v2}
			with _ ->
				e)
		| TConst TThis ->
			f_this e
		| _ ->
			map_expr build_expr e
	in
	build_expr e

let rec skip e = match e.eexpr with
	| TParenthesis e1 | TMeta(_,e1) | TBlock [e1] | TCast(e1,None) -> skip e1
	| _ -> e

let foldmap_list f acc el =
	let rec loop acc el acc2 = (match el with
		| [] -> acc,(List.rev acc2)
		| e1 :: el ->
			let acc,e1 = f acc e1 in
			loop acc el (e1 :: acc2))
	in loop acc el []

let foldmap_opt f acc eo = match eo with
	| Some(e) -> let acc,e = f acc e in acc,Some(e)
	| None    -> acc,eo

let foldmap_pairs f acc pairs =
	let acc,pairs = List.fold_left
		(fun (acc,el) (v,e) -> let acc,e = f acc e in (acc,(v,e) :: el))
		(acc,[])
		pairs
	in acc,(List.rev pairs)

let foldmap f acc e =
	begin match e.eexpr with
	| TConst _
	| TLocal _
	| TBreak
	| TContinue
	| TTypeExpr _
	| TIdent _ ->
		acc,e
	| TArray (e1,e2) ->
		let acc,e1 = f acc e1 in
		let acc,e2 = f acc e2 in
		acc,{ e with eexpr = TArray (e1, e2) }
	| TBinop (op,e1,e2) ->
		let acc,e1 = f acc e1 in
		let acc,e2 = f acc e2 in
		acc,{ e with eexpr = TBinop (op,e1,e2) }
	| TFor (v,e1,e2) ->
		let acc,e1 = f acc e1 in
		let acc,e2 = f acc e2 in
		acc,{ e with eexpr = TFor (v,e1,e2) }
	| TWhile (e1,e2,flag) ->
		let acc,e1 = f acc e1 in
		let acc,e2 = f acc e2 in
		acc,{ e with eexpr = TWhile (e1,e2,flag) }
	| TThrow e1 ->
		let acc,e1 = f acc e1 in
		acc,{ e with eexpr = TThrow (e1) }
	| TEnumParameter (e1,ef,i) ->
		let acc,e1 = f acc e1 in
		acc,{ e with eexpr = TEnumParameter(e1,ef,i) }
	| TEnumIndex e1 ->
		let acc,e1 = f acc e1 in
		acc,{ e with eexpr = TEnumIndex e1 }
	| TField (e1,v) ->
		let acc,e1 = f acc e1 in
		acc,{ e with eexpr = TField (e1,v) }
	| TParenthesis e1 ->
		let acc,e1 = f acc e1 in
		acc,{ e with eexpr = TParenthesis (e1) }
	| TUnop (op,pre,e1) ->
		let acc,e1 = f acc e1 in
		acc,{ e with eexpr = TUnop (op,pre,e1) }
	| TArrayDecl el ->
		let acc,el = foldmap_list f acc el in
		acc,{ e with eexpr = TArrayDecl el }
	| TNew (t,pl,el) ->
		let acc,el = foldmap_list f acc el in
		acc,{ e with eexpr = TNew (t,pl,el) }
	| TBlock el ->
		let acc,el = foldmap_list f acc el in
		acc,{ e with eexpr = TBlock (el) }
	| TObjectDecl el ->
		let acc,el = foldmap_pairs f acc el in
		acc,{ e with eexpr = TObjectDecl el }
	| TCall (e1,el) ->
		let acc,e1 = f acc e1 in
		let acc,el = foldmap_list f acc el in
		acc,{ e with eexpr = TCall (e1,el) }
	| TVar (v,eo) ->
		let acc,eo = foldmap_opt f acc eo in
		acc,{ e with eexpr = TVar (v, eo) }
	| TFunction fu ->
		let acc,e1 = f acc fu.tf_expr in
		acc,{ e with eexpr = TFunction { fu with tf_expr = e1 } }
	| TIf (ec,e1,eo) ->
		let acc,ec = f acc ec in
		let acc,e1 = f acc e1 in
		let acc,eo = foldmap_opt f acc eo in
		acc,{ e with eexpr = TIf (ec,e1,eo)}
	| TSwitch switch ->
		let acc,e1 = f acc switch.switch_subject in
		let acc,cases = List.fold_left (fun (acc,cases) case ->
			let acc,el = foldmap_list f acc case.case_patterns in
			let acc,e2 = f acc case.case_expr in
			acc,({case_patterns = el;case_expr = e2} :: cases)
		) (acc,[]) switch.switch_cases in
		let acc,def = foldmap_opt f acc switch.switch_default in
		acc,{ e with eexpr = TSwitch {switch with switch_subject = e1;switch_cases = cases;switch_default = def} }
	| TTry (e1,catches) ->
		let acc,e1 = f acc e1 in
		let acc,catches = foldmap_pairs f acc catches in
		acc,{ e with eexpr = TTry (e1, catches) }
	| TReturn eo ->
		let acc,eo = foldmap_opt f acc eo in
		acc,{ e with eexpr = TReturn eo }
	| TCast (e1,t) ->
		let acc,e1 = f acc e1 in
		acc,{ e with eexpr = TCast (e1,t) }
	| TMeta (m,e1) ->
		let acc,e1 = f acc e1 in
		acc,{ e with eexpr = TMeta(m,e1)}
	end

(* Collection of functions that return expressions *)
module Builder = struct
	let make_static_this c p =
		mk (TTypeExpr (TClassDecl c)) c.cl_type p

	let make_typeexpr mt pos =
		let t =
			match resolve_typedef mt with
			| TClassDecl c -> c.cl_type
			| TEnumDecl e -> e.e_type
			| TAbstractDecl a -> TType(abstract_module_type a [],[])
			| _ -> die "" __LOC__
		in
		mk (TTypeExpr mt) t pos

	let make_static_field c cf p =
		let e_this = make_static_this c p in
		mk (TField(e_this,FStatic(c,cf))) cf.cf_type p

	let make_throw e p =
		mk (TThrow e) t_dynamic p

	let make_int basic i p =
		mk (TConst (TInt (Int32.of_int i))) basic.tint p

	let make_float basic f p =
		mk (TConst (TFloat f)) basic.tfloat p

	let make_bool basic b p =
		mk (TConst(TBool b)) basic.tbool p

	let make_string basic s p =
		mk (TConst (TString s)) basic.tstring p

	let make_null t p =
		mk (TConst TNull) t p

	let make_local v p =
		mk (TLocal v) v.v_type p

	let make_const_texpr basic ct p = match ct with
		| TString s -> mk (TConst (TString s)) basic.tstring p
		| TInt i -> mk (TConst (TInt i)) basic.tint p
		| TFloat f -> mk (TConst (TFloat f)) basic.tfloat p
		| TBool b -> mk (TConst (TBool b)) basic.tbool p
		| TNull -> mk (TConst TNull) (basic.tnull (mk_mono())) p
		| _ -> raise_typing_error "Unsupported constant" p

	let field e name t p =
		let f =
			try
				quick_field e.etype name
			with Not_found ->
				let field = (s_type (print_context()) e.etype) ^ "." ^ name in
				die ("Field " ^ field ^ " requested but not found") __LOC__
		in
		mk (TField (e,f)) t p

	let fcall e name el ret p =
		let ft = tfun (List.map (fun e -> e.etype) el) ret in
		mk (TCall (field e name ft p,el)) ret p

	let mk_parent e =
		mk (TParenthesis e) e.etype e.epos

	let ensure_parent e =
		match e.eexpr with
		| TParenthesis _ -> e
		| _ -> mk_parent e

	let mk_return e =
		mk (TReturn (Some e)) t_dynamic e.epos

	let binop op a b t p =
		mk (TBinop (op,a,b)) t p

	let index basic e index t p =
		mk (TArray (e,mk (TConst (TInt (Int32.of_int index))) basic.tint p)) t p

	let resolve_and_make_static_call c name args p =
		ignore(c.cl_build());
		let cf = try
			PMap.find name c.cl_statics
		with Not_found ->
			die "" __LOC__
		in
		let ef = make_static_field c cf (mk_zero_range_pos p) in
		let tret = match follow ef.etype with
			| TFun(_,r) -> r
			| _ -> assert false
		in
		mk (TCall (ef, args)) tret p

end

let set_default basic a c p =
	let t = a.v_type in
	let ve = mk (TLocal a) t p in
	let cond =  TBinop (OpEq,ve,mk (TConst TNull) t p) in
	mk (TIf (Builder.mk_parent (mk cond basic.tbool p), mk (TBinop (OpAssign,ve,c)) t p,None)) basic.tvoid p

(*
	Tells if the constructor might be called without any issue whatever its parameters
*)
let rec constructor_side_effects e =
	match e.eexpr with
	| TBinop (op,_,_) when op <> OpAssign ->
		true
	| TField (_,FEnum _) ->
		false
	| TUnop _ | TArray _ | TField _ | TEnumParameter _ | TEnumIndex _ | TCall _ | TNew _ | TFor _ | TWhile _ | TSwitch _ | TReturn _ | TThrow _ ->
		true
	| TBinop _ | TTry _ | TIf _ | TBlock _ | TVar _
	| TFunction _ | TArrayDecl _ | TObjectDecl _
	| TParenthesis _ | TTypeExpr _ | TLocal _ | TMeta _
	| TConst _ | TContinue | TBreak | TCast _ | TIdent _ ->
		try
			iter (fun e -> if constructor_side_effects e then raise Exit) e;
			false;
		with Exit ->
			true

let replace_separators s c =
	String.concat c (ExtString.String.nsplit s "_")

let type_constant basic c p =
	match c with
	| Int (s,_) ->
		if String.length s > 10 && String.sub s 0 2 = "0x" then raise_typing_error "Invalid hexadecimal integer" p;
		if String.length s > 34 && String.sub s 0 2 = "0b" then raise_typing_error "Invalid binary integer" p;
		(try mk (TConst (TInt (Int32.of_string s))) basic.tint p
		with _ -> mk (TConst (TFloat s)) basic.tfloat p)
	| Float (f,_) -> mk (TConst (TFloat f)) basic.tfloat p
	| String(s,qs) -> mk (TConst (TString s)) basic.tstring p (* STRINGTODO: qs? *)
	| Ident "true" -> mk (TConst (TBool true)) basic.tbool p
	| Ident "false" -> mk (TConst (TBool false)) basic.tbool p
	| Ident "null" -> mk (TConst TNull) (basic.tnull (mk_mono())) p
	| Ident t -> raise_typing_error ("Invalid constant :  " ^ t) p
	| Regexp _ -> raise_typing_error "Invalid constant" p

let rec type_constant_value basic (e,p) =
	match e with
	| EConst c ->
		type_constant basic c p
	| EParenthesis e ->
		type_constant_value basic e
	| EObjectDecl el ->
		mk (TObjectDecl (List.map (fun (k,e) -> k,type_constant_value basic e) el)) (mk_anon (ref Closed)) p
	| EArrayDecl el ->
		mk (TArrayDecl (List.map (type_constant_value basic) el)) (basic.tarray t_dynamic) p
	| _ ->
		raise_typing_error "Constant value expected" p

let is_constant_value basic e =
	try (ignore (type_constant_value basic e); true) with Error {err_message = Custom _} -> false

let for_remap basic v e1 e2 p =
	let v' = alloc_var v.v_kind v.v_name e1.etype e1.epos in
	let ev' = mk (TLocal v') e1.etype e1.epos in
	let t1 = (Abstract.follow_with_abstracts e1.etype) in
	let ehasnext = mk (TField(ev',try quick_field t1 "hasNext" with Not_found -> raise_typing_error (s_type (print_context()) t1 ^ "has no field hasNext()") p)) (tfun [] basic.tbool) e1.epos in
	let ehasnext = mk (TCall(ehasnext,[])) basic.tbool ehasnext.epos in
	let enext = mk (TField(ev',quick_field t1 "next")) (tfun [] v.v_type) e1.epos in
	let enext = mk (TCall(enext,[])) v.v_type e1.epos in
	let eassign = mk (TVar(v,Some enext)) basic.tvoid p in
	let ebody = concat eassign e2 in
	mk (TBlock [
		mk (TVar (v',Some e1)) basic.tvoid e1.epos;
		mk (TWhile((mk (TParenthesis ehasnext) ehasnext.etype ehasnext.epos),ebody,NormalWhile)) basic.tvoid e1.epos;
	]) basic.tvoid p

(* -------------------------------------------------------------------------- *)
(* BUILD META DATA OBJECT *)

let build_metadata api t =
	let p, meta, fields, statics = (match t with
		| TClassDecl c ->
			let fields = List.map (fun f -> f.cf_name,f.cf_meta) (c.cl_ordered_fields @ (match c.cl_constructor with None -> [] | Some f -> [{ f with cf_name = "_" }])) in
			let statics =  List.map (fun f -> f.cf_name,f.cf_meta) c.cl_ordered_statics in
			(c.cl_pos, ["",c.cl_meta],fields,statics)
		| TEnumDecl e ->
			(e.e_pos, ["",e.e_meta],List.map (fun n -> n, (PMap.find n e.e_constrs).ef_meta) e.e_names, [])
		| TTypeDecl t ->
			(t.t_pos, ["",t.t_meta],(match follow t.t_type with TAnon a -> PMap.fold (fun f acc -> (f.cf_name,f.cf_meta) :: acc) a.a_fields [] | _ -> []),[])
		| TAbstractDecl a ->
			(a.a_pos, ["",a.a_meta],[],[])
	) in
	let filter l =
		let l = List.map (fun (n,ml) -> n, ExtList.List.filter_map (fun (m,el,p) -> match m with Meta.Custom s when String.length s > 0 && s.[0] <> ':' -> Some (s,el,p) | _ -> None) ml) l in
		List.filter (fun (_,ml) -> ml <> []) l
	in
	let meta, fields, statics = filter meta, filter fields, filter statics in
	let make_meta_field ml =
		let h = Hashtbl.create 0 in
		mk (TObjectDecl (List.map (fun (f,el,p) ->
			if Hashtbl.mem h f then raise_typing_error ("Duplicate metadata '" ^ f ^ "'") p;
			Hashtbl.add h f ();
			(f,null_pos,NoQuotes), mk (match el with [] -> TConst TNull | _ -> TArrayDecl (List.map (type_constant_value api) el)) (api.tarray t_dynamic) p
		) ml)) t_dynamic p
	in
	let make_meta l =
		mk (TObjectDecl (List.map (fun (f,ml) -> (f,null_pos,NoQuotes),make_meta_field ml) l)) t_dynamic p
	in
	if meta = [] && fields = [] && statics = [] then
		None
	else
		let meta_obj = [] in
		let meta_obj = (if fields = [] then meta_obj else (("fields",null_pos,NoQuotes),make_meta fields) :: meta_obj) in
		let meta_obj = (if statics = [] then meta_obj else (("statics",null_pos,NoQuotes),make_meta statics) :: meta_obj) in
		let meta_obj = (try (("obj",null_pos,NoQuotes), make_meta_field (List.assoc "" meta)) :: meta_obj with Not_found -> meta_obj) in
		Some (mk (TObjectDecl meta_obj) t_dynamic p)

let dump_with_pos tabs e =
	let buf = Buffer.create 0 in
	let add = Buffer.add_string buf in
	let rec loop' tabs e =
		let p = e.epos in
		let add s = add (Printf.sprintf "%4i-%4i %s%s\n" p.pmin p.pmax tabs s) in
		let loop e = loop' (tabs ^ "  ") e in
		match e.eexpr with
		| TConst ct -> add (s_const ct)
		| TLocal v -> add ("TLocal " ^ v.v_name)
		| TTypeExpr mt -> add ("TTypeExpr " ^ (s_type_path (t_infos mt).mt_path))
		| TIdent s -> add ("TIdent " ^ s)
		| TEnumParameter(e1,ef,_) ->
			add ("TEnumParameter " ^ ef.ef_name);
			loop e1
		| TEnumIndex e1 ->
			add "TEnumIndex";
			loop e1
		| TArray(e1,e2) ->
			add "TArray";
			loop e1;
			loop e2;
		| TBinop(op,e1,e2) ->
			add ("TBinop " ^ (s_binop op));
			loop e1;
			loop e2;
		| TField(e1,s) ->
			add ("TField " ^ (field_name s));
			loop e1
		| TParenthesis e1 ->
			add "TParenthesis";
			loop e1
		| TObjectDecl fl ->
			add "TObjectDecl";
			List.iter (fun ((n,p,_),e1) ->
				Buffer.add_string buf (Printf.sprintf "%4i-%4i %s%s\n" p.pmin p.pmax tabs n);
				loop e1
			) fl;
		| TArrayDecl el ->
			add "TArrayDecl";
			List.iter loop el
		| TCall(e1,el) ->
			add "TCall";
			loop e1;
			List.iter loop el
		| TNew(c,_,el) ->
			add ("TNew " ^ s_type_path c.cl_path);
			List.iter loop el
		| TUnop(op,_,e1) ->
			add ("TUnop " ^ (s_unop op));
			loop e1
		| TVar(v,eo) ->
			add ("TVar " ^ v.v_name);
			begin match eo with
				| None -> ()
				| Some e ->
					loop' (Printf.sprintf "%s  " tabs) e
			end
		| TFunction tf ->
			add "TFunction";
			loop tf.tf_expr;
		| TBlock el ->
			add "TBlock";
			List.iter loop el
		| TFor(v,e1,e2) ->
			add ("TFor " ^ v.v_name);
			loop e1;
			loop e2;
		| TIf(e1,e2,eo) ->
			add "TIf";
			loop e1;
			loop e2;
			Option.may loop eo;
		| TWhile(e1,e2,_) ->
			add "TWhile";
			loop e1;
			loop e2;
		| TSwitch switch ->
			add "TSwitch";
			loop switch.switch_subject;
			List.iter (fun case ->
				List.iter (loop' (tabs ^ "    ")) case.case_patterns;
				loop' (tabs ^ "      ") case.case_expr;
			) switch.switch_cases;
			Option.may (loop' (tabs ^ "      ")) switch.switch_default
		| TTry(e1,catches) ->
			add "TTry";
			loop e1;
			List.iter (fun (v,e) ->
				loop' (tabs ^ "    ") e
			) catches
		| TReturn eo ->
			add "TReturn";
			Option.may loop eo;
		| TBreak ->
			add "TBreak";
		| TContinue ->
			add "TContinue"
		| TThrow e1 ->
			add "EThrow";
			loop e1
		| TCast(e1,_) ->
			add "TCast";
			loop e1;
		| TMeta((m,_,_),e1) ->
			add ("TMeta " ^ (Meta.to_string m));
			loop e1
	in
	loop' tabs e;
	Buffer.contents buf

let collect_captured_vars e =
	let known = Hashtbl.create 0 in
	let unknown = ref [] in
	let accesses_this = ref false in
	let declare v = Hashtbl.add known v.v_id () in
	let rec loop e = match e.eexpr with
		| TLocal v when has_var_flag v VCaptured &&  not (Hashtbl.mem known v.v_id) ->
			Hashtbl.add known v.v_id ();
			unknown := v :: !unknown
		| TConst (TThis | TSuper) ->
			accesses_this := true;
		| TVar(v,eo) ->
			Option.may loop eo;
			declare v
		| TFor(v,e1,e2) ->
			declare v;
			loop e1;
			loop e2;
		| TFunction tf ->
			List.iter (fun (v,_) -> declare v) tf.tf_args;
			loop tf.tf_expr
		| TTry(e1,catches) ->
			loop e1;
			List.iter (fun (v,e) ->
				declare v;
				loop e;
			) catches
		| _ ->
			iter loop e
	in
	loop e;
	List.rev !unknown,!accesses_this

(**
	If `e` contains a sequence of unsafe casts, then look if that sequence
	already has casts to `t` and return the bottom-most of such casts.
	If `require_cast` is `false` and the first non-cast expression has type `t`, then return that expression without any casts.
	In other cases return `e` as-is.
*)
let reduce_unsafe_casts ?(require_cast=false) e t =
	let same_type t1 t2 =
		fast_eq (follow_without_null t1) (follow_without_null t2)
	in
	let rec loop e =
		match e.eexpr with
		| TCast(subject,None) ->
			let result = loop subject in
			if same_type e.etype subject.etype then result
			else { e with eexpr = TCast(result,None) }
		| _ -> e
	in
	match loop e with
	| { eexpr = TCast _ } as result -> result
	| result when require_cast -> { e with eexpr = TCast(result,None) }
	| result -> result

(**
	Returns a position spanning from the first expr to the last expr in `el`.
	Returns `default_pos` if `el` is empty or values of `pfile` of the first and
	the last positions are different.
*)
let punion_el default_pos el =
	match el with
	| [] -> default_pos
	| [{ epos = p }] -> p
	| { epos = first } :: { epos = last } :: el ->
		let rec loop = function
			| [] -> last
			| [{ epos = last }] -> last
			| _ :: el -> loop el
		in
		let last = loop el in
		if first.pfile <> last.pfile then
			default_pos
		else
			punion first last

let is_exhaustive switch =
	switch.switch_exhaustive

let mk_switch subject cases default exhaustive = {
	switch_subject = subject;
	switch_cases = cases;
	switch_default = default;
	switch_exhaustive = exhaustive;
}

let rec is_true_expr e1 = match e1.eexpr with
	| TConst(TBool true) -> true
	| TParenthesis e1 -> is_true_expr e1
	| _ -> false

let rec is_false_expr e1 = match e1.eexpr with
	| TConst(TBool false) -> true
	| TParenthesis e1 -> is_false_expr e1
	| _ -> false

module DeadEnd = struct
	exception BreakOrContinue

	(*
		Checks if execution of provided expression is guaranteed to be terminated with `return`, `throw`, `break` or `continue`.
	*)
	let has_dead_end e =
		let rec loop e =
			let in_loop e =
				try
					loop e
				with BreakOrContinue ->
					false
			in
			match e.eexpr with
			| TContinue | TBreak ->
				raise BreakOrContinue
			| TThrow e1 ->
				loop e1 || true
			| TReturn (Some e1) ->
				loop e1 || true (* recurse first, could be `return continue` *)
			| TReturn None ->
				true
			| TFunction _ ->
				false (* This isn't executed, so don't recurse *)
			| TIf (cond, if_body, Some else_body) ->
				loop cond || loop if_body && loop else_body
			| TIf (cond, _, None) ->
				loop cond
			| TSwitch switch ->
				let check_exhaustive () =
					(is_exhaustive switch) && List.for_all (fun case ->
						List.exists loop case.case_patterns ||
						loop case.case_expr
					) switch.switch_cases &&
					Option.map_default (loop ) true switch.switch_default (* true because we know it's exhaustive *)
				in
				loop switch.switch_subject || check_exhaustive ()
			| TFor(_, e1, _) ->
				loop e1
			| TBinop(OpBoolAnd, e1, e2) ->
				loop e1 || is_true_expr e1 && loop e2
			| TBinop(OpBoolOr, e1, e2) ->
				loop e1 || is_false_expr e1 && loop e2
			| TWhile(cond, body, flag) ->
				loop cond || ((flag = DoWhile || is_true_expr cond) && in_loop body)
			| TTry(e1,[]) ->
				loop e1
			| TTry(_,catches) ->
				(* The try expression is irrelevant because we have to conservatively assume that
				   anything could throw control flow into the catch expressions. *)
				List.for_all (fun (_,e) -> loop e) catches
			| _ ->
				check_expr loop e
		in
		try
			loop e
		with BreakOrContinue ->
			true
end
