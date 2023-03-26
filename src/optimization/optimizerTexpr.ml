open Ast
open Type
open Error
open Globals

(* tells if an expression causes side effects. This does not account for potential null accesses (fields/arrays/ops) *)
let has_side_effect e =
	let rec loop e =
		match e.eexpr with
		| TConst _ | TLocal _ | TTypeExpr _ | TFunction _ | TIdent _ -> ()
		| TCall({eexpr = TField(e1,fa)},el) when PurityState.is_pure_field_access fa -> loop e1; List.iter loop el
		| TNew(c,_,el) when (match c.cl_constructor with Some cf when PurityState.is_pure c cf -> true | _ -> false) -> List.iter loop el
		| TField(_,fa) when PurityState.is_explicitly_impure fa -> raise Exit
		| TNew _ | TCall _ | TBinop ((OpAssignOp _ | OpAssign),_,_) | TUnop ((Increment|Decrement),_,_) -> raise Exit
		| TReturn _ | TBreak | TContinue | TThrow _ | TCast (_,Some _) -> raise Exit
		| TArray _ | TEnumParameter _ | TEnumIndex _ | TCast (_,None) | TBinop _ | TUnop _ | TParenthesis _ | TMeta _ | TWhile _ | TFor _
		| TField _ | TIf _ | TTry _ | TSwitch _ | TArrayDecl _ | TBlock _ | TObjectDecl _ | TVar _ -> Type.iter loop e
	in
	try
		loop e; false
	with Exit ->
		true

let is_read_only_field_access e fa = match fa with
	| FEnum _ ->
		true
	| FDynamic _ ->
		false
	| FAnon {cf_kind = Var {v_write = AccNo}} when (match e.eexpr with TIdent _ -> true | _ -> false) -> true
	| FInstance (c,_,cf) | FStatic (c,cf) | FClosure (Some(c,_),cf) ->
		begin match cf.cf_kind with
			| Method MethDynamic -> false
			| Method _ -> true
			| Var {v_write = AccNever} when not (has_class_flag c CInterface) -> true
			| _ -> false
		end
	| FAnon cf | FClosure(None,cf) ->
		begin match cf.cf_kind with
			| Method MethDynamic -> false
			| Method _ -> true
			| _ -> false
		end

let create_affection_checker () =
	let modified_locals = Hashtbl.create 0 in
	let rec might_be_affected e =
		let rec loop e = match e.eexpr with
			| TConst _ | TFunction _ | TTypeExpr _ -> ()
			| TLocal v when has_var_flag v VCaptured -> raise Exit
			| TLocal v when Hashtbl.mem modified_locals v.v_id -> raise Exit
			| TField(e1,fa) when not (is_read_only_field_access e1 fa) -> raise Exit
			| TCall _ | TNew _ -> raise Exit
			| _ -> Type.iter loop e
		in
		try
			loop e;
			false
		with Exit ->
			true
	in
	let rec collect_modified_locals e = match e.eexpr with
		| TUnop((Increment | Decrement),_,{eexpr = TLocal v}) ->
			Hashtbl.add modified_locals v.v_id true
		| TBinop((OpAssign | OpAssignOp _),{eexpr = TLocal v},e2) ->
			collect_modified_locals e2;
			Hashtbl.add modified_locals v.v_id true
		| _ ->
			Type.iter collect_modified_locals e
	in
	might_be_affected,collect_modified_locals

let optimize_binop e op e1 e2 =
	let is_float t =
		match follow t with
		| TAbstract({ a_path = [],"Float" },_) -> true
		| _ -> false
	in
	let is_numeric t =
		match follow t with
		| TAbstract({ a_path = [],("Float"|"Int") },_) -> true
		| _ -> false
	in
	let check_float op f1 f2 =
		let f = op f1 f2 in
		let fstr = Numeric.float_repres f in
		if (match classify_float f with FP_nan | FP_infinite -> false | _ -> float_of_string fstr = f) then { e with eexpr = TConst (TFloat fstr) } else e
	in
	(match e1.eexpr, e2.eexpr with
	| TConst (TInt 0l) , _ when op = OpAdd && is_numeric e2.etype -> e2
	| TConst (TInt 1l) , _ when op = OpMult -> e2
	| TConst (TFloat v) , _ when op = OpAdd && float_of_string v = 0. && is_float e2.etype -> e2
	| TConst (TFloat v) , _ when op = OpMult && float_of_string v = 1. && is_float e2.etype -> e2
	| _ , TConst (TInt 0l) when (match op with OpAdd -> is_numeric e1.etype | OpSub | OpShr | OpShl -> true | _ -> false) -> e1 (* bits operations might cause overflow *)
	| _ , TConst (TInt 1l) when op = OpMult -> e1
	| _ , TConst (TFloat v) when (match op with OpAdd | OpSub -> float_of_string v = 0. && is_float e1.etype | _ -> false) -> e1 (* bits operations might cause overflow *)
	| _ , TConst (TFloat v) when op = OpMult && float_of_string v = 1. && is_float e1.etype -> e1
	| TConst TNull, TConst TNull ->
		(match op with
		| OpEq -> { e with eexpr = TConst (TBool true) }
		| OpNotEq -> { e with eexpr = TConst (TBool false) }
		| _ -> e)
	| TFunction _, TConst TNull ->
		(match op with
		| OpEq -> { e with eexpr = TConst (TBool false) }
		| OpNotEq -> { e with eexpr = TConst (TBool true) }
		| _ -> e)
	| TConst TNull, TFunction _ ->
		(match op with
		| OpEq -> { e with eexpr = TConst (TBool false) }
		| OpNotEq -> { e with eexpr = TConst (TBool true) }
		| _ -> e)
	| TConst (TInt a), TConst (TInt b) ->
		let opt f = try { e with eexpr = TConst (TInt (f a b)) } with Exit -> e in
		let check_overflow f =
			opt (fun a b ->
				let v = f (Int64.of_int32 a) (Int64.of_int32 b) in
				let iv = Int64.to_int32 v in
				if Int64.compare (Int64.of_int32 iv) v <> 0 then raise Exit;
				iv
			)
		in
		let ebool t =
			{ e with eexpr = TConst (TBool (t (Int32.compare a b) 0)) }
		in
		(match op with
		| OpAdd -> check_overflow Int64.add
		| OpSub -> check_overflow Int64.sub
		| OpMult -> check_overflow Int64.mul
		| OpDiv -> check_float ( /. ) (Int32.to_float a) (Int32.to_float b)
		| OpAnd -> opt Int32.logand
		| OpOr -> opt Int32.logor
		| OpXor -> opt Int32.logxor
		| OpShl -> opt (fun a b -> Int32.shift_left a (Int32.to_int (Int32.logand b i32_31)))
		| OpShr -> opt (fun a b -> Int32.shift_right a (Int32.to_int (Int32.logand b i32_31)))
		| OpUShr -> opt (fun a b -> Int32.shift_right_logical a (Int32.to_int (Int32.logand b i32_31)))
		| OpEq -> ebool (=)
		| OpNotEq -> ebool (<>)
		| OpGt -> ebool (>)
		| OpGte -> ebool (>=)
		| OpLt -> ebool (<)
		| OpLte -> ebool (<=)
		| _ -> e)
	| TConst ((TFloat _ | TInt _) as ca), TConst ((TFloat _ | TInt _) as cb) ->
		let fa = (match ca with
			| TFloat a -> float_of_string a
			| TInt a -> Int32.to_float a
			| _ -> die "" __LOC__
		) in
		let fb = (match cb with
			| TFloat b -> float_of_string b
			| TInt b -> Int32.to_float b
			| _ -> die "" __LOC__
		) in
		let fop op = check_float op fa fb in
		let ebool t =
			{ e with eexpr = TConst (TBool (t (compare fa fb) 0)) }
		in
		(match op with
		| OpAdd -> fop (+.)
		| OpDiv -> fop (/.)
		| OpSub -> fop (-.)
		| OpMult -> fop ( *. )
		| OpEq -> ebool (=)
		| OpNotEq -> ebool (<>)
		| OpGt -> ebool (>)
		| OpGte -> ebool (>=)
		| OpLt -> ebool (<)
		| OpLte -> ebool (<=)
		| _ -> e)
	| TConst (TString ""),TConst (TString s) | TConst (TString s),TConst (TString "") when op = OpAdd ->
		{e with eexpr = TConst (TString s)}
	| TConst (TBool a), TConst (TBool b) ->
		let ebool f =
			{ e with eexpr = TConst (TBool (f a b)) }
		in
		(match op with
		| OpEq -> ebool (=)
		| OpNotEq -> ebool (<>)
		| OpBoolAnd -> ebool (&&)
		| OpBoolOr -> ebool (||)
		| _ -> e)
	| TConst a, TConst b when op = OpEq || op = OpNotEq ->
		let ebool b =
			{ e with eexpr = TConst (TBool (if op = OpEq then b else not b)) }
		in
		(match a, b with
		| TInt a, TFloat b | TFloat b, TInt a -> ebool (Int32.to_float a = float_of_string b)
		| _ -> ebool (a = b))
	| TConst (TBool a), _ ->
		(match op with
		| OpBoolAnd -> if a then e2 else { e with eexpr = TConst (TBool false) }
		| OpBoolOr -> if a then { e with eexpr = TConst (TBool true) } else e2
		| _ -> e)
	| _ , TConst (TBool a) ->
		(match op with
		| OpBoolAnd when a  -> e1
		| OpBoolOr when not a -> e1
		| _ -> e)
	| TField (_,FEnum (e1,f1)), TField (_,FEnum (e2,f2)) when e1 == e2 ->
		(match op with
		| OpEq -> { e with eexpr = TConst (TBool (f1 == f2)) }
		| OpNotEq -> { e with eexpr = TConst (TBool (f1 != f2)) }
		| _ -> e)
	| e1, TCall ({ eexpr = TField (_,FEnum _) },el) | TCall ({ eexpr = TField (_,FEnum _) },el),e1 ->
		begin match op,e1 with
		| (OpEq | OpNotEq),TConst TNull ->
			let e0 = {e with eexpr = TConst (TBool (op = OpNotEq))} in
			{e with eexpr = TBlock (el @ [e0])}
		| OpAssign,_ ->
			e
		| _ ->
			typing_error "You cannot directly compare enums with arguments. Use either `switch`, `match` or `Type.enumEq`" e.epos
		end
	| _ ->
		e)

let optimize_unop e op flag esub =
	let is_int t = match follow t with
		| TAbstract({a_path = [],"Int"},_) -> true
		| _ -> false
	in
	match op, esub.eexpr with
		| Not, (TConst (TBool f) | TParenthesis({eexpr = TConst (TBool f)})) -> { e with eexpr = TConst (TBool (not f)) }
		| Not, (TBinop(op,e1,e2) | TParenthesis({eexpr = TBinop(op,e1,e2)})) ->
			begin
				let is_int = is_int e1.etype && is_int e2.etype in
				try
					let op = match is_int, op with
						| true, OpGt -> OpLte
						| true, OpGte -> OpLt
						| true, OpLt -> OpGte
						| true, OpLte -> OpGt
						| _, OpEq -> OpNotEq
						| _, OpNotEq -> OpEq
						| _ -> raise Exit
					in
					{e with eexpr = TBinop(op,e1,e2)}
				with Exit ->
					e
			end
		| Neg, TConst (TInt i) -> { e with eexpr = TConst (TInt (Int32.neg i)) }
		| NegBits, TConst (TInt i) -> { e with eexpr = TConst (TInt (Int32.lognot i)) }
		| Neg, TConst (TFloat f) ->
			let v = 0. -. float_of_string f in
			let vstr = Numeric.float_repres v in
			if float_of_string vstr = v then
				{ e with eexpr = TConst (TFloat vstr) }
			else
				e
		| _ -> e