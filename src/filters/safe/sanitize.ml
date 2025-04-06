open Ast
open Type

(* ---------------------------------------------------------------------- *)
(* SANITIZE *)

(*
	makes sure that when an AST get generated to source code, it will not
	generate expressions that evaluate differently. It is then necessary to
	add parenthesises around some binary expressions when the AST does not
	correspond to the natural operand priority order for the platform
*)

(*
	this is the standard C++ operator precedence, which is also used by both JS and PHP
*)
let standard_precedence op =
	let left = true and right = false in
	match op with
	| OpIn -> 4, right
	| OpMult | OpDiv | OpMod -> 5, left
	| OpAdd | OpSub -> 6, left
	| OpShl | OpShr | OpUShr -> 7, left
	| OpLt | OpLte | OpGt | OpGte -> 8, left
	| OpEq | OpNotEq -> 9, left
	| OpAnd -> 10, left
	| OpXor -> 11, left
	| OpOr -> 12, left
	| OpInterval -> 13, right (* haxe specific *)
	| OpBoolAnd -> 14, left
	| OpBoolOr -> 15, left
	| OpArrow -> 16, left
	| OpNullCoal -> 17, right
	| OpAssignOp OpAssign -> 18, right (* mimics ?: *)
	| OpAssign | OpAssignOp _ -> 19, right

let rec need_parent e =
	match e.eexpr with
	| TConst _ | TLocal _ | TArray _ | TField _ | TEnumParameter _ | TEnumIndex _ | TParenthesis _
	| TCall _ | TNew _ | TTypeExpr _ | TObjectDecl _ | TArrayDecl _ | TIdent _ -> false
	| TCast (e,None) | TMeta(_,e) -> need_parent e
	| TCast _ | TThrow _ | TReturn _ | TTry _ | TSwitch _ | TIf _ | TWhile _ | TBinop _ | TContinue | TBreak
	| TBlock _ | TVar _ | TFunction _ | TUnop _ -> true

let sanitize_expr config e =
	let parent e =
		match e.eexpr with
		| TParenthesis _ -> e
		| _ -> mk (TParenthesis e) e.etype e.epos
	in
	let block e =
		match e.eexpr with
		| TBlock _ -> e
		| _ -> mk (TBlock [e]) e.etype e.epos
	in
	let complex e =
		(* complex expressions are the one that once generated to source consists in several expressions  *)
		match e.eexpr with
		| TVar _	(* needs to be put into blocks *)
		| TCall ({ eexpr = TIdent "__js__" },_) (* we never know *)
			-> block e
		| _ -> e
	in
	(* tells if the printed expresssion ends with an if without else *)
	let rec has_if e =
		match e.eexpr with
		| TIf (_,_,None) -> true
		| TWhile (_,e,NormalWhile) -> has_if e
		| _ -> false
	in
	match e.eexpr with
	| TConst TNull ->
		if config.PlatformConfig.pf_static && not (is_nullable e.etype) then begin
			let rec loop t = match follow t with
				| TMono _ -> () (* in these cases the null will cast to default value *)
				| TFun _ -> () (* this is a bit a particular case, maybe flash-specific actually *)
				(* TODO: this should use get_underlying_type, but we do not have access to Codegen here.  *)
				| TAbstract(a,tl) when not (Meta.has Meta.CoreType a.a_meta) -> loop (apply_params a.a_params tl a.a_this)
				| _ ->
					if config != Common.default_config then (* This is atrocious *)
						Error.raise_typing_error ("On static platforms, null can't be used as basic type " ^ s_type (print_context()) e.etype) e.epos
			in
			loop e.etype
		end;
		e
	| TBinop (op,e1,e2) ->
		let swap op1 op2 =
			let p1, left1 = standard_precedence op1 in
			let p2, _ = standard_precedence op2 in
			left1 && p1 <= p2
		in
		let rec loop ee left =
			match ee.eexpr with
			| TBinop (op2,_,_) -> if left then not (swap op2 op) else swap op op2
			| TIf _ -> if left then not (swap (OpAssignOp OpAssign) op) else swap op (OpAssignOp OpAssign)
			| TCast (e,None) | TMeta (_,e) -> loop e left
			| TConst (TInt i) when not left ->
				(match op with
					| OpAdd | OpSub -> (Int32.to_int i) < 0
					| _ -> false
				)
			| TConst (TFloat flt) when not left ->
				(match op with
					| OpAdd | OpSub -> String.get flt 0 = '-'
					| _ -> false
				)
			| _ -> false
		in
		let e1 = if loop e1 true then parent e1 else e1 in
		let e2 = if loop e2 false then parent e2 else e2 in
		{ e with eexpr = TBinop (op,e1,e2) }
	| TUnop (Not,Prefix,{ eexpr = (TUnop (Not,Prefix,e1)) | (TParenthesis { eexpr = TUnop (Not,Prefix,e1) }) })
		when ExtType.is_bool (Abstract.follow_with_abstracts_without_null e1.etype) ->
		e1
	| TUnop (op,mode,e1) ->
		let rec loop ee =
			match ee.eexpr with
			| TConst (TInt i) when op = Neg && (Int32.to_int i) < 0 -> parent e1
			| TConst (TFloat flt) when op = Neg && String.get flt 0 = '-' -> parent e1
			| TBinop _ | TIf _ | TUnop _ -> parent e1
			| TCast (e,None) | TMeta (_, e) -> loop e
			| _ -> e1
		in
		{ e with eexpr = TUnop (op,mode,loop e1)}
	| TIf (e1,e2,eelse) ->
		let e1 = parent e1 in
		let e2 = (if (eelse <> None && has_if e2) || (match e2.eexpr with TIf _ -> true | _ -> false) then block e2 else complex e2) in
		let eelse = (match eelse with None -> None | Some e -> Some (complex e)) in
		{ e with eexpr = TIf (e1,e2,eelse) }
	| TWhile (e1,e2,flag) ->
		let e1 = parent e1 in
		let e2 = complex e2 in
		{ e with eexpr = TWhile (e1,e2,flag) }
	| TFunction f ->
		let f = (match f.tf_expr.eexpr with
			| TBlock exprs ->
				if ExtType.is_void (follow f.tf_type) then
					match List.rev exprs with
					| { eexpr = TReturn None } :: rest -> { f with tf_expr = { f.tf_expr with eexpr = TBlock (List.rev rest) } }
					| _ -> f
				else
					f
			| _ -> { f with tf_expr = block f.tf_expr }
		) in
		{ e with eexpr = TFunction f }
	| TCall (e2,args) ->
		if need_parent e2 then { e with eexpr = TCall(parent e2,args) } else e
	| TEnumParameter (e2,ef,i) ->
		if need_parent e2 then { e with eexpr = TEnumParameter(parent e2,ef,i) } else e
	| TEnumIndex e2 ->
		if need_parent e2 then { e with eexpr = TEnumIndex(parent e2) } else e
	| TField (e2,f) ->
		if need_parent e2 then { e with eexpr = TField(parent e2,f) } else e
	| TArray (e1,e2) ->
		if need_parent e1 then { e with eexpr = TArray(parent e1,e2) } else e
	| TTry (e1,catches) ->
		let e1 = block e1 in
		let catches = List.map (fun (v,e) -> v, block e) catches in
		{ e with eexpr = TTry (e1,catches) }
	| TSwitch switch ->
		let e1 = parent switch.switch_subject in
		let cases = List.map (fun case -> {case with case_expr = complex case.case_expr}) switch.switch_cases in
		let def = Option.map complex switch.switch_default in
		let switch = { switch with
			switch_subject = e1;
			switch_cases = cases;
			switch_default = def;
		} in
		{ e with eexpr = TSwitch switch }
	| _ ->
		e

let reduce_expr com e =
	match e.eexpr with
	| TBlock l ->
		(match List.rev l with
		| [] -> e
		| ec :: l ->
			(* remove all no-ops : not-final constants in blocks *)
			match List.filter (fun e -> match e.eexpr with
				| TConst _
				| TBlock []
				| TObjectDecl [] ->
					false
				| _ ->
					true
			) l with
			| [] -> ec
			| l -> { e with eexpr = TBlock (List.rev (ec :: l)) })
	| TParenthesis ec ->
		{ ec with epos = e.epos }
	| TTry (e,[]) ->
		e
	| _ ->
		e

let rec sanitize config e =
	sanitize_expr config (reduce_expr config (Type.map_expr (sanitize config) e))