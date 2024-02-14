open Globals
open Type
open CoroTypes
open CoroFunctions

let terminate cb kind t p =
	if cb.cb_next.next_kind = NextUnknown then
		cb.cb_next <- {next_kind = kind; next_type = t; next_pos = p}

let e_no_value = Texpr.Builder.make_null t_dynamic null_pos

let add_expr cb e =
	if cb.cb_next.next_kind = NextUnknown && e != e_no_value then
		DynArray.add cb.cb_el e

type coro_ret =
	| RLocal of tvar
	| RTerminate of (coro_block -> texpr -> unit)
	| RValue
	| RBlock

let expr_to_coro ctx (vresult,verror) cb_root e =
	let ordered_value_marker = ref false in
	let start_ordered_value_list () =
		let old = !ordered_value_marker in
		(fun () ->
			let cur = !ordered_value_marker in
			ordered_value_marker := old;
			cur
		)
	in
	let block_from_e e =
		make_block (Some(e.etype,e.epos))
	in
	let cb_unreachable = make_block None in
	let rec loop cb ret e = match e.eexpr with
		(* simple values *)
		| TConst _ | TLocal _ | TTypeExpr _ | TIdent _ ->
			cb,e
		(* compound values *)
		| TBlock [e1] ->
			loop cb ret e1
		| TBlock _ ->
			let cb_sub = block_from_e e in
			let cb_sub_next,e1 = loop_block cb_sub ret e in
			let cb_next = make_block None in
			terminate cb (NextSub(cb_sub,cb_next)) e.etype e.epos;
			cb_next,e1
		| TArray(e1,e2) ->
			let cb,el = ordered_loop cb [e1;e2] in
			begin match el with
			| [e1;e2] ->
				cb,{e with eexpr = TArray(e1,e2)}
			| _ ->
				die "" __LOC__
			end
		| TArrayDecl el ->
			let cb,el = ordered_loop cb el in
			cb,{e with eexpr = TArrayDecl el}
		| TObjectDecl fl ->
			let cb,el = ordered_loop cb (List.map snd fl) in
			let fl = List.map2 (fun (f,_) e -> (f,e)) fl el in
			cb,{e with eexpr = TObjectDecl fl}
		| TField(e1,fa) ->
			(* TODO: this is quite annoying because factoring out field access behaves very creatively on
			   some targets. This means that (coroCall()).field doesn't work (and isn't tested). *)
			cb,e
		| TEnumParameter(e1,ef,i) ->
			let cb,e1 = loop cb RValue e1 in
			cb,{e with eexpr = TEnumParameter(e1,ef,i)}
		| TEnumIndex e1 ->
			let cb,e1 = loop cb RValue e1 in
			cb,{e with eexpr = TEnumIndex e1}
		| TNew(c,tl,el) ->
			let cb,el = ordered_loop cb el in
			cb,{e with eexpr = TNew(c,tl,el)}
		(* rewrites & forwards *)
		| TWhile(e1,e2,flag) when not (is_true_expr e1) ->
			loop cb ret (Texpr.not_while_true_to_while_true ctx.com.Common.basic e1 e2 flag e.etype e.epos)
		| TFor(v,e1,e2) ->
			loop cb ret (Texpr.for_remap ctx.com.basic v e1 e2 e.epos)
		| TCast(e1,o) ->
			let cb,e1 = loop cb ret e1 in
			cb,{e with eexpr = TCast(e1,o)}
		| TParenthesis e1 ->
			let cb,e1 = loop cb ret e1 in
			cb,{e with eexpr = TParenthesis e1}
		| TMeta(meta,e1) ->
			let cb,e1 = loop cb ret e1 in
			cb,{e with eexpr = TMeta(meta,e1)}
		| TUnop(op,flag,e1) ->
			let cb,e1 = loop cb ret (* TODO: is this right? *) e1 in
			cb,{e with eexpr = TUnop(op,flag,e1)}
		| TBinop(OpAssign,({eexpr = TLocal v} as e1),e2) ->
			let cb,e2 = loop_assign cb (RLocal v) e2 in
			cb,{e with eexpr = TBinop(OpAssign,e1,e2)}
		(* TODO: OpAssignOp and other OpAssign *)
		| TBinop(op,e1,e2) ->
			let cb,e1 = loop cb RValue e1 in
			let cb,e2 = loop cb RValue e2 in
			cb,{e with eexpr = TBinop(op,e1,e2)}
		(* variables *)
		| TVar(v,None) ->
			add_expr cb e;
			cb,e_no_value
		| TVar(v,Some e1) ->
			add_expr cb {e with eexpr = TVar(v,None)};
			let cb,e1 = loop_assign cb (RLocal v) e1 in
			cb,e_no_value
		(* calls *)
		| TCall(e1,el) ->
			let cb,el = ordered_loop cb (e1 :: el) in
			begin match el with
				| e1 :: el ->
					begin match follow_with_coro e1.etype with
					| Coro _ ->
						let cb_next = block_from_e e1 in
						let suspend = {
							cs_fun = e1;
							cs_args = el;
							cs_pos = e.epos
						} in
						terminate cb (NextSuspend(suspend,cb_next)) t_dynamic null_pos;
						let eresult = Texpr.Builder.make_local vresult e.epos in
						let eresult = mk_cast eresult e.etype e.epos in
						cb_next,eresult
					| _ ->
						cb,{e with eexpr = TCall(e1,el)}
					end
				| [] ->
					die "" __LOC__
			end
		(* terminators *)
		| TBreak ->
			terminate cb NextBreak e.etype e.epos;
			cb,e_no_value
		| TContinue ->
			terminate cb NextContinue e.etype e.epos;
			cb,e_no_value
		| TReturn None ->
			terminate cb NextReturnVoid e.etype e.epos;
			cb_unreachable,e_no_value
		| TReturn (Some e1) ->
			let f_terminate cb e1 =
				terminate cb (NextReturn e1) e.etype e.epos;
			in
			let ret = RTerminate f_terminate in
			let cb_ret,e1 = loop_assign cb ret e1 in
			terminate cb_ret (NextReturn e1) e.etype e.epos;
			cb_unreachable,e_no_value
		| TThrow e1 ->
			let f_terminate cb e1 =
				terminate cb (NextThrow e1) e.etype e.epos;
			in
			let ret = RTerminate f_terminate in
			let cb_ret,e1 = loop_assign cb ret e1 in
			terminate cb_ret (NextThrow e1) e.etype e.epos;
			cb_unreachable,e_no_value
		(* branching *)
		| TIf(e1,e2,None) ->
			let cb,e1 = loop cb RValue e1 in
			let cb_then = block_from_e e2 in
			let _ = loop_block cb_then RBlock e2 in
			let cb_next = make_block None in
			terminate cb (NextIfThen(e1,cb_then,cb_next)) e.etype e.epos;
			cb_next,e_no_value
		| TIf(e1,e2,Some e3) ->
			let cb,e1 = loop cb RValue e1 in
			let cb_then = block_from_e e2 in
			let _ = loop_block cb_then ret e2 in
			let cb_else = block_from_e e3 in
			let _ = loop_block cb_else ret e3 in
			let cb_next = make_block None in
			terminate cb (NextIfThenElse(e1,cb_then,cb_else,cb_next)) e.etype e.epos;
			cb_next,e_no_value
		| TSwitch switch ->
			let e1 = switch.switch_subject in
			let cb,e1 = loop cb RValue e1 in
			let cases = List.map (fun case ->
				let cb_case = block_from_e case.case_expr in
				let _ = loop_block cb_case ret case.case_expr in
				(case.case_patterns,cb_case)
			) switch.switch_cases in
			let def = match switch.switch_default with
				| None ->
					None
				| Some e ->
					let cb_default = block_from_e e in
					let _ = loop_block cb_default ret e in
					Some cb_default
			in
			let switch = {
				cs_subject = e1;
				cs_cases = cases;
				cs_default = def;
				cs_exhaustive = switch.switch_exhaustive
			} in
			let cb_next = make_block None in
			terminate cb (NextSwitch(switch,cb_next)) e.etype e.epos;
			cb_next,e_no_value
		| TWhile(e1,e2,flag) (* always while(true) *) ->
			let cb_body = block_from_e e2 in
			let _ = loop_block cb_body RBlock e2 in
			let cb_next = make_block None in
			terminate cb (NextWhile(e1,cb_body,cb_next)) e.etype e.epos;
			cb_next,e_no_value
		| TTry(e1,catches) ->
			let cb_try = block_from_e e1 in
			let _ = loop_block cb_try ret e1 in
			let catches = List.map (fun (v,e) ->
				let cb_catch = block_from_e e in
				let _ = loop_block cb_catch ret e in
				v,cb_catch
			) catches in
			let cb_next = make_block None in
			terminate cb (NextTry(cb_try,catches,cb_next)) e.etype e.epos;
			cb_next,e_no_value
		| TFunction tf ->
			cb,e
	and ordered_loop cb el =
		let close = start_ordered_value_list () in
		let rec aux' cb acc el = match el with
			| [] ->
				cb,List.rev acc
			| e :: el ->
				let cb,e = loop cb RValue e in
				aux' cb (e :: acc) el
		in
		let cb,el = aux' cb [] el in
		let _ = close () in
		cb,el
	and loop_assign cb ret e =
		let cb,e = loop cb ret e in
		match ret with
			| RBlock ->
				add_expr cb e;
				cb,e_no_value
			| RValue ->
				cb,e
			| RLocal v ->
				let ev = Texpr.Builder.make_local v v.v_pos in
				let eass = Texpr.Builder.binop OpAssign ev e ev.etype ev.epos in
				add_expr cb eass;
				cb,ev
			| RTerminate f ->
				f cb e;
				cb_unreachable,e_no_value
	and loop_block cb ret e =
		let el = match e.eexpr with
			| TBlock el ->
				el
			| _ ->
				[e]
		in
		let rec aux' cb el = match el with
			| [] ->
				assert false
			| [e] ->
				loop_assign cb ret e
			| e :: el ->
				let cb,e = loop cb RBlock e in
				add_expr cb e;
				aux' cb el
		in
		match el with
			| [] ->
				cb,e_no_value
			| _ ->
				aux' cb el
	in
	loop_block cb_root RBlock e