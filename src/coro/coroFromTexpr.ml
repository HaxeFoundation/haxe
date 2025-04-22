open Globals
open Type
open CoroTypes
open CoroFunctions

let e_no_value = Texpr.Builder.make_null t_dynamic null_pos

type coro_ret =
	| RLocal of tvar
	| RTerminate of (coro_block -> texpr -> unit)
	| RValue
	| RBlock

let expr_to_coro ctx etmp cb_root e =
	let ordered_value_marker = ref false in
	let start_ordered_value_list () =
		let old = !ordered_value_marker in
		(fun () ->
			let cur = !ordered_value_marker in
			ordered_value_marker := old;
			cur
		)
	in
	let make_block typepos =
		make_block ctx typepos
	in
	let block_from_e e =
		make_block (Some(e.etype,e.epos))
	in
	let has_side_effect e = match e.eexpr with
		| TVar _ ->
			(* has_side_effect doesn't consider var declarations a side effect which may just be wrong *)
			true
		| _ ->
			OptimizerTexpr.has_side_effect e
	in
	let add_expr cb e =
		if cb.cb_next = NextUnknown && e != e_no_value && cb != ctx.cb_unreachable && has_side_effect e then
			DynArray.add cb.cb_el e
	in
	let terminate cb kind t p =
		if cb.cb_next = NextUnknown && cb != ctx.cb_unreachable then
			cb.cb_next <- kind;
	in
	let fall_through cb_from cb_to =
		terminate cb_from (NextFallThrough cb_to) t_dynamic null_pos
	in
	let goto cb_from cb_to =
		terminate cb_from (NextGoto cb_to) t_dynamic null_pos
	in
	let loop_stack = ref [] in
	let rec loop cb ret e = match e.eexpr with
		(* special cases *)
		| TConst TThis ->
			cb,e
		(* simple values *)
		| TConst _ | TLocal _ | TTypeExpr _ | TIdent _ ->
			cb,e
		(* compound values *)
		| TBlock [e1] ->
			loop cb ret e1
		| TBlock _ ->
			let cb_sub = block_from_e e in
			let cb_sub_next,e1 = loop_block cb_sub ret e in
			let cb_next = if cb_sub_next == ctx.cb_unreachable then
				cb_sub_next
			else begin
				let cb_next = make_block None in
				fall_through cb_sub_next cb_next;
				cb_next
			end in
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
		| TCast(e1,o) ->
			let cb,e1 = loop cb ret e1 in
			if e1 == e_no_value then
				cb,e1
			else
				cb,{e with eexpr = TCast(e1,o)}
		| TParenthesis e1 ->
			let cb,e1 = loop cb ret e1 in
			if e1 == e_no_value then
				cb,e1
			else
				cb,{e with eexpr = TParenthesis e1}
		| TMeta(meta,e1) ->
			let cb,e1 = loop cb ret e1 in
			if e1 == e_no_value then
				cb,e1
			else
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
			cb,e1
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
						cb_next,etmp
					| _ ->
						cb,{e with eexpr = TCall(e1,el)}
					end
				| [] ->
					die "" __LOC__
			end
		(* terminators *)
		| TBreak ->
			terminate cb (NextBreak (snd (List.hd !loop_stack))) e.etype e.epos;
			cb,e_no_value
		| TContinue ->
			terminate cb (NextContinue (fst (List.hd !loop_stack))) e.etype e.epos;
			cb,e_no_value
		| TReturn None ->
			terminate cb NextReturnVoid e.etype e.epos;
			ctx.cb_unreachable,e_no_value
		| TReturn (Some e1) ->
			let f_terminate cb e1 =
				terminate cb (NextReturn e1) e.etype e.epos;
			in
			let ret = RTerminate f_terminate in
			let cb_ret,e1 = loop_assign cb ret e1 in
			terminate cb_ret (NextReturn e1) e.etype e.epos;
			ctx.cb_unreachable,e_no_value
		| TThrow e1 ->
			let f_terminate cb e1 =
				terminate cb (NextThrow e1) e.etype e.epos;
			in
			let ret = RTerminate f_terminate in
			let cb_ret,e1 = loop_assign cb ret e1 in
			terminate cb_ret (NextThrow e1) e.etype e.epos;
			ctx.cb_unreachable,e_no_value
		(* branching *)
		| TIf(e1,e2,None) ->
			let cb,e1 = loop cb RValue e1 in
			let cb_then = block_from_e e2 in
			let cb_then_next,_ = loop_block cb_then RBlock e2 in
			let cb_next = make_block None in
			fall_through cb_then_next cb_next;
			terminate cb (NextIfThen(e1,cb_then,cb_next)) e.etype e.epos;
			cb_next,e_no_value
		| TIf(e1,e2,Some e3) ->
			let cb,e1 = loop cb RValue e1 in
			let cb_then = block_from_e e2 in
			let cb_then_next,_ = loop_block cb_then ret e2 in
			let cb_else = block_from_e e3 in
			let cb_else_next,_ = loop_block cb_else ret e3 in
			let cb_next = make_block None in
			fall_through cb_then_next cb_next;
			fall_through cb_else_next cb_next;
			terminate cb (NextIfThenElse(e1,cb_then,cb_else,cb_next)) e.etype e.epos;
			cb_next,e_no_value
		| TSwitch switch ->
			let e1 = switch.switch_subject in
			let cb,e1 = loop cb RValue e1 in
			let cb_next = make_block None in
			let cases = List.map (fun case ->
				let cb_case = block_from_e case.case_expr in
				let cb_case_next,_ = loop_block cb_case ret case.case_expr in
				fall_through cb_case_next cb_next;
				(case.case_patterns,cb_case)
			) switch.switch_cases in
			let def = match switch.switch_default with
				| None ->
					None
				| Some e ->
					let cb_default = block_from_e e in
					let cb_default_next,_ = loop_block cb_default ret e in
					fall_through cb_default_next cb_next;
					Some cb_default
			in
			let switch = {
				cs_subject = e1;
				cs_cases = cases;
				cs_default = def;
				cs_exhaustive = switch.switch_exhaustive
			} in
			terminate cb (NextSwitch(switch,cb_next)) e.etype e.epos;
			cb_next,e_no_value
		| TWhile(e1,e2,flag) when not (is_true_expr e1) ->
			loop cb ret (Texpr.not_while_true_to_while_true ctx.typer.com.Common.basic e1 e2 flag e.etype e.epos)
		| TWhile(e1,e2,flag) (* always while(true) *) ->
			let cb_next = make_block None in
			let cb_body = block_from_e e2 in
			loop_stack := (cb_body,cb_next) :: !loop_stack;
			let cb_body_next,_ = loop_block cb_body RBlock e2 in
			goto cb_body_next cb_body;
			loop_stack := List.tl !loop_stack;
			terminate cb (NextWhile(e1,cb_body,cb_next)) e.etype e.epos;
			cb_next,e_no_value
		| TTry(e1,catches) ->
			ctx.has_catch <- true;
			let cb_next = make_block None in
			let catches = List.map (fun (v,e) ->
				let cb_catch = block_from_e e in
				add_expr cb_catch (mk (TVar(v,Some etmp)) ctx.typer.t.tvoid null_pos);
				let cb_catch_next,_ = loop_block cb_catch ret e in
				fall_through cb_catch_next cb_next;
				v,cb_catch
			) catches in
			let catch = make_block None in
			(* This block is handled in a special way in the texpr transformer, let's mark it as
			   already generated so we don't generate it twice. *)
			add_block_flag catch CbGenerated;
			let old = ctx.current_catch in
			ctx.current_catch <- Some catch;
			let catch = {
				cc_cb = catch;
				cc_catches = catches;
			} in
			let cb_try = block_from_e e1 in
			let cb_try_next,_ = loop_block cb_try ret e1 in
			ctx.current_catch <- old;
			fall_through cb_try_next cb_next;
			terminate cb (NextTry(cb_try,catch,cb_next)) e.etype e.epos;
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
		if e == e_no_value then
			cb,e
		else match ret with
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
				ctx.cb_unreachable,e_no_value
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

let optimize_cfg ctx cb =
	let forward_el cb_from cb_to =
		if DynArray.length cb_from.cb_el > 0 then begin
			if DynArray.length cb_to.cb_el = 0 then begin
				DynArray.iter (fun e -> DynArray.add cb_to.cb_el e) cb_from.cb_el
			end else begin
				let e = mk (TBlock (DynArray.to_list cb_from.cb_el)) ctx.typer.t.tvoid null_pos in
				DynArray.set cb_to.cb_el 0 (concat e (DynArray.get cb_to.cb_el 0))
			end
		end
	in
	(* first pass: find empty blocks and store their replacement*)
	let forward = Array.make ctx.next_block_id None in
	let rec loop cb =
		if not (has_block_flag cb CbEmptyMarked) then begin
			add_block_flag cb CbEmptyMarked;
			match cb.cb_next with
			| NextSub(cb_sub,cb_next) when cb_next == ctx.cb_unreachable ->
				loop cb_sub;
				forward_el cb cb_sub;
				forward.(cb.cb_id) <- Some cb_sub
			| NextFallThrough cb_next | NextGoto cb_next | NextBreak cb_next | NextContinue cb_next when DynArray.empty cb.cb_el ->
				loop cb_next;
				forward.(cb.cb_id) <- Some cb_next
			| _ ->
				coro_iter loop cb
		end
	in
	loop cb;
	(* second pass: map graph to skip forwarding block *)
	let rec loop cb = match forward.(cb.cb_id) with
		| Some cb ->
			loop cb
		| None ->
			if not (has_block_flag cb CbForwardMarked) then begin
				add_block_flag cb CbForwardMarked;
				coro_next_map loop cb;
			end;
			cb
	in
	let cb = loop cb in
	let is_empty_termination_block cb =
		DynArray.empty cb.cb_el && match cb.cb_next with
			| NextReturnVoid | NextUnknown ->
				true
			| _ ->
				false
	in
	let rec loop cb =
		if not (has_block_flag cb CbTcoChecked) then begin
			add_block_flag cb CbTcoChecked;
			begin match cb.cb_next with
			| NextSuspend(_,cb_next) ->
				if not (is_empty_termination_block cb_next) then
					raise Exit;
			| _ ->
				()
			end;
			coro_iter loop cb;
		end
	in
	if ctx.allow_tco && not ctx.has_catch then
		(try loop cb; raise (CoroTco cb) with Exit -> ());
	(* third pass: reindex cb_id for tighter switches. Breadth-first because that makes the numbering more natural, maybe. *)
	let i = ref 0 in
	let queue = Queue.create () in
	Queue.push cb queue;
	let rec loop () =
		if not (Queue.is_empty queue) then begin
			let cb = Queue.pop queue in
			if not (has_block_flag cb CbReindexed) then begin
				add_block_flag cb CbReindexed;
				cb.cb_id <- !i;
				incr i;
				coro_iter (fun cb -> Queue.add cb queue) cb;
				Option.may (fun cb -> Queue.add cb queue) cb.cb_catch;
			end;
			loop ()
		end
	in
	loop ();
	ctx.next_block_id <- !i;
	cb