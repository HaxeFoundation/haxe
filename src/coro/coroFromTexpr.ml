open Globals
open Type
open CoroTypes
open CoroFunctions
open LocalUsage

let e_no_value = Texpr.Builder.make_null t_dynamic null_pos

type coro_ret =
	| RLocal of tvar
	| RTerminate of (coro_block -> texpr -> unit)
	| RValue
	| RBlock
	| RMapExpr of coro_ret * (texpr -> texpr)

let expr_to_coro ctx etmp_result etmp_error_unwrapped cb_root e =

	(* TODO : Not have this be copy and pasted from capturedVars with slight modifications *)
	let wrapper = ctx.typer.com.local_wrapper in
	let scom = SafeCom.of_com ctx.typer.com in
	let scom = { scom with platform_config = { scom.platform_config with pf_capture_policy = CPWrapRef } } in
	let e = CapturedVars.captured_vars scom wrapper true e in

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
		if cb.cb_next = NextUnknown && e != e_no_value && has_side_effect e then
			DynArray.add cb.cb_el e
	in
	let terminate cb kind t p =
		if cb.cb_next = NextUnknown then
			cb.cb_next <- kind;
	in
	let fall_through cb_from cb_to =
		terminate cb_from (NextFallThrough cb_to) t_dynamic null_pos
	in
	let goto cb_from cb_to =
		terminate cb_from (NextGoto cb_to) t_dynamic null_pos
	in
	let tmp_local cb t eo p =
		let v = alloc_var VGenerated "tmp" t p in
		add_expr cb (mk (TVar(v, eo)) ctx.typer.t.tvoid p);
		v
	in
	let check_complex cb ret t p = match ret with
		| RValue ->
			let v = tmp_local cb t None p in
			let ev = Texpr.Builder.make_local v v.v_pos in
			ev,RLocal v
		| RLocal v ->
			let ev = Texpr.Builder.make_local v v.v_pos in
			ev,ret
		| _ ->
			e_no_value,ret
	in
	let ret_map_expr ret f =
		let ret = RMapExpr(ret,f) in
		(ret,(fun e -> if e == e_no_value then e else f e))
	in
	let loop_stack = ref [] in
	let rec loop cb ret e = match e.eexpr with
		(* special cases *)
		| TConst TThis | TBlock [] ->
			Some (cb,e)
		(* simple values *)
		| TConst _ | TLocal _ | TTypeExpr _ | TIdent _ ->
			Some (cb,e)
		(* compound values *)
		| TBlock [e1] ->
			loop cb ret e1
		| TBlock el ->
			let cb_sub = block_from_e e in
			let ret = match ret,el with
				| RValue,_ :: _ ->
					(*
					   If we have a multi-element block in a value-place we might need a temp var
					   because the result expression might reference local variables declared in
					   that block (https://github.com/Aidan63/haxe/issues/79).
					*)
					let v = tmp_local cb e.etype None e.epos in
					RLocal v
				| _ ->
					ret
			in
			let sub_next = loop_block cb_sub ret e in
			let cb_next = match sub_next with
				| None ->
					None
				| Some (cb_sub_next,e1) ->
					let cb_next = make_block None in
					fall_through cb_sub_next cb_next;
					Some (cb_next,e1)
			in
			terminate cb (NextSub(cb_sub,Option.map fst cb_next)) e.etype e.epos;
			cb_next
		| TArray(e1,e2) ->
			let cb = ordered_loop cb [e1;e2] in
			Option.map (fun (cb,el) -> match el with
				| [e1;e2] ->
					(cb,{e with eexpr = TArray(e1,e2)})
				| _ ->
					die "" __LOC__
			) cb
		| TArrayDecl el ->
			let cb = ordered_loop cb el in
			Option.map (fun (cb,el) -> (cb,{e with eexpr = TArrayDecl el})) cb
		| TObjectDecl fl ->
			let cb = ordered_loop cb (List.map snd fl) in
			Option.map (fun (cb,el) ->
				let fl = List.map2 (fun (f,_) e -> (f,e)) fl el in
				(cb,{e with eexpr = TObjectDecl fl})
			) cb
		| TField(e1,fa) ->
			let cb = loop cb RValue e1 in
			Option.map (fun (cb,e1) -> (cb,{e with eexpr = TField(e1,fa)})) cb
		| TEnumParameter(e1,ef,i) ->
			let cb = loop cb RValue e1 in
			Option.map (fun (cb,e1) -> (cb,{e with eexpr = TEnumParameter(e1,ef,i)})) cb
		| TEnumIndex e1 ->
			let cb = loop cb RValue e1 in
			Option.map (fun (cb,e1) -> (cb,{e with eexpr = TEnumIndex e1})) cb
		| TNew(c,tl,el) ->
			let cb = ordered_loop cb el in
			Option.map (fun (cb,e1) -> cb,{e with eexpr = TNew(c,tl,el)}) cb
		(* rewrites & forwards *)
		| TCast(e1,o) ->
			let (ret,map) = ret_map_expr ret (fun e1 -> {e with eexpr = TCast(e1,o)}) in
			let cb = loop cb ret e1 in
			Option.map (fun (cb,e1) -> (cb,map e1)) cb
		| TParenthesis e1 ->
			let (ret,map) = ret_map_expr ret (fun e1 -> {e with eexpr = TParenthesis e1}) in
			let cb = loop cb ret e1 in
			Option.map (fun (cb,e1) -> (cb,map e1)) cb
		| TMeta(meta,e1) ->
			let cb = loop cb ret e1 in
			Option.map (fun (cb,e1) -> (cb,{e with eexpr = TMeta(meta,e1)})) cb
		| TUnop(op,flag,e1) ->
			let cb = loop cb ret (* TODO: is this right? *) e1 in
			Option.map (fun (cb,e1) -> (cb,{e with eexpr = TUnop(op,flag,e1)})) cb
		| TBinop(OpAssign,({eexpr = TLocal v} as e1),e2) ->
			let cb = loop_assign cb (RLocal v) e2 in
			Option.map (fun (cb,e2) -> (cb,{e with eexpr = TBinop(OpAssign,e1,e2)})) cb
		(* TODO: OpAssignOp and other OpAssign *)
		| TBinop(op,e1,e2) ->
			let cb = loop cb RValue e1 in
			begin match cb with
			| None ->
				None
			| Some (cb,e1) ->
				let cb2 = loop cb RValue e2 in
				begin match cb2 with
				| None ->
					add_expr cb e1;
					None
				| Some (cb,e2) ->
					Some (cb,{e with eexpr = TBinop(op,e1,e2)})
				end
			end
		(* variables *)
		| TVar(v,None) ->
			add_expr cb e;
			Some (cb,e_no_value)
		| TVar(v,Some e1) ->
			add_expr cb {e with eexpr = TVar(v,None)};
			let cb = loop_assign cb (RLocal v) e1 in
			cb
		(* calls *)
		| TCall(e1,el) ->
			let cb = ordered_loop cb (e1 :: el) in
			Option.map (fun (cb,el) ->
				begin match el with
					| e1 :: el ->
						begin match follow_with_coro e1.etype with
						| Coro _ ->
							let cb_next = block_from_e e1 in
							add_block_flag cb_next CbResumeState;
							add_block_flag cb CbSuspendState;
							let eres,res = match ret with
							| RValue ->
								let v = tmp_local cb e.etype None e.epos in
								let ev = Texpr.Builder.make_local v v.v_pos in
								cb_next.cb_stack_value <- Some ev;
								ev,SusResult
							| RTerminate _ | RMapExpr _ | RLocal _ ->
								etmp_result,SusResult
							| RBlock ->
								e_no_value,SusBlock
							in
							let might_be_affected,collect_modified_locals = OptimizerTexpr.create_affection_checker() in
							(* Because of hoisting requirements, we want to temp var anything that has a side-effect
							   or could be affected by one. *)
							let el = List.map (fun e ->
								let has_side_effect = has_side_effect e in
								let might_be_affected = might_be_affected e in
								if has_side_effect then collect_modified_locals e;
								if has_side_effect || might_be_affected then begin
									let v = tmp_local cb e.etype (Some e) e.epos in
									Texpr.Builder.make_local v v.v_pos
								end else
									e
							) el in
							let suspend = {
								cs_fun = e1;
								cs_args = el;
								cs_pos = e.epos;
								cs_result = res;
							} in
							terminate cb (NextSuspend(suspend,Some cb_next)) t_dynamic null_pos;
							cb_next,eres
						| _ ->
							cb,{e with eexpr = TCall(e1,el)}
						end
					| [] ->
						die "" __LOC__
				end
			) cb
		(* terminators *)
		| TBreak ->
			begin match !loop_stack with
				| hd :: _ ->
					terminate cb (NextBreak (Lazy.force (snd hd))) e.etype e.epos;
				| [] ->
					(* Ignore, this failed during typing already. *)
					()
			end;
			None
		| TContinue ->
			begin match !loop_stack with
				| hd :: _ ->
					terminate cb (NextContinue (fst hd)) e.etype e.epos;
				| [] ->
					(* Ignore, this failed during typing already. *)
					()
			end;
			None
		| TReturn None ->
			terminate cb NextReturnVoid e.etype e.epos;
			None
		| TReturn (Some e1) ->
			let f_terminate cb e1 =
				terminate cb (NextReturn e1) e.etype e.epos;
			in
			let ret = RTerminate f_terminate in
			let cb_ret = loop_assign cb ret e1 in
			Option.may (fun (cb_ret,e1) -> terminate cb_ret (NextReturn e1) e.etype e.epos) cb_ret;
			None
		| TThrow e1 ->
			let f_terminate cb e1 =
				terminate cb (NextThrow e1) e.etype e.epos;
			in
			let ret = RTerminate f_terminate in
			let cb_ret = loop_assign cb ret e1 in
			Option.may (fun (cb_ret,e1) -> terminate cb_ret (NextThrow e1) e.etype e.epos) cb_ret;
			None
		(* branching *)
		| TIf(e1,e2,None) ->
			let cb = loop cb RValue e1 in
			Option.map (fun (cb,e1) ->
				let cb_then = block_from_e e2 in
				let cb_then_next = loop_block cb_then RBlock e2 in
				let cb_next = make_block None in
				Option.may (fun (cb_then_next,_) -> fall_through cb_then_next cb_next) cb_then_next;
				terminate cb (NextIfThen(e1,cb_then,cb_next)) e.etype e.epos;
				cb_next,e_no_value
			) cb
		| TIf(e1,e2,Some e3) ->
			let e_value,ret = check_complex cb ret e.etype e.epos in
			let cb = loop cb RValue e1 in
			begin match cb with
				| None ->
					None
				| Some(cb,e1) ->
					let cb_then = block_from_e e2 in
					let cb_then_next = loop_block cb_then ret e2 in
					let cb_else = block_from_e e3 in
					let cb_else_next = loop_block cb_else ret e3 in
					let cb_next = match cb_then_next,cb_else_next with
						| Some (cb_then_next,_),Some(cb_else_next,_) ->
							let cb_next = make_block None in
							fall_through cb_then_next cb_next;
							fall_through cb_else_next cb_next;
							Some cb_next
						| (Some (cb_branch_next,_),None) | (None,Some (cb_branch_next,_)) ->
							let cb_next = make_block None in
							fall_through cb_branch_next cb_next;
							Some cb_next
						| None,None ->
							None
					in
					terminate cb (NextIfThenElse(e1,cb_then,cb_else,cb_next)) e.etype e.epos;
					Option.map (fun cb_next -> (cb_next,e_value)) cb_next
			end
		| TSwitch switch ->
			let e_value,ret = check_complex cb ret e.etype e.epos in
			let e1 = switch.switch_subject in
			let cb = loop cb RValue e1 in
			begin match cb with
				| None ->
					None
				| Some(cb,e1) ->
					let cb_next = lazy (make_block None) in
					let cases = List.map (fun case ->
						let cb_case = block_from_e case.case_expr in
						let cb_case_next = loop_block cb_case ret case.case_expr in
						Option.may (fun (cb_case_next,_) ->
							fall_through cb_case_next (Lazy.force cb_next);
						) cb_case_next;
						(case.case_patterns,cb_case)
					) switch.switch_cases in
					let def = match switch.switch_default with
						| None ->
							None
						| Some e ->
							let cb_default = block_from_e e in
							let cb_default_next = loop_block cb_default ret e in
							Option.may (fun (cb_default_next,_) ->
								fall_through cb_default_next (Lazy.force cb_next);
							) cb_default_next;
							Some cb_default
					in
					let switch = {
						cs_subject = e1;
						cs_cases = cases;
						cs_default = def;
						cs_exhaustive = switch.switch_exhaustive
					} in
					let cb_next = if Lazy.is_val cb_next || not switch.cs_exhaustive then Some (Lazy.force cb_next) else None in
					terminate cb (NextSwitch(switch,cb_next)) e.etype e.epos;
					Option.map (fun cb_next -> (cb_next,e_value)) cb_next
			end
		| TWhile(e1,e2,flag) when not (is_true_expr e1) ->
			loop cb ret (Texpr.not_while_true_to_while_true ctx.typer.com.Common.basic e1 e2 flag e.etype e.epos)
		| TWhile(e1,e2,flag) (* always while(true) *) ->
			let cb_next = lazy (make_block None) in
			let cb_body = block_from_e e2 in
			loop_stack := (cb_body,cb_next) :: !loop_stack;
			let cb_body_next = loop_block cb_body RBlock e2 in
			Option.may (fun (cb_body_next,_) -> goto cb_body_next cb_body) cb_body_next;
			loop_stack := List.tl !loop_stack;
			let cb_next = if Lazy.is_val cb_next then Some (Lazy.force cb_next) else None in
			terminate cb (NextWhile(e1,cb_body,cb_next)) e.etype e.epos;
			Option.map (fun cb_next -> (cb_next,e_no_value)) cb_next
		| TTry(e1,catches) ->
			let e_value,ret = check_complex cb ret e.etype e.epos in
			ctx.has_catch <- true;
			let cb_next = lazy (make_block None) in
			let catches = List.map (fun (v,e) ->
				let cb_catch = block_from_e e in
				add_expr cb_catch (mk (TVar(v,Some (Lazy.force etmp_error_unwrapped))) ctx.typer.t.tvoid null_pos);
				let cb_catch_next = loop_block cb_catch ret e in
				Option.may (fun (cb_catch_next,_) ->
					fall_through cb_catch_next (Lazy.force cb_next);
				) cb_catch_next;
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
			let cb_try_next = loop_block cb_try ret e1 in
			ctx.current_catch <- old;
			Option.may (fun (cb_try_next,_) ->
				fall_through cb_try_next (Lazy.force cb_next)
			) cb_try_next;
			let cb_next = if Lazy.is_val cb_next then Some (Lazy.force cb_next) else None in
			terminate cb (NextTry(cb_try,catch,cb_next)) e.etype e.epos;
			Option.map (fun cb_next -> (cb_next,e_value)) cb_next
		| TFunction tf ->
			Some (cb,e)
	and ordered_loop cb el =
		let rec aux' cb acc el = match el with
			| [] ->
				Some (cb,List.rev acc)
			| e :: el ->
				let cb' = loop cb RValue e in
				match cb' with
				| None ->
					List.iter (fun e ->
						add_expr cb e
					) (List.rev acc);
					None
				| Some (cb,e) ->
					aux' cb (e :: acc) el
		in
		aux' cb [] el
	and loop_assign cb ret e =
		let cb = loop cb ret e in
		let rec aux ret cb = match cb with
			| Some (cb,e) when e != e_no_value ->
				begin match ret with
				| RBlock ->
					add_expr cb e;
					Some (cb,e_no_value)
				| RValue ->
					Some (cb,e)
				| RLocal v ->
					let ev = Texpr.Builder.make_local v v.v_pos in
					let eass = Texpr.Builder.binop OpAssign ev e ev.etype ev.epos in
					add_expr cb eass;
					Some (cb,ev)
				| RTerminate f ->
					f cb e;
					None
				| RMapExpr(ret,f) ->
					aux ret (Some(cb,f e))
				end
			| Some(cb,e) ->
				Some(cb,e)
			| None ->
				None
		in
		aux ret cb
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
				let cb = loop cb RBlock e in
				begin match cb with
				| None ->
					None
				| Some(cb,e) ->
					add_expr cb e;
					aux' cb el
				end
		in
		match el with
			| [] ->
				Some(cb,e_no_value)
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
			| NextSub(cb_sub,None) ->
				loop cb_sub;
				forward_el cb cb_sub;
				if has_block_flag cb CbResumeState then add_block_flag cb_sub CbResumeState;
				forward.(cb.cb_id) <- Some cb_sub
			| NextFallThrough cb_next | NextGoto cb_next when DynArray.empty cb.cb_el && not (has_block_flag cb CbResumeState) ->
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
	let is_empty_termination_block cb = match cb with
		| None ->
			true
		| Some cb ->
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