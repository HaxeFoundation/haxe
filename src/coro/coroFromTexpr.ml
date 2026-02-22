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
	| RMapExpr of coro_ret * (texpr -> texpr)
	| RTailBlock (* tail call in block/void position *)
	| RTailReturn (* tail call in return position *)

type map_suspension_result =
	| HasSuspension
	| HasNoSuspension of texpr

let check_captures ctx args expr =
	let vars = Hashtbl.create 16 in
	let declare v = Hashtbl.add vars v.v_id () in
	List.iter (fun (v,_) -> declare v) args;
	let rec browse e = match e.eexpr with
		| TConst (TThis | TSuper) ->
			ctx.captures_this <- true
		| TLocal v ->
			if not (Hashtbl.mem vars v.v_id) then
				ctx.has_capture_vars <- true
		| TVar(v, eo) ->
			declare v;
			Option.may browse eo
		| TTry(e1, catches) ->
			browse e1;
			List.iter (fun (v, e) -> declare v; browse e) catches
		| TFunction tf ->
			List.iter (fun (v,_) -> declare v) tf.tf_args;
			browse tf.tf_expr
		| _ ->
			Type.iter browse e
	in
	browse expr

let expr_to_coro ctx etmp_result etmp_error_unwrapped cb_root scope make_inline_return make_inline_tail_call e =

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
	let scope_allows_suspension_call_on e1 args el =
		match scope with
		| None ->
			true
		| Some scope when not scope.restricted_suspension ->
			true
		| Some scope ->
			let rec is_scope_local_expr e = match (Texpr.skip e).eexpr with
				| TLocal v when v == scope.scope_var ->
					true
				| TField(e1,_) ->
					is_scope_local_expr e1
				| _ ->
					false
			in
			let has_scope_local_first_argument () =
			(* Allow calls where the scope var is the first argument because that's what happens when
			   using `scope.staticExtension()`. *)
				begin match args,el with
				| ((_,_,t) :: _),arg1 :: _ when is_scope_local_expr arg1 ->
					let is_restricted meta =
						match CoroScopeConfig.of_meta_list meta with
						| Some config -> config.CoroScopeConfig.restricted_suspension
						| None -> false
					in
					begin match e1.eexpr with
					| TField(_,FStatic({cl_kind = KAbstractImpl a}, cf)) when has_class_field_flag cf CfImpl ->
						is_restricted a.a_meta
					| _ ->
						begin try
							let mt = t_infos (module_type_of_type t) in
							is_restricted mt.mt_meta
						with Exit ->
							false
						end
					end
				| _ ->
					false
				end
			in
			is_scope_local_expr e1 || has_scope_local_first_argument ()
	in
	let scope_allows_access_to v = match scope with
		| Some scope when scope.scope_var == v ->
			true
		| None ->
			true (* I think? *)
		| _ ->
			false
	in
	let map_suspension cb ret e =
		let allow_tco = (match ret with RTailBlock | RTailReturn -> true | _ -> false) && cb.cb_catch = None in
		let exception Found in
		let rec remap can_tco loop_depth e = match e.eexpr with
			| TCall(e1,el) when (match follow_with_coro e1.etype with Coro _ -> true | _ -> false) ->
				if can_tco then
					make_inline_tail_call {
						cs_fun = e1;
						cs_args = el;
						cs_pos = e.epos;
						cs_result = SusBlock;
					}
				else
					raise Found
			| TReturn None ->
				make_inline_return None e.epos
			| TReturn (Some e1) ->
				(* `return suspensionCall()` — if `e1` is a coro call, delegate to
				   the TCall arm (which may inline it as a TCO tail call or raise
				   Found). We must not wrap with make_inline_return in that case,
				   because make_inline_tail_call already handles the return. *)
				begin match e1.eexpr with
				| TCall(efun, _) when (match follow_with_coro efun.etype with Coro _ -> true | _ -> false) ->
					remap can_tco loop_depth e1
				| _ ->
					let e1 = remap false loop_depth e1 in
					make_inline_return (Some e1) e.epos
				end
			| TThrow _ ->
				(* TODO: too much of a special case for now, let's bail until the rest works *)
				raise Found
			| TBreak | TContinue when loop_depth = 0 ->
				(* Breaking or continuing while we're in block mode means we need to stay in block mode *)
				raise Found
			| TBlock [] -> e
			| TBlock el ->
				(* Only the last element of a block is in tail position. *)
				let rec remap_block = function
					| [] -> []
					| [last] -> [remap can_tco loop_depth last]
					| hd :: tl -> remap false loop_depth hd :: remap_block tl
				in
				{e with eexpr = TBlock (remap_block el)}
			| TIf(e1, e2, e3_opt) ->
				let e1' = remap false loop_depth e1 in
				let e2' = remap can_tco loop_depth e2 in
				let e3_opt' = Option.map (remap can_tco loop_depth) e3_opt in
				{e with eexpr = TIf(e1', e2', e3_opt')}
			| TSwitch switch ->
				let switch_subject = remap false loop_depth switch.switch_subject in
				let switch_cases = List.map (fun case ->
					{case with case_expr = remap can_tco loop_depth case.case_expr}
				) switch.switch_cases in
				let switch_default = Option.map (remap can_tco loop_depth) switch.switch_default in
				{e with eexpr = TSwitch {switch with switch_subject; switch_cases; switch_default}}
			| TTry(e1, catches) ->
				(* The try body has a catch handler (this TTry's), so no TCO there.
				   The catch bodies don't have a catch handler from this TTry node,
				   so they inherit can_tco from the outer context. *)
				let e1 = remap false loop_depth e1 in
				let catches = List.map (fun (v, e) -> (v, remap can_tco loop_depth e)) catches in
				{e with eexpr = TTry(e1, catches)}
			| TWhile(e1,e2,flag) ->
				let e1 = remap false loop_depth e1 in
				let e2 = remap false (loop_depth + 1) e2 in
				{e with eexpr = TWhile(e1,e2,flag)}
			| TFunction _ ->
				e
			| _ ->
				Type.map_expr (remap false loop_depth) e
		in
		try HasNoSuspension (remap allow_tco 0 e)
		with Found -> HasSuspension
	in
	let loop_stack = ref [] in
	let rec loop cb ret e =
	match e.eexpr with
		(* special cases *)
		| TConst TThis | TBlock [] ->
			Some (cb,e)
		| TLocal v when (has_var_flag v VCoroScope) && not (scope_allows_access_to v) ->
			Error.raise_typing_error "Invalid usage of a coroutine scope in a different coroutine scope" e.epos
		(* simple values *)
		| TConst _ | TLocal _ | TTypeExpr _ | TIdent _ ->
			Some (cb,e)
		(* compound values *)
		| TBlock [e1] ->
			loop cb ret e1
		| TBlock el ->
			begin match map_suspension cb ret e with
			| HasNoSuspension e' ->
				Some (cb, e')
			| HasSuspension ->
				loop_block cb ret e
			end
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
			Option.map (fun (cb,el) -> cb,{e with eexpr = TNew(c,tl,el)}) cb
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
		| TBinop((OpBoolOr | OpBoolAnd) as op, e1, e2) ->
			begin match map_suspension cb ret e with
			| HasNoSuspension e' ->
				Some (cb, e')
			| HasSuspension ->
				(* At least one operand has a suspension call; desugar to if/else for correct short-circuit semantics:
				   a || b  →  if (a) true else b
				   a && b  →  if (a) b else false *)
				let t = e.etype and p = e.epos in
				let then_e, else_e = match op with
					| OpBoolOr -> mk (TConst (TBool true)) t p, e2
					| _        -> e2, mk (TConst (TBool false)) t p
				in
				split_if_then_else cb ret t p e1 then_e else_e
			end
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
			let cb_opt = ordered_loop cb (e1 :: el) in
			begin match cb_opt with
			| None ->
				None
			| Some (cb,el) ->
				begin match el with
					| e1 :: el ->
						begin match follow_with_coro e1.etype with
						| Coro (args,_) ->
							if not (scope_allows_suspension_call_on e1 args el) then
								Common.display_error ctx.typer.com "Invalid suspension call in restricted suspension scope" e.epos;
							(* Because of hoisting requirements, we want to temp var anything that has a side-effect
							   or could be affected by one. *)
							let might_be_affected,collect_modified_locals = OptimizerTexpr.create_affection_checker() in
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
							let make_next_block () =
								let cb_next = block_from_e e1 in
								add_block_flag cb_next CbResumeState;
								add_block_flag cb CbSuspendState;
								cb_next
							in
							let res,next = match ret with
							| RValue ->
								let v = tmp_local cb e.etype None e.epos in
								let ev = Texpr.Builder.make_local v v.v_pos in
								let cb_next = make_next_block () in
								cb_next.cb_stack_value <- Some ev;
								SusResult,Some(cb_next,ev)
							| RTailBlock when cb.cb_catch = None ->
								SusBlock,None
							| RBlock | RTailBlock ->
								SusBlock,Some ((make_next_block (),e_no_value))
							| RTailReturn when cb.cb_catch = None ->
								SusResult,None
							| RTerminate _ | RMapExpr _ | RLocal _ | RTailReturn ->
								SusResult,Some ((make_next_block ()),etmp_result)
							in
							let suspend = {
								cs_fun = e1;
								cs_args = el;
								cs_pos = e.epos;
								cs_result = res;
							} in
							terminate cb (NextSuspend(suspend,Option.map fst next)) t_dynamic null_pos;
							next
						| _ ->
							Some(cb,{e with eexpr = TCall(e1,el)})
						end
					| [] ->
						die "" __LOC__
				end
			end
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
			let cb_ret = loop_assign cb RTailReturn e1 in
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
				match map_suspension cb ret e2 with
				| HasNoSuspension e2' ->
					add_expr cb {e with eexpr = TIf(e1,e2',None)};
					cb,e_no_value
				| HasSuspension ->
					split_if_then cb e1 e.etype e.epos e2
			) cb
		| TIf(e1,e2,Some e3) ->
			begin match map_suspension cb ret e2, map_suspension cb ret e3 with
			| HasNoSuspension e2', HasNoSuspension e3' ->
				let cb = loop cb RValue e1 in
				Option.map (fun (cb,e1) -> cb,{e with eexpr = TIf(e1,e2',Some e3')}) cb
			| _ ->
				split_if_then_else cb ret e.etype e.epos e1 e2 e3
			end
		| TSwitch switch ->
			let map_switch_cases () =
				let rec aux acc cases = match cases with
					| [] ->
						let def_opt = match switch.switch_default with
							| None -> Some None
							| Some e -> match map_suspension cb ret e with
								| HasNoSuspension e' -> Some (Some e')
								| HasSuspension -> None
						in
						Option.map (fun def -> (List.rev acc, def)) def_opt
					| case :: rest ->
						match map_suspension cb ret case.case_expr with
						| HasNoSuspension e' -> aux ({case with case_expr = e'} :: acc) rest
						| HasSuspension -> None
				in
				aux [] switch.switch_cases
			in
			begin match map_switch_cases () with
			| Some (cases', def') ->
				let cb = loop cb RValue switch.switch_subject in
				Option.map (fun (cb,e1) ->
					let switch' = {switch with switch_subject = e1; switch_cases = cases'; switch_default = def'} in
					cb,{e with eexpr = TSwitch switch'}
				) cb
			| None ->
				split_switch cb ret switch e
			end
		| TWhile(e1,e2,flag) when not (is_true_expr e1) ->
			loop cb ret (Texpr.not_while_true_to_while_true ctx.typer.com.Common.basic e1 e2 flag e.etype e.epos)
		| TWhile(e1,e2,flag) (* always while(true) *) ->
			begin match map_suspension cb RBlock e2 with
			| HasNoSuspension e2' ->
				add_expr cb {e with eexpr = TWhile(e1,e2',flag)};
				Some (cb,e_no_value)
			| HasSuspension ->
				split_while cb e1 e2 e.etype e.epos
			end
		| TTry(e1,catches) ->
			let map_catches () =
				let rec aux acc catches = match catches with
					| [] -> Some (List.rev acc)
					| (v,e) :: rest ->
						match map_suspension cb RBlock e with
						| HasNoSuspension e' -> aux ((v,e') :: acc) rest
						| HasSuspension -> None
				in
				aux [] catches
			in
			begin match map_suspension cb RBlock e1, map_catches () with
			| HasNoSuspension e1', Some catches' ->
				Some (cb,{e with eexpr = TTry(e1',catches')})
			| _ ->
				split_try cb ret e1 catches e.etype e.epos
			end
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
				| RTailBlock ->
					add_expr cb e;
					Some (cb,e_no_value)
				| RTailReturn ->
					terminate cb (NextReturn e) t_dynamic null_pos;
					None
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
	and split_if_then cb e1 etype epos e2 =
		let cb_then = block_from_e e2 in
		let cb_then_next = loop_block cb_then RBlock e2 in
		let cb_next = make_block None in
		Option.may (fun (cb_then_next,_) -> fall_through cb_then_next cb_next) cb_then_next;
		terminate cb (NextIfThen(e1,cb_then,cb_next)) etype epos;
		(cb_next,e_no_value)
	and split_if_then_else cb ret etype epos e1 e2 e3 =
		let e_value,ret = check_complex cb ret etype epos in
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
				terminate cb (NextIfThenElse(e1,cb_then,cb_else,cb_next)) etype epos;
				Option.map (fun cb_next -> (cb_next,e_value)) cb_next
		end
	and split_switch cb ret switch_orig e =
		let e_value,ret = check_complex cb ret e.etype e.epos in
		let cb_s = loop cb RValue switch_orig.switch_subject in
		begin match cb_s with
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
				) switch_orig.switch_cases in
				let def = match switch_orig.switch_default with
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
					cs_exhaustive = switch_orig.switch_exhaustive
				} in
				let cb_next = if Lazy.is_val cb_next || not switch.cs_exhaustive then Some (Lazy.force cb_next) else None in
				terminate cb (NextSwitch(switch,cb_next)) e.etype e.epos;
				Option.map (fun cb_next -> (cb_next,e_value)) cb_next
		end
	and split_while cb e1 e2 etype epos =
		let cb_next = lazy (make_block None) in
		let cb_body = block_from_e e2 in
		loop_stack := (cb_body,cb_next) :: !loop_stack;
		let cb_body_next = loop_block cb_body RBlock e2 in
		Option.may (fun (cb_body_next,_) -> goto cb_body_next cb_body) cb_body_next;
		loop_stack := List.tl !loop_stack;
		let cb_next = if Lazy.is_val cb_next then Some (Lazy.force cb_next) else None in
		terminate cb (NextWhile(e1,cb_body,cb_next)) etype epos;
		Option.map (fun cb_next -> (cb_next,e_no_value)) cb_next
	and split_try cb ret e1 catches etype epos =
		let e_value,ret = check_complex cb ret etype epos in
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
		terminate cb (NextTry(cb_try,catch,cb_next)) etype epos;
		Option.map (fun cb_next -> (cb_next,e_value)) cb_next
	in
	loop_block cb_root RTailBlock e