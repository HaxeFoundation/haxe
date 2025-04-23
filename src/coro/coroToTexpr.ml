open Globals
open CoroTypes
open CoroFunctions
open Type
open Texpr
open CoroControl

type coro_state = {
	cs_id : int;
	mutable cs_el : texpr list;
}

type coro_to_texpr_exprs = {
	econtinuation : texpr;
	ecompletion :  texpr;
	econtrol : texpr;
	eresult : texpr;
	estate : texpr;
	eerror : texpr;
	etmp : texpr;
}

let mk_int com i = Texpr.Builder.make_int com.Common.basic i null_pos

let make_suspending_call basic call econtinuation =
	(* lose Coroutine<T> type for the called function not to confuse further filters and generators *)
	let tfun = match follow_with_coro call.cs_fun.etype with
		| Coro (args, ret) ->
			let args,ret = Common.expand_coro_type basic args ret in
			TFun (args, ret)
		| NotCoro _ ->
			die "Unexpected coroutine type" __LOC__
	in
	let efun = { call.cs_fun with etype = tfun } in
	let args = call.cs_args @ [ econtinuation ] in
	mk (TCall (efun, args)) (basic.tcoro.continuation_result basic.tany) call.cs_pos

let block_to_texpr_coroutine ctx cb cont cls tf_args forbidden_vars exprs p =
	let {econtinuation;ecompletion;econtrol;eresult;estate;eerror;etmp} = exprs in
	let open Texpr.Builder in
	let com = ctx.typer.com in

	let assign lhs rhs =
		mk (TBinop(OpAssign,lhs,rhs)) lhs.etype null_pos
	in

	let mk_assign estate eid =
		mk (TBinop (OpAssign,estate,eid)) eid.etype null_pos
	in

	let set_state id = mk_assign estate (mk_int com id) in

	let set_control (c : coro_control) = mk_assign econtrol (CoroControl.mk_control com.basic c) in

	let std_is e t =
		let type_expr = mk (TTypeExpr (module_type_of_type t)) t_dynamic null_pos in
		Texpr.Builder.resolve_and_make_static_call com.std "isOfType" [e;type_expr] p
	in

	let ereturn = mk (TReturn (Some econtinuation)) econtinuation.etype p in

	let mk_suspending_call call =
		let p = call.cs_pos in
		let base_continuation_field_on e cf t =
			mk (TField(e,FInstance(com.basic.tcoro.continuation_result_class, [com.basic.tany], cf))) t null_pos
		in
		let ecreatecoroutine = make_suspending_call com.basic call econtinuation in

		let vcororesult = alloc_var VGenerated "_hx_tmp" (com.basic.tcoro.continuation_result com.basic.tany) p in
		let ecororesult = make_local vcororesult p in
		let cororesult_var = mk (TVar (vcororesult, (Some ecreatecoroutine))) com.basic.tany p in
		let open ContTypes in
		let esubject = base_continuation_field_on ecororesult cont.control cont.control.cf_type in
		let esuspended = mk (TBlock [
			set_control CoroPending;
			ereturn;
		]) com.basic.tvoid p in
		let ereturned = assign etmp (base_continuation_field_on ecororesult cont.result com.basic.tany) in
		let ethrown = mk (TBlock [
			assign etmp (base_continuation_field_on ecororesult cont.error cont.error.cf_type);
			mk TBreak t_dynamic p;
		]) com.basic.tvoid p in
		let econtrol_switch = CoroControl.make_control_switch com.basic esubject esuspended ereturned ethrown p in
		[
			cororesult_var;
			econtrol_switch;
		]
	in

	let states = ref [] in

	let init_state = cb.cb_id in

	let make_state id el = {
		cs_id = id;
		cs_el = el;
	} in

	(* TODO: this sucks a bit and its usage isn't much better *)
	let wrap_thrown = match com.basic.texception with
		| TInst(c,_) ->
			(fun e -> Texpr.Builder.resolve_and_make_static_call c "thrown" [e] e.epos)
		| _ ->
			die "" __LOC__
	in
	let eif_error =
		let el = if ctx.throw then
			[mk (TThrow eerror) t_dynamic p]
		else [
			assign etmp eerror;
			mk TBreak t_dynamic p;
		] in
		let e_then = mk (TBlock el) com.basic.tvoid null_pos in
		mk (TIf (
			mk (TBinop (
				OpNotEq,
				eerror,
				make_null eerror.etype p
			)) com.basic.tbool p,
			e_then,
			None
		)) com.basic.tvoid p
	in

	let exc_state_map = Array.init ctx.next_block_id (fun _ -> ref []) in
	let generate cb =
		assert (cb != ctx.cb_unreachable);
		let el = get_block_exprs cb in

		let add_state next_id extra_el =
			let el = el in
			let el = match next_id with
				| None ->
					el
				| Some id ->
					el @ [set_state id]
			in
			let el = if has_block_flag cb CbResumeState then
				eif_error :: el
			else
				el
			in
			let el = el @ extra_el in
			states := (make_state cb.cb_id el) :: !states;
			begin match cb.cb_catch with
				| None ->
					()
				| Some cb' ->
					let r = exc_state_map.(cb'.cb_id) in
					r := cb.cb_id :: !r
			end;
			cb.cb_id
		in
		match cb.cb_next with
		| NextSuspend (call, cb_next) ->
			let ecallcoroutine = mk_suspending_call call in
			add_state (Some cb_next.cb_id) ecallcoroutine;
		| NextUnknown ->
			add_state (Some (-1)) [set_control CoroReturned; ereturn]
		| NextFallThrough cb_next | NextGoto cb_next | NextBreak cb_next | NextContinue cb_next ->
			add_state (Some cb_next.cb_id) []
		| NextReturnVoid ->
			add_state (Some (-1)) [ set_control CoroReturned; ereturn ]
		| NextReturn e ->
			add_state (Some (-1)) [ set_control CoroReturned; assign eresult e; ereturn ]
		| NextThrow e1 ->
			if ctx.throw then
				add_state None [mk (TThrow e1) t_dynamic p]
			else
				add_state None [ assign etmp e1; mk TBreak t_dynamic p ]
		| NextSub (cb_sub,cb_next) ->
			ignore(cb_next.cb_id);
			add_state (Some cb_sub.cb_id) []

		| NextIfThen (econd,cb_then,cb_next) ->
			let eif = mk (TIf (econd, set_state cb_then.cb_id, Some (set_state cb_next.cb_id))) com.basic.tint p in
			add_state None [eif]

		| NextIfThenElse (econd,cb_then,cb_else,cb_next) ->
			let eif = mk (TIf (econd, set_state cb_then.cb_id, Some (set_state cb_else.cb_id))) com.basic.tint p in
			add_state None [eif]

		| NextSwitch(switch,cb_next) ->
			let esubj = switch.cs_subject in
			let ecases = List.map (fun (patterns,cb) ->
				{case_patterns = patterns;case_expr = set_state cb.cb_id}
			) switch.cs_cases in
			let default_state_id = match switch.cs_default with
				| Some cb ->
					cb.cb_id
				| None ->
					cb_next.cb_id
			in
			let eswitch = mk_switch esubj ecases (Some (set_state default_state_id)) true in
			let eswitch = mk (TSwitch eswitch) com.basic.tvoid p in

			add_state None [eswitch]

		| NextWhile (e_cond,cb_body,cb_next) ->
			add_state (Some cb_body.cb_id) []

		| NextTry (cb_try,catch,cb_next) ->
			let new_exc_state_id = catch.cc_cb.cb_id in
			let erethrow = match catch.cc_cb.cb_catch with
				| Some cb ->
					set_state cb.cb_id
				| None ->
					mk (TBlock [
					mk TBreak t_dynamic p
				]) t_dynamic null_pos
			in
			let eif =
				List.fold_left (fun enext (vcatch,cb_catch) ->
					match follow vcatch.v_type with
					| TDynamic _ ->
						set_state cb_catch.cb_id (* no next *)
					| t ->
						let etypecheck = std_is etmp vcatch.v_type in
						mk (TIf (etypecheck, set_state cb_catch.cb_id, Some enext)) com.basic.tvoid null_pos
				) erethrow (List.rev catch.cc_catches)
			in
			states := (make_state new_exc_state_id [eif]) :: !states;
			add_state (Some cb_try.cb_id) []
	in
	let rec loop cb =
		if not (has_block_flag cb CbGenerated) then begin
			add_block_flag cb CbGenerated;
			ignore(generate cb);
			coro_iter loop cb;
		end
	in
	loop cb;

	let states = !states in
	let states = states |> List.sort (fun state1 state2 -> state1.cs_id - state2.cs_id) in

	let module IntSet = Set.Make(struct
		let compare a b = b - a
		type t = int
	end) in

	(* function arguments are accessible from the initial state without hoisting needed, so set that now *)
	let arg_state_set = IntSet.of_list [ (List.hd states).cs_id ] in
	let var_usages    = tf_args |> List.map (fun (v, _) -> v.v_id, arg_state_set) |> List.to_seq |> Hashtbl.of_seq in

	(* First iteration, just add newly discovered local variables *)
	(* After this var_usages will contain all arguments and local vars and the states sets will be just the creation state *)
	(* We don't handle locals here so we don't poison the var_usage hashtbl with non local var data *)
	List.iter (fun state ->
		let rec loop e =
			match e.eexpr with
			| TVar (v, eo) ->
				Option.may loop eo;
				Hashtbl.replace var_usages v.v_id (IntSet.of_list [ state.cs_id ])
			| _ ->
				Type.iter loop e
		in
		List.iter loop state.cs_el
	) states;

	(* Second interation, visit all locals and update any local variable state sets *)
	List.iter (fun state ->
		let rec loop e =
			match e.eexpr with
			| TLocal (v) ->
				(match Hashtbl.find_opt var_usages v.v_id with
				| Some set ->
					Hashtbl.replace var_usages v.v_id (IntSet.add state.cs_id set)
				| None ->
					())
			| _ ->
				Type.iter loop e
		in
		List.iter loop state.cs_el
	) states;

	let is_used_across_states v_id =
		let many_states set v_id =
			IntSet.elements set |> List.length > 1 in
		(* forbidden vars are things like the _hx_continuation variable, they should not be hoisted *)
		let non_coro_var v_id =
			forbidden_vars |> List.exists (fun id -> id = v_id) |> not in

		match Hashtbl.find_opt var_usages v_id with
		| Some set when many_states set v_id && non_coro_var v_id ->
			true
		| _ ->
			false
	in

	let fields =
		tf_args
		|> List.filter_map (fun (v, _) ->
			if is_used_across_states v.v_id then
				Some (v.v_id, mk_field v.v_name v.v_type v.v_pos null_pos)
			else
				None)
		|> List.to_seq
		|> Hashtbl.of_seq in

	(* Third iteration, create fields for vars used across states and remap access to those fields *)
	List.iter (fun state ->
		let rec loop e =
			match e.eexpr with
			| TVar (v, eo) when is_used_across_states v.v_id ->
				let name = if v.v_kind = VGenerated then
					Printf.sprintf "_hx_hoisted%i" v.v_id
				else
					v.v_name in

				let field = mk_field name v.v_type v.v_pos null_pos in

				Hashtbl.replace fields v.v_id field;

				begin match eo with
					| None ->
						(* We need an expression, so let's just emit `null`. The analyzer will clean this up. *)
						Builder.make_null t_dynamic e.epos
					| Some e ->
						let efield = mk (TField(econtinuation,FInstance(cls, [], field))) field.cf_type p in
						let einit  =
							match eo with
							| None -> default_value v.v_type v.v_pos
							| Some e -> Type.map_expr loop e in
						mk_assign efield einit
				end
			(* A local of a var should never appear before its declaration, right? *)
			| TLocal (v) when is_used_across_states v.v_id ->
				let field = Hashtbl.find fields v.v_id in

				mk (TField(econtinuation,FInstance(cls, [], field))) field.cf_type p
			| _ ->
				Type.map_expr loop e
		in
		state.cs_el <- List.map loop state.cs_el
	) states;

	(* We need to do this argument copying as the last thing we do *)
	(* Doing it when the initial fields hashtbl is created will cause the third iterations TLocal to re-write them... *)
	List.iter (fun (v, _) ->
		if is_used_across_states v.v_id then
			let initial = List.hd states in
			let field   = Hashtbl.find fields v.v_id in
			let efield  = mk (TField(econtinuation,FInstance(cls, [], field))) field.cf_type p in
			let assign  = mk_assign efield (Builder.make_local v p) in

			initial.cs_el <- assign :: initial.cs_el) tf_args;

	let ethrow = mk (TBlock [
		assign etmp (make_string com.basic "Invalid coroutine state" p);
		mk TBreak t_dynamic p
	]) com.basic.tvoid null_pos
	in

	let switch =
		let cases = List.map (fun state ->
			{case_patterns = [mk_int com state.cs_id];
			case_expr = mk (TBlock state.cs_el) com.basic.tvoid (punion_el null_pos state.cs_el);
		}) states in
		mk_switch estate cases (Some ethrow) true
	in
	let eswitch = mk (TSwitch switch) com.basic.tvoid p in

	let eloop = mk (TWhile (make_bool com.basic true p, eswitch, NormalWhile)) com.basic.tvoid p in

	let etry = if ctx.nothrow || (ctx.throw && not ctx.has_catch) then
		eloop
	else
		mk (TTry (
			eloop,
			[
				let vcaught = alloc_var VGenerated "e" t_dynamic null_pos in
				(vcaught,assign etmp (make_local vcaught null_pos))
			]
		)) com.basic.tvoid null_pos
	in

	let eexchandle =
		let cases = DynArray.create () in
		Array.iteri (fun i l -> match !l with
			| [] ->
				()
			| l ->
				let patterns = List.map (mk_int com) l in
				let expr = mk (TBlock [
					set_state i;
				]) com.basic.tvoid null_pos in
				DynArray.add cases {case_patterns = patterns; case_expr = expr};
		) exc_state_map;
		let el = if ctx.throw then [
			mk (TThrow etmp) t_dynamic null_pos
		] else [
			assign eerror (wrap_thrown etmp);
			set_control CoroThrown;
			ereturn;
		] in
		let default = mk (TBlock el) com.basic.tvoid null_pos in
		if DynArray.empty cases then
			default
		else begin
			let switch = {
				switch_subject = estate;
				switch_cases = DynArray.to_list cases;
				switch_default = Some default;
				switch_exhaustive = true
			} in
			mk (TSwitch switch) com.basic.tvoid null_pos
		end
	in

	let etry = mk (TBlock [
		etry;
		eexchandle;
	]) com.basic.tvoid null_pos in

	let eloop = if ctx.has_catch then
		mk (TWhile (make_bool com.basic true p, etry, NormalWhile)) com.basic.tvoid p
	else
		(* If there is no catch we don't need to pseudo-goto back into the state loop, so we don't need a control loop. *)
		etry
	in

	eloop, init_state, fields |> Hashtbl.to_seq_values |> List.of_seq
