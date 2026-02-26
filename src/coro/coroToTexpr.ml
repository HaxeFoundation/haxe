open Globals
open CoroTypes
open CoroFunctions
open Type
open ContTypes
open Texpr

module IntSet = Set.Make(struct
	let compare a b = b - a
	type t = int
end)

type coro_state = {
	cs_id : int;
	mutable cs_el_tail : texpr list option;
	mutable cs_el : texpr list;
	mutable cs_declarations : tvar list;

	cs_mapped_local : (int, tvar) Hashtbl.t;
	mutable cs_reads : IntSet.t;
	mutable cs_writes : IntSet.t;
}

type coro_to_texpr_exprs = {
	econtinuation : texpr;
	ecompletion :  texpr;
	estate : texpr;
	eresult : texpr;
	egoto : texpr;
	eerror : texpr;
	etmp_result : texpr Lazy.t;
	etmp_error : texpr;
	etmp_error_unwrapped : texpr Lazy.t;
}

let make_suspending_call basic cont call econtinuation =
	(* lose Coroutine<T> type for the called function not to confuse further filters and generators *)
	let tfun = match follow_with_coro call.cs_fun.etype with
		| Coro (args, ret) ->
			let args,ret = Common.expand_coro_type basic args ret in
			TFun (args, ret)
		| NotCoro _ ->
			die "Unexpected coroutine type" __LOC__
	in
	let efun = { call.cs_fun with etype = tfun } in
	let args = econtinuation :: call.cs_args in
	mk (TCall (efun, args)) (cont.suspension_result basic.tany) call.cs_pos

let handle_locals ctx cls params states tf_args econtinuation =
	let b = ctx.builder in
	let fst_state     = List.hd states in
	let arg_state_set = IntSet.of_list [ fst_state.cs_id ] in

	(* Keep an extra table of all vars and what states they appear in, easier check if a var is used across states this way. *)
	let var_usages = tf_args |> List.map (fun (v, _) -> v.v_id, arg_state_set) |> List.to_seq |> Hashtbl.of_seq in

	List.iter (fun state ->
		let rec loop e =
			match e.eexpr with
			| TVar (v, eo) ->
				state.cs_declarations <- v :: state.cs_declarations;

				Hashtbl.replace var_usages v.v_id (IntSet.of_list [ state.cs_id ]);

				Option.may loop eo
			| TLocal v when Hashtbl.mem var_usages v.v_id ->
				let existing = Hashtbl.find var_usages v.v_id in

				Hashtbl.replace var_usages v.v_id (IntSet.add state.cs_id existing)
			| _ ->
				Type.iter loop e
		in
		List.iter loop state.cs_el;
		Option.may (List.iter loop) state.cs_el_tail
	) states;

	(*
	 * Each variable which is used across multiple states is given a field in the continuation class to store it's value
	 * during suspension.
	 * TODO : Instead of giving each variable a field have a set of "slots" which can be used by a field if no other variable is currently using it.
	 *)
	let fields = Hashtbl.create 0 in
	let is_used_across_multiple_states id =
		match Hashtbl.find_opt var_usages id with
		| Some set ->
			(match IntSet.elements set with
			| [ _ ] ->
				false
			| _ ->
				true)
		| _ ->
			false
	in

	let force_hoisted_ids = Hashtbl.create 0 in
	List.iter (fun (v, _) ->
		begin
			let field = mk_field (Printf.sprintf "_hx_hoisted%i" v.v_id) v.v_type v.v_pos v.v_pos in

			Hashtbl.replace fields v.v_id field;
			(* Create a fresh restored var rather than reusing the original argument variable.
			   This prevents the same v_id appearing as both an outer function parameter and
			   a TVar declaration inside the state machine body (which confuses renameVars). *)
			let restored_var = alloc_var VGenerated (Printf.sprintf "_hx_restored%i" v.v_id) v.v_type v.v_pos in
			Hashtbl.replace fst_state.cs_mapped_local v.v_id restored_var;
			Hashtbl.replace force_hoisted_ids v.v_id ();
		end) tf_args;

	List.iter (fun state ->

		let get_or_create_local_mapping v =
			match Hashtbl.find_opt state.cs_mapped_local v.v_id with
			| Some v -> v
			| None ->
				let new_v = alloc_var VGenerated (Printf.sprintf "_hx_restored%i" v.v_id) v.v_type v.v_pos in
				Hashtbl.replace state.cs_mapped_local v.v_id new_v;
				new_v
		in

		let rec mapper e =
			match e.eexpr with
			| TVar (v, eo) when is_used_across_multiple_states v.v_id ->
				if Option.is_some eo then
					state.cs_writes <- IntSet.add v.v_id state.cs_writes;

				let field = mk_field (Printf.sprintf "_hx_hoisted%i" v.v_id) v.v_type v.v_pos v.v_pos in

				Hashtbl.replace fields v.v_id field;
				Hashtbl.replace state.cs_mapped_local v.v_id v;

				{ e with eexpr = TVar (v, Option.map mapper eo) }
			| TBinop ((OpAssign | OpAssignOp _) as op, elhs, erhs) ->
				(match Texpr.skip elhs with
				| { eexpr = TLocal v } when is_used_across_multiple_states v.v_id || Hashtbl.mem force_hoisted_ids v.v_id ->
					state.cs_writes <- IntSet.add v.v_id state.cs_writes;

					let new_local = { elhs with eexpr = TLocal (get_or_create_local_mapping v) } in
					let new_rhs   = mapper erhs in

					{ e with eexpr = TBinop (op, new_local, new_rhs) }
				| _ ->
					Type.map_expr mapper e)
			| TUnop ((Increment | Decrement) as mode, flag, erhs) ->
				(match Texpr.skip erhs with
				| { eexpr = TLocal v  } when is_used_across_multiple_states v.v_id || Hashtbl.mem force_hoisted_ids v.v_id ->
					state.cs_writes <- IntSet.add v.v_id state.cs_writes;

					let new_rhs = { erhs with eexpr = TLocal (get_or_create_local_mapping v) } in
					{ e with eexpr = TUnop (mode, flag, new_rhs) }
				| _ ->
					Type.map_expr mapper e)
			| TLocal v when is_used_across_multiple_states v.v_id || Hashtbl.mem force_hoisted_ids v.v_id ->
				(* Each state generates new local variables for variables which are used across states. *)
				(* Here we generate and store those new variables and remap local access to them *)

				state.cs_reads <- IntSet.add v.v_id state.cs_reads;

				{ e with eexpr = TLocal (get_or_create_local_mapping v) }
			| TCall ({eexpr = TLocal v},[]) when Hashtbl.mem ctx.deferred_exprs v.v_id ->
				mapper ((Hashtbl.find ctx.deferred_exprs v.v_id) ())
			| _ ->
				Type.map_expr mapper e
		in
		state.cs_el <- List.map mapper state.cs_el;
		state.cs_el_tail <- Option.map (List.map mapper) state.cs_el_tail
	) states;

	(* Remove hoisted fields for variables that are never saved or restored:
	   - Force-hoisted function args that are never actually read in the state machine
	     (e.g., a discarded `_` or an unused `node` parameter).
	   - Any cross-state field that ended up with no actual reads or writes.
	   Removing these avoids passing dead constructor parameters and emitting
	   prototype fields that are never accessed. *)
	let used_ids = List.fold_left (fun acc state ->
		IntSet.union acc (IntSet.union state.cs_writes state.cs_reads)
	) IntSet.empty states in
	Hashtbl.filter_map_inplace (fun id field ->
		if IntSet.mem id used_ids then Some field else None
	) fields;

	List.iter (fun state ->
		let restoring = IntSet.union state.cs_writes state.cs_reads |> IntSet.to_list |> List.filter_map (fun id ->
			(* We don't want to restore a variable which is declared in this state *)
			(* Doing so would mean if the var is an argument the arguments value would be overwritten by whatever is in the hoisted field *)
			if List.exists (fun v -> v.v_id = id) state.cs_declarations then
				None
			else
				let v       = Hashtbl.find state.cs_mapped_local id in
				let field   = Hashtbl.find fields id in
				let access  = b#instance_field econtinuation cls params field field.cf_type in
				Some (b#var_init v access)
		) in

		let saving =
			state.cs_writes |> IntSet.to_list |> List.map (fun id ->
				let v       = Hashtbl.find state.cs_mapped_local id in
				let field   = Hashtbl.find fields id in
				let access  = b#instance_field econtinuation cls params field field.cf_type in
				let local   = b#local v v.v_pos in
				b#assign access local) in

		let tail = state.cs_el_tail |> Option.default [] in
		state.cs_el <- restoring @ state.cs_el @ saving @ tail)
		states;

	fields
	|> Hashtbl.to_seq_values
	|> List.of_seq

let build_call_stack ctx cont econtinuation p =
	let b = ctx.builder in
	if not ctx.typer.com.debug then
		b#void_block_at [] p
	else begin
		let basic = ctx.typer.t in
		let build_cf = PMap.find "buildCallStack" cont.base_continuation_class.cl_fields in
		let eaccess = b#instance_field econtinuation cont.base_continuation_class [basic.tany] build_cf build_cf.cf_type in
		mk (TCall (eaccess, [])) basic.tvoid p
	end

module SuspensionCalls = struct
	(* Save a reference to the top-level make_suspending_call before any local shadowing *)
	let mk_coro_call = make_suspending_call

	let make_suspension_call_and_assign ctx cont call econtinuation =
		let com = ctx.typer.com in
		let b = ctx.builder in
		let p = call.cs_pos in
		let ecreatecoroutine = mk_coro_call com.Common.basic cont call {econtinuation with epos = p} in

		let vcororesult = alloc_var VGenerated "_hx_tmp" (cont.suspension_result com.basic.tany) p in
		let ecororesult = b#local vcororesult p in
		let cororesult_var = b#var_init vcororesult ecreatecoroutine in
		(cororesult_var,ecororesult)

	(* Returns (esubject, eres, eerror) field accesses on ecororesult *)
	let unpack_result_fields ctx cont ecororesult =
		let b = ctx.builder in
		let com = ctx.typer.com in
		let open ContTypes in
		let base cf t = b#instance_field ecororesult cont.suspension_result_class [com.basic.tany] cf t in
		let esubject  = base cont.state cont.state.cf_type in
		let eres      = base cont.result com.basic.tany in
		let eerror    = base cont.error cont.error.cf_type in
		(esubject, eres, eerror)

	let make_suspended_return b cont p =
		let esuspensionresult = Builder.make_static_this cont.suspension_result_class p in
		b#static_field esuspensionresult cont.suspension_result_class cont.suspended cont.suspended.cf_type

	let make_suspending_call ctx cont exprs call =
		let {econtinuation;etmp_result;etmp_error;_} = exprs in
		let com = ctx.typer.com in
		let b = ctx.builder in
		let p = call.cs_pos in
		let outcome = call.cs_kind in
		if outcome.CoroConfig.no_return && outcome.CoroConfig.no_throw then begin
			(* Always-suspending: call + return suspended singleton *)
			let ecall_stmt = b#void_block [mk_coro_call com.Common.basic cont call {econtinuation with epos = p}] in
			let esuspended_val = make_suspended_return b cont p in
			(ecall_stmt, b#void_block [b#return esuspended_val])
		end else begin
			(* Generic version: call + switch *)
			let (cororesult_var, ecororesult) = make_suspension_call_and_assign ctx cont call econtinuation in
			let (esubject, eres, eerror) = unpack_result_fields ctx cont ecororesult in
			let esuspended = b#void_block [b#return (make_suspended_return b cont p)] in
			let ereturned = match call.cs_result with
				| SusBlock ->
					b#void_block_at [] p
				| SusResult ->
					b#assign (Lazy.force etmp_result) eres
			in
			let ethrown = b#void_block [
				b#assign etmp_error eerror;
				b#break p;
			] in
			let estate_switch = if outcome.CoroConfig.no_throw then
				(* Callee can't throw: skip the Thrown case entirely. *)
				make_custom_control_switch b esubject [
					[CoroPending],  esuspended;
					[CoroReturned], ereturned;
				] p
			else
				make_control_switch b esubject esuspended ereturned ethrown p
			in
			cororesult_var,
			estate_switch
		end

	let make_suspending_tail_call ctx cont exprs call =
		let {econtinuation;ecompletion;_} = exprs in
		let com = ctx.typer.com in
		let b = ctx.builder in
		let ecompletion_field = b#instance_field econtinuation cont.base_continuation_class [com.basic.tany] cont.completion ecompletion.etype in
		let (cororesult_var, ecororesult) = make_suspension_call_and_assign ctx cont call ecompletion_field in
		(cororesult_var, b#return ecororesult)

	(* Generate an inline call+result check for a no_suspend callee.
	   Returns (call_stmt, check_stmt) — assembled into a void_block by the caller.
	   For single-state coroutines there is no enclosing while loop, so the Thrown
	   branch emits the full error-handler inline instead of using `break`.
	   If the callee also has no_throw, the result is set directly without any switch. *)
	let make_sync_call_and_check ctx cont exprs call e_opt =
		let {econtinuation;eerror;etmp_error;_} = exprs in
		let b = ctx.builder in
		let p = call.cs_pos in
		let outcome = call.cs_kind in
		let (cororesult_var, ecororesult) = make_suspension_call_and_assign ctx cont call econtinuation in
		let (esubject, eres, eerr_field) = unpack_result_fields ctx cont ecororesult in
		let ereturned = match e_opt with
			| None ->
				b#void_block_at [] p
			| Some e ->
				b#assign e eres
		in
		if outcome.CoroConfig.no_throw then
			(* Callee can't throw, and we know it can't suspend (no_suspend = true),
			   so the result is always Returned. No switch needed. *)
			(cororesult_var, ereturned)
		else begin
			let ethrown =
				if ctx.num_states = 1 then begin
					(* Single-state: no while loop to break out of.
					   Emit the error handler directly (equivalent to eexchandle). *)
					let ewrapped_call = build_call_stack ctx cont econtinuation p in
					b#void_block [
						b#assign etmp_error eerr_field;
						b#assign eerror etmp_error;
						ewrapped_call;
						b#assign exprs.estate (b#int (Obj.magic CoroThrown) p);
						b#return econtinuation;
					]
				end else
					b#void_block [
						b#assign etmp_error eerr_field;
						b#break p;
					]
			in
			let echeck = make_custom_control_switch b esubject [
				[CoroReturned], ereturned;
				[CoroThrown], ethrown;
			] p in
			(cororesult_var, echeck)
		end
end


let block_to_texpr_coroutine ctx cb cont cls params tf_args exprs p stack_item_inserter start_exception =
	let {econtinuation;ecompletion;estate;eresult;egoto;eerror;etmp_result;etmp_error;etmp_error_unwrapped} = exprs in
	let com = ctx.typer.com in
	let b = ctx.builder in

	(* In a single-state coroutine there is no while/switch dispatch loop, so
	   gotoLabel is never read. *)
	let single_state = ctx.num_states = 1 in

	let set_state id = b#assign egoto (b#int id p) in

	let set_control (c : coro_control) = b#assign estate (b#int (Obj.magic c) p) in

	let std_is e t =
		let type_expr = mk (TTypeExpr (module_type_of_type t)) t_dynamic p in
		Texpr.Builder.resolve_and_make_static_call com.std "isOfType" [e;type_expr] p
	in

	let ereturn = b#return econtinuation in

	let states = ref [] in

	let init_state = cb.cb_id in

	let make_state id el el_state_checl = {
		cs_id = id;
		cs_el = el;
		cs_el_tail = el_state_checl;
		cs_declarations = [];
		cs_mapped_local = Hashtbl.create 0;
		cs_reads = IntSet.empty;
		cs_writes = IntSet.empty;
	} in

	let get_caught,unwrap_exception = match com.basic.texception with
		| TInst(c,_) ->
			let unwrap =
				let cf = PMap.find "unwrap" c.cl_fields in
				(fun e ->
					let e = b#instance_field e c [] cf cf.cf_type in
					b#call e [] com.basic.tany
				)
			in
			(fun e -> Texpr.Builder.resolve_and_make_static_call c "caught" [e] e.epos),
			unwrap
		| _ ->
			die "" __LOC__
	in
	let eif_error cb =
		let el = [
			b#assign etmp_error eerror;
			b#break p;
		] in
		let e_then = b#void_block el in
		let e_if = b#binop OpNotEq eerror (b#null eerror.etype p) com.basic.tbool in
		match cb.cb_stack_value with
			| None ->
				b#if_then e_if e_then
			| Some e ->
				let e_assign = b#assign e (Lazy.force etmp_result) in
				b#if_then_else e_if e_then e_assign com.basic.tvoid
	in

	let exc_state_map = Array.init ctx.next_block_id (fun _ -> ref []) in
	let generate cb =
		let el = get_block_exprs cb in

		let add_state next_id extra_el state_check =
			let el = match (if single_state then None else next_id) with
				| None ->
					el
				| Some id ->
					el @ [set_state id]
			in
			let el = if has_block_flag cb CbResumeState then
				eif_error cb :: el
			else
				el
			in
			let el = el @ extra_el in
			states := (make_state cb.cb_id el state_check) :: !states;
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
		| NextSuspend (call, None) ->
			let ecallcoroutine, eret = SuspensionCalls.make_suspending_tail_call ctx cont exprs call in
			add_state None [ stack_item_inserter call.cs_pos ] (Some [ ecallcoroutine; eret ]);
		| NextSuspend (call, Some cb_next) ->
			let ecallcoroutine, estateswitch = SuspensionCalls.make_suspending_call ctx cont exprs call in
			add_state (Some cb_next.cb_id) [ stack_item_inserter call.cs_pos ] (Some [ ecallcoroutine; estateswitch ]);
		| NextUnknown ->
			add_state (Some (-1)) [set_control CoroReturned; ereturn] None
		| NextFallThrough cb_next | NextGoto cb_next | NextBreak cb_next | NextContinue cb_next ->
			add_state (Some cb_next.cb_id) [] None
		| NextReturnVoid ->
			add_state (Some (-1)) [ set_control CoroReturned; ereturn ] None
		| NextReturn e ->
			add_state (Some (-1)) [ set_control CoroReturned; b#assign eresult e; ereturn ] None
		| NextThrow e1 ->
			(* In multi-state mode the break exits the while/switch loop to reach the
			   outer error handler.  In single-state mode there is no loop, so no break
			   is needed — the error handler follows the body naturally. *)
			let tail = if single_state then None else Some [ b#break p ] in
			add_state None ([b#assign etmp_error (get_caught e1); stack_item_inserter e1.epos; b#assign etmp_error (start_exception etmp_error); ]) tail
		| NextIfThen (econd,cb_then,cb_next) ->
			let eif = b#if_then_else econd (set_state cb_then.cb_id) (set_state cb_next.cb_id) com.basic.tint in
			add_state None [eif] None

		| NextIfThenElse (econd,cb_then,cb_else,cb_next) ->
			let eif = b#if_then_else econd (set_state cb_then.cb_id) (set_state cb_else.cb_id) com.basic.tint in
			add_state None [eif] None

		| NextSwitch(switch,cb_next) ->
			let esubj = switch.cs_subject in
			let ecases = List.map (fun (patterns,cb) ->
				{case_patterns = patterns;case_expr = set_state cb.cb_id}
			) switch.cs_cases in
			let next_id = match switch.cs_default with
				| Some cb ->
					Some (set_state cb.cb_id)
				| None ->
					Option.map (fun cb_next -> set_state cb_next.cb_id) cb_next
			in
			let eswitch = mk_switch esubj ecases next_id true in
			let eswitch = mk (TSwitch eswitch) com.basic.tvoid p in

			add_state None [eswitch] None

		| NextWhile (e_cond,cb_body,cb_next) ->
			add_state (Some cb_body.cb_id) [] None

		| NextTry (cb_try,catch,cb_next) ->
			let new_exc_state_id = catch.cc_cb.cb_id in
			let erethrow = match catch.cc_cb.cb_catch with
				| Some cb ->
					set_state cb.cb_id
				| None ->
					b#void_block [
						b#break p
					]
			in
			let eif =
				List.fold_left (fun enext (vcatch,cb_catch) ->
					match follow vcatch.v_type with
					| TDynamic _ ->
						set_state cb_catch.cb_id (* no next *)
					| t ->
						let etypecheck = std_is (Lazy.force etmp_error_unwrapped) vcatch.v_type in
						b#if_then_else etypecheck (set_state cb_catch.cb_id) enext com.basic.tvoid
				) erethrow (List.rev catch.cc_catches)
			in
			let el = if Lazy.is_val etmp_error_unwrapped then
				[b#assign (Lazy.force etmp_error_unwrapped) (unwrap_exception etmp_error);eif]
			else
				[eif]
			in
			states := (make_state new_exc_state_id el None) :: !states;
			add_state (Some cb_try.cb_id) [] None
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

	let fields_and_decls = handle_locals ctx cls params states tf_args econtinuation in

	let eloop = match states with
		| [state] ->
			b#void_block_at state.cs_el (Texpr.punion_el p state.cs_el)
		| _ ->
			let ethrow = b#void_block [
				b#assign etmp_error (get_caught (b#string "Invalid coroutine state" p));
				b#break p
			] in
			let switch =
				let cases = List.map (fun state ->
					{case_patterns = [b#int state.cs_id p];
						case_expr = b#void_block_at state.cs_el (Texpr.punion_el p state.cs_el);
					}) states in
				mk_switch egoto cases (Some ethrow) true
			in
			let eswitch = mk (TSwitch switch) com.basic.tvoid p in
			mk (TWhile (b#bool true p, eswitch, NormalWhile)) com.basic.tvoid p
	in

	let etry = if ctx.config.outcome.no_throw then
		eloop
	else
		mk (TTry (
			eloop,
			[
				let vcaught = alloc_var VGenerated "e" t_dynamic p in
				let ecaught = b#local vcaught p in
				let ecaught = get_caught ecaught in
				let e = b#void_block [
					b#assign etmp_error (start_exception ecaught);
				] in
				(vcaught,e)
			]
		)) com.basic.tvoid p
	in

	let eexchandle =
		let cases = DynArray.create () in
		Array.iteri (fun i l -> match !l with
			| [] ->
				()
			| l ->
				let patterns = List.map (fun i -> b#int i p) l in
				let expr = b#void_block [
					set_state i;
				] in
				DynArray.add cases {case_patterns = patterns; case_expr = expr};
		) exc_state_map;
		let el =
			let ewrapped_call = build_call_stack ctx cont econtinuation p in
			[
				b#assign eerror etmp_error;
				ewrapped_call;
				set_control CoroThrown;
				ereturn;
			]
		in
		let default = b#void_block el in
		if DynArray.empty cases then
			default
		else begin
			let switch = {
				switch_subject = egoto;
				switch_cases = DynArray.to_list cases;
				switch_default = Some default;
				switch_exhaustive = true
			} in
			mk (TSwitch switch) com.basic.tvoid p
		end
	in

	let etry = b#void_block [
		etry;
		eexchandle;
	] in

	let eloop = if ctx.has_catch then
		mk (TWhile (b#bool true p, etry, NormalWhile)) com.basic.tvoid p
	else
		(* If there is no catch we don't need to pseudo-goto back into the state loop, so we don't need a control loop. *)
		etry
	in

	eloop, init_state, fields_and_decls, List.length states
