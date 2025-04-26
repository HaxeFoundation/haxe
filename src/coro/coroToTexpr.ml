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
	mk (TCall (efun, args)) (basic.tcoro.suspension_result basic.tany) call.cs_pos

let handle_locals ctx b cls states tf_args forbidden_vars econtinuation =
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
				Some (v.v_id, mk_field v.v_name v.v_type v.v_pos v.v_pos)
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

				let field = mk_field name v.v_type v.v_pos v.v_pos in

				Hashtbl.replace fields v.v_id field;

				begin match eo with
					| None ->
						(* We need an expression, so let's just emit `null`. The analyzer will clean this up. *)
						b#null t_dynamic e.epos
					| Some e ->
						let efield = b#instance_field econtinuation cls [] field field.cf_type in
						let einit  =
							match eo with
							| None -> Builder.default_value v.v_type v.v_pos
							| Some e -> Type.map_expr loop e in
						b#assign efield einit
				end
			(* A local of a var should never appear before its declaration, right? *)
			| TLocal (v) when is_used_across_states v.v_id ->
				let field = Hashtbl.find fields v.v_id in

				b#instance_field econtinuation cls [] field field.cf_type
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
			let efield  = b#instance_field econtinuation cls [] field field.cf_type in
			let assign  = b#assign efield (b#local v v.v_pos) in

			initial.cs_el <- assign :: initial.cs_el) tf_args;
	fields

let block_to_texpr_coroutine ctx cb cont cls params tf_args forbidden_vars exprs p stack_item_inserter start_exception =
	let {econtinuation;ecompletion;econtrol;eresult;estate;eerror;etmp} = exprs in
	let com = ctx.typer.com in
	let b = ctx.builder in

	let set_state id = b#assign estate (b#int id p) in

	let set_control (c : coro_control) = b#assign econtrol (CoroControl.mk_control com.basic c) in

	let std_is e t =
		let type_expr = mk (TTypeExpr (module_type_of_type t)) t_dynamic p in
		Texpr.Builder.resolve_and_make_static_call com.std "isOfType" [e;type_expr] p
	in

	let ereturn = b#return econtinuation in

	let mk_suspending_call call =
		let p = call.cs_pos in
		let base_continuation_field_on e cf t =
			b#instance_field e com.basic.tcoro.suspension_result_class [com.basic.tany] cf t
		in
		let ecreatecoroutine = make_suspending_call com.basic call econtinuation in

		let vcororesult = alloc_var VGenerated "_hx_tmp" (com.basic.tcoro.suspension_result com.basic.tany) p in
		let ecororesult = b#local vcororesult p in
		let cororesult_var = b#var_init vcororesult ecreatecoroutine in
		let open ContTypes in
		let esubject = base_continuation_field_on ecororesult cont.control cont.control.cf_type in
		let esuspended = b#void_block [
			set_control CoroPending;
			ereturn;
		] in
		let ereturned = b#assign etmp (base_continuation_field_on ecororesult cont.result com.basic.tany) in
		let ethrown = b#void_block [
			b#assign eresult (* TODO: wrong type? *) (base_continuation_field_on ecororesult cont.result com.basic.tany);
			b#assign etmp (base_continuation_field_on ecororesult cont.error cont.error.cf_type);
			b#break p;
		] in
		let econtrol_switch = CoroControl.make_control_switch com.basic esubject esuspended ereturned ethrown p in
		[
			stack_item_inserter call.cs_pos;
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
	let wrap_thrown,get_caught = match com.basic.texception with
		| TInst(c,_) ->
			(fun e -> Texpr.Builder.resolve_and_make_static_call c "thrown" [e] e.epos),
			(fun e -> Texpr.Builder.resolve_and_make_static_call c "caught" [e] e.epos)
		| _ ->
			die "" __LOC__
	in
	let eif_error =
		let el = if ctx.throw then
			[b#throw eerror]
		else [
			b#assign etmp eerror;
			b#break p;
		] in
		let e_then = b#void_block el in
		b#if_then
			(b#binop OpNotEq eerror (b#null eerror.etype p) com.basic.tbool)
			e_then
	in

	let exc_state_map = Array.init ctx.next_block_id (fun _ -> ref []) in
	let generate cb =
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
			add_state (Option.map (fun cb_next -> cb_next.cb_id) cb_next) ecallcoroutine;
		| NextUnknown ->
			add_state (Some (-1)) [set_control CoroReturned; ereturn]
		| NextFallThrough cb_next | NextGoto cb_next | NextBreak cb_next | NextContinue cb_next ->
			add_state (Some cb_next.cb_id) []
		| NextReturnVoid ->
			add_state (Some (-1)) [ set_control CoroReturned; ereturn ]
		| NextReturn e ->
			add_state (Some (-1)) [ set_control CoroReturned; b#assign eresult e; ereturn ]
		| NextThrow e1 ->
			if ctx.throw then
				add_state None ([stack_item_inserter e1.epos; start_exception (b#bool true p); b#throw e1])
			else
				add_state None ([stack_item_inserter e1.epos; start_exception (b#bool true p); b#assign etmp e1; b#break p ])
		| NextSub (cb_sub,cb_next) ->
			add_state (Some cb_sub.cb_id) []

		| NextIfThen (econd,cb_then,cb_next) ->
			let eif = b#if_then_else econd (set_state cb_then.cb_id) (set_state cb_next.cb_id) com.basic.tint in
			add_state None [eif]

		| NextIfThenElse (econd,cb_then,cb_else,cb_next) ->
			let eif = b#if_then_else econd (set_state cb_then.cb_id) (set_state cb_else.cb_id) com.basic.tint in
			add_state None [eif]

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

			add_state None [eswitch]

		| NextWhile (e_cond,cb_body,cb_next) ->
			add_state (Some cb_body.cb_id) []

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
						let etypecheck = std_is etmp vcatch.v_type in
						b#if_then_else etypecheck (set_state cb_catch.cb_id) enext com.basic.tvoid
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

	let fields = handle_locals ctx b cls states tf_args forbidden_vars econtinuation in

	let ethrow = b#void_block [
		b#assign etmp (b#string "Invalid coroutine state" p);
		b#break p
	] in

	let switch =
		let cases = List.map (fun state ->
			{case_patterns = [b#int state.cs_id p];
				case_expr = b#void_block state.cs_el;
			}) states in
		mk_switch estate cases (Some ethrow) true
	in
	let eswitch = mk (TSwitch switch) com.basic.tvoid p in

	let eloop = mk (TWhile (b#bool true p, eswitch, NormalWhile)) com.basic.tvoid p in

	let etry = if ctx.nothrow || (ctx.throw && not ctx.has_catch) then
		eloop
	else
		mk (TTry (
			eloop,
			[
				let vcaught = alloc_var VGenerated "e" t_dynamic p in
				let ecaught = b#local vcaught p in
				let e = b#void_block [
					start_exception (b#bool false p);
					b#assign etmp ecaught
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
		let el = if ctx.throw then [
			b#throw etmp
		] else begin
			let field         = PMap.find "buildCallStack" com.basic.tcoro.base_continuation_class.cl_fields in
			let eaccess       = b#instance_field econtinuation com.basic.tcoro.base_continuation_class params field field.cf_type in
			let ewrapped_call = mk (TCall (eaccess, [ ])) com.basic.tvoid p in
			[
				ewrapped_call;
				b#assign eerror (wrap_thrown etmp);
				set_control CoroThrown;
				ereturn;
			]
		end in
		let default = b#void_block el in
		if DynArray.empty cases then
			default
		else begin
			let switch = {
				switch_subject = estate;
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

	eloop, init_state, fields |> Hashtbl.to_seq_values |> List.of_seq
