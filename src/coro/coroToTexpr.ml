open Globals
open CoroTypes
open Type
open Texpr
open CoroControl

type coro_state = {
	cs_id : int;
	mutable cs_el : texpr list;
}

let mk_int com i = Texpr.Builder.make_int com.Common.basic i null_pos

let block_to_texpr_coroutine ctx cb cont cls tf_args forbidden_vars econtinuation ecompletion econtrol eresult estate eerror p = (* TODO: this arg list is awful *)
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

	let base_continuation_field_on e cf =
		mk (TField(e,FInstance(com.basic.tcoro.continuation_result_class, [] (* TODO: once we have them *), cf))) cf.cf_type null_pos
	in

	let ereturn = mk (TReturn (Some econtinuation)) econtinuation.etype p in

	let cb_uncaught = CoroFunctions.make_block ctx None in
	let mk_suspending_call call =
		let p = call.cs_pos in

		(* lose Coroutine<T> type for the called function not to confuse further filters and generators *)
		(* let tcoroutine = tfun [t_dynamic; t_dynamic] com.basic.tvoid in *)
		let tfun = match follow_with_coro call.cs_fun.etype with
			| Coro (args, ret) ->
				let args,ret = Common.expand_coro_type com.basic args ret in
				TFun (args, ret)
			| NotCoro _ ->
				die "Unexpected coroutine type" __LOC__
		in
		let efun = { call.cs_fun with etype = tfun } in
		let args = call.cs_args @ [ econtinuation ] in
		let ecreatecoroutine = mk (TCall (efun, args)) com.basic.tcoro.continuation_result call.cs_pos in

		let vcororesult = alloc_var VGenerated "_hx_tmp" com.basic.tcoro.continuation_result p in
		let ecororesult = make_local vcororesult p in
		let cororesult_var = mk (TVar (vcororesult, (Some ecreatecoroutine))) com.basic.tany p in

		let esubject = base_continuation_field_on ecororesult cont.ContTypes.control in
		let esuspended = mk (TBlock [
			set_control CoroPending;
			ereturn;
		]) com.basic.tvoid p in
		let ereturned = assign (base_continuation_field_on econtinuation cont.ContTypes.result) (base_continuation_field_on ecororesult cont.ContTypes.result) in
		let edoesnthappenyet = ereturn in
		let econtrol_switch = CoroControl.make_control_switch com.basic esubject esuspended ereturned edoesnthappenyet p in
		[
			cororesult_var;
			econtrol_switch;
		]
	in

	let states = ref [] in

	let init_state = ref 1 in (* TODO: this seems brittle *)

	let make_state id el = {
		cs_id = id;
		cs_el = el;
	} in

	let exc_state_map = Array.init ctx.next_block_id (fun _ -> ref []) in
	let rec loop cb current_el =
		assert (cb != ctx.cb_unreachable);
		let el = DynArray.to_list cb.cb_el in

		let add_state next_id extra_el =
			let el = current_el @ el @ extra_el in
			let el = match next_id with
				| None ->
					el
				| Some id ->
					(set_state id) :: el
			in
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
		match cb.cb_next.next_kind with
		| NextSuspend (call, cb_next) ->
			let next_state_id = loop cb_next [] in
			let ecallcoroutine = mk_suspending_call call in
			add_state (Some next_state_id) ecallcoroutine;
		| NextUnknown ->
			add_state (Some (-1)) [set_control CoroReturned; ereturn]
		| NextFallThrough cb_next | NextGoto cb_next | NextBreak cb_next | NextContinue cb_next ->
			let rec skip_loop cb =
				if DynArray.empty cb.cb_el then begin match cb.cb_next.next_kind with
					| NextFallThrough cb_next | NextGoto cb_next | NextBreak cb_next | NextContinue cb_next ->
						skip_loop cb_next
					| _ ->
						cb.cb_id
				end else
					cb.cb_id
			in
			if not (DynArray.empty cb.cb_el) then
				add_state (Some (skip_loop cb_next)) []
			else
				skip_loop cb
		| NextReturnVoid ->
			add_state (Some (-1)) [ set_control CoroReturned; ereturn ]
		| NextReturn e ->
			add_state (Some (-1)) [ set_control CoroReturned; assign eresult e; ereturn ]
		| NextThrow e1 ->
			let ethrow = mk (TThrow e1) t_dynamic p in
			add_state None [ethrow]
		| NextSub (cb_sub,cb_next) when cb_next == ctx.cb_unreachable ->
			(* If we're skipping our initial state we have to track this for the _hx_state init *)
			if cb.cb_id = !init_state then
				init_state := cb_sub.cb_id;
			loop cb_sub (current_el @ el)
		| NextSub (bb_sub,bb_next) ->
			let next_state_id = loop bb_next [] in
			let sub_state_id = loop bb_sub [] in
			ignore(next_state_id);
			add_state (Some sub_state_id) []

		| NextIfThen (econd,bb_then,bb_next) ->
			let next_state_id = loop bb_next [] in
			let then_state_id = loop bb_then [] in
			let eif = mk (TIf (econd, set_state then_state_id, Some (set_state next_state_id))) com.basic.tint p in
			add_state None [eif]

		| NextIfThenElse (econd,bb_then,bb_else,bb_next) ->
			let _ = loop bb_next [] in
			let then_state_id = loop bb_then [] in
			let else_state_id = loop bb_else [] in
			let eif = mk (TIf (econd, set_state then_state_id, Some (set_state else_state_id))) com.basic.tint p in
			add_state None [eif]

		| NextSwitch(switch, bb_next) ->
			let esubj = switch.cs_subject in
			let next_state_id = loop bb_next [] in
			let ecases = List.map (fun (patterns,bb) ->
				let case_state_id = loop bb [] in
				{case_patterns = patterns;case_expr = set_state case_state_id}
			) switch.cs_cases in
			let default_state_id = match switch.cs_default with
				| Some bb ->
					let default_state_id = loop bb [] in
					default_state_id
				| None ->
					next_state_id
			in
			let eswitch = mk_switch esubj ecases (Some (set_state default_state_id)) true in
			let eswitch = mk (TSwitch eswitch) com.basic.tvoid p in

			add_state None [eswitch]

		| NextWhile (e_cond, bb_body, bb_next) ->
			let body_state_id = loop bb_body [] in
			let _ = loop bb_next [] in
			add_state (Some body_state_id) []

		| NextTry (bb_try,catch,bb_next) ->
			let new_exc_state_id = catch.cc_cb.cb_id in
			let _ = loop bb_next [] in
			let try_state_id = loop bb_try [] in
			let erethrow = mk (TBlock [
				mk_assign eerror eresult;
				set_state (match catch.cc_cb.cb_catch with None -> cb_uncaught.cb_id | Some cb -> cb.cb_id);
			]) t_dynamic null_pos in
			let eif =
				List.fold_left (fun enext (vcatch,bb_catch) ->
					let ecatchvar = mk (TVar (vcatch, Some eresult)) com.basic.tvoid null_pos in
					let catch_state_id = loop bb_catch [ecatchvar] in
					match follow vcatch.v_type with
					| TDynamic _ ->
						set_state catch_state_id (* no next *)
					| t ->
						let etypecheck = std_is eresult vcatch.v_type in
						mk (TIf (etypecheck, set_state catch_state_id, Some enext)) com.basic.tvoid null_pos
				) erethrow (List.rev catch.cc_catches)
			in
			states := (make_state new_exc_state_id [eif]) :: !states;
			add_state (Some try_state_id) []
	in
	ignore(loop cb []);

	let states = !states in
	let rethrow_state_id = cb_uncaught.cb_id in
	let rethrow_state = make_state rethrow_state_id [mk (TThrow eerror) com.basic.tvoid null_pos] in
	let states = states @ [rethrow_state] |> List.sort (fun state1 state2 -> state1.cs_id - state2.cs_id) in

	let module IntSet = Set.Make(struct
		let compare a b = b - a
		type t = int
	end) in

	(* TODO: this (and the coroutine transform in general) should probably be run before captured vars handling *)
	(* very ugly, but seems to work: extract locals that are used across states *)

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
			(* TODO : Should this be handled here? *)
			(* Also need to check if this should be the continuation instead of completion *)
			| TCall ({ eexpr = TField (_, FStatic ({ cl_path = (["haxe";"coro"], "Intrinsics") }, { cf_name = "currentContinuation" })) }, []) ->
				ecompletion
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

	(* TODO:
		we can optimize while and switch in some cases:
		- if there's only one state (no suspensions) - don't wrap into while/switch, don't introduce state var
	*)

	let ethrow = mk (TBlock [
		mk (TThrow (make_string com.basic "Invalid coroutine state" p)) com.basic.tvoid p
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

	let eif_error =
		mk (TIf (
			mk (TBinop (
				OpNotEq,
				eerror,
				make_null eerror.etype p
			)) com.basic.tbool p,
			set_state cb_uncaught.cb_id,
			None
		)) com.basic.tvoid p
	in

	let etry = if not ctx.has_catch then
		eswitch (* If our coro doesn't catch anything then we shouldn't have to rethrow by hand *)
	else mk (TTry (
		eswitch,
		[
			let vcaught = alloc_var VGenerated "e" t_dynamic null_pos in
			let cases = DynArray.create () in
			Array.iteri (fun i l -> match !l with
				| [] ->
					()
				| l ->
					let patterns = List.map (mk_int com) l in
					let expr = mk (TBlock [
						set_state i;
						Builder.binop OpAssign eresult (Builder.make_local vcaught null_pos) vcaught.v_type null_pos;
					]) com.basic.tvoid null_pos in
					DynArray.add cases {case_patterns = patterns; case_expr = expr};
			) exc_state_map;
			let default = mk (TBlock [
				set_state rethrow_state_id;
				mk (TThrow(make_local vcaught null_pos)) t_dynamic null_pos;
			]) com.basic.tvoid null_pos in
			if DynArray.empty cases then
				(vcaught,default)
			else begin
				let switch = {
					switch_subject = estate;
					switch_cases = DynArray.to_list cases;
					switch_default = Some default;
					switch_exhaustive = true
				} in
				let e = mk (TSwitch switch) com.basic.tvoid null_pos in
				(vcaught,e)
			end
		]
	)) com.basic.tvoid null_pos in

	let eloop = mk (TWhile (make_bool com.basic true p, etry, NormalWhile)) com.basic.tvoid p in

	eloop, eif_error, !init_state, fields |> Hashtbl.to_seq_values |> List.of_seq
