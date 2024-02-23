open Globals
open CoroTypes
open Type
open Texpr

type coro_state = {
	cs_id : int;
	mutable cs_el : texpr list;
}

type coro_control =
	| CoroNormal
	| CoroError
	| CoroSuspend

let mk_int com i = Texpr.Builder.make_int com.Common.basic i null_pos

let mk_control com (c : coro_control) = mk_int com (Obj.magic c)

let make_control_switch com e_subject e_normal e_error p =
	let cases = [{
		case_patterns = [mk_control com CoroNormal];
		case_expr = e_normal;
	}; {
		case_patterns = [mk_control com CoroError];
		case_expr = e_error;
	}] in
	let switch = {
		switch_subject = e_subject;
		switch_cases = cases;
		switch_default = None;
		switch_exhaustive = true;
	} in
	mk (TSwitch switch) com.basic.tvoid p

let block_to_texpr_coroutine ctx cb vcontinuation vresult vcontrol p =
	let open Texpr.Builder in
	let com = ctx.com in

	let eresult = make_local vresult vresult.v_pos in
	let econtrol = make_local vcontrol vcontrol.v_pos in

	let mk_assign estate eid =
		mk (TBinop (OpAssign,estate,eid)) eid.etype null_pos
	in

	let vstate = alloc_var VGenerated "_hx_state" com.basic.tint p in
	let estate = make_local vstate p in
	let set_state id = mk_assign estate (mk_int com id) in

	let tstatemachine = tfun [t_dynamic; t_dynamic] com.basic.tvoid in
	let vstatemachine = alloc_var VGenerated "_hx_stateMachine" tstatemachine p in
	let estatemachine = make_local vstatemachine p in

	let mk_continuation_call eresult p =
		let econtinuation = make_local vcontinuation p in
		mk (TCall (econtinuation, [eresult; mk_control com CoroNormal])) com.basic.tvoid p
	in
	let mk_continuation_call_error eerror p =
		let econtinuation = make_local vcontinuation p in
		mk (TCall (econtinuation, [eerror; mk_control com CoroError])) com.basic.tvoid p
	in

	let cb_uncaught = CoroFunctions.make_block ctx None in
	let mk_suspending_call call =
		let p = call.cs_pos in

		(* lose Coroutine<T> type for the called function not to confuse further filters and generators *)
		let tcoroutine = tfun [t_dynamic; t_dynamic] com.basic.tvoid in
		let tfun = match follow_with_coro call.cs_fun.etype with
			| Coro (args, ret) ->
				let args,ret = Common.expand_coro_type ctx.com.basic args ret in
				TFun (args, tcoroutine)
			| NotCoro _ ->
				die "Unexpected coroutine type" __LOC__
		in
		let efun = { call.cs_fun with etype = tfun } in
		let args = call.cs_args @ [ estatemachine ] in
		let ecreatecoroutine = mk (TCall (efun, args)) tcoroutine call.cs_pos in
		let enull = make_null t_dynamic p in
		mk (TCall (ecreatecoroutine, [enull; mk_control com CoroNormal])) com.basic.tvoid call.cs_pos
	in

	let std_is e t =
		let type_expr = mk (TTypeExpr (module_type_of_type t)) t_dynamic null_pos in
		Texpr.Builder.resolve_and_make_static_call ctx.com.std "isOfType" [e;type_expr] p
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

		let ereturn = mk (TReturn None) com.basic.tvoid p in

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
			add_state (Some next_state_id) [ecallcoroutine; ereturn];
		| NextUnknown ->
			let ecallcontinuation = mk_continuation_call (make_null t_dynamic p) p in
			add_state (Some (-1)) [ecallcontinuation; ereturn]
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
		| NextReturnVoid | NextReturn _ as r ->
			let eresult = match r with
				| NextReturn e -> e
				| _ -> make_null t_dynamic p
			in
			let ecallcontinuation = mk_continuation_call eresult p in
			add_state (Some (-1)) [ecallcontinuation; ereturn]
		| NextThrow e1 ->
			let ethrow = mk (TThrow e1) t_dynamic p in
			add_state None [ethrow]
		| NextSub (cb_sub,cb_next) when cb_next == ctx.cb_unreachable ->
			(* If we're skipping our initial state we have to track this for the _hx_state init *)
			if cb.cb_id = !init_state then
				init_state := cb_sub.cb_id;
			loop cb_sub current_el
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

	(* TODO: this (and the coroutine transform in general) should probably be run before captured vars handling *)
	(* very ugly, but seems to work: extract locals that are used across states *)
	let var_usages = Hashtbl.create 5 in
	begin
		let use v state_id =
			let m = try
				Hashtbl.find var_usages v.v_id
			with Not_found ->
				let m = Hashtbl.create 1 in
				Hashtbl.add var_usages v.v_id m;
				m
			in
			Hashtbl.replace m state_id true
		in
		List.iter (fun state ->
			let rec loop e =
				match e.eexpr with
				| TVar (v, eo) ->
					Option.may loop eo;
					use v state.cs_id;
				| TLocal v ->
					use v state.cs_id;
				| _ ->
					Type.iter loop e
			in
			List.iter loop state.cs_el
		) states;
	end;
	let decls = begin
		let is_used_across_states v_id =
			let m = Hashtbl.find var_usages v_id in
			(Hashtbl.length m) > 1
		in
		let rec loop cases decls =
			match cases with
			| state :: rest ->
				let decls = ref decls in
				begin
					let rec loop e =
						match e.eexpr with
						| TVar (v, eo) when is_used_across_states v.v_id ->
							decls := v :: !decls;
							let elocal = make_local v e.epos in
							(match eo with
							| None -> elocal
							| Some einit -> mk (TBinop (OpAssign,elocal,einit)) v.v_type e.epos)
						| _ ->
							Type.map_expr loop e
					in
					state.cs_el <- List.map loop state.cs_el
				end;
				loop rest !decls
			| [] ->
				decls
		in
		loop states []
	end in

	(* TODO:
		we can optimize while and switch in some cases:
		- if there's only one state (no suspensions) - don't wrap into while/switch, don't introduce state var
	*)

	let rethrow_state_id = cb_uncaught.cb_id in
	let rethrow_state = make_state rethrow_state_id [mk (TThrow eresult) com.basic.tvoid null_pos] in
	let states = states @ [rethrow_state] in
	let states = List.sort (fun state1 state2 -> state1.cs_id - state2.cs_id) states in

	let ethrow = mk (TBlock [
		set_state rethrow_state_id;
		mk (TThrow (make_string com.basic "Invalid coroutine state" p)) com.basic.tvoid p
	]) com.basic.tvoid null_pos
	in

	let switch =
		let cases = List.map (fun state ->
			{case_patterns = [mk_int com state.cs_id];
			case_expr = mk (TBlock state.cs_el) ctx.com.basic.tvoid (punion_el null_pos state.cs_el);
		}) states in
		mk_switch estate cases (Some ethrow) true
	in
	let eswitch = mk (TSwitch switch) com.basic.tvoid p in

	let econtrolswitch =
		let e_normal = mk (TBlock []) ctx.com.basic.tvoid p in
		let e_error = set_state cb_uncaught.cb_id in
		make_control_switch com econtrol e_normal e_error p
	in

	let etry = mk (TTry (
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
					]) ctx.com.basic.tvoid null_pos in
					DynArray.add cases {case_patterns = patterns; case_expr = expr};
			) exc_state_map;
			let default = mk (TBlock [
				set_state rethrow_state_id;
				mk_continuation_call_error (make_local vcaught null_pos) null_pos;
				mk (TReturn None) t_dynamic null_pos;
			]) ctx.com.basic.tvoid null_pos in
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

	let eloop = mk (TWhile (make_bool com.basic true p, etry, DoWhile)) com.basic.tvoid p in

	let estatemachine_def = mk (TFunction {
		tf_args = [(vresult,None); (vcontrol,None)];
		tf_type = com.basic.tvoid;
		tf_expr = mk (TBlock [econtrolswitch;eloop]) com.basic.tvoid null_pos
	}) tstatemachine p in

	let state_var = mk (TVar (vstate, Some (make_int com.basic !init_state p))) com.basic.tvoid p in
	let shared_vars = List.map (fun v -> mk (TVar (v,Some (Texpr.Builder.default_value v.v_type v.v_pos))) com.basic.tvoid null_pos) decls in
	let shared_vars = List.rev (state_var :: shared_vars) in
	let shared_vars = match ctx.vthis with
		| None ->
			shared_vars
		| Some v ->
			let e_this = mk (TConst TThis) v.v_type v.v_pos in
			let e_var = mk (TVar(v,Some e_this)) com.basic.tvoid null_pos in
			e_var :: shared_vars
	in

	mk (TBlock (shared_vars @ [
		mk (TVar (vstatemachine, None)) com.basic.tvoid p;
		binop OpAssign estatemachine estatemachine_def estatemachine.etype p;
		mk (TReturn (Some estatemachine)) com.basic.tvoid p;
	])) com.basic.tvoid p
