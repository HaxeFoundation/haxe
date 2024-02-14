open Globals
open CoroTypes
open Type
open Texpr

type coro_state = {
	cs_id : int;
	mutable cs_el : texpr list;
}

let is_empty cb =
	DynArray.empty cb.cb_el

let block_to_texpr_coroutine ctx bb vcontinuation vresult verror p =
	let open Texpr.Builder in
	let com = ctx.com in

	let eerror = make_local verror null_pos in

	let mk_int i = make_int com.basic i null_pos in

	let mk_assign estate eid =
		mk (TBinop (OpAssign,estate,eid)) eid.etype null_pos
	in

	let vstate = alloc_var VGenerated "_hx_state" com.basic.tint p in
	let estate = make_local vstate p in
	let set_state id = mk_assign estate (mk_int id) in

	let vexcstate = alloc_var VGenerated "_hx_exceptionState" com.basic.tint p in
	let eexcstate = make_local vexcstate p in
	let set_excstate id = mk_assign eexcstate (mk_int id) in

	let tstatemachine = tfun [t_dynamic; t_dynamic] com.basic.tvoid in
	let vstatemachine = alloc_var VGenerated "_hx_stateMachine" tstatemachine p in
	let estatemachine = make_local vstatemachine p in

	let get_next_state_id =
		let counter = ref 0 in
		fun () -> (let id = !counter in incr counter; id)
	in

	let get_rethrow_state_id =
		let rethrow_state_id = ref (-1) in
		fun () -> begin
			if !rethrow_state_id = (-1) then rethrow_state_id := get_next_state_id ();
			!rethrow_state_id;
		end
	in

	let mk_continuation_call eresult p =
		let econtinuation = make_local vcontinuation p in
		mk (TCall (econtinuation, [eresult; make_null t_dynamic p])) com.basic.tvoid p
	in
	let mk_continuation_call_error eerror p =
		let econtinuation = make_local vcontinuation p in
		mk (TCall (econtinuation, [make_null t_dynamic p; eerror])) com.basic.tvoid p
	in

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
		mk (TCall (ecreatecoroutine, [enull; enull])) com.basic.tvoid call.cs_pos
	in

	let std_is e t =
		let type_expr = mk (TTypeExpr (module_type_of_type t)) t_dynamic null_pos in
		Texpr.Builder.resolve_and_make_static_call ctx.com.std "isOfType" [e;type_expr] p
	in

	let states = ref [] in

	let exc_states = ref [] in

	let make_state id el = {
		cs_id = id;
		cs_el = el;
	} in
	let debug_endline s =
		if ctx.coro_debug then
			print_endline s
	in
	debug_endline "---";
	let rec loop bb state_id back_state_id current_el while_loop exc_state_id_getter =
		let el = DynArray.to_list bb.cb_el in

		let ereturn = mk (TReturn None) com.basic.tvoid p in

		let add_state next_id extra_el =
			let el = current_el @ el @ extra_el in
			let el = match next_id with
				| None ->
					el
				| Some id ->
					(set_state id) :: el
			in
			states := (make_state state_id el) :: !states
		in

		match bb.cb_next.next_kind with
		| NextSuspend (call, bb_next) ->
			let next_state_id = get_next_state_id () in
			debug_endline (Printf.sprintf "suspend cur:%d,next:%d,back:%d" state_id next_state_id back_state_id);
			loop bb_next next_state_id back_state_id [] while_loop exc_state_id_getter;
			let ecallcoroutine = mk_suspending_call call in
			add_state (Some next_state_id) [ecallcoroutine; ereturn]
		| NextUnknown when back_state_id = (-1) ->
			let ecallcontinuation = mk_continuation_call (make_null t_dynamic p) p in
			add_state (Some (-1)) [ecallcontinuation; ereturn]
		| NextUnknown ->
			add_state (Some back_state_id) []
		| NextBreak ->
			let _,next_state_id = Option.get while_loop in
			add_state (Some next_state_id) []
		| NextContinue ->
			let body_state_id,_ = Option.get while_loop in
			add_state (Some body_state_id) []
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
		| NextSub (bb_sub,bb_next) ->
			let sub_state_id = get_next_state_id () in
			let next_state_id = get_next_state_id () in
			debug_endline (Printf.sprintf "sub cur:%d,sub:%d,next:%d,back:%d" state_id sub_state_id next_state_id back_state_id);
			loop bb_next next_state_id back_state_id [] while_loop exc_state_id_getter;
			loop bb_sub sub_state_id next_state_id [] while_loop exc_state_id_getter;
			add_state (Some sub_state_id) []

		| NextIfThen (econd,bb_then,bb_next) ->
			let then_state_id = get_next_state_id () in
			let next_state_id = get_next_state_id () in
			debug_endline (Printf.sprintf "if-then cur:%d,then:%d,next:%d,back:%d" state_id then_state_id next_state_id back_state_id);
			loop bb_then then_state_id next_state_id [] while_loop exc_state_id_getter;
			loop bb_next next_state_id back_state_id [] while_loop exc_state_id_getter;
			let eif = mk (TIf (econd, set_state then_state_id, Some (set_state next_state_id))) com.basic.tint p in
			add_state None [eif]

		| NextIfThenElse (econd,bb_then,bb_else,bb_next) ->
			let then_state_id = get_next_state_id () in
			let else_state_id = get_next_state_id () in
			let next_state_id = get_next_state_id () in
			debug_endline (Printf.sprintf "if-then-else cur:%d,then:%d,else:%d,next:%d,back:%d" state_id then_state_id else_state_id next_state_id back_state_id);
			loop bb_then then_state_id next_state_id [] while_loop exc_state_id_getter;
			loop bb_else else_state_id next_state_id [] while_loop exc_state_id_getter;
			loop bb_next next_state_id back_state_id [] while_loop exc_state_id_getter;
			let eif = mk (TIf (econd, set_state then_state_id, Some (set_state else_state_id))) com.basic.tint p in
			add_state None [eif]

		| NextSwitch(switch, bb_next) ->
			let esubj = switch.cs_subject in
			let next_state_id = get_next_state_id () in
			debug_endline (Printf.sprintf "switch cur:%d,next:%d,back:%d" state_id next_state_id back_state_id);
			let ecases = List.map (fun (patterns,bb) ->
				(* TODO: variable capture and other fancy things O_o *)
				let case_state_id = get_next_state_id () in
				debug_endline (Printf.sprintf "  case %d" case_state_id);
				loop bb case_state_id next_state_id [] while_loop exc_state_id_getter;
				{case_patterns = patterns;case_expr = set_state case_state_id}
			) switch.cs_cases in
			let default_state_id = match switch.cs_default with
				| Some bb ->
					let default_state_id = get_next_state_id () in
					loop bb default_state_id next_state_id [] while_loop exc_state_id_getter;
					default_state_id
				| None ->
					next_state_id
			in
			debug_endline (Printf.sprintf "  default %d" default_state_id);
			let eswitch = mk_switch esubj ecases (Some (set_state default_state_id)) true in
			let eswitch = mk (TSwitch eswitch) com.basic.tvoid p in
			loop bb_next next_state_id back_state_id [] while_loop exc_state_id_getter;
			add_state None [eswitch]

		| NextWhile (e_cond, bb_body, bb_next) ->
			let body_state_id = get_next_state_id () in
			let next_state_id = get_next_state_id () in
			debug_endline (Printf.sprintf "while cur:%d,body:%d,next:%d,back:%d" state_id body_state_id next_state_id back_state_id);
			let new_while_loop = Some (body_state_id,next_state_id) in
			(* TODO: next is empty? *)
			loop bb_body body_state_id body_state_id [] new_while_loop exc_state_id_getter;
			loop bb_next next_state_id back_state_id [] while_loop exc_state_id_getter;
			add_state (Some body_state_id) []

		| NextTry (bb_try,catches,bb_next) ->
			let try_state_id = get_next_state_id () in
			let new_exc_state_id = get_next_state_id () in
			let next_state_id = get_next_state_id () in
			debug_endline (Printf.sprintf "try cur:%d,try:%d,catch:%d,next:%d,back:%d" state_id try_state_id new_exc_state_id next_state_id back_state_id);
			loop bb_try try_state_id next_state_id [set_excstate new_exc_state_id] while_loop (fun () -> new_exc_state_id); (* TODO: add test for nested try/catch *)
			let esetexcstate = set_excstate (exc_state_id_getter ()) in
			let catch_case =
				let erethrow = mk (TThrow eerror) t_dynamic null_pos in
				let eif =
					List.fold_left (fun enext (vcatch,bb_catch) ->
						let catch_state_id = get_next_state_id () in
						let ecatchvar = mk (TVar (vcatch, Some eerror)) com.basic.tvoid null_pos in
						loop bb_catch catch_state_id next_state_id [esetexcstate; ecatchvar] while_loop exc_state_id_getter;

						(* TODO: exceptions filter... *)
						match follow vcatch.v_type with
						| TDynamic _ ->
							set_state catch_state_id (* no next *)
						| t ->
							let etypecheck = std_is (make_local verror null_pos) vcatch.v_type in
							mk (TIf (etypecheck, set_state catch_state_id, Some enext)) com.basic.tvoid null_pos
					) erethrow catches
				in
				make_state new_exc_state_id [eif]
			in
			exc_states := catch_case :: !exc_states;
			loop bb_next next_state_id back_state_id [esetexcstate (* TODO: test propagation after try/catch *)] while_loop exc_state_id_getter;
			add_state (Some try_state_id) []
	in
	loop bb (get_next_state_id ()) (-1) [] None get_rethrow_state_id;

	let states = !states @ !exc_states in

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

	let rethrow_state_id = get_rethrow_state_id () in
	let rethrow_state = make_state rethrow_state_id [mk (TThrow eerror) com.basic.tvoid null_pos] in
	let states = states @ [rethrow_state] in
	let states = List.sort (fun state1 state2 -> state1.cs_id - state2.cs_id) states in

	let ethrow = mk (TBlock [
		set_state rethrow_state_id;
		mk (TThrow (make_string com.basic "Invalid coroutine state" p)) com.basic.tvoid p
	]) com.basic.tvoid null_pos
	in

	let switch =
		let cases = List.map (fun state ->
			{case_patterns = [mk_int state.cs_id];
			case_expr = mk (TBlock state.cs_el) ctx.com.basic.tvoid (punion_el null_pos state.cs_el);
		}) states in
		mk_switch estate cases (Some ethrow) true
	in
	let eswitch = mk (TSwitch switch) com.basic.tvoid p in

	let etry = mk (TTry (
		eswitch,
		[
			let vcaught = alloc_var VGenerated "e" t_dynamic null_pos in
			(vcaught, mk (TIf (
				mk (TBinop (OpEq, estate, mk_int rethrow_state_id)) com.basic.tbool null_pos,
				mk (TBlock [
					mk_assign eexcstate (mk_int rethrow_state_id);
					mk_continuation_call_error (make_local vcaught null_pos) null_pos;
					mk (TReturn None) com.basic.tvoid null_pos;
				]) com.basic.tvoid null_pos,
				Some (mk (TBlock [
					mk_assign estate eexcstate;
					mk_assign eerror (make_local vcaught null_pos);
				]) com.basic.tvoid null_pos)
			)) com.basic.tvoid null_pos)
		]
	)) com.basic.tvoid null_pos in

	let eloop = mk (TWhile (make_bool com.basic true p, etry, DoWhile)) com.basic.tvoid p in

	let eif = mk (TIf (
		mk (TBinop (
			OpNotEq,
			eerror,
			make_null verror.v_type p
		)) com.basic.tbool p,
		mk_assign estate eexcstate,
		None
	)) com.basic.tvoid p in

	let estatemachine_def = mk (TFunction {
		tf_args = [(vresult,None); (verror,None)];
		tf_type = com.basic.tvoid;
		tf_expr = mk (TBlock [eif; eloop]) com.basic.tvoid null_pos
	}) tstatemachine p in

	let state_var = mk (TVar (vstate, Some (make_int com.basic 0 p))) com.basic.tvoid p in
	let excstate_var = mk (TVar (vexcstate, Some (make_int com.basic rethrow_state_id p))) com.basic.tvoid p in
	let shared_vars = List.map (fun v -> mk (TVar (v,Some (Texpr.Builder.default_value v.v_type v.v_pos))) com.basic.tvoid null_pos) decls in
	let shared_vars = List.rev (excstate_var :: state_var :: shared_vars) in

	mk (TBlock (shared_vars @ [
		mk (TVar (vstatemachine, None)) com.basic.tvoid p;
		binop OpAssign estatemachine estatemachine_def estatemachine.etype p;
		mk (TReturn (Some estatemachine)) com.basic.tvoid p;
	])) com.basic.tvoid p
