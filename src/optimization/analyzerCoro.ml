open Globals
open Type
open AnalyzerTypes
open BasicBlock
open Graph
open Texpr

let block_to_texpr_coroutine ctx bb vcontinuation vresult verror p =
	assert(bb.bb_closed);

	let open Texpr.Builder in
	let com = ctx.com in

	let eerror = make_local verror null_pos in

	let mk_int i = make_int com.basic i null_pos in

	let mk_assign estate eid =
		mk (TBinop (OpAssign,estate,eid)) eid.etype null_pos
	in

	let vstate = alloc_var VGenerated "_hx_state" com.basic.tint p in
	add_var_flag vstate VCaptured;
	declare_var ctx.graph vstate bb;
	let estate = make_local vstate p in
	let set_state id = mk_assign estate (mk_int id) in

	let vexcstate = alloc_var VGenerated "_hx_exceptionState" com.basic.tint p in
	add_var_flag vexcstate VCaptured;
	declare_var ctx.graph vexcstate bb;
	let eexcstate = make_local vexcstate p in
	let set_excstate id = mk_assign eexcstate (mk_int id) in

	let tstatemachine = tfun [t_dynamic; t_dynamic] com.basic.tvoid in
	let vstatemachine = alloc_var VGenerated "_hx_stateMachine" tstatemachine p in
	add_var_flag vstatemachine VCaptured;
	declare_var ctx.graph vstatemachine bb;
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
		let p = call.pos in

		(* lose Coroutine<T> type for the called function not to confuse further filters and generators *)
		let tcoroutine = tfun [t_dynamic; t_dynamic] com.basic.tvoid in
		let tfun = match follow call.efun.etype with
			| TFun (args, ret, true) ->
				let tcontinuation = tfun [ret; t_dynamic] com.basic.tvoid in
				let args = args @ [("",false,tcontinuation)] in
				TFun (args, tcoroutine, false)
			| _ ->
				die "Unexpected coroutine type" __LOC__
		in
		let efun = { call.efun with etype = tfun } in
		let args = call.args @ [ estatemachine ] in
		let ecreatecoroutine = mk (TCall (efun, args)) tcoroutine call.pos in
		let enull = make_null t_dynamic p in
		mk (TCall (ecreatecoroutine, [enull; enull])) com.basic.tvoid call.pos
	in

	(* TODO: stolen from exceptions.ml. we should really figure out the filter ordering here *)
	let std_is e t =
		let std_cls =
			(* TODO: load it? *)
			match (try List.find (fun t -> t_path t = ([],"Std")) com.types with Not_found -> die "" __LOC__) with
			| TClassDecl cls -> cls
			| _ -> die "" __LOC__
		in
		let isOfType_field =
			try PMap.find "isOfType" std_cls.cl_statics
			with Not_found -> die "" __LOC__
		in
		let type_expr = mk (TTypeExpr (module_type_of_type t)) t_dynamic null_pos in
		let isOfType_expr = Typecore.make_static_field_access std_cls isOfType_field isOfType_field.cf_type null_pos in
		mk (TCall (isOfType_expr, [e; type_expr])) com.basic.tbool null_pos
	in


	let states = ref [] in

	let exc_states = ref [] in

	let debug_endline s =
		if ctx.config.coro_debug then
			print_endline s
	in
	(* TODO: maybe merge this into block_to_texpr somehow, and only introduce new states when there is a suspension point *)
	debug_endline "---";
	let rec loop bb state_id back_state_id current_el while_loop exc_state_id_getter =
		let p = bb.bb_pos in
		(* TODO: only do this in the end, avoid unnecessary List.rev *)
		let el = DynArray.to_list bb.bb_el in

		let ereturn = mk (TReturn None) com.basic.tvoid p in

		let add_state el =
			states := (state_id,mk (TBlock el) com.basic.tvoid null_pos) :: !states
		in
		let get_cond_branch () = match bb.bb_terminator with TermCondBranch e -> e | _ -> die "" __LOC__ in

		match bb.bb_syntax_edge with
		| SESuspend (call, bb_next) ->
			let next_state_id = get_next_state_id () in
			debug_endline (Printf.sprintf "suspend cur:%d,next:%d,back:%d" state_id next_state_id back_state_id);
			loop bb_next next_state_id back_state_id [] while_loop exc_state_id_getter;
			let ecallcoroutine = mk_suspending_call call in
			let esetstate = set_state next_state_id in
			add_state (current_el @ el @ [esetstate; ecallcoroutine; ereturn])

		| SENone ->
			debug_endline (Printf.sprintf "none cur:%d,back:%d" state_id back_state_id);
			(match bb.bb_terminator with
			| TermBreak _ -> (* todo use pos *)
				let _,next_state_id = Option.get while_loop in
				let esetstate = set_state next_state_id in
				add_state (current_el @ el @ [esetstate])
			| TermContinue _ -> (* todo use pos *)
				let body_state_id,_ = Option.get while_loop in
				let esetstate = set_state body_state_id in
				add_state (current_el @ el @ [esetstate])
			| TermReturn _ | TermReturnValue _ -> (* todo use pos *)
				let esetstate = set_state (-1) in
				let eresult = match bb.bb_terminator with
					| TermReturnValue (e,_) -> e
					| _ -> make_null t_dynamic p
				in
				let ecallcontinuation = mk_continuation_call eresult p in
				add_state (current_el @ el @ [esetstate; ecallcontinuation; ereturn])
			| TermNone when back_state_id = -1 ->
				let esetstate = set_state (-1) in
				let ecallcontinuation = mk_continuation_call (make_null t_dynamic p) p in
				add_state (current_el @ el @ [esetstate; ecallcontinuation; ereturn])
			| TermNone ->
				add_state (current_el @ el @ [set_state back_state_id])
			| TermThrow (e,p) ->
				let ethrow = mk (TThrow e) t_dynamic p in
				add_state (current_el @ el @ [ethrow])
			| TermCondBranch _ ->
				die "unexpected TermCondBranch" __LOC__)

		| SEMerge bb_next ->
			debug_endline (Printf.sprintf "merge cur:%d,back:%d" state_id back_state_id);
			loop bb_next state_id back_state_id (current_el @ el) while_loop exc_state_id_getter

		| SESubBlock (bb_sub,bb_next) ->
			let sub_state_id = get_next_state_id () in
			let next_state_id = get_next_state_id () in
			debug_endline (Printf.sprintf "sub cur:%d,sub:%d,next:%d,back:%d" state_id sub_state_id next_state_id back_state_id);
			loop bb_next next_state_id back_state_id [] while_loop exc_state_id_getter;
			loop bb_sub sub_state_id next_state_id [] while_loop exc_state_id_getter;
			add_state (current_el @ el @ [set_state sub_state_id])

		| SEIfThen (bb_then,bb_next,p) ->
			let econd = get_cond_branch () in
			let then_state_id = get_next_state_id () in
			let next_state_id = get_next_state_id () in
			debug_endline (Printf.sprintf "if-then cur:%d,then:%d,next:%d,back:%d" state_id then_state_id next_state_id back_state_id);
			loop bb_then then_state_id next_state_id [] while_loop exc_state_id_getter;
			loop bb_next next_state_id back_state_id [] while_loop exc_state_id_getter;
			let eif = mk (TIf (econd, set_state then_state_id, Some (set_state next_state_id))) com.basic.tint p in
			add_state (current_el @ el @ [eif])

		| SEIfThenElse (bb_then,bb_else,bb_next,t,p) ->
			let econd = get_cond_branch () in
			let then_state_id = get_next_state_id () in
			let else_state_id = get_next_state_id () in
			let next_state_id = get_next_state_id () in
			debug_endline (Printf.sprintf "if-then-else cur:%d,then:%d,else:%d,next:%d,back:%d" state_id then_state_id else_state_id next_state_id back_state_id);
			loop bb_then then_state_id next_state_id [] while_loop exc_state_id_getter;
			loop bb_else else_state_id next_state_id [] while_loop exc_state_id_getter;
			loop bb_next next_state_id back_state_id [] while_loop exc_state_id_getter;
			let eif = mk (TIf (econd, set_state then_state_id, Some (set_state else_state_id))) com.basic.tint p in
			add_state (current_el @ el @ [eif])

		| SESwitch switch ->
			let esubj = get_cond_branch () in
			let next_state_id = get_next_state_id () in
			debug_endline (Printf.sprintf "switch cur:%d,next:%d,back:%d" state_id next_state_id back_state_id);
			let ecases = List.map (fun (patterns,bb) ->
				(* TODO: variable capture and other fancy things O_o *)
				let case_state_id = get_next_state_id () in
				debug_endline (Printf.sprintf "  case %d" case_state_id);
				loop bb case_state_id next_state_id [] while_loop exc_state_id_getter;
				{case_patterns = patterns;case_expr = set_state case_state_id}
			) switch.ss_cases in
			let default_state_id = match switch.ss_default with
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
			loop switch.ss_next next_state_id back_state_id [] while_loop exc_state_id_getter;
			add_state (current_el @ el @ [eswitch])

		| SEWhile (bb_body, bb_next, p) ->
			let body_state_id = get_next_state_id () in
			let next_state_id = get_next_state_id () in
			debug_endline (Printf.sprintf "while cur:%d,body:%d,next:%d,back:%d" state_id body_state_id next_state_id back_state_id);
			let new_while_loop = Some (body_state_id,next_state_id) in
			(* TODO: next is empty? *)
			loop bb_body body_state_id body_state_id [] new_while_loop exc_state_id_getter;
			loop bb_next next_state_id back_state_id [] while_loop exc_state_id_getter;
			add_state (current_el @ el @ [set_state body_state_id]);

		| SETry (bb_try,_,catches,bb_next,p) ->
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
				(new_exc_state_id, eif)
			in
			exc_states := catch_case :: !exc_states;
			loop bb_next next_state_id back_state_id [esetexcstate (* TODO: test propagation after try/catch *)] while_loop exc_state_id_getter;
			add_state (current_el @ el @ [set_state try_state_id])
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
		List.iter (fun (state_id, expr) ->
			let rec loop e =
				match e.eexpr with
				| TVar (v, eo) ->
					Option.may loop eo;
					use v state_id;
				| TLocal v ->
					use v state_id;
				| _ ->
					Type.iter loop e
			in
			loop expr
		) states;
	end;
	let states, decls = begin
		let is_used_across_states v_id =
			let m = Hashtbl.find var_usages v_id in
			(Hashtbl.length m) > 1
		in
		let rec loop cases cases_acc decls =
			match cases with
			| (id,expr) :: rest ->
				let decls = ref decls in
				let expr = begin
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
					loop expr
				end in
				loop rest ((id,expr) :: cases_acc) !decls
			| [] ->
				List.rev cases_acc, decls
		in
		loop states [] []
	end in

	(* TODO:
		we can optimize while and switch in some cases:
		- if there's only one state (no suspensions) - don't wrap into while/switch, don't introduce state var
	*)

	let rethrow_state_id = get_rethrow_state_id () in
	let rethrow_state = (rethrow_state_id, mk (TThrow eerror) com.basic.tvoid null_pos) in
	let states = states @ [rethrow_state] in

	let ethrow = mk (TBlock [
		set_state rethrow_state_id;
		mk (TThrow (make_string com.basic "Invalid coroutine state" p)) com.basic.tvoid p
	]) com.basic.tvoid null_pos
	in

	let switch =
		let cases = List.map (fun (id,e) -> {case_patterns = [mk_int id];case_expr = e}) states in
		mk_switch estate cases (Some ethrow) true
	in
	let eswitch = mk (TSwitch switch) com.basic.tvoid p in

	let etry = mk (TTry (
		eswitch,
		[
			let vcaught = alloc_var VGenerated "e" t_dynamic null_pos in
			declare_var ctx.graph vcaught bb;
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
	let shared_vars = List.map (fun v -> mk (TVar (v,None)) com.basic.tvoid null_pos) decls in
	let shared_vars = List.rev (excstate_var :: state_var :: shared_vars) in

	mk (TBlock (shared_vars @ [
		mk (TVar (vstatemachine, Some estatemachine_def)) com.basic.tvoid p;
		mk (TReturn (Some estatemachine)) com.basic.tvoid p;
	])) com.basic.tvoid p
