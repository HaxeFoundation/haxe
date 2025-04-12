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

let block_to_texpr_coroutine ctx cb cls tf_args forbidden_vars econtinuation ecompletion eresult estate p =
	let open Texpr.Builder in
	let com = ctx.typer.com in

	let mk_assign estate eid =
		mk (TBinop (OpAssign,estate,eid)) eid.etype null_pos
	in

	let set_state id = mk_assign estate (mk_int com id) in

	let std_is e t =
		let type_expr = mk (TTypeExpr (module_type_of_type t)) t_dynamic null_pos in
		Texpr.Builder.resolve_and_make_static_call com.std "isOfType" [e;type_expr] p
	in

	let cb_uncaught = CoroFunctions.make_block ctx None in
	let mk_suspending_call call =
		let p = call.cs_pos in

		(* lose Coroutine<T> type for the called function not to confuse further filters and generators *)
		(* let tcoroutine = tfun [t_dynamic; t_dynamic] com.basic.tvoid in *)
		let tfun = match follow_with_coro call.cs_fun.etype with
			| Coro (args, ret) ->
				let args,ret = Common.expand_coro_type com.basic args ret in
				TFun (args, com.basic.tany)
			| NotCoro _ ->
				die "Unexpected coroutine type" __LOC__
		in
		let efun = { call.cs_fun with etype = tfun } in
		let args = call.cs_args @ [ econtinuation ] in
		let ecreatecoroutine = mk (TCall (efun, args)) com.basic.tany call.cs_pos in

		let vcororesult = alloc_var VGenerated "_hx_tmp" com.basic.tany p in
		let ecororesult = make_local vcororesult p in
		let cororesult_var = mk (TVar (vcororesult, (Some ecreatecoroutine))) com.basic.tany p in

		let cls_primitive =
			match com.basic.tcoro_primitive with
			| TInst (cls, _) -> cls
			| _ -> die "Unexpected coroutine primitive type" __LOC__
			in

		let cls_field = cls_primitive.cl_statics |> PMap.find "suspended" in

		let tcond = std_is ecororesult com.basic.tcoro_primitive in
		let tif = mk (TReturn (Some (make_static_field cls_primitive cls_field p))) com.basic.tany p in
		let telse = mk_assign eresult ecororesult in
		[
			cororesult_var;
			mk (TIf (tcond, tif, Some telse)) com.basic.tvoid p
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

		let ereturn = mk (TReturn (Some (make_null com.basic.tany p))) com.basic.tany p in

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
			add_state (Some (-1)) [ereturn]
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
			add_state (Some (-1)) [ mk (TReturn (Some (make_null com.basic.tany p))) com.basic.tany p ]
		| NextReturn e ->
			(* let eresult = match r with
				| NextReturn e -> e
				| _ -> make_null t_dynamic p
			in *)
			(* let ecallcontinuation = mk_continuation_call eresult p in *)
			(* ecallcontinuation; *)
			add_state (Some (-1)) [ mk (TReturn (Some e)) com.basic.tany p ]
		| NextThrow e1 ->
			let ethrow = mk (TThrow e1) t_dynamic p in
			add_state (Some (-1)) [ethrow]
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
	let rethrow_state_id = cb_uncaught.cb_id in
	let rethrow_state = make_state rethrow_state_id [mk (TThrow eresult) com.basic.tvoid null_pos] in
	let states = states @ [rethrow_state] in

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
			(Hashtbl.length m) > 1 && not ((List.exists (fun id -> id = v_id)) forbidden_vars)
		in
		let rec loop cases decls =
			match cases with
			| state :: rest ->
				let decls = ref decls in
				begin
					let rec loop e =
						match e.eexpr with
						(* TODO : Should this be handled here? *)
						(* Also need to check if this should be the continuation instead of completion *)
						| TCall ({ eexpr = TField (_, FStatic ({ cl_path = (["haxe";"coro"], "Intrinsics") }, { cf_name = "currentContinuation" })) }, []) ->
							ecompletion
						| TVar (v, eo) when is_used_across_states v.v_id ->
							decls := v :: !decls;

							let name = if v.v_kind = VGenerated then
								Printf.sprintf "_hx_hoisted%i" v.v_id
							else
								v.v_name in
							let field  = mk_field name v.v_type v.v_pos null_pos in
							let efield = mk (TField(econtinuation,FInstance(cls, [], field))) field.cf_type p in
							let einit  =
								match eo with
								| None -> default_value v.v_type v.v_pos
								| Some e -> Type.map_expr loop e in
							mk (TBinop (OpAssign,efield,einit)) v.v_type e.epos
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

	List.iter
		(fun s ->
			let is_used_across_states v_id =
				match Hashtbl.find_opt var_usages v_id with
				| Some m ->
					(Hashtbl.length m) > 1 && not ((List.exists (fun id -> id = v_id)) forbidden_vars)
				| None ->
					false
			in
			let rec loop e =
				match e.eexpr with
				| TLocal v when is_used_across_states v.v_id ->
					let name = if v.v_kind = VGenerated then
						Printf.sprintf "_hx_hoisted%i" v.v_id
					else
						v.v_name in
					let field = mk_field name v.v_type v.v_pos null_pos in
					mk (TField(econtinuation,FInstance(cls, [], field))) field.cf_type p
				| _ -> Type.map_expr loop e
			in
			s.cs_el <- List.map loop s.cs_el)
		states;

	let states = List.sort (fun state1 state2 -> state1.cs_id - state2.cs_id) states in

	(* Also check function argumens to see if they're used across states *)
	(* If so insert an assignment into the initial state to set our hoisted field *)
	let decls = decls @ List.filter_map (fun (arg, _) ->
		let is_used_across_states v_id =
			match Hashtbl.find_opt var_usages v_id with
			| Some m ->
				(Hashtbl.length m) > 1 && not ((List.exists (fun id -> id = v_id)) forbidden_vars)
			| None ->
				false
		in
		if is_used_across_states arg.v_id then
			let mk_assign estate eid =
				mk (TBinop (OpAssign,estate,eid)) eid.etype null_pos
			in

			let initial = List.hd states in
			let name = if arg.v_kind = VGenerated then
				Printf.sprintf "_hx_hoisted%i" arg.v_id
			else
				arg.v_name in
			let field   = mk_field name arg.v_type arg.v_pos null_pos in
			let efield  = mk (TField(econtinuation,FInstance(cls, [], field))) field.cf_type p in
			let assign  = mk_assign efield (Builder.make_local arg p) in

			initial.cs_el <- assign :: initial.cs_el;

			Some arg
		else
			None
	) tf_args in

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

	(* let shared_vars = List.map (fun v -> mk (TVar (v,Some (Texpr.Builder.default_value v.v_type v.v_pos))) com.basic.tvoid null_pos) decls in
	let shared_vars = List.rev shared_vars in
	let shared_vars = match ctx.vthis with
		| None ->
			shared_vars
		| Some v ->
			let e_this = mk (TConst TThis) v.v_type v.v_pos in
			let e_var = mk (TVar(v,Some e_this)) com.basic.tvoid null_pos in
			e_var :: shared_vars
	in *)

	eloop, !init_state, decls |> List.map (fun v ->
		let name = if v.v_kind = VGenerated then
			Printf.sprintf "_hx_hoisted%i" v.v_id
		else
			v.v_name in
		mk_field name v.v_type v.v_pos null_pos)
