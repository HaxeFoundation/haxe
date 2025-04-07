open Globals
open Type

let check_local_vars_init scom e =
	let intersect vl1 vl2 =
		PMap.mapi (fun v t -> t && PMap.find v vl2) vl1
	in
	let join vars cvars =
		List.iter (fun v -> vars := intersect !vars v) cvars
	in
	let restore vars old_vars declared =
		(* restore variables declared in this block to their previous state *)
		vars := List.fold_left (fun acc v ->
			try	PMap.add v (PMap.find v old_vars) acc with Not_found -> PMap.remove v acc
		) !vars declared;
	in
	let declared = ref [] in
	let outside_vars = ref IntMap.empty in
	(* Set variables which belong to current function *)
	let set_all_vars vars =
		vars := PMap.mapi (fun id is_set -> if IntMap.mem id !outside_vars then is_set else true) !vars
	in
	let rec loop vars e =
		match e.eexpr with
		| TLocal v ->
			let init = (try PMap.find v.v_id !vars with Not_found -> true) in
			if not init then begin
				if IntMap.mem v.v_id !outside_vars then
					if v.v_name = "this" then SafeCom.add_warning scom WVarInit "this might be used before assigning a value to it" e.epos
					else SafeCom.add_warning scom WVarInit ("Local variable " ^ v.v_name ^ " might be used before being initialized") e.epos
				else
					if v.v_name = "this" then Error.raise_typing_error "Missing this = value" e.epos
					else Error.raise_typing_error ("Local variable " ^ v.v_name ^ " used without being initialized") e.epos
			end
		| TVar (v,eo) ->
			begin
				match eo with
				| None when (match v.v_kind with VInlinedConstructorVariable _ -> true | _ -> false) ->
					()
				| None ->
					declared := v.v_id :: !declared;
					vars := PMap.add v.v_id false !vars
				| Some e ->
					loop vars e
			end
		| TBlock el ->
			let old = !declared in
			let old_vars = !vars in
			declared := [];
			List.iter (loop vars) el;
			restore vars old_vars (List.rev !declared);
			declared := old;
		| TBinop (OpAssign,{ eexpr = TLocal v },e) when PMap.mem v.v_id !vars ->
			begin match (Texpr.skip e).eexpr with
				| TFunction _ ->
					(* We can be sure that the function doesn't execute immediately, so it's fine to
					   consider the local initialized (issue #9919). *)
					vars := PMap.add v.v_id true !vars;
					loop vars e;
				| _ ->
					loop vars e;
					vars := PMap.add v.v_id true !vars
			end
		| TIf (e1,e2,eo) ->
			loop vars e1;
			let vbase = !vars in
			loop vars e2;
			(match eo with
			| None -> vars := vbase
			(* ignore else false cases (they are added by the side-effect handler) *)
			| Some {eexpr = TConst (TBool(false))} -> ()
			| Some e ->
				let v1 = !vars in
				vars := vbase;
				loop vars e;
				vars := intersect !vars v1)
		| TWhile (cond,e,flag) ->
			(match flag with
			| NormalWhile when (match cond.eexpr with TParenthesis {eexpr = TConst (TBool true)} -> false | _ -> true) ->
				loop vars cond;
				let old = !vars in
				loop vars e;
				vars := old;
			| _ ->
				loop vars e;
				loop vars cond)
		| TTry (e,catches) ->
			let cvars = List.map (fun (v,e) ->
				let old = !vars in
				loop vars e;
				let v = !vars in
				vars := old;
				v
			) catches in
			loop vars e;
			join vars cvars;
		| TSwitch ({switch_subject = e;switch_cases = cases;switch_default = def} as switch) ->
			loop vars e;
			let cvars = List.map (fun {case_patterns = ec;case_expr = e} ->
				let old = !vars in
				List.iter (loop vars) ec;
				vars := old;
				loop vars e;
				let v = !vars in
				vars := old;
				v
			) cases in
			(match def with
			| None when switch.switch_exhaustive ->
				(match cvars with
				| cv :: cvars ->
					PMap.iter (fun i b -> if b then vars := PMap.add i b !vars) cv;
					join vars cvars
				| [] -> ())
			| None -> ()
			| Some e ->
				loop vars e;
				join vars cvars)
		(* mark all reachable vars as initialized, since we don't exit the block  *)
		| TBreak | TContinue | TReturn None ->
			set_all_vars vars
		| TThrow e | TReturn (Some e) ->
			loop vars e;
			set_all_vars vars
		| TFunction tf ->
			let old = !outside_vars in
			(* Mark all known variables as "outside" so we can ignore their initialization state within the function.
			   We cannot use `vars` directly because we still care about initializations the function might make.
			*)
			PMap.iter (fun i _ -> outside_vars := IntMap.add i true !outside_vars) !vars;
			loop vars tf.tf_expr;
			outside_vars := old;
		| _ ->
			Type.iter (loop vars) e
	in
	loop (ref PMap.empty) e;
	e
