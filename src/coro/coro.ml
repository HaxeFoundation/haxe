open Globals
open Type
open CoroTypes
open CoroFunctions
open Texpr
open ContTypes

let next_closure_id = Hashtbl.create 0;

type coro_for =
	| LocalFunc of tfunc * tvar
	| ClassField of tclass * tclass_field * tfunc * pos (* expr pos *)

type coro_cls = {
	params : typed_type_param list;
	param_types : Type.t list;
	cls_t : Type.t;
	result_type : Type.t;
	cont_type : Type.t;
}

let substitute_type_params subst t =
	let rec loop t = match t with
		| TInst({cl_kind = KTypeParameter ttp}, []) ->
			(try List.assq ttp subst with Not_found -> t)
		| _ ->
			Type.map loop t
	in
	loop t

module ContinuationClassBuilder = struct
	type coro_class = {
		cls : tclass;
		name_pos : pos;
		(* inside = inside the continuation class *)
		inside : coro_cls;
		(* outside = in the original function *)
		outside : coro_cls;
		type_param_subst : (typed_type_param * Type.t) list;
		coro_type : coro_for;
		continuation_api : ContTypes.continuation_api;
	}

	let create ctx coro_type =
		(* Mangle class names to hopefully get unique names and avoid collisions *)
		let name, result_type, name_pos =
			let managled_class_name = Printf.sprintf "HxCoro_%s_%s" (ctx.typer.c.curclass.cl_path |> fst |> String.concat "_") (ctx.typer.c.curclass.cl_path |> snd) in
			match coro_type with
			| ClassField (_, field, tf, _) ->
				Printf.sprintf "%s_%s" managled_class_name field.cf_name,
				tf.tf_type,
				field.cf_name_pos
			| LocalFunc(f,v) ->
				let next_id =
					match Hashtbl.find_opt next_closure_id managled_class_name with
					| Some id ->
						Hashtbl.replace next_closure_id managled_class_name (id + 1);
						id
					| _ ->
						Hashtbl.replace next_closure_id managled_class_name 1;
						0
					in
				let n = Printf.sprintf "%s_AnonFunc%i" managled_class_name next_id in

				n, f.tf_type, v.v_pos
			in

		let result_type = if ExtType.is_void (follow result_type) then ctx.typer.t.tunit else result_type in
		(* Is there a pre-existing function somewhere to a valid path? *)
		let cls_path = ((fst ctx.typer.m.curmod.m_path) @ [ Printf.sprintf "_%s" (snd ctx.typer.m.curmod.m_path) ]), name in
		let cls      = mk_class ctx.typer.m.curmod cls_path name_pos name_pos in
		let params_outside = ctx.typer.type_params in
		let params_inside = List.map (fun ttp ->
			(* TODO: this duplicates clone_type_parameter *)
			let c = ttp.ttp_class in
			let map = fun t -> t in (* TODO: ? *)
			let c = {c with cl_path = ([],ttp.ttp_name)} in
			let def = Option.map map ttp.ttp_default in
			let constraints = match ttp.ttp_constraints with
				| None -> None
				| Some constraints -> Some (lazy (List.map map (Lazy.force constraints)))
			in
			mk_type_param c TPHType (* !!! *) def constraints
		 ) params_outside in
		cls.cl_params <- params_inside;

		let continuation_api = match ctx.typer.g.continuation_api with
			| Some api ->
				api
			| None ->
				CoroInit.make_continuation_api ctx.typer
		in

		let param_types_inside = extract_param_types params_inside in
		let param_types_outside = extract_param_types params_outside in
		let subst = List.combine params_outside param_types_inside in
		let result_type_inside = substitute_type_params subst result_type in
		cls.cl_super <- Some (continuation_api.base_continuation_class, [result_type_inside]);

		{
			cls        = cls;
			name_pos;
			inside = {
				params = params_inside;
				param_types = param_types_inside;
				cls_t = TInst(cls,param_types_inside);
				result_type = result_type_inside;
				cont_type = TInst(continuation_api.base_continuation_class,[result_type_inside]);
			};
			outside = {
				params = params_outside;
				param_types = param_types_outside;
				cls_t = TInst(cls,param_types_outside);
				result_type = result_type;
				cont_type = TInst(continuation_api.base_continuation_class,[result_type]);
			};
			type_param_subst = subst;
			coro_type  = coro_type;
			continuation_api;
		}

	let mk_ctor ctx cont coro_class initial_state cf_captured hoisted_args =
		let basic = ctx.typer.t in
		let b     = ctx.builder in
		let name  = "completion" in
		let ethis = mk (TConst TThis) coro_class.inside.cls_t coro_class.name_pos in

		let vargcompletion    = alloc_var VGenerated name cont.continuation coro_class.name_pos in
		let evarargcompletion = b#local vargcompletion coro_class.name_pos in
		let einitialstate     = b#int initial_state coro_class.name_pos in
		let esuper            = b#call (b#super coro_class.inside.cont_type coro_class.name_pos) [ evarargcompletion; einitialstate ] basic.tvoid in

		let this_field cf =
			b#instance_field ethis coro_class.cls coro_class.inside.param_types cf cf.cf_type
		in

		let captured =
			cf_captured
			|> Option.map
				(fun field ->
					let vargcaptured    = alloc_var VGenerated "_hx_captured" field.cf_type coro_class.name_pos in
					let eargcaptured    = b#local vargcaptured coro_class.name_pos in
					let ecapturedfield  = this_field field in
					vargcaptured, b#assign ecapturedfield eargcaptured)
			in

		(* For the inline path (no captures), function arguments are passed as constructor parameters
		   so the thin wrapper can be a single  new Ctor(completion, arg1, ...).invokeResume()  call. *)
		let hoisted =
			List.map (fun (name, _, field) ->
				let varg = alloc_var VGenerated name field.cf_type coro_class.name_pos in
				let earg = b#local varg coro_class.name_pos in
				let efield = this_field field in
				varg, b#assign efield earg
			) hoisted_args
		in

		(* If the coroutine field is not static then our HxCoro class needs to capture this for future resuming *)

		let eblock, tfun_args, tfunction_args =
			let extra_exprs, extra_tfun_args, extra_tfunction_args =
				captured |>
					Option.map_default
						(fun (v, expr) ->
							[ expr ],
							[ (v.v_name, false, v.v_type) ],
							[ (v, None) ])
						([], [], [])
				in
			let hoisted_exprs        = List.map snd hoisted in
			let hoisted_tfun_args    = List.map (fun (v, _) -> (v.v_name, false, v.v_type)) hoisted in
			let hoisted_tfunction_args = List.map (fun (v, _) -> (v, None)) hoisted in

			b#void_block (esuper :: hoisted_exprs @ extra_exprs),
			[ (name, false, cont.continuation) ] @ hoisted_tfun_args @ extra_tfun_args,
			[ (vargcompletion, None) ] @ hoisted_tfunction_args @ extra_tfunction_args
		in

		let field = mk_field "new" (TFun (tfun_args, basic.tvoid)) coro_class.name_pos coro_class.name_pos in
		let func  = TFunction { tf_type = basic.tvoid; tf_args = tfunction_args; tf_expr = eblock } in
		let expr = mk func field.cf_type coro_class.name_pos in
		field.cf_expr <- Some expr;
		field.cf_kind <- Method MethNormal;

		if ctx.config.debug then
			s_expr_debug expr |> Printf.printf "%s\n";

		field

	let mk_invoke_resume_with_body ctx coro_class vcontinuation vtmp_result vtmp_error vtmp_error_unwrapped eresult eloop =
		let basic = ctx.typer.t in
		let b     = ctx.builder in
		let tret_invoke_resume = (TInst(Lazy.force ctx.typer.t.tcoro.suspension_result_class,[coro_class.inside.result_type])) in
		let ethis = b#this coro_class.inside.cls_t coro_class.name_pos in
		let subst = substitute_type_params coro_class.type_param_subst in
		let var_map = Hashtbl.create 8 in
		let map_var v =
			match Hashtbl.find_opt var_map v.v_id with
			| Some v' -> v'
			| None ->
				let v' = { v with v_type = subst v.v_type } in
				Hashtbl.replace var_map v.v_id v';
				v'
		in
		let rec subst_eloop e =
			Texpr.map_expr_type subst_eloop subst map_var e
		in
		let el = [
			b#var_init vcontinuation ethis;
		] @ (if Lazy.is_val vtmp_result then [b#var_init (Lazy.force vtmp_result) eresult] else []) @ [
			b#var_init_null vtmp_error;
		] in
		let el = if Lazy.is_val vtmp_error_unwrapped then
			el @ [b#var_init_null (Lazy.force vtmp_error_unwrapped)]
		else
			el
		in
		let el = el @ [eloop] in
		let block = subst_eloop (b#void_block el) in
		let func  = TFunction { tf_type = tret_invoke_resume; tf_args = []; tf_expr = block } in
		let expr  = mk func basic.tvoid coro_class.name_pos in
		let field = mk_field "invokeResume" (TFun ([], tret_invoke_resume)) coro_class.name_pos coro_class.name_pos in
		add_class_field_flag field CfOverride;
		field.cf_expr <- Some expr;
		field.cf_kind <- Method MethNormal;

		if ctx.config.debug then
			s_expr_debug expr |> Printf.printf "%s\n";

		field

	let mk_invoke_resume_thunk_call ctx coro_class cf_captured =
		let basic = ctx.typer.t in
		let b     = ctx.builder in
		let tret_invoke_resume = (TInst(Lazy.force ctx.typer.t.tcoro.suspension_result_class,[coro_class.inside.result_type])) in
		let ethis = b#this coro_class.inside.cls_t coro_class.name_pos in
		let ecaptured   = b#instance_field ethis coro_class.cls coro_class.inside.param_types cf_captured cf_captured.cf_type in
		let ecall = b#call ecaptured [] tret_invoke_resume in
		let block = b#void_block [ b#return ecall ] in
		let func  = TFunction { tf_type = tret_invoke_resume; tf_args = []; tf_expr = block } in
		let expr  = mk func basic.tvoid coro_class.name_pos in
		let field = mk_field "invokeResume" (TFun ([], tret_invoke_resume)) coro_class.name_pos coro_class.name_pos in
		add_class_field_flag field CfOverride;
		field.cf_expr <- Some expr;
		field.cf_kind <- Method MethNormal;

		if ctx.config.debug then
			s_expr_debug expr |> Printf.printf "%s\n";

		field
end

let create_continuation_class ctx cont coro_class initial_state invoke_resume_field gen_mode hoisted_args =
	let cf_captured_field = match gen_mode with
		| GenInline cfo -> Option.map snd cfo
		| GenThunk cf -> Some cf
	in
	let ctor   = ContinuationClassBuilder.mk_ctor ctx cont coro_class initial_state cf_captured_field hoisted_args in
	TClass.add_field coro_class.cls invoke_resume_field;
	Option.may (TClass.add_field coro_class.cls) cf_captured_field;
	coro_class.cls.cl_constructor <- Some ctor;
	if ctx.config.debug then
		Printer.s_tclass "\t" coro_class.cls |> Printf.printf "%s\n";

	ctx.typer.m.curmod.m_types <- ctx.typer.m.curmod.m_types @ [ TClassDecl coro_class.cls ]

let check_assertions assert_config num_states num_hoisted p =
	let open CoroConfig in
	begin match assert_config with
	| None -> ()
	| Some assert_config ->
		(match assert_config.num_states with
		| None -> ()
		| Some expected ->
			if num_states <> expected then
				Error.raise_typing_error
					(Printf.sprintf "Expected %d coroutine state(s), got %d" expected num_states) p);
		(match assert_config.num_hoisted with
		| None -> ()
		| Some expected ->
			if num_hoisted <> expected then
				Error.raise_typing_error
					(Printf.sprintf "Expected %d hoisted field(s), got %d" expected num_hoisted) p)
	end

let coro_to_state_machine ctx coro_class cb_root exprs args vtmp_result vtmp_error vtmp_error_unwrapped vcompletion vcontinuation gen_mode stack_item_inserter start_exception =
	let b = ctx.builder in
	let cont = coro_class.ContinuationClassBuilder.continuation_api in
	let eloop, initial_state, fields, num_states = CoroToTexpr.block_to_texpr_coroutine ctx cb_root cont coro_class.cls coro_class.outside.param_types args exprs coro_class.name_pos stack_item_inserter start_exception in
	(* Check @:coroutine(assert) config *)
	check_assertions ctx.config.assert_config num_states (List.length fields) coro_class.name_pos;
	(* update cf_type to use inside type parameters *)
	List.iter (fun cf ->
		cf.cf_type <- substitute_type_params coro_class.type_param_subst cf.cf_type;
		TClass.add_field coro_class.cls cf
	) fields;

	let {CoroToTexpr.ecompletion;eresult;_} = exprs in
	let tret_invoke_resume = cont.suspension_result coro_class.outside.result_type in

	let invoke_resume_field = match gen_mode with
		| GenInline cfo ->
			ContinuationClassBuilder.mk_invoke_resume_with_body ctx coro_class vcontinuation vtmp_result vtmp_error vtmp_error_unwrapped eresult eloop
		| GenThunk cf_captured ->
			ContinuationClassBuilder.mk_invoke_resume_thunk_call ctx coro_class cf_captured
	in

	(* Collect (orig_var, hoisted_field) pairs for function arguments that were hoisted
	   into continuation fields.  These become constructor parameters for both the inline
	   and thunk paths, so the constructor can assign them in one place. *)
	let hoisted_args = List.filter_map (fun (v, _) ->
		let field_name = Printf.sprintf "_hx_hoisted%i" v.v_id in
		match (try Some (PMap.find field_name coro_class.ContinuationClassBuilder.cls.cl_fields) with Not_found -> None) with
		| Some field -> Some (v.v_name, b#local v v.v_pos, field)
		| None -> None
	) args in
	create_continuation_class ctx cont coro_class initial_state invoke_resume_field gen_mode hoisted_args;

	let t = coro_class.outside.cls_t in

	begin match gen_mode with
		| GenInline cf_captured ->
			(* Inline path: pass hoisted args directly to the constructor and call invokeResume()
			   on the result — no intermediate variable, no separate field assignments. *)
			let ctor_args = ecompletion :: List.map (fun (_, e, _) -> e) hoisted_args @ (Option.map_default (fun (e,_) -> [e]) [] cf_captured) in
			let tnew = mk (TNew (coro_class.ContinuationClassBuilder.cls, coro_class.outside.param_types, ctor_args)) t coro_class.name_pos in
			let invoke_resume_type = TFun([], tret_invoke_resume) in
			let einvoke = b#instance_field tnew coro_class.ContinuationClassBuilder.cls coro_class.outside.param_types invoke_resume_field invoke_resume_type in
			b#return (b#call einvoke [] tret_invoke_resume)
		| GenThunk _ ->
			(* Thunk path: build the closure that captures outer locals, allocate the continuation
			   (passing hoisted args to the constructor), then call invokeResume(). *)
			let inside_to_outside t =
				apply_params coro_class.inside.params coro_class.outside.param_types t
			in
			let econt = b#local vcontinuation coro_class.name_pos in
			let continuation_field cf ty =
				b#instance_field econt coro_class.ContinuationClassBuilder.cls coro_class.outside.param_types cf ty
			in
			let invoke_resume_type = inside_to_outside invoke_resume_field.cf_type in
			let einvoke_resume_call = b#call (continuation_field invoke_resume_field invoke_resume_type) [] tret_invoke_resume in
			let thunk_body_el =
				(if Lazy.is_val vtmp_result then [b#var_init (Lazy.force vtmp_result) eresult] else []) @ [
				b#var_init_null vtmp_error;
			] @ (if Lazy.is_val vtmp_error_unwrapped then [b#var_init_null (Lazy.force vtmp_error_unwrapped)] else [])
			@ [eloop] in
			let thunk_type = TFun([], tret_invoke_resume) in
			let ethunk = mk (TFunction { tf_type = tret_invoke_resume; tf_args = []; tf_expr = b#void_block thunk_body_el })
				thunk_type coro_class.name_pos in
			let vthunk = alloc_var VGenerated "_hx_thunk" thunk_type coro_class.name_pos in
			let ctor_args = ecompletion :: List.map (fun (_, e, _) -> e) hoisted_args @ [b#local vthunk coro_class.name_pos] in
			let tnew = mk (TNew (coro_class.ContinuationClassBuilder.cls, coro_class.outside.param_types, ctor_args)) t coro_class.name_pos in
			let null_safety_off = b#meta1 Meta.NullSafety (EConst (Ident "Off"),vcontinuation.v_pos) in
			null_safety_off
				begin b#void_block ([
					b#var_init_null vcontinuation;
					b#var_init vthunk ethunk;
					b#assign (b#local vcontinuation coro_class.name_pos) tnew;
					b#return einvoke_resume_call]) end
	end

let rewrite_super_field ctx egthis e =
	let basic = ctx.typer.t in
	let b = ctx.builder in
	let curclass = ctx.typer.c.curclass in
	let curclass_params = extract_param_types ctx.typer.type_params in
	let make_super_helper super_field_expr super_cl super_cf p =
		let helper_name = Printf.sprintf "_hx_super_%s" super_cf.cf_name in
		begin try
			PMap.find helper_name curclass.cl_fields
		with Not_found ->
			(* Helper has same Coro type as the super method, so expr_to_coro will
				recognize it as a coroutine call and add the completion arg. *)
			let helper_cf = mk_field helper_name super_cf.cf_type p p in
			helper_cf.cf_kind <- Method MethNormal;
			(* Helper body: forwards all args (including completion) to super.X().
				We build the body in expanded form (explicit completion parameter)
				so it is never run through fun_to_coro again. *)
			let body_args, body_ret = match follow_with_coro super_cf.cf_type with
				| Coro (args, ret) -> Common.expand_coro_type basic args ret
				| NotCoro _ -> die "super helper: expected Coro type" __LOC__
			in
			let param_vars = List.map (fun (n, _, t) -> alloc_var VGenerated n t p) body_args in
			let eparam_exprs = List.map (fun v -> b#local v p) param_vars in
			(* Expand the super field type so the TCall type-checks *)
			let efun_expanded = { super_field_expr with etype = TFun(body_args, body_ret) } in
			let ecall = mk (TCall(efun_expanded, eparam_exprs)) body_ret p in
			let tf_expr = mk (TReturn (Some ecall)) t_dynamic p in
			helper_cf.cf_expr <- Some (mk
				(TFunction {
					tf_args = List.map (fun v -> v, None) param_vars;
					tf_type = body_ret;
					tf_expr
				}) super_cf.cf_type p);
			TClass.add_field curclass helper_cf;
			helper_cf
		end
	in
	match e.eexpr with
	| TField({eexpr = TConst TSuper} as esuper_this, (FInstance(super_cl, _, super_cf) as fa)) ->
		let super_field_expr = { eexpr = TField(esuper_this, fa); etype = super_cf.cf_type; epos = e.epos } in
		let helper_cf = make_super_helper super_field_expr super_cl super_cf e.epos in
		{ e with eexpr = TField(egthis, FInstance(curclass, curclass_params, helper_cf)) }
	| _ ->
		e

let make_deferred_api ctx b =
	let make_deferred build t =
		let v = alloc_var VGenerated "_hx_coro_deferred" t_dynamic null_pos in
		Hashtbl.add ctx.deferred_exprs v.v_id build;
		b#call (b#local v v.v_pos) [] t
	in

	(* These refs will be filled in after the continuation API is created (step 4). *)
	let make_inline_return_impl = ref None in
	let make_inline_tail_call_impl = ref None in
	let make_this_impl = ref None in
	let make_super_field_impl = ref None in

	let make_inline_return e1_opt pos =
		make_deferred (fun () -> (Option.get !make_inline_return_impl) e1_opt pos) t_dynamic (* TODO: ? *)
	in
	let make_inline_tail_call call =
		make_deferred (fun () -> (Option.get !make_inline_tail_call_impl) call) t_dynamic (* TODO: ? *)
	in
	let make_this e =
		make_deferred (fun() -> (Option.get !make_this_impl) e) e.etype
	in
	let make_super_field e =
		make_deferred (fun() -> (Option.get !make_super_field_impl) e) e.etype
	in
	let deferred = {
		make_inline_return;
		make_inline_tail_call;
		make_this;
		make_super_field;
	} in
	let install api =
		make_inline_return_impl := Some api.make_inline_return;
		make_inline_tail_call_impl := Some api.make_inline_tail_call;
		make_this_impl := Some api.make_this;
		make_super_field_impl := Some api.make_super_field;
	in
	deferred,install

let fun_to_coro ctx coro_type =
	let basic = ctx.typer.t in
	let b = ctx.builder in

	(* 1. Setup continuation class *)

	let coro_class = ContinuationClassBuilder.create ctx coro_type in

	(* 2. Create expressions and variables that we need for expr_to_coro *)

	let vtmp_result = lazy (alloc_var VGenerated "_hx_result" (basic.tnull basic.tany) coro_class.name_pos) in
	let etmp_result = lazy (b#local (Lazy.force vtmp_result) coro_class.name_pos) in
	let vtmp_error = alloc_var VGenerated "_hx_error" (basic.tnull basic.texception) coro_class.name_pos in
	let vtmp_error_unwrapped = lazy (alloc_var VGenerated "_hx_error_unwrapped" (basic.tnull basic.tany) coro_class.name_pos) in
	let etmp_error_unwrapped = lazy (b#local (Lazy.force vtmp_error_unwrapped) coro_class.name_pos) in

	let expr, args, name =
		match coro_type with
		| ClassField (_, cf, f, _) ->
			f.tf_expr, f.tf_args, cf.cf_name
		| LocalFunc(f,v) ->
			f.tf_expr, f.tf_args, v.v_name
		in

	let cb_root = make_block ctx (Some(expr.etype, coro_class.name_pos)) in
	let scope = match args with
		| (v,_) :: _ ->
			if has_var_flag v VCoroScope then Some {
				scope_var = v;
				restricted_suspension = has_var_flag v VCoroRestrictedSuspension
			} else
				None
		| _ ->
			None
	in

	(* 3. Run expr_to_coro to build the CFG and set ctx.captures_this/ctx.has_capture_vars. *)

	let deferred,install_deferred = make_deferred_api ctx b in
	CoroFromTexpr.check_captures ctx args expr;
	ignore(CoroFromTexpr.expr_to_coro ctx etmp_result etmp_error_unwrapped cb_root scope deferred expr);

	(* Count the number of reachable CFG blocks. *)
	let count = ref 0 in
	CoroFunctions.coro_walk (fun _ -> incr count) cb_root;
	ctx.num_states <- !count;

	(* 4. Setup continuation API — now that ctx.captures_this/ctx.has_capture_vars are
	      fully set we can create the continuation variables with informed types. *)

	let cont = coro_class.continuation_api in

	(* Determine the generation mode (inline vs. thunk) now that we know whether any
	   outside variables are captured. *)
	let tret_invoke_resume_inside = cont.suspension_result coro_class.inside.result_type in
	let gen_mode = match coro_class.coro_type with
		| ClassField (_, field, _, _) when has_class_field_flag field CfStatic ->
			GenInline None
		| ClassField (_,field, _, _) ->
			if not ctx.captures_this then
				GenInline None
			else begin
				let cf = mk_field "_hx_captured" ctx.typer.c.tthis field.cf_name_pos field.cf_name_pos in
				GenInline (Some (b#this ctx.typer.c.tthis coro_class.name_pos, cf))
			end
		| LocalFunc _ when not ctx.captures_this && not ctx.has_capture_vars ->
			GenInline None
		| LocalFunc (f,v) ->
			let make_captured_field p =
				mk_field "_hx_captured" (TFun([], tret_invoke_resume_inside)) p p
			in
			GenThunk (make_captured_field v.v_pos)
	in

	let vcompletion = alloc_var VGenerated "_hx_completion" cont.continuation coro_class.name_pos in
	let ecompletion = b#local vcompletion coro_class.name_pos in

	let vcontinuation_type = match gen_mode with
		| GenInline _ -> coro_class.inside.cls_t
		| GenThunk _ -> coro_class.outside.cls_t
	in
	let vcontinuation = alloc_var VGenerated "_hx_continuation" vcontinuation_type coro_class.name_pos in
	let econtinuation = b#local vcontinuation coro_class.name_pos in

	let continuation_field c cf t =
		b#instance_field econtinuation c [basic.tany] cf t
	in

	let estate = continuation_field cont.suspension_result_class cont.state cont.suspension_state in
	let eresult = continuation_field cont.suspension_result_class cont.result (basic.tnull basic.tany) in
	let eerror = continuation_field cont.suspension_result_class cont.error (basic.tnull basic.texception) in

	let continuation_field cf t =
		b#instance_field econtinuation cont.base_continuation_class [basic.tany] cf t
	in

	let egoto  = continuation_field cont.goto_label basic.tint in
	let etmp_error = b#local vtmp_error coro_class.name_pos in
	let exprs = {CoroToTexpr.econtinuation;ecompletion;estate;eresult;egoto;eerror;etmp_result;etmp_error;etmp_error_unwrapped} in
	let stack_item_inserter pos =
		let field, eargs =
			match coro_type with
			| ClassField (cls, field, _, _) ->
				PMap.find "setClassFuncStackItem" cont.base_continuation_class.cl_fields,
				[
					b#string (s_class_path cls) coro_class.name_pos;
					b#string field.cf_name coro_class.name_pos;
				]
			| LocalFunc (_, v) ->
				PMap.find "setLocalFuncStackItem" cont.base_continuation_class.cl_fields,
				[
					b#int v.v_id coro_class.name_pos;
				]
		in
		let eaccess = continuation_field field field.cf_type in
		let l1,c1,_,_ = Lexer.get_pos_coords pos in
		let eargs   = eargs @ [
			b#string pos.pfile coro_class.name_pos;
			b#int l1 coro_class.name_pos;
			b#int c1 coro_class.name_pos;
			b#int pos.pmin coro_class.name_pos;
			b#int pos.pmax coro_class.name_pos;
		] in
		mk (TCall (eaccess, eargs)) basic.tvoid coro_class.name_pos
	in

	(* 5. Fill in the deferred callback implementations now that the continuation API exists *)

	let vgthis = lazy (alloc_var VGenerated "_hx_this" ctx.typer.c.tthis coro_class.name_pos) in

	let deferred_impl =
		let egthis = lazy (match gen_mode with
			| GenThunk _ ->
				b#local (Lazy.force vgthis) coro_class.name_pos
			| GenInline (Some (_,cf)) ->
				b#instance_field econtinuation coro_class.ContinuationClassBuilder.cls coro_class.inside.param_types cf cf.cf_type
			| GenInline None ->
				die "" __LOC__
		) in
		{
			make_inline_return = (fun e1_opt pos ->
				let stmts = if ctx.num_states = 1 then [] else [
					b#assign egoto (b#int (-1) pos);
				] in
				let stmts = stmts @ [
					b#assign estate (CoroControl.mk_control basic CoroControl.CoroReturned);
				] in
				let stmts = match e1_opt with
					| None -> stmts
					| Some e1 -> stmts @ [b#assign eresult e1]
				in
				let stmts = stmts @ [b#return econtinuation] in
				mk (TBlock stmts) t_dynamic pos
			);
			make_inline_tail_call = (fun call ->
				let (ecallcoroutine, eret) = CoroToTexpr.SuspensionCalls.make_suspending_tail_call ctx cont exprs call in
				b#void_block [stack_item_inserter call.cs_pos; ecallcoroutine; eret]
			);
			make_this = (fun e ->
				let egthis = Lazy.force egthis in
				{ e with eexpr = egthis.eexpr; etype = egthis.etype }
			);
			make_super_field = (fun e ->
				rewrite_super_field ctx (Lazy.force egthis) e;
			);
		}
	in
	install_deferred deferred_impl;

	(* 6. Transform blocks to state machine *)

	let start_exception =
		let cf = PMap.find "startException" cont.base_continuation_class.cl_fields in
		let ef = continuation_field cf cf.cf_type in
		(fun e ->
			mk (TCall(ef,[e])) basic.tvoid coro_class.name_pos
		)
	in
	let tf_expr = coro_to_state_machine ctx coro_class cb_root exprs args vtmp_result vtmp_error vtmp_error_unwrapped vcompletion vcontinuation gen_mode stack_item_inserter start_exception in

	(* For non-static ClassField: prepend  var _hx_this = this  to the thin wrapper so the
	   thunk (built inside coro_to_state_machine) can capture `_hx_this` via closure, making
	   the original class instance accessible throughout the state machine. *)
	let tf_expr = if Lazy.is_val vgthis then
		b#void_block [ b#var_init (Lazy.force vgthis) (b#this ctx.typer.c.tthis coro_class.name_pos); tf_expr ]
	else
		tf_expr
	in

	let tf_args = (vcompletion,None) :: args in
	(* I'm not sure what this should be, but let's stick to the widest one for now.
	   Cpp dies if I try to use coro_class.outside.cls_t here, which might be something
	   to investigate independently. *)
	let tf_type = cont.suspension_result coro_class.outside.result_type in
	if ctx.config.debug then begin
		print_endline ("BEFORE:\n" ^ (s_expr_debug expr));
		CoroDebug.create_dotgraph (DotGraph.get_dump_path (SafeCom.of_com ctx.typer.com) (ctx.typer.c.curclass.cl_path) name) cb_root
	end;
	let e = mk (TFunction {tf_args; tf_expr; tf_type}) (TFun (tf_args |> List.map (fun (v, _) -> (v.v_name, false, v.v_type)), tf_type)) tf_expr.epos in
	if ctx.config.debug then print_endline ("AFTER:\n" ^ (s_expr_debug e));
	e

let create_coro_context typer config =
	let builder = new CoroElsewhere.texpr_builder typer.Typecore.t in
	let ctx = {
		builder;
		typer;
		config;
		deferred_exprs = Hashtbl.create 0;
		has_capture_vars = false;
		captures_this = false;
		next_block_id = 0;
		current_catch = None;
		has_catch = false;
		num_states = 0;
	} in
	ctx
