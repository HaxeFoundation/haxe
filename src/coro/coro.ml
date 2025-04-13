open Globals
open Type
open CoroTypes
open CoroFunctions
open Texpr

let localFuncCount = ref 0

type coro_for =
	| LocalFunc of tfunc
	| ClassField of tclass * tclass_field * tfunc * pos (* expr pos *)

module ContinuationClassBuilder = struct
	type coro_class = {
		cls : tclass;
		coro_type : coro_for;
		completion : tclass_field;
		context : tclass_field;
		state : tclass_field;
		result : tclass_field;
		error : tclass_field;
		(* Some coroutine classes (member functions, local functions) need to capture state, this field stores that *)
		captured : tclass_field option;
	}

	let mk_assign estate eid =
		mk (TBinop (OpAssign,estate,eid)) eid.etype null_pos

	let create ctx coro_type =
		let basic = ctx.typer.t in
		(* Mangle class names to hopefully get unique names and avoid collisions *)
		let name, cls_captured =
			let captured_field_name = "_hx_captured" in
			match coro_type with
			| ClassField (cls, field, _, _) ->
				Printf.sprintf "HxCoro_%s_%s_%s" (ctx.typer.m.curmod.m_path |> fst |> String.concat "_") (ctx.typer.m.curmod.m_path |> snd) field.cf_name,
				if has_class_field_flag field CfStatic then
					None
				else
					Some (mk_field captured_field_name ctx.typer.c.tthis null_pos null_pos)
			| LocalFunc f ->
				let n = Printf.sprintf "HxCoroAnonFunc_%i" !localFuncCount in
				localFuncCount := !localFuncCount + 1;

				let t = TFun ([ ("_hx_continuation", false, basic.tcoro_continuation) ], basic.tany) in
				n, Some (mk_field captured_field_name t null_pos null_pos)
			in

		(* Is there a pre-existing function somewhere to a valid path? *)
		let cls_path = ((fst ctx.typer.m.curmod.m_path) @ [ Printf.sprintf "_%s" (snd ctx.typer.m.curmod.m_path) ]), name in
		let cls      = mk_class ctx.typer.m.curmod cls_path null_pos null_pos in

		(match basic.tcoro_continuation with
		| TInst (cls_cont, _) ->
			cls.cl_implements <- [ (cls_cont, [ basic.tany ]) ]
		| _ ->
			die "Excepted continuation to be TInst" __LOC__);

		let cls_completion = mk_field "_hx_completion" basic.tcoro_continuation null_pos null_pos in
		let cls_context    = mk_field "_hx_context" basic.tcoro_context null_pos null_pos in
		let cls_state      = mk_field "_hx_state" basic.tint null_pos null_pos in
		let cls_result     = mk_field "_hx_result" basic.tany null_pos null_pos in
		let cls_error      = mk_field "_hx_error" basic.texception null_pos null_pos in

		{
			cls        = cls;
			coro_type  = coro_type;
			completion = cls_completion;
			context    = cls_context;
			state      = cls_state;
			result     = cls_result;
			error      = cls_error;
			captured   = cls_captured;
		}

	let mk_ctor ctx coro_class initial_state =
		let basic = ctx.typer.t in
		let name  = "completion" in
		let ethis = mk (TConst TThis) (TInst (coro_class.cls, [])) null_pos in

		let vargcompletion = alloc_var VGenerated name basic.tcoro_continuation null_pos in
		(* let vargcaptured   = alloc_var VGenerated "captured" ctx.typer.c.tthis null_pos in *)

		let eassigncompletion =
			let eargcompletion    = Builder.make_local vargcompletion null_pos in
			let ecompletionfield  = mk (TField(ethis,FInstance(coro_class.cls, [], coro_class.completion))) basic.tcoro_continuation null_pos in
			mk_assign ecompletionfield eargcompletion in

		let eassignstate =
			let estatefield = mk (TField(ethis,FInstance(coro_class.cls, [], coro_class.state))) basic.tint null_pos in
			mk_assign estatefield (mk (TConst (TInt (Int32.of_int initial_state) )) basic.tint null_pos) in

		(* let eassigncaptured =
			let eargcaptured    = Builder.make_local vargcaptured null_pos in
			let ecapturedfield  = mk (TField(ethis,FInstance(coro_class.cls, [], coro_class.captured))) ctx.typer.c.tthis p in
			mk_assign ecapturedfield eargcaptured in *)
		let captured =
			coro_class.captured
			|> Option.map
				(fun field ->
					let vargcaptured    = alloc_var VGenerated "captured" field.cf_type null_pos in
					let eargcaptured    = Builder.make_local vargcaptured null_pos in
					let ecapturedfield  = mk (TField(ethis,FInstance(coro_class.cls, [], field))) field.cf_type null_pos in
					vargcaptured, mk_assign ecapturedfield eargcaptured)
			in

		let eassigncontext =
			let eargcompletion = Builder.make_local vargcompletion null_pos in
			let econtextfield  =
				match basic.tcoro_continuation with
				| TInst (cls, _) ->
					(* let field = PMap.find "_hx_context" cls.cl_fields in *)
					mk (TField(eargcompletion, FInstance(cls, [], coro_class.context))) coro_class.context.cf_type null_pos
				| _ ->
					die "Expected context to be TInst" __LOC__
			in

			let ecompletionfield = mk (TField(ethis,FInstance(coro_class.cls, [], coro_class.context))) basic.tcoro_context null_pos in
			mk_assign ecompletionfield econtextfield
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

			mk (TBlock (extra_exprs @ [ eassigncompletion; eassignstate; eassigncontext ])) basic.tvoid null_pos,
			extra_tfun_args @ [ (name, false, basic.tcoro_continuation) ],
			extra_tfunction_args @ [ (vargcompletion, None) ]
		in

		let field = mk_field "new" (TFun (tfun_args, basic.tvoid)) null_pos null_pos in
		let func  = TFunction { tf_type = basic.tvoid; tf_args = tfunction_args; tf_expr = eblock } in
		let expr  = mk func field.cf_type null_pos in

		if ctx.coro_debug then
			s_expr_debug expr |> Printf.printf "%s\n";

		{ field with cf_kind = Method MethNormal; cf_expr = Some expr }

	let mk_resume ctx coro_class =
		let basic = ctx.typer.t in
		let result_name = "result" in
		let error_name  = "error" in
		let field       = mk_field "resume" (TFun ([ (result_name, false, basic.tany); (error_name, false, basic.texception) ], basic.tvoid)) null_pos null_pos in
		let vargresult  = alloc_var VGenerated result_name basic.tany null_pos in
		let vargerror   = alloc_var VGenerated error_name basic.texception null_pos in
		let eargresult  = Builder.make_local vargresult null_pos in
		let eargerror   = Builder.make_local vargerror null_pos in
		let ethis       = mk (TConst TThis) (TInst (coro_class.cls, [])) null_pos in

		(* Create a custom this variable to be captured, should the compiler already handle this? *)
		let vfakethis    = alloc_var VGenerated "fakethis" (TInst (coro_class.cls, [])) null_pos in
		let evarfakethis = mk (TVar (vfakethis, Some ethis)) (TInst (coro_class.cls, [])) null_pos in

		(* Assign result and error *)
		let eresultfield  = mk (TField(ethis,FInstance(coro_class.cls, [], coro_class.result))) basic.tany null_pos in
		let eerrorfield   = mk (TField(ethis,FInstance(coro_class.cls, [], coro_class.error))) basic.texception null_pos in
		let eassignresult = mk_assign eresultfield eargresult in
		let eassignerror  = mk_assign eerrorfield eargerror in

		(* Setup the continuation call *)

		let std_is e t =
			let type_expr = mk (TTypeExpr (module_type_of_type t)) t_dynamic null_pos in
			Texpr.Builder.resolve_and_make_static_call ctx.typer.com.std "isOfType" [e;type_expr] null_pos
		in

		let try_block =
			let ethis        = Builder.make_local vfakethis null_pos in
			let eresumefield =
				let ecompletionfield = mk (TField(ethis,FInstance(coro_class.cls, [], coro_class.completion))) coro_class.completion.cf_type null_pos in
				let completion, resultfield =
					match coro_class.completion.cf_type with
					| TInst (completion, _) -> completion, PMap.find "resume" completion.cl_fields
					| _ -> die "Expected scheduler to be TInst" __LOC__
				in
				mk (TField(ecompletionfield,FInstance(completion, [], resultfield))) resultfield.cf_type null_pos
			in
			let ecorocall =
				match coro_class.coro_type with
				| ClassField (cls, field, f, _) when has_class_field_flag field CfStatic ->
					let args      = (f.tf_args |> List.map (fun (v, _) -> Texpr.Builder.default_value v.v_type null_pos)) @ [ ethis ] in
					let efunction = Builder.make_static_field cls field null_pos in
					mk (TCall (efunction, args)) basic.tany null_pos
				| ClassField (cls, field,f, _) ->
					let args      = (f.tf_args |> List.map (fun (v, _) -> Texpr.Builder.default_value v.v_type null_pos)) @ [ ethis ] in
					let captured  = coro_class.captured |> Option.get in
					let ecapturedfield = mk (TField(ethis,FInstance(coro_class.cls, [], captured))) captured.cf_type null_pos in
					let efunction      = mk (TField(ecapturedfield,FInstance(cls, [], field))) field.cf_type null_pos in
					mk (TCall (efunction, args)) basic.tany null_pos
				| LocalFunc f ->
					let args      = [ ethis ] in
					let captured  = coro_class.captured |> Option.get in
					let ecapturedfield = mk (TField(ethis,FInstance(coro_class.cls, [], captured))) captured.cf_type null_pos in
					mk (TCall (ecapturedfield, args)) basic.tany null_pos
				in
			let vresult    = alloc_var VGenerated "result" basic.tany null_pos in
			let evarresult = mk (TVar (vresult, (Some ecorocall))) basic.tvoid null_pos in
			let eresult    = Builder.make_local vresult null_pos in
			let tcond      = std_is eresult basic.tcoro_primitive in
			let tif        = mk (TReturn None) t_dynamic null_pos in
			let telse      = mk (TCall (eresumefield, [ eresult; Builder.make_null basic.texception null_pos ])) basic.tvoid null_pos in

			let etryblock =
				mk (TBlock [
					evarresult;
					mk (TIf (tcond, tif, Some telse)) basic.tvoid null_pos
				]) basic.tvoid null_pos
			in

			let vcatch = alloc_var VGenerated "exn" basic.texception null_pos in
			let ecatch = Builder.make_local vcatch null_pos in
			let ecatchblock =
				vcatch,
				mk (TCall (eresumefield, [ Builder.make_null basic.texception null_pos; ecatch ])) basic.tvoid null_pos
			in

			mk (TTry (etryblock, [ ecatchblock ])) basic.tvoid null_pos
		in

		(* if ctx.coro_debug then
			s_expr_debug try_block |> Printf.printf "%s\n"; *)

		(* Bounce our continuation through the scheduler *)
		let econtextfield   = mk (TField(ethis, FInstance(coro_class.cls, [], coro_class.context))) basic.tany null_pos in
		let eschedulerfield =
			match basic.tcoro_context with
			| TInst (cls, _) ->
				let field = PMap.find "scheduler" cls.cl_fields in
				mk (TField(econtextfield, FInstance(cls, [], field))) field.cf_type null_pos
			| _ ->
				die "Expected context to be TInst" __LOC__
		in
		let eschedulefield =
			match eschedulerfield.etype with
			| TInst (cls, _) ->
				let field = PMap.find "schedule" cls.cl_fields in
				mk (TField(eschedulerfield, FInstance(cls, [], field))) field.cf_type null_pos
			| _ ->
				die "Expected scheduler to be TInst" __LOC__
		in
		let lambda =
			mk
				(TFunction { tf_expr = try_block; tf_type = basic.tvoid; tf_args = [] })
				(TFun ([], basic.tvoid))
				null_pos in

		let eschedulecall =
			mk (TCall (eschedulefield, [ lambda ])) basic.tvoid null_pos
		in

		let block = mk (TBlock [ evarfakethis; eassignresult; eassignerror; eschedulecall ]) basic.tvoid null_pos in
		let func  = TFunction { tf_type = basic.tvoid; tf_args = [ (vargresult, None); (vargerror, None) ]; tf_expr = block } in
		let expr  = mk (func) basic.tvoid null_pos in

		if ctx.coro_debug then
			s_expr_debug expr |> Printf.printf "%s\n";

		{ field with cf_kind = Method MethNormal; cf_expr = Some expr }
end

let fun_to_coro ctx coro_type =
	let basic = ctx.typer.t in

	let mk_assign estate eid =
		mk (TBinop (OpAssign,estate,eid)) eid.etype null_pos
	in

	let coro_class = ContinuationClassBuilder.create ctx coro_type in

	(* Generate and assign the continuation variable *)
	let vcompletion = alloc_var VGenerated "_hx_completion" basic.tcoro_continuation null_pos in
	let ecompletion = Builder.make_local vcompletion null_pos in

	let vcontinuation = alloc_var VGenerated "_hx_continuation" (TInst (coro_class.cls, [])) null_pos in
	let econtinuation = Builder.make_local vcontinuation null_pos in

	let estate  = mk (TField(econtinuation,FInstance(coro_class.cls, [], coro_class.state))) basic.tint null_pos in
	let eresult = mk (TField(econtinuation,FInstance(coro_class.cls, [], coro_class.result))) basic.tany null_pos in
	let eerror = mk (TField(econtinuation,FInstance(coro_class.cls, [], coro_class.error))) basic.texception null_pos in

	let expr, args, pe =
		match coro_type with
		| ClassField (_, cf, f, p) ->
			f.tf_expr, f.tf_args, p
		| LocalFunc f ->
			f.tf_expr, f.tf_args, f.tf_expr.epos
		in

	let cb_root = make_block ctx (Some(expr.etype, null_pos)) in

	ignore(CoroFromTexpr.expr_to_coro ctx eresult cb_root expr);
	let eloop, eif_error, initial_state, fields = CoroToTexpr.block_to_texpr_coroutine ctx cb_root coro_class.cls args [ vcompletion.v_id; vcontinuation.v_id ] econtinuation ecompletion eresult estate eerror null_pos in
	let ctor   = ContinuationClassBuilder.mk_ctor ctx coro_class initial_state in
	let resume = ContinuationClassBuilder.mk_resume ctx coro_class in

	TClass.add_field coro_class.cls coro_class.completion;
	TClass.add_field coro_class.cls coro_class.context;
	TClass.add_field coro_class.cls coro_class.state;
	TClass.add_field coro_class.cls coro_class.result;
	TClass.add_field coro_class.cls coro_class.error;
	TClass.add_field coro_class.cls resume;
	Option.may (TClass.add_field coro_class.cls) coro_class.captured;
	List.iter (TClass.add_field coro_class.cls) fields;

	coro_class.cls.cl_constructor <- Some ctor;

	if ctx.coro_debug then
		Printer.s_tclass "\t" coro_class.cls |> Printf.printf "%s\n";

	ctx.typer.m.curmod.m_types <- ctx.typer.m.curmod.m_types @ [ TClassDecl coro_class.cls ];

	let continuation_var = mk (TVar (vcontinuation, Some (Builder.make_null (TInst (coro_class.cls, [])) null_pos))) (TInst (coro_class.cls, [])) null_pos in

	let std_is e t =
		let type_expr = mk (TTypeExpr (module_type_of_type t)) t_dynamic null_pos in
		Texpr.Builder.resolve_and_make_static_call ctx.typer.com.std "isOfType" [e;type_expr] null_pos
	in

	let prefix_arg, mapper, vcompletion =
		match coro_class.coro_type with
		| ClassField (_, field, _, _) when has_class_field_flag field CfStatic ->
			[], (fun e -> e), vcompletion
		| ClassField _ ->
			[ mk (TConst TThis) ctx.typer.c.tthis null_pos; ], (fun e -> e), vcompletion
		| LocalFunc f ->
			let vnewcompletion = alloc_var VGenerated "_hx_completion_outer" basic.tcoro_continuation null_pos in
			let enewcompletion = Builder.make_local vnewcompletion null_pos in

			let tf             = TFun ([ (vcompletion.v_name, false, vcompletion.v_type) ], basic.tany) in
			let vcorofunc      = alloc_var VGenerated "_hx_coro_func" (basic.tarray tf) null_pos in
			let ecorofunclocal = Builder.make_local vcorofunc null_pos in
			let eindex         = mk (TArray (ecorofunclocal, Builder.make_int basic 0 null_pos)) tf null_pos in

			[ eindex ],
			(fun e ->
				let null_init = mk (TArrayDecl [ Builder.make_null tf null_pos ]) vcorofunc.v_type null_pos in
				let evar      = mk (TVar (vcorofunc, Some null_init)) vcorofunc.v_type null_pos in
				let efunc     = mk (TFunction { tf_args = [ (vcompletion, None) ]; tf_type = basic.tany; tf_expr = e }) tf null_pos in
				let eassign   = mk_assign eindex efunc in

				let ecall   = mk (TCall (eindex, [ enewcompletion ])) basic.tany null_pos in
				let ereturn = Builder.mk_return ecall in
				mk (TBlock [
					evar;
					eassign;
					ereturn;
				]) basic.tvoid null_pos),
			vnewcompletion
	in

	let continuation_assign =
		let t         = TInst (coro_class.cls, []) in
		let tcond     = std_is ecompletion t in
		let tif       = mk_assign econtinuation (mk_cast ecompletion t null_pos) in
		let tif       = mk (TBlock [
			tif;
			eif_error;
		]) basic.tvoid null_pos in
		let ctor_args = prefix_arg @ [ ecompletion ] in
		let telse = mk_assign econtinuation (mk (TNew (coro_class.cls, [], ctor_args)) t null_pos) in
		mk (TIf (tcond, tif, Some telse)) basic.tvoid null_pos
	in

	let tf_expr = mk (TBlock [
		continuation_var;
		continuation_assign;
		eloop;
		Builder.mk_return (Builder.make_null basic.tany null_pos);
	]) basic.tvoid null_pos |> mapper in

	let tf_args = args @ [ (vcompletion,None) ] in
	let tf_type = basic.tany in
	if ctx.coro_debug then begin
		print_endline ("BEFORE:\n" ^ (s_expr_debug expr));
		CoroDebug.create_dotgraph (DotGraph.get_dump_path (SafeCom.of_com ctx.typer.com) (* TODO: stupid *) ([],pe.pfile) (Printf.sprintf "pos_%i" pe.pmin)) cb_root
	end;
	let e = mk (TFunction {tf_args; tf_expr; tf_type}) (TFun (tf_args |> List.map (fun (v, _) -> (v.v_name, false, v.v_type)), basic.tany)) pe in
	if ctx.coro_debug then print_endline ("AFTER:\n" ^ (s_expr_debug e));
	e

let create_coro_context typer meta =
	let ctx = {
		typer;
		coro_debug = Meta.has (Meta.Custom ":coroutine.debug") meta;
		vthis = None;
		next_block_id = 0;
		cb_unreachable = Obj.magic "";
		current_catch = None;
		has_catch = false;
	} in
	ctx.cb_unreachable <- make_block ctx None;
	ctx