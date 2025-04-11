open Globals
open Type
open CoroTypes
open CoroFunctions
open Texpr

let localFuncCount = ref 0

type coro_for =
	| LocalFunc of texpr
	| ClassField of tclass * tclass_field

module ContinuationClassBuilder = struct
	type coro_class = {
		cls : tclass;
		completion : tclass_field;
		context : tclass_field;
		state : tclass_field;
		result : tclass_field;
		error : tclass_field;
		coro_type : coro_for;
	}

	let mk_assign estate eid =
		mk (TBinop (OpAssign,estate,eid)) eid.etype null_pos

	let create ctx coro_type =
		(* Mangle class names to hopefully get unique names and avoid collisions *)
		let name =
			match coro_type with
			| ClassField (cls, field) ->
				Printf.sprintf "HxCoro_%s_%s_%s" (ctx.typer.m.curmod.m_path |> fst |> String.concat "_") (ctx.typer.m.curmod.m_path |> snd) field.cf_name
			| LocalFunc _ ->
				let n = Printf.sprintf "HxCoroAnonFunc_%i" !localFuncCount in
				localFuncCount := !localFuncCount + 1;
				n
			in

		(* Is there a pre-existing function somewhere to a valid path? *)
		let cls_path = ((fst ctx.typer.m.curmod.m_path) @ [ Printf.sprintf "_%s" (snd ctx.typer.m.curmod.m_path) ]), name in
		let cls      = mk_class ctx.typer.m.curmod cls_path null_pos null_pos in

		(match ctx.typer.com.basic.tcoro_continuation with
		| TInst (cls_cont, _) ->
			cls.cl_implements <- [ (cls_cont, [ ctx.typer.com.basic.tany ]) ]
		| _ ->
			die "Excepted continuation to be TInst" __LOC__);

		let cls_completion = mk_field "_hx_completion" ctx.typer.com.basic.tcoro_continuation null_pos null_pos in
		let cls_context    = mk_field "_hx_context" ctx.typer.com.basic.tcoro_context null_pos null_pos in
		let cls_state      = mk_field "_hx_state" ctx.typer.com.basic.tint null_pos null_pos in
		let cls_result     = mk_field "_hx_result" ctx.typer.com.basic.tany null_pos null_pos in
		let cls_error      = mk_field "_hx_error" ctx.typer.com.basic.texception null_pos null_pos in

		{
			cls        = cls;
			completion = cls_completion;
			context    = cls_context;
			state      = cls_state;
			result     = cls_result;
			error      = cls_error;
			coro_type  = coro_type;
		}

	let mk_ctor ctx coro_class initial_state =
		let name  = "completion" in
		let ethis = mk (TConst TThis) (TInst (coro_class.cls, [])) null_pos in

		let vargcompletion = alloc_var VGenerated name ctx.typer.com.basic.tcoro_continuation null_pos in
		(* let vargcaptured   = alloc_var VGenerated "captured" ctx.typer.c.tthis null_pos in *)

		let eassigncompletion =
			let eargcompletion    = Builder.make_local vargcompletion null_pos in
			let ecompletionfield  = mk (TField(ethis,FInstance(coro_class.cls, [], coro_class.completion))) ctx.typer.com.basic.tcoro_continuation null_pos in
			mk_assign ecompletionfield eargcompletion in

		let eassignstate =
			let estatefield = mk (TField(ethis,FInstance(coro_class.cls, [], coro_class.state))) ctx.typer.com.basic.tint null_pos in
			mk_assign estatefield (mk (TConst (TInt (Int32.of_int initial_state) )) ctx.typer.com.basic.tint null_pos) in

		(* let eassigncaptured =
			let eargcaptured    = Builder.make_local vargcaptured null_pos in
			let ecapturedfield  = mk (TField(ethis,FInstance(coro_class.cls, [], coro_class.captured))) ctx.typer.c.tthis p in
			mk_assign ecapturedfield eargcaptured in *)

		let eassigncontext =
			let eargcompletion = Builder.make_local vargcompletion null_pos in
			let econtextfield  =
				match ctx.typer.com.basic.tcoro_continuation with
				| TInst (cls, _) ->
					(* let field = PMap.find "_hx_context" cls.cl_fields in *)
					mk (TField(eargcompletion, FInstance(cls, [], coro_class.context))) coro_class.context.cf_type null_pos
				| _ ->
					die "Expected context to be TInst" __LOC__
			in

			let ecompletionfield = mk (TField(ethis,FInstance(coro_class.cls, [], coro_class.context))) ctx.typer.com.basic.tcoro_context null_pos in
			mk_assign ecompletionfield econtextfield
		in

		(* If the coroutine field is not static then our HxCoro class needs to capture this for future resuming *)

		let eblock, tfun_args, tfunction_args =
			match coro_class.coro_type with
			| ClassField (cls, field) when has_class_field_flag field CfStatic ->
				mk (TBlock [ eassigncompletion; eassignstate; eassigncontext ]) ctx.typer.com.basic.tvoid null_pos,
				[ (name, false, ctx.typer.com.basic.tcoro_continuation) ],
				[ (vargcompletion, None) ]
			| ClassField (cls, field) ->
				(* mk (TBlock [ eassigncaptured; eassigncompletion; eassignstate; eassigncontext ]) ctx.typer.com.basic.tvoid p *)
				(* [ ("captured", false, ctx.typer.c.tthis); (name, false, ctx.typer.com.basic.tcoro_continuation) ] *)
				(* [ (vargcaptured, None); (vargcompletion, None) ] *)
				die "" __LOC__
			| LocalFunc _ ->
				die "" __LOC__
		in

		let field = mk_field "new" (TFun (tfun_args, ctx.typer.com.basic.tvoid)) null_pos null_pos in
		let func  = TFunction { tf_type = ctx.typer.com.basic.tvoid; tf_args = tfunction_args; tf_expr = eblock } in
		let expr  = mk func field.cf_type null_pos in

		if ctx.coro_debug then
			s_expr_debug expr |> Printf.printf "%s\n";

		{ field with cf_kind = Method MethNormal; cf_expr = Some expr }

	let mk_resume ctx coro_class =
		let result_name = "result" in
		let error_name  = "error" in
		let field       = mk_field "resume" (TFun ([ (result_name, false, ctx.typer.com.basic.tany); (error_name, false, ctx.typer.com.basic.texception) ], ctx.typer.com.basic.tvoid)) null_pos null_pos in
		let vargresult  = alloc_var VGenerated result_name ctx.typer.com.basic.tany null_pos in
		let vargerror   = alloc_var VGenerated error_name ctx.typer.com.basic.texception null_pos in
		let eargresult  = Builder.make_local vargresult null_pos in
		let eargerror   = Builder.make_local vargerror null_pos in
		let ethis       = mk (TConst TThis) (TInst (coro_class.cls, [])) null_pos in

		(* Create a custom this variable to be captured, should the compiler already handle this? *)
		let vfakethis = alloc_var VGenerated "fakethis" (TInst (coro_class.cls, [])) null_pos in
		let evarfakethis = mk (TVar (vfakethis, Some ethis)) (TInst (coro_class.cls, [])) null_pos in

		(* Assign result and error *)
		let eresultfield  = mk (TField(ethis,FInstance(coro_class.cls, [], coro_class.result))) ctx.typer.com.basic.tany null_pos in
		let eerrorfield   = mk (TField(ethis,FInstance(coro_class.cls, [], coro_class.error))) ctx.typer.com.basic.texception null_pos in
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
				| ClassField (cls, ({ cf_expr = Some ({ eexpr = TFunction f }) } as field)) when has_class_field_flag field CfStatic ->
					let args      = (f.tf_args |> List.map (fun (v, _) -> Texpr.Builder.default_value v.v_type null_pos)) @ [ ethis ] in
					let efunction = Builder.make_static_field cls field null_pos in
					mk (TCall (efunction, args)) ctx.typer.com.basic.tany null_pos
				| _ ->
					die "" __LOC__
				(* let args = (tf_args |> List.map (fun (v, _) -> Texpr.Builder.default_value v.v_type v.v_pos)) @ [ ethis ] in

				if has_class_field_flag ctx.typer.f.curfield CfStatic then
					let efunction = Builder.make_static_field ctx.typer.c.curclass ctx.typer.f.curfield p in
					mk (TCall (efunction, args)) ctx.typer.com.basic.tany p
				else
					let ecapturedfield = mk (TField(ethis,FInstance(coro_class.cls, [], coro_class.captured))) ctx.typer.c.tthis p in
					let efunction      = mk (TField(ecapturedfield,FInstance(coro_class.cls, [], ctx.typer.f.curfield))) tf_return p in

					mk (TCall (efunction, args)) ctx.typer.com.basic.tany p *)
				in
			let vresult    = alloc_var VGenerated "result" ctx.typer.com.basic.tany null_pos in
			let evarresult = mk (TVar (vresult, (Some ecorocall))) ctx.typer.com.basic.tany null_pos in
			let eresult    = Builder.make_local vresult null_pos in
			let tcond      = std_is eresult ctx.typer.com.basic.tcoro_primitive in
			let tif        = mk (TReturn None) ctx.typer.com.basic.tany null_pos in
			let telse      = mk (TCall (eresumefield, [ eresult; Builder.make_null ctx.typer.com.basic.texception null_pos ])) ctx.typer.com.basic.tvoid null_pos in

			let etryblock =
				mk (TBlock [
					evarresult;
					mk (TIf (tcond, tif, Some telse)) ctx.typer.com.basic.tvoid null_pos
				]) ctx.typer.com.basic.tvoid null_pos
			in

			let vcatch = alloc_var VGenerated "exn" ctx.typer.com.basic.texception null_pos in
			let ecatch = Builder.make_local vcatch null_pos in
			let ecatchblock =
				vcatch,
				mk (TCall (eresumefield, [ Builder.make_null ctx.typer.com.basic.texception null_pos; ecatch ])) ctx.typer.com.basic.tvoid null_pos
			in

			mk (TTry (etryblock, [ ecatchblock ])) ctx.typer.com.basic.tvoid null_pos
		in

		(* if ctx.coro_debug then
			s_expr_debug try_block |> Printf.printf "%s\n"; *)

		(* Bounce our continuation through the scheduler *)
		let econtextfield   = mk (TField(ethis, FInstance(coro_class.cls, [], coro_class.context))) ctx.typer.com.basic.tany null_pos in
		let eschedulerfield =
			match ctx.typer.com.basic.tcoro_context with
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
				(TFunction { tf_expr = try_block; tf_type = ctx.typer.com.basic.tvoid; tf_args = [] })
				(TFun ([], ctx.typer.com.basic.tvoid))
				null_pos in

		let eschedulecall =
			mk (TCall (eschedulefield, [ lambda ])) ctx.typer.com.basic.tvoid null_pos
		in

		let block = mk (TBlock [ evarfakethis; eassignresult; eassignerror; eschedulecall ]) ctx.typer.com.basic.tvoid null_pos in
		let func  = TFunction { tf_type = ctx.typer.com.basic.tvoid; tf_args = [ (vargresult, None); (vargerror, None) ]; tf_expr = block } in
		let expr  = mk (func) ctx.typer.com.basic.tvoid null_pos in

		if ctx.coro_debug then
			s_expr_debug expr |> Printf.printf "%s\n";

		{ field with cf_kind = Method MethNormal; cf_expr = Some expr }
end

let fun_to_coro ctx coro_type =

	let p, name, e, tf_args, tf_return, tf_expr =
		match coro_type with
		| ClassField (cls, ({ cf_expr = Some ({ eexpr = (TFunction f) } as e) } as field)) ->
			field.cf_pos,
			Printf.sprintf "HxCoro_%s_%s_%s" (ctx.typer.m.curmod.m_path |> fst |> String.concat "_") (ctx.typer.m.curmod.m_path |> snd) field.cf_name,
			e,
			f.tf_args,
			f.tf_type,
			f.tf_expr
		| ClassField (_, field) ->
			die (Printer.s_tclass_field "\t" field) __LOC__
		| LocalFunc e ->
			die (s_expr_debug e) __LOC__ in

	let mk_assign estate eid =
		mk (TBinop (OpAssign,estate,eid)) eid.etype null_pos
	in

	(* if ctx.coro_debug then (
		Printf.printf "%s\n" name;
		Printf.printf "type - %s\n" (s_type_kind (follow tf.tf_type));
		Printf.printf "args - %s\n" (tf.tf_args |> List.map (fun (v, _) -> s_type_kind ((follow v.v_type))) |> String.concat ", ")); *)

	let coro_class = ContinuationClassBuilder.create ctx coro_type in

	(* Generate and assign the continuation variable *)
	let vcompletion = alloc_var VGenerated "_hx_completion" ctx.typer.com.basic.tcoro_continuation p in
	let ecompletion = Builder.make_local vcompletion p in

	let vcontinuation = alloc_var VGenerated "_hx_continuation" (TInst (coro_class.cls, [])) p in
	let econtinuation = Builder.make_local vcontinuation p in

	let estate = mk (TField(econtinuation,FInstance(coro_class.cls, [], coro_class.state))) ctx.typer.com.basic.tint p in
	let eresult = mk (TField(econtinuation,FInstance(coro_class.cls, [], coro_class.result))) ctx.typer.com.basic.tint p in

	let cb_root = make_block ctx (Some(e.etype,p)) in

	ignore(CoroFromTexpr.expr_to_coro ctx eresult cb_root tf_expr);
	let eloop, initial_state, fields = CoroToTexpr.block_to_texpr_coroutine ctx cb_root coro_class.cls tf_args [ vcompletion.v_id; vcontinuation.v_id ] econtinuation ecompletion eresult estate p in
	let ctor   = ContinuationClassBuilder.mk_ctor ctx coro_class initial_state in
	let resume = ContinuationClassBuilder.mk_resume ctx coro_class in

	TClass.add_field coro_class.cls coro_class.completion;
	TClass.add_field coro_class.cls coro_class.context;
	TClass.add_field coro_class.cls coro_class.state;
	TClass.add_field coro_class.cls coro_class.result;
	TClass.add_field coro_class.cls coro_class.error;
	TClass.add_field coro_class.cls resume;
	(* if not (has_class_field_flag ctx.typer.f.curfield CfStatic) then
		TClass.add_field cls cls_captured; *)
	List.iter (TClass.add_field coro_class.cls) fields;

	coro_class.cls.cl_constructor <- Some ctor;

	if ctx.coro_debug then
		Printer.s_tclass "\t" coro_class.cls |> Printf.printf "%s\n";

	ctx.typer.m.curmod.m_types <- ctx.typer.m.curmod.m_types @ [ TClassDecl coro_class.cls ];

	let continuation_var = mk (TVar (vcontinuation, Some (Builder.make_null (TInst (coro_class.cls, [])) p))) (TInst (coro_class.cls, [])) p in

	let std_is e t =
		let type_expr = mk (TTypeExpr (module_type_of_type t)) t_dynamic null_pos in
		Texpr.Builder.resolve_and_make_static_call ctx.typer.com.std "isOfType" [e;type_expr] null_pos
	in

	let continuation_assign =
		let t         = TInst (coro_class.cls, []) in
		let tcond     = std_is ecompletion t in
		let tif       = mk_assign econtinuation (mk_cast ecompletion t p) in
		let ctor_args =
			if has_class_field_flag ctx.typer.f.curfield CfStatic then
				[ ecompletion ]
			else
				[ mk (TConst TThis) ctx.typer.c.tthis p; ecompletion ]
		in
		let telse = mk_assign econtinuation (mk (TNew (coro_class.cls, [], ctor_args)) t p) in
		mk (TIf (tcond, tif, Some telse)) ctx.typer.com.basic.tvoid p
	in

	let tf_expr = mk (TBlock [
		continuation_var;
		continuation_assign;
		eloop;
		Builder.mk_return (Builder.make_null ctx.typer.com.basic.tany p);
	]) ctx.typer.com.basic.tvoid p in

	let tf_args = tf_args @ [(vcompletion,None)] in
	let tf_type = ctx.typer.com.basic.tany in
	if ctx.coro_debug then begin
		print_endline ("BEFORE:\n" ^ (s_expr_debug e));
		(* CoroDebug.create_dotgraph (DotGraph.get_dump_path ctx.typer.com ([],e.epos.pfile) (Printf.sprintf "pos_%i" e.epos.pmin)) cb_root *)
	end;
	let e = { e with eexpr = TFunction {tf_args; tf_expr; tf_type}; etype = TFun (tf_args |> List.map (fun (v, _) -> (v.v_name, false, v.v_type)), ctx.typer.com.basic.tany) } in
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
	} in
	ctx.cb_unreachable <- make_block ctx None;
	ctx