open Globals
open Type
open CoroTypes
open CoroFunctions
open Texpr

let localFuncCount = ref 0

let fun_to_coro ctx e tf name =

	let p = e.epos in

	let mk_assign estate eid =
		mk (TBinop (OpAssign,estate,eid)) eid.etype null_pos
	in

	let std_is e t =
		let type_expr = mk (TTypeExpr (module_type_of_type t)) t_dynamic null_pos in
		Texpr.Builder.resolve_and_make_static_call ctx.typer.com.std "isOfType" [e;type_expr] p
	in

	(* Create the functions IContinuation implementation class *)
	let name = match name with
		| Some n ->
			Printf.sprintf "HxCoro_%s_%s_%s" (ctx.typer.m.curmod.m_path |> fst |> String.concat "_") (ctx.typer.m.curmod.m_path |> snd) n
		| _ ->
			let v = Printf.sprintf "HxCoro_AnonFunc%i" !localFuncCount in
			localFuncCount := !localFuncCount + 1;
			v
	in

	let cls_path = ((fst ctx.typer.m.curmod.m_path) @ [ Printf.sprintf "_%s" (snd ctx.typer.m.curmod.m_path) ]), name in
	let cls = mk_class ctx.typer.m.curmod cls_path null_pos null_pos in

	(match ctx.typer.com.basic.tcoro_continuation with
	| TInst (cls_cont, _) ->
		cls.cl_implements <- [ (cls_cont, [ ctx.typer.com.basic.tany ]) ]
	| _ ->
		die "Excepted continuation to be TInst" __LOC__);

	let cls_completion = mk_field "_hx_completion" ctx.typer.com.basic.tcoro_continuation null_pos null_pos in
	let cls_state      = mk_field "_hx_state" ctx.typer.com.basic.tint null_pos null_pos in
	let cls_result     = mk_field "_hx_result" ctx.typer.com.basic.tany null_pos null_pos in
	let cls_error      = mk_field "_hx_error" ctx.typer.com.basic.texception null_pos null_pos in

	let cls_ctor =
		let name              = "completion" in
		let field             = mk_field "new" (TFun ([ (name, false, ctx.typer.com.basic.tcoro_continuation) ], ctx.typer.com.basic.tvoid)) null_pos null_pos in
		let vargcompletion    = alloc_var VGenerated name ctx.typer.com.basic.tcoro_continuation p in
		let eargcompletion    = Builder.make_local vargcompletion p in
		let ethis             = mk (TConst TThis) (TInst (cls, [])) p in
		let ecompletionfield  = mk (TField(ethis,FInstance(cls, [], cls_completion))) ctx.typer.com.basic.tint p in
		let estatefield       = mk (TField(ethis,FInstance(cls, [], cls_state))) ctx.typer.com.basic.tint p in
		let eassigncompletion = mk_assign ecompletionfield eargcompletion in
		let eassignstate      = mk_assign estatefield (mk (TConst (TInt (Int32.of_int 1) )) ctx.typer.com.basic.tint p) in
		let eblock            = mk (TBlock [ eassigncompletion; eassignstate ]) ctx.typer.com.basic.tvoid p in

		let func = TFunction { tf_type = ctx.typer.com.basic.tvoid; tf_args = [ (vargcompletion, None) ]; tf_expr = eblock } in
		let expr = mk (func) field.cf_type p in

		if ctx.coro_debug then
			s_expr_debug expr |> Printf.printf "%s\n";

		{ field with cf_kind = Method MethNormal; cf_expr = Some expr }
	in

	let cls_resume =
		let result_name = "result" in
		let error_name  = "error" in
		let field       = mk_field "resume" (TFun ([ (result_name, false, ctx.typer.com.basic.tany); (error_name, false, ctx.typer.com.basic.texception) ], ctx.typer.com.basic.tvoid)) null_pos null_pos in
		let vargresult  = alloc_var VGenerated result_name ctx.typer.com.basic.tany p in
		let vargerror   = alloc_var VGenerated error_name ctx.typer.com.basic.texception p in
		let eargresult  = Builder.make_local vargresult p in
		let eargerror   = Builder.make_local vargerror p in
		let ethis       = mk (TConst TThis) (TInst (cls, [])) p in

		(* Assign result and error *)
		let eresultfield  = mk (TField(ethis,FInstance(cls, [], cls_result))) ctx.typer.com.basic.tany p in
		let eerrorfield   = mk (TField(ethis,FInstance(cls, [], cls_error))) ctx.typer.com.basic.texception p in
		let eassignresult = mk_assign eresultfield eargresult in
		let eassignerror  = mk_assign eerrorfield eargerror in

		let block = mk (TBlock [ eassignresult; eassignerror; ]) ctx.typer.com.basic.tvoid p in
		let func  = TFunction { tf_type = ctx.typer.com.basic.tvoid; tf_args = [ (vargresult, None); (vargerror, None) ]; tf_expr = block } in
		let expr  = mk (func) ctx.typer.com.basic.tvoid p in		

		if ctx.coro_debug then
			s_expr_debug expr |> Printf.printf "%s\n";

		{ field with cf_kind = Method MethNormal; cf_expr = Some expr }
	in

	TClass.add_field cls cls_completion;
	TClass.add_field cls cls_state;
	TClass.add_field cls cls_result;
	TClass.add_field cls cls_error;
	TClass.add_field cls cls_resume;

	cls.cl_constructor <- Some cls_ctor;

	if ctx.coro_debug then
		Printer.s_tclass "\t" cls |> Printf.printf "%s\n";

	(* ctx.typer.com.types <- ctx.typer.com.types @ [ TClassDecl cls ]; *)
	ctx.typer.m.curmod.m_types <- ctx.typer.m.curmod.m_types @ [ TClassDecl cls ];

	(* Generate and assign the continuation variable *)
	let vcompletion = alloc_var VGenerated "_hx_completion" ctx.typer.com.basic.tcoro_continuation p in
	let ecompletion = Builder.make_local vcompletion p in

	let vcontinuation = alloc_var VGenerated "_hx_continuation" ctx.typer.com.basic.tcoro_continuation p in
	let econtinuation = Builder.make_local vcontinuation p in

	let estate = mk (TField(econtinuation,FInstance(cls, [], cls_state))) ctx.typer.com.basic.tint p in
	let eresult = mk (TField(econtinuation,FInstance(cls, [], cls_result))) ctx.typer.com.basic.tint p in

	let continuation_var = mk (TVar (vcontinuation, Some (Builder.make_null (TInst (cls, [])) p))) ctx.typer.com.basic.tvoid p in
	
	let cb_root = make_block ctx (Some(e.etype,p)) in
	ignore(CoroFromTexpr.expr_to_coro ctx eresult cb_root tf.tf_expr);
	
	let continuation_assign =
		let t     = TInst (cls, []) in
		let tcond = std_is econtinuation t in
		let tif   = mk_assign econtinuation (mk_cast ecompletion t p) in
		let telse = mk_assign econtinuation (mk (TNew (cls, [], [ econtinuation ])) t p) in
		mk (TIf (tcond, tif, Some telse)) ctx.typer.com.basic.tvoid p
	in
	
	let eloop   = CoroToTexpr.block_to_texpr_coroutine ctx cb_root econtinuation eresult estate e.epos in
	let tf_expr = mk (TBlock [
		continuation_var;
		continuation_assign;
		eloop;
		Builder.mk_return (Builder.make_null ctx.typer.com.basic.tany p);
	]) ctx.typer.com.basic.tvoid p in

	let tf_args = tf.tf_args @ [(vcompletion,None)] in
	let tf_type = ctx.typer.com.basic.tany in
	if ctx.coro_debug then begin
		print_endline ("BEFORE:\n" ^ (s_expr_debug e));
		CoroDebug.create_dotgraph (DotGraph.get_dump_path ctx.typer.com ([],e.epos.pfile) (Printf.sprintf "pos_%i" e.epos.pmin)) cb_root
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