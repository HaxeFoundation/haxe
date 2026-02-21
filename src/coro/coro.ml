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
		(* Some coroutine classes (member functions, local functions) need to capture state, this field stores that *)
		captured : tclass_field option;
	}

	let create ctx coro_type =
		let basic = ctx.typer.t in
		(* Mangle class names to hopefully get unique names and avoid collisions *)
		let name, cf_captured, result_type, name_pos =
			let captured_field_name = "captured" in
			let managled_class_name = Printf.sprintf "HxCoro_%s_%s" (ctx.typer.c.curclass.cl_path |> fst |> String.concat "_") (ctx.typer.c.curclass.cl_path |> snd) in
			match coro_type with
			| ClassField (_, field, tf, _) ->
				Printf.sprintf "%s_%s" managled_class_name field.cf_name,
				(if has_class_field_flag field CfStatic then
					None
				else
					Some (mk_field captured_field_name ctx.typer.c.tthis field.cf_name_pos field.cf_name_pos)),
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

				let args = List.map (fun (v, _) -> (v.v_name, false, v.v_type)) f.tf_args in
				let t    = TFun (Common.expand_coro_type basic args f.tf_type) in

				n, Some (mk_field captured_field_name t v.v_pos v.v_pos), f.tf_type, v.v_pos
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
		cf_captured |> Option.may (fun cf -> cf.cf_type <- substitute_type_params subst cf.cf_type);

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
			captured   = cf_captured;
		}

	let mk_ctor ctx cont coro_class initial_state =
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
			coro_class.captured
			|> Option.map
				(fun field ->
					let vargcaptured    = alloc_var VGenerated "captured" field.cf_type coro_class.name_pos in
					let eargcaptured    = b#local vargcaptured coro_class.name_pos in
					let ecapturedfield  = this_field field in
					vargcaptured, b#assign ecapturedfield eargcaptured)
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

			b#void_block (esuper :: extra_exprs),
			extra_tfun_args @ [ (name, false, cont.continuation) ],
			extra_tfunction_args @ [ (vargcompletion, None) ]
		in

		let field = mk_field "new" (TFun (tfun_args, basic.tvoid)) coro_class.name_pos coro_class.name_pos in
		let func  = TFunction { tf_type = basic.tvoid; tf_args = tfunction_args; tf_expr = eblock } in
		let expr = mk func field.cf_type coro_class.name_pos in
		field.cf_expr <- Some expr;
		field.cf_kind <- Method MethNormal;

		if ctx.coro_debug then
			s_expr_debug expr |> Printf.printf "%s\n";

		field

	let default_value basic t p = match follow_without_null t with
		| TAbstract({a_path = ([],"Int")},[]) ->
			mk (TConst (TInt (Int32.zero))) t p
		| TAbstract({a_path = ([],"Float")},[]) ->
			mk (TConst (TFloat "0.0")) t p
		| TAbstract({a_path = ([],"Bool")},[]) ->
			mk (TConst (TBool false)) t p
		| TMono r when not (is_nullable_mono r) ->
			(* This might be inferred to anything later, so the best course of action
			   is to make the mono nullable and use null. *)
			Monomorph.add_modifier r (MNullable basic.tnull);
			mk (TConst TNull) t p
		| TFun _ ->
			mk (TConst TNull) t p
		| _ ->
			if is_nullable t then
				mk (TConst TNull) t p
			else
				mk (TConst (TInt (Int32.zero))) t p (* I guess *)

	let mk_invoke_resume ctx coro_class =
		let basic     = ctx.typer.t in
		let b         = ctx.builder in
		let tret_invoke_resume = (TInst(Lazy.force ctx.typer.t.tcoro.suspension_result_class,[coro_class.outside.result_type])) in
		let ethis     = b#this coro_class.inside.cls_t coro_class.name_pos in
		let ecorocall =
			let this_field cf =
				b#instance_field ethis coro_class.cls coro_class.inside.param_types cf cf.cf_type
			in
			let map_args =
				List.map (fun (v, eo) ->
					let t = substitute_type_params coro_class.type_param_subst v.v_type in
					let t = Abstract.follow_with_abstracts t in
					if eo <> None then
						mk (TConst TNull) (ctx.typer.t.tnull t) coro_class.name_pos
					else
						default_value ctx.typer.t t coro_class.name_pos
				)
			in
			match coro_class.coro_type with
			| ClassField (cls, field, f, _) when has_class_field_flag field CfStatic ->
				let args      = ethis :: (f.tf_args |> map_args) in
				let estaticthis = Builder.make_static_this cls coro_class.name_pos in
				let tcf = substitute_type_params coro_class.type_param_subst field.cf_type in
				let efunction = b#static_field estaticthis cls field tcf in
				b#call efunction args tret_invoke_resume
			| ClassField (cls, field,f, _) ->
				let args      = ethis :: (f.tf_args |> map_args) in
				let captured  = coro_class.captured |> Option.get in
				let ecapturedfield = this_field captured in
				let efunction      = b#instance_field ecapturedfield cls coro_class.outside.param_types field field.cf_type in
				b#call efunction args tret_invoke_resume
			| LocalFunc(f,_) ->
				let args      = ethis :: (f.tf_args |> map_args) in
				let captured  = coro_class.captured |> Option.get in
				let ecapturedfield = this_field captured in
				b#call ecapturedfield args tret_invoke_resume
		in

		let field = mk_field "invokeResume" (TFun ([], tret_invoke_resume)) coro_class.name_pos coro_class.name_pos in
		add_class_field_flag field CfOverride;
		let block = b#void_block [ b#return ecorocall ] in
		let func  = TFunction { tf_type = tret_invoke_resume; tf_args = []; tf_expr = block } in
		let expr  = mk (func) basic.tvoid coro_class.name_pos in
		field.cf_expr <- Some expr;
		field.cf_kind <- Method MethNormal;

		if ctx.coro_debug then
			s_expr_debug expr |> Printf.printf "%s\n";

		field
end

let create_continuation_class ctx cont coro_class initial_state =
	let ctor   = ContinuationClassBuilder.mk_ctor ctx cont coro_class initial_state in
	let resume = ContinuationClassBuilder.mk_invoke_resume ctx coro_class in
	TClass.add_field coro_class.cls resume;
	Option.may (TClass.add_field coro_class.cls) coro_class.captured;
	coro_class.cls.cl_constructor <- Some ctor;
	if ctx.coro_debug then
		Printer.s_tclass "\t" coro_class.cls |> Printf.printf "%s\n";

	ctx.typer.m.curmod.m_types <- ctx.typer.m.curmod.m_types @ [ TClassDecl coro_class.cls ]

let coro_to_state_machine ctx coro_class cb_root exprs args vtmp_result vtmp_error vtmp_error_unwrapped vcompletion vcontinuation stack_item_inserter start_exception =
	let basic = ctx.typer.t in
	let b = ctx.builder in
	let cont = coro_class.ContinuationClassBuilder.continuation_api in
	let eloop, initial_state, fields, is_single_state = CoroToTexpr.block_to_texpr_coroutine ctx cb_root cont coro_class.cls coro_class.outside.param_types args [ vcompletion.v_id; vcontinuation.v_id ] exprs coro_class.name_pos stack_item_inserter start_exception in
	(* update cf_type to use inside type parameters *)
	List.iter (fun cf ->
		cf.cf_type <- substitute_type_params coro_class.type_param_subst cf.cf_type;
		TClass.add_field coro_class.cls cf
	) fields;
	create_continuation_class ctx cont coro_class initial_state;

	let std_is e t =
		let type_expr = mk (TTypeExpr (module_type_of_type t)) t_dynamic coro_class.name_pos in
		Texpr.Builder.resolve_and_make_static_call ctx.typer.com.std "isOfType" [e;type_expr] coro_class.name_pos
	in

	let prefix_arg =
		match coro_class.coro_type with
		| ClassField (_, field, _, _) when has_class_field_flag field CfStatic ->
			[]
		| ClassField _ ->
			[ b#this ctx.typer.c.tthis coro_class.name_pos ]
		| LocalFunc(f,v) ->
			[ b#local v coro_class.name_pos ]
	in

	let {CoroToTexpr.econtinuation;ecompletion;estate;eresult;egoto;eerror} = exprs in

	let continuation_assign =
		let t = coro_class.outside.cls_t in
		let ctor_args = prefix_arg @ [ ecompletion ] in
		let tnew = (mk (TNew (coro_class.cls, coro_class.outside.param_types, ctor_args)) t coro_class.name_pos) in
		if is_single_state then
			(* Single-state coroutines can never be resumed mid-body, so we never
			   recurse into ourselves. Always allocate a fresh continuation and skip
			   the recursing check entirely. *)
			tnew
		else begin
			let ecastedcompletion = mk_cast ecompletion t coro_class.name_pos in
			let tcond =
				let erecursingfield = b#instance_field ecastedcompletion coro_class.cls coro_class.outside.param_types cont.recursing basic.tbool in
				let estdis          = std_is ecompletion t in
				let erecursingcheck = b#op_eq erecursingfield (b#bool false coro_class.name_pos) in
				b#op_bool_and estdis erecursingcheck
			in
			let tif   = b#void_block [ecastedcompletion] in
			let telse = tnew in
			b#if_then_else tcond tif telse basic.tvoid
		end
	in

	let continuation_field cf t =
		b#instance_field econtinuation coro_class.cls coro_class.outside.param_types cf t
	in
	let el = [
		b#var_init vcontinuation continuation_assign;
	] in
	let el = if is_single_state then el else
		(* For multi-state coroutines, mark the continuation as actively recursing so
		   the entry-point check knows whether to reuse it or allocate a fresh one. *)
		el @ [b#assign
			(continuation_field cont.recursing basic.tbool)
			(b#bool true coro_class.name_pos)]
	in
	let el = el @ [
		b#var_init vtmp_result eresult;
		b#var_init_null vtmp_error;
	] in
	let el = if Lazy.is_val vtmp_error_unwrapped then
		el @ [b#var_init_null (Lazy.force vtmp_error_unwrapped)]
	else
		el
	in
	let el = el @ [
		eloop;
	] in
	b#void_block el

let fun_to_coro ctx coro_type =
	let basic = ctx.typer.t in
	let b = ctx.builder in

	let coro_class = ContinuationClassBuilder.create ctx coro_type in
	let cont = coro_class.continuation_api in

	(* Generate and assign the continuation variable *)
	let vcompletion = alloc_var VGenerated "_hx_completion" cont.continuation coro_class.name_pos in
	let ecompletion = b#local vcompletion coro_class.name_pos in

	let vcontinuation = alloc_var VGenerated "_hx_continuation" coro_class.outside.cls_t coro_class.name_pos in
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

	let vtmp_result = alloc_var VGenerated "_hx_result" (basic.tnull basic.tany) coro_class.name_pos in
	let etmp_result = b#local vtmp_result coro_class.name_pos in
	let vtmp_error = alloc_var VGenerated "_hx_error" (basic.tnull basic.texception) coro_class.name_pos in
	let etmp_error = b#local vtmp_error coro_class.name_pos in
	let vtmp_error_unwrapped = lazy (alloc_var VGenerated "_hx_error_unwrapped" (basic.tnull basic.tany) coro_class.name_pos) in
	let etmp_error_unwrapped = lazy (b#local (Lazy.force vtmp_error_unwrapped) coro_class.name_pos) in

	let expr, args, name =
		match coro_type with
		| ClassField (_, cf, f, p) ->
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
	let make_inline_return e1_opt pos =
		let stmts = [
			b#assign egoto (b#int (-1) pos);
			b#assign estate (CoroControl.mk_control basic CoroControl.CoroReturned);
		] in
		let stmts = match e1_opt with
			| None -> stmts
			| Some e1 -> stmts @ [b#assign eresult e1]
		in
		let stmts = stmts @ [b#return econtinuation] in
		mk (TBlock stmts) t_dynamic pos
	in
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
			| LocalFunc (f, v) ->
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
	let make_inline_tail_call call =
		let (ecallcoroutine, eret) = CoroToTexpr.SuspensionCalls.make_suspending_tail_call ctx cont exprs call in
		b#void_block [stack_item_inserter call.cs_pos; ecallcoroutine; eret]
	in
	ignore(CoroFromTexpr.expr_to_coro ctx etmp_result etmp_error_unwrapped cb_root scope make_inline_return make_inline_tail_call expr);
	let start_exception =
		let cf = PMap.find "startException" cont.base_continuation_class.cl_fields in
		let ef = continuation_field cf cf.cf_type in
		(fun e ->
			mk (TCall(ef,[e])) basic.tvoid coro_class.name_pos
		)
	in
	let tf_expr = coro_to_state_machine ctx coro_class cb_root exprs args vtmp_result vtmp_error vtmp_error_unwrapped vcompletion vcontinuation stack_item_inserter start_exception in

	let tf_args = (vcompletion,None) :: args in
	(* I'm not sure what this should be, but let's stick to the widest one for now.
	   Cpp dies if I try to use coro_class.outside.cls_t here, which might be something
	   to investigate independently. *)
	let tf_type = cont.suspension_result coro_class.outside.result_type in
	if ctx.coro_debug then begin
		print_endline ("BEFORE:\n" ^ (s_expr_debug expr));
		CoroDebug.create_dotgraph (DotGraph.get_dump_path (SafeCom.of_com ctx.typer.com) (ctx.typer.c.curclass.cl_path) name) cb_root
	end;
	let e = mk (TFunction {tf_args; tf_expr; tf_type}) (TFun (tf_args |> List.map (fun (v, _) -> (v.v_name, false, v.v_type)), tf_type)) tf_expr.epos in
	if ctx.coro_debug then print_endline ("AFTER:\n" ^ (s_expr_debug e));
	e

let create_coro_context typer meta =
	let builder = new CoroElsewhere.texpr_builder typer.Typecore.t in
	let ctx = {
		builder;
		typer;
		coro_debug = Meta.has (Meta.Custom ":coroutine.debug") meta;
		nothrow = Meta.has (Meta.Custom ":coroutine.nothrow") meta;
		vthis = None;
		next_block_id = 0;
		current_catch = None;
		has_catch = false;
	} in
	ctx
