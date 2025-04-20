open Globals
open Type
open CoroTypes
open CoroFunctions
open Texpr
open ContTypes

let localFuncCount = ref 0

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

	let mk_assign estate eid =
		mk (TBinop (OpAssign,estate,eid)) eid.etype null_pos

	let create ctx coro_type =
		let basic = ctx.typer.t in
		(* Mangle class names to hopefully get unique names and avoid collisions *)
		let name, cf_captured, params_outside, result_type =
			let captured_field_name = "_hx_captured" in
			match coro_type with
			| ClassField (cls, field, tf, _) ->
				Printf.sprintf "HxCoro_%s_%s_%s" (ctx.typer.m.curmod.m_path |> fst |> String.concat "_") (ctx.typer.m.curmod.m_path |> snd) field.cf_name,
				(if has_class_field_flag field CfStatic then
					None
				else
					Some (mk_field captured_field_name ctx.typer.c.tthis null_pos null_pos)),
				field.cf_params,
				tf.tf_type
			| LocalFunc(f,v) ->
				let n = Printf.sprintf "HxCoroAnonFunc_%i" !localFuncCount in
				localFuncCount := !localFuncCount + 1;

				let args = List.map (fun (v, _) -> (v.v_name, false, v.v_type)) f.tf_args in
				let t    = TFun (Common.expand_coro_type basic args f.tf_type) in

				n, Some (mk_field captured_field_name t null_pos null_pos), (match v.v_extra with Some ve -> ve.v_params | None -> []), f.tf_type
			in

		(* Is there a pre-existing function somewhere to a valid path? *)
		let cls_path = ((fst ctx.typer.m.curmod.m_path) @ [ Printf.sprintf "_%s" (snd ctx.typer.m.curmod.m_path) ]), name in
		let cls      = mk_class ctx.typer.m.curmod cls_path null_pos null_pos in
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

		let param_types_inside = extract_param_types params_inside in
		let param_types_outside = extract_param_types params_outside in
		let subst = List.combine params_outside param_types_inside in
		let result_type_inside = substitute_type_params subst result_type in
		cls.cl_super <- Some (basic.tcoro.base_continuation_class, [result_type_inside]);
		cf_captured |> Option.may (fun cf -> cf.cf_type <- substitute_type_params subst cf.cf_type);

		(* TODO: This should be cached on the typer context so we don't have to dig up the fields for every coro *)
		let cf_control    = PMap.find "_hx_control" basic.tcoro.continuation_result_class.cl_fields in
		let cf_result     = PMap.find "_hx_result" basic.tcoro.continuation_result_class.cl_fields in
		let cf_error      = PMap.find "_hx_error" basic.tcoro.continuation_result_class.cl_fields in
		let cf_completion = PMap.find "_hx_completion" basic.tcoro.base_continuation_class.cl_fields in
		let cf_context    = PMap.find "_hx_context" basic.tcoro.base_continuation_class.cl_fields in
		let cf_state      = PMap.find "_hx_state" basic.tcoro.base_continuation_class.cl_fields in
		let cf_recursing  = PMap.find "_hx_recursing" basic.tcoro.base_continuation_class.cl_fields in
		let continuation_api = ContTypes.create_continuation_api cf_control cf_result cf_error cf_completion cf_context cf_state cf_recursing in

		{
			cls        = cls;
			inside = {
				params = params_inside;
				param_types = param_types_inside;
				cls_t = TInst(cls,param_types_inside);
				result_type = result_type_inside;
				cont_type = basic.tcoro.base_continuation result_type_inside;
			};
			outside = {
				params = params_outside;
				param_types = param_types_outside;
				cls_t = TInst(cls,param_types_outside);
				result_type = result_type;
				cont_type = basic.tcoro.base_continuation result_type;
			};
			type_param_subst = subst;
			coro_type  = coro_type;
			continuation_api;
			captured   = cf_captured;
		}

	let mk_ctor ctx coro_class initial_state =
		let basic = ctx.typer.t in
		let name  = "completion" in
		let ethis = mk (TConst TThis) coro_class.inside.cls_t null_pos in

		let vargcompletion    = alloc_var VGenerated name basic.tcoro.continuation null_pos in
		let evarargcompletion = Builder.make_local vargcompletion null_pos in
		let einitialstate     = mk (TConst (TInt (Int32.of_int initial_state) )) basic.tint null_pos in
		let esuper            = mk (TCall ((mk (TConst TSuper) coro_class.inside.cont_type null_pos), [ evarargcompletion; einitialstate ])) basic.tvoid null_pos in

		let this_field cf =
			mk (TField(ethis,FInstance(coro_class.cls, coro_class.inside.param_types, cf))) cf.cf_type null_pos
		in

		let captured =
			coro_class.captured
			|> Option.map
				(fun field ->
					let vargcaptured    = alloc_var VGenerated "captured" field.cf_type null_pos in
					let eargcaptured    = Builder.make_local vargcaptured null_pos in
					let ecapturedfield  = this_field field in
					vargcaptured, mk_assign ecapturedfield eargcaptured)
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

			mk (TBlock (esuper :: extra_exprs)) basic.tvoid null_pos,
			extra_tfun_args @ [ (name, false, basic.tcoro.continuation) ],
			extra_tfunction_args @ [ (vargcompletion, None) ]
		in

		let field = mk_field "new" (TFun (tfun_args, basic.tvoid)) null_pos null_pos in
		let func  = TFunction { tf_type = basic.tvoid; tf_args = tfunction_args; tf_expr = eblock } in
		let expr = mk func field.cf_type null_pos in
		field.cf_expr <- Some expr;
		field.cf_kind <- Method MethNormal;

		if ctx.coro_debug then
			s_expr_debug expr |> Printf.printf "%s\n";

		field

	let mk_invoke_resume ctx coro_class =
		let basic     = ctx.typer.t in
		let tret_invoke_resume = coro_class.inside.cls_t in
		let ethis     = mk (TConst TThis) coro_class.inside.cls_t null_pos in
		let ecorocall =
			let this_field cf =
				mk (TField(ethis,FInstance(coro_class.cls, coro_class.inside.param_types, cf))) cf.cf_type null_pos
			in
			match coro_class.coro_type with
			| ClassField (cls, field, f, _) when has_class_field_flag field CfStatic ->
				let args      = (f.tf_args |> List.map (fun (v, _) -> Texpr.Builder.default_value v.v_type null_pos)) @ [ ethis ] in
				let efunction = Builder.make_static_field cls field null_pos in
				mk (TCall (efunction, args)) tret_invoke_resume null_pos
			| ClassField (cls, field,f, _) ->
				let args      = (f.tf_args |> List.map (fun (v, _) -> Texpr.Builder.default_value v.v_type null_pos)) @ [ ethis ] in
				let captured  = coro_class.captured |> Option.get in
				let ecapturedfield = this_field captured in
				let efunction      = mk (TField(ecapturedfield,FInstance(cls, [] (* TODO: check *), field))) field.cf_type null_pos in
				mk (TCall (efunction, args)) tret_invoke_resume null_pos
			| LocalFunc(f,_) ->
				let args      = (List.map (fun (v, _) -> Texpr.Builder.default_value v.v_type null_pos) f.tf_args) @ [ ethis ] in
				let captured  = coro_class.captured |> Option.get in
				let ecapturedfield = this_field captured in
				mk (TCall (ecapturedfield, args)) tret_invoke_resume null_pos
		in
		(* TODO: this is awkward, it would be better to avoid the entire expression and work with the correct types right away *)
		let rec map_expr_type e =
			Type.map_expr_type map_expr_type (substitute_type_params coro_class.type_param_subst) (fun v -> v) e
		in
		let ecorocall = map_expr_type ecorocall in

		let field = mk_field "invokeResume" (TFun ([], tret_invoke_resume)) null_pos null_pos in
		add_class_field_flag field CfOverride;
		let block = mk (TBlock [ Builder.mk_return ecorocall ]) tret_invoke_resume null_pos in
		let func  = TFunction { tf_type = tret_invoke_resume; tf_args = []; tf_expr = block } in
		let expr  = mk (func) basic.tvoid null_pos in
		field.cf_expr <- Some expr;
		field.cf_kind <- Method MethNormal;

		if ctx.coro_debug then
			s_expr_debug expr |> Printf.printf "%s\n";

		field
end

let fun_to_coro ctx coro_type =
	let basic = ctx.typer.t in

	let mk_assign estate eid =
		mk (TBinop (OpAssign,estate,eid)) eid.etype null_pos
	in

	let coro_class = ContinuationClassBuilder.create ctx coro_type in
	let cont = coro_class.continuation_api in

	(* Generate and assign the continuation variable *)
	let vcompletion = alloc_var VGenerated "_hx_completion" basic.tcoro.continuation null_pos in
	let ecompletion = Builder.make_local vcompletion null_pos in

	let vcontinuation = alloc_var VGenerated "_hx_continuation" coro_class.outside.cls_t null_pos in
	let econtinuation = Builder.make_local vcontinuation null_pos in

	let continuation_field cf t =
		mk (TField(econtinuation,FInstance(coro_class.cls, coro_class.outside.param_types, cf))) t null_pos
	in

	let estate  = continuation_field cont.state basic.tint in
	let econtrol = continuation_field cont.control basic.tcoro.control in
	let eresult = continuation_field cont.result basic.tany in
	let eerror = continuation_field cont.error basic.texception in

	let expr, args, pe =
		match coro_type with
		| ClassField (_, cf, f, p) ->
			f.tf_expr, f.tf_args, p
		| LocalFunc(f,_) ->
			f.tf_expr, f.tf_args, f.tf_expr.epos
		in

	let cb_root = make_block ctx (Some(expr.etype, null_pos)) in

	ignore(CoroFromTexpr.expr_to_coro ctx eresult cb_root expr);
	let eloop, eif_error, initial_state, fields = CoroToTexpr.block_to_texpr_coroutine ctx cb_root cont coro_class.cls args [ vcompletion.v_id; vcontinuation.v_id ] econtinuation ecompletion econtrol eresult estate eerror null_pos in
	(* update cf_type to use inside type parameters *)
	List.iter (fun cf ->
		cf.cf_type <- substitute_type_params coro_class.type_param_subst cf.cf_type;
		TClass.add_field coro_class.cls cf
	) fields;
	let ctor   = ContinuationClassBuilder.mk_ctor ctx coro_class initial_state in
	let resume = ContinuationClassBuilder.mk_invoke_resume ctx coro_class in

	TClass.add_field coro_class.cls resume;
	Option.may (TClass.add_field coro_class.cls) coro_class.captured;

	coro_class.cls.cl_constructor <- Some ctor;

	if ctx.coro_debug then
		Printer.s_tclass "\t" coro_class.cls |> Printf.printf "%s\n";

	ctx.typer.m.curmod.m_types <- ctx.typer.m.curmod.m_types @ [ TClassDecl coro_class.cls ];

	let continuation_var = mk (TVar (vcontinuation, Some (Builder.make_null coro_class.outside.cls_t null_pos))) coro_class.outside.cls_t null_pos in

	let std_is e t =
		let type_expr = mk (TTypeExpr (module_type_of_type t)) t_dynamic null_pos in
		Texpr.Builder.resolve_and_make_static_call ctx.typer.com.std "isOfType" [e;type_expr] null_pos
	in

	let prefix_arg =
		match coro_class.coro_type with
		| ClassField (_, field, _, _) when has_class_field_flag field CfStatic ->
			[]
		| ClassField _ ->
			[ mk (TConst TThis) ctx.typer.c.tthis null_pos ]
		| LocalFunc(f,v) ->
			[ Builder.make_local v null_pos ]
	in

	let continuation_assign =
		let t = coro_class.outside.cls_t in

		let ecastedcompletion = mk_cast ecompletion t null_pos in

		let tcond =
			let erecursingfield = mk (TField(ecastedcompletion, FInstance(coro_class.cls, coro_class.outside.param_types, cont.recursing))) basic.tbool null_pos in
			let estdis          = std_is ecompletion t in
			let erecursingcheck = mk (TBinop (OpEq, erecursingfield, (mk (TConst (TBool false)) basic.tbool null_pos))) basic.tbool null_pos in
			mk (TBinop (OpBoolAnd, estdis, erecursingcheck)) basic.tbool null_pos
		in
		let tif       = mk_assign econtinuation ecastedcompletion in
		let tif       = mk (TBlock [
			tif;
			eif_error;
		]) basic.tvoid null_pos in
		let ctor_args = prefix_arg @ [ ecompletion ] in
		let telse = mk_assign econtinuation (mk (TNew (coro_class.cls, coro_class.outside.param_types, ctor_args)) t null_pos) in
		mk (TIf (tcond, tif, Some telse)) basic.tvoid null_pos
	in

	let tf_expr = mk (TBlock [
		continuation_var;
		continuation_assign;
		mk_assign
			(continuation_field cont.recursing basic.tbool)
			(mk (TConst (TBool true)) basic.tbool null_pos);
		eloop;
		Builder.mk_return (Builder.make_null basic.tany null_pos);
	]) basic.tvoid null_pos in

	let tf_args = args @ [ (vcompletion,None) ] in
	(* I'm not sure what this should be, but let's stick to the narrowest one for now.
	   Cpp dies if I try to use coro_class.outside.cls_t here, which might be something
	   to investigate independently. *)
	let tf_type = basic.tcoro.continuation_result coro_class.outside.result_type in
	if ctx.coro_debug then begin
		print_endline ("BEFORE:\n" ^ (s_expr_debug expr));
		CoroDebug.create_dotgraph (DotGraph.get_dump_path (SafeCom.of_com ctx.typer.com) (* TODO: stupid *) ([],pe.pfile) (Printf.sprintf "pos_%i" pe.pmin)) cb_root
	end;
	let e = mk (TFunction {tf_args; tf_expr; tf_type}) (TFun (tf_args |> List.map (fun (v, _) -> (v.v_name, false, v.v_type)), tf_type)) pe in
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