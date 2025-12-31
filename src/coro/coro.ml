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
		| _ ->
			if is_nullable t then
				mk (TConst TNull) t p
			else
				mk (TConst (TInt (Int32.zero))) t p (* I guess *)

	let mk_invoke_resume ctx coro_class =
		let basic     = ctx.typer.t in
		let b         = ctx.builder in
		let tret_invoke_resume = coro_class.inside.cls_t in
		let ethis     = b#this coro_class.inside.cls_t coro_class.name_pos in
		let ecorocall =
			let this_field cf =
				b#instance_field ethis coro_class.cls coro_class.inside.param_types cf cf.cf_type
			in
			let map_args =
				List.map (fun (v, _) ->
					let t = substitute_type_params coro_class.type_param_subst v.v_type in

					default_value ctx.typer.t (Abstract.follow_with_abstracts t) coro_class.name_pos
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
				let efunction      = b#instance_field ecapturedfield cls [] (* TODO: check *) field field.cf_type in
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
	let eloop, initial_state, fields = CoroToTexpr.block_to_texpr_coroutine ctx cb_root cont coro_class.cls coro_class.outside.param_types args [ vcompletion.v_id; vcontinuation.v_id ] exprs coro_class.name_pos stack_item_inserter start_exception in
	(* update cf_type to use inside type parameters *)
	List.iter (fun cf ->
		cf.cf_type <- substitute_type_params coro_class.type_param_subst cf.cf_type;
		TClass.add_field coro_class.cls cf
	) fields;
	create_continuation_class ctx cont coro_class initial_state;
	let continuation_var = b#var_init_null vcontinuation in

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

		let ecastedcompletion = mk_cast ecompletion t coro_class.name_pos in

		let tcond =
			let erecursingfield = b#instance_field ecastedcompletion coro_class.cls coro_class.outside.param_types cont.recursing basic.tbool in
			let estdis          = std_is ecompletion t in
			let erecursingcheck = b#op_eq erecursingfield (b#bool false coro_class.name_pos) in
			b#op_bool_and estdis erecursingcheck
		in
		let tif       = b#assign econtinuation ecastedcompletion in
		let tif       = b#void_block [tif] in
		let ctor_args = prefix_arg @ [ ecompletion ] in
		let telse = b#assign econtinuation (mk (TNew (coro_class.cls, coro_class.outside.param_types, ctor_args)) t coro_class.name_pos) in
		b#if_then_else tcond tif telse basic.tvoid
	in

	let continuation_field cf t =
		b#instance_field econtinuation coro_class.cls coro_class.outside.param_types cf t
	in
	let el = [
		continuation_var;
		continuation_assign;
		b#assign
			(continuation_field cont.recursing basic.tbool)
			(b#bool true coro_class.name_pos);
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
		b#return (b#null basic.tany coro_class.name_pos);
	] in
	b#void_block el

let coro_to_normal ctx cont coro_class cb_root exprs vcontinuation =
	let open ContinuationClassBuilder in
	let open CoroToTexpr in
	let basic = ctx.typer.t in
	let b = ctx.builder in
	create_continuation_class ctx cont coro_class 0;
	let rec loop cb previous_el =
		let bad_pos = coro_class.name_pos in
		let loop cb el =
			if not (has_block_flag cb CbGenerated) then begin
				add_block_flag cb CbGenerated;
				loop cb el
			end else
				el,false
		in
		let loop_as_block cb =
			let el,term = loop cb [] in
			b#void_block el,term
		in
		let current_el = ref (previous_el @ (get_block_exprs cb)) in
		let continue cb_next e =
			loop cb_next (!current_el @ [e])
		in
		let maybe_continue cb_next term e = match cb_next with
			| Some cb_next when not term ->
				continue cb_next e
			| _ ->
				(!current_el @ [e]),true
		in
		let add e = current_el := !current_el @ [e] in
		let terminate e =
			add e;
			!current_el,true
		in
		begin match cb.cb_next with
			| NextSub(cb_sub,cb_next) ->
				let e_next,term = loop_as_block cb_sub in
				maybe_continue cb_next term e_next
			| NextReturn e1 ->
				let e1 = coro_class.continuation_api.immediate_result e1 in
				terminate (b#return e1);
			| NextThrow e1 ->
				let e1 = coro_class.continuation_api.immediate_error e1 coro_class.inside.result_type in
				terminate (b#return e1);
			| NextUnknown | NextReturnVoid ->
				let e1 = coro_class.continuation_api.immediate_result (b#null t_dynamic coro_class.name_pos) in
				terminate (b#return e1);
			| NextBreak _ ->
				terminate (b#break bad_pos);
			| NextContinue _ ->
				terminate (b#continue bad_pos);
			| NextIfThen(e1,cb_then,cb_next) ->
				let e_then,_ = loop_as_block cb_then in
				let e_if = b#if_then e1 e_then in
				continue cb_next e_if
			| NextIfThenElse(e1,cb_then,cb_else,cb_next) ->
				let e_then,term_then = loop_as_block cb_then in
				let e_else,term_else = loop_as_block cb_else in
				let e_if = b#if_then_else e1 e_then e_else basic.tvoid in
				maybe_continue cb_next (term_then && term_else) e_if
			| NextSwitch(switch,cb_next) ->
				let term = ref true in
				let p = ref switch.cs_subject.epos in
				let switch_cases = List.map (fun (el,cb) ->
					let e,term' = loop_as_block cb in
					term := !term && term';
					p := Ast.punion !p e.epos;
					{
						case_patterns = el;
						case_expr = e;
					}
				) switch.cs_cases in
				let switch_default = Option.map (fun cb ->
					let e,term' = loop_as_block cb in
					p := Ast.punion !p e.epos;
					term := !term && term';
					e
				) switch.cs_default in
				let switch = {
					switch_subject = switch.cs_subject;
					switch_cases;
					switch_default;
					switch_exhaustive = switch.cs_exhaustive
				} in
				maybe_continue cb_next (switch.switch_exhaustive && !term) (mk (TSwitch switch) basic.tvoid !p)
			| NextWhile(e1,cb_body,cb_next) ->
				let e_body,_ = loop_as_block cb_body in
				let e_while = mk (TWhile(e1,e_body,NormalWhile)) basic.tvoid (Ast.punion e1.epos e_body.epos) in
				maybe_continue cb_next false e_while
			| NextTry(cb_try,catches,cb_next) ->
				let e_try,term = loop_as_block cb_try in
				let p = ref e_try.epos in
				let term = ref term in
				let catches = List.map (fun (v,cb) ->
					let e,term' = loop_as_block cb in
					p := Ast.punion !p e.epos;
					term := !term && term';
					(v,e)
				) catches.cc_catches in
				let e_try = mk (TTry(e_try,catches)) basic.tvoid !p in
				maybe_continue cb_next !term e_try
			| NextFallThrough _ | NextGoto _ ->
				!current_el,false
			| NextSuspend(suspend,cb_next) ->
				let e_sus = CoroToTexpr.make_suspending_call basic cont suspend {exprs.ecompletion with epos = suspend.cs_pos} in
				add (mk (TReturn (Some e_sus)) t_dynamic e_sus.epos);
				!current_el,true
		end
	in
	let el,_ = loop cb_root [] in
	let e = b#void_block el in
	let e = if ctx.nothrow then
		e
	else begin
		let catch =
			let v = alloc_var VGenerated "e" t_dynamic e.epos in
			let ev = b#local v e.epos in
			let eerr = coro_class.continuation_api.immediate_error ev coro_class.inside.result_type in
			let eret = b#return eerr in
			(v,eret)
		in
		mk (TTry(e,[catch])) basic.tvoid e.epos
	end in
	b#void_block [e]

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
		b#instance_field econtinuation c coro_class.outside.param_types cf t
	in

	let estate = continuation_field cont.suspension_result_class cont.state cont.suspension_state in
	let eresult = continuation_field cont.suspension_result_class cont.result basic.tany in
	let eerror = continuation_field cont.suspension_result_class cont.error basic.texception in

	let continuation_field cf t =
		b#instance_field econtinuation cont.base_continuation_class coro_class.outside.param_types cf t
	in

	let egoto  = continuation_field cont.goto_label basic.tint in

	let vtmp_result = alloc_var VGenerated "_hx_result" basic.tany coro_class.name_pos in
	let etmp_result = b#local vtmp_result coro_class.name_pos in
	let vtmp_error = alloc_var VGenerated "_hx_error" basic.texception coro_class.name_pos in
	let etmp_error = b#local vtmp_error coro_class.name_pos in
	let vtmp_error_unwrapped = lazy (alloc_var VGenerated "_hx_error_unwrapped" basic.tany coro_class.name_pos) in
	let etmp_error_unwrapped = lazy (b#local (Lazy.force vtmp_error_unwrapped) coro_class.name_pos) in

	let expr, args, name =
		match coro_type with
		| ClassField (_, cf, f, p) ->
			f.tf_expr, f.tf_args, cf.cf_name
		| LocalFunc(f,v) ->
			f.tf_expr, f.tf_args, v.v_name
		in

	let cb_root = make_block ctx (Some(expr.etype, coro_class.name_pos)) in

	ignore(CoroFromTexpr.expr_to_coro ctx etmp_result etmp_error_unwrapped cb_root expr);
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
	let start_exception =
		let cf = PMap.find "startException" cont.base_continuation_class.cl_fields in
		let ef = continuation_field cf cf.cf_type in
		(fun e ->
			mk (TCall(ef,[e])) basic.tvoid coro_class.name_pos
		)
	in
	let tf_expr,cb_root = try
		let cb_root = if ctx.optimize then CoroFromTexpr.optimize_cfg ctx cb_root else cb_root in
		coro_to_state_machine ctx coro_class cb_root exprs args vtmp_result vtmp_error vtmp_error_unwrapped vcompletion vcontinuation stack_item_inserter start_exception, cb_root
	with CoroTco cb_root ->
		coro_to_normal ctx cont coro_class cb_root exprs vcontinuation,cb_root
	in

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
	(* let optimize = not (Define.raw_defined typer.Typecore.com.defines "coroutine.noopt") in *)
	let optimize = false in
	let builder = new CoroElsewhere.texpr_builder typer.Typecore.t in
	let ctx = {
		builder;
		typer;
		coro_debug = Meta.has (Meta.Custom ":coroutine.debug") meta;
		optimize;
		allow_tco = optimize && not (Meta.has (Meta.Custom ":coroutine.notco") meta);
		nothrow = Meta.has (Meta.Custom ":coroutine.nothrow") meta;
		vthis = None;
		next_block_id = 0;
		current_catch = None;
		has_catch = false;
	} in
	ctx
