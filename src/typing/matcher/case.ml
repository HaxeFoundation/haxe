open Globals
open Ast
open Type
open Typecore
open Error
open ExprToPattern

type t = {
	case_guard : texpr option;
	case_expr : texpr option;
	case_pos : pos;
}

let make ctx t el eg eo_ast with_type postfix_match p =
	let rec collapse_case el = match el with
		| e :: [] ->
			e
		| e :: el ->
			let e2 = collapse_case el in
			EBinop(OpOr,e,e2),punion (pos e) (pos e2)
		| [] ->
			raise_typing_error "case without pattern" p
	in
	let e = collapse_case el in
	let monos = List.map (fun _ -> mk_mono()) ctx.type_params in
	let map = apply_params ctx.type_params monos in
	let save = save_locals ctx in
	let old_types = PMap.fold (fun v acc ->
		let t_old = v.v_type in
		v.v_type <- map v.v_type;
		(v,t_old) :: acc
	) ctx.locals [] in
	let old_ret = ctx.ret in
	ctx.ret <- map ctx.ret;
	let pctx = {
		ctx = ctx;
		current_locals = PMap.empty;
		ctx_locals = ctx.locals;
		or_locals = None;
		in_reification = false;
		is_postfix_match = postfix_match;
		unapply_type_parameters = (fun () -> unapply_type_parameters ctx.type_params monos);
	} in
	let pat = ExprToPattern.make pctx true (map t) e in
	ignore(unapply_type_parameters ctx.type_params monos);
	let eg = match eg with
		| None -> None
		| Some e ->
			let e = type_expr ctx e WithType.value in
			Some (AbstractCast.cast_or_unify ctx ctx.t.tbool e e.epos)
	in
	let eo = match eo_ast,with_type with
		| None,WithType.WithType(t,_) ->
			unify ctx ctx.t.tvoid t (pos e);
			None
		| None,_ ->
			None
		| Some e,WithType.WithType(t,_) ->
			let e = type_expr ctx e (WithType.with_type (map t)) in
			let e = AbstractCast.cast_or_unify ctx (map t) e e.epos in
			Some e
		| Some e,_ ->
			let e = type_expr ctx e with_type in
			Some e
	in
	ctx.ret <- old_ret;
	List.iter (fun (v,t) -> v.v_type <- t) old_types;
	save();
	{
		case_guard = eg;
		case_expr = eo;
		case_pos = p;
	},[],pat