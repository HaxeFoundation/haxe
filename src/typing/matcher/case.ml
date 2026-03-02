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

(*
	Build a per-case type substitution that replaces "switch-level type parameters"
	with fresh monomorphisms. Two kinds of type variables are treated as switch-level
	type parameters:

	1. Formal type parameters from ctx.type_params (TInst with KTypeParameter):
	   Each occurrence of a formal type param T is replaced with a fresh mono m_T,
	   identical to the old apply_params ctx.type_params monos behaviour. This makes
	   pattern matching refine T per case even though T itself is not a monomorphism.

	2. Free (unbound) monomorphisms in enum-abstract type-argument positions:
	   When the subject type contains Kind<m> where m is a free mono, each case
	   gets a fresh copy m_new so that matching KString refines m_new=String in that
	   case without permanently constraining the original m. Free monos at other
	   positions (e.g. the whole subject of an integer switch) are left untouched so
	   that normal type-inference accumulation across cases is not disturbed.

	Both substitutions use memoization (same type variable → same fresh mono) so that
	if T appears in both the kind field and the value field, they are unified.

	Returns (subst, rebind_unrefined) where:
	- subst: the substitution function to apply to types
	- rebind_unrefined: to be called after pattern processing; for any type-param
	  mono that was not bound by the pattern (i.e. T was not refined), binds it back
	  to the original type parameter so the case body continues to see T, not Unknown.
*)
let make_subst ctx t =
	(* Classes of the formal type parameters currently in scope *)
	let tp_classes = List.map (fun ttp -> ttp.ttp_class) ctx.type_params in
	(* Memoisation tables: original → fresh mono *)
	let tp_memo = ref [] in  (* ttp_class -> tmono *)
	let fm_memo = ref [] in  (* tmono -> tmono *)
	let get_or_create_tp c =
		try List.assq c !tp_memo
		with Not_found ->
			let m = Monomorph.create () in
			tp_memo := (c, m) :: !tp_memo;
			m
	in
	let get_or_create_free m =
		try List.assq m !fm_memo
		with Not_found ->
			let m_new = Monomorph.create () in
			fm_memo := (m, m_new) :: !fm_memo;
			m_new
	in
	(*
		subst_tparam: called inside enum-abstract type-argument positions.
		Substitutes both formal type params and free monos.
	*)
	let rec subst_tparam ty = match ty with
		| TMono m ->
			(match m.tm_type with
			| None -> TMono (get_or_create_free m)
			| Some t -> subst_tparam t)
		| TInst({cl_kind = KTypeParameter _} as c, []) when List.memq c tp_classes ->
			TMono (get_or_create_tp c)
		| _ -> Type.map subst_all ty
	(*
		subst_all: general substitution – replaces formal type params everywhere,
		but restricts free-mono substitution to enum-abstract type-arg positions.
	*)
	and subst_all ty = match ty with
		| TMono m ->
			(match m.tm_type with
			| Some t -> subst_all t
			| None -> ty)  (* free monos at non-enum-abstract positions: leave alone *)
		| TInst({cl_kind = KTypeParameter _} as c, []) when List.memq c tp_classes ->
			TMono (get_or_create_tp c)
		| TAbstract(a, tl) when a.a_enum ->
			TAbstract(a, List.map subst_tparam tl)
		| _ -> Type.map subst_all ty
	in
	let subst =
		if tp_classes = [] then
			(* Fast path: no type params, only substitute free monos in enum-abstract args *)
			let rec subst_no_tp ty = match ty with
				| TMono m ->
					(match m.tm_type with
					| Some t -> subst_no_tp t
					| None -> ty)
				| TAbstract(a, tl) when a.a_enum ->
					TAbstract(a, List.map (fun arg -> match arg with
						| TMono m when m.tm_type = None -> TMono (get_or_create_free m)
						| TMono m -> (match m.tm_type with Some t -> subst_no_tp t | None -> arg)
						| _ -> subst_no_tp arg) tl)
				| _ -> Type.map subst_no_tp ty
			in
			subst_no_tp
		else
			subst_all
	in
	(*
		After pattern processing, any type-param mono that is still unbound (i.e. the
		pattern did not refine T) is bound back to the original type parameter type so
		that the case body continues to see T instead of Unknown<N>.
		This mirrors the old unapply_type_parameters behaviour.
	*)
	let rebind_unrefined () =
		List.iter (fun (c, m) ->
			if m.tm_type = None then
				Monomorph.do_bind m (TInst(c, []))
		) !tp_memo
	in
	subst, rebind_unrefined

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
	let subst,rebind_unrefined = make_subst ctx t in
	let save = save_locals ctx in
	let old_types = PMap.fold (fun v acc ->
		let t_old = v.v_type in
		v.v_type <- subst v.v_type;
		(v,t_old) :: acc
	) ctx.f.locals [] in
	let old_ret = ctx.e.ret in
	ctx.e.ret <- subst ctx.e.ret;
	let pctx = {
		ctx = ctx;
		current_locals = PMap.empty;
		ctx_locals = ctx.f.locals;
		or_locals = None;
		in_reification = false;
		is_postfix_match = postfix_match;
	} in
	let pat = ExprToPattern.make pctx true (subst t) e in
	(* For any type-param mono not refined by the pattern, rebind it to T *)
	rebind_unrefined ();
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
			let e = type_expr ctx e (WithType.with_type (subst t)) in
			let e = AbstractCast.cast_or_unify ctx (subst t) e e.epos in
			Some e
		| Some e,_ ->
			let e = type_expr ctx e with_type in
			Some e
	in
	ctx.e.ret <- old_ret;
	List.iter (fun (v,t) -> v.v_type <- t) old_types;
	save();
	{
		case_guard = eg;
		case_expr = eo;
		case_pos = p;
	},[],pat