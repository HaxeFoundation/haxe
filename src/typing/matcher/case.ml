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

	2. Free (unbound) monomorphisms in enum or enum-abstract type-argument positions:
	   When the subject type contains Kind<m> where m is a free mono, each case
	   gets a fresh copy m_new so that matching KString refines m_new=String in that
	   case without permanently constraining the original m. Free monos at other
	   positions (e.g. the whole subject of an integer switch) are left untouched so
	   that normal type-inference accumulation across cases is not disturbed.

	Both substitutions use memoization (same type variable → same fresh mono) so that
	if T appears in both the kind field and the value field, they are unified.

	Returns (subst, rebind_unrefined, unsubst) where:
	- subst: the substitution function to apply to types
	- rebind_unrefined: to be called after pattern processing; for any type-param
	  mono that was not bound by the pattern (i.e. T was not refined), binds it back
	  to the original type parameter so the case body continues to see T, not Unknown.
	- unsubst: reverses the substitution (m_T → TInst(c,[]), m_new → TMono m_orig);
	  used for extractor expressions which should see the original types.
*)
let make_subst ctx t =
	(* Classes of the formal type parameters currently in scope *)
	let tp_classes = List.map (fun ttp -> ttp.ttp_class) ctx.type_params in
	(* Memoisation tables: original → fresh mono *)
	let tp_memo = ref [] in  (* ttp_class -> tmono *)
	let fm_memo = ref [] in  (* tmono -> tmono *)
	let get_or_create l c =
		try List.assq c !l
		with Not_found ->
			let m = Monomorph.create () in
			l := (c, m) :: !l;
			m
	in
	let get_or_create_free m = get_or_create fm_memo m in
	let get_or_create_tp m = get_or_create tp_memo m in
	(*
		subst_tparam: called inside enum/enum-abstract type-argument positions.
		Substitutes both formal type params and free monos.
	*)
	let rec subst_tparam ty = match ty with
		| TMono m ->
			(match m.tm_type with
			| None -> TMono (get_or_create_free m)
			| Some t -> subst_tparam t)
		| TInst({cl_kind = KTypeParameter _} as c, []) when List.memq c tp_classes ->
			TMono (get_or_create_tp c)
		| TEnum(en, tl) ->
			TEnum(en, List.map subst_tparam tl)
		| _ -> Type.map subst_tparam ty
	(*
		subst: general substitution – replaces formal type params everywhere,
		but restricts free-mono substitution to enum/enum-abstract type-arg positions.
	*)
	and subst ty = match ty with
		| TMono m ->
			(match m.tm_type with
			| Some t -> subst t
			| None -> ty)  (* free monos at non-enum positions: leave alone *)
		| TInst({cl_kind = KTypeParameter _} as c, []) when List.memq c tp_classes ->
			TMono (get_or_create_tp c)
		| TAbstract(a, tl) when a.a_enum ->
			TAbstract(a, List.map subst_tparam tl)
		| TEnum(en, tl) ->
			TEnum(en, List.map subst_tparam tl)
		| _ -> Type.map subst ty
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
	(*
		Reverse substitution: maps fresh monos back to their originals.
		m_T (created for a type param) → TInst(c, [])
		m_new (created for a free mono) → TMono m_orig
		Used so that extractor expressions see the original types (#5952).
	*)
	let unsubst ty =
		let rec loop ty = match ty with
			| TMono m ->
				(match m.tm_type with
				| None ->
					(* Use physical equality (==) for monomorph identity: two separate TMono
					   values wrapping distinct tmono records must not be conflated. *)
					begin try
						let c = fst (List.find (fun (_,m') -> m == m') !tp_memo) in
						TInst(c, [])
					with Not_found ->
						try
							let m_orig = fst (List.find (fun (_,m_new) -> m == m_new) !fm_memo) in
							TMono m_orig
						with Not_found ->
							ty
					end
				| Some t -> loop t)
			| _ -> Type.map loop ty
		in
		loop ty
	in
	subst, rebind_unrefined, unsubst

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
	let subst,rebind_unrefined,unsubst = make_subst ctx t in
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
		unsubst = unsubst;
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