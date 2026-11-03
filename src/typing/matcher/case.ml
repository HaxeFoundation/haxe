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
	Switch-level type parameter refinement.

	Two kinds of type variables participate:
	1. Formal type parameters from ctx.type_params (TInst with KTypeParameter).
	2. Free (unbound) monomorphisms in the type arguments of the top-level
	   enum / enum-abstract subject types (or tuple elements thereof).

	The analysis of *which* free monomorphisms are eligible for substitution is
	done once per switch by collect_free_monos, before any cases are examined.
	Per-case, make_subst creates fresh copies so that each case can independently
	refine the type through pattern matching.
*)

(* Collect free (unbound) monomorphisms from the type arguments of direct
   enum / enum-abstract subjects. Only top-level positions are examined:
   for a single subject, its enum-abstract or enum type args; for a tuple
   subject (TFun(args, fake_tuple_type)), each element independently.
   Nested field types (e.g. in a structural match) are NOT traversed. *)
let collect_free_monos t =
	let acc = ref [] in
	let rec walk_tl tl =
		List.iter walk tl
	and walk ty = match ty with
		| TMono m ->
			(match m.tm_type with
			| None ->
				if not (List.exists (fun m' -> m == m') !acc) then
					acc := m :: !acc
			| Some t -> walk t)
		| TEnum(_, tl) -> walk_tl tl
		| TInst(_, tl) -> walk_tl tl
		| TAbstract(_, tl) -> walk_tl tl
		| TType(t, tl) -> walk (apply_typedef t tl)
		| TFun(args, ret) -> List.iter (fun (_,_,t) -> walk t) args; walk ret
		| TAnon a -> PMap.iter (fun _ f -> walk f.cf_type) a.a_fields
		| TLazy f -> walk (lazy_type f)
		| TDynamic (Some t) -> walk t
		| TDynamic None -> ()
	in
	let rec collect_one ty = match ty with
		| TAbstract(a, tl) when a.a_enum -> walk_tl tl
		| TEnum(_, tl) -> walk_tl tl
		| TMono m ->
			(match m.tm_type with
			| Some t -> collect_one t
			| None -> ())
		| TType(t,tl) -> collect_one (apply_typedef t tl)
		| _ -> ()
	in
	let rec collect_top ty = match ty with
		| TFun(args, tr) when tr == ExprToPattern.fake_tuple_type ->
			List.iter (fun (_,_,t) -> collect_one (follow t)) args
		| TMono m ->
			(match m.tm_type with
			| Some t -> collect_top t
			| None -> ())
		| TType(t,tl) -> collect_top (apply_typedef t tl)
		| _ -> collect_one ty
	in
	collect_top (follow t);
	!acc

(* Create a per-case substitution from pre-collected free monomorphs and
   the formal type parameters. Returns (subst, rebind_unrefined, unsubst). *)
let make_subst ctx free_monos =
	let tp_classes = List.map (fun ttp -> ttp.ttp_class) ctx.type_params in
	let tp_memo = ref [] in
	let fm_memo = ref [] in
	let get_or_create l c =
		try List.assq c !l
		with Not_found ->
			let m = Monomorph.create () in
			l := (c, m) :: !l;
			m
	in
	let get_or_create_free m = get_or_create fm_memo m in
	let get_or_create_tp c = get_or_create tp_memo c in
	(* subst: replaces formal type params everywhere, and replaces
	   collected free monos wherever they appear. *)
	let rec subst ty = match ty with
		| TMono m ->
			(match m.tm_type with
			| Some t -> subst t
			| None ->
				(* Only substitute free monos that were pre-collected from
				   top-level enum/enum-abstract subject type arguments. *)
				if List.memq m free_monos then TMono (get_or_create_free m)
				else ty)
		| TInst({cl_kind = KTypeParameter _} as c, []) when List.memq c tp_classes ->
			TMono (get_or_create_tp c)
		| _ -> Type.map subst ty
	in
	let rebind_unrefined () =
		List.iter (fun (c, m) ->
			if m.tm_type = None then
				Monomorph.do_bind m (TInst(c, []))
		) !tp_memo
	in
	let unsubst ty =
		let rec loop ty = match ty with
			| TMono m ->
				(match m.tm_type with
				| None ->
					(* Physical equality (==) for monomorph identity *)
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

let make ctx t free_monos el eg eo_ast with_type postfix_match p =
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
	let subst,rebind_unrefined,unsubst = make_subst ctx free_monos in
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