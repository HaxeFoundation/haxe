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
	Collect free (unbound) monomorphisms from type parameter positions of enum
	abstracts within the switch subject type. These become "switch-level type
	parameters": their fresh copies are refined per case by matching against
	typed enum abstract constructors (GADT-like refinement).

	Only enum-abstract type params are collected because only those participate in
	GADT-style refinement (each constructor carries a specific instantiation of
	the abstract's type parameter).  Free monomorphisms at other positions
	(e.g. the entire subject type of an integer switch) are left alone so that
	normal type-inference accumulation across cases is not disturbed.
*)
let collect_subject_free_monos t =
	let seen = ref [] in
	let acc = ref [] in
	let add m =
		if not (List.memq m !seen) then begin
			seen := m :: !seen;
			acc := m :: !acc
		end
	in
	(* Called when we are inside a type-parameter position of an enum abstract *)
	let rec loop_tparam ty = match ty with
		| TMono m when m.tm_type = None -> add m
		| TMono m -> (match m.tm_type with Some t -> loop_tparam t | None -> ())
		| _ -> loop ty
	(* General traversal – only enter "type-param collecting" mode for enum abstracts *)
	and loop ty = match ty with
		| TMono m -> (match m.tm_type with Some t -> loop t | None -> ())
		| TAbstract(a,tl) when a.a_enum -> List.iter loop_tparam tl
		| _ -> TFunctions.iter loop ty
	in
	loop t;
	!acc

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
	(*
		Collect free monomorphisms from the subject type (switch-level type parameters)
		and build a substitution that replaces each free mono with a fresh copy.
		This allows GADT-like per-case type refinement even without an explicit function
		type parameter: if the subject type contains a free monomorphism m, matching a
		constructor like [TInst, name] will resolve the fresh copy m_new = String,
		so that name : String in this branch (while m remains unbound outside).
	*)
	let make_free_mono_subst () =
		let free_monos = collect_subject_free_monos t in
		match free_monos with
		| [] ->
			(fun t -> t)
		| _ ->
			let pairs = List.map (fun m -> (m, Monomorph.create ())) free_monos in
			let rec subst ty = match ty with
				| TMono m ->
					begin match m.tm_type with
					| Some t' -> subst t'
					| None ->
						begin try TMono (List.assq m pairs)
						with Not_found -> ty
						end
					end
				| _ -> Type.map subst ty
			in
			subst
	in
	let subst = make_free_mono_subst () in
	let save = save_locals ctx in
	let old_types = PMap.fold (fun v acc ->
		let t_old = v.v_type in
		v.v_type <- subst (map v.v_type);
		(v,t_old) :: acc
	) ctx.f.locals [] in
	let old_ret = ctx.e.ret in
	ctx.e.ret <- subst (map ctx.e.ret);
	let pctx = {
		ctx = ctx;
		current_locals = PMap.empty;
		ctx_locals = ctx.f.locals;
		or_locals = None;
		in_reification = false;
		is_postfix_match = postfix_match;
		unapply_type_parameters = (fun () -> unapply_type_parameters ctx.type_params monos);
	} in
	let pat = ExprToPattern.make pctx true (subst (map t)) e in
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
			let e = type_expr ctx e (WithType.with_type (subst (map t))) in
			let e = AbstractCast.cast_or_unify ctx (subst (map t)) e e.epos in
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