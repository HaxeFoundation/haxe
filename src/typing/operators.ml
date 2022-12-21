open Globals
open Ast
open Type
open TyperBase
open Typecore
open Error
open CallUnification
open Calls
open Fields
open FieldAccess

module BinopResult = struct

	type normal_binop = {
		binop_op : binop;
		binop_lhs : texpr;
		binop_rhs : texpr;
		binop_type : Type.t;
		binop_needs_assign : bool;
		binop_swapped : bool;
		binop_pos : pos;
	}

	type t =
		| BinopNormal of normal_binop
		| BinopSpecial of texpr * bool

	let to_string br =
		let st = s_type (print_context()) in
		let se = s_expr_pretty false "" false st in
		match br with
		| BinopNormal bn ->
			Printer.s_record_fields "" [
				"binop_op",s_binop bn.binop_op;
				"binop_lhs",se bn.binop_lhs;
				"binop_rhs",se bn.binop_rhs;
				"binop_type",st bn.binop_type;
				"binop_needs_assign",string_of_bool bn.binop_needs_assign;
				"binop_swapped",string_of_bool bn.binop_swapped;
				"binop_pos",Printer.s_pos bn.binop_pos;
			]
		| BinopSpecial(e,needs_assign) ->
			Printf.sprintf "BinopSpecial(%s,%b)" (se e) needs_assign

	let create_normal op e1 e2 t needs_assign swapped p =
		BinopNormal {
			binop_op = op;
			binop_lhs = e1;
			binop_rhs = e2;
			binop_type = t;
			binop_needs_assign = needs_assign;
			binop_swapped = swapped;
			binop_pos = p;
		}

	let create_special e needs_assign =
		BinopSpecial(e,needs_assign)

	let to_texpr vr br assign = match br with
		| BinopNormal bn ->
			let e1 = bn.binop_lhs in
			let e2 = bn.binop_rhs in
			let e1,e2 = match bn.binop_swapped with
				| false ->
					e1,e2
				| true ->
					let eloc1 = vr#as_var "lhs" e2 in
					let eloc2 = vr#as_var "rhs" e1 in
					eloc2,eloc1
			in
			let e = mk (TBinop(bn.binop_op,e1,e2)) bn.binop_type bn.binop_pos in
			if bn.binop_needs_assign then assign e else e
		| BinopSpecial(e,needs_assign) ->
			if needs_assign then assign e else e

	let get_type br = match br with
		| BinopNormal bn -> bn.binop_type
		| BinopSpecial(e,_) -> e.etype
end

let check_assign ctx e =
	match e.eexpr with
	| TLocal v when has_var_flag v VFinal && not (Common.ignore_error ctx.com) ->
		typing_error "Cannot assign to final" e.epos
	| TLocal {v_extra = None} | TArray _ | TField _ | TIdent _ ->
		()
	| TConst TThis | TTypeExpr _ when ctx.untyped ->
		()
	| _ ->
		if not (Common.ignore_error ctx.com) then
			invalid_assign e.epos

type type_class =
	| KInt
	| KFloat
	| KString
	| KUnk
	| KDyn
	| KOther
	| KNumParam of t
	| KStrParam of t
	| KAbstract of tabstract * t list

let rec classify t =
	match follow t with
	| TInst ({ cl_path = ([],"String") },[]) -> KString
	| TAbstract({a_impl = Some _} as a,tl) -> KAbstract (a,tl)
	| TAbstract ({ a_path = [],"Int" },[]) -> KInt
	| TAbstract ({ a_path = [],"Float" },[]) -> KFloat
	| TAbstract (a,[]) when List.exists (fun t -> match classify t with KInt | KFloat -> true | _ -> false) a.a_to -> KNumParam t
	| TInst ({ cl_kind = KTypeParameter ctl },_) when List.exists (fun t -> match classify t with KInt | KFloat -> true | _ -> false) ctl -> KNumParam t
	| TAbstract (a,[]) when List.exists (fun t -> match classify t with KString -> true | _ -> false) a.a_to -> KStrParam t
	| TInst ({ cl_kind = KTypeParameter ctl },_) when List.exists (fun t -> match classify t with KString -> true | _ -> false) ctl -> KStrParam t
	| TMono r when r.tm_type = None -> KUnk
	| TDynamic _ -> KDyn
	| _ -> KOther

(*
	We want to try unifying as an integer and apply side effects.
	However, in case the value is not a normal Monomorph but one issued
	from a Dynamic relaxation, we will instead unify with float since
	we don't want to accidentaly truncate the value
*)
let unify_int ctx e k =
	let is_dynamic t =
		match follow t with
		| TDynamic _ -> true
		| _ -> false
	in
	let is_dynamic_array t =
		match follow t with
		| TInst (_,[p]) -> is_dynamic p
		| _ -> true
	in
	let is_dynamic_field t f =
		match follow t with
		| TAnon a ->
			(try is_dynamic (PMap.find f a.a_fields).cf_type with Not_found -> false)
		| TMono m ->
			begin match Monomorph.classify_down_constraints m with
			| CStructural(fields,_) ->
				(try is_dynamic (PMap.find f fields).cf_type with Not_found -> false)
			| _ ->
				true
			end
		| TInst (c,tl) ->
			(try is_dynamic (apply_params c.cl_params tl ((let _,t,_ = Type.class_field c tl f in t))) with Not_found -> false)
		| _ ->
			true
	in
	let is_dynamic_return t =
		match follow t with
		| TFun (_,r) -> is_dynamic r
		| _ -> true
	in
	(*
		This is some quick analysis that matches the most common cases of dynamic-to-mono convertions
	*)
	let rec maybe_dynamic_mono e =
		match e.eexpr with
		| TLocal _ -> is_dynamic e.etype
		| TArray({ etype = t } as e,_) -> is_dynamic_array t || maybe_dynamic_rec e t
		| TField({ etype = t } as e,f) -> is_dynamic_field t (field_name f) || maybe_dynamic_rec e t
		| TCall({ etype = t } as e,_) -> is_dynamic_return t || maybe_dynamic_rec e t
		| TParenthesis e | TMeta(_,e) -> maybe_dynamic_mono e
		| TIf (_,a,Some b) -> maybe_dynamic_mono a || maybe_dynamic_mono b
		| _ -> false
	and maybe_dynamic_rec e t =
		match follow t with
		| TMono _ | TDynamic _ -> maybe_dynamic_mono e
		(* we might have inferenced a tmono into a single field *)
		(* TODO: check what this did exactly *)
		(* | TAnon a when !(a.a_status) = Opened -> maybe_dynamic_mono e *)
		| _ -> false
	in
	match k with
	| KUnk | KDyn when maybe_dynamic_mono e ->
		unify ctx e.etype ctx.t.tfloat e.epos;
		false
	| _ ->
		unify ctx e.etype ctx.t.tint e.epos;
		true

let make_binop ctx op e1 e2 is_assign_op with_type p =
	let tint = ctx.t.tint in
	let tfloat = ctx.t.tfloat in
	let tstring = ctx.t.tstring in
	let to_string e =
		let rec loop t = match classify t with
			| KAbstract ({a_impl = Some c},_) when PMap.mem "toString" c.cl_statics ->
				call_to_string ctx e
			| KInt | KFloat | KString -> e
			| KUnk | KDyn | KNumParam _ | KStrParam _ | KOther ->
				let std = type_type ctx ([],"Std") e.epos in
				let acc = acc_get ctx (type_field_default_cfg ctx std "string" e.epos (MCall []) with_type) e.epos in
				ignore(follow acc.etype);
				let acc = (match acc.eexpr with TField (e,FClosure (Some (c,tl),f)) -> { acc with eexpr = TField (e,FInstance (c,tl,f)) } | _ -> acc) in
				make_call ctx acc [e] ctx.t.tstring e.epos
			| KAbstract (a,tl) ->
				try
					AbstractCast.cast_or_unify_raise ctx tstring e p
				with Error (Unify _,_) ->
					loop (Abstract.get_underlying_type a tl)
		in
		loop e.etype
	in
	let mk_op e1 e2 t =
		let e1,e2 = if op = OpAdd && (classify t) = KString then
			let e1 = to_string e1 in
			let e2 = to_string e2 in
			e1,e2
		else
			e1,e2
		in
		BinopResult.create_normal op e1 e2 t is_assign_op false p
	in
	match op with
	| OpAdd ->
		mk_op e1 e2 (match classify e1.etype, classify e2.etype with
		| KInt , KInt ->
			tint
		| KFloat , KInt
		| KInt, KFloat
		| KFloat, KFloat ->
			tfloat
		| KUnk , KInt ->
			if unify_int ctx e1 KUnk then tint else tfloat
		| KUnk , KFloat
		| KUnk , KString  ->
			unify ctx e1.etype e2.etype e1.epos;
			e1.etype
		| KInt , KUnk ->
			if unify_int ctx e2 KUnk then tint else tfloat
		| KFloat , KUnk
		| KString , KUnk ->
			unify ctx e2.etype e1.etype e2.epos;
			e2.etype
		| _ , KString
		| KString , _ ->
			tstring
		| _ , KDyn ->
			e2.etype
		| KDyn , _ ->
			e1.etype
		| KUnk , KUnk ->
			let ok1 = unify_int ctx e1 KUnk in
			let ok2 = unify_int ctx e2 KUnk in
			if ok1 && ok2 then tint else tfloat
		| KNumParam t1, KNumParam t2 when Type.type_iseq t1 t2 ->
			t1
		| KNumParam t, KInt | KInt, KNumParam t ->
			t
		| KNumParam _, KFloat | KFloat, KNumParam _ | KNumParam _, KNumParam _ ->
			tfloat
		| KNumParam t, KUnk ->
			unify ctx e2.etype tfloat e2.epos;
			tfloat
		| KUnk, KNumParam t ->
			unify ctx e1.etype tfloat e1.epos;
			tfloat
		| KStrParam _, _
		| _, KStrParam _ ->
			tstring
		| KAbstract _,KFloat ->
			unify ctx e1.etype tfloat e1.epos;
			tfloat
		| KFloat, KAbstract _ ->
			unify ctx e2.etype tfloat e2.epos;
			tfloat
		| KAbstract _,KInt ->
			unify ctx e1.etype ctx.t.tint e1.epos;
			ctx.t.tint
		| KInt, KAbstract _ ->
			unify ctx e2.etype ctx.t.tint e2.epos;
			ctx.t.tint
		| KAbstract _,_
		| _,KAbstract _
		| KNumParam _, _
		| _, KNumParam _
		| KOther, _
		| _ , KOther ->
			let pr = print_context() in
			typing_error ("Cannot add " ^ s_type pr e1.etype ^ " and " ^ s_type pr e2.etype) p
		)
	| OpAnd
	| OpOr
	| OpXor
	| OpShl
	| OpShr
	| OpUShr ->
		let i = tint in
		unify ctx e1.etype i e1.epos;
		unify ctx e2.etype i e2.epos;
		mk_op e1 e2 i
	| OpMod
	| OpMult
	| OpDiv
	| OpSub ->
		let result = ref (if op = OpDiv then tfloat else tint) in
		(match classify e1.etype, classify e2.etype with
		| KFloat, KFloat ->
			result := tfloat
		| KNumParam t1, KNumParam t2 when Type.type_iseq t1 t2 ->
			if op <> OpDiv then result := t1
		| KNumParam _, KNumParam _ ->
			result := tfloat
		| KNumParam t, KInt | KInt, KNumParam t ->
			if op <> OpDiv then result := t
		| KNumParam _, KFloat | KFloat, KNumParam _ ->
			result := tfloat
		| KFloat, k ->
			ignore(unify_int ctx e2 k);
			result := tfloat
		| k, KFloat ->
			ignore(unify_int ctx e1 k);
			result := tfloat
		| k1 , k2 ->
			let ok1 = unify_int ctx e1 k1 in
			let ok2 = unify_int ctx e2 k2 in
			if not ok1 || not ok2  then result := tfloat;
		);
		mk_op e1 e2 !result
	| OpEq
	| OpNotEq ->
		let e1,e2 = try
			(* we only have to check one type here, because unification fails if one is Void and the other is not *)
			(match follow e2.etype with TAbstract({a_path=[],"Void"},_) -> typing_error "Cannot compare Void" p | _ -> ());
			AbstractCast.cast_or_unify_raise ctx e2.etype e1 p,e2
		with Error (Unify _,_) ->
			e1,AbstractCast.cast_or_unify ctx e1.etype e2 p
		in
		if not ctx.com.config.pf_supports_function_equality then begin match e1.eexpr, e2.eexpr with
		| TConst TNull , _ | _ , TConst TNull -> ()
		| _ ->
			match follow e1.etype, follow e2.etype with
			| TFun _ , _ | _, TFun _ -> warning ctx WClosureCompare "Comparison of function values is unspecified on this target, use Reflect.compareMethods instead" p
			| _ -> ()
		end;
		mk_op e1 e2 ctx.t.tbool
	| OpGt
	| OpGte
	| OpLt
	| OpLte ->
		(match classify e1.etype, classify e2.etype with
		| KInt , KInt | KInt , KFloat | KFloat , KInt | KFloat , KFloat | KString , KString -> ()
		| KInt , KUnk -> ignore(unify_int ctx e2 KUnk)
		| KFloat , KUnk | KString , KUnk -> unify ctx e2.etype e1.etype e2.epos
		| KUnk , KInt -> ignore(unify_int ctx e1 KUnk)
		| KUnk , KFloat | KUnk , KString -> unify ctx e1.etype e2.etype e1.epos
		| KUnk , KUnk ->
			ignore(unify_int ctx e1 KUnk);
			ignore(unify_int ctx e2 KUnk);
		| KDyn , KInt | KDyn , KFloat | KDyn , KString -> ()
		| KInt , KDyn | KFloat , KDyn | KString , KDyn -> ()
		| KDyn , KDyn -> ()
		| KNumParam _ , (KInt | KFloat | KNumParam _ | KDyn | KUnk ) -> ()
		| (KInt | KFloat | KDyn | KUnk ), KNumParam _ -> ()
		| KStrParam _ , (KString | KStrParam _ | KUnk | KDyn) -> ()
		| (KString | KUnk | KDyn) , KStrParam _ -> ()
		| KAbstract _,_
		| _,KAbstract _
		| KDyn , KUnk
		| KUnk , KDyn
		| KString , KInt
		| KString , KFloat
		| KInt , KString
		| KFloat , KString
		| KNumParam _ , _
		| _ , KNumParam _
		| KStrParam _ , _
		| _ , KStrParam _
		| KOther , _
		| _ , KOther ->
			let pr = print_context() in
			typing_error ("Cannot compare " ^ s_type pr e1.etype ^ " and " ^ s_type pr e2.etype) p
		);
		mk_op e1 e2 ctx.t.tbool
	| OpBoolAnd
	| OpBoolOr ->
		let b = ctx.t.tbool in
		unify ctx e1.etype b p;
		unify ctx e2.etype b p;
		mk_op e1 e2 b
	| OpInterval ->
		let t = Typeload.load_core_type ctx "IntIterator" in
		unify ctx e1.etype tint e1.epos;
		unify ctx e2.etype tint e2.epos;
		BinopSpecial (mk (TNew ((match t with TInst (c,[]) -> c | _ -> die "" __LOC__),[],[e1;e2])) t p,false)
	| OpArrow ->
		typing_error "Unexpected =>" p
	| OpIn ->
		typing_error "Unexpected in" p
	| OpNullCoal
	| OpAssign
	| OpAssignOp _ ->
		die "" __LOC__

let find_abstract_binop_overload ctx op e1 e2 a c tl left is_assign_op with_type p =
	let map = apply_params a.a_params tl in
	let make op_cf cf e1 e2 tret needs_assign swapped =
		if cf.cf_expr = None && not (has_class_field_flag cf CfExtern) then begin
			if not (Meta.has Meta.NoExpr cf.cf_meta) then Common.display_error ctx.com "Recursive operator method" p;
			if not (Meta.has Meta.CoreType a.a_meta) then begin
				(* for non core-types we require that the return type is compatible to the native result type *)
				let result = make_binop ctx op {e1 with etype = Abstract.follow_with_abstracts e1.etype} {e1 with etype = Abstract.follow_with_abstracts e2.etype} is_assign_op with_type p in
				let t_expected = BinopResult.get_type result in
				begin try
					unify_raise tret t_expected p
				with Error (Unify _,_) ->
					match follow tret with
						| TAbstract(a,tl) when type_iseq (Abstract.get_underlying_type a tl) t_expected ->
							()
						| _ ->
							let st = s_type (print_context()) in
							typing_error (Printf.sprintf "The result of this operation (%s) is not compatible with declared return type %s" (st t_expected) (st tret)) p
				end;
			end;
			(*
				If the `@:op`-field has no expr, then user wants the semantics of an underlying type.
				which is supposed to make an assignment for AssignOp's.
				Hence we use `is_assign_op` here instead of `needs_assign`.
			*)
			BinopResult.create_normal op e1 e2 tret is_assign_op swapped p
		end else if swapped then begin
			let vr = new value_reference ctx in
			let e2' = vr#as_var "lhs" e2 in
			let e1' = vr#as_var "rhs" e1 in
			let e = make_static_call ctx c cf map [e1';e2'] tret p in
			let e = vr#to_texpr e in
			BinopResult.create_special e needs_assign
		end else
			BinopResult.create_special (make_static_call ctx c cf map [e1;e2] tret p) needs_assign
	in
	(* special case for == and !=: if the second type is a monomorph, assume that we want to unify
		it with the first type to preserve comparison semantics. *)
	let is_eq_op = match op with OpEq | OpNotEq -> true | _ -> false in
	if is_eq_op then begin match follow e1.etype,follow e2.etype with
		| TMono _,_ | _,TMono _ ->
			Type.unify e1.etype e2.etype
		| _ ->
			()
	end;
	let rec loop find_op ol =
		match ol with
		| (op_cf,cf) :: ol when op_cf = find_op ->
			let is_impl = has_class_field_flag cf CfImpl in
			begin
				match follow cf.cf_type with
				| TFun((_,_,t1) :: (_,_,t2) :: pos_infos, tret) ->
					(match pos_infos with
					| [] -> ()
					| [_,true,t] when is_pos_infos t -> ()
					| _ -> die ~p:cf.cf_pos ("Unexpected arguments list of function " ^ cf.cf_name) __LOC__
					);
					let check e1 e2 swapped =
						let map_arguments () =
							let monos = Monomorph.spawn_constrained_monos (fun t -> t) cf.cf_params in
							let map t = map (apply_params cf.cf_params monos t) in
							let t1 = map t1 in
							let t2 = map t2 in
							let tret = map tret in
							monos,t1,t2,tret
						in
						let monos,t1,t2,tret = map_arguments() in
						let make e1 e2 = make op_cf cf e1 e2 tret in
						let t1 = if is_impl then Abstract.follow_with_abstracts t1 else t1 in
						let e1,e2 = if left || not left && swapped then begin
							Type.type_eq EqStrict (if is_impl then Abstract.follow_with_abstracts e1.etype else e1.etype) t1;
							e1,AbstractCast.cast_or_unify_raise ctx t2 e2 p
						end else begin
							Type.type_eq EqStrict e2.etype t2;
							AbstractCast.cast_or_unify_raise ctx t1 e1 p,e2
						end in
						let check_null e t = if is_eq_op then match e.eexpr with
							| TConst TNull when not (is_explicit_null t) -> raise (Unify_error [])
							| _ -> ()
						in
						(* If either expression is `null` we only allow operator resolving if the argument type
							is explicitly Null<T> (issue #3376) *)
						if is_eq_op then begin
							check_null e2 t2;
							check_null e1 t1;
						end;
						let needs_assign = is_assign_op && op_cf = op in
						make e1 e2 needs_assign swapped
					in
					begin try
						check e1 e2 false
					with Error (Unify _,_) | Unify_error _ -> try
						if not (Meta.has Meta.Commutative cf.cf_meta) then raise Not_found;
						check e2 e1 true
					with Not_found | Error (Unify _,_) | Unify_error _ ->
						loop find_op ol
					end
				| _ ->
					die "" __LOC__
			end
		| [] ->
			raise Not_found
		| _ :: ol ->
			loop find_op ol
	in
	let find loop =
		if left then
			loop a.a_ops
		else
			let not_impl_or_is_commutative (_, cf) =
				not (has_class_field_flag cf CfImpl) || Meta.has Meta.Commutative cf.cf_meta
			in
			loop (List.filter not_impl_or_is_commutative a.a_ops)
	in
	if is_assign_op then
		try find (loop (OpAssignOp op))
		with Not_found -> find (loop op)
	else
		find (loop op)

let try_abstract_binop_overloads ctx op e1 e2 is_assign_op with_type p =
	try
		begin match follow e1.etype with
			| TAbstract({a_impl = Some c} as a,tl) -> find_abstract_binop_overload ctx op e1 e2 a c tl true is_assign_op with_type p
			| _ -> raise Not_found
		end
	with Not_found ->
		begin match follow e2.etype with
			| TAbstract({a_impl = Some c} as a,tl) -> find_abstract_binop_overload ctx op e1 e2 a c tl false is_assign_op with_type p
			| _ -> raise Not_found
		end

let type_binop_rhs ctx op (e1 : texpr) (e2 : Ast.expr) is_assign_op wt p =
	let with_type = match op with
		| OpEq | OpNotEq | OpLt | OpLte | OpGt | OpGte -> WithType.with_type e1.etype
		| _ -> wt
	in
	type_expr ctx e2 with_type,with_type

let type_binop2 ctx op (e1 : texpr) (e2 : Ast.expr) is_assign_op with_type p =
	let e2,with_type = type_binop_rhs ctx op e1 e2 is_assign_op with_type p in
	try
		try_abstract_binop_overloads ctx op e1 e2 is_assign_op with_type p
	with Not_found ->
		make_binop ctx op e1 e2 is_assign_op with_type p

let type_assign ctx e1 e2 with_type p =
	let e1 = !type_access_ref ctx (fst e1) (snd e1) (MSet (Some e2)) with_type in
	let type_rhs with_type = type_expr ctx e2 with_type in
	let assign_to e1 =
		let e2 = type_rhs (WithType.with_type e1.etype) in
		let e2 = AbstractCast.cast_or_unify ctx e1.etype e2 p in
		check_assign ctx e1;
		(match e1.eexpr , e2.eexpr with
		| TLocal i1 , TLocal i2 when i1 == i2 -> typing_error "Assigning a value to itself" p
		| TField ({ eexpr = TConst TThis },FInstance (_,_,f1)) , TField ({ eexpr = TConst TThis },FInstance (_,_,f2)) when f1 == f2 ->
			typing_error "Assigning a value to itself" p
		| _ , _ -> ());
		mk (TBinop (OpAssign,e1,e2)) e1.etype p
	in
	match e1 with
	| AKNo(s,p) -> typing_error ("Cannot access " ^ s ^ " for writing") p
	| AKUsingField _ | AKSafeNav _ ->
		typing_error "Invalid operation" p
	| AKExpr { eexpr = TLocal { v_kind = VUser TVOLocalFunction; v_name = name } } ->
		typing_error ("Cannot access function " ^ name ^ " for writing") p
	| AKField fa ->
		let ef = FieldAccess.get_field_expr fa FWrite in
		assign_to ef
	| AKExpr e1  ->
		assign_to e1
	| AKAccessor fa ->
		let dispatcher = new call_dispatcher ctx (MSet (Some e2)) with_type p in
		dispatcher#accessor_call fa [] [e2]
	| AKAccess(a,tl,c,ebase,ekey) ->
		let e2 = type_rhs WithType.value in
		mk_array_set_call ctx (AbstractCast.find_array_write_access ctx a tl ekey e2 p) c ebase p
	| AKResolve(sea,name) ->
		let eparam = sea.se_this in
		let e_name = Texpr.Builder.make_string ctx.t name null_pos in
		(new call_dispatcher ctx (MCall [e2]) with_type p)#field_call sea.se_access [eparam;e_name] [e2]
	| AKUsingAccessor sea ->
		let fa_set = match FieldAccess.resolve_accessor sea.se_access (MSet (Some e2)) with
			| AccessorFound fa -> fa
			| _ -> typing_error "Could not resolve accessor" p
		in
		let dispatcher = new call_dispatcher ctx (MCall [e2]) with_type p in
		dispatcher#field_call fa_set [sea.se_this] [e2]

let type_non_assign_op ctx op e1 e2 is_assign_op abstract_overload_only with_type p =
	(* If the with_type is an abstract which has exactly one applicable @:op method, we can promote it
		to the individual arguments (issue #2786). *)
	let wt = match with_type with
		| WithType.WithType(t,_) ->
			begin match follow t with
				| TAbstract(a,_) ->
					begin match List.filter (fun (o,_) -> o = OpAssignOp(op) || o == op) a.a_ops with
						| [_] -> with_type
						| _ -> WithType.value
					end
				| _ ->
					WithType.value
			end
		| _ ->
			WithType.value
	in
	let e1 = type_expr ctx e1 wt in
	let result = if abstract_overload_only then begin
		let e2,with_type = type_binop_rhs ctx op e1 e2 is_assign_op with_type p in
		try_abstract_binop_overloads ctx op e1 e2 is_assign_op with_type p
	end else
		type_binop2 ctx op e1 e2 is_assign_op wt p
	in
	let vr = new value_reference ctx in
	let e = BinopResult.to_texpr vr result (fun _ -> raise Not_found) in
	vr#to_texpr e

let process_lhs_expr ctx name e_lhs =
	let vr = new value_reference ctx in
	let e = vr#get_expr name e_lhs in
	e,vr

let type_assign_op ctx op e1 e2 with_type p =
	let field_rhs_by_name op name ev with_type =
		let access_get = type_field_default_cfg ctx ev name p MGet with_type in
		let e_get = acc_get ctx access_get p in
		e_get.etype,type_binop2 ctx op e_get e2 true WithType.value p
	in
	let field_rhs op cf ev =
		field_rhs_by_name op cf.cf_name ev (WithType.with_type cf.cf_type)
	in
	let assign vr e r_rhs =
		check_assign ctx e;
		let assign e_rhs =
			let e_rhs = AbstractCast.cast_or_unify ctx e.etype e_rhs p in
			match e_rhs.eexpr with
			| TBinop(op',e1',e2') when op = op' && Texpr.equal e e1' ->
				mk (TBinop(OpAssignOp op',e1',e2')) e.etype p
			| _ ->
				mk (TBinop(OpAssign,e,e_rhs)) e.etype p
		in
		let e = BinopResult.to_texpr vr r_rhs assign in
		vr#to_texpr e
	in
	let set vr fa t_lhs r_rhs el =
		let assign e_rhs =
			let e_rhs = AbstractCast.cast_or_unify ctx t_lhs e_rhs p in
			let dispatcher = new call_dispatcher ctx (MSet (Some e2)) with_type p in
			dispatcher#accessor_call fa (el @ [e_rhs]) [];
		in
		let e = BinopResult.to_texpr vr r_rhs assign in
		vr#to_texpr e
	in
	(match !type_access_ref ctx (fst e1) (snd e1) (MSet (Some e2)) with_type with
	| AKNo(s,p) ->
		(* try abstract operator overloading *)
		(try type_non_assign_op ctx op e1 e2 true true with_type p
		with Not_found -> typing_error ("Cannot access " ^ s ^ " for writing") p
		)
	| AKUsingField _ | AKSafeNav _ ->
		typing_error "Invalid operation" p
	| AKExpr e ->
		let e,vr = process_lhs_expr ctx "lhs" e in
		let e_rhs = type_binop2 ctx op e e2 true WithType.value p in
		assign vr e e_rhs
	| AKField fa ->
		let vr = new value_reference ctx in
		let ef = vr#get_expr_part "fh" fa.fa_on in
		let _,e_rhs = field_rhs op fa.fa_field ef in
		let e_lhs = FieldAccess.get_field_expr {fa with fa_on = ef} FWrite in
		assign vr e_lhs e_rhs
	| AKAccessor fa ->
		let vr = new value_reference ctx in
		let ef = vr#get_expr_part "fh" fa.fa_on in
		let t_lhs,e_rhs = field_rhs op fa.fa_field ef in
		set vr {fa with fa_on = ef} t_lhs e_rhs []
	| AKUsingAccessor sea ->
		let fa = sea.se_access in
		let ef,vr = process_lhs_expr ctx "fh" sea.se_this in
		let t_lhs,e_rhs = field_rhs op fa.fa_field ef in
		set vr sea.se_access t_lhs e_rhs [ef]
	| AKAccess(a,tl,c,ebase,ekey) ->
		let cf_get,tf_get,r_get,ekey = AbstractCast.find_array_read_access ctx a tl ekey p in
		(* bind complex keys to a variable so they do not make it into the output twice *)
		let save = save_locals ctx in
		let maybe_bind_to_temp e = match Optimizer.make_constant_expression ctx e with
			| Some e -> e,None
			| None ->
				let v = gen_local ctx e.etype p in
				let e' = mk (TLocal v) e.etype p in
				e', Some (mk (TVar (v,Some e)) ctx.t.tvoid p)
		in
		let ekey,ekey' = maybe_bind_to_temp ekey in
		let ebase,ebase' = maybe_bind_to_temp ebase in
		let eget = mk_array_get_call ctx (cf_get,tf_get,r_get,ekey) c ebase p in
		let eget = type_binop2 ctx op eget e2 true WithType.value p in
		let vr = new value_reference ctx in
		let eget = BinopResult.to_texpr vr eget (fun e -> e) in
		unify ctx eget.etype r_get p;
		let cf_set,tf_set,r_set,ekey,eget = AbstractCast.find_array_write_access ctx a tl ekey eget p in
		let et = type_module_type ctx (TClassDecl c) None p in
		let e = match cf_set.cf_expr,cf_get.cf_expr with
			| None,None ->
				let ea = mk (TArray(ebase,ekey)) r_get p in
				mk (TBinop(OpAssignOp op,ea,type_expr ctx e2 (WithType.with_type r_get))) r_set p
			| Some _,Some _ ->
				let ef_set = mk (TField(et,(FStatic(c,cf_set)))) tf_set p in
				let el = [make_call ctx ef_set [ebase;ekey;eget] r_set p] in
				let el = match ebase' with None -> el | Some ebase -> ebase :: el in
				let el = match ekey' with None -> el | Some ekey -> ekey :: el in
				begin match el with
					| [e] -> e
					| el -> mk (TBlock el) r_set p
				end
			| _ ->
				typing_error "Invalid array access getter/setter combination" p
		in
		save();
		vr#to_texpr	e
	| AKResolve(sea,name) ->
		let e,vr = process_lhs_expr ctx "fh" sea.se_this in
		let t_lhs,r_rhs = field_rhs_by_name op name e WithType.value in
		let assign e_rhs =
			let e_name = Texpr.Builder.make_string ctx.t name null_pos in
			(new call_dispatcher ctx (MCall [e2]) with_type p)#field_call sea.se_access [sea.se_this;e_name;e_rhs] []
		in
		let e = BinopResult.to_texpr vr r_rhs assign in
		vr#to_texpr e
	)

let type_binop ctx op e1 e2 is_assign_op with_type p =
	match op with
	| OpAssign ->
		type_assign ctx e1 e2 with_type p
	| OpAssignOp (OpBoolAnd | OpBoolOr) ->
		typing_error "The operators ||= and &&= are not supported" p
	| OpAssignOp op ->
		type_assign_op ctx op e1 e2 with_type p
	| _ ->
		try
			type_non_assign_op ctx op e1 e2 is_assign_op false with_type p
		with Not_found ->
			let op = if is_assign_op then OpAssignOp op else op in
			die ~p ("Failed to type binary operation " ^ (s_binop op)) __LOC__


let type_unop ctx op flag e with_type p =
	let try_abstract_unop_overloads e = match follow e.etype with
		| TAbstract ({a_impl = Some c} as a,tl) ->
			let rec loop opl = match opl with
				| [] ->
					raise Not_found
				| (op2,flag2,cf) :: opl when op == op2 && flag == flag2 ->
					let sea = if has_class_field_flag cf CfImpl then
						make_abstract_static_extension_access a tl c cf e false p
					else
						make_static_extension_access c cf e false p
					in
					begin try
						unify_field_call ctx sea.se_access [sea.se_this] [] p false
					with Error _ ->
						loop opl
					end
				| (_,_,cf) :: opl ->
					loop opl
			in
			let fcc = loop a.a_unops in
			ignore(follow fcc.fc_field.cf_type);
			begin match fcc.fc_field.cf_expr with
			| None ->
				mk (TUnop(op,flag,e)) fcc.fc_ret p
			| Some _ ->
				fcc.fc_data()
			end
		| _ ->
			raise Not_found
	in
	let unexpected_spread p =
		typing_error "Spread unary operator is only allowed for unpacking the last argument in a call with rest arguments" p
	in
	let make e =
		let check_int () =
			match classify e.etype with
			| KFloat -> ctx.t.tfloat
			| KNumParam t ->
				unify ctx e.etype ctx.t.tfloat e.epos;
				t
			| k ->
				if unify_int ctx e k then ctx.t.tint else ctx.t.tfloat
		in
		let t = match op with
			| Not ->
				if flag = Postfix then Common.display_error ctx.com "Postfix ! is not supported" p;
				unify ctx e.etype ctx.t.tbool e.epos;
				ctx.t.tbool
			| NegBits ->
				unify ctx e.etype ctx.t.tint e.epos;
				ctx.t.tint
			| Increment
			| Decrement ->
				check_assign ctx e;
				check_int()
			| Neg ->
				check_int()
			| Spread ->
				unexpected_spread p
		in
		mk (TUnop (op,flag,e)) t p
	in
	let find_overload_or_make e =
		try
			try_abstract_unop_overloads e
		with Not_found ->
			make e
	in
	match op with
	| Spread ->
		unexpected_spread p
	| Not | Neg | NegBits ->
		let access_get = !type_access_ref ctx (fst e) (snd e) MGet WithType.value (* WITHTYPETODO *) in
		let e = acc_get ctx access_get p in
		find_overload_or_make e
	| Increment | Decrement ->
		let binop = if op = Increment then OpAdd else OpSub in
		let e_one = mk (TConst (TInt Int32.one)) ctx.t.tint p in
		let maybe_tempvar_postfix vr e_lhs =
			if flag = Postfix && with_type <> WithType.no_value then begin
				let e_lhs = vr#get_expr "lhs" e_lhs in
				e_lhs,Some (vr#as_var "postfix" e_lhs)
			end else
				e_lhs,None
		in
		let read_on vr ef fa =
			let access_get = type_field_default_cfg ctx ef fa.fa_field.cf_name p MGet WithType.value in
			let e_lhs = acc_get ctx access_get p in
			let e_lhs,e_out = maybe_tempvar_postfix vr e_lhs in
			e_lhs,e_out
		in
		let generate vr e_out e = match e_out with
			| None -> vr#to_texpr e
			| Some e' -> vr#to_texpr_el [e] e'
		in
		let access_set = !type_access_ref ctx (fst e) (snd e) (MSet None) WithType.value (* WITHTYPETODO *) in
		match access_set with
		| AKNo(s,p) ->
			typing_error ("Cannot access " ^ s ^ " for writing") p
		| AKExpr e ->
			find_overload_or_make e
		| AKField fa ->
			let vr = new value_reference ctx in
			let ef = vr#get_expr_part "fh" fa.fa_on in
			let access_get = type_field_default_cfg ctx ef fa.fa_field.cf_name p MGet WithType.value in
			let e,e_out = match access_get with
			| AKField _ ->
				let e = FieldAccess.get_field_expr {fa with fa_on = ef} FGet in
				find_overload_or_make e,None
			| _ ->
				let e_set = FieldAccess.get_field_expr {fa with fa_on = ef} FWrite in
				let e_lhs = acc_get ctx access_get p in
				let e_lhs,e_out = maybe_tempvar_postfix vr e_lhs in
				let e_op = mk (TBinop(binop,e_lhs,e_one)) e_lhs.etype p in
				mk (TBinop(OpAssign,e_set,e_op)) e_set.etype p,e_out
			in
			generate vr e_out e
		| AKAccessor fa ->
			let vr = new value_reference ctx in
			let ef = vr#get_expr_part "fh" fa.fa_on in
			let fa = {fa with fa_on = ef} in
			let e_lhs,e_out = read_on vr ef fa in
			let e_op = mk (TBinop(binop,e_lhs,e_one)) e_lhs.etype p in
			let dispatcher = new call_dispatcher ctx (MSet None) WithType.value p in
			let e = dispatcher#accessor_call fa [e_op] [] in
			generate vr e_out e
		| AKUsingAccessor sea ->
			let ef,vr = process_lhs_expr ctx "fh" sea.se_this in
			let e_lhs,e_out = read_on vr ef sea.se_access in
			let e_op = mk (TBinop(binop,e_lhs,e_one)) e_lhs.etype p in
			let dispatcher = new call_dispatcher ctx (MSet None) WithType.value p in
			let e = dispatcher#accessor_call sea.se_access [ef;e_op] [] in
			generate vr e_out e
		| AKAccess(a,tl,c,ebase,ekey) ->
			begin try
				(match op with Increment | Decrement -> () | _ -> raise Not_found);
				let v_key = alloc_var VGenerated "tmp" ekey.etype ekey.epos in
				let evar_key = mk (TVar(v_key,Some ekey)) ctx.com.basic.tvoid ekey.epos in
				let ekey = mk (TLocal v_key) ekey.etype ekey.epos in
				(* get *)
				let e_get = mk_array_get_call ctx (AbstractCast.find_array_read_access_raise ctx a tl ekey p) c ebase p in
				let v_get = alloc_var VGenerated "tmp" e_get.etype e_get.epos in
				let ev_get = mk (TLocal v_get) v_get.v_type p in
				let evar_get = mk (TVar(v_get,Some e_get)) ctx.com.basic.tvoid p in
				(* op *)
				let e_one = mk (TConst (TInt (Int32.of_int 1))) ctx.com.basic.tint p in
				let e_op = mk (TBinop((if op = Increment then OpAdd else OpSub),ev_get,e_one)) ev_get.etype p in
				(* set *)
				let e_set = mk_array_set_call ctx (AbstractCast.find_array_write_access_raise ctx a tl ekey e_op p) c ebase p in
				let el = evar_key :: evar_get :: e_set :: (if flag = Postfix then [ev_get] else []) in
				mk (TBlock el) e_set.etype p
			with Not_found ->
				let e = mk_array_get_call ctx (AbstractCast.find_array_read_access ctx a tl ekey p) c ebase p in
				find_overload_or_make e
			end
		| AKUsingField _ | AKResolve _ | AKSafeNav _ ->
			typing_error "Invalid operation" p
