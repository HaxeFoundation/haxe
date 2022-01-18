open Globals
open TType
open TFunctions
open TPrinting

type unify_error =
	| Cannot_unify of t * t
	| Invalid_field_type of string
	| Has_no_field of t * string
	| Has_no_runtime_field of t * string
	| Has_extra_field of t * string
	| Invalid_kind of string * field_kind * field_kind
	| Invalid_visibility of string
	| Not_matching_optional of string
	| Cant_force_optional
	| Invariant_parameter of int
	| Constraint_failure of string
	| Missing_overload of tclass_field * t
	| FinalInvariance (* nice band name *)
	| Invalid_function_argument of int (* index *) * int (* total *)
	| Invalid_return_type
	| Unify_custom of string

exception Unify_error of unify_error list

type eq_kind =
	| EqStrict
	| EqCoreType
	| EqRightDynamic
	| EqBothDynamic
	| EqDoNotFollowNull (* like EqStrict, but does not follow Null<T> *)

type unification_context = {
	allow_transitive_cast : bool;
	allow_abstract_cast   : bool; (* allows a non-transitive abstract cast (from,to,@:from,@:to) *)
	allow_dynamic_to_cast : bool; (* allows a cast from dynamic to non-dynamic *)
	equality_kind         : eq_kind;
	equality_underlying   : bool;
}

type unify_min_result =
	| UnifyMinOk of t
	| UnifyMinError of unify_error list * int

let error l = raise (Unify_error l)

let check_constraint name f =
	try
		f()
	with Unify_error l ->
		raise (Unify_error ((Constraint_failure name) :: l))

let unify_ref : (unification_context -> t -> t -> unit) ref = ref (fun _ _ _ -> ())
let unify_min_ref : (unification_context -> t -> t list -> unify_min_result) ref = ref (fun _ _ _ -> assert false)

let default_unification_context = {
	allow_transitive_cast = true;
	allow_abstract_cast   = true;
	allow_dynamic_to_cast = true;
	equality_kind         = EqStrict;
	equality_underlying   = false;
}

module Monomorph = struct
	let create () = {
		tm_type = None;
		tm_down_constraints = [];
		tm_up_constraints = []
	}

	(* constraining *)

	let add_up_constraint m ((t,name) as constr) =
		m.tm_up_constraints <- constr :: m.tm_up_constraints;
		match t with
		| TMono m2 -> m2.tm_down_constraints <- MMono (m,name) :: m2.tm_down_constraints
		| _ -> ()

	let add_down_constraint m constr =
		m.tm_down_constraints <- constr :: m.tm_down_constraints;
		match constr with
		| MMono (m2,s) -> m2.tm_up_constraints <- (TMono m,s) :: m.tm_up_constraints
		| _ -> ()

	let constraint_of_type name t = match follow t with
		| TMono m2 ->
			[MMono(m2,name)]
		| TAnon an when not (PMap.is_empty an.a_fields) ->
			PMap.fold (fun cf l ->
				(MField cf) :: l
			) an.a_fields []
		| TAnon _ ->
			[MEmptyStructure]
		| _ ->
			[MType(t,name)]

	let constrain_to_type m name t =
		List.iter (add_down_constraint m) (constraint_of_type name t)

	(* Note: This function is called by printing and others and should thus not modify state. *)

	let rec classify_down_constraints' m =
		let types = DynArray.create () in
		let fields = ref PMap.empty in
		let is_open = ref false in
		let monos = ref [] in
		let rec check constr = match constr with
			| MMono(m2,name) ->
				begin match m2.tm_type with
				| None ->
					let more_monos,kind = classify_down_constraints' m2 in
					monos := !monos @ more_monos;
					begin match kind with
					| CUnknown ->
						(* Collect unconstrained monomorphs because we have to bind them. *)
						monos := m2 :: !monos;
					| _ ->
						(* Recursively inherit constraints. *)
						List.iter check m2.tm_down_constraints
					end
				| Some t ->
					List.iter (fun constr -> check constr) (constraint_of_type name t)
				end;
			| MField cf ->
				fields := PMap.add cf.cf_name cf !fields;
			| MType(t2,name) ->
				DynArray.add types (t2,name)
			| MOpenStructure
			| MEmptyStructure ->
				is_open := true
		in
		List.iter check m.tm_down_constraints;
		let kind =
			let k1 =
				if DynArray.length types > 0 then
					CTypes (DynArray.to_list types)
				else
					CUnknown
			in
			if not (PMap.is_empty !fields) || !is_open then
				let k2 = CStructural(!fields,!is_open) in
				match k1 with
				| CTypes _ -> CMixed [k1; k2]
				| _ -> k2
			else
				k1
		in
		!monos,kind

	let classify_down_constraints m = snd (classify_down_constraints' m)

	let rec check_down_constraints constr t =
		match constr with
		| CUnknown ->
			()
		| CTypes tl ->
			List.iter (fun (t2,name) ->
				let f () = (!unify_ref) default_unification_context t t2 in
				match name with
				| Some name -> check_constraint name f
				| None -> f()
			) tl
		| CStructural(fields,is_open) ->
			let t2 = mk_anon ~fields (ref Closed) in
			(!unify_ref) default_unification_context t t2
		| CMixed l ->
			List.iter (fun constr -> check_down_constraints constr t) l

	let rec collect_up_constraints m =
		let rec collect m acc =
			List.fold_left (fun acc (t,name) ->
				match t with
				| TMono m2 ->
					(match m2.tm_type with
					| Some t ->
						(match follow t with
						| TMono _ -> acc
						| _ -> (t,name) :: acc)
					| None -> collect m2 acc
					)
				| _ -> (t,name) :: acc
			) acc m.tm_up_constraints
		in
		collect m []

	let check_up_constraints m t =
		List.iter (fun (t2,constraint_name) ->
			let check() =
				(!unify_ref) default_unification_context t2 t
			in
			match constraint_name with
			| Some name -> check_constraint name check
			| None -> check()
		) (collect_up_constraints m)

	(* binding *)

	let do_bind m t =
		(* assert(m.tm_type = None); *) (* TODO: should be here, but matcher.ml does some weird bind handling at the moment. *)
		m.tm_type <- Some t;
		m.tm_down_constraints <- [];
		m.tm_up_constraints <- []

	let rec bind m t =
		begin match t with
		| TAnon _ when List.mem MOpenStructure m.tm_down_constraints ->
			(* If we assign an open structure monomorph to another structure, the semantics want us to merge the
			   fields. This is kinda weird, but that's how it has always worked. *)
			constrain_to_type m None t;
			ignore(close m)
		| TMono m2 ->
			if m != m2 then begin match m2.tm_type with
			| None ->
				List.iter (add_down_constraint m2) m.tm_down_constraints;
				List.iter (add_up_constraint m2) m.tm_up_constraints;
				do_bind m t;
			| Some t ->
				bind m t
			end
		| _ ->
			check_up_constraints m t;
			(* Due to recursive constraints like in #9603, we tentatively bind the monomorph to the type we're checking
			   against before checking the constraints. *)
			m.tm_type <- Some t;
			let kind = classify_down_constraints m in
			Std.finally (fun () -> m.tm_type <- None) (fun () -> check_down_constraints kind t) ();
			do_bind m t
		end

	and close m = match m.tm_type with
		| Some _ ->
			()
		| None -> match classify_down_constraints m with
			| CUnknown ->
				()
			| CTypes [(t,_)] ->
				do_bind m t;
				()
			| CTypes _ | CMixed _ ->
				()
			| CStructural(fields,_) ->
				let check_recursion cf =
					let rec loop t = match t with
					| TMono m2 when m == m2 ->
						let pctx = print_context() in
						let s = Printf.sprintf "%s appears in { %s: %s }" (s_type pctx t) cf.cf_name (s_type pctx cf.cf_type) in
						raise (Unify_error [Unify_custom "Recursive type";Unify_custom s]);
					| _ ->
						TFunctions.map loop t
					in
					ignore(loop cf.cf_type);
				in
				(* We found a bunch of fields but no type, create a merged structure type and bind to that *)
				PMap.iter (fun _ cf -> check_recursion cf) fields;
				do_bind m (mk_anon ~fields (ref Closed));
				()

	let unbind m =
		m.tm_type <- None

	let spawn_constrained_monos map params =
		let checks = DynArray.create () in
		let monos = List.map (fun tp ->
			let mono = create () in
			begin match follow tp.ttp_type with
				| TInst ({ cl_kind = KTypeParameter constr; cl_path = path },_) when constr <> [] ->
					DynArray.add checks (mono,constr,s_type_path path)
				| _ ->
					()
			end;
			TMono mono
		) params in
		let map t = map (apply_params params monos t) in
		DynArray.iter (fun (mono,constr,path) ->
			List.iter (fun t -> constrain_to_type mono (Some path) (map t)) constr
		) checks;
		monos

end

let rec follow_and_close t = match follow t with
	| TMono r as t ->
		Monomorph.close r;
		if r.tm_type <> None then follow_and_close t
		else t
	| t ->
		t

let rec link e a b =
	(* tell if setting a == b will create a type-loop *)
	let rec loop t =
		if t == a then
			true
		else match t with
		| TMono t -> (match t.tm_type with None -> false | Some t -> loop t)
		| TEnum (_,tl) -> List.exists loop tl
		| TInst (_,tl) | TType (_,tl) | TAbstract (_,tl) -> List.exists loop tl
		| TFun (tl,t) -> List.exists (fun (_,_,t) -> loop t) tl || loop t
		| TDynamic t2 ->
			if t == t2 then
				false
			else
				loop t2
		| TLazy f ->
			loop (lazy_type f)
		| TAnon a ->
			try
				PMap.iter (fun _ f -> if loop f.cf_type then raise Exit) a.a_fields;
				false
			with
				Exit -> true
	in
	(* tell is already a ~= b *)
	if loop b then
		(follow b) == a
	else if b == t_dynamic then
		true
	else begin
		Monomorph.bind e b;
		true
	end

let link_dynamic a b = match follow a,follow b with
	| TMono r,TDynamic _ -> Monomorph.bind r b
	| TDynamic _,TMono r -> Monomorph.bind r a
	| _ -> ()

let fast_eq_check type_param_check a b =
	if a == b then
		true
	else match a , b with
	| TFun (l1,r1) , TFun (l2,r2) when List.length l1 = List.length l2 ->
		List.for_all2 (fun (_,_,t1) (_,_,t2) -> type_param_check t1 t2) l1 l2 && type_param_check r1 r2
	| TType (t1,l1), TType (t2,l2) ->
		t1 == t2 && List.for_all2 type_param_check l1 l2
	| TEnum (e1,l1), TEnum (e2,l2) ->
		e1 == e2 && List.for_all2 type_param_check l1 l2
	| TInst (c1,l1), TInst (c2,l2) ->
		c1 == c2 && List.for_all2 type_param_check l1 l2
	| TAbstract (a1,l1), TAbstract (a2,l2) ->
		a1 == a2 && List.for_all2 type_param_check l1 l2
	| _ , _ ->
		false

let rec fast_eq a b = fast_eq_check fast_eq a b

let fast_eq_pair (a,b) (a',b') = fast_eq a a' && fast_eq b b'

let rec fast_eq_unbound_mono a b =
	match a, b with
	| TMono { tm_type = None }, TMono { tm_type = None } -> true
	| _ -> fast_eq_check fast_eq_unbound_mono a b

let rec fast_eq_mono ml a b =
	if fast_eq_check (fast_eq_mono ml) a b then
		true
	else match a , b with
	| TMono _, _ ->
		List.memq a ml
	| _ , _ ->
		false

let rec shallow_eq a b =
	a == b
	|| begin
		let a = follow a
		and b = follow b in
		fast_eq_check shallow_eq a b
		|| match a , b with
			| t, TMono { tm_type = None } when t == t_dynamic -> true
			| TMono { tm_type = None }, t when t == t_dynamic -> true
			| TMono { tm_type = None }, TMono { tm_type = None } -> true
			| TAnon a1, TAnon a2 ->
				let fields_eq() =
					let rec loop fields1 fields2 =
						match fields1, fields2 with
						| [], [] -> true
						| _, [] | [], _ -> false
						| f1 :: rest1, f2 :: rest2 ->
							f1.cf_name = f2.cf_name
							&& (try shallow_eq f1.cf_type f2.cf_type with Not_found -> false)
							&& loop rest1 rest2
					in
					let fields1 = PMap.fold (fun field fields -> field :: fields) a1.a_fields []
					and fields2 = PMap.fold (fun field fields -> field :: fields) a2.a_fields []
					and sort_compare f1 f2 = compare f1.cf_name f2.cf_name in
					loop (List.sort sort_compare fields1) (List.sort sort_compare fields2)
				in
				(match !(a2.a_status), !(a1.a_status) with
				| Statics c, Statics c2 -> c == c2
				| EnumStatics e, EnumStatics e2 -> e == e2
				| AbstractStatics a, AbstractStatics a2 -> a == a2
				| Extend tl1, Extend tl2 -> fields_eq() && List.for_all2 shallow_eq tl1 tl2
				| Closed, Closed -> fields_eq()
				| Const, Const -> fields_eq()
				| _ -> false
				)
			| _ , _ ->
				false
	end

(* perform unification with subtyping.
   the first type is always the most down in the class hierarchy
   it's also the one that is pointed by the position.
   It's actually a typecheck of  A :> B where some mutations can happen *)

let cannot_unify a b = Cannot_unify (a,b)
let invalid_field n = Invalid_field_type n
let invalid_kind n a b = Invalid_kind (n,a,b)
let invalid_visibility n = Invalid_visibility n
let has_no_field t n = Has_no_field (t,n)
let has_extra_field t n = Has_extra_field (t,n)

(*
	we can restrict access as soon as both are runtime-compatible
*)
let unify_access a1 a2 =
	a1 = a2 || match a1, a2 with
	| _, AccNo | _, AccNever -> true
	| AccInline, AccNormal -> true
	| _ -> false

let direct_access = function
	| AccNo | AccNever | AccNormal | AccInline | AccRequire _ | AccCtor -> true
	| AccCall -> false

let unify_kind k1 k2 =
	k1 = k2 || match k1, k2 with
		| Var v1, Var v2 -> unify_access v1.v_read v2.v_read && unify_access v1.v_write v2.v_write
		| Var v, Method m ->
			(match v.v_read, v.v_write, m with
			| AccNormal, _, MethNormal -> true
			| AccNormal, AccNormal, MethDynamic -> true
			| _ -> false)
		| Method m, Var v ->
			(match m with
			| MethDynamic -> direct_access v.v_read && direct_access v.v_write
			| MethMacro -> false
			| MethNormal | MethInline ->
				match v.v_read,v.v_write with
				| AccNormal,(AccNo | AccNever) -> true
				| _ -> false)
		| Method m1, Method m2 ->
			match m1,m2 with
			| MethInline, MethNormal
			| MethDynamic, MethNormal -> true
			| _ -> false

type 'a rec_stack = {
	mutable rec_stack : 'a list;
}

let new_rec_stack() = { rec_stack = [] }
let rec_stack_exists f s = List.exists f s.rec_stack
let rec_stack_memq v s = List.memq v s.rec_stack
let rec_stack_loop stack value f arg =
	stack.rec_stack <- value :: stack.rec_stack;
	try
		let r = f arg in
		stack.rec_stack <- List.tl stack.rec_stack;
		r
	with e ->
		stack.rec_stack <- List.tl stack.rec_stack;
		raise e

let eq_stack = new_rec_stack()

let rec_stack stack value fcheck frun ferror =
	if not (rec_stack_exists fcheck stack) then begin
		try
			stack.rec_stack <- value :: stack.rec_stack;
			let v = frun() in
			stack.rec_stack <- List.tl stack.rec_stack;
			v
		with
			Unify_error l ->
				stack.rec_stack <- List.tl stack.rec_stack;
				ferror l
			| e ->
				stack.rec_stack <- List.tl stack.rec_stack;
				raise e
	end

let rec_stack_default stack value fcheck frun def =
	if not (rec_stack_exists fcheck stack) then rec_stack_loop stack value frun () else def

let rec type_eq uctx a b =
	let param = uctx.equality_kind in
	let can_follow t = match param with
		| EqCoreType -> false
		| EqDoNotFollowNull -> not (is_explicit_null t)
		| _ -> true
	in
	let can_follow_abstract ab = uctx.equality_underlying && match ab.a_this with
		| TAbstract (ab',_) -> ab' != ab
		| _ -> true
	in
	if a == b then
		()
	else match a , b with
	| TLazy f , _ -> type_eq uctx (lazy_type f) b
	| _ , TLazy f -> type_eq uctx a (lazy_type f)
	| TMono t , _ ->
		(match t.tm_type with
		| None -> if param = EqCoreType || not (link t a b) then error [cannot_unify a b]
		| Some t -> type_eq uctx t b)
	| _ , TMono t ->
		(match t.tm_type with
		| None -> if param = EqCoreType || not (link t b a) then error [cannot_unify a b]
		| Some t -> type_eq uctx a t)
	| TDynamic a , TDynamic b ->
		type_eq uctx a b
	| _ , _ when a == t_dynamic && param = EqBothDynamic ->
		()
	| _ , _ when b == t_dynamic && (param = EqRightDynamic || param = EqBothDynamic) ->
		()
	| TAbstract ({a_path=[],"Null"},[t1]),TAbstract ({a_path=[],"Null"},[t2]) ->
		type_eq uctx t1 t2
	| TAbstract ({a_path=[],"Null"},[t]),_ when param <> EqDoNotFollowNull ->
		type_eq uctx t b
	| _,TAbstract ({a_path=[],"Null"},[t]) when param <> EqDoNotFollowNull ->
		type_eq uctx a t
	| TType (t1,tl1), TType (t2,tl2) when (t1 == t2 || (param = EqCoreType && t1.t_path = t2.t_path)) && List.length tl1 = List.length tl2 ->
		type_eq_params uctx a b tl1 tl2
	| TType (t,tl) , _ when can_follow a ->
		rec_stack eq_stack (a,b) (fast_eq_pair (a,b))
			(fun() -> try_apply_params_rec t.t_params tl t.t_type (fun a -> type_eq uctx a b))
			(fun l -> error (cannot_unify a b :: l))
	| _ , TType (t,tl) when can_follow b ->
		rec_stack eq_stack (a,b) (fast_eq_pair (a,b))
			(fun() -> try_apply_params_rec t.t_params tl t.t_type (type_eq uctx a))
			(fun l -> error (cannot_unify a b :: l))
	| TEnum (e1,tl1) , TEnum (e2,tl2) ->
		if e1 != e2 && not (param = EqCoreType && e1.e_path = e2.e_path) then error [cannot_unify a b];
		type_eq_params uctx a b tl1 tl2
	| TInst (c1,tl1) , TInst (c2,tl2) ->
		if c1 != c2 && not (param = EqCoreType && c1.cl_path = c2.cl_path) && (match c1.cl_kind, c2.cl_kind with KExpr _, KExpr _ -> false | _ -> true) then error [cannot_unify a b];
		type_eq_params uctx a b tl1 tl2
	| TFun (l1,r1) , TFun (l2,r2) when List.length l1 = List.length l2 ->
		let i = ref 0 in
		(try
			type_eq uctx r1 r2;
			List.iter2 (fun (n,o1,t1) (_,o2,t2) ->
				incr i;
				if o1 <> o2 then error [Not_matching_optional n];
				type_eq uctx t1 t2
			) l1 l2
		with
			Unify_error l ->
				let msg = if !i = 0 then Invalid_return_type else Invalid_function_argument(!i,List.length l1) in
				error (cannot_unify a b :: msg :: l)
		)
	| TAbstract (a1,tl1) , TAbstract (a2,tl2) when a1 == a2 || (param = EqCoreType && a1.a_path = a2.a_path) ->
		type_eq_params uctx a b tl1 tl2
	| TAbstract (ab,tl) , _ when can_follow_abstract ab ->
		type_eq uctx (apply_params ab.a_params tl ab.a_this) b
	| _ , TAbstract (ab,tl) when can_follow_abstract ab ->
		type_eq uctx a (apply_params ab.a_params tl ab.a_this)
	| TAnon a1, TAnon a2 ->
		(try
			(match !(a2.a_status) with
			| Statics c -> (match !(a1.a_status) with Statics c2 when c == c2 -> () | _ -> error [])
			| EnumStatics e -> (match !(a1.a_status) with EnumStatics e2 when e == e2 -> () | _ -> error [])
			| AbstractStatics a -> (match !(a1.a_status) with AbstractStatics a2 when a == a2 -> () | _ -> error [])
			| _ -> ()
			);
			PMap.iter (fun n f1 ->
				try
					let f2 = PMap.find n a2.a_fields in
					if f1.cf_kind <> f2.cf_kind && (param = EqStrict || param = EqCoreType || not (unify_kind f1.cf_kind f2.cf_kind)) then error [invalid_kind n f1.cf_kind f2.cf_kind];
					let a = f1.cf_type and b = f2.cf_type in
					(try type_eq uctx a b with Unify_error l -> error (invalid_field n :: l));
					if (has_class_field_flag f1 CfPublic) != (has_class_field_flag f2 CfPublic) then error [invalid_visibility n];
				with
					Not_found ->
						error [has_no_field b n];
			) a1.a_fields;
			PMap.iter (fun n f2 ->
				if not (PMap.mem n a1.a_fields) then begin
					error [has_no_field a n];
				end;
			) a2.a_fields;
		with
			Unify_error l -> error (cannot_unify a b :: l))
	| _ , _ ->
		error [cannot_unify a b]

and type_eq_params uctx a b tl1 tl2 =
	let i = ref 0 in
	List.iter2 (fun t1 t2 ->
		incr i;
		try
			type_eq uctx t1 t2
		with Unify_error l ->
			let err = cannot_unify a b in
			error (err :: (Invariant_parameter !i) :: l)
		) tl1 tl2

let type_iseq uctx a b =
	try
		type_eq uctx a b;
		true
	with
		Unify_error _ -> false

let type_iseq_strict a b =
	try
		type_eq {default_unification_context with equality_kind = EqDoNotFollowNull} a b;
		true
	with Unify_error _ ->
		false

let unify_stack = new_rec_stack()
let variance_stack = new_rec_stack()
let abstract_cast_stack = new_rec_stack()
let unify_new_monos = new_rec_stack()

let print_stacks() =
	let ctx = print_context() in
	let st = s_type ctx in
	print_endline "unify_stack";
	List.iter (fun (a,b) -> Printf.printf "\t%s , %s\n" (st a) (st b)) unify_stack.rec_stack;
	print_endline "variance_stack";
	List.iter (fun (a,b) -> Printf.printf "\t%s , %s\n" (st a) (st b)) variance_stack.rec_stack;
	print_endline "monos";
	List.iter (fun m -> print_endline ("\t" ^ st m)) unify_new_monos.rec_stack;
	print_endline "abstract_cast_stack";
	List.iter (fun (a,b) -> Printf.printf "\t%s , %s\n" (st a) (st b)) abstract_cast_stack.rec_stack

let rec unify (uctx : unification_context) a b =
	if a == b then
		()
	else match a, b with
	| TLazy f , _ -> unify uctx (lazy_type f) b
	| _ , TLazy f -> unify uctx a (lazy_type f)
	| TMono t , _ ->
		(match t.tm_type with
		| None -> if not (link t a b) then error [cannot_unify a b]
		| Some t -> unify uctx t b)
	| _ , TMono t ->
		(match t.tm_type with
		| None -> if not (link t b a) then error [cannot_unify a b]
		| Some t -> unify uctx a t)
	| TType (t,tl) , _ ->
		rec_stack unify_stack (a,b)
			(fun(a2,b2) -> fast_eq_unbound_mono a a2 && fast_eq b b2)
			(fun() -> try_apply_params_rec t.t_params tl t.t_type (fun a -> unify uctx a b))
			(fun l -> error (cannot_unify a b :: l))
	| _ , TType (t,tl) ->
		rec_stack unify_stack (a,b)
			(fun(a2,b2) -> fast_eq a a2 && fast_eq_unbound_mono b b2)
			(fun() -> try_apply_params_rec t.t_params tl t.t_type (unify uctx a))
			(fun l -> error (cannot_unify a b :: l))
	| TEnum (ea,tl1) , TEnum (eb,tl2) ->
		if ea != eb then error [cannot_unify a b];
		unify_type_params uctx a b tl1 tl2
	| TAbstract ({a_path=[],"Null"},[t]),_ ->
		begin try unify uctx t b
		with Unify_error l -> error (cannot_unify a b :: l) end
	| _,TAbstract ({a_path=[],"Null"},[t]) ->
		begin try unify uctx a t
		with Unify_error l -> error (cannot_unify a b :: l) end
	| TAbstract (a1,tl1) , TAbstract (a2,tl2) when a1 == a2 ->
		begin try
			unify_type_params uctx a b tl1 tl2
		with Unify_error _ as err ->
			(* the type could still have a from/to relation to itself (issue #3494) *)
			begin try
				unify_abstracts uctx a b a1 tl1 a2 tl2
			with Unify_error _ ->
				raise err
			end
		end
	| TAbstract ({a_path=[],"Void"},_) , _
	| _ , TAbstract ({a_path=[],"Void"},_) ->
		error [cannot_unify a b]
	| TAbstract ({ a_path = ["haxe"],"NotVoid" },[]), _
	| _, TAbstract ({ a_path = ["haxe"],"NotVoid" },[]) ->
		()
	| TAbstract (ab,tl), TAbstract ({ a_path = ["haxe"],("FlatEnum" | "Function" | "Constructible") },_) ->
		unify_to {uctx with allow_transitive_cast = false} a b ab tl
	| TAbstract (a1,tl1) , TAbstract (a2,tl2) ->
		unify_abstracts uctx a b a1 tl1 a2 tl2
	| TInst (c1,tl1) , TInst (c2,tl2) ->
		let rec loop c tl =
			if c == c2 then begin
				unify_type_params uctx a b tl tl2;
				true
			end else (match c.cl_super with
				| None -> false
				| Some (cs,tls) ->
					loop cs (List.map (apply_params c.cl_params tl) tls)
			) || List.exists (fun (cs,tls) ->
				loop cs (List.map (apply_params c.cl_params tl) tls)
			) c.cl_implements
			|| (match c.cl_kind with
			| KTypeParameter pl -> List.exists (fun t ->
				match follow t with
				| TInst (cs,tls) -> loop cs (List.map (apply_params c.cl_params tl) tls)
				| TAbstract(aa,tl) -> unifies_to uctx a b aa tl
				| _ -> false
			) pl
			| _ -> false)
		in
		if not (loop c1 tl1) then error [cannot_unify a b]
	| TFun (l1,r1) , TFun (l2,r2) when List.length l1 = List.length l2 ->
		let uctx = get_nested_context uctx in
		let i = ref 0 in
		(try
			(match follow r2 with
			| TAbstract ({a_path=[],"Void"},_) -> incr i
			| _ -> unify uctx r1 r2; incr i);
			List.iter2 (fun (_,o1,t1) (_,o2,t2) ->
				if o1 && not o2 then error [Cant_force_optional];
				unify uctx t1 t2;
				incr i
			) l2 l1 (* contravariance *)
		with
			Unify_error l ->
				let msg = if !i = 0 then Invalid_return_type else Invalid_function_argument(!i,List.length l1) in
				error (cannot_unify a b :: msg :: l))
	| TInst (c,tl) , TAnon an ->
		if PMap.is_empty an.a_fields then (match c.cl_kind with
			| KTypeParameter pl ->
				(* one of the constraints must unify with { } *)
				if not (List.exists (fun t -> match follow t with TInst _ | TAnon _ -> true | _ -> false) pl) then error [cannot_unify a b]
			| _ -> ());
		ignore(c.cl_build());
		(try
			PMap.iter (fun n f2 ->
				(*
					introducing monomorphs while unifying might create infinite loops - see #2315
					let's store these monomorphs and make sure we reach a fixed point
				*)
				let monos = ref [] in
				let make_type f =
					match f.cf_params with
					| [] -> f.cf_type
					| l ->
						let ml = List.map (fun _ -> mk_mono()) l in
						monos := ml;
						apply_params f.cf_params ml f.cf_type
				in
				let _, ft, f1 = (try raw_class_field make_type c tl n with Not_found -> error [has_no_field a n]) in
				let ft = apply_params c.cl_params tl ft in
				if not (unify_kind f1.cf_kind f2.cf_kind) then error [invalid_kind n f1.cf_kind f2.cf_kind];
				if (has_class_field_flag f2 CfPublic) && not (has_class_field_flag f1 CfPublic) then error [invalid_visibility n];

				(match f2.cf_kind with
				| Var { v_read = AccNo } | Var { v_read = AccNever } ->
					(* we will do a recursive unification, so let's check for possible recursion *)
					let old_monos = unify_new_monos.rec_stack in
					unify_new_monos.rec_stack <- !monos @ unify_new_monos.rec_stack;
					rec_stack unify_stack (ft,f2.cf_type)
						(fun (a2,b2) -> fast_eq b2 f2.cf_type && fast_eq_mono unify_new_monos.rec_stack ft a2)
						(fun() -> try unify_with_access uctx f1 ft f2 with e -> unify_new_monos.rec_stack <- old_monos; raise e)
						(fun l -> error (invalid_field n :: l));
					unify_new_monos.rec_stack <- old_monos;
				| Method MethNormal | Method MethInline | Var { v_write = AccNo } | Var { v_write = AccNever } ->
					(* same as before, but unification is reversed (read-only var) *)
					let old_monos = unify_new_monos.rec_stack in
					unify_new_monos.rec_stack <- !monos @ unify_new_monos.rec_stack;
					rec_stack unify_stack (f2.cf_type,ft)
						(fun(a2,b2) -> fast_eq_mono unify_new_monos.rec_stack b2 ft && fast_eq f2.cf_type a2)
						(fun() -> try unify_with_access uctx f1 ft f2 with e -> unify_new_monos.rec_stack <- old_monos; raise e)
						(fun l -> error (invalid_field n :: l));
					unify_new_monos.rec_stack <- old_monos;
				| _ ->
					(* will use fast_eq, which have its own stack *)
					try
						unify_with_access uctx f1 ft f2
					with
						Unify_error l ->
							error (invalid_field n :: l));

				List.iter (fun f2o ->
					if not (List.exists (fun f1o -> type_iseq uctx f1o.cf_type f2o.cf_type) (f1 :: f1.cf_overloads))
					then error [Missing_overload (f1, f2o.cf_type)]
				) f2.cf_overloads;
				(* we mark the field as :?used because it might be used through the structure *)
				if not (Meta.has Meta.MaybeUsed f1.cf_meta) then begin
					f1.cf_meta <- (Meta.MaybeUsed,[],f1.cf_pos) :: f1.cf_meta;
					match f2.cf_kind with
					| Var vk ->
						let check name =
							try
								let _,_,cf = raw_class_field make_type c tl name in
								if not (Meta.has Meta.MaybeUsed cf.cf_meta) then
									cf.cf_meta <- (Meta.MaybeUsed,[],f1.cf_pos) :: cf.cf_meta
							with Not_found ->
								()
						in
						(match vk.v_read with AccCall -> check ("get_" ^ f1.cf_name) | _ -> ());
						(match vk.v_write with AccCall -> check ("set_" ^ f1.cf_name) | _ -> ());
					| _ -> ()
				end;
				(match f1.cf_kind with
				| Method MethInline ->
					if ((has_class_flag c CExtern) || has_class_field_flag f1 CfExtern) && not (Meta.has Meta.Runtime f1.cf_meta) then error [Has_no_runtime_field (a,n)];
				| _ -> ());
			) an.a_fields;
			(match !(an.a_status) with
			| Statics _ | EnumStatics _ | AbstractStatics _ -> error []
			| Closed | Extend _ | Const -> ())
		with
			Unify_error l -> error (cannot_unify a b :: l))
	| TAnon a1, TAnon a2 ->
		unify_anons uctx a b a1 a2
	| TAnon an, TAbstract ({ a_path = [],"Class" },[pt]) ->
		(match !(an.a_status) with
		| Statics cl -> unify uctx (TInst (cl,List.map (fun _ -> mk_mono()) cl.cl_params)) pt
		| _ -> error [cannot_unify a b])
	| TAnon an, TAbstract ({ a_path = [],"Enum" },[pt]) ->
		(match !(an.a_status) with
		| EnumStatics e -> unify uctx (TEnum (e,List.map (fun _ -> mk_mono()) e.e_params)) pt
		| _ -> error [cannot_unify a b])
	| TEnum _, TAbstract ({ a_path = [],"EnumValue" },[]) ->
		()
	| TEnum(en,_), TAbstract ({ a_path = ["haxe"],"FlatEnum" },[]) when Meta.has Meta.FlatEnum en.e_meta ->
		()
	| TFun _, TAbstract ({ a_path = ["haxe"],"Function" },[]) ->
		()
	| TInst(c,tl),TAbstract({a_path = ["haxe"],"Constructible"},[t1]) ->
		begin try
			begin match c.cl_kind with
				| KTypeParameter tl ->
					(* type parameters require an equal Constructible constraint *)
					if not (List.exists (fun t -> match follow t with TAbstract({a_path = ["haxe"],"Constructible"},[t2]) -> type_iseq uctx t1 t2 | _ -> false) tl) then error [cannot_unify a b]
				| _ ->
					let _,t,cf = class_field c tl "new" in
					if not (has_class_field_flag cf CfPublic) then error [invalid_visibility "new"];
					begin try unify uctx t t1
					with Unify_error l -> error (cannot_unify a b :: l) end
			end
		with Not_found ->
			error [has_no_field a "new"]
		end
	| TDynamic t , _ ->
		if t == a && uctx.allow_dynamic_to_cast then
			()
		else (match b with
		| TDynamic t2 ->
			if t2 != b then
				(try
					type_eq {uctx with equality_kind = EqRightDynamic} t t2
				with
					Unify_error l -> error (cannot_unify a b :: l));
		| TAbstract(bb,tl) ->
			unify_from uctx a b bb tl
		| _ ->
			error [cannot_unify a b])
	| _ , TDynamic t ->
		if t == b then
			()
		else (match a with
		| TDynamic t2 ->
			if t2 != a then
				(try
					type_eq {uctx with equality_kind = EqRightDynamic} t t2
				with
					Unify_error l -> error (cannot_unify a b :: l));
		| TAnon an ->
			(try
				(match !(an.a_status) with
				| Statics _ | EnumStatics _ -> error []
				| _ -> ());
				PMap.iter (fun _ f ->
					try
						type_eq uctx (field_type f) t
					with Unify_error l ->
						error (invalid_field f.cf_name :: l)
				) an.a_fields
			with Unify_error l ->
				error (cannot_unify a b :: l))
		| TAbstract(aa,tl) ->
			unify_to uctx a b aa tl
		| _ ->
			error [cannot_unify a b])
	| TAbstract (aa,tl), _  ->
		unify_to uctx a b aa tl
	| TInst ({ cl_kind = KTypeParameter ctl } as c,pl), TAbstract (bb,tl) ->
		(* one of the constraints must satisfy the abstract *)
		if not (List.exists (fun t ->
			let t = apply_params c.cl_params pl t in
			try unify uctx t b; true with Unify_error _ -> false
		) ctl) then unify_from uctx a b bb tl
	| _, TAbstract (bb,tl) ->
		unify_from uctx a b bb tl
	| _ , _ ->
		error [cannot_unify a b]

and unify_anons uctx a b a1 a2 =
	(try
		PMap.iter (fun n f2 ->
		try
			let f1 = PMap.find n a1.a_fields in
			if not (unify_kind f1.cf_kind f2.cf_kind) then
				error [invalid_kind n f1.cf_kind f2.cf_kind];
			if (has_class_field_flag f2 CfPublic) && not (has_class_field_flag f1 CfPublic) then error [invalid_visibility n];
			try
				let f1_type =
					if fast_eq f1.cf_type f2.cf_type then f1.cf_type
					else field_type f1
				in
				unify_with_access uctx f1 f1_type f2;
				(match !(a1.a_status) with
				| Statics c when not (Meta.has Meta.MaybeUsed f1.cf_meta) -> f1.cf_meta <- (Meta.MaybeUsed,[],f1.cf_pos) :: f1.cf_meta
				| _ -> ());
			with
				Unify_error l -> error (invalid_field n :: l)
		with
			Not_found ->
				match !(a1.a_status) with
				| Const when Meta.has Meta.Optional f2.cf_meta ->
					a1.a_fields <- PMap.add f2.cf_name f2 a1.a_fields
				| _ ->
					error [has_no_field a n];
		) a2.a_fields;
		(match !(a2.a_status) with
		| Statics c -> (match !(a1.a_status) with Statics c2 when c == c2 -> () | _ -> error [])
		| EnumStatics e -> (match !(a1.a_status) with EnumStatics e2 when e == e2 -> () | _ -> error [])
		| AbstractStatics a -> (match !(a1.a_status) with AbstractStatics a2 when a == a2 -> () | _ -> error [])
		| Const | Extend _ | Closed -> ())
	with
		Unify_error l -> error (cannot_unify a b :: l))

and does_func_unify f =
	try f(); true with Unify_error _ -> false

and does_func_unify_arg f arg =
	try f arg; true with Unify_error _ -> false

and get_abstract_context uctx a b ab =
	if (Meta.has Meta.CoreType ab.a_meta) || (Meta.has Meta.Transitive ab.a_meta) then
		uctx
	else if uctx.allow_abstract_cast then
		{uctx with allow_abstract_cast = false}
	else
		error [cannot_unify a b]

and get_nested_context uctx =
	{uctx with allow_abstract_cast = true}

and unifies_with_abstract uctx a b f =
	rec_stack_default abstract_cast_stack (a,b) (fast_eq_pair (a,b)) (fun() ->
		(uctx.allow_transitive_cast && f {uctx with allow_transitive_cast = false}) || f uctx
	) false

and get_abstract_unify_func uctx equality_kind =
	if uctx.allow_transitive_cast then unify uctx else type_eq {uctx with equality_kind = equality_kind}

and unify_abstracts uctx a b a1 tl1 a2 tl2 =
	if not (unifies_abstracts uctx a b a1 tl1 a2 tl2) then error [cannot_unify a b]

and unify_from uctx a b ab tl =
	if not (unifies_from uctx a b ab tl) then error [cannot_unify a b]

and unify_to uctx a b ab tl =
	if not (unifies_to uctx a b ab tl) then error [cannot_unify a b]

and unifies_abstracts uctx a b a1 tl1 a2 tl2 =
	unifies_with_abstract uctx a b (fun uctx ->
		List.exists (unifies_to_direct uctx a b a1 tl1) a1.a_to ||
		List.exists (unifies_from_direct uctx a b a2 tl2) a2.a_from
	)

and unifies_from uctx a b ab tl =
	unifies_with_abstract uctx a b (fun uctx ->
		List.exists (unifies_from_direct uctx a b ab tl) ab.a_from
	)

and unifies_to uctx a b ab tl =
	unifies_with_abstract uctx a b (fun uctx ->
		List.exists (unifies_to_direct uctx a b ab tl) ab.a_to
	)

and unifies_from_direct uctx a b ab tl t =
	does_func_unify (fun() ->
		let t = apply_params ab.a_params tl t in
		let uctx = get_abstract_context uctx a b ab in
		let unify_func = get_abstract_unify_func uctx EqRightDynamic in
		unify_func a t)

and unifies_to_direct uctx a b ab tl t =
	does_func_unify (fun() ->
		let t = apply_params ab.a_params tl t in
		let uctx = get_abstract_context uctx a b ab in
		let unify_func = get_abstract_unify_func uctx EqStrict in
		unify_func t b)

and unifies_from_field uctx a b ab tl (t,cf) =
	does_func_unify (fun() ->
		match follow cf.cf_type with
		| TFun(_,r) ->
			let map = apply_params ab.a_params tl in
			let monos = Monomorph.spawn_constrained_monos map cf.cf_params in
			let map t = map (apply_params cf.cf_params monos t) in
			let uctx = get_abstract_context uctx a b ab in
			let unify_func = get_abstract_unify_func uctx EqStrict in
			unify_func a (map t);
			unify_func (map r) b;
		| _ -> die "" __LOC__)

and unifies_to_field uctx a b ab tl (t,cf) =
	does_func_unify (fun() ->
		match follow cf.cf_type with
		| TFun((_,_,ta) :: _,_) ->
			let map = apply_params ab.a_params tl in
			let monos = Monomorph.spawn_constrained_monos map cf.cf_params in
			let map t = map (apply_params cf.cf_params monos t) in
			let uctx = get_abstract_context uctx a b ab in
			let unify_func = get_abstract_unify_func uctx EqStrict in
			let athis = map ab.a_this in
			(* we cannot allow implicit casts when the this type is not completely known yet *)
			with_variance uctx (type_eq {uctx with equality_kind = EqStrict}) athis (map ta);
			unify_func (map t) b;
		| _ -> die "" __LOC__)

and unify_with_variance uctx f t1 t2 =
	let t1 = follow_without_type t1 in
	let t2 = follow_without_type t2 in
	let fail () = error [cannot_unify t1 t2] in
	let unify_rec f = rec_stack variance_stack (t1,t2) (fast_eq_pair (t1,t2)) f (fun _ -> fail()) in
	let unify_nested t1 t2 = with_variance (get_nested_context uctx) f t1 t2 in
	let unify_tls tl1 tl2 = List.iter2 unify_nested tl1 tl2 in
	let get_this_type ab tl = follow_without_type (apply_params ab.a_params tl ab.a_this) in
	let get_defined_type td tl = follow_without_type (apply_typedef td tl) in
	let compare_underlying () = type_eq {uctx with equality_underlying = true; equality_kind = EqBothDynamic} t1 t2 in
	let unifies_abstract uctx a b ab tl ats =
		try
			let uctx = get_abstract_context uctx a b ab in
			rec_stack_default abstract_cast_stack (a,b) (fast_eq_pair (a,b)) (fun() ->
				List.exists (does_func_unify_arg (fun at ->
					let at = apply_params ab.a_params tl at in
					if ats == ab.a_to then
						with_variance uctx f at b
					else
						with_variance uctx f a at
				)) ats
			) false
		with Unify_error _ -> false
	in
	match t1,t2 with
	| TInst(c1,tl1),TInst(c2,tl2) when c1 == c2 ->
		unify_tls tl1 tl2
	| TEnum(en1,tl1),TEnum(en2,tl2) when en1 == en2 ->
		unify_tls tl1 tl2
	| TAbstract(a1,tl1),TAbstract(a2,tl2) when a1 == a2 ->
		unify_tls tl1 tl2
	| TType(td1,tl1),TType(td2,tl2) when td1 == td2 ->
		unify_tls tl1 tl2
	| TType(td,tl),_ ->
		unify_rec (fun() -> unify_with_variance uctx f (get_defined_type td tl) t2)
	| _,TType(td,tl) ->
		unify_rec (fun() -> unify_with_variance uctx f t1 (get_defined_type td tl))
	| TAbstract(ab,tl),_ when Meta.has Meta.ForwardVariance ab.a_meta ->
		with_variance uctx f (get_this_type ab tl) t2
	| _,TAbstract(ab,tl) when Meta.has Meta.ForwardVariance ab.a_meta ->
		with_variance uctx f t1 (get_this_type ab tl)
	| TAbstract(a1,tl1),TAbstract(a2,tl2) ->
		if not (unifies_abstract uctx t1 t2 a1 tl1 a1.a_to)
		&& not (unifies_abstract uctx t1 t2 a2 tl2 a2.a_from) then fail();
		compare_underlying();
	| TAbstract(ab,tl),_ ->
		if not (unifies_abstract uctx t1 t2 ab tl ab.a_to) then fail();
		compare_underlying();
	| _,TAbstract(ab,tl) ->
		if not (unifies_abstract uctx t1 t2 ab tl ab.a_from) then fail();
		compare_underlying();
	| TAnon(a1),TAnon(a2) ->
		unify_anons uctx t1 t2 a1 a2
	| TFun(al1,r1),TFun(al2,r2) when List.length al1 = List.length al2 ->
		List.iter2 (fun (_,_,t1) (_,_,t2) -> unify_nested t1 t2) al1 al2;
		unify_nested r1 r2;
	| _ ->
		fail()

and unify_type_params uctx a b tl1 tl2 =
	let uctx = get_nested_context uctx in
	let i = ref 0 in
	List.iter2 (fun t1 t2 ->
		incr i;
		try
			with_variance uctx (type_eq {uctx with equality_kind = EqRightDynamic}) t1 t2
		with Unify_error l ->
			let err = cannot_unify a b in
			error (err :: (Invariant_parameter !i) :: l)
	) tl1 tl2

and with_variance uctx f t1 t2 =
	try
		f t1 t2
	with Unify_error l -> try
		unify_with_variance uctx f t1 t2
	with Unify_error _ ->
		raise (Unify_error l)

and unify_with_access uctx f1 t1 f2 =
	let uctx = get_nested_context uctx in
	match f2.cf_kind with
	(* write only *)
	| Var { v_read = AccNo } | Var { v_read = AccNever } -> unify uctx f2.cf_type t1
	(* read only *)
	| Method MethNormal | Method MethInline | Var { v_write = AccNo } | Var { v_write = AccNever } ->
		if (has_class_field_flag f1 CfFinal) <> (has_class_field_flag f2 CfFinal) then raise (Unify_error [FinalInvariance]);
		unify uctx t1 f2.cf_type
	(* read/write *)
	| _ -> with_variance uctx (type_eq {uctx with equality_kind = EqBothDynamic}) t1 f2.cf_type

let does_unify a b =
	try
		unify default_unification_context a b;
		true
	with Unify_error _ ->
		false

let unify_custom = unify
let unify = unify default_unification_context

let type_eq_custom = type_eq
let type_eq param = type_eq {default_unification_context with equality_kind = param}

let type_iseq_custom = type_iseq
let type_iseq = type_iseq default_unification_context

module UnifyMinT = struct
	let collect_base_types t =
		let tl = ref [] in
		let rec loop t = (match t with
			| TInst(cl, params) ->
				(match cl.cl_kind with
				| KTypeParameter tl -> List.iter loop tl
				| _ -> ());
				List.iter (fun (ic, ip) ->
					let t = apply_params cl.cl_params params (TInst (ic,ip)) in
					loop t
				) cl.cl_implements;
				(match cl.cl_super with None -> () | Some (csup, pl) ->
					let t = apply_params cl.cl_params params (TInst (csup,pl)) in
					loop t);
				tl := t :: !tl;
			| TType (td,pl) ->
				loop (apply_typedef td pl);
				(* prioritize the most generic definition *)
				tl := t :: !tl;
			| TLazy f -> loop (lazy_type f)
			| TMono r -> (match r.tm_type with None -> () | Some t -> loop t)
			| _ -> tl := t :: !tl)
		in
		loop t;
		!tl

	let unify_min' uctx common_types tl =
		let first_error = ref None in
		let rec loop index common_types tl = match tl with
			| [] ->
				begin match common_types with
				| [] ->
					begin match !first_error with
					| None -> die "" __LOC__
					| Some(l,p) -> UnifyMinError(l,p)
					end
				| hd :: _ ->
					UnifyMinOk hd
				end
			| t :: tl ->
				let common_types = List.filter (fun t' ->
					try
						unify_custom uctx t t';
						true
					with Unify_error l ->
						if !first_error = None then first_error := Some(l,index);
						false
				) common_types in
				loop (index + 1) common_types tl
		in
		loop 0 common_types tl

	let unify_min uctx t0 tl =
		match tl with
		| [] ->
			UnifyMinOk t0
		| _ ->
			let common_types = collect_base_types t0 in
			unify_min' uctx common_types tl
end
;;
unify_ref := unify_custom;;
unify_min_ref := UnifyMinT.unify_min;;
monomorph_classify_constraints_ref := Monomorph.classify_down_constraints
