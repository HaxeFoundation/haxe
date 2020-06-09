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
	equality_kind         : eq_kind;
}

let error l = raise (Unify_error l)

let check_constraint name f =
	try
		f()
	with Unify_error l ->
		raise (Unify_error ((Constraint_failure name) :: l))

let unify_ref : (unification_context -> t -> t -> unit) ref = ref (fun _ _ _ -> ())

let default_unification_context = {
	allow_transitive_cast = true;
	equality_kind         = EqStrict;
}

module Monomorph = struct
	let create () = {
		tm_type = None;
	}

	let do_bind m t =
		(* assert(m.tm_type = None); *) (* TODO: should be here, but matcher.ml does some weird bind handling at the moment. *)
		m.tm_type <- Some t

	let rec bind m t =
		m.tm_type <- Some t

	let unbind m =
		m.tm_type <- None
end

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

let would_produce_recursive_anon field_acceptor field_donor =
	try
		(match !(field_acceptor.a_status) with
		| Opened ->
			PMap.iter (fun n field ->
				match follow field.cf_type with
				| TAnon a when field_acceptor == a -> raise Exit
				| _ -> ()
			) field_donor.a_fields;
		| _ -> ());
		false
	with Exit -> true

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
				| Opened, Opened -> fields_eq()
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
	| AccResolve | AccCall -> false

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

let rec_stack_bool stack value fcheck frun =
	if (rec_stack_exists fcheck stack) then false else begin
		try
			stack.rec_stack <- value :: stack.rec_stack;
			frun();
			stack.rec_stack <- List.tl stack.rec_stack;
			true
		with
			Unify_error l ->
				stack.rec_stack <- List.tl stack.rec_stack;
				false
			| e ->
				stack.rec_stack <- List.tl stack.rec_stack;
				raise e
	end

let rec type_eq uctx a b =
	let param = uctx.equality_kind in
	let can_follow t = match param with
		| EqCoreType -> false
		| EqDoNotFollowNull -> not (is_explicit_null t)
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
	| TAbstract ({a_path=[],"Null"},[t1]),TAbstract ({a_path=[],"Null"},[t2]) ->
		type_eq uctx t1 t2
	| TAbstract ({a_path=[],"Null"},[t]),_ when param <> EqDoNotFollowNull ->
		type_eq uctx t b
	| _,TAbstract ({a_path=[],"Null"},[t]) when param <> EqDoNotFollowNull ->
		type_eq uctx a t
	| TType (t1,tl1), TType (t2,tl2) when (t1 == t2 || (param = EqCoreType && t1.t_path = t2.t_path)) && List.length tl1 = List.length tl2 ->
		type_eq_params uctx a b tl1 tl2
	| TType (t,tl) , _ when can_follow a ->
		try_apply_params_rec t.t_params tl t.t_type (fun a -> type_eq uctx a b)
	| _ , TType (t,tl) when can_follow b ->
		rec_stack eq_stack (a,b)
			(fun (a2,b2) -> fast_eq a a2 && fast_eq b b2)
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
	| TDynamic a , TDynamic b ->
		type_eq uctx a b
	| TAbstract (a1,tl1) , TAbstract (a2,tl2) ->
		if a1 != a2 && not (param = EqCoreType && a1.a_path = a2.a_path) then error [cannot_unify a b];
		type_eq_params uctx a b tl1 tl2
	| TAnon a1, TAnon a2 ->
		(try
			(match !(a2.a_status) with
			| Statics c -> (match !(a1.a_status) with Statics c2 when c == c2 -> () | _ -> error [])
			| EnumStatics e -> (match !(a1.a_status) with EnumStatics e2 when e == e2 -> () | _ -> error [])
			| AbstractStatics a -> (match !(a1.a_status) with AbstractStatics a2 when a == a2 -> () | _ -> error [])
			| _ -> ()
			);
			if would_produce_recursive_anon a1 a2 || would_produce_recursive_anon a2 a1 then error [cannot_unify a b];
			PMap.iter (fun n f1 ->
				try
					let f2 = PMap.find n a2.a_fields in
					if f1.cf_kind <> f2.cf_kind && (param = EqStrict || param = EqCoreType || not (unify_kind f1.cf_kind f2.cf_kind)) then error [invalid_kind n f1.cf_kind f2.cf_kind];
					let a = f1.cf_type and b = f2.cf_type in
					(try type_eq uctx a b with Unify_error l -> error (invalid_field n :: l));
					if (has_class_field_flag f1 CfPublic) != (has_class_field_flag f2 CfPublic) then error [invalid_visibility n];
				with
					Not_found ->
						if is_closed a2 then error [has_no_field b n];
						if not (link (Monomorph.create()) b f1.cf_type) then error [cannot_unify a b];
						a2.a_fields <- PMap.add n f1 a2.a_fields
			) a1.a_fields;
			PMap.iter (fun n f2 ->
				if not (PMap.mem n a1.a_fields) then begin
					if is_closed a1 then error [has_no_field a n];
					if not (link (Monomorph.create()) a f2.cf_type) then error [cannot_unify a b];
					a1.a_fields <- PMap.add n f2 a1.a_fields
				end;
			) a2.a_fields;
		with
			Unify_error l -> error (cannot_unify a b :: l))
	| _ , _ ->
		if b == t_dynamic && (param = EqRightDynamic || param = EqBothDynamic) then
			()
		else if a == t_dynamic && param = EqBothDynamic then
			()
		else
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

let type_iseq a b =
	try
		type_eq default_unification_context a b;
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
let abstract_cast_stack = new_rec_stack()
let unify_new_monos = new_rec_stack()

let print_stacks() =
	let ctx = print_context() in
	let st = s_type ctx in
	print_endline "unify_stack";
	List.iter (fun (a,b) -> Printf.printf "\t%s , %s\n" (st a) (st b)) unify_stack.rec_stack;
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
				| TAbstract(aa,tl) -> List.exists (unify_to uctx aa tl b) aa.a_to
				| _ -> false
			) pl
			| _ -> false)
		in
		if not (loop c1 tl1) then error [cannot_unify a b]
	| TFun (l1,r1) , TFun (l2,r2) when List.length l1 = List.length l2 ->
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
					if not (List.exists (fun f1o -> type_iseq f1o.cf_type f2o.cf_type) (f1 :: f1.cf_overloads))
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
					if (c.cl_extern || has_class_field_flag f1 CfExtern) && not (Meta.has Meta.Runtime f1.cf_meta) then error [Has_no_runtime_field (a,n)];
				| _ -> ());
			) an.a_fields;
			(match !(an.a_status) with
			| Opened -> an.a_status := Closed;
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
					if not (List.exists (fun t -> match follow t with TAbstract({a_path = ["haxe"],"Constructible"},[t2]) -> type_iseq t1 t2 | _ -> false) tl) then error [cannot_unify a b]
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
		if t == a then
			()
		else (match b with
		| TDynamic t2 ->
			if t2 != b then
				(try
					type_eq {uctx with equality_kind = EqRightDynamic} t t2
				with
					Unify_error l -> error (cannot_unify a b :: l));
		| TAbstract(bb,tl) when (List.exists (unify_from uctx bb tl a b) bb.a_from) ->
			()
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
				| Opened -> an.a_status := Closed
				| _ -> ());
				PMap.iter (fun _ f ->
					try
						type_eq uctx (field_type f) t
					with Unify_error l ->
						error (invalid_field f.cf_name :: l)
				) an.a_fields
			with Unify_error l ->
				error (cannot_unify a b :: l))
		| TAbstract(aa,tl) when (List.exists (unify_to uctx aa tl b) aa.a_to) ->
			()
		| _ ->
			error [cannot_unify a b])
	| TAbstract (aa,tl), _  ->
		if not (List.exists (unify_to uctx aa tl b) aa.a_to) then error [cannot_unify a b];
	| TInst ({ cl_kind = KTypeParameter ctl } as c,pl), TAbstract (bb,tl) ->
		(* one of the constraints must satisfy the abstract *)
		if not (List.exists (fun t ->
			let t = apply_params c.cl_params pl t in
			try unify uctx t b; true with Unify_error _ -> false
		) ctl) && not (List.exists (unify_from uctx bb tl a b) bb.a_from) then error [cannot_unify a b];
	| _, TAbstract (bb,tl) ->
		if not (List.exists (unify_from uctx bb tl a b) bb.a_from) then error [cannot_unify a b]
	| _ , _ ->
		error [cannot_unify a b]

and unify_abstracts uctx a b a1 tl1 a2 tl2 =
	let uctx_no_transitive_casts = {uctx with allow_transitive_cast = false} in
	if (List.exists (unify_to uctx_no_transitive_casts a1 tl1 b) a1.a_to)
	|| (List.exists (unify_from uctx_no_transitive_casts a2 tl2 a b) a2.a_from)
	|| (((Meta.has Meta.CoreType a1.a_meta) || (Meta.has Meta.CoreType a2.a_meta))
		&& ((List.exists (unify_to uctx a1 tl1 b) a1.a_to) || (List.exists (unify_from uctx a2 tl2 a b) a2.a_from))) then
		()
	else
		error [cannot_unify a b]

and unify_anons uctx a b a1 a2 =
	if would_produce_recursive_anon a1 a2 then error [cannot_unify a b];
	(try
		PMap.iter (fun n f2 ->
		try
			let f1 = PMap.find n a1.a_fields in
			if not (unify_kind f1.cf_kind f2.cf_kind) then
				(match !(a1.a_status), f1.cf_kind, f2.cf_kind with
				| Opened, Var { v_read = AccNormal; v_write = AccNo }, Var { v_read = AccNormal; v_write = AccNormal } ->
					f1.cf_kind <- f2.cf_kind;
				| _ -> error [invalid_kind n f1.cf_kind f2.cf_kind]);
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
				| Opened ->
					if not (link (Monomorph.create()) a f2.cf_type) then error [];
					a1.a_fields <- PMap.add n f2 a1.a_fields
				| Const when Meta.has Meta.Optional f2.cf_meta ->
					()
				| _ ->
					error [has_no_field a n];
		) a2.a_fields;
		(match !(a1.a_status) with
		| Const when not (PMap.is_empty a2.a_fields) ->
			PMap.iter (fun n _ -> if not (PMap.mem n a2.a_fields) then error [has_extra_field a n]) a1.a_fields;
		| Opened ->
			a1.a_status := Closed
		| _ -> ());
		(match !(a2.a_status) with
		| Statics c -> (match !(a1.a_status) with Statics c2 when c == c2 -> () | _ -> error [])
		| EnumStatics e -> (match !(a1.a_status) with EnumStatics e2 when e == e2 -> () | _ -> error [])
		| AbstractStatics a -> (match !(a1.a_status) with AbstractStatics a2 when a == a2 -> () | _ -> error [])
		| Opened -> a2.a_status := Closed
		| Const | Extend _ | Closed -> ())
	with
		Unify_error l -> error (cannot_unify a b :: l))

and unify_from uctx ab tl a b t =
	rec_stack_bool abstract_cast_stack (a,b)
		(fun (a2,b2) -> fast_eq a a2 && fast_eq b b2)
		(fun() ->
			let t = apply_params ab.a_params tl t in
			let unify_func = if uctx.allow_transitive_cast then unify uctx else type_eq {uctx with equality_kind = EqRightDynamic} in
			unify_func a t)

and unify_to uctx ab tl b t =
	let t = apply_params ab.a_params tl t in
	let unify_func = if uctx.allow_transitive_cast then unify uctx else type_eq {uctx with equality_kind = EqStrict} in
	try
		unify_func t b;
		true
	with Unify_error _ ->
		false

and unify_from_field uctx ab tl a b (t,cf) =
	rec_stack_bool abstract_cast_stack (a,b)
		(fun (a2,b2) -> fast_eq a a2 && fast_eq b b2)
		(fun() ->
			let unify_func = if uctx.allow_transitive_cast then unify uctx else type_eq {uctx with equality_kind = EqStrict} in
			match follow cf.cf_type with
			| TFun(_,r) ->
				let monos = List.map (fun _ -> mk_mono()) cf.cf_params in
				let map t = apply_params ab.a_params tl (apply_params cf.cf_params monos t) in
				unify_func a (map t);
				List.iter2 (fun m (name,t) -> match follow t with
					| TInst ({ cl_kind = KTypeParameter constr },_) when constr <> [] ->
						List.iter (fun tc -> match follow m with TMono _ -> raise (Unify_error []) | _ -> unify uctx m (map tc) ) constr
					| _ -> ()
				) monos cf.cf_params;
				unify_func (map r) b;
				true
			| _ -> die "" __LOC__)

and unify_to_field uctx ab tl b (t,cf) =
	let a = TAbstract(ab,tl) in
	rec_stack_bool abstract_cast_stack (b,a)
		(fun (b2,a2) -> fast_eq a a2 && fast_eq b b2)
		(fun() ->
			let unify_func = if uctx.allow_transitive_cast then unify uctx else type_eq {uctx with equality_kind = EqStrict} in
			match follow cf.cf_type with
			| TFun((_,_,ta) :: _,_) ->
				let monos = List.map (fun _ -> mk_mono()) cf.cf_params in
				let map t = apply_params ab.a_params tl (apply_params cf.cf_params monos t) in
				let athis = map ab.a_this in
				(* we cannot allow implicit casts when the this type is not completely known yet *)
				(* if has_mono athis then raise (Unify_error []); *)
				with_variance uctx (type_eq {uctx with equality_kind = EqStrict}) athis (map ta);
				(* immediate constraints checking is ok here because we know there are no monomorphs *)
				List.iter2 (fun m (name,t) -> match follow t with
					| TInst ({ cl_kind = KTypeParameter constr },_) when constr <> [] ->
						List.iter (fun tc -> match follow m with TMono _ -> raise (Unify_error []) | _ -> unify uctx m (map tc) ) constr
					| _ -> ()
				) monos cf.cf_params;
				unify_func (map t) b;
			| _ -> die "" __LOC__)

and unify_with_variance uctx f t1 t2 =
	let allows_variance_to t tf = type_iseq tf t in
	match follow t1,follow t2 with
	| TInst(c1,tl1),TInst(c2,tl2) when c1 == c2 ->
		List.iter2 f tl1 tl2
	| TEnum(en1,tl1),TEnum(en2,tl2) when en1 == en2 ->
		List.iter2 f tl1 tl2
	| TAbstract(a1,tl1),TAbstract(a2,tl2) when a1 == a2 && Meta.has Meta.CoreType a1.a_meta ->
		List.iter2 f tl1 tl2
	| TAbstract(a1,pl1),TAbstract(a2,pl2) ->
		if (Meta.has Meta.CoreType a1.a_meta) && (Meta.has Meta.CoreType a2.a_meta) then begin
			let ta1 = apply_params a1.a_params pl1 a1.a_this in
			let ta2 = apply_params a2.a_params pl2 a2.a_this in
			type_eq {uctx with equality_kind = EqStrict} ta1 ta2;
		end;
		if not (List.exists (allows_variance_to t2) a1.a_to) && not (List.exists (allows_variance_to t1) a2.a_from) then
			error [cannot_unify t1 t2]
	| TAbstract(a,pl),t ->
		type_eq {uctx with equality_kind = EqBothDynamic} (apply_params a.a_params pl a.a_this) t;
		if not (List.exists (fun t2 -> allows_variance_to t (apply_params a.a_params pl t2)) a.a_to) then error [cannot_unify t1 t2]
	| t,TAbstract(a,pl) ->
		type_eq {uctx with equality_kind = EqBothDynamic} t (apply_params a.a_params pl a.a_this);
		if not (List.exists (fun t2 -> allows_variance_to t (apply_params a.a_params pl t2)) a.a_from) then error [cannot_unify t1 t2]
	| (TAnon a1 as t1), (TAnon a2 as t2) ->
		rec_stack unify_stack (t1,t2)
			(fun (a,b) -> fast_eq a t1 && fast_eq b t2)
			(fun() -> unify_anons uctx t1 t2 a1 a2)
			(fun l -> error l)
	| _ ->
		error [cannot_unify t1 t2]

and unify_type_params uctx a b tl1 tl2 =
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
		unify_with_variance uctx (with_variance uctx f) t1 t2
	with Unify_error _ ->
		raise (Unify_error l)

and unify_with_access uctx f1 t1 f2 =
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

;;
unify_ref := unify_custom;;