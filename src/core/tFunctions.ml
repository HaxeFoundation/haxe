open Globals
open Ast
open TType

let monomorph_create_ref : (unit -> tmono) ref = ref (fun _ -> die "" __LOC__)
let monomorph_bind_ref : (tmono -> t -> unit) ref = ref (fun _ _ -> die "" __LOC__)
let monomorph_classify_constraints_ref : (tmono -> tmono_constraint_kind) ref = ref (fun _ -> die "" __LOC__)

let has_meta m ml = List.exists (fun (m2,_,_) -> m = m2) ml
let get_meta m ml = List.find (fun (m2,_,_) -> m = m2) ml

(* Flags *)

let has_flag flags flag =
	flags land (1 lsl flag) > 0

let set_flag flags flag =
	flags lor (1 lsl flag)

let unset_flag flags flag =
	flags land (lnot (1 lsl flag))

let int_of_class_flag (flag : flag_tclass) =
	Obj.magic flag

let add_class_flag c (flag : flag_tclass) =
	c.cl_flags <- set_flag c.cl_flags (int_of_class_flag flag)

let remove_class_flag c (flag : flag_tclass) =
	c.cl_flags <- unset_flag c.cl_flags (int_of_class_flag flag)

let has_class_flag c (flag : flag_tclass) =
	has_flag c.cl_flags (int_of_class_flag flag)

let int_of_class_field_flag (flag : flag_tclass_field) =
	Obj.magic flag

let add_class_field_flag cf (flag : flag_tclass_field) =
	cf.cf_flags <- set_flag cf.cf_flags (int_of_class_field_flag flag)

let remove_class_field_flag cf (flag : flag_tclass_field) =
	cf.cf_flags <- unset_flag cf.cf_flags (int_of_class_field_flag flag)

let has_class_field_flag cf (flag : flag_tclass_field) =
	has_flag cf.cf_flags (int_of_class_field_flag flag)

let int_of_var_flag (flag : flag_tvar) =
	Obj.magic flag

let add_var_flag v (flag : flag_tvar) =
	v.v_flags <- set_flag v.v_flags (int_of_var_flag flag)

let remove_var_flag v (flag : flag_tvar) =
	v.v_flags <- unset_flag v.v_flags (int_of_var_flag flag)

let has_var_flag v (flag : flag_tvar) =
	has_flag v.v_flags (int_of_var_flag flag)

(* ======= General utility ======= *)

let alloc_var =
	let uid = ref 0 in
	(fun kind n t p ->
		incr uid;
		{
			v_kind = kind;
			v_name = n;
			v_type = t;
			v_id = !uid;
			v_extra = None;
			v_meta = [];
			v_pos = p;
			v_flags = (match kind with VUser TVOLocalFunction -> int_of_var_flag VFinal | _ -> 0);
		}
	)

let alloc_mid =
	let mid = ref 0 in
	(fun() -> incr mid; !mid)

let mk e t p = { eexpr = e; etype = t; epos = p }

let mk_block e =
	match e.eexpr with
	| TBlock _ -> e
	| _ -> mk (TBlock [e]) e.etype e.epos

let mk_cast e t p = mk (TCast(e,None)) t p

let null t p = mk (TConst TNull) t p

let mk_mono() = TMono (!monomorph_create_ref ())

let rec t_dynamic = TDynamic t_dynamic

let mk_anon ?fields status =
	let fields = match fields with Some fields -> fields | None -> PMap.empty in
	TAnon { a_fields = fields; a_status = status; }

(* We use this for display purposes because otherwise we never see the Dynamic type that
   is defined in StdTypes.hx. This is set each time a typer is created, but this is fine
   because Dynamic is the same in all contexts. If this ever changes we'll have to review
   how we handle this. *)
let t_dynamic_def = ref t_dynamic

let tfun pl r = TFun (List.map (fun t -> "",false,t) pl,r)

let fun_args l = List.map (fun (a,c,t) -> a, c <> None, t) l

let mk_class m path pos name_pos =
	{
		cl_path = path;
		cl_module = m;
		cl_pos = pos;
		cl_name_pos = name_pos;
		cl_doc = None;
		cl_meta = [];
		cl_private = false;
		cl_kind = KNormal;
		cl_flags = 0;
		cl_params = [];
		cl_using = [];
		cl_super = None;
		cl_implements = [];
		cl_fields = PMap.empty;
		cl_ordered_statics = [];
		cl_ordered_fields = [];
		cl_statics = PMap.empty;
		cl_dynamic = None;
		cl_array_access = None;
		cl_constructor = None;
		cl_init = None;
		cl_build = (fun() -> Built);
		cl_restore = (fun() -> ());
		cl_descendants = [];
	}

let mk_typedef m path pos name_pos t =
	{
		t_path = path;
		t_module = m;
		t_pos = pos;
		t_name_pos = name_pos;
		t_private = false;
		t_doc = None;
		t_meta = [];
		t_params = [];
		t_using = [];
		t_type = t;
	}

let module_extra file sign time kind policy =
	{
		m_file = Path.UniqueKey.create_lazy file;
		m_sign = sign;
		m_display = {
			m_inline_calls = [];
			m_type_hints = [];
			m_import_positions = PMap.empty;
		};
		m_cache_state = MSGood;
		m_added = 0;
		m_checked = 0;
		m_time = time;
		m_processed = 0;
		m_deps = PMap.empty;
		m_kind = kind;
		m_binded_res = PMap.empty;
		m_if_feature = [];
		m_features = Hashtbl.create 0;
		m_check_policy = policy;
	}


let mk_field name ?(public = true) ?(static = false) t p name_pos = {
	cf_name = name;
	cf_type = t;
	cf_pos = p;
	cf_name_pos = name_pos;
	cf_doc = None;
	cf_meta = [];
	cf_kind = Var { v_read = AccNormal; v_write = AccNormal };
	cf_expr = None;
	cf_expr_unoptimized = None;
	cf_params = [];
	cf_overloads = [];
	cf_flags = (
		let flags = if static then set_flag 0 (int_of_class_field_flag CfStatic) else 0 in
		if public then set_flag flags (int_of_class_field_flag CfPublic) else flags
	);
}

let null_module = {
		m_id = alloc_mid();
		m_path = [] , "";
		m_types = [];
		m_statics = None;
		m_extra = module_extra "" "" 0. MFake [];
	}

let null_class =
	let c = mk_class null_module ([],"") null_pos null_pos in
	c.cl_private <- true;
	c

let null_field = mk_field "" t_dynamic null_pos null_pos

let null_abstract = {
	a_path = ([],"");
	a_module = null_module;
	a_pos = null_pos;
	a_name_pos = null_pos;
	a_private = true;
	a_doc = None;
	a_meta = [];
	a_params = [];
	a_using = [];
	a_ops = [];
	a_unops = [];
	a_impl = None;
	a_this = t_dynamic;
	a_from = [];
	a_from_field = [];
	a_to = [];
	a_to_field = [];
	a_array_read = [];
	a_array_write = [];
	a_read = None;
	a_write = None;
	a_call = None;
	a_enum = false;
}

let add_dependency m mdep =
	if m != null_module && m != mdep then begin
		m.m_extra.m_deps <- PMap.add mdep.m_id mdep m.m_extra.m_deps;
		(* In case the module is cached, we'll have to run post-processing on it again (issue #10635) *)
		m.m_extra.m_processed <- 0
	end

let arg_name (a,_) = a.v_name

let t_infos t : tinfos =
	match t with
	| TClassDecl c -> Obj.magic c
	| TEnumDecl e -> Obj.magic e
	| TTypeDecl t -> Obj.magic t
	| TAbstractDecl a -> Obj.magic a

let t_path t = (t_infos t).mt_path

let rec extends c csup =
	if c == csup || List.exists (fun (i,_) -> extends i csup) c.cl_implements then
		true
	else match c.cl_super with
		| None -> false
		| Some (c,_) -> extends c csup

let add_descendant c descendant =
	c.cl_descendants <- descendant :: c.cl_descendants

let lazy_type f =
	match !f with
	| LAvailable t -> t
	| LProcessing f | LWait f -> f()

let lazy_available t = LAvailable t
let lazy_processing f = LProcessing f
let lazy_wait f = LWait f

let map loop t =
	match t with
	| TMono r ->
		(match r.tm_type with
		| None -> t
		| Some t -> loop t) (* erase*)
	| TEnum (_,[]) | TInst (_,[]) | TType (_,[]) | TAbstract (_,[]) ->
		t
	| TEnum (e,tl) ->
		TEnum (e, List.map loop tl)
	| TInst (c,tl) ->
		TInst (c, List.map loop tl)
	| TType (t2,tl) ->
		TType (t2,List.map loop tl)
	| TAbstract (a,tl) ->
		TAbstract (a,List.map loop tl)
	| TFun (tl,r) ->
		TFun (List.map (fun (s,o,t) -> s, o, loop t) tl,loop r)
	| TAnon a ->
		let fields = PMap.map (fun f -> { f with cf_type = loop f.cf_type }) a.a_fields in
		mk_anon ~fields a.a_status
	| TLazy f ->
		let ft = lazy_type f in
		let ft2 = loop ft in
		if ft == ft2 then t else ft2
	| TDynamic t2 ->
		if t == t2 then	t else TDynamic (loop t2)

let iter loop t =
	match t with
	| TMono r ->
		(match r.tm_type with
		| None -> ()
		| Some t -> loop t)
	| TEnum (_,[]) | TInst (_,[]) | TType (_,[]) ->
		()
	| TEnum (e,tl) ->
		List.iter loop tl
	| TInst (c,tl) ->
		List.iter loop tl
	| TType (t2,tl) ->
		List.iter loop tl
	| TAbstract (a,tl) ->
		List.iter loop tl
	| TFun (tl,r) ->
		List.iter (fun (_,_,t) -> loop t) tl;
		loop r
	| TAnon a ->
		PMap.iter (fun _ f -> loop f.cf_type) a.a_fields
	| TLazy f ->
		let ft = lazy_type f in
		loop ft
	| TDynamic t2 ->
		if t != t2 then	loop t2

let duplicate t =
	let monos = ref [] in
	let rec loop t =
		match t with
		| TMono { tm_type = None } ->
			(try
				List.assq t !monos
			with Not_found ->
				let m = mk_mono() in
				monos := (t,m) :: !monos;
				m)
		| _ ->
			map loop t
	in
	loop t

let dynamify_monos t =
	let rec loop t =
		match t with
		| TMono { tm_type = None } ->
			t_dynamic
		| _ ->
			map loop t
	in
	loop t

exception ApplyParamsRecursion

(* substitute parameters with other types *)
let apply_params ?stack cparams params t =
	match cparams with
	| [] -> t
	| _ ->
	let rec loop l1 l2 =
		match l1, l2 with
		| [] , [] -> []
		| {ttp_type = TLazy f} as tp :: l1, _ -> loop ({tp with ttp_type = lazy_type f} :: l1) l2
		| tp :: l1 , t2 :: l2 -> (tp.ttp_type,t2) :: loop l1 l2
		| _ -> die "" __LOC__
	in
	let subst = loop cparams params in
	let rec loop t =
		try
			List.assq t subst
		with Not_found ->
		match t with
		| TMono r ->
			(match r.tm_type with
			| None -> t
			| Some t -> loop t)
		| TEnum (e,tl) ->
			(match tl with
			| [] -> t
			| _ -> TEnum (e,List.map loop tl))
		| TType (t2,tl) ->
			(match tl with
			| [] -> t
			| _ ->
				let new_applied_params = List.map loop tl in
				(match stack with
				| None -> ()
				| Some stack ->
					List.iter (fun (subject, old_applied_params) ->
						(*
							E.g.:
							```
							typedef Rec<T> = { function method():Rec<Array<T>> }
							```
							We need to make sure that we are not applying the result of previous
							application to the same place, which would mean the result of current
							application would go into `apply_params` again and then again and so on.

							Argument `stack` holds all previous results of `apply_params` to typedefs in current
							unification process.

							Imagine we are trying to unify `Rec<Int>` with something.

							Once `apply_params Array<T> Int Rec<Array<T>>` is called for the first time the result
							will be `Rec< Array<Int> >`. Store `Array<Int>` into `stack`

							Then the next params application looks like this:
								`apply_params Array<T> Array<Int> Rec<Array<T>>`
							Notice the second argument is actually the result of a previous `apply_params` call.
							And the result of the current call is `Rec< Array<Array<Int>> >`.

							The third call would be:
								`apply_params Array<T> Array<Array<Int>> Rec<Array<T>>`
							and so on.

							To stop infinite params application we need to check that we are trying to apply params
							produced by the previous `apply_params Array<Int> _ Rec<Array<T>>` to the same `Rec<Array<T>>`
						*)
						if
							subject == t (* Check the place that we're applying to is the same `Rec<Array<T>>` *)
							&& old_applied_params == params (* Check that params we're applying are the same params
																produced by the previous call to
																`apply_params Array<T> _ Rec<Array<T>>` *)
						then
							raise ApplyParamsRecursion
					) !stack;
					stack := (t, new_applied_params) :: !stack;
				);
				TType (t2,new_applied_params))
		| TAbstract (a,tl) ->
			(match tl with
			| [] -> t
			| _ -> TAbstract (a,List.map loop tl))
		| TInst (c,tl) ->
			(match tl with
			| [] ->
				t
			| [TMono r] ->
				(match r.tm_type with
				| Some tt when t == tt ->
					(* for dynamic *)
					let pt = mk_mono() in
					let t = TInst (c,[pt]) in
					(match pt with TMono r -> !monomorph_bind_ref r t | _ -> die "" __LOC__);
					t
				| _ -> TInst (c,List.map loop tl))
			| _ ->
				TInst (c,List.map loop tl))
		| TFun (tl,r) ->
			TFun (List.map (fun (s,o,t) -> s, o, loop t) tl,loop r)
		| TAnon a ->
			let fields = PMap.map (fun f -> { f with cf_type = loop f.cf_type }) a.a_fields in
			mk_anon ~fields a.a_status
		| TLazy f ->
			let ft = lazy_type f in
			let ft2 = loop ft in
			if ft == ft2 then
				t
			else
				ft2
		| TDynamic t2 ->
			if t == t2 then
				t
			else
				TDynamic (loop t2)
	in
	loop t

let apply_typedef td tl =
	apply_params td.t_params tl td.t_type

let monomorphs eparams t =
	apply_params eparams (List.map (fun _ -> mk_mono()) eparams) t

let apply_params_stack = ref []

let try_apply_params_rec cparams params t success =
	let old_stack = !apply_params_stack in
	try
		let result = success (apply_params ~stack:apply_params_stack cparams params t) in
		apply_params_stack := old_stack;
		result
	with
		| ApplyParamsRecursion ->
			apply_params_stack := old_stack;
		| err ->
			apply_params_stack := old_stack;
			raise err

let rec follow t =
	match t with
	| TMono r ->
		(match r.tm_type with
		| Some t -> follow t
		| _ -> t)
	| TLazy f ->
		follow (lazy_type f)
	| TType (t,tl) ->
		follow (apply_typedef t tl)
	| TAbstract({a_path = [],"Null"},[t]) ->
		follow t
	| _ -> t

let follow_once t =
	match t with
	| TMono r ->
		(match r.tm_type with
		| None -> t
		| Some t -> t)
	| TAbstract _ | TEnum _ | TInst _ | TFun _ | TAnon _ | TDynamic _ ->
		t
	| TType (t,tl) ->
		apply_typedef t tl
	| TLazy f ->
		lazy_type f

let rec follow_without_null t =
	match t with
	| TMono r ->
		(match r.tm_type with
		| Some t -> follow_without_null t
		| _ -> t)
	| TLazy f ->
		follow_without_null (lazy_type f)
	| TType (t,tl) ->
		follow_without_null (apply_typedef t tl)
	| _ -> t

let rec follow_without_type t =
	match t with
	| TMono r ->
		(match r.tm_type with
		| Some t -> follow_without_type t
		| _ -> t)
	| TLazy f ->
		follow_without_type (lazy_type f)
	| TAbstract({a_path = [],"Null"},[t]) ->
		follow_without_type t
	| _ -> t

let rec ambiguate_funs t =
	match follow t with
	| TFun _ -> TFun ([], t_dynamic)
	| _ -> map ambiguate_funs t

let rec is_nullable ?(no_lazy=false) = function
	| TMono r ->
		(match r.tm_type with None -> false | Some t -> is_nullable ~no_lazy t)
	| TAbstract ({ a_path = ([],"Null") },[_]) ->
		true
	| TLazy f ->
		(match !f with
		| LAvailable t -> is_nullable ~no_lazy t
		| _ when no_lazy -> raise Exit
		| _ -> is_nullable (lazy_type f)
		)
	| TType (t,tl) ->
		is_nullable ~no_lazy (apply_typedef t tl)
	| TFun _ ->
		false
(*
	Type parameters will most of the time be nullable objects, so we don't want to make it hard for users
	to have to specify Null<T> all over the place, so while they could be a basic type, let's assume they will not.

	This will still cause issues with inlining and haxe.rtti.Generic. In that case proper explicit Null<T> is required to
	work correctly with basic types. This could still be fixed by redoing a nullability inference on the typed AST.

	| TInst ({ cl_kind = KTypeParameter },_) -> false
*)
	| TAbstract (a,_) when Meta.has Meta.CoreType a.a_meta ->
		not (Meta.has Meta.NotNull a.a_meta)
	| TAbstract (a,tl) ->
		not (Meta.has Meta.NotNull a.a_meta) && is_nullable (apply_params a.a_params tl a.a_this)
	| _ ->
		true

let rec is_null ?(no_lazy=false) = function
	| TMono r ->
		(match r.tm_type with None -> false | Some t -> is_null ~no_lazy t)
	| TAbstract ({ a_path = ([],"Null") },[t]) ->
		not (is_nullable ~no_lazy (follow t))
	| TLazy f ->
		(match !f with
		| LAvailable t -> is_null ~no_lazy t
		| _ when no_lazy -> raise Exit
		| _ -> is_null (lazy_type f)
		)
	| TType (t,tl) ->
		is_null ~no_lazy (apply_typedef t tl)
	| _ ->
		false

(* Determines if we have a Null<T>. Unlike is_null, this returns true even if the wrapped type is nullable itself. *)
let rec is_explicit_null = function
	| TMono r ->
		(match r.tm_type with None -> false | Some t -> is_explicit_null t)
	| TAbstract ({ a_path = ([],"Null") },[t]) ->
		true
	| TLazy f ->
		is_explicit_null (lazy_type f)
	| TType (t,tl) ->
		is_explicit_null (apply_typedef t tl)
	| _ ->
		false

let rec has_mono t = match t with
	| TMono r ->
		(match r.tm_type with None -> true | Some t -> has_mono t)
	| TInst(_,pl) | TEnum(_,pl) | TAbstract(_,pl) | TType(_,pl) ->
		List.exists has_mono pl
	| TDynamic _ ->
		false
	| TFun(args,r) ->
		has_mono r || List.exists (fun (_,_,t) -> has_mono t) args
	| TAnon a ->
		PMap.fold (fun cf b -> has_mono cf.cf_type || b) a.a_fields false
	| TLazy f ->
		has_mono (lazy_type f)

let concat e1 e2 =
	let e = (match e1.eexpr, e2.eexpr with
		| TBlock el1, TBlock el2 -> TBlock (el1@el2)
		| TBlock el, _ -> TBlock (el @ [e2])
		| _, TBlock el -> TBlock (e1 :: el)
		| _ , _ -> TBlock [e1;e2]
	) in
	mk e e2.etype (punion e1.epos e2.epos)

let extract_param_type tp = tp.ttp_type
let extract_param_types = List.map extract_param_type
let extract_param_name tp = tp.ttp_name
let lookup_param n l =
	let rec loop l = match l with
		| [] ->
			raise Not_found
		| tp :: l ->
			if n = tp.ttp_name then tp.ttp_type else loop l
	in
	loop l

let mk_type_param n t def = {
	ttp_name = n;
	ttp_type = t;
	ttp_default = def;
}

let type_of_module_type = function
	| TClassDecl c -> TInst (c,extract_param_types c.cl_params)
	| TEnumDecl e -> TEnum (e,extract_param_types e.e_params)
	| TTypeDecl t -> TType (t,extract_param_types t.t_params)
	| TAbstractDecl a -> TAbstract (a,extract_param_types a.a_params)

let rec module_type_of_type = function
	| TInst(c,_) -> TClassDecl c
	| TEnum(en,_) -> TEnumDecl en
	| TType(t,_) -> TTypeDecl t
	| TAbstract(a,_) -> TAbstractDecl a
	| TLazy f -> module_type_of_type (lazy_type f)
	| TMono r ->
		(match r.tm_type with
		| Some t -> module_type_of_type t
		| _ -> raise Exit)
	| _ ->
		raise Exit

let tconst_to_const = function
	| TInt i -> Int (Int32.to_string i, None)
	| TFloat s -> Float (s, None)
	| TString s -> String(s,SDoubleQuotes)
	| TBool b -> Ident (if b then "true" else "false")
	| TNull -> Ident "null"
	| TThis -> Ident "this"
	| TSuper -> Ident "super"

let has_ctor_constraint c = match c.cl_kind with
	| KTypeParameter tl ->
		List.exists (fun t -> match follow t with
			| TAnon a when PMap.mem "new" a.a_fields -> true
			| TAbstract({a_path=["haxe"],"Constructible"},_) -> true
			| _ -> false
		) tl;
	| _ -> false

(* ======= Field utility ======= *)

let field_name f =
	match f with
	| FAnon f | FInstance (_,_,f) | FStatic (_,f) | FClosure (_,f) -> f.cf_name
	| FEnum (_,f) -> f.ef_name
	| FDynamic n -> n

let extract_field = function
	| FAnon f | FInstance (_,_,f) | FStatic (_,f) | FClosure (_,f) -> Some f
	| _ -> None

let is_physical_var_field f =
	match f.cf_kind with
	| Var { v_read = AccNormal | AccInline | AccNo } | Var { v_write = AccNormal | AccNo } -> true
	| Var _ -> Meta.has Meta.IsVar f.cf_meta
	| _ -> false

let is_physical_field f =
	match f.cf_kind with
	| Method _ -> true
	| _ -> is_physical_var_field f

let field_type f =
	match f.cf_params with
	| [] -> f.cf_type
	| l -> monomorphs l f.cf_type

let rec raw_class_field build_type c tl i =
	let apply = apply_params c.cl_params tl in
	try
		let f = PMap.find i c.cl_fields in
		Some (c,tl), build_type f , f
	with Not_found -> try (match c.cl_constructor with
		| Some ctor when i = "new" -> Some (c,tl), build_type ctor,ctor
		| _ -> raise Not_found)
	with Not_found -> try
		match c.cl_super with
		| None ->
			raise Not_found
		| Some (c,tl) ->
			let c2 , t , f = raw_class_field build_type c (List.map apply tl) i in
			c2, apply_params c.cl_params tl t , f
	with Not_found ->
		match c.cl_kind with
		| KTypeParameter tl ->
			let rec loop = function
				| [] ->
					raise Not_found
				| t :: ctl ->
					match follow t with
					| TAnon a ->
						(try
							let f = PMap.find i a.a_fields in
							None, build_type f, f
						with
							Not_found -> loop ctl)
					| TInst (c,tl) ->
						(try
							let c2, t , f = raw_class_field build_type c (List.map apply tl) i in
							c2, apply_params c.cl_params tl t, f
						with
							Not_found -> loop ctl)
					| _ ->
						loop ctl
			in
			loop tl
		| _ ->
			if not (has_class_flag c CInterface) then raise Not_found;
			(*
				an interface can implements other interfaces without
				having to redeclare its fields
			*)
			let rec loop = function
				| [] ->
					raise Not_found
				| (c,tl) :: l ->
					try
						let c2, t , f = raw_class_field build_type c (List.map apply tl) i in
						c2, apply_params c.cl_params tl t, f
					with
						Not_found -> loop l
			in
			loop c.cl_implements

let class_field = raw_class_field field_type

let quick_field t n =
	match follow t with
	| TInst (c,tl) ->
		let c, _, f = raw_class_field (fun f -> f.cf_type) c tl n in
		(match c with None -> FAnon f | Some (c,tl) -> FInstance (c,tl,f))
	| TAnon a ->
		(match !(a.a_status) with
		| EnumStatics e ->
			let ef = PMap.find n e.e_constrs in
			FEnum(e,ef)
		| Statics c ->
			FStatic (c,PMap.find n c.cl_statics)
		| AbstractStatics a ->
			begin match a.a_impl with
				| Some c ->
					let cf = PMap.find n c.cl_statics in
					FStatic(c,cf) (* is that right? *)
				| _ ->
					raise Not_found
			end
		| _ ->
			FAnon (PMap.find n a.a_fields))
	| TDynamic _ ->
		FDynamic n
	| TEnum _  | TMono _ | TAbstract _ | TFun _ ->
		raise Not_found
	| TLazy _ | TType _ ->
		die "" __LOC__

let quick_field_dynamic t s =
	try quick_field t s
	with Not_found -> FDynamic s

let rec get_constructor_class c tl =
	match c.cl_constructor, c.cl_super with
	| Some cf, _ -> (cf,c,tl)
	| None, None -> raise Not_found
	| None, Some (csup,tlsup) -> get_constructor_class csup (List.map (apply_params c.cl_params tl) tlsup)

let rec get_constructor c =
	match c.cl_constructor, c.cl_super with
	| Some c, _ -> c
	| None, None -> raise Not_found
	| None, Some (csup,_) -> get_constructor csup

let has_constructor c =
	try
		ignore(get_constructor c);
		true
	with Not_found -> false

let is_module_fields_class c =
	match c.cl_kind with KModuleFields _ -> true | _ -> false

let is_pos_outside_class c p =
	p.pfile <> c.cl_pos.pfile || p.pmax < c.cl_pos.pmin || p.pmin > c.cl_pos.pmax

let resolve_typedef t =
	match t with
	| TClassDecl _ | TEnumDecl _ | TAbstractDecl _ -> t
	| TTypeDecl td ->
		match follow td.t_type with
		| TEnum (e,_) -> TEnumDecl e
		| TInst (c,_) -> TClassDecl c
		| TAbstract (a,_) -> TAbstractDecl a
		| _ -> t

(**
	Check if type `t` has meta `m`.
	Does not follow typedefs, monomorphs etc.
*)
let type_has_meta t m =
	match t with
		| TMono _ | TFun _ | TAnon _ | TDynamic _ | TLazy _ -> false
		| TEnum ({ e_meta = metadata }, _)
		| TInst ({ cl_meta = metadata }, _)
		| TType ({ t_meta = metadata }, _)
		| TAbstract ({ a_meta = metadata }, _) -> has_meta m metadata

(* tvar *)

let var_extra params e = {
	v_params = params;
	v_expr = e;
}