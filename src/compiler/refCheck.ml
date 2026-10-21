(*
	Stale reference checker (docs/incremental-compilation.md, phase 0 item 2
	and phase 2).

	Every reference from a module of the compilation to a type or a field must
	point to the object that its module currently holds: a reference to an
	object of an older version of a module is stale. Enabled with
	`-D incremental-check-refs`; each stale reference is reported as an error.

	Stale references are reported with their entity key (phase 2, item 1), and
	whether the current version of the module has an entity with the same key
	(which grafting would remap the reference to). The check also verifies that
	the entity keys of every module are unique.
*)
open Globals
open Type
open Common

let check com =
	let reported = Hashtbl.create 0 in
	let count = ref 0 in
	let report what p =
		if not (Hashtbl.mem reported (what,p)) then begin
			Hashtbl.add reported (what,p) ();
			incr count;
			if !count <= 20 then
				com.error_ext (Error.make_error (Error.Custom ("Stale reference: " ^ what)) p)
		end
	in
	let current_module m =
		try Some (com.module_lut#find m.m_path) with Not_found -> None
	in
	let cc = CommonCache.get_cache com in
	(* key of [e] in module [m], and whether module [m'] has an entity with that key *)
	let describe m m' e =
		let key = ref None in
		EntityKey.iter_module (fun k e' -> if !key = None && EntityKey.same_entity e e' then key := Some k) m;
		match !key with
		| None ->
			"no entity key"
		| Some k ->
			let index = try Some (cc#get_entity_index m') with Failure _ -> None in
			let graftable = match index with Some index -> Hashtbl.mem index k | None -> false in
			Printf.sprintf "key %s, %s in the current module" (EntityKey.to_string k) (if graftable then "present" else "absent")
	in
	(* A module type is valid if its module is the current module of its path
	   and it is one of that module's types (or its implementation class). *)
	let owns m mt =
		List.exists (fun mt' -> match mt', mt with
			| TClassDecl c1, TClassDecl c2 -> c1 == c2
			| TEnumDecl e1, TEnumDecl e2 -> e1 == e2
			| TTypeDecl t1, TTypeDecl t2 -> t1 == t2
			| TAbstractDecl a1, TAbstractDecl a2 -> a1 == a2
			| TAbstractDecl a, TClassDecl c -> (match a.a_impl with Some c' -> c == c' | None -> false)
			| _ -> false
		) m.m_types
		|| (match m.m_statics, mt with Some c, TClassDecl c' -> c == c' | _ -> false)
	in
	let check_module_type mt p =
		let infos = t_infos mt in
		let m = infos.mt_module in
		match mt with
		| TClassDecl { cl_kind = KTypeParameter ttp } when m != null_module ->
			(* a type parameter belongs to the module of its host *)
			begin match current_module m with
			| Some m' when m' != m ->
				report (Printf.sprintf "type parameter %s refers to an older version of module %s (%s)" ttp.ttp_name (s_type_path m.m_path) (describe m m' (EntityKey.ETypeParam ttp))) p
			| _ ->
				()
			end
		| TClassDecl { cl_kind = KTypeParameter _ | KGenericInstance _ | KExpr _ } ->
			()
		| _ when m == null_module ->
			()
		| TTypeDecl { t_path = (_,name) } when String.contains name '<' ->
			(* Synthetic typedefs such as `Class<T>` and `Enum<T>`, created for
			   static accesses, are not types of their module. *)
			()
		| _ ->
			match current_module m with
			| None ->
				()
			| Some m' when m' != m ->
				report (Printf.sprintf "%s refers to an older version of module %s (%s)" (s_type_path infos.mt_path) (s_type_path m.m_path) (describe m m' (EntityKey.EType mt))) p
			| Some m' ->
				if not (owns m' mt) then
					report (Printf.sprintf "%s is not a type of module %s" (s_type_path infos.mt_path) (s_type_path m.m_path)) p
	in
	let check_class_field c cf p =
		let rec in_list l = List.exists (fun cf' -> cf' == cf || in_list cf'.cf_overloads) l in
		if not (in_list c.cl_ordered_fields || in_list c.cl_ordered_statics
			|| (match c.cl_constructor with Some cf' -> cf' == cf || in_list cf'.cf_overloads | None -> false)
			|| (match c.cl_kind with KTypeParameter _ | KGenericInstance _ | KExpr _ -> true | _ -> false)) then
			report (Printf.sprintf "field %s is not a field of %s" cf.cf_name (s_type_path c.cl_path)) p
	in
	let visited_anons = ref [] in
	let rec check_t p t = match t with
		| TInst(c,tl) -> check_module_type (TClassDecl c) p; List.iter (check_t p) tl
		| TEnum(e,tl) -> check_module_type (TEnumDecl e) p; List.iter (check_t p) tl
		| TType(td,tl) -> check_module_type (TTypeDecl td) p; List.iter (check_t p) tl
		| TAbstract(a,tl) -> check_module_type (TAbstractDecl a) p; List.iter (check_t p) tl
		| TFun(args,ret) -> List.iter (fun (_,_,t) -> check_t p t) args; check_t p ret
		| TAnon an ->
			if not (List.memq an !visited_anons) then begin
				visited_anons := an :: !visited_anons;
				PMap.iter (fun _ cf -> check_t p cf.cf_type) an.a_fields
			end
		| TMono r -> (match r.tm_type with Some t -> check_t p t | None -> ())
		| TDynamic (Some t) -> check_t p t
		| TDynamic None | TLazy _ -> ()
	in
	let rec check_expr e =
		begin match e.eexpr with
		| TTypeExpr mt -> check_module_type mt e.epos
		| TNew(c,_,_) -> check_module_type (TClassDecl c) e.epos
		| TField(_,(FInstance(c,_,cf) | FStatic(c,cf) | FClosure(Some(c,_),cf))) ->
			check_module_type (TClassDecl c) e.epos;
			check_class_field c cf e.epos
		| TField({etype = TType({t_params = []} as td,_)},FAnon cf) ->
			(* a field of the structure a typedef declares *)
			let rec declared t = match t with
				| TMono { tm_type = Some t } -> declared t
				| TLazy r -> (match !r with LAvailable t -> declared t | _ -> t)
				| _ -> t
			in
			(* the typedef of the current version of its module *)
			let td = match current_module td.t_module with
				| Some m' -> (try List.find_map (function TTypeDecl td' when td'.t_path = td.t_path -> Some td' | _ -> None) m'.m_types |> Option.get with _ -> td)
				| None -> td
			in
			begin match declared td.t_type with
			| TAnon an when PMap.mem cf.cf_name an.a_fields && PMap.find cf.cf_name an.a_fields != cf ->
				report (Printf.sprintf "field %s is not a field of the structure of %s" cf.cf_name (s_type_path td.t_path)) e.epos
			| _ ->
				()
			end
		| TField(_,FEnum(en,ef)) ->
			check_module_type (TEnumDecl en) e.epos;
			if not (try PMap.find ef.ef_name en.e_constrs == ef with Not_found -> false) then
				report (Printf.sprintf "constructor %s is not a constructor of %s" ef.ef_name (s_type_path en.e_path)) e.epos
		| _ -> ()
		end;
		Type.iter check_expr e
	in
	let check_field cf =
		check_t cf.cf_pos cf.cf_type;
		Option.may check_expr cf.cf_expr;
		List.iter (fun cf -> check_t cf.cf_pos cf.cf_type) cf.cf_overloads
	in
	List.iter (fun m ->
		(try ignore (cc#get_entity_index m) with Failure msg -> report (Printf.sprintf "module %s: %s" (s_type_path m.m_path) msg) null_pos);
		List.iter (fun mt -> match mt with
			| TClassDecl c ->
				Option.may (fun (csup,tl) -> check_module_type (TClassDecl csup) c.cl_pos; List.iter (check_t c.cl_pos) tl) c.cl_super;
				List.iter (fun (ci,tl) -> check_module_type (TClassDecl ci) c.cl_pos; List.iter (check_t c.cl_pos) tl) c.cl_implements;
				List.iter check_field c.cl_ordered_fields;
				List.iter check_field c.cl_ordered_statics;
				Option.may check_field c.cl_constructor;
				Option.may check_expr (TClass.get_cl_init c)
			| TEnumDecl en ->
				PMap.iter (fun _ ef -> check_t ef.ef_pos ef.ef_type) en.e_constrs
			| TTypeDecl td ->
				check_t td.t_pos td.t_type
			| TAbstractDecl a ->
				check_t a.a_pos a.a_this
		) m.m_types
	) com.modules;
	if !count > 20 then
		com.error_ext (Error.make_error (Error.Custom (Printf.sprintf "Stale reference: %d more" (!count - 20))) null_pos)

let check com =
	if Common.defined com Define.IncrementalCheckRefs then
		Timer.time com.timer_ctx ["incremental";"check refs"] check com
