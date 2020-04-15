open Globals
open DisplayTypes.DisplayMode
open Common
open Ast
open Type
open Typecore
open Error

exception Generic_Exception of string * pos

type generic_context = {
	ctx : typer;
	subst : (t * (t * texpr option)) list;
	name : string;
	p : pos;
	mutable mg : module_def option;
}

let generic_check_const_expr ctx t =
	match follow t with
	| TInst({cl_kind = KExpr e},_) ->
		let e = type_expr {ctx with locals = PMap.empty} e WithType.value in
		e.etype,Some e
	| _ -> t,None

let make_generic ctx ps pt p =
	let rec loop l1 l2 =
		match l1, l2 with
		| [] , [] -> []
		| (x,TLazy f) :: l1, _ -> loop ((x,lazy_type f) :: l1) l2
		| (_,t1) :: l1 , t2 :: l2 ->
			let t,eo = generic_check_const_expr ctx t2 in
			(t1,(t,eo)) :: loop l1 l2
		| _ -> die()
	in
	let name =
		String.concat "_" (List.map2 (fun (s,_) t ->
			let rec subst s = "_" ^ string_of_int (Char.code (String.get (Str.matched_string s) 0)) ^ "_" in
			let ident_safe = Str.global_substitute (Str.regexp "[^a-zA-Z0-9_]") subst in
			let s_type_path_underscore (p,s) = match p with [] -> s | _ -> String.concat "_" p ^ "_" ^ s in
			let rec loop top t = match t with
				| TInst(c,tl) -> (match c.cl_kind with
					| KExpr e -> ident_safe (Ast.Printer.s_expr e)
					| _ -> (ident_safe (s_type_path_underscore c.cl_path)) ^ (loop_tl top tl))
				| TType (td,tl) -> (s_type_path_underscore td.t_path) ^ (loop_tl top tl)
				| TEnum(en,tl) -> (s_type_path_underscore en.e_path) ^ (loop_tl top tl)
				| TAnon(a) -> "anon_" ^ String.concat "_" (PMap.foldi (fun s f acc -> (s ^ "_" ^ (loop false (follow f.cf_type))) :: acc) a.a_fields [])
				| TFun(args, return_type) -> "func_" ^ (String.concat "_" (List.map (fun (_, _, t) -> loop false t) args)) ^ "_" ^ (loop false return_type)
				| TAbstract(a,tl) -> (s_type_path_underscore a.a_path) ^ (loop_tl top tl)
				| _ when not top ->
					follow_or t top (fun() -> "_") (* allow unknown/incompatible types as type parameters to retain old behavior *)
				| TMono { tm_type = None } -> raise (Generic_Exception (("Could not determine type for parameter " ^ s), p))
				| TDynamic _ -> "Dynamic"
				| t ->
					follow_or t top (fun() -> raise (Generic_Exception (("Unsupported type parameter: " ^ (s_type (print_context()) t) ^ ")"), p)))
			and loop_tl top tl = match tl with
				| [] -> ""
				| tl -> "_" ^ String.concat "_" (List.map (loop top) tl)
			and follow_or t top or_fn =
				let ft = follow_once t in
				if ft == t then or_fn()
				else loop top ft
			in
			loop true t
		) ps pt)
	in
	{
		ctx = ctx;
		subst = loop ps pt;
		name = name;
		p = p;
		mg = None;
	}

let rec generic_substitute_type gctx t =
	match t with
	| TInst ({ cl_kind = KGeneric } as c2,tl2) ->
		(* maybe loop, or generate cascading generics *)
		let _, _, f = gctx.ctx.g.do_build_instance gctx.ctx (TClassDecl c2) gctx.p in
		let t = f (List.map (generic_substitute_type gctx) tl2) in
		(match follow t,gctx.mg with TInst(c,_), Some m -> add_dependency m c.cl_module | _ -> ());
		t
	| _ ->
		try
			let t,_ = List.assq t gctx.subst in
			generic_substitute_type gctx t
		with Not_found ->
			Type.map (generic_substitute_type gctx) t

let generic_substitute_expr gctx e =
	let vars = Hashtbl.create 0 in
	let build_var v =
		try
			Hashtbl.find vars v.v_id
		with Not_found ->
			let v2 = alloc_var v.v_kind v.v_name (generic_substitute_type gctx v.v_type) v.v_pos in
			v2.v_meta <- v.v_meta;
			Hashtbl.add vars v.v_id v2;
			v2
	in
	let rec build_expr e =
		match e.eexpr with
		| TField(e1, FInstance({cl_kind = KGeneric} as c,tl,cf)) ->
			let _, _, f = gctx.ctx.g.do_build_instance gctx.ctx (TClassDecl c) gctx.p in
			let t = f (List.map (generic_substitute_type gctx) tl) in
			begin match follow t with
			| TInst(c',_) when c == c' ->
				(* The @:generic class wasn't expanded, let's not recurse to avoid infinite loop (#6430) *)
				map_expr_type build_expr (generic_substitute_type gctx) build_var e
			| _ ->
				let fa = try
					quick_field t cf.cf_name
				with Not_found ->
					error (Printf.sprintf "Type %s has no field %s (possible typing order issue)" (s_type (print_context()) t) cf.cf_name) e.epos
				in
				build_expr {e with eexpr = TField(e1,fa)}
			end;
		| TTypeExpr (TClassDecl ({cl_kind = KTypeParameter _;} as c)) when Meta.has Meta.Const c.cl_meta ->
			let rec loop subst = match subst with
				| (t1,(_,eo)) :: subst ->
					begin match follow t1 with
						| TInst(c2,_) when c == c2 -> eo
						| _ -> loop subst
					end
				| [] -> raise Not_found
			in
			begin try
				let eo = loop gctx.subst in
				begin match eo with
					| Some e -> e
					| None -> error "Only Const type parameters can be used as value" e.epos
				end
			with Not_found ->
				e
			end
		| _ ->
			map_expr_type build_expr (generic_substitute_type gctx) build_var e
	in
	build_expr e

let get_short_name =
	let i = ref (-1) in
	(fun () ->
		incr i;
		Printf.sprintf "Hx___short___hx_type_%i" !i
	)

let static_method_container gctx c cf p =
	let ctx = gctx.ctx in
	let pack = fst c.cl_path in
	let name = (snd c.cl_path) ^ "_" ^ cf.cf_name ^ "_" ^ gctx.name in
	try
		let t = Typeload.load_instance ctx (mk_type_path (pack,name),p) true in
		match t with
		| TInst(cg,_) -> cg
		| _ -> error ("Cannot specialize @:generic static method because the generated type name is already used: " ^ name) p
	with Error(Module_not_found path,_) when path = (pack,name) ->
		let m = (try Hashtbl.find ctx.g.modules (Hashtbl.find ctx.g.types_module c.cl_path) with Not_found -> die()) in
		let mg = {
			m_id = alloc_mid();
			m_path = (pack,name);
			m_types = [];
			m_extra = module_extra (s_type_path (pack,name)) m.m_extra.m_sign 0. MFake m.m_extra.m_check_policy;
		} in
		gctx.mg <- Some mg;
		let cg = mk_class mg (pack,name) c.cl_pos null_pos in
		mg.m_types <- [TClassDecl cg];
		Hashtbl.add ctx.g.modules mg.m_path mg;
		add_dependency mg m;
		add_dependency ctx.m.curmod mg;
		cg

let rec build_generic ctx c p tl =
	let pack = fst c.cl_path in
	let recurse = ref false in
	let rec check_recursive t =
		match follow t with
		| TInst (c2,tl) ->
			(match c2.cl_kind with
			| KTypeParameter tl ->
				if not (TypeloadCheck.is_generic_parameter ctx c2) && has_ctor_constraint c2 then
					error "Type parameters with a constructor cannot be used non-generically" p;
				recurse := true
			| _ -> ());
			List.iter check_recursive tl;
		| _ ->
			()
	in
	List.iter check_recursive tl;
	if !recurse || not (ctx.com.display.dms_full_typing) then begin
		TInst (c,tl) (* build a normal instance *)
	end else begin
	let gctx = make_generic ctx c.cl_params tl p in
	let name = (snd c.cl_path) ^ "_" ^ gctx.name in
	try
		let t = Typeload.load_instance ctx (mk_type_path (pack,name),p) false in
		match t with
		| TInst({ cl_kind = KGenericInstance (csup,_) },_) when c == csup -> t
		| _ -> error ("Cannot specialize @:generic because the generated type name is already used: " ^ name) p
	with Error(Module_not_found path,_) when path = (pack,name) ->
		let m = (try Hashtbl.find ctx.g.modules (Hashtbl.find ctx.g.types_module c.cl_path) with Not_found -> die()) in
		(* let ctx = { ctx with m = { ctx.m with module_types = m.m_types @ ctx.m.module_types } } in *)
		ignore(c.cl_build()); (* make sure the super class is already setup *)
		let mg = {
			m_id = alloc_mid();
			m_path = (pack,name);
			m_types = [];
			m_extra = module_extra (s_type_path (pack,name)) m.m_extra.m_sign 0. MFake m.m_extra.m_check_policy;
		} in
		gctx.mg <- Some mg;
		let cg = mk_class mg (pack,name) c.cl_pos null_pos in
		mg.m_types <- [TClassDecl cg];
		Hashtbl.add ctx.g.modules mg.m_path mg;
		add_dependency mg m;
		add_dependency ctx.m.curmod mg;
		(* ensure that type parameters are set in dependencies *)
		let dep_stack = ref [] in
		let rec loop t =
			if not (List.memq t !dep_stack) then begin
			dep_stack := t :: !dep_stack;
			match t with
			| TInst (c,tl) -> add_dep c.cl_module tl
			| TEnum (e,tl) -> add_dep e.e_module tl
			| TType (t,tl) -> add_dep t.t_module tl
			| TAbstract (a,tl) -> add_dep a.a_module tl
			| TMono r ->
				(match r.tm_type with
				| None -> ()
				| Some t -> loop t)
			| TLazy f ->
				loop (lazy_type f);
			| TDynamic t2 ->
				if t == t2 then () else loop t2
			| TAnon a ->
				PMap.iter (fun _ f -> loop f.cf_type) a.a_fields
			| TFun (args,ret) ->
				List.iter (fun (_,_,t) -> loop t) args;
				loop ret
			end
		and add_dep m tl =
			add_dependency mg m;
			List.iter loop tl
		in
		List.iter loop tl;
		let build_field cf_old =
			(* We have to clone the type parameters (issue #4672). We cannot substitute the constraints immediately because
			   we need the full substitution list first. *)
			let param_subst,params = List.fold_left (fun (subst,params) (s,t) -> match follow t with
				| TInst(c,tl) as t ->
					let t2 = TInst({c with cl_module = mg;},tl) in
					(t,(t2,None)) :: subst,(s,t2) :: params
				| _ -> die()
			) ([],[]) cf_old.cf_params in
			let gctx = {gctx with subst = param_subst @ gctx.subst} in
			let cf_new = {cf_old with cf_pos = cf_old.cf_pos} in (* copy *)
			(* Type parameter constraints are substituted here. *)
			cf_new.cf_params <- List.rev_map (fun (s,t) -> match follow t with
				| TInst({cl_kind = KTypeParameter tl1} as c,_) ->
					let tl1 = List.map (generic_substitute_type gctx) tl1 in
					c.cl_kind <- KTypeParameter tl1;
					s,t
				| _ -> die()
			) params;
			let f () =
				let t = generic_substitute_type gctx cf_old.cf_type in
				ignore (follow t);
				begin try (match cf_old.cf_expr with
					| None ->
						begin match cf_old.cf_kind with
							| Method _ when not c.cl_interface && not c.cl_extern ->
								display_error ctx (Printf.sprintf "Field %s has no expression (possible typing order issue)" cf_new.cf_name) cf_new.cf_pos;
								display_error ctx (Printf.sprintf "While building %s" (s_type_path cg.cl_path)) p;
							| _ ->
								()
						end
					| Some e ->
						cf_new.cf_expr <- Some (generic_substitute_expr gctx e)
				) with Unify_error l ->
					error (error_msg (Unify l)) cf_new.cf_pos
				end;
				t
			in
			let r = exc_protect ctx (fun r ->
				let t = mk_mono() in
				r := lazy_processing (fun() -> t);
				let t0 = f() in
				unify_raise ctx t0 t p;
				link_dynamic t0 t;
				t
			) "build_generic" in
			cf_new.cf_type <- TLazy r;
			cf_new
		in
		if c.cl_init <> None then error "This class can't be generic" p;
		List.iter (fun cf -> match cf.cf_kind with
			| Method MethMacro when not ctx.in_macro -> ()
			| _ -> error "A generic class can't have static fields" cf.cf_pos
		) c.cl_ordered_statics;
		cg.cl_super <- (match c.cl_super with
			| None -> None
			| Some (cs,pl) ->
				let ts = follow (apply_params c.cl_params tl (TInst(cs,pl))) in
				let cs,pl = TypeloadCheck.Inheritance.check_extends ctx c ts p in
				match cs.cl_kind with
				| KGeneric ->
					(match build_generic ctx cs p pl with
					| TInst (cs,pl) -> Some (cs,pl)
					| _ -> die())
				| _ -> Some(cs,pl)
		);
		TypeloadFunction.add_constructor ctx cg false p;
		cg.cl_kind <- KGenericInstance (c,tl);
		cg.cl_meta <- (Meta.NoDoc,[],null_pos) :: cg.cl_meta;
		if has_meta Meta.Keep c.cl_meta then cg.cl_meta <- (Meta.Keep,[],null_pos) :: cg.cl_meta;
		cg.cl_interface <- c.cl_interface;
		cg.cl_constructor <- (match cg.cl_constructor, c.cl_constructor, c.cl_super with
			| _, Some cf, _ -> Some (build_field cf)
			| Some ctor, _, _ -> Some ctor
			| None, None, None -> None
			| _ -> error "Please define a constructor for this class in order to use it as generic" c.cl_pos
		);
		cg.cl_implements <- List.map (fun (i,tl) ->
			(match follow (generic_substitute_type gctx (TInst (i, List.map (generic_substitute_type gctx) tl))) with
			| TInst (i,tl) -> i, tl
			| _ -> die())
		) c.cl_implements;
		cg.cl_ordered_fields <- List.map (fun f ->
			let f = build_field f in
			cg.cl_fields <- PMap.add f.cf_name f cg.cl_fields;
			f
		) c.cl_ordered_fields;
		cg.cl_overrides <- List.map (fun f ->
			try PMap.find f.cf_name cg.cl_fields with Not_found -> die()
		) c.cl_overrides;
		(* In rare cases the class name can become too long, so let's shorten it (issue #3090). *)
		if String.length (snd cg.cl_path) > 254 then begin
			let n = get_short_name () in
			cg.cl_meta <- (Meta.Native,[EConst(String (n,SDoubleQuotes)),p],null_pos) :: cg.cl_meta;
		end;
		TInst (cg,[])
	end
