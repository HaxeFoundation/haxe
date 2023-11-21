open Globals
open DisplayTypes.DisplayMode
open Common
open Ast
open Type
open Typecore
open Error

type generic_context = {
	ctx : typer;
	subst : (t * (t * texpr option)) list;
	name : string;
	p : pos;
	mutable mg : module_def option;
	generic_debug : bool;
}

let make_generic ctx ps pt debug p =
	let subst s = "_" ^ string_of_int (Char.code (String.get (Str.matched_string s) 0)) ^ "_" in
	let ident_safe = Str.global_substitute (Str.regexp "[^a-zA-Z0-9_]") subst in
	let s_type_path_underscore (p,s) = match p with [] -> s | _ -> String.concat "_" p ^ "_" ^ s in
	let process t =
		let rec loop top t = match t with
			| TInst(c,tl) ->
				begin match c.cl_kind with
					| KExpr e ->
						let name = ident_safe (Ast.Printer.s_expr e) in
						let e = type_expr {ctx with locals = PMap.empty} e WithType.value in
						name,(t,Some e)
					| _ ->
						((ident_safe (s_type_path_underscore c.cl_path)) ^ (loop_tl top tl),(t,None))
				end
			| TType (td,tl) ->
				(s_type_path_underscore td.t_path) ^ (loop_tl top tl),(t,None)
			| TEnum(en,tl) ->
				(s_type_path_underscore en.e_path) ^ (loop_tl top tl),(t,None)
			| TAnon(a) ->
				"anon_" ^ String.concat "_" (PMap.foldi (fun s f acc -> (s ^ "_" ^ (loop_deep (follow f.cf_type))) :: acc) a.a_fields []),(t,None)
			| TFun(args, return_type) ->
				("func_" ^ (String.concat "_" (List.map (fun (_, _, t) -> loop_deep t) args)) ^ "_" ^ (loop_deep return_type)),(t,None)
			| TAbstract(a,tl) ->
				(s_type_path_underscore a.a_path) ^ (loop_tl top tl),(t,None)
			| TDynamic _ ->
				"Dynamic",(t,None)
			| TMono { tm_type = None } ->
				if not top then
					"_",(t,None)
				else
					raise Exit
			| TMono { tm_type = Some t} ->
				loop top t
			| TLazy f ->
				loop top (lazy_type f)
		and loop_tl top tl = match tl with
			| [] -> ""
			| tl -> "_" ^ String.concat "_" (List.map (fun t -> fst (loop top t)) tl)
		and loop_deep t =
			fst (loop false t)
		in
		loop true t
	in
	let rec loop acc_name acc_subst ttpl tl = match ttpl,tl with
		| ttp :: ttpl,t :: tl ->
			let name,t = try process t with Exit -> raise_typing_error ("Could not determine type for parameter " ^ ttp.ttp_name) p in
			loop (name :: acc_name) ((follow ttp.ttp_type,t) :: acc_subst) ttpl tl
		| [],[] ->
			let name = String.concat "_" (List.rev acc_name) in
			name,acc_subst
		| _ ->
			die "" __LOC__
	in
	let name,subst = loop [] [] ps pt in
	{
		ctx = ctx;
		subst = subst;
		name = name;
		p = p;
		mg = None;
		generic_debug = debug;
	}

let rec generic_substitute_type' gctx allow_expr t =
	match t with
	| TInst ({ cl_kind = KGeneric } as c2,tl2) ->
		(* maybe loop, or generate cascading generics *)
		let info = gctx.ctx.g.get_build_info gctx.ctx (TClassDecl c2) gctx.p in
		let t = info.build_apply (List.map (generic_substitute_type' gctx true) tl2) in
		(match follow t,gctx.mg with TInst(c,_), Some m -> add_dependency m c.cl_module | _ -> ());
		t
	| _ ->
		try
			let t,eo = List.assq t gctx.subst in
			(* Somewhat awkward: If we allow expression types, use the original KExpr one. This is so
			   recursing into further KGeneric expands correctly. *)
			begin match eo with
			| Some e when not allow_expr ->
				e.etype
			| _ ->
				generic_substitute_type' gctx false t
			end
		with Not_found ->
			Type.map (generic_substitute_type' gctx allow_expr) t

let generic_substitute_type gctx t =
	generic_substitute_type' gctx false t

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
		let e = match e.eexpr with
		| TField(e1, FInstance({cl_kind = KGeneric} as c,tl,cf)) ->
			let info = gctx.ctx.g.get_build_info gctx.ctx (TClassDecl c) gctx.p in
			let t = info.build_apply (List.map (generic_substitute_type' gctx true) tl) in
			begin match follow t with
			| TInst(c',_) when c == c' ->
				(* The @:generic class wasn't expanded, let's not recurse to avoid infinite loop (#6430) *)
				map_expr_type build_expr (generic_substitute_type gctx) build_var e
			| _ ->
				let fa = try
					quick_field t cf.cf_name
				with Not_found ->
					raise_typing_error (Printf.sprintf "Type %s has no field %s (possible typing order issue)" (s_type (print_context()) t) cf.cf_name) e.epos
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
					| None -> raise_typing_error "Only Const type parameters can be used as value" e.epos
				end
			with Not_found ->
				e
			end
		| _ ->
			map_expr_type build_expr (generic_substitute_type gctx) build_var e
		in
		CallUnification.maybe_reapply_overload_call gctx.ctx e
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
		let t = Typeload.load_instance ctx (mk_type_path (pack,name),p) ParamSpawnMonos in
		match t with
		| TInst(cg,_) -> cg
		| _ -> raise_typing_error ("Cannot specialize @:generic static method because the generated type name is already used: " ^ name) p
	with Error { err_message = Module_not_found path } when path = (pack,name) ->
		let m = (try ctx.com.module_lut#find (ctx.com.type_to_module#find c.cl_path) with Not_found -> die "" __LOC__) in
		let mg = {
			m_id = alloc_mid();
			m_path = (pack,name);
			m_types = [];
			m_statics = None;
			m_extra = module_extra (s_type_path (pack,name)) m.m_extra.m_sign 0. MFake m.m_extra.m_check_policy;
		} in
		gctx.mg <- Some mg;
		let cg = mk_class mg (pack,name) c.cl_pos c.cl_name_pos in
		mg.m_types <- [TClassDecl cg];
		ctx.com.module_lut#add mg.m_path mg;
		add_dependency mg m;
		add_dependency ctx.m.curmod mg;
		cg

let set_type_parameter_dependencies mg tl =
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
		| TDynamic None ->
			()
		| TDynamic (Some t2) ->
			loop t2
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
	List.iter loop tl

let build_instances ctx t p =
	let rec loop t =
		let t = Typeload.maybe_build_instance ctx t ParamNormal p in
		Type.map loop t
	in
	loop t


let build_generic_class ctx c p tl =
	let pack = fst c.cl_path in
	let recurse = ref false in
	let rec check_recursive t =
		match follow t with
		| TInst (c2,tl) ->
			(match c2.cl_kind with
			| KTypeParameter tl ->
				if not (TypeloadCheck.is_generic_parameter ctx c2) && has_ctor_constraint c2 then
					raise_typing_error "Type parameters with a constructor cannot be used non-generically" p;
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
	let gctx = make_generic ctx c.cl_params tl (Meta.has (Meta.Custom ":debug.generic") c.cl_meta) p in
	let name = (snd c.cl_path) ^ "_" ^ gctx.name in
	try
		let t = Typeload.load_instance ctx (mk_type_path (pack,name),p) ParamNormal in
		match t with
		| TInst({ cl_kind = KGenericInstance (csup,_) },_) when c == csup -> t
		| _ -> raise_typing_error ("Cannot specialize @:generic because the generated type name is already used: " ^ name) p
	with Error { err_message = Module_not_found path } when path = (pack,name) ->
		let m = (try ctx.com.module_lut#find (ctx.com.type_to_module#find c.cl_path) with Not_found -> die "" __LOC__) in
		if gctx.generic_debug then begin
			print_endline (Printf.sprintf "[GENERIC] Building @:generic class %s as %s with:" (s_type_path c.cl_path) name);
			List.iter (fun (t1,(t2,eo)) ->
				let name = match follow t1 with
					| TInst(c,_) -> snd c.cl_path
					| _ -> die "" __LOC__
				in
				let expr = match eo with
					| None -> ""
					| Some e -> Printf.sprintf " (expr: %s)" (s_expr_debug e)
				in
				print_endline (Printf.sprintf "[GENERIC]   %s: %s%s" name (s_type_kind t2) expr);
			) gctx.subst
		end;
		ignore(c.cl_build()); (* make sure the super class is already setup *)
		let mg = {
			m_id = alloc_mid();
			m_path = (pack,name);
			m_types = [];
			m_statics = None;
			m_extra = module_extra (s_type_path (pack,name)) m.m_extra.m_sign 0. MFake m.m_extra.m_check_policy;
		} in
		gctx.mg <- Some mg;
		let cg = mk_class mg (pack,name) c.cl_pos c.cl_name_pos in
		cg.cl_meta <- List.filter (fun (m,_,_) -> match m with
			| Meta.Access | Allow
			| Final
			| Hack
			| Internal
			| Keep | KeepSub
			| NoClosure | NullSafety
			| Pure
			| Struct | StructInit
			| Using
			| AutoBuild ->
				true
			| _ ->
				false
		) c.cl_meta;
		cg.cl_meta <- (Meta.NoDoc,[],null_pos) :: cg.cl_meta;
		mg.m_types <- [TClassDecl cg];
		ctx.com.module_lut#add mg.m_path mg;
		add_dependency mg m;
		add_dependency ctx.m.curmod mg;
		set_type_parameter_dependencies mg tl;
		let build_field cf_old =
			(* We have to clone the type parameters (issue #4672). We cannot substitute the constraints immediately because
			   we need the full substitution list first. *)
			let param_subst,params = List.fold_left (fun (subst,params) tp -> match follow tp.ttp_type with
				| TInst(c,tl) as t ->
					let t2 = TInst({c with cl_module = mg;},tl) in
					(t,(t2,None)) :: subst,({tp with ttp_type=t2}) :: params
				| _ -> die "" __LOC__
			) ([],[]) cf_old.cf_params in
			let gctx = {gctx with subst = param_subst @ gctx.subst} in
			let cf_new = {cf_old with cf_pos = cf_old.cf_pos; cf_expr_unoptimized = None} in (* copy *)
			(* Type parameter constraints are substituted here. *)
			cf_new.cf_params <- List.rev_map (fun tp -> match follow tp.ttp_type with
				| TInst({cl_kind = KTypeParameter tl1} as c,_) ->
					let tl1 = List.map (generic_substitute_type gctx) tl1 in
					c.cl_kind <- KTypeParameter tl1;
					tp (* TPTODO: weird mapping *)
				| _ -> die "" __LOC__
			) params;
			let f () =
				ignore(follow cf_old.cf_type);
				(* We update here because the follow could resolve some TLazy things that end up modifying flags, such as
				   the inferred CfOverride from #11010. *)
				cf_new.cf_flags <- cf_old.cf_flags;
				remove_class_field_flag cf_new CfPostProcessed;
				if gctx.generic_debug then print_endline (Printf.sprintf "[GENERIC] expanding %s" cf_old.cf_name);
				let t = generic_substitute_type gctx cf_old.cf_type in
				let update_expr e =
					cf_new.cf_expr <- Some (generic_substitute_expr gctx e)
				in
				begin match cf_old.cf_expr with
					| Some e ->
						update_expr e
					| None ->
						(* There can be cases like #11152 where cf_expr isn't ready yet. It should be safe to delay this to the end
						   of the PTypeField pass. *)
						delay_late ctx PTypeField (fun () -> match cf_old.cf_expr with
							| Some e ->
								update_expr e
							| None ->
								begin match cf_old.cf_kind with
									| Method _ when not (has_class_flag c CInterface) && not (has_class_flag c CExtern) && not (has_class_field_flag cf_old CfAbstract) ->
										(* TODO use sub error *)
										display_error ctx.com (Printf.sprintf "Field %s has no expression (possible typing order issue)" cf_new.cf_name) cf_new.cf_pos;
										display_error ctx.com (Printf.sprintf "While building %s" (s_type_path cg.cl_path)) p;
									| _ ->
										()
								end
						);
				end;
				if gctx.generic_debug then print_endline (Printf.sprintf "[GENERIC] %s" (Printer.s_tclass_field "  " cf_new));
				t
			in
			let t = spawn_monomorph ctx p in
			let r = make_lazy ctx t (fun r ->
				let t0 = f() in
				unify_raise t0 t p;
				link_dynamic t0 t;
				t
			) "build_generic_class" in
			cf_new.cf_type <- TLazy r;
			cf_new
		in
		if c.cl_init <> None then raise_typing_error "This class can't be generic" p;
		List.iter (fun cf -> match cf.cf_kind with
			| Method MethMacro when not ctx.com.is_macro_context -> ()
			| _ -> raise_typing_error "A generic class can't have static fields" cf.cf_pos
		) c.cl_ordered_statics;
		cg.cl_super <- (match c.cl_super with
			| None -> None
			| Some (cs,pl) ->
				let ts = build_instances ctx (follow (apply_params c.cl_params tl (TInst(cs,pl)))) p in
				let cs,tl = TypeloadCheck.Inheritance.check_extends ctx c ts p in
				Some(cs,tl)
		);
		TypeloadFunction.add_constructor ctx cg false p;
		cg.cl_kind <- KGenericInstance (c,tl);
		if (has_class_flag c CInterface) then add_class_flag cg CInterface;
		if (has_class_flag c CAbstract) then add_class_flag cg CAbstract;
		cg.cl_constructor <- (match cg.cl_constructor, c.cl_constructor, c.cl_super with
			| _, Some cf, _ -> Some (build_field cf)
			| Some ctor, _, _ -> Some ctor
			| None, None, None -> None
			| _ -> raise_typing_error "Please define a constructor for this class in order to use it as generic" c.cl_pos
		);
		cg.cl_implements <- List.map (fun (i,tl) ->
			(match follow (generic_substitute_type gctx (TInst (i, List.map (generic_substitute_type gctx) tl))) with
			| TInst (i,tl) -> i, tl
			| _ -> die "" __LOC__)
		) c.cl_implements;
		cg.cl_ordered_fields <- List.map (fun f ->
			let f = build_field f in
			cg.cl_fields <- PMap.add f.cf_name f cg.cl_fields;
			f
		) c.cl_ordered_fields;
		(* In rare cases the class name can become too long, so let's shorten it (issue #3090). *)
		if String.length (snd cg.cl_path) > 254 then begin
			let n = get_short_name () in
			cg.cl_meta <- (Meta.Native,[EConst(String (n,SDoubleQuotes)),p],null_pos) :: cg.cl_meta;
		end;
		cg.cl_using <- c.cl_using;
		if gctx.generic_debug then print_endline (Printf.sprintf "[GENERIC] %s" (Printer.s_tclass "  " cg));
		TInst (cg,[])
	end

let type_generic_function ctx fa fcc with_type p =
	let c,stat = match fa.fa_host with
		| FHInstance(c,tl) -> c,false
		| FHStatic c -> c,true
		| FHAbstract(a,tl,c) -> c,true
		| _ -> die "" __LOC__
	in
	let cf = fcc.fc_field in
	if cf.cf_params = [] then raise_typing_error "Function has no type parameters and cannot be generic" p;
	begin match with_type with
		| WithType.WithType(t,_) ->
			(* In cases like #5482, we might have a return type that still needs expansion. *)
			unify ctx (build_instances ctx fcc.fc_ret p) t p
		| _ ->
			()
	end;
	let monos = fcc.fc_monos in
	List.iter (fun t -> match follow t with
		| TMono m -> safe_mono_close ctx m p
		| _ -> ()
	) monos;
	let el = fcc.fc_args in
	let gctx = make_generic ctx cf.cf_params monos (Meta.has (Meta.Custom ":debug.generic") cf.cf_meta) p in
	let fc_type = build_instances ctx fcc.fc_type p in
	let name = cf.cf_name ^ "_" ^ gctx.name in
	let unify_existing_field tcf pcf = try
		unify_raise tcf fc_type p
	with Error ({ err_message = Unify _; err_depth = depth } as err) ->
		raise (Error { err with err_sub = (make_error
			~depth
			~sub:[make_error ~depth:(depth+1) (Custom (compl_msg "Conflicting field was defined here")) pcf]
			(Custom ("Cannot create field " ^ name ^ " due to type mismatch"))
			p
		) :: err.err_sub })
	in
	let fa = try
		let cf2 = if stat then
			let cf2 = PMap.find name c.cl_statics in
			unify_existing_field cf2.cf_type cf2.cf_pos;
			cf2
		else
			let cf2 = PMap.find name c.cl_fields in
			unify_existing_field cf2.cf_type cf2.cf_pos;
			cf2
		in
		{fa with fa_field = cf2}
		(*
			java.Lib.array() relies on the ability to shadow @:generic function for certain types
			see https://github.com/HaxeFoundation/haxe/issues/8393#issuecomment-508685760
		*)
		(* if cf.cf_name_pos = cf2.cf_name_pos then
			cf2
		else
			error ("Cannot specialize @:generic because the generated function name is already used: " ^ name) p *)
	with Not_found ->
		let finalize_field c cf2 =
			ignore(follow cf.cf_type);
			let rec check e = match e.eexpr with
				| TNew({cl_kind = KTypeParameter _} as c,_,_) when not (TypeloadCheck.is_generic_parameter ctx c) ->
					(* TODO use sub error *)
					display_error ctx.com "Only generic type parameters can be constructed" e.epos;
					display_error ctx.com "While specializing this call" p;
				| _ ->
					Type.iter check e
			in
			cf2.cf_expr <- (match cf.cf_expr with
				| None ->
					display_error ctx.com "Recursive @:generic function" p; None;
				| Some e ->
					let e = generic_substitute_expr gctx e in
					check e;
					Some e
			);
			cf2.cf_kind <- cf.cf_kind;
			if not (has_class_field_flag cf CfPublic) then remove_class_field_flag cf2 CfPublic;
			cf2.cf_meta <- (Meta.NoCompletion,[],p) :: (Meta.NoUsing,[],p) :: (Meta.GenericInstance,[],p) :: cf.cf_meta
		in
		let mk_cf2 name =
			mk_field ~static:stat name fc_type cf.cf_pos cf.cf_name_pos
		in
		if stat then begin
			if Meta.has Meta.GenericClassPerMethod c.cl_meta then begin
				let c = static_method_container gctx c cf p in
				set_type_parameter_dependencies c.cl_module monos;
				let cf2 = try
					let cf2 = PMap.find cf.cf_name c.cl_statics in
					unify_existing_field cf2.cf_type cf2.cf_pos;
					cf2
				with Not_found ->
					let cf2 = mk_cf2 cf.cf_name in
					c.cl_statics <- PMap.add cf2.cf_name cf2 c.cl_statics;
					c.cl_ordered_statics <- cf2 :: c.cl_ordered_statics;
					finalize_field c cf2;
					cf2
				in
				{fa with fa_host = FHStatic c;fa_field = cf2;fa_on = Builder.make_static_this c p}
			end else begin
				set_type_parameter_dependencies c.cl_module monos;
				let cf2 = mk_cf2 name in
				c.cl_statics <- PMap.add cf2.cf_name cf2 c.cl_statics;
				c.cl_ordered_statics <- cf2 :: c.cl_ordered_statics;
				finalize_field c cf2;
				{fa with fa_field = cf2}
			end
		end else begin
			set_type_parameter_dependencies c.cl_module monos;
			let cf2 = mk_cf2 name in
			if has_class_field_flag cf CfOverride then add_class_field_flag cf2 CfOverride;
			c.cl_fields <- PMap.add cf2.cf_name cf2 c.cl_fields;
			c.cl_ordered_fields <- cf2 :: c.cl_ordered_fields;
			finalize_field c cf2;
			{fa with fa_field = cf2}
		end
	in
	let dispatch = new CallUnification.call_dispatcher ctx (MCall []) with_type p in
	dispatch#field_call fa el []

;;
Typecore.type_generic_function_ref := type_generic_function
