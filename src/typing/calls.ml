open Globals
open DisplayTypes.DisplayMode
open Common
open Ast
open Type
open Typecore
open TyperBase
open Fields
open Error
open CallUnification

let make_call ctx e params t ?(force_inline=false) p =
	try
		let ethis,cl,f = match e.eexpr with
			| TField (ethis,fa) ->
				let co,cf = match fa with
					| FInstance(c,_,cf) | FStatic(c,cf) -> Some c,cf
					| FAnon cf -> None,cf
					| _ -> raise Exit
				in
				ethis,co,cf
			| _ ->
				raise Exit
		in
		if not force_inline then begin
			let is_extern_class = match cl with Some c -> (has_class_flag c CExtern) | _ -> false in
			if not (Inline.needs_inline ctx is_extern_class f) then raise Exit;
		end else begin
			match cl with
			| None ->
				()
			| Some c ->
				(* Delay this to filters because that's when cl_descendants is set. *)
				ctx.com.callbacks#add_before_save (fun () ->
					let rec has_override c =
						PMap.mem f.cf_name c.cl_fields
						|| List.exists has_override c.cl_descendants
					in
					if List.exists has_override c.cl_descendants then error (Printf.sprintf "Cannot force inline-call to %s because it is overridden" f.cf_name) p
				)
		end;
		let config = Inline.inline_config cl f params t in
		ignore(follow f.cf_type); (* force evaluation *)
		(match cl, ctx.curclass.cl_kind, params with
			| Some c, KAbstractImpl _, { eexpr = TLocal { v_meta = v_meta } } :: _ when c == ctx.curclass ->
				if
					f.cf_name <> "_new"
					&& has_meta Meta.This v_meta
					&& has_class_field_flag f CfModifiesThis
				then
					if assign_to_this_is_allowed ctx then
						(* Current method needs to infer CfModifiesThis flag, since we are calling a method, which modifies `this` *)
						add_class_field_flag ctx.curfield CfModifiesThis
					else
						error ("Abstract 'this' value can only be modified inside an inline function. '" ^ f.cf_name ^ "' modifies 'this'") p;
			| _ -> ()
		);
		let params = List.map (ctx.g.do_optimize ctx) params in
		let force_inline = is_forced_inline cl f in
		(match f.cf_expr_unoptimized,f.cf_expr with
		| Some fd,_
		| None,Some { eexpr = TFunction fd } ->
			(match Inline.type_inline ctx f fd ethis params t config p force_inline with
			| None ->
				if force_inline then error "Inline could not be done" p;
				raise Exit;
			| Some e -> e)
		| _ ->
			(*
				we can't inline because there is most likely a loop in the typing.
				this can be caused by mutually recursive vars/functions, some of them
				being inlined or not. In that case simply ignore inlining.
			*)
			raise Exit)
	with Exit ->
		mk (TCall (e,params)) t p

let mk_array_get_call ctx (cf,tf,r,e1,e2o) c ebase p = match cf.cf_expr with
	| None ->
		if not (Meta.has Meta.NoExpr cf.cf_meta) then display_error ctx "Recursive array get method" p;
		mk (TArray(ebase,e1)) r p
	| Some _ ->
		let et = type_module_type ctx (TClassDecl c) None p in
		let ef = mk (TField(et,(FStatic(c,cf)))) tf p in
		make_call ctx ef [ebase;e1] r p

let mk_array_set_call ctx (cf,tf,r,e1,e2o) c ebase p =
	let evalue = match e2o with None -> die "" __LOC__ | Some e -> e in
	match cf.cf_expr with
		| None ->
			if not (Meta.has Meta.NoExpr cf.cf_meta) then display_error ctx "Recursive array set method" p;
			let ea = mk (TArray(ebase,e1)) r p in
			mk (TBinop(OpAssign,ea,evalue)) r p
		| Some _ ->
			let et = type_module_type ctx (TClassDecl c) None p in
			let ef = mk (TField(et,(FStatic(c,cf)))) tf p in
			make_call ctx ef [ebase;e1;evalue] r p

let abstract_using_param_type sea = match follow sea.se_this.etype with
	| TAbstract(a,tl) when has_class_field_flag sea.se_access.fa_field CfImpl -> apply_params a.a_params tl a.a_this
	| _ -> sea.se_this.etype

let rec acc_get ctx g p =
	let inline_read fa =
		let cf = fa.fa_field in
		(* do not create a closure for static calls *)
		let apply_params = match fa.fa_host with
			| FHStatic c ->
				(fun t -> t)
			| FHInstance(c,tl) ->
				(fun t -> t)
			| FHAbstract(a,tl,c) ->
				if a.a_enum then begin
					(* Enum abstracts have to apply their type parameters because they are basically statics with type params (#8700). *)
					let monos = Monomorph.spawn_constrained_monos (fun t -> t) a.a_params in
					apply_params a.a_params monos;
				end else
					(fun t -> t)
			| _ ->
				die "" __LOC__
		in
		ignore(follow cf.cf_type); (* force computing *)
		begin match cf.cf_kind,cf.cf_expr with
		| _ when not (ctx.com.display.dms_inline) ->
			FieldAccess.get_field_expr fa FRead
		| Method _,_->
			let chk_class c = ((has_class_flag c CExtern) || has_class_field_flag cf CfExtern) && not (Meta.has Meta.Runtime cf.cf_meta) in
			let wrap_extern c =
				let c2 =
					let m = c.cl_module in
					let mpath = (fst m.m_path @ ["_" ^ snd m.m_path],(snd m.m_path) ^ "_Impl_") in
					try
						let rec loop mtl = match mtl with
							| (TClassDecl c) :: _ when c.cl_path = mpath -> c
							| _ :: mtl -> loop mtl
							| [] -> raise Not_found
						in
						loop c.cl_module.m_types
					with Not_found ->
						let c2 = mk_class c.cl_module mpath c.cl_pos null_pos in
						c.cl_module.m_types <- (TClassDecl c2) :: c.cl_module.m_types;
						c2
				in
				let cf = try
					PMap.find cf.cf_name c2.cl_statics
				with Not_found ->
					let cf = {cf with cf_kind = Method MethNormal} in
					c2.cl_statics <- PMap.add cf.cf_name cf c2.cl_statics;
					c2.cl_ordered_statics <- cf :: c2.cl_ordered_statics;
					cf
				in
				let e_t = type_module_type ctx (TClassDecl c2) None p in
				FieldAccess.get_field_expr (FieldAccess.create e_t cf (FHStatic c2) true p) FRead
			in
			let e_def = FieldAccess.get_field_expr fa FRead in
			begin match follow fa.fa_on.etype with
				| TInst (c,_) when chk_class c ->
					display_error ctx "Can't create closure on an extern inline member method" p;
					e_def
				| TAnon a ->
					begin match !(a.a_status) with
						| Statics c when has_class_field_flag cf CfExtern ->
							display_error ctx "Cannot create closure on @:extern inline method" p;
							e_def
						| Statics c when chk_class c -> wrap_extern c
						| _ -> e_def
					end
				| _ -> e_def
			end
		| Var _,Some e ->
			let rec loop e = Type.map_expr loop { e with epos = p; etype = apply_params e.etype } in
			let e = loop e in
			let e = Inline.inline_metadata e cf.cf_meta in
			let tf = apply_params cf.cf_type in
			if not (type_iseq tf e.etype) then mk (TCast(e,None)) tf e.epos
			else e
		| Var _,None when ctx.com.display.dms_display ->
			 FieldAccess.get_field_expr fa FRead
		| Var _,None ->
			error "Recursive inline is not supported" p
		end
	in
	let dispatcher () = new call_dispatcher ctx MGet WithType.value p in
	match g with
	| AKNo f -> error ("Field " ^ f ^ " cannot be accessed for reading") p
	| AKExpr e -> e
	| AKAccess _ -> die "" __LOC__
	| AKResolve(sea,name) ->
		(dispatcher ())#resolve_call sea name
	| AKUsingAccessor sea | AKUsingField sea when ctx.in_display ->
		(* Generate a TField node so we can easily match it for position/usage completion (issue #1968) *)
		let e_field = FieldAccess.get_field_expr sea.se_access FGet in
		(* TODO *)
		(* let ec = {ec with eexpr = (TMeta((Meta.StaticExtension,[],null_pos),ec))} in *)
		let t = match follow e_field.etype with
			| TFun (_ :: args,ret) -> TFun(args,ret)
			| t -> t
		in
		{e_field with etype = t}
	| AKField fa ->
		begin match fa.fa_field.cf_kind with
		| Method MethMacro ->
			(* If we are in display mode, we're probably hovering a macro call subject. Just generate a normal field. *)
			if ctx.in_display then
				FieldAccess.get_field_expr fa FRead
			else
				error "Invalid macro access" p
		| _ ->
			if fa.fa_inline then
				inline_read fa
			else
				FieldAccess.get_field_expr fa FRead
		end
	| AKAccessor fa ->
		(dispatcher())#field_call fa [] []
	| AKUsingAccessor sea ->
		(dispatcher())#field_call sea.se_access [sea.se_this] []
	| AKUsingField sea ->
		let e = sea.se_this in
		let e_field = FieldAccess.get_field_expr sea.se_access FGet in
		(* build a closure with first parameter applied *)
		(match follow e_field.etype with
		| TFun ((_,_,t0) :: args,ret) ->
			let te = abstract_using_param_type sea in
			unify ctx te t0 e.epos;
			let tcallb = TFun (args,ret) in
			let twrap = TFun ([("_e",false,e.etype)],tcallb) in
			(* arguments might not have names in case of variable fields of function types, so we generate one (issue #2495) *)
			let args = List.map (fun (n,o,t) ->
				let t = if o then ctx.t.tnull t else t in
				o,if n = "" then gen_local ctx t e.epos else alloc_var VGenerated n t e.epos (* TODO: var pos *)
			) args in
			let ve = alloc_var VGenerated "_e" e.etype e.epos in
			let ecall = make_call ctx e_field (List.map (fun v -> mk (TLocal v) v.v_type p) (ve :: List.map snd args)) ret p in
			let ecallb = mk (TFunction {
				tf_args = List.map (fun (o,v) -> v,if o then Some (Texpr.Builder.make_null v.v_type v.v_pos) else None) args;
				tf_type = ret;
				tf_expr = (match follow ret with | TAbstract ({a_path = [],"Void"},_) -> ecall | _ -> mk (TReturn (Some ecall)) t_dynamic p);
			}) tcallb p in
			let ewrap = mk (TFunction {
				tf_args = [ve,None];
				tf_type = tcallb;
				tf_expr = mk (TReturn (Some ecallb)) t_dynamic p;
			}) twrap p in
			make_call ctx ewrap [e] tcallb p
		| _ -> die "" __LOC__)

let build_call ?(mode=MGet) ctx acc el (with_type:WithType.t) p =
	let dispatch = new call_dispatcher ctx mode with_type p in
	match acc with
	| AKField fa ->
		dispatch#field_call fa [] el
	| AKUsingField sea ->
		let eparam = sea.se_this in
		dispatch#field_call sea.se_access [eparam] el
	| AKResolve(sea,name) ->
		dispatch#expr_call (dispatch#resolve_call sea name) el
	| AKNo _ | AKAccess _ ->
		ignore(acc_get ctx acc p);
		error ("Unexpected access mode, please report this: " ^ (s_access_kind acc)) p
	| AKAccessor fa ->
		let e = dispatch#field_call fa [] [] in
		dispatch#expr_call e el
	| AKUsingAccessor sea ->
		let e = dispatch#field_call sea.se_access [sea.se_this] [] in
		dispatch#expr_call e el
	| AKExpr e ->
		dispatch#expr_call e el

let rec needs_temp_var e =
	match e.eexpr with
	| TLocal _ | TTypeExpr _ | TConst _ -> false
	| TField (e, _) | TParenthesis e -> needs_temp_var e
	| _ -> true

let call_to_string ctx ?(resume=false) e =
	let gen_to_string e =
		(* Ignore visibility of the toString field. *)
		ctx.meta <- (Meta.PrivateAccess,[],e.epos) :: ctx.meta;
		let acc = type_field (TypeFieldConfig.create resume) ctx e "toString" e.epos (MCall []) (WithType.with_type ctx.t.tstring) in
		ctx.meta <- List.tl ctx.meta;
		build_call ctx acc [] (WithType.with_type ctx.t.tstring) e.epos
	in
	if ctx.com.config.pf_static && not (is_nullable e.etype) then
		gen_to_string e
	else begin (* generate `if(e == null) 'null' else e.toString()` *)
		let string_null = mk (TConst (TString "null")) ctx.t.tstring e.epos in
		if needs_temp_var e then
			let tmp = alloc_var VGenerated "tmp" e.etype e.epos in
			let tmp_local = mk (TLocal tmp) tmp.v_type tmp.v_pos in
			let check_null = mk (TBinop (OpEq, tmp_local, mk (TConst TNull) tmp.v_type tmp.v_pos)) ctx.t.tbool e.epos in
			{
				eexpr = TBlock([
					mk (TVar (tmp, Some e)) tmp.v_type tmp.v_pos;
					mk (TIf (check_null, string_null, Some (gen_to_string tmp_local))) ctx.t.tstring tmp.v_pos;

				]);
				etype = ctx.t.tstring;
				epos = e.epos;
			}
		else
			let check_null = mk (TBinop (OpEq, e, mk (TConst TNull) e.etype e.epos)) ctx.t.tbool e.epos in
			mk (TIf (check_null, string_null, Some (gen_to_string e))) ctx.t.tstring e.epos
	end

let type_bind ctx (e : texpr) (args,ret) params p =
	let vexpr v = mk (TLocal v) v.v_type p in
	let acount = ref 0 in
	let alloc_name n =
		if n = "" && not ctx.is_display_file then begin
			incr acount;
			"a" ^ string_of_int !acount;
		end else
			n
	in
	let rec loop args params given_args missing_args ordered_args = match args, params with
		| [], [] -> given_args,missing_args,ordered_args
		| [], _ -> error "Too many callback arguments" p
		| (n,o,t) :: args , [] when o ->
			let a = if is_pos_infos t then
					let infos = mk_infos ctx p [] in
					ordered_args @ [type_expr ctx infos (WithType.with_argument t n)]
				else if ctx.com.config.pf_pad_nulls then
					(ordered_args @ [(mk (TConst TNull) t_dynamic p)])
				else
					ordered_args
			in
			loop args [] given_args missing_args a
		| (n,o,t) :: _ , (EConst(Ident "_"),p) :: _ when not ctx.com.config.pf_can_skip_non_nullable_argument && o && not (is_nullable t) ->
			error "Usage of _ is not supported for optional non-nullable arguments" p
		| (n,o,t) :: args , ([] as params)
		| (n,o,t) :: args , (EConst(Ident "_"),_) :: params ->
			let v = alloc_var VGenerated (alloc_name n) (if o then ctx.t.tnull t else t) p in
			loop args params given_args (missing_args @ [v,o]) (ordered_args @ [vexpr v])
		| (n,o,t) :: args , param :: params ->
			let e = type_expr ctx param (WithType.with_argument t n) in
			let e = AbstractCast.cast_or_unify ctx t e (pos param) in
			let v = alloc_var VGenerated (alloc_name n) t (pos param) in
			loop args params (given_args @ [v,o,Some e]) missing_args (ordered_args @ [vexpr v])
	in
	let given_args,missing_args,ordered_args = loop args params [] [] [] in
	let var_decls = List.map (fun (v,_,e_opt) -> mk (TVar(v,e_opt)) ctx.t.tvoid v.v_pos) given_args in
	let e,var_decls =
		let is_immutable_method cf =
			match cf.cf_kind with Method k -> k <> MethDynamic | _ -> false
		in
		match e.eexpr with
		| TFunction _ | TLocal { v_kind = VUser TVOLocalFunction } ->
			e,var_decls
		| TField(_,(FStatic(_,cf) | FInstance(_,_,cf))) when is_immutable_method cf ->
			e,var_decls
		| _ ->
			let e_var = alloc_var VGenerated "`" e.etype e.epos in
			(mk (TLocal e_var) e.etype e.epos), (mk (TVar(e_var,Some e)) ctx.t.tvoid e.epos) :: var_decls
	in
	let call = make_call ctx e ordered_args ret p in
	let body =
		if ExtType.is_void (follow ret) then call
		else mk (TReturn(Some call)) ret p
	in
	let arg_default optional t =
		if optional then Some (Texpr.Builder.make_null t null_pos)
		else None
	in
	let fn = {
		tf_args = List.map (fun (v,o) -> v,arg_default o v.v_type) missing_args;
		tf_type = ret;
		tf_expr = body;
	} in
	let t = TFun(List.map (fun (v,o) -> v.v_name,o,v.v_type) missing_args,ret) in
	{
		eexpr = TBlock (var_decls @ [mk (TFunction fn) t p]);
		etype = t;
		epos = p;
	}

let array_access ctx e1 e2 mode p =
	let has_abstract_array_access = ref false in
	try
		(match follow e1.etype with
		| TAbstract ({a_impl = Some c} as a,pl) when a.a_array <> [] ->
			begin match mode with
			| MSet _ ->
				(* resolve later *)
				AKAccess (a,pl,c,e1,e2)
			| _ ->
				has_abstract_array_access := true;
				let e = mk_array_get_call ctx (AbstractCast.find_array_access ctx a pl e2 None p) c e1 p in
				AKExpr e
			end
		| _ -> raise Not_found)
	with Not_found ->
		let base_ok = ref true in
		let rec loop ?(skip_abstract=false) et =
			match skip_abstract,follow et with
			| _, TInst ({ cl_array_access = Some t; cl_params = pl },tl) ->
				apply_params pl tl t
			| _, TInst ({ cl_super = Some (c,stl); cl_params = pl },tl) ->
				apply_params pl tl (loop (TInst (c,stl)))
			| _, TInst ({ cl_path = [],"ArrayAccess" },[t]) ->
				t
			| _, TInst ({ cl_path = [],"Array"},[t]) when t == t_dynamic ->
				t_dynamic
			| false, TAbstract(a,tl) when Meta.has Meta.ArrayAccess a.a_meta ->
				let at = apply_params a.a_params tl a.a_this in
				let skip_abstract = fast_eq et at in
				loop ~skip_abstract at
			| _, _ ->
				let pt = spawn_monomorph ctx p in
				let t = ctx.t.tarray pt in
				begin try
					unify_raise ctx et t p
				with Error(Unify _,_) ->
					if not ctx.untyped then begin
						let msg = if !has_abstract_array_access then
							"No @:arrayAccess function accepts an argument of " ^ (s_type (print_context()) e2.etype)
						else
							"Array access is not allowed on " ^ (s_type (print_context()) e1.etype)
						in
						base_ok := false;
						raise_or_display_message ctx msg e1.epos;
					end
				end;
				pt
		in
		let pt = loop e1.etype in
		if !base_ok then unify ctx e2.etype ctx.t.tint e2.epos;
		AKExpr (mk (TArray (e1,e2)) pt p)

(*
	given chain of fields as the `path` argument and an `access_mode->access_kind` getter for some starting expression as `e`,
	return a new `access_mode->access_kind` getter for the whole field access chain.
*)
let field_chain ctx path access mode with_type =
	let rec loop access path = match path with
		| [] ->
			access
		| [(name,_,p)] ->
			let e = acc_get ctx access p in
			type_field_default_cfg ctx e name p mode with_type
		| (name,_,p) :: path ->
			let e = acc_get ctx access p in
			let access = type_field_default_cfg ctx e name p MGet WithType.value in
			loop access path
	in
	loop access path