open Globals
open DisplayTypes.DisplayMode
open Common
open Ast
open Type
open Typecore
open TyperBase
open Fields
open Error

let is_forced_inline c cf =
	match c with
	| Some { cl_kind = KAbstractImpl _ } -> true
	| Some c when has_class_flag c CExtern -> true
	| _ when has_class_field_flag cf CfExtern -> true
	| _ -> false

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
		!build_call_ref ctx acc [] (WithType.with_type ctx.t.tstring) e.epos
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

let rec unify_call_args' ctx el args r callp inline force_inline =
	let in_call_args = ctx.in_call_args in
	ctx.in_call_args <- true;
	let call_error err p =
		raise (Error (Call_error err,p))
	in
	let arg_error ul name opt p =
		let err = Stack (ul,Custom ("For " ^ (if opt then "optional " else "") ^ "function argument '" ^ name ^ "'")) in
		call_error (Could_not_unify err) p
	in
	let mk_pos_infos t =
		let infos = mk_infos ctx callp [] in
		type_expr ctx infos (WithType.with_type t)
	in
	let rec default_value name t =
		if is_pos_infos t then
			mk_pos_infos t
		else
			null (ctx.t.tnull t) callp
	in
	let skipped = ref [] in
	let invalid_skips = ref [] in
	let skip name ul t p =
		if not ctx.com.config.pf_can_skip_non_nullable_argument && not (is_nullable t) then
			invalid_skips := name :: !invalid_skips;
		skipped := (name,ul,p) :: !skipped;
		default_value name t
	in
	(* let force_inline, is_extern = match cf with Some(TInst(c,_),f) -> is_forced_inline (Some c) f, (has_class_flag c CExtern) | _ -> false, false in *)
	let type_against name t e =
		try
			let e = type_expr ctx e (WithType.with_argument t name) in
			AbstractCast.cast_or_unify_raise ctx t e e.epos
		with Error(l,p) when (match l with Call_error _ | Module_not_found _ -> false | _ -> true) ->
			raise (WithTypeError (l,p))
	in
	let rec loop el args = match el,args with
		| [],[] ->
			begin match List.rev !invalid_skips with
				| [] -> ()
				| name :: _ -> call_error (Cannot_skip_non_nullable name) callp;
			end;
			[]
		| _,[name,false,t] when (match follow t with TAbstract({a_path = ["haxe";"extern"],"Rest"},_) -> true | _ -> false) ->
			begin match follow t with
				| TAbstract({a_path=(["haxe";"extern"],"Rest")},[t]) ->
					(try List.map (fun e -> type_against name t e,false) el with WithTypeError(ul,p) -> arg_error ul name false p)
				| _ ->
					die "" __LOC__
			end
		| [],(_,false,_) :: _ ->
			call_error (Not_enough_arguments args) callp
		| [],(name,true,t) :: args ->
			begin match loop [] args with
				| [] when not (inline && (ctx.g.doinline || force_inline)) && not ctx.com.config.pf_pad_nulls ->
					if is_pos_infos t then [mk_pos_infos t,true]
					else []
				| args ->
					let e_def = default_value name t in
					(e_def,true) :: args
			end
		| (e,p) :: el, [] ->
			begin match List.rev !skipped with
				| [] ->
					if ctx.is_display_file && not (Diagnostics.is_diagnostics_run ctx.com p) then begin
						ignore(type_expr ctx (e,p) WithType.value);
						loop el []
					end	else call_error Too_many_arguments p
				| (s,ul,p) :: _ -> arg_error ul s true p
			end
		| e :: el,(name,opt,t) :: args ->
			begin try
				let e = type_against name t e in
				(e,opt) :: loop el args
			with
				WithTypeError (ul,p)->
					if opt && List.length el < List.length args then
						let e_def = skip name ul t p in
						(e_def,true) :: loop (e :: el) args
					else
						match List.rev !skipped with
						| [] -> arg_error ul name opt p
						| (s,ul,p) :: _ -> arg_error ul s true p
			end
	in
	let el = try loop el args with exc -> ctx.in_call_args <- in_call_args; raise exc; in
	ctx.in_call_args <- in_call_args;
	el,TFun(args,r)

let unify_call_args ctx el args r p inline force_inline =
	let el,tf = unify_call_args' ctx el args r p inline force_inline in
	List.map fst el,tf

let unify_field_call ctx fa el args ret p inline =
	let map_cf cf0 map cf =
		let monos = Monomorph.spawn_constrained_monos map cf.cf_params in
		let t = map (apply_params cf.cf_params monos cf.cf_type) in
		t,cf
	in
	let expand_overloads map cf =
		(TFun(args,ret),cf) :: (List.map (map_cf cf map) cf.cf_overloads)
	in
	let candidates,co,static,cf,mk_fa = match fa with
		| FStatic(c,cf) ->
			expand_overloads (fun t -> t) cf,Some c,true,cf,(fun cf -> FStatic(c,cf))
		| FAnon cf ->
			expand_overloads (fun t -> t) cf,None,false,cf,(fun cf -> FAnon cf)
		| FInstance(c,tl,cf) ->
			let map = apply_params c.cl_params tl in
			let cfl = if cf.cf_name = "new" || not (Meta.has Meta.Overload cf.cf_meta && ctx.com.config.pf_overload) then
				(TFun(args,ret),cf) :: List.map (map_cf cf map) cf.cf_overloads
			else
				List.map (fun (t,cf) ->
					let monos = Monomorph.spawn_constrained_monos map cf.cf_params in
					map (apply_params cf.cf_params monos t),cf
				) (Overloads.get_overloads ctx.com c cf.cf_name)
			in
			cfl,Some c,false,cf,(fun cf -> FInstance(c,tl,cf))
		| FClosure(co,cf) ->
			let c = match co with None -> None | Some (c,_) -> Some c in
			expand_overloads (fun t -> t) cf,c,false,cf,(fun cf -> match co with None -> FAnon cf | Some (c,tl) -> FInstance(c,tl,cf))
		| _ ->
			error "Invalid field call" p
	in
	let is_forced_inline = is_forced_inline co cf in
	let is_overload = Meta.has Meta.Overload cf.cf_meta in
	let attempt_call t cf = match follow t with
		| TFun(args,ret) ->
			let el,tf = unify_call_args' ctx el args ret p inline is_forced_inline in
			let mk_call ethis p_field inline =
				let ef = mk (TField(ethis,mk_fa cf)) t p_field in
				make_call ctx ef (List.map fst el) ret ~force_inline:inline p
			in
			make_field_call_candidate el tf cf mk_call
		| _ ->
			die "" __LOC__
	in
	let maybe_raise_unknown_ident cerr p =
		let rec loop err =
			match err with
			| Unknown_ident _ -> error (error_msg err) p
			| Stack (e1,e2) -> (loop e1; loop e2)
			| _ -> ()
		in
		match cerr with Could_not_unify err -> loop err | _ -> ()
	in
	let attempt_calls candidates =
		let rec loop candidates = match candidates with
			| [] -> [],[]
			| (t,cf) :: candidates ->
				let known_monos = List.map (fun (m,_) ->
					m,m.tm_type,m.tm_constraints
				) ctx.monomorphs.perfunction in
				begin try
					let candidate = attempt_call t cf in
					if ctx.com.config.pf_overload && is_overload then begin
						let candidates,failures = loop candidates in
						candidate :: candidates,failures
					end else
						[candidate],[]
				with Error ((Call_error cerr as err),p) ->
					List.iter (fun (m,t,constr) ->
						m.tm_type <- t;
						m.tm_constraints <- constr;
					) known_monos;
					maybe_raise_unknown_ident cerr p;
					let candidates,failures = loop candidates in
					candidates,(cf,err,p) :: failures
				end
		in
		loop candidates
	in
	let fail_fun () =
		let tf = TFun(args,ret) in
		let call = (fun ethis p_field _ ->
			let e1 = mk (TField(ethis,mk_fa cf)) tf p_field in
			mk (TCall(e1,[])) ret p)
		in
		make_field_call_candidate [] tf cf call
	in
	let maybe_check_access cf =
		(* type_field doesn't check access for overloads, so let's check it here *)
		begin match co with
		| Some c ->
			check_field_access ctx c cf static p;
		| None ->
			()
		end;
	in
	match candidates with
	| [t,cf] ->
		if is_overload then maybe_check_access cf;
		begin try
			attempt_call t cf
		with Error _ when ctx.com.display.dms_error_policy = EPIgnore ->
			fail_fun();
		end
	| _ ->
		let candidates,failures = attempt_calls candidates in
		let fail () =
			let failures = List.map (fun (cf,err,p) -> cf,error_msg err,p) failures in
			let failures = remove_duplicates (fun (_,msg1,_) (_,msg2,_) -> msg1 <> msg2) failures in
			begin match failures with
			| [_,msg,p] ->
				error msg p
			| _ ->
				display_error ctx "Could not find a suitable overload, reasons follow" p;
				List.iter (fun (cf,msg,p2) ->
					display_error ctx ("Overload resolution failed for " ^ (s_type (print_context()) cf.cf_type)) p;
					display_error ctx msg p2;
				) failures;
				error "End of overload failure reasons" p
			end
		in
		if is_overload && ctx.com.config.pf_overload then begin match Overloads.Resolution.reduce_compatible candidates with
			| [] -> fail()
			| [fcc] ->
				maybe_check_access fcc.fc_field;
				fcc
			| _ -> error "Ambiguous overload" p
		end else begin match List.rev candidates with
			| [] -> fail()
			| fcc :: _ -> fcc
		end

let type_generic_function ctx (e,fa) el ?(using_param=None) with_type p =
	let c,tl,cf,stat = match fa with
		| FInstance(c,tl,cf) -> c,tl,cf,false
		| FStatic(c,cf) -> c,[],cf,true
		| _ -> die "" __LOC__
	in
	if cf.cf_params = [] then error "Function has no type parameters and cannot be generic" p;
	let map = if stat then (fun t -> t) else apply_params c.cl_params tl in
	let monos = Monomorph.spawn_constrained_monos map cf.cf_params in
	let map_monos t = apply_params cf.cf_params monos t in
	let map t = if stat then map_monos t else apply_params c.cl_params tl (map_monos t) in
	let t = map cf.cf_type in
	let args,ret = match t,using_param with
		| TFun((_,_,ta) :: args,ret),Some e ->
			let ta = if not (Meta.has Meta.Impl cf.cf_meta) then ta
			else match follow ta with TAbstract(a,tl) -> Abstract.get_underlying_type a tl | _ -> die "" __LOC__
			in
			(* manually unify first argument *)
			unify ctx e.etype ta p;
			args,ret
		| TFun(args,ret),None -> args,ret
		| _ ->  error "Invalid field type for generic call" p
	in
	begin match with_type with
		| WithType.WithType(t,_) -> unify ctx ret t p
		| _ -> ()
	end;
	let el,_ = unify_call_args ctx el args ret p false false in
	List.iter (fun t -> match follow t with
		| TMono m -> safe_mono_close ctx m p
		| _ -> ()
	) monos;
	let el = match using_param with None -> el | Some e -> e :: el in
	(try
		let gctx = Generic.make_generic ctx cf.cf_params monos p in
		let name = cf.cf_name ^ "_" ^ gctx.Generic.name in
		let unify_existing_field tcf pcf = try
			unify_raise ctx tcf t p
		with Error(Unify _,_) as err ->
			display_error ctx ("Cannot create field " ^ name ^ " due to type mismatch") p;
			display_error ctx (compl_msg "Conflicting field was defined here") pcf;
			raise err
		in
		let c, cf2 = try
			let cf2 = if stat then
				let cf2 = PMap.find name c.cl_statics in
				unify_existing_field cf2.cf_type cf2.cf_pos;
				cf2
			else
				let cf2 = PMap.find name c.cl_fields in
				unify_existing_field cf2.cf_type cf2.cf_pos;
				cf2
			in
			c, cf2
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
						display_error ctx "Only generic type parameters can be constructed" e.epos;
						display_error ctx "While specializing this call" p;
					| _ ->
						Type.iter check e
				in
				cf2.cf_expr <- (match cf.cf_expr with
					| None ->
						display_error ctx "Recursive @:generic function" p; None;
					| Some e ->
						let e = Generic.generic_substitute_expr gctx e in
						check e;
						Some e
				);
				cf2.cf_kind <- cf.cf_kind;
				if not (has_class_field_flag cf CfPublic) then remove_class_field_flag cf2 CfPublic;
				let metadata = List.filter (fun (m,_,_) -> match m with
					| Meta.Generic -> false
					| _ -> true
				) cf.cf_meta in
				cf2.cf_meta <- (Meta.NoCompletion,[],p) :: (Meta.NoUsing,[],p) :: (Meta.GenericInstance,[],p) :: metadata
			in
			let mk_cf2 name =
				mk_field ~static:stat name (map_monos cf.cf_type) cf.cf_pos cf.cf_name_pos
			in
			if stat then begin
				if Meta.has Meta.GenericClassPerMethod c.cl_meta then begin
					let c = Generic.static_method_container gctx c cf p in
					try
						let cf2 = PMap.find cf.cf_name c.cl_statics in
						unify_existing_field cf2.cf_type cf2.cf_pos;
						c, cf2
					with Not_found ->
						let cf2 = mk_cf2 cf.cf_name in
						c.cl_statics <- PMap.add cf2.cf_name cf2 c.cl_statics;
						c.cl_ordered_statics <- cf2 :: c.cl_ordered_statics;
						finalize_field c cf2;
						c, cf2
				end else begin
					let cf2 = mk_cf2 name in
					c.cl_statics <- PMap.add cf2.cf_name cf2 c.cl_statics;
					c.cl_ordered_statics <- cf2 :: c.cl_ordered_statics;
					finalize_field c cf2;
					c, cf2
				end
			end else begin
				let cf2 = mk_cf2 name in
				if has_class_field_flag cf CfOverride then add_class_field_flag cf2 CfOverride;
				c.cl_fields <- PMap.add cf2.cf_name cf2 c.cl_fields;
				c.cl_ordered_fields <- cf2 :: c.cl_ordered_fields;
				finalize_field c cf2;
				c, cf2
			end
		in
		let e = match c.cl_kind with
			| KAbstractImpl(a) ->
				type_type ctx a.a_path p
			| _ when stat ->
				Builder.make_typeexpr (TClassDecl c) e.epos
			| _ -> e
		in
		let fa = if stat then FStatic (c,cf2) else FInstance (c,tl,cf2) in
		let e = mk (TField(e,fa)) cf2.cf_type p in
		make_call ctx e el ret p
	with Generic.Generic_Exception (msg,p) ->
		error msg p)

let rec acc_get ctx g p =
	match g with
	| AKNo f -> error ("Field " ^ f ^ " cannot be accessed for reading") p
	| AKExpr e -> e
	| AKSet _ | AKAccess _ | AKFieldSet _ -> die "" __LOC__
	| AKUsing (et,c,cf,e,_) when ctx.in_display ->
		(* Generate a TField node so we can easily match it for position/usage completion (issue #1968) *)
		let ec = type_module_type ctx (TClassDecl c) None p in
		let ec = {ec with eexpr = (TMeta((Meta.StaticExtension,[],null_pos),ec))} in
		let t = match follow et.etype with
			| TFun (_ :: args,ret) -> TFun(args,ret)
			| _ -> et.etype
		in
		mk (TField(ec,FStatic(c,cf))) t et.epos
	| AKUsing (et,_,cf,e,_) ->
		(* build a closure with first parameter applied *)
		(match follow et.etype with
		| TFun (_ :: args,ret) ->
			let tcallb = TFun (args,ret) in
			let twrap = TFun ([("_e",false,e.etype)],tcallb) in
			(* arguments might not have names in case of variable fields of function types, so we generate one (issue #2495) *)
			let args = List.map (fun (n,o,t) ->
				let t = if o then ctx.t.tnull t else t in
				o,if n = "" then gen_local ctx t e.epos else alloc_var VGenerated n t e.epos (* TODO: var pos *)
			) args in
			let ve = alloc_var VGenerated "_e" e.etype e.epos in
			let ecall = make_call ctx et (List.map (fun v -> mk (TLocal v) v.v_type p) (ve :: List.map snd args)) ret p in
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
	| AKInline (e,f,fmode,t) ->
		(* do not create a closure for static calls *)
		let cmode,apply_params = match fmode with
			| FStatic(c,_) ->
				let f = match c.cl_kind with
					| KAbstractImpl a when Meta.has Meta.Enum a.a_meta ->
						(* Enum abstracts have to apply their type parameters because they are basically statics with type params (#8700). *)
						let monos = Monomorph.spawn_constrained_monos (fun t -> t) a.a_params in
						apply_params a.a_params monos;
					| _ -> (fun t -> t)
				in
				fmode,f
			| FInstance (c,tl,f) ->
				(FClosure (Some (c,tl),f),(fun t -> t))
			| _ ->
				die "" __LOC__
		in
		ignore(follow f.cf_type); (* force computing *)
		begin match f.cf_kind,f.cf_expr with
		| _ when not (ctx.com.display.dms_inline) ->
			mk (TField (e,cmode)) t p
		| Method _,_->
			let chk_class c = ((has_class_flag c CExtern) || has_class_field_flag f CfExtern) && not (Meta.has Meta.Runtime f.cf_meta) in
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
					PMap.find f.cf_name c2.cl_statics
				with Not_found ->
					let cf = {f with cf_kind = Method MethNormal} in
					c2.cl_statics <- PMap.add cf.cf_name cf c2.cl_statics;
					c2.cl_ordered_statics <- cf :: c2.cl_ordered_statics;
					cf
				in
				let e_t = type_module_type ctx (TClassDecl c2) None p in
				mk (TField(e_t,FStatic(c2,cf))) t p
			in
			let e_def = mk (TField (e,cmode)) t p in
			begin match follow e.etype with
				| TInst (c,_) when chk_class c ->
					display_error ctx "Can't create closure on an extern inline member method" p;
					e_def
				| TAnon a ->
					begin match !(a.a_status) with
						| Statics c when has_class_field_flag f CfExtern ->
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
			let e = Inline.inline_metadata e f.cf_meta in
			let tf = apply_params f.cf_type in
			if not (type_iseq tf e.etype) then mk (TCast(e,None)) tf e.epos
			else e
		| Var _,None when ctx.com.display.dms_display ->
			 mk (TField (e,cmode)) t p
		| Var _,None ->
			error "Recursive inline is not supported" p
		end
	| AKMacro(e,cf) ->
		(* If we are in display mode, we're probably hovering a macro call subject. Just generate a normal field. *)
		if ctx.in_display then begin match e.eexpr with
			| TTypeExpr (TClassDecl c) ->
				mk (TField(e,FStatic(c,cf))) cf.cf_type e.epos
			| _ ->
				error "Invalid macro access" p
		end else
			error "Invalid macro access" p

let rec build_call ?(mode=MGet) ctx acc el (with_type:WithType.t) p =
	let is_set = match mode with MSet _ -> true | _ -> false in
	let check_assign () = if is_set then invalid_assign p in
	match acc with
	| AKInline (ethis,f,fmode,t) when Meta.has Meta.Generic f.cf_meta ->
		check_assign();
		type_generic_function ctx (ethis,fmode) el with_type p
	| AKInline (ethis,f,fmode,t) ->
		check_assign();
		(match follow t with
			| TFun (args,r) ->
				let fcc = unify_field_call ctx fmode el args r p true in
				fcc.fc_data ethis p true
			| _ ->
				error (s_type (print_context()) t ^ " cannot be called") p
		)
	| AKUsing (et,cl,ef,eparam,forced_inline (* TOOD? *)) when Meta.has Meta.Generic ef.cf_meta ->
		check_assign();
		(match et.eexpr with
		| TField(ec,fa) ->
			type_generic_function ctx (ec,fa) el ~using_param:(Some eparam) with_type p
		| _ -> die "" __LOC__)
	| AKUsing (et,cl,ef,eparam,force_inline) ->
		begin match ef.cf_kind with
		| Method MethMacro ->
			let ethis = type_module_type ctx (TClassDecl cl) None p in
			let eparam,f = push_this ctx eparam in
			let e = build_call ~mode ctx (AKMacro (ethis,ef)) (eparam :: el) with_type p in
			f();
			e
		| _ ->
			check_assign();
			let t = follow (field_type ctx cl [] ef p) in
			(* for abstracts we have to apply their parameters to the static function *)
			let t,tthis = match follow eparam.etype with
				| TAbstract(a,tl) when Meta.has Meta.Impl ef.cf_meta -> apply_params a.a_params tl t,apply_params a.a_params tl a.a_this
				| te -> t,te
			in
			let params,args,r,eparam = match t with
				| TFun ((_,_,t1) :: args,r) ->
					unify ctx tthis t1 eparam.epos;
					let ef = prepare_using_field ef in
					begin match unify_call_args ctx el args r p (ef.cf_kind = Method MethInline) (is_forced_inline (Some cl) ef) with
					| el,TFun(args,r) -> el,args,r,eparam
					| _ -> die "" __LOC__
					end
				| _ -> die "" __LOC__
			in
			make_call ctx ~force_inline et (eparam :: params) r p
		end
	| AKMacro (ethis,cf) ->
		if ctx.macro_depth > 300 then error "Stack overflow" p;
		ctx.macro_depth <- ctx.macro_depth + 1;
		ctx.with_type_stack <- with_type :: ctx.with_type_stack;
		let ethis_f = ref (fun () -> ()) in
		let f = (match ethis.eexpr with
		| TTypeExpr (TClassDecl c) ->
			DeprecationCheck.check_cf ctx.com cf p;
			(match ctx.g.do_macro ctx MExpr c.cl_path cf.cf_name el p with
			| None -> (fun() -> type_expr ~mode ctx (EConst (Ident "null"),p) WithType.value)
			| Some (EMeta((Meta.MergeBlock,_,_),(EBlock el,_)),_) -> (fun () -> let e = (!type_block_ref) ctx el with_type p in mk (TMeta((Meta.MergeBlock,[],p), e)) e.etype e.epos)
			| Some e -> (fun() -> type_expr ~mode ctx e with_type))
		| _ ->
			(* member-macro call : since we will make a static call, let's find the actual class and not its subclass *)
			(match follow ethis.etype with
			| TInst (c,_) ->
				let rec loop c =
					if PMap.mem cf.cf_name c.cl_fields then
						let eparam,f = push_this ctx ethis in
						ethis_f := f;
						let e = match ctx.g.do_macro ctx MExpr c.cl_path cf.cf_name (eparam :: el) p with
							| None -> (fun() -> type_expr ~mode ctx (EConst (Ident "null"),p) WithType.value)
							| Some e -> (fun() -> type_expr ~mode ctx e WithType.value)
						in
						e
					else
						match c.cl_super with
						| None -> die "" __LOC__
						| Some (csup,_) -> loop csup
				in
				loop c
			| _ -> die "" __LOC__))
		in
		ctx.macro_depth <- ctx.macro_depth - 1;
		ctx.with_type_stack <- List.tl ctx.with_type_stack;
		let old = ctx.on_error in
		ctx.on_error <- (fun ctx msg ep ->
			(* display additional info in the case the error is not part of our original call *)
			if ep.pfile <> p.pfile || ep.pmax < p.pmin || ep.pmin > p.pmax then begin
				TypeloadFields.locate_macro_error := false;
				old ctx msg ep;
				TypeloadFields.locate_macro_error := true;
				ctx.com.error (compl_msg "Called from macro here") p;
			end else
				old ctx msg ep;
		);
		let e = try
			f()
		with exc ->
			ctx.on_error <- old;
			!ethis_f();
			raise exc
		in
		let e = Diagnostics.secure_generated_code ctx e in
		ctx.on_error <- old;
		!ethis_f();
		e
	| AKNo _ | AKSet _ | AKAccess _ | AKFieldSet _ ->
		ignore(acc_get ctx acc p);
		die "" __LOC__
	| AKExpr e ->
		let rec loop t = match follow t with
		| TFun (args,r) ->
			begin match e.eexpr with
				| TField(e1,fa) when not (match fa with FEnum _ | FDynamic _ -> true | _ -> false) ->
					begin match fa with
						| FInstance(_,_,cf) | FStatic(_,cf) when Meta.has Meta.Generic cf.cf_meta ->
							type_generic_function ctx (e1,fa) el with_type p
						| _ ->
							let fcc = unify_field_call ctx fa el args r p false in
							if has_class_field_flag fcc.fc_field CfAbstract then begin match e1.eexpr with
								| TConst TSuper -> display_error ctx (Printf.sprintf "abstract method %s cannot be accessed directly" fcc.fc_field.cf_name) p;
								| _ -> ()
							end;
							fcc.fc_data e1 e.epos false
					end
				| _ ->
					let el, tfunc = unify_call_args ctx el args r p false false in
					let r = match tfunc with TFun(_,r) -> r | _ -> die "" __LOC__ in
					mk (TCall (e,el)) r p
			end
		| TAbstract(a,tl) when Meta.has Meta.Callable a.a_meta ->
			loop (Abstract.get_underlying_type a tl)
		| TMono _ ->
			let t = mk_mono() in
			let el = List.map (fun e -> type_expr ctx e WithType.value) el in
			unify ctx (tfun (List.map (fun e -> e.etype) el) t) e.etype e.epos;
			mk (TCall (e,el)) t p
		| t ->
			let el = List.map (fun e -> type_expr ctx e WithType.value) el in
			let t = if t == t_dynamic then
				t_dynamic
			else if ctx.untyped then
				mk_mono()
			else
				error (s_type (print_context()) e.etype ^ " cannot be called") e.epos
			in
			mk (TCall (e,el)) t p
		in
		loop e.etype

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
let field_chain ctx path e =
	List.fold_left (fun e (f,_,p) ->
		let e = acc_get ctx (e MGet WithType.value (* WITHTYPETODO *)) p in
		type_field_default_cfg ctx e f p
	) e path
