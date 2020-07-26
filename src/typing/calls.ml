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

type overload_kind =
	| OverloadProper (* @:overload or overload *)
	| OverloadMeta (* @:overload(function() {}) *)
	| OverloadNone

let unify_field_call ctx fa el_typed el p inline =
	let expand_overloads cf =
		cf :: cf.cf_overloads
	in
	let candidates,co,static,map,tmap = match fa.fa_host with
		| FHStatic c ->
			expand_overloads fa.fa_field,Some c,true,(fun t -> t),(fun t -> t)
		| FHAnon ->
			expand_overloads fa.fa_field,None,false,(fun t -> t),(fun t -> t)
		| FHInstance(c,tl) ->
			let cf = fa.fa_field in
			let cfl = if cf.cf_name = "new" || not (has_class_field_flag cf CfOverload) then
				cf :: cf.cf_overloads
			else
				List.map (fun (t,cf) ->
					cf
				) (Overloads.get_overloads ctx.com c cf.cf_name)
			in
			cfl,Some c,false,TClass.get_map_function c tl,(fun t -> t)
		| FHAbstract(a,tl,c) ->
			let map = apply_params a.a_params tl in
			expand_overloads fa.fa_field,Some c,true,map,(fun t -> map a.a_this)
	in
	let is_forced_inline = is_forced_inline co fa.fa_field in
	let overload_kind = if has_class_field_flag fa.fa_field CfOverload then OverloadProper
		else if fa.fa_field.cf_overloads <> [] then OverloadMeta
		else OverloadNone
	in
	let attempt_call cf =
		let monos = Monomorph.spawn_constrained_monos map cf.cf_params in
		let t = map (apply_params cf.cf_params monos cf.cf_type) in
		match follow t with
		| TFun(args,ret) ->
			let rec loop acc_el acc_args tmap args el_typed = match args,el_typed with
				| ((_,opt,t0) as arg) :: args,e :: el_typed ->
					begin try
						unify_raise ctx (tmap e.etype) t0 e.epos;
					with Error(Unify _ as msg,p) ->
						let call_error = Call_error(Could_not_unify msg) in
						raise(Error(call_error,p))
					end;
					loop ((e,opt) :: acc_el) (arg :: acc_args) (fun t -> t) args el_typed
				| _ ->
					List.rev acc_el,List.rev acc_args,args
			in
			let el_typed,args_typed,args = loop [] [] tmap args el_typed in
			let el,_ = unify_call_args' ctx el args ret p inline is_forced_inline in
			let el = el_typed @ el in
			let tf = TFun(args_typed @ args,ret) in
			let mk_call () =
				let ef = mk (TField(fa.fa_on,FieldAccess.apply_fa cf fa.fa_host)) t fa.fa_pos in
				let el = List.map fst el in
				make_call ctx ef el ret ~force_inline:inline p
			in
			make_field_call_candidate el ret monos tf cf mk_call
		| t ->
			error (s_type (print_context()) t ^ " cannot be called") p
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
			| cf :: candidates ->
				let known_monos = List.map (fun (m,_) ->
					m,m.tm_type,m.tm_constraints
				) ctx.monomorphs.perfunction in
				let current_monos = ctx.monomorphs.perfunction in
				begin try
					let candidate = attempt_call cf in
					ctx.monomorphs.perfunction <- current_monos;
					if overload_kind = OverloadProper then begin
						let candidates,failures = loop candidates in
						candidate :: candidates,failures
					end else
						[candidate],[]
				with Error ((Call_error cerr as err),p) ->
					List.iter (fun (m,t,constr) ->
						if t != m.tm_type then m.tm_type <- t;
						if constr != m.tm_constraints then m.tm_constraints <- constr;
					) known_monos;
					ctx.monomorphs.perfunction <- current_monos;
					maybe_raise_unknown_ident cerr p;
					let candidates,failures = loop candidates in
					candidates,(cf,err,p) :: failures
				end
		in
		loop candidates
	in
	let fail_fun () =
		let tf = TFun(List.map (fun _ -> ("",false,t_dynamic)) el,t_dynamic) in
		let call () =
			let ef = mk (TField(fa.fa_on,FieldAccess.apply_fa fa.fa_field fa.fa_host)) tf fa.fa_pos in
			mk (TCall(ef,[])) t_dynamic p
		in
		make_field_call_candidate [] t_dynamic [] tf fa.fa_field call
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
	| [cf] ->
		if overload_kind = OverloadProper then maybe_check_access cf;
		begin try
			attempt_call cf
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
		if overload_kind = OverloadProper then begin match Overloads.Resolution.reduce_compatible candidates with
			| [] -> fail()
			| [fcc] ->
				maybe_check_access fcc.fc_field;
				fcc
			| fcc :: l ->
				display_error ctx "Ambiguous overload, candidates follow" p;
				let st = s_type (print_context()) in
				List.iter (fun fcc ->
					display_error ctx (Printf.sprintf "... %s" (st fcc.fc_type)) fcc.fc_field.cf_name_pos;
				) (fcc :: l);
				fcc
		end else begin match List.rev candidates with
			| [] -> fail()
			| fcc :: _ -> fcc
		end

let type_generic_function ctx fa el_typed el with_type p =
	let c,tl,stat = match fa.fa_host with
		| FHInstance(c,tl) -> c,tl,false
		| FHStatic c -> c,[],true
		| _ -> die "" __LOC__
	in
	let cf = fa.fa_field in
	if cf.cf_params = [] then error "Function has no type parameters and cannot be generic" p;
	let map = if stat then (fun t -> t) else apply_params c.cl_params tl in
	let monos = Monomorph.spawn_constrained_monos map cf.cf_params in
	let map_monos t = apply_params cf.cf_params monos t in
	let map t = if stat then map_monos t else apply_params c.cl_params tl (map_monos t) in
	let t = map cf.cf_type in
	let args,ret = match t,el_typed with
		| TFun((_,_,ta) :: args,ret),(e :: _) ->
			unify ctx e.etype ta p;
			args,ret
		| TFun(args,ret),_ -> args,ret
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
	let el = el_typed @ el in
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
				cf2.cf_meta <- (Meta.NoCompletion,[],p) :: (Meta.NoUsing,[],p) :: (Meta.GenericInstance,[],p) :: cf.cf_meta
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
				Builder.make_typeexpr (TClassDecl c) p
			| _ ->
				fa.fa_on
		in
		let fa = if stat then FStatic (c,cf2) else FInstance (c,tl,cf2) in
		let e = mk (TField(e,fa)) cf2.cf_type p in
		make_call ctx e el ret p
	with Generic.Generic_Exception (msg,p) ->
		error msg p)

let abstract_using_param_type sea = match follow sea.se_this.etype with
	| TAbstract(a,tl) when has_class_field_flag sea.se_access.fa_field CfImpl -> apply_params a.a_params tl a.a_this
	| _ -> sea.se_this.etype

class call_dispatcher
	(ctx : typer)
	(mode : access_mode)
	(with_type : WithType.t)
	(p : pos)
=
	let is_set = match mode with MSet _ -> true | _ -> false in
	let check_assign () = if is_set then invalid_assign p in

object(self)

	method private make_field_call (fa : field_access) (el_typed : texpr list) (el : expr list) =
		let fcc = unify_field_call ctx fa el_typed el p fa.fa_inline in
		if has_class_field_flag fcc.fc_field CfAbstract then begin match fa.fa_on.eexpr with
			| TConst TSuper -> display_error ctx (Printf.sprintf "abstract method %s cannot be accessed directly" fcc.fc_field.cf_name) p;
			| _ -> ()
		end;
		fcc.fc_data()

	method private macro_call (ethis : texpr) (cf : tclass_field) (el : expr list) =
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

	(* Calls `e` with arguments `el`. Does not inspect the callee expression, so it should only be
	   used with actual expression calls and not with something like field calls. *)
	method expr_call (e : texpr) (el : expr list) =
		check_assign();
		let rec loop t = match follow t with
		| TFun (args,r) ->
			let el, tfunc = unify_call_args ctx el args r p false false in
			let r = match tfunc with TFun(_,r) -> r | _ -> die "" __LOC__ in
			mk (TCall (e,el)) r p
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

	(* Calls the resolve method represented by `sea` with an additional string-expression argument `name`. *)
	method resolve_call (sea : static_extension_access) (name : string) =
		let eparam = sea.se_this in
		let e_name = Texpr.Builder.make_string ctx.t name null_pos in
		self#field_call sea.se_access [eparam;e_name] []

	method setter_call fa el_typed el =
		let fa_set = match FieldAccess.resolve_accessor fa (MSet None) with
			| AccessorFound fa -> fa
			| _ -> error "Could not resolve accessor" p
		in
		let dispatcher = new call_dispatcher ctx (MCall el) with_type p in
		dispatcher#field_call fa_set el_typed el

	(* Calls the field represented by `fa` with the typed arguments `el_typed` and the syntactic arguments `el`.

	   This function inspects the nature of the field being called and dispatches the call accordingly:

	   * If the field is `@:generic`, call `type_generic_function`.
	   * If the field is a non-macro method, call it via `make_field_call`.
	   * If the field is a property, resolve the accessor (depending on `mode`) and recurse onto it.
	   * Otherwise, call the field as a normal expression via `expr_call`.
	*)
	method field_call (fa : field_access) (el_typed : texpr list) (el : expr list) =
		match fa.fa_field.cf_kind with
		| Method (MethNormal | MethInline | MethDynamic) ->
			check_assign();
			 if has_class_field_flag fa.fa_field CfGeneric then begin
				type_generic_function ctx fa el_typed el with_type p
			end else
				self#make_field_call fa el_typed el
		| Method MethMacro ->
			begin match el_typed with
			| [] ->
				self#macro_call fa.fa_on fa.fa_field el
			| el_typed ->
				let cur = ctx.this_stack in
				let el' = List.map (fun e -> fst (push_this ctx e)) el_typed in
				let e = self#macro_call fa.fa_on fa.fa_field (el' @ el) in
				ctx.this_stack <- cur;
				e
			end;
		| Var v ->
			begin match (if is_set then v.v_write else v.v_read) with
			| AccCall ->
				begin match FieldAccess.resolve_accessor fa mode with
				| AccessorFound fa' ->
					let t = FieldAccess.get_map_function fa fa.fa_field.cf_type in
					let e = self#field_call fa' el_typed el in
					if not (type_iseq_strict t e.etype) then mk (TCast(e,None)) t e.epos else e
				| AccessorAnon ->
					(* Anons might not have the accessor defined and rely on FDynamic in such cases *)
					let e = fa.fa_on in
					let t = FieldAccess.get_map_function fa fa.fa_field.cf_type in
					let tf = tfun (List.map (fun e -> e.etype) el_typed) t in
					make_call ctx (mk (TField (e,quick_field_dynamic e.etype ("get_" ^ fa.fa_field.cf_name))) tf p) el_typed t p
				| AccessorNotFound ->
					error ("Could not resolve accessor") fa.fa_pos
				| AccessorInvalid ->
					die "Trying to resolve accessor on field that isn't AccCall" __LOC__
				end
			| _ ->
				self#expr_call (FieldAccess.get_field_expr fa FCall) el
			end
end

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