open Globals
open Ast
open Type
open Typecore
open Error
open FieldAccess

let is_forced_inline c cf =
	match c with
	| Some { cl_kind = KAbstractImpl _ } -> true
	| Some c when has_class_flag c CExtern -> true
	| _ when has_class_field_flag cf CfExtern -> true
	| _ -> false

let relative_path ctx file =
	let slashes path = String.concat "/" (ExtString.String.nsplit path "\\") in
	let fpath = slashes (Path.get_full_path file) in
	let fpath_lower = String.lowercase fpath in
	let flen = String.length fpath_lower in
	let rec loop = function
		| [] -> file
		| path :: l ->
			let spath = String.lowercase (slashes path) in
			let slen = String.length spath in
			if slen > 0 && slen < flen && String.sub fpath_lower 0 slen = spath then String.sub fpath slen (flen - slen) else loop l
	in
	loop ctx.com.Common.class_path

let mk_infos ctx p params =
	let file = if ctx.in_macro then p.pfile else if Common.defined ctx.com Define.AbsolutePath then Path.get_full_path p.pfile else relative_path ctx p.pfile in
	(EObjectDecl (
		(("fileName",null_pos,NoQuotes) , (EConst (String(file,SDoubleQuotes)) , p)) ::
		(("lineNumber",null_pos,NoQuotes) , (EConst (Int (string_of_int (Lexer.get_error_line p))),p)) ::
		(("className",null_pos,NoQuotes) , (EConst (String (s_type_path ctx.curclass.cl_path,SDoubleQuotes)),p)) ::
		if ctx.curfield.cf_name = "" then
			params
		else
			(("methodName",null_pos,NoQuotes), (EConst (String (ctx.curfield.cf_name,SDoubleQuotes)),p)) :: params
	) ,p)

let rec is_pos_infos = function
	| TMono r ->
		(match r.tm_type with
		| Some t -> is_pos_infos t
		| _ -> false)
	| TLazy f ->
		is_pos_infos (lazy_type f)
	| TType ({ t_path = ["haxe"] , "PosInfos" },[]) ->
		true
	| TType (t,tl) ->
		is_pos_infos (apply_params t.t_params tl t.t_type)
	| TAbstract({a_path=[],"Null"},[t]) ->
		is_pos_infos t
	| _ ->
		false

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
			!cast_or_unify_raise_ref ctx t e e.epos
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
			let tmap = if fa.fa_field.cf_name = "_new" (* TODO: BAD BAD BAD BAD *) then (fun t -> t) else (fun t -> map a.a_this) in
			expand_overloads fa.fa_field,Some c,true,map,tmap
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
				!make_call_ref ctx ef el ret ~force_inline:inline p
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
	let c,stat = match fa.fa_host with
		| FHInstance(c,tl) -> c,false
		| FHStatic c -> c,true
		| FHAbstract(a,tl,c) -> c,true
		| _ -> die "" __LOC__
	in
	let cf = fa.fa_field in
	if cf.cf_params = [] then error "Function has no type parameters and cannot be generic" p;
	let fcc = unify_field_call ctx fa el_typed el p false in
	begin match with_type with
		| WithType.WithType(t,_) -> unify ctx fcc.fc_ret t p
		| _ -> ()
	end;
	let monos = fcc.fc_monos in
	List.iter (fun t -> match follow t with
		| TMono m -> safe_mono_close ctx m p
		| _ -> ()
	) monos;
	let el = List.map fst fcc.fc_args in
	(try
		let gctx = Generic.make_generic ctx cf.cf_params monos p in
		let name = cf.cf_name ^ "_" ^ gctx.Generic.name in
		let unify_existing_field tcf pcf = try
			unify_raise ctx tcf fcc.fc_type p
		with Error(Unify _,_) as err ->
			display_error ctx ("Cannot create field " ^ name ^ " due to type mismatch") p;
			display_error ctx (compl_msg "Conflicting field was defined here") pcf;
			raise err
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
				mk_field ~static:stat name fcc.fc_type cf.cf_pos cf.cf_name_pos
			in
			if stat then begin
				if Meta.has Meta.GenericClassPerMethod c.cl_meta then begin
					let c = Generic.static_method_container gctx c cf p in
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
					let cf2 = mk_cf2 name in
					c.cl_statics <- PMap.add cf2.cf_name cf2 c.cl_statics;
					c.cl_ordered_statics <- cf2 :: c.cl_ordered_statics;
					finalize_field c cf2;
					{fa with fa_field = cf2}
				end
			end else begin
				let cf2 = mk_cf2 name in
				if has_class_field_flag cf CfOverride then add_class_field_flag cf2 CfOverride;
				c.cl_fields <- PMap.add cf2.cf_name cf2 c.cl_fields;
				c.cl_ordered_fields <- cf2 :: c.cl_ordered_fields;
				finalize_field c cf2;
				{fa with fa_field = cf2}
			end
		in
		let e = FieldAccess.get_field_expr fa FCall in
		make_call ctx e el fcc.fc_ret p
	with Generic.Generic_Exception (msg,p) ->
		error msg p)

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
				locate_macro_error := false;
				old ctx msg ep;
				locate_macro_error := true;
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