open Globals
open Ast
open Type
open Common
open Typecore
open Error
open FieldAccess

let is_forced_inline c cf =
	match c with
	| Some { cl_kind = KAbstractImpl _ } -> true
	| Some c when has_class_flag c CExtern -> true
	| _ when has_class_field_flag cf CfExtern -> true
	| _ -> false

let rec unify_call_args ctx el args r callp inline force_inline in_overload =
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
	let handle_errors fn =
		try
			fn()
		with Error(l,p) when (match l with Call_error _ | Module_not_found _ -> false | _ -> true) ->
			raise (WithTypeError (l,p))
	in
	(* let force_inline, is_extern = match cf with Some(TInst(c,_),f) -> is_forced_inline (Some c) f, (has_class_flag c CExtern) | _ -> false, false in *)
	let type_against name t e =
		handle_errors (fun() ->
			let e = type_expr ctx e (WithType.with_argument t name) in
			!cast_or_unify_raise_ref ctx t e e.epos
		)
	in
	let rec loop el args = match el,args with
		| [],[] ->
			begin match List.rev !invalid_skips with
				| [] -> ()
				| name :: _ -> call_error (Cannot_skip_non_nullable name) callp;
			end;
			[]
		| _,[name,false,TAbstract({ a_path = ["cpp"],"Rest" },[t])] ->
			(try List.map (fun e -> type_against name t e) el
			with WithTypeError(ul,p) -> arg_error ul name false p)
		| _,[name,false,t] when ExtType.is_rest (follow t) ->
			begin match follow t with
				| TAbstract({a_path=(["haxe"],"Rest")},[arg_t]) ->
					let unexpected_spread p =
						arg_error (Custom "Cannot spread arguments with additional rest arguments") name false p
					in
					(* these platforms deal with rest args on their own *)
					if ctx.com.config.pf_supports_rest_args then
						let type_rest t =
							List.map (fun e ->
								match e with
								| (EUnop (Spread,Prefix,_),p) -> unexpected_spread p
								| _ -> type_against name (t()) e
							) el
						in
						match el with
						| [(EUnop (Spread,Prefix,e),p)] ->
							(try [mk (TUnop (Spread, Prefix, type_against name t e)) t p]
							with WithTypeError(ul,p) -> arg_error ul name false p)
						| _ when ExtType.is_mono (follow arg_t) ->
							(try
								let el = type_rest mk_mono in
								unify ctx (unify_min ctx el) arg_t (punion_el callp el);
								el
							with WithTypeError(ul,p) ->
								arg_error ul name false p)
						| _ ->
							(try
								type_rest (fun() -> arg_t)
							with WithTypeError(ul,p) ->
								arg_error ul name false p)
					(* for other platforms make sure rest arguments are wrapped in an array *)
					else begin
						match el with
						| [(EUnop (Spread,Prefix,e),p)] ->
							(try [type_against name t e]
							with WithTypeError(ul,p) -> arg_error ul name false p)
						| [] ->
							(try [type_against name t (EArrayDecl [],callp)]
							with WithTypeError(ul,p) -> arg_error ul name false p)
						| (_,p1) :: _ ->
							let p =
								List.fold_left (fun p (e1,p2) ->
									match e1 with
									| EUnop (Spread,Prefix,_) -> unexpected_spread p2
									| _ -> punion p p2
								) p1 el
							in
							(try
								let do_type e = [type_against name t e] in
								let e = EArrayDecl el,p in
								(* typer requires dynamic arrays to be explicitly declared as Array<Dynamic> *)
								if follow arg_t == t_dynamic then begin
									let dynamic = CTPath(mk_type_path ([],"Dynamic")),p in
									let params = [TPType dynamic] in
									let tp = mk_type_path ~params ([],"Array") in
									do_type (ECheckType(e,(CTPath tp, p)),p) (* ([arg1, arg2...]:Array<Dynamic>) *)
								end else
									do_type e
							with WithTypeError(ul,p) ->
								arg_error ul name false p
							)
					end
				| _ ->
					die "" __LOC__
			end
		| [],(_,false,_) :: _ ->
			call_error (Not_enough_arguments args) callp
		| [],(name,true,t) :: args ->
			if not ctx.allow_transform then begin
				ignore(loop [] args);
				[]
			end else begin match loop [] args with
				| [] when not (inline && (ctx.g.doinline || force_inline)) && not ctx.com.config.pf_pad_nulls ->
					if is_pos_infos t then [mk_pos_infos t]
					else []
				| args ->
					let e_def = default_value name t in
					e_def :: args
			end
		| (e,p) :: el, [] ->
			begin match List.rev !skipped with
				| [] ->
					if ctx.is_display_file && not (Diagnostics.error_in_diagnostics_run ctx.com p) then begin
						ignore(type_expr ctx (e,p) WithType.value);
						ignore(loop el [])
					end;
					call_error Too_many_arguments p
				| (s,ul,p) :: _ -> arg_error ul s true p
			end
		| e :: el,(name,opt,t) :: args ->
			let might_skip = List.length el < List.length args in
			begin try
				let e = type_against name t e in
				e :: loop el args
			with
				WithTypeError (ul,p)->
					if opt && might_skip then
						let e_def = skip name ul t p in
						e_def :: loop (e :: el) args
					else
						match List.rev !skipped with
						| [] -> arg_error ul name opt p
						| (s,ul,p) :: _ -> arg_error ul s true p
			end
	in
	let restore =
		let in_call_args = ctx.in_call_args in
		let in_overload_call_args = ctx.in_overload_call_args in
		ctx.in_call_args <- true;
		ctx.in_overload_call_args <- in_overload;
		(fun () ->
			ctx.in_call_args <- in_call_args;
			ctx.in_overload_call_args <- in_overload_call_args;
		)
	in
	let el = try loop el args with exc -> restore(); raise exc; in
	restore();
	el,TFun(args,r)

type overload_kind =
	| OverloadProper (* @:overload or overload *)
	| OverloadMeta (* @:overload(function() {}) *)
	| OverloadNone

(**
	Unifies `el_typed` against the types from `args` list starting at the beginning
	of `args` list.

	Returns a tuple of a part of `args` covered by `el_typed`, and a part of `args`
	not used for `el_typed` unification.
*)
let unify_typed_args ctx tmap args el_typed call_pos =
	let rec loop acc_args tmap args el =
		match args,el with
		| [], _ :: _ ->
			let call_error = Call_error(Too_many_arguments) in
			raise(Error(call_error,call_pos))
		| _, [] ->
			List.rev acc_args,args
		| ((_,opt,t0) as arg) :: args,e :: el ->
			begin try
				unify_raise (tmap e.etype) t0 e.epos;
			with Error(Unify _ as msg,p) ->
				let call_error = Call_error(Could_not_unify msg) in
				raise(Error(call_error,p))
			end;
			loop (arg :: acc_args) (fun t -> t) args el
	in
	loop [] tmap args el_typed

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
	(* Delayed display handling works like this: If ctx.in_overload_call_args is set (via attempt_calls calling unify_call_args' below),
	   the code which normally raises eager Display exceptions (in typerDisplay.ml handle_display) instead stores them in ctx.delayed_display.
	   The overload handling here extracts them and associates the exception with the field call candidates. Afterwards, normal overload resolution
	   can take place and only then the display callback is actually committed.
	*)
	let extract_delayed_display () = match ctx.delayed_display with
		| Some f ->
			ctx.delayed_display <- None;
			Some f
		| None ->
			None
	in
	let raise_augmented_display_exception cf de =
		let default () = raise (DisplayException.DisplayException de) in
		let javadoc = match gen_doc_text_opt cf.cf_doc with
			| None -> default()
			| Some s -> new Javadoc.javadoc s
		in
		match de with
		| DisplayHover hover ->
			begin match hover.hexpected with
			| Some (WithType(t,Some si)) ->
				let si = match si with
				| FunctionArgument ({si_doc = None} as si) ->
					WithType.FunctionArgument {si with si_doc = javadoc#get_param_info si.si_name};
				| StructureField ({si_doc = None} as si) ->
					WithType.StructureField {si with si_doc = javadoc#get_param_info si.si_name};
				| _ ->
					si
				in
				let expected = WithType.WithType(t,Some si) in
				DisplayException.raise_hover hover.hitem (Some expected) hover.hpos
			| _ ->
				default()
			end
		| _ ->
			default()
	in
	let commit_delayed_display fcc =
		Option.may (fun de ->
			raise_augmented_display_exception fcc.fc_field de;
		) (snd fcc.fc_data);
		{fcc with fc_data = fst fcc.fc_data}
	in
	let attempt_call cf in_overload =
		let monos = Monomorph.spawn_constrained_monos map cf.cf_params in
		let t = map (apply_params cf.cf_params monos cf.cf_type) in
		match follow t with
		| TFun(args,ret) ->
			let args_typed,args = unify_typed_args ctx tmap args el_typed p in
			let el,_ =
				try
					unify_call_args ctx el args ret p inline is_forced_inline in_overload
				with DisplayException.DisplayException de ->
					raise_augmented_display_exception cf de;
			in
			(* here *)
			let el = el_typed @ el in
			let tf = TFun(args_typed @ args,ret) in
			let mk_call () =
				let ef = mk (TField(fa.fa_on,FieldAccess.apply_fa cf fa.fa_host)) t fa.fa_pos in
				!make_call_ref ctx ef el ret ~force_inline:inline p
			in
			make_field_call_candidate el ret monos tf cf (mk_call,extract_delayed_display())
		| t ->
			typing_error (s_type (print_context()) t ^ " cannot be called") p
	in
	let maybe_raise_unknown_ident cerr p =
		let rec loop err =
			match err with
			| Unknown_ident _ -> typing_error (error_msg err) p
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
					m,m.tm_type,m.tm_down_constraints
				) ctx.monomorphs.perfunction in
				let current_monos = ctx.monomorphs.perfunction in
				begin try
					let candidate = attempt_call cf true in
					ctx.monomorphs.perfunction <- current_monos;
					if overload_kind = OverloadProper then begin
						let candidates,failures = loop candidates in
						candidate :: candidates,failures
					end else
						[candidate],[]
				with Error ((Call_error cerr as err),p) ->
					List.iter (fun (m,t,constr) ->
						if t != m.tm_type then m.tm_type <- t;
						if constr != m.tm_down_constraints then m.tm_down_constraints <- constr;
					) known_monos;
					ctx.monomorphs.perfunction <- current_monos;
					maybe_raise_unknown_ident cerr p;
					let candidates,failures = loop candidates in
					candidates,(cf,err,p,extract_delayed_display()) :: failures
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
			commit_delayed_display (attempt_call cf false)
		with Error _ when Common.ignore_error ctx.com ->
			fail_fun();
		end
	| _ ->
		let candidates,failures = attempt_calls candidates in
		let fail () =
			let failures = List.map (fun (cf,err,p,delayed_display) ->
				(* If any resolution attempt had a delayed display result, we might as well raise it now. *)
				Option.may (fun de ->
					raise_augmented_display_exception cf de;
				) delayed_display;
				cf,error_msg err,p
			) failures in
			let failures = remove_duplicates (fun (_,msg1,_) (_,msg2,_) -> msg1 <> msg2) failures in
			begin match failures with
			| [_,msg,p] ->
				typing_error msg p
			| _ ->
				display_error ctx.com "Could not find a suitable overload, reasons follow" p;
				List.iter (fun (cf,msg,p2) ->
					display_error ctx.com ("Overload resolution failed for " ^ (s_type (print_context()) cf.cf_type)) p;
					display_error ctx.com msg p2;
				) failures;
				typing_error "End of overload failure reasons" p
			end
		in
		if overload_kind = OverloadProper then begin match Overloads.Resolution.reduce_compatible candidates with
			| [] -> fail()
			| [fcc] ->
				maybe_check_access fcc.fc_field;
				commit_delayed_display fcc
			| fcc :: l ->
				display_error ctx.com "Ambiguous overload, candidates follow" p;
				let st = s_type (print_context()) in
				List.iter (fun fcc ->
					display_error ctx.com (Printf.sprintf "... %s" (st fcc.fc_type)) fcc.fc_field.cf_name_pos;
				) (fcc :: l);
				commit_delayed_display fcc
		end else begin match List.rev candidates with
			| [] -> fail()
			| fcc :: _ -> commit_delayed_display fcc
		end

class call_dispatcher
	(ctx : typer)
	(mode : access_mode)
	(with_type : WithType.t)
	(p : pos)
=
	let is_set = match mode with MSet _ -> true | _ -> false in
	let check_assign () = if is_set && not (Common.ignore_error ctx.com) then invalid_assign p in

object(self)

	method private make_field_call (fa : field_access) (el_typed : texpr list) (el : expr list) =
		let fcc = unify_field_call ctx fa el_typed el p fa.fa_inline in
		if has_class_field_flag fcc.fc_field CfGeneric then begin
			!type_generic_function_ref ctx fa fcc with_type p
		end else begin
			if has_class_field_flag fcc.fc_field CfAbstract then begin match fa.fa_on.eexpr with
				| TConst TSuper -> display_error ctx.com (Printf.sprintf "abstract method %s cannot be accessed directly" fcc.fc_field.cf_name) p;
				| _ -> ()
			end;
			fcc.fc_data()
		end

	method private macro_call (ethis : texpr) (cf : tclass_field) (el : expr list) =
		if ctx.macro_depth > 300 then typing_error "Stack overflow" p;
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
		let old = ctx.com.error in
		ctx.com.error <- (fun msg ep ->
			(* display additional info in the case the error is not part of our original call *)
			if ep.pfile <> p.pfile || ep.pmax < p.pmin || ep.pmin > p.pmax then begin
				locate_macro_error := false;
				old msg ep;
				locate_macro_error := true;
				old (compl_msg "Called from macro here") p;
			end else
				old msg ep;
		);
		let e = try
			f()
		with exc ->
			ctx.com.error <- old;
			!ethis_f();
			raise exc
		in
		let e = Diagnostics.secure_generated_code ctx e in
		ctx.com.error <- old;
		!ethis_f();
		e

	(* Calls `e` with arguments `el`. Does not inspect the callee expression, so it should only be
	   used with actual expression calls and not with something like field calls. *)
	method expr_call (e : texpr) (el_typed : texpr list) (el : expr list) =
		check_assign();
		let default t =
			let el = el_typed @ List.map (fun e -> type_expr ctx e WithType.value) el in
			let t = if t == t_dynamic then
				t_dynamic
			else if ctx.untyped then
				mk_mono()
			else
				typing_error (s_type (print_context()) e.etype ^ " cannot be called") e.epos
			in
			mk (TCall (e,el)) t p
		in
		let rec loop t = match follow t with
		| TFun (args,r) ->
			let args_typed,args_left = unify_typed_args ctx (fun t -> t) args el_typed p in
			let el, tfunc = unify_call_args ctx el args_left r p false false false in
			let el = el_typed @ el in
			let r = match tfunc with TFun(_,r) -> r | _ -> die "" __LOC__ in
			mk (TCall (e,el)) r p
		| TAbstract(a,tl) as t ->
			let check_callable () =
				if Meta.has Meta.Callable a.a_meta then
					loop (Abstract.get_underlying_type a tl)
				else
					default t
				in
			begin match a.a_call,a.a_impl with
			| Some cf,Some c ->
				let e_static = Builder.make_static_this c e.epos in
				let fa = FieldAccess.create e_static cf (FHAbstract(a,tl,c)) false e.epos in
				self#field_call fa (e :: el_typed) el
			| _ ->
				check_callable();
			end
		| TMono _ ->
			let t = mk_mono() in
			let el = el_typed @ List.map (fun e -> type_expr ctx e WithType.value) el in
			unify ctx (tfun (List.map (fun e -> e.etype) el) t) e.etype e.epos;
			mk (TCall (e,el)) t p
		| t ->
			default t
		in
		loop e.etype

	(* Calls the resolve method represented by `sea` with an additional string-expression argument `name`. *)
	method resolve_call (sea : static_extension_access) (name : string) =
		let eparam = sea.se_this in
		let e_name = Texpr.Builder.make_string ctx.t name null_pos in
		self#field_call sea.se_access [eparam;e_name] []

	(* Resolves the accessor function for `fa` and calls it with the provided arguments.
	   If no accessor function is found (AccessorAnon case), a generic field access is generated instead.
	*)
	method accessor_call fa el_typed el =
		match FieldAccess.resolve_accessor fa mode with
			| AccessorFound fa_accessor ->
				let dispatcher = new call_dispatcher ctx (MCall el) with_type p in
				let e = dispatcher#field_call fa_accessor el_typed el in
				let t = FieldAccess.get_map_function fa fa.fa_field.cf_type in
				if not (type_iseq_strict t e.etype) then mk (TCast(e,None)) t e.epos else e
			| AccessorAnon ->
				let e = fa.fa_on in
				let t = FieldAccess.get_map_function fa fa.fa_field.cf_type in
				let el = List.map (fun e -> type_expr ctx e WithType.value) el in
				let el_typed = el_typed @ el in
				let tf = tfun (List.map (fun e -> e.etype) el_typed) t in
				let name = Printf.sprintf "%s_%s" (if is_set then "set" else "get") fa.fa_field.cf_name in
				make_call ctx (mk (TField (e,quick_field_dynamic e.etype name)) tf p) el_typed t p
			| _ ->
				typing_error "Could not resolve accessor" p

	(* Calls the field represented by `fa` with the typed arguments `el_typed` and the syntactic arguments `el`.

	   This function inspects the nature of the field being called and dispatches the call accordingly:

	   * If the field is a non-macro method, call it via `make_field_call`.
	   * If the field is a property, resolve the accessor (depending on `mode`) and recurse onto it.
	   * Otherwise, call the field as a normal expression via `expr_call`.
	*)
	method field_call (fa : field_access) (el_typed : texpr list) (el : expr list) =
		match fa.fa_field.cf_kind with
		| Method (MethNormal | MethInline | MethDynamic) ->
			check_assign();
			self#make_field_call fa el_typed el
		| Method MethMacro ->
			begin match el_typed with
			| [] ->
				self#macro_call fa.fa_on fa.fa_field el
			| el_typed ->
				let el',fl = List.split (List.map (fun e -> push_this ctx e) el_typed) in
				let e = self#macro_call fa.fa_on fa.fa_field (el' @ el) in
				List.iter (fun f -> f()) fl;
				e
			end;
		| Var v ->
			begin match (if is_set then v.v_write else v.v_read) with
			| AccCall ->
				self#accessor_call fa el_typed el
			| _ ->
				self#expr_call (FieldAccess.get_field_expr fa FCall) el_typed el
			end
end

let maybe_reapply_overload_call ctx e =
	match e.eexpr with
		| TCall({eexpr = TField(e1,fa)} as ef,el) ->
			let recall fh cf =
				let fa = FieldAccess.create e1 cf fh false ef.epos in
				let fcc = unify_field_call ctx fa el [] e.epos false in
				let e1 = fcc.fc_data() in
				(try Type.unify e1.etype e.etype
				with Unify_error _ -> die ~p:e.epos "Failed to reapply overload call" __LOC__);
				e1
			in
			begin match fa with
			| FStatic(c,cf) when has_class_field_flag cf CfOverload ->
				recall (FHStatic c) cf
			| FInstance(c,tl,cf) when has_class_field_flag cf CfOverload ->
				recall (FHInstance(c,tl)) cf
			| _ ->
				e
			end
		| _ ->
			e