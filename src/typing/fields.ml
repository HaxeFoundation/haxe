open Globals
open Common
open Ast
open Type
open TyperBase
open Error
open Typecore
open FieldAccess

module TypeFieldConfig = struct
	type t = {
		allow_resolve : bool;
		do_resume : bool;
	}

	let allow_resolve cfg = cfg.allow_resolve

	let do_resume cfg = cfg.do_resume

	let default = {
		allow_resolve = true;
		do_resume = false;
	}

	let create resume = {
		allow_resolve = true;
		do_resume = resume;
	}

	let with_resume cfg = {cfg with do_resume = true}
end

(*
	temporally remove the constant flag from structures to allow larger unification
*)
let remove_constant_flag t callb =
	let tmp = ref [] in
	let rec loop t =
		match follow t with
		| TAnon a ->
			if !(a.a_status) = Const then begin
				a.a_status := Closed;
				tmp := a :: !tmp;
			end;
			PMap.iter (fun _ f -> loop f.cf_type) a.a_fields;
		|  _ ->
			()
	in
	let restore() =
		List.iter (fun a -> a.a_status := Const) (!tmp)
	in
	try
		loop t;
		let ret = callb (!tmp <> []) in
		restore();
		ret
	with e ->
		restore();
		raise e

let enum_field_type ctx en ef p =
	let tl_en = Monomorph.spawn_constrained_monos (fun t -> t) en.e_params in
	let map = apply_params en.e_params tl_en in
	let tl_ef = Monomorph.spawn_constrained_monos map ef.ef_params in
	let map t = map (apply_params ef.ef_params tl_ef t) in
	map ef.ef_type

let field_type ctx c pl f p =
	match f.cf_params with
	| [] -> f.cf_type
	| l ->
		let monos = Monomorph.spawn_constrained_monos (if pl = [] then (fun t -> t) else apply_params c.cl_params pl) f.cf_params in
		apply_params l monos f.cf_type

let no_abstract_constructor c p =
	if has_class_flag c CAbstract then raise_typing_error (Abstract_class (TClassDecl c)) p

let check_constructor_access ctx c f p =
	if (Meta.has Meta.CompilerGenerated f.cf_meta) then display_error ctx.com (error_msg (No_constructor (TClassDecl c))) p;
	if not (can_access ctx c f true || extends ctx.curclass c) && not ctx.untyped then display_error ctx.com (Printf.sprintf "Cannot access private constructor of %s" (s_class_path c)) p

let check_no_closure_meta ctx cf fa mode p =
	match mode with
	| MGet | MSet _ when not (DisplayPosition.display_position#enclosed_in p) ->
		let check_field f cl_meta =
			match f.cf_kind with
			| Method _ ->
				if
					Meta.has Meta.NoClosure cl_meta
					|| Meta.has Meta.NoClosure f.cf_meta
				then
					typing_error ("Method " ^ f.cf_name ^ " cannot be used as a value") p
			| _ -> ()
		in
		begin match cf.cf_kind with
		| Method _ ->
			let meta = match fa with
				| FHStatic c | FHInstance(c,_) | FHAbstract(_,_,c) -> c.cl_meta
				| _ -> []
			in
			check_field cf meta
		| _ ->
			()
		end
	| _ ->
		()

let field_access ctx mode f fh e pfield =
	let pfull = punion e.epos pfield in
	let is_set = match mode with MSet _ -> true | _ -> false in
	check_no_closure_meta ctx f fh mode pfield;
	let bypass_accessor = if ctx.bypass_accessor > 0 then (ctx.bypass_accessor <- ctx.bypass_accessor - 1; true) else false in
	let make_access inline = FieldAccess.create e f fh (inline && ctx.allow_inline) pfull in
	match f.cf_kind with
	| Method m ->
		let normal () = AKField(make_access false) in
		if is_set && m <> MethDynamic && not ctx.untyped then typing_error "Cannot rebind this method : please use 'dynamic' before method declaration" pfield;
		let maybe_check_visibility c static =
			(* For overloads we have to resolve the actual field before we can check accessibility. *)
			begin match mode with
			| MCall _ when has_class_field_flag f CfOverload ->
				()
			| _ ->
				check_field_access ctx c f static pfield
			end;
		in
		let default () =
			match m, mode with
			| MethInline, _ when ctx.g.doinline && ctx.allow_inline ->
				AKField (make_access true)
			| MethMacro, MGet ->
				display_error ctx.com "Macro functions must be called immediately" pfield; normal()
			| _ , MGet ->
				if has_class_field_flag f CfGeneric then display_error ctx.com "Cannot create closure on generic function" pfield;
				normal()
			| _ ->
				normal()
		in
		begin match fh with
		| FHInstance(c,tl) ->
			if e.eexpr = TConst TSuper then begin match mode with
				| MSet _ | MGet ->
					display_error ctx.com "Cannot create closure on super method" pfield
				| MCall _ ->
					()
			end;
			(* We need the actual class type (i.e. a potential child class) for visibility checks. *)
			begin match follow e.etype with
			| TInst(c,_) ->
				maybe_check_visibility c false;
			| _ ->
				()
			end;
			default();
		| FHStatic c ->
			maybe_check_visibility c true;
			default()
		| FHAnon ->
			default()
		| FHAbstract(a,tl,c) ->
			maybe_check_visibility c true;
			let sea = make_abstract_static_extension_access a tl c f e false pfull in
			AKUsingField sea
		end;
	| Var v ->
		begin match fh with
		| FHStatic c | FHAbstract(_,_,c) ->
			check_field_access ctx c f true pfield
		| FHInstance _ ->
			begin match follow e.etype with
			| TInst(c,_) ->
				check_field_access ctx c f false pfield
			| _ ->
				()
			end;
			if e.eexpr = TConst TSuper then begin match mode with
				| MGet | MCall _ when v.v_read = AccCall ->
					()
				| MSet _ when v.v_write = AccCall ->
					()
				| _ ->
					display_error ctx.com "Normal variables cannot be accessed with 'super', use 'this' instead" pfield;
			end;
		| FHAnon ->
			()
		end;
		let normal inline =
			AKField (make_access inline)
		in
		match (match mode with MGet | MCall _ -> v.v_read | MSet _ -> v.v_write) with
		| AccNo when not (Meta.has Meta.PrivateAccess ctx.meta) ->
			(match follow e.etype with
			| TInst (c,_) when extends ctx.curclass c || can_access ctx c { f with cf_flags = unset_flag f.cf_flags (int_of_class_field_flag CfPublic) } false ->
				normal false
			| TAnon a ->
				(match !(a.a_status) with
				| Statics c2 when ctx.curclass == c2 || can_access ctx c2 { f with cf_flags = unset_flag f.cf_flags (int_of_class_field_flag CfPublic) } true -> normal false
				| _ -> if ctx.untyped then normal false else AKNo f.cf_name)
			| _ ->
				if ctx.untyped then normal false else AKNo f.cf_name)
		| AccNormal | AccNo ->
			normal false
		| AccCall when (not ctx.allow_transform) || (ctx.in_display && DisplayPosition.display_position#enclosed_in pfull) ->
			normal false
		| AccCall ->
			let m = (match mode with MSet _ -> "set_" | _ -> "get_") ^ f.cf_name in
			let bypass_accessor =
				bypass_accessor
				||
				(
					m = ctx.curfield.cf_name
					&&
					match e.eexpr with
					| TConst TThis -> true
					| TLocal v -> Option.map_default (fun vthis -> v == vthis) false ctx.vthis
					| TTypeExpr (TClassDecl c) when c == ctx.curclass -> true
					| _ -> false
				)
			in
			if bypass_accessor then (
				(match e.eexpr with TLocal _ when Common.defined ctx.com Define.Haxe3Compat -> warning ctx WTemp "Field set has changed here in Haxe 4: call setter explicitly to keep Haxe 3.x behaviour" pfield | _ -> ());
				if not (is_physical_field f) then begin
					display_error ctx.com "This field cannot be accessed because it is not a real variable" pfield;
					display_error ctx.com "Add @:isVar here to enable it" f.cf_pos;
				end;
				normal false
			)
			else begin match fh with
			| FHAbstract(a,tl,c) ->
				let sea = make_abstract_static_extension_access a tl c f e false pfull in
				AKUsingAccessor sea
			| _ ->
				AKAccessor (make_access false)
			end
		| AccNever ->
			if ctx.untyped then normal false else AKNo f.cf_name
		| AccInline ->
			normal true
		| AccCtor ->
			let is_child_of_abstract c =
				has_class_flag c CAbstract && extends ctx.curclass c
			in
			(match ctx.curfun, fh with
				| FunConstructor, FHInstance(c,_) when c == ctx.curclass || is_child_of_abstract c -> normal false
				| _ -> AKNo f.cf_name
			)
		| AccRequire (r,msg) ->
			match msg with
			| None -> error_require r pfield
			| Some msg -> typing_error msg pfield

let class_field ctx c tl name p =
	raw_class_field (fun f -> field_type ctx c tl f p) c tl name

(* Resolves field [i] on typed expression [e] using the given [mode]. *)
(* Note: if mode = MCall, with_type (if known) refers to the return type *)
let type_field cfg ctx e i p mode (with_type : WithType.t) =
	let pfield = if e.epos = p then p else { p with pmin = p.pmax - (String.length i) } in
	let is_set = match mode with MSet _ -> true | _ -> false in
	let field_access e f fmode = field_access ctx mode f fmode e pfield in
	let class_field_with_access e c tl =
		let c2, t, f = class_field ctx c tl i p in
		let fmode = match c2 with None -> FHAnon | Some (c,tl) -> FHInstance (c,tl) in
		let acc = field_access e f fmode in
		f, acc
	in
	let find_some = function
		| Some x -> x
		| None -> raise Not_found
	in
	let type_field_by_et f e t =
		f { e with etype = t } (follow_without_type t)
	in
	let type_field_by_e f e =
		f e (follow_without_type e.etype)
	in
	let rec type_field_by_list f = function
		| [] -> raise Not_found
		| x :: l -> try f x with Not_found -> type_field_by_list f l
	in
	let type_field_by_forward f meta a =
		let _,el,_ = Meta.get meta a.a_meta in
		if el <> [] && not (List.exists (fun e -> match fst e with
			| EConst (Ident i' | String (i',_)) -> i' = i
			| _ -> typing_error "Identifier or string expected as argument to @:forward" (pos e)
		) el) then raise Not_found;
		f()
	in
	let type_field_by_forward_static f a =
		type_field_by_forward f Meta.ForwardStatics a
	in
	let type_field_by_forward_member f e a tl =
		let f () = type_field_by_et f e (Abstract.get_underlying_type ~return_first:true a tl) in
		type_field_by_forward f Meta.Forward a
	in
	let type_field_by_typedef f e td tl =
		f e (follow_without_type (apply_typedef td tl))
	in
	let type_field_by_interfaces e c =
		(* For extern lib types we didn't go through check_interfaces and check_abstract_class, which handles some field
		   generation. We instead do this lazily here by browsing the implemented interfaces (issue #9768). *)
		if not (has_class_flag c CExtern && Meta.has Meta.LibType c.cl_meta) then raise Not_found;
		type_field_by_list (fun (ci,tl) ->
			let f, acc = class_field_with_access e ci tl in
			(* It should be fine to just add the field to our class to make future lookups a bit faster. *)
			TClass.add_field c f;
			acc
		) c.cl_implements
	in
	let rec type_field_by_type e t =
		let field_access = field_access e in
		match t with
		| TType (td,tl) -> type_field_by_typedef type_field_by_type e td tl
		| TInst (c,tl) ->
			(try
				snd (class_field_with_access e c tl)
			with Not_found -> try
				match c.cl_kind with
				| KTypeParameter tl ->
					type_field_by_list (fun t -> match follow t with
						| TAbstract _ -> type_field_by_e type_field_by_type (mk_cast e t p);
						| _ -> raise Not_found
					) tl
				| _ -> raise Not_found
			with Not_found ->
				type_field_by_interfaces e c
			)
		| TAnon a ->
			(try
				let f = PMap.find i a.a_fields in
				if has_class_field_flag f CfImpl && not (has_class_field_flag f CfEnum) then display_error ctx.com "Cannot access non-static abstract field statically" pfield;
				match !(a.a_status) with
				| EnumStatics en ->
					let c = try PMap.find f.cf_name en.e_constrs with Not_found -> die "" __LOC__ in
					let fmode = FEnum (en,c) in
					let t = enum_field_type ctx en c p in
					AKExpr (mk (TField (e,fmode)) t p)
				| Statics c ->
					field_access f (FHStatic c)
				| _ ->
					field_access f FHAnon
			with Not_found ->
				match !(a.a_status) with
				| Statics { cl_kind = KAbstractImpl a } ->
					type_field_by_forward_static (fun() ->
						let mt = try module_type_of_type a.a_this with Exit -> raise Not_found in
						let et = type_module_type ctx mt None p in
						type_field_by_e type_field_by_type et
					) a
				| _ -> raise Not_found
			)
		| TMono r ->
			let mk_field () = {
				(mk_field i (mk_mono()) p null_pos) with
				cf_kind = Var { v_read = AccNormal; v_write = if is_set then AccNormal else AccNo }
			} in
			let rec check_constr = function
			| CStructural (fields,is_open) ->
				(try
					let f = PMap.find i fields in
					(match f.cf_kind with
					(* We previously inferred to read-only, but now we want to write. This can happen in cases like #8079. *)
					| Var ({ v_write = AccNo } as acc) when is_open && is_set -> f.cf_kind <- Var { acc with v_write = AccNormal }
					| _ -> ());
					field_access f FHAnon
				with Not_found when is_open ->
					let f = mk_field() in
					Monomorph.add_down_constraint r (MField f);
					field_access f FHAnon
				)
			| CTypes tl ->
				type_field_by_list (fun (t,_) -> type_field_by_et type_field_by_type e t) tl
			| CUnknown ->
				if not (List.exists (fun (m,_) -> m == r) ctx.monomorphs.perfunction) && not (ctx.untyped && ctx.com.platform = Neko) then
					ctx.monomorphs.perfunction <- (r,p) :: ctx.monomorphs.perfunction;
				let f = mk_field() in
				Monomorph.add_down_constraint r (MField f);
				Monomorph.add_down_constraint r MOpenStructure;
				field_access f FHAnon
			| CMixed l ->
				let rec loop_constraints l =
					match l with
					| [] ->
						raise Not_found
					| constr :: l ->
						try check_constr constr
						with Not_found -> loop_constraints l
				in
				loop_constraints l
			in
			check_constr (Monomorph.classify_down_constraints r)
		| TAbstract (a,tl) ->
			(try
				let c = find_some a.a_impl in
				let f = PMap.find i c.cl_statics in
				if not (has_class_field_flag f CfImpl) then raise Not_found;
				field_access f (FHAbstract (a,tl,c))
			with Not_found ->
				type_field_by_forward_member type_field_by_type e a tl
			)
		| _ -> raise Not_found
	in
	let type_field_by_extension f t e =
		let check_constant_struct = ref false in
		let e = match t with
			| TInst _ when e.eexpr = TConst TSuper -> { e with eexpr = TCast(mk (TConst TThis) (mk_mono()) e.epos,None) }
			| _ -> e
		in
		let loop = type_field_by_list (fun (c,pc) ->
			let cf0 = PMap.find i c.cl_statics in
			let rec check cfl = match cfl with
				| [] ->
					raise Not_found
				| cf :: cfl when Meta.has Meta.NoUsing cf.cf_meta || not (can_access ctx c cf true) || (has_class_field_flag cf CfImpl) ->
					check cfl
				| cf :: cfl ->
					(* We always want to reset monomorphs here because they will be handled again when making the actual call. *)
					let current_monos = ctx.monomorphs.perfunction in
					let check () =
						ctx.monomorphs.perfunction <- current_monos;
						check cfl
					in
					try
						let monos = Monomorph.spawn_constrained_monos (fun t -> t) cf.cf_params in
						let cft = follow (apply_params cf.cf_params monos cf.cf_type) in
						match cft with
						| TFun ((_,_,(TType ({ t_path = ["haxe";"macro"],"ExprOf" },[t0]) | t0)) :: _,_) ->
							if t == t_dynamic && follow t0 != t then
								check()
							else begin
								let e = unify_static_extension ctx e t0 p in
								ImportHandling.mark_import_position ctx pc;
								ctx.monomorphs.perfunction <- current_monos;
								AKUsingField (make_static_extension_access c cf e false p)
							end
						| _ ->
							check()
					with Unify_error el | Error (Unify el,_) ->
						check_constant_struct := !check_constant_struct || List.exists (function
							| Has_extra_field _ -> true
							| _ -> false
						) el;
						check()
			in
			check (cf0 :: cf0.cf_overloads)
		) in
		try
			f loop
		with Not_found when !check_constant_struct ->
			remove_constant_flag t (function
				| true -> f loop
				| false -> raise Not_found)
	in
	let rec type_field_by_type_extension e t =
		if is_set then raise Not_found;
		let type_field_by_extension () = type_field_by_extension (fun loop ->
			let mt = try module_type_of_type t with Exit -> raise Not_found in
			loop (t_infos mt).mt_using
		) t e in
		match t with
		| TType (td,tl) ->
			(try
				type_field_by_extension()
			with Not_found ->
				type_field_by_typedef type_field_by_type_extension e td tl
			)
		| TMono _ -> raise Not_found
		| TAbstract (a,tl) ->
			(try
				type_field_by_extension()
			with Not_found ->
				type_field_by_forward_member type_field_by_type_extension e a tl
			)
		| _ -> type_field_by_extension()
	in
	let rec type_field_by_module_extension e t =
		if is_set then raise Not_found;
		let type_field_by_extension () = type_field_by_extension (fun loop ->
			try
				loop ctx.m.module_using
			with Not_found ->
				match loop ctx.g.global_using with
				| AKUsingField { se_access = { fa_host = FHStatic c } } as acc ->
					add_dependency ctx.m.curmod c.cl_module;
					acc
				| _ -> die "" __LOC__
		) t e in
		match t with
		| TType (td,tl) -> type_field_by_typedef type_field_by_module_extension e td tl
		| TMono r ->
			(match Monomorph.classify_down_constraints r with
			| CStructural (_,is_open) when not is_open -> type_field_by_extension()
			| _ -> raise Not_found
			)
		| TAbstract (a,tl) ->
			(try
				type_field_by_extension()
			with Not_found ->
				type_field_by_forward_member type_field_by_module_extension e a tl
			)
		| _ -> type_field_by_extension()
	in
	let rec type_field_by_fallback e t =
		match t with
		| TType (td,tl) -> type_field_by_typedef type_field_by_fallback e td tl
		| TInst (c,tl) ->
			(try
				let rec loop c tl = match c with
					| { cl_dynamic = Some t } -> AKExpr (mk (TField (e,FDynamic i)) (apply_params c.cl_params tl t) p)
					| { cl_super = Some (c,tl) } -> loop c tl
					| _ -> raise Not_found
				in
				loop c tl
			with Not_found when PMap.mem i c.cl_statics ->
				typing_error ("Cannot access static field " ^ i ^ " from a class instance") pfield;
			)
		| TDynamic t ->
			AKExpr (mk (TField (e,FDynamic i)) t p)
		| TAbstract (a,tl) ->
			(try
				if not (TypeFieldConfig.allow_resolve cfg) then raise Not_found;
				let c = find_some a.a_impl in
				let f = find_some (if is_set then a.a_write else a.a_read) in
				let sea = make_abstract_static_extension_access a tl c f e false p in
				AKResolve(sea,i)
			with Not_found -> try
				type_field_by_forward_member type_field_by_fallback e a tl
			with Not_found when not (has_class_field_flag (PMap.find i (find_some a.a_impl).cl_statics) CfImpl) ->
				typing_error ("Invalid call to static function " ^ i ^ " through abstract instance") pfield
			)
		| _ -> raise Not_found
	in
	let t = follow_without_type e.etype in
	try
		type_field_by_type e t
	with Not_found -> try
		type_field_by_type_extension e t
	with Not_found -> try
		type_field_by_module_extension e t
	with Not_found -> try
		type_field_by_fallback e t
	with Not_found when not (TypeFieldConfig.do_resume cfg) ->
		if not ctx.untyped then begin
			let has_special_field a =
				List.exists (fun (_,cf) -> cf.cf_name = i) a.a_ops
				|| List.exists (fun (_,_,cf) -> cf.cf_name = i) a.a_unops
				|| List.exists (fun cf -> cf.cf_name = i) a.a_array
			in
			match follow t with
			| TAnon { a_status = { contents = Statics { cl_kind = KAbstractImpl a } } }
			| TInst ({ cl_kind = KAbstractImpl a },_)
			| TAbstract (a,_) when has_special_field a ->
				(* the abstract field is not part of the field list, which is only true when it has no expression (issue #2344) *)
				display_error ctx.com ("Field " ^ i ^ " cannot be called directly because it has no expression") pfield;
			| TAnon { a_status = { contents = Statics c } } when PMap.mem i c.cl_fields ->
				display_error ctx.com ("Static access to instance field " ^ i ^ " is not allowed") pfield;
			| _ ->
				let tthis = e.etype in
				try
					if not (Diagnostics.error_in_diagnostics_run ctx.com pfield) then raise Exit;
					DisplayFields.handle_missing_field_raise ctx tthis i mode with_type pfield
				with Exit ->
					display_error ctx.com (StringError.string_error i (string_source tthis) (s_type (print_context()) tthis ^ " has no field " ^ i)) pfield
		end;
		AKExpr (mk (TField (e,FDynamic i)) (spawn_monomorph ctx p) p)

let type_field_default_cfg = type_field TypeFieldConfig.default

(**
	Generates a list of fields for `@:structInit` class `c` with type params `tl`
	as it's needed for anonymous object syntax.
*)
let get_struct_init_anon_fields c tl =
	let args =
		match c.cl_constructor with
		| Some cf ->
			let javadoc = match gen_doc_text_opt cf.cf_doc with
				| None -> None
				| Some s -> Some (new Javadoc.javadoc s)
			in
			let extract_param_info name = match javadoc with
				| Some javadoc -> javadoc#get_param_info name
				| None -> None
			in
			(match follow cf.cf_type with
			| TFun (args,_) ->
				Some (match cf.cf_expr with
					| Some { eexpr = TFunction fn } ->
						List.map (fun (name,_,t) ->
							let t = apply_params c.cl_params tl t in
							let p = try
								let v,_ = List.find (fun (v,_) -> v.v_name = name) fn.tf_args in
								v.v_pos
							with Not_found ->
								cf.cf_name_pos
							in
							name,t,p,extract_param_info name
						) args
					| _ ->
						List.map
							(fun (name,_,t) ->
								let t = apply_params c.cl_params tl t in
								try
									let cf = PMap.find name c.cl_fields in
									name,t,cf.cf_name_pos,gen_doc_text_opt cf.cf_doc
								with Not_found ->
									name,t,cf.cf_name_pos,extract_param_info name
							) args
				)
			| _ -> None
			)
		| _ -> None
	in
	match args with
	| Some args ->
		List.fold_left (fun fields (name,t,p,doc) ->
			let cf = mk_field name t p p in
			cf.cf_doc <- (doc_from_string_opt doc);
			PMap.add cf.cf_name cf fields
		) PMap.empty args
	| _ ->
		PMap.fold (fun cf fields ->
		match cf.cf_kind with
		| Var _ ->
			let cf = {cf with cf_type = apply_params c.cl_params tl cf.cf_type} in
			PMap.add cf.cf_name cf fields
		| _ ->
			fields
	) c.cl_fields PMap.empty