open Globals
open Common
open Ast
open Type
open TyperBase
open Error
open Typecore

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
	if has_class_flag c CAbstract then raise_error (Abstract_class (TClassDecl c)) p

let check_constructor_access ctx c f p =
	if (Meta.has Meta.CompilerGenerated f.cf_meta) then display_error ctx (error_msg (No_constructor (TClassDecl c))) p;
	if not (can_access ctx c f true || extends ctx.curclass c) && not ctx.untyped then display_error ctx (Printf.sprintf "Cannot access private constructor of %s" (s_class_path c)) p

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
					error ("Method " ^ f.cf_name ^ " cannot be used as a value") p
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

let check_field_access ctx c f stat p =
	if not ctx.untyped && not (can_access ctx c f stat) then
		display_error ctx ("Cannot access private field " ^ f.cf_name) p

let field_access ctx mode f famode e p =
	let is_set = match mode with MSet _ -> true | _ -> false in
	check_no_closure_meta ctx f famode mode p;
	let bypass_accessor = if ctx.bypass_accessor > 0 then (ctx.bypass_accessor <- ctx.bypass_accessor - 1; true) else false in
	let make_access inline = FieldAccess.create e f famode inline p in
	match f.cf_kind with
	| Method m ->
		let normal () = AKField(make_access false) in
		if is_set && m <> MethDynamic && not ctx.untyped then error "Cannot rebind this method : please use 'dynamic' before method declaration" p;
		let maybe_check_visibility c static =
			(* For overloads we have to resolve the actual field before we can check accessibility. *)
			begin match mode with
			| MCall _ when has_class_field_flag f CfOverload ->
				()
			| _ ->
				check_field_access ctx c f static p;
			end;
		in
		let default () =
			match m, mode with
			| MethInline, _ ->
				AKField (make_access true)
			| MethMacro, MGet ->
				display_error ctx "Macro functions must be called immediately" p; normal()
			| _ , MGet ->
				if has_class_field_flag f CfGeneric then display_error ctx "Cannot create closure on generic function" p;
				normal()
			| _ ->
				normal()
		in
		begin match famode with
		| FHInstance(c,tl) ->
			if e.eexpr = TConst TSuper then (match mode,f.cf_kind with
				| MGet,Var {v_read = AccCall }
				| MSet _,Var {v_write = AccCall }
				| MCall _,Var {v_read = AccCall } ->
					()
				| MCall _, Var _ ->
					display_error ctx "Cannot access superclass variable for calling: needs to be a proper method" p
				| MCall _, _ ->
					()
				| MGet,Var _
				| MSet _,Var _ when ctx.com.platform = Flash && has_class_flag c CExtern ->
					()
				| _, Method _ ->
					display_error ctx "Cannot create closure on super method" p
				| _ ->
					display_error ctx "Normal variables cannot be accessed with 'super', use 'this' instead" p);
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
			let sea = make_abstract_static_extension_access a tl c f e false p in
			AKUsingField sea
		end;
	| Var v ->
		let normal inline =
			AKField (make_access inline)
		in
		match (match mode with MGet | MCall _ -> v.v_read | MSet _ -> v.v_write) with
		| AccNo when not (Meta.has Meta.PrivateAccess ctx.meta) ->
			(match follow e.etype with
			| TInst (c,_) when extends ctx.curclass c || can_access ctx c { f with cf_flags = unset_flag f.cf_flags (int_of_class_field_flag CfPublic) } false -> normal false
			| TAnon a ->
				(match !(a.a_status) with
				| Statics c2 when ctx.curclass == c2 || can_access ctx c2 { f with cf_flags = unset_flag f.cf_flags (int_of_class_field_flag CfPublic) } true -> normal false
				| _ -> if ctx.untyped then normal false else AKNo f.cf_name)
			| _ ->
				if ctx.untyped then normal false else AKNo f.cf_name)
		| AccNormal | AccNo ->
			normal false
		| AccCall when ctx.in_display && DisplayPosition.display_position#enclosed_in p ->
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
				(match e.eexpr with TLocal _ when Common.defined ctx.com Define.Haxe3Compat -> ctx.com.warning "Field set has changed here in Haxe 4: call setter explicitly to keep Haxe 3.x behaviour" p | _ -> ());
				if not (is_physical_field f) then begin
					display_error ctx "This field cannot be accessed because it is not a real variable" p;
					display_error ctx "Add @:isVar here to enable it" f.cf_pos;
				end;
				normal false
			)
			else begin match famode with
			| FHAbstract(a,tl,c) ->
				let sea = make_abstract_static_extension_access a tl c f e false p in
				AKUsingAccessor sea
			| _ ->
				AKAccessor (make_access false)
			end
		| AccNever ->
			if ctx.untyped then normal false else AKNo f.cf_name
		| AccInline ->
			normal true
		| AccCtor ->
			(match ctx.curfun, famode with
				| FunConstructor, FHInstance(c,_) when c == ctx.curclass -> normal false
				| _ -> AKNo f.cf_name
			)
		| AccRequire (r,msg) ->
			match msg with
			| None -> error_require r p
			| Some msg -> error msg p

let class_field ctx c tl name p =
	raw_class_field (fun f -> field_type ctx c tl f p) c tl name

let rec using_field ctx mode e i p =
	let is_set = match mode with MSet _ -> true | _ -> false in
	if is_set then raise Not_found;
	(* do not try to find using fields if the type is a monomorph, which could lead to side-effects *)
	let t = follow e.etype in
	let is_dynamic = match t with
		| TMono {tm_constraints = []} -> raise Not_found
		| t -> t == t_dynamic
	in
	let check_constant_struct = ref false in
	let rec loop = function
	| [] ->
		raise Not_found
	| (c,pc) :: l ->
		try
			let cf = PMap.find i c.cl_statics in
			if Meta.has Meta.NoUsing cf.cf_meta || not (can_access ctx c cf true) || (has_class_field_flag cf CfImpl) then raise Not_found;
			let monos = Monomorph.spawn_constrained_monos (fun t -> t) cf.cf_params in
			let map = apply_params cf.cf_params monos in
			let t = map cf.cf_type in
			begin match follow t with
				| TFun((_,_,(TType({t_path = ["haxe";"macro"],"ExprOf"},[t0]) | t0)) :: args,r) ->
					if is_dynamic && follow t0 != t_dynamic then raise Not_found;
					let e = unify_static_extension ctx e t0 p in
					ImportHandling.mark_import_position ctx pc;
					AKUsingField (make_static_extension_access c cf e false p)
				| _ ->
					raise Not_found
			end
		with Not_found ->
			loop l
		| Unify_error el | Error (Unify el,_) ->
			if List.exists (function Has_extra_field _ -> true | _ -> false) el then check_constant_struct := true;
			loop l
	in
	try
		(* type using from `@:using(Path)` *)
		loop (t_infos (module_type_of_type t)).mt_using
	with Not_found | Exit -> try
		(* module using from `using Path` *)
		loop ctx.m.module_using
	with Not_found -> try
		(* global using *)
		let acc = loop ctx.g.global_using in
		(match acc with
		| AKUsingField {se_access = {fa_host = FHStatic c}} -> add_dependency ctx.m.curmod c.cl_module
		| _ -> die "" __LOC__);
		acc
	with Not_found ->
		if not !check_constant_struct then raise Not_found;
		remove_constant_flag e.etype (fun ok -> if ok then using_field ctx mode e i p else raise Not_found)

let emit_missing_field_error ctx i t pfield =
	display_error ctx (StringError.string_error i (string_source t) (s_type (print_context()) t ^ " has no field " ^ i)) pfield

let handle_missing_field ctx tthis i mode with_type pfield =
	try
		if not (Diagnostics.is_diagnostics_run ctx.com pfield) then raise Exit;
		DisplayFields.handle_missing_field_raise ctx tthis i mode with_type pfield
	with Exit ->
		emit_missing_field_error ctx i tthis pfield

(* Resolves field [i] on typed expression [e] using the given [mode]. *)
(* Note: if mode = MCall, with_type (if known) refers to the return type *)
let rec type_field cfg ctx e i p mode (with_type : WithType.t) =
	let pfield = if (e.epos = p) then p else {p with pmin = p.pmax - (String.length i)} in
	let is_set = match mode with MSet _ -> true | _ -> false in
	let no_field() =
		if TypeFieldConfig.do_resume cfg then raise Not_found;
		let t = match follow e.etype with
			| TAnon a -> (match !(a.a_status) with
				| Statics {cl_kind = KAbstractImpl a} -> TAbstract(a,[])
				| _ -> e.etype)
			| TInst({cl_kind = KAbstractImpl a},_) -> TAbstract(a,[])
			| _ -> e.etype
		in
		let has_special_field a =
			List.exists (fun (_,cf) -> cf.cf_name = i) a.a_ops
			|| List.exists (fun (_,_,cf) -> cf.cf_name = i) a.a_unops
			|| List.exists (fun cf -> cf.cf_name = i) a.a_array
		in
		if not ctx.untyped then begin
			match t with
			| TAbstract(a,_) when has_special_field a ->
				(* the abstract field is not part of the field list, which is only true when it has no expression (issue #2344) *)
				display_error ctx ("Field " ^ i ^ " cannot be called directly because it has no expression") pfield;
			| _ ->
				match follow t with
				| TAnon { a_status = { contents = Statics c } } when PMap.mem i c.cl_fields ->
					display_error ctx ("Static access to instance field " ^ i ^ " is not allowed") pfield;
				| _ ->
					handle_missing_field ctx e.etype i mode with_type pfield
		end;
		AKExpr (mk (TField (e,FDynamic i)) (spawn_monomorph ctx p) p)
	in
	let does_forward a stat =
		try
			let _,el,_ = Meta.get (if stat then Meta.ForwardStatics else Meta.Forward) a.a_meta in
			match el with
				| [] ->
					true
				| _ ->
					List.exists (fun e -> match fst e with
						| EConst(Ident s | String(s,_)) -> s = i
						| _ -> error "Identifier or string expected as argument to @:forward" (pos e)
					) el
		with Not_found ->
			false
	in
	match follow e.etype with
	| TInst (c,params) ->
		let rec loop_dyn c params =
			match c.cl_dynamic with
			| Some t ->
				let t = apply_params c.cl_params params t in
				AKExpr (mk (TField (e,FDynamic i)) t p)
			| None ->
				match c.cl_super with
				| None -> raise Not_found
				| Some (c,params) -> loop_dyn c params
		in
		(try
			let c2, t , f = class_field ctx c params i p in
			field_access ctx mode f (match c2 with None -> FHAnon | Some (c,tl) -> FHInstance (c,tl)) e p
		with Not_found -> try
			begin match e.eexpr with
				| TConst TSuper -> raise Not_found
				| _ -> using_field ctx mode e i p
			end
		with Not_found -> try
			loop_dyn c params
		with Not_found -> try
			(* if we have an abstract constraint we have to check its static fields and recurse (issue #2343) *)
			begin match c.cl_kind with
				| KTypeParameter tl ->
					let rec loop tl = match tl with
						| t :: tl ->
							begin match follow t with
								| TAbstract({a_impl = Some c},tl) when PMap.mem i c.cl_statics ->
									let e = mk_cast e t p in
									type_field cfg ctx e i p mode with_type;
								| _ ->
									loop tl
							end
						| [] ->
							raise Not_found
					in
					loop tl
				| _ ->
					raise Not_found
			end
		with Not_found ->
			if PMap.mem i c.cl_statics then error ("Cannot access static field " ^ i ^ " from a class instance") pfield;
			no_field())
	| TDynamic t ->
		(try
			using_field ctx mode e i p
		with Not_found ->
			AKExpr (mk (TField (e,FDynamic i)) t p))
	| TAnon a ->
		(try
			let f = PMap.find i a.a_fields in
			if has_class_field_flag f CfImpl && not (has_class_field_flag f CfEnum) then display_error ctx "Cannot access non-static abstract field statically" pfield;
			begin match mode with
			| MCall _ when has_class_field_flag f CfOverload ->
				()
			| _ ->
				if not (has_class_field_flag f CfPublic) && not ctx.untyped then begin
					match !(a.a_status) with
					| Closed | Extend _ -> () (* always allow anon private fields access *)
					| Statics c when can_access ctx c f true -> ()
					| _ -> display_error ctx ("Cannot access private field " ^ i) pfield
				end;
			end;
			let access fmode =
				field_access ctx mode f fmode e p
			in
			begin match !(a.a_status) with
				| Statics c ->
					access (FHStatic c)
				| EnumStatics en ->
					let c = (try PMap.find f.cf_name en.e_constrs with Not_found -> die "" __LOC__) in
					let fmode = FEnum (en,c) in
					let t = enum_field_type ctx en c p in
					AKExpr (mk (TField (e,fmode)) t p)
				| _ ->
					access FHAnon
			end
		with Not_found -> try
				match !(a.a_status) with
				| Statics {cl_kind = KAbstractImpl a} when does_forward a true ->
					let mt = try module_type_of_type a.a_this with Exit -> raise Not_found in
					let et = type_module_type ctx mt None p in
					type_field cfg ctx et i p mode with_type;
				| _ ->
					raise Not_found
			with Not_found ->
				try
					using_field ctx mode e i p
				with Not_found ->
					no_field()
		)
	| TMono r ->
		let mk_field () = {
			(mk_field i (mk_mono()) p null_pos) with
			cf_kind = Var { v_read = AccNormal; v_write = (match mode with MSet _ -> AccNormal | MGet | MCall _ -> AccNo) };
		} in
		let access f =
			field_access ctx mode f FHAnon e p
		in
		begin match Monomorph.classify_constraints r with
		| CStructural(fields,is_open) ->
			begin try
				let f = PMap.find i fields in
				if is_open && is_set then begin match f.cf_kind with
					(* We previously inferred to read-only, but now we want to write. This can happen in cases like #8079. *)
					| Var ({v_write = AccNo} as acc) -> f.cf_kind <- Var {acc with v_write = AccNormal}
					| _ -> ()
				end;
				access f
			with Not_found ->
				if not is_open then
					try
						using_field ctx mode e i p
					with Not_found ->
						no_field()
				else begin
					let f = mk_field() in
					Monomorph.add_constraint r (MField f);
					access f
				end
			end
		| CTypes tl ->
			let rec loop tl = match tl with
				| [] ->
					no_field()
				| (t,_) :: tl ->
					try
						type_field (TypeFieldConfig.with_resume cfg) ctx {e with etype = t} i p mode with_type
					with Not_found ->
						loop tl
			in
			loop tl
		| CUnknown ->
			if not (List.exists (fun (m,_) -> m == r) ctx.monomorphs.perfunction) && not (ctx.untyped && ctx.com.platform = Neko) then begin
				ctx.monomorphs.perfunction <- (r,p) :: ctx.monomorphs.perfunction;
			end;
			let f = mk_field() in
			Monomorph.add_constraint r (MField f);
			Monomorph.add_constraint r MOpenStructure;
			access f
		end
	| TAbstract (a,pl) ->
		let static_abstract_access_through_instance = ref false in
		(try
			let c = (match a.a_impl with None -> raise Not_found | Some c -> c) in
			let f = PMap.find i c.cl_statics in
			if not (has_class_field_flag f CfImpl) then begin
				static_abstract_access_through_instance := true;
				raise Not_found;
			end;
			field_access ctx mode f (FHAbstract(a,pl,c)) e p
		with Not_found -> try
			if does_forward a false then
				let underlying_type = Abstract.get_underlying_type ~return_first:true a pl in
				type_field (TypeFieldConfig.with_resume cfg) ctx {e with etype = underlying_type} i p mode with_type
			else
				raise Not_found
		with Not_found -> try
			using_field ctx mode e i p
		(* TODO: not sure what this is/was doing (see #9680) *)
		(* with Not_found -> try
			(match ctx.curfun, e.eexpr with
			| FunMemberAbstract, TConst (TThis) -> type_field cfg ctx {e with etype = apply_params a.a_params pl a.a_this} i p mode;
			| _ -> raise Not_found) *)
		with Not_found -> try
			let get_resolve is_write =
				let c,cf = match a.a_impl,(if is_write then a.a_write else a.a_read) with
					| Some c,Some cf -> c,cf
					| _ -> raise Not_found
				in
				let sea = make_abstract_static_extension_access a pl c cf e false p in
				AKResolve(sea,i)
			in
			if not (TypeFieldConfig.allow_resolve cfg) then raise Not_found;
			get_resolve (is_set)
		with Not_found ->
			if !static_abstract_access_through_instance then error ("Invalid call to static function " ^ i ^ " through abstract instance") pfield
			else no_field())
	| _ ->
		try using_field ctx mode e i p with Not_found -> no_field()

let type_field_default_cfg = type_field TypeFieldConfig.default

(**
	Generates a list of fields for `@:structInit` class `c` with type params `tl`
	as it's needed for anonymous object syntax.
*)
let get_struct_init_anon_fields c tl =
	let args =
		match c.cl_constructor with
		| Some cf ->
			(match follow cf.cf_type with
			| TFun (args,_) ->
				Some (match cf.cf_expr with
					| Some { eexpr = TFunction fn } ->
						List.map (fun (name,_,t) ->
							let t = apply_params c.cl_params tl t in
							try
								let v,_ = List.find (fun (v,_) -> v.v_name = name) fn.tf_args in
								name,t,v.v_pos
							with Not_found ->
								name,t,cf.cf_name_pos
						) args
					| _ ->
						List.map
							(fun (name,_,t) ->
								let t = apply_params c.cl_params tl t in
								try
									let cf = PMap.find name c.cl_fields in
									name,t,cf.cf_name_pos
								with Not_found ->
									name,t,cf.cf_name_pos
							) args
				)
			| _ -> None
			)
		| _ -> None
	in
	match args with
	| Some args ->
		List.fold_left (fun fields (name,t,p) ->
			let cf = mk_field name t p p in
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