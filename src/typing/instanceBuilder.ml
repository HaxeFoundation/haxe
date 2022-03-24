open Globals
open Ast
open Typecore
open Type
open Error

let get_macro_path ctx e args p =
	let rec loop e =
		match fst e with
		| EField (e,f,_) -> f :: loop e
		| EConst (Ident i) -> [i]
		| _ -> typing_error "Invalid macro call" p
	in
	let path = match e with
		| (EConst(Ident i)),_ ->
			let path = try
				if not (PMap.mem i ctx.curclass.cl_statics) then raise Not_found;
				ctx.curclass.cl_path
			with Not_found -> try
				(t_infos (let path,_,_ = PMap.find i ctx.m.module_globals in path)).mt_path
			with Not_found ->
				typing_error "Invalid macro call" p
			in
			i :: (snd path) :: (fst path)
		| _ ->
			loop e
	in
	(match path with
	| meth :: cl :: path -> (List.rev path,cl), meth, args
	| _ -> typing_error "Invalid macro call" p)

let build_macro_type ctx pl p =
	let path, field, args = (match pl with
		| [TInst ({ cl_kind = KExpr (ECall (e,args),_) },_)]
		| [TInst ({ cl_kind = KExpr (EArrayDecl [ECall (e,args),_],_) },_)] ->
			get_macro_path ctx e args p
		| _ ->
			typing_error "MacroType requires a single expression call parameter" p
	) in
	let old = ctx.ret in
	let t = (match ctx.g.do_macro ctx MMacroType path field args p with
		| None -> spawn_monomorph ctx p
		| Some _ -> ctx.ret
	) in
	ctx.ret <- old;
	t

let build_macro_build ctx c pl cfl p =
	let path, field, args =
		let build_expr =
			try Meta.get Meta.GenericBuild c.cl_meta
			with Not_found -> typing_error ((s_type_path c.cl_path) ^ " is missing @:genericBuild meta. Was it removed by a macro?") p
		in
		match build_expr with
		| _,[ECall(e,args),_],_ -> get_macro_path ctx e args p
		| _ -> typing_error "genericBuild requires a single expression call parameter" p
	in
	let old = ctx.ret,ctx.get_build_infos in
	ctx.get_build_infos <- (fun() -> Some (TClassDecl c, pl, cfl));
	let t = (match ctx.g.do_macro ctx MMacroType path field args p with
		| None -> spawn_monomorph ctx p
		| Some _ -> ctx.ret
	) in
	ctx.ret <- fst old;
	ctx.get_build_infos <- snd old;
	t

(* -------------------------------------------------------------------------- *)
(* API EVENTS *)

let build_instance ctx mtype p =
	match mtype with
	| TClassDecl c ->
		if ctx.pass > PBuildClass then ignore(c.cl_build());
		let build f s =
			let r = exc_protect ctx (fun r ->
				let t = spawn_monomorph ctx p in
				r := lazy_processing (fun() -> t);
				let tf = (f()) in
				unify_raise tf t p;
				link_dynamic t tf;
				(match tf with
					| TInst (c, _) -> ignore(c.cl_build())
					| TAbstract (a, _) -> Abstract.build_abstract a
					| _ -> ()
				);
				t
			) s in
			TLazy r
		in
		let ft = (fun pl ->
			match c.cl_kind with
			| KGeneric ->
				build (fun () -> Generic.build_generic ctx c p pl) "build_generic"
			| KMacroType ->
				build (fun () -> build_macro_type ctx pl p) "macro_type"
			| KGenericBuild cfl ->
				build (fun () -> build_macro_build ctx c pl cfl p) "generic_build"
			| _ ->
				TInst (c,pl)
		) in
		c.cl_params , c.cl_path , ft
	| TEnumDecl e ->
		e.e_params , e.e_path , (fun t -> TEnum (e,t))
	| TTypeDecl t ->
		t.t_params , t.t_path , (fun tl -> TType(t,tl))
	| TAbstractDecl a ->
		a.a_params, a.a_path, (fun tl -> TAbstract(a,tl))
