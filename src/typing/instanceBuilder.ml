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
		| _ -> raise_typing_error "Invalid macro call" p
	in
	let path = match e with
		| (EConst(Ident i)),_ ->
			let path = try
				if not (PMap.mem i ctx.c.curclass.cl_statics) then raise Not_found;
				ctx.c.curclass.cl_path
			with Not_found -> try
				(t_infos (let path,_,_ = PMap.find i (ctx.m.import_resolution#extract_field_imports) in path)).mt_path
			with Not_found ->
				raise_typing_error "Invalid macro call" p
			in
			i :: (snd path) :: (fst path)
		| _ ->
			loop e
	in
	(match path with
	| meth :: cl :: path -> (List.rev path,cl), meth, args
	| _ -> raise_typing_error "Invalid macro call" p)

let build_macro_type ctx pl p =
	let path, field, args = (match pl with
		| [TInst ({ cl_kind = KExpr (ECall (e,args),_) },_)]
		| [TInst ({ cl_kind = KExpr (EArrayDecl [ECall (e,args),_],_) },_)] ->
			get_macro_path ctx e args p
		| _ ->
			raise_typing_error "MacroType requires a single expression call parameter" p
	) in
	let old = ctx.e.ret in
	let t = (match ctx.g.do_macro ctx MMacroType path field args p with
		| MError | MMacroInMacro -> spawn_monomorph ctx.e p
		| MSuccess _ -> ctx.e.ret
	) in
	ctx.e.ret <- old;
	t

let build_macro_build ctx c pl cfl p =
	let path, field, args =
		let build_expr =
			try Meta.get Meta.GenericBuild c.cl_meta
			with Not_found -> raise_typing_error ((s_type_path c.cl_path) ^ " is missing @:genericBuild meta. Was it removed by a macro?") p
		in
		match build_expr with
		| _,[ECall(e,args),_],_ -> get_macro_path ctx e args p
		| _ -> raise_typing_error "genericBuild requires a single expression call parameter" p
	in
	let old = ctx.e.ret,ctx.c.get_build_infos in
	ctx.c.get_build_infos <- (fun() -> Some (TClassDecl c, pl, cfl));
	let t = (match ctx.g.do_macro ctx MMacroType path field args p with
		| MError | MMacroInMacro -> spawn_monomorph ctx.e p
		| MSuccess _ -> ctx.e.ret
	) in
	ctx.e.ret <- fst old;
	ctx.c.get_build_infos <- snd old;
	t

(* -------------------------------------------------------------------------- *)
(* API EVENTS *)

let get_build_info ctx mtype p =
	match mtype with
	| TClassDecl c ->
		if ctx.pass > PBuildClass then ignore(c.cl_build());
		let build f s tl =
			let t = spawn_monomorph ctx.e p in
			let r = make_lazy ctx t (fun r ->
				let tf = f tl in
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
		let kind,f = match c.cl_kind with
			| KGeneric ->
				(BuildGeneric c),build (fun tl -> Generic.build_generic_class ctx c p tl) "build_generic"
			| KGenericBuild cfl ->
				BuildGenericBuild,build (fun tl -> build_macro_build ctx c tl cfl p) "build_generic_build"
			| KMacroType ->
				BuildMacroType,build (fun tl -> build_macro_type ctx tl p) "build_macro_type"
			| _ ->
				BuildNormal,(fun tl -> TInst(c,tl))
		in
		make_build_info kind c.cl_path c.cl_params (has_class_flag c CExtern) f
	| TEnumDecl e ->
		make_build_info BuildNormal e.e_path e.e_params e.e_extern (fun t -> TEnum (e,t))
	| TTypeDecl td ->
		begin try
			let msg = match Meta.get Meta.Deprecated td.t_meta with
				| _,[EConst(String(s,_)),_],_ -> s
				| _ -> "This typedef is deprecated in favor of " ^ (s_type (print_context()) td.t_type)
			in
			DeprecationCheck.warn_deprecation (create_deprecation_context ctx) msg p
		with Not_found ->
				()
		end;
		make_build_info BuildNormal td.t_path td.t_params false (fun tl -> TType(td,tl))
	| TAbstractDecl a ->
		make_build_info BuildNormal a.a_path a.a_params false (fun tl -> TAbstract(a,tl))
