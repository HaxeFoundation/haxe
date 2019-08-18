open Globals
open Ast
open DisplayTypes
open DisplayMode
open CompletionResultKind
open CompletionItem
open CompletionModuleKind
open CompletionModuleType
open ClassFieldOrigin
open DisplayException
open DisplayEmitter
open Common
open Type
open Typecore
open TyperBase
open Fields
open Calls
open Error

let convert_function_signature ctx values (args,ret) = match DisplayEmitter.completion_type_of_type ctx ~values (TFun(args,ret)) with
	| CompletionType.CTFunction ctf -> ((args,ret),ctf)
	| _ -> assert false

let completion_item_of_expr ctx e =
	let retype e s t =
		try
			let e' = type_expr ctx (EConst(Ident s),null_pos) (WithType.with_type t) in
			Texpr.equal e e'
		with _ ->
			false
	in
	let tpair ?(values=PMap.empty) t =
		let ct = DisplayEmitter.completion_type_of_type ctx ~values t in
		(t,ct)
	in
	let of_field e origin cf scope make_ci =
		let is_qualified = retype e cf.cf_name e.etype in
		make_ci (CompletionClassField.make cf scope origin is_qualified) (tpair ~values:(get_value_meta cf.cf_meta) e.etype)
	in
	let of_enum_field e origin ef =
		let is_qualified = retype e ef.ef_name e.etype in
		make_ci_enum_field (CompletionEnumField.make ef origin is_qualified) (tpair e.etype)
	in
	let itexpr e =
		let t = tpair e.etype in
		make_ci_expr e t
	in
	let rec loop e = match e.eexpr with
		| TLocal v | TVar(v,_) -> make_ci_local v (tpair ~values:(get_value_meta v.v_meta) v.v_type)
		| TField(e1,FStatic(c,cf)) ->
			Display.merge_core_doc ctx (TClassDecl c);
			let decl = decl_of_class c in
			let origin = match c.cl_kind,e1.eexpr with
				| KAbstractImpl _,_ when Meta.has Meta.Impl cf.cf_meta -> Self decl
				| _,TMeta((Meta.StaticExtension,_,_),_) -> StaticExtension decl
				| _ -> Self decl
			in
			let make_ci = match c.cl_kind with
				| KAbstractImpl a when Meta.has Meta.Enum cf.cf_meta -> make_ci_enum_abstract_field a
				| _ -> make_ci_class_field
			in
			of_field e origin cf CFSStatic make_ci
		| TField(e1,(FInstance(c,_,cf) | FClosure(Some(c,_),cf))) ->
			Display.merge_core_doc ctx (TClassDecl c);
			let origin = match follow e1.etype with
			| TInst(c',_) when c != c' ->
				Parent (TClassDecl c)
			| _ ->
				Self (TClassDecl c)
			in
			of_field e origin cf CFSMember make_ci_class_field
		| TField(_,FEnum(en,ef)) -> of_enum_field e (Self (TEnumDecl en)) ef
		| TField(e1,(FAnon cf | FClosure(None,cf))) ->
			begin match follow e1.etype with
				| TAnon an ->
					let origin = match e1.etype with
						| TType(td,_) -> Self (TTypeDecl td)
						| _ -> AnonymousStructure an
					in
					of_field e origin cf CFSMember make_ci_class_field
				| _ -> itexpr e
			end
		| TTypeExpr (TClassDecl {cl_kind = KAbstractImpl a}) ->
			Display.merge_core_doc ctx (TAbstractDecl a);
			let t = TType(abstract_module_type a (List.map snd a.a_params),[]) in
			let t = tpair t in
			make_ci_type (CompletionModuleType.of_module_type (TAbstractDecl a)) ImportStatus.Imported (Some t)
		| TTypeExpr mt ->
			Display.merge_core_doc ctx mt;
			let t = tpair e.etype in
			make_ci_type (CompletionModuleType.of_module_type mt) ImportStatus.Imported (Some t) (* TODO *)
		| TConst (TThis | TSuper) -> itexpr e (* TODO *)
		| TConst(ct) -> make_ci_literal (s_const ct) (tpair e.etype)
		| TObjectDecl _ ->
			begin match follow e.etype with
				| TAnon an -> make_ci_anon an (tpair e.etype)
				| _ -> itexpr e
			end
		| TNew(c,tl,_) ->
			Display.merge_core_doc ctx (TClassDecl c);
			(* begin match fst e_ast with
			| EConst (Regexp (r,opt)) ->
				let present,absent = List.partition (String.contains opt) ['g';'i';'m';'s';'u'] in
				let doc flag desc = Printf.sprintf "* %s: %s" (String.make 1 flag) desc in
				let f c = match c with
					| 'g' -> doc c "global split and replace"
					| 'i' -> doc c "case insensitive matching"
					| 'm' -> doc c "multiline matching"
					| 's' -> doc c "dot also match newlines"
					| 'u' -> doc c "use UTF-8 matching"
					| _ -> assert false
				in
				let present = List.map f present in
				let present = match present with [] -> [] | _ -> "\n\nActive flags:\n\n" :: present in
				let absent = List.map f absent in
				let absent = match absent with [] -> [] | _ -> "\n\nInactive flags:\n\n" :: absent in
				(TInst(c,tl)),Some ("Regular expression\n\n" ^ (String.concat "\n" (present @ absent)))
			| _ -> *)
				let t,cf = get_constructor ctx c tl e.epos in
				let t = match follow t with
					| TFun(args,_) -> TFun(args,TInst(c,tl))
					| _ -> t
				in
				make_ci_class_field (CompletionClassField.make cf CFSConstructor (Self (decl_of_class c)) true) (tpair ~values:(get_value_meta cf.cf_meta) t)
			(* end *)
		| TCall({eexpr = TConst TSuper; etype = t} as e1,_) ->
			itexpr e1 (* TODO *)
		| TCast(e1,_) -> loop {e1 with etype = e.etype}
		| TParenthesis e1 | TMeta(_,e1) -> loop e1
		| _ -> itexpr e
	in
	loop e

let get_expected_type ctx with_type =
	let t = match with_type with
		| WithType.WithType(t,_) -> Some t
		| _ -> None
	in
	match t with
		| None -> None
		| Some t -> Some (completion_type_of_type ctx t,completion_type_of_type ctx (follow t))

let raise_toplevel ctx dk with_type (subject,psubject) po =
	let expected_type = get_expected_type ctx with_type in
	DisplayToplevel.collect_and_raise ctx (match dk with DKPattern _ -> TKPattern psubject | _ -> TKExpr psubject) with_type (CRToplevel expected_type) (subject,psubject) po

let display_dollar_type ctx p make_type =
	let mono = mk_mono() in
	let doc = Some "Outputs type of argument as a warning and uses argument as value" in
	let arg = ["expression",false,mono] in
	begin match ctx.com.display.dms_kind with
	| DMSignature ->
		raise_signatures [(convert_function_signature ctx PMap.empty (arg,mono),doc)] 0 0 SKCall
	| DMHover ->
		let t = TFun(arg,mono) in
		raise_hover (make_ci_expr (mk (TIdent "trace") t p) (make_type t)) (Some (WithType.named_argument "expression")) p
	| DMDefinition | DMTypeDefinition ->
		raise_positions []
	| _ ->
		error "Unsupported method" p
	end

let rec handle_signature_display ctx e_ast with_type =
	ctx.in_display <- true;
	let p = pos e_ast in
	let handle_call tl el p0 =
		let rec follow_with_callable (t,doc,values) = match follow t with
			| TAbstract(a,tl) when Meta.has Meta.Callable a.a_meta -> follow_with_callable (Abstract.get_underlying_type a tl,doc,values)
			| TFun(args,ret) -> ((args,ret),doc,values)
			| _ -> error ("Not a callable type: " ^ (s_type (print_context()) t)) p
		in
		let tl = List.map follow_with_callable tl in
		let rec loop i acc el = match el with
			| e :: el ->
				begin match fst e with
				| EDisplay(e1,DKMarked) -> i,List.rev (e1 :: acc) @ el
				| _ -> loop (i + 1) (e :: acc) el
				end
			| [] ->
				0,List.rev acc
		in
		let display_arg,el = loop 0 [] el in
		(* If our display position exceeds the argument number we add a null expression in order to make
		unify_call_args error out. *)
		let el = if el <> [] && display_arg >= List.length el then el @ [EConst (Ident "null"),null_pos] else el in
		let rec loop acc tl = match tl with
			| (t,doc,values) :: tl ->
				let keep (args,r) =
					begin try
						let _ = unify_call_args' ctx el args r p false false in
						true
					with
					| Error(Call_error (Not_enough_arguments _),_) -> true
					| _ -> false
					end
				in
				loop (if keep t then (t,doc,values) :: acc else acc) tl
			| [] ->
				acc
		in
		let overloads = match loop [] tl with [] -> tl | tl -> tl in
		let overloads = List.map (fun (t,doc,values) -> (convert_function_signature ctx values t,doc)) overloads in
		raise_signatures overloads 0 (* ? *) display_arg SKCall
	in
	let find_constructor_types t = match follow t with
		| TInst ({cl_kind = KTypeParameter tl} as c,_) ->
			let rec loop tl = match tl with
				| [] -> raise_error (No_constructor (TClassDecl c)) p
				| t :: tl -> match follow t with
					| TAbstract({a_path = ["haxe"],"Constructible"},[t]) -> t
					| _ -> loop tl
			in
			[loop tl,None,PMap.empty]
		| TInst (c,tl) | TAbstract({a_impl = Some c},tl) ->
			Display.merge_core_doc ctx (TClassDecl c);
			let ct,cf = get_constructor ctx c tl p in
			let tl = (ct,cf.cf_doc,get_value_meta cf.cf_meta) :: List.rev_map (fun cf' -> cf'.cf_type,cf.cf_doc,get_value_meta cf'.cf_meta) cf.cf_overloads in
			tl
		| _ ->
			[]
	in
	match fst e_ast with
		| ECall(e1,el) ->
			let def () =
				try
					acc_get ctx (!type_call_target_ref ctx e1 with_type false (pos e1)) (pos e1)
				with
				| Error (Unknown_ident "trace",_) ->
					let e = expr_of_type_path (["haxe";"Log"],"trace") p in
					type_expr ctx e WithType.value
				| Error (Unknown_ident "$type",p) ->
					display_dollar_type ctx p (fun t -> t,(DisplayEmitter.completion_type_of_type ctx t))
			in
			let e1 = match e1 with
				| (EField (e,"bind"),p) ->
					let e = type_expr ctx e WithType.value in
					(match follow e.etype with
						| TFun signature -> e
						| _ -> def ())
				| _ ->	def()
			in
			let tl = match e1.eexpr with
				| TField(_,fa) ->
					begin match extract_field fa with
						| Some cf -> (e1.etype,cf.cf_doc,get_value_meta cf.cf_meta) :: List.rev_map (fun cf' -> cf'.cf_type,cf.cf_doc,get_value_meta cf'.cf_meta) cf.cf_overloads
						| None -> [e1.etype,None,PMap.empty]
					end
				| TConst TSuper ->
					find_constructor_types e1.etype
				| TLocal v ->
					[e1.etype,None,get_value_meta v.v_meta]
				| _ -> [e1.etype,None,PMap.empty]
			in
			handle_call tl el e1.epos
		| ENew(tpath,el) ->
			let t = Typeload.load_instance ctx tpath true in
			handle_call (find_constructor_types t) el (pos tpath)
		| EArray(e1,e2) ->
			let e1 = type_expr ctx e1 WithType.value in
			begin match follow e1.etype with
			| TInst({cl_path=([],"Array")},[t]) ->
				let res = convert_function_signature ctx PMap.empty (["index",false,ctx.t.tint],t) in
				raise_signatures [res,Some "The array index"] 0 0 SKCall
			| TAbstract(a,tl) ->
				(match a.a_impl with Some c -> ignore(c.cl_build()) | _ -> ());
				let sigs = ExtList.List.filter_map (fun cf -> match follow cf.cf_type with
					| TFun(_ :: args,r) ->
						if ExtType.is_void (follow r) && (match with_type with WithType.NoValue -> false | _ -> true) then
							None
						else begin
							let map = apply_params a.a_params tl in
							let tl = List.map (fun (n,o,t) -> n,o,map t) args in
							let r = map r in
							Some (convert_function_signature ctx PMap.empty (tl,r),cf.cf_doc)
						end
					| _ ->
						None
				) a.a_array in
				raise_signatures sigs 0 0 SKArrayAccess
			| _ ->
				raise_signatures [] 0 0 SKArrayAccess
			end
		| _ -> error "Call expected" p

and display_expr ctx e_ast e dk with_type p =
	let get_super_constructor () = match ctx.curclass.cl_super with
		| None -> error "Current class does not have a super" p
		| Some (c,params) ->
			let _, f = get_constructor ctx c params p in
			f
	in
	match ctx.com.display.dms_kind with
	| DMResolve _ | DMPackage ->
		assert false
	| DMSignature ->
		handle_signature_display ctx e_ast with_type
	| DMHover ->
		let item = completion_item_of_expr ctx e in
		raise_hover item (Some with_type) e.epos
	| DMUsage _ ->
		let rec loop e = match e.eexpr with
		| TField(_,FEnum(_,ef)) ->
			Display.ReferencePosition.set (ef.ef_name,ef.ef_name_pos,KEnumField);
		| TField(_,(FAnon cf | FInstance (_,_,cf) | FStatic (_,cf) | FClosure (_,cf))) ->
			Display.ReferencePosition.set (cf.cf_name,cf.cf_name_pos,KClassField);
		| TLocal v | TVar(v,_) ->
			Display.ReferencePosition.set (v.v_name,v.v_pos,KVar);
		| TTypeExpr mt ->
			let ti = t_infos mt in
			Display.ReferencePosition.set (snd ti.mt_path,ti.mt_name_pos,KModuleType);
		| TNew(c,tl,_) ->
			begin try
				let _,cf = get_constructor ctx c tl p in
				Display.ReferencePosition.set (snd c.cl_path,cf.cf_name_pos,KConstructor);
			with Not_found ->
				()
			end
		| TCall({eexpr = TConst TSuper},_) ->
			begin try
				let cf = get_super_constructor() in
				Display.ReferencePosition.set (cf.cf_name,cf.cf_name_pos,KClassField);
			with Not_found ->
				()
			end
		| TConst TSuper ->
			begin match ctx.curclass.cl_super with
				| None -> ()
				| Some (c,_) -> Display.ReferencePosition.set (snd c.cl_path,c.cl_name_pos,KModuleType);
			end
		| TCall(e1,_) ->
			loop e1
		| _ ->
			()
		in
		loop e;
		e
	| DMDefinition ->
		let rec loop e = match e.eexpr with
		| TField(_,FEnum(_,ef)) -> [ef.ef_name_pos]
		| TField(_,(FStatic (c,cf))) when Meta.has Meta.CoreApi c.cl_meta ->
			let c' = ctx.g.do_load_core_class ctx c in
			cf.cf_name_pos :: (try [(PMap.find cf.cf_name c'.cl_statics).cf_name_pos] with Not_found -> [])
		| TField(_,(FInstance (c,tl,cf) | FClosure (Some(c,tl),cf))) when Meta.has Meta.CoreApi c.cl_meta ->
			let c' = ctx.g.do_load_core_class ctx c in
			let l = try
				let _,_,cf = Type.class_field c' tl cf.cf_name in
				[cf.cf_name_pos]
			with Not_found ->
				[]
			in
			cf.cf_name_pos :: l
		| TField(_,(FAnon cf | FInstance (_,_,cf) | FStatic (_,cf) | FClosure (_,cf))) -> [cf.cf_name_pos]
		| TLocal v | TVar(v,_) -> [v.v_pos]
		| TTypeExpr (TClassDecl c) when Meta.has Meta.CoreApi c.cl_meta ->
			let c' = ctx.g.do_load_core_class ctx c in
			[c.cl_name_pos;c'.cl_name_pos]
		| TTypeExpr mt -> [(t_infos mt).mt_name_pos]
		| TNew(c,tl,_) ->
			begin try
				let _,cf = get_constructor ctx c tl p in
				if Meta.has Meta.CoreApi c.cl_meta then begin
					let c' = ctx.g.do_load_core_class ctx c in
					begin match c'.cl_constructor with
					| Some cf' -> [cf.cf_name_pos;cf'.cf_name_pos]
					| None -> [cf.cf_name_pos]
					end
				end else
					[cf.cf_name_pos]
			with Not_found ->
				[]
			end
		| TCall({eexpr = TConst TSuper},_) ->
			begin try
				let cf = get_super_constructor() in
				[cf.cf_name_pos]
			with Not_found ->
				[]
			end
		| TConst TSuper ->
			begin match ctx.curclass.cl_super with
				| None -> []
				| Some (c,_) -> [c.cl_name_pos]
			end
		| TCall(e1,_) ->
			loop e1
		| _ ->
			[]
		in
		let pl = loop e in
		raise_positions pl
	| DMTypeDefinition ->
		raise_position_of_type e.etype
	| DMDefault when not (!Parser.had_resume)->
		let display_fields e_ast e1 l =
			let fields = DisplayFields.collect ctx e_ast e1 dk with_type p in
			let item = completion_item_of_expr ctx e1 in
			raise_fields fields (CRField(item,e1.epos,None,None)) (Some {e.epos with pmin = e.epos.pmax - l;})
		in
		begin match fst e_ast,e.eexpr with
			| EField(e1,s),TField(e2,_) ->
				display_fields e1 e2 (String.length s)
			| _ ->
				if dk = DKDot then display_fields e_ast e 0
				else begin
					let name = try String.concat "." (string_list_of_expr_path_raise e_ast) with Exit -> "" in
					let name = if name = "null" then "" else name in
					raise_toplevel ctx dk with_type (name,pos e_ast) None
				end
		end
	| DMDefault | DMNone | DMModuleSymbols _ | DMDiagnostics _ | DMStatistics ->
		let fields = DisplayFields.collect ctx e_ast e dk with_type p in
		let item = completion_item_of_expr ctx e in
		let iterator = try
			let it = (ForLoop.IterationKind.of_texpr ~resume:true ctx e (fun _ -> false) e.epos) in
			match follow it.it_type with
				| TDynamic _ ->  None
				| t -> Some t
			with Error _ | Not_found ->
				None
		in
		let keyValueIterator =
			try begin
				let _,pt = ForLoop.IterationKind.check_iterator ~resume:true ctx "keyValueIterator" e e.epos in
				match follow pt with
					| TAnon a ->
						let key = PMap.find "key" a.a_fields in
						let value = PMap.find "value" a.a_fields in
						Some (key.cf_type,value.cf_type)
					| _ ->
						None
			end with Error _ | Not_found ->
				None
		in
		raise_fields fields (CRField(item,e.epos,iterator,keyValueIterator)) None

let handle_structure_display ctx e fields origin =
	let p = pos e in
	let fields = PMap.foldi (fun _ cf acc -> cf :: acc) fields [] in
	let fields = List.sort (fun cf1 cf2 -> -compare cf1.cf_pos.pmin cf2.cf_pos.pmin) fields in
	let tpair ?(values=PMap.empty) t =
		let ct = DisplayEmitter.completion_type_of_type ctx ~values t in
		(t,ct)
	in
	match fst e with
	| EObjectDecl fl ->
		let fields = List.fold_left (fun acc cf ->
			if Expr.field_mem_assoc cf.cf_name fl then acc
			else (make_ci_class_field (CompletionClassField.make cf CFSMember origin true) (tpair ~values:(get_value_meta cf.cf_meta) cf.cf_type)) :: acc
		) [] fields in
		raise_fields fields CRStructureField None
	| EBlock [] ->
		let fields = List.fold_left (fun acc cf ->
			make_ci_class_field (CompletionClassField.make cf CFSMember origin true) (tpair ~values:(get_value_meta cf.cf_meta) cf.cf_type) :: acc
		) [] fields in
		raise_fields fields CRStructureField None
	| _ ->
		error "Expected object expression" p

let handle_display ?resume_typing ctx e_ast dk with_type =
	let old = ctx.in_display,ctx.in_call_args in
	ctx.in_display <- true;
	ctx.in_call_args <- false;
	let tpair t =
		let ct = DisplayEmitter.completion_type_of_type ctx t in
		(t,ct)
	in
	let e = match e_ast,with_type with
	| (EConst (Ident "$type"),p),_ ->
		display_dollar_type ctx p tpair
	| (EConst (Ident "trace"),_),_ ->
		let doc = Some "Print given arguments" in
		let arg = ["value",false,t_dynamic] in
		let ret = ctx.com.basic.tvoid in
		let p = pos e_ast in
		begin match ctx.com.display.dms_kind with
		| DMSignature ->
			raise_signatures [(convert_function_signature ctx PMap.empty (arg,ret),doc)] 0 0 SKCall
		| DMHover ->
			let t = TFun(arg,ret) in
			raise_hover (make_ci_expr (mk (TIdent "trace") t p) (tpair t)) (Some (WithType.named_argument "value")) p
		| DMDefinition | DMTypeDefinition ->
			raise_positions []
		| _ ->
			error "Unsupported method" p
		end
	| (EConst (Ident "_"),p),WithType.WithType(t,_) ->
		mk (TConst TNull) t p (* This is "probably" a bind skip, let's just use the expected type *)
	| (_,p),_ -> try
		match resume_typing with
		| None -> type_expr ctx e_ast with_type
		| Some fn -> fn ctx e_ast with_type
	with Error (Unknown_ident n,_) when ctx.com.display.dms_kind = DMDefault ->
        if dk = DKDot && ctx.com.json_out = None then raise (Parser.TypePath ([n],None,false,p))
		else raise_toplevel ctx dk with_type (n,p) (Some p)
	| Error ((Type_not_found (path,_) | Module_not_found path),_) as err when ctx.com.display.dms_kind = DMDefault ->
		if ctx.com.json_out = None then	begin try
			raise_fields (DisplayFields.get_submodule_fields ctx path) (CRField((make_ci_module path),p,None,None)) None
		with Not_found ->
			raise err
		end else
			raise_toplevel ctx dk with_type (s_type_path path,p) (Some p)
	| DisplayException(DisplayFields Some({fkind = CRTypeHint} as r)) when (match fst e_ast with ENew _ -> true | _ -> false) ->
		let timer = Timer.timer ["display";"toplevel";"filter ctors"] in
		ctx.pass <- PBuildClass;
		let l = List.filter (fun item ->
			let is_private_to_current_module mt =
				(* Remove the _Module nonsense from the package *)
				let pack = List.rev (List.tl (List.rev mt.pack)) in
				(pack,mt.module_name) = ctx.m.curmod.m_path
			in
			match item.ci_kind with
			| ITType({kind = (Class | Abstract | TypeAlias)} as mt,_) when not mt.is_private || is_private_to_current_module mt ->
				begin match mt.has_constructor with
				| Yes -> true
				| YesButPrivate ->
					if (Meta.has Meta.PrivateAccess ctx.meta) then true
					else begin
						let path = (mt.pack,mt.name) in
						let rec loop c =
							if c.cl_path = path then true
							else match c.cl_super with
								| Some(c,_) -> loop c
								| None -> false
						in
						loop ctx.curclass
					end
				| No -> false
				| Maybe ->
					begin try
						let mt = ctx.g.do_load_type_def ctx null_pos {tpackage=mt.pack;tname=mt.module_name;tsub=Some mt.name;tparams=[]} in
						begin match resolve_typedef mt with
						| TClassDecl c when has_constructor c -> true
						| TAbstractDecl {a_impl = Some c} ->
							ignore(c.cl_build());
							PMap.mem "_new" c.cl_statics
						| _ -> false
						end
					with _ ->
						false
					end
				end
			| ITTypeParameter {cl_kind = KTypeParameter tl} when get_constructible_constraint ctx tl null_pos <> None ->
				true
			| _ -> false
		) r.fitems in
		timer();
		raise_fields l CRNew r.finsert_pos
	in
	let e = match e.eexpr with
		| TField(e1,FDynamic "bind") when (match follow e1.etype with TFun _ -> true | _ -> false) -> e1
		| _ -> e
	in
	let is_display_debug = Meta.has (Meta.Custom ":debug.display") ctx.curfield.cf_meta in
	if is_display_debug then begin
		print_endline (Printf.sprintf "expected type: %s" (WithType.to_string with_type));
		print_endline (Printf.sprintf "typed expr:\n%s" (s_expr_ast true "" (s_type (print_context())) e));
	end;
	let p = e.epos in
	begin match with_type with
		| WithType.WithType(t,_) ->
			(* We don't want to actually use the transformed expression which may have inserted implicit cast calls.
			   It only matters that unification takes place. *)
			(try ignore(AbstractCast.cast_or_unify_raise ctx t e e.epos) with Error (Unify l,p) -> ());
		| _ ->
			()
	end;
	if is_display_debug then begin
		print_endline (Printf.sprintf "cast expr:\n%s" (s_expr_ast true "" (s_type (print_context())) e));
	end;
	ctx.in_display <- fst old;
	ctx.in_call_args <- snd old;
	display_expr ctx e_ast e dk with_type p

let handle_edisplay ?resume_typing ctx e dk with_type =
	let handle_display ctx e dk with_type =
		match resume_typing with
		| Some resume_typing -> handle_display ~resume_typing ctx e dk with_type
		| None -> handle_display ctx e dk with_type
	in
	match dk,ctx.com.display.dms_kind with
	| DKCall,(DMSignature | DMDefault) -> handle_signature_display ctx e with_type
	| DKStructure,DMDefault ->
		begin match with_type with
			| WithType.WithType(t,_) ->
				begin match follow t with
					| TAnon an ->
						let origin = match t with
							| TType(td,_) -> Self (TTypeDecl td)
							| _ -> AnonymousStructure an
						in
						handle_structure_display ctx e an.a_fields origin
					| TInst(c,tl) when Meta.has Meta.StructInit c.cl_meta ->
						let fields = PMap.map (fun cf -> {cf with cf_type = apply_params c.cl_params tl cf.cf_type}) c.cl_fields in
						handle_structure_display ctx e fields (Self (TClassDecl c))
					| _ -> handle_display ctx e dk with_type
				end
			| _ ->
				handle_display ctx e dk with_type
		end
	| DKPattern outermost,DMDefault ->
		begin try
			handle_display ctx e dk with_type
		with DisplayException(DisplayFields Some({fkind = CRToplevel _} as r)) ->
			raise_fields r.fitems (CRPattern ((get_expected_type ctx with_type),outermost)) r.finsert_pos
		end
	| _ -> handle_display ctx e dk with_type