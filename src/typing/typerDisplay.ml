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
open Common
open Type
open Typecore
open TyperBase
open Fields
open Calls
open Error

let completion_item_of_expr ctx e =
	let retype e s t =
		try
			let e' = type_expr ctx (EConst(Ident s),null_pos) (WithType t) in
			Texpr.equal e e'
		with _ ->
			false
	in
	let of_field e origin cf scope =
		let is_qualified = retype e cf.cf_name e.etype in
		make_ci_class_field (CompletionClassField.make cf scope origin is_qualified) (DisplayEmitter.patch_type ctx e.etype)
	in
	let of_enum_field e origin ef =
		let is_qualified = retype e ef.ef_name e.etype in
		make_ci_enum_field (CompletionEnumField.make ef origin is_qualified) (DisplayEmitter.patch_type ctx e.etype)
	in
	let itexpr e =
		let t = DisplayEmitter.patch_type ctx e.etype in
		make_ci_expr {e with etype = t}
	in
	let rec loop e = match e.eexpr with
		| TLocal v | TVar(v,_) -> make_ci_local v (DisplayEmitter.patch_type ctx v.v_type)
		| TField(_,FStatic(c,cf)) -> of_field e (Self (TClassDecl c)) cf CFSStatic
		| TField(_,(FInstance(c,_,cf) | FClosure(Some(c,_),cf))) -> of_field e (Self (TClassDecl c)) cf CFSMember
		| TField(_,FEnum(en,ef)) -> of_enum_field e (Self (TEnumDecl en)) ef
		| TField(e1,FAnon cf) ->
			begin match follow e1.etype with
				| TAnon an ->
					let origin = match e1.etype with
						| TType(td,_) -> Self (TTypeDecl td)
						| _ -> AnonymousStructure an
					in
					of_field e origin cf CFSMember
				| _ -> itexpr e
			end
		| TTypeExpr (TClassDecl {cl_kind = KAbstractImpl a}) ->
			let t = TType(abstract_module_type a (List.map snd a.a_params),[]) in
			let t = DisplayEmitter.patch_type ctx t in
			make_ci_type (CompletionModuleType.of_module_type (TAbstractDecl a)) ImportStatus.Imported (Some t)
		| TTypeExpr mt ->
			let t = DisplayEmitter.patch_type ctx e.etype in
			make_ci_type (CompletionModuleType.of_module_type mt) ImportStatus.Imported (Some t) (* TODO *)
		| TConst (TThis | TSuper) -> itexpr e (* TODO *)
		| TConst(ct) -> make_ci_literal (s_const ct) e.etype
		| TObjectDecl _ ->
			begin match follow e.etype with
				| TAnon an -> make_ci_anon an e.etype
				| _ -> itexpr e
			end
		| TNew(c,tl,_) ->
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
				make_ci_class_field (CompletionClassField.make {cf with cf_type = t} CFSConstructor (Self (TClassDecl c)) true) (DisplayEmitter.patch_type ctx t)
			(* end *)
		| TCall({eexpr = TConst TSuper; etype = t} as e1,_) ->
			itexpr e1 (* TODO *)
		| TParenthesis e1 | TMeta(_,e1) | TCast(e1,_) -> loop e1
		| _ -> itexpr e
	in
	loop e

let raise_toplevel ctx with_type po p =
	let t = match with_type with
		| WithType t -> Some t
		| _ -> None
	in
	raise_fields (DisplayToplevel.collect ctx (Some p) with_type) (CRToplevel t) po

let rec handle_signature_display ctx e_ast with_type =
	ctx.in_display <- true;
	let p = pos e_ast in
	let handle_call tl el p0 =
		let rec follow_with_callable (t,doc) = match follow t with
			| TAbstract(a,tl) when Meta.has Meta.Callable a.a_meta -> follow_with_callable (Abstract.get_underlying_type a tl,doc)
			| TFun(args,ret) -> ((args,ret),doc)
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
		let el = if display_arg >= List.length el then el @ [EConst (Ident "null"),null_pos] else el in
		let rec loop acc tl = match tl with
			| (t,doc) :: tl ->
				let keep (args,r) =
					begin try
						let _ = unify_call_args' ctx el args r p false false in
						true
					with
					| Error(Call_error (Not_enough_arguments _),_) -> true
					| _ -> false
					end
				in
				loop (if keep t then (t,doc) :: acc else acc) tl
			| [] ->
				acc
		in
		let overloads = match loop [] tl with [] -> tl | tl -> tl in
		raise_signatures overloads 0 (* ? *) display_arg
	in
	let find_constructor_types t = match follow t with
		| TInst ({cl_kind = KTypeParameter tl} as c,_) ->
			let rec loop tl = match tl with
				| [] -> raise_error (No_constructor (TClassDecl c)) p
				| t :: tl -> match follow t with
					| TAbstract({a_path = ["haxe"],"Constructible"},[t]) -> t
					| _ -> loop tl
			in
			[loop tl,None]
		| TInst (c,tl) | TAbstract({a_impl = Some c},tl) ->
			let ct,cf = get_constructor ctx c tl p in
			let tl = (ct,cf.cf_doc) :: List.rev_map (fun cf' -> cf'.cf_type,cf.cf_doc) cf.cf_overloads in
			tl
		| _ ->
			[]
	in
	match fst e_ast with
		| ECall(e1,el) ->
			let def () = try
				type_expr ctx e1 Value
			with Error (Unknown_ident "trace",_) ->
				let e = expr_of_type_path (["haxe";"Log"],"trace") p in
				type_expr ctx e Value
			in
			let e1 = match e1 with
				| (EField (e,"bind"),p) ->
					let e = type_expr ctx e Value in
					(match follow e.etype with
						| TFun signature -> e
						| _ -> def ())
				| _ ->	def()
			in
			let tl = match e1.eexpr with
				| TField(_,fa) ->
					begin match extract_field fa with
						| Some cf -> (e1.etype,cf.cf_doc) :: List.rev_map (fun cf' -> cf'.cf_type,cf.cf_doc) cf.cf_overloads
						| None -> [e1.etype,None]
					end
				| TConst TSuper ->
					find_constructor_types e1.etype
				| _ -> [e1.etype,None]
			in
			handle_call tl el e1.epos
		| ENew(tpath,el) ->
			let t = Typeload.load_instance ctx tpath true p in
			handle_call (find_constructor_types t) el (pos tpath)
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
		raise_hover item e.epos
	| DMUsage _ ->
		let rec loop e = match e.eexpr with
		| TField(_,FEnum(_,ef)) ->
			Display.reference_position := ef.ef_name_pos;
		| TField(_,(FAnon cf | FInstance (_,_,cf) | FStatic (_,cf) | FClosure (_,cf))) ->
			Display.reference_position := cf.cf_name_pos;
		| TLocal v | TVar(v,_) ->
			Display.reference_position := v.v_pos;
		| TTypeExpr mt ->
			let ti = t_infos mt in
			Display.reference_position := ti.mt_name_pos;
		| TNew(c,tl,_) ->
			begin try
				let _,cf = get_constructor ctx c tl p in
				Display.reference_position := cf.cf_name_pos;
			with Not_found ->
				()
			end
		| TCall({eexpr = TConst TSuper},_) ->
			begin try
				let cf = get_super_constructor() in
				Display.reference_position := cf.cf_name_pos;
			with Not_found ->
				()
			end
		| TConst TSuper ->
			begin match ctx.curclass.cl_super with
				| None -> ()
				| Some (c,_) -> Display.reference_position := c.cl_name_pos;
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
		| TField(_,(FAnon cf | FInstance (_,_,cf) | FStatic (_,cf) | FClosure (_,cf))) -> [cf.cf_name_pos]
		| TLocal v | TVar(v,_) -> [v.v_pos]
		| TTypeExpr mt -> [(t_infos mt).mt_name_pos]
		| TNew(c,tl,_) ->
			begin try
				let _,cf = get_constructor ctx c tl p in
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
		raise_position pl
	| DMDefault when not (!Parser.had_resume)->
		begin match fst e_ast,e.eexpr with
			| EField(e1,s),TField(e2,_) ->
				let fields = DisplayFields.collect ctx e1 e2 dk with_type p in
				let item = completion_item_of_expr ctx e2 in
				raise_fields fields (CRField(item,e2.epos)) (Some {e.epos with pmin = e.epos.pmax - String.length s;})
			| _ ->
				raise_toplevel ctx with_type None p
		end
	| DMDefault | DMNone | DMModuleSymbols _ | DMDiagnostics _ | DMStatistics ->
		let fields = DisplayFields.collect ctx e_ast e dk with_type p in
		let item = completion_item_of_expr ctx e in
		raise_fields fields (CRField(item,e.epos)) None

let handle_structure_display ctx e t an =
	let p = pos e in
	let fields = PMap.foldi (fun _ cf acc -> cf :: acc) an.a_fields [] in
	let fields = List.sort (fun cf1 cf2 -> -compare cf1.cf_pos.pmin cf2.cf_pos.pmin) fields in
	let origin = match t with
		| TType(td,_) -> Self (TTypeDecl td)
		| _ -> AnonymousStructure an
	in
	match fst e with
	| EObjectDecl fl ->
		let fields = List.fold_left (fun acc cf ->
			if Expr.field_mem_assoc cf.cf_name fl then acc
			else (make_ci_class_field (CompletionClassField.make cf CFSMember origin true) cf.cf_type) :: acc
		) [] fields in
		raise_fields fields CRStructureField None
	| EBlock [] ->
		let fields = List.fold_left (fun acc cf ->
			make_ci_class_field (CompletionClassField.make cf CFSMember origin true) cf.cf_type :: acc
		) [] fields in
		raise_fields fields CRStructureField None
	| _ ->
		error "Expected object expression" p

let handle_display ctx e_ast dk with_type =
	let old = ctx.in_display,ctx.in_call_args in
	ctx.in_display <- true;
	ctx.in_call_args <- false;
	let e = match e_ast,with_type with
	| (EConst (Ident "$type"),_),_ ->
		let mono = mk_mono() in
		let doc = Some "Outputs type of argument as a warning and uses argument as value" in
		let arg = ["expression",false,mono] in
		begin match ctx.com.display.dms_kind with
		| DMSignature ->
			raise_signatures [((arg,mono),doc)] 0 0
		| _ ->
			let t = TFun(arg,mono) in
			raise_hover (make_ci_expr (mk (TIdent "trace") t (pos e_ast))) (pos e_ast);
		end
	| (EConst (Ident "trace"),_),_ ->
		let doc = Some "Print given arguments" in
		let arg = ["value",false,t_dynamic] in
		let ret = ctx.com.basic.tvoid in
		begin match ctx.com.display.dms_kind with
		| DMSignature ->
			raise_signatures [((arg,ret),doc)] 0 0
		| _ ->
			let t = TFun(arg,ret) in
			raise_hover (make_ci_expr (mk (TIdent "trace") t (pos e_ast))) (pos e_ast);
		end
	| (EConst (Ident "_"),p),WithType t ->
		mk (TConst TNull) t p (* This is "probably" a bind skip, let's just use the expected type *)
	| (_,p),_ -> try
		type_expr ctx e_ast with_type
	with Error (Unknown_ident n,_) when ctx.com.display.dms_kind = DMDefault ->
        if dk = DKDot && ctx.com.json_out = None then raise (Parser.TypePath ([n],None,false,p))
		else raise_toplevel ctx with_type (Some (Parser.cut_pos_at_display p)) p
	| Error ((Type_not_found (path,_) | Module_not_found path),_) as err when ctx.com.display.dms_kind = DMDefault ->
		if ctx.com.json_out = None then	begin try
			let s = s_type_path path in
			raise_fields (DisplayFields.get_submodule_fields ctx path) (CRField((make_ci_module s),p)) None
		with Not_found ->
			raise err
		end else
			raise_toplevel ctx with_type (Some (Parser.cut_pos_at_display p)) p
	| DisplayException(DisplayFields(l,CRTypeHint,p)) when (match fst e_ast with ENew _ -> true | _ -> false) ->
		let timer = Timer.timer ["display";"toplevel";"filter ctors"] in
		ctx.pass <- PBuildClass;
		let l = List.filter (fun item -> match item.ci_kind with
			| ITType({kind = (Class | Abstract)} as mt,_) when not mt.is_private ->
				begin match mt.has_constructor with
				| Yes -> true
				| No -> false
				| Maybe ->
					begin try
						let mt = ctx.g.do_load_type_def ctx null_pos {tpackage=mt.pack;tname=mt.module_name;tsub=Some mt.name;tparams=[]} in
						begin match mt with
						| TClassDecl c when has_constructor c -> true
						| TAbstractDecl {a_impl = Some c} -> PMap.mem "_new" c.cl_statics
						| _ -> false
						end
					with _ ->
						false
					end
				end
			| ITTypeParameter {cl_kind = KTypeParameter tl} when has_constructible_constraint ctx tl [] null_pos ->
				true
			| _ -> false
		) l in
		timer();
		raise_fields l CRNew p
	in
	let is_display_debug = Meta.has (Meta.Custom ":debug.display") ctx.curfield.cf_meta in
	if is_display_debug then begin
		print_endline (Printf.sprintf "expected type: %s" (s_with_type with_type));
		print_endline (Printf.sprintf "typed expr:\n%s" (s_expr_ast true "" (s_type (print_context())) e));
	end;
	let p = e.epos in
	begin match with_type with
		| WithType t ->
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

let handle_edisplay ctx e dk with_type =
	match dk,ctx.com.display.dms_kind with
	| DKCall,(DMSignature | DMDefault) -> handle_signature_display ctx e with_type
	| DKStructure,DMDefault ->
		begin match with_type with
			| WithType t ->
				begin match follow t with
					| TAnon an -> handle_structure_display ctx e t an
					| _ -> handle_display ctx e dk with_type
				end
			| _ ->
				handle_display ctx e dk with_type
		end
	| DKPattern,DMDefault ->
		begin try
			handle_display ctx e dk with_type
		with DisplayException(DisplayFields(l,CRToplevel _,p)) ->
			raise_fields l CRPattern p
		end
	| _ -> handle_display ctx e dk with_type