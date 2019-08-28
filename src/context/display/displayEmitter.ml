open Globals
open Ast
open Type
open Typecore
open DisplayException
open DisplayTypes
open DisplayMode
open CompletionItem
open CompletionType
open ImportStatus
open ClassFieldOrigin
open DisplayTypes.CompletionResultKind
open Common
open Display
open DisplayPosition

let get_expected_name with_type = match with_type with
	| WithType.Value (Some src) | WithType.WithType(_,Some src) ->
		(match src with
		| WithType.FunctionArgument name -> Some name
		| WithType.StructureField name -> Some name
		| WithType.ImplicitReturn -> None
		)
	| _ -> None

let sort_fields l with_type tk =
	let p = match tk with
		| TKExpr p | TKField p -> Some p
		| _ -> None
	in
	let expected_name = get_expected_name with_type in
	let l = List.map (fun ci ->
		let i = get_sort_index tk ci (Option.default Globals.null_pos p) expected_name in
		ci,i
	) l in
	let sort l =
		List.map fst (List.sort (fun (_,i1) (_,i2) -> compare i1 i2) l)
	in
	(* This isn't technically accurate, but I don't think it matters. *)
	let rec dynamify_type_params t = match follow t with
		| TInst({cl_kind = KTypeParameter _},_) -> mk_mono()
		| _ -> Type.map dynamify_type_params t
	in
	let l = match with_type with
		| WithType.WithType(t,_) when (match follow t with TMono _ -> false | _ -> true) ->
			let rec comp item = match item.ci_type with
				| None -> 9
				| Some (t',_) ->
				(* For enum constructors, we consider the return type of the constructor function
				   so it has the same priority as argument-less constructors. *)
				let t' = match item.ci_kind,follow t' with
					| ITEnumField _,TFun(_,r) -> r
					| _ -> t'
				in
				let t' = dynamify_type_params t' in
				if type_iseq t' t then 0 (* equal types - perfect *)
				else if t' == t_dynamic then 5 (* dynamic isn't good, but better than incompatible *)
				else try Type.unify t' t; 1 (* assignable - great *)
				with Unify_error _ -> match follow t' with
					| TFun(_,tr) ->
						if type_iseq tr t then 2 (* function returns our exact type - alright *)
						else (try Type.unify tr t; 3 (* function returns compatible type - okay *)
						with Unify_error _ -> 7) (* incompatible function - useless *)
					| _ ->
						6 (* incompatible type - probably useless *)
			in
			let l = List.map (fun (item,i1) ->
				let i2 = comp item in
				item,(i2,i1)
			) l in
			sort l
		| _ ->
			sort l
	in
	l

let get_import_status ctx path =
	try
		let mt' = ctx.g.do_load_type_def ctx null_pos {tpackage = []; tname = snd path; tparams = []; tsub = None} in
		if path <> (t_infos mt').mt_path then Shadowed else Imported
	with _ ->
		Unimported

let display_module_type ctx mt p = match ctx.com.display.dms_kind with
	| DMDefinition | DMTypeDefinition ->
		begin match mt with
		| TClassDecl c when Meta.has Meta.CoreApi c.cl_meta ->
			let c' = ctx.g.do_load_core_class ctx c in
			raise_positions [c.cl_name_pos;c'.cl_name_pos]
		| _ ->
			raise_positions [(t_infos mt).mt_name_pos];
		end
	| DMUsage _ ->
		let infos = t_infos mt in
		ReferencePosition.set (snd infos.mt_path,infos.mt_name_pos,KModuleType)
	| DMHover ->
		let t = type_of_module_type mt in
		let ct = CompletionType.from_type (get_import_status ctx) t in
		raise_hover (make_ci_type (CompletionModuleType.of_module_type mt) ImportStatus.Imported (Some (t,ct))) None p
	| _ -> ()

let rec display_type ctx t p =
	let dm = ctx.com.display in
	try
		display_module_type ctx (module_type_of_type t) p
	with Exit ->
		match follow t,follow !t_dynamic_def with
		| _,TDynamic _ -> () (* sanity check in case it's still t_dynamic *)
		| TDynamic _,_ -> display_type ctx !t_dynamic_def p
		| _ ->
			match dm.dms_kind with
			| DMHover ->
				let ct = CompletionType.from_type (get_import_status ctx) t in
				let ci = make_ci_expr (mk (TConst TNull) t p) (t,ct) in
				raise_hover ci None p
			| _ ->
				()

let check_display_type ctx t p =
	let add_type_hint () =
		ctx.g.type_hints <- (ctx.m.curmod.m_extra.m_display,p,t) :: ctx.g.type_hints;
	in
	let maybe_display_type () =
		if ctx.is_display_file && display_position#enclosed_in p then
			display_type ctx t p
	in
	add_type_hint();
	maybe_display_type()

let raise_position_of_type t =
	let mt =
		let rec follow_null t =
			match t with
				| TMono r -> (match !r with None -> raise_positions [null_pos] | Some t -> follow_null t)
				| TLazy f -> follow_null (lazy_type f)
				| TAbstract({a_path = [],"Null"},[t]) -> follow_null t
				| TDynamic _ -> !t_dynamic_def
				| _ -> t
		in
		try
			Type.module_type_of_type (follow_null t)
		with
			Exit -> raise_positions [null_pos]
	in
	raise_positions [(t_infos mt).mt_name_pos]

let display_variable ctx v p = match ctx.com.display.dms_kind with
	| DMDefinition -> raise_positions [v.v_pos]
	| DMTypeDefinition -> raise_position_of_type v.v_type
	| DMUsage _ -> ReferencePosition.set (v.v_name,v.v_pos,KVar)
	| DMHover ->
		let ct = CompletionType.from_type (get_import_status ctx) ~values:(get_value_meta v.v_meta) v.v_type in
		raise_hover (make_ci_local v (v.v_type,ct)) None p
	| _ -> ()

let display_field ctx origin scope cf p = match ctx.com.display.dms_kind with
	| DMDefinition -> raise_positions [cf.cf_name_pos]
	| DMTypeDefinition -> raise_position_of_type cf.cf_type
	| DMUsage _ ->
		let name,kind = match cf.cf_name,origin with
			| "new",(Self (TClassDecl c) | Parent(TClassDecl c)) ->
				(* For constructors, we care about the class name so we don't end up looking for "new". *)
				snd c.cl_path,KConstructor
			| _ ->
				cf.cf_name,KClassField
		in
		ReferencePosition.set (name,cf.cf_name_pos,kind)
	| DMHover ->
		let cf = if Meta.has Meta.Impl cf.cf_meta then
			prepare_using_field cf
		else
			cf
		in
        let cf = match origin,scope,follow cf.cf_type with
            | Self (TClassDecl c),CFSConstructor,TFun(tl,_) -> {cf with cf_type = TFun(tl,TInst(c,List.map snd c.cl_params))}
            | _ -> cf
        in
		let ct = CompletionType.from_type (get_import_status ctx) ~values:(get_value_meta cf.cf_meta) cf.cf_type in
		raise_hover (make_ci_class_field (CompletionClassField.make cf scope origin true) (cf.cf_type,ct)) None p
	| _ -> ()

let maybe_display_field ctx origin scope cf p =
	if display_position#enclosed_in p then display_field ctx origin scope cf p

let display_enum_field ctx en ef p = match ctx.com.display.dms_kind with
	| DMDefinition -> raise_positions [ef.ef_name_pos]
	| DMTypeDefinition -> raise_position_of_type ef.ef_type
	| DMUsage _ -> ReferencePosition.set (ef.ef_name,ef.ef_name_pos,KEnumField)
	| DMHover ->
		let ct = CompletionType.from_type (get_import_status ctx) ef.ef_type in
		raise_hover (make_ci_enum_field (CompletionEnumField.make ef (Self (TEnumDecl en)) true) (ef.ef_type,ct)) None p
	| _ -> ()

let display_meta com meta p = match com.display.dms_kind with
	| DMHover ->
		begin match meta with
		| Meta.Custom _ | Meta.Dollar _ -> ()
		| _ ->
			if com.json_out = None then begin match Meta.get_documentation meta with
				| None -> ()
				| Some (_,s) ->
					raise_metadata ("<metadata>" ^ s ^ "</metadata>")
			end else
				raise_hover (make_ci_metadata meta) None p
		end
	| DMDefault ->
		let all = Meta.get_all() in
		let all = List.map make_ci_metadata all in
		let subject = if meta = Meta.Last then None else Some (Meta.to_string meta) in
		raise_fields all CRMetadata (make_subject subject p);
	| _ ->
		()

let check_display_metadata ctx meta =
	List.iter (fun (meta,args,p) ->
		if display_position#enclosed_in p then display_meta ctx.com meta p;
		List.iter (fun e ->
			if display_position#enclosed_in (pos e) then begin
				let e = ExprPreprocessing.process_expr ctx.com e in
				delay ctx PTypeField (fun _ -> ignore(type_expr ctx e WithType.value));
			end
		) args
	) meta

let check_field_modifiers ctx c cf override display_modifier =
	match override,display_modifier with
		| Some p,_ when display_position#enclosed_in p && ctx.com.display.dms_kind = DMDefinition ->
			begin match c.cl_super with
			| Some(c,tl) ->
				let _,_,cf = raw_class_field (fun cf -> cf.cf_type) c tl cf.cf_name in
				display_field ctx (Parent(TClassDecl c)) CFSMember cf p
			| _ ->
				()
			end
		| Some _,_ when ctx.com.display.dms_kind = DMDefault ->
			let all_fields = TClass.get_all_super_fields c in
			let missing_fields = List.fold_left (fun fields cf -> PMap.remove cf.cf_name fields) all_fields c.cl_ordered_fields in
			let l = PMap.fold (fun (c,cf) fields ->
				let origin = Parent (TClassDecl c) in
				ignore(follow cf.cf_type);
				let ct = CompletionType.from_type (get_import_status ctx) ~values:(get_value_meta cf.cf_meta) cf.cf_type in
				make_ci_class_field (CompletionClassField.make cf CFSMember origin true) (cf.cf_type,ct) :: fields
			) missing_fields [] in
			let l = sort_fields l NoValue TKOverride in
			raise_fields l CROverride (make_subject (Some cf.cf_name) cf.cf_name_pos)
		| _ -> ()