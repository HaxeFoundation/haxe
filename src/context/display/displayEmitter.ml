open Globals
open Ast
open Type
open Typecore
open DisplayException
open DisplayTypes
open DisplayMode
open CompletionItem
open CompletionType
open ClassFieldOrigin
open DisplayTypes.CompletionResultKind
open Common
open Display
open DisplayPosition

let symbol_of_module_type = function
	| TClassDecl c -> SKClass c
	| TEnumDecl en -> SKEnum en
	| TTypeDecl td -> SKTypedef td
	| TAbstractDecl a -> SKAbstract a

let display_module_type ctx mt p = match ctx.com.display.dms_kind with
	| DMDefinition | DMTypeDefinition ->
		begin match mt with
		| TClassDecl c when Meta.has Meta.CoreApi c.cl_meta ->
			let c' = ctx.g.do_load_core_class ctx c in
			raise_positions [c.cl_name_pos;c'.cl_name_pos]
		| _ ->
			raise_positions [(t_infos mt).mt_name_pos];
		end
	| DMUsage _ | DMImplementation ->
		let infos = t_infos mt in
		ReferencePosition.set (snd infos.mt_path,infos.mt_name_pos,symbol_of_module_type mt)
	| DMHover ->
		let t = type_of_module_type mt in
		let ct = CompletionType.from_type (get_import_status ctx) t in
		raise_hover (make_ci_type (CompletionModuleType.of_module_type mt) ImportStatus.Imported (Some (t,ct))) None p
	| DMDefault ->
		DisplayToplevel.collect_and_raise ctx TKType WithType.value CRTypeHint ((s_type_path (t_infos mt).mt_path),p) p
	| _ -> ()

let rec display_type ctx t p =
	let dm = ctx.com.display in
	try
		display_module_type ctx (module_type_of_type t) p
	with Exit ->
		match follow t,follow !t_dynamic_def with
		| _,TDynamic -> () (* sanity check in case it's still t_dynamic *)
		| TDynamic,_ -> display_type ctx !t_dynamic_def p
		| _ ->
			match dm.dms_kind with
			| DMHover ->
				let ct = CompletionType.from_type (get_import_status ctx) t in
				let ci = make_ci_expr (mk (TConst TNull) t p) (t,ct) in
				raise_hover ci None p
			| _ ->
				()

let check_display_type ctx t path =
	let add_type_hint () =
		ctx.g.type_hints <- (ctx.m.curmod.m_extra.m_display,pos path,t) :: ctx.g.type_hints;
	in
	let maybe_display_type () =
		if ctx.is_display_file && display_position#enclosed_in (pos path) then
			let p =
				match path with
				| ({ tpackage = pack; tname = name; tsub = sub },p) ->
					let strings = match sub with None -> name :: pack | Some s -> s :: name :: pack in
					let length = String.length (String.concat "." strings) in
					{ p with pmax = p.pmin + length }
			in
			display_type ctx t p
	in
	add_type_hint();
	maybe_display_type()

let raise_position_of_type t =
	let mt =
		let rec follow_null t =
			match t with
				| TMono r -> (match r.tm_type with None -> raise_positions [null_pos] | Some t -> follow_null t)
				| TLazy f -> follow_null (lazy_type f)
				| TAbstract({a_path = [],"Null"},[t]) -> follow_null t
				| TDynamic -> !t_dynamic_def
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
	| DMUsage _ -> ReferencePosition.set (v.v_name,v.v_pos,SKVariable v)
	| DMHover ->
		let ct = CompletionType.from_type (get_import_status ctx) ~values:(get_value_meta v.v_meta) v.v_type in
		raise_hover (make_ci_local v (v.v_type,ct)) None p
	| _ -> ()

let display_field ctx origin scope cf p = match ctx.com.display.dms_kind with
	| DMDefinition -> raise_positions [cf.cf_name_pos]
	| DMTypeDefinition -> raise_position_of_type cf.cf_type
	| DMUsage _ | DMImplementation ->
		let name,kind = match cf.cf_name,origin with
			| "new",(Self (TClassDecl c) | Parent(TClassDecl c)) ->
				(* For constructors, we care about the class name so we don't end up looking for "new". *)
				snd c.cl_path,SKConstructor cf
			| _,(Self (TClassDecl c) | Parent(TClassDecl c)) ->
				cf.cf_name,SKField (cf,Some c.cl_path)
			| _ ->
				cf.cf_name,SKField (cf,None)
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
	| DMUsage _ -> ReferencePosition.set (ef.ef_name,ef.ef_name_pos,SKEnumField ef)
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