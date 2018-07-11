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

let sort_fields l with_type p =
	let l = List.map (fun ci ->
		let i = get_sort_index ci (Option.default Globals.null_pos p) in
		ci,i
	) l in
	let sort l =
		List.map fst (List.sort (fun (_,i1) (_,i2) -> compare i1 i2) l)
	in
	let l = match with_type with
		| WithType t when (match follow t with TMono _ -> false | _ -> true) ->
			let rec comp t' = match t' with
				| None -> 9
				| Some (t',_) ->
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
			let l = List.map (fun (ck,i1) ->
				let i2 = comp (get_type ck) in
				ck,(i2,i1)
			) l in
			sort l
		| _ ->
			sort l
	in
	l

let completion_type_of_type ctx ?(values=PMap.empty) t =
	let get_import_status path =
		try
			let mt' = ctx.g.do_load_type_def ctx null_pos {tpackage = []; tname = snd path; tparams = []; tsub = None} in
			if path <> (t_infos mt').mt_path then Shadowed else Imported
		with _ ->
			Unimported
	in
	let rec ppath mpath tpath tl = {
		ct_pack = fst tpath;
		ct_module_name = snd mpath;
		ct_type_name = snd tpath;
		ct_import_status = get_import_status tpath;
		ct_params = List.map (from_type PMap.empty) tl;
	}
	and funarg value (name,opt,t) = {
		ct_name = name;
		ct_optional = opt;
		ct_type = from_type PMap.empty t;
		ct_value = value
	}
	and from_type values t = match t with
		| TMono r ->
			begin match !r with
				| None -> CTMono
				| Some t -> from_type values t
			end
		| TLazy r ->
			from_type values (lazy_type r)
		| TInst({cl_kind = KTypeParameter _} as c,_) ->
			CTInst ({
				ct_pack = fst c.cl_path;
				ct_module_name = snd c.cl_module.m_path;
				ct_type_name = snd c.cl_path;
				ct_import_status = Imported;
				ct_params = [];
			})
		| TInst(c,tl) ->
			CTInst (ppath c.cl_module.m_path c.cl_path tl)
		| TEnum(en,tl) ->
			CTEnum (ppath en.e_module.m_path en.e_path tl)
		| TType(td,tl) ->
			CTTypedef (ppath td.t_module.m_path td.t_path tl)
		| TAbstract(a,tl) ->
			CTAbstract (ppath a.a_module.m_path a.a_path tl)
		| TFun(tl,t) when not (PMap.is_empty values) ->
			let get_arg n = try Some (PMap.find n values) with Not_found -> None in
			CTFunction {
				ct_args = List.map (fun (n,o,t) -> funarg (get_arg n) (n,o,t)) tl;
				ct_return = from_type PMap.empty t;
			}
		| TFun(tl,t) ->
			CTFunction {
				ct_args = List.map (funarg None) tl;
				ct_return = from_type PMap.empty t;
			}
		| TAnon an ->
			let afield af = {
				ctf_field = af;
				ctf_type = from_type PMap.empty af.cf_type;
			} in
			CTAnonymous {
				ct_fields = PMap.fold (fun cf acc -> afield cf :: acc) an.a_fields [];
				ct_status = !(an.a_status);
			}
		| TDynamic t ->
			CTDynamic (if t == t_dynamic then None else Some (from_type PMap.empty t))
	in
	from_type values t

let display_module_type ctx mt p = match ctx.com.display.dms_kind with
	| DMDefinition | DMTypeDefinition -> raise_position [(t_infos mt).mt_name_pos];
	| DMUsage _ ->
		let infos = t_infos mt in
		reference_position := (snd infos.mt_path,infos.mt_name_pos,KModuleType)
	| DMHover ->
		let t = type_of_module_type mt in
		let ct = completion_type_of_type ctx t in
		raise_hover (make_ci_type (CompletionModuleType.of_module_type mt) ImportStatus.Imported (Some (t,ct))) p
	| _ -> ()

let rec display_type ctx t p =
	let dm = ctx.com.display in
	match dm.dms_kind with
	| DMHover ->
		let ct = completion_type_of_type ctx t in
		let ci = make_ci_expr (mk (TConst TNull) t p) (t,ct) in
		raise_hover ci p
	| _ ->
		try display_module_type ctx (module_type_of_type t) p
		with Exit -> match follow t,follow !t_dynamic_def with
			| _,TDynamic _ -> () (* sanity check in case it's still t_dynamic *)
			| TDynamic _,_ -> display_type ctx !t_dynamic_def p
			| _ -> ()

let check_display_type ctx t p =
	let add_type_hint () =
		let md = ctx.m.curmod.m_extra.m_display in
		md.m_type_hints <- (p,t) :: md.m_type_hints;
	in
	let maybe_display_type () =
		if ctx.is_display_file && encloses_display_position p then
			display_type ctx t p
	in
	add_type_hint();
	maybe_display_type()

let raise_position_of_type t =
	let mt =
		let rec follow_null t =
			match t with
				| TMono r -> (match !r with None -> raise_position [null_pos] | Some t -> follow_null t)
				| TAbstract({a_path = [],"Null"},[t]) -> follow_null t
				| _ -> t
		in
		try
			Type.module_type_of_type (follow_null t)
		with
			Exit -> raise_position [null_pos]
	in
	raise_position [(t_infos mt).mt_name_pos]

let display_variable ctx v p = match ctx.com.display.dms_kind with
	| DMDefinition -> raise_position [v.v_pos]
	| DMTypeDefinition -> raise_position_of_type v.v_type
	| DMUsage _ -> reference_position := (v.v_name,v.v_pos,KVar)
	| DMHover ->
		let ct = completion_type_of_type ctx ~values:(get_value_meta v.v_meta) v.v_type in
		raise_hover (make_ci_local v (v.v_type,ct)) p
	| _ -> ()

let display_field ctx origin scope cf p = match ctx.com.display.dms_kind with
	| DMDefinition -> raise_position [cf.cf_name_pos]
	| DMTypeDefinition -> raise_position_of_type cf.cf_type
	| DMUsage _ ->
		let name,kind = match cf.cf_name,origin with
			| "new",(Self (TClassDecl c) | Parent(TClassDecl c)) ->
				(* For constructors, we care about the class name so we don't end up looking for "new". *)
				snd c.cl_path,KConstructor
			| _ ->
				cf.cf_name,KClassField
		in
		reference_position := (name,cf.cf_name_pos,kind)
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
		let ct = completion_type_of_type ctx ~values:(get_value_meta cf.cf_meta) cf.cf_type in
		raise_hover (make_ci_class_field (CompletionClassField.make cf scope origin true) (cf.cf_type,ct)) p
	| _ -> ()

let maybe_display_field ctx origin scope cf p =
	if encloses_display_position p then display_field ctx origin scope cf p

let display_enum_field ctx en ef p = match ctx.com.display.dms_kind with
	| DMDefinition -> raise_position [ef.ef_name_pos]
	| DMTypeDefinition -> raise_position_of_type ef.ef_type
	| DMUsage _ -> reference_position := (ef.ef_name,ef.ef_name_pos,KEnumField)
	| DMHover ->
		let ct = completion_type_of_type ctx ef.ef_type in
		raise_hover (make_ci_enum_field (CompletionEnumField.make ef (Self (TEnumDecl en)) true) (ef.ef_type,ct)) p
	| _ -> ()

let display_meta com meta p = match com.display.dms_kind with
	| DMHover ->
		begin match meta with
		| Meta.Custom _ | Meta.Dollar _ -> ()
		| _ -> match Meta.get_documentation meta with
			| None -> ()
			| Some (_,s) ->
				(* TODO: hack until we support proper output for hover display mode *)
				if com.json_out = None then
					raise_metadata ("<metadata>" ^ s ^ "</metadata>")
				else
					raise_hover (make_ci_metadata (Meta.to_string meta) (Some s)) p
		end
	| DMDefault ->
		let all,_ = Meta.get_documentation_list() in
		let all = List.map (fun (s,doc) ->
			make_ci_metadata s (Some doc)
		) all in
		raise_fields all CRMetadata (Some p)
	| _ ->
		()

let check_display_metadata ctx meta =
	List.iter (fun (meta,args,p) ->
		if encloses_display_position p then display_meta ctx.com meta p;
		List.iter (fun e ->
			if encloses_display_position (pos e) then begin
				let e = ExprPreprocessing.process_expr ctx.com e in
				delay ctx PTypeField (fun _ -> ignore(type_expr ctx e Value));
			end
		) args
	) meta

let check_field_modifiers ctx c cf override display_modifier =
	match override,display_modifier with
		| Some p,_ when encloses_display_position p && ctx.com.display.dms_kind = DMDefinition ->
			begin match c.cl_super with
			| Some(c,tl) ->
				let _,_,cf = raw_class_field (fun cf -> cf.cf_type) c tl cf.cf_name in
				display_field ctx (Parent(TClassDecl c)) CFSMember cf p
			| _ ->
				()
			end
		| _,Some (AOverride,p) when ctx.com.display.dms_kind = DMDefault ->
			let all_fields = TClass.get_all_super_fields c in
			let missing_fields = List.fold_left (fun fields cf -> PMap.remove cf.cf_name fields) all_fields c.cl_ordered_fields in
			let l = PMap.fold (fun (c,cf) fields ->
				let origin = Parent (TClassDecl c) in
				ignore(follow cf.cf_type);
				let ct = completion_type_of_type ctx ~values:(get_value_meta cf.cf_meta) cf.cf_type in
				make_ci_class_field (CompletionClassField.make cf CFSMember origin true) (cf.cf_type,ct) :: fields
			) missing_fields [] in
			let l = sort_fields l NoValue None in
			raise_fields l CROverride None
		| _ -> ()