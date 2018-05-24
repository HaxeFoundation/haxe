open Globals
open Ast
open Type
open Typecore
open DisplayException
open DisplayTypes.DisplayMode
open CompletionItem
open ClassFieldOrigin
open DisplayTypes.CompletionResultKind
open Common
open Display

let requires_import ctx path =
	try
		let mt' = ctx.g.do_load_type_def ctx null_pos {tpackage = []; tname = snd path; tparams = []; tsub = None} in
		path <> (t_infos mt').mt_path
	with _ ->
		true

let patch_type ctx t =
	let rec patch t = match t with
		| TInst(c,tl) when not (requires_import ctx c.cl_path) -> TInst({c with cl_path = ([],snd c.cl_path)},List.map patch tl)
		| TEnum(en,tl) when not (requires_import ctx en.e_path) -> TEnum({en with e_path = ([],snd en.e_path)},List.map patch tl)
		| TType(td,tl) when not (requires_import ctx td.t_path) -> TType({td with t_path = ([],snd td.t_path)},List.map patch tl)
		| TAbstract(a,tl) when not (requires_import ctx a.a_path) -> TAbstract({a with a_path = ([],snd a.a_path)},List.map patch tl)
		| _ -> Type.map patch t
	in
	patch t

let display_module_type ctx mt p = match ctx.com.display.dms_kind with
	| DMDefinition -> raise_position [(t_infos mt).mt_name_pos];
	| DMUsage _ -> reference_position := (t_infos mt).mt_name_pos
	| DMHover -> raise_hover (Some (patch_type ctx (type_of_module_type mt))) p (t_infos mt).mt_doc
	| _ -> ()

let rec display_type ctx t p =
	let dm = ctx.com.display in
	match dm.dms_kind with
	| DMHover ->
		raise_hover (Some (patch_type ctx t)) p None
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
		if ctx.is_display_file && is_display_position p then
			display_type ctx t p
	in
	match ctx.com.display.dms_kind with
	| DMStatistics -> add_type_hint()
	| DMUsage _ -> add_type_hint(); maybe_display_type()
	| _ -> maybe_display_type()

let display_variable ctx v p = match ctx.com.display.dms_kind with
	| DMDefinition -> raise_position [v.v_pos]
	| DMUsage _ -> reference_position := v.v_pos
	| DMHover -> raise_hover (Some (patch_type ctx v.v_type)) p None
	| _ -> ()

let display_field ctx c cf p = match ctx.com.display.dms_kind with
	| DMDefinition -> raise_position [cf.cf_name_pos]
	| DMUsage _ -> reference_position := cf.cf_name_pos
	| DMHover ->
		let t = if Meta.has Meta.Impl cf.cf_meta then
			(prepare_using_field cf).cf_type
		else
			cf.cf_type
		in
		let t = match c,follow t with
			| Some c,TFun(tl,_) when cf.cf_name = "new" -> TFun(tl,TInst(c,List.map snd c.cl_params))
			| _ -> t
		in
		raise_hover (Some (patch_type ctx t)) p cf.cf_doc
	| _ -> ()

let maybe_display_field ctx c cf p =
	if is_display_position p then display_field ctx c cf p

let display_enum_field ctx ef p = match ctx.com.display.dms_kind with
	| DMDefinition -> raise_position [ef.ef_name_pos]
	| DMUsage _ -> reference_position := ef.ef_name_pos
	| DMHover -> raise_hover (Some (patch_type ctx ef.ef_type)) p ef.ef_doc
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
					raise_hover None null_pos (Some s)
		end
	| DMDefault ->
		let all,_ = Meta.get_documentation_list() in
		let all = List.map (fun (s,doc) ->
			ITMetadata(s,Some doc)
		) all in
		raise_fields all CRMetadata (Some p) false
	| _ ->
		()

let check_display_metadata ctx meta =
	List.iter (fun (meta,args,p) ->
		if is_display_position p then display_meta ctx.com meta p;
		List.iter (fun e ->
			if is_display_position (pos e) then begin
				let e = ExprPreprocessing.process_expr ctx.com e in
				delay ctx PTypeField (fun _ -> ignore(type_expr ctx e Value));
			end
		) args
	) meta

let check_field_modifiers ctx c cf override display_modifier =
	match override,display_modifier with
		| Some p,_ when Display.is_display_position p && ctx.com.display.dms_kind = DMDefinition ->
			begin match c.cl_super with
			| Some(c,tl) ->
				let _,_,cf = raw_class_field (fun cf -> cf.cf_type) c tl cf.cf_name in
				display_field ctx (Some c) cf p
			| _ ->
				()
			end
		| _,Some (AOverride,p) when ctx.com.display.dms_kind = DMDefault ->
			let all_fields = TClass.get_all_super_fields c in
			let missing_fields = List.fold_left (fun fields cf -> PMap.remove cf.cf_name fields) all_fields c.cl_ordered_fields in
			let l = PMap.fold (fun (c,cf) fields ->
				let origin = Parent (TClassDecl c) in
				(ITClassField (CompletionClassField.make cf CFSMember origin true)) :: fields
			) missing_fields [] in
			raise_fields l CROverride None false
		| _ -> ()