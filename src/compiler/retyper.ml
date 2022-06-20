open Globals
open Ast
open Typecore
open Type
open TypeloadModule
open TypeloadFields

exception Fail of string

type retyping_context = {
	typer : typer;
}

let fail s =
	raise (Fail s)

let disable_typeloading ctx f =
	let old = ctx.g.load_only_cached_modules in
	ctx.g.load_only_cached_modules <- true;
	try
		Std.finally (fun () -> ctx.g.load_only_cached_modules <- old) f ()
	with (Error.Error (Module_not_found path,_)) ->
		fail (Printf.sprintf "Could not load module %s" (s_type_path path))

let pair_type th t = match th with
	| None ->
		TExprToExpr.convert_type t,null_pos
	| Some t ->
		t

let pair_class_field rctx ctx cctx fctx cf cff p =
	match cff.cff_kind with
	| FFun fd ->
		(* Fill in blanks by using typed information *)
		let targs,tret = match follow cf.cf_type with
			| TFun(args,ret) ->
				args,ret
			| _ ->
				fail "Type change"
		in
		let args = try
			List.map2 (fun (name,opt,meta,th,eo) (_,_,t) ->
				(name,opt,meta,Some (pair_type th t),eo)
			) fd.f_args targs
		with Invalid_argument _ ->
			fail "Type change"
		in
		let ret = pair_type fd.f_type tret in
		let fd = {
			fd with
			f_args = args;
			f_type = Some ret
		} in
		let load_args_ret () =
			setup_args_ret ctx cctx fctx (fst cff.cff_name) fd p
		in
		let args,ret = disable_typeloading ctx load_args_ret in
		let t = TFun(args#for_type,ret) in
		(fun () ->
			(* This is the only part that should actually modify anything. *)
			cf.cf_type <- t;
			TypeBinding.bind_method ctx cctx fctx cf t args ret fd.f_expr (match fd.f_expr with Some e -> snd e | None -> cff.cff_pos);
			if ctx.com.display.dms_full_typing then
				remove_class_field_flag cf CfPostProcessed;
		)
	| FVar(th,eo) | FProp(_,_,th,eo) ->
		let th = Some (pair_type th cf.cf_type) in
		let t = disable_typeloading ctx (fun () -> load_variable_type_hint ctx eo (pos cff.cff_name) th) in
		(fun () ->
			cf.cf_type <- t;
			TypeBinding.bind_var ctx cctx fctx cf eo;
			if ctx.com.display.dms_full_typing then
				remove_class_field_flag cf CfPostProcessed;
		)

let pair_classes rctx context_init c d p =
	let fail s =
		fail (Printf.sprintf "[Class %s] %s" (s_type_path c.cl_path) s)
	in
	c.cl_restore();
	let cctx = create_class_context c context_init p in
	let ctx = create_typer_context_for_class rctx.typer cctx p in
	let fl = List.map (fun cff ->
		let name = fst cff.cff_name in
		let fail s =
			fail (Printf.sprintf "[Field %s] %s" name s)
		in
		let display_modifier = Typeload.check_field_access ctx cff in
		let fctx = create_field_context cctx cff ctx.is_display_file display_modifier in
		let cf = match fctx.field_kind with
			| FKConstructor ->
				begin match c.cl_constructor with
				| None ->
					fail "Constructor not found"
				| Some cf ->
					cf
				end
			| FKNormal ->
				begin try
					PMap.find name (if fctx.is_static then c.cl_statics else c.cl_fields)
				with Not_found ->
					fail "Field not found"
				end
			| FKInit ->
				fail "TODO"
		in
		pair_class_field rctx ctx cctx fctx cf cff p
	) d.d_data in
	fl @ [fun () -> TypeloadFields.finalize_class ctx cctx]

let pair_enums ctx rctx en d =
	let ctx = { ctx with type_params = en.e_params } in
	let fail s =
		fail (Printf.sprintf "[Enum %s] %s" (s_type_path en.e_path) s)
	in
	List.iter (fun eff ->
		let name = fst eff.ec_name in
		let fail s =
			fail (Printf.sprintf "[Field %s] %s" name s)
		in
		let ef = try
			PMap.find name en.e_constrs
		with Not_found ->
			fail "Field not found"
		in
		let th = pair_type eff.ec_type ef.ef_type in
		ignore (disable_typeloading ctx (fun () -> Typeload.load_complex_type ctx false th))
	) d.d_data;
	[]

let pair_typedefs ctx rctx td d =
	let ctx = { ctx with type_params = td.t_params } in
	ignore (disable_typeloading ctx (fun () -> Typeload.load_complex_type ctx false d.d_data));
	[]

let pair_abstracts ctx rctx context_init a d p =
	let fail s =
		fail (Printf.sprintf "[Abstract %s] %s" (s_type_path a.a_path) s)
	in
	match a.a_impl with
	| Some c ->
		c.cl_restore();
		let cctx = create_class_context c context_init p in
		let ctx = create_typer_context_for_class rctx.typer cctx p in
		let fl = List.map (fun cff ->
			let cff = TypeloadFields.transform_abstract_field2 ctx a cff in
			let name = fst cff.cff_name in
			let fail s =
				fail (Printf.sprintf "[Field %s] %s" name s)
			in
			let display_modifier = Typeload.check_field_access ctx cff in
			let fctx = create_field_context cctx cff ctx.is_display_file display_modifier in
			let cf = try
				PMap.find name c.cl_statics
			with Not_found ->
				fail "Field not found"
			in
			pair_class_field rctx ctx cctx fctx cf cff p
		) d.d_data in
		fl @ [fun () -> TypeloadFields.finalize_class ctx cctx]
	| None ->
		(* ?*)
		[]

let attempt_retyping ctx m p =
	let com = ctx.com in
	let file,_,_,decls = TypeloadParse.parse_module' com m.m_path p in
	let ctx = create_typer_context_for_module ctx m in
	let rctx = {
		typer = ctx;
	} in
	(* log rctx 0 (Printf.sprintf "Retyping module %s" (s_type_path m.m_path)); *)
	let context_init = new TypeloadFields.context_init in
	let find_type name = try
		List.find (fun t -> snd (t_infos t).mt_path = name) ctx.m.curmod.m_types
	with Not_found ->
		fail (Printf.sprintf "Type %s not found in module %s" name (s_type_path m.m_path))
	in
	let rec loop acc decls = match decls with
		| [] ->
			List.rev acc
		| (d,p) :: decls ->
			begin match d with
			| EImport (path,mode) ->
				ImportHandling.init_import ctx context_init path mode p;
				ImportHandling.commit_import ctx path mode p;
				loop acc decls
			| EUsing path ->
				ImportHandling.init_using ctx context_init path p;
				loop acc decls
			| EClass c ->
				let mt = find_type (fst c.d_name) in
				loop ((d,mt) :: acc) decls
			| EEnum en ->
				let mt = find_type (fst en.d_name) in
				loop ((d,mt) :: acc) decls
			| ETypedef td ->
				let mt = find_type (fst td.d_name) in
				loop ((d,mt) :: acc) decls
			| EAbstract a ->
				let mt = find_type (fst a.d_name) in
				loop ((d,mt) :: acc) decls
			| _ ->
				loop acc decls
			end;
	in
	try
		m.m_extra.m_cache_state <- MSUnknown;
		let pairs = loop [] decls in
		let fl = List.map (fun (d,mt) -> match d,mt with
			| EClass d,TClassDecl c ->
				pair_classes rctx context_init c d p
			| EEnum d,TEnumDecl en ->
				pair_enums ctx rctx en d
			| ETypedef d,TTypeDecl td ->
				pair_typedefs ctx rctx td d
			| EAbstract d,TAbstractDecl a ->
				pair_abstracts ctx rctx context_init  a d p
			| _ ->
				fail "?"
		) pairs in
		(* If we get here we know that the everything is ok. *)
		delay ctx PConnectField (fun () -> context_init#run);
		List.iter (fun fl ->
			List.iter (fun f -> f()) fl
		) fl;
		m.m_extra.m_cache_state <- MSGood;
		m.m_extra.m_time <- Common.file_time file;
		None
	with Fail s ->
		Some s