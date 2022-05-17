open Globals
open Ast
open Typecore
open Type
open TypeloadModule
open TypeloadFields

exception Fail of string

type retyping_context = {
	typer : typer;
	mutable log : string list;
}

let fail s =
	raise (Fail s)

let log rctx indent message =
	print_endline (Printf.sprintf "[retyper] %*s%s" (indent * 4) "" message)

let pair_classes rctx context_init m mt d p =
	log rctx 1 (Printf.sprintf "Pairing class [%s]" (s_type_path (t_infos mt).mt_path));
	let kind_fail want got =
		fail (Printf.sprintf "Expected %s  for %s, found %s" want (s_type_path (t_infos mt).mt_path) got)
	in
	let c = match mt with
		| TClassDecl c ->
			c
		| TEnumDecl _ ->
			kind_fail "class" "enum"
		| TTypeDecl _ ->
			kind_fail "class" "typedef"
		| TAbstractDecl _ ->
			kind_fail "class" "abstract"
	in
	c.cl_restore();
	(* TODO: check various things *)
	let cctx = create_class_context c context_init p in
	let ctx = create_typer_context_for_class rctx.typer cctx p in
	log rctx 1 "Found matching type kind";
	let fl = List.map (fun cff ->
		let name = fst cff.cff_name in
		log rctx 2 (Printf.sprintf "Pairing field [%s]" name);
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
		let disable_typeloading f =
			let old = ctx.g.load_only_cached_modules in
			ctx.g.load_only_cached_modules <- true;
			try
				Std.finally (fun () -> ctx.g.load_only_cached_modules <- old) f ()
			with (Error.Error (Module_not_found path,_)) ->
				fail (Printf.sprintf "Could not load module %s" (s_type_path path))
		in
		let pair_type th t = match th with
			| None ->
				Some (TExprToExpr.convert_type t,null_pos)
			| Some t ->
				Some t
		in
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
					(name,opt,meta,pair_type th t,eo)
				) fd.f_args targs
			with Invalid_argument _ ->
				fail "Type change"
			in
			let ret = pair_type fd.f_type tret in
			let fd = {
				fd with
				f_args = args;
				f_type = ret
			} in
			let load_args_ret () =
				setup_args_ret ctx cctx fctx (fst cff.cff_name) fd p
			in
			let args,ret = disable_typeloading load_args_ret in
			let t = TFun(args#for_type,ret) in
			(fun () ->
				(* This is the only part that should actually modify anything. *)
				cf.cf_type <- t;
				TypeBinding.bind_method ctx cctx fctx cf t args ret fd.f_expr (match fd.f_expr with Some e -> snd e | None -> cff.cff_pos);
				if ctx.com.display.dms_full_typing then
					remove_class_field_flag cf CfPostProcessed;
				log rctx 2 ("Field updated")
			)
		| FVar(th,eo) | FProp(_,_,th,eo) ->
			let th = pair_type th cf.cf_type in
			let t = disable_typeloading (fun () -> load_variable_type_hint ctx eo (pos cff.cff_name) th) in
			(fun () ->
				cf.cf_type <- t;
				TypeBinding.bind_var ctx cctx fctx cf eo;
				if ctx.com.display.dms_full_typing then
					remove_class_field_flag cf CfPostProcessed;
				log rctx 2 ("Field updated")
			)
	) d.d_data in
	cctx,fl

let attempt_retyping ctx m p =
	let com = ctx.com in
	let file,remap,pack,decls = TypeloadParse.parse_module' com m.m_path p in
	let ctx = create_typer_context_for_module ctx m in
	let rctx = {
		typer = ctx;
		log = []
	} in
	log rctx 0 (Printf.sprintf "Retyping module %s" (s_type_path m.m_path));
	let context_init = new TypeloadFields.context_init in
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
			| EClass d ->
				let mt = try
					List.find (fun t -> snd (t_infos t).mt_path = fst d.d_name) ctx.m.curmod.m_types
				with Not_found ->
					fail (Printf.sprintf "Type %s not found in module %s" (fst d.d_name) (s_type_path m.m_path))
				in
				loop ((d,mt) :: acc) decls
			| _ ->
				loop acc decls
			end;
	in
	let result = try
		m.m_extra.m_cache_state <- MSUnknown;
		let pairs = loop [] decls in
		let fl = List.map (fun (d,mt) ->
			pair_classes rctx context_init m mt d p
		) pairs in
		(* If we get here we know that the everything is ok. *)
		delay ctx PConnectField (fun () -> context_init#run);
		List.iter (fun (cctx,fl) ->
			TypeloadFields.finalize_class ctx cctx
		) fl;
		m.m_extra.m_cache_state <- MSGood;
		m.m_extra.m_time <- Common.file_time file;
		log rctx 0 (Printf.sprintf "Retyped module %s" (s_type_path m.m_path));
		true
	with Fail s ->
		log rctx 0 (Printf.sprintf "Failed retyping module %s" (s_type_path m.m_path));
		log rctx 1 s;
		false
	in
	result