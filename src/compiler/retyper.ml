open Globals
open Ast
open Typecore
open Type
open TypeloadModule
open TypeloadFields

exception Fail of string

let fail s =
	raise (Fail s)

let log indent message =
	print_endline (Printf.sprintf "[retyper] %*s%s" (indent * 4) "" message)

let pair_classes ctx context_init m mt d p =
	log 1 (Printf.sprintf "Pairing class [%s]" (s_type_path (t_infos mt).mt_path));
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
	let ctx = create_typer_context_for_class ctx cctx p in
	log 1 "Found matching type kind";
	List.iter (fun cff ->
		let name = fst cff.cff_name in
		log 2 (Printf.sprintf "Pairing field [%s]" name);
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
		begin match cff.cff_kind with
		| FFun fd ->
			let load_args_ret () =
				setup_args_ret ctx cctx fctx cff fd p
			in
			let old = ctx.g.load_only_cached_modules in
			ctx.g.load_only_cached_modules <- true;
			let args,ret = try
				Std.finally (fun () -> ctx.g.load_only_cached_modules <- old) load_args_ret ()
			with (Error.Error (Module_not_found path,_)) ->
				fail (Printf.sprintf "Could not load module %s" (s_type_path path))
			in
			let t = TFun(args#for_type,ret) in
			let old_t = cf.cf_type in
			cf.cf_type <- t;
			TypeBinding.bind_method ctx cctx fctx cf t args ret fd.f_expr (match fd.f_expr with Some e -> snd e | None -> cff.cff_pos);
			delay ctx PCheckConstraint (fun () ->
				if not (type_iseq old_t cf.cf_type) then begin
					let st = s_type (print_context()) in
					log 1 (Printf.sprintf "Field type mismatch for %s:\n\twas: %s\n\t is: %s" cf.cf_name (st old_t) (st cf.cf_type))
				end;
			);
			if ctx.com.display.dms_full_typing then
				remove_class_field_flag cf CfPostProcessed;
		| _ ->
			(* TODO *)
			()
		end;
		log 2 ("Field updated")
	) d.d_data;
	TypeloadFields.finalize_class ctx cctx

let attempt_retyping ctx m p =
	let com = ctx.com in
	let file,remap,pack,decls = TypeloadParse.parse_module' com m.m_path p in
	log 0 (Printf.sprintf "Retyping module %s" (s_type_path m.m_path));
	let ctx = create_typer_context_for_module ctx m in
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
	try
		m.m_extra.m_dirty <- None;
		let pairs = loop [] decls in
		List.iter (fun (d,mt) ->
			pair_classes ctx context_init m mt d p
		) pairs;
		delay ctx PConnectField (fun () -> context_init#run);
		m.m_extra.m_time <- Common.file_time file;
		true
	with Fail s ->
		log 0 (Printf.sprintf "Failed: %s" s);
		false