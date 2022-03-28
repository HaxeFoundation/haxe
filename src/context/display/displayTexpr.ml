open Globals
open Common
open Ast
open Type
open Typecore
open DisplayPosition
open CompletionItem
open CompilationCache
open ClassFieldOrigin

let find_field_by_position sc p =
	List.find (fun cff ->
		if pos cff.cff_name = p then true else false
	) sc.d_data

let find_enum_field_by_position sc p =
	List.find (fun eff ->
		if pos eff.ec_name = p then true else false
	) sc.d_data

let find_class_by_position decls p =
	let rec loop dl = match dl with
		| (EClass c,_) :: dl when pos c.d_name = p -> c
		| _ :: dl -> loop dl
		| [] -> raise Not_found
	in
	loop decls

let find_module_static_by_position decls p =
	let rec loop dl = match dl with
		| (EStatic d,_) :: dl when pos d.d_name = p -> d
		| _ :: dl -> loop dl
		| [] -> raise Not_found
	in
	loop decls

let find_enum_by_position decls p =
	let rec loop dl = match dl with
		| (EEnum en,_) :: dl when pos en.d_name = p -> en
		| _ :: dl -> loop dl
		| [] -> raise Not_found
	in
	loop decls

let find_typedef_by_position decls p =
	let rec loop dl = match dl with
		| (ETypedef td,_) :: dl when pos td.d_name = p -> td
		| _ :: dl -> loop dl
		| [] -> raise Not_found
	in
	loop decls

let find_abstract_by_position decls p =
	let rec loop dl = match dl with
		| (EAbstract a,_) :: dl when pos a.d_name = p -> a
		| _ :: dl -> loop dl
		| [] -> raise Not_found
	in
	loop decls

let actually_check_display_field ctx c cff p =
	let context_init = new TypeloadFields.context_init in
	let cctx = TypeloadFields.create_class_context ctx.com c context_init p in
	let ctx = TypeloadFields.create_typer_context_for_class ctx cctx p in
	let cff = TypeloadFields.transform_field (ctx,cctx) c cff (ref []) (pos cff.cff_name) in
	let display_modifier = Typeload.check_field_access ctx cff in
	let fctx = TypeloadFields.create_field_context cctx cff true display_modifier in
	let cf = TypeloadFields.init_field (ctx,cctx,fctx) cff in
	flush_pass ctx PTypeField "check_display_field";
	ignore(follow cf.cf_type)

let check_display_field ctx sc c cf =
	let cff = find_field_by_position sc cf.cf_name_pos in
	actually_check_display_field ctx c cff cf.cf_pos

let check_display_class ctx decls c =
	let check_field sc cf =
		if display_position#enclosed_in cf.cf_pos then
			check_display_field ctx sc c cf;
		DisplayEmitter.check_display_metadata ctx cf.cf_meta
	in
	let check_field sc cf =
		check_field sc cf;
		List.iter (check_field sc) cf.cf_overloads
	in
	match c.cl_kind with
	| KAbstractImpl a ->
		let sa = find_abstract_by_position decls c.cl_name_pos in
		let check_field = check_field sa in
		List.iter check_field c.cl_ordered_statics;
	| _ ->
		let sc = find_class_by_position decls c.cl_name_pos in
		ignore(Typeload.type_type_params ctx TPHType c.cl_path (fun() -> c.cl_params) null_pos sc.d_params);
		List.iter (function
			| (HExtends(ct,p) | HImplements(ct,p)) when display_position#enclosed_in p ->
				ignore(Typeload.load_instance ~allow_display:true ctx (ct,p) false)
			| _ ->
				()
		) sc.d_flags;
		let check_field = check_field sc in
		List.iter check_field c.cl_ordered_statics;
		List.iter check_field c.cl_ordered_fields;
		Option.may check_field c.cl_constructor

let check_display_enum ctx decls en =
	let se = find_enum_by_position decls en.e_name_pos in
	ignore(Typeload.type_type_params ctx TPHType en.e_path (fun() -> en.e_params) null_pos se.d_params);
	PMap.iter (fun _ ef ->
		if display_position#enclosed_in ef.ef_pos then begin
			let sef = find_enum_field_by_position se ef.ef_name_pos in
			ignore(TypeloadModule.load_enum_field ctx en (TEnum (en,extract_param_types en.e_params)) (ref false) (ref 0) sef)
		end
	) en.e_constrs

let check_display_typedef ctx decls td =
	let st = find_typedef_by_position decls td.t_name_pos in
	ignore(Typeload.type_type_params ctx TPHType td.t_path (fun() -> td.t_params) null_pos st.d_params);
	ignore(Typeload.load_complex_type ctx true st.d_data)

let check_display_abstract ctx decls a =
	let sa = find_abstract_by_position decls a.a_name_pos in
	ignore(Typeload.type_type_params ctx TPHType a.a_path (fun() -> a.a_params) null_pos sa.d_params);
	List.iter (function
		| (AbOver(ct,p) | AbFrom(ct,p) | AbTo(ct,p)) when display_position#enclosed_in p ->
			ignore(Typeload.load_complex_type ctx true (ct,p))
		| _ ->
			()
	) sa.d_flags

let check_display_module_fields ctx decls m =
	Option.may (fun c ->
		List.iter (fun cf ->
			if display_position#enclosed_in cf.cf_pos then begin
				let cff = find_module_static_by_position decls cf.cf_name_pos in
				actually_check_display_field ctx c (TypeloadModule.field_of_static_definition cff cf.cf_pos) cf.cf_pos;
			end;
			DisplayEmitter.check_display_metadata ctx cf.cf_meta
		) c.cl_ordered_statics
	) m.m_statics

let check_display_module ctx decls m =
	let imports = List.filter (function
		| (EImport _ | EUsing _),_ -> true
		| _ -> false
	) decls in
	let imports = TypeloadModule.handle_import_hx ctx m imports null_pos in
	let ctx = TypeloadModule.type_types_into_module ctx m imports null_pos in
	List.iter (fun md ->
		let infos = t_infos md in
		if display_position#enclosed_in infos.mt_name_pos then
			DisplayEmitter.display_module_type ctx md infos.mt_name_pos;
		begin if display_position#enclosed_in infos.mt_pos then match md with
		| TClassDecl c ->
			check_display_class ctx decls c
		| TEnumDecl en ->
			check_display_enum ctx decls en
		| TTypeDecl td ->
			check_display_typedef ctx decls td
		| TAbstractDecl a ->
			check_display_abstract ctx decls a
		end;
		DisplayEmitter.check_display_metadata ctx infos.mt_meta
	) m.m_types;
	check_display_module_fields ctx decls m

let check_display_file ctx cs =
	match ctx.com.cache with
	| Some cc ->
		begin try
			let p = DisplayPosition.display_position#get in
			let cfile = cc#find_file (ctx.com.file_keys#get p.pfile) in
			let path = (cfile.c_package,get_module_name_of_cfile p.pfile cfile) in
			TypeloadParse.PdiHandler.handle_pdi ctx.com cfile.c_pdi;
			(* We have to go through type_module_hook because one of the module's dependencies could be
			   invalid (issue #8991). *)
			begin match !TypeloadModule.type_module_hook ctx path null_pos with
			| None -> raise Not_found
			| Some m -> check_display_module ctx cfile.c_decls m
			end
		with Not_found ->
			let fkey = DisplayPosition.display_position#get_file_key in
			(* force parsing again : if the completion point have been changed *)
			cs#remove_files fkey;
			cs#taint_modules fkey "check_display_file";
		end
	| None ->
		()