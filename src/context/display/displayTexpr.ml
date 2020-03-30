open Globals
open Common
open Ast
open Type
open Typecore
open DisplayPosition
open CompletionItem
open CompilationServer
open ClassFieldOrigin

let find_field_by_position sc p =
	List.find (fun cff ->
		if pos cff.cff_name = p then true else false
	) sc.d_data

let find_enum_field_by_position sc p =
	List.find (fun eff ->
		if pos eff.ec_name = p then true else false
	) sc.d_data

let find_class_by_position cfile p =
	let rec loop dl = match dl with
		| (EClass c,_) :: dl when pos c.d_name = p -> c
		| _ :: dl -> loop dl
		| [] -> raise Not_found
	in
	loop cfile.c_decls

let find_enum_by_position cfile p =
	let rec loop dl = match dl with
		| (EEnum en,_) :: dl when pos en.d_name = p -> en
		| _ :: dl -> loop dl
		| [] -> raise Not_found
	in
	loop cfile.c_decls

let find_typedef_by_position cfile p =
	let rec loop dl = match dl with
		| (ETypedef td,_) :: dl when pos td.d_name = p -> td
		| _ :: dl -> loop dl
		| [] -> raise Not_found
	in
	loop cfile.c_decls

let find_abstract_by_position cfile p =
	let rec loop dl = match dl with
		| (EAbstract a,_) :: dl when pos a.d_name = p -> a
		| _ :: dl -> loop dl
		| [] -> raise Not_found
	in
	loop cfile.c_decls

let check_display_field ctx sc c cf =
	let cff = find_field_by_position sc cf.cf_name_pos in
	let context_init = new TypeloadFields.context_init in
	let ctx,cctx = TypeloadFields.create_class_context ctx c context_init cf.cf_pos in
	let cff = TypeloadFields.transform_field (ctx,cctx) c cff (ref []) (pos cff.cff_name) in
	let ctx,fctx = TypeloadFields.create_field_context (ctx,cctx) c cff in
	let cf = TypeloadFields.init_field (ctx,cctx,fctx) cff in
	flush_pass ctx PTypeField "check_display_field";
	ignore(follow cf.cf_type)

let check_display_class ctx cc cfile c =
	let check_field sc cf =
		if display_position#enclosed_in cf.cf_pos then
			check_display_field ctx sc c cf;
		DisplayEmitter.check_display_metadata ctx cf.cf_meta
	in
	match c.cl_kind with
	| KAbstractImpl a ->
		let sa = find_abstract_by_position cfile c.cl_name_pos in
		let check_field = check_field sa in
		List.iter check_field c.cl_ordered_statics;
	| _ ->
		let sc = find_class_by_position cfile c.cl_name_pos in
		ignore(Typeload.type_type_params ctx c.cl_path (fun() -> c.cl_params) null_pos sc.d_params);
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

let check_display_enum ctx cc cfile en =
	let se = find_enum_by_position cfile en.e_name_pos in
	ignore(Typeload.type_type_params ctx en.e_path (fun() -> en.e_params) null_pos se.d_params);
	PMap.iter (fun _ ef ->
		if display_position#enclosed_in ef.ef_pos then begin
			let sef = find_enum_field_by_position se ef.ef_name_pos in
			ignore(TypeloadModule.load_enum_field ctx en (TEnum (en,List.map snd en.e_params)) (ref false) (ref 0) sef)
		end
	) en.e_constrs

let check_display_typedef ctx cc cfile td =
	let st = find_typedef_by_position cfile td.t_name_pos in
	ignore(Typeload.type_type_params ctx td.t_path (fun() -> td.t_params) null_pos st.d_params);
	ignore(Typeload.load_complex_type ctx true st.d_data)

let check_display_abstract ctx cc cfile a =
	let sa = find_abstract_by_position cfile a.a_name_pos in
	ignore(Typeload.type_type_params ctx a.a_path (fun() -> a.a_params) null_pos sa.d_params);
	List.iter (function
		| (AbOver(ct,p) | AbFrom(ct,p) | AbTo(ct,p)) when display_position#enclosed_in p ->
			ignore(Typeload.load_complex_type ctx true (ct,p))
		| _ ->
			()
	) sa.d_flags

let check_display_module ctx cc cfile m =
	let imports = List.filter (function
		| (EImport _ | EUsing _),_ -> true
		| _ -> false
	) cfile.c_decls in
	let imports = TypeloadModule.handle_import_hx ctx m imports null_pos in
	let ctx = TypeloadModule.type_types_into_module ctx m imports null_pos in
	List.iter (fun md ->
		let infos = t_infos md in
		if display_position#enclosed_in infos.mt_name_pos then
			DisplayEmitter.display_module_type ctx md infos.mt_name_pos;
		begin if display_position#enclosed_in infos.mt_pos then match md with
		| TClassDecl c ->
			check_display_class ctx cc cfile c
		| TEnumDecl en ->
			check_display_enum ctx cc cfile en
		| TTypeDecl td ->
			check_display_typedef ctx cc cfile td
		| TAbstractDecl a ->
			check_display_abstract ctx cc cfile a
		end;
		DisplayEmitter.check_display_metadata ctx infos.mt_meta
	) m.m_types

let check_display_file ctx cs =
	match ctx.com.cache with
	| Some cc ->
		begin try
			let p = DisplayPosition.display_position#get in
			let cfile = cc#find_file (Path.unique_full_path p.pfile) in
			let path = (cfile.c_package,get_module_name_of_cfile p.pfile cfile) in
			TypeloadParse.PdiHandler.handle_pdi ctx.com cfile.c_pdi;
			(* We have to go through type_module_hook because one of the module's dependencies could be
			   invalid (issue #8991). *)
			begin match !TypeloadModule.type_module_hook ctx path null_pos with
			| None -> raise Not_found
			| Some m -> check_display_module ctx cc cfile m
			end
		with Not_found ->
			if ctx.com.display.dms_display then begin
				let file = (DisplayPosition.display_position#get).pfile in
				(* force parsing again : if the completion point have been changed *)
				cs#remove_files file;
				cs#taint_modules file;
			end;
		end
	| None ->
		()