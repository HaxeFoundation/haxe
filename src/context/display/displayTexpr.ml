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

let find_class_by_position cfile p =
	let rec loop dl = match dl with
		| (EClass c,_) :: dl when pos c.d_name = p ->
			c
		| _ :: dl ->
			loop dl
		| [] ->
			raise Not_found
	in
	loop cfile.c_decls

let check_display_field ctx sc c cf =
	let cff = find_field_by_position sc cf.cf_name_pos in
	let ctx,cctx = TypeloadFields.create_class_context ctx c (fun () -> ()) cf.cf_pos in
	let ctx,fctx = TypeloadFields.create_field_context (ctx,cctx) c cff in
	let cf = TypeloadFields.init_field (ctx,cctx,fctx) cff in
	ignore(follow cf.cf_type)

let check_display_class ctx cc cfile c =
	let sc = find_class_by_position cfile c.cl_name_pos in
	List.iter (function
		| (HExtends(ct,p) | HImplements(ct,p)) when display_position#enclosed_in p ->
			ignore(Typeload.load_instance ~allow_display:true ctx (ct,p) false)
		| _ ->
			()
	) sc.d_flags;
	let check_field cf =
		if display_position#enclosed_in cf.cf_pos then
			check_display_field ctx sc c cf;
		DisplayEmitter.check_display_metadata ctx cf.cf_meta
	in
	if display_position#enclosed_in c.cl_name_pos then
		DisplayEmitter.display_module_type ctx (TClassDecl c) c.cl_name_pos;
	List.iter check_field c.cl_ordered_statics;
	List.iter check_field c.cl_ordered_fields;
	Option.may check_field c.cl_constructor

let check_display_module ctx cc cfile m =
	let imports = List.filter (function
		| (EImport _ | EUsing _),_ -> true
		| _ -> false
	) cfile.c_decls in
	let imports = TypeloadModule.handle_import_hx ctx m imports null_pos in
	let ctx = TypeloadModule.type_types_into_module ctx m imports null_pos in
	List.iter (fun md -> match md with
		| TClassDecl c ->
			if display_position#enclosed_in c.cl_pos then
				check_display_class ctx cc cfile c;
			DisplayEmitter.check_display_metadata ctx c.cl_meta
		| _ ->
			() (* TODO *)
	) m.m_types

let check_display_file ctx cs =
	match ctx.com.cache with
	| Some cc ->
		begin try
			(* TODO: diagnostics currently relies on information collected during typing. *)
			begin match ctx.com.display.dms_kind with
				| DMDiagnostics _ -> raise Not_found
				| _ -> ()
			end;
			let p = DisplayPosition.display_position#get in
			let cfile = cc#find_file (Path.unique_full_path p.pfile) in
			let path = (cfile.c_package,get_module_name_of_cfile p.pfile cfile) in
			let m = cc#find_module path in
			check_display_module ctx cc cfile m
		with Not_found ->
			(* Special case for diagnostics: It's not treated as a display mode, but we still want to invalidate the
				current file in order to run diagnostics on it again. *)
			if ctx.com.display.dms_display || (match ctx.com.display.dms_kind with DMDiagnostics _ -> true | _ -> false) then begin
				let file = (DisplayPosition.display_position#get).pfile in
				(* force parsing again : if the completion point have been changed *)
				cs#remove_files file;
				cs#taint_modules file;
			end;
		end
	| None ->
		()