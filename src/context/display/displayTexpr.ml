open Globals
open Common
open Ast
open Type
open Typecore
open DisplayPosition
open CompletionItem
open CompilationServer
open ClassFieldOrigin

let find_type_by_position cc p =
	let m = cc#find_module_by_file (Path.unique_full_path p.pfile) in
	List.find (fun mt ->
		(t_infos mt).mt_name_pos = p
	) m.m_types

let find_field_by_position cc p =
	let cfile = cc#find_file (Path.unique_full_path p.pfile) in
	let rec loop imports dl = match dl with
		| (EClass c,pc) :: _ when pc.pmin <= p.pmin && pc.pmax >= p.pmax ->
			List.find (fun cff ->
				if pos cff.cff_name = p then true else false
			) c.d_data,List.rev imports
		| ((EImport _ | EUsing _),p as d) :: dl ->
			loop (d :: imports) dl
		| _ :: dl ->
			loop imports dl
		| [] ->
			raise Not_found
	in
	loop [] cfile.c_decls

let check_display_field ctx cc c is_static is_constructor cf =
	let cff,imports = find_field_by_position cc cf.cf_name_pos in
	let ctx = TypeloadModule.type_types_into_module ctx c.cl_module imports cf.cf_pos in
	ctx.is_display_file <- true;
	let ctx,cctx = TypeloadFields.create_class_context ctx c (fun () -> ()) cf.cf_pos in
	let ctx,fctx = TypeloadFields.create_field_context (ctx,cctx) c cff in
	let cf = TypeloadFields.init_field (ctx,cctx,fctx) cff in
	ignore(follow cf.cf_type)

let check_display_class ctx cc c =
	let check_field is_static is_constructor cf =
		if display_position#enclosed_in cf.cf_pos then
			check_display_field ctx cc c is_static is_constructor cf; (* TODO: abstract ctor? *)
		DisplayEmitter.check_display_metadata ctx cf.cf_meta
	in
	if display_position#enclosed_in c.cl_name_pos then
		DisplayEmitter.display_module_type ctx (TClassDecl c) c.cl_name_pos;
	List.iter (check_field true false) c.cl_ordered_statics;
	List.iter (check_field false false) c.cl_ordered_fields;
	Option.may (check_field false true) c.cl_constructor

let check_display_module ctx cc m =
	List.iter (fun (p,pn) ->
		if display_position#enclosed_in p then begin
			let mt = find_type_by_position cc pn in
			let t = type_of_module_type mt in
			DisplayEmitter.display_type ctx t p
		end;
	) m.m_extra.m_display.m_type_hints;
	List.iter (fun md -> match md with
		| TClassDecl c ->
			if display_position#enclosed_in c.cl_pos then
				check_display_class ctx cc c;
			DisplayEmitter.check_display_metadata ctx c.cl_meta
		| _ ->
			() (* TODO *)
	) m.m_types

let check_display_file ctx cs =
	match ctx.com.cache with
	| Some cc ->
		begin try
			let m = cc#find_module_by_file (DisplayPosition.display_position#get).pfile in
			check_display_module ctx cc m
		with Not_found ->
			print_endline "fell through";
			Printexc.print_backtrace stdout;
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