open Globals
open Common
open Type
open Typecore
open DisplayPosition
open CompletionItem
open ClassFieldOrigin

let get_field_scope_and_origin c cf is_static is_constructor =
	let scope,cf = match c.cl_kind with
		| KAbstractImpl _ ->
			if Meta.has Meta.Impl cf.cf_meta then
				(if cf.cf_name = "_new" then
					CFSConstructor, {cf with cf_name = "new"}
				else
					CFSMember, cf)
			else
				CFSStatic, cf;
		| _ ->
			(if is_static then
				CFSStatic
			else if is_constructor then
				CFSConstructor
			else
				CFSMember), cf;
	in
	let origin = match c.cl_kind with
		| KAbstractImpl a -> Self (TAbstractDecl a)
		| _ -> Self (TClassDecl c)
	in
	cf,scope,origin

let find_type_by_position cc p =
	let m = cc#find_module_by_file (Path.unique_full_path p.pfile) in
	List.find (fun mt ->
		(t_infos mt).mt_name_pos = p
	) m.m_types

let check_display_field ctx c is_static is_constructor cf =
	let cf,scope,origin = get_field_scope_and_origin c cf is_static is_constructor in
	DisplayEmitter.maybe_display_field ctx origin scope cf cf.cf_name_pos

let check_display_class ctx c =
	let check_field is_static is_constructor cf =
		if display_position#enclosed_in cf.cf_pos then
			check_display_field ctx c is_static is_constructor cf (* TODO: abstract ctor? *)
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
				check_display_class ctx c
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