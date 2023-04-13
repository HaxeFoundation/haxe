open Globals
open Ast
open Typecore
open Type
open TypeloadModule
open TypeloadFields

exception Fail of string

type retyping_context = {
	typer : typer;
	print_stack : string list;
}

let fail rctx s =
	let stack = String.concat " " (List.rev rctx.print_stack) in
	raise (Fail (Printf.sprintf "%s: %s" stack s))

let disable_typeloading rctx ctx f =
	let old = ctx.g.load_only_cached_modules in
	ctx.g.load_only_cached_modules <- true;
	try
		Std.finally (fun () -> ctx.g.load_only_cached_modules <- old) f ()
	with (Error.Error { err_message = Module_not_found path }) ->
		fail rctx (Printf.sprintf "Could not load [Module %s]" (s_type_path path))

let pair_type th t = match th with
	| None ->
		TExprToExpr.convert_type t,null_pos
	| Some t ->
		t

let pair_class_field rctx ctx cctx fctx cf cff p =
	match cff.cff_kind with
	| FFun fd ->
		let targs,tret = match follow cf.cf_type with
			| TFun(args,ret) ->
				args,ret
			| _ ->
				fail rctx "Type change"
		in
		let args = try
			List.map2 (fun (name,opt,meta,th,eo) (_,_,t) ->
				(name,opt,meta,Some (pair_type th t),eo)
			) fd.f_args targs
		with Invalid_argument _ ->
			fail rctx "Type change"
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
		let args,ret = disable_typeloading rctx ctx load_args_ret in
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
		let t = disable_typeloading rctx ctx (fun () -> load_variable_type_hint ctx fctx eo (pos cff.cff_name) th) in
		(fun () ->
			cf.cf_type <- t;
			TypeBinding.bind_var ctx cctx fctx cf eo;
			if ctx.com.display.dms_full_typing then
				remove_class_field_flag cf CfPostProcessed;
		)

let pair_classes rctx context_init c d p =
	let rctx = {rctx with
		print_stack = (Printf.sprintf "[Class %s]" (s_type_path c.cl_path)) :: rctx.print_stack
	} in
	c.cl_restore();
	(* TODO: What do we do with build macros? *)
	let cctx = create_class_context c context_init p in
	let ctx = create_typer_context_for_class rctx.typer cctx p in
	let _ =
		let rctx = {rctx with
			print_stack = (Printf.sprintf "[Relations]") :: rctx.print_stack
		} in
		let has_extends = ref false in
		let implements = ref c.cl_implements in
		List.iter (function
			| HExtends(path,p) ->
				has_extends := true;
				begin match c.cl_super with
				| None ->
					fail rctx (Printf.sprintf "parent %s appeared" (Ast.Printer.s_complex_type_path "" (path,p)))
				| Some(c,tl) ->
					let th = pair_type (Some(CTPath path,p)) (TInst(c,tl)) in
					ignore (disable_typeloading rctx ctx (fun () -> Typeload.load_complex_type ctx false th))
				end
			| HImplements(path,p) ->
				begin match !implements with
					| (c,tl) :: rest ->
						(* TODO: I think this should somehow check if it's actually the same interface. There could be cases
						   where the order changes or something like that... Maybe we can compare the loaded type.
						   However, this doesn't matter until we start retyping invalidated modules.
						*)
						implements := rest;
						let th = pair_type (Some(CTPath path,p)) (TInst(c,tl)) in
						ignore (disable_typeloading rctx ctx (fun () -> Typeload.load_complex_type ctx false th));
					| [] ->
						fail rctx (Printf.sprintf "interface %s appeared" (Ast.Printer.s_complex_type_path "" (path,p)))
				end
			| _ ->
				()
		) d.d_flags;
		(* TODO: There are probably cases where the compiler generates a cl_super even though it's not in syntax *)
		if not !has_extends then begin match c.cl_super with
			| None -> ()
			| Some(c,_) -> fail rctx (Printf.sprintf "parent %s disappeared" (s_type_path c.cl_path))
		end;
		begin match !implements with
			| (c,_) :: _ -> fail rctx (Printf.sprintf "interface %s disappeared" (s_type_path c.cl_path))
			| [] -> ()
		end
	in
	let fl = List.map (fun cff ->
		let name = fst cff.cff_name in
		let rctx = {rctx with
			print_stack = (Printf.sprintf "[Field %s]" name) :: rctx.print_stack
		} in
		let display_modifier = Typeload.check_field_access ctx cff in
		let fctx = create_field_context cctx cff ctx.is_display_file display_modifier in
		let cf = match fctx.field_kind with
			| FKConstructor ->
				begin match c.cl_constructor with
				| None ->
					fail rctx "Constructor not found"
				| Some cf ->
					cf
				end
			| FKNormal ->
				begin try
					PMap.find name (if fctx.is_static then c.cl_statics else c.cl_fields)
				with Not_found ->
					fail rctx "Field not found"
				end
			| FKInit ->
				fail rctx "TODO"
		in
		pair_class_field rctx ctx cctx fctx cf cff p
	) d.d_data in
	fl @ [fun () -> TypeloadFields.finalize_class ctx cctx]

let pair_enums ctx rctx en d =
	let ctx = { ctx with type_params = en.e_params } in
	let rctx = {rctx with
		print_stack = (Printf.sprintf "[Enum %s]" (s_type_path en.e_path)) :: rctx.print_stack
	} in
	List.iter (fun eff ->
		let name = fst eff.ec_name in
		let rctx = {rctx with
			print_stack = (Printf.sprintf "[Field %s]" name) :: rctx.print_stack
		} in
		let ef = try
			PMap.find name en.e_constrs
		with Not_found ->
			fail rctx "Field not found"
		in
		let th = pair_type eff.ec_type ef.ef_type in
		ignore (disable_typeloading rctx ctx (fun () -> Typeload.load_complex_type ctx false th))
	) d.d_data;
	[]

let pair_typedefs ctx rctx td d =
	let rctx = {rctx with
		print_stack = (Printf.sprintf "[Typedef %s]" (s_type_path td.t_path)) :: rctx.print_stack
	} in
	let ctx = { ctx with type_params = td.t_params } in
	ignore (disable_typeloading rctx ctx (fun () -> Typeload.load_complex_type ctx false d.d_data));
	[]

let pair_abstracts ctx rctx context_init a d p =
	let rctx = {rctx with
		print_stack = (Printf.sprintf "[Abstract %s]" (s_type_path a.a_path)) :: rctx.print_stack
	} in
	match a.a_impl with
	| Some c ->
		c.cl_restore();
		let cctx = create_class_context c context_init p in
		let ctx = create_typer_context_for_class rctx.typer cctx p in
		let fl = List.map (fun cff ->
			let cff = TypeloadFields.transform_abstract_field2 ctx a cff in
			let name = fst cff.cff_name in
			let rctx = {rctx with
				print_stack = (Printf.sprintf "[Field %s]" name) :: rctx.print_stack
			} in
			let display_modifier = Typeload.check_field_access ctx cff in
			let fctx = create_field_context cctx cff ctx.is_display_file display_modifier in
			let cf = try
				PMap.find name c.cl_statics
			with Not_found ->
				fail rctx "Field not found"
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
		print_stack = [Printf.sprintf "[Module %s]" (s_type_path m.m_path)];
	} in
	(* log rctx 0 (Printf.sprintf "Retyping module %s" (s_type_path m.m_path)); *)
	let context_init = new TypeloadFields.context_init in
	let find_type name = try
		List.find (fun t -> snd (t_infos t).mt_path = name) ctx.m.curmod.m_types
	with Not_found ->
		fail rctx (Printf.sprintf "Type %s not found" name)
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
				pair_abstracts ctx rctx context_init a d p
			| _ ->
				fail rctx "?"
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
