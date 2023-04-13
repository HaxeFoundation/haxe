(*
	The Haxe Compiler
	Copyright (C) 2005-2019  Haxe Foundation

	This program is free software; you can redistribute it and/or
	modify it under the terms of the GNU General Public License
	as published by the Free Software Foundation; either version 2
	of the License, or (at your option) any later version.

	This program is distributed in the hope that it will be useful,
	but WITHOUT ANY WARRANTY; without even the implied warranty of
	MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
	GNU General Public License for more details.

	You should have received a copy of the GNU General Public License
	along with this program; if not, write to the Free Software
	Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA  02110-1301, USA.
 *)

open Ast
open DisplayTypes.DisplayMode
open Common
open Type
open Typecore
open Error
open Globals

module Eval = struct
	include EvalEncode
	include EvalDecode
	include EvalValue
	include EvalContext
	include EvalMain
end

module InterpImpl = Eval (* Hlmacro *)

module Interp = struct
	module BuiltApi = MacroApi.MacroApiImpl(InterpImpl)
	include InterpImpl
	include BuiltApi
end

let macro_enable_cache = ref false
let macro_interp_cache = ref None

let safe_decode com v expected t p f =
	try
		f ()
	with MacroApi.Invalid_expr | EvalContext.RunTimeException _ ->
		let path = [dump_path com;"decoding_error"] in
		let ch = Path.create_file false ".txt" [] path  in
		let errors = Interp.handle_decoding_error (output_string ch) v t in
		List.iter (fun (s,i) -> Printf.fprintf ch "\nline %i: %s" i s) (List.rev errors);
		close_out ch;
		raise_typing_error (Printf.sprintf "Expected %s but got %s (see %s.txt for details)" expected (Interp.value_string v) (String.concat "/" path)) p

let get_type_patch ctx t sub =
	let new_patch() =
		{ tp_type = None; tp_remove = false; tp_meta = [] }
	in
	let path = Ast.parse_path t in
	let h, tp = (try
		Hashtbl.find ctx.g.type_patches path
	with Not_found ->
		let h = Hashtbl.create 0 in
		let tp = new_patch() in
		Hashtbl.add ctx.g.type_patches path (h,tp);
		h, tp
	) in
	match sub with
	| None -> tp
	| Some k ->
		try
			Hashtbl.find h k
		with Not_found ->
			let tp = new_patch() in
			Hashtbl.add h k tp;
			tp

let macro_timer com l =
	Timer.timer (if Common.defined com Define.MacroTimes then ("macro" :: l) else ["macro"])

let typing_timer ctx need_type f =
	let t = Timer.timer ["typing"] in
	let old = ctx.com.error_ext and oldp = ctx.pass and oldlocals = ctx.locals in
	let restore_report_mode = disable_report_mode ctx.com in
	(*
		disable resumable errors... unless we are in display mode (we want to reach point of completion)
	*)
	(* if ctx.com.display.dms_kind = DMNone then ctx.com.error <- (fun e -> raise_error e); *) (* TODO: review this... *)
	ctx.com.error_ext <- (fun err -> raise_error { err with err_from_macro = true });

	if need_type && ctx.pass < PTypeField then begin
		ctx.pass <- PTypeField;
		flush_pass ctx PBuildClass "typing_timer";
	end;
	let exit() =
		t();
		ctx.com.error_ext <- old;
		ctx.pass <- oldp;
		ctx.locals <- oldlocals;
		restore_report_mode ();
	in
	try
		let r = f() in
		exit();
		r
	with Error err ->
		exit();
		Interp.compiler_error err
	| WithTypeError err ->
		exit();
		Interp.compiler_error err
	| e ->
		exit();
		raise e

let make_macro_com_api com p =
	{
		MacroApi.pos = p;
		get_com = (fun () -> com);
		get_macro_stack = (fun () ->
			let envs = Interp.call_stack (Interp.get_eval (Interp.get_ctx ())) in
			let envs = match envs with
				| _ :: envs -> envs (* Skip call to getMacroStack() *)
				| _ -> envs
			in
			List.map (fun (env:Interp.env) -> {pfile = EvalHash.rev_hash env.env_info.pfile;pmin = env.env_leave_pmin; pmax = env.env_leave_pmax}) envs
		);
		init_macros_done = (fun () -> com.stage >= CInitMacrosDone);
		get_type = (fun s ->
			Interp.exc_string "unsupported"
		);
		resolve_type = (fun t p ->
			Interp.exc_string "unsupported"
		);
		get_module = (fun s ->
			Interp.exc_string "unsupported"
		);
		after_init_macros = (fun f ->
			com.callbacks#add_after_init_macros (fun () ->
				let t = macro_timer com ["afterInitMacros"] in
				f ();
				t()
			)
		);
		after_typing = (fun f ->
			com.callbacks#add_after_typing (fun tl ->
				let t = macro_timer com ["afterTyping"] in
				f tl;
				t()
			)
		);
		on_generate = (fun f b ->
			(if b then com.callbacks#add_before_save else com.callbacks#add_after_save) (fun() ->
				let t = macro_timer com ["onGenerate"] in
				f (List.map type_of_module_type com.types);
				t()
			)
		);
		after_generate = (fun f ->
			com.callbacks#add_after_generation (fun() ->
				let t = macro_timer com ["afterGenerate"] in
				f();
				t()
			)
		);
		on_type_not_found = (fun f ->
			com.load_extern_type <- com.load_extern_type @ ["onTypeNotFound",fun path p ->
				let td = f (s_type_path path) in
				if td = Interp.vnull then
					None
				else
					let (pack,name),tdef,p = Interp.decode_type_def td in
					Some (pack,[tdef,p])
			];
		);
		parse_string = (fun s p inl ->
			(* TODO: typing_timer *)
			Interp.exc_string "unsupported"
		);
		parse = (fun entry s ->
			match ParserEntry.parse_string entry com.defines s null_pos raise_typing_error false with
			| ParseSuccess(r,_,_) -> r
			| ParseError(_,(msg,p),_) -> Parser.error msg p
		);
		type_expr = (fun e ->
			Interp.exc_string "unsupported"
		);
		flush_context = (fun f ->
			Interp.exc_string "unsupported"
		);
		store_typed_expr = (fun te ->
			let p = te.epos in
			snd (Typecore.store_typed_expr com te p)
		);
		allow_package = (fun v -> Common.allow_package com v);
		type_patch = (fun t f s v ->
			Interp.exc_string "unsupported"
		);
		meta_patch = (fun m t f s p ->
			Interp.exc_string "unsupported"
		);
		set_js_generator = (fun gen ->
			com.js_gen <- Some (fun() ->
				Path.mkdir_from_path com.file;
				let js_ctx = Genjs.alloc_ctx com (get_es_version com) in
				let t = macro_timer com ["jsGenerator"] in
				gen js_ctx;
				t()
			);
		);
		get_local_type = (fun() ->
			Interp.exc_string "unsupported"
		);
		get_expected_type = (fun() ->
			Interp.exc_string "unsupported"
		);
		get_call_arguments = (fun() ->
			Interp.exc_string "unsupported"
		);
		get_local_method = (fun() ->
			Interp.exc_string "unsupported"
		);
		get_local_using = (fun() ->
			Interp.exc_string "unsupported"
		);
		get_local_imports = (fun() ->
			Interp.exc_string "unsupported"
		);
		get_local_vars = (fun () ->
			Interp.exc_string "unsupported"
		);
		get_build_fields = (fun() ->
			Interp.exc_string "unsupported"
		);
		define_type = (fun v mdep ->
			Interp.exc_string "unsupported"
		);
		define_module = (fun m types imports usings ->
			Interp.exc_string "unsupported"
		);
		module_dependency = (fun mpath file ->
			Interp.exc_string "unsupported"
		);
		current_module = (fun() ->
			Interp.exc_string "unsupported"
		);
		use_cache = (fun() ->
			!macro_enable_cache
		);
		format_string = (fun s p ->
			Interp.exc_string "unsupported"
		);
		cast_or_unify = (fun t e p ->
			Interp.exc_string "unsupported"
		);
		add_global_metadata = (fun s1 s2 config p ->
			Interp.exc_string "unsupported"
		);
		add_module_check_policy = (fun sl il b i ->
			Interp.exc_string "unsupported"
		);
		register_define = (fun s data -> Define.register_user_define com.user_defines s data);
		register_metadata = (fun s data -> Meta.register_user_meta com.user_metas s data);
		decode_expr = Interp.decode_expr;
		encode_expr = Interp.encode_expr;
		encode_ctype = Interp.encode_ctype;
		decode_type = Interp.decode_type;
		display_error = display_error com;
		with_imports = (fun imports usings f ->
			Interp.exc_string "unsupported"
		);
		with_options = (fun opts f ->
			Interp.exc_string "unsupported"
		);
		info = (fun ?(depth=0) msg p ->
			com.info ~depth msg p
		);
		warning = (fun ?(depth=0) w msg p ->
			Interp.exc_string "unsupported"
		);
	}

let make_macro_api ctx p =
	let parse_expr_string s p inl =
		typing_timer ctx false (fun() ->
			match ParserEntry.parse_expr_string ctx.com.defines s p raise_typing_error inl with
				| ParseSuccess(data,true,_) when inl -> data (* ignore errors when inline-parsing in display file *)
				| ParseSuccess(data,_,_) -> data
				| ParseError _ -> raise MacroApi.Invalid_expr)
	in
	let parse_metadata s p =
		try
			match ParserEntry.parse_string Grammar.parse_meta ctx.com.defines s null_pos raise_typing_error false with
			| ParseSuccess(meta,_,_) -> meta
			| ParseError(_,_,_) -> raise_typing_error "Malformed metadata string" p
		with _ ->
			raise_typing_error "Malformed metadata string" p
	in
	let com_api = make_macro_com_api ctx.com p in
	{
		com_api with
		MacroApi.get_type = (fun s ->
			typing_timer ctx false (fun() ->
				let path = parse_path s in
				let tp = match List.rev (fst path) with
					| s :: sl when String.length s > 0 && (match s.[0] with 'A'..'Z' -> true | _ -> false) ->
						mk_type_path ~sub:(snd path) (List.rev sl,s)
					| _ ->
						mk_type_path path
				in
				try
					let m = Some (Typeload.load_instance ctx (tp,p) true) in
					m
				with Error { err_message = Module_not_found _; err_pos = p2 } when p == p2 ->
					None
			)
		);
		MacroApi.resolve_type = (fun t p ->
			typing_timer ctx false (fun() -> Typeload.load_complex_type ctx false (t,p))
		);
		MacroApi.get_module = (fun s ->
			typing_timer ctx false (fun() ->
				let path = parse_path s in
				let m = List.map type_of_module_type (TypeloadModule.load_module ctx path p).m_types in
				m
			)
		);
		MacroApi.parse_string = parse_expr_string;
		MacroApi.type_expr = (fun e ->
			typing_timer ctx true (fun() -> type_expr ctx e WithType.value)
		);
		MacroApi.flush_context = (fun f ->
			typing_timer ctx true f
		);
		MacroApi.type_patch = (fun t f s v ->
			typing_timer ctx false (fun() ->
				let v = (match v with None -> None | Some s ->
					match ParserEntry.parse_string Grammar.parse_complex_type ctx.com.defines s null_pos raise_typing_error false with
					| ParseSuccess((ct,_),_,_) -> Some ct
					| ParseError(_,(msg,p),_) -> Parser.error msg p (* p is null_pos, but we don't have anything else here... *)
				) in
				let tp = get_type_patch ctx t (Some (f,s)) in
				match v with
				| None -> tp.tp_remove <- true
				| Some t -> tp.tp_type <- Some t
			);
		);
		MacroApi.meta_patch = (fun m t f s p ->
			let ml = parse_metadata m p in
			let tp = get_type_patch ctx t (match f with None -> None | Some f -> Some (f,s)) in
			tp.tp_meta <- tp.tp_meta @ (List.map (fun (m,el,_) -> (m,el,p)) ml);
		);
		MacroApi.get_local_type = (fun() ->
			match ctx.get_build_infos() with
			| Some (mt,tl,_) ->
				Some (match mt with
					| TClassDecl c -> TInst (c,tl)
					| TEnumDecl e -> TEnum (e,tl)
					| TTypeDecl t -> TType (t,tl)
					| TAbstractDecl a -> TAbstract(a,tl)
				)
			| _ ->
				if ctx.curclass == null_class then
					None
				else
					Some (TInst (ctx.curclass,[]))
		);
		MacroApi.get_expected_type = (fun() ->
			match ctx.with_type_stack with
				| (WithType.WithType(t,_)) :: _ -> Some t
				| _ -> None
		);
		MacroApi.get_call_arguments = (fun() ->
			match ctx.call_argument_stack with
				| [] -> None
				| el :: _ -> Some el
		);
		MacroApi.get_local_method = (fun() ->
			ctx.curfield.cf_name;
		);
		MacroApi.get_local_using = (fun() ->
			List.map fst ctx.m.module_using;
		);
		MacroApi.get_local_imports = (fun() ->
			ctx.m.import_statements;
		);
		MacroApi.get_local_vars = (fun () ->
			ctx.locals;
		);
		MacroApi.get_build_fields = (fun() ->
			match ctx.get_build_infos() with
			| None -> Interp.vnull
			| Some (_,_,fields) -> Interp.encode_array (List.map Interp.encode_field fields)
		);
		MacroApi.define_type = (fun v mdep ->
			let cttype = mk_type_path ~sub:"TypeDefinition" (["haxe";"macro"],"Expr") in
			let mctx = (match ctx.g.macros with None -> die "" __LOC__ | Some (_,mctx) -> mctx) in
			let ttype = Typeload.load_instance mctx (cttype,p) false in
			let f () = Interp.decode_type_def v in
			let m, tdef, pos = safe_decode ctx.com v "TypeDefinition" ttype p f in
			let has_native_meta = match tdef with
				| EClass d -> Meta.has Meta.Native d.d_meta
				| EEnum d -> Meta.has Meta.Native d.d_meta
				| ETypedef d -> Meta.has Meta.Native d.d_meta
				| EAbstract d -> Meta.has Meta.Native d.d_meta
				| _ -> false
			in
			let add is_macro ctx =
				let mdep = Option.map_default (fun s -> TypeloadModule.load_module ctx (parse_path s) pos) ctx.m.curmod mdep in
				let mnew = TypeloadModule.type_module ctx ~dont_check_path:(has_native_meta) m (Path.UniqueKey.lazy_path mdep.m_extra.m_file) [tdef,pos] pos in
				mnew.m_extra.m_kind <- if is_macro then MMacro else MFake;
				add_dependency mnew mdep;
				ctx.com.module_nonexistent_lut#clear;
			in
			add false ctx;
			(* if we are adding a class which has a macro field, we also have to add it to the macro context (issue #1497) *)
			if not ctx.com.is_macro_context then match tdef with
			| EClass c when List.exists (fun cff -> (Meta.has Meta.Macro cff.cff_meta || List.mem_assoc AMacro cff.cff_access)) c.d_data ->
				add true mctx
			| _ ->
				()
		);
		MacroApi.define_module = (fun m types imports usings ->
			let types = List.map (fun v ->
				let _, tdef, pos = (try Interp.decode_type_def v with MacroApi.Invalid_expr -> Interp.exc_string "Invalid type definition") in
				tdef, pos
			) types in
			let pos = (match types with [] -> null_pos | (_,p) :: _ -> p) in
			let imports = List.map (fun (il,ik) -> EImport(il,ik),pos) imports in
			let usings = List.map (fun tp ->
				let sl = tp.tpackage @ [tp.tname] @ (match tp.tsub with None -> [] | Some s -> [s]) in
				EUsing (List.map (fun s -> s,null_pos) sl),pos
			) usings in
			let types = imports @ usings @ types in
			let mpath = Ast.parse_path m in
			begin try
				let m = ctx.com.module_lut#find mpath in
				ignore(TypeloadModule.type_types_into_module ctx m types pos)
			with Not_found ->
				let mnew = TypeloadModule.type_module ctx mpath (Path.UniqueKey.lazy_path ctx.m.curmod.m_extra.m_file) types pos in
				mnew.m_extra.m_kind <- MFake;
				add_dependency mnew ctx.m.curmod;
				ctx.com.module_nonexistent_lut#clear;
			end
		);
		MacroApi.module_dependency = (fun mpath file ->
			let m = typing_timer ctx false (fun() ->
				let old_deps = ctx.m.curmod.m_extra.m_deps in
				let m = TypeloadModule.load_module ctx (parse_path mpath) p in
				ctx.m.curmod.m_extra.m_deps <- old_deps;
				m
			) in
			add_dependency m (create_fake_module ctx file);
		);
		MacroApi.current_module = (fun() ->
			ctx.m.curmod
		);
		MacroApi.format_string = (fun s p ->
			ctx.g.do_format_string ctx s p
		);
		MacroApi.cast_or_unify = (fun t e p ->
			typing_timer ctx true (fun () ->
				try
					ignore(AbstractCast.cast_or_unify_raise ctx t e p);
					true
				with Error { err_message = Unify _ } ->
					false
			)
		);
		MacroApi.add_global_metadata = (fun s1 s2 config p ->
			let meta = parse_metadata s2 p in
			List.iter (fun (m,el,_) ->
				let m = (m,el,p) in
				ctx.g.global_metadata <- (ExtString.String.nsplit s1 ".",m,config) :: ctx.g.global_metadata;
			) meta;
		);
		MacroApi.add_module_check_policy = (fun sl il b i ->
			let add ctx =
				ctx.g.module_check_policies <- (List.fold_left (fun acc s -> (ExtString.String.nsplit s ".",List.map Obj.magic il,b) :: acc) ctx.g.module_check_policies sl);
				ctx.com.module_lut#iter (fun _ m -> m.m_extra.m_check_policy <- TypeloadModule.get_policy ctx.g m.m_path);
			in
			let add_macro ctx = match ctx.g.macros with
				| None -> ()
				| Some(_,mctx) -> add mctx;
			in
			let open CompilationCache in
			match Obj.magic i with
			| NormalContext -> add ctx
			| MacroContext -> add_macro ctx
			| NormalAndMacroContext -> add ctx; add_macro ctx;
		);
		MacroApi.with_imports = (fun imports usings f ->
			let old_globals = ctx.m.module_globals in
			let old_imports = ctx.m.module_imports in
			let old_using = ctx.m.module_using in
			let run () =
				let context_init = new TypeloadFields.context_init in
				List.iter (fun (path,mode) ->
					ImportHandling.init_import ctx context_init path mode null_pos
				) imports;
				List.iter (fun path ->
					ImportHandling.init_using ctx context_init path null_pos
				) usings;
				context_init#run;
				f()
			in
			let restore () =
				ctx.m.module_globals <- old_globals;
				ctx.m.module_imports <- old_imports;
				ctx.m.module_using <- old_using;
			in
			Std.finally restore run ()
		);
		MacroApi.with_options = (fun opts f ->
			let old_inline = ctx.allow_inline in
			let old_transform = ctx.allow_transform in
			(match opts.opt_inlining with
			| None -> ()
			| Some v -> ctx.allow_inline <- v);
			(match opts.opt_transform with
			| None -> ()
			| Some v -> ctx.allow_transform <- v);
			let restore() =
				ctx.allow_inline <- old_inline;
				ctx.allow_transform <- old_transform;
			in
			Std.finally restore f ()
		);
		MacroApi.warning = (fun ?(depth=0) w msg p ->
			warning ~depth ctx w msg p
		);
	}

let rec init_macro_interp mctx mint =
	let p = null_pos in
	ignore(TypeloadModule.load_module mctx (["haxe";"macro"],"Expr") p);
	ignore(TypeloadModule.load_module mctx (["haxe";"macro"],"Type") p);
	Interp.init mint;
	if !macro_enable_cache && not (Common.defined mctx.com Define.NoMacroCache) then begin
		macro_interp_cache := Some mint;
	end

and flush_macro_context mint mctx =
	let t = macro_timer mctx.com ["flush"] in
	let mctx = (match mctx.g.macros with None -> die "" __LOC__ | Some (_,mctx) -> mctx) in
	Finalization.finalize mctx;
	let _, types, modules = Finalization.generate mctx in
	mctx.com.types <- types;
	mctx.com.Common.modules <- modules;
	(* we should maybe ensure that all filters in Main are applied. Not urgent atm *)
	let expr_filters = [
		"local_statics",Filters.LocalStatic.run mctx;
		"VarLazifier",VarLazifier.apply mctx.com;
		"handle_abstract_casts",AbstractCast.handle_abstract_casts mctx;
		"Exceptions",Exceptions.filter mctx;
		"captured_vars",CapturedVars.captured_vars mctx.com;
	] in
	(*
		some filters here might cause side effects that would break compilation server.
		let's save the minimal amount of information we need
	*)
	let minimal_restore t =
		if (t_infos t).mt_module.m_extra.m_processed = 0 then
			(t_infos t).mt_module.m_extra.m_processed <- mctx.com.compilation_step;

		match t with
		| TClassDecl c ->
			let mk_field_restore f =
				let e = f.cf_expr in
				(fun () -> f.cf_expr <- e)
			in
			let meta = c.cl_meta
			and path = c.cl_path
			and field_restores = List.map mk_field_restore c.cl_ordered_fields
			and static_restores = List.map mk_field_restore c.cl_ordered_statics
			and ctor_restore = Option.map mk_field_restore c.cl_constructor
			in
			c.cl_restore <- (fun() ->
				c.cl_meta <- meta;
				c.cl_path <- path;
				c.cl_descendants <- [];
				Option.may (fun fn -> fn()) ctor_restore;
				List.iter (fun fn -> fn()) field_restores;
				List.iter (fun fn -> fn()) static_restores;
			);
		| _ ->
			()
	in
	let type_filters = [
		Filters.remove_generic_base;
		Exceptions.patch_constructors mctx;
		(fun mt -> Filters.add_field_inits mctx.curclass.cl_path (RenameVars.init mctx.com) mctx.com mt);
		minimal_restore;
		Filters.apply_native_paths
	] in
	let ready = fun t ->
		Filters.apply_filters_once mctx expr_filters t;
		List.iter (fun f -> f t) type_filters
	in
	(try Interp.add_types mint types ready
	with Error err -> t(); raise (Fatal_error err));
	t()

let create_macro_interp api mctx =
	let com2 = mctx.com in
	let mint, init = (match !macro_interp_cache with
		| None ->
			let mint = Interp.create com2 api true in
			Interp.select mint;
			mint, (fun() -> init_macro_interp mctx mint)
		| Some mint ->
			Interp.do_reuse mint api;
			mint, (fun() -> ())
	) in
	let on_error = com2.error_ext in
	com2.error_ext <- (fun err ->
		Interp.set_error (Interp.get_ctx()) true;
		macro_interp_cache := None;
		on_error { err with err_from_macro = true }
	);
	let on_warning = com2.warning in
	com2.warning <- (fun ?(depth=0) ?(from_macro=false) w options msg p ->
		on_warning ~depth ~from_macro:true w options msg p
	);
	let on_info = com2.info in
	com2.info <- (fun ?(depth=0) ?(from_macro=false) msg p ->
		on_info ~depth ~from_macro:true msg p
	);
	(* mctx.g.core_api <- ctx.g.core_api; // causes some issues because of optional args and Null type in Flash9 *)
	init();
	let init = (fun() -> Interp.select mint) in
	mctx.g.macros <- Some (init,mctx);
	init

let create_macro_context com =
	let com2 = Common.clone com true in
	com.get_macros <- (fun() -> Some com2);
	com2.package_rules <- PMap.empty;
	com2.main_class <- None;
	(* Inherit most display settings, but require normal typing. *)
	com2.display <- {com.display with dms_kind = DMNone; dms_full_typing = true; dms_force_macro_typing = true; dms_inline = true; };
	com2.class_path <- List.filter (fun s -> not (ExtString.String.exists s "/_std/")) com2.class_path;
	let name = platform_name !Globals.macro_platform in
	com2.class_path <- List.map (fun p -> p ^ name ^ "/_std/") com2.std_path @ com2.class_path;
	let defines = adapt_defines_to_macro_context com2.defines; in
	com2.defines.values <- defines.values;
	com2.defines.defines_signature <- None;
	Common.init_platform com2 !Globals.macro_platform;
	let mctx = !create_context_ref com2 in
	mctx.is_display_file <- false;
	CommonCache.lock_signature com2 "get_macro_context";
	mctx

let get_macro_context ctx =
	match ctx.g.macros with
	| Some (select,ctx) ->
		select();
		ctx
	| None ->
		let mctx = create_macro_context ctx.com in
		let api = make_macro_api ctx null_pos in
		let init = create_macro_interp api mctx in
		ctx.g.macros <- Some (init,mctx);
		mctx.g.macros <- Some (init,mctx);
		mctx

let load_macro_module mctx com cpath display p =
	let m = (try com.type_to_module#find cpath with Not_found -> cpath) in
	(* Temporarily enter display mode while typing the macro. *)
	let old = mctx.com.display in
	if display then mctx.com.display <- com.display;
	let mloaded = TypeloadModule.load_module mctx m p in
	mctx.m <- {
		curmod = mloaded;
		module_imports = [];
		module_using = [];
		module_globals = PMap.empty;
		wildcard_packages = [];
		import_statements = [];
	};
	mloaded,(fun () -> mctx.com.display <- old)

let load_macro'' com mctx display cpath f p =
	let mint = Interp.get_ctx() in
	try mctx.com.cached_macros#find (cpath,f) with Not_found ->
		let t = macro_timer com ["typing";s_type_path cpath ^ "." ^ f] in
		let mpath, sub = (match List.rev (fst cpath) with
			| name :: pack when name.[0] >= 'A' && name.[0] <= 'Z' -> (List.rev pack,name), Some (snd cpath)
			| _ -> cpath, None
		) in
		let mloaded,restore = load_macro_module mctx com mpath display p in
		let cl, meth =
			try
				if sub <> None || mloaded.m_path <> cpath then raise Not_found;
				match mloaded.m_statics with
				| None -> raise Not_found
				| Some c ->
					Finalization.finalize mctx;
					c, PMap.find f c.cl_statics
			with Not_found ->
				let name = Option.default (snd mpath) sub in
				let path = fst mpath, name in
				let mt = try List.find (fun t2 -> (t_infos t2).mt_path = path) mloaded.m_types with Not_found -> raise_typing_error_ext (make_error (Type_not_found (mloaded.m_path,name,Not_defined)) p) in
				match mt with
				| TClassDecl c ->
					Finalization.finalize mctx;
					c, (try PMap.find f c.cl_statics with Not_found -> raise_typing_error ("Method " ^ f ^ " not found on class " ^ s_type_path cpath) p)
				| _ -> raise_typing_error "Macro should be called on a class" p
		in
		let meth = (match follow meth.cf_type with TFun (args,ret) -> (args,ret,cl,meth),mloaded | _ -> raise_typing_error "Macro call should be a method" p) in
		restore();
		if not com.is_macro_context then flush_macro_context mint mctx;
		mctx.com.cached_macros#add (cpath,f) meth;
		mctx.m <- {
			curmod = null_module;
			module_imports = [];
			module_using = [];
			module_globals = PMap.empty;
			wildcard_packages = [];
			import_statements = [];
		};
		t();
		meth

let load_macro' ctx display cpath f p =
	(* TODO: The only reason this nonsense is here is because this is the signature
	   that typer.di_load_macro wants, and the only reason THAT exists is the stupid
	   voodoo stuff in displayToplevel.ml *)
	fst (load_macro'' ctx.com (get_macro_context ctx) display cpath f p)

let load_macro ctx display cpath f p =
	let api = make_macro_api ctx p in
	let mctx = get_macro_context ctx in
	let meth,mloaded = load_macro'' ctx.com mctx display cpath f p in
	let _,_,{cl_path = cpath},_ = meth in
	let call args =
		add_dependency ctx.m.curmod mloaded;
		if ctx.com.verbose then Common.log ctx.com ("Calling macro " ^ s_type_path cpath ^ "." ^ f ^ " (" ^ p.pfile ^ ":" ^ string_of_int (Lexer.get_error_line p) ^ ")");
		let t = macro_timer ctx.com ["execution";s_type_path cpath ^ "." ^ f] in
		incr stats.s_macros_called;
		let r = Interp.call_path (Interp.get_ctx()) ((fst cpath) @ [snd cpath]) f args api in
		t();
		if ctx.com.verbose then Common.log ctx.com ("Exiting macro " ^ s_type_path cpath ^ "." ^ f);
		r
	in
	mctx, meth, call

type macro_arg_type =
	| MAExpr
	| MAFunction
	| MAOther

let type_macro ctx mode cpath f (el:Ast.expr list) p =
	let mctx, (margs,mret,mclass,mfield), call_macro = load_macro ctx (mode = MDisplay) cpath f p in
	let margs =
		(*
			Replace "rest:haxe.Rest<Expr>" in macro signatures with "rest:Array<Expr>".
			This allows to avoid handling special cases for rest args in macros during typing.
		*)
		match List.rev margs with
		| (n,o,t) :: margs_rev ->
			(match follow t with
			| TAbstract ({ a_path = ["haxe"],"Rest" }, [t1]) -> List.rev ((n,o,mctx.t.tarray t1) :: margs_rev)
			| _ -> margs)
		| _ -> margs
	in
	let mpos = mfield.cf_pos in
	let ctexpr = mk_type_path (["haxe";"macro"],"Expr") in
	let expr = Typeload.load_instance mctx (ctexpr,p) false in
	(match mode with
	| MDisplay ->
		raise Exit (* We don't have to actually call the macro. *)
	| MExpr ->
		unify mctx mret expr mpos;
	| MBuild ->
		let params = [TPType (CTPath (mk_type_path ~sub:"Field" (["haxe";"macro"],"Expr")),null_pos)] in
		let ctfields = mk_type_path ~params ([],"Array") in
		let tfields = Typeload.load_instance mctx (ctfields,p) false in
		unify mctx mret tfields mpos
	| MMacroType ->
		let cttype = mk_type_path (["haxe";"macro"],"Type") in
		let ttype = Typeload.load_instance mctx (cttype,p) false in
		try
			unify_raise mret ttype mpos;
			(* TODO: enable this again in the future *)
			(* warning ctx WDeprecated "Returning Type from @:genericBuild macros is deprecated, consider returning ComplexType instead" p; *)
		with Error { err_message = Unify _ } ->
			let cttype = mk_type_path ~sub:"ComplexType" (["haxe";"macro"],"Expr") in
			let ttype = Typeload.load_instance mctx (cttype,p) false in
			unify_raise mret ttype mpos;
	);
	(*
		if the function's last argument is of Array<Expr>, split the argument list and use [] for unify_call_args
	*)
	let el,el2 = match List.rev margs with
		| (_,_,TInst({cl_path=([], "Array")},[e])) :: rest when (try Type.type_eq EqStrict e expr; true with Unify_error _ -> false) ->
			let rec loop (acc1,acc2) el1 el2 = match el1,el2 with
				| [],[] ->
					List.rev acc1, List.rev acc2
				| [], e2 :: [] ->
					(List.rev ((EArrayDecl [],p) :: acc1), [])
				| [], _ ->
					(* not enough arguments, will be handled by unify_call_args *)
					List.rev acc1, List.rev acc2
				| e1 :: l1, e2 :: [] ->
					loop (((EArrayDecl [],p) :: acc1), [e1]) l1 []
				| e1 :: l1, [] ->
					loop (acc1, e1 :: acc2) l1 []
				| e1 :: l1, e2 :: l2 ->
					loop (e1 :: acc1, acc2) l1 l2
			in
			loop ([],[]) el margs
		| _ ->
			el,[]
	in
	let args =
		(*
			force default parameter types to haxe.macro.Expr, and if success allow to pass any value type since it will be encoded
		*)
		let eargs = List.map (fun (n,o,t) ->
			try unify_raise t expr p; (n, o, t_dynamic), MAExpr
			with Error { err_message = Unify _ } -> match follow t with
				| TFun _ ->
					(n,o,t), MAFunction
				| _ ->
					(n,o,t), MAOther
			) margs in
		(*
			this is quite tricky here : we want to use unify_call_args which will type our AST expr
			but we want to be able to get it back after it's been padded with nulls
		*)
		let index = ref (-1) in
		let constants = List.map (fun e ->
			let p = snd e in
			let e =
				let rec is_function e = match fst e with
					| EFunction _ -> true
					| EParenthesis e1 | ECast(e1,_) | EMeta(_,e1) -> is_function e1
					| _ -> false
				in
				if Texpr.is_constant_value ctx.com.basic e then
					(* temporarily disable format strings processing for macro call argument typing since we want to pass raw constants *)
					let rec loop e =
						match e with
						| (EConst (String (s,SSingleQuotes)),p) -> (EConst (String (s,SDoubleQuotes)), p)
						| _ -> Ast.map_expr loop e
					in
					loop e
				else if is_function e then
					(* If we pass a function expression we don't want to type it as part of `unify_call_args` because that result just gets
					   discarded. Use null here so it passes, then do the actual typing in the MAFunction part below. *)
					(EConst (Ident "null"),p)
				else
					(* if it's not a constant, let's make something that is typed as haxe.macro.Expr - for nice error reporting *)
					(ECheckType ((EConst (Ident "null"),p), (CTPath ctexpr,p)), p)
			in
			(* let's track the index by doing [e][index] (we will keep the expression type this way) *)
			incr index;
			(EArray ((EArrayDecl [e],p),(EConst (Int (string_of_int (!index), None)),p)),p)
		) el in
		let elt = fst (CallUnification.unify_call_args mctx constants (List.map fst eargs) t_dynamic p false false false) in
		List.map2 (fun ((n,_,t),mct) e ->
			let e, et = (match e.eexpr with
				(* get back our index and real expression *)
				| TArray ({ eexpr = TArrayDecl [e] }, { eexpr = TConst (TInt index) }) -> List.nth el (Int32.to_int index), e
				(* added by unify_call_args *)
				| TConst TNull -> (EConst (Ident "null"),e.epos), e
				| _ -> die "" __LOC__
			) in
			let ictx = Interp.get_ctx() in
			match mct with
			| MAExpr ->
				Interp.encode_expr e
			| MAFunction ->
				let e = type_expr mctx e (WithType.with_argument t n) in
				unify mctx e.etype t e.epos;
				begin match Interp.eval_expr ictx e with
				| Some v -> v
				| None -> Interp.vnull
				end
			| MAOther -> match Interp.eval_expr ictx et with
				| None -> Interp.vnull
				| Some v -> v
		) eargs elt
	in
	let args = match el2 with
		| [] -> args
		| _ -> (match List.rev args with _::args -> List.rev args | [] -> []) @ [Interp.encode_array (List.map Interp.encode_expr el2)]
	in
	let call() =
		match call_macro args with
		| None -> None
		| Some v ->
			let expected,process = match mode with
				| MExpr | MDisplay ->
					"Expr",(fun () -> Some (Interp.decode_expr v))
				| MBuild ->
					"Array<Field>",(fun () ->
						let fields = if v = Interp.vnull then
								(match ctx.get_build_infos() with
								| None -> die "" __LOC__
								| Some (_,_,fields) -> fields)
							else
								List.map Interp.decode_field (Interp.decode_array v)
						in
						Some (EVars [mk_evar ~t:(CTAnonymous fields,p) ("fields",null_pos)],p)
					)
				| MMacroType ->
					"ComplexType",(fun () ->
						let t = if v = Interp.vnull then
							spawn_monomorph ctx p
						else try
							let ct = Interp.decode_ctype v in
							Typeload.load_complex_type ctx false ct;
						with MacroApi.Invalid_expr | EvalContext.RunTimeException _ ->
							Interp.decode_type v
						in
						ctx.ret <- t;
						Some (EBlock [],p)
					)
			in
			safe_decode ctx.com v expected mret p process
	in
	let e = if ctx.com.is_macro_context then
		Some (EThrow((EConst(String("macro-in-macro",SDoubleQuotes))),p),p)
	else
		call()
	in
	e

let call_macro ctx path meth args p =
	let mctx, (margs,_,mclass,mfield), call = load_macro ctx false path meth p in
	mctx.curclass <- null_class;
	let el, _ = CallUnification.unify_call_args mctx args margs t_dynamic p false false false in
	call (List.map (fun e -> try Interp.make_const e with Exit -> raise_typing_error "Argument should be a constant" e.epos) el)

let call_init_macro ctx e =
	let p = { pfile = "--macro " ^ e; pmin = -1; pmax = -1 } in
	let e = try
		if String.get e (String.length e - 1) = ';' then raise_typing_error "Unexpected ;" p;
		begin match ParserEntry.parse_expr_string ctx.com.defines e p raise_typing_error false with
		| ParseSuccess(data,_,_) -> data
		| ParseError(_,(msg,p),_) -> (Parser.error msg p)
		end
	with err ->
		display_error ctx.com ("Could not parse `" ^ e ^ "`") p;
		raise err
	in
	match fst e with
	| ECall (e,args) ->
		let rec loop e =
			match fst e with
			| EField (e,f,_) -> f :: loop e
			| EConst (Ident i) -> [i]
			| _ -> raise_typing_error "Invalid macro call" p
		in
		let path, meth = (match loop e with
		| [meth] -> (["haxe";"macro"],"Compiler"), meth
		| [meth;"server"] -> (["haxe";"macro"],"CompilationServer"), meth
		| meth :: cl :: path -> (List.rev path,cl), meth
		| _ -> raise_typing_error "Invalid macro call" p) in
		ignore(call_macro ctx path meth args p);
	| _ ->
		raise_typing_error "Invalid macro call" p

let interpret ctx =
	let mctx = Interp.create ctx.com (make_macro_api ctx null_pos) false in
	Interp.add_types mctx ctx.com.types (fun t -> ());
	match ctx.com.main with
		| None -> ()
		| Some e -> ignore(Interp.eval_expr mctx e)

let setup() =
	Interp.setup Interp.macro_api

let type_stored_expr ctx e1 =
	let id = match e1 with (EConst (Int (s, _)),_) -> int_of_string s | _ -> die "" __LOC__ in
	get_stored_typed_expr ctx.com id
