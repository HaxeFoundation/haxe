(*
	The Haxe Compiler
	Copyright (C) 2005-2016  Haxe Foundation

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
open Common.DisplayMode
open Common
open Type
open Typecore
open Error
open Globals

(* module Interp = Hlmacro *)

module InterpImpl = Interp
module Interp = struct
	module BuiltApi = MacroApi.MacroApiImpl(InterpImpl)
	include InterpImpl
	include BuiltApi
end

let macro_enable_cache = ref false
let macro_interp_cache = ref None
let macro_interp_on_reuse = ref []
let macro_interp_reused = ref false

let delayed_macro_result = ref ((fun() -> assert false) : unit -> unit -> Interp.value)
let unify_call_args_ref = ref (fun _ _ _ _ _ _ _-> assert false)
let unify_call_args a b c d e f g : (texpr list * t) = !unify_call_args_ref a b c d e f g

let get_next_stored_typed_expr_id =
	let uid = ref 0 in
	(fun() -> incr uid; !uid)

let get_stored_typed_expr com id =
	let e = PMap.find id com.stored_typed_exprs in
	Texpr.duplicate_tvars e

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

let macro_timer ctx l =
	Common.timer (if Common.defined ctx.com Define.MacroTimes then ("macro" :: l) else ["macro"])

let typing_timer ctx need_type f =
	let t = Common.timer ["typing"] in
	let old = ctx.com.error and oldp = ctx.pass in
	(*
		disable resumable errors... unless we are in display mode (we want to reach point of completion)
	*)
	(*if ctx.com.display = DMNone then ctx.com.error <- (fun e p -> raise (Error(Custom e,p)));*) (* TODO: review this... *)
	ctx.com.error <- (fun e p -> raise (Error(Custom e,p)));
	if need_type && ctx.pass < PTypeField then ctx.pass <- PTypeField;
	let exit() =
		t();
		ctx.com.error <- old;
		ctx.pass <- oldp;
	in
	try
		let r = f() in
		exit();
		r
	with Error (ekind,p) ->
			exit();
			Interp.compiler_error (error_msg ekind) p
		| WithTypeError (l,p) ->
			exit();
			Interp.compiler_error (error_msg l) p
		| e ->
			exit();
			raise e

let load_macro_ref : (typer -> bool -> path -> string -> pos -> (typer * ((string * bool * t) list * t * tclass * Type.tclass_field) * (Interp.value list -> Interp.value option))) ref = ref (fun _ _ _ _ -> assert false)

let make_macro_api ctx p =
	let parse_expr_string s p inl =
		typing_timer ctx false (fun() -> try Parser.parse_expr_string ctx.com s p error inl with Exit -> raise MacroApi.Invalid_expr)
	in
	{
		MacroApi.pos = p;
		MacroApi.get_com = (fun() -> ctx.com);
		MacroApi.get_type = (fun s ->
			typing_timer ctx true (fun() ->
				let path = parse_path s in
				let tp = match List.rev (fst path) with
					| s :: sl when String.length s > 0 && (match s.[0] with 'A'..'Z' -> true | _ -> false) ->
						{ tpackage = List.rev sl; tname = s; tparams = []; tsub = Some (snd path) }
					| _ ->
						{ tpackage = fst path; tname = snd path; tparams = []; tsub = None }
				in
				try
					let m = Some (Typeload.load_instance ctx (tp,null_pos) true p) in
					m
				with Error (Module_not_found _,p2) when p == p2 ->
					None
			)
		);
		MacroApi.resolve_type = (fun t p ->
			typing_timer ctx true (fun() -> Typeload.load_complex_type ctx false p (t,null_pos))
		);
		MacroApi.get_module = (fun s ->
			typing_timer ctx true (fun() ->
				let path = parse_path s in
				let m = List.map type_of_module_type (Typeload.load_module ctx path p).m_types in
				m
			)
		);
		MacroApi.after_typing = (fun f ->
			Common.add_typing_filter ctx.com (fun tl ->
				let t = macro_timer ctx ["afterTyping"] in
				f tl;
				t()
			)
		);
		MacroApi.on_generate = (fun f ->
			Common.add_filter ctx.com (fun() ->
				let t = macro_timer ctx ["onGenerate"] in
				f (List.map type_of_module_type ctx.com.types);
				t()
			)
		);
		MacroApi.after_generate = (fun f ->
			Common.add_final_filter ctx.com (fun() ->
				let t = macro_timer ctx ["afterGenerate"] in
				f();
				t()
			)
		);
		MacroApi.on_type_not_found = (fun f ->
			ctx.com.load_extern_type <- ctx.com.load_extern_type @ [fun path p ->
				let td = f (s_type_path path) in
				if td = Interp.vnull then
					None
				else
					let (pack,name),tdef,p = Interp.decode_type_def td in
					Some (name,(pack,[tdef,p]))
			];
		);
		MacroApi.parse_string = parse_expr_string;
		MacroApi.type_expr = (fun e ->
			typing_timer ctx true (fun() -> type_expr ctx e Value)
		);
		MacroApi.type_macro_expr = (fun e ->
			let e = typing_timer ctx true (fun() -> type_expr ctx e Value) in
			let rec loop e = match e.eexpr with
				| TField(_,FStatic(c,({cf_kind = Method _} as cf))) -> ignore(!load_macro_ref ctx false c.cl_path cf.cf_name e.epos)
				| _ -> Type.iter loop e
			in
			loop e;
			e
		);
		MacroApi.store_typed_expr = (fun te ->
			let p = te.epos in
			let id = get_next_stored_typed_expr_id() in
			ctx.com.stored_typed_exprs <- PMap.add id te ctx.com.stored_typed_exprs;
			let eid = (EConst (Int (string_of_int id))), p in
			(EMeta ((Meta.StoredTypedExpr,[],p), eid)), p
		);
		MacroApi.allow_package = (fun v -> Common.allow_package ctx.com v);
		MacroApi.type_patch = (fun t f s v ->
			typing_timer ctx false (fun() ->
				let v = (match v with None -> None | Some s ->
					match Parser.parse_string ctx.com ("typedef T = " ^ s) null_pos error false with
					| _,[ETypedef { d_data = ct },_] -> Some ct
					| _ -> assert false
				) in
				let tp = get_type_patch ctx t (Some (f,s)) in
				match v with
				| None -> tp.tp_remove <- true
				| Some _ -> tp.tp_type <- Option.map fst v
			);
		);
		MacroApi.meta_patch = (fun m t f s ->
			let m = (match Parser.parse_string ctx.com (m ^ " typedef T = T") null_pos error false with
				| _,[ETypedef t,_] -> t.d_meta
				| _ -> assert false
			) in
			let tp = get_type_patch ctx t (match f with None -> None | Some f -> Some (f,s)) in
			tp.tp_meta <- tp.tp_meta @ m;
		);
		MacroApi.set_js_generator = (fun gen ->
			let js_ctx = Genjs.alloc_ctx ctx.com in
			ctx.com.js_gen <- Some (fun() ->
				let t = macro_timer ctx ["jsGenerator"] in
				gen js_ctx;
				t()
			);
		);
		MacroApi.get_local_type = (fun() ->
			match ctx.g.get_build_infos() with
			| Some (mt,tl,_) ->
				Some (match mt with
					| TClassDecl c -> TInst (c,tl)
					| TEnumDecl e -> TEnum (e,tl)
					| TTypeDecl t -> TType (t,tl)
					| TAbstractDecl a -> TAbstract(a,tl))
			| None ->
				if ctx.curclass == null_class then
					None
				else
					Some (TInst (ctx.curclass,[]))
		);
		MacroApi.get_expected_type = (fun() ->
			match ctx.with_type_stack with
				| (WithType t) :: _ -> Some t
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
			ctx.m.module_imports;
		);
		MacroApi.get_local_vars = (fun () ->
			ctx.locals;
		);
		MacroApi.get_build_fields = (fun() ->
			match ctx.g.get_build_infos() with
			| None -> Interp.vnull
			| Some (_,_,fields) -> Interp.enc_array (List.map Interp.encode_field fields)
		);
		MacroApi.get_pattern_locals = (fun e t ->
			!get_pattern_locals_ref ctx e t
		);
		MacroApi.define_type = (fun v ->
			let m, tdef, pos = (try Interp.decode_type_def v with MacroApi.Invalid_expr -> Interp.exc_string "Invalid type definition") in
			let add is_macro ctx =
				let mnew = Typeload.type_module ctx m ctx.m.curmod.m_extra.m_file [tdef,pos] pos in
				mnew.m_extra.m_kind <- if is_macro then MMacro else MFake;
				add_dependency mnew ctx.m.curmod;
			in
			add false ctx;
			(* if we are adding a class which has a macro field, we also have to add it to the macro context (issue #1497) *)
			if not ctx.in_macro then match tdef,ctx.g.macros with
			| EClass c,Some (_,mctx) when List.exists (fun cff -> (Meta.has Meta.Macro cff.cff_meta || List.mem AMacro cff.cff_access)) c.d_data ->
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
				let m = Hashtbl.find ctx.g.modules mpath in
				ignore(Typeload.type_types_into_module ctx m types pos)
			with Not_found ->
				let mnew = Typeload.type_module ctx mpath ctx.m.curmod.m_extra.m_file types pos in
				mnew.m_extra.m_kind <- MFake;
				add_dependency mnew ctx.m.curmod;
			end
		);
		MacroApi.module_dependency = (fun mpath file ismacro ->
			let m = typing_timer ctx false (fun() -> Typeload.load_module ctx (parse_path mpath) p) in
			if ismacro then
				m.m_extra.m_macro_calls <- file :: List.filter ((<>) file) m.m_extra.m_macro_calls
			else
				add_dependency m (create_fake_module ctx file);
		);
		MacroApi.current_module = (fun() ->
			ctx.m.curmod
		);
		MacroApi.current_macro_module = (fun () -> assert false);
		MacroApi.delayed_macro = (fun i ->
			let mctx = (match ctx.g.macros with None -> assert false | Some (_,mctx) -> mctx) in
			let f = (try DynArray.get mctx.g.delayed_macros i with _ -> failwith "Delayed macro retrieve failure") in
			f();
			let ret = !delayed_macro_result in
			delayed_macro_result := (fun() -> assert false);
			ret
		);
		MacroApi.use_cache = (fun() ->
			!macro_enable_cache
		);
		MacroApi.format_string = (fun s p ->
			ctx.g.do_format_string ctx s p
		);
		MacroApi.cast_or_unify = (fun t e p ->
			AbstractCast.cast_or_unify_raise ctx t e p
		);
		MacroApi.add_global_metadata = (fun s1 s2 config ->
			let meta = (match Parser.parse_string ctx.com (s2 ^ " typedef T = T") null_pos error false with
				| _,[ETypedef t,_] -> t.d_meta
				| _ -> assert false
			) in
			List.iter (fun m ->
				ctx.g.global_metadata <- (ExtString.String.nsplit s1 ".",m,config) :: ctx.g.global_metadata;
			) meta;
		);
		MacroApi.add_module_check_policy = (fun sl il b i ->
			let add ctx =
				ctx.g.module_check_policies <- (List.fold_left (fun acc s -> (ExtString.String.nsplit s ".",List.map Obj.magic il,b) :: acc) ctx.g.module_check_policies sl);
				Hashtbl.iter (fun _ m -> m.m_extra.m_check_policy <- Typeload.get_policy ctx m.m_path) ctx.g.modules;
			in
			let add_macro ctx = match ctx.g.macros with
				| None -> ()
				| Some(_,mctx) -> add mctx;
			in
			match Obj.magic i with
			| CompilationServer.NormalContext -> add ctx
			| CompilationServer.MacroContext -> add_macro ctx
			| CompilationServer.NormalAndMacroContext -> add ctx; add_macro ctx;
		);
		MacroApi.on_reuse = (fun f ->
			macro_interp_on_reuse := f :: !macro_interp_on_reuse
		);
	}

let rec init_macro_interp ctx mctx mint =
	let p = null_pos in
	ignore(Typeload.load_module mctx (["haxe";"macro"],"Expr") p);
	ignore(Typeload.load_module mctx (["haxe";"macro"],"Type") p);
	flush_macro_context mint ctx;
	Interp.init mint;
	if !macro_enable_cache && not (Common.defined mctx.com Define.NoMacroCache) then begin
		macro_interp_cache := Some mint;
		macro_interp_on_reuse := [];
		macro_interp_reused := true;
	end

and flush_macro_context mint ctx =
	let t = macro_timer ctx ["flush"] in
	let mctx = (match ctx.g.macros with None -> assert false | Some (_,mctx) -> mctx) in
	ctx.g.do_finalize mctx;
	let _, types, modules = ctx.g.do_generate mctx in
	mctx.com.types <- types;
	mctx.com.Common.modules <- modules;
	let check_reuse() =
		if !macro_interp_reused then
			true
		else if not (List.for_all (fun f -> f())  !macro_interp_on_reuse) then
			false
		else begin
			macro_interp_reused := true;
			true;
		end
	in
	(* if one of the type we are using has been modified, we need to create a new macro context from scratch *)
	let mint = if not (Interp.can_reuse mint types && check_reuse()) then begin
		let com2 = mctx.com in
		let mint = Interp.create com2 (make_macro_api ctx Globals.null_pos) in
		let macro = ((fun() -> Interp.select mint), mctx) in
		ctx.g.macros <- Some macro;
		mctx.g.macros <- Some macro;
		init_macro_interp ctx mctx mint;
		mint
	end else mint in
	(* we should maybe ensure that all filters in Main are applied. Not urgent atm *)
	let expr_filters = [AbstractCast.handle_abstract_casts mctx; Filters.captured_vars mctx.com; Filters.rename_local_vars mctx] in

	(*
		some filters here might cause side effects that would break compilation server.
		let's save the minimal amount of information we need
	*)
	let minimal_restore t =
		match t with
		| TClassDecl c ->
			let meta = c.cl_meta in
			let path = c.cl_path in
			c.cl_restore <- (fun() -> c.cl_meta <- meta; c.cl_path <- path);
		| _ ->
			()
	in
	let type_filters = [
		Filters.add_field_inits mctx;
		minimal_restore;
		Filters.apply_native_paths mctx
	] in
	let ready = fun t ->
		Filters.apply_filters_once mctx expr_filters t;
		List.iter (fun f -> f t) type_filters
	in
	(try Interp.add_types mint types ready
	with Error (e,p) -> t(); raise (Fatal_error(error_msg e,p)));
	t();
	Filters.next_compilation()

let create_macro_interp ctx mctx =
	let com2 = mctx.com in
	let mint, init = (match !macro_interp_cache with
		| None ->
			let mint = Interp.create com2 (make_macro_api ctx null_pos) in
			mint, (fun() -> init_macro_interp ctx mctx mint)
		| Some mint ->
			macro_interp_reused := false;
			Interp.do_reuse mint (make_macro_api ctx null_pos);
			mint, (fun() -> ())
	) in
	let on_error = com2.error in
	com2.error <- (fun e p ->
		Interp.set_error (Interp.get_ctx()) true;
		macro_interp_cache := None;
		on_error e p
	);
	let macro = ((fun() -> Interp.select mint), mctx) in
	ctx.g.macros <- Some macro;
	mctx.g.macros <- Some macro;
	(* mctx.g.core_api <- ctx.g.core_api; // causes some issues because of optional args and Null type in Flash9 *)
	init()

let get_macro_context ctx p =
	let api = make_macro_api ctx p in
	match ctx.g.macros with
	| Some (select,ctx) ->
		select();
		api, ctx
	| None ->
		let com2 = Common.clone ctx.com in
		ctx.com.get_macros <- (fun() -> Some com2);
		com2.package_rules <- PMap.empty;
		com2.main_class <- None;
		com2.display <- DisplayMode.create DMNone;
		List.iter (fun p -> com2.defines <- PMap.remove (Globals.platform_name p) com2.defines) Globals.platforms;
		com2.defines_signature <- None;
		com2.class_path <- List.filter (fun s -> not (ExtString.String.exists s "/_std/")) com2.class_path;
		let name = platform_name !Globals.macro_platform in
		com2.class_path <- List.map (fun p -> p ^ name ^ "/_std/") com2.std_path @ com2.class_path;
		let to_remove = List.map (fun d -> fst (Define.infos d)) [Define.NoTraces] in
		let to_remove = to_remove @ List.map (fun (_,d) -> "flash" ^ d) Common.flash_versions in
		com2.defines <- PMap.foldi (fun k v acc -> if List.mem k to_remove then acc else PMap.add k v acc) com2.defines PMap.empty;
		Common.define com2 Define.Macro;
		Common.init_platform com2 !Globals.macro_platform;
		let mctx = ctx.g.do_create com2 in
		mctx.is_display_file <- ctx.is_display_file;
		create_macro_interp ctx mctx;
		api, mctx

let load_macro ctx display cpath f p =
	let t = macro_timer ctx ["typing";s_type_path cpath ^ "." ^ f] in
	let api, mctx = get_macro_context ctx p in
	let mint = Interp.get_ctx() in
	let cpath, sub = (match List.rev (fst cpath) with
		| name :: pack when name.[0] >= 'A' && name.[0] <= 'Z' -> (List.rev pack,name), Some (snd cpath)
		| _ -> cpath, None
	) in
	let meth = try Hashtbl.find mctx.com.cached_macros (cpath,f) with Not_found ->
		(* Temporarily enter display mode while typing the macro. *)
		if display then mctx.com.display <- ctx.com.display;
		let m = (try Hashtbl.find ctx.g.types_module cpath with Not_found -> cpath) in
		let mloaded = Typeload.load_module mctx m p in
		api.MacroApi.current_macro_module <- (fun() -> mloaded);
		mctx.m <- {
			curmod = mloaded;
			module_types = [];
			module_using = [];
			module_globals = PMap.empty;
			wildcard_packages = [];
			module_imports = [];
		};
		add_dependency ctx.m.curmod mloaded;
		let mt = Typeload.load_type_def mctx p { tpackage = fst cpath; tname = snd cpath; tparams = []; tsub = sub } in
		let cl, meth = (match mt with
			| TClassDecl c ->
				let t = macro_timer ctx ["finalize"] in
				mctx.g.do_finalize mctx;
				t();
				c, (try PMap.find f c.cl_statics with Not_found -> error ("Method " ^ f ^ " not found on class " ^ s_type_path cpath) p)
			| _ -> error "Macro should be called on a class" p
		) in
		if not (Common.defined ctx.com Define.NoDeprecationWarnings) then
			Display.DeprecationCheck.check_cf mctx.com meth p;
		let meth = (match follow meth.cf_type with TFun (args,ret) -> args,ret,cl,meth | _ -> error "Macro call should be a method" p) in
		mctx.com.display <- DisplayMode.create DMNone;
		if not ctx.in_macro then flush_macro_context mint ctx;
		Hashtbl.add mctx.com.cached_macros (cpath,f) meth;
		mctx.m <- {
			curmod = null_module;
			module_types = [];
			module_using = [];
			module_globals = PMap.empty;
			wildcard_packages = [];
			module_imports = [];
		};
		meth
	in
	t();
	let call args =
		let t = macro_timer ctx ["execution";s_type_path cpath ^ "." ^ f] in
		incr stats.s_macros_called;
		let r = Interp.call_path (Interp.get_ctx()) ((fst cpath) @ [(match sub with None -> snd cpath | Some s -> s)]) f args api in
		t();
		r
	in
	mctx, meth, call

type macro_arg_type =
	| MAExpr
	| MAFunction
	| MAOther

let type_macro ctx mode cpath f (el:Ast.expr list) p =
	let mctx, (margs,mret,mclass,mfield), call_macro = load_macro ctx (mode = MDisplay) cpath f p in
	let mpos = mfield.cf_pos in
	let ctexpr = { tpackage = ["haxe";"macro"]; tname = "Expr"; tparams = []; tsub = None } in
	let expr = Typeload.load_instance mctx (ctexpr,null_pos) false p in
	(match mode with
	| MDisplay ->
		raise Exit (* We don't have to actually call the macro. *)
	| MExpr ->
		unify mctx mret expr mpos;
	| MBuild ->
		let ctfields = { tpackage = []; tname = "Array"; tparams = [TPType (CTPath { tpackage = ["haxe";"macro"]; tname = "Expr"; tparams = []; tsub = Some "Field" },null_pos)]; tsub = None } in
		let tfields = Typeload.load_instance mctx (ctfields,null_pos) false p in
		unify mctx mret tfields mpos
	| MMacroType ->
		let cttype = { tpackage = ["haxe";"macro"]; tname = "Type"; tparams = []; tsub = None } in
		let ttype = Typeload.load_instance mctx (cttype,null_pos) false p in
		try
			unify_raise mctx mret ttype mpos;
			(* TODO: enable this again in the future *)
			(* ctx.com.warning "Returning Type from @:genericBuild macros is deprecated, consider returning ComplexType instead" p; *)
		with Error (Unify _,_) ->
			let cttype = { tpackage = ["haxe";"macro"]; tname = "Expr"; tparams = []; tsub = Some ("ComplexType") } in
			let ttype = Typeload.load_instance mctx (cttype,null_pos) false p in
			unify_raise mctx mret ttype mpos;
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
	let todo = ref [] in
	let args =
		(*
			force default parameter types to haxe.macro.Expr, and if success allow to pass any value type since it will be encoded
		*)
		let eargs = List.map (fun (n,o,t) ->
			try unify_raise mctx t expr p; (n, o, t_dynamic), MAExpr
			with Error (Unify _,_) -> match follow t with
				| TFun _ ->
					(n,o,t_dynamic), MAFunction
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
			let e = (try
				(match Codegen.type_constant_value ctx.com e with
				| { eexpr = TConst (TString _); epos = p } when Lexer.is_fmt_string p ->
					Lexer.remove_fmt_string p;
					todo := (fun() -> Lexer.add_fmt_string p) :: !todo;
				| _ -> ());
				e
			with Error (Custom _,_) ->
				(* if it's not a constant, let's make something that is typed as haxe.macro.Expr - for nice error reporting *)
				(EBlock [
					(EVars [("__tmp",null_pos),Some (CTPath ctexpr,p),Some (EConst (Ident "null"),p)],p);
					(EConst (Ident "__tmp"),p);
				],p)
			) in
			(* let's track the index by doing [e][index] (we will keep the expression type this way) *)
			incr index;
			(EArray ((EArrayDecl [e],p),(EConst (Int (string_of_int (!index))),p)),p)
		) el in
		let elt, _ = unify_call_args mctx constants (List.map fst eargs) t_dynamic p false false in
		List.iter (fun f -> f()) (!todo);
		List.map2 (fun (_,mct) e ->
			let e, et = (match e.eexpr with
				(* get back our index and real expression *)
				| TArray ({ eexpr = TArrayDecl [e] }, { eexpr = TConst (TInt index) }) -> List.nth el (Int32.to_int index), e
				(* added by unify_call_args *)
				| TConst TNull -> (EConst (Ident "null"),e.epos), e
				| _ -> assert false
			) in
			let ictx = Interp.get_ctx() in
			match mct with
			| MAExpr ->
				Interp.encode_expr e
			| MAFunction ->
				let e = ictx.Interp.curapi.MacroApi.type_macro_expr e in
				begin match Interp.eval_expr ictx e with
				| Some v -> v
				| None -> Interp.vnull
				end
			| MAOther -> match Interp.eval_expr ictx et with
				| None -> assert false
				| Some v -> v
		) eargs elt
	in
	let args = match el2 with
		| [] -> args
		| _ -> (match List.rev args with _::args -> List.rev args | [] -> []) @ [Interp.enc_array (List.map Interp.encode_expr el2)]
	in
	let call() =
		match call_macro args with
		| None -> None
		| Some v ->
			try
				Some (match mode with
				| MExpr | MDisplay -> Interp.decode_expr v
				| MBuild ->
					let fields = if v = Interp.vnull then
							(match ctx.g.get_build_infos() with
							| None -> assert false
							| Some (_,_,fields) -> fields)
						else
							List.map Interp.decode_field (Interp.dec_array v)
					in
					(EVars [("fields",null_pos),Some (CTAnonymous fields,p),None],p)
				| MMacroType ->
					let t = if v = Interp.vnull then
						mk_mono()
					else try
						let ct = Interp.decode_ctype v in
						Typeload.load_complex_type ctx false p ct;
					with MacroApi.Invalid_expr ->
						Interp.decode_type v
					in
					ctx.ret <- t;
					(EBlock [],p)
				)
			with MacroApi.Invalid_expr ->
				if v = Interp.vnull then
					error "Unexpected null value returned from macro" p
				else
					error "The macro didn't return a valid result" p
	in
	let e = (if ctx.in_macro then begin
		(*
			this is super-tricky : we can't evaluate a macro inside a macro because we might trigger some cycles.
			So instead, we generate a haxe.macro.Context.delayedCalled(i) expression that will only evaluate the
			macro if/when it is called.

			The tricky part is that the whole delayed-evaluation process has to use the same contextual informations
			as if it was evaluated now.
		*)
		let ctx = {
			ctx with locals = ctx.locals;
		} in
		let pos = DynArray.length mctx.g.delayed_macros in
		DynArray.add mctx.g.delayed_macros (fun() ->
			delayed_macro_result := (fun() ->
				let mint = Interp.get_ctx() in
				match call() with
				| None -> (fun() -> raise MacroApi.Abort)
				| Some e -> Interp.eval_delayed mint (type_expr ctx e Value)
			);
		);
		ctx.m.curmod.m_extra.m_time <- -1.; (* disable caching for modules having macro-in-macro *)
		if Common.defined ctx.com Define.MacroDebug then
			ctx.com.warning "Macro-in-macro call detected" p;
		let e = (EConst (Ident "$__delayed_call__"),p) in
		Some (EUntyped (ECall (e,[EConst (Int (string_of_int pos)),p]),p),p)
	end else
		call()
	) in
	e

let call_macro ctx path meth args p =
	let mctx, (margs,_,mclass,mfield), call = load_macro ctx false path meth p in
	let el, _ = unify_call_args mctx args margs t_dynamic p false false in
	call (List.map (fun e -> try Interp.make_const e with Exit -> error "Parameter should be a constant" e.epos) el)

let call_init_macro ctx e =
	let p = { pfile = "--macro"; pmin = 0; pmax = 0 } in
	let e = try
		Parser.parse_expr_string ctx.com e p error false
	with err ->
		display_error ctx ("Could not parse `" ^ e ^ "`") p;
		raise err
	in
	match fst e with
	| ECall (e,args) ->
		let rec loop e =
			match fst e with
			| EField (e,f) -> f :: loop e
			| EConst (Ident i) -> [i]
			| _ -> error "Invalid macro call" p
		in
		let path, meth = (match loop e with
		| [meth] -> (["haxe";"macro"],"Compiler"), meth
		| [meth;"server"] -> (["haxe";"macro"],"CompilationServer"), meth
		| meth :: cl :: path -> (List.rev path,cl), meth
		| _ -> error "Invalid macro call" p) in
		ignore(call_macro ctx path meth args p);
	| _ ->
		error "Invalid macro call" p

let interpret ctx =
	let mctx = Interp.create ctx.com (make_macro_api ctx null_pos) in
	Interp.add_types mctx ctx.com.types (fun t -> ());
	match ctx.com.main with
	| None -> ()
	| Some e -> ignore(Interp.eval_expr mctx e)

let setup() =
	Interp.setup Interp.macro_api

;;
load_macro_ref := load_macro;
