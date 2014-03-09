(*
 * Copyright (C)2005-2013 Haxe Foundation
 *
 * Permission is hereby granted, free of charge, to any person obtaining a
 * copy of this software and associated documentation files (the "Software"),
 * to deal in the Software without restriction, including without limitation
 * the rights to use, copy, modify, merge, publish, distribute, sublicense,
 * and/or sell copies of the Software, and to permit persons to whom the
 * Software is furnished to do so, subject to the following conditions:
 *
 * The above copyright notice and this permission notice shall be included in
 * all copies or substantial portions of the Software.
 *
 * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
 * IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
 * FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
 * AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
 * LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING
 * FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER
 * DEALINGS IN THE SOFTWARE.
 *)

open Ast
open Type
open Common
open Typecore

(*
	Build module structure : should be atomic - no type loading is possible
*)
let make_module ctx mpath file tdecls loadp =
	let decls = ref [] in
	let make_path name priv =
		if List.exists (fun (t,_) -> snd (t_path t) = name) !decls then error ("Type name " ^ name ^ " is already defined in this module") loadp;
		if priv then (fst mpath @ ["_" ^ snd mpath], name) else (fst mpath, name)
	in
	let m = {
		m_id = alloc_mid();
		m_path = mpath;
		m_types = [];
		m_extra = module_extra (Common.unique_full_path file) (Common.get_signature ctx.com) (file_time file) (if ctx.in_macro then MMacro else MCode);
	} in
	let pt = ref None in
	let rec make_decl acc decl =
		let p = snd decl in
		let acc = (match fst decl with
		| EImport _ | EUsing _ ->
			(match !pt with
			| None -> acc
			| Some pt ->
				display_error ctx "import and using may not appear after a type declaration" p;
				error "Previous type declaration found here" pt)
		| EClass d ->
			pt := Some p;
			let priv = List.mem HPrivate d.d_flags in
			let path = make_path d.d_name priv in
			let c = mk_class m path p in
			c.cl_module <- m;
			c.cl_private <- priv;
			c.cl_doc <- d.d_doc;
			c.cl_meta <- d.d_meta;
			decls := (TClassDecl c, decl) :: !decls;
			acc
		| EEnum d ->
			pt := Some p;
			let priv = List.mem EPrivate d.d_flags in
			let path = make_path d.d_name priv in
			let e = {
				e_path = path;
				e_module = m;
				e_pos = p;
				e_doc = d.d_doc;
				e_meta = d.d_meta;
				e_types = [];
				e_private = priv;
				e_extern = List.mem EExtern d.d_flags;
				e_constrs = PMap.empty;
				e_names = [];
				e_type = {
					t_path = fst path, "Enum<" ^ (snd path) ^ ">";
					t_module = m;
					t_doc = None;
					t_pos = p;
					t_type = mk_mono();
					t_private = true;
					t_types = [];
					t_meta = [];
				};
			} in
			decls := (TEnumDecl e, decl) :: !decls;
			acc
		| ETypedef d ->
			pt := Some p;
			let priv = List.mem EPrivate d.d_flags in
			let path = make_path d.d_name priv in
			let t = {
				t_path = path;
				t_module = m;
				t_pos = p;
				t_doc = d.d_doc;
				t_private = priv;
				t_types = [];
				t_type = mk_mono();
				t_meta = d.d_meta;
			} in
			decls := (TTypeDecl t, decl) :: !decls;
			acc
		 | EAbstract d ->
			let priv = List.mem APrivAbstract d.d_flags in
			let path = make_path d.d_name priv in
			let a = {
				a_path = path;
				a_private = priv;
				a_module = m;
				a_pos = p;
				a_doc = d.d_doc;
				a_types = [];
				a_meta = d.d_meta;
				a_from = [];
				a_to = [];
				a_ops = [];
				a_unops = [];
				a_impl = None;
				a_array = [];
				a_this = mk_mono();
			} in
			decls := (TAbstractDecl a, decl) :: !decls;
			match d.d_data with
			| [] when Meta.has Meta.CoreType a.a_meta ->
				a.a_this <- t_dynamic;
				acc
			| fields ->
				let rec loop = function
					| [] ->
						let params = List.map (fun t -> TPType (CTPath { tname = t.tp_name; tparams = []; tsub = None; tpackage = [] })) d.d_params in
						CTPath { tpackage = []; tname = d.d_name; tparams = params; tsub = None }
					| AIsType t :: _ -> t
					| _ :: l -> loop l
				in
				let this_t = loop d.d_flags in
				let fields = List.map (fun f ->
					let stat = List.mem AStatic f.cff_access in
					let p = f.cff_pos in
					match f.cff_kind with
					| FProp (("get" | "never"),("set" | "never"),_,_) when not stat ->
						(* TODO: hack to avoid issues with abstract property generation on As3 *)
						if Common.defined ctx.com Define.As3 then f.cff_meta <- (Meta.Extern,[],p) :: f.cff_meta;
						{ f with cff_access = AStatic :: f.cff_access; cff_meta = (Meta.Impl,[],p) :: f.cff_meta }
					| FProp _ when not stat ->
						display_error ctx "Member property accessors must be get/set or never" p;
						f
					| FFun fu when f.cff_name = "new" && not stat ->
						let init p = (EVars ["this",Some this_t,None],p) in
						let ret p = (EReturn (Some (EConst (Ident "this"),p)),p) in
						if Meta.has Meta.MultiType a.a_meta then begin
							if List.mem AInline f.cff_access then error "MultiType constructors cannot be inline" f.cff_pos;
							if fu.f_expr <> None then error "MultiType constructors cannot have a body" f.cff_pos;
						end;
						let has_call e =
							let rec loop e = match fst e with
								| ECall _ -> raise Exit
								| _ -> Ast.map_expr loop e
							in
							try ignore(loop e); false with Exit -> true
						in
						let fu = {
							fu with
							f_expr = (match fu.f_expr with
							| None -> if Meta.has Meta.MultiType a.a_meta then Some (EConst (Ident "null"),p) else None
							| Some (EBlock [EBinop (OpAssign,(EConst (Ident "this"),_),e),_],_ | EBinop (OpAssign,(EConst (Ident "this"),_),e),_) when not (has_call e) ->
								Some (EReturn (Some e), pos e)
							| Some (EBlock el,p) -> Some (EBlock (init p :: el @ [ret p]),p)
							| Some e -> Some (EBlock [init p;e;ret p],p)
							);
							f_type = Some this_t;
						} in
						{ f with cff_name = "_new"; cff_access = AStatic :: f.cff_access; cff_kind = FFun fu; cff_meta = (Meta.Impl,[],p) :: f.cff_meta }
					| FFun fu when not stat ->
						if Meta.has Meta.From f.cff_meta then error "@:from cast functions must be static" f.cff_pos;
						let fu = { fu with f_args = (if List.mem AMacro f.cff_access then fu.f_args else ("this",false,Some this_t,None) :: fu.f_args) } in
						{ f with cff_kind = FFun fu; cff_access = AStatic :: f.cff_access; cff_meta = (Meta.Impl,[],p) :: f.cff_meta }
					| _ ->
						f
				) fields in
				let meta = ref [] in
				if has_meta Meta.Dce a.a_meta then meta := (Meta.Dce,[],p) :: !meta;
				let acc = make_decl acc (EClass { d_name = d.d_name ^ "_Impl_"; d_flags = [HPrivate]; d_data = fields; d_doc = None; d_params = []; d_meta = !meta },p) in
				(match !decls with
				| (TClassDecl c,_) :: _ ->
					List.iter (fun m -> match m with
						| ((Meta.Build | Meta.CoreApi | Meta.Allow | Meta.Access | Meta.Enum),_,_) ->
							c.cl_meta <- m :: c.cl_meta;
						| _ ->
							()
					) a.a_meta;
					a.a_impl <- Some c;
					c.cl_kind <- KAbstractImpl a
				| _ -> assert false);
				acc
		) in
		decl :: acc
	in
	let tdecls = List.fold_left make_decl [] tdecls in
	let decls = List.rev !decls in
	m.m_types <- List.map fst decls;
	m, decls, List.rev tdecls

let parse_file com file p =
	let ch = (try open_in_bin file with _ -> error ("Could not open " ^ file) p) in
	let t = Common.timer "parsing" in
	Lexer.init file;
	incr stats.s_files_parsed;
	let data = (try Parser.parse com (Lexing.from_channel ch) with e -> close_in ch; t(); raise e) in
	close_in ch;
	t();
	Common.log com ("Parsed " ^ file);
	data

let parse_hook = ref parse_file
let type_module_hook = ref (fun _ _ _ -> None)
let type_function_params_rec = ref (fun _ _ _ _ -> assert false)
let return_partial_type = ref false

let type_function_param ctx t e opt p =
	if opt then
		let e = (match e with None -> Some (EConst (Ident "null"),p) | _ -> e) in
		ctx.t.tnull t, e
	else
		let t = match e with Some (EConst (Ident "null"),p) -> ctx.t.tnull t | _ -> t in
		t, e

let type_var_field ctx t e stat p =
	if stat then ctx.curfun <- FunStatic else ctx.curfun <- FunMember;
	let e = type_expr ctx e (WithType t) in
	unify ctx e.etype t p;
	match t with
	| TType ({ t_path = ([],"UInt") },[]) | TAbstract ({ a_path = ([],"UInt") },[]) when stat -> { e with etype = t }
	| _ -> e

let apply_macro ctx mode path el p =
	let cpath, meth = (match List.rev (ExtString.String.nsplit path ".") with
		| meth :: name :: pack -> (List.rev pack,name), meth
		| _ -> error "Invalid macro path" p
	) in
	ctx.g.do_macro ctx mode cpath meth el p

(** since load_type_def and load_instance are used in PASS2, they should not access the structure of a type **)

(*
	load a type or a subtype definition
*)
let rec load_type_def ctx p t =
	let no_pack = t.tpackage = [] in
	let tname = (match t.tsub with None -> t.tname | Some n -> n) in
	try
		if t.tsub <> None then raise Not_found;
		List.find (fun t2 ->
			let tp = t_path t2 in
			tp = (t.tpackage,tname) || (no_pack && snd tp = tname)
		) (ctx.m.curmod.m_types @ ctx.m.module_types)
	with
		Not_found ->
			let next() =
				let t, m = (try
					t, ctx.g.do_load_module ctx (t.tpackage,t.tname) p
				with Error (Module_not_found _,p2) as e when p == p2 ->
					match t.tpackage with
					| "std" :: l ->
						let t = { t with tpackage = l } in
						t, ctx.g.do_load_module ctx (t.tpackage,t.tname) p
					| _ -> raise e
				) in
				let tpath = (t.tpackage,tname) in
				try
					List.find (fun t -> not (t_infos t).mt_private && t_path t = tpath) m.m_types
				with
					Not_found -> raise (Error (Type_not_found (m.m_path,tname),p))
			in
			(* lookup in wildcard imported packages *)
			try
				if not no_pack then raise Exit;
				let rec loop = function
					| [] -> raise Exit
					| wp :: l ->
						try
							load_type_def ctx p { t with tpackage = wp }
						with
							| Error (Module_not_found _,p2)
							| Error (Type_not_found _,p2) when p == p2 -> loop l
				in
				loop ctx.m.wildcard_packages
			with Exit ->
			(* lookup in our own package - and its upper packages *)
			let rec loop = function
				| [] -> raise Exit
				| (_ :: lnext) as l ->
					try
						load_type_def ctx p { t with tpackage = List.rev l }
					with
						| Error (Module_not_found _,p2)
						| Error (Type_not_found _,p2) when p == p2 -> loop lnext
			in
			try
				if not no_pack then raise Exit;
				(match fst ctx.m.curmod.m_path with
				| [] -> raise Exit
				| x :: _ ->
					(* this can occur due to haxe remoting : a module can be
						already defined in the "js" package and is not allowed
						to access the js classes *)
					try
						(match PMap.find x ctx.com.package_rules with
						| Forbidden -> raise Exit
						| _ -> ())
					with Not_found -> ());
				loop (List.rev (fst ctx.m.curmod.m_path));
			with
				Exit -> next()

let check_param_constraints ctx types t pl c p =
	match follow t with
	| TMono _ -> ()
	| TInst({cl_kind = KTypeParameter _},_) -> ()
	| _ ->
		let ctl = (match c.cl_kind with KTypeParameter l -> l | _ -> []) in
		List.iter (fun ti ->
			let ti = apply_params types pl ti in
			let ti = (match follow ti with
				| TInst ({ cl_kind = KGeneric } as c,pl) ->
					(* if we solve a generic contraint, let's substitute with the actual generic instance before unifying *)
					let _,_, f = ctx.g.do_build_instance ctx (TClassDecl c) p in
					f pl
				| TInst({cl_kind = KGenericInstance(c2,tl)},_) ->
					(* build generic instance again with applied type parameters (issue 1965) *)
					let _,_, f = ctx.g.do_build_instance ctx (TClassDecl c2) p in
					f (List.map (fun t -> apply_params types pl t) tl)
				| _ -> ti
			) in
			try
				unify_raise ctx t ti p
			with Error(Unify l,p) ->
				if not ctx.untyped then display_error ctx (error_msg (Unify (Constraint_failure (s_type_path c.cl_path) :: l))) p;
		) ctl

(* build an instance from a full type *)
let rec load_instance ctx t p allow_no_params =
	try
		if t.tpackage <> [] || t.tsub <> None then raise Not_found;
		let pt = List.assoc t.tname ctx.type_params in
		if t.tparams <> [] then error ("Class type parameter " ^ t.tname ^ " can't have parameters") p;
		pt
	with Not_found ->
		let mt = load_type_def ctx p t in
		let is_generic = match mt with TClassDecl {cl_kind = KGeneric} -> true | _ -> false in
		let types , path , f = ctx.g.do_build_instance ctx mt p in
		if allow_no_params && t.tparams = [] then begin
			let pl = ref [] in
			pl := List.map (fun (name,t) ->
				match follow t with
				| TInst (c,_) ->
					let t = mk_mono() in
					if c.cl_kind <> KTypeParameter [] || is_generic then delay ctx PCheckConstraint (fun() -> check_param_constraints ctx types t (!pl) c p);
					t;
				| _ -> assert false
			) types;
			f (!pl)
		end else if path = ([],"Dynamic") then
			match t.tparams with
			| [] -> t_dynamic
			| [TPType t] -> TDynamic (load_complex_type ctx p t)
			| _ -> error "Too many parameters for Dynamic" p
		else begin
			if List.length types <> List.length t.tparams then error ("Invalid number of type parameters for " ^ s_type_path path) p;
			let tparams = List.map (fun t ->
				match t with
				| TPExpr e ->
					let name = (match fst e with
						| EConst (String s) -> "S" ^ s
						| EConst (Int i) -> "I" ^ i
						| EConst (Float f) -> "F" ^ f
						| _ -> "Expr"
					) in
					let c = mk_class null_module ([],name) p in
					c.cl_kind <- KExpr e;
					TInst (c,[])
				| TPType t -> load_complex_type ctx p t
			) t.tparams in
			let params = List.map2 (fun t (name,t2) ->
				let isconst = (match t with TInst ({ cl_kind = KExpr _ },_) -> true | _ -> false) in
				if isconst <> (name = "Const") && t != t_dynamic then error (if isconst then "Constant value unexpected here" else "Constant value excepted as type parameter") p;
				match follow t2 with
				| TInst ({ cl_kind = KTypeParameter [] }, []) when not is_generic ->
					t
				| TInst (c,[]) ->
					let r = exc_protect ctx (fun r ->
						r := (fun() -> t);
						delay ctx PCheckConstraint (fun() -> check_param_constraints ctx types t tparams c p);
						t
					) "constraint" in
					delay ctx PForce (fun () -> ignore(!r()));
					TLazy r
				| _ -> assert false
			) tparams types in
			f params
		end
(*
	build an instance from a complex type
*)
and load_complex_type ctx p t =
	match t with
	| CTParent t -> load_complex_type ctx p t
	| CTPath t -> load_instance ctx t p false
	| CTOptional _ -> error "Optional type not allowed here" p
	| CTExtend (tl,l) ->
		(match load_complex_type ctx p (CTAnonymous l) with
		| TAnon a as ta ->
			let mk_extension t =
				match follow t with
				| TInst ({cl_kind = KTypeParameter _},_) ->
					error "Cannot structurally extend type parameters" p
				| TInst (c,tl) ->
					let c2 = mk_class null_module (fst c.cl_path,"+" ^ snd c.cl_path) p in
					c2.cl_private <- true;
					PMap.iter (fun f _ ->
						try
							ignore(class_field c f);
							error ("Cannot redefine field " ^ f) p
						with
							Not_found -> ()
					) a.a_fields;
					(* do NOT tag as extern - for protect *)
					c2.cl_kind <- KExtension (c,tl);
					c2.cl_super <- Some (c,tl);
					c2.cl_fields <- a.a_fields;
					TInst (c2,[])
				| TMono _ ->
					error "Loop found in cascading signatures definitions. Please change order/import" p
				| TAnon a2 ->
					PMap.iter (fun f _ ->
						if PMap.mem f a2.a_fields then error ("Cannot redefine field " ^ f) p;
					) a.a_fields;
					mk_anon (PMap.foldi PMap.add a.a_fields a2.a_fields)
				| _ -> error "Can only extend classes and structures" p
			in
			let loop t = match follow t with
				| TAnon a2 ->
					PMap.iter (fun f cf ->
						if PMap.mem f a.a_fields then error ("Cannot redefine field " ^ f) p;
						a.a_fields <- PMap.add f cf a.a_fields
					) a2.a_fields
				| _ ->
					error "Multiple structural extension is only allowed for structures" p
			in
			let il = List.map (fun t -> load_instance ctx t p false) tl in
			let tr = ref None in
			let t = TMono tr in
			let r = exc_protect ctx (fun r ->
				r := (fun _ -> t);
				tr := Some (match il with
					| [i] ->
						mk_extension i
					| _ ->
						List.iter loop il;
						ta);
				t
			) "constraint" in
			delay ctx PForce (fun () -> ignore(!r()));
			TLazy r
		| _ -> assert false)
	| CTAnonymous l ->
		let rec loop acc f =
			let n = f.cff_name in
			let p = f.cff_pos in
			if PMap.mem n acc then error ("Duplicate field declaration : " ^ n) p;
			let topt = function
				| None -> error ("Explicit type required for field " ^ n) p
				| Some t -> load_complex_type ctx p t
			in
			let no_expr = function
				| None -> ()
				| Some (_,p) -> error "Expression not allowed here" p
			in
			let pub = ref true in
			let dyn = ref false in
			let params = ref [] in
			List.iter (fun a ->
				match a with
				| APublic -> ()
				| APrivate -> pub := false;
				| ADynamic when (match f.cff_kind with FFun _ -> true | _ -> false) -> dyn := true
				| AStatic | AOverride | AInline | ADynamic | AMacro -> error ("Invalid access " ^ Ast.s_access a) p
			) f.cff_access;
			let t , access = (match f.cff_kind with
				| FVar (Some (CTPath({tpackage=[];tname="Void"})), _)  | FProp (_,_,Some (CTPath({tpackage=[];tname="Void"})),_) ->
					error "Fields of type Void are not allowed in structures" p
				| FVar (t, e) ->
					no_expr e;
					topt t, Var { v_read = AccNormal; v_write = AccNormal }
				| FFun fd ->
					params := (!type_function_params_rec) ctx fd f.cff_name p;
					no_expr fd.f_expr;
					let old = ctx.type_params in
					ctx.type_params <- !params @ old;
					let args = List.map (fun (name,o,t,e) -> no_expr e; name, o, topt t) fd.f_args in
					let t = TFun (args,topt fd.f_type), Method (if !dyn then MethDynamic else MethNormal) in
					ctx.type_params <- old;
					t
				| FProp (i1,i2,t,e) ->
					no_expr e;
					let access m get =
						match m with
						| "null" -> AccNo
						| "never" -> AccNever
						| "default" -> AccNormal
						| "dynamic" -> AccCall
						| "get" when get -> AccCall
						| "set" when not get -> AccCall
						| x when get && x = "get_" ^ n -> AccCall
						| x when not get && x = "set_" ^ n -> AccCall
						| _ ->
							error "Custom property access is no longer supported in Haxe 3" f.cff_pos;
					in
					let t = (match t with None -> error "Type required for structure property" p | Some t -> t) in
					load_complex_type ctx p t, Var { v_read = access i1 true; v_write = access i2 false }
			) in
			let t = if Meta.has Meta.Optional f.cff_meta then ctx.t.tnull t else t in
			let cf = {
				cf_name = n;
				cf_type = t;
				cf_pos = p;
				cf_public = !pub;
				cf_kind = access;
				cf_params = !params;
				cf_expr = None;
				cf_doc = f.cff_doc;
				cf_meta = f.cff_meta;
				cf_overloads = [];
			} in
			init_meta_overloads ctx cf;
			PMap.add n cf acc
		in
		mk_anon (List.fold_left loop PMap.empty l)
	| CTFunction (args,r) ->
		match args with
		| [CTPath { tpackage = []; tparams = []; tname = "Void" }] ->
			TFun ([],load_complex_type ctx p r)
		| _ ->
			TFun (List.map (fun t ->
				let t, opt = (match t with CTOptional t -> t, true | _ -> t,false) in
				"",opt,load_complex_type ctx p t
			) args,load_complex_type ctx p r)

and init_meta_overloads ctx cf =
	let overloads = ref [] in
	cf.cf_meta <- List.filter (fun m ->
		match m with
		| (Meta.Overload,[(EFunction (fname,f),p)],_)  ->
			if fname <> None then error "Function name must not be part of @:overload" p;
			(match f.f_expr with Some (EBlock [], _) -> () | _ -> error "Overload must only declare an empty method body {}" p);
			let old = ctx.type_params in
			(match cf.cf_params with
			| [] -> ()
			| l -> ctx.type_params <- List.filter (fun t -> not (List.mem t l)) ctx.type_params);
			let params = (!type_function_params_rec) ctx f cf.cf_name p in
			ctx.type_params <- params @ ctx.type_params;
			let topt = function None -> error "Explicit type required" p | Some t -> load_complex_type ctx p t in
			let args = List.map (fun (a,opt,t,_) ->  a,opt,topt t) f.f_args in
			overloads := (args,topt f.f_type, params) :: !overloads;
			ctx.type_params <- old;
			false
		| (Meta.Overload,[],_) when ctx.com.config.pf_overload ->
			let topt (n,_,t) = match t with | TMono t when !t = None -> error ("Explicit type required for overload functions\nFor function argument '" ^ n ^ "'") cf.cf_pos | _ -> () in
			(match follow cf.cf_type with
			| TFun (args,_) -> List.iter topt args
			| _ -> () (* could be a variable *));
			true
		| (Meta.Overload,[],p) ->
				error "This platform does not support this kind of overload declaration. Try @:overload(function()... {}) instead" p
		| (Meta.Overload,_,p) ->
				error "Invalid @:overload metadata format" p
		| _ ->
			true
	) cf.cf_meta;
	cf.cf_overloads <- List.map (fun (args,ret,params) -> { cf with cf_type = TFun (args,ret); cf_params = params }) (List.rev !overloads)

let hide_types ctx =
	let old_m = ctx.m in
	let old_type_params = ctx.type_params in
	let old_deps = ctx.g.std.m_extra.m_deps in
	ctx.m <- {
		curmod = ctx.g.std;
		module_types = [];
		module_using = [];
		module_globals = PMap.empty;
		wildcard_packages = [];
	};
	ctx.type_params <- [];
	(fun() ->
		ctx.m <- old_m;
		ctx.type_params <- old_type_params;
		(* restore dependencies that might be have been wronly inserted *)
		ctx.g.std.m_extra.m_deps <- old_deps;
	)

(*
	load a type while ignoring the current imports or local types
*)
let load_core_type ctx name =
	let show = hide_types ctx in
	let t = load_instance ctx { tpackage = []; tname = name; tparams = []; tsub = None; } null_pos false in
	show();
	add_dependency ctx.m.curmod (match t with
	| TInst (c,_) -> c.cl_module
	| TType (t,_) -> t.t_module
	| TAbstract (a,_) -> a.a_module
	| TEnum (e,_) -> e.e_module
	| _ -> assert false);
	t

let t_iterator ctx =
	let show = hide_types ctx in
	match load_type_def ctx null_pos { tpackage = []; tname = "Iterator"; tparams = []; tsub = None } with
	| TTypeDecl t ->
		show();
		add_dependency ctx.m.curmod t.t_module;
		if List.length t.t_types <> 1 then assert false;
		let pt = mk_mono() in
		apply_params t.t_types [pt] t.t_type, pt
	| _ ->
		assert false

(*
	load either a type t or Null<Unknown> if not defined
*)
let load_type_opt ?(opt=false) ctx p t =
	let t = (match t with None -> mk_mono() | Some t -> load_complex_type ctx p t) in
	if opt then ctx.t.tnull t else t

(* ---------------------------------------------------------------------- *)
(* Structure check *)

let valid_redefinition ctx f1 t1 f2 t2 =
	let valid t1 t2 =
		Type.unify t1 t2;
		if is_null t1 <> is_null t2 then raise (Unify_error [Cannot_unify (t1,t2)]);
	in
	let t1, t2 = (match f1.cf_params, f2.cf_params with
		| [], [] -> t1, t2
		| l1, l2 when List.length l1 = List.length l2 ->
			let to_check = ref [] in
			let monos = List.map2 (fun (name,p1) (_,p2) ->
				(match follow p1, follow p2 with
				| TInst ({ cl_kind = KTypeParameter ct1 } as c1,pl1), TInst ({ cl_kind = KTypeParameter ct2 } as c2,pl2) ->
					(match ct1, ct2 with
					| [], [] -> ()
					| _, _ when List.length ct1 = List.length ct2 ->
						(* if same constraints, they are the same type *)
						let check monos =
							List.iter2 (fun t1 t2  ->
								try
									let t1 = apply_params l1 monos (apply_params c1.cl_types pl1 t1) in
									let t2 = apply_params l2 monos (apply_params c2.cl_types pl2 t2) in
									type_eq EqStrict t1 t2
								with Unify_error l ->
									raise (Unify_error (Unify_custom "Constraints differ" :: l))
							) ct1 ct2
						in
						to_check := check :: !to_check;
					| _ ->
						raise (Unify_error [Unify_custom "Different number of constraints"]))
				| _ -> ());
				TInst (mk_class null_module ([],name) Ast.null_pos,[])
			) l1 l2 in
			List.iter (fun f -> f monos) !to_check;
			apply_params l1 monos t1, apply_params l2 monos t2
		| _  ->
			(* ignore type params, will create other errors later *)
			t1, t2
	) in
	match follow t1, follow t2 with
	| TFun (args1,r1) , TFun (args2,r2) when List.length args1 = List.length args2 -> (try
			List.iter2 (fun (n,o1,a1) (_,o2,a2) ->
				if o1 <> o2 then raise (Unify_error [Not_matching_optional n]);
				(try valid a2 a1 with Unify_error _ -> raise (Unify_error [Cannot_unify(a1,a2)]))
			) args1 args2;
			valid r1 r2
		with Unify_error l ->
			raise (Unify_error (Cannot_unify (t1,t2) :: l)))
	| _ , _ ->
		(* in case args differs, or if an interface var *)
		type_eq EqStrict t1 t2;
		if is_null t1 <> is_null t2 then raise (Unify_error [Cannot_unify (t1,t2)])

let copy_meta meta_src meta_target sl =
	let meta = ref meta_target in
	List.iter (fun (m,e,p) ->
		if List.mem m sl then meta := (m,e,p) :: !meta
	) meta_src;
	!meta

let same_overload_args t1 t2 f1 f2 =
  if List.length f1.cf_params <> List.length f2.cf_params then
    false
  else
    let rec follow_skip_null t = match t with
    | TMono r ->
      (match !r with
      | Some t -> follow_skip_null t
      | _ -> t)
    | TLazy f ->
      follow_skip_null (!f())
    | TType ({ t_path = [],"Null" } as t, [p]) ->
      TType(t,[follow p])
    | TType (t,tl) ->
      follow_skip_null (apply_params t.t_types tl t.t_type)
    | _ -> t
    in
    let same_arg t1 t2 =
      let t1 = follow_skip_null t1 in
      let t2 = follow_skip_null t2 in
      match follow_skip_null t1, follow_skip_null t2 with
      | TType _, TType _ -> type_iseq t1 t2
      | TType _, _
      | _, TType _ -> false
      | _ -> type_iseq t1 t2
    in

    match follow (apply_params f1.cf_params (List.map (fun (_,t) -> t) f2.cf_params) t1), follow t2 with
    | TFun(a1,_), TFun(a2,_) ->
      (try
        List.for_all2 (fun (_,_,t1) (_,_,t2) ->
          same_arg t1 t2) a1 a2
      with | Invalid_argument("List.for_all2") ->
        false)
    | _ -> assert false

(** retrieves all overloads from class c and field i, as (Type.t * tclass_field) list *)
let rec get_overloads c i =
	let ret = try
			let f = PMap.find i c.cl_fields in
			(f.cf_type, f) :: (List.map (fun f -> f.cf_type, f) f.cf_overloads)
		with | Not_found -> []
	in
	let rsup = match c.cl_super with
	| None when c.cl_interface ->
			let ifaces = List.concat (List.map (fun (c,tl) ->
				List.map (fun (t,f) -> apply_params c.cl_types tl t, f) (get_overloads c i)
			) c.cl_implements) in
			ret @ ifaces
	| None -> ret
	| Some (c,tl) ->
			ret @ ( List.map (fun (t,f) -> apply_params c.cl_types tl t, f) (get_overloads c i) )
	in
	ret @ (List.filter (fun (t,f) -> not (List.exists (fun (t2,f2) -> same_overload_args t t2 f f2) ret)) rsup)


let check_overloads ctx c =
	(* check if field with same signature was declared more than once *)
	List.iter (fun f ->
		if Meta.has Meta.Overload f.cf_meta then
			List.iter (fun f2 ->
				try
					ignore (List.find (fun f3 -> f3 != f2 && same_overload_args f2.cf_type f3.cf_type f2 f3) (f :: f.cf_overloads));
					display_error ctx ("Another overloaded field of same signature was already declared : " ^ f2.cf_name) f2.cf_pos
				with | Not_found -> ()
		) (f :: f.cf_overloads)) (c.cl_ordered_fields @ c.cl_ordered_statics)

let check_overriding ctx c =
	match c.cl_super with
	| None ->
		(match c.cl_overrides with
		| [] -> ()
		| i :: _ ->
			display_error ctx ("Field " ^ i.cf_name ^ " is declared 'override' but doesn't override any field") i.cf_pos)
	| Some (csup,params) ->
		PMap.iter (fun i f ->
			let p = f.cf_pos in
			let check_field f get_super_field is_overload = try
				(if is_overload && not (Meta.has Meta.Overload f.cf_meta) then
					display_error ctx ("Missing @:overload declaration for field " ^ i) p);
				let t, f2 = get_super_field csup i in
				(* allow to define fields that are not defined for this platform version in superclass *)
				(match f2.cf_kind with
				| Var { v_read = AccRequire _ } -> raise Not_found;
				| _ -> ());
				if ctx.com.config.pf_overload && (Meta.has Meta.Overload f2.cf_meta && not (Meta.has Meta.Overload f.cf_meta)) then
					display_error ctx ("Field " ^ i ^ " should be declared with @:overload since it was already declared as @:overload in superclass") p
				else if not (List.memq f c.cl_overrides) then
					display_error ctx ("Field " ^ i ^ " should be declared with 'override' since it is inherited from superclass") p
				else if not f.cf_public && f2.cf_public then
					display_error ctx ("Field " ^ i ^ " has less visibility (public/private) than superclass one") p
				else (match f.cf_kind, f2.cf_kind with
				| _, Method MethInline ->
					display_error ctx ("Field " ^ i ^ " is inlined and cannot be overridden") p
				| a, b when a = b -> ()
				| Method MethInline, Method MethNormal ->
					() (* allow to redefine a method as inlined *)
				| _ ->
					display_error ctx ("Field " ^ i ^ " has different property access than in superclass") p);
				try
					let t = apply_params csup.cl_types params t in
					valid_redefinition ctx f f.cf_type f2 t
				with
					Unify_error l ->
						display_error ctx ("Field " ^ i ^ " overloads parent class with different or incomplete type") p;
						display_error ctx (error_msg (Unify l)) p;
			with
				Not_found ->
					if List.memq f c.cl_overrides then
						let msg = if is_overload then
							("Field " ^ i ^ " is declared 'override' but no compatible overload was found")
						else
							("Field " ^ i ^ " is declared 'override' but doesn't override any field")
						in
						display_error ctx msg p
			in
			if ctx.com.config.pf_overload && Meta.has Meta.Overload f.cf_meta then begin
				let overloads = get_overloads csup i in
				List.iter (fun (t,f2) ->
					(* check if any super class fields are vars *)
					match f2.cf_kind with
					| Var _ ->
						display_error ctx ("A variable named '" ^ f2.cf_name ^ "' was already declared in a superclass") f.cf_pos
					| _ -> ()
				) overloads;
				List.iter (fun f ->
					(* find the exact field being overridden *)
					check_field f (fun csup i ->
						List.find (fun (t,f2) ->
							same_overload_args f.cf_type (apply_params csup.cl_types params t) f f2
						) overloads
					) true
				) f.cf_overloads
      end else
				check_field f (fun csup i ->
					let _, t, f2 = raw_class_field (fun f -> f.cf_type) csup i in
					t, f2) false
		) c.cl_fields

let class_field_no_interf c i =
	try
		let f = PMap.find i c.cl_fields in
		f.cf_type , f
	with Not_found ->
		match c.cl_super with
		| None ->
			raise Not_found
		| Some (c,tl) ->
			(* rec over class_field *)
			let _, t , f = raw_class_field (fun f -> f.cf_type) c i in
			apply_params c.cl_types tl t , f

let rec check_interface ctx c intf params =
	let p = c.cl_pos in
	let rec check_field i f =
		(if ctx.com.config.pf_overload then
			List.iter (function
				| f2 when f != f2 ->
						check_field i f2
				| _ -> ()) f.cf_overloads);
		let is_overload = ref false in
		try
			let t2, f2 = class_field_no_interf c i in
			let t2, f2 =
				if ctx.com.config.pf_overload && (f2.cf_overloads <> [] || Meta.has Meta.Overload f2.cf_meta) then
					let overloads = get_overloads c i in
					is_overload := true;
					let t = (apply_params intf.cl_types params f.cf_type) in
					List.find (fun (t1,f1) -> same_overload_args t t1 f f1) overloads
				else
					t2, f2
			in

			ignore(follow f2.cf_type); (* force evaluation *)
			let p = (match f2.cf_expr with None -> p | Some e -> e.epos) in
			let mkind = function
				| MethNormal | MethInline -> 0
				| MethDynamic -> 1
				| MethMacro -> 2
			in
			if f.cf_public && not f2.cf_public then
				display_error ctx ("Field " ^ i ^ " should be public as requested by " ^ s_type_path intf.cl_path) p
			else if not (unify_kind f2.cf_kind f.cf_kind) || not (match f.cf_kind, f2.cf_kind with Var _ , Var _ -> true | Method m1, Method m2 -> mkind m1 = mkind m2 | _ -> false) then
				display_error ctx ("Field " ^ i ^ " has different property access than in " ^ s_type_path intf.cl_path ^ " (" ^ s_kind f2.cf_kind ^ " should be " ^ s_kind f.cf_kind ^ ")") p
			else try
				valid_redefinition ctx f2 t2 f (apply_params intf.cl_types params f.cf_type)
			with
				Unify_error l ->
					if not (Meta.has Meta.CsNative c.cl_meta && c.cl_extern) then begin
						display_error ctx ("Field " ^ i ^ " has different type than in " ^ s_type_path intf.cl_path) p;
						display_error ctx (error_msg (Unify l)) p;
					end
		with
			| Not_found when not c.cl_interface ->
				let msg = if !is_overload then
					let ctx = print_context() in
					let args = match follow f.cf_type with | TFun(args,_) -> String.concat ", " (List.map (fun (n,o,t) -> (if o then "?" else "") ^ n ^ " : " ^ (s_type ctx t)) args) | _ -> assert false in
					"No suitable overload for " ^ i ^ "( " ^ args ^ " ), as needed by " ^ s_type_path intf.cl_path ^ " was found"
				else
					("Field " ^ i ^ " needed by " ^ s_type_path intf.cl_path ^ " is missing")
				in
				display_error ctx msg p
			| Not_found -> ()
	in
	PMap.iter check_field intf.cl_fields;
	List.iter (fun (i2,p2) ->
		check_interface ctx c i2 (List.map (apply_params intf.cl_types params) p2)
	) intf.cl_implements

let check_interfaces ctx c =
	match c.cl_path with
	| "Proxy" :: _ , _ -> ()
	| _ ->
	List.iter (fun (intf,params) -> check_interface ctx c intf params) c.cl_implements

let rec return_flow ctx e =
	let error() = display_error ctx "A return is missing here" e.epos; raise Exit in
	let return_flow = return_flow ctx in
	match e.eexpr with
	| TReturn _ | TThrow _ -> ()
	| TParenthesis e | TMeta(_,e) ->
		return_flow e
	| TBlock el ->
		let rec loop = function
			| [] -> error()
			| [e] -> return_flow e
			| { eexpr = TReturn _ } :: _ | { eexpr = TThrow _ } :: _ -> ()
			| _ :: l -> loop l
		in
		loop el
	| TIf (_,e1,Some e2) ->
		return_flow e1;
		return_flow e2;
	| TSwitch (v,cases,Some e) ->
		List.iter (fun (_,e) -> return_flow e) cases;
		return_flow e
	| TSwitch ({eexpr = TMeta((Meta.Exhaustive,_,_),_)},cases,None) ->
		List.iter (fun (_,e) -> return_flow e) cases;
	| TPatMatch dt ->
		let rec loop d = match d with
			| DTExpr e -> return_flow e
			| DTGuard(_,dt1,dt2) ->
				loop dt1;
				(match dt2 with None -> () | Some dt -> loop dt)
			| DTBind (_,d) -> loop d
			| DTSwitch (_,cl,dto) ->
				List.iter (fun (_,dt) -> loop dt) cl;
				(match dto with None -> () | Some dt -> loop dt)
			| DTGoto i -> loop (dt.dt_dt_lookup.(i))
		in
		loop (dt.dt_dt_lookup.(dt.dt_first))
	| TTry (e,cases) ->
		return_flow e;
		List.iter (fun (_,e) -> return_flow e) cases;
	| TWhile({eexpr = (TConst (TBool true))},e,_) ->
		(* a special case for "inifite" while loops that have no break *)
		let rec loop e = match e.eexpr with
			(* ignore nested loops to not accidentally get one of its breaks *)
			| TWhile _ | TFor _ -> ()
			| TBreak -> error()
			| _ -> Type.iter loop e
		in
		loop e
	| _ ->
		error()

(* ---------------------------------------------------------------------- *)
(* PASS 1 & 2 : Module and Class Structure *)

let is_generic_parameter ctx c =
	(* first check field parameters, then class parameters *)
	try
		ignore (List.assoc (snd c.cl_path) ctx.curfield.cf_params);
		Meta.has Meta.Generic ctx.curfield.cf_meta
	with Not_found -> try
		ignore(List.assoc (snd c.cl_path) ctx.type_params);
		(match ctx.curclass.cl_kind with | KGeneric -> true | _ -> false);
	with Not_found ->
		false

let check_extends ctx c t p = match follow t with
	| TInst ({ cl_path = [],"Array" },_)
	| TInst ({ cl_path = [],"String" },_)
	| TInst ({ cl_path = [],"Date" },_)
	| TInst ({ cl_path = [],"Xml" },_) when ((not (platform ctx.com Cpp)) && (match c.cl_path with ("mt" | "flash") :: _ , _ -> false | _ -> true)) ->
		error "Cannot extend basic class" p;
	| TInst (csup,params) ->
		if is_parent c csup then error "Recursive class" p;
		begin match csup.cl_kind with
			| KTypeParameter _ when not (is_generic_parameter ctx csup) -> error "Cannot extend non-generic type parameters" p
			| _ -> csup,params
		end
	| _ -> error "Should extend by using a class" p

let rec add_constructor ctx c p =
	match c.cl_constructor, c.cl_super with
	| None, Some ({ cl_constructor = Some cfsup } as csup,cparams) when not c.cl_extern ->
		let cf = {
			cfsup with
			cf_pos = p;
			cf_meta = [];
			cf_doc = None;
			cf_expr = None;
		} in
		let r = exc_protect ctx (fun r ->
			let t = mk_mono() in
			r := (fun() -> t);
			let ctx = { ctx with
				curfield = cf;
				pass = PTypeField;
			} in
			ignore (follow cfsup.cf_type); (* make sure it's typed *)
			(if ctx.com.config.pf_overload then List.iter (fun cf -> ignore (follow cf.cf_type)) cf.cf_overloads);
			let args = (match cfsup.cf_expr with
				| Some { eexpr = TFunction f } ->
					List.map (fun (v,def) ->
						(*
							let's optimize a bit the output by not always copying the default value
							into the inherited constructor when it's not necessary for the platform
						*)
						match ctx.com.platform, def with
						| _, Some _ when not ctx.com.config.pf_static -> v, (Some TNull)
						| Flash, Some (TString _) -> v, (Some TNull)
						| Cpp, Some (TString _) -> v, def
						| Cpp, Some _ -> { v with v_type = ctx.t.tnull v.v_type }, (Some TNull)
						| _ -> v, def
					) f.tf_args
				| _ ->
					match follow cfsup.cf_type with
					| TFun (args,_) -> List.map (fun (n,o,t) -> alloc_var n (if o then ctx.t.tnull t else t), if o then Some TNull else None) args
					| _ -> assert false
			) in
			let p = c.cl_pos in
			let vars = List.map (fun (v,def) -> alloc_var v.v_name (apply_params csup.cl_types cparams v.v_type), def) args in
			let super_call = mk (TCall (mk (TConst TSuper) (TInst (csup,cparams)) p,List.map (fun (v,_) -> mk (TLocal v) v.v_type p) vars)) ctx.t.tvoid p in
			let constr = mk (TFunction {
				tf_args = vars;
				tf_type = ctx.t.tvoid;
				tf_expr = super_call;
			}) (TFun (List.map (fun (v,c) -> v.v_name, c <> None, v.v_type) vars,ctx.t.tvoid)) p in
			cf.cf_expr <- Some constr;
			cf.cf_type <- t;
			unify ctx t constr.etype p;
			t
		) "add_constructor" in
		cf.cf_type <- TLazy r;
		c.cl_constructor <- Some cf;
		delay ctx PForce (fun() -> ignore((!r)()));
	| _ ->
		(* nothing to do *)
		()

let set_heritance ctx c herits p =
	let ctx = { ctx with curclass = c; type_params = c.cl_types; } in
	let process_meta csup =
		List.iter (fun m ->
			match m with
			| Meta.Final, _, _ -> if not (Meta.has Meta.Hack c.cl_meta || (match c.cl_kind with KTypeParameter _ -> true | _ -> false)) then error "Cannot extend a final class" p;
			| Meta.AutoBuild, el, p -> c.cl_meta <- (Meta.Build,el,p) :: m :: c.cl_meta
			| _ -> ()
		) csup.cl_meta
	in
	let has_interf = ref false in
	let rec loop = function
		| HPrivate | HExtern | HInterface ->
			()
		| HExtends t ->
			if c.cl_super <> None then error "Cannot extend several classes" p;
			let t = load_instance ctx t p false in
			let csup,params = check_extends ctx c t p in
			csup.cl_build();
			process_meta csup;
			if c.cl_interface then begin
				if not csup.cl_interface then error "Cannot extend by using a class" p;
				c.cl_implements <- (csup,params) :: c.cl_implements
			end else begin
				if csup.cl_interface then error "Cannot extend by using an interface" p;
				c.cl_super <- Some (csup,params)
			end
		| HImplements t ->
			let t = load_instance ctx t p false in
			(match follow t with
			| TInst ({ cl_path = [],"ArrayAccess"; cl_extern = true; },[t]) ->
				if c.cl_array_access <> None then error "Duplicate array access" p;
				c.cl_array_access <- Some t
			| TInst (intf,params) ->
				intf.cl_build();
				if is_parent c intf then error "Recursive class" p;
				if c.cl_interface then error "Interfaces cannot implement another interface (use extends instead)" p;
				if not intf.cl_interface then error "You can only implements an interface" p;
				process_meta intf;
				c.cl_implements <- (intf, params) :: c.cl_implements;
				if not !has_interf then begin
					delay ctx PForce (fun() -> check_interfaces ctx c);
					has_interf := true;
				end
			| TDynamic t ->
				if c.cl_dynamic <> None then error "Cannot have several dynamics" p;
				c.cl_dynamic <- Some t
			| _ -> error "Should implement by using an interface" p)
	in
	(*
		resolve imports before calling build_inheritance, since it requires full paths.
		that means that typedefs are not working, but that's a fair limitation
	*)
	let rec resolve_imports t =
		match t.tpackage with
		| _ :: _ -> t
		| [] ->
			try
				let find = List.find (fun lt -> snd (t_path lt) = t.tname) in
				let lt = try find ctx.m.curmod.m_types with Not_found -> find ctx.m.module_types in
				{ t with tpackage = fst (t_path lt) }
			with
				Not_found -> t
	in
	let herits = List.map (function
		| HExtends t -> HExtends (resolve_imports t)
		| HImplements t -> HImplements (resolve_imports t)
		| h -> h
	) herits in
	List.iter loop (List.filter (ctx.g.do_inherit ctx c p) herits)

let rec type_type_params ?(enum_constructor=false) ctx path get_params p tp =
	let n = tp.tp_name in
	let c = mk_class ctx.m.curmod (fst path @ [snd path],n) p in
	c.cl_types <- List.map (type_type_params ctx c.cl_path get_params p) tp.tp_params;
	if enum_constructor then c.cl_meta <- (Meta.EnumConstructorParam,[],c.cl_pos) :: c.cl_meta;
	let t = TInst (c,List.map snd c.cl_types) in
	match tp.tp_constraints with
	| [] ->
		c.cl_kind <- KTypeParameter [];
		n, t
	| _ ->
		let r = exc_protect ctx (fun r ->
			r := (fun _ -> t);
			let ctx = { ctx with type_params = ctx.type_params @ get_params() } in
			let constr = List.map (load_complex_type ctx p) tp.tp_constraints in
			(* check against direct recursion *)
			let rec loop t =
				match follow t with
				| TInst (c2,_) when c == c2 -> error "Recursive constraint parameter is not allowed" p
				| TInst ({ cl_kind = KTypeParameter cl },_) ->
					List.iter loop cl
				| _ ->
					()
			in
			List.iter loop constr;
			c.cl_kind <- KTypeParameter constr;
			t
		) "constraint" in
		delay ctx PForce (fun () -> ignore(!r()));
		n, TLazy r

let type_function_params ctx fd fname p =
	let params = ref [] in
	params := List.map (fun tp ->
		type_type_params ctx ([],fname) (fun() -> !params) p tp
	) fd.f_params;
	!params

let type_function ctx args ret fmode f do_display p =
	let locals = save_locals ctx in
	let fargs = List.map (fun (n,c,t) ->
		if n.[0] = '$' then error "Function argument names starting with a dollar are not allowed" p;
		let c = (match c with
			| None -> None
			| Some e ->
				let p = pos e in
				let e = ctx.g.do_optimize ctx (type_expr ctx e (WithType t)) in
				unify ctx e.etype t p;
				let rec loop e = match e.eexpr with
					| TConst c -> Some c
					| TCast(e,None) -> loop e
					| _ -> display_error ctx "Parameter default value should be constant" p; None
				in
				loop e
		) in
		let v,c = add_local ctx n t, c in
		if n = "this" then v.v_meta <- (Meta.This,[],p) :: v.v_meta;
		v,c
	) args in
	let old_ret = ctx.ret in
	let old_fun = ctx.curfun in
	let old_opened = ctx.opened in
	ctx.curfun <- fmode;
	ctx.ret <- ret;
	ctx.opened <- [];
	let e = match f.f_expr with None -> error "Function body required" p | Some e -> e in
	let e = if not do_display then type_expr ctx e NoValue else try
		if Common.defined ctx.com Define.NoCOpt then raise Exit;
		type_expr ctx (Optimizer.optimize_completion_expr e) NoValue
	with DisplayTypes [TMono _] | Parser.TypePath (_,None) | Exit ->
		type_expr ctx e NoValue
	in
	let e = match e.eexpr with
		| TMeta((Meta.MergeBlock,_,_), ({eexpr = TBlock el} as e1)) -> e1
		| _ -> e
	in
	let rec loop e =
		match e.eexpr with
		| TReturn (Some e) -> (match follow e.etype with TAbstract({a_path = [],"Void"},[]) -> () | _ -> raise Exit)
		| TFunction _ -> ()
		| _ -> Type.iter loop e
	in
	let have_ret = (try loop e; false with Exit -> true) in
	if have_ret then
		(try return_flow ctx e with Exit -> ())
	else (try type_eq EqStrict ret ctx.t.tvoid with Unify_error _ ->
		match e.eexpr with
		(* accept final throw (issue #1923) *)
		| TBlock el when (match List.rev el with ({eexpr = TThrow _} :: _) -> true | _ -> false) -> ()
		| _ -> display_error ctx ("Missing return " ^ (s_type (print_context()) ret)) p);
	let rec loop e =
		match e.eexpr with
		| TCall ({ eexpr = TConst TSuper },_) -> raise Exit
		| TFunction _ -> ()
		| _ -> Type.iter loop e
	in
	let has_super_constr() =
		match ctx.curclass.cl_super with
		| None -> false
		| Some (csup,_) ->
			try ignore(get_constructor (fun f->f.cf_type) csup); true with Not_found -> false
	in
	if fmode = FunConstructor && has_super_constr() then
		(try
			loop e;
			display_error ctx "Missing super constructor call" p
		with
			Exit -> ());
	locals();
	let e = match ctx.curfun, ctx.vthis with
		| (FunMember|FunConstructor), Some v ->
			let ev = mk (TVar (v,Some (mk (TConst TThis) ctx.tthis p))) ctx.t.tvoid p in
			(match e.eexpr with
			| TBlock l -> { e with eexpr = TBlock (ev::l) }
			| _ -> mk (TBlock [ev;e]) e.etype p)
		| _ -> e
	in
	List.iter (fun r -> r := Closed) ctx.opened;
	ctx.ret <- old_ret;
	ctx.curfun <- old_fun;
	ctx.opened <- old_opened;
	e , fargs

let load_core_class ctx c =
	let ctx2 = (match ctx.g.core_api with
		| None ->
			let com2 = Common.clone ctx.com in
			com2.defines <- PMap.empty;
			Common.define com2 Define.CoreApi;
			Common.define com2 Define.Sys;
			if ctx.in_macro then Common.define com2 Define.Macro;
			com2.class_path <- ctx.com.std_path;
			let ctx2 = ctx.g.do_create com2 in
			ctx.g.core_api <- Some ctx2;
			ctx2
		| Some c ->
			c
	) in
	let tpath = match c.cl_kind with
		| KAbstractImpl a -> { tpackage = fst a.a_path; tname = snd a.a_path; tparams = []; tsub = None; }
		| _ -> { tpackage = fst c.cl_path; tname = snd c.cl_path; tparams = []; tsub = None; }
	in
	let t = load_instance ctx2 tpath c.cl_pos true in
	flush_pass ctx2 PFinal "core_final";
	match t with
	| TInst (ccore,_) | TAbstract({a_impl = Some ccore}, _) ->
		ccore
	| _ ->
		assert false

let init_core_api ctx c =
	let ccore = load_core_class ctx c in
	begin try
		List.iter2 (fun (n1,t1) (n2,t2) -> match follow t1, follow t2 with
			| TInst({cl_kind = KTypeParameter l1},_),TInst({cl_kind = KTypeParameter l2},_) ->
				begin try
					List.iter2 (fun t1 t2 -> type_eq EqCoreType t2 t1) l1 l2
				with
					| Invalid_argument _ ->
						error "Type parameters must have the same number of constraints as core type" c.cl_pos
					| Unify_error l ->
						display_error ctx ("Type parameter " ^ n2 ^ " has different constraint than in core type") c.cl_pos;
						display_error ctx (error_msg (Unify l)) c.cl_pos
				end
			| t1,t2 ->
				Printf.printf "%s %s" (s_type (print_context()) t1) (s_type (print_context()) t2);
				assert false
		) ccore.cl_types c.cl_types;
	with Invalid_argument _ ->
		error "Class must have the same number of type parameters as core type" c.cl_pos
	end;
	(match c.cl_doc with
	| None -> c.cl_doc <- ccore.cl_doc
	| Some _ -> ());
	let compare_fields f f2 =
		let p = (match f2.cf_expr with None -> c.cl_pos | Some e -> e.epos) in
		(try
			type_eq EqCoreType (apply_params ccore.cl_types (List.map snd c.cl_types) f.cf_type) f2.cf_type
		with Unify_error l ->
			display_error ctx ("Field " ^ f.cf_name ^ " has different type than in core type") p;
			display_error ctx (error_msg (Unify l)) p);
		if f2.cf_public <> f.cf_public then error ("Field " ^ f.cf_name ^ " has different visibility than core type") p;
		(match f2.cf_doc with
		| None -> f2.cf_doc <- f.cf_doc
		| Some _ -> ());
		if f2.cf_kind <> f.cf_kind then begin
			match f2.cf_kind, f.cf_kind with
			| Method MethInline, Method MethNormal -> () (* allow to add 'inline' *)
			| Method MethNormal, Method MethInline -> () (* allow to disable 'inline' *)
			| _ ->
				error ("Field " ^ f.cf_name ^ " has different property access than core type") p;
		end;
		(match follow f.cf_type, follow f2.cf_type with
		| TFun (pl1,_), TFun (pl2,_) ->
			if List.length pl1 != List.length pl2 then error "Argument count mismatch" p;
			List.iter2 (fun (n1,_,_) (n2,_,_) ->
				if n1 <> n2 then error ("Method parameter name '" ^ n2 ^ "' should be '" ^ n1 ^ "'") p;
			) pl1 pl2;
		| _ -> ());
	in
	let check_fields fcore fl =
		PMap.iter (fun i f ->
			if not f.cf_public then () else
			let f2 = try PMap.find f.cf_name fl with Not_found -> error ("Missing field " ^ i ^ " required by core type") c.cl_pos in
			compare_fields f f2;
		) fcore;
		PMap.iter (fun i f ->
			let p = (match f.cf_expr with None -> c.cl_pos | Some e -> e.epos) in
			if f.cf_public && not (Meta.has Meta.Hack f.cf_meta) && not (PMap.mem f.cf_name fcore) && not (List.memq f c.cl_overrides) then error ("Public field " ^ i ^ " is not part of core type") p;
		) fl;
	in
	check_fields ccore.cl_fields c.cl_fields;
	check_fields ccore.cl_statics c.cl_statics;
	(match ccore.cl_constructor, c.cl_constructor with
	| None, None -> ()
	| Some { cf_public = false }, _ -> ()
	| Some f, Some f2 -> compare_fields f f2
	| None, Some { cf_public = false } -> ()
	| _ -> error "Constructor differs from core type" c.cl_pos)

let patch_class ctx c fields =
	let h = (try Some (Hashtbl.find ctx.g.type_patches c.cl_path) with Not_found -> None) in
	match h with
	| None -> fields
	| Some (h,hcl) ->
		c.cl_meta <- c.cl_meta @ hcl.tp_meta;
		let rec loop acc = function
			| [] -> acc
			| f :: l ->
				(* patch arguments types *)
				(match f.cff_kind with
				| FFun ff ->
					let param ((n,opt,t,e) as p) =
						try
							let t2 = (try Hashtbl.find h (("$" ^ f.cff_name ^ "__" ^ n),false) with Not_found -> Hashtbl.find h (("$" ^ n),false)) in
							n, opt, t2.tp_type, e
						with Not_found ->
							p
					in
					f.cff_kind <- FFun { ff with f_args = List.map param ff.f_args }
				| _ -> ());
				(* other patches *)
				match (try Some (Hashtbl.find h (f.cff_name,List.mem AStatic f.cff_access)) with Not_found -> None) with
				| None -> loop (f :: acc) l
				| Some { tp_remove = true } -> loop acc l
				| Some p ->
					f.cff_meta <- f.cff_meta @ p.tp_meta;
					(match p.tp_type with
					| None -> ()
					| Some t ->
						f.cff_kind <- match f.cff_kind with
						| FVar (_,e) -> FVar (Some t,e)
						| FProp (get,set,_,eo) -> FProp (get,set,Some t,eo)
						| FFun f -> FFun { f with f_type = Some t });
					loop (f :: acc) l
		in
		List.rev (loop [] fields)

let rec string_list_of_expr_path (e,p) =
	match e with
	| EConst (Ident i) -> [i]
	| EField (e,f) -> f :: string_list_of_expr_path e
	| _ -> error "Invalid path" p

let build_enum_abstract ctx c a fields p =
	List.iter (fun field ->
		match field.cff_kind with
		| FVar(ct,eo) when not (List.mem AStatic field.cff_access) ->
			begin match ct with
				| Some _ -> error "Type hints on enum abstract fields are not allowed" field.cff_pos
				| None -> ()
			end;
			field.cff_access <- [AStatic;APublic;AInline];
			field.cff_meta <- (Meta.Enum,[],field.cff_pos) :: (Meta.Impl,[],field.cff_pos) :: field.cff_meta;
 			let e = match eo with
				| None -> error "Value required" field.cff_pos
				| Some e -> (ECast(e,None),field.cff_pos)
			in
			field.cff_kind <- FVar(ct,Some e)
		| _ ->
			()
	) fields;
	EVars ["",Some (CTAnonymous fields),None],p

let build_module_def ctx mt meta fvars context_init fbuild =
	let rec loop = function
		| (Meta.Build,args,p) :: l ->
			let epath, el = (match args with
				| [ECall (epath,el),p] -> epath, el
				| _ -> error "Invalid build parameters" p
			) in
			let s = try String.concat "." (List.rev (string_list_of_expr_path epath)) with Error (_,p) -> error "Build call parameter must be a class path" p in
			if ctx.in_macro then error "You cannot use @:build inside a macro : make sure that your enum is not used in macro" p;
			let old = ctx.g.get_build_infos in
			ctx.g.get_build_infos <- (fun() -> Some (mt, List.map snd (t_infos mt).mt_types, fvars()));
			context_init();
			let r = try apply_macro ctx MBuild s el p with e -> ctx.g.get_build_infos <- old; raise e in
			ctx.g.get_build_infos <- old;
			(match r with
			| None -> error "Build failure" p
			| Some e -> fbuild e; loop l)
		| (Meta.Enum,_,p) :: l ->
			begin match mt with
				| TClassDecl ({cl_kind = KAbstractImpl a} as c) ->
					context_init();
					let e = build_enum_abstract ctx c a (fvars()) p in
					fbuild e;
					loop l
				| _ ->
					loop l
			end
		| _ :: l -> loop l
		| [] -> ()
	in
	(* let errors go through to prevent resume if build fails *)
	loop meta

let init_class ctx c p context_init herits fields =
	let ctx = {
		ctx with
		curclass = c;
		type_params = c.cl_types;
		pass = PBuildClass;
		tthis = (match c.cl_kind with
			| KAbstractImpl a ->
				(match a.a_this with
				| TMono r when !r = None -> TAbstract (a,List.map snd c.cl_types)
				| t -> t)
			| _ -> TInst (c,List.map snd c.cl_types));
		on_error = (fun ctx msg ep ->
			ctx.com.error msg ep;
			(* macros expressions might reference other code, let's recall which class we are actually compiling *)
			if ep.pfile <> c.cl_pos.pfile then ctx.com.error "Defined in this class" c.cl_pos
		);
	} in
	incr stats.s_classes_built;
	let fields = patch_class ctx c fields in
	let fields = ref fields in
	let get_fields() = !fields in
	build_module_def ctx (TClassDecl c) c.cl_meta get_fields context_init (fun (e,p) ->
		match e with
		| EVars [_,Some (CTAnonymous f),None] ->
			List.iter (fun f ->
				if List.mem AMacro f.cff_access then
					(match ctx.g.macros with
					| Some (_,mctx) when Hashtbl.mem mctx.g.types_module c.cl_path ->
						(* assume that if we had already a macro with the same name, it has not been changed during the @:build operation *)
						if not (List.exists (fun f2 -> f2.cff_name = f.cff_name && List.mem AMacro f2.cff_access) (!fields)) then
							error "Class build macro cannot return a macro function when the class has already been compiled into the macro context" p
					| _ -> ())
			) f;
			fields := f
		| _ -> error "Class build macro must return a single variable with anonymous fields" p
	);
	let fields = !fields in
	let core_api = Meta.has Meta.CoreApi c.cl_meta in
	let is_class_macro = Meta.has Meta.Macro c.cl_meta in
	if is_class_macro then display_error ctx "Macro classes are no longer allowed in haxe 3" p;
	let fields, herits = if is_class_macro && not ctx.in_macro then begin
		c.cl_extern <- true;
		List.filter (fun f -> List.mem AStatic f.cff_access) fields, []
	end else fields, herits in
	if core_api && ctx.com.display = DMNone then delay ctx PForce (fun() -> init_core_api ctx c);
	let rec extends_public c =
		Meta.has Meta.PublicFields c.cl_meta ||
		match c.cl_super with
		| None -> false
		| Some (c,_) -> extends_public c
	in
	let extends_public = extends_public c in
	let is_public access parent =
		if List.mem APrivate access then
			false
		else if List.mem APublic access then
			true
		else match parent with
			| Some { cf_public = p } -> p
			| _ -> c.cl_extern || c.cl_interface || extends_public
	in
	let rec get_parent c name =
		match c.cl_super with
		| None -> None
		| Some (csup,_) ->
			try
				Some (PMap.find name csup.cl_fields)
			with
				Not_found -> get_parent csup name
	in
	let type_opt ctx p t =
		match t with
		| None when c.cl_extern || c.cl_interface ->
			display_error ctx "Type required for extern classes and interfaces" p;
			t_dynamic
		| None when core_api ->
			display_error ctx "Type required for core api classes" p;
			t_dynamic
		| _ ->
			load_type_opt ctx p t
	in
	let rec has_field f = function
		| None -> false
		| Some (c,_) ->
			PMap.exists f c.cl_fields || has_field f c.cl_super || List.exists (fun i -> has_field f (Some i)) c.cl_implements
	in

	(match c.cl_super with None -> () | Some _ -> delay ctx PForce (fun() -> check_overriding ctx c));
	if ctx.com.config.pf_overload then delay ctx PForce (fun() -> check_overloads ctx c);

	(* ----------------------- COMPLETION ----------------------------- *)

	let display_file = if ctx.com.display <> DMNone then Common.unique_full_path p.pfile = (!Parser.resume_display).pfile else false in

	let cp = !Parser.resume_display in

	let delayed_expr = ref [] in

	let rec is_full_type t =
		match t with
		| TFun (args,ret) -> is_full_type ret && List.for_all (fun (_,_,t) -> is_full_type t) args
		| TMono r -> (match !r with None -> false | Some t -> is_full_type t)
		| TAbstract _ | TInst _ | TEnum _ | TLazy _ | TDynamic _ | TAnon _ | TType _ -> true
	in
	let bind_type ctx cf r p macro =
		if ctx.com.display <> DMNone then begin
			let cp = !Parser.resume_display in
			if display_file && (cp.pmin = 0 || (p.pmin <= cp.pmin && p.pmax >= cp.pmax)) then begin
				if macro && not ctx.in_macro then
					(* force macro system loading of this class in order to get completion *)
					delay ctx PTypeField (fun() -> ignore(ctx.g.do_macro ctx MExpr c.cl_path cf.cf_name [] p))
				else begin
					cf.cf_type <- TLazy r;
					delayed_expr := (ctx,Some r) :: !delayed_expr;
				end
			end else begin
				if not (is_full_type cf.cf_type) then begin
					delayed_expr := (ctx, None) :: !delayed_expr;
					cf.cf_type <- TLazy r;
				end;
			end
		end else if macro && not ctx.in_macro then
			()
		else begin
			cf.cf_type <- TLazy r;
			delayed_expr := (ctx,Some r) :: !delayed_expr;
		end
	in

	let bind_var ctx cf e stat inline =
		let p = cf.cf_pos in
		if not stat && has_field cf.cf_name c.cl_super then error ("Redefinition of variable " ^ cf.cf_name ^ " in subclass is not allowed") p;
		let t = cf.cf_type in

		match e with
		| None -> ()
		| Some e ->
			let check_cast e =
				(* insert cast to keep explicit field type (issue #1901) *)
				if not (type_iseq e.etype cf.cf_type)
				then mk (TCast(e,None)) cf.cf_type e.epos
				else e
			in
			let r = exc_protect ctx (fun r ->
				(* type constant init fields (issue #1956) *)
				if not !return_partial_type || (match fst e with EConst _ -> true | _ -> false) then begin
					r := (fun() -> t);
					context_init();
					if ctx.com.verbose then Common.log ctx.com ("Typing " ^ (if ctx.in_macro then "macro " else "") ^ s_type_path c.cl_path ^ "." ^ cf.cf_name);
					let e = type_var_field ctx t e stat p in
					let require_constant_expression e msg = match Optimizer.make_constant_expression ctx e with
						| Some e -> e
						| None -> display_error ctx msg p; e
					in
					let e = (match cf.cf_kind with
					| Var v when c.cl_extern || Meta.has Meta.Extern cf.cf_meta ->
						if not stat then begin
							display_error ctx "Extern non-static variables may not be initialized" p;
							e
						end else if v.v_read <> AccInline then begin
							display_error ctx "Extern non-inline variables may not be initialized" p;
							e
						end else require_constant_expression e "Extern variable initialization must be a constant value"
					| Var v when is_extern_field cf ->
						(* disallow initialization of non-physical fields (issue #1958) *)
						display_error ctx "This field cannot be initialized because it is not a real variable" p; e
					| Var v when not stat ->
						let e = match Optimizer.make_constant_expression ctx e with
							| Some e -> e
							| None ->
								let rec has_this e = match e.eexpr with
									| TConst TThis ->
										display_error ctx "Cannot access this or other member field in variable initialization" e.epos;
									| _ ->
									Type.iter has_this e
								in
								has_this e;
								e
						in
						check_cast e
					| Var v when v.v_read = AccInline ->
						let e = require_constant_expression e "Inline variable initialization must be a constant value" in
						begin match c.cl_kind with
							| KAbstractImpl a when Meta.has Meta.Enum cf.cf_meta && Meta.has Meta.Enum a.a_meta ->
								unify ctx (TAbstract(a,(List.map (fun _ -> mk_mono()) a.a_types))) t p;
								begin match e.eexpr with
									| TCast(e1,None) -> unify ctx e1.etype a.a_this e1.epos
									| _ -> assert false
								end
							| _ ->
								()
						end;
						check_cast e
					| _ ->
						e
					) in
					cf.cf_expr <- Some e;
					cf.cf_type <- t;
				end;
				t
			) "bind_var" in
			bind_type ctx cf r (snd e) false
	in

	(* ----------------------- FIELD INIT ----------------------------- *)


	let loop_cf f =
		let name = f.cff_name in
		let p = f.cff_pos in
		if name.[0] = '$' && ctx.com.display = DMNone then error "Field names starting with a dollar are not allowed" p;
		let stat = List.mem AStatic f.cff_access in
		let extern = Meta.has Meta.Extern f.cff_meta || c.cl_extern in
		let is_abstract,allow_inline =
			match c.cl_kind, f.cff_kind with
			| KAbstractImpl _, _ -> true,true
			|_, FFun _ -> false,ctx.g.doinline || extern
			| _ -> false,true
		in
		let inline = List.mem AInline f.cff_access && allow_inline in
		let override = List.mem AOverride f.cff_access in
		let is_macro = Meta.has Meta.Macro f.cff_meta in
		if is_macro then ctx.com.warning "@:macro should now be 'macro' accessor" p;
		let is_macro = is_macro || List.mem AMacro f.cff_access in
		List.iter (fun acc ->
			match (acc, f.cff_kind) with
			| APublic, _ | APrivate, _ | AStatic, _ -> ()
			| ADynamic, FFun _ | AOverride, FFun _ | AMacro, FFun _ | AInline, FFun _ | AInline, FVar _ -> ()
			| _, FVar _ -> error ("Invalid accessor '" ^ Ast.s_access acc ^ "' for variable " ^ name) p
			| _, FProp _ -> error ("Invalid accessor '" ^ Ast.s_access acc ^ "' for property " ^ name) p
		) f.cff_access;
		if override then (match c.cl_super with None -> error "Invalid override: class has no super class" p | _ -> ());
		(* build the per-field context *)
		let ctx = {
			ctx with
			pass = PBuildClass; (* will be set later to PTypeExpr *)
		} in
		match f.cff_kind with
		| FVar (t,e) ->
			if not stat && is_abstract then error"Cannot declare member variable in abstract" p;
			if inline && not stat then error "Inline variable must be static" p;
			if inline && e = None then error "Inline variable must be initialized" p;

			let t = (match t with
				| None when not stat && e = None ->
					error ("Type required for member variable " ^ name) p;
				| None ->
					mk_mono()
				| Some t ->
					let old = ctx.type_params in
					if stat then ctx.type_params <- [];
					let t = load_complex_type ctx p t in
					if stat then ctx.type_params <- old;
					t
			) in
			let cf = {
				cf_name = name;
				cf_doc = f.cff_doc;
				cf_meta = f.cff_meta;
				cf_type = t;
				cf_pos = f.cff_pos;
				cf_kind = Var (if inline then { v_read = AccInline ; v_write = AccNever } else { v_read = AccNormal; v_write = AccNormal });
				cf_expr = None;
				cf_public = is_public f.cff_access None;
				cf_params = [];
				cf_overloads = [];
			} in
			ctx.curfield <- cf;
			bind_var ctx cf e stat inline;
			f, false, cf, true
		| FFun fd ->
			let params = type_function_params ctx fd f.cff_name p in
			if inline && c.cl_interface then error "You can't declare inline methods in interfaces" p;
			if Meta.has Meta.Generic f.cff_meta then begin
				if params = [] then error "Generic functions must have type parameters" p;
			end;
			let is_macro = is_macro || (is_class_macro && stat) in
			let f, stat, fd = if not is_macro || stat then
				f, stat, fd
			else if ctx.in_macro then
				(* non-static macros methods are turned into static when we are running the macro *)
				{ f with cff_access = AStatic :: f.cff_access }, true, fd
			else
				(* remove display of first argument which will contain the "this" expression *)
				f, stat, { fd with f_args = match fd.f_args with [] -> [] | _ :: l -> l }
			in
			let fd = if not is_macro then
				fd
			else begin
				if ctx.in_macro then begin
					(* a class with a macro cannot be extern in macro context (issue #2015) *)
					c.cl_extern <- false;
					let texpr = CTPath { tpackage = ["haxe";"macro"]; tname = "Expr"; tparams = []; tsub = None } in
					(* ExprOf type parameter might contain platform-specific type, let's replace it by Expr *)
					let no_expr_of = function
						| CTPath { tpackage = ["haxe";"macro"]; tname = "Expr"; tsub = Some ("ExprOf"); tparams = [TPType _] }
						| CTPath { tpackage = []; tname = ("ExprOf"); tsub = None; tparams = [TPType _] } -> Some texpr
						| t -> Some t
					in
					{
						f_params = fd.f_params;
						f_type = (match fd.f_type with None -> Some texpr | Some t -> no_expr_of t);
						f_args = List.map (fun (a,o,t,e) -> a,o,(match t with None -> Some texpr | Some t -> no_expr_of t),e) fd.f_args;
						f_expr = fd.f_expr;
					}
				end else
					let tdyn = Some (CTPath { tpackage = []; tname = "Dynamic"; tparams = []; tsub = None }) in
					let to_dyn = function
						| { tpackage = ["haxe";"macro"]; tname = "Expr"; tsub = Some ("ExprOf"); tparams = [TPType t] } -> Some t
						| { tpackage = []; tname = ("ExprOf"); tsub = None; tparams = [TPType t] } -> Some t
						| { tpackage = ["haxe"]; tname = ("PosInfos"); tsub = None; tparams = [] } -> error "haxe.PosInfos is not allowed on macro functions, use Context.currentPos() instead" p
						| _ -> tdyn
					in
					{
						f_params = fd.f_params;
						f_type = (match fd.f_type with Some (CTPath t) -> to_dyn t | _ -> tdyn);
						f_args = List.map (fun (a,o,t,_) -> a,o,(match t with Some (CTPath t) -> to_dyn t | _ -> tdyn),None) fd.f_args;
						f_expr = None;
					}
			end in
			let parent = (if not stat then get_parent c name else None) in
			let dynamic = List.mem ADynamic f.cff_access || (match parent with Some { cf_kind = Method MethDynamic } -> true | _ -> false) in
			if inline && dynamic then error "You can't have both 'inline' and 'dynamic'" p;
			ctx.type_params <- (match c.cl_kind with
				| KAbstractImpl a when Meta.has Meta.Impl f.cff_meta || Meta.has Meta.From f.cff_meta || Meta.has Meta.MultiType a.a_meta && Meta.has Meta.To f.cff_meta ->
					params @ a.a_types
				| _ ->
					if stat then params else params @ ctx.type_params);
			let constr = (name = "new") in
			let ret = if constr then ctx.t.tvoid else type_opt ctx p fd.f_type in
			let args = List.map (fun (name,opt,t,c) ->
				let t, c = type_function_param ctx (type_opt ctx p t) c opt p in
				name, c, t
			) fd.f_args in
			let t = TFun (fun_args args,ret) in
			if c.cl_interface && not stat && fd.f_expr <> None then error "An interface method cannot have a body" p;
			if constr then begin
				if c.cl_interface then error "An interface cannot have a constructor" p;
				if stat then error "A constructor must not be static" p;
				match fd.f_type with
					| None | Some (CTPath { tpackage = []; tname = "Void" }) -> ()
					| _ -> error "A class constructor can't have a return value" p
			end;
			let cf = {
				cf_name = name;
				cf_doc = f.cff_doc;
				cf_meta = f.cff_meta;
				cf_type = t;
				cf_pos = f.cff_pos;
				cf_kind = Method (if is_macro then MethMacro else if inline then MethInline else if dynamic then MethDynamic else MethNormal);
				cf_expr = None;
				cf_public = is_public f.cff_access parent;
				cf_params = params;
				cf_overloads = [];
			} in
			let do_bind = ref (((not c.cl_extern || inline) && not c.cl_interface) || cf.cf_name = "__init__") in
			let do_add = ref true in
			(match c.cl_kind with
				| KAbstractImpl a ->
					let m = mk_mono() in
					let ta = TAbstract(a, List.map (fun _ -> mk_mono()) a.a_types) in
					let tthis = if Meta.has Meta.Impl f.cff_meta || Meta.has Meta.To f.cff_meta then monomorphs a.a_types a.a_this else a.a_this in
					let check_bind () =
						if fd.f_expr = None then begin
							if inline then error ("Inline functions must have an expression") f.cff_pos;
							begin match fd.f_type with
								| None -> error ("Functions without expressions must have an explicit return type") f.cff_pos
								| Some _ -> ()
							end;
							do_add := false;
							do_bind := false;
						end
					in
					let rec loop ml = match ml with
						| (Meta.From,_,_) :: _ ->
							if is_macro then error "Macro cast functions are not supported" p;
							(* the return type of a from-function must be the abstract, not the underlying type *)
							(try type_eq EqStrict ret ta with Unify_error l -> error (error_msg (Unify l)) p);
							let t = match t with
								| TFun([_,_,t],_) -> t
								| _ -> error "@:from cast functions must accept exactly one argument" p
							in
							a.a_from <- (t,Some cf) :: a.a_from;
						| (Meta.To,_,_) :: _ ->
							if is_macro then error "Macro cast functions are not supported" p;
							let args = if Meta.has Meta.MultiType a.a_meta then begin
								(* the return type of multitype @:to functions must unify with a_this *)
								delay ctx PFinal (fun () -> unify ctx m tthis f.cff_pos);
								(* the arguments must be compatible with the original constructor, which we have to find at this point *)
								try (match follow (monomorphs a.a_types (PMap.find "_new" c.cl_statics).cf_type) with
									| TFun(args,_) -> List.map (fun (_,_,t) -> t) args
									| _ -> assert false)
								with Not_found ->
									error "Constructor of multi-type abstract must be defined before the individual @:to-functions are" cf.cf_pos
							end else [] in
							(* the first argument of a to-function must be the underlying type, not the abstract *)
							(try unify_raise ctx t (tfun (tthis :: args) m) f.cff_pos with Error (Unify l,p) -> error (error_msg (Unify l)) p);
							if not (Meta.has Meta.Impl cf.cf_meta) then cf.cf_meta <- (Meta.Impl,[],cf.cf_pos) :: cf.cf_meta;
							let m = match follow m with
								| TMono _ when (match cf.cf_type with TFun(_,r) -> r == t_dynamic | _ -> false) -> t_dynamic
								| m -> m
							in
							a.a_to <- (m, Some cf) :: a.a_to
						| (Meta.ArrayAccess,_,_) :: _ ->
							if is_macro then error "Macro array-access functions are not supported" p;
							a.a_array <- cf :: a.a_array;
							if Meta.has Meta.CoreType a.a_meta then check_bind();
						| (Meta.Op,[EBinop(op,_,_),_],_) :: _ ->
							if is_macro then error "Macro operator functions are not supported" p;
							let targ = if Meta.has Meta.Impl f.cff_meta then tthis else ta in
							let left_eq = type_iseq t (tfun [targ;m] (mk_mono())) in
							let right_eq = type_iseq t (tfun [mk_mono();targ] (mk_mono())) in
							if not (left_eq || right_eq) then error ("The left or right argument type must be " ^ (s_type (print_context()) targ)) f.cff_pos;
							if right_eq && Meta.has Meta.Commutative f.cff_meta then error ("@:commutative is only allowed if the right argument is not " ^ (s_type (print_context()) targ)) f.cff_pos;
							a.a_ops <- (op,cf) :: a.a_ops;
							check_bind();
						| (Meta.Op,[EUnop(op,flag,_),_],_) :: _ ->
							if is_macro then error "Macro operator functions are not supported" p;
							let targ = if Meta.has Meta.Impl f.cff_meta then tthis else ta in
							(try type_eq EqStrict t (tfun [targ] (mk_mono())) with Unify_error l -> raise (Error ((Unify l),f.cff_pos)));
							a.a_unops <- (op,flag,cf) :: a.a_unops;
							check_bind();
						| _ :: ml ->
							loop ml
						| [] ->
							()
					in
					loop f.cff_meta;
					if f.cff_name = "_new" && Meta.has Meta.MultiType a.a_meta then do_bind := false;
				| _ ->
					());
			init_meta_overloads ctx cf;
			ctx.curfield <- cf;
			let r = exc_protect ctx (fun r ->
				if not !return_partial_type then begin
					r := (fun() -> t);
					context_init();
					incr stats.s_methods_typed;
					if ctx.com.verbose then Common.log ctx.com ("Typing " ^ (if ctx.in_macro then "macro " else "") ^ s_type_path c.cl_path ^ "." ^ name);
					let fmode = (match c.cl_kind with
						| KAbstractImpl _ ->
							(match args with
							| ("this",_,_) :: _ -> FunMemberAbstract
							| _ when name = "_new" -> FunMemberAbstract
							| _ -> FunStatic)
						| _ ->
							if constr then FunConstructor else if stat then FunStatic else FunMember
					) in
					let display_field = display_file && (f.cff_pos.pmin <= cp.pmin && f.cff_pos.pmax >= cp.pmax) in
					let e , fargs = type_function ctx args ret fmode fd display_field p in
					let f = {
						tf_args = fargs;
						tf_type = ret;
						tf_expr = e;
					} in
					if stat && name = "__init__" then
						(match e.eexpr with
						| TBlock [] | TBlock [{ eexpr = TConst _ }] | TConst _ | TObjectDecl [] -> ()
						| _ -> c.cl_init <- Some e);
					cf.cf_expr <- Some (mk (TFunction f) t p);
					cf.cf_type <- t;
				end;
				t
			) "type_fun" in
			if !do_bind then bind_type ctx cf r (match fd.f_expr with Some e -> snd e | None -> f.cff_pos) is_macro;
			f, constr, cf, !do_add
		| FProp (get,set,t,eo) ->
			(match c.cl_kind with
			| KAbstractImpl a when Meta.has Meta.Impl f.cff_meta ->
				ctx.type_params <- a.a_types;
			| _ -> ());
			let ret = (match t, eo with
				| None, None -> error "Property must either define a type or a default value" p;
				| None, _ -> mk_mono()
				| Some t, _ -> load_complex_type ctx p t
			) in
			let t_get,t_set = match c.cl_kind with
				| KAbstractImpl a when Meta.has Meta.Impl f.cff_meta ->
					if Meta.has Meta.IsVar f.cff_meta then error "Abstract properties cannot be real variables" f.cff_pos;
					let ta = apply_params a.a_types (List.map snd a.a_types) a.a_this in
					tfun [ta] ret, tfun [ta;ret] ret
				| _ -> tfun [] ret, tfun [ret] ret
			in
			let check_method m t req_name =
				if ctx.com.display <> DMNone then () else
				try
					let _, t2, f = (if stat then let f = PMap.find m c.cl_statics in Some c, f.cf_type, f else class_field c m) in
					(* accessors must be public on As3 (issue #1872) *)
					if Common.defined ctx.com Define.As3 then f.cf_meta <- (Meta.Public,[],p) :: f.cf_meta;
					(match f.cf_kind with
						| Method MethMacro ->
							display_error ctx "Macro methods cannot be used as property accessor" p;
							display_error ctx "Accessor method is here" f.cf_pos;
						| _ -> ());
					unify_raise ctx t2 t f.cf_pos;
					(match req_name with None -> () | Some n -> display_error ctx ("Please use " ^ n ^ " to name your property access method") f.cf_pos);
				with
					| Error (Unify l,p) -> raise (Error (Stack (Custom ("In method " ^ m ^ " required by property " ^ name),Unify l),p))
					| Not_found ->
						if req_name <> None then display_error ctx "Custom property accessor is no longer supported, please use get/set" p else
						if not (c.cl_interface || c.cl_extern) then display_error ctx ("Method " ^ m ^ " required by property " ^ name ^ " is missing") p
			in
			let get = (match get with
				| "null" -> AccNo
				| "dynamic" -> AccCall
				| "never" -> AccNever
				| "default" -> AccNormal
				| _ ->
					let get = if get = "get" then "get_" ^ name else get in
					delay ctx PForce (fun() -> check_method get t_get (if get <> "get" && get <> "get_" ^ name then Some ("get_" ^ name) else None));
					AccCall
			) in
			let set = (match set with
				| "null" ->
					(* standard flash library read-only variables can't be accessed for writing, even in subclasses *)
					if c.cl_extern && (match c.cl_path with "flash" :: _	, _ -> true | _ -> false) && ctx.com.platform = Flash then
						AccNever
					else
						AccNo
				| "never" -> AccNever
				| "dynamic" -> AccCall
				| "default" -> AccNormal
				| _ ->
					let set = if set = "set" then "set_" ^ name else set in
					delay ctx PForce (fun() -> check_method set t_set (if set <> "set" && set <> "set_" ^ name then Some ("set_" ^ name) else None));
					AccCall
			) in
			if set = AccNormal && (match get with AccCall -> true | _ -> false) then error "Unsupported property combination" p;
			let cf = {
				cf_name = name;
				cf_doc = f.cff_doc;
				cf_meta = f.cff_meta;
				cf_pos = f.cff_pos;
				cf_kind = Var { v_read = get; v_write = set };
				cf_expr = None;
				cf_type = ret;
				cf_public = is_public f.cff_access None;
				cf_params = [];
				cf_overloads = [];
			} in
			ctx.curfield <- cf;
			bind_var ctx cf eo stat inline;
			f, false, cf, true
	in
	let rec check_require = function
		| [] -> None
		| (Meta.Require,conds,_) :: l ->
			let rec loop = function
				| [] -> check_require l
				| e :: l ->
					let sc = match fst e with
						| EConst (Ident s) -> s
						| EBinop ((OpEq|OpNotEq|OpGt|OpGte|OpLt|OpLte) as op,(EConst (Ident s),_),(EConst ((Int _ | Float _ | String _) as c),_)) -> s ^ s_binop op ^ s_constant c
						| _ -> ""
					in
					if not (Parser.is_true (Parser.eval ctx.com e)) then
						Some (sc,(match List.rev l with (EConst (String msg),_) :: _ -> Some msg | _ -> None))
					else
						loop l
			in
			loop conds
		| _ :: l ->
			check_require l
	in
	let cl_req = check_require c.cl_meta in
	List.iter (fun f ->
		let p = f.cff_pos in
		try
			let fd , constr, f, do_add = loop_cf f in
			let is_static = List.mem AStatic fd.cff_access in
			if (is_static || constr) && c.cl_interface && f.cf_name <> "__init__" then error "You can't declare static fields in interfaces" p;
			begin try
				let _,args,_ = Meta.get Meta.IfFeature f.cf_meta in
				List.iter (fun e -> match fst e with
					| EConst(String s) ->
						ctx.m.curmod.m_extra.m_features <- (s,(c,f,is_static)) :: ctx.m.curmod.m_extra.m_features;
					| _ ->
						error "String expected" (pos e)
				) args
			with Not_found -> () end;
			let req = check_require fd.cff_meta in
			let req = (match req with None -> if is_static || constr then cl_req else None | _ -> req) in
			(match req with
			| None -> ()
			| Some r -> f.cf_kind <- Var { v_read = AccRequire (fst r, snd r); v_write = AccRequire (fst r, snd r) });
			if constr then begin
				match c.cl_constructor with
					| None ->
							c.cl_constructor <- Some f
					| Some ctor when ctx.com.config.pf_overload ->
              if Meta.has Meta.Overload f.cf_meta && Meta.has Meta.Overload ctor.cf_meta then
  							ctor.cf_overloads <- f :: ctor.cf_overloads
              else if Meta.has Meta.Overload f.cf_meta <> Meta.has Meta.Overload ctor.cf_meta then
								display_error ctx ("If using overloaded constructors, all constructors must be declared with @:overload") (if Meta.has Meta.Overload f.cf_meta then ctor.cf_pos else f.cf_pos)
					| Some ctor ->
								display_error ctx "Duplicate constructor" p
			end else if not is_static || f.cf_name <> "__init__" then begin
				let dup = if is_static then PMap.exists f.cf_name c.cl_fields || has_field f.cf_name c.cl_super else PMap.exists f.cf_name c.cl_statics in
				if dup then error ("Same field name can't be use for both static and instance : " ^ f.cf_name) p;
				if List.mem AOverride fd.cff_access then c.cl_overrides <- f :: c.cl_overrides;
				let is_var f = match f.cf_kind with | Var _ -> true | _ -> false in
				if PMap.mem f.cf_name (if is_static then c.cl_statics else c.cl_fields) then
					if ctx.com.config.pf_overload && Meta.has Meta.Overload f.cf_meta && not (is_var f) then
						let mainf = PMap.find f.cf_name (if is_static then c.cl_statics else c.cl_fields) in
						if is_var mainf then display_error ctx "Cannot declare a variable with same name as a method" mainf.cf_pos;
						(if not (Meta.has Meta.Overload mainf.cf_meta) then display_error ctx ("Overloaded methods must have @:overload metadata") mainf.cf_pos);
						mainf.cf_overloads <- f :: mainf.cf_overloads
					else
						display_error ctx ("Duplicate class field declaration : " ^ f.cf_name) p
				else
				if not do_add then
					()
				else if is_static then begin
					c.cl_statics <- PMap.add f.cf_name f c.cl_statics;
					c.cl_ordered_statics <- f :: c.cl_ordered_statics;
				end else begin
					c.cl_fields <- PMap.add f.cf_name f c.cl_fields;
					c.cl_ordered_fields <- f :: c.cl_ordered_fields;
				end;
			end
		with Error (Custom str,p2) when p = p2 ->
			display_error ctx str p
	) fields;
	(match c.cl_kind with
	| KAbstractImpl a ->
		a.a_to <- List.rev a.a_to;
		a.a_from <- List.rev a.a_from;
		a.a_ops <- List.rev a.a_ops;
		a.a_unops <- List.rev a.a_unops;
	| _ -> ());
	c.cl_ordered_statics <- List.rev c.cl_ordered_statics;
	c.cl_ordered_fields <- List.rev c.cl_ordered_fields;
	(*
		make sure a default contructor with same access as super one will be added to the class structure at some point.
	*)
	(* add_constructor does not deal with overloads correctly *)
	if not ctx.com.config.pf_overload then add_constructor ctx c p;
	(* check overloaded constructors *)
	(if ctx.com.config.pf_overload then match c.cl_constructor with
	| Some ctor ->
		List.iter (fun f ->
			try
				(* TODO: consider making a broader check, and treat some types, like TAnon and type parameters as Dynamic *)
				ignore(List.find (fun f2 -> f != f2 && same_overload_args f.cf_type f2.cf_type f f2) (ctor :: ctor.cf_overloads));
				display_error ctx ("Another overloaded field of same signature was already declared : " ^ f.cf_name) f.cf_pos;
			with Not_found -> ()
		) (ctor :: ctor.cf_overloads)
	| _ -> ());
	(* push delays in reverse order so they will be run in correct order *)
	List.iter (fun (ctx,r) ->
		ctx.pass <- PTypeField;
		(match r with
		| None -> ()
		| Some r -> delay ctx PTypeField (fun() -> ignore((!r)())))
	) !delayed_expr

let resolve_typedef t =
	match t with
	| TClassDecl _ | TEnumDecl _ | TAbstractDecl _ -> t
	| TTypeDecl td ->
		match follow td.t_type with
		| TEnum (e,_) -> TEnumDecl e
		| TInst (c,_) -> TClassDecl c
		| TAbstract (a,_) -> TAbstractDecl a
		| _ -> t

let add_module ctx m p =
	let decl_type t =
		let t = t_infos t in
		try
			let m2 = Hashtbl.find ctx.g.types_module t.mt_path in
			if m.m_path <> m2 && String.lowercase (s_type_path m2) = String.lowercase (s_type_path m.m_path) then error ("Module " ^ s_type_path m2 ^ " is loaded with a different case than " ^ s_type_path m.m_path) p;
			error ("Type name " ^ s_type_path t.mt_path ^ " is redefined from module " ^ s_type_path m2) p
		with
			Not_found ->
				Hashtbl.add ctx.g.types_module t.mt_path m.m_path
	in
	List.iter decl_type m.m_types;
	Hashtbl.add ctx.g.modules m.m_path m

(*
	In this pass, we can access load and access other modules types, but we cannot follow them or access their structure
	since they have not been setup. We also build a context_init list that will be evaluated the first time we evaluate
	an expression into the context
*)
let rec init_module_type ctx context_init do_init (decl,p) =
	let get_type name =
		try List.find (fun t -> snd (t_infos t).mt_path = name) ctx.m.curmod.m_types with Not_found -> assert false
	in
	match decl with
	| EImport (path,mode) ->
		let rec loop acc = function
			| x :: l when is_lower_ident (fst x) -> loop (x::acc) l
			| rest -> List.rev acc, rest
		in
		let pack, rest = loop [] path in
		(match rest with
		| [] ->
			(match mode with
			| IAll ->
				ctx.m.wildcard_packages <- List.map fst pack :: ctx.m.wildcard_packages
			| _ ->
				(match List.rev path with
				| [] -> assert false
				| (_,p) :: _ -> error "Module name must start with an uppercase letter" p))
		| (tname,p2) :: rest ->
			let p1 = (match pack with [] -> p2 | (_,p1) :: _ -> p1) in
			let p = punion p1 p2 in
			let md = ctx.g.do_load_module ctx (List.map fst pack,tname) p in
			let types = md.m_types in
			let no_private t = not (t_infos t).mt_private in
			let chk_private t p = if (t_infos t).mt_private then error "You can't import a private type" p in
			let has_name name t = snd (t_infos t).mt_path = name in
			let get_type tname =
				let t = (try List.find (has_name tname) types with Not_found -> error (string_error tname (List.map (fun mt -> snd (t_infos mt).mt_path) types) ("Module " ^ s_type_path md.m_path ^ " does not define type " ^ tname)) p) in
				chk_private t p;
				t
			in
			let rebind t name =
				let _, _, f = ctx.g.do_build_instance ctx t p in
				(* create a temp private typedef, does not register it in module *)
				TTypeDecl {
					t_path = (fst md.m_path @ ["_" ^ snd md.m_path],name);
					t_module = md;
					t_pos = p;
					t_private = true;
					t_doc = None;
					t_meta = [];
					t_types = (t_infos t).mt_types;
					t_type = f (List.map snd (t_infos t).mt_types);
				}
			in
			let add_static_init t name s =
				let name = (match name with None -> s | Some n -> n) in
				match resolve_typedef t with
				| TClassDecl c ->
					c.cl_build();
					ignore(PMap.find s c.cl_statics);
					ctx.m.module_globals <- PMap.add name (TClassDecl c,s) ctx.m.module_globals
				| TEnumDecl e ->
					ignore(PMap.find s e.e_constrs);
					ctx.m.module_globals <- PMap.add name (TEnumDecl e,s) ctx.m.module_globals
				| _ ->
					raise Not_found
			in
			(match mode with
			| INormal | IAsName _ ->
				let name = (match mode with IAsName n -> Some n | _ -> None) in
				(match rest with
				| [] ->
					(match name with
					| None ->
						ctx.m.module_types <- List.filter no_private types @ ctx.m.module_types
					| Some newname ->
						ctx.m.module_types <- rebind (get_type tname) newname :: ctx.m.module_types);
				| [tsub,p2] ->
					let p = punion p1 p2 in
					(try
						let tsub = List.find (has_name tsub) types in
						chk_private tsub p;
						ctx.m.module_types <- (match name with None -> tsub | Some n -> rebind tsub n) :: ctx.m.module_types
					with Not_found ->
						(* this might be a static property, wait later to check *)
						let tmain = get_type tname in
						context_init := (fun() ->
							try
								add_static_init tmain name tsub
							with Not_found ->
								error (s_type_path (t_infos tmain).mt_path ^ " has no field or subtype " ^ tsub) p
						) :: !context_init)
				| (tsub,p2) :: (fname,p3) :: rest ->
					(match rest with
					| [] -> ()
					| (n,p) :: _ -> error ("Unexpected " ^ n) p);
					let tsub = get_type tsub in
					context_init := (fun() ->
						try
							add_static_init tsub name fname
						with Not_found ->
							error (s_type_path (t_infos tsub).mt_path ^ " has no field " ^ fname) (punion p p3)
					) :: !context_init;
				)
			| IAll ->
				let t = (match rest with
					| [] -> get_type tname
					| [tsub,_] -> get_type tsub
					| _ :: (n,p) :: _ -> error ("Unexpected " ^ n) p
				) in
				context_init := (fun() ->
					match resolve_typedef t with
					| TClassDecl c
					| TAbstractDecl {a_impl = Some c} ->
						c.cl_build();
						PMap.iter (fun _ cf -> if not (has_meta Meta.NoImportGlobal cf.cf_meta) then ctx.m.module_globals <- PMap.add cf.cf_name (TClassDecl c,cf.cf_name) ctx.m.module_globals) c.cl_statics
					| TEnumDecl e ->
						PMap.iter (fun _ c -> if not (has_meta Meta.NoImportGlobal c.ef_meta) then ctx.m.module_globals <- PMap.add c.ef_name (TEnumDecl e,c.ef_name) ctx.m.module_globals) e.e_constrs
					| _ ->
						error "No statics to import from this type" p
				) :: !context_init
			))
	| EUsing t ->
		(* do the import first *)
		let types = (match t.tsub with
			| None ->
				let md = ctx.g.do_load_module ctx (t.tpackage,t.tname) p in
				let types = List.filter (fun t -> not (t_infos t).mt_private) md.m_types in
				ctx.m.module_types <- types @ ctx.m.module_types;
				types
			| Some _ ->
				let t = load_type_def ctx p t in
				ctx.m.module_types <- t :: ctx.m.module_types;
				[t]
		) in
		(* delay the using since we need to resolve typedefs *)
		let filter_classes types =
			let rec loop acc types = match types with
				| td :: l ->
					(match resolve_typedef td with
					| TClassDecl c | TAbstractDecl({a_impl = Some c}) ->
						loop (c :: acc) l
					| td ->
						loop acc l)
				| [] ->
					acc
			in
			loop [] types
		in
		context_init := (fun() -> ctx.m.module_using <- filter_classes types @ ctx.m.module_using) :: !context_init
	| EClass d ->
		let c = (match get_type d.d_name with TClassDecl c -> c | _ -> assert false) in
		let herits = d.d_flags in
		if Meta.has Meta.Generic c.cl_meta && c.cl_types <> [] then c.cl_kind <- KGeneric;
		if Meta.has Meta.GenericBuild c.cl_meta then c.cl_kind <- KGenericBuild d.d_data;
		if c.cl_path = (["haxe";"macro"],"MacroType") then c.cl_kind <- KMacroType;
		c.cl_extern <- List.mem HExtern herits;
		c.cl_interface <- List.mem HInterface herits;
		let build() =
			c.cl_build <- (fun()->());
			set_heritance ctx c herits p;
			init_class ctx c p do_init d.d_flags d.d_data
		in
		ctx.pass <- PBuildClass;
		ctx.curclass <- c;
		c.cl_build <- make_pass ctx build;
		ctx.pass <- PBuildModule;
		ctx.curclass <- null_class;
		delay ctx PBuildClass (fun() -> c.cl_build());
	| EEnum d ->
		let e = (match get_type d.d_name with TEnumDecl e -> e | _ -> assert false) in
		let ctx = { ctx with type_params = e.e_types } in
		let h = (try Some (Hashtbl.find ctx.g.type_patches e.e_path) with Not_found -> None) in
		(match h with
		| None -> ()
		| Some (h,hcl) ->
			Hashtbl.iter (fun _ _ -> error "Field type patch not supported for enums" e.e_pos) h;
			e.e_meta <- e.e_meta @ hcl.tp_meta);
		let constructs = ref d.d_data in
		let get_constructs() =
			List.map (fun c ->
				{
					cff_name = c.ec_name;
					cff_doc = c.ec_doc;
					cff_meta = c.ec_meta;
					cff_pos = c.ec_pos;
					cff_access = [];
					cff_kind = (match c.ec_args, c.ec_params with
						| [], [] -> FVar (c.ec_type,None)
						| _ -> FFun { f_params = c.ec_params; f_type = c.ec_type; f_expr = None; f_args = List.map (fun (n,o,t) -> n,o,Some t,None) c.ec_args });
				}
			) (!constructs)
		in
		let init () = List.iter (fun f -> f()) !context_init in
		build_module_def ctx (TEnumDecl e) e.e_meta get_constructs init (fun (e,p) ->
			match e with
			| EVars [_,Some (CTAnonymous fields),None] ->
				constructs := List.map (fun f ->
					let args, params, t = (match f.cff_kind with
					| FVar (t,None) -> [], [], t
					| FFun { f_params = pl; f_type = t; f_expr = (None|Some (EBlock [],_)); f_args = al } ->
						let al = List.map (fun (n,o,t,_) -> match t with None -> error "Missing function parameter type" f.cff_pos | Some t -> n,o,t) al in
						al, pl, t
					| _ ->
						error "Invalid enum constructor in @:build result" p
					) in
					{
						ec_name = f.cff_name;
						ec_doc = f.cff_doc;
						ec_meta = f.cff_meta;
						ec_pos = f.cff_pos;
						ec_args = args;
						ec_params = params;
						ec_type = t;
					}
				) fields
			| _ -> error "Enum build macro must return a single variable with anonymous object fields" p
		);
		let et = TEnum (e,List.map snd e.e_types) in
		let names = ref [] in
		let index = ref 0 in
		let is_flat = ref true in
		let fields = ref PMap.empty in
		List.iter (fun c ->
			let p = c.ec_pos in
			let params = ref [] in
			params := List.map (fun tp -> type_type_params ~enum_constructor:true ctx ([],c.ec_name) (fun() -> !params) c.ec_pos tp) c.ec_params;
			let params = !params in
			let ctx = { ctx with type_params = params @ ctx.type_params } in
			let rt = (match c.ec_type with
				| None -> et
				| Some t ->
					let t = load_complex_type ctx p t in
					(match follow t with
					| TEnum (te,_) when te == e ->
						()
					| _ ->
						error "Explicit enum type must be of the same enum type" p);
					t
			) in
			let t = (match c.ec_args with
				| [] -> rt
				| l ->
					is_flat := false;
					let pnames = ref PMap.empty in
					TFun (List.map (fun (s,opt,t) ->
						(match t with CTPath({tpackage=[];tname="Void"}) -> error "Arguments of type Void are not allowed in enum constructors" c.ec_pos | _ -> ());
						if PMap.mem s (!pnames) then error ("Duplicate parameter '" ^ s ^ "' in enum constructor " ^ c.ec_name) p;
						pnames := PMap.add s () (!pnames);
						s, opt, load_type_opt ~opt ctx p (Some t)
					) l, rt)
			) in
			if PMap.mem c.ec_name e.e_constrs then error ("Duplicate constructor " ^ c.ec_name) p;
			let f = {
				ef_name = c.ec_name;
				ef_type = t;
				ef_pos = p;
				ef_doc = c.ec_doc;
				ef_index = !index;
				ef_params = params;
				ef_meta = c.ec_meta;
			} in
			let cf = {
				cf_name = f.ef_name;
				cf_public = true;
				cf_type = f.ef_type;
				cf_kind = (match follow f.ef_type with
					| TFun _ -> Method MethNormal
					| _ -> Var { v_read = AccNormal; v_write = AccNo }
				);
				cf_pos = e.e_pos;
				cf_doc = None;
				cf_meta = no_meta;
				cf_expr = None;
				cf_params = f.ef_params;
				cf_overloads = [];
			} in
			e.e_constrs <- PMap.add f.ef_name f e.e_constrs;
			fields := PMap.add cf.cf_name cf !fields;
			incr index;
			names := c.ec_name :: !names;
		) (!constructs);
		e.e_names <- List.rev !names;
		e.e_extern <- e.e_extern;
		e.e_type.t_types <- e.e_types;
		e.e_type.t_type <- TAnon {
			a_fields = !fields;
			a_status = ref (EnumStatics e);
		};
		if !is_flat then e.e_meta <- (Meta.FlatEnum,[],e.e_pos) :: e.e_meta;
	| ETypedef d ->
		let t = (match get_type d.d_name with TTypeDecl t -> t | _ -> assert false) in
		let ctx = { ctx with type_params = t.t_types } in
		let tt = load_complex_type ctx p d.d_data in
		(*
			we exceptionnaly allow follow here because we don't care the type we get as long as it's not our own
		*)
		(match d.d_data with
		| CTExtend _ -> ()
		| _ ->
			if t.t_type == follow tt then error "Recursive typedef is not allowed" p);
		(match t.t_type with
		| TMono r ->
			(match !r with
			| None -> r := Some tt;
			| Some _ -> assert false);
		| _ -> assert false);
	| EAbstract d ->
		let a = (match get_type d.d_name with TAbstractDecl a -> a | _ -> assert false) in
		let ctx = { ctx with type_params = a.a_types } in
		let is_type = ref false in
		let load_type t from =
			let t = load_complex_type ctx p t in
			if not (Meta.has Meta.CoreType a.a_meta) then begin
				if !is_type then begin
					delay ctx PFinal (fun () ->
						let at = monomorphs a.a_types a.a_this in
						(try (if from then Type.unify t at else Type.unify at t) with Unify_error _ -> error "You can only declare from/to with compatible types" p)
					);
				end else
					error "Missing underlying type declaration or @:coreType declaration" p;
			end;
			t
		in
		List.iter (function
			| AFromType t -> a.a_from <- (load_type t true, None) :: a.a_from
			| AToType t -> a.a_to <- (load_type t false, None) :: a.a_to
			| AIsType t ->
				if a.a_impl = None then error "Abstracts with underlying type must have an implementation" a.a_pos;
				if Meta.has Meta.CoreType a.a_meta then error "@:coreType abstracts cannot have an underlying type" p;
				let at = load_complex_type ctx p t in
				(match at with TAbstract(a2,_) when a == a2 -> error "Abstract underlying type cannot be recursive" a.a_pos | _ -> ());
				a.a_this <- at;
				is_type := true;
			| APrivAbstract -> ()
		) d.d_flags;
		if not !is_type then begin
			if Meta.has Meta.CoreType a.a_meta then
				a.a_this <- TAbstract(a,List.map snd a.a_types)
			else
				error "Abstract is missing underlying type declaration" a.a_pos
		end

let type_module ctx m file tdecls p =
	let m, decls, tdecls = make_module ctx m file tdecls p in
	add_module ctx m p;
	(* define the per-module context for the next pass *)
	let ctx = {
		com = ctx.com;
		g = ctx.g;
		t = ctx.t;
		m = {
			curmod = m;
			module_types = ctx.g.std.m_types;
			module_using = [];
			module_globals = PMap.empty;
			wildcard_packages = [];
		};
		meta = [];
		this_stack = [];
		with_type_stack = [];
		pass = PBuildModule;
		on_error = (fun ctx msg p -> ctx.com.error msg p);
		macro_depth = ctx.macro_depth;
		curclass = null_class;
		curfield = null_field;
		tthis = ctx.tthis;
		ret = ctx.ret;
		locals = PMap.empty;
		type_params = [];
		curfun = FunStatic;
		untyped = false;
		in_super_call = false;
		in_macro = ctx.in_macro;
		in_display = false;
		in_loop = false;
		opened = [];
		vthis = None;
	} in
	if ctx.g.std != null_module then begin
		add_dependency m ctx.g.std;
		(* this will ensure both String and (indirectly) Array which are basic types which might be referenced *)
		ignore(load_core_type ctx "String");
	end;
	(* here is an additional PASS 1 phase, which define the type parameters for all module types.
		 Constraints are handled lazily (no other type is loaded) because they might be recursive anyway *)
	List.iter (fun d ->
		match d with
		| (TClassDecl c, (EClass d, p)) ->
			c.cl_types <- List.map (type_type_params ctx c.cl_path (fun() -> c.cl_types) p) d.d_params;
		| (TEnumDecl e, (EEnum d, p)) ->
			e.e_types <- List.map (type_type_params ctx e.e_path (fun() -> e.e_types) p) d.d_params;
		| (TTypeDecl t, (ETypedef d, p)) ->
			t.t_types <- List.map (type_type_params ctx t.t_path (fun() -> t.t_types) p) d.d_params;
		| (TAbstractDecl a, (EAbstract d, p)) ->
			a.a_types <- List.map (type_type_params ctx a.a_path (fun() -> a.a_types) p) d.d_params;
		| _ ->
			assert false
	) decls;
	(* setup module types *)
	let context_init = ref [] in
	let do_init() =
		match !context_init with
		| [] -> ()
		| l -> context_init := []; List.iter (fun f -> f()) (List.rev l)
	in
	List.iter (init_module_type ctx context_init do_init) tdecls;
	m


let resolve_module_file com m remap p =
	let forbid = ref false in
	let file = (match m with
		| [] , name -> name
		| x :: l , name ->
			let x = (try
				match PMap.find x com.package_rules with
				| Forbidden -> forbid := true; x
				| Directory d -> d
				| Remap d -> remap := d :: l; d
				with Not_found -> x
			) in
			String.concat "/" (x :: l) ^ "/" ^ name
	) ^ ".hx" in
	let file = Common.find_file com file in
	let file = (match String.lowercase (snd m) with
	| "con" | "aux" | "prn" | "nul" | "com1" | "com2" | "com3" | "lpt1" | "lpt2" | "lpt3" when Sys.os_type = "Win32" ->
		(* these names are reserved by the OS - old DOS legacy, such files cannot be easily created but are reported as visible *)
		if (try (Unix.stat file).Unix.st_size with _ -> 0) > 0 then file else raise Not_found
	| _ -> file
	) in
	(* if we try to load a std.xxxx class and resolve a real std file, the package name is not valid, ignore *)
	(match fst m with
	| "std" :: _ ->
		let file = Common.unique_full_path file in
		if List.exists (fun path -> ExtString.String.starts_with file (try Common.unique_full_path path with _ -> path)) com.std_path then raise Not_found;
	| _ -> ());
	if !forbid then begin
		let _, decls = (!parse_hook) com file p in
		let meta = (match decls with
		| (EClass d,_) :: _ -> d.d_meta
		| (EEnum d,_) :: _ -> d.d_meta
		| (EAbstract d,_) :: _ -> d.d_meta
		| (ETypedef d,_) :: _ -> d.d_meta
		| _ -> []
		) in
		if not (Meta.has Meta.NoPackageRestrict meta) then begin
			let x = (match fst m with [] -> assert false | x :: _ -> x) in
			raise (Forbid_package ((x,m,p),[],if Common.defined com Define.Macro then "macro" else platform_name com.platform));
		end;
	end;
	file

let parse_module ctx m p =
	let remap = ref (fst m) in
	let file = resolve_module_file ctx.com m remap p in
	let pack, decls = (!parse_hook) ctx.com file p in
	if pack <> !remap then begin
		let spack m = if m = [] then "<empty>" else String.concat "." m in
		if p == Ast.null_pos then
			display_error ctx ("Invalid commandline class : " ^ s_type_path m ^ " should be " ^ s_type_path (pack,snd m)) p
		else
			display_error ctx ("Invalid package : " ^ spack (fst m) ^ " should be " ^ spack pack) p
	end;
	file, if !remap <> fst m then
		(* build typedefs to redirect to real package *)
		List.rev (List.fold_left (fun acc (t,p) ->
			let build f d =
				let priv = List.mem f d.d_flags in
				(ETypedef {
					d_name = d.d_name;
					d_doc = None;
					d_meta = [];
					d_params = d.d_params;
					d_flags = if priv then [EPrivate] else [];
					d_data = CTPath (if priv then { tpackage = []; tname = "Dynamic"; tparams = []; tsub = None; } else
						{
							tpackage = !remap;
							tname = d.d_name;
							tparams = List.map (fun tp ->
								TPType (CTPath { tpackage = []; tname = tp.tp_name; tparams = []; tsub = None; })
							) d.d_params;
							tsub = None;
						});
				},p) :: acc
			in
			match t with
			| EClass d -> build HPrivate d
			| EEnum d -> build EPrivate d
			| ETypedef d -> build EPrivate d
			| EAbstract d -> build APrivAbstract d
			| EImport _ | EUsing _ -> acc
		) [(EImport (List.map (fun s -> s,null_pos) (!remap @ [snd m]),INormal),null_pos)] decls)
	else
		decls

let load_module ctx m p =
	let m2 = (try
		Hashtbl.find ctx.g.modules m
	with
		Not_found ->
			match !type_module_hook ctx m p with
			| Some m -> m
			| None ->
			let file, decls = (try
				parse_module ctx m p
			with Not_found ->
				let rec loop = function
					| [] ->
						raise (Error (Module_not_found m,p))
					| load :: l ->
						match load m p with
						| None -> loop l
						| Some (file,(_,a)) -> file, a
				in
				loop ctx.com.load_extern_type
			) in
			try
				type_module ctx m file decls p
			with Forbid_package (inf,pl,pf) when p <> Ast.null_pos ->
				raise (Forbid_package (inf,p::pl,pf))
	) in
	add_dependency ctx.m.curmod m2;
	if ctx.pass = PTypeField then flush_pass ctx PBuildClass "load_module";
	m2

;;
type_function_params_rec := type_function_params
