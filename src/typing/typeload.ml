(*
	The Haxe Compiler
	Copyright (C) 2005-2018  Haxe Foundation

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
open Common
open Common.DisplayMode
open Type
open Typecore
open Error
open Globals

exception Build_canceled of build_state

let locate_macro_error = ref true
let build_count = ref 0

let transform_abstract_field com this_t a_t a f =
	let stat = List.mem AStatic f.cff_access in
	let p = f.cff_pos in
	match f.cff_kind with
	| FProp ((("get" | "never"),_),(("set" | "never"),_),_,_) when not stat ->
		(* TODO: hack to avoid issues with abstract property generation on As3 *)
		if Common.defined com Define.As3 then f.cff_meta <- (Meta.Extern,[],null_pos) :: f.cff_meta;
		{ f with cff_access = AStatic :: f.cff_access; cff_meta = (Meta.Impl,[],null_pos) :: f.cff_meta }
	| FProp _ when not stat ->
		error "Member property accessors must be get/set or never" p;
	| FFun fu when fst f.cff_name = "new" && not stat ->
		let init p = (EVars [("this",null_pos),Some this_t,None],p) in
		let cast e = (ECast(e,None)),pos e in
		let ret p = (EReturn (Some (cast (EConst (Ident "this"),p))),p) in
		let meta = (Meta.Impl,[],null_pos) :: f.cff_meta in
		let meta = if Meta.has Meta.MultiType a.a_meta then begin
			if List.mem AInline f.cff_access then error "MultiType constructors cannot be inline" f.cff_pos;
			if fu.f_expr <> None then error "MultiType constructors cannot have a body" f.cff_pos;
			(Meta.Extern,[],null_pos) :: meta
		end else
			meta
		in
		(* We don't want the generated expression positions to shadow the real code. *)
		let p = { p with pmax = p.pmin } in
		let fu = {
			fu with
			f_expr = (match fu.f_expr with
			| None -> if Meta.has Meta.MultiType a.a_meta then Some (EConst (Ident "null"),p) else None
			| Some (EBlock el,_) -> Some (EBlock (init p :: el @ [ret p]),p)
			| Some e -> Some (EBlock [init p;e;ret p],p)
			);
			f_type = Some a_t;
		} in
		{ f with cff_name = "_new",pos f.cff_name; cff_access = AStatic :: f.cff_access; cff_kind = FFun fu; cff_meta = meta }
	| FFun fu when not stat ->
		if Meta.has Meta.From f.cff_meta then error "@:from cast functions must be static" f.cff_pos;
		let fu = { fu with f_args = (if List.mem AMacro f.cff_access then fu.f_args else (("this",null_pos),false,[],Some this_t,None) :: fu.f_args) } in
		{ f with cff_kind = FFun fu; cff_access = AStatic :: f.cff_access; cff_meta = (Meta.Impl,[],null_pos) :: f.cff_meta }
	| _ ->
		f

let get_policy ctx mpath =
	let sl1 = full_dot_path mpath mpath in
	List.fold_left (fun acc (sl2,policy,recursive) -> if match_path recursive sl1 sl2 then policy @ acc else acc) [] ctx.g.module_check_policies

let make_module ctx mpath file loadp =
	let m = {
		m_id = alloc_mid();
		m_path = mpath;
		m_types = [];
		m_extra = module_extra (Path.unique_full_path file) (Define.get_signature ctx.com.defines) (file_time file) (if ctx.in_macro then MMacro else MCode) (get_policy ctx mpath);
	} in
	m

(*
	Build module structure : should be atomic - no type loading is possible
*)
let module_pass_1 ctx m tdecls loadp =
	let com = ctx.com in
	let decls = ref [] in
	let make_path name priv =
		if List.exists (fun (t,_) -> snd (t_path t) = name) !decls then error ("Type name " ^ name ^ " is already defined in this module") loadp;
		if priv then (fst m.m_path @ ["_" ^ snd m.m_path], name) else (fst m.m_path, name)
	in
	let pt = ref None in
	let rec make_decl acc decl =
		let p = snd decl in
		let acc = (match fst decl with
		| EImport _ | EUsing _ ->
			(match !pt with
			| None -> acc
			| Some _ -> error "import and using may not appear after a type declaration" p)
		| EClass d ->
			let name = fst d.d_name in
			if String.length name > 0 && name.[0] = '$' then error "Type names starting with a dollar are not allowed" p;
			pt := Some p;
			let priv = List.mem HPrivate d.d_flags in
			let path = make_path name priv in
			let c = mk_class m path p (pos d.d_name) in
			(* we shouldn't load any other type until we propertly set cl_build *)
			c.cl_build <- (fun() -> error (s_type_path c.cl_path ^ " is not ready to be accessed, separate your type declarations in several files") p);
			c.cl_module <- m;
			c.cl_private <- priv;
			c.cl_doc <- d.d_doc;
			c.cl_meta <- d.d_meta;
			decls := (TClassDecl c, decl) :: !decls;
			acc
		| EEnum d ->
			let name = fst d.d_name in
			if String.length name > 0 && name.[0] = '$' then error "Type names starting with a dollar are not allowed" p;
			pt := Some p;
			let priv = List.mem EPrivate d.d_flags in
			let path = make_path name priv in
			let e = {
				e_path = path;
				e_module = m;
				e_pos = p;
				e_name_pos = (pos d.d_name);
				e_doc = d.d_doc;
				e_meta = d.d_meta;
				e_params = [];
				e_private = priv;
				e_extern = List.mem EExtern d.d_flags;
				e_constrs = PMap.empty;
				e_names = [];
				e_type = enum_module_type m path p;
			} in
			decls := (TEnumDecl e, decl) :: !decls;
			acc
		| ETypedef d ->
			let name = fst d.d_name in
			if String.length name > 0 && name.[0] = '$' then error "Type names starting with a dollar are not allowed" p;
			pt := Some p;
			let priv = List.mem EPrivate d.d_flags in
			let path = make_path name priv in
			let t = {
				t_path = path;
				t_module = m;
				t_pos = p;
				t_name_pos = pos d.d_name;
				t_doc = d.d_doc;
				t_private = priv;
				t_params = [];
				t_type = mk_mono();
				t_meta = d.d_meta;
			} in
			(* failsafe in case the typedef is not initialized (see #3933) *)
			delay ctx PBuildModule (fun () ->
				match t.t_type with
				| TMono r -> (match !r with None -> r := Some com.basic.tvoid | _ -> ())
				| _ -> ()
			);
			decls := (TTypeDecl t, decl) :: !decls;
			acc
		 | EAbstract d ->
		 	let name = fst d.d_name in
			if String.length name > 0 && name.[0] = '$' then error "Type names starting with a dollar are not allowed" p;
			let priv = List.mem APrivAbstract d.d_flags in
			let path = make_path name priv in
			let a = {
				a_path = path;
				a_private = priv;
				a_module = m;
				a_pos = p;
				a_name_pos = pos d.d_name;
				a_doc = d.d_doc;
				a_params = [];
				a_meta = d.d_meta;
				a_from = [];
				a_to = [];
				a_from_field = [];
				a_to_field = [];
				a_ops = [];
				a_unops = [];
				a_impl = None;
				a_array = [];
				a_this = mk_mono();
				a_resolve = None;
			} in
			decls := (TAbstractDecl a, decl) :: !decls;
			match d.d_data with
			| [] when Meta.has Meta.CoreType a.a_meta ->
				a.a_this <- t_dynamic;
				acc
			| fields ->
				let a_t =
					let params = List.map (fun t -> TPType (CTPath { tname = fst t.tp_name; tparams = []; tsub = None; tpackage = [] },null_pos)) d.d_params in
					CTPath { tpackage = []; tname = fst d.d_name; tparams = params; tsub = None },null_pos
				in
				let rec loop = function
					| [] -> a_t
					| AIsType t :: _ -> t
					| _ :: l -> loop l
				in
				let this_t = loop d.d_flags in
				let fields = List.map (transform_abstract_field com this_t a_t a) fields in
				let meta = ref [] in
				if has_meta Meta.Dce a.a_meta then meta := (Meta.Dce,[],null_pos) :: !meta;
				let acc = make_decl acc (EClass { d_name = (fst d.d_name) ^ "_Impl_",snd d.d_name; d_flags = [HPrivate]; d_data = fields; d_doc = None; d_params = []; d_meta = !meta },p) in
				(match !decls with
				| (TClassDecl c,_) :: _ ->
					List.iter (fun m -> match m with
						| ((Meta.Build | Meta.CoreApi | Meta.Allow | Meta.Access | Meta.Enum | Meta.Dce | Meta.Native | Meta.JsRequire | Meta.PythonImport | Meta.Expose | Meta.Deprecated | Meta.PhpGlobal),_,_) ->
							c.cl_meta <- m :: c.cl_meta;
						| _ ->
							()
					) a.a_meta;
					a.a_impl <- Some c;
					c.cl_kind <- KAbstractImpl a;
					c.cl_meta <- (Meta.Final,[],null_pos) :: c.cl_meta
				| _ -> assert false);
				acc
		) in
		decl :: acc
	in
	let tdecls = List.fold_left make_decl [] tdecls in
	let decls = List.rev !decls in
	decls, List.rev tdecls

let parse_file_from_lexbuf com file p lexbuf =
	let t = Timer.timer ["parsing"] in
	Lexer.init file true;
	incr stats.s_files_parsed;
	let data = try
		ParserEntry.parse com.defines lexbuf
	with
		| Sedlexing.MalFormed ->
			t();
			error "Malformed file. Source files must be encoded with UTF-8." {pfile = file; pmin = 0; pmax = 0}
		| e ->
			t();
			raise e
	in
	begin match !display_default with
		| DMModuleSymbols filter when filter <> None || Display.is_display_file file ->
			let ds = Display.DocumentSymbols.collect_module_symbols data in
			com.shared.shared_display_information.document_symbols <- (file,ds) :: com.shared.shared_display_information.document_symbols;
		| _ ->
			()
	end;
	t();
	Common.log com ("Parsed " ^ file);
	data

let parse_file_from_string com file p string =
	parse_file_from_lexbuf com file p (Sedlexing.Utf8.from_string string)

let current_stdin = ref None (* TODO: we're supposed to clear this at some point *)

let parse_file com file p =
	let use_stdin = (Common.defined com Define.DisplayStdin) && Display.is_display_file file in
	if use_stdin then
		let s =
			match !current_stdin with
			| Some s ->
				s
			| None ->
				let s = Std.input_all stdin in
				close_in stdin;
				current_stdin := Some s;
				s
		in
		parse_file_from_string com file p s
	else
		let ch = try open_in_bin file with _ -> error ("Could not open " ^ file) p in
		Std.finally (fun() -> close_in ch) (parse_file_from_lexbuf com file p) (Sedlexing.Utf8.from_channel ch)

let parse_hook = ref parse_file
let type_module_hook = ref (fun _ _ _ -> None)
let type_function_params_rec = ref (fun _ _ _ _ -> assert false)
let return_partial_type = ref false

let type_function_arg ctx t e opt p =
	if opt then
		let e = (match e with None -> Some (EConst (Ident "null"),p) | _ -> e) in
		ctx.t.tnull t, e
	else
		let t = match e with Some (EConst (Ident "null"),p) -> ctx.t.tnull t | _ -> t in
		t, e

let type_var_field ctx t e stat do_display p =
	if stat then ctx.curfun <- FunStatic else ctx.curfun <- FunMember;
	let e = if do_display then Display.ExprPreprocessing.process_expr ctx.com e else e in
	let e = type_expr ctx e (WithType t) in
	let e = (!cast_or_unify_ref) ctx t e p in
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
	if tname = "" then raise (Display.DisplayToplevel (DisplayToplevel.collect ctx true));
	try
		if t.tsub <> None then raise Not_found;
		let path_matches t2 =
			let tp = t_path t2 in
			tp = (t.tpackage,tname) || (no_pack && snd tp = tname)
		in
		try
			List.find path_matches ctx.m.curmod.m_types
		with Not_found ->
			let t,pi = List.find (fun (t2,pi) -> path_matches t2) ctx.m.module_types in
			Display.ImportHandling.mark_import_position ctx.com pi;
			t
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
				let rec loop l = match l with
					| [] -> raise Exit
					| (wp,pi) :: l ->
						try
							let t = load_type_def ctx p { t with tpackage = wp } in
							Display.ImportHandling.mark_import_position ctx.com pi;
							t
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

let resolve_position_by_path ctx path p =
	let mt = load_type_def ctx p path in
	let p = (t_infos mt).mt_pos in
	raise (Display.DisplayPosition [p])

let check_param_constraints ctx types t pl c p =
	match follow t with
	| TMono _ -> ()
	| _ ->
		let ctl = (match c.cl_kind with KTypeParameter l -> l | _ -> []) in
		List.iter (fun ti ->
			let ti = apply_params types pl ti in
			let ti = (match follow ti with
				| TInst ({ cl_kind = KGeneric } as c,pl) ->
					(* if we solve a generic contraint, let's substitute with the actual generic instance before unifying *)
					let _,_, f = ctx.g.do_build_instance ctx (TClassDecl c) p in
					f pl
				| _ -> ti
			) in
			try
				unify_raise ctx t ti p
			with Error(Unify l,p) ->
				if not ctx.untyped then display_error ctx (error_msg (Unify (Constraint_failure (s_type_path c.cl_path) :: l))) p;
		) ctl

let requires_value_meta com co =
	Common.defined com Define.DocGen || (match co with
		| None -> false
		| Some c -> c.cl_extern || Meta.has Meta.Rtti c.cl_meta)

let generate_value_meta com co cf args =
	if requires_value_meta com co then begin
		let values = List.fold_left (fun acc ((name,p),_,_,_,eo) -> match eo with Some e -> ((name,p,NoQuotes),e) :: acc | _ -> acc) [] args in
		match values with
			| [] -> ()
			| _ -> cf.cf_meta <- ((Meta.Value,[EObjectDecl values,cf.cf_pos],null_pos) :: cf.cf_meta)
	end

let pselect p1 p2 =
	if p1 = null_pos then p2 else p1

(* build an instance from a full type *)
let rec load_instance ?(allow_display=false) ctx (t,pn) allow_no_params p =
	let p = pselect pn p in
	let t = try
		if t.tpackage <> [] || t.tsub <> None then raise Not_found;
		let pt = List.assoc t.tname ctx.type_params in
		if t.tparams <> [] then error ("Class type parameter " ^ t.tname ^ " can't have parameters") p;
		pt
	with Not_found ->
		let mt = load_type_def ctx p t in
		let is_generic,is_generic_build = match mt with
			| TClassDecl {cl_kind = KGeneric} -> true,false
			| TClassDecl {cl_kind = KGenericBuild _} -> false,true
			| _ -> false,false
		in
		let types , path , f = ctx.g.do_build_instance ctx mt p in
		let is_rest = is_generic_build && (match types with ["Rest",_] -> true | _ -> false) in
		if allow_no_params && t.tparams = [] && not is_rest then begin
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
			| [TPType t] -> TDynamic (load_complex_type ctx true p t)
			| _ -> error "Too many parameters for Dynamic" p
		else begin
			if not is_rest && ctx.com.display.dms_error_policy <> EPIgnore && List.length types <> List.length t.tparams then error ("Invalid number of type parameters for " ^ s_type_path path) p;
			let tparams = List.map (fun t ->
				match t with
				| TPExpr e ->
					let name = (match fst e with
						| EConst (String s) -> "S" ^ s
						| EConst (Int i) -> "I" ^ i
						| EConst (Float f) -> "F" ^ f
						| _ -> "Expr"
					) in
					let c = mk_class ctx.m.curmod ([],name) p (pos e) in
					c.cl_kind <- KExpr e;
					TInst (c,[])
				| TPType t -> load_complex_type ctx true p t
			) t.tparams in
			let rec loop tl1 tl2 is_rest = match tl1,tl2 with
				| t :: tl1,(name,t2) :: tl2 ->
					let check_const c =
						let is_expression = (match t with TInst ({ cl_kind = KExpr _ },_) -> true | _ -> false) in
						let expects_expression = name = "Const" || Meta.has Meta.Const c.cl_meta in
						let accepts_expression = name = "Rest" in
						if is_expression then begin
							if not expects_expression && not accepts_expression then
								error "Constant value unexpected here" p
						end else if expects_expression then
							error "Type parameter is expected to be a constant value" p
					in
					let is_rest = is_rest || name = "Rest" && is_generic_build in
					let t = match follow t2 with
						| TInst ({ cl_kind = KTypeParameter [] } as c, []) when not is_generic ->
							check_const c;
							t
						| TInst (c,[]) ->
							check_const c;
							let r = exc_protect ctx (fun r ->
								r := lazy_available t;
								delay ctx PCheckConstraint (fun() -> check_param_constraints ctx types t tparams c p);
								t
							) "constraint" in
							TLazy r
						| _ -> assert false
					in
					t :: loop tl1 tl2 is_rest
				| [],[] ->
					[]
				| [],["Rest",_] when is_generic_build ->
					[]
				| [],(_,t) :: tl when ctx.com.display.dms_error_policy = EPIgnore ->
					t :: loop [] tl is_rest
				| [],_ ->
					error ("Not enough type parameters for " ^ s_type_path path) p
				| t :: tl,[] ->
					if is_rest then
						t :: loop tl [] true
					else
						error ("Too many parameters for " ^ s_type_path path) p
			in
			let params = loop tparams types false in
			f params
		end
	in
	if allow_display then Display.DisplayEmitter.check_display_type ctx t pn;
	t

(*
	build an instance from a complex type
*)
and load_complex_type ctx allow_display p (t,pn) =
	let p = pselect pn p in
	match t with
	| CTParent t -> load_complex_type ctx allow_display p t
	| CTPath t -> load_instance ~allow_display ctx (t,pn) false p
	| CTOptional _ -> error "Optional type not allowed here" p
	| CTNamed _ -> error "Named type not allowed here" p
	| CTExtend (tl,l) ->
		(match load_complex_type ctx allow_display p (CTAnonymous l,p) with
		| TAnon a as ta ->
			let is_redefined cf1 a2 =
				try
					let cf2 = PMap.find cf1.cf_name a2.a_fields in
					let st = s_type (print_context()) in
					if not (type_iseq cf1.cf_type cf2.cf_type) then begin
						display_error ctx ("Cannot redefine field " ^ cf1.cf_name ^ " with different type") p;
						display_error ctx ("First type was " ^ (st cf1.cf_type)) cf1.cf_pos;
						error ("Second type was " ^ (st cf2.cf_type)) cf2.cf_pos
					end else
						true
				with Not_found ->
					false
			in
			let mk_extension t =
				match follow t with
				| TInst ({cl_kind = KTypeParameter _},_) ->
					error "Cannot structurally extend type parameters" p
				| TMono _ ->
					error "Loop found in cascading signatures definitions. Please change order/import" p
				| TAnon a2 ->
					PMap.iter (fun _ cf -> ignore(is_redefined cf a2)) a.a_fields;
					TAnon { a_fields = (PMap.foldi PMap.add a.a_fields a2.a_fields); a_status = ref (Extend [t]); }
				| _ -> error "Can only extend structures" p
			in
			let loop t = match follow t with
				| TAnon a2 ->
					PMap.iter (fun f cf ->
						if not (is_redefined cf a) then
							a.a_fields <- PMap.add f cf a.a_fields
					) a2.a_fields
				| _ ->
					error "Can only extend structures" p
			in
			let il = List.map (fun (t,pn) -> load_instance ctx ~allow_display (t,pn) false p) tl in
			let tr = ref None in
			let t = TMono tr in
			let r = exc_protect ctx (fun r ->
				r := lazy_processing (fun() -> t);
				tr := Some (match il with
					| [i] ->
						mk_extension i
					| _ ->
						List.iter loop il;
						a.a_status := Extend il;
						ta);
				t
			) "constraint" in
			TLazy r
		| _ -> assert false)
	| CTAnonymous l ->
		let rec loop acc f =
			let n = fst f.cff_name in
			let p = f.cff_pos in
			if PMap.mem n acc then error ("Duplicate field declaration : " ^ n) p;
			let topt = function
				| None -> error ("Explicit type required for field " ^ n) p
				| Some t -> load_complex_type ctx allow_display p t
			in
			if n = "new" then ctx.com.warning "Structures with new are deprecated, use haxe.Constraints.Constructible instead" p;
			let no_expr = function
				| None -> ()
				| Some (_,p) -> error "Expression not allowed here" p
			in
			let pub = ref true in
			let dyn = ref false in
			let params = ref [] in
			let final = ref false in
			List.iter (fun a ->
				match a with
				| APublic -> ()
				| APrivate -> pub := false;
				| ADynamic when (match f.cff_kind with FFun _ -> true | _ -> false) -> dyn := true
				| AFinal -> final := true
				| AStatic | AOverride | AInline | ADynamic | AMacro -> error ("Invalid access " ^ Ast.s_access a) p
			) f.cff_access;
			let t , access = (match f.cff_kind with
				| FVar(t,e) when !final ->
					no_expr e;
					let t = (match t with None -> error "Type required for structure property" p | Some t -> t) in
					load_complex_type ctx allow_display p t, Var { v_read = AccNormal; v_write = AccNever }
				| FVar (Some (CTPath({tpackage=[];tname="Void"}),_), _)  | FProp (_,_,Some (CTPath({tpackage=[];tname="Void"}),_),_) ->
					error "Fields of type Void are not allowed in structures" p
				| FVar (t, e) ->
					no_expr e;
					topt t, Var { v_read = AccNormal; v_write = AccNormal }
				| FFun fd ->
					params := (!type_function_params_rec) ctx fd (fst f.cff_name) p;
					no_expr fd.f_expr;
					let old = ctx.type_params in
					ctx.type_params <- !params @ old;
					let args = List.map (fun ((name,_),o,_,t,e) -> no_expr e; name, o, topt t) fd.f_args in
					let t = TFun (args,topt fd.f_type), Method (if !dyn then MethDynamic else MethNormal) in
					ctx.type_params <- old;
					t
				| FProp (i1,i2,t,e) ->
					no_expr e;
					let access (m,_) get =
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
					load_complex_type ctx allow_display p t, Var { v_read = access i1 true; v_write = access i2 false }
			) in
			let t = if Meta.has Meta.Optional f.cff_meta then ctx.t.tnull t else t in
			let cf = {
				(mk_field n t p (pos f.cff_name)) with
				cf_public = !pub;
				cf_kind = access;
				cf_params = !params;
				cf_doc = f.cff_doc;
				cf_meta = f.cff_meta;
			} in
			init_meta_overloads ctx None cf;
			if ctx.is_display_file then begin
				Display.DisplayEmitter.check_display_metadata ctx cf.cf_meta;
				Display.DisplayEmitter.maybe_display_field ctx (cf.cf_name_pos) cf;
			end;
			PMap.add n cf acc
		in
		mk_anon (List.fold_left loop PMap.empty l)
	| CTFunction (args,r) ->
		match args with
		| [CTPath { tpackage = []; tparams = []; tname = "Void" },_] ->
			TFun ([],load_complex_type ctx allow_display p r)
		| _ ->
			TFun (List.map (fun t ->
				let t, opt = (match fst t with CTOptional t -> t, true | _ -> t,false) in
				let n,t = (match fst t with CTNamed (n,t) -> (fst n), t | _ -> "", t) in
				n,opt,load_complex_type ctx allow_display p t
			) args,load_complex_type ctx allow_display p r)

and init_meta_overloads ctx co cf =
	let overloads = ref [] in
	let filter_meta m = match m with
		| ((Meta.Overload | Meta.Value),_,_) -> false
		| _ -> true
	in
	let cf_meta = List.filter filter_meta cf.cf_meta in
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
			let topt = function None -> error "Explicit type required" p | Some t -> load_complex_type ctx true p t in
			let args = List.map (fun ((a,_),opt,_,t,_) -> a,opt,topt t) f.f_args in
			let cf = { cf with cf_type = TFun (args,topt f.f_type); cf_params = params; cf_meta = cf_meta} in
			generate_value_meta ctx.com co cf f.f_args;
			overloads := cf :: !overloads;
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
	cf.cf_overloads <- (List.rev !overloads)

let hide_params ctx =
	let old_m = ctx.m in
	let old_type_params = ctx.type_params in
	let old_deps = ctx.g.std.m_extra.m_deps in
	ctx.m <- {
		curmod = ctx.g.std;
		module_types = [];
		module_using = [];
		module_globals = PMap.empty;
		wildcard_packages = [];
		module_imports = [];
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
	let show = hide_params ctx in
	let t = load_instance ctx ({ tpackage = []; tname = name; tparams = []; tsub = None; },null_pos) false null_pos in
	show();
	add_dependency ctx.m.curmod (match t with
	| TInst (c,_) -> c.cl_module
	| TType (t,_) -> t.t_module
	| TAbstract (a,_) -> a.a_module
	| TEnum (e,_) -> e.e_module
	| _ -> assert false);
	t

let t_iterator ctx =
	let show = hide_params ctx in
	match load_type_def ctx null_pos { tpackage = []; tname = "Iterator"; tparams = []; tsub = None } with
	| TTypeDecl t ->
		show();
		add_dependency ctx.m.curmod t.t_module;
		if List.length t.t_params <> 1 then assert false;
		let pt = mk_mono() in
		apply_params t.t_params [pt] t.t_type, pt
	| _ ->
		assert false

(*
	load either a type t or Null<Unknown> if not defined
*)
let load_type_hint ?(opt=false) ctx pcur t =
	let t = match t with
		| None -> mk_mono()
		| Some (t,p) ->
			try
				load_complex_type ctx true pcur (t,p)
			with Error(Module_not_found(([],name)),p) as exc ->
				if Display.Diagnostics.is_diagnostics_run ctx then DisplayToplevel.handle_unresolved_identifier ctx name p true;
				(* Default to Dynamic in display mode *)
				if ctx.com.display.dms_display then t_dynamic else raise exc
	in
	if opt then ctx.t.tnull t else t

(* ---------------------------------------------------------------------- *)
(* Structure check *)

let valid_redefinition ctx f1 t1 f2 t2 = (* child, parent *)
	let valid t1 t2 =
		Type.unify t1 t2;
		if is_null t1 <> is_null t2 || ((follow t1) == t_dynamic && (follow t2) != t_dynamic) then raise (Unify_error [Cannot_unify (t1,t2)]);
	in
	let open OptimizerTexpr in
	begin match PurityState.get_purity_from_meta f2.cf_meta,PurityState.get_purity_from_meta f1.cf_meta with
		| PurityState.Pure,PurityState.MaybePure -> f1.cf_meta <- (Meta.Pure,[EConst(Ident "expect"),f2.cf_pos],null_pos) :: f1.cf_meta
		| PurityState.ExpectPure p,PurityState.MaybePure -> f1.cf_meta <- (Meta.Pure,[EConst(Ident "expect"),p],null_pos) :: f1.cf_meta
		| _ -> ()
	end;
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
									let t1 = apply_params l1 monos (apply_params c1.cl_params pl1 t1) in
									let t2 = apply_params l2 monos (apply_params c2.cl_params pl2 t2) in
									type_eq EqStrict t1 t2
								with Unify_error l ->
									raise (Unify_error (Unify_custom "Constraints differ" :: l))
							) ct1 ct2
						in
						to_check := check :: !to_check;
					| _ ->
						raise (Unify_error [Unify_custom "Different number of constraints"]))
				| _ -> ());
				TInst (mk_class null_module ([],name) null_pos null_pos,[])
			) l1 l2 in
			List.iter (fun f -> f monos) !to_check;
			apply_params l1 monos t1, apply_params l2 monos t2
		| _  ->
			(* ignore type params, will create other errors later *)
			t1, t2
	) in
	match f1.cf_kind,f2.cf_kind with
	| Method m1, Method m2 when not (m1 = MethDynamic) && not (m2 = MethDynamic) ->
		begin match follow t1, follow t2 with
		| TFun (args1,r1) , TFun (args2,r2) -> (
			if not (List.length args1 = List.length args2) then raise (Unify_error [Unify_custom "Different number of function arguments"]);
			try
				List.iter2 (fun (n,o1,a1) (_,o2,a2) ->
					if o1 <> o2 then raise (Unify_error [Not_matching_optional n]);
					(try valid a2 a1 with Unify_error _ -> raise (Unify_error [Cannot_unify(a1,a2)]))
				) args1 args2;
				valid r1 r2
			with Unify_error l ->
				raise (Unify_error (Cannot_unify (t1,t2) :: l)))
		| _ ->
			assert false
		end
	| _,(Var { v_write = AccNo | AccNever }) ->
		(* write variance *)
		valid t1 t2
	| _,(Var { v_read = AccNo | AccNever }) ->
		(* read variance *)
		valid t2 t1
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

let check_overriding ctx c f =
	match c.cl_super with
	| None ->
		if List.memq f c.cl_overrides then display_error ctx ("Field " ^ f.cf_name ^ " is declared 'override' but doesn't override any field") f.cf_pos
	| _ when c.cl_extern && Meta.has Meta.CsNative c.cl_meta -> () (* -net-lib specific: do not check overrides on extern CsNative classes *)
	| Some (csup,params) ->
		let p = f.cf_pos in
		let i = f.cf_name in
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
				display_error ctx ("Field " ^ i ^ " should be declared with 'override' since it is inherited from superclass " ^ s_type_path csup.cl_path) p
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
			if has_meta Meta.Final f2.cf_meta then display_error ctx ("Cannot override final method " ^ i) p;
			try
				let t = apply_params csup.cl_params params t in
				valid_redefinition ctx f f.cf_type f2 t
			with
				Unify_error l ->
					display_error ctx ("Field " ^ i ^ " overloads parent class with different or incomplete type") p;
					display_error ctx ("Base field is defined here") f2.cf_pos;
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
			let overloads = Overloads.get_overloads csup i in
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
						Overloads.same_overload_args f.cf_type (apply_params csup.cl_params params t) f f2
					) overloads
				) true
			) (f :: f.cf_overloads)
		end else
			check_field f (fun csup i ->
				let _, t, f2 = raw_class_field (fun f -> f.cf_type) csup params i in
				t, f2) false

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
			let _, t , f = raw_class_field (fun f -> f.cf_type) c tl i in
			apply_params c.cl_params tl t , f

let rec return_flow ctx e =
	let error() =
		display_error ctx (Printf.sprintf "Missing return: %s" (s_type (print_context()) ctx.ret)) e.epos; raise Exit
	in
	let return_flow = return_flow ctx in
	let rec uncond e = match e.eexpr with
		| TIf _ | TWhile _ | TSwitch _ | TTry _ | TFunction _ -> ()
		| TReturn _ | TThrow _ -> raise Exit
		| _ -> Type.iter uncond e
	in
	let has_unconditional_flow e = try uncond e; false with Exit -> true in
	match e.eexpr with
	| TReturn _ | TThrow _ -> ()
	| TParenthesis e | TMeta(_,e) ->
		return_flow e
	| TBlock el ->
		let rec loop = function
			| [] -> error()
			| [e] -> return_flow e
			| e :: _ when has_unconditional_flow e -> ()
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

let type_function_arg_value ctx t c do_display =
	match c with
		| None -> None
		| Some e ->
			let p = pos e in
			let e = if do_display then Display.ExprPreprocessing.process_expr ctx.com e else e in
			let e = ctx.g.do_optimize ctx (type_expr ctx e (WithType t)) in
			unify ctx e.etype t p;
			let rec loop e = match e.eexpr with
				| TConst c -> Some c
				| TCast(e,None) -> loop e
				| _ ->
					if not ctx.com.display.dms_display || ctx.com.display.dms_inline && ctx.com.display.dms_error_policy = EPCollect then
						display_error ctx "Parameter default value should be constant" p;
					None
			in
			loop e

(**** strict meta ****)
let get_native_repr md pos =
	let path, meta = match md with
		| TClassDecl cl -> cl.cl_path, cl.cl_meta
		| TEnumDecl e -> e.e_path, e.e_meta
		| TTypeDecl t -> t.t_path, t.t_meta
		| TAbstractDecl a -> a.a_path, a.a_meta
	in
	let rec loop acc = function
		| (Meta.JavaCanonical,[EConst(String pack),_; EConst(String name),_],_) :: _ ->
			ExtString.String.nsplit pack ".", name
		| (Meta.Native,[EConst(String name),_],_) :: meta ->
			loop (Ast.parse_path name) meta
		| _ :: meta ->
			loop acc meta
		| [] ->
			acc
	in
	let pack, name = loop path meta in
	match pack with
		| [] ->
			(EConst(Ident(name)), pos)
		| hd :: tl ->
			let rec loop pack expr = match pack with
				| hd :: tl ->
					loop tl (EField(expr,hd),pos)
				| [] ->
					(EField(expr,name),pos)
			in
			loop tl (EConst(Ident(hd)),pos)

let rec process_meta_argument ?(toplevel=true) ctx expr = match expr.eexpr with
	| TField(e,f) ->
		(EField(process_meta_argument ~toplevel:false ctx e,field_name f),expr.epos)
	| TConst(TInt i) ->
		(EConst(Int (Int32.to_string i)), expr.epos)
	| TConst(TFloat f) ->
		(EConst(Float f), expr.epos)
	| TConst(TString s) ->
		(EConst(String s), expr.epos)
	| TConst TNull ->
		(EConst(Ident "null"), expr.epos)
	| TConst(TBool b) ->
		(EConst(Ident (string_of_bool b)), expr.epos)
	| TCast(e,_) | TMeta(_,e) | TParenthesis(e) ->
		process_meta_argument ~toplevel ctx e
	| TTypeExpr md when toplevel ->
		let p = expr.epos in
		if ctx.com.platform = Cs then
			(ECall( (EConst(Ident "typeof"), p), [get_native_repr md expr.epos] ), p)
		else
			(EField(get_native_repr md expr.epos, "class"), p)
	| TTypeExpr md ->
		get_native_repr md expr.epos
	| _ ->
		display_error ctx "This expression is too complex to be a strict metadata argument" expr.epos;
		(EConst(Ident "null"), expr.epos)

let make_meta ctx texpr extra =
	match texpr.eexpr with
		| TNew(c,_,el) ->
			ECall(get_native_repr (TClassDecl c) texpr.epos, (List.map (process_meta_argument ctx) el) @ extra), texpr.epos
		| TTypeExpr(md) ->
			ECall(get_native_repr md texpr.epos, extra), texpr.epos
		| _ ->
			display_error ctx "Unexpected expression" texpr.epos; assert false

let field_to_type_path ctx e =
	let rec loop e pack name = match e with
		| EField(e,f),p when Char.lowercase (String.get f 0) <> String.get f 0 -> (match name with
			| [] | _ :: [] ->
				loop e pack (f :: name)
			| _ -> (* too many name paths *)
				display_error ctx ("Unexpected " ^ f) p;
				raise Exit)
		| EField(e,f),_ ->
			loop e (f :: pack) name
		| EConst(Ident f),_ ->
			let pack, name, sub = match name with
				| [] ->
					let fchar = String.get f 0 in
					if Char.uppercase fchar = fchar then
						pack, f, None
					else begin
						display_error ctx "A class name must start with an uppercase character" (snd e);
						raise Exit
					end
				| [name] ->
					f :: pack, name, None
				| [name; sub] ->
					f :: pack, name, Some sub
				| _ ->
					assert false
			in
			{ tpackage=pack; tname=name; tparams=[]; tsub=sub }
		| _,pos ->
			display_error ctx "Unexpected expression when building strict meta" pos;
			raise Exit
	in
	loop e [] []

let handle_fields ctx fields_to_check with_type_expr =
	List.map (fun ((name,_,_),expr) ->
		let pos = snd expr in
		let field = (EField(with_type_expr,name), pos) in
		let fieldexpr = (EConst(Ident name),pos) in
		let left_side = match ctx.com.platform with
			| Cs -> field
			| Java -> (ECall(field,[]),pos)
			| _ -> assert false
		in

		let left = type_expr ctx left_side NoValue in
		let right = type_expr ctx expr (WithType left.etype) in
		unify ctx left.etype right.etype (snd expr);
		(EBinop(Ast.OpAssign,fieldexpr,process_meta_argument ctx right), pos)
	) fields_to_check

let get_strict_meta ctx params pos =
	let pf = ctx.com.platform in
	let changed_expr, fields_to_check, ctype = match params with
		| [ECall(ef, el),p] ->
			(* check last argument *)
			let el, fields = match List.rev el with
				| (EObjectDecl(decl),_) :: el ->
					List.rev el, decl
				| _ ->
					el, []
			in
			let tpath = field_to_type_path ctx ef in
			if pf = Cs then
				(ENew((tpath,snd ef), el), p), fields, CTPath tpath
			else
				ef, fields, CTPath tpath
		| [EConst(Ident i),p as expr] ->
			let tpath = { tpackage=[]; tname=i; tparams=[]; tsub=None } in
			if pf = Cs then
				(ENew((tpath,p), []), p), [], CTPath tpath
			else
				expr, [], CTPath tpath
		| [ (EField(_),p as field) ] ->
			let tpath = field_to_type_path ctx field in
			if pf = Cs then
				(ENew((tpath,p), []), p), [], CTPath tpath
			else
				field, [], CTPath tpath
		| _ ->
			display_error ctx "A @:strict metadata must contain exactly one parameter. Please check the documentation for more information" pos;
			raise Exit
	in
	let texpr = type_expr ctx changed_expr NoValue in
	let with_type_expr = (ECheckType( (EConst (Ident "null"), pos), (ctype,null_pos) ), pos) in
	let extra = handle_fields ctx fields_to_check with_type_expr in
	Meta.Meta, [make_meta ctx texpr extra], pos

let check_strict_meta ctx metas =
	let pf = ctx.com.platform in
	match pf with
		| Cs | Java ->
			let ret = ref [] in
			List.iter (function
				| Meta.Strict,params,pos -> (try
					ret := get_strict_meta ctx params pos :: !ret
				with | Exit -> ())
				| _ -> ()
			) metas;
			!ret
		| _ -> []

(**** end of strict meta handling *****)

let add_constructor ctx c force_constructor p =
	match c.cl_constructor, c.cl_super with
	| None, Some ({ cl_constructor = Some cfsup } as csup,cparams) when not c.cl_extern ->
		let cf = {
			cfsup with
			cf_pos = p;
			cf_meta = List.filter (fun (m,_,_) -> m = Meta.CompilerGenerated) cfsup.cf_meta;
			cf_doc = None;
			cf_expr = None;
		} in
		let r = exc_protect ctx (fun r ->
			let t = mk_mono() in
			r := lazy_processing (fun() -> t);
			let ctx = { ctx with
				curfield = cf;
				pass = PTypeField;
			} in
			ignore (follow cfsup.cf_type); (* make sure it's typed *)
			(if ctx.com.config.pf_overload then List.iter (fun cf -> ignore (follow cf.cf_type)) cf.cf_overloads);
			let map_arg (v,def) =
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
			in
			let args = (match cfsup.cf_expr with
				| Some { eexpr = TFunction f } ->
					List.map map_arg f.tf_args
				| _ ->
					let values = get_value_meta cfsup.cf_meta in
					match follow cfsup.cf_type with
					| TFun (args,_) ->
						List.map (fun (n,o,t) ->
							let def = try type_function_arg_value ctx t (Some (PMap.find n values)) false with Not_found -> if o then Some TNull else None in
							map_arg (alloc_var n (if o then ctx.t.tnull t else t) p,def) (* TODO: var pos *)
						) args
					| _ -> assert false
			) in
			let p = c.cl_pos in
			let vars = List.map (fun (v,def) -> alloc_var v.v_name (apply_params csup.cl_params cparams v.v_type) v.v_pos, def) args in
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
	| None,_ when force_constructor ->
		let constr = mk (TFunction {
			tf_args = [];
			tf_type = ctx.t.tvoid;
			tf_expr = mk (TBlock []) ctx.t.tvoid p;
		}) (tfun [] ctx.t.tvoid) p in
		let cf = mk_field "new" constr.etype p null_pos in
		cf.cf_expr <- Some constr;
		cf.cf_type <- constr.etype;
		cf.cf_meta <- [Meta.CompilerGenerated,[],null_pos];
		cf.cf_kind <- Method MethNormal;
		c.cl_constructor <- Some cf;
	| _ ->
		(* nothing to do *)
		()

let check_struct_init_constructor ctx c p = match c.cl_constructor with
	| Some _ ->
		()
	| None ->
		let params = List.map snd c.cl_params in
		let ethis = mk (TConst TThis) (TInst(c,params)) p in
		let args,el,tl = List.fold_left (fun (args,el,tl) cf -> match cf.cf_kind with
			| Var _ ->
				let opt = Meta.has Meta.Optional cf.cf_meta in
				let t = if opt then ctx.t.tnull cf.cf_type else cf.cf_type in
				let v = alloc_var cf.cf_name t p in
				let ef = mk (TField(ethis,FInstance(c,params,cf))) t p in
				let ev = mk (TLocal v) v.v_type p in
				let e = mk (TBinop(OpAssign,ef,ev)) ev.etype p in
				(v,None) :: args,e :: el,(cf.cf_name,opt,t) :: tl
			| Method _ ->
				args,el,tl
		) ([],[],[]) (List.rev c.cl_ordered_fields) in
		let tf = {
			tf_args = args;
			tf_type = ctx.t.tvoid;
			tf_expr = mk (TBlock el) ctx.t.tvoid p
		} in
		let e = mk (TFunction tf) (TFun(tl,ctx.t.tvoid)) p in
		let cf = mk_field "new" e.etype p null_pos in
		cf.cf_expr <- Some e;
		cf.cf_type <- e.etype;
		cf.cf_meta <- [Meta.CompilerGenerated,[],null_pos];
		cf.cf_kind <- Method MethNormal;
		c.cl_constructor <- Some cf

module Inheritance = struct
	let check_extends ctx c t p = match follow t with
		| TInst ({ cl_path = [],"Array"; cl_extern = basic_extern },_)
		| TInst ({ cl_path = [],"String"; cl_extern = basic_extern },_)
		| TInst ({ cl_path = [],"Date"; cl_extern = basic_extern },_)
		| TInst ({ cl_path = [],"Xml"; cl_extern = basic_extern },_) when not (c.cl_extern && basic_extern) ->
			error "Cannot extend basic class" p;
		| TInst (csup,params) ->
			if is_parent c csup then error "Recursive class" p;
			begin match csup.cl_kind with
				| KTypeParameter _ ->
					if is_generic_parameter ctx csup then error "Extending generic type parameters is no longer allowed in Haxe 4" p;
					error "Cannot extend type parameters" p
				| _ -> csup,params
			end
		| _ -> error "Should extend by using a class" p

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
						let overloads = Overloads.get_overloads c i in
						is_overload := true;
						let t = (apply_params intf.cl_params params f.cf_type) in
						List.find (fun (t1,f1) -> Overloads.same_overload_args t t1 f f1) overloads
					else
						t2, f2
				in
				if ctx.com.display.dms_collect_data then begin
						let h = ctx.com.display_information in
						h.interface_field_implementations <- (intf,f,c,Some f2) :: h.interface_field_implementations;
				end;
				ignore(follow f2.cf_type); (* force evaluation *)
				let p = (match f2.cf_expr with None -> p | Some e -> e.epos) in
				let mkind = function
					| MethNormal | MethInline -> 0
					| MethDynamic -> 1
					| MethMacro -> 2
				in
				if f.cf_public && not f2.cf_public && not (Meta.has Meta.CompilerGenerated f.cf_meta) then
					display_error ctx ("Field " ^ i ^ " should be public as requested by " ^ s_type_path intf.cl_path) p
				else if not (unify_kind f2.cf_kind f.cf_kind) || not (match f.cf_kind, f2.cf_kind with Var _ , Var _ -> true | Method m1, Method m2 -> mkind m1 = mkind m2 | _ -> false) then
					display_error ctx ("Field " ^ i ^ " has different property access than in " ^ s_type_path intf.cl_path ^ " (" ^ s_kind f2.cf_kind ^ " should be " ^ s_kind f.cf_kind ^ ")") p
				else try
					valid_redefinition ctx f2 t2 f (apply_params intf.cl_params params f.cf_type)
				with
					Unify_error l ->
						if not (Meta.has Meta.CsNative c.cl_meta && c.cl_extern) then begin
							display_error ctx ("Field " ^ i ^ " has different type than in " ^ s_type_path intf.cl_path) p;
							display_error ctx ("Interface field is defined here") f.cf_pos;
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
			check_interface ctx c i2 (List.map (apply_params intf.cl_params params) p2)
		) intf.cl_implements

	let check_interfaces ctx c =
		match c.cl_path with
		| "Proxy" :: _ , _ -> ()
		| _ when c.cl_extern && Meta.has Meta.CsNative c.cl_meta -> ()
		| _ ->
		List.iter (fun (intf,params) -> check_interface ctx c intf params) c.cl_implements

	let set_heritance ctx c herits p =
		let is_lib = Meta.has Meta.LibType c.cl_meta in
		let ctx = { ctx with curclass = c; type_params = c.cl_params; } in
		let old_meta = c.cl_meta in
		let process_meta csup =
			List.iter (fun m ->
				match m with
				| Meta.Final, _, _ -> if not (Meta.has Meta.Hack c.cl_meta || (match c.cl_kind with KTypeParameter _ -> true | _ -> false)) then error "Cannot extend a final class" p;
				| Meta.AutoBuild, el, p -> c.cl_meta <- (Meta.Build,el,{ c.cl_pos with pmax = c.cl_pos.pmin }(* prevent display metadata *)) :: m :: c.cl_meta
				| _ -> ()
			) csup.cl_meta
		in
		let check_cancel_build csup =
			match csup.cl_build() with
			| Built -> ()
			| state ->
				(* for macros reason, our super class is not yet built - see #2177 *)
				(* let's reset our build and delay it until we are done *)
				c.cl_meta <- old_meta;
				raise (Build_canceled state)
		in
		let has_interf = ref false in
		(*
			resolve imports before calling build_inheritance, since it requires full paths.
			that means that typedefs are not working, but that's a fair limitation
		*)
		let resolve_imports (t,p) =
			match t.tpackage with
			| _ :: _ -> t,p
			| [] ->
				try
					let path_matches lt = snd (t_path lt) = t.tname in
					let lt = try
						List.find path_matches ctx.m.curmod.m_types
					with Not_found ->
						let t,pi = List.find (fun (lt,_) -> path_matches lt) ctx.m.module_types in
						Display.ImportHandling.mark_import_position ctx.com pi;
						t
					in
					{ t with tpackage = fst (t_path lt) },p
				with
					Not_found -> t,p
		in
		let herits = ExtList.List.filter_map (function
			| HExtends t -> Some(true,resolve_imports t)
			| HImplements t -> Some(false,resolve_imports t)
			| t -> None
		) herits in
		let herits = List.filter (ctx.g.do_inherit ctx c p) herits in
		(* Pass 1: Check and set relations *)
		let check_herit t is_extends =
			if is_extends then begin
				if c.cl_super <> None then error "Cannot extend several classes" p;
				let csup,params = check_extends ctx c t p in
				if c.cl_interface then begin
					if not csup.cl_interface then error "Cannot extend by using a class" p;
					c.cl_implements <- (csup,params) :: c.cl_implements;
					if not !has_interf then begin
						if not is_lib then delay ctx PForce (fun() -> check_interfaces ctx c);
						has_interf := true;
					end
				end else begin
					if csup.cl_interface then error "Cannot extend by using an interface" p;
					c.cl_super <- Some (csup,params)
				end;
				(fun () ->
					check_cancel_build csup;
					process_meta csup;
				)
			end else begin match follow t with
				| TInst ({ cl_path = [],"ArrayAccess"; cl_extern = true; },[t]) ->
					if c.cl_array_access <> None then error "Duplicate array access" p;
					c.cl_array_access <- Some t;
					(fun () -> ())
				| TInst (intf,params) ->
					if is_parent c intf then error "Recursive class" p;
					if c.cl_interface then error "Interfaces cannot implement another interface (use extends instead)" p;
					if not intf.cl_interface then error "You can only implement an interface" p;
					c.cl_implements <- (intf, params) :: c.cl_implements;
					if not !has_interf && not is_lib && not (Meta.has (Meta.Custom "$do_not_check_interf") c.cl_meta) then begin
						delay ctx PForce (fun() -> check_interfaces ctx c);
						has_interf := true;
					end;
					(fun () ->
						check_cancel_build intf;
						process_meta intf;
					)
				| TDynamic t ->
					if c.cl_dynamic <> None then error "Cannot have several dynamics" p;
					c.cl_dynamic <- Some t;
					(fun () -> ())
				| _ ->
					error "Should implement by using an interface" p
			end
		in
		let fl = ExtList.List.filter_map (fun (is_extends,t) ->
			try
				let t = load_instance ~allow_display:true ctx t false p in
				Some (check_herit t is_extends)
			with Error(Module_not_found(([],name)),p) when ctx.com.display.dms_display ->
				if Display.Diagnostics.is_diagnostics_run ctx then DisplayToplevel.handle_unresolved_identifier ctx name p true;
				None
		) herits in
		fl
end

let rec type_type_param ?(enum_constructor=false) ctx path get_params p tp =
	let n = fst tp.tp_name in
	let c = mk_class ctx.m.curmod (fst path @ [snd path],n) (pos tp.tp_name) (pos tp.tp_name) in
	c.cl_params <- type_type_params ctx c.cl_path get_params p tp.tp_params;
	c.cl_kind <- KTypeParameter [];
	c.cl_meta <- tp.Ast.tp_meta;
	if enum_constructor then c.cl_meta <- (Meta.EnumConstructorParam,[],null_pos) :: c.cl_meta;
	let t = TInst (c,List.map snd c.cl_params) in
	if ctx.is_display_file && Display.is_display_position (pos tp.tp_name) then
		Display.DisplayEmitter.display_type ctx.com.display t (pos tp.tp_name);
	match tp.tp_constraints with
	| [] ->
		n, t
	| _ ->
		let r = exc_protect ctx (fun r ->
			r := lazy_processing (fun() -> t);
			let ctx = { ctx with type_params = ctx.type_params @ get_params() } in
			let constr = List.map (load_complex_type ctx true p) tp.tp_constraints in
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
		n, TLazy r

and type_type_params ?(enum_constructor=false) ctx path get_params p tpl =
	let names = ref [] in
	List.map (fun tp ->
		if List.exists (fun name -> name = fst tp.tp_name) !names then display_error ctx ("Duplicate type parameter name: " ^ fst tp.tp_name) (pos tp.tp_name);
		names := (fst tp.tp_name) :: !names;
		type_type_param ~enum_constructor ctx path get_params p tp
	) tpl

let type_function_params ctx fd fname p =
	let params = ref [] in
	params := type_type_params ctx ([],fname) (fun() -> !params) p fd.f_params;
	!params

let save_function_state ctx =
	let old_ret = ctx.ret in
	let old_fun = ctx.curfun in
	let old_opened = ctx.opened in
	let locals = ctx.locals in
	(fun () ->
		ctx.locals <- locals;
		ctx.ret <- old_ret;
		ctx.curfun <- old_fun;
		ctx.opened <- old_opened;
	)

let type_function ctx args ret fmode f do_display p =
	let fargs = List.map2 (fun (n,c,t) ((_,pn),_,m,_,_) ->
		if n.[0] = '$' then error "Function argument names starting with a dollar are not allowed" p;
		let c = type_function_arg_value ctx t c do_display in
		let v,c = add_local ctx n t pn, c in
		v.v_meta <- m;
		if do_display && Display.is_display_position pn then
			Display.DisplayEmitter.display_variable ctx.com.display v pn;
		if n = "this" then v.v_meta <- (Meta.This,[],null_pos) :: v.v_meta;
		v,c
	) args f.f_args in
	ctx.curfun <- fmode;
	ctx.ret <- ret;
	ctx.opened <- [];
	let e = match f.f_expr with
		| None ->
			if ctx.com.display.dms_error_policy = EPIgnore then
				(* when we don't care because we're in display mode, just act like
				   the function has an empty block body. this is fine even if function
				   defines a return type, because returns aren't checked in this mode
				*)
				EBlock [],p
			else
				error "Function body required" p
		| Some e -> e
	in
	let e = if not do_display then
		type_expr ctx e NoValue
	else begin
		let e = Display.ExprPreprocessing.process_expr ctx.com e in
		try
			if Common.defined ctx.com Define.NoCOpt then raise Exit;
			type_expr ctx (Optimizer.optimize_completion_expr e) NoValue
		with
		| Parser.TypePath (_,None,_) | Exit ->
			type_expr ctx e NoValue
		| Display.DisplayType (t,_,_) when (match follow t with TMono _ -> true | _ -> false) ->
			type_expr ctx (if ctx.com.display.dms_kind = DMToplevel then Display.ExprPreprocessing.find_enclosing ctx.com e else e) NoValue
	end in
	let e = match e.eexpr with
		| TMeta((Meta.MergeBlock,_,_), ({eexpr = TBlock el} as e1)) -> e1
		| _ -> e
	in
	let has_return e =
		let rec loop e =
			match e.eexpr with
			| TReturn (Some _) -> raise Exit
			| TFunction _ -> ()
			| _ -> Type.iter loop e
		in
		try loop e; false with Exit -> true
	in
	begin match follow ret with
		| TAbstract({a_path=[],"Void"},_) -> ()
		(* We have to check for the presence of return expressions here because
		   in the case of Dynamic ctx.ret is still a monomorph. If we indeed
		   don't have a return expression we can link the monomorph to Void. We
		   can _not_ use type_iseq to avoid the Void check above because that
		   would turn Dynamic returns to Void returns. *)
		| TMono t when not (has_return e) -> ignore(link t ret ctx.t.tvoid)
		| _ when ctx.com.display.dms_error_policy = EPIgnore -> ()
		| _ -> (try return_flow ctx e with Exit -> ())
	end;
	let rec loop e =
		match e.eexpr with
		| TCall ({ eexpr = TConst TSuper },_) -> raise Exit
		| TFunction _ -> ()
		| _ -> Type.iter loop e
	in
	let has_super_constr() =
		match ctx.curclass.cl_super with
		| None ->
			None
		| Some (csup,tl) ->
			try
				let _,cf = get_constructor (fun f->f.cf_type) csup in
				Some (Meta.has Meta.CompilerGenerated cf.cf_meta,TInst(csup,tl))
			with Not_found ->
				None
	in
	let e = if fmode <> FunConstructor then
		e
	else begin
		let final_vars = Hashtbl.create 0 in
		List.iter (fun cf -> match cf.cf_kind with
			| Var _ when Meta.has Meta.Final cf.cf_meta && cf.cf_expr = None ->
				Hashtbl.add final_vars cf.cf_name cf
			| _ ->
				()
		) ctx.curclass.cl_ordered_fields;
		if Hashtbl.length final_vars > 0 then begin
			let rec find_inits e = match e.eexpr with
				| TBinop(OpAssign,{eexpr = TField({eexpr = TConst TThis},fa)},e2) ->
					Hashtbl.remove final_vars (field_name fa);
					find_inits e2;
				| _ ->
					Type.iter find_inits e
			in
			find_inits e;
			Hashtbl.iter (fun _ cf ->
				display_error ctx ("final field " ^ cf.cf_name ^ " must be initialized immediately or in the constructor") cf.cf_pos;
			) final_vars
		end;
		match has_super_constr() with
		| Some (was_forced,t_super) ->
			(try
				loop e;
				if was_forced then
					let e_super = mk (TConst TSuper) t_super e.epos in
					let e_super_call = mk (TCall(e_super,[])) ctx.t.tvoid e.epos in
					concat e_super_call e
				else begin
					display_error ctx "Missing super constructor call" p;
					e
				end
			with
				Exit -> e);
		| None ->
			e
	end in
	let e = match ctx.curfun, ctx.vthis with
		| (FunMember|FunConstructor), Some v ->
			let ev = mk (TVar (v,Some (mk (TConst TThis) ctx.tthis p))) ctx.t.tvoid p in
			(match e.eexpr with
			| TBlock l -> { e with eexpr = TBlock (ev::l) }
			| _ -> mk (TBlock [ev;e]) e.etype p)
		| _ -> e
	in
	List.iter (fun r -> r := Closed) ctx.opened;
	e , fargs

let type_function ctx args ret fmode f do_display p =
	let save = save_function_state ctx in
	Std.finally save (type_function ctx args ret fmode f do_display) p

let load_core_class ctx c =
	let ctx2 = (match ctx.g.core_api with
		| None ->
			let com2 = Common.clone ctx.com in
			com2.defines.Define.values <- PMap.empty;
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
	let t = load_instance ctx2 (tpath,c.cl_pos) true c.cl_pos in
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
		) ccore.cl_params c.cl_params;
	with Invalid_argument _ ->
		error "Class must have the same number of type parameters as core type" c.cl_pos
	end;
	(match c.cl_doc with
	| None -> c.cl_doc <- ccore.cl_doc
	| Some _ -> ());
	let compare_fields f f2 =
		let p = (match f2.cf_expr with None -> c.cl_pos | Some e -> e.epos) in
		(try
			type_eq EqCoreType (apply_params ccore.cl_params (List.map snd c.cl_params) f.cf_type) f2.cf_type
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

let check_global_metadata ctx meta f_add mpath tpath so =
	let sl1 = full_dot_path mpath tpath in
	let sl1,field_mode = match so with None -> sl1,false | Some s -> sl1 @ [s],true in
	List.iter (fun (sl2,m,(recursive,to_types,to_fields)) ->
		let add = ((field_mode && to_fields) || (not field_mode && to_types)) && (match_path recursive sl1 sl2) in
		if add then f_add m
	) ctx.g.global_metadata;
	if ctx.is_display_file then delay ctx PCheckConstraint (fun () -> Display.DisplayEmitter.check_display_metadata ctx meta)

let patch_class ctx c fields =
	let path = match c.cl_kind with
		| KAbstractImpl a -> a.a_path
		| _ -> c.cl_path
	in
	let h = (try Some (Hashtbl.find ctx.g.type_patches path) with Not_found -> None) in
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
					let param (((n,pn),opt,m,_,e) as p) =
						try
							let t2 = (try Hashtbl.find h (("$" ^ (fst f.cff_name) ^ "__" ^ n),false) with Not_found -> Hashtbl.find h (("$" ^ n),false)) in
							(n,pn), opt, m, (match t2.tp_type with None -> None | Some t -> Some (t,null_pos)), e
						with Not_found ->
							p
					in
					f.cff_kind <- FFun { ff with f_args = List.map param ff.f_args }
				| _ -> ());
				(* other patches *)
				match (try Some (Hashtbl.find h (fst f.cff_name,List.mem AStatic f.cff_access)) with Not_found -> None) with
				| None -> loop (f :: acc) l
				| Some { tp_remove = true } -> loop acc l
				| Some p ->
					f.cff_meta <- f.cff_meta @ p.tp_meta;
					(match p.tp_type with
					| None -> ()
					| Some t ->
						f.cff_kind <- match f.cff_kind with
						| FVar (_,e) -> FVar (Some (t,null_pos),e)
						| FProp (get,set,_,eo) -> FProp (get,set,Some (t,null_pos),eo)
						| FFun f -> FFun { f with f_type = Some (t,null_pos) });
					loop (f :: acc) l
		in
		List.rev (loop [] fields)

let string_list_of_expr_path (e,p) =
	try string_list_of_expr_path_raise (e,p)
	with Exit -> error "Invalid path" p

let build_enum_abstract ctx c a fields p =
	List.iter (fun field ->
		match field.cff_kind with
		| FVar(ct,eo) when not (List.mem AStatic field.cff_access) ->
			field.cff_access <- [AStatic; if (List.mem APrivate field.cff_access) then APrivate else APublic];
			field.cff_meta <- (Meta.Enum,[],null_pos) :: (Meta.Impl,[],null_pos) :: field.cff_meta;
			let ct = match ct with
				| Some _ -> ct
				| None -> Some (TExprToExpr.convert_type (TAbstract(a,List.map snd a.a_params)),null_pos)
			in
			begin match eo with
				| None ->
					if not c.cl_extern then error "Value required" field.cff_pos
					else field.cff_kind <- FProp(("default",null_pos),("never",null_pos),ct,None)
				| Some e ->
					field.cff_access <- AInline :: field.cff_access;
					let e = (ECast(e,None),(pos e)) in
					field.cff_kind <- FVar(ct,Some e)
			end
		| _ ->
			()
	) fields;
	EVars [("",null_pos),Some (CTAnonymous fields,p),None],p

let is_java_native_function meta = try
	match Meta.get Meta.Native meta with
		| (Meta.Native,[],_) -> true
		| _ -> false
	with | Not_found -> false

let build_module_def ctx mt meta fvars context_init fbuild =
	let loop (f_build,f_enum) = function
		| Meta.Build,args,p -> (fun () ->
				let epath, el = (match args with
					| [ECall (epath,el),p] -> epath, el
					| _ -> error "Invalid build parameters" p
				) in
				let s = try String.concat "." (List.rev (string_list_of_expr_path epath)) with Error (_,p) -> error "Build call parameter must be a class path" p in
				if ctx.in_macro then error "You cannot use @:build inside a macro : make sure that your type is not used in macro" p;
				let old = ctx.g.get_build_infos in
				ctx.g.get_build_infos <- (fun() -> Some (mt, List.map snd (t_infos mt).mt_params, fvars()));
				context_init();
				let r = try apply_macro ctx MBuild s el p with e -> ctx.g.get_build_infos <- old; raise e in
				ctx.g.get_build_infos <- old;
				(match r with
				| None -> error "Build failure" p
				| Some e -> fbuild e)
			) :: f_build,f_enum
		| Meta.Enum,_,p -> f_build,Some (fun () ->
				begin match mt with
					| TClassDecl ({cl_kind = KAbstractImpl a} as c) ->
						context_init();
						let e = build_enum_abstract ctx c a (fvars()) p in
						fbuild e;
					| _ ->
						()
				end
			)
		| _ ->
			f_build,f_enum
	in
	(* let errors go through to prevent resume if build fails *)
	let f_build,f_enum = List.fold_left loop ([],None) meta in
	List.iter (fun f -> f()) (List.rev f_build);
	(match f_enum with None -> () | Some f -> f())

module ClassInitializer = struct
	type class_init_ctx = {
		tclass : tclass; (* I don't trust ctx.curclass because it's mutable. *)
		is_lib : bool;
		is_native : bool;
		is_core_api : bool;
		is_class_debug : bool;
		extends_public : bool;
		abstract : tabstract option;
		context_init : unit -> unit;
		mutable delayed_expr : (typer * tlazy ref option) list;
		mutable force_constructor : bool;
		mutable uninitialized_final : pos option;
	}

	type field_kind =
		| FKNormal
		| FKConstructor
		| FKInit

	type field_init_ctx = {
		is_inline : bool;
		is_final : bool;
		is_static : bool;
		is_override : bool;
		is_extern : bool;
		is_macro : bool;
		is_abstract_member : bool;
		is_display_field : bool;
		is_field_debug : bool;
		field_kind : field_kind;
		mutable do_bind : bool;
		mutable do_add : bool;
	}

	let dump_class_context cctx =
		Printer.s_record_fields "" [
			"tclass",Printer.s_tclass "\t" cctx.tclass;
			"is_lib",string_of_bool cctx.is_lib;
			"is_native",string_of_bool cctx.is_native;
			"is_core_api",string_of_bool cctx.is_core_api;
			"is_class_debug",string_of_bool cctx.is_class_debug;
			"extends_public",string_of_bool cctx.extends_public;
			"abstract",Printer.s_opt (Printer.s_tabstract "\t") cctx.abstract;
			"force_constructor",string_of_bool cctx.force_constructor;
		]

	let s_field_kind = function
		| FKNormal -> "FKNormal"
		| FKConstructor -> "FKConstructor"
		| FKInit -> "FKInit"

	let dump_field_context fctx =
		Printer.s_record_fields "" [
			"is_inline",string_of_bool fctx.is_inline;
			"is_static",string_of_bool fctx.is_static;
			"is_override",string_of_bool fctx.is_override;
			"is_extern",string_of_bool fctx.is_extern;
			"is_macro",string_of_bool fctx.is_macro;
			"is_abstract_member",string_of_bool fctx.is_abstract_member;
			"is_display_field",string_of_bool fctx.is_display_field;
			"is_field_debug",string_of_bool fctx.is_field_debug;
			"field_kind",s_field_kind fctx.field_kind;
			"do_bind",string_of_bool fctx.do_bind;
			"do_add",string_of_bool fctx.do_add;
		]

	let create_class_context ctx c context_init p =
		locate_macro_error := true;
		incr stats.s_classes_built;
		let abstract = match c.cl_kind with
			| KAbstractImpl a -> Some a
			| _ -> None
		in
		let ctx = {
			ctx with
			curclass = c;
			type_params = c.cl_params;
			pass = PBuildClass;
			tthis = (match abstract with
				| Some a ->
					(match a.a_this with
					| TMono r when !r = None -> TAbstract (a,List.map snd c.cl_params)
					| t -> t)
				| None -> TInst (c,List.map snd c.cl_params));
			on_error = (fun ctx msg ep ->
				ctx.com.error msg ep;
				(* macros expressions might reference other code, let's recall which class we are actually compiling *)
				if !locate_macro_error && (ep.pfile <> c.cl_pos.pfile || ep.pmax < c.cl_pos.pmin || ep.pmin > c.cl_pos.pmax) then ctx.com.error "Defined in this class" c.cl_pos
			);
		} in
		(* a lib type will skip most checks *)
		let is_lib = Meta.has Meta.LibType c.cl_meta in
		if is_lib && not c.cl_extern then ctx.com.error "@:libType can only be used in extern classes" c.cl_pos;
		(* a native type will skip one check: the static vs non-static field *)
		let is_native = Meta.has Meta.JavaNative c.cl_meta || Meta.has Meta.CsNative c.cl_meta in
		if Meta.has Meta.Macro c.cl_meta then display_error ctx "Macro classes are no longer allowed in haxe 3" c.cl_pos;
		let rec extends_public c =
			Meta.has Meta.PublicFields c.cl_meta ||
			match c.cl_super with
			| None -> false
			| Some (c,_) -> extends_public c
		in
		let cctx = {
			tclass = c;
			is_lib = is_lib;
			is_native = is_native;
			is_core_api = Meta.has Meta.CoreApi c.cl_meta;
			is_class_debug = false;
			extends_public = extends_public c;
			abstract = abstract;
			context_init = context_init;
			force_constructor = false;
			uninitialized_final = None;
			delayed_expr = [];
		} in
		ctx,cctx

	let create_field_context (ctx,cctx) c cff =
		let ctx = {
			ctx with
			pass = PBuildClass; (* will be set later to PTypeExpr *)
		} in
		let is_static = List.mem AStatic cff.cff_access in
		let is_extern = Meta.has Meta.Extern cff.cff_meta || c.cl_extern in
		let allow_inline = cctx.abstract <> None || match cff.cff_kind with
			| FFun _ -> ctx.g.doinline || is_extern
			| _ -> true
		in
		let is_inline = allow_inline && List.mem AInline cff.cff_access in
		let is_override = List.mem AOverride cff.cff_access in
		let is_macro = List.mem AMacro cff.cff_access in
		let field_kind = match fst cff.cff_name with
			| "new" -> FKConstructor
			| "__init__" when is_static -> FKInit
			| _ -> FKNormal
		in
		let fctx = {
			is_inline = is_inline;
			is_static = is_static;
			is_override = is_override;
			is_macro = is_macro;
			is_extern = is_extern;
			is_final = List.mem AFinal cff.cff_access;
			is_display_field = ctx.is_display_file && Display.is_display_position cff.cff_pos;
			is_field_debug = cctx.is_class_debug;
			is_abstract_member = cctx.abstract <> None && Meta.has Meta.Impl cff.cff_meta;
			field_kind = field_kind;
			do_bind = (((not c.cl_extern || is_inline) && not c.cl_interface) || field_kind = FKInit);
			do_add = true;
		} in
		ctx,fctx

	let is_public (ctx,cctx) access parent =
		let c = cctx.tclass in
		if List.mem APrivate access then
			false
		else if List.mem APublic access then
			true
		else match parent with
			| Some { cf_public = p } -> p
			| _ -> c.cl_extern || c.cl_interface || cctx.extends_public

	let rec get_parent c name =
		match c.cl_super with
		| None -> None
		| Some (csup,_) ->
			try
				Some (PMap.find name csup.cl_fields)
			with
				Not_found -> get_parent csup name

	let add_field c cf is_static =
		if is_static then begin
			c.cl_statics <- PMap.add cf.cf_name cf c.cl_statics;
			c.cl_ordered_statics <- cf :: c.cl_ordered_statics;
		end else begin
			c.cl_fields <- PMap.add cf.cf_name cf c.cl_fields;
			c.cl_ordered_fields <- cf :: c.cl_ordered_fields;
		end

	let type_opt (ctx,cctx) p t =
		let c = cctx.tclass in
		match t with
		| None when c.cl_extern || c.cl_interface ->
			display_error ctx "Type required for extern classes and interfaces" p;
			t_dynamic
		| None when cctx.is_core_api ->
			display_error ctx "Type required for core api classes" p;
			t_dynamic
		| _ ->
			load_type_hint ctx p t

	let build_fields (ctx,cctx) c fields =
		let fields = ref fields in
		let get_fields() = !fields in
		let pending = ref [] in
		c.cl_build <- (fun() -> BuildMacro pending);
		build_module_def ctx (TClassDecl c) c.cl_meta get_fields cctx.context_init (fun (e,p) ->
			match e with
			| EVars [_,Some (CTAnonymous f,p),None] ->
				let f = List.map (fun f ->
					let f = match cctx.abstract with
						| Some a ->
							let a_t = TExprToExpr.convert_type' (TAbstract(a,List.map snd a.a_params)) in
							let this_t = TExprToExpr.convert_type' a.a_this in (* TODO: better pos? *)
							transform_abstract_field ctx.com this_t a_t a f
						| None ->
							f
					in
					if List.mem AMacro f.cff_access then
						(match ctx.g.macros with
						| Some (_,mctx) when Hashtbl.mem mctx.g.types_module c.cl_path ->
							(* assume that if we had already a macro with the same name, it has not been changed during the @:build operation *)
							if not (List.exists (fun f2 -> f2.cff_name = f.cff_name && List.mem AMacro f2.cff_access) (!fields)) then
								error "Class build macro cannot return a macro function when the class has already been compiled into the macro context" p
						| _ -> ());
					f
				) f in
				fields := f
			| _ -> error "Class build macro must return a single variable with anonymous fields" p
		);
		c.cl_build <- (fun() -> Building [c]);
		List.iter (fun f -> f()) !pending;
		!fields

	let bind_type (ctx,cctx,fctx) cf r p =
		let c = cctx.tclass in
		let rec is_full_type t =
			match t with
			| TFun (args,ret) -> is_full_type ret && List.for_all (fun (_,_,t) -> is_full_type t) args
			| TMono r -> (match !r with None -> false | Some t -> is_full_type t)
			| TAbstract _ | TInst _ | TEnum _ | TLazy _ | TDynamic _ | TAnon _ | TType _ -> true
		in
		let force_macro () =
			(* force macro system loading of this class in order to get completion *)
			delay ctx PTypeField (fun() -> try ignore(ctx.g.do_macro ctx MDisplay c.cl_path cf.cf_name [] p) with Exit | Error _ -> ())
		in
		let handle_display_field () =
			if fctx.is_macro && not ctx.in_macro then
				force_macro()
			else begin
				cf.cf_type <- TLazy r;
				cctx.delayed_expr <- (ctx,Some r) :: cctx.delayed_expr;
			end
		in
		if ctx.com.display.dms_full_typing then begin
			if fctx.is_macro && not ctx.in_macro then
				()
			else begin
				cf.cf_type <- TLazy r;
				(* is_lib ? *)
				cctx.delayed_expr <- (ctx,Some r) :: cctx.delayed_expr;
			end
		end else if ctx.com.display.dms_force_macro_typing && fctx.is_macro && not ctx.in_macro then
			force_macro()
		else begin
			if fctx.is_display_field then begin
				handle_display_field()
			end else begin
				if not (is_full_type cf.cf_type) then begin
					cctx.delayed_expr <- (ctx, None) :: cctx.delayed_expr;
					cf.cf_type <- TLazy r;
				end;
			end
		end

	let bind_var (ctx,cctx,fctx) cf e =
		let c = cctx.tclass in
		let p = cf.cf_pos in
		let rec get_declared f = function
			| None -> None
			| Some (c,a) when PMap.exists f c.cl_fields ->
				Some (c,a)
			| Some (c,_) ->
				let ret = get_declared f c.cl_super in
				match ret with
					| Some r -> Some r
					| None ->
						let rec loop ifaces = match ifaces with
							| [] -> None
							| i :: ifaces -> match get_declared f (Some i) with
								| Some r -> Some r
								| None -> loop ifaces
						in
						loop c.cl_implements
		in
		if not fctx.is_static && not cctx.is_lib then begin match get_declared cf.cf_name c.cl_super with
				| None -> ()
				| Some (csup,_) ->
					(* this can happen on -net-lib generated classes if a combination of explicit interfaces and variables with the same name happens *)
					if not (csup.cl_interface && Meta.has Meta.CsNative c.cl_meta) then
						error ("Redefinition of variable " ^ cf.cf_name ^ " in subclass is not allowed. Previously declared at " ^ (s_type_path csup.cl_path) ) p
		end;
		let t = cf.cf_type in

		match e with
		| None ->
			if fctx.is_display_field then Display.DisplayEmitter.maybe_display_field ctx (cf.cf_name_pos) cf;
		| Some e ->
			if requires_value_meta ctx.com (Some c) then cf.cf_meta <- ((Meta.Value,[e],null_pos) :: cf.cf_meta);
			let check_cast e =
				(* insert cast to keep explicit field type (issue #1901) *)
				if type_iseq e.etype cf.cf_type then
					e
				else begin match e.eexpr,follow cf.cf_type with
					| TConst (TInt i),TAbstract({a_path=[],"Float"},_) ->
						(* turn int constant to float constant if expected type is float *)
						{e with eexpr = TConst (TFloat (Int32.to_string i))}
					| _ ->
						mk_cast e cf.cf_type e.epos
				end
			in
			let r = exc_protect ~force:false ctx (fun r ->
				(* type constant init fields (issue #1956) *)
				if not !return_partial_type || (match fst e with EConst _ -> true | _ -> false) then begin
					r := lazy_processing (fun() -> t);
					cctx.context_init();
					if ctx.com.verbose then Common.log ctx.com ("Typing " ^ (if ctx.in_macro then "macro " else "") ^ s_type_path c.cl_path ^ "." ^ cf.cf_name);
					let e = type_var_field ctx t e fctx.is_static fctx.is_display_field p in
					let maybe_run_analyzer e = match e.eexpr with
						| TConst _ | TLocal _ | TFunction _ -> e
						| _ -> !analyzer_run_on_expr_ref ctx.com e
					in
					let require_constant_expression e msg =
						if ctx.com.display.dms_display && ctx.com.display.dms_error_policy <> EPCollect then
							e
						else match Optimizer.make_constant_expression ctx (maybe_run_analyzer e) with
						| Some e -> e
						| None -> display_error ctx msg p; e
					in
					let e = (match cf.cf_kind with
					| Var v when c.cl_extern || Meta.has Meta.Extern cf.cf_meta ->
						if not fctx.is_static then begin
							display_error ctx "Extern non-static variables may not be initialized" p;
							e
						end else if not fctx.is_inline then begin
							display_error ctx "Extern non-inline variables may not be initialized" p;
							e
						end else require_constant_expression e "Extern variable initialization must be a constant value"
					| Var v when not (is_physical_field cf) ->
						(* disallow initialization of non-physical fields (issue #1958) *)
						display_error ctx "This field cannot be initialized because it is not a real variable" p; e
					| Var v when not fctx.is_static ->
						let e = if ctx.com.display.dms_display && ctx.com.display.dms_error_policy <> EPCollect then
							e
						else begin
							let e = Optimizer.reduce_loop ctx (maybe_run_analyzer e) in
							let e = (match Optimizer.make_constant_expression ctx e with
								| Some e -> e
								| None -> e
							) in
							let rec check_this e = match e.eexpr with
								| TConst TThis ->
									display_error ctx "Cannot access this or other member field in variable initialization" e.epos;
									raise Exit
								| TLocal v when (match ctx.vthis with Some v2 -> v == v2 | None -> false) ->
									display_error ctx "Cannot access this or other member field in variable initialization" e.epos;
									raise Exit
								| _ ->
								Type.iter check_this e
							in
							(try check_this e with Exit -> ());
							e
						end in
						e
					| Var v when v.v_read = AccInline ->
						let e = require_constant_expression e "Inline variable initialization must be a constant value" in
						begin match c.cl_kind with
							| KAbstractImpl a when Meta.has Meta.Enum cf.cf_meta && Meta.has Meta.Enum a.a_meta ->
								unify ctx t (TAbstract(a,(List.map (fun _ -> mk_mono()) a.a_params))) p;
								begin match e.eexpr with
									| TCast(e1,None) -> unify ctx e1.etype a.a_this e1.epos
									| _ -> assert false
								end
							| _ ->
								()
						end;
						e
					| _ ->
						e
					) in
					let e = check_cast e in
					cf.cf_expr <- Some e;
					cf.cf_type <- t;
					if fctx.is_display_field then Display.DisplayEmitter.maybe_display_field ctx (cf.cf_name_pos) cf;
				end;
				t
			) "bind_var" in
			if not fctx.is_static then cctx.force_constructor <- true;
			bind_type (ctx,cctx,fctx) cf r (snd e)

	let create_variable (ctx,cctx,fctx) c f t eo p =
		if not fctx.is_static && cctx.abstract <> None then error (fst f.cff_name ^ ": Cannot declare member variable in abstract") p;
		if fctx.is_inline && not fctx.is_static then error (fst f.cff_name ^ ": Inline variable must be static") p;
		if fctx.is_inline && eo = None then error (fst f.cff_name ^ ": Inline variable must be initialized") p;
		if fctx.is_final && eo = None then begin
			if fctx.is_static then error (fst f.cff_name ^ ": Static final variable must be initialized") p
			else cctx.uninitialized_final <- Some f.cff_pos;
		end;
		let t = (match t with
			| None when not fctx.is_static && eo = None ->
				error ("Type required for member variable " ^ fst f.cff_name) p;
			| None ->
				mk_mono()
			| Some t ->
				(* TODO is_lib: only load complex type if needed *)
				let old = ctx.type_params in
				if fctx.is_static then ctx.type_params <- (match cctx.abstract with
					| Some a -> a.a_params
					| _ -> []
				);
				let t = load_complex_type ctx true p t in
				if fctx.is_static then ctx.type_params <- old;
				t
		) in
		let kind = if fctx.is_inline then
			{ v_read = AccInline ; v_write = AccNever }
		else if fctx.is_final then
			{ v_read = AccNormal ; v_write = if fctx.is_static then AccNever else AccCtor }
		else
			{ v_read = AccNormal ; v_write = AccNormal }
		in
		let cf = {
			(mk_field (fst f.cff_name) t f.cff_pos (pos f.cff_name)) with
			cf_doc = f.cff_doc;
			cf_meta = (if fctx.is_final && not (Meta.has Meta.Final f.cff_meta) then (Meta.Final,[],null_pos) :: f.cff_meta else f.cff_meta);
			cf_kind = Var kind;
			cf_public = is_public (ctx,cctx) f.cff_access None;
		} in
		ctx.curfield <- cf;
		bind_var (ctx,cctx,fctx) cf eo;
		cf

	let check_abstract (ctx,cctx,fctx) c cf fd t ret p =
		match cctx.abstract with
			| Some a ->
				let m = mk_mono() in
				let ta = TAbstract(a, List.map (fun _ -> mk_mono()) a.a_params) in
				let tthis = if fctx.is_abstract_member || Meta.has Meta.To cf.cf_meta then monomorphs a.a_params a.a_this else a.a_this in
				let allows_no_expr = ref (Meta.has Meta.CoreType a.a_meta) in
				let rec loop ml = match ml with
					| (Meta.From,_,_) :: _ ->
						let r = exc_protect ctx (fun r ->
							r := lazy_processing (fun () -> t);
							(* the return type of a from-function must be the abstract, not the underlying type *)
							if not fctx.is_macro then (try type_eq EqStrict ret ta with Unify_error l -> error (error_msg (Unify l)) p);
							match t with
								| TFun([_,_,t],_) -> t
								| _ -> error (cf.cf_name ^ ": @:from cast functions must accept exactly one argument") p
						) "@:from" in
						a.a_from_field <- (TLazy r,cf) :: a.a_from_field;
					| (Meta.To,_,_) :: _ ->
						if fctx.is_macro then error (cf.cf_name ^ ": Macro cast functions are not supported") p;
						(* TODO: this doesn't seem quite right... *)
						if not (Meta.has Meta.Impl cf.cf_meta) then cf.cf_meta <- (Meta.Impl,[],null_pos) :: cf.cf_meta;
						let resolve_m args =
							(try unify_raise ctx t (tfun (tthis :: args) m) cf.cf_pos with Error (Unify l,p) -> error (error_msg (Unify l)) p);
							match follow m with
								| TMono _ when (match t with TFun(_,r) -> r == t_dynamic | _ -> false) -> t_dynamic
								| m -> m
						in
						let r = exc_protect ctx (fun r ->
							r := lazy_processing (fun () -> t);
							let args = if Meta.has Meta.MultiType a.a_meta then begin
								let ctor = try
									PMap.find "_new" c.cl_statics
								with Not_found ->
									error "Constructor of multi-type abstract must be defined before the individual @:to-functions are" cf.cf_pos
								in
								(* delay ctx PFinal (fun () -> unify ctx m tthis f.cff_pos); *)
								let args = match follow (monomorphs a.a_params ctor.cf_type) with
									| TFun(args,_) -> List.map (fun (_,_,t) -> t) args
									| _ -> assert false
								in
								args
							end else
								[]
							in
							let t = resolve_m args in
							t
						) "@:to" in
						a.a_to_field <- (TLazy r, cf) :: a.a_to_field
					| ((Meta.ArrayAccess,_,_) | (Meta.Op,[(EArrayDecl _),_],_)) :: _ ->
						if fctx.is_macro then error (cf.cf_name ^ ": Macro array-access functions are not supported") p;
						a.a_array <- cf :: a.a_array;
					| (Meta.Op,[EBinop(op,_,_),_],_) :: _ ->
						if fctx.is_macro then error (cf.cf_name ^ ": Macro operator functions are not supported") p;
						let targ = if fctx.is_abstract_member then tthis else ta in
						let left_eq,right_eq = match follow t with
							| TFun([(_,_,t1);(_,_,t2)],_) ->
								type_iseq targ t1,type_iseq targ t2
							| _ ->
								if fctx.is_abstract_member then
									error (cf.cf_name ^ ": Member @:op functions must accept exactly one argument") cf.cf_pos
								else
									error (cf.cf_name ^ ": Static @:op functions must accept exactly two arguments") cf.cf_pos
						in
						if not (left_eq || right_eq) then error (cf.cf_name ^ ": The left or right argument type must be " ^ (s_type (print_context()) targ)) cf.cf_pos;
						if right_eq && Meta.has Meta.Commutative cf.cf_meta then error (cf.cf_name ^ ": @:commutative is only allowed if the right argument is not " ^ (s_type (print_context()) targ)) cf.cf_pos;
						a.a_ops <- (op,cf) :: a.a_ops;
						allows_no_expr := true;
					| (Meta.Op,[EUnop(op,flag,_),_],_) :: _ ->
						if fctx.is_macro then error (cf.cf_name ^ ": Macro operator functions are not supported") p;
						let targ = if fctx.is_abstract_member then tthis else ta in
						(try type_eq EqStrict t (tfun [targ] (mk_mono())) with Unify_error l -> raise (Error ((Unify l),cf.cf_pos)));
						a.a_unops <- (op,flag,cf) :: a.a_unops;
						allows_no_expr := true;
					| (Meta.Impl,_,_) :: ml when cf.cf_name <> "_new" && not fctx.is_macro ->
						begin match follow t with
							| TFun((_,_,t1) :: _, _) when type_iseq tthis t1 ->
								()
							| _ ->
								display_error ctx ("First argument of implementation function must be " ^ (s_type (print_context()) tthis)) cf.cf_pos
						end;
						loop ml
					| ((Meta.Resolve,_,_) | (Meta.Op,[EField _,_],_)) :: _ ->
						if a.a_resolve <> None then error "Multiple resolve methods are not supported" cf.cf_pos;
						let targ = if fctx.is_abstract_member then tthis else ta in
						begin match follow t with
							| TFun([(_,_,t1);(_,_,t2)],_) ->
								if not fctx.is_macro then begin
									if not (type_iseq targ t1) then error ("First argument type must be " ^ (s_type (print_context()) targ)) cf.cf_pos;
									if not (type_iseq ctx.t.tstring t2) then error ("Second argument type must be String") cf.cf_pos
								end
							| _ ->
								error ("Field type of resolve must be " ^ (s_type (print_context()) targ) ^ " -> String -> T") cf.cf_pos
						end;
						a.a_resolve <- Some cf;
					| _ :: ml ->
						loop ml
					| [] ->
						()
				in
				loop cf.cf_meta;
				let check_bind () =
					if fd.f_expr = None then begin
						if fctx.is_inline then error (cf.cf_name ^ ": Inline functions must have an expression") cf.cf_pos;
						begin match fd.f_type with
							| None -> error (cf.cf_name ^ ": Functions without expressions must have an explicit return type") cf.cf_pos
							| Some _ -> ()
						end;
						cf.cf_meta <- (Meta.NoExpr,[],null_pos) :: cf.cf_meta;
						fctx.do_bind <- false;
						if not (Meta.has Meta.CoreType a.a_meta) then fctx.do_add <- false;
					end
				in
				if cf.cf_name = "_new" && Meta.has Meta.MultiType a.a_meta then fctx.do_bind <- false;
				if !allows_no_expr then check_bind()
			| _ ->
				()

	let create_method (ctx,cctx,fctx) c f fd p =
		let params = type_function_params ctx fd (fst f.cff_name) p in
		if Meta.has Meta.Generic f.cff_meta then begin
			if params = [] then error (fst f.cff_name ^ ": Generic functions must have type parameters") p;
		end;
		let fd = if fctx.is_macro && not ctx.in_macro && not fctx.is_static then
			(* remove display of first argument which will contain the "this" expression *)
			{ fd with f_args = match fd.f_args with [] -> [] | _ :: l -> l }
		else
			fd
		in
		let fd = if not fctx.is_macro then
			fd
		else begin
			if ctx.in_macro then begin
				(* a class with a macro cannot be extern in macro context (issue #2015) *)
				c.cl_extern <- false;
				let texpr = CTPath { tpackage = ["haxe";"macro"]; tname = "Expr"; tparams = []; tsub = None } in
				(* ExprOf type parameter might contain platform-specific type, let's replace it by Expr *)
				let no_expr_of (t,p) = match t with
					| CTPath { tpackage = ["haxe";"macro"]; tname = "Expr"; tsub = Some ("ExprOf"); tparams = [TPType _] }
					| CTPath { tpackage = []; tname = ("ExprOf"); tsub = None; tparams = [TPType _] } -> Some (texpr,p)
					| t -> Some (t,p)
				in
				{
					f_params = fd.f_params;
					f_type = (match fd.f_type with None -> Some (texpr,null_pos) | Some t -> no_expr_of t);
					f_args = List.map (fun (a,o,m,t,e) -> a,o,m,(match t with None -> Some (texpr,null_pos) | Some t -> no_expr_of t),e) fd.f_args;
					f_expr = fd.f_expr;
				}
			end else
				let tdyn = Some (CTPath { tpackage = []; tname = "Dynamic"; tparams = []; tsub = None },null_pos) in
				let to_dyn p t = match t with
					| { tpackage = ["haxe";"macro"]; tname = "Expr"; tsub = Some ("ExprOf"); tparams = [TPType t] } -> Some t
					| { tpackage = []; tname = ("ExprOf"); tsub = None; tparams = [TPType t] } -> Some t
					| { tpackage = ["haxe"]; tname = ("PosInfos"); tsub = None; tparams = [] } -> error "haxe.PosInfos is not allowed on macro functions, use Context.currentPos() instead" p
					| _ -> tdyn
				in
				{
					f_params = fd.f_params;
					f_type = (match fd.f_type with Some (CTPath t,p) -> to_dyn p t | _ -> tdyn);
					f_args = List.map (fun (a,o,m,t,_) -> a,o,m,(match t with Some (CTPath t,p) -> to_dyn p t | _ -> tdyn),None) fd.f_args;
					f_expr = None;
				}
		end in
		begin match c.cl_interface,fctx.field_kind with
			| true,FKConstructor ->
				error "An interface cannot have a constructor" p;
			| true,_ ->
				if not fctx.is_static && fd.f_expr <> None then error (fst f.cff_name ^ ": An interface method cannot have a body") p;
				if fctx.is_inline && c.cl_interface then error (fst f.cff_name ^ ": You can't declare inline methods in interfaces") p;
			| false,FKConstructor ->
				if fctx.is_static then error "A constructor must not be static" p;
				begin match fd.f_type with
					| None | Some (CTPath { tpackage = []; tname = "Void" },_) -> ()
					| _ -> error "A class constructor can't have a return value" p;
				end
			| false,_ ->
				()
		end;
		let parent = (if not fctx.is_static then get_parent c (fst f.cff_name) else None) in
		let dynamic = List.mem ADynamic f.cff_access || (match parent with Some { cf_kind = Method MethDynamic } -> true | _ -> false) in
		if fctx.is_inline && dynamic then error (fst f.cff_name ^ ": You can't have both 'inline' and 'dynamic'") p;
		ctx.type_params <- (match cctx.abstract with
			| Some a when fctx.is_abstract_member ->
				params @ a.a_params
			| _ ->
				if fctx.is_static then params else params @ ctx.type_params);
		(* TODO is_lib: avoid forcing the return type to be typed *)
		let ret = if fctx.field_kind = FKConstructor then ctx.t.tvoid else type_opt (ctx,cctx) p fd.f_type in
		let rec loop args = match args with
			| ((name,p),opt,m,t,ct) :: args ->
				(* TODO is_lib: avoid forcing the field to be typed *)
				let t, ct = type_function_arg ctx (type_opt (ctx,cctx) p t) ct opt p in
				delay ctx PTypeField (fun() -> match follow t with
					| TAbstract({a_path = ["haxe";"extern"],"Rest"},_) ->
						if not c.cl_extern then error "Rest argument are only supported for extern methods" p;
						if opt then error "Rest argument cannot be optional" p;
						begin match ct with None -> () | Some (_,p) -> error "Rest argument cannot have default value" p end;
						if args <> [] then error "Rest should only be used for the last function argument" p;
					| _ ->
						()
				);
				(name, ct, t) :: (loop args)
			| [] ->
				[]
		in
		let args = loop fd.f_args in
		let t = TFun (fun_args args,ret) in
		let cf = {
			(mk_field (fst f.cff_name) t f.cff_pos (pos f.cff_name)) with
			cf_doc = f.cff_doc;
			cf_meta = (if fctx.is_final && not (Meta.has Meta.Final f.cff_meta) then (Meta.Final,[],null_pos) :: f.cff_meta else f.cff_meta);
			cf_kind = Method (if fctx.is_macro then MethMacro else if fctx.is_inline then MethInline else if dynamic then MethDynamic else MethNormal);
			cf_public = is_public (ctx,cctx) f.cff_access parent;
			cf_params = params;
		} in
		cf.cf_meta <- List.map (fun (m,el,p) -> match m,el with
			| Meta.AstSource,[] -> (m,(match fd.f_expr with None -> [] | Some e -> [e]),p)
			| _ -> m,el,p
		) cf.cf_meta;
		generate_value_meta ctx.com (Some c) cf fd.f_args;
		check_abstract (ctx,cctx,fctx) c cf fd t ret p;
		init_meta_overloads ctx (Some c) cf;
		ctx.curfield <- cf;
		let r = exc_protect ~force:false ctx (fun r ->
			if not !return_partial_type then begin
				r := lazy_processing (fun() -> t);
				cctx.context_init();
				incr stats.s_methods_typed;
				if ctx.com.verbose then Common.log ctx.com ("Typing " ^ (if ctx.in_macro then "macro " else "") ^ s_type_path c.cl_path ^ "." ^ fst f.cff_name);
				let fmode = (match cctx.abstract with
					| Some _ ->
						(match args with
						| ("this",_,_) :: _ -> FunMemberAbstract
						| _ when fst f.cff_name = "_new" -> FunMemberAbstract
						| _ -> FunStatic)
					| None ->
						if fctx.field_kind = FKConstructor then FunConstructor else if fctx.is_static then FunStatic else FunMember
				) in
				(match ctx.com.platform with
					| Java when is_java_native_function cf.cf_meta ->
						if fd.f_expr <> None then
							ctx.com.warning "@:native function definitions shouldn't include an expression. This behaviour is deprecated." cf.cf_pos;
						cf.cf_expr <- None;
						cf.cf_type <- t
					| _ ->
						let e , fargs = type_function ctx args ret fmode fd fctx.is_display_field p in
						begin match fctx.field_kind with
						| FKNormal when not fctx.is_static -> check_overriding ctx c cf
						| _ -> ()
						end;
						(* Disabled for now, see https://github.com/HaxeFoundation/haxe/issues/3033 *)
						(* List.iter (fun (v,_) ->
							if v.v_name <> "_" && has_mono v.v_type then ctx.com.warning "Uninferred function argument, please add a type-hint" v.v_pos;
						) fargs; *)
						let tf = {
							tf_args = fargs;
							tf_type = ret;
							tf_expr = e;
						} in
						if fctx.field_kind = FKInit then
							(match e.eexpr with
							| TBlock [] | TBlock [{ eexpr = TConst _ }] | TConst _ | TObjectDecl [] -> ()
							| _ -> c.cl_init <- Some e);
						cf.cf_expr <- Some (mk (TFunction tf) t p);
						cf.cf_type <- t;
					if fctx.is_display_field then Display.DisplayEmitter.maybe_display_field ctx (cf.cf_name_pos) cf);
			end;
			t
		) "type_fun" in
		if fctx.do_bind then bind_type (ctx,cctx,fctx) cf r (match fd.f_expr with Some e -> snd e | None -> f.cff_pos)
		else if fctx.is_display_field then Display.DisplayEmitter.maybe_display_field ctx (cf.cf_name_pos) cf;
		cf

	let create_property (ctx,cctx,fctx) c f (get,set,t,eo) p =
		let name = fst f.cff_name in
		(match cctx.abstract with
		| Some a when fctx.is_abstract_member ->
			ctx.type_params <- a.a_params;
		| _ -> ());
		(* TODO is_lib: lazify load_complex_type *)
		let ret = (match t, eo with
			| None, None -> error (name ^ ": Property must either define a type or a default value") p;
			| None, _ -> mk_mono()
			| Some t, _ -> load_complex_type ctx true p t
		) in
		let t_get,t_set = match cctx.abstract with
			| Some a when fctx.is_abstract_member ->
				if Meta.has Meta.IsVar f.cff_meta then error (name ^ ": Abstract properties cannot be real variables") f.cff_pos;
				let ta = apply_params a.a_params (List.map snd a.a_params) a.a_this in
				tfun [ta] ret, tfun [ta;ret] ret
			| _ -> tfun [] ret, TFun(["value",false,ret],ret)
		in
		let find_accessor m =
			(* on pf_overload platforms, the getter/setter may have been defined as an overloaded function; get all overloads *)
			if ctx.com.config.pf_overload then
				if fctx.is_static then
					let f = PMap.find m c.cl_statics in
					(f.cf_type, f) :: (List.map (fun f -> f.cf_type, f) f.cf_overloads)
				else
					Overloads.get_overloads c m
			else
				[ if fctx.is_static then
					let f = PMap.find m c.cl_statics in
					f.cf_type, f
				else match class_field c (List.map snd c.cl_params) m with
					| _, t,f -> t,f ]
		in
		let check_method m t req_name =
			if ctx.com.display.dms_error_policy = EPIgnore then () else
			try
				let overloads = find_accessor m in
				(* choose the correct overload if and only if there is more than one overload found *)
				let rec get_overload overl = match overl with
					| [tf] -> tf
					| (t2,f2) :: overl ->
						if type_iseq t t2 then
							(t2,f2)
						else
							get_overload overl
					| [] ->
						if c.cl_interface then
							raise Not_found
						else
							raise (Error (Custom
								(Printf.sprintf "No overloaded method named %s was compatible with the property %s with expected type %s" m (name) (s_type (print_context()) t)
							), p))
				in
				let t2, f2 = get_overload overloads in
				(* accessors must be public on As3 (issue #1872) *)
				if Common.defined ctx.com Define.As3 then f2.cf_meta <- (Meta.Public,[],null_pos) :: f2.cf_meta;
				(match f2.cf_kind with
					| Method MethMacro ->
						display_error ctx (f2.cf_name ^ ": Macro methods cannot be used as property accessor") p;
						display_error ctx (f2.cf_name ^ ": Accessor method is here") f2.cf_pos;
					| _ -> ());
				unify_raise ctx t2 t f2.cf_pos;
				if (fctx.is_abstract_member && not (Meta.has Meta.Impl f2.cf_meta)) || (Meta.has Meta.Impl f2.cf_meta && not (fctx.is_abstract_member)) then
					display_error ctx "Mixing abstract implementation and static properties/accessors is not allowed" f2.cf_pos;
				(match req_name with None -> () | Some n -> display_error ctx ("Please use " ^ n ^ " to name your property access method") f2.cf_pos);
				f2.cf_meta <- List.fold_left (fun acc ((m,_,_) as meta) -> match m with
					| Meta.Deprecated -> meta :: acc
					| _ -> acc
				) f2.cf_meta f.cff_meta;
			with
				| Error (Unify l,p) -> raise (Error (Stack (Custom ("In method " ^ m ^ " required by property " ^ name),Unify l),p))
				| Not_found ->
					if req_name <> None then display_error ctx (name ^ ": Custom property accessor is no longer supported, please use get/set") p else
					if c.cl_interface then begin
						let cf = mk_field m t p null_pos in
						cf.cf_meta <- [Meta.CompilerGenerated,[],null_pos];
						cf.cf_kind <- Method MethNormal;
						c.cl_fields <- PMap.add cf.cf_name cf c.cl_fields;
						c.cl_ordered_fields <- cf :: c.cl_ordered_fields;
					end else if not c.cl_extern then begin
						try
							let _, _, f2 = (if not fctx.is_static then let f = PMap.find m c.cl_statics in None, f.cf_type, f else class_field c (List.map snd c.cl_params) m) in
							display_error ctx (Printf.sprintf "Method %s is no valid accessor for %s because it is %sstatic" m (name) (if fctx.is_static then "not " else "")) f2.cf_pos
						with Not_found ->
							display_error ctx ("Method " ^ m ^ " required by property " ^ name ^ " is missing") p
					end
		in
		let display_accessor m p =
			try
				let cf = match find_accessor m with [_,cf] -> cf | _ -> raise Not_found in
				Display.DisplayEmitter.display_field ctx.com.display cf p
			with Not_found ->
				()
		in
		let delay_check = if c.cl_interface then delay_late ctx PBuildClass else delay ctx PTypeField in
		let get = (match get with
			| "null",_ -> AccNo
			| "dynamic",_ -> AccCall
			| "never",_ -> AccNever
			| "default",_ -> AccNormal
			| get,pget ->
				let get = if get = "get" then "get_" ^ name else get in
				if fctx.is_display_field && Display.is_display_position pget then delay ctx PTypeField (fun () -> display_accessor get pget);
				if not cctx.is_lib then delay_check (fun() -> check_method get t_get (if get <> "get" && get <> "get_" ^ name then Some ("get_" ^ name) else None));
				AccCall
		) in
		let set = (match set with
			| "null",_ ->
				(* standard flash library read-only variables can't be accessed for writing, even in subclasses *)
				if c.cl_extern && (match c.cl_path with "flash" :: _	, _ -> true | _ -> false) && ctx.com.platform = Flash then
					AccNever
				else
					AccNo
			| "never",_ -> AccNever
			| "dynamic",_ -> AccCall
			| "default",_ -> AccNormal
			| set,pset ->
				let set = if set = "set" then "set_" ^ name else set in
				if fctx.is_display_field && Display.is_display_position pset then delay ctx PTypeField (fun () -> display_accessor set pset);
				if not cctx.is_lib then delay_check (fun() -> check_method set t_set (if set <> "set" && set <> "set_" ^ name then Some ("set_" ^ name) else None));
				AccCall
		) in
		if (set = AccNormal && get = AccCall) || (set = AccNever && get = AccNever)  then error (name ^ ": Unsupported property combination") p;
		let cf = {
			(mk_field name ret f.cff_pos (pos f.cff_name)) with
			cf_doc = f.cff_doc;
			cf_meta = f.cff_meta;
			cf_kind = Var { v_read = get; v_write = set };
			cf_public = is_public (ctx,cctx) f.cff_access None;
		} in
		ctx.curfield <- cf;
		bind_var (ctx,cctx,fctx) cf eo;
		cf

	let init_field (ctx,cctx,fctx) f =
		let c = cctx.tclass in
		let name = fst f.cff_name in
		check_global_metadata ctx f.cff_meta (fun m -> f.cff_meta <- m :: f.cff_meta) c.cl_module.m_path c.cl_path (Some name);
		let p = f.cff_pos in
		if name.[0] = '$' then display_error ctx "Field names starting with a dollar are not allowed" p;
		List.iter (fun acc ->
			match (acc, f.cff_kind) with
			| APublic, _ | APrivate, _ | AStatic, _ | AFinal, _ -> ()
			| ADynamic, FFun _ | AOverride, FFun _ | AMacro, FFun _ | AInline, FFun _ | AInline, FVar _ -> ()
			| _, FVar _ -> error ("Invalid accessor '" ^ Ast.s_access acc ^ "' for variable " ^ name) p
			| _, FProp _ -> error ("Invalid accessor '" ^ Ast.s_access acc ^ "' for property " ^ name) p
		) f.cff_access;
		if fctx.is_override then (match c.cl_super with None -> error ("Invalid override on field '" ^ name ^ "': class has no super class") p | _ -> ());
		match f.cff_kind with
		| FVar (t,e) ->
			create_variable (ctx,cctx,fctx) c f t e p
		| FFun fd ->
			create_method (ctx,cctx,fctx) c f fd p
		| FProp (get,set,t,eo) ->
			create_property (ctx,cctx,fctx) c f (get,set,t,eo) p

	let check_overloads ctx c =
		(* check if field with same signature was declared more than once *)
		List.iter (fun f ->
			if Meta.has Meta.Overload f.cf_meta then
				List.iter (fun f2 ->
					try
						ignore (List.find (fun f3 -> f3 != f2 && Overloads.same_overload_args f2.cf_type f3.cf_type f2 f3) (f :: f.cf_overloads));
						display_error ctx ("Another overloaded field of same signature was already declared : " ^ f2.cf_name) f2.cf_pos
					with | Not_found -> ()
			) (f :: f.cf_overloads)) (c.cl_ordered_fields @ c.cl_ordered_statics)

	let init_class ctx c p context_init herits fields =
		let ctx,cctx = create_class_context ctx c context_init p in
		if cctx.is_class_debug then print_endline ("Created class context: " ^ dump_class_context cctx);
		let fields = patch_class ctx c fields in
		let fields = build_fields (ctx,cctx) c fields in
		if cctx.is_core_api && ctx.com.display.dms_check_core_api then delay ctx PForce (fun() -> init_core_api ctx c);
		if not cctx.is_lib then begin
			if ctx.com.config.pf_overload then delay ctx PForce (fun() -> check_overloads ctx c)
		end;
		let rec has_field f = function
			| None -> false
			| Some (c,_) ->
				PMap.exists f c.cl_fields || has_field f c.cl_super || List.exists (fun i -> has_field f (Some i)) c.cl_implements
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
						if not (ParserEntry.is_true (ParserEntry.eval ctx.com.defines e)) then
							Some (sc,(match List.rev l with (EConst (String msg),_) :: _ -> Some msg | _ -> None))
						else
							loop l
				in
				loop conds
			| _ :: l ->
				check_require l
		in
		let rec check_if_feature = function
			| [] -> []
			| (Meta.IfFeature,el,_) :: _ -> List.map (fun (e,p) -> match e with EConst (String s) -> s | _ -> error "String expected" p) el
			| _ :: l -> check_if_feature l
		in
		let cl_if_feature = check_if_feature c.cl_meta in
		let cl_req = check_require c.cl_meta in
		List.iter (fun f ->
			let p = f.cff_pos in
			try
				let ctx,fctx = create_field_context (ctx,cctx) c f in
				if fctx.is_field_debug then print_endline ("Created field context: " ^ dump_field_context fctx);
				let cf = init_field (ctx,cctx,fctx) f in
				if fctx.is_field_debug then print_endline ("Created field: " ^ Printer.s_tclass_field "" cf);
				if fctx.is_static && c.cl_interface && fctx.field_kind <> FKInit && not cctx.is_lib then error "You can't declare static fields in interfaces" p;
				let set_feature s =
					ctx.m.curmod.m_extra.m_if_feature <- (s,(c,cf,fctx.is_static)) :: ctx.m.curmod.m_extra.m_if_feature
				in
				List.iter set_feature cl_if_feature;
				List.iter set_feature (check_if_feature cf.cf_meta);
				let req = check_require f.cff_meta in
				let req = (match req with None -> if fctx.is_static || fctx.field_kind = FKConstructor then cl_req else None | _ -> req) in
				(match req with
				| None -> ()
				| Some r -> cf.cf_kind <- Var { v_read = AccRequire (fst r, snd r); v_write = AccRequire (fst r, snd r) });
				begin match fctx.field_kind with
				| FKConstructor ->
					begin match c.cl_constructor with
					| None ->
							c.cl_constructor <- Some cf
					| Some ctor when ctx.com.config.pf_overload ->
							if Meta.has Meta.Overload cf.cf_meta && Meta.has Meta.Overload ctor.cf_meta then
								ctor.cf_overloads <- cf :: ctor.cf_overloads
							else
								display_error ctx ("If using overloaded constructors, all constructors must be declared with @:overload") (if Meta.has Meta.Overload cf.cf_meta then ctor.cf_pos else cf.cf_pos)
					| Some ctor ->
								display_error ctx "Duplicate constructor" p
					end
				| FKInit ->
					()
				| FKNormal ->
					let dup = if fctx.is_static then PMap.exists cf.cf_name c.cl_fields || has_field cf.cf_name c.cl_super else PMap.exists cf.cf_name c.cl_statics in
					if not cctx.is_native && not c.cl_extern && dup then error ("Same field name can't be use for both static and instance : " ^ cf.cf_name) p;
					if fctx.is_override then c.cl_overrides <- cf :: c.cl_overrides;
					let is_var f = match cf.cf_kind with | Var _ -> true | _ -> false in
					if PMap.mem cf.cf_name (if fctx.is_static then c.cl_statics else c.cl_fields) then
						if ctx.com.config.pf_overload && Meta.has Meta.Overload cf.cf_meta && not (is_var f) then
							let mainf = PMap.find cf.cf_name (if fctx.is_static then c.cl_statics else c.cl_fields) in
							if is_var mainf then display_error ctx "Cannot declare a variable with same name as a method" mainf.cf_pos;
							(if not (Meta.has Meta.Overload mainf.cf_meta) then display_error ctx ("Overloaded methods must have @:overload metadata") mainf.cf_pos);
							mainf.cf_overloads <- cf :: mainf.cf_overloads
						else
							display_error ctx ("Duplicate class field declaration : " ^ cf.cf_name) p
					else
					if fctx.do_add then add_field c cf (fctx.is_static || fctx.is_macro && ctx.in_macro)
				end
			with Error (Custom str,p2) when p = p2 ->
				display_error ctx str p
		) fields;
		(match cctx.abstract with
		| Some a ->
			a.a_to_field <- List.rev a.a_to_field;
			a.a_from_field <- List.rev a.a_from_field;
			a.a_ops <- List.rev a.a_ops;
			a.a_unops <- List.rev a.a_unops;
			a.a_array <- List.rev a.a_array;
		| None -> ());
		c.cl_ordered_statics <- List.rev c.cl_ordered_statics;
		c.cl_ordered_fields <- List.rev c.cl_ordered_fields;
		(*
			make sure a default contructor with same access as super one will be added to the class structure at some point.
		*)
		begin match cctx.uninitialized_final with
			| Some pf when c.cl_constructor = None ->
				display_error ctx "This class has uninitialized final vars, which requires a constructor" p;
				error "Example of an uninitialized final var" pf
			| _ ->
				()
		end;
		(* add_constructor does not deal with overloads correctly *)
		if not ctx.com.config.pf_overload then add_constructor ctx c cctx.force_constructor p;
		if Meta.has Meta.StructInit c.cl_meta then check_struct_init_constructor ctx c p;
		(* check overloaded constructors *)
		(if ctx.com.config.pf_overload && not cctx.is_lib then match c.cl_constructor with
		| Some ctor ->
			delay ctx PTypeField (fun() ->
				List.iter (fun f ->
					try
						(* TODO: consider making a broader check, and treat some types, like TAnon and type parameters as Dynamic *)
						ignore(List.find (fun f2 -> f != f2 && Overloads.same_overload_args f.cf_type f2.cf_type f f2) (ctor :: ctor.cf_overloads));
						display_error ctx ("Another overloaded field of same signature was already declared : " ^ f.cf_name) f.cf_pos;
					with Not_found -> ()
				) (ctor :: ctor.cf_overloads)
			)
		| _ -> ());
		(* push delays in reverse order so they will be run in correct order *)
		List.iter (fun (ctx,r) ->
			init_class_done ctx;
			(match r with
			| None -> ()
			| Some r -> delay ctx PTypeField (fun() -> ignore(lazy_type r)))
		) cctx.delayed_expr
end

let check_module_types ctx m p t =
	let t = t_infos t in
	try
		let m2 = Hashtbl.find ctx.g.types_module t.mt_path in
		if m.m_path <> m2 && String.lowercase (s_type_path m2) = String.lowercase (s_type_path m.m_path) then error ("Module " ^ s_type_path m2 ^ " is loaded with a different case than " ^ s_type_path m.m_path) p;
		error ("Type name " ^ s_type_path t.mt_path ^ " is redefined from module " ^ s_type_path m2) p
	with
		Not_found ->
			Hashtbl.add ctx.g.types_module t.mt_path m.m_path

let add_module ctx m p =
	List.iter (check_module_types ctx m p) m.m_types;
	Hashtbl.add ctx.g.modules m.m_path m

let handle_path_display ctx path p =
	let open Display.ImportHandling in
	match Display.ImportHandling.convert_import_to_something_usable !Parser.resume_display path,ctx.com.display.dms_kind with
		| (IDKPackage sl,_),_ ->
			raise (Parser.TypePath(sl,None,true))
		| (IDKModule(sl,s),_),DMPosition ->
			(* We assume that we want to go to the module file, not a specific type
			   which might not even exist anyway. *)
			let mt = ctx.g.do_load_module ctx (sl,s) p in
			let p = { pfile = mt.m_extra.m_file; pmin = 0; pmax = 0} in
			raise (Display.DisplayPosition [p])
		| (IDKModule(sl,s),_),_ ->
			(* TODO: wait till nadako requests @type display for these, then implement it somehow *)
			raise (Parser.TypePath(sl,Some(s,false),true))
		| (IDKSubType(sl,sm,st),p),DMPosition ->
			resolve_position_by_path ctx { tpackage = sl; tname = sm; tparams = []; tsub = Some st} p
		| (IDKSubType(sl,sm,st),_),_ ->
			raise (Parser.TypePath(sl @ [sm],Some(st,false),true))
		| ((IDKSubTypeField(sl,sm,st,sf) | IDKModuleField(sl,(sm as st),sf)),p),_ ->
			let m = ctx.g.do_load_module ctx (sl,sm) p in
			List.iter (fun t -> match t with
				| TClassDecl c when snd c.cl_path = st ->
					ignore(c.cl_build());
					let cf = PMap.find sf c.cl_statics in
					Display.DisplayEmitter.display_field ctx.com.display cf p
				| _ ->
					()
			) m.m_types;
		| (IDK,_),_ ->
			()

(*
	In this pass, we can access load and access other modules types, but we cannot follow them or access their structure
	since they have not been setup. We also build a context_init list that will be evaluated the first time we evaluate
	an expression into the context
*)
let init_module_type ctx context_init do_init (decl,p) =
	let get_type name =
		try List.find (fun t -> snd (t_infos t).mt_path = name) ctx.m.curmod.m_types with Not_found -> assert false
	in
	let check_path_display path p = match ctx.com.display.dms_kind with
		(* We cannot use ctx.is_display_file because the import could come from an import.hx file. *)
		| DMDiagnostics b when (b || Display.is_display_file p.pfile) && not (ExtString.String.ends_with p.pfile "import.hx") ->
			Display.ImportHandling.add_import_position ctx.com p path;
		| DMStatistics | DMUsage _ ->
			Display.ImportHandling.add_import_position ctx.com p path;
		| _ ->
			if Display.is_display_file p.pfile then handle_path_display ctx path p
	in
	match decl with
	| EImport (path,mode) ->
		ctx.m.module_imports <- (path,mode) :: ctx.m.module_imports;
		check_path_display path p;
		let rec loop acc = function
			| x :: l when is_lower_ident (fst x) -> loop (x::acc) l
			| rest -> List.rev acc, rest
		in
		let pack, rest = loop [] path in
		(match rest with
		| [] ->
			(match mode with
			| IAll ->
				ctx.m.wildcard_packages <- (List.map fst pack,p) :: ctx.m.wildcard_packages
			| _ ->
				(match List.rev path with
				| [] -> raise (Display.DisplayToplevel (DisplayToplevel.collect ctx true));
				| (_,p) :: _ -> error "Module name must start with an uppercase letter" p))
		| (tname,p2) :: rest ->
			let p1 = (match pack with [] -> p2 | (_,p1) :: _ -> p1) in
			let p_type = punion p1 p2 in
			let md = ctx.g.do_load_module ctx (List.map fst pack,tname) p_type in
			let types = md.m_types in
			let no_private (t,_) = not (t_infos t).mt_private in
			let chk_private t p = if (t_infos t).mt_private then error "You can't import a private type" p in
			let has_name name t = snd (t_infos t).mt_path = name in
			let get_type tname =
				let t = (try List.find (has_name tname) types with Not_found -> error (StringError.string_error tname (List.map (fun mt -> snd (t_infos mt).mt_path) types) ("Module " ^ s_type_path md.m_path ^ " does not define type " ^ tname)) p_type) in
				chk_private t p_type;
				t
			in
			let rebind t name =
				if not (name.[0] >= 'A' && name.[0] <= 'Z') then
					error "Type aliases must start with an uppercase letter" p;
				let _, _, f = ctx.g.do_build_instance ctx t p_type in
				(* create a temp private typedef, does not register it in module *)
				TTypeDecl {
					t_path = (fst md.m_path @ ["_" ^ snd md.m_path],name);
					t_module = md;
					t_pos = p;
					t_name_pos = null_pos;
					t_private = true;
					t_doc = None;
					t_meta = [];
					t_params = (t_infos t).mt_params;
					t_type = f (List.map snd (t_infos t).mt_params);
				}
			in
			let add_static_init t name s =
				let name = (match name with None -> s | Some n -> n) in
				match resolve_typedef t with
				| TClassDecl c ->
					ignore(c.cl_build());
					ignore(PMap.find s c.cl_statics);
					ctx.m.module_globals <- PMap.add name (TClassDecl c,s,p) ctx.m.module_globals
				| TEnumDecl e ->
					ignore(PMap.find s e.e_constrs);
					ctx.m.module_globals <- PMap.add name (TEnumDecl e,s,p) ctx.m.module_globals
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
						ctx.m.module_types <- List.filter no_private (List.map (fun t -> t,p) types) @ ctx.m.module_types
					| Some newname ->
						ctx.m.module_types <- (rebind (get_type tname) newname,p) :: ctx.m.module_types);
				| [tsub,p2] ->
					let pu = punion p1 p2 in
					(try
						let tsub = List.find (has_name tsub) types in
						chk_private tsub pu;
						ctx.m.module_types <- ((match name with None -> tsub | Some n -> rebind tsub n),p) :: ctx.m.module_types
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
						ignore(c.cl_build());
						PMap.iter (fun _ cf -> if not (has_meta Meta.NoImportGlobal cf.cf_meta) then ctx.m.module_globals <- PMap.add cf.cf_name (TClassDecl c,cf.cf_name,p) ctx.m.module_globals) c.cl_statics
					| TEnumDecl e ->
						PMap.iter (fun _ c -> if not (has_meta Meta.NoImportGlobal c.ef_meta) then ctx.m.module_globals <- PMap.add c.ef_name (TEnumDecl e,c.ef_name,p) ctx.m.module_globals) e.e_constrs
					| _ ->
						error "No statics to import from this type" p
				) :: !context_init
			))
	| EUsing path ->
		check_path_display path p;
		let t = match List.rev path with
			| (s1,_) :: (s2,_) :: sl ->
				if is_lower_ident s2 then { tpackage = (List.rev (s2 :: List.map fst sl)); tname = s1; tsub = None; tparams = [] }
				else { tpackage = List.rev (List.map fst sl); tname = s2; tsub = Some s1; tparams = [] }
			| (s1,_) :: sl ->
				{ tpackage = List.rev (List.map fst sl); tname = s1; tsub = None; tparams = [] }
			| [] ->
				raise (Display.DisplayToplevel (DisplayToplevel.collect ctx true));
		in
		(* do the import first *)
		let types = (match t.tsub with
			| None ->
				let md = ctx.g.do_load_module ctx (t.tpackage,t.tname) p in
				let types = List.filter (fun t -> not (t_infos t).mt_private) md.m_types in
				ctx.m.module_types <- (List.map (fun t -> t,p) types) @ ctx.m.module_types;
				types
			| Some _ ->
				let t = load_type_def ctx p t in
				ctx.m.module_types <- (t,p) :: ctx.m.module_types;
				[t]
		) in
		(* delay the using since we need to resolve typedefs *)
		let filter_classes types =
			let rec loop acc types = match types with
				| td :: l ->
					(match resolve_typedef td with
					| TClassDecl c | TAbstractDecl({a_impl = Some c}) ->
						loop ((c,p) :: acc) l
					| td ->
						loop acc l)
				| [] ->
					acc
			in
			loop [] types
		in
		context_init := (fun() -> ctx.m.module_using <- filter_classes types @ ctx.m.module_using) :: !context_init
	| EClass d ->
		let c = (match get_type (fst d.d_name) with TClassDecl c -> c | _ -> assert false) in
		if ctx.is_display_file && Display.is_display_position (pos d.d_name) then
			Display.DisplayEmitter.display_module_type ctx.com.display (match c.cl_kind with KAbstractImpl a -> TAbstractDecl a | _ -> TClassDecl c) (pos d.d_name);
		check_global_metadata ctx c.cl_meta (fun m -> c.cl_meta <- m :: c.cl_meta) c.cl_module.m_path c.cl_path None;
		let herits = d.d_flags in
		c.cl_extern <- List.mem HExtern herits;
		c.cl_interface <- List.mem HInterface herits;
		let prev_build_count = ref (!build_count - 1) in
		let build() =
			let fl = Inheritance.set_heritance ctx c herits p in
			let rec build() =
				c.cl_build <- (fun()-> Building [c]);
				try
					List.iter (fun f -> f()) fl;
					ClassInitializer.init_class ctx c p do_init d.d_flags d.d_data;
					c.cl_build <- (fun()-> Built);
					incr build_count;
					List.iter (fun (_,t) -> ignore(follow t)) c.cl_params;
					Built;
				with Build_canceled state ->
					c.cl_build <- make_pass ctx build;
					let rebuild() =
						delay_late ctx PBuildClass (fun() -> ignore(c.cl_build()));
					in
					(match state with
					| Built -> assert false
					| Building cl ->
						if !build_count = !prev_build_count then error ("Loop in class building prevent compiler termination (" ^ String.concat "," (List.map (fun c -> s_type_path c.cl_path) cl) ^ ")") c.cl_pos;
						prev_build_count := !build_count;
						rebuild();
						Building (c :: cl)
					| BuildMacro f ->
						f := rebuild :: !f;
						state);
				| exn ->
					c.cl_build <- (fun()-> Built);
					raise exn
			in
			build()
		in
		ctx.pass <- PBuildClass;
		ctx.curclass <- c;
		c.cl_build <- make_pass ctx build;
		ctx.pass <- PBuildModule;
		ctx.curclass <- null_class;
		delay ctx PBuildClass (fun() -> ignore(c.cl_build()));
		if (ctx.com.platform = Java || ctx.com.platform = Cs) && not c.cl_extern then
			delay ctx PTypeField (fun () ->
				let metas = check_strict_meta ctx c.cl_meta in
				if metas <> [] then c.cl_meta <- metas @ c.cl_meta;
				let rec run_field cf =
					let metas = check_strict_meta ctx cf.cf_meta in
					if metas <> [] then cf.cf_meta <- metas @ cf.cf_meta;
					List.iter run_field cf.cf_overloads
				in
				List.iter run_field c.cl_ordered_statics;
				List.iter run_field c.cl_ordered_fields;
				match c.cl_constructor with
					| Some f -> run_field f
					| _ -> ()
			);
	| EEnum d ->
		let e = (match get_type (fst d.d_name) with TEnumDecl e -> e | _ -> assert false) in
		if ctx.is_display_file && Display.is_display_position (pos d.d_name) then
			Display.DisplayEmitter.display_module_type ctx.com.display (TEnumDecl e) (pos d.d_name);
		let ctx = { ctx with type_params = e.e_params } in
		let h = (try Some (Hashtbl.find ctx.g.type_patches e.e_path) with Not_found -> None) in
		check_global_metadata ctx e.e_meta (fun m -> e.e_meta <- m :: e.e_meta) e.e_module.m_path e.e_path None;
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
						| _ -> FFun { f_params = c.ec_params; f_type = c.ec_type; f_expr = None; f_args = List.map (fun (n,o,t) -> (n,null_pos),o,[],Some t,None) c.ec_args });
				}
			) (!constructs)
		in
		let init () = List.iter (fun f -> f()) !context_init in
		build_module_def ctx (TEnumDecl e) e.e_meta get_constructs init (fun (e,p) ->
			match e with
			| EVars [_,Some (CTAnonymous fields,p),None] ->
				constructs := List.map (fun f ->
					let args, params, t = (match f.cff_kind with
					| FVar (t,None) -> [], [], t
					| FFun { f_params = pl; f_type = t; f_expr = (None|Some (EBlock [],_)); f_args = al } ->
						let al = List.map (fun ((n,_),o,_,t,_) -> match t with None -> error "Missing function parameter type" f.cff_pos | Some t -> n,o,t) al in
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
		let et = TEnum (e,List.map snd e.e_params) in
		let names = ref [] in
		let index = ref 0 in
		let is_flat = ref true in
		let fields = ref PMap.empty in
		List.iter (fun c ->
			let p = c.ec_pos in
			let params = ref [] in
			params := type_type_params ~enum_constructor:true ctx ([],fst c.ec_name) (fun() -> !params) c.ec_pos c.ec_params;
			let params = !params in
			let ctx = { ctx with type_params = params @ ctx.type_params } in
			let rt = (match c.ec_type with
				| None -> et
				| Some t ->
					let t = load_complex_type ctx true p t in
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
					TFun (List.map (fun (s,opt,(t,tp)) ->
						(match t with CTPath({tpackage=[];tname="Void"}) -> error "Arguments of type Void are not allowed in enum constructors" c.ec_pos | _ -> ());
						if PMap.mem s (!pnames) then error ("Duplicate parameter '" ^ s ^ "' in enum constructor " ^ fst c.ec_name) p;
						pnames := PMap.add s () (!pnames);
						s, opt, load_type_hint ~opt ctx p (Some (t,tp))
					) l, rt)
			) in
			if PMap.mem (fst c.ec_name) e.e_constrs then error ("Duplicate constructor " ^ fst c.ec_name) p;
			let f = {
				ef_name = fst c.ec_name;
				ef_type = t;
				ef_pos = p;
				ef_name_pos = snd c.ec_name;
				ef_doc = c.ec_doc;
				ef_index = !index;
				ef_params = params;
				ef_meta = c.ec_meta;
			} in
			let cf = {
				(mk_field f.ef_name f.ef_type p f.ef_name_pos) with
				cf_kind = (match follow f.ef_type with
					| TFun _ -> Method MethNormal
					| _ -> Var { v_read = AccNormal; v_write = AccNo }
				);
				cf_doc = f.ef_doc;
				cf_params = f.ef_params;
			} in
 			if ctx.is_display_file && Display.is_display_position p then
 				Display.DisplayEmitter.display_enum_field ctx.com.display f p;
			e.e_constrs <- PMap.add f.ef_name f e.e_constrs;
			fields := PMap.add cf.cf_name cf !fields;
			incr index;
			names := (fst c.ec_name) :: !names;
		) (!constructs);
		e.e_names <- List.rev !names;
		e.e_extern <- e.e_extern;
		e.e_type.t_params <- e.e_params;
		e.e_type.t_type <- TAnon {
			a_fields = !fields;
			a_status = ref (EnumStatics e);
		};
		if !is_flat then e.e_meta <- (Meta.FlatEnum,[],null_pos) :: e.e_meta;

		if (ctx.com.platform = Java || ctx.com.platform = Cs) && not e.e_extern then
			delay ctx PTypeField (fun () ->
				let metas = check_strict_meta ctx e.e_meta in
				e.e_meta <- metas @ e.e_meta;
				PMap.iter (fun _ ef ->
					let metas = check_strict_meta ctx ef.ef_meta in
					if metas <> [] then ef.ef_meta <- metas @ ef.ef_meta
				) e.e_constrs
			);
	| ETypedef d ->
		let t = (match get_type (fst d.d_name) with TTypeDecl t -> t | _ -> assert false) in
		if ctx.is_display_file && Display.is_display_position (pos d.d_name) then
			Display.DisplayEmitter.display_module_type ctx.com.display (TTypeDecl t) (pos d.d_name);
		check_global_metadata ctx t.t_meta (fun m -> t.t_meta <- m :: t.t_meta) t.t_module.m_path t.t_path None;
		let ctx = { ctx with type_params = t.t_params } in
		let tt = load_complex_type ctx true p d.d_data in
		let tt = (match fst d.d_data with
		| CTExtend _ -> tt
		| CTPath { tpackage = ["haxe";"macro"]; tname = "MacroType" } ->
			(* we need to follow MacroType immediately since it might define other module types that we will load afterwards *)
			if t.t_type == follow tt then error "Recursive typedef is not allowed" p;
			tt
		| _ ->
			if (Meta.has Meta.Eager d.d_meta) then
				follow tt
			else begin
				let rec check_rec tt =
					if tt == t.t_type then error "Recursive typedef is not allowed" p;
					match tt with
					| TMono r ->
						(match !r with
						| None -> ()
						| Some t -> check_rec t)
					| TLazy f ->
						check_rec (lazy_type f);
					| TType (td,tl) ->
						if td == t then error "Recursive typedef is not allowed" p;
						check_rec (apply_params td.t_params tl td.t_type)
					| _ ->
						()
				in
				let r = exc_protect ctx (fun r ->
					r := lazy_processing (fun() -> tt);
					check_rec tt;
					tt
				) "typedef_rec_check" in
				TLazy r
			end
		) in
		(match t.t_type with
		| TMono r ->
			(match !r with
			| None -> r := Some tt;
			| Some _ -> assert false);
		| _ -> assert false);
		if ctx.com.platform = Cs && t.t_meta <> [] then
			delay ctx PTypeField (fun () ->
				let metas = check_strict_meta ctx t.t_meta in
				if metas <> [] then t.t_meta <- metas @ t.t_meta;
			);
	| EAbstract d ->
		let a = (match get_type (fst d.d_name) with TAbstractDecl a -> a | _ -> assert false) in
		if ctx.is_display_file && Display.is_display_position (pos d.d_name) then
			Display.DisplayEmitter.display_module_type ctx.com.display (TAbstractDecl a) (pos d.d_name);
		check_global_metadata ctx a.a_meta (fun m -> a.a_meta <- m :: a.a_meta) a.a_module.m_path a.a_path None;
		let ctx = { ctx with type_params = a.a_params } in
		let is_type = ref false in
		let load_type t from =
			let t = load_complex_type ctx true p t in
			let t = if not (Meta.has Meta.CoreType a.a_meta) then begin
				if !is_type then begin
					let r = exc_protect ctx (fun r ->
						r := lazy_processing (fun() -> t);
						let at = monomorphs a.a_params a.a_this in
						(try (if from then Type.unify t at else Type.unify at t) with Unify_error _ -> error "You can only declare from/to with compatible types" p);
						t
					) "constraint" in
					TLazy r
				end else
					error "Missing underlying type declaration or @:coreType declaration" p;
			end else begin
				if Meta.has Meta.Callable a.a_meta then
					error "@:coreType abstracts cannot be @:callable" p;
				t
			end in
			t
		in
		List.iter (function
			| AFromType t -> a.a_from <- (load_type t true) :: a.a_from
			| AToType t -> a.a_to <- (load_type t false) :: a.a_to
			| AIsType t ->
				if a.a_impl = None then error "Abstracts with underlying type must have an implementation" a.a_pos;
				if Meta.has Meta.CoreType a.a_meta then error "@:coreType abstracts cannot have an underlying type" p;
				let at = load_complex_type ctx true p t in
				delay ctx PForce (fun () ->
					begin match follow at with
						| TAbstract(a2,_) when a == a2 -> error "Abstract underlying type cannot be recursive" a.a_pos
						| _ -> ()
					end;
				);
				a.a_this <- at;
				is_type := true;
			| AExtern ->
				(match a.a_impl with Some c -> c.cl_extern <- true | None -> (* Hmmmm.... *) ())
			| APrivAbstract -> ()
		) d.d_flags;
		if not !is_type then begin
			if Meta.has Meta.CoreType a.a_meta then
				a.a_this <- TAbstract(a,List.map snd a.a_params)
			else
				error "Abstract is missing underlying type declaration" a.a_pos
		end

let module_pass_2 ctx m decls tdecls p =
	(* here is an additional PASS 1 phase, which define the type parameters for all module types.
		 Constraints are handled lazily (no other type is loaded) because they might be recursive anyway *)
	List.iter (fun d ->
		match d with
		| (TClassDecl c, (EClass d, p)) ->
			c.cl_params <- type_type_params ctx c.cl_path (fun() -> c.cl_params) p d.d_params;
			if Meta.has Meta.Generic c.cl_meta && c.cl_params <> [] then c.cl_kind <- KGeneric;
			if Meta.has Meta.GenericBuild c.cl_meta then begin
				if ctx.in_macro then error "@:genericBuild cannot be used in macros" c.cl_pos;
				c.cl_kind <- KGenericBuild d.d_data;
			end;
			if c.cl_path = (["haxe";"macro"],"MacroType") then c.cl_kind <- KMacroType;
		| (TEnumDecl e, (EEnum d, p)) ->
			e.e_params <- type_type_params ctx e.e_path (fun() -> e.e_params) p d.d_params;
		| (TTypeDecl t, (ETypedef d, p)) ->
			t.t_params <- type_type_params ctx t.t_path (fun() -> t.t_params) p d.d_params;
		| (TAbstractDecl a, (EAbstract d, p)) ->
			a.a_params <- type_type_params ctx a.a_path (fun() -> a.a_params) p d.d_params;
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
	List.iter (init_module_type ctx context_init do_init) tdecls

(*
	Creates a module context for [m] and types [tdecls] using it.
*)
let type_types_into_module ctx m tdecls p =
	let decls, tdecls = module_pass_1 ctx m tdecls p in
	let types = List.map fst decls in
	List.iter (check_module_types ctx m p) types;
	m.m_types <- m.m_types @ types;
	(* define the per-module context for the next pass *)
	let ctx = {
		com = ctx.com;
		g = ctx.g;
		t = ctx.t;
		m = {
			curmod = m;
			module_types = List.map (fun t -> t,null_pos) ctx.g.std.m_types;
			module_using = [];
			module_globals = PMap.empty;
			wildcard_packages = [];
			module_imports = [];
		};
		is_display_file = (ctx.com.display.dms_display && Display.is_display_file m.m_extra.m_file);
		meta = [];
		this_stack = [];
		with_type_stack = [];
		call_argument_stack = [];
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
		in_macro = ctx.in_macro;
		in_display = false;
		in_loop = false;
		opened = [];
		in_call_args = false;
		vthis = None;
	} in
	if ctx.g.std != null_module then begin
		add_dependency m ctx.g.std;
		(* this will ensure both String and (indirectly) Array which are basic types which might be referenced *)
		ignore(load_core_type ctx "String");
	end;
	module_pass_2 ctx m decls tdecls p;
	ctx

let handle_import_hx ctx m decls p =
	let path_split = match List.rev (Path.get_path_parts m.m_extra.m_file) with
		| [] -> []
		| _ :: l -> l
	in
	let join l = String.concat Path.path_sep (List.rev ("import.hx" :: l)) in
	let rec loop path pack = match path,pack with
		| _,[] -> [join path]
		| (p :: path),(_ :: pack) -> (join (p :: path)) :: (loop path pack)
		| _ -> []
	in
	let candidates = loop path_split (fst m.m_path) in
	let make_import_module path r =
		Hashtbl.replace ctx.com.parser_cache path r;
		(* We use the file path as module name to make it unique. This may or may not be a good idea... *)
		let m_import = make_module ctx ([],path) path p in
		m_import.m_extra.m_kind <- MImport;
		add_module ctx m_import p;
		m_import
	in
	List.fold_left (fun acc path ->
		let decls = try
			let r = Hashtbl.find ctx.com.parser_cache path in
			let mimport = Hashtbl.find ctx.g.modules ([],path) in
			if mimport.m_extra.m_kind <> MFake then add_dependency m mimport;
			r
		with Not_found ->
			if Sys.file_exists path then begin
				let _,r = parse_file ctx.com path p in
				List.iter (fun (d,p) -> match d with EImport _ | EUsing _ -> () | _ -> error "Only import and using is allowed in import.hx files" p) r;
				add_dependency m (make_import_module path r);
				r
			end else begin
				let r = [] in
				(* Add empty decls so we don't check the file system all the time. *)
				(make_import_module path r).m_extra.m_kind <- MFake;
				r
			end
		in
		decls @ acc
	) decls candidates

(*
	Creates a new module and types [tdecls] into it.
*)
let type_module ctx mpath file ?(is_extern=false) tdecls p =
	let m = make_module ctx mpath file p in
	Hashtbl.add ctx.g.modules m.m_path m;
	let tdecls = handle_import_hx ctx m tdecls p in
	let ctx = type_types_into_module ctx m tdecls p in
	if is_extern then m.m_extra.m_kind <- MExtern;
	begin if ctx.is_display_file then match ctx.com.display.dms_kind with
		| DMResolve s ->
			resolve_position_by_path ctx {tname = s; tpackage = []; tsub = None; tparams = []} p
		| _ ->
			()
	end;
	m

let resolve_module_file com m remap p =
	let forbid = ref false in
	let compose_path no_rename =
		(match m with
		| [] , name -> name
		| x :: l , name ->
			let x = (try
				match PMap.find x com.package_rules with
				| Forbidden -> forbid := true; x
				| Directory d -> if no_rename then x else d
				| Remap d -> remap := d :: l; d
				with Not_found -> x
			) in
			String.concat "/" (x :: l) ^ "/" ^ name
		) ^ ".hx"
	in
	let file = try
			Common.find_file com (compose_path false)
		with Not_found ->
			Common.find_file com (compose_path true)
	in
	let file = (match String.lowercase (snd m) with
	| "con" | "aux" | "prn" | "nul" | "com1" | "com2" | "com3" | "lpt1" | "lpt2" | "lpt3" when Sys.os_type = "Win32" ->
		(* these names are reserved by the OS - old DOS legacy, such files cannot be easily created but are reported as visible *)
		if (try (Unix.stat file).Unix.st_size with _ -> 0) > 0 then file else raise Not_found
	| _ -> file
	) in
	(* if we try to load a std.xxxx class and resolve a real std file, the package name is not valid, ignore *)
	(match fst m with
	| "std" :: _ ->
		let file = Path.unique_full_path file in
		if List.exists (fun path -> ExtString.String.starts_with file (try Path.unique_full_path path with _ -> path)) com.std_path then raise Not_found;
	| _ -> ());
	if !forbid then begin
		let _, decls = (!parse_hook) com file p in
		let rec loop decls = match decls with
			| ((EImport _,_) | (EUsing _,_)) :: decls -> loop decls
			| (EClass d,_) :: _ -> d.d_meta
			| (EEnum d,_) :: _ -> d.d_meta
			| (EAbstract d,_) :: _ -> d.d_meta
			| (ETypedef d,_) :: _ -> d.d_meta
			| [] -> []
		in
		let meta = loop decls in
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
		if p == null_pos then
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
							tname = fst d.d_name;
							tparams = List.map (fun tp ->
								TPType (CTPath { tpackage = []; tname = fst tp.tp_name; tparams = []; tsub = None; },null_pos)
							) d.d_params;
							tsub = None;
						}),null_pos;
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
			let is_extern = ref false in
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
				is_extern := true;
				loop ctx.com.load_extern_type
			) in
			let is_extern = !is_extern in
			try
				type_module ctx m file ~is_extern decls p
			with Forbid_package (inf,pl,pf) when p <> null_pos ->
				raise (Forbid_package (inf,p::pl,pf))
	) in
	add_dependency ctx.m.curmod m2;
	if ctx.pass = PTypeField then flush_pass ctx PBuildClass "load_module";
	m2

;;
type_function_params_rec := type_function_params

(* -------------------------------------------------------------------------- *)
(* generic classes *)

exception Generic_Exception of string * pos

type generic_context = {
	ctx : typer;
	subst : (t * t) list;
	name : string;
	p : pos;
	mutable mg : module_def option;
}

let make_generic ctx ps pt p =
	let rec loop l1 l2 =
		match l1, l2 with
		| [] , [] -> []
		| (x,TLazy f) :: l1, _ -> loop ((x,lazy_type f) :: l1) l2
		| (_,t1) :: l1 , t2 :: l2 -> (t1,t2) :: loop l1 l2
		| _ -> assert false
	in
	let name =
		String.concat "_" (List.map2 (fun (s,_) t ->
			let s_type_path_underscore (p,s) = match p with [] -> s | _ -> String.concat "_" p ^ "_" ^ s in
			let rec loop top t = match follow t with
				| TInst(c,tl) -> (s_type_path_underscore c.cl_path) ^ (loop_tl tl)
				| TEnum(en,tl) -> (s_type_path_underscore en.e_path) ^ (loop_tl tl)
				| TAbstract(a,tl) -> (s_type_path_underscore a.a_path) ^ (loop_tl tl)
				| _ when not top -> "_" (* allow unknown/incompatible types as type parameters to retain old behavior *)
				| TMono _ -> raise (Generic_Exception (("Could not determine type for parameter " ^ s), p))
				| TDynamic _ -> "Dynamic"
				| t -> raise (Generic_Exception (("Type parameter must be a class or enum instance (found " ^ (s_type (print_context()) t) ^ ")"), p))
			and loop_tl tl = match tl with
				| [] -> ""
				| tl -> "_" ^ String.concat "_" (List.map (loop false) tl)
			in
			loop true t
		) ps pt)
	in
	{
		ctx = ctx;
		subst = loop ps pt;
		name = name;
		p = p;
		mg = None;
	}

let rec generic_substitute_type gctx t =
	match t with
	| TInst ({ cl_kind = KGeneric } as c2,tl2) ->
		(* maybe loop, or generate cascading generics *)
		let _, _, f = gctx.ctx.g.do_build_instance gctx.ctx (TClassDecl c2) gctx.p in
		let t = f (List.map (generic_substitute_type gctx) tl2) in
		(match follow t,gctx.mg with TInst(c,_), Some m -> add_dependency m c.cl_module | _ -> ());
		t
	| _ ->
		try
			generic_substitute_type gctx (List.assq t gctx.subst)
		with Not_found ->
			Type.map (generic_substitute_type gctx) t

let generic_substitute_expr gctx e =
	let vars = Hashtbl.create 0 in
	let build_var v =
		try
			Hashtbl.find vars v.v_id
		with Not_found ->
			let v2 = alloc_var v.v_name (generic_substitute_type gctx v.v_type) v.v_pos in
			v2.v_meta <- v.v_meta;
			Hashtbl.add vars v.v_id v2;
			v2
	in
	let rec build_expr e =
		match e.eexpr with
		| TField(e1, FInstance({cl_kind = KGeneric} as c,tl,cf)) ->
			let _, _, f = gctx.ctx.g.do_build_instance gctx.ctx (TClassDecl c) gctx.p in
			let t = f (List.map (generic_substitute_type gctx) tl) in
			let fa = try
				quick_field t cf.cf_name
			with Not_found ->
				error (Printf.sprintf "Type %s has no field %s (possible typing order issue)" (s_type (print_context()) t) cf.cf_name) e.epos
			in
			build_expr {e with eexpr = TField(e1,fa)}
		| TTypeExpr (TClassDecl ({cl_kind = KTypeParameter _;} as c)) when Meta.has Meta.Const c.cl_meta ->
			let rec loop subst = match subst with
				| (t1,t2) :: subst ->
					begin match follow t1 with
						| TInst(c2,_) when c == c2 -> t2
						| _ -> loop subst
					end
				| [] -> raise Not_found
			in
			begin try
				let t = loop gctx.subst in
				begin match follow t with
					| TInst({cl_kind = KExpr e},_) -> type_expr gctx.ctx e Value
					| _ -> error "Only Const type parameters can be used as value" e.epos
				end
			with Not_found ->
				e
			end
		| _ ->
			map_expr_type build_expr (generic_substitute_type gctx) build_var e
	in
	build_expr e

let get_short_name =
	let i = ref (-1) in
	(fun () ->
		incr i;
		Printf.sprintf "Hx___short___hx_type_%i" !i
	)

let rec build_generic ctx c p tl =
	let pack = fst c.cl_path in
	let recurse = ref false in
	let rec check_recursive t =
		match follow t with
		| TInst (c2,tl) ->
			(match c2.cl_kind with
			| KTypeParameter tl ->
				if not (is_generic_parameter ctx c2) && has_ctor_constraint c2 then
					error "Type parameters with a constructor cannot be used non-generically" p;
				recurse := true
			| _ -> ());
			List.iter check_recursive tl;
		| _ ->
			()
	in
	List.iter check_recursive tl;
	if !recurse || not (ctx.com.display.dms_full_typing) then begin
		TInst (c,tl) (* build a normal instance *)
	end else begin
	let gctx = make_generic ctx c.cl_params tl p in
	let name = (snd c.cl_path) ^ "_" ^ gctx.name in
	try
		load_instance ctx ({ tpackage = pack; tname = name; tparams = []; tsub = None },p) false p
	with Error(Module_not_found path,_) when path = (pack,name) ->
		let m = (try Hashtbl.find ctx.g.modules (Hashtbl.find ctx.g.types_module c.cl_path) with Not_found -> assert false) in
		(* let ctx = { ctx with m = { ctx.m with module_types = m.m_types @ ctx.m.module_types } } in *)
		ignore(c.cl_build()); (* make sure the super class is already setup *)
		let mg = {
			m_id = alloc_mid();
			m_path = (pack,name);
			m_types = [];
			m_extra = module_extra (s_type_path (pack,name)) m.m_extra.m_sign 0. MFake m.m_extra.m_check_policy;
		} in
		gctx.mg <- Some mg;
		let cg = mk_class mg (pack,name) c.cl_pos null_pos in
		mg.m_types <- [TClassDecl cg];
		Hashtbl.add ctx.g.modules mg.m_path mg;
		add_dependency mg m;
		add_dependency ctx.m.curmod mg;
		(* ensure that type parameters are set in dependencies *)
		let dep_stack = ref [] in
		let rec loop t =
			if not (List.memq t !dep_stack) then begin
			dep_stack := t :: !dep_stack;
			match t with
			| TInst (c,tl) -> add_dep c.cl_module tl
			| TEnum (e,tl) -> add_dep e.e_module tl
			| TType (t,tl) -> add_dep t.t_module tl
			| TAbstract (a,tl) -> add_dep a.a_module tl
			| TMono r ->
				(match !r with
				| None -> ()
				| Some t -> loop t)
			| TLazy f ->
				loop (lazy_type f);
			| TDynamic t2 ->
				if t == t2 then () else loop t2
			| TAnon a ->
				PMap.iter (fun _ f -> loop f.cf_type) a.a_fields
			| TFun (args,ret) ->
				List.iter (fun (_,_,t) -> loop t) args;
				loop ret
			end
		and add_dep m tl =
			add_dependency mg m;
			List.iter loop tl
		in
		List.iter loop tl;
		let build_field cf_old =
			(* We have to clone the type parameters (issue #4672). We cannot substitute the constraints immediately because
			   we need the full substitution list first. *)
			let param_subst,params = List.fold_left (fun (subst,params) (s,t) -> match follow t with
				| TInst(c,tl) as t ->
					let t2 = TInst({c with cl_module = mg;},tl) in
					(t,t2) :: subst,(s,t2) :: params
				| _ -> assert false
			) ([],[]) cf_old.cf_params in
			let gctx = {gctx with subst = param_subst @ gctx.subst} in
			let cf_new = {cf_old with cf_pos = cf_old.cf_pos} in (* copy *)
			(* Type parameter constraints are substituted here. *)
			cf_new.cf_params <- List.rev_map (fun (s,t) -> match follow t with
				| TInst({cl_kind = KTypeParameter tl1} as c,_) ->
					let tl1 = List.map (generic_substitute_type gctx) tl1 in
					c.cl_kind <- KTypeParameter tl1;
					s,t
				| _ -> assert false
			) params;
			let f () =
				let t = generic_substitute_type gctx cf_old.cf_type in
				ignore (follow t);
				begin try (match cf_old.cf_expr with
					| None ->
						begin match cf_old.cf_kind with
							| Method _ when not c.cl_interface && not c.cl_extern ->
								display_error ctx (Printf.sprintf "Field %s has no expression (possible typing order issue)" cf_new.cf_name) cf_new.cf_pos;
								display_error ctx (Printf.sprintf "While building %s" (s_type_path cg.cl_path)) p;
							| _ ->
								()
						end
					| Some e ->
						cf_new.cf_expr <- Some (generic_substitute_expr gctx e)
				) with Unify_error l ->
					error (error_msg (Unify l)) cf_new.cf_pos
				end;
				t
			in
			let r = exc_protect ctx (fun r ->
				let t = mk_mono() in
				r := lazy_processing (fun() -> t);
				unify_raise ctx (f()) t p;
				t
			) "build_generic" in
			cf_new.cf_type <- TLazy r;
			cf_new
		in
		if c.cl_init <> None || c.cl_dynamic <> None then error "This class can't be generic" p;
		List.iter (fun cf -> match cf.cf_kind with
			| Method MethMacro when not ctx.in_macro -> ()
			| _ -> error "A generic class can't have static fields" cf.cf_pos
		) c.cl_ordered_statics;
		cg.cl_super <- (match c.cl_super with
			| None -> None
			| Some (cs,pl) ->
				let ts = follow (apply_params c.cl_params tl (TInst(cs,pl))) in
				let cs,pl = Inheritance.check_extends ctx c ts p in
				match cs.cl_kind with
				| KGeneric ->
					(match build_generic ctx cs p pl with
					| TInst (cs,pl) -> Some (cs,pl)
					| _ -> assert false)
				| _ -> Some(cs,pl)
		);
		add_constructor ctx cg false p;
		cg.cl_kind <- KGenericInstance (c,tl);
		cg.cl_meta <- (Meta.NoDoc,[],null_pos) :: cg.cl_meta;
		if has_meta Meta.Keep c.cl_meta then cg.cl_meta <- (Meta.Keep,[],null_pos) :: cg.cl_meta;
		cg.cl_interface <- c.cl_interface;
		cg.cl_constructor <- (match cg.cl_constructor, c.cl_constructor, c.cl_super with
			| _, Some cf, _ -> Some (build_field cf)
			| Some ctor, _, _ -> Some ctor
			| None, None, None -> None
			| _ -> error "Please define a constructor for this class in order to use it as generic" c.cl_pos
		);
		cg.cl_implements <- List.map (fun (i,tl) ->
			(match follow (generic_substitute_type gctx (TInst (i, List.map (generic_substitute_type gctx) tl))) with
			| TInst (i,tl) -> i, tl
			| _ -> assert false)
		) c.cl_implements;
		cg.cl_ordered_fields <- List.map (fun f ->
			let f = build_field f in
			cg.cl_fields <- PMap.add f.cf_name f cg.cl_fields;
			f
		) c.cl_ordered_fields;
		cg.cl_overrides <- List.map (fun f ->
			try PMap.find f.cf_name cg.cl_fields with Not_found -> assert false
		) c.cl_overrides;
		(* In rare cases the class name can become too long, so let's shorten it (issue #3090). *)
		if String.length (snd cg.cl_path) > 254 then begin
			let n = get_short_name () in
			cg.cl_meta <- (Meta.Native,[EConst(String (n)),p],null_pos) :: cg.cl_meta;
		end;
		TInst (cg,[])
	end

(* -------------------------------------------------------------------------- *)
(* MACRO TYPE *)

let get_macro_path ctx e args p =
	let rec loop e =
		match fst e with
		| EField (e,f) -> f :: loop e
		| EConst (Ident i) -> [i]
		| _ -> error "Invalid macro call" p
	in
	let path = match e with
		| (EConst(Ident i)),_ ->
			let path = try
				if not (PMap.mem i ctx.curclass.cl_statics) then raise Not_found;
				ctx.curclass.cl_path
			with Not_found -> try
				(t_infos (let path,_,_ = PMap.find i ctx.m.module_globals in path)).mt_path
			with Not_found ->
				error "Invalid macro call" p
			in
			i :: (snd path) :: (fst path)
		| _ ->
			loop e
	in
	(match path with
	| meth :: cl :: path -> (List.rev path,cl), meth, args
	| _ -> error "Invalid macro call" p)

let build_macro_type ctx pl p =
	let path, field, args = (match pl with
		| [TInst ({ cl_kind = KExpr (ECall (e,args),_) },_)]
		| [TInst ({ cl_kind = KExpr (EArrayDecl [ECall (e,args),_],_) },_)] ->
			get_macro_path ctx e args p
		| _ ->
			error "MacroType requires a single expression call parameter" p
	) in
	let old = ctx.ret in
	let t = (match ctx.g.do_macro ctx MMacroType path field args p with
		| None -> mk_mono()
		| Some _ -> ctx.ret
	) in
	ctx.ret <- old;
	t

let build_macro_build ctx c pl cfl p =
	let path, field, args = match Meta.get Meta.GenericBuild c.cl_meta with
		| _,[ECall(e,args),_],_ -> get_macro_path ctx e args p
		| _ -> error "genericBuild requires a single expression call parameter" p
	in
	let old = ctx.ret,ctx.g.get_build_infos in
	ctx.g.get_build_infos <- (fun() -> Some (TClassDecl c, pl, cfl));
	let t = (match ctx.g.do_macro ctx MMacroType path field args p with
		| None -> mk_mono()
		| Some _ -> ctx.ret
	) in
	ctx.ret <- fst old;
	ctx.g.get_build_infos <- snd old;
	t

(* -------------------------------------------------------------------------- *)
(* API EVENTS *)

let build_instance ctx mtype p =
	match mtype with
	| TClassDecl c ->
		if ctx.pass > PBuildClass then ignore(c.cl_build());
		let build f s =
			let r = exc_protect ctx (fun r ->
				let t = mk_mono() in
				r := lazy_processing (fun() -> t);
				let tf = (f()) in
				unify_raise ctx tf t p;
				link_dynamic t tf;
				if ctx.pass >= PBuildClass then flush_pass ctx PBuildClass "after_build_instance";
				t
			) s in
			TLazy r
		in
		let ft = (fun pl ->
			match c.cl_kind with
			| KGeneric ->
				build (fun () -> build_generic ctx c p pl) "build_generic"
			| KMacroType ->
				build (fun () -> build_macro_type ctx pl p) "macro_type"
			| KGenericBuild cfl ->
				build (fun () -> build_macro_build ctx c pl cfl p) "generic_build"
			| _ ->
				TInst (c,pl)
		) in
		c.cl_params , c.cl_path , ft
	| TEnumDecl e ->
		e.e_params , e.e_path , (fun t -> TEnum (e,t))
	| TTypeDecl t ->
		t.t_params , t.t_path , (fun tl -> TType(t,tl))
	| TAbstractDecl a ->
		a.a_params, a.a_path, (fun tl -> TAbstract(a,tl))
