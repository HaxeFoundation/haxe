(*
 *  Haxe Compiler
 *  Copyright (c)2005-2008 Nicolas Cannasse
 *
 *  This program is free software; you can redistribute it and/or modify
 *  it under the terms of the GNU General Public License as published by
 *  the Free Software Foundation; either version 2 of the License, or
 *  (at your option) any later version.
 *
 *  This program is distributed in the hope that it will be useful,
 *  but WITHOUT ANY WARRANTY; without even the implied warranty of
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 *  GNU General Public License for more details.
 *
 *  You should have received a copy of the GNU General Public License
 *  along with this program; if not, write to the Free Software
 *  Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
 *)
open Ast
open Type
open Common
open Typecore

let exc_protect f =
	let rec r = ref (fun() ->
		try
			f r
		with
			| Error (Protect _,_) as e -> raise e
			| Error (m,p) -> raise (Error (Protect m,p))
	) in
	r

let type_static_var ctx t e p =
	ctx.in_static <- true;
	let e = type_expr ctx e true in
	unify ctx e.etype t p;
	e

(** since load_type is used in PASS2 , it cannot access the structure of a type **)

let load_type_def ctx p tpath =
	let no_pack = fst tpath = [] in
	try
		List.find (fun t ->
			let tp = t_path t in
			tp = tpath || (no_pack && snd tp = snd tpath)
		) ctx.local_types
	with
		Not_found ->
			let tpath, m = (try
				if not no_pack || fst ctx.current.mpath = [] then raise Exit;
				let tpath2 = fst ctx.current.mpath , snd tpath in
				tpath2, ctx.api.load_module tpath2 p
			with
				| Error (Module_not_found _,p2) when p == p2 -> tpath, ctx.api.load_module tpath p
				| Exit -> tpath, ctx.api.load_module tpath p
			) in
			try
				List.find (fun t -> not (t_private t) && t_path t = tpath) m.mtypes
			with
				Not_found -> error ("Module " ^ s_type_path tpath ^ " does not define type " ^ snd tpath) p

let rec load_normal_type ctx t p allow_no_params =
	try
		if t.tpackage <> [] then raise Not_found;
		let pt = List.assoc t.tname ctx.type_params in
		if t.tparams <> [] then error ("Class type parameter " ^ t.tname ^ " can't have parameters") p;
		pt
	with Not_found ->
		let types , path , f = ctx.api.build_instance (load_type_def ctx p (t.tpackage,t.tname)) p in
		if allow_no_params && t.tparams = [] then
			f (List.map (fun (name,t) ->
				match follow t with
				| TInst (c,_) -> if c.cl_implements = [] then mk_mono() else error ("Type parameter " ^ name ^ " need constraint") p
				| _ -> assert false
			) types)
		else if path = ([],"Dynamic") then
			match t.tparams with
			| [] -> t_dynamic
			| [TPType t] -> TDynamic (load_type ctx p t)
			| _ -> error "Too many parameters for Dynamic" p
		else begin
			if List.length types <> List.length t.tparams then error ("Invalid number of type parameters for " ^ s_type_path path) p;
			let tparams = List.map (fun t ->
				match t with
				| TPConst c ->
					let name, const = (match c with
						| String s -> "S" ^ s, TString s
						| Int i -> "I" ^ i, TInt (Int32.of_string i)
						| Float f -> "F" ^ f, TFloat f
						| _ -> assert false
					) in
					let c = mk_class ([],name) p None false in
					c.cl_kind <- KConstant const;
					TInst (c,[])
				| TPType t -> load_type ctx p t
			) t.tparams in
			let params = List.map2 (fun t (name,t2) ->
				let isconst = (match t with TInst ({ cl_kind = KConstant _ },_) -> true | _ -> false) in
				if isconst <> (name = "Const") && t != t_dynamic then error (if isconst then "Constant value unexpected here" else "Constant value excepted as type parameter") p;
				(match follow t2 with
				| TInst (c,[]) ->
					List.iter (fun (i,params) ->
						unify ctx t (apply_params types tparams (TInst (i,params))) p
					) c.cl_implements
				| _ -> assert false);
				t
			) tparams types in
			f params
		end

and load_type ctx p t =
	match t with
	| TPParent t -> load_type ctx p t
	| TPNormal t -> load_normal_type ctx t p false
	| TPExtend (t,l) ->
		(match load_type ctx p (TPAnonymous l) with
		| TAnon a ->
			let rec loop t =
				match follow t with
				| TInst (c,tl) ->
					let c2 = mk_class (fst c.cl_path,"+" ^ snd c.cl_path) p None true in
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
					error "Please ensure correct initialization of cascading signatures" p
				| TAnon a2 ->
					PMap.iter (fun f _ ->
						if PMap.mem f a2.a_fields then error ("Cannot redefine field " ^ f) p
					) a.a_fields;
					mk_anon (PMap.foldi PMap.add a.a_fields a2.a_fields)
				| _ -> error "Cannot only extend classes and anonymous" p
			in
			loop (load_normal_type ctx t p false)
		| _ -> assert false)
	| TPAnonymous l ->
		let rec loop acc (n,pub,f,p) =
			if PMap.mem n acc then error ("Duplicate field declaration : " ^ n) p;
			let t , get, set = (match f with
				| AFVar t ->
					load_type ctx p t, NormalAccess, NormalAccess
				| AFFun (tl,t) ->
					let t = load_type ctx p t in
					let args = List.map (fun (name,o,t) -> name , o, load_type ctx p t) tl in
					TFun (args,t), NormalAccess, MethodCantAccess
				| AFProp (t,i1,i2) ->
					let access m get =
						match m with
						| "null" -> NoAccess
						| "default" -> NormalAccess
						| "dynamic" -> MethodAccess ((if get then "get_"  else "set_") ^ n)
						| _ -> MethodAccess m
					in
					load_type ctx p t, access i1 true, access i2 false
			) in
			PMap.add n {
				cf_name = n;
				cf_type = t;
				cf_public = (match pub with None -> true | Some p -> p);
				cf_get = get;
				cf_set = set;
				cf_params = [];
				cf_expr = None;
				cf_doc = None;
			} acc
		in
		mk_anon (List.fold_left loop PMap.empty l)
	| TPFunction (args,r) ->
		match args with
		| [TPNormal { tpackage = []; tparams = []; tname = "Void" }] ->
			TFun ([],load_type ctx p r)
		| _ ->
			TFun (List.map (fun t -> "",false,load_type ctx p t) args,load_type ctx p r)

let hide_types ctx =
	let old_locals = ctx.local_types in
	let old_type_params = ctx.type_params in
	ctx.local_types <- ctx.std.mtypes;
	ctx.type_params <- [];
	(fun() ->
		ctx.local_types <- old_locals;
		ctx.type_params <- old_type_params;
	)

let load_core_type ctx name =
	let show = hide_types ctx in
	let t = load_normal_type ctx { tpackage = []; tname = name; tparams = [] } null_pos false in
	show();
	t

let is_int t =
	match follow t with
	| TInst (c,[]) ->
		c.cl_path = ([],"Int")
	| _ ->
		false

let is_float t =
	match follow t with
	| TInst (c,[]) ->
		c.cl_path = ([],"Float")
	| _ ->
		false

let t_array_access ctx =
	let show = hide_types ctx in
	match load_type_def ctx null_pos ([],"ArrayAccess") with
	| TClassDecl c ->
		show();
		if List.length c.cl_types <> 1 then assert false;
		let pt = mk_mono() in
		TInst (c,[pt]) , pt
	| _ ->
		assert false

let t_iterator ctx =
	let show = hide_types ctx in
	match load_type_def ctx null_pos ([],"Iterator") with
	| TTypeDecl t ->
		show();
		if List.length t.t_types <> 1 then assert false;
		let pt = mk_mono() in
		apply_params t.t_types [pt] t.t_type, pt
	| _ ->
		assert false

let load_type_opt ?(opt=false) ctx p t =
	let t = (match t with None -> mk_mono() | Some t -> load_type ctx p t) in
	if opt then ctx.api.tnull t else t

(* ---------------------------------------------------------------------- *)
(* Structure check *)

let valid_redefinition ctx f t =
	let ft = field_type f in
	match follow ft , follow t with
	| TFun (args,r) , TFun (targs,tr) when List.length args = List.length targs ->
		List.iter2 (fun (n,o1,a1) (_,o2,a2) ->
			if o1 <> o2 then raise (Unify_error [Not_matching_optional n]);
			type_eq EqStrict a1 a2
		) args targs;
		Type.unify r tr
	| _ , _ ->
		type_eq EqStrict ft t

let check_overriding ctx c p () =
	match c.cl_super with
	| None -> ()
	| Some (csup,params) ->
		PMap.iter (fun i f ->
			try
				let t , f2 = class_field csup i in
				let t = apply_params csup.cl_types params t in
				ignore(follow f.cf_type); (* force evaluation *)
				let p = (match f.cf_expr with None -> p | Some e -> e.epos) in
				if not (List.mem i c.cl_overrides) then
					display_error ctx ("Field " ^ i ^ " should be declared with 'override' since it is inherited from superclass") p
				else if f.cf_public <> f2.cf_public then
					display_error ctx ("Field " ^ i ^ " has different visibility (public/private) than superclass one") p
				else if f2.cf_get = InlineAccess then
					display_error ctx ("Field " ^ i ^ " is inlined and cannot be overridden") p
				else if f2.cf_get <> f.cf_get || f2.cf_set <> f.cf_set then
					display_error ctx ("Field " ^ i ^ " has different property access than in superclass") p
				else try
					valid_redefinition ctx f t
				with
					Unify_error l ->
						display_error ctx ("Field " ^ i ^ " overload parent class with different or incomplete type") p;
						display_error ctx (error_msg (Unify l)) p;
			with
				Not_found ->
					if List.mem i c.cl_overrides then display_error ctx ("Field " ^ i ^ " is declared 'override' but doesn't override any field") p
		) c.cl_fields

let class_field_no_interf c i =
	try
		let f = PMap.find i c.cl_fields in
		field_type f , f
	with Not_found ->
		match c.cl_super with
		| None ->
			raise Not_found
		| Some (c,tl) ->
			(* rec over class_field *)
			let t , f = class_field c i in
			apply_params c.cl_types tl t , f

let rec check_interface ctx c p intf params =
	PMap.iter (fun i f ->
		try
			let t , f2 = class_field_no_interf c i in
			ignore(follow f.cf_type); (* force evaluation *)
			let p = (match f.cf_expr with None -> p | Some e -> e.epos) in
			if f.cf_public && not f2.cf_public then
				display_error ctx ("Field " ^ i ^ " should be public as requested by " ^ s_type_path intf.cl_path) p
			else if not(unify_access f2.cf_get f.cf_get) then
				display_error ctx ("Field " ^ i ^ " has different property access than in " ^ s_type_path intf.cl_path) p
			else
				let t1 = apply_params intf.cl_types params (field_type f) in
				try
					valid_redefinition ctx f2 t1
				with
					Unify_error l ->
						display_error ctx ("Field " ^ i ^ " has different type than in " ^ s_type_path intf.cl_path) p;
						display_error ctx (error_msg (Unify l)) p;
		with
			Not_found ->
				if not c.cl_interface then display_error ctx ("Field " ^ i ^ " needed by " ^ s_type_path intf.cl_path ^ " is missing") p
	) intf.cl_fields;
	List.iter (fun (i2,p2) ->
		check_interface ctx c p i2 (List.map (apply_params intf.cl_types params) p2)
	) intf.cl_implements

let check_interfaces ctx c p () =
	match c.cl_path with
	| "Proxy" :: _ , _ -> ()
	| _ ->
	List.iter (fun (intf,params) -> check_interface ctx c p intf params) c.cl_implements

let rec return_flow ctx e =
	let error() = display_error ctx "A return is missing here" e.epos; raise Exit in
	let return_flow = return_flow ctx in
	match e.eexpr with
	| TReturn _ | TThrow _ -> ()
	| TParenthesis e ->
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
	| TSwitch (e,cases,None) when (match follow e.etype with TEnum _ -> true | _ -> false) ->
		List.iter (fun (_,e) -> return_flow e) cases;
	| TMatch (_,_,cases,def) ->
		List.iter (fun (_,_,e) -> return_flow e) cases;
		(match def with None -> () | Some e -> return_flow e)
	| TTry (e,cases) ->
		return_flow e;
		List.iter (fun (_,_,e) -> return_flow e) cases;
	| _ ->
		error()

(* ---------------------------------------------------------------------- *)
(* PASS 1 & 2 : Module and Class Structure *)

let set_heritance ctx c herits p =
	let rec loop = function
		| HPrivate | HExtern | HInterface ->
			()
		| HExtends t ->
			if c.cl_super <> None then error "Cannot extend several classes" p;
			let t = load_normal_type ctx t p false in
			(match follow t with
			| TInst (cl,params) ->
				if is_parent c cl then error "Recursive class" p;
				if c.cl_interface then error "Cannot extend an interface" p;
				if cl.cl_interface then error "Cannot extend by using an interface" p;
				c.cl_super <- Some (cl,params)
			| _ -> error "Should extend by using a class" p)
		| HImplements t ->
			let t = load_normal_type ctx t p false in
			(match follow t with
			| TInst (cl,params) ->
				if is_parent c cl then error "Recursive class" p;
				c.cl_implements <- (cl, params) :: c.cl_implements
			| TDynamic t ->
				if c.cl_dynamic <> None then error "Cannot have several dynamics" p;
				c.cl_dynamic <- Some t
			| _ -> error "Should implement by using an interface or a class" p)
	in
	List.iter loop (List.filter (ctx.api.on_inherit c p) herits)

let type_type_params ctx path p (n,flags) =
	let c = mk_class (fst path @ [snd path],n) p None false in
	c.cl_kind <- KTypeParameter;
	let t = TInst (c,[]) in
	match flags with
	| [] -> n, t
	| _ ->
		let r = exc_protect (fun r ->
			r := (fun _ -> t);
			set_heritance ctx c (List.map (fun t -> HImplements t) flags) p;
			t
		) in
		ctx.delays := [(fun () -> ignore(!r()))] :: !(ctx.delays);
		n, TLazy r

let type_function ctx t static constr f p =
	let locals = save_locals ctx in
	let fargs , r = (match t with
		| TFun (args,r) -> List.map (fun (n,opt,t) -> add_local ctx n t, opt, t) args, r
		| _ -> assert false
	) in
	let old_ret = ctx.ret in
	let old_static = ctx.in_static in
	let old_constr = ctx.in_constructor in
	let old_opened = ctx.opened in
	ctx.in_static <- static;
	ctx.in_constructor <- constr;
	ctx.ret <- r;
	ctx.opened <- [];
	let e = type_expr ctx f.f_expr false in
	let rec loop e =
		match e.eexpr with
		| TReturn (Some _) -> raise Exit
		| TFunction _ -> ()
		| _ -> Type.iter loop e
	in
	let have_ret = (try loop e; false with Exit -> true) in
	if have_ret then
		(try return_flow ctx e with Exit -> ())
	else
		unify ctx r ctx.api.tvoid p;
	let rec loop e =
		match e.eexpr with
		| TCall ({ eexpr = TConst TSuper },_) -> raise Exit
		| TFunction _ -> ()
		| _ -> Type.iter loop e
	in
	if constr && (match ctx.curclass.cl_super with None -> false | Some (cl,_) -> cl.cl_constructor <> None) then
		(try
			loop e;
			error "Missing super constructor call" p
		with
			Exit -> ());
	locals();
	List.iter (fun r -> r := Closed) ctx.opened;
	ctx.ret <- old_ret;
	ctx.in_static <- old_static;
	ctx.in_constructor <- old_constr;
	ctx.opened <- old_opened;
	e , fargs

let init_class ctx c p herits fields =
	ctx.type_params <- c.cl_types;
	c.cl_extern <- List.mem HExtern herits;
	c.cl_interface <- List.mem HInterface herits;
	set_heritance ctx c herits p;
	let tthis = TInst (c,List.map snd c.cl_types) in
	let rec extends_public c =
		List.exists (fun (c,_) -> c.cl_path = (["haxe"],"Public") || extends_public c) c.cl_implements ||
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
	let type_opt ?opt ctx p t =
		match t with
		| None when c.cl_extern || c.cl_interface ->
			display_error ctx "Type required for extern classes and interfaces" p;
			t_dynamic
		| _ ->
			load_type_opt ?opt ctx p t
	in
	let rec has_field f = function
		| None -> false
		| Some (c,_) ->
			PMap.exists f c.cl_fields || has_field f c.cl_super || List.exists (fun i -> has_field f (Some i)) c.cl_implements
	in
	let loop_cf f p =
		match f with
		| FVar (name,doc,access,t,e) ->
			let stat = List.mem AStatic access in
			let inline = List.mem AInline access in
			if not stat && has_field name c.cl_super then error ("Redefinition of variable " ^ name ^ " in subclass is not allowed") p;
			if inline && not stat then error "Inline variable must be static" p;
			if inline && e = None then error "Inline variable must be initialized" p;
			let t = (match t with
				| None ->
					if not stat then display_error ctx ("Type required for member variable " ^ name) p;
					mk_mono()
				| Some t ->
					let old = ctx.type_params in
					if stat then ctx.type_params <- [];
					let t = load_type ctx p t in
					if stat then ctx.type_params <- old;
					t
			) in
			let cf = {
				cf_name = name;
				cf_doc = doc;
				cf_type = t;
				cf_get = if inline then InlineAccess else NormalAccess;
				cf_set = if inline then NeverAccess else NormalAccess;
				cf_expr = None;
				cf_public = is_public access None;
				cf_params = [];
			} in
			let delay = (match e with
				| None -> (fun() -> ())
				| Some e ->
					let ctx = { ctx with curclass = c; tthis = tthis } in
					let r = exc_protect (fun r ->
						r := (fun() -> t);
						if ctx.com.verbose then print_endline ("Typing " ^ s_type_path c.cl_path ^ "." ^ name);
						cf.cf_expr <- Some (type_static_var ctx t e p);
						t
					) in
					cf.cf_type <- TLazy r;
					(fun () -> ignore(!r()))
			) in
			access, false, cf, delay
		| FFun (name,doc,access,params,f) ->
			let params = List.map (fun (n,flags) ->
				match flags with
				| [] ->
					type_type_params ctx ([],name) p (n,[])
				| _ -> error "This notation is not allowed because it can't be checked" p
			) params in
			let stat = List.mem AStatic access in
			let inline = List.mem AInline access in
			let parent = (if not stat then get_parent c name else None) in
			let dynamic = List.mem ADynamic access || (match parent with Some { cf_set = NormalAccess } -> true | _ -> false) in
			let ctx = { ctx with
				curclass = c;
				curmethod = name;
				tthis = tthis;
				type_params = if stat then params else params @ ctx.type_params;
			} in
			let ret = type_opt ctx p f.f_type in
			let args = List.map (fun (name,opt,t) -> name , opt, type_opt ~opt ctx p t) f.f_args in
			let t = TFun (args,ret) in
			let constr = (name = "new") in
			if constr && c.cl_interface then error "An interface cannot have a constructor" p;
			if c.cl_interface && not stat && (match f.f_expr with EBlock [] , _ -> false | _ -> true) then error "An interface method cannot have a body" p;
			if constr then (match f.f_type with
				| None | Some (TPNormal { tpackage = []; tname = "Void" }) -> ()
				| _ -> error "A class constructor can't have a return value" p
			);
			let cf = {
				cf_name = name;
				cf_doc = doc;
				cf_type = t;
				cf_get = if inline then InlineAccess else NormalAccess;
				cf_set = (if inline then NeverAccess else if dynamic then NormalAccess else MethodCantAccess);
				cf_expr = None;
				cf_public = is_public access parent;
				cf_params = params;
			} in
			let r = exc_protect (fun r ->
				r := (fun() -> t);
				if ctx.com.verbose then print_endline ("Typing " ^ s_type_path c.cl_path ^ "." ^ name);
				let e , fargs = type_function ctx t stat constr f p in
				let f = {
					tf_args = fargs;
					tf_type = ret;
					tf_expr = e;
				} in
				if stat && name = "__init__" then c.cl_init <- Some e;
				cf.cf_expr <- Some (mk (TFunction f) t p);
				t
			) in
			let delay = (
				if (c.cl_extern || c.cl_interface || ctx.isproxy) && cf.cf_name <> "__init__" then
					(fun() -> ())
				else begin
					cf.cf_type <- TLazy r;
					(fun() -> ignore((!r)()))
				end
			) in
			access, constr, cf, delay
		| FProp (name,doc,access,get,set,t) ->
			let ret = load_type ctx p t in
			let check_get = ref (fun() -> ()) in
			let check_set = ref (fun() -> ()) in
			let check_method m t () =
				try
					let t2 = (if List.mem AStatic access then (PMap.find m c.cl_statics).cf_type else fst (class_field c m)) in
					unify_raise ctx t2 t p;
				with
					| Error (Unify l,_) -> raise (Error (Stack (Custom ("In method " ^ m ^ " required by property " ^ name),Unify l),p))
					| Not_found -> if not c.cl_interface then error ("Method " ^ m ^ " required by property " ^ name ^ " is missing") p
			in
			let get = (match get with
				| "null" -> NoAccess
				| "dynamic" -> MethodAccess ("get_" ^ name)
				| "default" -> NormalAccess
				| _ ->
					check_get := check_method get (TFun ([],ret));
					MethodAccess get
			) in
			let set = (match set with
				| "null" ->
					(* standard flash library read-only variables can't be accessed for writing, even in subclasses *)
					if c.cl_extern && (match c.cl_path with "flash" :: _  , _ -> true | _ -> false) && Common.defined ctx.com "flash9" then
						NeverAccess
					else
						NoAccess
				| "dynamic" -> MethodAccess ("set_" ^ name)
				| "default" -> NormalAccess
				| _ ->
					check_set := check_method set (TFun (["",false,ret],ret));
					MethodAccess set
			) in
			if set = NormalAccess && (match get with MethodAccess _ -> true | _ -> false) then error "Unsupported property combination" p;
			let cf = {
				cf_name = name;
				cf_doc = doc;
				cf_get = get;
				cf_set = set;
				cf_expr = None;
				cf_type = ret;
				cf_public = is_public access None;
				cf_params = [];
			} in
			access, false, cf, (fun() -> (!check_get)(); (!check_set)())
	in
	let fl = List.map (fun (f,p) ->
		let access , constr, f , delayed = loop_cf f p in
		let is_static = List.mem AStatic access in
		if is_static && f.cf_name = "name" && Common.defined ctx.com "js" then error "This identifier cannot be used in Javascript for statics" p;
		if (is_static || constr) && c.cl_interface && f.cf_name <> "__init__" then error "You can't declare static fields in interfaces" p;
		if constr then begin
			if c.cl_constructor <> None then error "Duplicate constructor" p;
			c.cl_constructor <- Some f;
		end else if not is_static || f.cf_name <> "__init__" then begin
			if PMap.mem f.cf_name (if is_static then c.cl_statics else c.cl_fields) then error ("Duplicate class field declaration : " ^ f.cf_name) p;
			if is_static then begin
				c.cl_statics <- PMap.add f.cf_name f c.cl_statics;
				c.cl_ordered_statics <- f :: c.cl_ordered_statics;
			end else begin
				c.cl_fields <- PMap.add f.cf_name f c.cl_fields;
				c.cl_ordered_fields <- f :: c.cl_ordered_fields;
				if List.mem AOverride access then c.cl_overrides <- f.cf_name :: c.cl_overrides;
			end;
		end;
		delayed
	) fields in
	c.cl_ordered_statics <- List.rev c.cl_ordered_statics;
	c.cl_ordered_fields <- List.rev c.cl_ordered_fields;
	(*
		define a default inherited constructor.
		This is actually pretty tricky since we can't assume that the constructor of the
		superclass has been defined yet because type structure is not stabilized wrt recursion.
	*)
	let rec define_constructor ctx c =
		try
			Some (Hashtbl.find ctx.constructs c.cl_path)
		with Not_found ->
			match c.cl_super with
			| None -> None
			| Some (csuper,_) ->
				match define_constructor ctx csuper with
				| None -> None
				| Some (acc,pl,f) as infos ->
					let p = c.cl_pos in
					let esuper = (ECall ((EConst (Ident "super"),p),List.map (fun (n,_,_) -> (EConst (Ident n),p)) f.f_args),p) in
					let acc = (if csuper.cl_extern && acc = [] then [APublic] else acc) in
					(* remove types that are superclass type-parameters *)
					let replace_type = function
						| Some (TPNormal { tpackage = []; tname = name; tparams = [] }) when List.exists (fun (param,_) -> name = param) csuper.cl_types ->
							None
						| t -> t
					in
					let fnew = { f with f_expr = esuper; f_args = List.map (fun (a,opt,t) -> a,opt,replace_type t) f.f_args } in
					let _, _, cf, delayed = loop_cf (FFun ("new",None,acc,pl,fnew)) p in
					c.cl_constructor <- Some cf;
					Hashtbl.add ctx.constructs c.cl_path (acc,pl,f);
					ctx.delays := [delayed] :: !(ctx.delays);
					infos
	in
	ignore(define_constructor ctx c);
	fl

let type_module ctx m tdecls loadp =
	(* PASS 1 : build module structure - does not load any module or type - should be atomic ! *)
	let decls = ref [] in
	let decl_with_name name p priv =
		let tpath = if priv then (fst m @ ["_" ^ snd m], name) else (fst m, name) in
		if priv then begin
			if List.exists (fun t -> tpath = t_path t) (!decls) then error ("Type name " ^ name ^ " is alreday defined in this module") p;
			tpath
		end else try
			let m2 = Hashtbl.find ctx.types_module tpath in
			if m <> m2 && String.lowercase (s_type_path m2) = String.lowercase (s_type_path m) then error ("Module " ^ s_type_path m2 ^ " is loaded with a different case than " ^ s_type_path m) loadp;
			error ("Type name " ^ s_type_path tpath ^ " is redefined from module " ^ s_type_path m2) p
		with
			Not_found ->
				Hashtbl.add ctx.types_module (fst m,name) m;
				tpath
	in
	List.iter (fun (d,p) ->
		match d with
		| EImport _ -> ()
		| EClass d ->
			let priv = List.mem HPrivate d.d_flags in
			let path = decl_with_name d.d_name p priv in
			let c = mk_class path p d.d_doc priv in
			(* store the constructor for later usage *)
			List.iter (fun (cf,_) ->
				match cf with
				| FFun ("new",_,acc,pl,f) -> Hashtbl.add ctx.constructs path (acc,pl,f)
				| _ -> ()
			) d.d_data;
			decls := TClassDecl c :: !decls
		| EEnum d ->
			let priv = List.mem EPrivate d.d_flags in
			let path = decl_with_name d.d_name p priv in
			let e = {
				e_path = path;
				e_pos = p;
				e_doc = d.d_doc;
				e_types = [];
				e_private = priv;
				e_extern = List.mem EExtern d.d_flags || d.d_data = [];
				e_constrs = PMap.empty;
				e_names = [];
			} in
			decls := TEnumDecl e :: !decls
		| ETypedef d ->
			let priv = List.mem EPrivate d.d_flags in
			let path = decl_with_name d.d_name p priv in
			let t = {
				t_path = path;
				t_pos = p;
				t_doc = d.d_doc;
				t_private = priv;
				t_types = [];
				t_type = mk_mono();
			} in
			decls := TTypeDecl t :: !decls
	) tdecls;
	let m = {
		mpath = m;
		mtypes = List.rev !decls;
		mimports = [];
	} in
	Hashtbl.add ctx.modules m.mpath m;
	(* PASS 2 : build types structure - does not type any expression ! *)
	let ctx = {
		com = ctx.com;
		api = ctx.api;
		modules = ctx.modules;
		delays = ctx.delays;
		constructs = ctx.constructs;
		types_module = ctx.types_module;
		curclass = ctx.curclass;
		tthis = ctx.tthis;
		std = ctx.std;
		ret = ctx.ret;
		isproxy = ctx.isproxy;
		doinline = ctx.doinline;
		current = m;
		locals = PMap.empty;
		locals_map = PMap.empty;
		locals_map_inv = PMap.empty;
		local_types = ctx.std.mtypes @ m.mtypes;
		type_params = [];
		curmethod = "";
		super_call = false;
		in_constructor = false;
		in_static = false;
		in_display = false;
		in_loop = false;
		untyped = false;
		opened = [];
		param_type = None;
	} in
	let delays = ref [] in
	let get_class name =
		let c = List.find (fun d -> match d with TClassDecl { cl_path = _ , n } -> n = name | _ -> false) m.mtypes in
		match c with TClassDecl c -> c | _ -> assert false
	in
	let get_enum name =
		let e = List.find (fun d -> match d with TEnumDecl { e_path = _ , n } -> n = name | _ -> false) m.mtypes in
		match e with TEnumDecl e -> e | _ -> assert false
	in
	let get_tdef name =
		let s = List.find (fun d -> match d with TTypeDecl { t_path = _ , n } -> n = name | _ -> false) m.mtypes in
		match s with TTypeDecl s -> s | _ -> assert false
	in
	(* here is an additional PASS 1 phase, which handle the type parameters declaration, with lazy contraints *)
	List.iter (fun (d,p) ->
		match d with
		| EImport _ -> ()
		| EClass d ->
			let c = get_class d.d_name in
			c.cl_types <- List.map (type_type_params ctx c.cl_path p) d.d_params;
		| EEnum d ->
			let e = get_enum d.d_name in
			e.e_types <- List.map (type_type_params ctx e.e_path p) d.d_params;
		| ETypedef d ->
			let t = get_tdef d.d_name in
			t.t_types <- List.map (type_type_params ctx t.t_path p) d.d_params;
	) tdecls;
	(* back to PASS2 *)
	List.iter (fun (d,p) ->
		match d with
		| EImport (pack,name,topt) ->
			let md = ctx.api.load_module (pack,name) p in
			let types = List.filter (fun t -> not (t_private t)) md.mtypes in
			(match topt with
			| None -> ctx.local_types <- ctx.local_types @ types
			| Some t ->
				try
					let t = List.find (fun tdecl -> snd (t_path tdecl) = t) types in
					ctx.local_types <- ctx.local_types @ [t]
				with
					Not_found -> error ("Module " ^ s_type_path (pack,name) ^ " does not define type " ^ t) p
			);
			m.mimports <- (md,topt) :: m.mimports;
		| EClass d ->
			let c = get_class d.d_name in
			delays := !delays @ check_overriding ctx c p :: check_interfaces ctx c p :: init_class ctx c p d.d_flags d.d_data
		| EEnum d ->
			let e = get_enum d.d_name in
			ctx.type_params <- e.e_types;
			let et = TEnum (e,List.map snd e.e_types) in
			let names = ref [] in
			let index = ref 0 in
			List.iter (fun (c,doc,t,p) ->
				if c = "name" && Common.defined ctx.com "js" then error "This identifier cannot be used in Javascript" p;
				let t = (match t with
					| [] -> et
					| l -> TFun (List.map (fun (s,opt,t) -> s, opt, load_type_opt ~opt ctx p (Some t)) l, et)
				) in
				if PMap.mem c e.e_constrs then error ("Duplicate constructor " ^ c) p;
				e.e_constrs <- PMap.add c {
					ef_name = c;
					ef_type = t;
					ef_pos = p;
					ef_doc = doc;
					ef_index = !index;
				} e.e_constrs;
				incr index;
				names := c :: !names;
			) d.d_data;
			e.e_names <- List.rev !names;
		| ETypedef d ->
			let t = get_tdef d.d_name in
			ctx.type_params <- t.t_types;
			let tt = load_type ctx p d.d_data in
			if t.t_type == follow tt then error "Recursive typedef is not allowed" p;
			(match t.t_type with
			| TMono r ->
				(match !r with
				| None -> r := Some tt;
				| Some _ -> assert false);
			| _ -> assert false);
	) tdecls;
	(* PASS 3 : type checking, delayed until all modules and types are built *)
	ctx.delays := !delays :: !(ctx.delays);
	m.mimports <- List.rev m.mimports;
	m

let parse_module ctx m p =
	let file = (match m with
		| [] , name -> name
		| x :: l , name ->
			let x = (try
				match PMap.find x ctx.com.package_rules with
				| Forbidden -> error ("You can't access the " ^ x ^ " package with current compilation flags") p;
				| Directory d -> d
				with Not_found -> x
			) in
			String.concat "/" (x :: l) ^ "/" ^ name
	) ^ ".hx" in
	let file = (try Common.find_file ctx.com file with Not_found -> raise (Error (Module_not_found m,p))) in
	let ch = (try open_in_bin file with _ -> error ("Could not open " ^ file) p) in
	let t = Common.timer "parsing" in
	let pack , decls = (try Parser.parse ctx.com (Lexing.from_channel ch) file with e -> close_in ch; t(); raise e) in
	t();
	close_in ch;
	if ctx.com.verbose then print_endline ("Parsed " ^ file);
	if pack <> fst m then begin
		let spack m = if m = [] then "<empty>" else String.concat "." m in
		if p == Ast.null_pos then
			error ("Invalid commandline class : " ^ s_type_path m ^ " should be " ^ s_type_path (pack,snd m)) p
		else
			error ("Invalid package : " ^ spack (fst m) ^ " should be " ^ spack pack) p
	end;
	decls

let load_module ctx m p =
	try
		Hashtbl.find ctx.modules m
	with
		Not_found ->
			let decls = parse_module ctx m p in
			type_module ctx m decls p
