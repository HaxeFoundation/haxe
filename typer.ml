(*
 *  Haxe Compiler
 *  Copyright (c)2005 Nicolas Cannasse
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

type context = {
	(* shared *)
	types : (module_path, module_path) Hashtbl.t;
	modules : (module_path , module_def) Hashtbl.t;
	delays : (unit -> unit) list list ref;
	warn : string -> pos -> unit;
	mutable std : module_def;
	mutable untyped : bool;
	(* per-module *)
	current : module_def;
	mutable local_types : module_type list;
	(* per-class *)
	mutable curclass : tclass;
	mutable tthis : t;
	mutable type_params : (string * t) list;
	(* per-function *)
	mutable curmethod : string;
	mutable in_constructor : bool;
	mutable in_static : bool;
	mutable in_loop : bool;
	mutable ret : t;
	mutable locals : (string, t) PMap.t;
	mutable locals_map : (string, string) PMap.t;
	mutable locals_map_inv : (string, string) PMap.t;
}

(* ---------------------------------------------------------------------- *)
(* TOOLS *)

type access_kind =
	| AccNo of string
	| AccExpr of texpr
	| AccSet of texpr * string * t * string

type switch_mode =
	| CMatch of (string * (string * t) list option)
	| CExpr of texpr

type error_msg =
	| Module_not_found of module_path
	| Unify of unify_error list
	| Custom of string
	| Protect of error_msg
	| Unknown_ident of string
	| Stack of error_msg * error_msg

exception Error of error_msg * pos

let unify_error_msg ctx = function
	| Cannot_unify (t1,t2) ->
		s_type ctx t1 ^ " should be " ^ s_type ctx t2
	| Invalid_field_type s ->
		"Invalid type for field " ^ s ^ " :"
	| Has_no_field (t,n) ->
		s_type ctx t ^ " has no field " ^ n
	| Invalid_access (f,get) ->
		"Inconsistent " ^ (if get then "getter" else "setter") ^ " for field " ^ f

let rec error_msg = function
	| Module_not_found m -> "Class not found : " ^ s_type_path m
	| Unify l ->
		let ctx = print_context() in
		String.concat "\n" (List.map (unify_error_msg ctx) l)
	| Unknown_ident s -> "Unknown identifier : " ^ s
	| Custom s -> s
	| Stack (m1,m2) -> error_msg m1 ^ "\n" ^ error_msg m2
	| Protect m -> error_msg m

let forbidden_packages = ref []

let error msg p = raise (Error (Custom msg,p))

let load_ref : (context -> module_path -> pos -> module_def) ref = ref (fun _ _ _ -> assert false)
let type_expr_ref = ref (fun _ ?need_val _ -> assert false)

let load ctx m p = (!load_ref) ctx m p

let field_type f =
	match f.cf_params with
	| [] -> f.cf_type 
	| l -> apply_params l (List.map (fun _ -> mk_mono()) l) f.cf_type

let unify ctx t1 t2 p =
	try
		unify t1 t2
	with
		Unify_error l ->
			if not ctx.untyped then raise (Error (Unify l,p))

let save_locals ctx =
	let locals = ctx.locals in
	let map = ctx.locals_map in
	let inv = ctx.locals_map_inv in
	(fun() ->
		ctx.locals <- locals;
		ctx.locals_map <- map;
		ctx.locals_map_inv <- inv;
	)

let add_local ctx v t =
	let rec loop n =
		let nv = (if n = 0 then v else v ^ string_of_int n) in
		if PMap.mem nv ctx.locals || PMap.mem nv ctx.locals_map_inv then
			loop (n+1)
		else begin
			ctx.locals <- PMap.add v t ctx.locals;
			if n <> 0 then begin
				ctx.locals_map <- PMap.add v nv ctx.locals_map;
				ctx.locals_map_inv <- PMap.add nv v ctx.locals_map_inv;
			end;
			nv
		end
	in
	loop 0

let gen_local ctx t =
	let rec loop n =
		let nv = (if n = 0 then "_g" else "_g" ^ string_of_int n) in
		if PMap.mem nv ctx.locals || PMap.mem nv ctx.locals_map_inv then
			loop (n+1)
		else
			nv
	in
	add_local ctx (loop 0) t

let exc_protect f =
	let rec r = ref (fun() ->
		try
			f r
		with
			| Error (Protect _,_) as e -> raise e
			| Error (m,p) -> raise (Error (Protect m,p))
	) in
	r

let mk_infos ctx p params =
	(EObjectDecl (
		("fileName" , (EConst (String (Filename.basename p.pfile)) , p)) ::
		("lineNumber" , (EConst (Int (string_of_int (Lexer.get_error_line p))),p)) ::
		("className" , (EConst (String (s_type_path ctx.curclass.cl_path)),p)) ::
		if ctx.curmethod = "" then
			params
		else
			("methodName", (EConst (String ctx.curmethod),p)) :: params
	) ,p)

let field_access ctx get f t e p =
	match if get then f.cf_get else f.cf_set with
	| NoAccess ->
		(match follow e.etype with
		| TInst (c,_) when is_parent c ctx.curclass ->
			AccExpr (mk (TField (e,f.cf_name)) t p)
		| _ ->
			AccNo f.cf_name)
	| NormalAccess ->
		AccExpr (mk (TField (e,f.cf_name)) t p)
	| MethodAccess m ->
		if get then
			AccExpr (mk (TCall (mk (TField (e,m)) (mk_mono()) p,[])) t p)
		else
			AccSet (e,m,t,f.cf_name)

let acc_get g p =
	match g with
	| AccNo f -> error ("Field " ^ f ^ " cannot be accessed for reading") p
	| AccExpr e -> e
	| AccSet _ -> assert false

(** since load_type is used in PASS2 , it cannot access the structure of a type **)

let load_type_def ctx p tpath =
	let no_pack = fst tpath = [] in
	try
		List.find (fun t ->
			let tp = type_path t in
			tp = tpath || (no_pack && snd tp = snd tpath)
		) ctx.local_types
	with
		Not_found ->
			let tpath, m = (try
				if not no_pack || fst ctx.current.mpath = [] then raise Exit;
				let tpath2 = fst ctx.current.mpath , snd tpath in
				tpath2, load ctx tpath2 p
			with
				| Error (Module_not_found _,p2) when p == p2 -> tpath, load ctx tpath p
				| Exit -> tpath, load ctx tpath p
			) in
			try
				List.find (fun t -> not (t_private t) && type_path t = tpath) m.mtypes
			with
				Not_found -> error ("Module " ^ s_type_path tpath ^ " does not define type " ^ snd tpath) p

let rec load_normal_type ctx t p allow_no_params =
	try
		if t.tpackage <> [] then raise Not_found;
		let pt = List.assoc t.tname ctx.type_params in
		if t.tparams <> [] then error ("Class type parameter " ^ t.tname ^ " can't have parameters") p;
		pt
	with Not_found ->
		let types , path , f = match load_type_def ctx p (t.tpackage,t.tname) with
			| TClassDecl c -> c.cl_types , c.cl_path , (fun t -> TInst (c,t))
			| TEnumDecl e -> e.e_types , e.e_path , (fun t -> TEnum (e,t))
		in
		if allow_no_params && t.tparams = [] then
			f (List.map (fun (name,t) ->
				match follow t with
				| TEnum _ -> mk_mono()
				| _ -> error ("Type parameter " ^ name ^ " need constraint") p
			) types)
		else if path = ([],"Dynamic") then
			match t.tparams with
			| [] -> t_dynamic
			| [t] -> TDynamic (load_type ctx p t)
			| _ -> error "Too many parameters for Dynamic" p
		else begin
			if List.length types <> List.length t.tparams then error ("Invalid number of type parameters for " ^ s_type_path path) p;
			let params = List.map2 (fun t (_,t2) ->
				let t = load_type ctx p t in
				(match follow t2 with
				| TInst (c,[]) ->
					(match c.cl_super with
					| None -> ()
					| Some (c,params) ->
						unify ctx t (TInst (c,params)) p);
					List.iter (fun (i,params) ->
						unify ctx t (TInst (i,params)) p
					) c.cl_implements
				| TEnum (c,[]) -> ()
				| _ -> assert false);
				t
			) t.tparams types in
			f params
		end

and load_type ctx p t =
	match t with
	| TPParent t -> load_type ctx p t
	| TPNormal t -> load_normal_type ctx t p false
	| TPAnonymous l ->
		let rec loop acc (n,t) =
			let t = load_type ctx p t in
			if PMap.mem n acc then error ("Duplicate field declaration : " ^ n) p;
			PMap.add n {
				cf_name = n;
				cf_type = t;
				cf_public = true;
				cf_get = NormalAccess;
				cf_set = NormalAccess;
				cf_params = [];
				cf_expr = None;
				cf_doc = None;
			} acc
		in
		TAnon (List.fold_left loop PMap.empty l,None)
	| TPFunction (args,r) ->
		match args with
		| [TPNormal { tpackage = []; tparams = []; tname = "Void" }] ->
			TFun ([],load_type ctx p r)
		| _ ->
			TFun (List.map (fun t -> "",load_type ctx p t) args,load_type ctx p r)

let load_type_opt ctx p t =
	match t with
	| None -> mk_mono()
	| Some t -> load_type ctx p t

let set_heritance ctx c herits p =
	let rec loop = function
		| HPrivate | HExtern | HInterface ->
			()
		| HExtends t ->
			if c.cl_super <> None then error "Cannot extend several classes" p;
			let t = load_normal_type ctx t p false in
			(match t with
			| TInst (cl,params) ->
				if is_parent c cl then error "Recursive class" p;
				if c.cl_interface then error "Cannot extend an interface" p;
				c.cl_super <- Some (cl,params)
			| _ -> error "Should extend a class" p)
		| HImplements t ->
			let t = load_normal_type ctx t p false in
			(match t with
			| TInst (cl,params) ->
				if is_parent c cl then error "Recursive class" p;
				c.cl_implements <- (cl, params) :: c.cl_implements
			| TDynamic t ->
				if c.cl_dynamic <> None then error "Cannot have several dynamics" p;
				c.cl_dynamic <- Some t
			| _ -> error "Should implement a class" p)
	in
	List.iter loop herits

let type_type_params ctx path p (n,flags) =
	let t = (match flags with
	| [] ->
		(* build a phantom enum *)
		let e = {
			e_path = (fst path @ [snd path],n);
			e_pos = p;
			e_private = true;
			e_types = [];
			e_constrs = PMap.empty;
			e_doc = None;
		} in
		TEnum (e,[])
	| l ->
		(* build a phantom class *)
		let c = mk_class (fst path @ [snd path],n) p None true in
		c.cl_locked <- true;
		let t = TInst (c,[]) in
		let r = exc_protect (fun r ->
			r := (fun _ -> t);
			set_heritance ctx c (List.map (fun t -> HImplements t) l) p;
			t
		) in
		ctx.delays := [(fun () -> ignore(!r()))] :: !(ctx.delays);
		TLazy r
	) in
	n , t

let hide_types ctx =
	let old_locals = ctx.local_types in
	let old_type_params = ctx.type_params in
	ctx.local_types <- (try (Hashtbl.find ctx.modules ([],"StdTypes")).mtypes with Not_found -> assert false);
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

let t_int ctx = load_core_type ctx "Int"
let t_float ctx = load_core_type ctx "Float"
let t_bool ctx = load_core_type ctx "Bool"
let t_void ctx = load_core_type ctx "Void"
let t_string ctx = load_core_type ctx "String"

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

let is_flash_extern t =
	match follow t with
	| TInst (c,_) ->
		(match fst c.cl_path with
		| "flash" :: _ -> c.cl_extern && not (Plugin.defined "check_flash_args")
		| _ -> false)
	| _ -> false

let t_array ctx =
	let show = hide_types ctx in
	match load_type_def ctx null_pos ([],"Array") with
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
	| TClassDecl c ->
		show();
		if List.length c.cl_types <> 1 then assert false;
		let pt = mk_mono() in
		TInst (c,[pt]) , pt
	| _ ->
		assert false

let rec return_flow e =
	let error() = error "A return is missing here" e.epos in
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
	| TSwitch (_,cases,Some e) ->
		List.iter (fun (_,e) -> return_flow e) cases;
		return_flow e
	| TMatch (_,_,cases,def) ->
		List.iter (fun (_,_,e) -> return_flow e) cases;
		(match def with None -> () | Some e -> return_flow e)
	| TTry (e,cases) ->
		return_flow e;
		List.iter (fun (_,_,e) -> return_flow e) cases;
	| _ ->
		error()

(* ---------------------------------------------------------------------- *)
(* PASS 3 : type expression & check structure *)

let unify_call_params ctx t el args p =
	let error flag =
		if flag && is_flash_extern t then
			el (* allow fewer args for flash API only *)
		else
			let argstr = "Function require " ^ (if args = [] then "no argument" else "arguments : " ^ String.concat ", " (List.map fst args)) in
			error ((if flag then "Not enough" else "Too many") ^ " arguments\n" ^ argstr) p;
	in
	let rec loop l l2 =
		match l , l2 with
		| [] , [] ->
			el
		| [] , [(_,t)] ->
			(match follow t with
			| TInst ({ cl_path = ([],"PosInfos") },[]) -> 
				let infos = mk_infos ctx p [] in
				let e = (!type_expr_ref) ctx ~need_val:true infos in
				el @ [e]
			| _ -> error true)
		| [] , _ ->
			error true
		| _ , [] ->
			error false
		| e :: l, (name,t) :: l2 ->
			(try
				unify ctx e.etype t e.epos;
			with
				| Error (Protect _,_) as e -> raise e
				| Error (m,p) -> raise (Error (Stack (m,Custom ("For function argument '" ^ name ^ "'")), p))
			);
			loop l l2
	in
	loop el args

let rec class_field c i =
	try
		let f = PMap.find i c.cl_fields in
		field_type f , f
	with
		Not_found ->
			match c.cl_super with
			| None -> raise Not_found
			| Some (c,params) ->
				let t , f = class_field c i in
				apply_params c.cl_types params t , f

let type_local ctx i p =
	(* local lookup *)
	let t = PMap.find i ctx.locals in
	let i = (try PMap.find i ctx.locals_map with Not_found -> i) in
	mk (TLocal i) t p

let type_ident ctx i p get =
	match i with
	| "true" ->
		if get then
			AccExpr (mk (TConst (TBool true)) (t_bool ctx) p)
		else
			AccNo i
	| "false" ->
		if get then
			AccExpr (mk (TConst (TBool false)) (t_bool ctx) p)
		else
			AccNo i
	| "this" ->
		if not ctx.untyped && ctx.in_static then error "Cannot access this from a static function" p;
		if get then
			AccExpr (mk (TConst TThis) ctx.tthis p)
		else
			AccNo i
	| "super" ->
		let t = (match ctx.curclass.cl_super with
		| None -> error "Current class does not have a superclass" p
		| Some (c,params) -> TInst(c,params)
		) in
		if ctx.in_static then error "Cannot access super from a static function" p;
		if get then
			AccExpr (mk (TConst TSuper) t p)
		else
			AccNo i
	| "null" ->
		if get then
			AccExpr (mk (TConst TNull) (mk_mono()) p)
		else
			AccNo i
	| "here" ->
		let infos = mk_infos ctx p [] in
		let e = (!type_expr_ref) ctx ~need_val:true infos in
		if get then
			AccExpr { e with etype = load_core_type ctx "PosInfos" }
		else
			AccNo i
	| _ ->
	try
		let e = type_local ctx i p in
		AccExpr e
	with Not_found -> try
		(* member variable lookup *)
		if ctx.in_static then raise Not_found;
		let t , f = class_field ctx.curclass i in
		field_access ctx get f t (mk (TConst TThis) ctx.tthis p) p
	with Not_found -> try
		(* static variable lookup *)
		let f = PMap.find i ctx.curclass.cl_statics in
		let tt = mk (TType (TClassDecl ctx.curclass)) (mk_mono()) p in
		field_access ctx get f (field_type f) tt p
	with Not_found -> try
		(* lookup imported *)
		let rec loop l =
			match l with
			| [] -> raise Not_found
			| t :: l ->
				match t with
				| TClassDecl c ->
					loop l
				| TEnumDecl e ->
					try
						let ef = PMap.find i e.e_constrs in
						mk (TEnumField (e,i)) (monomorphs e.e_types ef.ef_type) p
					with
						Not_found -> loop l
		in
		let e = loop ctx.local_types in
		if get then
			AccExpr e
		else
			AccNo i
	with Not_found ->
		if ctx.untyped then
			AccExpr (mk (TLocal i) (mk_mono()) p)
		else begin
			if ctx.in_static && PMap.mem i ctx.curclass.cl_fields then error ("Cannot access " ^ i ^ " in static function") p;
			raise (Error (Unknown_ident i,p))
		end

let type_type ctx tpath p =
	match load_type_def ctx p tpath with
	| TClassDecl c ->
		let pub = is_parent c ctx.curclass in
		let types = List.map (fun (_,t) ->
			match follow t with
			| TEnum _ -> mk_mono()
			| _ -> t
		) c.cl_types in
		let fl = PMap.fold (fun f acc ->
			PMap.add f.cf_name {
				cf_name = f.cf_name;
				cf_public = f.cf_public || pub;
				cf_type = apply_params c.cl_types types f.cf_type;
				cf_get = f.cf_get;
				cf_set = f.cf_set;
				cf_params = f.cf_params;
				cf_doc = None;
				cf_expr = None;
			} acc
		) c.cl_statics PMap.empty in
		mk (TType (TClassDecl c)) (TAnon (fl,Some ("#" ^ s_type_path c.cl_path))) p
	| TEnumDecl e ->
		let types = List.map (fun _ -> mk_mono()) e.e_types in
		let fl = PMap.fold (fun f acc ->
			PMap.add f.ef_name {
				cf_name = f.ef_name;
				cf_public = true;
				cf_type = apply_params e.e_types types f.ef_type;
				cf_get = NormalAccess;
				cf_set = NoAccess;
				cf_doc = None;
				cf_expr = None;
				cf_params = [];
			} acc
		) e.e_constrs PMap.empty in
		mk (TType (TEnumDecl e)) (TAnon (fl,Some ("#" ^ s_type_path e.e_path))) p

let type_constant ctx c p =
	match c with
	| Int i -> mk (TConst (TInt i)) (t_int ctx) p
	| Float f -> mk (TConst (TFloat f)) (t_float ctx) p
	| String s -> mk (TConst (TString s)) (t_string ctx) p
	| Regexp (r,opt) ->
		let str = mk (TConst (TString r)) (t_string ctx) p in
		let opt = mk (TConst (TString opt)) (t_string ctx) p in
		let t = load_core_type ctx "EReg" in
		mk (TNew ((match t with TInst (c,[]) -> c | _ -> assert false),[],[str;opt])) t p
	| Ident _
	| Type _ -> assert false

let check_assign ctx e =
	match e.eexpr with
	| TLocal _ | TArray _ | TField _ ->
		()
	| TType _ when ctx.untyped ->
		()
	| _ ->
		error "Invalid assign" e.epos

let type_matching ctx (enum,params) (e,p) ecases =
	let invalid() = error "Invalid enum matching" p in
	let needs n = error ("This constructor needs " ^ string_of_int n ^ " parameters") p in
	let constr name =
		if PMap.mem name (!ecases) then error "This constructor has already been used" p;
		ecases := PMap.add name () (!ecases);
		try
			PMap.find name enum.e_constrs
		with
			Not_found -> error ("This constructor is not part of the enum " ^ s_type_path enum.e_path) p
	in
	match e with
	| EConst (Ident name) | EConst (Type name) ->
		let c = constr name in
		(match c.ef_type with
			| TFun (l,_) -> needs (List.length l)
			| TEnum _ -> ()
			| _ -> assert false
		);
		(name,None)
	| ECall ((EConst (Ident name),_),el)
	| ECall ((EConst (Type name),_),el) ->
		let c = constr name in
		let args = (match c.ef_type with
			| TFun (l,_) ->
				if List.length l <> List.length el then needs (List.length l);
				List.map (fun (_,t) -> apply_params enum.e_types params t) l
			| TEnum _ -> error "This constructor does not take any paramter" p
			| _ -> assert false
		) in
		let idents = List.map2 (fun (e,_) t ->
			match e with
			| EConst (Ident name) | EConst (Type name) ->
				let name = add_local ctx name t in
				name , t
			| _ -> invalid()
		) el args in
		(name,Some idents)
	| _ ->
		invalid()

let type_field ctx e i p get =
	let no_field() =
		if ctx.untyped then 
			AccExpr (mk (TField (e,i)) (mk_mono()) p)
		else
			error (s_type (print_context()) e.etype ^ " have no field " ^ i) p
	in
	match follow e.etype with
	| TInst (c,params) ->
		let priv = is_parent c ctx.curclass in
		let find i c = 
			try
				let f = PMap.find i c.cl_fields in
				f , field_type f
			with Not_found ->
				let rec loop = function
					| [] -> raise Not_found
					| (c,tl) :: l ->
						try
							let f = PMap.find i c.cl_fields in
							f , apply_params c.cl_types tl (field_type f)
						with
							Not_found -> loop l
				in
				loop c.cl_implements
		in		
		let rec loop c params =
			try
				let f, t = find i c in
				if not f.cf_public && not priv && not ctx.untyped then error ("Cannot access to private field " ^ i) p;
				field_access ctx get f (apply_params c.cl_types params t) e p
			with
				Not_found ->
					match c.cl_super with
					| None -> raise Not_found
					| Some (c,params) -> loop c params
		in
		let rec loop_dyn c params =
			match c.cl_dynamic with
			| Some t ->
				AccExpr (mk (TField (e,i)) (apply_params c.cl_types params t) p)
			| None ->
				match c.cl_super with
				| None -> raise Not_found
				| Some (c,params) -> loop_dyn c params
		in
		(try
			loop c params
		with Not_found -> try
			loop_dyn c params
		with Not_found ->
			no_field())
	| TDynamic t ->
		AccExpr (mk (TField (e,i)) t p)
	| TAnon (fl,_) ->
		(try
			let f = PMap.find i fl in
			if not f.cf_public && not ctx.untyped then error ("Cannot access to private field " ^ i) p;
			field_access ctx get f (field_type f) e p
		with Not_found -> no_field())
	| t ->
		no_field()

type type_class =
	| KInt
	| KFloat
	| KString
	| KUnk
	| KDyn
	| KOther

let classify t =
	match follow t with
	| TInst ({ cl_path = ([],"Int") },[]) -> KInt
	| TInst ({ cl_path = ([],"Float") },[]) -> KFloat
	| TInst ({ cl_path = ([],"String") },[]) -> KString
	| TMono r when !r = None -> KUnk
	| TDynamic _ -> KDyn
	| _ -> KOther

let rec type_binop ctx op e1 e2 p =
	match op with
	| OpAssign ->
		let e1 = type_access ctx (fst e1) (snd e1) false in
		let e2 = type_expr ctx e2 in
		(match e1 with
		| AccNo s -> error ("Cannot access field or identifier " ^ s ^ " for writing") p
		| AccExpr e1 ->
			unify ctx e2.etype e1.etype p;
			check_assign ctx e1;
			(match e1.eexpr , e2.eexpr with
			| TLocal i1 , TLocal i2 
			| TField ({ eexpr = TConst TThis },i1) , TField ({ eexpr = TConst TThis },i2) when i1 = i2 ->
				error "Assigning a value to itself" p
			| _ , _ -> ());
			mk (TBinop (op,e1,e2)) e1.etype p
		| AccSet (e,m,t,_) ->
			unify ctx e2.etype t p;
			mk (TCall (mk (TField (e,m)) (mk_mono()) p,[e2])) t p)
	| OpAssignOp op ->
		(match type_access ctx (fst e1) (snd e1) false with
		| AccNo s -> error ("Cannot access field or identifier " ^ s ^ " for writing") p
		| AccExpr e ->
			let eop = type_binop ctx op e1 e2 p in
			(match eop.eexpr with
			| TBinop (_,_,e2) ->
				unify ctx e2.etype e.etype p;
				check_assign ctx e;
				mk (TBinop (OpAssignOp op,e,e2)) e.etype p;
			| _ ->				
				assert false)
		| AccSet (e,m,t,f) ->
			let l = save_locals ctx in
			let v = gen_local ctx e.etype in
			let ev = mk (TLocal v) e.etype p in
			let get = type_binop ctx op (EField ((EConst (Ident v),p),f),p) e2 p in
			unify ctx get.etype t p;
			l();
			mk (TBlock [
				mk (TVars [v,e.etype,Some e]) (t_void ctx) p;
				mk (TCall (mk (TField (ev,m)) (mk_mono()) p,[get])) t p
			]) t p)
	| _ ->
	let e1 = type_expr ctx e1 in
	let e2 = type_expr ctx e2 in
	let mk_op t = mk (TBinop (op,e1,e2)) t p in
	match op with
	| OpAdd ->
		mk_op (match classify e1.etype, classify e2.etype with
		| KInt , KInt ->
			t_int ctx
		| KFloat , KInt
		| KInt, KFloat
		| KFloat, KFloat ->
			t_float ctx
		| KUnk , KInt
		| KUnk , KFloat
		| KUnk , KString  ->
			unify ctx e1.etype e2.etype e1.epos;
			e1.etype
		| KInt , KUnk
		| KFloat , KUnk
		| KString , KUnk ->
			unify ctx e2.etype e1.etype e2.epos;
			e2.etype
		| _ , KString
		| _ , KDyn ->
			e2.etype
		| KString , _
		| KDyn , _ ->
			e1.etype
		| KUnk , KUnk ->
			let t = t_int ctx in
			unify ctx e1.etype t e1.epos;
			unify ctx e2.etype t e2.epos;
			t
		| KOther, _
		| _ , KOther ->
			let pr = print_context() in
			error ("Cannot add " ^ s_type pr e1.etype ^ " and " ^ s_type pr e2.etype) p
		)
	| OpAnd
	| OpOr
	| OpXor
	| OpShl
	| OpShr
	| OpUShr ->
		let i = t_int ctx in
		unify ctx e1.etype i e1.epos;
		unify ctx e2.etype i e2.epos;
		mk_op i
	| OpMod
	| OpMult
	| OpDiv
	| OpSub ->
		let i = t_int ctx in
		let f1 = is_float e1.etype in
		let f2 = is_float e2.etype in
		if not f1 then unify ctx e1.etype i e1.epos;
		if not f2 then unify ctx e2.etype i e2.epos;
		if op <> OpDiv && not f1 && not f2 then
			mk_op i
		else
			mk_op (t_float ctx)
	| OpEq
	| OpPhysEq
	| OpPhysNotEq
	| OpNotEq
	| OpGt
	| OpGte
	| OpLt
	| OpLte ->
		(try
			unify ctx e1.etype e2.etype p
		with
			Error (Unify _,_) -> unify ctx e2.etype e1.etype p);
		mk_op (t_bool ctx)
	| OpBoolAnd
	| OpBoolOr ->
		let b = t_bool ctx in
		unify ctx e1.etype b p;
		unify ctx e2.etype b p;
		mk_op b
	| OpInterval ->
		let i = t_int ctx in
		let t = load_core_type ctx "IntIter" in
		unify ctx e1.etype i e1.epos;
		unify ctx e2.etype i e2.epos;
		mk (TNew ((match t with TInst (c,[]) -> c | _ -> assert false),[],[e1;e2])) t p
	| OpAssign
	| OpAssignOp _ ->
		assert false

and type_unop ctx op flag e p =
	let set = (op = Increment || op = Decrement) in
	let acc = type_access ctx (fst e) (snd e) (not set) in
	match acc with
	| AccExpr e -> 
		let t = (match op with
		| Not ->
			let b = t_bool ctx in
			unify ctx e.etype b e.epos;
			b
		| Increment
		| Decrement
		| Neg
		| NegBits ->
			if set then check_assign ctx e;
			if is_float e.etype then
				t_float ctx
			else begin
				unify ctx e.etype (t_int ctx) e.epos;
				t_int ctx
			end
		) in
		mk (TUnop (op,flag,e)) t p
	| AccNo s ->
		error ("The field or identifier " ^ s ^ " is not accessible for " ^ (if set then "writing" else "reading")) p
	| AccSet (e,m,t,f) ->
		let l = save_locals ctx in
		let v = gen_local ctx e.etype in
		let v2 = gen_local ctx t in
		let ev = mk (TLocal v) e.etype p in
		let op = (match op with Increment -> OpAdd | Decrement -> OpSub | _ -> assert false) in
		let one = (EConst (Int "1"),p) in
		let get = type_binop ctx op (EField ((EConst (Ident v),p),f),p) one p in
		unify ctx get.etype t p;
		l();
		match flag with
		| Prefix ->
			mk (TBlock [
				mk (TVars [v,e.etype,Some e]) (t_void ctx) p;
				mk (TCall (mk (TField (ev,m)) (mk_mono()) p,[get])) t p
			]) t p
		| Postfix ->
			let ev2 = mk (TLocal v2) t p in
			mk (TBlock [
				mk (TVars [v,e.etype,Some e; v2,t,Some get]) (t_void ctx) p;
				mk (TCall (mk (TField (ev,m)) (mk_mono()) p,[ev2])) t p;
				ev2
			]) t p

and type_switch ctx e cases def need_val p =
	let e = type_expr ctx e in
	let t = (if need_val then mk_mono() else t_void ctx) in
	let rec lookup_enum l =
		match l with
		| [] -> None
		| (ECall ((EConst (Type name),p),_),_) :: l
		| (ECall ((EConst (Ident name),p),_),_) :: l
		| (EConst (Ident name),p) :: l
		| (EConst (Type name),p) :: l ->
			(try
				let e = acc_get (type_ident ctx name p true) p in
				(match e.eexpr with
				| TEnumField (e,_) -> Some (e, List.map (fun _ -> mk_mono()) e.e_types)
				| _ -> None)
			with
				Error (Custom _,_) -> lookup_enum l)
		| _ ->
			None
	in
	let enum = (match follow e.etype with
		| TEnum (e,params) -> Some (e,params)
		| TMono _ -> lookup_enum (List.map fst cases)
		| _ -> None
	) in
	(* does not treat Bool as an matchable enum, since it's a native type *)
	let enum = (match enum with None | Some ({ e_path = ([],"Bool") },_) -> None | _ -> enum) in
	let ecases = ref PMap.empty in
	let cases = List.map (fun (e1,e2) ->
		let locals = save_locals ctx in
		let e1 = (match enum with
		| Some e -> CMatch (type_matching ctx e e1 ecases)
		| None ->
			let e1 = type_expr ctx e1 in
			(* this inversion is needed *)
			unify ctx e.etype e1.etype e1.epos;
			CExpr e1
		) in
		let e2 = type_expr ctx e2 in
		locals();
		if need_val then unify ctx e2.etype t e2.epos;
		(e1,e2)
	) cases in
	let def = (match def with
		| None ->
			(match enum with
			| None -> ()
			| Some (e,_) ->
				let l = PMap.fold (fun c acc ->
					if PMap.mem c.ef_name (!ecases) then acc else c.ef_name :: acc
				) e.e_constrs [] in
				match l with
				| [] -> ()
				| _ -> error ("Some constructors are not matched : " ^ String.concat "," l) p
			);
			None
		| Some e ->
			let e = type_expr ctx e in
			if need_val then unify ctx e.etype t e.epos;
			Some e
	) in
	match enum with
	| None ->
		let exprs (c,e) =
			match c with
			| CExpr c -> c , e
			| _ -> assert false
		in
		mk (TSwitch (e,List.map exprs cases,def)) t p
	| Some enum ->
		let matchs (c,e) =
			match c with
			| CMatch (c,p) -> (c,p,e)
			| _ -> assert false
		in
		mk (TMatch (e,enum,List.map matchs cases,def)) t p

and type_access ctx e p get =
	match e with
	| EConst (Ident s) -> 
		type_ident ctx s p get
	| EConst (Type s) ->
		(try
			let e = type_local ctx s p in
			AccExpr e
		with Not_found -> try
			let e = type_type ctx ([],s) p in
			AccExpr e
		with Error (Module_not_found ([],s2),_) when s = s2 ->
			type_ident ctx s p get)
	| EField _ 
	| EType _ ->
		let fields path e =
			List.fold_left (fun e (f,_,p) ->
				let e = acc_get (e true) p in				
				type_field ctx e f p
			) e path
		in
		let type_path path =
			let rec loop acc path =
				match path with
				| [] ->
					(match List.rev acc with
					| [] -> assert false
					| (name,true,p) :: path -> fields path (type_access ctx (EConst (Type name)) p)
					| (name,false,p) :: path -> fields path (type_access ctx (EConst (Ident name)) p))
				| (_,false,_) as x :: path ->
					loop (x :: acc) path
				| (name,true,p) as x :: path ->
					let pack = List.rev_map (fun (x,_,_) -> x) acc in
					try
						let e = type_type ctx (pack,name) p in
						fields path (fun _ -> AccExpr e)
					with
						Error (Module_not_found m,_) when m = (pack,name) ->
							loop ((List.rev path) @ x :: acc) []
			in
			match path with
			| [] -> assert false
			| (name,_,p) :: pnext ->
				try
					fields pnext (fun _ -> AccExpr (type_local ctx name p))
				with
					Not_found -> loop [] path
		in
		let rec loop acc e =
			match fst e with
			| EField (e,s) ->
				loop ((s,false,p) :: acc) e
			| EType (e,s) ->
				loop ((s,true,p) :: acc) e
			| EConst (Ident i) ->
				type_path ((i,false,p) :: acc)
			| EConst (Type i) ->
				type_path ((i,true,p) :: acc)
			| _ ->
				fields acc (type_access ctx (fst e) (snd e))
		in
		loop [] (e,p) get
	| _ ->
		AccExpr (type_expr ctx (e,p))

and type_expr ctx ?(need_val=true) (e,p) =
	match e with
	| EField _
	| EType _ 
	| EConst (Ident _)
	| EConst (Type _) ->
		acc_get (type_access ctx e p true) p
	| EConst c ->
		type_constant ctx c p
	| EArray (e1,e2) ->
		let e1 = type_expr ctx e1 in
		let e2 = type_expr ctx e2 in
		unify ctx e2.etype (t_int ctx) e2.epos;
		let t , pt = t_array ctx in
		unify ctx e1.etype t e1.epos;
		mk (TArray (e1,e2)) pt p
    | EBinop (op,e1,e2) ->
		type_binop ctx op e1 e2 p
	| EBlock l ->
		let locals = save_locals ctx in
		let rec loop = function
			| [] -> []
			| [e] -> [type_expr ctx ~need_val e]
			| e :: l ->
				let e = type_expr ctx ~need_val:false e in
				e :: loop l
		in
		let l = loop l in
		locals();
		let rec loop = function
			| [] -> t_void ctx
			| [e] -> e.etype
			| _ :: l -> loop l
		in
		mk (TBlock l) (loop l) p
	| EParenthesis e ->
		let e = type_expr ctx ~need_val e in
		mk (TParenthesis e) e.etype p
	| EObjectDecl fl ->
		let rec loop (l,acc) (f,e) =
			if PMap.mem f acc then error ("Duplicate field in object declaration : " ^ f) p;
			let e = type_expr ctx e in
			let cf = {
				cf_name = f;
				cf_type = e.etype;
				cf_public = true;
				cf_get = NormalAccess;
				cf_set = NormalAccess;
				cf_expr = None;
				cf_doc = None;
				cf_params = [];
			} in
			((f,e) :: l, PMap.add f cf acc)
		in
		let fields , types = List.fold_left loop ([],PMap.empty) fl in
		mk (TObjectDecl fields) (TAnon (types,None)) p
	| EArrayDecl el ->
		let t , pt = t_array ctx in
		let dyn = ref ctx.untyped in
		let el = List.map (fun e ->
			let e = type_expr ctx e in
			if not (!dyn) then (try
				unify ctx e.etype pt e.epos;
			with
				Error (Unify _,_) -> dyn := true);
			e
		) el in
		let t = if !dyn then begin
			let t , pt = t_array ctx in
			unify ctx t_dynamic pt p;
			t
		end else t in
		mk (TArrayDecl el) t p
	| EVars vl ->
		let vl = List.map (fun (v,t,e) ->
			let t = load_type_opt ctx p t in
			let e = (match e with
				| None -> None
				| Some e ->
					let e = type_expr ctx e in
					unify ctx e.etype t p;
					Some e
			) in
			let v = add_local ctx v t in
			v , t , e
		) vl in
		mk (TVars vl) (t_void ctx) p
	| EFor (i,e1,e2) ->
		let e1 = type_expr ctx e1 in
		let t, pt = t_iterator ctx in
		let e1 = (match follow e1.etype with
		| TAnon _
		| TInst _ ->
			(try
				unify ctx e1.etype t e1.epos;
				e1
			with _ ->
				let acc = acc_get (type_field ctx e1 "iterator" e1.epos true) e1.epos in
				match follow acc.etype with
				| TFun ([],it) ->
					unify ctx it t e1.epos;
					mk (TCall (acc,[])) t e1.epos
				| _ ->
					error "The field iterator is not a method" e1.epos
			)
		| _ ->
			unify ctx e1.etype t e1.epos;
			e1
		) in
		let old_loop = ctx.in_loop in
		let old_locals = save_locals ctx in
		let i = add_local ctx i pt in
		ctx.in_loop <- true;
		let e = (match e1.eexpr with
		| TNew ({ cl_path = ([],"IntIter") },[],[i1;i2]) ->
			let rec loop e =
				match e.eexpr with
				| TContinue -> raise Exit
				| _ -> iter loop e
			in
			let max = gen_local ctx i2.etype in
			let n = gen_local ctx i1.etype in
			let e2 = type_expr ~need_val:false ctx e2 in
			let has_cont = (try loop e2; false with Exit -> true) in
			let i , block = (if has_cont then begin
				n , [
				mk (TVars [i,i1.etype,Some (mk (TLocal n) i1.etype p)]) (t_void ctx) p;
				mk (TUnop (Increment,Prefix,mk (TLocal n) i1.etype p)) i1.etype p;
				e2
			] end else i , [
				e2;
				mk (TUnop (Increment,Prefix,mk (TLocal i) i1.etype p)) i1.etype p;
			]) in
			let ident = mk (TLocal i) i1.etype p in
			mk (TBlock [
				mk (TVars [i,i1.etype,Some i1;max,i2.etype,Some i2]) (t_void ctx) p;
				mk (TWhile (
					mk (TBinop (OpLt, ident, mk (TLocal max) i2.etype p)) (t_bool ctx) p,
					mk (TBlock block) (t_void ctx) p,
					NormalWhile
				)) (t_void ctx) p;
			]) (t_void ctx) p
		| _ ->
			let e2 = type_expr ~need_val:false ctx e2 in
			mk (TFor (i,e1,e2)) (t_void ctx) p
		) in
		ctx.in_loop <- old_loop;
		old_locals();
		e
	| EIf (e,e1,e2) ->
		let e = type_expr ctx e in
		unify ctx e.etype (t_bool ctx) e.epos;
		let e1 = type_expr ctx ~need_val e1 in
		(match e2 with
		| None -> mk (TIf (e,e1,None)) (t_void ctx) p
		| Some e2 ->
			let e2 = type_expr ctx ~need_val e2 in
			let t = if not need_val then t_void ctx else (try
				unify ctx e1.etype e2.etype p;
				e2.etype
			with
				Error (Unify _,_) ->
					unify ctx e2.etype e1.etype p;
					e1.etype
			) in
			mk (TIf (e,e1,Some e2)) t p)
	| EWhile (cond,e,NormalWhile) ->
		let old_loop = ctx.in_loop in
		let cond = type_expr ctx cond in
		unify ctx cond.etype (t_bool ctx) cond.epos;
		ctx.in_loop <- true;
		let e = type_expr ~need_val:false ctx e in
		ctx.in_loop <- old_loop;
		mk (TWhile (cond,e,NormalWhile)) (t_void ctx) p
	| EWhile (cond,e,DoWhile) ->
		let old_loop = ctx.in_loop in
		ctx.in_loop <- true;
		let e = type_expr ~need_val:false ctx e in
		ctx.in_loop <- old_loop;
		let cond = type_expr ctx cond in
		unify ctx cond.etype (t_bool ctx) cond.epos;
		mk (TWhile (cond,e,DoWhile)) (t_void ctx) p
	| ESwitch (e,cases,def) ->
		type_switch ctx e cases def need_val p
	| EReturn e ->
		let e , t = (match e with
			| None ->
				let v = t_void ctx in
				unify ctx v ctx.ret p;
				None , v
			| Some e ->
				let e = type_expr ctx e in
				unify ctx e.etype ctx.ret e.epos;
				Some e , e.etype
		) in
		mk (TReturn e) (mk_mono()) p
	| EBreak ->
		if not ctx.in_loop then error "Break outside loop" p;
		mk TBreak (mk_mono()) p
	| EContinue ->
		if not ctx.in_loop then error "Continue outside loop" p;
		mk TContinue (mk_mono()) p
	| ETry (e1,catches) ->
		let e1 = type_expr ctx ~need_val e1 in
		let catches = List.map (fun (v,t,e) ->
			let t = load_type ctx (pos e) t in
			(match follow t with
			| TInst (_,params) ->
				List.iter (fun pt ->
					if pt != t_dynamic then error "Catch class parameter must be Dynamic" p;
				) params;
			| TDynamic _ -> ()
			| _ -> error "Catch type must be a class" p);
			let locals = save_locals ctx in
			let v = add_local ctx v t in
			let e = type_expr ctx ~need_val e in
			locals();
			if need_val then unify ctx e.etype e1.etype e.epos;
			v , t , e
		) catches in
		mk (TTry (e1,catches)) (if not need_val then t_void ctx else e1.etype) p
	| EThrow e ->
		let e = type_expr ctx e in
		mk (TThrow e) (mk_mono()) p
	| ECall ((EConst (Ident "trace"),p),e :: el) ->
		if Plugin.defined "notrace" then
			mk (TConst TNull) (t_void ctx) p
		else
		let params = (match el with [] -> [] | _ -> ["customParams",(EArrayDecl el , p)]) in
		let infos = mk_infos ctx p params in
		type_expr ctx (ECall ((EField ((EConst (Type "Log"),p),"trace"),p),[e;EUntyped infos,p]),p)
	| ECall ((EConst (Ident "type"),_),[e]) ->
		let e = type_expr ctx e in
		ctx.warn (s_type (print_context()) e.etype) e.epos;
		e
	| ECall ((EConst (Ident "super"),sp),el) ->
		let el = List.map (type_expr ctx) el in
		if ctx.in_static || not ctx.in_constructor then error "Cannot call superconstructor outside class constructor" p;
		let el, t = (match ctx.curclass.cl_super with
		| None -> error "Current class does not have a super" p
		| Some (c,params) ->
			let f = (match c.cl_constructor with Some f -> f | None -> error (s_type_path c.cl_path ^ " does not have a constructor") p) in
			let el = (match follow (apply_params c.cl_types params (field_type f)) with
			| TFun (args,_) ->
				unify_call_params ctx (TInst (c,[])) el args p;
			| _ ->
				error "Constructor is not a function" p
			) in
			el , TInst (c,params)
		) in
		mk (TCall (mk (TConst TSuper) t sp,el)) (t_void ctx) p
	| ECall (e,el) ->
		let e = type_expr ctx e in
		let el = List.map (type_expr ctx) el in
		let el , t = (match follow e.etype with
		| TFun (args,r) ->
			let el = unify_call_params ctx (match e.eexpr with TField (e,_) -> e.etype | _ -> t_dynamic) el args p in
			el , r
		| TMono _ ->
			let t = mk_mono() in
			unify ctx (TFun (List.map (fun e -> "",e.etype) el,t)) e.etype e.epos;
			el, t
		| t ->
			el, if t == t_dynamic then
				t_dynamic
			else if ctx.untyped then
				mk_mono()
			else
				error (s_type (print_context()) t ^ " cannot be called") e.epos
		) in
		mk (TCall (e,el)) t p
	| ENew (t,el) ->
		let name = (match t.tpackage with [] -> t.tname | x :: _ -> x) in
		if PMap.mem name ctx.locals then error ("Local variable " ^ name ^ " is preventing usage of this class here") p;
		let t = load_normal_type ctx t p true in
		let el = List.map (type_expr ctx) el in
		let el, c , params , t = (match t with
		| TInst (c,params) ->
			let f = (match c.cl_constructor with Some f -> f | None -> error (s_type_path c.cl_path ^ " does not have a constructor") p) in
			if not f.cf_public && not (is_parent c ctx.curclass) && not ctx.untyped then error "Cannot access private constructor" p;
			let el = (match follow (apply_params c.cl_types params (field_type f)) with
			| TFun (args,r) ->
				unify_call_params ctx t el args p
			| _ ->
				error "Constructor is not a function" p
			) in
			el , c , params , t
		| _ ->
			error (s_type (print_context()) t ^ " cannot be constructed") p
		) in
		mk (TNew (c,params,el)) t p
	| EUnop (op,flag,e) ->
		type_unop ctx op flag e p
	| EFunction f ->
		let rt = load_type_opt ctx p f.f_type in
		let args = List.map (fun (s,t) -> s , load_type_opt ctx p t) f.f_args in
		let ft = TFun (args,rt) in
		let e , fargs = type_function ctx ft true false f p in
		let f = {
			tf_args = fargs;
			tf_type = rt;
			tf_expr = e;
		} in
		mk (TFunction f) ft p
	| EUntyped e ->
		let old = ctx.untyped in
		ctx.untyped <- true;
		let e = type_expr ctx e in
		ctx.untyped <- old;
		{
			eexpr = e.eexpr;
			etype = mk_mono();
			epos = e.epos;
		}
	| ECast (e,t) ->
		type_expr ctx (ETry ((EThrow e,p),[
			("e",t,(EConst (Ident "e"),p));
			("e",TPNormal { tpackage = []; tname = "Dynamic"; tparams = [] },(EThrow (EConst (String "Class cast error"),p),p))
		]),p)

and type_function ctx t static constr f p =
	let locals = save_locals ctx in
	let fargs , r = (match t with
		| TFun (args,r) -> List.map (fun (n,t) -> add_local ctx n t, t) args, r
		| _ -> assert false
	) in
	let old_ret = ctx.ret in
	let old_static = ctx.in_static in
	let old_constr = ctx.in_constructor in
	ctx.in_static <- static;
	ctx.in_constructor <- constr;
	ctx.ret <- r;
	let e = type_expr ~need_val:false ctx f.f_expr in
	let rec loop e =
		match e.eexpr with
		| TReturn (Some _) -> raise Exit
		| TFunction _ -> ()
		| _ -> Type.iter loop e
	in
	let have_ret = (try loop e; false with Exit -> true) in
	if have_ret then
		return_flow e
	else
		unify ctx r (t_void ctx) p;
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
	ctx.ret <- old_ret;
	ctx.in_static <- old_static;
	ctx.in_constructor <- old_constr;
	e , fargs

let type_static_var ctx t e p =
	ctx.in_static <- true;
	let e = type_expr ctx e in
	unify ctx e.etype t p;
	e

let check_overloading c p () =
	let rec loop s f =
		match s with
		| None -> ()
		| Some (c,_) ->
			try
				let f2 = PMap.find f.cf_name c.cl_fields in
				if not (type_eq false (field_type f) (field_type f2)) then error ("Field " ^ f.cf_name ^ " overload parent class with different or incomplete type") p;
				if f.cf_public <> f2.cf_public then error ("Field " ^ f.cf_name ^ " has different access right than previous one") p;
			with
				Not_found -> loop c.cl_super f
	in
	PMap.iter (fun _ f -> loop c.cl_super f) c.cl_fields

let check_interfaces c p () =
	List.iter (fun (intf,params) ->
		PMap.iter (fun i f ->
			try
				let t , f2 = class_field c i in
				if f2.cf_public <> f.cf_public || f2.cf_get <> f.cf_get || f2.cf_set <> f.cf_set then error ("Field " ^ i ^ " has different access than in " ^ s_type_path intf.cl_path) p;
				if not (type_eq false (field_type f2) (apply_params intf.cl_types params (field_type f))) then error ("Field " ^ i ^ " has different type than in " ^ s_type_path intf.cl_path) p;
			with
				Not_found ->
					error ("Field " ^ i ^ " needed by " ^ s_type_path intf.cl_path ^ " is missing") p
		) intf.cl_fields;
	) c.cl_implements

(* ---------------------------------------------------------------------- *)
(* PASS 1 & 2 : Module and Class Structure *)

let init_class ctx c p herits fields =
	ctx.type_params <- c.cl_types;
	c.cl_extern <- List.mem HExtern herits;
	c.cl_interface <- List.mem HInterface herits;
	set_heritance ctx c herits p;
	let tthis = TInst (c,List.map snd c.cl_types) in
	let is_public access =
		if c.cl_extern || c.cl_interface then not (List.mem APrivate access) else List.mem APublic access
	in
	let type_opt ctx p t =
		match t with
		| None when c.cl_extern || c.cl_interface ->
			error "Type required for extern classes and interfaces" p
		| _ ->
			load_type_opt ctx p t
	in
	let loop_cf f p =
		match f with
		| FVar (name,doc,access,t,e) ->
			let stat = List.mem AStatic access in
			let t = (match t with
				| None ->
					if not stat then error ("Type required for member variable " ^ name) p;
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
				cf_get = NormalAccess;
				cf_set = NormalAccess;
				cf_expr = None;
				cf_public = is_public access;
				cf_params = [];
			} in
			let delay = (match e with
				| None -> (fun() -> ())
				| Some e ->
					let ctx = { ctx with curclass = c; tthis = tthis } in
					let r = exc_protect (fun r ->
						r := (fun() -> t);
						if !Plugin.verbose then print_endline ("Typing " ^ s_type_path c.cl_path ^ "." ^ name);
						cf.cf_expr <- Some (type_static_var ctx t e p);
						t
					) in
					cf.cf_type <- TLazy r;
					(fun () -> ignore(!r()))
			) in
			List.mem AStatic access, false, cf, delay
		| FFun (name,doc,access,params,f) ->
			let params = List.map (fun (n,flags) -> 
				match flags with
				| [] -> type_type_params ctx c.cl_path p (n,[])
				| _ -> error "This notation is not allowed because it can't be checked" p
			) params in
			let ctx = { ctx with 
				curclass = c;
				curmethod = name;
				tthis = tthis;
				type_params = params @ ctx.type_params;
			} in
			let ret = type_opt ctx p f.f_type in
			let args = List.map (fun (name,t) -> name , type_opt ctx p t) f.f_args in
			let t = TFun (args,ret) in
			let stat = List.mem AStatic access in
			let constr = (name = "new") in
			let cf = {
				cf_name = name;
				cf_doc = doc;
				cf_type = t;
				cf_get = NormalAccess;
				cf_set = NormalAccess;
				cf_expr = None;
				cf_public = is_public access;
				cf_params = params;
			} in
			let r = exc_protect (fun r ->
				r := (fun() -> t);
				if !Plugin.verbose then print_endline ("Typing " ^ s_type_path c.cl_path ^ "." ^ name);
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
				if (c.cl_extern || c.cl_interface) && cf.cf_name <> "__init__" then
					(fun() -> ())
				else begin
					cf.cf_type <- TLazy r;
					(fun() -> ignore((!r)()))
				end
			) in
			stat, constr, cf, delay
		| FProp (name,doc,access,get,set,t) ->
			let ret = load_type ctx p t in
			let check_get = ref (fun() -> ()) in
			let check_set = ref (fun() -> ()) in
			let check_method m t () =
				try
					let t2, _ = class_field c m in
					unify ctx t2 t p;
				with
					| Error (Unify l,_) -> raise (Error (Stack (Custom ("In method " ^ m ^ " required by property " ^ name),Unify l),p))
					| Not_found -> error ("Method " ^ m ^ " required by property " ^ name ^ " is missing") p
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
				| "null" -> NoAccess
				| "dynamic" -> MethodAccess ("set_" ^ name)
				| "default" -> NormalAccess
				| _ ->
					check_set := check_method set (TFun (["",ret],ret));
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
				cf_public = is_public access;
				cf_params = [];
			} in
			List.mem AStatic access, false, cf, (fun() -> (!check_get)(); (!check_set)())
	in
	let fl = List.map (fun (f,p) ->
		let static , constr, f , delayed = loop_cf f p in
		if constr then begin
			if c.cl_constructor <> None then error "Duplicate constructor" p;
			c.cl_constructor <- Some f;
		end else if not static || f.cf_name <> "__init__" then begin
			if PMap.mem f.cf_name (if static then c.cl_statics else c.cl_fields) then error ("Duplicate class field declaration : " ^ f.cf_name) p;
			if static then begin
				c.cl_statics <- PMap.add f.cf_name f c.cl_statics;
				c.cl_ordered_statics <- f :: c.cl_ordered_statics;
			end else
				c.cl_fields <- PMap.add f.cf_name f c.cl_fields;
		end;
		delayed
	) fields in
	c.cl_ordered_statics <- List.rev c.cl_ordered_statics;
	(* define an default inherited constructor *)
	(match c.cl_constructor, c.cl_super with
	| None , Some ({ cl_constructor = Some f } as csuper, cparams) ->
		(match follow (field_type f) with
		| TFun (args,r) ->
			let t = field_type f in
			let n = ref 0 in
			let args = List.map (fun (_,t) -> incr n; "p" ^ string_of_int (!n) , t) args in
			let eargs = List.map (fun (n,t) -> mk (TLocal n) t p) args in
			let func = {
				tf_args = args;
				tf_type = t;
				tf_expr = mk (TCall (mk (TConst TSuper) (TInst (csuper,cparams)) p,eargs)) r p;
			} in
			c.cl_constructor <- Some {
				cf_name = "new";
				cf_type = t;
				cf_get = NormalAccess;
				cf_set = NoAccess;
				cf_doc = None;
				cf_expr = Some (mk (TFunction func) t p);
				cf_public = f.cf_public;
				cf_params = f.cf_params;
			}
		| _ -> assert false)
	| _ , _ ->
		());
	fl

let type_module ctx m tdecls =
	(* PASS 1 : build module structure - does not load any module or type - should be atomic ! *)
	let decls = ref [] in
	let decl_with_name name p priv =
		let tpath = (fst m,name) in
		if priv then begin
			if List.exists (fun t -> tpath = t_path t) (!decls) then error ("Type name " ^ name ^ " is alreday defined in this module") p;
			tpath
		end else try
			let m2 = Hashtbl.find ctx.types tpath in
			error ("Type name " ^ s_type_path tpath ^ " is redefined from module " ^ s_type_path m2) p
		with
			Not_found ->
				Hashtbl.add ctx.types (fst m,name) m;
				tpath
	in
	List.iter (fun (d,p) ->
		match d with
		| EImport _ -> ()
		| EClass (name,doc,_,flags,_) ->
			let priv = List.mem HPrivate flags in
			let path = decl_with_name name p priv in
			let c = mk_class path p doc priv in
			decls := TClassDecl c :: !decls
		| EEnum (name,doc,_,flags,_) ->
			let priv = List.mem EPrivate flags in
			let path = decl_with_name name p priv in
			let e = {
				e_path = path;
				e_pos = p;
				e_doc = doc;
				e_types = [];
				e_private = priv;
				e_constrs = PMap.empty;
			} in
			decls := TEnumDecl e :: !decls
	) tdecls;
	let m = {
		mpath = m;
		mtypes = List.rev !decls;
	} in
	Hashtbl.add ctx.modules m.mpath m;
	(* PASS 2 : build types structure - does not type any expression ! *)
	let ctx = {
		modules = ctx.modules;
		delays = ctx.delays;
		types = ctx.types;
		warn = ctx.warn;
		curclass = ctx.curclass;
		tthis = ctx.tthis;
		std = ctx.std;
		ret = ctx.ret;
		current = m;
		locals = PMap.empty;
		locals_map = PMap.empty;
		locals_map_inv = PMap.empty;
		local_types = ctx.std.mtypes @ m.mtypes;
		type_params = [];
		curmethod = "";
		in_constructor = false;
		in_static = false;
		in_loop = false;
		untyped = false;
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
	(* here is an additional PASS 1 phase, which handle the type parameters declaration, with lazy contraints *)
	List.iter (fun (d,p) ->
		match d with
		| EImport _ -> ()
		| EClass (name,_,types,_,_) ->
			let c = get_class name in
			c.cl_types <- List.map (type_type_params ctx c.cl_path p) types;
		| EEnum (name,_,types,_,_) ->
			let e = get_enum name in
			e.e_types <- List.map (type_type_params ctx e.e_path p) types;
	) tdecls;
	(* back to PASS2 *)
	List.iter (fun (d,p) ->
		match d with
		| EImport t ->
			let m = load ctx t p in
			ctx.local_types <- ctx.local_types @ (List.filter (fun t -> not (t_private t)) m.mtypes)
		| EClass (name,_,_,herits,fields) ->
			let c = get_class name in
			delays := !delays @ check_overloading c p :: check_interfaces c p :: init_class ctx c p herits fields
		| EEnum (name,_,_,_,constrs) ->
			let e = get_enum name in
			ctx.type_params <- e.e_types;
			let et = TEnum (e,List.map snd e.e_types) in
			List.iter (fun (c,doc,t,p) ->
				let t = (match t with
					| [] -> et
					| l -> TFun (List.map (fun (s,t) -> s, load_type ctx p t) l, et)
				) in
				e.e_constrs <- PMap.add c { ef_name = c; ef_type = t; ef_pos = p; ef_doc = doc } e.e_constrs
			) constrs
	) tdecls;
	(* PASS 3 : type checking, delayed until all modules and types are built *)
	ctx.delays := !delays :: !(ctx.delays);
	m

let load ctx m p =
	try
		Hashtbl.find ctx.modules m
	with
		Not_found ->
			let file = (match m with
				| [] , name -> name
				| x :: l , name ->
					if List.mem x (!forbidden_packages) then error ("You can't access the " ^ x ^ " package with current compilation flags") p;
					String.concat "/" (x :: l) ^ "/" ^ name
			) ^ ".hx" in
			let file = (try Plugin.find_file file with Not_found -> raise (Error (Module_not_found m,p))) in
			let ch = (try open_in file with _ -> error ("Could not open " ^ file) p) in
			let pack , decls = (try Parser.parse (Lexing.from_channel ch) file with e -> close_in ch; raise e) in
			close_in ch;
			if !Plugin.verbose then print_endline ("Parsed " ^ file);
			if pack <> fst m then begin
				let spack m = if m = [] then "<empty>" else String.concat "." m in
				if p == Ast.null_pos then
					error ("Invalid commandline class : " ^ s_type_path m ^ " should be " ^ s_type_path (pack,snd m)) p
				else
					error ("Invalid package : " ^ spack (fst m) ^ " should be " ^ spack pack) p
			end;
			type_module ctx m decls

let context warn =
	let empty =	{
		mpath = [] , "";
		mtypes = [];
	} in
	let ctx = {
		modules = Hashtbl.create 0;
		types = Hashtbl.create 0;
		delays = ref [];
		in_constructor = false;
		in_static = false;
		in_loop = false;
		untyped = false;
		ret = mk_mono();
		warn = warn;
		locals = PMap.empty;
		locals_map = PMap.empty;
		locals_map_inv = PMap.empty;
		local_types = [];
		type_params = [];
		curmethod = "";
		curclass = null_class;
		tthis = mk_mono();
		current = empty;
		std = empty;
	} in
	ctx.std <- (try
		load ctx ([],"StdTypes") null_pos
	with
		Error (Module_not_found ([],"StdTypes"),_) ->
			error "Standard library not found" null_pos
	);
	ctx

let rec finalize ctx =
	let delays = List.concat !(ctx.delays) in
	ctx.delays := [];
	match delays with
	| [] -> () (* at last done *)
	| l ->
		List.iter (fun f -> f()) l;
		finalize ctx

type state =
	| Generating
	| Done
	| NotYet

let types ctx main =
	let types = ref [] in
	let states = Hashtbl.create 0 in
	let state p = try Hashtbl.find states p with Not_found -> NotYet in
	let statics = ref PMap.empty in

	let rec loop t =
		let p = type_path t in
		match state p with
		| Done -> ()
		| Generating ->
			prerr_endline ("Warning : maybe loop in static generation of " ^ s_type_path p);
		| NotYet ->
			Hashtbl.add states p Generating;
			(match t with
			| TClassDecl c -> walk_class p c
			| TEnumDecl e -> ());
			Hashtbl.replace states p Done;
			types := t :: !types

    and loop_class p c =
		if c.cl_path <> p then loop (TClassDecl c)

	and loop_enum p e =
		if e.e_path <> p then loop (TEnumDecl e)

	and walk_static_call p c name =
		try
			let f = PMap.find name c.cl_statics in
			match f.cf_expr with
			| None -> ()
			| Some e ->
				if PMap.mem (c.cl_path,name) (!statics) then
					()
				else begin
					statics := PMap.add (c.cl_path,name) () (!statics);
					walk_expr p e;
				end
		with
			Not_found -> ()

	and walk_expr p e =
		match e.eexpr with
		| TType t ->
			(match t with
			| TClassDecl c -> loop_class p c
			| TEnumDecl e -> loop_enum p e)
		| TEnumField (e,_) ->
			loop_enum p e
		| TNew (c,_,_) ->
			iter (walk_expr p) e;
			loop_class p c
		| TMatch (_,(enum,_),_,_) ->
			loop_enum p enum;
			iter (walk_expr p) e
		| TCall (f,_) ->
			iter (walk_expr p) e;
			(* static call for initializing a variable *)
			let rec loop f =
				match f.eexpr with
				| TField ({ eexpr = TType t },name) ->
					(match t with
					| TEnumDecl _ -> ()
					| TClassDecl c -> walk_static_call p c name)
				| _ -> ()
			in
			loop f
		| _ ->
			iter (walk_expr p) e

    and walk_class p c =
		(match c.cl_super with None -> () | Some (c,_) -> loop_class p c);
		List.iter (fun (c,_) -> loop_class p c) c.cl_implements;
		(match c.cl_init with
		| None -> ()
		| Some e -> walk_expr p e);
		PMap.iter (fun _ f ->
			match f.cf_expr with
			| None -> ()
			| Some e ->
				match e.eexpr with
				| TFunction _ -> ()
				| _ -> walk_expr p e
		) c.cl_statics

	in
	Hashtbl.iter (fun _ m -> List.iter loop m.mtypes) ctx.modules;
	(match main with
	| None -> ()
	| Some cl ->
		let t = load_type_def ctx null_pos cl in
		(match t with
		| TEnumDecl _ -> error ("Invalid -main : " ^ s_type_path cl ^ " is not a class") null_pos
		| TClassDecl c ->
			try
				let f = PMap.find "main" c.cl_statics in
				match follow (field_type f) with
				| TFun ([],_) -> ()
				| _ -> error ("Invalid -main : " ^ s_type_path cl ^ " has invalid main function") null_pos
			with
				Not_found -> error ("Invalid -main : " ^ s_type_path cl ^ " does not have static function main") null_pos
		);
		let path = ([],"@Main") in
		let c = mk_class path null_pos None true in
		let f = {
			cf_name = "init";
			cf_type = mk_mono();
			cf_public = false;
			cf_get = NormalAccess;
			cf_set = NormalAccess;
			cf_doc = None;
			cf_params = [];
			cf_expr = Some (mk (TCall (mk (TField (mk (TType t) (mk_mono()) null_pos,"main")) (mk_mono()) null_pos,[])) (mk_mono()) null_pos);
		} in
		c.cl_statics <- PMap.add "init" f c.cl_statics;
		c.cl_ordered_statics <- f :: c.cl_ordered_statics;
		types := TClassDecl c :: !types
	);
	List.rev !types

;;
load_ref := load;
type_expr_ref := type_expr;
