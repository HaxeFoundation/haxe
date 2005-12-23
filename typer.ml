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
	mutable type_params : (string * t) list;
	(* per-function *)
	mutable curmethod : string;
	mutable in_constructor : bool;
	mutable in_static : bool;
	mutable in_loop : bool;
	mutable ret : t;
	mutable locals : (string, t) PMap.t;
}

(* ---------------------------------------------------------------------- *)
(* TOOLS *)

type error_msg =
	| Module_not_found of module_path
	| Cannot_unify of t * t
	| Custom of string

exception Error of error_msg * pos

let error_msg = function
	| Module_not_found m -> "Class not found : " ^ s_type_path m
	| Cannot_unify (t1,t2) -> 
		let ctx = print_context() in
		s_type ctx t1 ^ " should be " ^ s_type ctx t2
	| Custom s -> s

let error msg p = raise (Error (Custom msg,p))

let load_ref : (context -> module_path -> pos -> module_def) ref = ref (fun _ _ _ -> assert false)

let load ctx m p = (!load_ref) ctx m p

let unify ctx t1 t2 p =
	if not (unify t1 t2) && not ctx.untyped then raise (Error (Cannot_unify (t1,t2),p))

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
				List.find (fun t -> type_path t = tpath) m.mtypes
			with
				Not_found -> error ("Module " ^ s_type_path tpath ^ " does not define type " ^ snd tpath) p

let rec load_normal_type ctx t p allow_no_params =
	try
		if t.tpackage <> [] then raise Not_found;
		let pt = List.assoc t.tname ctx.type_params in
		if t.tparams <> [] then error ("Class type parameter " ^ t.tname ^ " can't have parameters") p;
		pt
	with Not_found ->
		let types , path , f , complex = match load_type_def ctx p (t.tpackage,t.tname) with
			| TClassDecl c -> c.cl_types , c.cl_path , (fun t -> TInst (c,t)) , true
			| TEnumDecl e -> e.e_types , e.e_path , (fun t -> TEnum (e,t)) , false
		in
		if allow_no_params && t.tparams = [] && not complex then
			f (List.map (fun _ -> mk_mono()) types)
		else if path = ([],"Dynamic") then
			match t.tparams with
			| [] -> t_dynamic
			| [t] -> TDynamic (load_type ctx p t)
			| _ -> error "Too many parameters for Dynamic" p 
		else begin
			if List.length types <> List.length t.tparams then error ("Invalid number of type parameters for " ^ s_type_path path) p;
			let params = List.map2 (fun t (_,t2) ->
				let t = load_type ctx p t in
				(match t2 with
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
	| TPNormal t -> load_normal_type ctx t p false
	| TPAnonymous l ->
		let rec loop acc (n,t) =
			let t = load_type ctx p t in
			if PMap.mem n acc then error ("Duplicate field declaration : " ^ n) p;
			PMap.add n {
				cf_name = n;
				cf_type = t;
				cf_public = true;
				cf_expr = None;
				cf_doc = None;
			} acc
		in
		TAnon (List.fold_left loop PMap.empty l)
	| TPFunction (args,r) ->
		match args with
		| [TPNormal { tpackage = []; tparams = []; tname = "Void" }] ->
			TFun ([],load_type ctx p r)
		| _ ->
			TFun (List.map (load_type ctx p) args,load_type ctx p r)

let load_type_opt ctx p t =
	match t with
	| None -> mk_mono()
	| Some t -> load_type ctx p t

let set_heritance ctx c herits p =
	let rec loop = function
		| HExtern | HInterface ->
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
			e_types = [];
			e_constrs = PMap.empty;
			e_doc = None;
		} in
		TEnum (e,[])
	| l ->
		(* build a phantom class *)
		let c = mk_class (fst path @ [snd path],n) p None in
		set_heritance ctx c (List.map (fun t -> HImplements t) l) p;
		let add_field ctypes params _ f =
			let f = { f with cf_type = apply_params ctypes params f.cf_type } in
			c.cl_fields <- PMap.add f.cf_name f c.cl_fields
		in
		List.iter (fun (cl,params) -> 
			PMap.iter (add_field cl.cl_types params) cl.cl_fields
		) c.cl_implements;
		TInst (c,[])
	) in
	n , t

let t_int ctx = load_normal_type ctx { tpackage = []; tname = "Int"; tparams = [] } null_pos false
let t_float ctx = load_normal_type ctx { tpackage = []; tname = "Float"; tparams = [] } null_pos false
let t_bool ctx = load_normal_type ctx { tpackage = []; tname = "Bool"; tparams = [] } null_pos false
let t_string ctx = load_normal_type ctx { tpackage = []; tname = "String"; tparams = [] } null_pos false
let t_void ctx = load_normal_type ctx { tpackage = []; tname = "Void"; tparams = [] } null_pos false

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

let t_array ctx =
	match load_type_def ctx null_pos ([],"Array") with
	| TClassDecl c ->
		if List.length c.cl_types <> 1 then assert false;
		let pt = mk_mono() in
		TInst (c,[pt]) , pt
	| _ ->
		assert false

let t_iterator ctx =
	match load_type_def ctx null_pos ([],"Iterator") with
	| TClassDecl c ->
		if List.length c.cl_types <> 1 then assert false;
		let pt = mk_mono() in
		TInst (c,[pt]) , pt
	| _ ->
		assert false

let rec return_flow e =
	let error() = error "A return is missing here" e.epos in
	match e.eexpr with
	| TReturn _ -> ()
	| TParenthesis e -> 
		return_flow e
	| TBlock el ->
		let rec loop = function
			| [] -> error()
			| [e] -> return_flow e
			| { eexpr = TReturn _ } :: _ -> ()
			| _ :: l -> loop l
		in
		loop el
	| TIf (_,e1,Some e2) ->
		return_flow e1;
		return_flow e2;
	| TWhile ({ eexpr = TConst (TBool true) },e,_) ->
		return_flow e
	| TSwitch (_,cases,Some e) ->
		List.iter (fun (_,e) -> return_flow e) cases;
		return_flow e
	| TTry (e,cases) ->
		return_flow e;
		List.iter (fun (_,_,e) -> return_flow e) cases;
	| _ ->
		error()

(* ---------------------------------------------------------------------- *)
(* PASS 3 : type expression & check structure *)

let rec class_field c i =
	try	
		let f = PMap.find i c.cl_fields in
		f.cf_type , f
	with
		Not_found ->
			match c.cl_super with
			| None -> raise Not_found
			| Some (c,params) ->
				let t , f = class_field c i in
				apply_params c.cl_types params t , f

let type_ident ctx i p =
	try
		(* local loookup *)
		let t = PMap.find i ctx.locals in
		mk (TLocal i) t p
	with Not_found -> try
		(* member variable lookup *)
		if ctx.in_static then raise Not_found;
		let t , _ = class_field ctx.curclass i in
		mk (TMember i) t p
	with Not_found -> try
		(* static variable lookup *)
		let f = PMap.find i ctx.curclass.cl_statics in
		let tt = mk (TType (TClassDecl ctx.curclass)) (mk_mono()) p in
		mk (TField (tt,i)) f.cf_type p
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
		loop ctx.local_types
	with Not_found ->
		if ctx.untyped then mk (TLocal i) t_dynamic p else begin
			if ctx.in_static && PMap.mem i ctx.curclass.cl_fields then error ("Cannot access " ^ i ^ " in static function") p;
			error ("Unknown identifier " ^ i) p 
		end

let type_type ctx tpath p =
	match load_type_def ctx p tpath with
	| TClassDecl c ->
		let fl = (if is_parent c ctx.curclass then 
			c.cl_statics
		else
			(* keep only publics *)
			PMap.fold (fun f acc -> if f.cf_public then PMap.add f.cf_name f acc else acc) c.cl_statics PMap.empty 
		) in
		mk (TType (TClassDecl c)) (TAnon fl) p
	| TEnumDecl e ->
		let fl = PMap.map (fun e -> { cf_name = e.ef_name; cf_public = true; cf_type = e.ef_type; cf_expr = None; cf_doc = None }) e.e_constrs in 
		mk (TType (TEnumDecl e)) (TAnon fl) p

let type_constant ctx c p =
	match c with
	| Int i -> mk (TConst (TInt i)) (t_int ctx) p
	| Float f -> mk (TConst (TFloat f)) (t_float ctx) p 
	| String s -> mk (TConst (TString s)) (t_string ctx) p
	| Ident "true" -> mk (TConst (TBool true)) (t_bool ctx) p
	| Ident "false" -> mk (TConst (TBool false)) (t_bool ctx) p
	| Ident "this" ->
		if not ctx.untyped && ctx.in_static then error "Cannot access this from a static function" p;
		mk (TConst TThis) (TInst (ctx.curclass,List.map snd ctx.curclass.cl_types)) p
	| Ident "super" ->
		let t = (match ctx.curclass.cl_super with
		| None -> error "Current class does not have a superclass" p
		| Some (c,params) -> TInst(c,params)
		) in
		if ctx.in_static then error "Cannot access super from a static function" p;
		mk (TConst TSuper) t p
	| Ident "null" -> mk (TConst TNull) (mk_mono()) p
	| Ident s -> type_ident ctx s p
	| Type s ->
		type_type ctx ([],s) p

let check_assign ctx e =
	match e.eexpr with
	| TLocal _ | TMember _ | TArray _ | TField _ ->
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
	| EConst (Ident name) ->
		let c = constr name in
		(match c.ef_type with
			| TFun (l,_) -> needs (List.length l)
			| TEnum _ -> ()
			| _ -> assert false
		);
		let t = TEnum (enum , params) in
		mk (TMatch (enum,name,None)) t p 
	| ECall ((EConst (Ident name),_),el) ->
		let c = constr name in
		let args = (match c.ef_type with
			| TFun (l,_) -> 
				if List.length l <> List.length el then needs (List.length l);
				List.map (apply_params enum.e_types params) l
			| TEnum _ -> error "This constructor does not take any paramter" p
			| _ -> assert false
		) in
		let idents = List.map2 (fun (e,_) t -> 
			match e with 
			| EConst (Ident name) ->
				ctx.locals <- PMap.add name t ctx.locals;
				name , t
			| _ -> invalid()
		) el args in
		let t = TEnum (enum, params) in
		mk (TMatch (enum,name,Some idents)) t p 
	| _ ->
		invalid()

let type_field ctx t i p =
	let no_field() =
		if ctx.untyped then t_dynamic else error (s_type (print_context()) t ^ " have no field " ^ i) p
	in
	match follow t with
	| TInst (c,params) ->
		let priv = is_parent c ctx.curclass in
		let rec loop c params =
			try	
				let f = PMap.find i c.cl_fields in
				if not f.cf_public && not priv && not ctx.untyped then error ("Cannot access to private field " ^ i) p;
				apply_params c.cl_types params f.cf_type
			with
				Not_found ->
					match c.cl_super with
					| None -> raise Not_found
					| Some (c,params) -> loop c params
		in
		let rec loop_dyn c params =
			match c.cl_dynamic with
			| Some t -> apply_params c.cl_types params t 
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
		t
	| TAnon fl ->
		(try (PMap.find i fl).cf_type with Not_found -> no_field())		
	| t ->
		no_field()

let rec type_binop ctx op e1 e2 p =
	let e1 = type_expr ctx e1 in
	let e2 = type_expr ctx e2 in
	let mk_op t = mk (TBinop (op,e1,e2)) t p in
	let rec loop op =
	match op with
	| OpAdd ->
		let i1 = is_int e1.etype in
		let i2 = is_int e2.etype in
		mk_op (if i1 && i2 then
			t_int ctx
		else if (i1 || is_float e1.etype) && (i2 || is_float e2.etype) then
			t_float ctx
		else
			t_string ctx)
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
			Error (Cannot_unify _,_) -> unify ctx e2.etype e1.etype p);
		mk_op (t_bool ctx)
	| OpBoolAnd
	| OpBoolOr ->
		let b = t_bool ctx in
		unify ctx e1.etype b p;
		unify ctx e2.etype b p;
		mk_op b
	| OpInterval ->
		let i = t_int ctx in
		let t = load_normal_type ctx { tpackage = []; tname = "IntIter"; tparams = [] } p false in
		unify ctx e1.etype i e1.epos;
		unify ctx e2.etype i e2.epos;
		mk (TNew ((match t with TInst (c,[]) -> c | _ -> assert false),[],[e1;e2])) t p
	| OpAssign ->
		unify ctx e2.etype e1.etype p;
		check_assign ctx e1;
		mk_op e1.etype
	| OpAssignOp op ->
		loop op
	in
	loop op

and type_unop ctx op flag e p =
	let e = type_expr ctx e in
	let t = (match op with
	| Not ->
		let b = t_bool ctx in
		unify ctx e.etype b e.epos;
		b
	| Increment
	| Decrement
	| Neg
	| NegBits ->
		if op = Increment || op = Decrement then check_assign ctx e;
		if is_float e.etype then 
			t_float ctx
		else begin
			unify ctx e.etype (t_int ctx) e.epos;
			t_int ctx
		end
	) in
	mk (TUnop (op,flag,e)) t p

and type_switch ctx e cases def need_val p =
	let e = type_expr ctx e in
	let t = (if need_val then mk_mono() else t_void ctx) in
	let rec lookup_enum l =
		match l with
		| [] -> None
		| (ECall ((EConst (Ident name),p),_),_) :: l
		| (EConst (Ident name),p) :: l ->
			(try 
				let e = type_ident ctx name p in
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
	let ecases = ref PMap.empty in
	let cases = List.map (fun (e1,e2) ->
		let locals = ctx.locals in
		let e1 = (match enum with Some e -> type_matching ctx e e1 ecases | None -> type_expr ctx e1) in
		(* this inversion is needed *)
		unify ctx e.etype e1.etype e1.epos; 
		let e2 = type_expr ctx e2 in
		ctx.locals <- locals;
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
	mk (TSwitch (e,cases,def)) t p


and type_expr ctx ?(need_val=true) (e,p) =
	match e with
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
		let locals = ctx.locals in
		let rec loop = function
			| [] -> []
			| [e] -> [type_expr ctx ~need_val e]
			| e :: l -> 
				let e = type_expr ctx ~need_val:false e in
				e :: loop l
		in
		let l = loop l in
		ctx.locals <- locals;
		let rec loop = function
			| [] -> t_void ctx
			| [e] -> e.etype
			| _ :: l -> loop l
		in
		mk (TBlock l) (loop l) p
	| EType (pack,s) ->
		let rec loop (e,p) =
			match e with
			| EField (e,s) -> s :: loop e
			| EConst (Ident i) -> [i]
			| _ -> assert false
		in
		let pack = List.rev (loop pack)	in
		type_type ctx (pack,s) p
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
				cf_public = false;
				cf_expr = None;
				cf_doc = None;
			} in
			((f,e) :: l, PMap.add f cf acc)
		in
		let fields , types = List.fold_left loop ([],PMap.empty) fl in
		mk (TObjectDecl fields) (TAnon types) p
	| EArrayDecl el ->
		let t , pt = t_array ctx in
		let el = List.map (fun e ->
			let e = type_expr ctx e in
			unify ctx e.etype pt e.epos;
			e
		) el in
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
			ctx.locals <- PMap.add v t ctx.locals;
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
				match follow (type_field ctx e1.etype "iterator" e1.epos) with
				| TFun ([],it) as ft ->
					unify ctx it t e1.epos;
					let fe = mk (TField (e1,"iterator")) ft e1.epos in
					mk (TCall (fe,[])) t e1.epos
				| _ ->
					error "The field iterator is not a method" e1.epos
			)
		| _ ->
			unify ctx e1.etype t e1.epos;
			e1
		) in
		let old_loop = ctx.in_loop in
		let old_locals = ctx.locals in
		ctx.locals <- PMap.add i pt ctx.locals;
		ctx.in_loop <- true;
		let e2 = type_expr ctx e2 in
		ctx.in_loop <- old_loop;
		ctx.locals <- old_locals;
		mk (TFor (i,e1,e2)) (t_void ctx) p
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
				Error (Cannot_unify _,_) ->
					unify ctx e2.etype e1.etype p;
					e1.etype
			) in
			mk (TIf (e,e1,Some e2)) t p)
	| EWhile (cond,e,flag) ->
		let old_loop = ctx.in_loop in
		let cond = type_expr ctx cond in
		unify ctx cond.etype (t_bool ctx) cond.epos;
		ctx.in_loop <- true;
		let e = type_expr ctx e in
		ctx.in_loop <- old_loop;
		mk (TWhile (cond,e,flag)) (t_void ctx) p
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
		mk (TReturn e) (t_void ctx) p
	| EBreak ->
		if not ctx.in_loop then error "Break outside loop" p;
		mk TBreak (t_void ctx) p
	| EContinue ->
		if not ctx.in_loop then error "Continue outside loop" p;
		mk TContinue (t_void ctx) p
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
			let locals = ctx.locals in
			ctx.locals <- PMap.add v t ctx.locals;
			let e = type_expr ctx ~need_val e in
			ctx.locals <- locals;
			if not need_val then unify ctx e.etype e1.etype e.epos;
			v , t , e
		) catches in
		mk (TTry (e1,catches)) (if not need_val then t_void ctx else e1.etype) p
	| EThrow e ->
		let e = type_expr ctx e in
		mk (TThrow e) (mk_mono()) p
	| ECall ((EConst (Ident "trace"),p),e :: el) ->
		if Hashtbl.mem Parser.defines "notrace" then
			mk (TConst TNull) (t_void ctx) p
		else
		let params = (match el with [] -> [] | _ -> ["customParams",(EArrayDecl el , p)]) in
		let infos = (EObjectDecl (
			("fileName" , (EConst (String (Filename.basename p.pfile)) , p)) ::
			("lineNumber" , (EConst (Int (string_of_int (Lexer.get_error_line p))),p)) ::
			("className" , (EConst (String (s_type_path ctx.curclass.cl_path)),p)) ::
			(if ctx.curmethod = "" then
				params
			else 
				("methodName", (EConst (String ctx.curmethod),p)) :: params)
		) ,p) in
		type_expr ctx (ECall ((EField ((EConst (Type "Log"),p),"trace"),p),[e;EUntyped infos,p]),p)
	| ECall ((EConst (Ident "type"),_),[e]) ->
		let e = type_expr ctx e in
		ctx.warn (s_type (print_context()) e.etype) e.epos;
		e
	| ECall ((EConst (Ident "super"),sp),el) ->
		let el = List.map (type_expr ctx) el in
		if ctx.in_static || not ctx.in_constructor then error "Cannot call superconstructor outside class constructor" p;
		let t = (match ctx.curclass.cl_super with
		| None -> error "Current class does not have a super" p
		| Some (c,params) ->
			let f = (match c.cl_constructor with Some f -> f | None -> error (s_type_path c.cl_path ^ " does not have a constructor") p) in
			(match apply_params c.cl_types params f.cf_type with
			| TFun (args,r) ->
				if List.length args <> List.length el then error "Invalid number of constructor parameters" p;
				List.iter2 (fun e t -> unify ctx e.etype t e.epos) el args;
			| _ ->
				error "Constructor is not a function" p);
			TInst (c,params)
		) in
		mk (TCall (mk (TConst TSuper) t sp,el)) (t_void ctx) p
	| ECall (e,el) ->
		let e = type_expr ctx e in
		let el = List.map (type_expr ctx) el in
		let t = (match follow e.etype with
		| TFun (args,r) ->
			if List.length args <> List.length el then error "Invalid number of arguments" p;
			List.iter2 (fun e t ->
				unify ctx e.etype t e.epos;
			) el args;
			r
		| TMono _ ->
			let t = mk_mono() in
			unify ctx (TFun (List.map (fun e -> e.etype) el,t)) e.etype e.epos;
			t
		| t ->
			if t == t_dynamic || ctx.untyped then
				t_dynamic
			else
				error (s_type (print_context()) t ^ " cannot be called") e.epos
		) in
		mk (TCall (e,el)) t p
	| EField (e,i) ->
		let e = type_expr ctx e in
		let t = type_field ctx e.etype i p in
		mk (TField (e,i)) t p
	| ENew (t,el) ->
		let t = load_normal_type ctx t p true in
		let el = List.map (type_expr ctx) el in
		let c , params , t = (match t with
		| TInst (c,params) ->
			let f = (match c.cl_constructor with Some f -> f | None -> error (s_type_path c.cl_path ^ " does not have a constructor") p) in
			if not f.cf_public && not (is_parent c ctx.curclass) && not ctx.untyped then error "Cannot access private constructor" p;
			(match apply_params c.cl_types params f.cf_type with
			| TFun (args,r) ->
				if List.length args <> List.length el then error "Invalid number of constructor parameters" p;
				List.iter2 (fun e t -> unify ctx e.etype t e.epos) el args;
			| _ ->
				error "Constructor is not a function" p);
			c , params , t
		| _ ->
			error (s_type (print_context()) t ^ " cannot be constructed") p
		) in
		mk (TNew (c,params,el)) t p
	| EUnop (op,flag,e) ->
		type_unop ctx op flag e p
	| EFunction f ->
		let rt = load_type_opt ctx p f.f_type in
		let args = List.map (fun (s,t) -> s , load_type_opt ctx p t) f.f_args in
		let ft = TFun (List.map snd args,rt) in
		let e = type_function ctx ft true false f p in
		let f = {
			tf_args = args;
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
			etype = t_dynamic;
			epos = e.epos;
		}

and type_function ctx t static constr f p =
	let locals = ctx.locals in
	let argst , r = (match t with TFun (args,r) -> args, r | _ -> assert false) in
	List.iter2 (fun (n,_) t ->
		ctx.locals <- PMap.add n t ctx.locals;		
	) f.f_args argst;
	let old_ret = ctx.ret in
	let old_static = ctx.in_static in
	let old_constr = ctx.in_constructor in
	ctx.in_static <- static;
	ctx.in_constructor <- constr;
	ctx.ret <- r;
	let e = type_expr ctx f.f_expr in
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
	ctx.locals <- locals;
	ctx.ret <- old_ret;
	ctx.in_static <- old_static;
	ctx.in_constructor <- old_constr;
	e

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
				if not (type_eq false f.cf_type f2.cf_type) then error ("Field " ^ f.cf_name ^ " overload parent class with different or incomplete type") p;
				if f.cf_public <> f2.cf_public then error ("Field " ^ f.cf_name ^ " have different access right than previous one") p; 
			with
				Not_found -> loop c.cl_super f
	in
	PMap.iter (fun _ f -> loop c.cl_super f) c.cl_fields

let check_interfaces c p () =
	List.iter (fun (intf,params) ->
		PMap.iter (fun i f ->
			try
				let t , f2 = class_field c i in
				if f2.cf_public <> f.cf_public then error ("Field " ^ i ^ " have different access than in " ^ s_type_path intf.cl_path) p;
				if not (type_eq false f2.cf_type (apply_params intf.cl_types params f.cf_type)) then error ("Field " ^ i ^ " have different type than in " ^ s_type_path intf.cl_path) p;
			with
				Not_found ->
					error ("Field " ^ i ^ " needed by " ^ s_type_path intf.cl_path ^ " is missing") p
		) intf.cl_fields;
	) c.cl_implements

(* ---------------------------------------------------------------------- *)
(* PASS 1 & 2 : Module and Class Structure *)

let init_class ctx c p types herits fields =
	ctx.type_params <- [];
	c.cl_types <- List.map (type_type_params ctx c.cl_path p) types;
	ctx.type_params <- c.cl_types;
	c.cl_extern <- List.mem HExtern herits;
	c.cl_interface <- List.mem HInterface herits;
	set_heritance ctx c herits p;
	let is_public access =
		if c.cl_extern || c.cl_interface then not (List.mem APrivate access) else List.mem APublic access
	in
	let type_opt p t =
		match t with
		| None when c.cl_extern || c.cl_interface ->
			error "Type required for extern classes and interfaces" p
		| _ ->
			load_type_opt ctx p t
	in
	let loop_cf f p =
		match f with
		| FVar (name,doc,access,t,e) ->
			let t = (match t with
				| None -> 
					if not (List.mem AStatic access) then error ("Type required for member variable " ^ name) p;
					mk_mono()
				| Some t -> load_type ctx p t
			) in
			let cf = {
				cf_name = name;
				cf_doc = doc;
				cf_type = t;
				cf_expr = None;
				cf_public = is_public access;
			} in
			let delay = (match e with 
				| None -> (fun() -> ())
				| Some e -> (fun () ->
					ctx.curclass <- c;
					cf.cf_expr <- Some (type_static_var ctx t e p)
				)
			) in
			List.mem AStatic access, false, cf, delay
		| FFun (name,doc,access,f) ->
			let r = type_opt p f.f_type in
			let args = List.map (fun (name,t) -> name , type_opt p t) f.f_args in
			let t = TFun (List.map snd args,r) in
			let stat = List.mem AStatic access in
			let constr = (name = "new") in
			let cf = {
				cf_name = name;
				cf_doc = doc;
				cf_type = t;
				cf_expr = None;
				cf_public = is_public access;
			} in
			let define_fun() = 
				ctx.curclass <- c;
				ctx.curmethod <- name;
				if !Plugin.verbose then print_endline ("Typing " ^ s_type_path c.cl_path ^ "." ^ name);
				let e = type_function ctx t stat constr f p in
				let f = {
					tf_args = args;
					tf_type = r;
					tf_expr = e;
				} in
				cf.cf_expr <- Some (mk (TFunction f) t p)
			in
			stat, constr, cf , (if c.cl_extern || c.cl_interface then (fun() -> ()) else define_fun)
	in
	let fl = List.map (fun (f,p) ->
		let static , constr, f , delayed = loop_cf f p in
		if constr then begin
			if c.cl_constructor <> None then error "Duplicate constructor" p;
			c.cl_constructor <- Some f;
		end else begin
			if PMap.mem f.cf_name (if static then c.cl_statics else c.cl_fields) then error ("Duplicate class field declaration : " ^ f.cf_name) p;
			if static then
				c.cl_statics <- PMap.add f.cf_name f c.cl_statics
			else
				c.cl_fields <- PMap.add f.cf_name f c.cl_fields;
		end;
		delayed
	) fields in
	(* define an default inherited constructor *)
	(match c.cl_constructor, c.cl_super with
	| None , Some ({ cl_constructor = Some f } as csuper, cparams) ->
		(match follow f.cf_type with
		| TFun (args,r) ->
			let t = f.cf_type in
			let n = ref 0 in
			let args = List.map (fun t -> incr n; "p" ^ string_of_int (!n) , t) args in
			let eargs = List.map (fun (n,t) -> mk (TLocal n) t p) args in
			let func = {
				tf_args = args;
				tf_type = t;
				tf_expr = mk (TCall (mk (TConst TSuper) (TInst (csuper,cparams)) p,eargs)) r p;
			} in
			c.cl_constructor <- Some {
				cf_name = "new";
				cf_type = t;
				cf_doc = None;
				cf_expr = Some (mk (TFunction func) t p);
				cf_public = f.cf_public;
			}
		| _ -> assert false)
	| _ , _ ->
		());
	fl

let type_module ctx m tdecls =
	(* PASS 1 : build module structure - does not load any module or type - should be atomic ! *)
	let decls = ref [] in
	let decl_with_name name p =
		let tpath = (fst m,name) in
		try
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
		| EClass (name,doc,_,_,_) ->
			let path = decl_with_name name p in
			let c = mk_class path p doc in
			decls := TClassDecl c :: !decls
		| EEnum (name,doc,_,_) ->
			let path = decl_with_name name p in
			let e = {
				e_path = path;
				e_pos = p;
				e_doc = doc;
				e_types = [];
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
		std = ctx.std;
		ret = ctx.ret;
		current = m;
		locals = PMap.empty;
		local_types = ctx.std.mtypes @ m.mtypes;
		type_params = [];
		curmethod = "";
		in_constructor = false;
		in_static = false;
		in_loop = false;
		untyped = false;
	} in
	let delays = ref [] in
	List.iter (fun (d,p) ->
		match d with
		| EImport t ->
			let m = load ctx t p in
			ctx.local_types <- ctx.local_types @ m.mtypes
		| EClass (name,_,types,herits,fields) ->
			let c = List.find (fun d -> match d with TClassDecl { cl_path = _ , n } -> n = name | _ -> false) m.mtypes in
			let c = (match c with TClassDecl c -> c | _ -> assert false) in
			delays := !delays @ check_overloading c p :: check_interfaces c p :: init_class ctx c p types herits fields
		| EEnum (name,_,types,constrs) ->
			let e = List.find (fun d -> match d with TEnumDecl { e_path = _ , n } -> n = name | _ -> false) m.mtypes in
			let e = (match e with TEnumDecl e -> e | _ -> assert false) in
			ctx.type_params <- [];
			e.e_types <- List.map (type_type_params ctx e.e_path p) types;
			ctx.type_params <- e.e_types;
			let et = TEnum (e,List.map snd e.e_types) in
			List.iter (fun (c,doc,t,p) ->
				let t = (match t with 
					| [] -> et
					| l -> TFun (List.map (fun (_,t) -> load_type ctx p t) l, et)
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
			let file = (match m with [] , name -> name | l , name -> String.concat "/" l ^ "/" ^ name) ^ ".hx" in
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
		local_types = [];
		type_params = [];
		curmethod = "";
		curclass = mk_class ([],"") null_pos None;
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
		| TMatch (e,_,_) ->
			loop_enum p e
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
				match follow f.cf_type with
				| TFun ([],_) -> ()
				| _ -> error ("Invalid -main : " ^ s_type_path cl ^ " has invalid main function") null_pos
			with
				Not_found -> error ("Invalid -main : " ^ s_type_path cl ^ " does not have static function main") null_pos
		);
		let path = ([],"@Main") in
		let c = mk_class path null_pos None in
		c.cl_statics <- PMap.add "init" {
			cf_name = "init";
			cf_type = mk_mono();
			cf_public = false;
			cf_doc = None;
			cf_expr = Some (mk (TCall (mk (TField (mk (TType t) (mk_mono()) null_pos,"main")) (mk_mono()) null_pos,[])) (mk_mono()) null_pos);
		} c.cl_statics;
		types := TClassDecl c :: !types
	);
	List.rev !types

;;
load_ref := load
