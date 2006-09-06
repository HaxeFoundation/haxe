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

type module_path = string list * string

type field_access =
	| NormalAccess
	| NoAccess
	| MethodAccess of string
	| F9MethodAccess

type variance = Ast.variance

type t =
	| TMono of t option ref
	| TEnum of tenum * tparams
	| TInst of tclass * tparams
	| TType of tdef * tparams
	| TFun of (string * bool * t) list * t
	| TAnon of tanon
	| TDynamic of t
	| TLazy of (unit -> t) ref

and tparams = (variance * t) list

and tconstant =
	| TInt of int32
	| TFloat of string
	| TString of string
	| TBool of bool
	| TNull
	| TThis
	| TSuper

and tfunc = {
	tf_args : (string * bool * t) list;
	tf_type : t;
	tf_expr : texpr;
}

and tanon = {
	mutable a_fields : (string, tclass_field) PMap.t;
	a_open : bool ref;
}

and texpr_expr =
	| TConst of tconstant
	| TLocal of string
	| TEnumField of tenum * string
	| TArray of texpr * texpr
	| TBinop of Ast.binop * texpr * texpr
	| TField of texpr * string
	| TTypeExpr of module_type
	| TParenthesis of texpr
	| TObjectDecl of (string * texpr) list
	| TArrayDecl of texpr list
	| TCall of texpr * texpr list
	| TNew of tclass * tparams * texpr list
	| TUnop of Ast.unop * Ast.unop_flag * texpr
	| TFunction of tfunc
	| TVars of (string * t * texpr option) list
	| TBlock of texpr list
	| TFor of string * texpr * texpr
	| TIf of texpr * texpr * texpr option
	| TWhile of texpr * texpr * Ast.while_flag
	| TSwitch of texpr * (texpr * texpr) list * texpr option
	| TMatch of texpr * (tenum * tparams) * (string * (string option * t) list option * texpr) list * texpr option
	| TTry of texpr * (string * t * texpr) list
	| TReturn of texpr option
	| TBreak
	| TContinue
	| TThrow of texpr

and texpr = {
	eexpr : texpr_expr;
	etype : t;
	epos : Ast.pos;
}

and tclass_field = {
	cf_name : string;
	mutable cf_type : t;
	cf_public : bool;
	cf_doc : Ast.documentation;
	cf_get : field_access;
	cf_set : field_access;
	cf_params : (string * t) list;
	mutable cf_expr : texpr option;
}

and tclass = {
	cl_path : module_path;
	cl_pos : Ast.pos;
	cl_doc : Ast.documentation;
	cl_private : bool;
	mutable cl_extern : bool;
	mutable cl_interface : bool;
	mutable cl_types : (variance * string * t) list;
	mutable cl_super : (tclass * tparams) option;
	mutable cl_implements : (tclass * tparams) list;
	mutable cl_fields : (string , tclass_field) PMap.t;
	mutable cl_statics : (string, tclass_field) PMap.t;
	mutable cl_ordered_statics : tclass_field list;
	mutable cl_dynamic : t option;
	mutable cl_constructor : tclass_field option;
	mutable cl_init : texpr option;
	mutable cl_overrides : string list;
}

and tenum_field = {
	ef_name : string;
	ef_type : t;
	ef_pos : Ast.pos;
	ef_doc : Ast.documentation;
}

and tenum = {
	e_path : module_path;
	e_pos : Ast.pos;
	e_doc : Ast.documentation;
	e_private : bool;
	e_extern : bool;
	mutable e_types : (variance * string * t) list;
	mutable e_constrs : (string , tenum_field) PMap.t;
}

and tdef = {
	t_path : module_path;
	t_pos : Ast.pos;
	t_doc : Ast.documentation;
	t_private : bool;
	t_static : tclass option;
	mutable t_types : (variance * string * t) list;
	mutable t_type : t;
}

and module_type =
	| TClassDecl of tclass
	| TEnumDecl of tenum
	| TTypeDecl of tdef

type module_def = {
	mpath : module_path;
	mtypes : module_type list;
	mutable mimports : (module_def * string option) list;
}

let mk e t p = { eexpr = e; etype = t; epos = p }

let not_opened = ref false

let mk_anon fl = TAnon { a_fields = fl; a_open = not_opened; }

let mk_field name t = {
	cf_name = name;
	cf_type = t;
	cf_doc = None;
	cf_public = true;
	cf_get = NormalAccess;
	cf_set = NormalAccess;
	cf_expr = None;
	cf_params = [];
}

let mk_mono() = TMono (ref None)

let rec t_dynamic = TDynamic t_dynamic

let mk_class path pos doc priv =
	{
		cl_path = path;
		cl_pos = pos;
		cl_doc = doc;
		cl_private = priv;
		cl_extern = false;
		cl_interface = false;
		cl_types = [];
		cl_super = None;
		cl_implements = [];
		cl_fields = PMap.empty;
		cl_ordered_statics = [];
		cl_statics = PMap.empty;
		cl_dynamic = None;
		cl_constructor = None;
		cl_init = None;
		cl_overrides = [];
	}

let null_class = mk_class ([],"") Ast.null_pos None true

let arg_name (name,_,_) = name

let t_private = function
	| TClassDecl c -> c.cl_private
	| TEnumDecl e -> e.e_private
	| TTypeDecl t -> t.t_private

let t_path = function
	| TClassDecl c -> c.cl_path
	| TEnumDecl e -> e.e_path
	| TTypeDecl t -> t.t_path

let print_context() = ref []

let rec s_type ctx t =
	match t with
	| TMono r ->
		(match !r with
		| None -> Printf.sprintf "Unknown<%d>" (try List.assq t (!ctx) with Not_found -> let n = List.length !ctx in ctx := (t,n) :: !ctx; n)
		| Some t -> s_type ctx t)
	| TEnum (e,tl) ->
		Ast.s_type_path e.e_path ^ s_type_params ctx tl
	| TInst (c,tl) ->
		Ast.s_type_path c.cl_path ^ s_type_params ctx tl
	| TType (t,tl) ->
		Ast.s_type_path t.t_path ^ s_type_params ctx tl
	| TFun ([],t) ->
		"Void -> " ^ s_fun ctx t false
	| TFun (l,t) ->
		String.concat " -> " (List.map (fun (s,b,t) ->
			(if b then "?" else "") ^ (if s = "" then "" else s ^ " : ") ^ s_fun ctx t true
		) l) ^ " -> " ^ s_fun ctx t false
	| TAnon a ->
		let fl = PMap.fold (fun f acc -> (" " ^ f.cf_name ^ " : " ^ s_type ctx f.cf_type) :: acc) a.a_fields [] in
		"{" ^ (if !(a.a_open) then "+" else "") ^  String.concat "," fl ^ " }"
	| TDynamic t2 ->
		"Dynamic" ^ s_type_params ctx (if t == t2 then [] else [VNo,t2])
	| TLazy f ->
		s_type ctx (!f())

and s_fun ctx t void =
	match t with
	| TFun _ -> "(" ^ s_type ctx t ^ ")"
	| TEnum ({ e_path = ([],"Void") },[]) when void -> "(" ^ s_type ctx t ^ ")"
	| _ -> s_type ctx t

and s_type_params ctx = function
	| [] -> ""
	| l -> "<" ^ String.concat ", " (List.map (fun (v,t) -> s_var v ^ s_type ctx t) l) ^ ">"

and s_var = function
	| VNo -> ""
	| VCo -> "+"
	| VContra -> "-"
	| VBi -> "*"

let rec is_parent csup c =
	if c == csup then
		true
	else match c.cl_super with
		| None -> false
		| Some (c,_) -> is_parent csup c

let rec link e a b =
	let rec loop t =
		if t == a then
			true
		else match t with
		| TMono t -> (match !t with None -> false | Some t -> loop t)
		| TEnum (e,tl) -> e.e_path = ([],"Protected") || List.exists (fun (_,t) -> loop t) tl
		| TInst (_,tl) | TType (_,tl) -> List.exists (fun (_,t) -> loop t) tl
		| TFun (tl,t) -> List.exists (fun (_,_,t) -> loop t) tl || loop t
		| TDynamic t2 ->
			if t == t2 then
				false
			else
				loop t2
		| TLazy f ->
			loop (!f())
		| TAnon a ->
			try
				PMap.iter (fun _ f -> if loop f.cf_type then raise Exit) a.a_fields;
				false
			with
				Exit -> true
	in
	if loop b then
		false
	else
		match b with
		| TDynamic _ -> true
		| _ -> e := Some b; true

(* substitute parameters with other types *)
let apply_params cparams params t =
	match cparams with
	| [] -> t
	| _ ->
	let rec loop l1 l2 =
		match l1, l2 with
		| [] , [] -> []
		| (a,b,TLazy f) :: l1, _ -> loop ((a,b,(!f)()) :: l1) l2
		| (_,_,t1) :: l1 , (v,t2) :: l2 -> (t1,(v,t2)) :: loop l1 l2
		| _ -> assert false
	in
	let protect() =
		TEnum ({
			e_path = [] , "Protected";
			e_pos = null_pos;
			e_doc = None;
			e_private = false;
			e_extern = true;
			e_types = [];
			e_constrs = PMap.empty;
		},[])
	in
	let subst = loop cparams params in
	let rec loop v t =
		try
			let v2, t = List.assq t subst in
			(match v2 with
			| VCo when v <> VContra -> VBi, protect()
			| VContra when v <> VCo -> VBi, protect()
			| VBi -> VBi, protect()
			| _ -> v2, t)
		with Not_found ->
		match t with
		| TMono r ->
			(match !r with
			| None -> v, t
			| Some t -> loop v t)
		| TEnum (e,tl) ->
			v, (match tl with
			| [] -> t
			| _ -> TEnum (e,List.map (vloop v) tl))
		| TType (t2,tl) ->
			v, (match tl with
			| [] -> t
			| _ -> TType (t2,List.map (vloop v) tl))
		| TInst (c,tl) ->
			v, (match tl with
			| [] ->
				t
			| [mv,TMono r] ->
				(match !r with
				| Some tt when t == tt ->
					(* for dynamic *)
					let pt = mk_mono() in
					let t = TInst (c,[mv,pt]) in
					(match pt with TMono r -> r := Some t | _ -> assert false);
					t
				| _ -> TInst (c,List.map (vloop v) tl))
			| _ ->
				TInst (c,List.map (vloop v) tl))
		| TFun (tl,r) ->
			v, TFun (List.map (fun (s,o,t) -> s, o, snd (loop VCo t)) tl,snd (loop VContra r))
		| TAnon a ->
			v, TAnon {
				a_fields = PMap.map (fun f -> { f with cf_type = snd (loop VCo f.cf_type) }) a.a_fields;
				a_open = a.a_open;
			}
		| TLazy f ->
			let ft = !f() in
			let v , ft2 = loop v ft in
			if ft == ft2 then
				v, t
			else
				v, ft2
		| TDynamic t2 ->
			if t == t2 then
				v, t
			else
				v, TDynamic (snd (loop VNo t2))
	and vloop v (v2,t) =
		(* only use the given variance position if no variance defined by default *)
		let v, t = loop v t in
		(* compute max. restricted variance based on both requested and found *)
		(match v , v2 with
		| _ , VBi | VBi , _ | VCo, VContra | VContra, VCo -> VBi
		| VCo , VCo | VContra , VContra -> v
		| VNo , _ -> v2
		| _ , VNo -> v) , t
	in
	snd (loop VNo t)

let rec follow t =
	match t with
	| TMono r ->
		(match !r with
		| Some t -> follow t
		| _ -> t)
	| TLazy f ->
		follow (!f())
	| TType (t,tl) ->
		follow (apply_params t.t_types tl t.t_type)
	| _ -> t

let monomorphs eparams t =
	apply_params eparams (List.map (fun (v,_,_) -> v , mk_mono()) eparams) t

let rec fast_eq a b =
	if a == b then
		true
	else match a , b with
	| TFun (l1,r1) , TFun (l2,r2) ->
		List.for_all2 (fun (_,_,t1) (_,_,t2) -> fast_eq t1 t2) l1 l2 && fast_eq r1 r2
	| TType (t1,l1), TType (t2,l2) ->
		t1 == t2 && List.for_all2 fast_peq l1 l2
	| TEnum (e1,l1), TEnum (e2,l2) ->
		e1 == e2 && List.for_all2 fast_peq l1 l2
	| TInst (c1,l1), TInst (c2,l2) ->
		c1 == c2 && List.for_all2 fast_peq l1 l2
	| _ , _ ->
		false

and fast_peq (_,a) (_,b) =
	fast_eq a b

let eq_stack = ref []

let rec type_eq param a b =
	if a == b || (param && b == t_dynamic) then
		true
	else match a , b with
	| TLazy f , _ -> type_eq param (!f()) b
	| _ , TLazy f -> type_eq param a (!f())
	| TMono t , _ -> (match !t with None -> link t a b | Some t -> type_eq param t b)
	| _ , TMono t -> (match !t with None -> link t b a | Some t -> type_eq param a t)
	| TType (t,tl) , _ -> type_eq param (apply_params t.t_types tl t.t_type) b
	| _ , TType (t,tl) ->
		if List.exists (fun (a2,b2) -> fast_eq a a2 && fast_eq b b2) (!eq_stack) then
			true
		else begin
			eq_stack := (a,b) :: !eq_stack;
			let r = type_eq param a (apply_params t.t_types tl t.t_type) in
			eq_stack := List.tl !eq_stack;
			r
		end
	| TEnum (a,tl1) , TEnum (b,tl2) -> a == b && List.for_all2 (type_peq param) tl1 tl2
	| TInst (c1,tl1) , TInst (c2,tl2) ->
		c1 == c2 && List.for_all2 (type_peq param) tl1 tl2
	| TFun (l1,r1) , TFun (l2,r2) when List.length l1 = List.length l2 ->
		type_eq param r1 r2 && List.for_all2 (fun (_,o1,t1) (_,o2,t2) -> o1 = o2 && type_eq param t1 t2) l1 l2
	| TDynamic a , TDynamic b ->
		type_eq param a b
	| TAnon a1, TAnon a2 ->
		(try
			PMap.iter (fun _ f1 ->
				try
					let f2 = PMap.find f1.cf_name a2.a_fields in
					if not (type_eq param f1.cf_type f2.cf_type) then raise Exit;
					if f1.cf_get <> f2.cf_get || f1.cf_set <> f2.cf_set then raise Exit;
				with
					Not_found ->
						if not !(a2.a_open) then raise Exit;
						a2.a_fields <- PMap.add f1.cf_name f1 a2.a_fields
			) a1.a_fields;
			PMap.iter (fun _ f2 ->
				if not (PMap.mem f2.cf_name a1.a_fields) then begin
					if not !(a1.a_open) then raise Exit;
					a1.a_fields <- PMap.add f2.cf_name f2 a1.a_fields
				end;
			) a2.a_fields;
			true
		with
			Exit -> false)
	| _ , _ ->
		false

and type_peq params (_,a) (_,b) =
	type_eq params a b


(* perform unification with subtyping.
   the first type is always the most down in the class hierarchy
   it's also the one that is pointed by the position.
   It's actually a typecheck of  A :> B where some mutations can happen *)

type unify_error =
	| Cannot_unify of t * t
	| Invalid_field_type of string
	| Has_no_field of t * string
	| Invalid_access of string * bool
	| Invalid_visibility of string
	| Not_matching_optional

exception Unify_error of unify_error list

let cannot_unify a b = Cannot_unify (a,b)
let invalid_field n = Invalid_field_type n
let invalid_access n get = Invalid_access (n,get)
let invalid_visibility n = Invalid_visibility n
let has_no_field t n = Has_no_field (t,n)
let error l = raise (Unify_error l)

let unify_stack = ref []

let unify_access a1 a2 =
	a1 = a2 || (a1 = NormalAccess && (a2 = NoAccess || a2 = F9MethodAccess))
	|| (a1 = F9MethodAccess && a2 = NormalAccess) (* unsafe, but no inference of prop. set *)

let field_type f =
	match f.cf_params with
	| [] -> f.cf_type
	| l -> monomorphs (List.map (fun (n,t) -> VNo, n, t) l) f.cf_type

let rec class_field c i =
	try
		let f = PMap.find i c.cl_fields in
		field_type f , f
	with Not_found -> try
		let rec loop = function
			| [] ->
				raise Not_found
			| (c,tl) :: l ->
				try
					let t , f = class_field c i in
					apply_params c.cl_types tl t, f
				with
					Not_found -> loop l
		in
		loop c.cl_implements
	with Not_found ->
		match c.cl_super with
		| None ->
			raise Not_found
		| Some (c,tl) ->
			let t , f = class_field c i in
			apply_params c.cl_types tl t , f

let rec unify a b =
	if a == b then
		()
	else match a, b with
	| TLazy f , _ -> unify (!f()) b
	| _ , TLazy f -> unify a (!f())
	| TMono t , _ ->
		(match !t with
		| None -> if not (link t a b) then error [cannot_unify a b]
		| Some t -> unify t b)
	| _ , TMono t ->
		(match !t with
		| None -> if not (link t b a) then error [cannot_unify a b]
		| Some t -> unify a t)
	| TType (t,tl) , _ ->
		(try
			unify (apply_params t.t_types tl t.t_type) b
		with
			Unify_error l -> error (cannot_unify a b :: l))
	| _ , TType (t,tl) ->
		if not (List.exists (fun (a2,b2) -> fast_eq a a2 && fast_eq b b2) (!unify_stack)) then begin
			try
				unify_stack := (a,b) :: !unify_stack;
				unify a (apply_params t.t_types tl t.t_type);
				unify_stack := List.tl !unify_stack;
			with
				Unify_error l ->
					unify_stack := List.tl !unify_stack;
					error (cannot_unify a b :: l)
		end
	| TEnum (ea,tl1) , TEnum (eb,tl2) ->
		if ea != eb then error [cannot_unify a b];
		unify_types a b tl1 tl2
	| TInst (c1,tl1) , TInst (c2,tl2) ->
		let rec loop c tl =
			if c == c2 then begin
				unify_types a b tl tl2;
				true
			end else (match c.cl_super with
				| None -> false
				| Some (cs,tls) ->
					loop cs (List.map (fun (v,t) -> v , apply_params c.cl_types tl t) tls)
			) || List.exists (fun (cs,tls) ->
				loop cs (List.map (fun (v,t) -> v , apply_params c.cl_types tl t) tls)
			) c.cl_implements
		in
		if not (loop c1 tl1) then error [cannot_unify a b]
	| TFun (l1,r1) , TFun (l2,r2) when List.length l1 = List.length l2 ->
		(try
			unify r1 r2;
			List.iter2 (fun (_,o1,t1) (_,o2,t2) ->
				if o1 && not o2 then error [Not_matching_optional];
				unify t1 t2
			) l2 l1 (* contravariance *)
		with
			Unify_error l -> error (cannot_unify a b :: l))
	| TInst (c,tl) , TAnon an ->
		(try
			PMap.iter (fun n f2 ->
				let ft, f1 = (try class_field c n with Not_found -> error [has_no_field a n]) in
				if not (unify_access f1.cf_get f2.cf_get) then error [invalid_access n true];
				if not (unify_access f1.cf_set f2.cf_set) then error [invalid_access n false];
				if f2.cf_public && not f1.cf_public then error [invalid_visibility n];
				try
					unify (apply_params c.cl_types tl ft) f2.cf_type
				with
					Unify_error l -> error (invalid_field n :: l)
			) an.a_fields;
			an.a_open := false;
		with
			Unify_error l -> error (cannot_unify a b :: l))
	| TAnon a1, TAnon a2 ->
		(try
			PMap.iter (fun n f2 ->
			try
				let f1 = PMap.find n a1.a_fields in
				if not (unify_access f1.cf_get f2.cf_get) then error [invalid_access n true];
				if not (unify_access f1.cf_set f2.cf_set) then error [invalid_access n false];
				if f2.cf_public && not f1.cf_public then error [invalid_visibility n];
				try
					unify f1.cf_type f2.cf_type;
				with
					Unify_error l -> error (invalid_field n :: l)
			with
				Not_found ->
					if not !(a1.a_open) then error [has_no_field a n];
					a1.a_fields <- PMap.add n f2 a1.a_fields
			) a2.a_fields;
			a1.a_open := false;
			a2.a_open := false;
		with
			Unify_error l -> error (cannot_unify a b :: l))
	| TDynamic t , _ ->
		if t == a then
			()
		else (match b with
		| TDynamic t2 ->
			if t2 != b && not (type_eq true t t2) then error [cannot_unify a b; cannot_unify t t2];
		| _ ->
			error [cannot_unify a b])
	| _ , TDynamic t ->
		if t == b then
			()
		else (match a with
		| TDynamic t2 ->
			if t2 != a && not (type_eq true t t2) then error [cannot_unify a b; cannot_unify t t2]
		| _ ->
			error [cannot_unify a b])
	| _ , _ ->
		error [cannot_unify a b]

and unify_types a b tl1 tl2 =
	try
		List.iter2 (fun (va,ta) (vb,tb) ->
			(match va, vb with
			| VNo , _
			| VCo , VCo
			| VContra, VContra
			| _ , VBi -> ()
			| _  -> error []
			);
			match vb with
			| VNo -> if not (type_eq true ta tb) then error [cannot_unify ta tb]
			| VCo -> unify ta tb
			| VContra -> unify tb ta
			| VBi -> ()
		) tl1 tl2
	with
		Unify_error l -> error ((cannot_unify a b) :: l)

let rec iter f e =
	match e.eexpr with
	| TConst _
	| TLocal _
	| TEnumField _
	| TBreak
	| TContinue
	| TTypeExpr _ ->
		()
	| TArray (e1,e2)
	| TBinop (_,e1,e2)
	| TFor (_,e1,e2)
	| TWhile (e1,e2,_) ->
		f e1;
		f e2;
	| TThrow e
	| TField (e,_)
	| TParenthesis e
	| TUnop (_,_,e) ->
		f e
	| TArrayDecl el
	| TNew (_,_,el)
	| TBlock el ->
		List.iter f el
	| TObjectDecl fl ->
		List.iter (fun (_,e) -> f e) fl
	| TCall (e,el) ->
		f e;
		List.iter f el
	| TVars vl ->
		List.iter (fun (_,_,e) -> match e with None -> () | Some e -> f e) vl
	| TFunction fu ->
		f fu.tf_expr
	| TIf (e,e1,e2) ->
		f e;
		f e1;
		(match e2 with None -> () | Some e -> f e)
	| TSwitch (e,cases,def) ->
		f e;
		List.iter (fun (e1,e2) -> f e1; f e2) cases;
		(match def with None -> () | Some e -> f e)
	| TMatch (e,_,cases,def) ->
		f e;
		List.iter (fun (_,_,e) -> f e) cases;
		(match def with None -> () | Some e -> f e)
	| TTry (e,catches) ->
		f e;
		List.iter (fun (_,_,e) -> f e) catches
	| TReturn eo ->
		(match eo with None -> () | Some e -> f e)
