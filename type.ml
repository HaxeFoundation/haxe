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
	| ResolveAccess
	| MethodAccess of string
	| MethodCantAccess
	| NeverAccess
	| InlineAccess

type t =
	| TMono of t option ref
	| TEnum of tenum * tparams
	| TInst of tclass * tparams
	| TType of tdef * tparams
	| TFun of (string * bool * t) list * t
	| TAnon of tanon
	| TDynamic of t
	| TLazy of (unit -> t) ref

and tparams = t list

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

and anon_status =
	| Closed
	| Opened
	| Const
	| Statics of tclass
	| EnumStatics of tenum

and tanon = {
	mutable a_fields : (string, tclass_field) PMap.t;
	a_status : anon_status ref;
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
	| TFor of string * t * texpr * texpr
	| TIf of texpr * texpr * texpr option
	| TWhile of texpr * texpr * Ast.while_flag
	| TSwitch of texpr * (texpr list * texpr) list * texpr option
	| TMatch of texpr * (tenum * tparams) * (int list * (string option * t) list option * texpr) list * texpr option
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

and tclass_kind =
	| KNormal
	| KTypeParameter
	| KExtension of tclass * tparams
	| KConstant of tconstant
	| KGeneric
	| KGenericInstance of tclass * tparams

and tclass = {
	cl_path : module_path;
	cl_pos : Ast.pos;
	cl_doc : Ast.documentation;
	cl_private : bool;
	mutable cl_kind : tclass_kind;
	mutable cl_extern : bool;
	mutable cl_interface : bool;
	mutable cl_types : (string * t) list;
	mutable cl_super : (tclass * tparams) option;
	mutable cl_implements : (tclass * tparams) list;
	mutable cl_fields : (string , tclass_field) PMap.t;
	mutable cl_statics : (string, tclass_field) PMap.t;
	mutable cl_ordered_statics : tclass_field list;
	mutable cl_ordered_fields : tclass_field list;
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
	ef_index : int;
}

and tenum = {
	e_path : module_path;
	e_pos : Ast.pos;
	e_doc : Ast.documentation;
	e_private : bool;
	e_extern : bool;
	mutable e_types : (string * t) list;
	mutable e_constrs : (string , tenum_field) PMap.t;
	mutable e_names : string list;
}

and tdef = {
	t_path : module_path;
	t_pos : Ast.pos;
	t_doc : Ast.documentation;
	t_private : bool;
	mutable t_types : (string * t) list;
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

let not_opened = ref Closed
let is_closed a = !(a.a_status) <> Opened
let mk_anon fl = TAnon { a_fields = fl; a_status = not_opened; }

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
		cl_kind = KNormal;
		cl_extern = false;
		cl_interface = false;
		cl_types = [];
		cl_super = None;
		cl_implements = [];
		cl_fields = PMap.empty;
		cl_ordered_statics = [];
		cl_ordered_fields = [];
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
		"{" ^ (if not (is_closed a) then "+" else "") ^  String.concat "," fl ^ " }"
	| TDynamic t2 ->
		"Dynamic" ^ s_type_params ctx (if t == t2 then [] else [t2])
	| TLazy f ->
		s_type ctx (!f())

and s_fun ctx t void =
	match t with
	| TFun _ -> "(" ^ s_type ctx t ^ ")"
	| TEnum ({ e_path = ([],"Void") },[]) when void -> "(" ^ s_type ctx t ^ ")"
	| _ -> s_type ctx t

and s_type_params ctx = function
	| [] -> ""
	| l -> "<" ^ String.concat ", " (List.map (s_type ctx) l) ^ ">"

let rec is_parent csup c =
	if c == csup then
		true
	else match c.cl_super with
		| None -> false
		| Some (c,_) -> is_parent csup c

let rec link e a b =
	(* tell if a is is b *)
	let rec loop t =
		if t == a then
			true
		else match t with
		| TMono t -> (match !t with None -> false | Some t -> loop t)
		| TEnum (e,tl) -> e.e_path = ([],"Protected") || List.exists loop tl
		| TInst (_,tl) | TType (_,tl) -> List.exists loop tl
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
	(* tell if a ~= b *)
	let rec loop2 t =
		if t == a then
			true
		else match t with
		| TMono t -> (match !t with None -> false | Some t -> loop2 t)
		| _ -> false
	in
	if loop b then
		loop2 b
	else
		match b with
		| TDynamic _ -> true
		| _ -> e := Some b; true

let map loop t =
	match t with
	| TMono r ->
		(match !r with
		| None -> t
		| Some t -> loop t) (* erase*)
	| TEnum (_,[]) | TInst (_,[]) | TType (_,[]) ->
		t
	| TEnum (e,tl) ->
		TEnum (e, List.map loop tl)
	| TInst (c,tl) ->
		TInst (c, List.map loop tl)
	| TType (t2,tl) ->
		TType (t2,List.map loop tl)
	| TFun (tl,r) ->
		TFun (List.map (fun (s,o,t) -> s, o, loop t) tl,loop r)
	| TAnon a ->
		TAnon {
			a_fields = PMap.map (fun f -> { f with cf_type = loop f.cf_type }) a.a_fields;
			a_status = a.a_status;
		}
	| TLazy f ->
		let ft = !f() in
		let ft2 = loop ft in
		if ft == ft2 then t else ft2
	| TDynamic t2 ->
		if t == t2 then	t else TDynamic (loop t2)

(* substitute parameters with other types *)
let apply_params cparams params t =
	match cparams with
	| [] -> t
	| _ ->
	let rec loop l1 l2 =
		match l1, l2 with
		| [] , [] -> []
		| (x,TLazy f) :: l1, _ -> loop ((x,(!f)()) :: l1) l2
		| (_,t1) :: l1 , t2 :: l2 -> (t1,t2) :: loop l1 l2
		| _ -> assert false
	in
	let subst = loop cparams params in
	let rec loop t =
		try
			List.assq t subst
		with Not_found ->
		match t with
		| TMono r ->
			(match !r with
			| None -> t
			| Some t -> loop t)
		| TEnum (e,tl) ->
			(match tl with
			| [] -> t
			| _ -> TEnum (e,List.map loop tl))
		| TType (t2,tl) ->
			(match tl with
			| [] -> t
			| _ -> TType (t2,List.map loop tl))
		| TInst (c,tl) ->
			(match tl with
			| [] ->
				t
			| [TMono r] ->
				(match !r with
				| Some tt when t == tt ->
					(* for dynamic *)
					let pt = mk_mono() in
					let t = TInst (c,[pt]) in
					(match pt with TMono r -> r := Some t | _ -> assert false);
					t
				| _ -> TInst (c,List.map loop tl))
			| _ ->
				TInst (c,List.map loop tl))
		| TFun (tl,r) ->
			TFun (List.map (fun (s,o,t) -> s, o, loop t) tl,loop r)
		| TAnon a ->
			TAnon {
				a_fields = PMap.map (fun f -> { f with cf_type = loop f.cf_type }) a.a_fields;
				a_status = a.a_status;
			}
		| TLazy f ->
			let ft = !f() in
			let ft2 = loop ft in
			if ft == ft2 then
				t
			else
				ft2
		| TDynamic t2 ->
			if t == t2 then
				t
			else
				TDynamic (loop t2)
	in
	loop t

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
	apply_params eparams (List.map (fun _ -> mk_mono()) eparams) t

let rec fast_eq a b =
	if a == b then
		true
	else match a , b with
	| TFun (l1,r1) , TFun (l2,r2) ->
		List.for_all2 (fun (_,_,t1) (_,_,t2) -> fast_eq t1 t2) l1 l2 && fast_eq r1 r2
	| TType (t1,l1), TType (t2,l2) ->
		t1 == t2 && List.for_all2 fast_eq l1 l2
	| TEnum (e1,l1), TEnum (e2,l2) ->
		e1 == e2 && List.for_all2 fast_eq l1 l2
	| TInst (c1,l1), TInst (c2,l2) ->
		c1 == c2 && List.for_all2 fast_eq l1 l2
	| _ , _ ->
		false

(* perform unification with subtyping.
   the first type is always the most down in the class hierarchy
   it's also the one that is pointed by the position.
   It's actually a typecheck of  A :> B where some mutations can happen *)

type unify_error =
	| Cannot_unify of t * t
	| Invalid_field_type of string
	| Has_no_field of t * string
	| Has_extra_field of t * string
	| Invalid_access of string * bool * field_access * field_access
	| Invalid_visibility of string
	| Not_matching_optional of string
	| Cant_force_optional

exception Unify_error of unify_error list

let cannot_unify a b = Cannot_unify (a,b)
let invalid_field n = Invalid_field_type n
let invalid_access n get a b = Invalid_access (n,get,a,b)
let invalid_visibility n = Invalid_visibility n
let has_no_field t n = Has_no_field (t,n)
let has_extra_field t n = Has_extra_field (t,n)
let error l = raise (Unify_error l)

let unify_access a1 a2 =
	a1 = a2 || (a1 = NormalAccess && (a2 = NoAccess || a2 = MethodCantAccess))
	|| (a1 = MethodCantAccess && a2 = NoAccess)

let eq_stack = ref []

type eq_kind =
	| EqStrict
	| EqRightDynamic
	| EqBothDynamic

let rec type_eq param a b =
	if a == b then
		()
	else match a , b with
	| TLazy f , _ -> type_eq param (!f()) b
	| _ , TLazy f -> type_eq param a (!f())
	| TMono t , _ ->
		(match !t with
		| None -> if not (link t a b) then error [cannot_unify a b]
		| Some t -> type_eq param t b)
	| _ , TMono t ->
		(match !t with
		| None -> if not (link t b a) then error [cannot_unify a b]
		| Some t -> type_eq param a t)
	| TType (t1,tl1), TType (t2,tl2) when t1 == t2 && List.length tl1 = List.length tl2 ->
		List.iter2 (type_eq param) tl1 tl2
	| TType (t,tl) , _ -> type_eq param (apply_params t.t_types tl t.t_type) b
	| _ , TType (t,tl) ->
		if List.exists (fun (a2,b2) -> fast_eq a a2 && fast_eq b b2) (!eq_stack) then
			()
		else begin
			eq_stack := (a,b) :: !eq_stack;
			try
				type_eq param a (apply_params t.t_types tl t.t_type);
				eq_stack := List.tl !eq_stack;
			with
				Unify_error l ->
					eq_stack := List.tl !eq_stack;
					error (cannot_unify a b :: l)
		end
	| TEnum (e1,tl1) , TEnum (e2,tl2) ->
		if e1 != e2 then error [cannot_unify a b];
		List.iter2 (type_eq param) tl1 tl2
	| TInst (c1,tl1) , TInst (c2,tl2) ->
		if c1 != c2 then error [cannot_unify a b];
		List.iter2 (type_eq param) tl1 tl2
	| TFun (l1,r1) , TFun (l2,r2) when List.length l1 = List.length l2 ->
		(try
			type_eq param r1 r2;
			List.iter2 (fun (n,o1,t1) (_,o2,t2) ->
				if o1 <> o2 then error [Not_matching_optional n];
				type_eq param t1 t2
			) l1 l2
		with
			Unify_error l -> error (cannot_unify a b :: l))
	| TDynamic a , TDynamic b ->
		type_eq param a b
	| TAnon a1, TAnon a2 ->
		(try
			PMap.iter (fun n f1 ->
				try
					let f2 = PMap.find n a2.a_fields in
					if f1.cf_get <> f2.cf_get && (param = EqStrict || not (unify_access f1.cf_get f2.cf_get)) then error [invalid_access n true f1.cf_get f2.cf_get];
					if f1.cf_set <> f2.cf_set && (param = EqStrict || not (unify_access f1.cf_set f2.cf_set)) then error [invalid_access n false f1.cf_set f2.cf_set];
					try
						type_eq param f1.cf_type f2.cf_type
					with
						Unify_error l -> error (invalid_field n :: l)
				with
					Not_found ->
						if is_closed a2 then error [has_no_field b n];
						if not (link (ref None) b f1.cf_type) then error [cannot_unify a b];
						a2.a_fields <- PMap.add n f1 a2.a_fields
			) a1.a_fields;
			PMap.iter (fun n f2 ->
				if not (PMap.mem n a1.a_fields) then begin
					if is_closed a1 then error [has_no_field a n];
					if not (link (ref None) a f2.cf_type) then error [cannot_unify a b];
					a1.a_fields <- PMap.add n f2 a1.a_fields
				end;
			) a2.a_fields;
		with
			Unify_error l -> error (cannot_unify a b :: l))
	| _ , _ ->
		if b == t_dynamic && (param = EqRightDynamic || param = EqBothDynamic) then
			()
		else if a == t_dynamic && param = EqBothDynamic then
			()
		else
			error [cannot_unify a b]

let type_iseq a b =
	try
		type_eq EqStrict a b;
		true
	with
		Unify_error _ -> false

let unify_stack = ref []

let field_type f =
	match f.cf_params with
	| [] -> f.cf_type
	| l -> monomorphs l f.cf_type

let rec class_field c i =
	try
		let f = PMap.find i c.cl_fields in
		field_type f , f
	with Not_found -> try
		match c.cl_super with
		| None ->
			raise Not_found
		| Some (c,tl) ->
			let t , f = class_field c i in
			apply_params c.cl_types tl t , f
	with Not_found ->
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
					loop cs (List.map (apply_params c.cl_types tl) tls)
			) || List.exists (fun (cs,tls) ->
				loop cs (List.map (apply_params c.cl_types tl) tls)
			) c.cl_implements
		in
		if not (loop c1 tl1) then error [cannot_unify a b]
	| TFun (l1,r1) , TFun (l2,r2) when List.length l1 = List.length l2 ->
		(try
			unify r1 r2;
			List.iter2 (fun (_,o1,t1) (_,o2,t2) ->
				if o1 && not o2 then error [Cant_force_optional];
				unify t1 t2
			) l2 l1 (* contravariance *)
		with
			Unify_error l -> error (cannot_unify a b :: l))
	| TInst (c,tl) , TAnon an ->
		(try
			PMap.iter (fun n f2 ->
				let ft, f1 = (try class_field c n with Not_found -> error [has_no_field a n]) in
				if not (unify_access f1.cf_get f2.cf_get) then error [invalid_access n true f1.cf_get f2.cf_get];
				if not (unify_access f1.cf_set f2.cf_set) then error [invalid_access n false f1.cf_set f2.cf_set];
				if f2.cf_public && not f1.cf_public then error [invalid_visibility n];
				try
					unify_with_access (apply_params c.cl_types tl ft) f2
				with
					Unify_error l -> error (invalid_field n :: l)
			) an.a_fields;
			if !(an.a_status) = Opened then an.a_status := Closed;
		with
			Unify_error l -> error (cannot_unify a b :: l))
	| TAnon a1, TAnon a2 ->
		(try
			PMap.iter (fun n f2 ->
			try
				let f1 = PMap.find n a1.a_fields in
				if not (unify_access f1.cf_get f2.cf_get) then error [invalid_access n true f1.cf_get f2.cf_get];
				if not (unify_access f1.cf_set f2.cf_set) then error [invalid_access n false f1.cf_set f2.cf_set];
				if f2.cf_public && not f1.cf_public then error [invalid_visibility n];
				try
					unify_with_access f1.cf_type f2;
				with
					Unify_error l -> error (invalid_field n :: l)
			with
				Not_found ->
					if is_closed a1 then error [has_no_field a n];
					if not (link (ref None) a f2.cf_type) then error [];
					a1.a_fields <- PMap.add n f2 a1.a_fields
			) a2.a_fields;
			(match !(a1.a_status) with
			| Const when not (PMap.is_empty a2.a_fields) ->
				PMap.iter (fun n _ -> if not (PMap.mem n a2.a_fields) then error [has_extra_field a n]) a1.a_fields;
			| Opened ->
				a1.a_status := Closed
			| _ -> ());
			if !(a2.a_status) = Opened then a2.a_status := Closed;
		with
			Unify_error l -> error (cannot_unify a b :: l))
	| TAnon an, TInst ({ cl_path = [],"Class" },[pt]) ->
		(match !(an.a_status) with
		| Statics cl -> unify (TInst (cl,List.map snd cl.cl_types)) pt
		| _ -> error [cannot_unify a b])
	| TAnon an, TInst ({ cl_path = [],"Enum" },[]) ->
		(match !(an.a_status) with
		| EnumStatics _ -> ()
		| _ -> error [cannot_unify a b])
	| TDynamic t , _ ->
		if t == a then
			()
		else (match b with
		| TDynamic t2 ->
			if t2 != b then
				(try
					type_eq EqRightDynamic t t2
				with
					Unify_error l -> error (cannot_unify a b :: l));
		| _ ->
			error [cannot_unify a b])
	| _ , TDynamic t ->
		if t == b then
			()
		else (match a with
		| TDynamic t2 ->
			if t2 != a then
				(try
					type_eq EqRightDynamic t t2
				with
					Unify_error l -> error (cannot_unify a b :: l));
		| _ ->
			error [cannot_unify a b])
	| _ , _ ->
		error [cannot_unify a b]

and unify_types a b tl1 tl2 =
	try
		List.iter2 (type_eq EqRightDynamic) tl1 tl2
	with
		Unify_error l -> error ((cannot_unify a b) :: l)

and unify_with_access t f =
	match f.cf_get, f.cf_set with
	| NoAccess , _ -> unify f.cf_type t
	| _ , NoAccess -> unify t f.cf_type
	| _ , _ -> type_eq EqBothDynamic t f.cf_type

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
	| TFor (_,_,e1,e2)
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
		List.iter (fun (el,e2) -> List.iter f el; f e2) cases;
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
