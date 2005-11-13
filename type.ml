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
 
type module_path = string list * string

type t = 
	| TMono of t option ref
	| TEnum of tenum * t list
	| TInst of tclass * t list
	| TFun of t list * t
	| TAnon of (string, tclass_field) PMap.t
	| TDynamic of t

and tconstant =
	| TInt of string
	| TFloat of string
	| TString of string
	| TBool of bool
	| TNull
	| TThis
	| TSuper

and tfunc = {
	tf_args : (string * t) list;
	tf_type : t;
	tf_expr : texpr;
}

and texpr_expr =
	| TConst of tconstant
	| TLocal of string
	| TMember of string
	| TEnumField of tenum * string
	| TArray of texpr * texpr
	| TBinop of Ast.binop * texpr * texpr
	| TField of texpr * string
	| TType of module_type
	| TParenthesis of texpr
	| TObjectDecl of (string * texpr) list
	| TArrayDecl of texpr list
	| TCall of texpr * texpr list
	| TNew of tclass * t list * texpr list
	| TUnop of Ast.unop * Ast.unop_flag * texpr
	| TVars of (string * t * texpr option) list
	| TFunction of tfunc
	| TBlock of texpr list
	| TFor of string * texpr * texpr
	| TIf of texpr * texpr * texpr option
	| TWhile of texpr * texpr * Ast.while_flag
	| TSwitch of texpr * (texpr * texpr) list * texpr option
	| TTry of texpr * (string * t * texpr) list
	| TReturn of texpr option
	| TBreak
	| TContinue
	| TMatch of tenum * string * (string * t) list option

and texpr = {
	eexpr : texpr_expr;
	etype : t;
	epos : Ast.pos;
}

and tclass_field = {
	cf_name : string;
	cf_type : t;
	cf_public : bool;
	mutable cf_expr : texpr option;
}

and tclass = {
	cl_path : module_path;
	mutable cl_native : bool;
	mutable cl_types : (string * t) list;
	mutable cl_super : (tclass * t list) option;
	mutable cl_implements : (tclass * t list) list;
	mutable cl_fields : (string , tclass_field) PMap.t;
	mutable cl_statics : (string, tclass_field) PMap.t;
	mutable cl_dynamic : t option;
}

and tenum_field = {
	ef_name : string;
	ef_type : t;
}

and tenum = {
	e_path : module_path;
	mutable e_types : (string * t) list;
	mutable e_constrs : (string , tenum_field) PMap.t;
}

and module_type = 
	| TClassDecl of tclass 
	| TEnumDecl of tenum

type module_def = {
	mpath : module_path;		
	mtypes : (module_path * module_type) list;
}

let mk e t p = { eexpr = e; etype = t; epos = p }

let mk_mono() = TMono (ref None)

let rec t_dynamic = TDynamic t_dynamic

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
	| TFun ([],t) ->
		"Void -> " ^ s_type ctx t
	| TFun (l,t) ->
		String.concat " -> " (List.map (fun t -> match t with TFun _ -> "(" ^ s_type ctx t ^ ")" | _ -> s_type ctx t) l) ^ " -> " ^ s_type ctx t
	| TAnon fl ->
		let fl = PMap.fold (fun f acc -> (" " ^ f.cf_name ^ " : " ^ s_type ctx f.cf_type) :: acc) fl [] in
		"{" ^ String.concat "," fl ^ " }";
	| TDynamic t2 ->
		"Dynamic" ^ s_type_params ctx (if t == t2 then [] else [t2])

and s_type_params ctx = function
	| [] -> ""
	| l -> "<" ^ String.concat ", " (List.map (s_type ctx) l) ^ ">"

let rec follow t =
	match t with
	| TMono r ->
		(match !r with
		| Some t -> follow t
		| _ -> t)
	| _ -> t

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
		| TEnum (_,tl) -> List.exists loop tl
		| TInst (_,tl) -> List.exists loop tl
		| TFun (tl,t) -> List.exists loop tl || loop t
		| TDynamic t2 ->
			if t == t2 then
				false
			else
				loop t2
		| TAnon fl ->
			try
				PMap.iter (fun _ f -> if loop f.cf_type then raise Exit) fl;
				false
			with
				Exit -> true
	in
	if loop b then
		false
	else begin
		e := Some b;
		true
	end

(* substitute parameters with other types *)
let apply_params cparams params t =
	let rec loop l1 l2 =
		match l1, l2 with
		| [] , [] -> []
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
			TEnum (e,List.map loop tl)
		| TInst (c,tl) ->
			(match tl with
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
			TFun (List.map loop tl,loop r)
		| TAnon fl ->
			TAnon (PMap.map (fun f -> { f with cf_type = loop f.cf_type }) fl)
		| TDynamic t2 ->
			if t == t2 then
				t
			else
				TDynamic (loop t2)
	in
	loop t

let monomorphs eparams t =
	apply_params eparams (List.map (fun _ -> mk_mono()) eparams) t

let rec type_eq a b =
	if a == b then
		true
	else match a , b with
	| TMono t , _ -> (match !t with None -> link t a b | Some t -> type_eq t b)
	| _ , TMono t -> (match !t with None -> link t b a | Some t -> type_eq a t)
	| TEnum (a,tl1) , TEnum (b,tl2) -> a == b && List.for_all2 type_eq tl1 tl2
	| TInst (c1,tl1) , TInst (c2,tl2) -> 
		c1 == c2 && List.for_all2 type_eq tl1 tl2
	| TFun (l1,r1) , TFun (l2,r2) when List.length l1 = List.length l2 ->
		type_eq r1 r2 && List.for_all2 type_eq l1 l2
	| _ , _ ->
		false

(* perform unification with subtyping.
   the first type is always the most down in the class hierarchy
   it's also the one that is pointed by the position.
   It's actually a typecheck of  A :> B where some mutations can happen *)

let rec unify a b =
	if a == b then
		true
	else match a, b with
	| TMono t , _ -> (match !t with None -> link t a b | Some t -> unify t b)
	| _ , TMono t -> (match !t with None -> link t b a | Some t -> unify a t)
	| TEnum (a,tl1) , TEnum (b,tl2) -> a == b && List.for_all2 type_eq tl1 tl2
	| TInst (c1,tl1) , TInst (c2,tl2) ->
		let rec loop c tl =
			if c == c2 then
				List.for_all2 type_eq tl tl2
			else (match c.cl_super with
				| None -> false
				| Some (cs,tls) ->
					loop cs (List.map (apply_params c.cl_types tl) tls)
			) || List.exists (fun (cs,tls) ->
				loop cs (List.map (apply_params c.cl_types tl) tls)
			) c.cl_implements
		in
		loop c1 tl1
	| TFun (l1,r1) , TFun (l2,r2) when List.length l1 = List.length l2 ->
		unify r1 r2 && List.for_all2 unify l2 l1 (* contravariance *)
	| TAnon fl1 , TAnon fl2 ->
		(try
			PMap.iter (fun n f2 ->
				let f1 = PMap.find n fl1 in
				if not (unify f1.cf_type f2.cf_type) then raise Not_found;
			) fl2;
			true
		with
			Not_found -> false)
	| TDynamic t , _ ->
		t == a || (match b with TDynamic t2 -> t2 == b || type_eq t t2 | _ -> false)
	| _ , TDynamic t ->
		t == b || (match a with TDynamic t2 -> t2 == a || type_eq t t2 | _ -> false)
	| _ , _ ->
		false

let rec iter f e =
	match e.eexpr with
	| TConst _
	| TLocal _
	| TMember _
	| TEnumField _
	| TBreak
	| TContinue
	| TMatch _
	| TType _ ->
		()
	| TArray (e1,e2)
	| TBinop (_,e1,e2)
	| TFor (_,e1,e2)
	| TWhile (e1,e2,_) ->
		f e1;
		f e2;
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
	| TTry (e,catches) ->
		f e;
		List.iter (fun (_,_,e) -> f e) catches
	| TReturn eo ->
		(match eo with None -> () | Some e -> f e)
