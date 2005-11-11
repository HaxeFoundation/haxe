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
	| TParameter of module_path * string  

and tconstant =
	| TInt of string
	| TFloat of string
	| TString of string
	| TBool of bool
	| TNull
	| TThis

and tfunc = {
	tf_args : (string * t) list;
	tf_type : t;
	tf_expr : texpr;
}

and texpr_decl =
	| TConst of tconstant
	| TLocal of string
	| TMember of tclass * string
	| TEnumField of tenum * string
	| TStaticField of tclass * string
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

and texpr = {
	edecl : texpr_decl;
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

let mk e t p = { edecl = e; etype = t; epos = p }

let mk_mono() = TMono (ref None)

let print_context() = ref []

let rec s_type ctx t = 
	match t with
	| TMono _ -> 
		Printf.sprintf "'%d" (try List.assq t (!ctx) with Not_found -> let n = List.length !ctx in ctx := (t,n) :: !ctx; n)
	| TEnum (e,tl) ->
		Ast.s_type_path e.e_path ^ s_type_params ctx tl
	| TInst (c,tl) ->
		Ast.s_type_path c.cl_path ^ s_type_params ctx tl
	| TFun ([],t) ->
		"void -> " ^ s_type ctx t
	| TFun (l,t) ->
		String.concat " -> " (List.map (fun t -> match t with TFun _ -> "(" ^ s_type ctx t ^ ")" | _ -> s_type ctx t) l) ^ " -> " ^ s_type ctx t
	| TParameter (p,n) ->
		Ast.s_type_path p ^ "#" ^ n

and s_type_params ctx = function
	| [] -> ""
	| l -> "<" ^ String.concat ", " (List.map (s_type ctx) l) ^ ">"

let rec link e a b =
	let rec loop t =
		if t == a then
			true
		else match t with
		| TMono t -> (match !t with None -> false | Some t -> loop t)
		| TEnum (_,tl) -> List.exists loop tl
		| TInst (_,tl) -> List.exists loop tl
		| TFun (tl,t) -> List.exists loop tl || loop t
		| TParameter (_,_) -> false
	in
	if loop b then
		false
	else begin
		e := Some b;
		true
	end

(* substitute parameters with other types *)
let apply_params cparams params t =
	assert false

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
		if c1 == c2 then
			List.for_all2 type_eq tl1 tl2
		else begin
			assert false
		end
	| TFun (l1,r1) , TFun (l2,r2) when List.length l1 = List.length l2 ->
		unify r1 r2 && List.for_all2 unify l2 l1 (* contravariance *)
	| _ , _ ->
		false
