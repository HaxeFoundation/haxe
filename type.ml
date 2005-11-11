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
	| TEnum of module_path
	| TInst of tclass * t list
	| TFun of t list * t

and tconstant =
	| TInt of int
	| TFloat of string
	| TString of string
	| TIdent of string

and texpr_decl =
	| TConst of tconstant

and texpr = {
	edecl : texpr_decl;
	etype : t;
}

and tclass_field = {
	cf_name : string;
	cf_type : t;
	cf_expr : texpr option;
	cf_public : bool;
}

and tclass = {
	cl_module : module_path;
	cl_name : string;
	mutable cl_native : bool;
	mutable cl_types : (string * t) list;
	mutable cl_super : (tclass * t list) option;
	mutable cl_implements : (tclass * t list) list;
	mutable cl_fields : (string , tclass_field) PMap.t;
	mutable cl_statics : (string, tclass_field) PMap.t;
}

type module_type = 
	| TClassDecl of tclass 
	| TEnumDecl

type module_def = {
	mpath : module_path;		
	mtypes : (module_path * module_type) list;
}

let rec type_eq a b =
	if a == b then
		true
	else match a , b with
	| TEnum a , TEnum b -> a = b
	| TInst (c1,tl1) , TInst (c2,tl2) -> 
		c1.cl_name = c2.cl_name && c1.cl_module = c2.cl_module && List.for_all2 type_eq tl1 tl2
	| TFun (l1,r1) , TFun (l2,r2) when List.length l1 = List.length l2 ->
		type_eq r1 r2 && List.for_all2 type_eq l1 l2
	| _ , _ ->
		false
