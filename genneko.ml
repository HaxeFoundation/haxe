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
open Nast
open Nxml

let error msg p =
	raise (Typer.Error (Typer.Custom msg,p))

let pos p =
	{
		psource = p.pfile;
		pline = Lexer.get_error_line p;
	}

let null p =
	(EConst Null,p)

let this p =
	(EConst This,p)

let int p n =
	(EConst (Int n),p)

let str p s =
	(EConst (String s),p)

let ident p s =
	let l = String.length s in
	if l > 10 && String.sub s 0 10 = "__dollar__" then
		(EConst (Builtin (String.sub s 10 (l - 10))),p)
	else
		(EConst (Ident s),p)

let field p e f =
	(EField (e,f),p)

let builtin p n =
	(EConst (Builtin n),p)

let call p e el =
	(ECall (e,el),p)

let array p el =
	call p (builtin p "array") el 

let pmap_list f p =
	PMap.fold (fun v acc -> f v :: acc) p []

let gen_type_path p (path,t) =
	match path with
	| [] -> ident p t
	| path :: l ->
		let epath = List.fold_left (fun e path -> field p e path) (ident p path) l in
		field p epath t

let gen_constant p c =
	match c with
	| TInt i -> (try int p (int_of_string i) with _ -> (EConst (Float i),p))
	| TFloat f -> (EConst (Float f),p)
	| TString s -> str p s
	| TBool b -> (EConst (if b then True else False),p)
	| TNull -> null p
	| TThis -> this p 
	| TSuper -> assert false

let op_str op =
	match op with
	| OpAdd -> "+"
	| OpMult -> "*"
	| OpDiv -> "/"
	| OpSub -> "-"
	| OpAssign -> "="
	| OpEq -> "=="
	| OpNotEq -> "!="
	| OpGt -> ">"
	| OpGte -> ">="
	| OpLt -> "<"
	| OpLte -> "<="
	| OpAnd -> "&"
	| OpOr -> "|"
	| OpXor -> "^"
	| OpBoolAnd -> "&&"
	| OpBoolOr -> "||"
	| OpShl -> "<<"
	| OpShr -> ">>"
	| OpUShr -> ">>>"
	| OpMod -> "%"
	| OpPhysEq
	| OpPhysNotEq
	| OpAssignOp _
	| OpInterval -> assert false

let rec gen_binop p op e1 e2 =
	let gen_op str =
		(EBinop (str,gen_expr e1,gen_expr e2),p)
	in
	match op with
	| OpPhysEq -> (EBinop ("==", call p (builtin p "pcompare") [gen_expr e1; gen_expr e2], int p 0),p)
	| OpPhysNotEq ->  (EBinop ("!=", call p (builtin p "pcompare") [gen_expr e1; gen_expr e2], int p 0),p)
	| OpAssignOp op -> gen_op (op_str op ^ "=")
	| OpInterval -> assert false (* handled by typer *)
	| _ -> gen_op (op_str op)

and gen_unop p op flag e =	
	match op with
	| Increment -> (EBinop ((if flag = Prefix then "+=" else "++="), gen_expr e , int p 1),p)
	| Decrement -> (EBinop ((if flag = Prefix then "-=" else "--="), gen_expr e , int p 1),p)
	| Not -> call p (builtin p "not") [gen_expr e]
	| Neg -> (EBinop ("-",int p 0, gen_expr e),p)
	| NegBits -> error "Operation not available" e.epos

and gen_expr e = 
	let p = pos e.epos in
	match e.eexpr with
	| TConst c ->
		gen_constant p c
	| TLocal s ->
		ident p s
	| TMember s ->
		field p (this p) s
	| TEnumField (e,f) ->
		field p (gen_type_path p e.e_path) f
	| TArray (e1,e2) ->
		(EArray (gen_expr e1,gen_expr e2),p)
	| TBinop (op,e1,e2) ->
		gen_binop p op e1 e2
	| TField (e,f) ->
		field p (gen_expr e) f
	| TType t ->
		(match t with
		| TClassDecl c -> gen_type_path p c.cl_path
		| TEnumDecl e -> gen_type_path p e.e_path)
	| TParenthesis e ->
		(EParenthesis (gen_expr e),p)
	| TObjectDecl fl ->
		(EObject (List.map (fun (f,e) -> f , gen_expr e) fl),p)
	| TArrayDecl el ->
		array p (List.map gen_expr el)
	| TCall (e,el) ->
		call p (gen_expr e) (List.map gen_expr el)
	| TNew (c,_,params) ->
		call p (field p (gen_type_path p c.cl_path) "new") (List.map gen_expr params)
	| TUnop (op,flag,e) ->
		gen_unop p op flag e
	| TVars vl ->
		(EVars (List.map (fun (v,_,e) -> v , (match e with None -> None | Some e -> Some (gen_expr e))) vl),p)
	| TFunction f ->
		(EFunction (List.map fst f.tf_args, gen_expr f.tf_expr),p)
	| TBlock el ->
		(EBlock (List.map gen_expr el), p)
	| TFor (v, it, e) ->
		(EBlock 
			[(EVars ["@tmp", Some (gen_expr it)],p);
			(EWhile (call p (field p (ident p "@tmp") "hasNext") [],
				(ENext 
					((EVars ["n", Some (call p (field p (ident p "@tmp") "next") [])],p),
					gen_expr e
				),p)
			,NormalWhile),p)]
		,p)	
	| TIf (cond,e1,e2) ->
		(EIf (gen_expr cond,gen_expr e1,(match e2 with None -> None | Some e -> Some (gen_expr e))),p)
	| TWhile (econd,e,flag) ->
		(EWhile (gen_expr econd, gen_expr e, match flag with Ast.NormalWhile -> NormalWhile | Ast.DoWhile -> DoWhile),p)
	| TTry (e,catchs) ->
		let catchs = null p in
		(ETry (gen_expr e,"@tmp",catchs),p)
	| TReturn eo ->
		(EReturn (match eo with None -> None | Some e -> Some (gen_expr e)),p)
	| TBreak ->
		(EBreak None,p)
	| TContinue ->
		(EContinue,p)
	| TThrow e ->
		call p (builtin p "throw") [gen_expr e]
	| TMatch _ ->
		assert false
	| TSwitch (e,cases,eo) ->
		null p

let gen_static_method c =
	match c.cf_expr with
	| None -> assert false
	| Some e ->
		c.cf_name , (match e.eexpr with
			| TFunction _ -> gen_expr e
			| _ -> null (pos e.epos)
		)

let gen_class p c =	
	let estat = (EBinop ("=",
		gen_type_path null_pos p,
		(EObject (pmap_list gen_static_method c.cl_statics),null_pos)
	),null_pos) in
	estat

let gen_enum_constr c =
	let p = pos c.ef_pos in
	c.ef_name , (match follow c.ef_type with
		| TFun (params,_) -> 
			let pcount = ref 0 in
			let params = List.map (fun _ -> incr pcount; "p" ^ string_of_int (!pcount)) params in
			(EFunction (params,array p (str p c.ef_name :: List.map (ident p) params)),p)
		| _ ->
			array p [str p c.ef_name]
	)

let gen_enum p e =
	(EBinop ("=",
		gen_type_path null_pos p,
		(EObject (pmap_list gen_enum_constr e.e_constrs),null_pos)
	),null_pos)

let gen_type (p,t) =
	match t with
	| TClassDecl c -> 
		if c.cl_extern then
			null null_pos
		else
			gen_class p c
	| TEnumDecl e -> 
		gen_enum p e

let gen_static_vars (_,t) =
	match t with
	| TEnumDecl _ -> []
	| TClassDecl c ->
		if c.cl_extern then
			[]
		else
			PMap.fold (fun f acc ->
				match f.cf_expr with
				| None -> acc
				| Some e ->
					match e.eexpr with
					| TFunction _ -> acc
					| _ -> 
						let p = pos e.epos in
						(EBinop ("=",
							(field p (gen_type_path p c.cl_path) f.cf_name),
							gen_expr e
						),p) :: acc
			) c.cl_statics []

let generate file types =
	let e = (EBlock (List.map gen_type types @ (List.concat (List.map gen_static_vars types))), null_pos) in
	let neko_file = Filename.chop_extension file ^ ".neko" in
	let ch = IO.output_channel (open_out neko_file) in
	Nxml.write ch (Nxml.to_xml e);
	IO.close_out ch
