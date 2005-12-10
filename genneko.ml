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

let nparams l =
	let pcount = ref 0 in
	List.map (fun _ -> incr pcount; "p" ^ string_of_int (!pcount)) l

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
	| TString s -> call p (field p (ident p "String") "new") [str p s]
	| TBool b -> (EConst (if b then True else False),p)
	| TNull -> null p
	| TThis -> this p 
	| TSuper -> assert false

let rec gen_binop p op e1 e2 =
	let gen_op str =
		(EBinop (str,gen_expr e1,gen_expr e2),p)
	in
	match op with
	| OpPhysEq -> (EBinop ("==", call p (builtin p "pcompare") [gen_expr e1; gen_expr e2], int p 0),p)
	| OpPhysNotEq ->  (EBinop ("!=", call p (builtin p "pcompare") [gen_expr e1; gen_expr e2], int p 0),p)
	| _ -> gen_op (Ast.s_binop op)

and gen_unop p op flag e =	
	match op with
	| Increment -> (EBinop ((if flag = Prefix then "+=" else "++="), gen_expr e , int p 1),p)
	| Decrement -> (EBinop ((if flag = Prefix then "-=" else "--="), gen_expr e , int p 1),p)
	| Not -> call p (builtin p "not") [gen_expr e]
	| Neg -> (EBinop ("-",int p 0, gen_expr e),p)
	| NegBits -> error "Operation not available" e.epos

and gen_call p e el =
	match e.eexpr , el with
	| TConst TSuper , _ ->
		let c = (match follow e.etype with TInst (c,_) -> c | _ -> assert false) in
		call p (builtin p "call") [
			field p (gen_type_path p c.cl_path) "__construct__";
			this p;
			array p (List.map gen_expr el)
		]
	| TField ({ eexpr = TConst TSuper; etype = t },f) , _ ->
		let c = (match follow t with TInst (c,_) -> c | _ -> assert false) in
		call p (builtin p "call") [
			field p (gen_type_path p (fst c.cl_path,"@" ^ snd c.cl_path)) f;
			this p;
			array p (List.map gen_expr el)
		]
	| _ , _ ->
		call p (gen_expr e) (List.map gen_expr el)

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
		call p (field p (ident p "Array") "new1") [array p (List.map gen_expr el); int p (List.length el)]
	| TCall (e,el) ->
		gen_call p e el
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
				(EBlock [
					(EVars [v, Some (call p (field p (ident p "@tmp") "next") [])],p);
					gen_expr e
				],p)
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
		try
			let l = List.map (fun (e,e2) -> match e.eexpr with TMatch (_,s,vl) -> (s,vl,e2) | _ -> raise Not_found) cases in
			(ENext (
				(EVars ["@tmp",Some (gen_expr e)],p),
				(ESwitch (
					(EArray (ident p "@tmp",int p 0),p),
					List.map (fun (s,el,e2) ->
						let count = ref 0 in
						let e = match el with
							| None -> gen_expr e2
							| Some el ->
								(EBlock [
									(EVars (List.map (fun (v,_) -> incr count; v , Some (EArray (ident p "@tmp",int p (!count)),p)) el),p);
									(gen_expr e2)
								],p)
						in
						str p s , e
					) l,
					(match eo with None -> None | Some e -> Some (gen_expr e))
				),p)
			),p)
		with
			Not_found ->
				(ESwitch (
					gen_expr e,
					List.map (fun (e1,e2) -> gen_expr e1, gen_expr e2) cases,
					(match eo with None -> None | Some e -> Some (gen_expr e))
				),p)

let gen_method c acc =
	match c.cf_expr with
	| None -> acc
	| Some e ->
		match e.eexpr with
		| TFunction _ -> ((if c.cf_name = "new" then "__construct__" else c.cf_name), gen_expr e) :: acc
		| _ -> acc

let gen_class p c =	
	let clpath = gen_type_path null_pos (fst p,"@" ^ snd p) in
	let stpath = gen_type_path null_pos p in
	let esuper = match c.cl_super with None -> null null_pos | Some (c,_) -> gen_type_path null_pos (fst c.cl_path,"@" ^ snd c.cl_path) in
	let fnew = (try
		let f = PMap.find "new" c.cl_statics in
		match follow f.cf_type with
		| TFun (args,_) ->
			let params = nparams args in
			let p = null_pos in
			["new",(EFunction (params,(EBlock [
				(EVars ["@o",Some (call p (builtin p "new") [clpath])],p);
				(call p (builtin p "call") [field p (this p) "__construct__"; ident p "@o"; array p (List.map (ident p) params)]);
				(EReturn (Some (ident p "@o")),p)
			],p)),p)]
		| _ -> []
	with Not_found ->
		[]
	) in
	let fstring = (try
		let f = PMap.find "toString" c.cl_fields in
		match follow f.cf_type with
		| TFun ([],_) ->
			let p = null_pos in
			["__string",(EFunction ([],(EBlock [
				EReturn (Some (field p (call p (field p (this p) "toString") []) "__s")),p
			],p)),p)]
		| _ -> []
	with Not_found -> 
		[]
	) in	
	let estat = (EBinop ("=",
		stpath,
		(EObject (PMap.fold gen_method c.cl_statics fnew),null_pos)
	),null_pos) in
	let p = null_pos in
	let eclass = (EBinop ("=",
		clpath,
		call p (builtin p "new") [esuper]
	),null_pos) in
	let interf = array p (List.map (fun (c,_) -> gen_type_path p c.cl_path) c.cl_implements) in
	let magic = ("__class__", call p (builtin p "array") [stpath; interf; match c.cl_super with None -> null p | Some _ -> field p esuper "__class__"]) in
	let methods = PMap.fold gen_method c.cl_fields fstring in
	(EBlock (
		estat ::
		eclass ::
		(EVars ["@tmp", Some clpath],p) ::
		(List.map (fun (f,e) -> (EBinop ("=",field p (ident p "@tmp") f,e),p)) (magic :: methods))
	),p)

let gen_enum_constr c =
	let p = pos c.ef_pos in
	c.ef_name , (match follow c.ef_type with
		| TFun (params,_) -> 
			let params = nparams params in
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

let gen_packages h ((p,_),t) =
	let rec loop acc p =
		match p with
		| [] -> []
		| x :: l ->
			let path = acc @ [x] in
			if not (Hashtbl.mem h path) then begin
				let p = null_pos in
				let e = (EBinop ("=",gen_type_path p (acc,x),call p (builtin p "new") [null p]),p) in
				Hashtbl.add h path ();
				e :: loop path l
			end else
				loop path l
	in
	loop [] p

let gen_boot() =
	call null_pos (field null_pos (ident null_pos "Boot") "__init") []

let generate file types =
	let h = Hashtbl.create 0 in
	let packs = List.concat (List.map (gen_packages h) types) in
	let methods = List.map gen_type types in
	let boot = gen_boot() in
	let vars = List.concat (List.map gen_static_vars types) in
	let e = (EBlock (packs @ methods @ boot :: vars), null_pos) in
	let neko_file = Filename.chop_extension file ^ ".neko" in
	let ch = IO.output_channel (open_out neko_file) in
	Nxml.write ch (Nxml.to_xml e);
	IO.close_out ch;
	if Sys.command ("nekovm neko " ^ neko_file) = 0 && not (!Plugin.verbose) then Sys.remove neko_file

;;
Nast.do_escape := false