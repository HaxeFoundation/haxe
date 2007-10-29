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

type context = {
	methods : bool;
	mutable curclass : string;
	mutable curmethod : string;
	mutable locals : (string , bool) PMap.t;
	mutable curblock : texpr list;
	mutable inits : texpr list;
}

let error msg p =
	raise (Typer.Error (Typer.Custom msg,p))

let files = Hashtbl.create 0

let pos ctx p =
	let file = (match ctx.methods with
		| true -> ctx.curclass ^ "::" ^ ctx.curmethod
		| false ->
			try
				Hashtbl.find files p.pfile
			with Not_found -> try
				let len = String.length p.pfile in
				let base = List.find (fun path ->
					let l = String.length path in
					len > l  && String.sub p.pfile 0 l = path
				) (!Plugin.class_path) in
				let l = String.length base in
				let path = String.sub p.pfile l (len - l) in
				Hashtbl.add files p.pfile path;
				path
			with Not_found ->
				Hashtbl.add files p.pfile p.pfile;
				p.pfile
	) in
	{
		psource = file;
		pline = Lexer.get_error_line p;
	}

let add_local ctx v p =
	let rec loop flag e =
		match e.eexpr with
		| TLocal a ->
			if flag && a = v then raise Exit
		| TFunction f ->
			if not (List.exists (fun (a,_,_) -> a = v) f.tf_args) then loop true f.tf_expr
		| TVars l ->
			if List.exists (fun (a,_,_) -> a = v) l then raise Not_found;
			Type.iter (loop flag) e
		| TFor (a,_,e1,e2) ->
			loop flag e1;
			if a <> v then loop flag e2
		| TMatch (e,_,cases,eo) ->
			loop flag e;
			(match eo with None -> () | Some e -> loop flag e);
			List.iter (fun (_,params,e) ->				
				match params with
				| Some l when List.exists (fun (a,_) -> a = Some v) l -> ()
				| _ -> loop flag e
			) cases
		| TBlock l ->
			(try
				List.iter (loop flag) l
			with
				Not_found -> ())
		| TTry (e,catchs) ->
			loop flag e;
			List.iter (fun (a,_,e) -> if a <> v then loop flag e) catchs
		| _ ->
			Type.iter (loop flag) e
	in
	let isref = (try
		List.iter (loop false) ctx.curblock;
		false
	with
		| Not_found -> false
		| Exit -> true
	) in
	ctx.locals <- PMap.add v isref ctx.locals;
	isref

let block ctx curblock =
	let l = ctx.locals in
	let b = ctx.curblock in
	ctx.curblock <- curblock;
	(fun() ->
		ctx.locals <- l;
		ctx.curblock <- b;
	)

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

let rec needs_return e =
	match e with
	| (EBlock l,_) ->
		let rec loop = function
			| [] -> true
			| [x] -> needs_return x
			| _ :: l -> loop l
		in
		loop l
	| (EReturn _,_) ->
		false
	| _ ->
		true

let with_return e =
	if needs_return e then
		let p = snd e in
		let ret = EReturn (Some (null p)),p in
		match e with
		| (EBlock l,_) ->
			(EBlock (l @ [ret]),p)
		| _ ->
			(EBlock [e;ret] , p)
	else
		e

let gen_type_path p (path,t) =
	match path with
	| [] ->
		ident p t
	| path :: l ->
		let epath = List.fold_left (fun e path -> field p e path) (ident p path) l in
		field p epath t

let rec gen_big_string ctx p s =
	let max = 1 lsl 16 - 1 in
	if String.length s > max then
		(EBinop ("+",str p (String.sub s 0 max),gen_big_string ctx p (String.sub s max (String.length s - max))),p)
	else
		str p s

let gen_constant ctx pe c =
	let p = pos ctx pe in
	match c with
	| TInt i -> (try int p (Int32.to_int i) with _ -> Typer.error "This integer is too big to be compiled to a Neko 31-bit integer. Please use a Float instead" pe)
	| TFloat f -> (EConst (Float f),p)
	| TString s -> call p (field p (ident p "String") "new") [gen_big_string ctx p s]
	| TBool b -> (EConst (if b then True else False),p)
	| TNull -> null p
	| TThis -> this p
	| TSuper -> assert false

let rec gen_binop ctx p op e1 e2 =
	let gen_op str =
		(EBinop (str,gen_expr ctx e1,gen_expr ctx e2),p)
	in
	match op with
	| OpPhysEq -> (EBinop ("==", call p (builtin p "pcompare") [gen_expr ctx e1; gen_expr ctx e2], int p 0),p)
	| OpPhysNotEq ->  (EBinop ("!=", call p (builtin p "pcompare") [gen_expr ctx e1; gen_expr ctx e2], int p 0),p)
	| _ -> gen_op (Ast.s_binop op)

and gen_unop ctx p op flag e =
	match op with
	| Increment -> (EBinop ((if flag = Prefix then "+=" else "++="), gen_expr ctx e , int p 1),p)
	| Decrement -> (EBinop ((if flag = Prefix then "-=" else "--="), gen_expr ctx e , int p 1),p)
	| Not -> call p (builtin p "not") [gen_expr ctx e]
	| Neg -> (EBinop ("-",int p 0, gen_expr ctx e),p)
	| NegBits -> error "Operation not available" e.epos

and gen_call ctx p e el =
	match e.eexpr , el with
	| TConst TSuper , _ ->
		let c = (match follow e.etype with TInst (c,_) -> c | _ -> assert false) in
		call p (builtin p "call") [
			field p (gen_type_path p c.cl_path) "__construct__";
			this p;
			array p (List.map (gen_expr ctx) el)
		]
	| TField ({ eexpr = TConst TSuper; etype = t },f) , _ ->
		let c = (match follow t with TInst (c,_) -> c | _ -> assert false) in
		call p (builtin p "call") [
			field p (gen_type_path p (fst c.cl_path,"@" ^ snd c.cl_path)) f;
			this p;
			array p (List.map (gen_expr ctx) el)
		]
	| TField (e,f) , el ->
		call p (field p (gen_expr ctx e) f) (List.map (gen_expr ctx) el)
	| _ , _ ->
		let e = (match gen_expr ctx e with EFunction _, _ as e -> (EBlock [e],p) | e -> e) in
		call p e (List.map (gen_expr ctx) el)

and gen_closure p t e f =
	match follow t with
	| TFun (args,_) ->
		let n = ref 0 in
		let args = List.map (fun _ -> incr n; "p" ^ string_of_int (!n)) args in
		let tmp = ident p "@tmp" in
		let ifun = ident p "@fun" in
		EBlock [
			(EVars ["@tmp", Some e; "@fun", Some (field p tmp f)] , p);
			(EIf ((EBinop ("==",ifun,null p),p),
				null p,
				Some (EFunction (args,(EBlock [
					(EBinop ("=",this p,tmp),p);
					(EReturn (Some (call p ifun (List.map (ident p) args))),p)
				],p)),p)
			),p)
		] , p
	| _ ->
		field p e f

and gen_expr ctx e =
	let p = pos ctx e.epos in
	match e.eexpr with
	| TConst c ->
		gen_constant ctx e.epos c
	| TLocal s ->
		let isref = try PMap.find s ctx.locals with Not_found -> false in
		if isref then
			(EArray (ident p s,int p 0),p)
		else
			ident p s
	| TEnumField (e,f) ->
		field p (gen_type_path p e.e_path) f
	| TArray (e1,e2) ->
		(EArray (gen_expr ctx e1,gen_expr ctx e2),p)
	| TBinop (OpAssign,{ eexpr = TField (e1,f) },e2) ->
		(EBinop ("=",field p (gen_expr ctx e1) f,gen_expr ctx e2),p)
	| TBinop (op,e1,e2) ->
		gen_binop ctx p op e1 e2
	| TField (e2,f) ->
		gen_closure p e.etype (gen_expr ctx e2) f
	| TTypeExpr t ->
		gen_type_path p (t_path t)
	| TParenthesis e ->
		(EParenthesis (gen_expr ctx e),p)
	| TObjectDecl fl ->
		(EObject (List.map (fun (f,e) -> f , gen_expr ctx e) fl),p)
	| TArrayDecl el ->
		if List.length el > 115 then error "This array declaration is too big, try to split it" e.epos;
		call p (field p (ident p "Array") "new1") [array p (List.map (gen_expr ctx) el); int p (List.length el)]
	| TCall (e,el) ->
		gen_call ctx p e el
	| TNew (c,_,params) ->
		call p (field p (gen_type_path p c.cl_path) "new") (List.map (gen_expr ctx) params)
	| TUnop (op,flag,e) ->
		gen_unop ctx p op flag e
	| TVars vl ->
		(EVars (List.map (fun (v,_,e) ->
			let isref = add_local ctx v p in
			let e = (match e with
				| None ->
					if isref then
						Some (call p (builtin p "array") [null p])
					else
						None
				| Some e ->
					let e = gen_expr ctx e in
					if isref then
						Some (call p (builtin p "array") [e])
					else
						Some e
			) in
			v , e
		) vl),p)
	| TFunction f ->
		let b = block ctx [f.tf_expr] in
		let inits = List.fold_left (fun acc (a,_,_) ->
			if add_local ctx a p then
				(a, Some (call p (builtin p "array") [ident p a])) :: acc
			else
				acc
		) [] f.tf_args in
		let e = gen_expr ctx f.tf_expr in
		let e = (match inits with [] -> e | _ -> (EBlock [(EVars (List.rev inits),p);e],p)) in
		let e = (EFunction (List.map arg_name f.tf_args, with_return e),p) in
		b();
		e
	| TBlock el ->
		let b = block ctx el in
		let rec loop = function
			| [] -> []
			| e :: l ->
				ctx.curblock <- l;
				let e = gen_expr ctx e in
				e :: loop l
		in
		let e = (EBlock (loop el), p) in
		b();
		e
	| TFor (v, _, it, e) ->
		let it = gen_expr ctx it in
		let b = block ctx [e] in
		let isref = add_local ctx v p in
		let e = gen_expr ctx e in
		b();
		let next = call p (field p (ident p "@tmp") "next") [] in
		let next = (if isref then call p (builtin p "array") [next] else next) in
		(EBlock
			[(EVars ["@tmp", Some it],p);
			(EWhile (call p (field p (ident p "@tmp") "hasNext") [],
				(EBlock [
					(EVars [v, Some next],p);
					e
				],p)
			,NormalWhile),p)]
		,p)
	| TIf (cond,e1,e2) ->
		(EIf (gen_expr ctx cond,gen_expr ctx e1,(match e2 with None -> None | Some e -> Some (gen_expr ctx e))),p)
	| TWhile (econd,e,flag) ->
		(EWhile (gen_expr ctx econd, gen_expr ctx e, match flag with Ast.NormalWhile -> NormalWhile | Ast.DoWhile -> DoWhile),p)
	| TTry (e,catchs) ->
		let rec loop = function
			| [] -> call p (builtin p "rethrow") [ident p "@tmp"]
			| (v,t,e) :: l ->
				let e2 = loop l in
				let path = (match follow t with
					| TInst (c,_) -> Some c.cl_path
					| TEnum (e,_) -> Some e.e_path
					| TDynamic _ -> None
					| _ -> assert false
				) in
				let cond = (match path with
					| None -> (EConst True,p)
					| Some path -> call p (field p (gen_type_path p (["neko"],"Boot")) "__instanceof") [ident p "@tmp"; gen_type_path p path]
				) in
				let b = block ctx [e] in
				let isref = add_local ctx v p in
				let id = ident p "@tmp" in
				let id = (if isref then call p (builtin p "array") [id] else id) in
				let e = gen_expr ctx e in
				b();
				(EIf (cond,(EBlock [
					EVars [v,Some id],p;
					e;
				],p),Some e2),p)
		in
		let catchs = loop catchs in
		let catchs = (EBlock [
			(EIf (
				(EBinop ("==",call p (builtin p "typeof") [ident p "@tmp"],builtin p "tstring"),p),
				(EBinop ("=",ident p "@tmp",call p (field p (ident p "String") "new") [ident p "@tmp"]),p),
				None
			),p);
			catchs;
		],p) in
		(ETry (gen_expr ctx e,"@tmp",catchs),p)
	| TReturn eo ->
		(EReturn (match eo with None -> Some (null p) | Some e -> Some (gen_expr ctx e)),p)
	| TBreak ->
		(EBreak None,p)
	| TContinue ->
		(EContinue,p)
	| TThrow e ->
		call p (builtin p "throw") [gen_expr ctx e]
	| TMatch (e,_,cases,eo) ->
		let etmp = (EVars ["@tmp",Some (gen_expr ctx e)],p) in
		let eindex = field p (ident p "@tmp") "index" in
		let gen_params params e =
			match params with
			| None ->
				gen_expr ctx e
			| Some el ->
				let b = block ctx [e] in
				let count = ref (-1) in
				let vars = List.fold_left (fun acc (v,_) ->
					incr count;
					match v with
					| None ->
						acc
					| Some v ->
						let isref = add_local ctx v p in
						let e = (EArray (ident p "@tmp",int p (!count)),p) in
						let e = (if isref then call p (builtin p "array") [e] else e) in
						(v , Some e) :: acc
				) [] el in
				let e = gen_expr ctx e in
				b();
				(EBlock [
					(EVars ["@tmp",Some (field p (ident p "@tmp") "args")],p);
					(match vars with [] -> null p | _ -> EVars vars,p);
					e
				],p)
		in
		(try
		  (EBlock [
			etmp;
			(ESwitch (
				eindex,
				List.map (fun (cl,params,e2) ->
					let cond = match cl with
						| [s] -> int p s
						| _ -> raise Exit
					in					
					cond , gen_params params e2
				) cases,
				(match eo with None -> None | Some e -> Some (gen_expr ctx e))
			),p)
		  ],p)
		with
			Exit ->
				(EBlock [
					etmp;
					(EVars ["@index",Some eindex],p);
					List.fold_left (fun acc (cl,params,e2) ->
						let cond = (match cl with
							| [] -> assert false
							| c :: l ->
								let eq c = (EBinop ("==",ident p "@index",int p c),p) in
								List.fold_left (fun acc c -> (EBinop ("||",acc,eq c),p)) (eq c) l
						) in
						EIf (cond,gen_params params e2,Some acc),p
					) (match eo with None -> null p | Some e -> (gen_expr ctx e)) (List.rev cases)
				],p)
		)
	| TSwitch (e,cases,eo) ->
		let e = gen_expr ctx e in
		let eo = (match eo with None -> None | Some e -> Some (gen_expr ctx e)) in
		try			
			(ESwitch (
				e,
				List.map (fun (el,e2) ->
					match List.map (gen_expr ctx) el with
					| [] -> assert false
					| [e] -> e, gen_expr ctx e2
					| _ -> raise Exit
				) cases,
				eo
			),p)
		with
			Exit ->
				(EBlock [
					(EVars ["@tmp",Some e],p);
					List.fold_left (fun acc (el,e) ->
						let cond = (match el with
							| [] -> assert false
							| e :: l ->
								let eq e = (EBinop ("==",ident p "@tmp",gen_expr ctx e),p) in
								List.fold_left (fun acc e -> (EBinop ("||",acc,eq e),p)) (eq e) l
						) in
						EIf (cond,gen_expr ctx e,Some acc),p
					) (match eo with None -> null p | Some e -> e) (List.rev cases)
				],p)

let gen_method ctx p c acc =
	ctx.curmethod <- c.cf_name;
	match c.cf_expr with
	| None ->
		if c.cf_get = ResolveAccess then acc else (c.cf_name, null p) :: acc
	| Some e ->		
		match e.eexpr with
		| TCall ({ eexpr = TField ({ eexpr = TTypeExpr (TClassDecl { cl_path = (["neko"],"Lib") }) }, "load")},[{ eexpr = TConst (TString m) };{ eexpr = TConst (TString f) };{ eexpr = TConst (TInt n) }]) ->
			(c.cf_name, call (pos ctx e.epos) (EField (builtin p "loader","loadprim"),p) [(EBinop ("+",(EBinop ("+",str p m,str p "@"),p),str p f),p); (EConst (Int (Int32.to_int n)),p)]) :: acc
		| TFunction _ -> ((if c.cf_name = "new" then "__construct__" else c.cf_name), gen_expr ctx e) :: acc
		| _ -> (c.cf_name, null p) :: acc

let gen_class ctx c =
	ctx.curclass <- s_type_path c.cl_path;
	ctx.curmethod <- "$init";
	let p = pos ctx c.cl_pos in
	let clpath = gen_type_path p (fst c.cl_path,"@" ^ snd c.cl_path) in
	let stpath = gen_type_path p c.cl_path in
	let fnew = (match c.cl_constructor with
	| Some f ->
		(match follow f.cf_type with
		| TFun (args,_) ->
			let params = List.map arg_name args in
			gen_method ctx p f ["new",(EFunction (params,(EBlock [
				(EVars ["@o",Some (call p (builtin p "new") [null p])],p);
				(call p (builtin p "objsetproto") [ident p "@o"; clpath]);
				(call p (builtin p "call") [field p (this p) "__construct__"; ident p "@o"; array p (List.map (ident p) params)]);
				(EReturn (Some (ident p "@o")),p)
			],p)),p)]
		| _ -> [])
	| None ->
		[]
	) in
	let fstring = (try
		let f = PMap.find "toString" c.cl_fields in
		match follow f.cf_type with
		| TFun ([],_) ->
			["__string",(EFunction ([],(EBlock [
				EVars ["@s",Some (call p (field p (this p) "toString") [])] ,p;
				EIf ((EBinop ("!=",call p (builtin p "typeof") [ident p "@s"],builtin p "tobject"),p),(EReturn (Some (null p)),p),None),p;
				EReturn (Some (field p (ident p "@s") "__s")),p;
			],p)),p)]
		| _ -> []
	with Not_found ->
		[]
	) in
	let fserialize = "__serialize" , ident p "@serialize" in
	let others = (match c.cl_implements with
		| [] -> []
		| l -> ["__interfaces__",array p (List.map (fun (c,_) -> gen_type_path p c.cl_path) l)]
	) @ (match c.cl_super with
		| None -> []
		| Some (c,_) -> ["__super__", gen_type_path p c.cl_path]
	) in
	let build (f,e) = (EBinop ("=",field p (ident p "@tmp") f,e),p) in
	let tmp = (EVars ["@tmp",Some (call p (builtin p "new") [null p])],p) in
	let estat = (EBinop ("=", stpath, ident p "@tmp"),p) in
	let sfields = List.map build
		(
			("prototype",clpath) ::
			PMap.fold (gen_method ctx p) c.cl_statics (fnew @ others)
		)
	in
	let eclass = (EBinop ("=", clpath, ident p "@tmp"),p) in
	let mfields = List.map build
		(PMap.fold (gen_method ctx p) c.cl_fields (fserialize :: fstring))
	in
	let emeta = (EBinop ("=",field p clpath "__class__",stpath),p) ::
		match c.cl_path with
		| [] , name -> [(EBinop ("=",field p (ident p "@classes") name,ident p name),p)]
		| _ -> []
	in
	let eextends = (match c.cl_super with
		| None -> []
		| Some (c,_) ->
			let esuper = gen_type_path p (fst c.cl_path,"@" ^ snd c.cl_path) in
			[call p (builtin p "objsetproto") [clpath; esuper]]
	) in
	(EBlock (tmp :: eclass :: mfields @ tmp :: estat :: sfields @ eextends @ emeta),p)

let gen_enum_constr ctx path c =
	ctx.curmethod <- c.ef_name;
	let p = pos ctx c.ef_pos in
	(EBinop ("=",field p path c.ef_name, match follow c.ef_type with
		| TFun (params,_) ->
			let params = List.map arg_name params in
			(EFunction (params,
				(EBlock [
					(EVars ["@tmp",Some (EObject [
						"tag" , str p c.ef_name;
						"index" , int p c.ef_index;
						"args" , array p (List.map (ident p) params);
					],p)],p);
					call p (builtin p "objsetproto") [ident p "@tmp"; field p path "prototype"];
					ident p "@tmp";
				],p)
			),p)
		| _ ->
			(EBlock [
				(EVars ["@tmp",Some (EObject ["tag" , str p c.ef_name; "index", int p c.ef_index; "__serialize" , ident p "@tag_serialize"],p)],p);
				call p (builtin p "objsetproto") [ident p "@tmp"; field p path "prototype"];				
				ident p "@tmp";
			],p)
	),p)

let gen_enum ctx e =
	ctx.curclass <- s_type_path e.e_path;
	ctx.curmethod <- "$init";
	let p = pos ctx e.e_pos in
	let path = gen_type_path p (fst e.e_path,snd e.e_path) in
	(EBlock (
		(EBinop ("=",path, call p (builtin p "new") [null p]),p) ::
		(EBinop ("=",field p path "prototype", (EObject [
			"__enum__" , path;
			"__serialize" , ident p "@serialize";
			"__string" , ident p "@enum_to_string"
		],p)),p) ::
		pmap_list (gen_enum_constr ctx path) e.e_constrs @
		match e.e_path with
		| [] , name -> [EBinop ("=",field p (ident p "@classes") name,ident p name),p]
		| _ -> []
	),p)

let gen_type ctx t acc =
	match t with
	| TClassDecl c ->
		(match c.cl_init with
		| None -> ()
		| Some e -> ctx.inits <- e :: ctx.inits);
		if c.cl_extern || c.cl_path = ([],"@Main") then
			acc
		else
			gen_class ctx c :: acc
	| TEnumDecl e ->
		if e.e_extern then
			acc
		else
			gen_enum ctx e :: acc
	| TTypeDecl t ->
		acc

let gen_static_vars ctx t =
	match t with
	| TEnumDecl _ | TTypeDecl _ -> []
	| TClassDecl c ->
		if c.cl_extern then
			[]
		else
			List.fold_right (fun f acc ->
				match f.cf_expr with
				| None -> acc
				| Some e ->
					match e.eexpr with
					| TFunction _ -> acc
					| _ ->
						ctx.curclass <- s_type_path c.cl_path;
						ctx.curmethod <- "$statics";
						let p = pos ctx e.epos in
						(EBinop ("=",
							(field p (gen_type_path p c.cl_path) f.cf_name),
							gen_expr ctx e
						),p) :: acc
			) c.cl_ordered_statics []

let gen_package ctx h t =
	let rec loop acc p =
		match p with
		| [] -> []
		| x :: l ->
			let path = acc @ [x] in
			if not (Hashtbl.mem h path) then begin
				let p = pos ctx (match t with TClassDecl c -> c.cl_pos | TEnumDecl e -> e.e_pos | TTypeDecl t -> t.t_pos) in
				let e = (EBinop ("=",gen_type_path p (acc,x),call p (builtin p "new") [null p]),p) in
				Hashtbl.add h path ();
				(match acc with
				| [] ->
					let reg = (EBinop ("=",field p (ident p "@classes") x,ident p x),p) in
					e :: reg :: loop path l
				| _ ->
					e :: loop path l)
			end else
				loop path l
	in
	loop [] (fst (t_path t))

let gen_boot ctx hres =
	let loop name data acc = (name , gen_constant ctx Ast.null_pos (TString data)) :: acc in
	let objres = (EObject (Hashtbl.fold loop hres []),null_pos) in
	(EBlock [
		EBinop ("=",field null_pos (gen_type_path null_pos (["neko"],"Boot")) "__classes",ident null_pos "@classes"),null_pos;
		call null_pos (field null_pos (gen_type_path null_pos (["neko"],"Boot")) "__init") [];
		EBinop ("=",field null_pos (gen_type_path null_pos (["neko"],"Boot")) "__res",objres),null_pos;
	],null_pos)

let gen_name ctx acc t =
	match t with
	| TEnumDecl e when e.e_extern ->
		acc
	| TEnumDecl e ->
		let p = pos ctx e.e_pos in
		let name = fst e.e_path @ [snd e.e_path] in
		let arr = call p (field p (ident p "Array") "new1") [array p (List.map (fun n -> gen_constant ctx e.e_pos (TString n)) name); int p (List.length name)] in
		let path = gen_type_path p e.e_path in
		let setname = (EBinop ("=",field p path "__ename__",arr),p) in
		let arr = call p (field p (ident p "Array") "new1") [array p (List.map (fun n -> gen_constant ctx e.e_pos (TString n)) e.e_names); int p (List.length e.e_names)] in
		let setconstrs = (EBinop ("=", field p path "__constructs__", arr),p) in
		setname :: setconstrs :: acc
	| TClassDecl c ->
		if c.cl_extern || c.cl_path = ([],"@Main") then
			acc
		else
			let p = pos ctx c.cl_pos in
			let name = fst c.cl_path @ [snd c.cl_path] in
			let arr = call p (field p (ident p "Array") "new1") [array p (List.map (fun n -> gen_constant ctx c.cl_pos (TString n)) name); int p (List.length name)] in
			(EBinop ("=",field p (gen_type_path p c.cl_path) "__name__",arr),p) ::
			(match c.cl_implements with
			| [] -> acc
			| l ->
				let interf = field p (gen_type_path p c.cl_path) "__interfaces__" in
				(EBinop ("=",interf, call p (field p (ident p "Array") "new1") [interf; int p (List.length l)]),p) :: acc)
	| TTypeDecl _ ->
		acc

let generate_libs_init = function
	| [] -> ""
	| libs ->
		let boot = 
			"var @s = $loader.loadprim(\"std@sys_string\",0)();" ^
			"var @env = $loader.loadprim(\"std@get_env\",1);" ^
			"var @b = if( @s == \"Windows\" ) " ^
				"@env(\"HAXEPATH\") + \"lib\\\\\"" ^
				"else try $loader.loadprim(\"std@file_contents\",1)(@env(\"HOME\")+\"/.haxelib\") + \"/\"" ^
				"catch e if( @s == \"Linux\" ) \"/usr/lib/haxe/lib/\" else \"/usr/local/lib/haxe/lib/\";" ^
			"@s = @s + \"/\";"
		in
		List.fold_left (fun acc l ->
			let full_path = l.[0] = '/' || l.[1] = ':' in
			acc ^ "$loader.path = $array(" ^ (if full_path then "" else "@b + ") ^ "\"" ^ Nast.escape l ^ "\" + @s,$loader.path);"
		) boot libs

let generate file types hres libs =
	let ctx = {
		methods = Plugin.defined "debug";
		curclass = "$boot";
		curmethod = "$init";
		inits = [];
		curblock = [];
		locals = PMap.empty;
	} in
	let t = Plugin.timer "neko ast" in
	let h = Hashtbl.create 0 in
	let header = ENeko (
		"@classes = $new(null);" ^
		"@Main = $new(null);" ^
		"@enum_to_string = function() { return neko.Boot.__enum_str(this); };" ^
		"@serialize = function() { return neko.Boot.__serialize(this); };" ^ 
		"@tag_serialize = function() { return neko.Boot.__tagserialize(this); };" ^ 
		generate_libs_init libs
	) , { psource = "<header>"; pline = 1; } in
	let packs = List.concat (List.map (gen_package ctx h) types) in
	let names = List.fold_left (gen_name ctx) [] types in
	let methods = List.rev (List.fold_left (fun acc t -> gen_type ctx t acc) [] types) in
	let boot = gen_boot ctx hres in
	let inits = List.map (gen_expr ctx) (List.rev ctx.inits) in
	let vars = List.concat (List.map (gen_static_vars ctx) types) in
	let e = (EBlock (header :: packs @ methods @ boot :: names @ inits @ vars), null_pos) in
	t();
	let neko_file = (try Filename.chop_extension file with _ -> file) ^ ".neko" in
	let w = Plugin.timer "neko ast write" in
	let ch = IO.output_channel (open_out_bin neko_file) in
	let source = Plugin.defined "neko_source" in	
	if source then Nxml.write ch (Nxml.to_xml e) else Binast.write ch e;
	IO.close_out ch;
	let command cmd = try Sys.command cmd with _ -> -1 in
	if source then begin
		if command ("nekoc -p \"" ^ neko_file ^ "\"") <> 0 then failwith "Failed to print neko code";
		Sys.remove neko_file;
		Sys.rename ((try Filename.chop_extension file with _ -> file) ^ "2.neko") neko_file;
	end;
	w();
	let c = Plugin.timer "neko compilation" in
	if command ("nekoc \"" ^ neko_file ^ "\"") <> 0 then failwith "Neko compilation failure";
	c();
	let output = Filename.chop_extension neko_file ^ ".n" in
	if output <> file then Sys.rename output file;
	if not source then Sys.remove neko_file
