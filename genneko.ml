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
	mutable locals : (string , bool) PMap.t;
	mutable curblock : texpr list;
	mutable inits : texpr list;
}

let error msg p =
	raise (Typer.Error (Typer.Custom msg,p))

let pos p =
	{
		psource = p.pfile;
		pline = Lexer.get_error_line p;
	}

let add_local ctx v p =
	let rec loop flag e =
		match e.eexpr with
		| TLocal a ->
			if flag && a = v then raise Exit
		| TFunction f ->
			if not (List.exists (fun (a,_) -> a = v) f.tf_args) then loop true f.tf_expr
		| TVars l -> 
			if List.exists (fun (a,_,_) -> a = v) l then raise Not_found;
			Type.iter (loop flag) e
		| TFor (a,e1,e2) ->
			loop flag e1;
			if a <> v then loop flag e2
		| TMatch (e,_,cases,eo) ->
			loop flag e;
			(match eo with None -> () | Some e -> loop flag e);
			List.iter (fun (_,vars,e) ->
				match vars with
				| Some l when List.exists (fun (a,_) -> a = v) l -> ()
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
		call p (gen_expr ctx e) (List.map (gen_expr ctx) el)

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
	let p = pos e.epos in
	match e.eexpr with
	| TConst c ->
		gen_constant p c
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
	| TType t ->
		gen_type_path p (type_path t)
	| TParenthesis e ->
		(EParenthesis (gen_expr ctx e),p)
	| TObjectDecl fl ->
		(EObject (List.map (fun (f,e) -> f , gen_expr ctx e) fl),p)
	| TArrayDecl el ->
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
		let inits = List.fold_left (fun acc (a,_) -> 
			if add_local ctx a p then 
				(a, Some (call p (builtin p "array") [ident p a])) :: acc
			else
				acc
		) [] f.tf_args in
		let e = gen_expr ctx f.tf_expr in
		let e = (match inits with [] -> e | _ -> (EBlock [(EVars (List.rev inits),p);e],p)) in
		let e = (EFunction (List.map fst f.tf_args, e),p) in
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
	| TFor (v, it, e) ->
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
		(EReturn (match eo with None -> None | Some e -> Some (gen_expr ctx e)),p)
	| TBreak ->
		(EBreak None,p)
	| TContinue ->
		(EContinue,p)
	| TThrow e ->
		call p (builtin p "throw") [gen_expr ctx e]
	| TMatch (e,_,cases,eo) ->
		(EBlock [
			(EVars ["@tmp",Some (gen_expr ctx e)],p);
			(ESwitch (
				field p (ident p "@tmp") "tag",
				List.map (fun (s,el,e2) ->
					let count = ref (-1) in
					let e = match el with
						| None -> gen_expr ctx e2
						| Some el ->
							let b = block ctx [e2] in
							let vars = List.map (fun (v,_) -> 
								incr count; 
								let isref = add_local ctx v p in
								let e = (EArray (ident p "@tmp",int p (!count)),p) in
								let e = (if isref then call p (builtin p "array") [e] else e) in
								v , Some e
							) el in
							let e2 = gen_expr ctx e2 in
							b();
							(EBlock [
								(EVars ["@tmp",Some (field p (ident p "@tmp") "args")],p);
								(EVars vars,p);
								e2
							],p)
					in
					str p s , e
				) cases,
				(match eo with None -> None | Some e -> Some (gen_expr ctx e))
			),p)
		],p)
	| TSwitch (e,cases,eo) ->
		(ESwitch (
			gen_expr ctx e,
			List.map (fun (e1,e2) -> gen_expr ctx e1, gen_expr ctx e2) cases,
			(match eo with None -> None | Some e -> Some (gen_expr ctx e))
		),p)

let gen_method ctx p c acc =
	match c.cf_expr with
	| None -> 
		(c.cf_name, null p) :: acc
	| Some e ->
		match e.eexpr with
		| TCall ({ eexpr = TField ({ eexpr = TType (TClassDecl { cl_path = (["neko"],"Lib") }) }, "load")},[{ eexpr = TConst (TString m) };{ eexpr = TConst (TString f) };{ eexpr = TConst (TInt n) }]) ->
			(c.cf_name, call (pos e.epos) (EField (builtin p "loader","loadprim"),p) [(EBinop ("+",(EBinop ("+",str p m,str p "@"),p),str p f),p); (EConst (Int (int_of_string n)),p)]) :: acc
		| TFunction _ -> ((if c.cf_name = "new" then "__construct__" else c.cf_name), gen_expr ctx e) :: acc
		| _ -> (c.cf_name, null p) :: acc

let gen_class ctx c =	
	let p = pos c.cl_pos in
	let clpath = gen_type_path p (fst c.cl_path,"@" ^ snd c.cl_path) in
	let stpath = gen_type_path p c.cl_path in
	let esuper = match c.cl_super with None -> null p | Some (c,_) -> gen_type_path p (fst c.cl_path,"@" ^ snd c.cl_path) in
	let fnew = (match c.cl_constructor with
	| Some f ->
		(match follow f.cf_type with
		| TFun (args,_) ->
			let params = List.map fst args in
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
	let interf = array p (List.map (fun (c,_) -> gen_type_path p c.cl_path) c.cl_implements) in
	let estat = (EBinop ("=",
		stpath,
		(EObject (
			("prototype",clpath) ::
			("__string", ident p "@class_to_string") ::
			("__super__", match c.cl_super with None -> null p | Some (c,_) -> gen_type_path p c.cl_path) ::
			("__interfaces__", interf) ::
			PMap.fold (gen_method ctx p) c.cl_statics fnew
		),p)
	),p) in
	let eclass = (EBinop ("=",
		clpath,
		(EObject (PMap.fold (gen_method ctx p) c.cl_fields (fserialize :: fstring)),p)
	),p) in
	let emeta = (EBinop ("=",field p clpath "__class__",stpath),p) ::
		match c.cl_path with
		| [] , name -> [(EBinop ("=",field p (ident p "@classes") name,ident p name),p)]
		| _ -> []
	in
	(EBlock ([eclass; estat; call p (builtin p "objsetproto") [clpath; esuper]] @ emeta),p)	

let gen_enum_constr path c =
	let p = pos c.ef_pos in
	(EBinop ("=",field p path c.ef_name, match follow c.ef_type with
		| TFun (params,_) ->
			let params = List.map fst params in
			(EFunction (params,
				(EBlock [
					(EVars ["@tmp",Some (EObject [
						"tag" , str p c.ef_name;
						"args" , array p (List.map (ident p) params);
					],p)],p);
					call p (builtin p "objsetproto") [ident p "@tmp"; field p path "prototype"];
					ident p "@tmp";
				],p)
			),p)
		| _ ->
			(EBlock [
				(EVars ["@tmp",Some (EObject ["tag" , str p c.ef_name],p)],p);
				call p (builtin p "objsetproto") [ident p "@tmp"; field p path "prototype"];
				ident p "@tmp";
			],p)
	),p)

let gen_enum e =
	let p = pos e.e_pos in
	let path = gen_type_path p e.e_path in
	(EBlock (
		(EBinop ("=",path, call p (builtin p "new") [null p]),p) ::
		(EBinop ("=",field p path "prototype", (EObject [
			"__enum__" , path;
			"__serialize" , ident p "@serialize";
			"__string" , ident p "@enum_to_string"
		],p)),p) ::
		pmap_list (gen_enum_constr path) e.e_constrs @
		match e.e_path with
		| [] , name -> [EBinop ("=",field p (ident p "@classes") name,ident p name),p]
		| _ -> []
	),p)

let gen_type ctx t =
	match t with
	| TClassDecl c -> 
		(match c.cl_init with
		| None -> ()
		| Some e -> ctx.inits <- e :: ctx.inits);
		if c.cl_extern then
			null (pos c.cl_pos)
		else
			gen_class ctx c
	| TEnumDecl e -> 
		if e.e_path = ([],"Bool") then
			null (pos e.e_pos)
		else
			gen_enum e

let gen_static_vars ctx t =
	match t with
	| TEnumDecl _ -> []
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
						let p = pos e.epos in
						(EBinop ("=",
							(field p (gen_type_path p c.cl_path) f.cf_name),
							gen_expr ctx e
						),p) :: acc
			) c.cl_ordered_statics []

let gen_package h t =
	let rec loop acc p =
		match p with
		| [] -> []
		| x :: l ->
			let path = acc @ [x] in
			if not (Hashtbl.mem h path) then begin
				let p = pos (match t with TClassDecl c -> c.cl_pos | TEnumDecl e -> e.e_pos) in
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
	loop [] (fst (type_path t))

let gen_boot hres =
	let loop name data acc = (name , gen_constant null_pos (TString data)) :: acc in
	let objres = (EObject (Hashtbl.fold loop hres []),null_pos) in
	(EBlock [
		call null_pos (field null_pos (gen_type_path null_pos (["neko"],"Boot")) "__init") [];
		EBinop ("=",field null_pos (gen_type_path null_pos (["neko"],"Boot")) "__res",objres),null_pos;
		EBinop ("=",field null_pos (gen_type_path null_pos (["neko"],"Boot")) "__classes",ident null_pos "@classes"),null_pos;
	],null_pos)

let gen_name acc t =
	match t with
	| TEnumDecl e ->
		let p = pos e.e_pos in
		let name = fst e.e_path @ [snd e.e_path] in
		let arr = call p (field p (ident p "Array") "new1") [array p (List.map (fun n -> gen_constant p (TString n)) name); int p (List.length name)] in
		(EBinop ("=",field p (gen_type_path p e.e_path) "__ename__",arr),p) :: acc
	| TClassDecl c -> 
		if c.cl_extern then
			acc
		else
			let p = pos c.cl_pos in
			let name = fst c.cl_path @ [snd c.cl_path] in
			let interf = field p (gen_type_path p c.cl_path) "__interfaces__" in
			let arr = call p (field p (ident p "Array") "new1") [array p (List.map (fun n -> gen_constant p (TString n)) name); int p (List.length name)] in
			(EBinop ("=",field p (gen_type_path p c.cl_path) "__name__",arr),p) :: 
			(EBinop ("=",interf, call p (field p (ident p "Array") "new1") [interf; int p (List.length c.cl_implements)]),p) ::
			acc

let generate file types hres =
	let ctx = {
		inits = [];
		curblock = [];
		locals = PMap.empty;
	} in
	let h = Hashtbl.create 0 in
	let header = ENeko (
		"@classes = $new(null);" ^
		"@enum_to_string = function() { return neko.Boot.__enum_str(this); };" ^
		"@class_to_string = function() { return this.__name__.join(String.new(\".\")); };" ^
		"@serialize = function() { return neko.Boot.__serialize(this); };"
	) , { psource = "<header>"; pline = 1; } in
	let packs = List.concat (List.map (gen_package h) types) in
	let names = List.fold_left gen_name [] types in
	let methods = List.map (gen_type ctx) types in
	let boot = gen_boot hres in
	let inits = List.map (gen_expr ctx) (List.rev ctx.inits) in
	let vars = List.concat (List.map (gen_static_vars ctx) types) in
	let e = (EBlock (header :: packs @ methods @ boot :: names @ inits @ vars), null_pos) in
	let neko_file = Filename.chop_extension file ^ ".neko" in
	let ch = IO.output_channel (open_out neko_file) in
	(if !Plugin.verbose then Nxml.write_fmt else Nxml.write) ch (Nxml.to_xml e);
	IO.close_out ch;
	if !Plugin.verbose then begin
		if Sys.command ("nekoc -p " ^ neko_file) <> 0 then failwith "Failed to print neko code";
		Sys.remove neko_file;
		Sys.rename (Filename.chop_extension file ^ "2.neko") neko_file;
	end;
	if Sys.command ("nekoc " ^ neko_file) <> 0 then failwith "Neko compilation failure";
	if not !Plugin.verbose then Sys.remove neko_file
