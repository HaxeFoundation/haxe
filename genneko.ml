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
open Common

type context = {
	version : int;
	com : Common.context;
	packages : (string list, unit) Hashtbl.t;
	globals : (string list * string, string) Hashtbl.t;
	mutable curglobal : int;
	mutable macros : bool;
	mutable curclass : string;
	mutable curmethod : string;
	mutable inits : (tclass * texpr) list;
}

let files = Hashtbl.create 0

let pos ctx p =
	if ctx.macros then
		{
			psource = p.pfile;
			pline = p.pmin lor ((p.pmax - p.pmin) lsl 20);
		}
	else let file = (match ctx.com.debug with
		| true -> ctx.curclass ^ "::" ^ ctx.curmethod
		| false ->
			try
				Hashtbl.find files p.pfile
			with Not_found -> try
				(* lookup relative path *)
				let len = String.length p.pfile in
				let base = List.find (fun path ->
					let l = String.length path in
					len > l && String.sub p.pfile 0 l = path
				) ctx.com.Common.class_path in
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

let gen_global_name ctx path =
	match path with
	| [], name -> name
	| _ ->
	try
		Hashtbl.find ctx.globals path
	with Not_found ->
		let name = "@G" ^ string_of_int ctx.curglobal in
		ctx.curglobal <- ctx.curglobal + 1;
		Hashtbl.add ctx.globals path name;
		name

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
	| TInt i ->
		(try
			let h = Int32.to_int (Int32.shift_right_logical i 24) in
			if (h land 128 = 0) <> (h land 64 = 0) then raise Exit;
			int p (Int32.to_int i)
		with _ ->
			if ctx.version < 2 then error "This integer is too big to be compiled to a Neko 31-bit integer. Please use a Float instead" pe;
			(EConst (Int32 i),p))
	| TFloat f -> (EConst (Float f),p)
	| TString s -> call p (field p (ident p "String") "new") [gen_big_string ctx p s]
	| TBool b -> (EConst (if b then True else False),p)
	| TNull -> null p
	| TThis -> this p
	| TSuper -> assert false

let rec gen_binop ctx p op e1 e2 =
	(EBinop (Ast.s_binop op,gen_expr ctx e1,gen_expr ctx e2),p)

and gen_unop ctx p op flag e =
	match op with
	| Increment -> (EBinop ((if flag = Prefix then "+=" else "++="), gen_expr ctx e , int p 1),p)
	| Decrement -> (EBinop ((if flag = Prefix then "-=" else "--="), gen_expr ctx e , int p 1),p)
	| Not -> call p (builtin p "not") [gen_expr ctx e]
	| Neg -> (EBinop ("-",int p 0, gen_expr ctx e),p)
	| NegBits -> (EBinop ("-",int p (-1), gen_expr ctx e),p)

and gen_call ctx p e el =
	match e.eexpr , el with
	| TConst TSuper , _ ->
		let c = (match follow e.etype with TInst (c,_) -> c | _ -> assert false) in
		call p (builtin p "call") [
			field p (gen_type_path p c.cl_path) "__construct__";
			this p;
			array p (List.map (gen_expr ctx) el)
		]
	| TLocal { v_name = "__resources__" }, [] ->
		call p (builtin p "array") (Hashtbl.fold (fun name data acc ->
			(EObject [("name",gen_constant ctx e.epos (TString name));("data",gen_big_string ctx p data)],p) :: acc
		) ctx.com.resources [])
	| TField ({ eexpr = TConst TSuper; etype = t },f) , _ ->
		let c = (match follow t with TInst (c,_) -> c | _ -> assert false) in
		call p (builtin p "call") [
			field p (gen_type_path p (fst c.cl_path,"@" ^ snd c.cl_path)) (field_name f);
			this p;
			array p (List.map (gen_expr ctx) el)
		]
	| _ , _ ->
		let e = (match gen_expr ctx e with EFunction _, _ as e -> (EBlock [e],p) | e -> e) in
		call p e (List.map (gen_expr ctx) el)

and gen_expr ctx e =
	let p = pos ctx e.epos in
	match e.eexpr with
	| TConst c ->
		gen_constant ctx e.epos c
	| TLocal v when v.v_name.[0] = '$' ->
		(EConst (Builtin (String.sub v.v_name 1 (String.length v.v_name - 1))),p)
	| TLocal v ->
		if v.v_capture then
			(EArray (ident p v.v_name,int p 0),p)
		else
			ident p v.v_name
	| TEnumField (e,f) ->
		field p (gen_type_path p e.e_path) f
	| TArray (e1,e2) ->
		(EArray (gen_expr ctx e1,gen_expr ctx e2),p)
	| TBinop (OpAssign,{ eexpr = TField (e1,f) },e2) ->
		(EBinop ("=",field p (gen_expr ctx e1) (field_name f),gen_expr ctx e2),p)
	| TBinop (op,e1,e2) ->
		gen_binop ctx p op e1 e2
	| TField (e,f) ->
		field p (gen_expr ctx e) (field_name f)
	| TClosure (({ eexpr = TTypeExpr _ } as e),f) ->
		field p (gen_expr ctx e) f
	| TClosure (e2,f) ->
		(match follow e.etype with
		| TFun (args,_) ->
			let n = List.length args in
			if n > 5 then error "Cannot create closure with more than 5 arguments" e.epos;
			let tmp = ident p "@tmp" in
			EBlock [
				(EVars ["@tmp", Some (gen_expr ctx e2); "@fun", Some (field p tmp f)] , p);
				if ctx.macros then
					call p (builtin p "closure") [ident p "@fun";tmp]
				else
					call p (ident p ("@closure" ^ string_of_int n)) [tmp;ident p "@fun"]
			] , p
		| _ -> assert false)
	| TTypeExpr t ->
		gen_type_path p (t_path t)
	| TParenthesis e ->
		(EParenthesis (gen_expr ctx e),p)
	| TObjectDecl fl ->
		let hasToString = ref false in
		let fl = List.map (fun (f,e) -> if f = "toString" then hasToString := (match follow e.etype with TFun ([],_) -> true | _ -> false); f , gen_expr ctx e) fl in
		(EObject (if !hasToString then ("__string",ident p "@default__string") :: fl else fl),p)
	| TArrayDecl el ->
		call p (field p (ident p "Array") "new1") [array p (List.map (gen_expr ctx) el); int p (List.length el)]
	| TCall (e,el) ->
		gen_call ctx p e el
	| TNew (c,_,params) ->
		call p (field p (gen_type_path p c.cl_path) "new") (List.map (gen_expr ctx) params)
	| TUnop (op,flag,e) ->
		gen_unop ctx p op flag e
	| TVars vl ->
		(EVars (List.map (fun (v,e) ->
			let e = (match e with
				| None ->
					if v.v_capture then
						Some (call p (builtin p "array") [null p])
					else
						None
				| Some e ->
					let e = gen_expr ctx e in
					if v.v_capture then
						Some (call p (builtin p "array") [e])
					else
						Some e
			) in
			v.v_name , e
		) vl),p)
	| TFunction f ->
		let inits = List.fold_left (fun acc (a,c) ->
			let acc = if a.v_capture then
				(EBinop ("=",ident p a.v_name,call p (builtin p "array") [ident p a.v_name]),p) :: acc
			else
				acc
			in
			match c with
			| None | Some TNull -> acc
			| Some c ->	gen_expr ctx (Codegen.set_default ctx.com a c e.epos) :: acc
		) [] f.tf_args in
		let e = gen_expr ctx f.tf_expr in
		let e = (match inits with [] -> e | _ -> EBlock (List.rev (e :: inits)),p) in
		(EFunction (List.map arg_name f.tf_args, with_return e),p)
	| TBlock el ->
		(EBlock (List.map (gen_expr ctx) el), p)
	| TFor (v, it, e) ->
		let it = gen_expr ctx it in
		let e = gen_expr ctx e in
		let next = call p (field p (ident p "@tmp") "next") [] in
		let next = (if v.v_capture then call p (builtin p "array") [next] else next) in
		(EBlock
			[(EVars ["@tmp", Some it],p);
			(EWhile (call p (field p (ident p "@tmp") "hasNext") [],
				(EBlock [
					(EVars [v.v_name, Some next],p);
					e
				],p)
			,NormalWhile),p)]
		,p)
	| TIf (cond,e1,e2) ->
		(* if(e)-1 is parsed as if( e - 1 ) *)
		let parent e = mk (TParenthesis e) e.etype e.epos in
		let e1 = (match e1.eexpr with TConst (TInt n) when n < 0l -> parent e1 | TConst (TFloat f) when f.[0] = '-' -> parent e1 | _ -> e1) in
		(EIf (gen_expr ctx cond,gen_expr ctx e1,(match e2 with None -> None | Some e -> Some (gen_expr ctx e))),p)
	| TWhile (econd,e,flag) ->
		(EWhile (gen_expr ctx econd, gen_expr ctx e, match flag with Ast.NormalWhile -> NormalWhile | Ast.DoWhile -> DoWhile),p)
	| TTry (e,catchs) ->
		let rec loop = function
			| [] -> call p (builtin p "rethrow") [ident p "@tmp"]
			| (v,e) :: l ->
				let e2 = loop l in
				let path = (match follow v.v_type with
					| TInst (c,_) -> Some c.cl_path
					| TEnum (e,_) -> Some e.e_path
					| TDynamic _ -> None
					| _ -> assert false
				) in
				let cond = (match path with
					| None -> (EConst True,p)
					| Some path -> call p (field p (gen_type_path p (["neko"],"Boot")) "__instanceof") [ident p "@tmp"; gen_type_path p path]
				) in
				let id = ident p "@tmp" in
				let id = (if v.v_capture then call p (builtin p "array") [id] else id) in
				let e = gen_expr ctx e in
				(EIf (cond,(EBlock [
					EVars [v.v_name,Some id],p;
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
	| TCast (e,None) ->
		gen_expr ctx e
	| TCast (e1,Some t) ->
		gen_expr ctx (Codegen.default_cast ~vtmp:"@tmp" ctx.com e1 t e.etype e.epos)
	| TMatch (e,_,cases,eo) ->
		let p = pos ctx e.epos in
		let etmp = (EVars ["@tmp",Some (gen_expr ctx e)],p) in
		let eindex = field p (ident p "@tmp") "index" in
		let gen_params params e =
			match params with
			| None ->
				gen_expr ctx e
			| Some el ->
				let count = ref (-1) in
				let vars = List.fold_left (fun acc v ->
					incr count;
					match v with
					| None ->
						acc
					| Some v ->
						let e = (EArray (ident p "@tmp",int p (!count)),p) in
						let e = (if v.v_capture then call p (builtin p "array") [e] else e) in
						(v.v_name , Some e) :: acc
				) [] el in
				let e = gen_expr ctx e in
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
	if is_extern_field c then acc else
	match c.cf_expr with
	| None ->
		((c.cf_name, null p) :: acc)
	| Some e ->
		match e.eexpr with
		| TCall ({ eexpr = TField (_,FStatic ({cl_path=["neko"],"Lib"},{cf_name="load" | "loadLazy" as load})) },[{ eexpr = TConst (TString m) };{ eexpr = TConst (TString f) };{ eexpr = TConst (TInt n) }]) ->
			let p = pos ctx e.epos in
			let e = call p (EField (builtin p "loader","loadprim"),p) [(EBinop ("+",(EBinop ("+",str p m,str p "@"),p),str p f),p); (EConst (Int (Int32.to_int n)),p)] in
			let e = (if load = "load" then e else (ETry (e,"@e",call p (ident p "@lazy_error") [ident p "@e"]),p)) in
			(c.cf_name, e) :: acc
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
			let params = List.map (fun (n,_,_) -> n) args in
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
		| TFun ([],_) -> ["__string",ident p "@default__string"]
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
	let gen_props props = (EObject (List.map (fun (n,s) -> n,str p s) props),p) in
	let sprops = (match Codegen.get_properties c.cl_ordered_statics with
		| [] -> []
		| l -> ["__properties__",gen_props l]
	) in
	let sfields = List.map build
		(
			("prototype",clpath) :: sprops @
			PMap.fold (gen_method ctx p) c.cl_statics (fnew @ others)
		)
	in
	let eclass = (EBinop ("=", clpath, ident p "@tmp"),p) in
	let mfields = List.map build
		(PMap.fold (gen_method ctx p) c.cl_fields (fserialize :: fstring))
	in
	let props = Codegen.get_properties c.cl_ordered_fields in
	let emeta = (EBinop ("=",field p clpath "__class__",stpath),p) ::
		(match props with
		| [] -> []
		| _ ->
			let props = gen_props props in
			let props = (match c.cl_super with
				| Some (csup,_) when Codegen.has_properties csup ->
					(EBlock [
						(EVars ["@tmp",Some props],p);
						call p (builtin p "objsetproto") [ident p "@tmp";field p (field p (gen_type_path p csup.cl_path) "prototype") "__properties__"];
						ident p "@tmp"
					],p)
				| _ -> props
			) in
			[EBinop ("=",field p clpath "__properties__",props),p])
		@ match c.cl_path with
		| [] , name -> [(EBinop ("=",field p (ident p "@classes") name,ident p name),p)]
		| _ -> []
	in
	let emeta = if ctx.macros then
		(EBinop ("=",field p stpath "__ct__",call p (builtin p "typewrap") [Obj.magic (TClassDecl c)]),p) :: emeta
	else
		emeta
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
			let params = List.map (fun (n,_,_) -> n) params in
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
	let path = gen_type_path p e.e_path in
	let uname = (EConst (Ident (gen_global_name ctx e.e_path)),p) in
	(EBlock (
		(EBinop ("=",uname, call p (builtin p "new") [null p]),p) ::
		(EBinop ("=",path, uname),p) ::
		(EBinop ("=",field p uname "prototype", (EObject [
			"__enum__" , uname;
			"__serialize" , ident p "@serialize";
			"__string" , ident p "@enum_to_string"
		],p)),p) ::
		pmap_list (gen_enum_constr ctx uname) e.e_constrs @
		(match e.e_path with
		| [] , name -> [EBinop ("=",field p (ident p "@classes") name,ident p name),p]
		| _ -> [])
	),p)

let gen_type ctx t acc =
	match t with
	| TClassDecl c ->
		(match c.cl_init with
		| None -> ()
		| Some e -> ctx.inits <- (c,e) :: ctx.inits);
		if c.cl_extern then
			acc
		else
			gen_class ctx c :: acc
	| TEnumDecl e ->
		if e.e_extern then
			acc
		else
			gen_enum ctx e :: acc
	| TTypeDecl _ | TAbstractDecl _ ->
		acc

let gen_static_vars ctx t =
	match t with
	| TEnumDecl _ | TTypeDecl _ | TAbstractDecl _ -> []
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

let gen_package ctx t =
	let rec loop acc p =
		match p with
		| [] -> []
		| x :: l ->
			let path = acc @ [x] in
			if not (Hashtbl.mem ctx.packages path) then begin
				let p = pos ctx (t_infos t).mt_pos in
				let e = (EBinop ("=",gen_type_path p (acc,x),call p (builtin p "new") [null p]),p) in
				Hashtbl.add ctx.packages path ();
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

let gen_boot ctx =
	(EBlock [
		EBinop ("=",field null_pos (gen_type_path null_pos (["neko"],"Boot")) "__classes",ident null_pos "@classes"),null_pos;
		call null_pos (field null_pos (gen_type_path null_pos (["neko"],"Boot")) "__init") [];
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
		let meta = (match Codegen.build_metadata ctx.com (TEnumDecl e) with
			| None -> []
			| Some e -> [EBinop ("=",field p path "__meta__", gen_expr ctx e),p]
		) in
		let meta = if ctx.macros then
			(EBinop ("=",field p path "__et__",call p (builtin p "typewrap") [Obj.magic t]),p) :: meta
		else
			meta
		in
		setname :: setconstrs :: meta @ acc
	| TClassDecl c ->
		if c.cl_extern then
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
	| TTypeDecl _ | TAbstractDecl _ ->
		acc

let generate_libs_init = function
	| [] -> []
	| libs ->
		(*
			var @s = $loader.loadprim("std@sys_string",0)();
			var @env = $loader.loadprim("std@get_env",1);
			var @b = if( @s == "Windows" )
				@env("HAXEPATH") + "lib\\"
				else try $loader.loadprim("std@file_contents",1)(@env("HOME")+"/.haxelib") + "/"
				catch e if( @s == "Linux" ) "/usr/lib/haxe/lib/" else "/usr/local/lib/haxe/lib/";
			if( $loader.loadprim("std@sys_is64",0)() ) @s = @s + 64;
			@s = @s + "/"
		*)
		let p = null_pos in
		let es = ident p "@s" in
		let loadp n nargs =
			call p (field p (builtin p "loader") "loadprim") [str p ("std@" ^ n); int p nargs]
		in
		let op o e1 e2 =
			(EBinop (o,e1,e2),p)
		in
		let boot = [
			(EVars [
				"@s",Some (call p (loadp "sys_string" 0) []);
				"@env",Some (loadp "get_env" 1);
				"@b", Some (EIf (op "==" es (str p "Windows"),
					op "+" (call p (ident p "@env") [str p "HAXEPATH"]) (str p "lib\\"),
					Some (ETry (
						op "+" (call p (loadp "file_contents" 1) [op "+" (call p (ident p "@env") [str p "HOME"]) (str p "./haxelib")]) (str p "/"),
						"e",
						(EIf (op "==" es (str p "Linux"),
							str p "/usr/lib/haxe/lib/",
							Some (str p "/usr/local/lib/haxe/lib/")
						),p)
					),p)
				),p);
			],p);
			(EIf (call p (loadp "sys_is64" 0) [],op "=" es (op "+" es (int p 64)),None),p);
			op "=" es (op "+" es (str p "/"));
		] in
		let lpath = field p (builtin p "loader") "path" in
		boot @ List.map (fun dir ->
			let full_path = dir.[0] = '/' || dir.[1] = ':' in
			let dstr = str p dir in
			(*
				// for each lib dir
				$loader.path = $array($loader.path,dir+@s);
			*)
			op "=" lpath (call p (builtin p "array") [op "+" (if full_path then dstr else op "+" (ident p "@b") dstr) (ident p "@s"); lpath])
		) libs

let new_context com ver macros =
	{
		version = ver;
		com = com;
		globals = Hashtbl.create 0;
		curglobal = 0;
		packages = Hashtbl.create 0;
		macros = macros;
		curclass = "$boot";
		curmethod = "$init";
		inits = [];
	}

let header() =
	let p = { psource = "<header>"; pline = 1 } in
	let fields l =
		let rec loop = function
			| [] -> assert false
			| [x] -> ident p x
			| x :: l -> field p (loop l) x
		in
		loop (List.rev l)
	in
	let func pl e =
		(EFunction (pl,(EReturn (Some e),p)),p)
	in
	let inits = [
		"@classes",call p (builtin p "new") [null p];
		"@enum_to_string",func [] (call p (fields ["neko";"Boot";"__enum_str"]) [this p]);
		"@serialize",func [] (call p (fields ["neko";"Boot";"__serialize"]) [this p]);
		"@tag_serialize",func [] (call p (fields ["neko";"Boot";"__tagserialize"]) [this p]);
		"@lazy_error",func ["e"] (call p (builtin p "varargs") [func ["_"] (call p (builtin p "throw") [ident p "e"])]);
		"@default__string",func [] (EBlock [
			EVars ["@s",Some (call p (field p (this p) "toString") [])] ,p;
			EIf ((EBinop ("!=",call p (builtin p "typeof") [ident p "@s"],builtin p "tobject"),p),(EReturn (Some (null p)),p),None),p;
			EReturn (Some (field p (ident p "@s") "__s")),p;
		],p)
	] in
	let inits = inits @ List.map (fun nargs ->
		let args = Array.to_list (Array.init nargs (fun i -> Printf.sprintf "%c" (char_of_int (int_of_char 'a' + i)))) in
		let efun = (EFunction (args,(EBlock [
			(EBinop ("=",(EConst This,p),ident p "@this"),p);
			call p (ident p "@fun") (List.map (ident p) args);
		],p)),p) in
		let eif = EIf ((EBinop ("==",ident p "@fun",null p),p),null p,Some efun) in
		let e = func ["@this";"@fun"] (eif,p) in
		"@closure" ^ string_of_int nargs, e
	) [0;1;2;3;4;5] in
	List.map (fun (v,e)-> EBinop ("=",ident p v,e),p) inits

let build ctx types =
	let packs = List.concat (List.map (gen_package ctx) types) in
	let names = List.fold_left (gen_name ctx) [] types in
	let methods = List.rev (List.fold_left (fun acc t -> gen_type ctx t acc) [] types) in
	let boot = gen_boot ctx in
	let inits = List.map (fun (c,e) ->
		ctx.curclass <- s_type_path c.cl_path;
		ctx.curmethod <- "__init__";
		gen_expr ctx e
	) (List.rev ctx.inits) in
	ctx.inits <- [];
	let vars = List.concat (List.map (gen_static_vars ctx) types) in
	packs @ methods @ boot :: names @ inits @ vars

let generate com =
	let ctx = new_context com (if Common.defined com Define.NekoV2 then 2 else 1) false in
	let t = Common.timer "neko generation" in
	let libs = (EBlock (generate_libs_init com.neko_libs) , { psource = "<header>"; pline = 1; }) in
	let el = build ctx com.types in
	let emain = (match com.main with None -> [] | Some e -> [gen_expr ctx e]) in
	let e = (EBlock ((header()) @ libs :: el @ emain), null_pos) in
	let source = Common.defined com Define.NekoSource in
	let use_nekoc = Common.defined com Define.UseNekoc in
	if not use_nekoc then begin
		let ch = IO.output_channel (open_out_bin com.file) in
		Nbytecode.write ch (Ncompile.compile ctx.version e);
		IO.close_out ch;
	end;
	let command cmd = try com.run_command cmd with _ -> -1 in
	let neko_file = (try Filename.chop_extension com.file with _ -> com.file) ^ ".neko" in
	if source || use_nekoc then begin
		let ch = IO.output_channel (open_out_bin neko_file) in
		Binast.write ch e;
		IO.close_out ch;
	end;
	if use_nekoc && command ("nekoc" ^ (if ctx.version > 1 then " -version " ^ string_of_int ctx.version else "") ^ " \"" ^ neko_file ^ "\"") <> 0 then failwith "Neko compilation failure";
	if source then begin
		if command ("nekoc -p \"" ^ neko_file ^ "\"") <> 0 then failwith "Failed to print neko code";
		Sys.remove neko_file;
		Sys.rename ((try Filename.chop_extension com.file with _ -> com.file) ^ "2.neko") neko_file;
	end;
	t()
