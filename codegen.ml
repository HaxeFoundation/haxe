(*
 *  Haxe Compiler
 *  Copyright (c)2005-2008 Nicolas Cannasse
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
open Common
open Typecore

(* -------------------------------------------------------------------------- *)
(* TOOLS *)

let field e name t p =
	mk (TField (e,name)) t p

let fcall e name el ret p =
	let ft = tfun (List.map (fun e -> e.etype) el) ret in
	mk (TCall (field e name ft p,el)) ret p

let string com str p =
	mk (TConst (TString str)) com.type_api.tstring p

let binop op a b t p =
	mk (TBinop (op,a,b)) t p

let index com e index t p =
	mk (TArray (e,mk (TConst (TInt (Int32.of_int index))) com.type_api.tint p)) t p

(* -------------------------------------------------------------------------- *)
(* REMOTING PROXYS *)

let extend_remoting ctx c t p async prot =
	if c.cl_super <> None then error "Cannot extend several classes" p;
	(* remove forbidden packages *)
	let rules = ctx.com.package_rules in
	ctx.com.package_rules <- PMap.foldi (fun key r acc -> match r with Forbidden -> acc | _ -> PMap.add key r acc) rules PMap.empty;
	(* parse module *)
	let path = (t.tpackage,t.tname) in
	let new_name = (if async then "Async_" else "Remoting_") ^ t.tname in
	(* check if the proxy already exists *)
	let t = (try
		Typeload.load_type_def ctx p (fst path,new_name)
	with
		Error (Module_not_found _,p2) when p == p2 ->
	(* build it *)
	if ctx.com.verbose then print_endline ("Building proxy for " ^ s_type_path path);
	let decls = (try Typeload.parse_module ctx path p with e -> ctx.com.package_rules <- rules; raise e) in
	ctx.com.package_rules <- rules;
	let base_fields = [
		(FVar ("__cnx",None,[],Some (TPNormal { tpackage = ["haxe";"remoting"]; tname = if async then "AsyncConnection" else "Connection"; tparams = [] }),None),p);
		(FFun ("new",None,[APublic],[],{ f_args = ["c",false,None]; f_type = None; f_expr = (EBinop (OpAssign,(EConst (Ident "__cnx"),p),(EConst (Ident "c"),p)),p) }),p);
	] in
	let tvoid = TPNormal { tpackage = []; tname = "Void"; tparams = [] } in
	let build_field is_public acc (f,p) =
		match f with
		| FFun ("new",_,_,_,_) ->
			acc
		| FFun (name,doc,acl,pl,f) when (is_public || List.mem APublic acl) && not (List.mem AStatic acl) ->
			if List.exists (fun (_,_,t) -> t = None) f.f_args then error ("Field " ^ name ^ " type is not complete and cannot be used by RemotingProxy") p;
			let eargs = [EArrayDecl (List.map (fun (a,_,_) -> (EConst (Ident a),p)) f.f_args),p] in
			let ftype = (match f.f_type with Some (TPNormal { tpackage = []; tname = "Void" }) -> None | _ -> f.f_type) in
			let fargs, eargs = if async then match ftype with
				| Some tret -> f.f_args @ ["__callb",true,Some (TPFunction ([tret],tvoid))], eargs @ [EConst (Ident "__callb"),p]
				| _ -> f.f_args, eargs @ [EConst (Ident "null"),p]
			else 
				f.f_args, eargs
			in
			let id = (EConst (String name), p) in
			let id = if prot then id else ECall ((EConst (Ident "__unprotect__"),p),[id]),p in 
			let expr = ECall (
				(EField (
					(ECall ((EField ((EConst (Ident "__cnx"),p),"resolve"),p),[id]),p),
					"call")
				,p),eargs),p
			in
			let expr = if async || ftype = None then expr else (EReturn (Some expr),p) in
			let f = {
				f_args = fargs;
				f_type = if async then None else ftype;
				f_expr = (EBlock [expr],p);
			} in
			(FFun (name,None,[APublic],pl,f),p) :: acc
		| _ -> acc
	in
	let decls = List.map (fun d ->
		match d with
		| EClass c, p when c.d_name = t.tname ->
			let is_public = List.mem HExtern c.d_flags || List.mem HInterface c.d_flags in
			let fields = List.rev (List.fold_left (build_field is_public) base_fields c.d_data) in	
			(EClass { c with d_flags = []; d_name = new_name; d_data = fields },p)
		| _ -> d
	) decls in
	let m = Typeload.type_module ctx (t.tpackage,new_name) decls p in
	try 
		List.find (fun tdecl -> snd (t_path tdecl) = new_name) m.mtypes
	with Not_found ->
		error ("Module " ^ s_type_path path ^ " does not define type " ^ t.tname) p
	) in
	match t with
	| TClassDecl c2 when c2.cl_types = [] -> c.cl_super <- Some (c2,[]);
	| _ -> error "Remoting proxy must be a class without parameters" p

(* -------------------------------------------------------------------------- *)
(* HAXE.RTTI.GENERIC *)

let build_generic ctx c p tl =
	let pack = fst c.cl_path in
	let recurse = ref false in
	let rec check_recursive t =
		match follow t with
		| TInst (c,tl) ->
			if c.cl_kind = KTypeParameter then recurse := true;
			List.iter check_recursive tl;
		| _ ->
			()
	in
	let name = String.concat "_" (snd c.cl_path :: (List.map (fun t ->
		check_recursive t;
		let path = (match follow t with
			| TInst (c,_) -> c.cl_path
			| TEnum (e,_) -> e.e_path
			| _ -> error "Type parameter must be a class or enum instance" p
		) in
		match path with
		| [] , name -> name
		| l , name -> String.concat "_" l ^ "_" ^ name
	) tl)) in
	if !recurse then
		TInst (c,tl) (* build a normal instance *)
	else try
		Typeload.load_normal_type ctx { tpackage = pack; tname = name; tparams = [] } p false
	with Error(Module_not_found path,_) when path = (pack,name) ->
		let m = (try Hashtbl.find ctx.modules (Hashtbl.find ctx.types_module c.cl_path) with Not_found -> assert false) in
		let ctx = { ctx with local_types = m.mtypes @ ctx.local_types } in
		let cg = mk_class (pack,name) c.cl_pos None false in
		let mg = {
			mpath = cg.cl_path;
			mtypes = [TClassDecl cg];
			mimports = [];
		} in
		Hashtbl.add ctx.modules mg.mpath mg;
		let rec loop l1 l2 =
			match l1, l2 with
			| [] , [] -> []
			| (x,TLazy f) :: l1, _ -> loop ((x,(!f)()) :: l1) l2
			| (_,t1) :: l1 , t2 :: l2 -> (t1,t2) :: loop l1 l2
			| _ -> assert false
		in
		let subst = loop c.cl_types tl in
		let rec build_type t =
			match t with
			| TInst ({ cl_kind = KGeneric } as c2,tl2) ->
				(* maybe loop, or generate cascading generics *)
				let _, _, f = ctx.api.build_instance (TClassDecl c2) p in
				f (List.map build_type tl2)
			| _ ->
				try List.assq t subst with Not_found -> Type.map build_type t
		in
		let rec build_expr e =
			let t = build_type e.etype in
			match e.eexpr with
			| TFunction f ->
				{
					eexpr = TFunction {
						tf_args = List.map (fun (n,o,t) -> n, o, build_type t) f.tf_args;
						tf_type = build_type f.tf_type;
						tf_expr = build_expr f.tf_expr;
					};
					etype = t;
					epos = e.epos;
				}
			| TNew (c,tl,el) ->
				let c, tl = (match follow t with TInst (c,tl) -> c, tl | _ -> assert false) in
				{
					eexpr = TNew (c,tl,List.map build_expr el);
					etype = t;
					epos = e.epos;
				};
			| TVars vl ->
				{
					eexpr = TVars (List.map (fun (v,t,eo) ->
						v, build_type t, (match eo with None -> None | Some e -> Some (build_expr e))
					) vl);
					etype = t;
					epos = e.epos;
				}
			(* there's still some 't' lefts in TFor, TMatch and TTry *)
			| _ ->
				Type.map_expr build_expr { e with etype = t }
		in
		let build_field f =
			let t = build_type f.cf_type in
			{ f with cf_type = t; cf_expr = (match f.cf_expr with None -> None | Some e -> Some (build_expr e)) }
		in
		if c.cl_super <> None || c.cl_init <> None || c.cl_dynamic <> None then error "This class can't be generic" p;
		if c.cl_ordered_statics <> [] then error "A generic class can't have static fields" p;
		cg.cl_kind <- KGenericInstance (c,tl);
		cg.cl_constructor <- (match c.cl_constructor with None -> None | Some c -> Some (build_field c));
		cg.cl_implements <- List.map (fun (i,tl) -> i, List.map build_type tl) c.cl_implements;
		cg.cl_ordered_fields <- List.map (fun f ->
			let f = build_field f in
			cg.cl_fields <- PMap.add f.cf_name f cg.cl_fields;
			f
		) c.cl_ordered_fields;
		TInst (cg,[])

(* -------------------------------------------------------------------------- *)
(* HAXE.XML.PROXY *)

let extend_xml_proxy ctx c t file p =
	let t = Typeload.load_type ctx p t in
	let file = (try Common.find_file ctx.com file with Not_found -> file) in
	try
		let rec loop = function
			| Xml.Element (_,attrs,childs) ->
				(try
					let id = List.assoc "id" attrs in
					if PMap.mem id c.cl_fields then error ("Duplicate id " ^ id) p;
					let f = {
						cf_name = id;
						cf_type = t;
						cf_public = true;
						cf_doc = None;
						cf_get = ResolveAccess;
						cf_set = NoAccess;
						cf_params = [];
						cf_expr = None;
					} in
					c.cl_fields <- PMap.add id f c.cl_fields;
				with
					Not_found -> ());
				List.iter loop childs;
			| Xml.PCData _ -> ()
		in
		loop (Xml.parse_file file)
	with
		| Xml.Error e -> error ("XML error " ^ Xml.error e) p
		| Xml.File_not_found f -> error ("XML File not found : " ^ f) p

(* -------------------------------------------------------------------------- *)
(* API EVENTS *)

let build_instance ctx mtype p =
	match mtype with
	| TClassDecl c ->
		c.cl_types , c.cl_path , (match c.cl_kind with KGeneric -> build_generic ctx c p | _ -> (fun t -> TInst (c,t)))
	| TEnumDecl e ->
		e.e_types , e.e_path , (fun t -> TEnum (e,t))
	| TTypeDecl t ->
		t.t_types , t.t_path , (fun tl -> TType(t,tl))

let on_inherit ctx c p h =
	match h with
	| HExtends { tpackage = ["haxe";"remoting"]; tname = "Proxy"; tparams = [TPType(TPNormal t)] } ->
		extend_remoting ctx c t p false true;
		false
	| HExtends { tpackage = ["haxe";"remoting"]; tname = "AsyncProxy"; tparams = [TPType(TPNormal t)] } ->
		extend_remoting ctx c t p true true;
		false
	| HExtends { tpackage = ["mt"]; tname = "AsyncProxy"; tparams = [TPType(TPNormal t)] } ->
		extend_remoting ctx c t p true false;
		false
	| HImplements { tpackage = ["haxe";"rtti"]; tname = "Generic"; tparams = [] } ->
		c.cl_kind <- KGeneric;
		false
	| HExtends { tpackage = ["haxe";"xml"]; tname = "Proxy"; tparams = [TPConst(String file);TPType t] } ->
		extend_xml_proxy ctx c t file p;
		true
	| _ ->
		true

let rec has_rtti c =
	List.exists (function (t,pl) ->
		match t, pl with
		| { cl_path = ["haxe";"rtti"],"Infos" },[] -> true
		| _ -> false
	) c.cl_implements || (match c.cl_super with None -> false | Some (c,_) -> has_rtti c)

let on_generate ctx t =
	match t with
	| TClassDecl c when has_rtti c ->
		let f = mk_field "__rtti" ctx.api.tstring in
		let str = Genxml.gen_type_string ctx.com t in
		f.cf_expr <- Some (mk (TConst (TString str)) f.cf_type c.cl_pos);
		c.cl_ordered_statics <- f :: c.cl_ordered_statics;
		c.cl_statics <- PMap.add f.cf_name f c.cl_statics;
	| _ ->
		()

(* -------------------------------------------------------------------------- *)
(* PER-BLOCK VARIABLES *)

(*
	This algorithm ensure that variables used in loop sub-functions are captured
	by value. It transforms the following expression :

	for( x in array )
		funs.push(function() return x);

	Into the following :

	for( x in array )
		funs.push(function(x) { function() return x; }(x));

	This way, each value is captured independantly.	
*)

let block_vars e =
	let add_var map v d = map := PMap.add v d (!map) in
	let wrap e used =
		match PMap.foldi (fun v _ acc -> v :: acc) used [] with
		| [] -> e
		| vars ->
			mk (TCall (
				(mk (TFunction {
					tf_args = List.map (fun v -> v , false, t_dynamic) vars;
					tf_type = t_dynamic;
					tf_expr = mk (TReturn (Some e)) t_dynamic e.epos;
				}) t_dynamic e.epos),
				List.map (fun v -> mk (TLocal v) t_dynamic e.epos) vars)
			) t_dynamic e.epos
	in
	let rec in_fun vars depth used_locals e =
		match e.eexpr with
		| TLocal v ->
			(try
				if PMap.find v vars = depth then add_var used_locals v depth;				
			with
				Not_found -> ())
		| _ ->
			iter (in_fun vars depth used_locals) e

	and in_loop vars depth e =
		match e.eexpr with
		| TVars l ->
			{ e with eexpr = TVars (List.map (fun (v,t,e) ->
				let e = (match e with None -> None | Some e -> Some (in_loop vars depth e)) in
				add_var vars v depth;
				v, t, e
			) l) }
		| TFor (v,t,i,e1) ->
			let new_vars = PMap.add v depth (!vars) in
			{ e with eexpr = TFor (v,t,in_loop vars depth i,in_loop (ref new_vars) depth e1) }
		| TTry (e1,cases) ->
			let e1 = in_loop vars depth e1 in
			let cases = List.map (fun (v,t,e) ->
				let new_vars = PMap.add v depth (!vars) in
				v , t, in_loop (ref new_vars) depth e
			) cases in
			{ e with eexpr = TTry (e1,cases) }
		| TMatch (e1,t,cases,def) ->
			let e1 = in_loop vars depth e1 in
			let cases = List.map (fun (cl,params,e) ->
				let e = (match params with
					| None -> in_loop vars depth e
					| Some l ->
						let new_vars = List.fold_left (fun acc (v,t) ->
							match v with
							| None -> acc
							| Some name -> PMap.add name depth acc
						) (!vars) l in
						in_loop (ref new_vars) depth e
				) in
				cl , params, e
			) cases in
			let def = (match def with None -> None | Some e -> Some (in_loop vars depth e)) in
			{ e with eexpr = TMatch (e1, t, cases, def) }
		| TBlock l ->
			let new_vars = (ref !vars) in
			map_expr (in_loop new_vars depth) e
		| TFunction _ ->
			let new_vars = !vars in
			let used = ref PMap.empty in
			iter (in_fun new_vars depth used) e;
			let e = wrap e (!used) in
			let new_vars = ref (PMap.foldi (fun v _ acc -> PMap.remove v acc) (!used) new_vars) in
			map_expr (in_loop new_vars (depth + 1)) e
		| _ ->
			map_expr (in_loop vars depth) e
	and out_loop e =
		match e.eexpr with
		| TFor _ | TWhile _ ->
			in_loop (ref PMap.empty) 0 e
		| _ ->
			map_expr out_loop e
	in
	out_loop e

(* -------------------------------------------------------------------------- *)
(* STACK MANAGEMENT EMULATION *)

type stack_context = {
	stack_var : string;
	stack_exc_var : string;
	stack_pos_var : string;
	stack_pos : pos;
	stack_expr : texpr;
	stack_pop : texpr;
	stack_save_pos : texpr;
	stack_restore : texpr list;
	stack_push : tclass -> string -> texpr;
	stack_return : texpr -> texpr;
}

let stack_context_init com stack_var exc_var pos_var tmp_var use_add p =
	let t = com.type_api in
	let st = t.tarray t.tstring in
	let stack_e = mk (TLocal stack_var) st p in
	let exc_e = mk (TLocal exc_var) st p in	
	let stack_pop = fcall stack_e "pop" [] t.tstring p in
	let stack_push c m =
		fcall stack_e "push" [
			if use_add then
				binop OpAdd (string com (s_type_path c.cl_path ^ "::") p) (string com m p) t.tstring p
			else
				string com (s_type_path c.cl_path ^ "::" ^ m) p
		] t.tvoid p
	in
	let stack_return e =
		mk (TBlock [
			mk (TVars [tmp_var, e.etype, Some e]) t.tvoid e.epos;
			stack_pop;
			mk (TReturn (Some (mk (TLocal tmp_var) e.etype e.epos))) e.etype e.epos
		]) e.etype e.epos
	in
	{
		stack_var = stack_var;
		stack_exc_var = exc_var;
		stack_pos_var = pos_var;
		stack_pos = p;
		stack_expr = stack_e;
		stack_pop = stack_pop;
		stack_save_pos = mk (TVars [pos_var, t.tint, Some (field stack_e "length" t.tint p)]) t.tvoid p;
		stack_push = stack_push;
		stack_return = stack_return;
		stack_restore = [
			binop OpAssign exc_e (mk (TArrayDecl []) st p) st p;
			mk (TWhile (
				binop OpGte	(field stack_e "length" t.tint p) (mk (TLocal pos_var) t.tint p) t.tbool p,
				fcall exc_e "unshift" [fcall stack_e "pop" [] t.tstring p] t.tvoid p,
				NormalWhile
			)) t.tvoid p;
			fcall stack_e "push" [index com exc_e 0 t.tstring p] t.tvoid p
		];
	}

let stack_init com use_add =
	stack_context_init com "$s" "$e" "$spos" "$tmp" use_add null_pos

let rec stack_block_loop ctx e =
	match e.eexpr with
	| TFunction _ ->
		e
	| TReturn None | TReturn (Some { eexpr = TConst _ }) | TReturn (Some { eexpr = TLocal _ }) ->
		mk (TBlock [
			ctx.stack_pop;
			e;
		]) e.etype e.epos
	| TReturn (Some e) ->	
		ctx.stack_return (stack_block_loop ctx e)
	| TTry (v,cases) ->
		let v = stack_block_loop ctx v in
		let cases = List.map (fun (n,t,e) ->
			let e = stack_block_loop ctx e in
			let e = (match (mk_block e).eexpr with
				| TBlock l -> mk (TBlock (ctx.stack_restore @ l)) e.etype e.epos
				| _ -> assert false
			) in
			n , t , e
		) cases in
		mk (TTry (v,cases)) e.etype e.epos
	| _ ->
		map_expr (stack_block_loop ctx) e

let stack_block ctx c m e =	
	match (mk_block e).eexpr with
	| TBlock l -> 
		mk (TBlock (
			ctx.stack_push c m ::
			ctx.stack_save_pos :: 
			List.map (stack_block_loop ctx) l
			@ [ctx.stack_pop]
		)) e.etype e.epos
	| _ ->
		assert false

(* -------------------------------------------------------------------------- *)
(* MISC FEATURES *)

let local_find flag vname e =
	let rec loop2 e =
		match e.eexpr with
		| TFunction f ->
			if not flag && not (List.exists (fun (a,_,_) -> a = vname) f.tf_args) then loop2 f.tf_expr
		| TBlock _ ->
			(try
				Type.iter loop2 e;
			with
				Not_found -> ())
		| TVars vl ->
			List.iter (fun (v,t,e) ->
				(match e with
				| None -> ()
				| Some e -> loop2 e);
				if v = vname then raise Not_found;
			) vl
		| TConst TSuper ->
			if vname = "super" then raise Exit
		| TLocal v ->
			if v = vname then raise Exit
		| _ ->
			iter loop2 e
	in
	let rec loop e =
		match e.eexpr with
		| TFunction f ->
			if not (List.exists (fun (a,_,_) -> a = vname) f.tf_args) then loop2 f.tf_expr
		| TBlock _ ->
			(try
				iter loop e;
			with
				Not_found -> ())
		| TVars vl ->
			List.iter (fun (v,t,e) ->
				(match e with
				| None -> ()
				| Some e -> loop e);
				if v = vname then raise Not_found;
			) vl
		| _ ->
			iter loop e
	in
	try
		(if flag then loop2 else loop) e;
		false
	with
		Exit ->
			true

let rec is_volatile t =
	match t with
	| TMono r ->
		(match !r with
		| Some t -> is_volatile t
		| _ -> false)
	| TLazy f ->
		is_volatile (!f())
	| TType (t,tl) ->
		(match t.t_path with
		| ["mt";"flash"],"Volatile" -> true
		| _ -> is_volatile (apply_params t.t_types tl t.t_type))
	| _ ->
		false
