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

let concat e1 e2 =
	let e = (match e1.eexpr, e2.eexpr with
		| TBlock el1, TBlock el2 -> TBlock (el1@el2)
		| TBlock el, _ -> TBlock (el @ [e2])
		| _, TBlock el -> TBlock (e1 :: el)
		| _ , _ -> TBlock [e1;e2]
	) in
	mk e e2.etype (punion e1.epos e2.epos)

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
		(FFun ("new",None,[APublic],[],{ f_args = ["c",false,None,None]; f_type = None; f_expr = (EBinop (OpAssign,(EConst (Ident "__cnx"),p),(EConst (Ident "c"),p)),p) }),p);
	] in
	let tvoid = TPNormal { tpackage = []; tname = "Void"; tparams = [] } in
	let build_field is_public acc (f,p) =
		match f with
		| FFun ("new",_,_,_,_) ->
			acc
		| FFun (name,doc,acl,pl,f) when (is_public || List.mem APublic acl) && not (List.mem AStatic acl) ->
			if List.exists (fun (_,_,t,_) -> t = None) f.f_args then error ("Field " ^ name ^ " type is not complete and cannot be used by RemotingProxy") p;
			let eargs = [EArrayDecl (List.map (fun (a,_,_,_) -> (EConst (Ident a),p)) f.f_args),p] in
			let ftype = (match f.f_type with Some (TPNormal { tpackage = []; tname = "Void" }) -> None | _ -> f.f_type) in
			let fargs, eargs = if async then match ftype with
				| Some tret -> f.f_args @ ["__callb",true,Some (TPFunction ([tret],tvoid)),None], eargs @ [EConst (Ident "__callb"),p]
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
(* LOCAL VARIABLES USAGE *)

type usage =
	| Block of ((usage -> unit) -> unit)
	| Loop of ((usage -> unit) -> unit)
	| Function of ((usage -> unit) -> unit)
	| Declare of string * t
	| Use of string

let rec local_usage f e =
	match e.eexpr with
	| TLocal v ->
		f (Use v)
	| TVars l ->
		List.iter (fun (v,t,e) ->
			(match e with None -> () | Some e -> local_usage f e);
			f (Declare (v,t));
		) l
	| TFunction tf ->
		let cc f =
			List.iter (fun (n,_,t) -> f (Declare (n,t))) tf.tf_args;
			local_usage f tf.tf_expr;
		in
		f (Function cc)		
	| TBlock l ->
		f (Block (fun f -> List.iter (local_usage f) l))		
	| TFor (v,t,it,e) ->
		local_usage f it;
		f (Loop (fun f ->
			f (Declare (v,t));
			local_usage f e;
		))
	| TWhile _ ->
		f (Loop (fun f ->
			iter (local_usage f) e
		))
	| TTry (e,catchs) ->
		local_usage f e;
		List.iter (fun (v,t,e) ->
			f (Block (fun f ->
				f (Declare (v,t));
				local_usage f e;
			))
		) catchs;
	| TMatch (e,_,cases,def) ->
		local_usage f e;
		List.iter (fun (_,vars,e) ->
			let cc f = 
				(match vars with
				| None -> ()
				| Some l ->	List.iter (fun (vo,t) -> match vo with None -> () | Some v -> f (Declare (v,t))) l);
				local_usage f e;
			in
			f (Block cc)
		) cases;
		(match def with None -> () | Some e -> local_usage f e);
	| _ ->
		iter (local_usage f) e

(* -------------------------------------------------------------------------- *)
(* PER-BLOCK VARIABLES *)

(*
	This algorithm ensure that variables used in loop sub-functions are captured
	by value. It transforms the following expression :

	for( x in array )
		funs.push(function() return x++);	

	Into the following :

	for( _x in array ) {
		var x = [_x];
		funs.push(function(x) { function() return x[0]++; }(x));
	}

	This way, each value is captured independantly.
*)

let block_vars ctx e =

	let uid = ref 0 in
	let gen_unique() =
		incr uid;
		"$t" ^ string_of_int !uid;
	in

	let t = ctx.type_api in

	let rec mk_init v vt vtmp pos =
		let at = t.tarray vt in
		mk (TVars [v,at,Some (mk (TArrayDecl [mk (TLocal vtmp) vt pos]) at pos)]) t.tvoid pos

	and wrap used e =
		match e.eexpr with
		| TVars vl ->
			let vl = List.map (fun (v,vt,e) ->
				if PMap.mem v used then begin
					let vt = t.tarray vt in					
					v, vt, (match e with None -> None | Some e -> Some (mk (TArrayDecl [wrap used e]) (t.tarray e.etype) e.epos))
				end else
					v, vt, (match e with None -> None | Some e -> Some (wrap used e))
			) vl in
			{ e with eexpr = TVars vl }
		| TLocal v when PMap.mem v used ->
			mk (TArray ({ e with etype = t.tarray e.etype },mk (TConst (TInt 0l)) t.tint e.epos)) e.etype e.epos
		| TFor (v,vt,it,expr) when PMap.mem v used ->			
			let vtmp = gen_unique() in
			let it = wrap used it in
			let expr = wrap used expr in
			mk (TFor (vtmp,vt,it,concat (mk_init v vt vtmp e.epos) expr)) e.etype e.epos
		| TTry (expr,catchs) ->
			let catchs = List.map (fun (v,t,e) ->
				let e = wrap used e in
				if PMap.mem v used then
					let vtmp = gen_unique()	in				
					vtmp, t, concat (mk_init v t vtmp e.epos) e
				else
					v, t, e
			) catchs in
			mk (TTry (wrap used expr,catchs)) e.etype e.epos
		| TMatch (expr,enum,cases,def) ->
			let cases = List.map (fun (il,vars,e) ->
				let pos = e.epos in
				let e = ref (wrap used e) in
				let vars = match vars with
					| None -> None
					| Some l ->
						Some (List.map (fun (vo,vt) ->
							match vo with
							| Some v when PMap.mem v used ->
								let vtmp = gen_unique() in
								e := concat (mk_init v vt vtmp pos) !e;
								Some vtmp, vt
							| _ -> vo, vt
						) l)
				in
				il, vars, !e
			) cases in
			let def = match def with None -> None | Some e -> Some (wrap used e) in
			mk (TMatch (wrap used expr,enum,cases,def)) e.etype e.epos
		| TFunction f ->
			(* 
				list variables that are marked as used, but also used in that
				function and which are not declared inside it !
			*)
			let fused = ref PMap.empty in
			let tmp_used = ref (PMap.foldi PMap.add used PMap.empty) in
			let rec browse = function
				| Block f | Loop f | Function f -> f browse
				| Use v -> 
					(try 
						fused := PMap.add v (PMap.find v !tmp_used) !fused;
					with Not_found ->
						())
				| Declare (v,_) ->
					tmp_used := PMap.remove v !tmp_used
			in
			local_usage browse e;			
			let vars = PMap.foldi (fun v vt acc -> (v,t.tarray vt) :: acc) !fused [] in
			(* in case the variable has been marked as used in a parallel scope... *)
			let fexpr = ref (wrap used f.tf_expr) in
			let fargs = List.map (fun (v,o,vt) ->
				if PMap.mem v used then
					let vtmp = gen_unique() in
					fexpr := concat (mk_init v vt vtmp e.epos) !fexpr;
					vtmp, o, vt
				else
					v, o, vt
			) f.tf_args in
			let e = { e with eexpr = TFunction { f with tf_args = fargs; tf_expr = !fexpr } } in			
			let args = List.map (fun (v,t) -> v, None, t) vars in
			mk (TCall (
				(mk (TFunction {
					tf_args = args;
					tf_type = e.etype;
					tf_expr = mk (TReturn (Some e)) e.etype e.epos;
				}) (TFun (fun_args args,e.etype)) e.epos),
				List.map (fun (v,t) -> mk (TLocal v) t e.epos) vars)
			) e.etype e.epos
		| _ ->
			map_expr (wrap used) e

	and out_loop e =
		match e.eexpr with
		| TFor _ | TWhile _ ->
			(*
				collect variables that are declared in loop but used in subfunctions
			*)
			let vars = ref PMap.empty in
			let used = ref PMap.empty in
			let depth = ref 0 in
			let rec collect_vars in_loop = function
				| Block f ->
					let old = !vars in
					f (collect_vars in_loop);
					vars := old;
				| Loop f ->
					let old = !vars in
					f (collect_vars true);
					vars := old;
				| Function f ->
					incr depth;
					f (collect_vars false);
					decr depth;
				| Declare (v,t) ->
					if in_loop then vars := PMap.add v (!depth,t) !vars;
				| Use v ->
					try
						let d, t = PMap.find v (!vars) in
						if d <> !depth then used := PMap.add v t !used;
					with Not_found ->
						()
			in
			local_usage (collect_vars false) e;
			if PMap.is_empty !used then e else wrap !used e
		| _ ->
			map_expr out_loop e
	in
	match ctx.platform with
	| Neko | Cross -> e
	| _ -> out_loop e

let post_process ctx =
	List.iter (function
		| TClassDecl c ->
			let process_field f =
				match f.cf_expr with
				| None -> ()
				| Some e -> f.cf_expr <- Some (block_vars ctx e)
			in
			List.iter process_field c.cl_ordered_fields;
			List.iter process_field c.cl_ordered_statics;
			(match c.cl_constructor with
			| None -> ()
			| Some f -> process_field f);
			(match c.cl_init with
			| None -> ()
			| Some e -> c.cl_init <- Some (block_vars ctx e));
		| TEnumDecl _ -> ()
		| TTypeDecl _ -> ()
	) ctx.types

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

let set_default ctx a c t p =
	let ve = mk (TLocal a) t p in
	mk (TIf (mk (TBinop (OpEq,ve,mk (TConst TNull) t p)) ctx.type_api.tbool p, mk (TBinop (OpAssign,ve,mk (TConst c) t p)) t p,None)) ctx.type_api.tvoid p
