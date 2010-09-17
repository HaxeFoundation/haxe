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
	mk (TConst (TString str)) com.basic.tstring p

let binop op a b t p =
	mk (TBinop (op,a,b)) t p

let index com e index t p =
	mk (TArray (e,mk (TConst (TInt (Int32.of_int index))) com.basic.tint p)) t p

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
		Typeload.load_type_def ctx p { tpackage = fst path; tname = new_name; tparams = []; tsub = None }
	with
		Error (Module_not_found _,p2) when p == p2 ->
	(* build it *)
	if ctx.com.verbose then print_endline ("Building proxy for " ^ s_type_path path);
	let decls = (try Typeload.parse_module ctx path p with e -> ctx.com.package_rules <- rules; raise e) in
	ctx.com.package_rules <- rules;
	let base_fields = [
		(FVar ("__cnx",None,[],[],Some (CTPath { tpackage = ["haxe";"remoting"]; tname = if async then "AsyncConnection" else "Connection"; tparams = []; tsub = None }),None),p);
		(FFun ("new",None,[],[APublic],[],{ f_args = ["c",false,None,None]; f_type = None; f_expr = (EBinop (OpAssign,(EConst (Ident "__cnx"),p),(EConst (Ident "c"),p)),p) }),p);
	] in
	let tvoid = CTPath { tpackage = []; tname = "Void"; tparams = []; tsub = None } in
	let build_field is_public acc (f,p) =
		match f with
		| FFun ("new",_,_,_,_,_) ->
			acc
		| FFun (name,doc,meta,acl,pl,f) when (is_public || List.mem APublic acl) && not (List.mem AStatic acl) ->
			if List.exists (fun (_,_,t,_) -> t = None) f.f_args then error ("Field " ^ name ^ " type is not complete and cannot be used by RemotingProxy") p;
			let eargs = [EArrayDecl (List.map (fun (a,_,_,_) -> (EConst (Ident a),p)) f.f_args),p] in
			let ftype = (match f.f_type with Some (CTPath { tpackage = []; tname = "Void" }) -> None | _ -> f.f_type) in
			let fargs, eargs = if async then match ftype with
				| Some tret -> f.f_args @ ["__callb",true,Some (CTFunction ([tret],tvoid)),None], eargs @ [EConst (Ident "__callb"),p]
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
			(FFun (name,None,[],[APublic],pl,f),p) :: acc
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

let rec build_generic ctx c p tl =
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
		Typeload.load_instance ctx { tpackage = pack; tname = name; tparams = []; tsub = None } p false
	with Error(Module_not_found path,_) when path = (pack,name) ->
		let m = (try Hashtbl.find ctx.g.modules (Hashtbl.find ctx.g.types_module c.cl_path) with Not_found -> assert false) in
		let ctx = { ctx with local_types = m.mtypes @ ctx.local_types } in
		let cg = mk_class (pack,name) c.cl_pos in
		let mg = {
			mpath = cg.cl_path;
			mtypes = [TClassDecl cg];
		} in
		Hashtbl.add ctx.g.modules mg.mpath mg;
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
				let _, _, f = ctx.g.do_build_instance ctx (TClassDecl c2) p in
				f (List.map build_type tl2)
			| _ ->
				try List.assq t subst with Not_found -> Type.map build_type t
		in
		let rec build_expr e = map_expr_type build_expr build_type e in
		let build_field f =
			let t = build_type f.cf_type in
			{ f with cf_type = t; cf_expr = (match f.cf_expr with None -> None | Some e -> Some (build_expr e)) }
		in
		if c.cl_init <> None || c.cl_dynamic <> None then error "This class can't be generic" p;
		if c.cl_ordered_statics <> [] then error "A generic class can't have static fields" p;
		cg.cl_super <- (match c.cl_super with
			| None -> None
			| Some (cs,pl) ->
				(match apply_params c.cl_types tl (TInst (cs,pl)) with
				| TInst (cs,pl) when cs.cl_kind = KGeneric ->
					(match build_generic ctx cs p pl with
					| TInst (cs,pl) -> Some (cs,pl)
					| _ -> assert false)
				| TInst (cs,pl) -> Some (cs,pl)
				| _ -> assert false)
		);
		cg.cl_kind <- KGenericInstance (c,tl);
		cg.cl_interface <- c.cl_interface;
		cg.cl_constructor <- (match c.cl_constructor with None -> None | Some c -> Some (build_field c));
		cg.cl_implements <- List.map (fun (i,tl) ->
			(match build_type (TInst (i, List.map build_type tl)) with
			| TInst (i,tl) -> i, tl
			| _ -> assert false)
		) c.cl_implements;
		cg.cl_ordered_fields <- List.map (fun f ->
			let f = build_field f in
			cg.cl_fields <- PMap.add f.cf_name f cg.cl_fields;
			f
		) c.cl_ordered_fields;
		TInst (cg,[])

(* -------------------------------------------------------------------------- *)
(* HAXE.XML.PROXY *)

let extend_xml_proxy ctx c t file p =
	let t = Typeload.load_complex_type ctx p t in
	let file = (try Common.find_file ctx.com file with Not_found -> file) in
	let used = ref PMap.empty in
	let print_results() =
		PMap.iter (fun id used ->
			if not used then ctx.com.warning (id ^ " is not used") p;
		) (!used)
	in
	let check_used = Common.defined ctx.com "check-xml-proxy" in
	if check_used then ctx.g.hook_generate <- print_results :: ctx.g.hook_generate;
	try
		let rec loop = function
			| Xml.Element (_,attrs,childs) ->
				(try
					let id = List.assoc "id" attrs in
					if PMap.mem id c.cl_fields then error ("Duplicate id " ^ id) p;
					let t = if not check_used then t else begin
						used := PMap.add id false (!used);
						let ft() = used := PMap.add id true (!used); t in
						TLazy (ref ft)
					end in
					let f = {
						cf_name = id;
						cf_type = t;
						cf_public = true;
						cf_doc = None;
						cf_meta = no_meta;
						cf_kind = Var { v_read = AccResolve; v_write = AccNo };
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
(* BUILD META DATA OBJECT *)

let build_metadata com t =
	let api = com.basic in
	let p, meta, fields, statics = (match t with
		| TClassDecl c ->
			let fields = List.map (fun f -> f.cf_name,f.cf_meta()) (c.cl_ordered_fields @ (match c.cl_constructor with None -> [] | Some f -> [{ f with cf_name = "_" }])) in
			let statics =  List.map (fun f -> f.cf_name,f.cf_meta()) c.cl_ordered_statics in
			(c.cl_pos, ["",c.cl_meta()],fields,statics)
		| TEnumDecl e ->
			(e.e_pos, ["",e.e_meta()],List.map (fun n -> n, (PMap.find n e.e_constrs).ef_meta()) e.e_names, [])
		| TTypeDecl t ->
			(t.t_pos, ["",t.t_meta()],(match follow t.t_type with TAnon a -> PMap.fold (fun f acc -> (f.cf_name,f.cf_meta()) :: acc) a.a_fields [] | _ -> []),[])
	) in
	let filter l = 
		let l = List.map (fun (n,ml) -> n, List.filter (fun (m,_) -> m.[0] <> ':') ml) l in
		List.filter (fun (_,ml) -> ml <> []) l
	in
	let meta, fields, statics = filter meta, filter fields, filter statics in
	let make_meta_field ml =
		mk (TObjectDecl (List.map (fun (f,l) -> 
			f, mk (match l with [] -> TConst TNull | _ -> TArrayDecl l) (api.tarray t_dynamic) p
		) ml)) (api.tarray t_dynamic) p
	in
	let make_meta l =
		mk (TObjectDecl (List.map (fun (f,ml) -> f,make_meta_field ml) l)) t_dynamic p
	in
	if meta = [] && fields = [] && statics = [] then
		None
	else
		let meta_obj = [] in
		let meta_obj = (if fields = [] then meta_obj else ("fields",make_meta fields) :: meta_obj) in		
		let meta_obj = (if statics = [] then meta_obj else ("statics",make_meta statics) :: meta_obj) in
		let meta_obj = (try ("obj", make_meta_field (List.assoc "" meta)) :: meta_obj with Not_found -> meta_obj) in
		Some (mk (TObjectDecl meta_obj) t_dynamic p)		

(* -------------------------------------------------------------------------- *)
(* API EVENTS *)

let build_instance ctx mtype p =
	match mtype with
	| TClassDecl c ->
		let ft = (fun pl ->
			match c.cl_kind with
			| KGeneric ->
				let r = exc_protect (fun r ->
					let t = mk_mono() in
					r := (fun() -> t);
					unify_raise ctx (build_generic ctx c p pl) t p;
					t
				) in
				delay ctx (fun() -> ignore ((!r)()));
				TLazy r
			| _ ->
				TInst (c,pl)
		) in
		c.cl_types , c.cl_path , ft
	| TEnumDecl e ->
		e.e_types , e.e_path , (fun t -> TEnum (e,t))
	| TTypeDecl t ->
		t.t_types , t.t_path , (fun tl -> TType(t,tl))

let on_inherit ctx c p h =
	match h with
	| HExtends { tpackage = ["haxe";"remoting"]; tname = "Proxy"; tparams = [TPType(CTPath t)] } ->
		extend_remoting ctx c t p false true;
		false
	| HExtends { tpackage = ["haxe";"remoting"]; tname = "AsyncProxy"; tparams = [TPType(CTPath t)] } ->
		extend_remoting ctx c t p true true;
		false
	| HExtends { tpackage = ["mt"]; tname = "AsyncProxy"; tparams = [TPType(CTPath t)] } ->
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
	| TClassDecl c ->
		let meta = ref (c.cl_meta()) in
		List.iter (fun m ->
			match m with
			| ":native",[{ eexpr = TConst (TString name) } as e] ->				
				meta := (":real",[{ e with eexpr = TConst (TString (s_type_path c.cl_path)) }]) :: !meta;
				c.cl_meta <- (fun() -> !meta);
				c.cl_path <- parse_path name;
			| _ -> ()
		) (!meta);
		if has_rtti c && not (PMap.mem "__rtti" c.cl_statics) then begin
			let f = mk_field "__rtti" ctx.t.tstring in
			let str = Genxml.gen_type_string ctx.com t in
			f.cf_expr <- Some (mk (TConst (TString str)) f.cf_type c.cl_pos);
			c.cl_ordered_statics <- f :: c.cl_ordered_statics;
			c.cl_statics <- PMap.add f.cf_name f c.cl_statics;
		end;
		if not ctx.in_macro then List.iter (fun f ->
			match f.cf_kind with
			| Method MethMacro -> 
				c.cl_statics <- PMap.remove f.cf_name c.cl_statics;
				c.cl_ordered_statics <- List.filter (fun f2 -> f != f2) c.cl_ordered_statics;
			| _ -> ()
		) c.cl_ordered_statics;
		(match build_metadata ctx.com t with
		| None -> ()
		| Some e -> 
			let f = mk_field "__meta__" t_dynamic in
			f.cf_expr <- Some e;
			c.cl_ordered_statics <- f :: c.cl_ordered_statics;
			c.cl_statics <- PMap.add f.cf_name f c.cl_statics);
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

let block_vars com e =

	let uid = ref 0 in
	let gen_unique() =
		incr uid;
		"$t" ^ string_of_int !uid;
	in

	let t = com.basic in

	let rec mk_init v vt vtmp pos =
		let at = t.tarray vt in
		mk (TVars [v,at,Some (mk (TArrayDecl [mk (TLocal vtmp) vt pos]) at pos)]) t.tvoid pos

	and wrap used e =
		match e.eexpr with
		| TVars vl ->
			let vl = List.map (fun (v,vt,ve) ->
				if PMap.mem v used then begin
					let vt = t.tarray vt in
					v, vt, Some (mk (TArrayDecl (match ve with None -> [] | Some e -> [wrap used e])) vt e.epos)
				end else
					v, vt, (match ve with None -> None | Some e -> Some (wrap used e))
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
			(match com.platform with
			| Cpp -> e
			| _ ->
				let args = List.map (fun (v,t) -> v, None, t) vars in
				mk (TCall (
					(mk (TFunction {
						tf_args = args;
						tf_type = e.etype;
						tf_expr = mk (TReturn (Some e)) e.etype e.epos;
					}) (TFun (fun_args args,e.etype)) e.epos),
					List.map (fun (v,t) -> mk (TLocal v) t e.epos) vars)
				) e.etype e.epos)
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
	and all_vars e =
		let vars = ref PMap.empty in
		let used = ref PMap.empty in
		let depth = ref 0 in
		let rec collect_vars = function
		| Block f ->
			let old = !vars in
			f collect_vars;
			vars := old;
		| Loop f ->
			let old = !vars in
			f collect_vars;
			vars := old;
		| Function f ->
			incr depth;
			f collect_vars;
			decr depth;
		| Declare (v,t) ->
			vars := PMap.add v (!depth,t) !vars;
		| Use v ->
			try
				let d, t = PMap.find v (!vars) in
				if d <> !depth then used := PMap.add v t !used;
			with Not_found -> ()
		in
	local_usage collect_vars e;
	if PMap.is_empty !used then e else wrap !used e
	in
	match com.platform with
	| Neko | Php | Cross -> e
	| Cpp -> all_vars e
	| _ -> out_loop e

(* -------------------------------------------------------------------------- *)
(* CHECK LOCAL VARS INIT *)

let check_local_vars_init e =
	let intersect vl1 vl2 =
		PMap.mapi (fun v t -> t && PMap.find v vl2) vl1
	in
	let join vars cvars =
		List.iter (fun v -> vars := intersect !vars v) cvars
	in
	let restore vars old_vars declared =
		(* restore variables declared in this block to their previous state *)
		vars := List.fold_left (fun acc v ->
			try	PMap.add v (PMap.find v old_vars) acc with Not_found -> PMap.remove v acc
		) !vars declared;
	in
	let declared = ref [] in
	let rec loop vars e =
		match e.eexpr with
		| TLocal name ->
			let init = (try PMap.find name !vars with Not_found -> true) in
			if not init then error ("Local variable " ^ name ^ " used without being initialized") e.epos;
		| TVars vl ->
			List.iter (fun (v,_,eo) ->
				let init = (match eo with None -> false | Some e -> loop vars e; true) in
				declared := v :: !declared;
				vars := PMap.add v init !vars
			) vl
		| TBlock el ->
			let old = !declared in
			let old_vars = !vars in
			declared := [];
			List.iter (loop vars) el;
			restore vars old_vars (List.rev !declared);
			declared := old;
		| TBinop (OpAssign,{ eexpr = TLocal name },e) ->
			loop vars e;
			vars := PMap.add name true !vars
		| TIf (e1,e2,eo) ->
			loop vars e1;
			let vbase = !vars in
			loop vars e2;
			(match eo with
			| None -> vars := vbase
			| Some e ->
				let v1 = !vars in
				vars := vbase;
				loop vars e;
				vars := intersect !vars v1)
		| TWhile (cond,e,flag) ->
			(match flag with
			| NormalWhile ->
				loop vars cond;
				let old = !vars in
				loop vars e;
				vars := old;
			| DoWhile ->
				loop vars e;
				loop vars cond)
		| TFor (v,_,it,e) ->
			loop vars it;
			let old = !vars in
			vars := PMap.add v true !vars;
			loop vars e;
			vars := old;
		| TFunction f ->
			let old = !vars in
			vars := List.fold_left (fun acc (v,_,_) -> PMap.add v true acc) !vars f.tf_args;
			loop vars f.tf_expr;
			vars := old;
		| TTry (e,catches) ->
			let cvars = List.map (fun (v,_,e) ->
				let old = !vars in
				loop vars e;
				let v = !vars in
				vars := old;
				v
			) catches in
			loop vars e;
			join vars cvars;
		| TSwitch (e,cases,def) ->
			loop vars e;
			let cvars = List.map (fun (ec,e) ->
				let old = !vars in
				List.iter (loop vars) ec;
				vars := old;
				loop vars e;
				let v = !vars in
				vars := old;
				v
			) cases in
			(match def with
			| None -> ()
			| Some e ->
				loop vars e;
				join vars cvars)
		| TMatch (e,_,cases,def) ->
			loop vars e;
			let old = !vars in
			let cvars = List.map (fun (_,vl,e) ->
				vars := old;
				let tvars = (match vl with
					| None -> []
					| Some vl -> List.map (fun (v,_) -> match v with None -> "" | Some v -> vars := PMap.add v true !vars; v) vl
				) in
				loop vars e;
				restore vars old tvars;
				!vars
			) cases in
			(match def with None -> () | Some e -> vars := old; loop vars e);
			join vars cvars
		(* mark all reachable vars as initialized, since we don't exit the block  *)
		| TBreak | TContinue | TReturn None ->
			vars := PMap.map (fun _ -> true) !vars
		| TThrow e | TReturn (Some e) ->
			loop vars e;
			vars := PMap.map (fun _ -> true) !vars
		| _ ->
			Type.iter (loop vars) e
	in
	loop (ref PMap.empty) e;
	e

(* -------------------------------------------------------------------------- *)
(* POST PROCESS *)

let post_process ctx filters tfilters =
	List.iter (fun t ->
		List.iter (fun f -> f t) tfilters;
		match t with
		| TClassDecl c ->
			let process_field f =
				match f.cf_expr with
				| None -> ()
				| Some e ->
					f.cf_expr <- Some (List.fold_left (fun e f -> f e) e filters)
			in
			List.iter process_field c.cl_ordered_fields;
			List.iter process_field c.cl_ordered_statics;
			(match c.cl_constructor with
			| None -> ()
			| Some f -> process_field f);
			(match c.cl_init with
			| None -> ()
			| Some e ->
				c.cl_init <- Some (List.fold_left (fun e f -> f e) e filters));
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
	let t = com.basic in
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
(* FIX OVERRIDES *)

(*
	on some platforms which doesn't support type parameters, we must have the
	exact same type for overriden/implemented function as the original one
*)
let fix_override c f fd =
	c.cl_fields <- PMap.remove f.cf_name c.cl_fields;
	let rec find_field c interf =
		try
			(match c.cl_super with
			| None ->
				raise Not_found
			| Some (c,_) ->
				find_field c false)
		with Not_found -> try
			let rec loop = function
				| [] ->
					raise Not_found
				| (c,_) :: l ->
					try
						find_field c true
					with
						Not_found -> loop l
			in
			loop c.cl_implements
		with Not_found ->
			interf, PMap.find f.cf_name c.cl_fields
	in
	let f2 = (try Some (find_field c true) with Not_found -> None) in
	let f = (match f2 with
		| Some (interf,f2) ->
			let targs, tret = (match follow f2.cf_type with TFun (args,ret) -> args, ret | _ -> assert false) in
			let fd2 = { fd with tf_args = List.map2 (fun (n,c,t) (_,_,t2) -> (n,c,t2)) fd.tf_args targs; tf_type = tret } in
			let fde = (match f.cf_expr with None -> assert false | Some e -> e) in
			{ f with cf_expr = Some { fde with eexpr = TFunction fd2 } }
		| _ -> f
	) in
	c.cl_fields <- PMap.add f.cf_name f c.cl_fields;
	f

let fix_overrides com t =
	match com.platform, t with
	| Flash9, TClassDecl c ->
		c.cl_ordered_fields <- List.map (fun f ->
			match f.cf_expr, f.cf_kind with
			| Some { eexpr = TFunction fd }, Method (MethNormal | MethInline) ->
				fix_override c f fd
			| _ ->
				f
		) c.cl_ordered_fields
	| _ ->
		()

(* -------------------------------------------------------------------------- *)
(* MISC FEATURES *)

(*
	Tells if we can find a local var in an expression or inside a sub closure
*)
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
	mk (TIf (mk (TBinop (OpEq,ve,mk (TConst TNull) t p)) ctx.basic.tbool p, mk (TBinop (OpAssign,ve,mk (TConst c) t p)) t p,None)) ctx.basic.tvoid p

let bytes_serialize data =
	let b64 = "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789%:" in
	let tbl = Array.init (String.length b64) (fun i -> String.get b64 i) in
	let str = Base64.str_encode ~tbl data in
	"s" ^ string_of_int (String.length str) ^ ":" ^ str

(*
	Tells if the constructor might be called without any issue whatever its parameters
*)
let rec constructor_side_effects e =
	match e.eexpr with
	| TBinop (op,_,_) when op <> OpAssign ->
		true
	| TUnop _ | TArray _ | TField _ | TCall _ | TNew _ | TFor _ | TWhile _ | TSwitch _ | TMatch _ | TReturn _ | TThrow _ | TClosure _ ->
		true
	| TBinop _ | TTry _ | TIf _ | TBlock _ | TVars _
	| TFunction _ | TArrayDecl _ | TObjectDecl _
	| TParenthesis _ | TTypeExpr _ | TEnumField _ | TLocal _
	| TConst _ | TContinue | TBreak | TCast _ ->
		try
			Type.iter (fun e -> if constructor_side_effects e then raise Exit) e;
			false;
		with Exit ->
			true

(*
	Make a dump of the full typed AST of all types
*)
let dump_types com =
	let s_type = s_type (Type.print_context()) in
	let params = function [] -> "" | l -> Printf.sprintf "<%s>" (String.concat "," (List.map (fun (n,t) -> n ^ " : " ^ s_type t) l)) in
	let rec create acc = function
		| [] -> ()
		| d :: l ->
			let dir = String.concat "/" (List.rev (d :: acc)) in
			if not (Sys.file_exists dir) then Unix.mkdir dir 0o755;
			create (d :: acc) l
	in
	List.iter (fun mt ->
		let path = Type.t_path mt in
		let dir = "dump" :: fst path in
		create [] dir;
		let ch = open_out (String.concat "/" dir ^ "/" ^ snd path ^ ".dump") in
		let buf = Buffer.create 0 in
		let print fmt = Printf.kprintf (fun s -> Buffer.add_string buf s) fmt in
		(match mt with
		| Type.TClassDecl c ->
			let print_field stat f =
				print "\t%s%s%s%s" (if stat then "static " else "") (if f.cf_public then "public " else "") f.cf_name (params f.cf_params);
				print "(%s) : %s" (s_kind f.cf_kind) (s_type f.cf_type);
				(match f.cf_expr with
				| None -> ()
				| Some e -> print "\n\n\t = %s" (Type.s_expr s_type e));
				print ";\n\n";
			in
			print "%s%s%s %s%s" (if c.cl_private then "private " else "") (if c.cl_extern then "extern " else "") (if c.cl_interface then "interface" else "class") (s_type_path path) (params c.cl_types);
			(match c.cl_super with None -> () | Some (c,pl) -> print " extends %s" (s_type (TInst (c,pl))));
			List.iter (fun (c,pl) -> print " implements %s" (s_type (TInst (c,pl)))) c.cl_implements;
			(match c.cl_dynamic with None -> () | Some t -> print " implements Dynamic<%s>" (s_type t));
			(match c.cl_array_access with None -> () | Some t -> print " implements ArrayAccess<%s>" (s_type t));
			print "{\n";
			(match c.cl_constructor with
			| None -> ()
			| Some f -> print_field false f);
			List.iter (print_field false) c.cl_ordered_fields;
			List.iter (print_field true) c.cl_ordered_statics;
			print "}";
		| Type.TEnumDecl e ->
			print "%s%senum %s%s {\n" (if e.e_private then "private " else "") (if e.e_extern then "extern " else "") (s_type_path path) (params e.e_types);
			List.iter (fun n ->
				let f = PMap.find n e.e_constrs in
				print "\t%s : %s;\n" f.ef_name (s_type f.ef_type);
			) e.e_names;
			print "}"
		| Type.TTypeDecl t ->
			print "%stype %s%s = %s" (if t.t_private then "private " else "") (s_type_path path) (params t.t_types) (s_type t.t_type);
		);
		output_string ch (Buffer.contents buf);
		close_out ch
	) com.types

(*
	Build a default safe-cast expression :
	{ var $t = <e>; if( Std.is($t,<t>) ) $t else throw "Class cast error"; }
*)
let default_cast ?(vtmp="$t") com e texpr t p =
	let api = com.basic in
	let mk_texpr = function
		| TClassDecl c -> TAnon { a_fields = PMap.empty; a_status = ref (Statics c) }
		| TEnumDecl e -> TAnon { a_fields = PMap.empty; a_status = ref (EnumStatics e) }
		| TTypeDecl _ -> assert false
	in	
	let var = mk (TVars [(vtmp,e.etype,Some e)]) api.tvoid p in
	let vexpr = mk (TLocal vtmp) e.etype p in
	let texpr = mk (TTypeExpr texpr) (mk_texpr texpr) p in
	let std = (try List.find (fun t -> t_path t = ([],"Std")) com.types with Not_found -> assert false) in
	let std = mk (TTypeExpr std) (mk_texpr std) p in
	let is = mk (TField (std,"is")) (tfun [t_dynamic;t_dynamic] api.tbool) p in
	let is = mk (TCall (is,[vexpr;texpr])) api.tbool p in
	let exc = mk (TThrow (mk (TConst (TString "Class cast error")) api.tstring p)) t p in
	let check = mk (TIf (is,mk (TCast (vexpr,None)) t p,Some exc)) t p in
	mk (TBlock [var;check;vexpr]) t p
