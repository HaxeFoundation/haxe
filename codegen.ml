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

let mk_parent e =
	mk (TParenthesis e) e.etype e.epos

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

let type_constant com c p =
	let t = com.basic in
	match c with
	| Int s ->
		if String.length s > 10 && String.sub s 0 2 = "0x" then error "Invalid hexadecimal integer" p;
		(try
			mk (TConst (TInt (Int32.of_string s))) t.tint p
		with
			_ -> mk (TConst (TFloat s)) t.tfloat p)
	| Float f -> mk (TConst (TFloat f)) t.tfloat p
	| String s -> mk (TConst (TString s)) t.tstring p
	| Ident "true" -> mk (TConst (TBool true)) t.tbool p
	| Ident "false" -> mk (TConst (TBool false)) t.tbool p
	| Ident "null" -> mk (TConst TNull) (t.tnull (mk_mono())) p
	| Ident t -> error ("Invalid constant :  " ^ t) p
	| Regexp _ -> error "Invalid constant" p

let rec type_constant_value com (e,p) =
	match e with
	| EConst c ->
		type_constant com c p
	| EParenthesis e ->
		type_constant_value com e
	| EObjectDecl el ->
		mk (TObjectDecl (List.map (fun (n,e) -> n, type_constant_value com e) el)) (TAnon { a_fields = PMap.empty; a_status = ref Closed }) p
	| EArrayDecl el ->
		mk (TArrayDecl (List.map (type_constant_value com) el)) (com.basic.tarray t_dynamic) p
	| _ ->
		error "Constant value expected" p

let rec has_properties c =
	List.exists (fun f ->
		match f.cf_kind with
		| Var { v_read = AccCall _ } -> true
		| Var { v_write = AccCall _ } -> true
		| _ -> false
	) c.cl_ordered_fields || (match c.cl_super with Some (c,_) -> has_properties c | _ -> false)

let get_properties fields =
	List.fold_left (fun acc f ->
		let acc = (match f.cf_kind with
		| Var { v_read = AccCall getter } -> ("get_" ^ f.cf_name , getter) :: acc
		| _ -> acc) in
		match f.cf_kind with
		| Var { v_write = AccCall setter } -> ("set_" ^ f.cf_name , setter) :: acc
		| _ -> acc
	) [] fields

let add_property_field com c =
	let p = c.cl_pos in
	let props = get_properties (c.cl_ordered_statics @ c.cl_ordered_fields) in
	match props with
	| [] -> ()
	| _ ->
		let fields,values = List.fold_left (fun (fields,values) (n,v) ->
			let cf = mk_field n com.basic.tstring p in
			PMap.add n cf fields,(n, string com v p) :: values
		) (PMap.empty,[]) props in
		let t = mk_anon fields in
		let e = mk (TObjectDecl values) t p in
		let cf = mk_field "__properties__" t p in
		cf.cf_expr <- Some e;
		c.cl_statics <- PMap.add cf.cf_name cf c.cl_statics;
		c.cl_ordered_statics <- cf :: c.cl_ordered_statics

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
	Common.log ctx.com ("Building proxy for " ^ s_type_path path);
	let file, decls = (try
		Typeload.parse_module ctx path p
	with
		| Not_found -> ctx.com.package_rules <- rules; error ("Could not load proxy module " ^ s_type_path path ^ (if fst path = [] then " (try using absolute path)" else "")) p
		| e -> ctx.com.package_rules <- rules; raise e) in
	ctx.com.package_rules <- rules;
	let base_fields = [
		{ cff_name = "__cnx"; cff_pos = p; cff_doc = None; cff_meta = []; cff_access = []; cff_kind = FVar (Some (CTPath { tpackage = ["haxe";"remoting"]; tname = if async then "AsyncConnection" else "Connection"; tparams = []; tsub = None }),None) };
		{ cff_name = "new"; cff_pos = p; cff_doc = None; cff_meta = []; cff_access = [APublic]; cff_kind = FFun { f_args = ["c",false,None,None]; f_type = None; f_expr = Some (EBinop (OpAssign,(EConst (Ident "__cnx"),p),(EConst (Ident "c"),p)),p); f_params = [] } };
	] in
	let tvoid = CTPath { tpackage = []; tname = "Void"; tparams = []; tsub = None } in
	let build_field is_public acc f =
		if f.cff_name = "new" then
			acc
		else match f.cff_kind with
		| FFun fd when (is_public || List.mem APublic f.cff_access) && not (List.mem AStatic f.cff_access) ->
			if List.exists (fun (_,_,t,_) -> t = None) fd.f_args then error ("Field " ^ f.cff_name ^ " type is not complete and cannot be used by RemotingProxy") p;
			let eargs = [EArrayDecl (List.map (fun (a,_,_,_) -> (EConst (Ident a),p)) fd.f_args),p] in
			let ftype = (match fd.f_type with Some (CTPath { tpackage = []; tname = "Void" }) -> None | _ -> fd.f_type) in
			let fargs, eargs = if async then match ftype with
				| Some tret -> fd.f_args @ ["__callb",true,Some (CTFunction ([tret],tvoid)),None], eargs @ [EConst (Ident "__callb"),p]
				| _ -> fd.f_args, eargs @ [EConst (Ident "null"),p]
			else
				fd.f_args, eargs
			in
			let id = (EConst (String f.cff_name), p) in
			let id = if prot then id else ECall ((EConst (Ident "__unprotect__"),p),[id]),p in
			let expr = ECall (
				(EField (
					(ECall ((EField ((EConst (Ident "__cnx"),p),"resolve"),p),[id]),p),
					"call")
				,p),eargs),p
			in
			let expr = if async || ftype = None then expr else (EReturn (Some expr),p) in
			let fd = {
				f_params = fd.f_params;
				f_args = fargs;
				f_type = if async then None else ftype;
				f_expr = Some (EBlock [expr],p);
			} in
			{ cff_name = f.cff_name; cff_pos = p; cff_doc = None; cff_meta = []; cff_access = [APublic]; cff_kind = FFun fd } :: acc
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
	let m = Typeload.type_module ctx (t.tpackage,new_name) file decls p in
	add_dependency ctx.current m;
	try
		List.find (fun tdecl -> snd (t_path tdecl) = new_name) m.m_types
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
			(match c.cl_kind with KTypeParameter _ -> recurse := true | _ -> ());
			List.iter check_recursive tl;
		| _ ->
			()
	in
	let name = String.concat "_" (snd c.cl_path :: (List.map (fun t ->
		check_recursive t;
		let path = (match follow t with
			| TInst (c,_) -> c.cl_path
			| TEnum (e,_) -> e.e_path
			| TMono _ -> error "Type parameter must be explicit when creating a generic instance" p
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
		let ctx = { ctx with local_types = m.m_types @ ctx.local_types } in
		let mg = {
			m_id = alloc_mid();
			m_path = (pack,name);
			m_types = [];
			m_extra = module_extra (s_type_path (pack,name)) m.m_extra.m_sign 0. MFake;
		} in
		let cg = mk_class mg (pack,name) c.cl_pos in
		mg.m_types <- [TClassDecl cg];
		Hashtbl.add ctx.g.modules mg.m_path mg;
		add_dependency mg m;
		add_dependency ctx.current mg;
		(* ensure that type parameters are set in dependencies *)
		let dep_stack = ref [] in
		let rec loop t =
			if not (List.memq t !dep_stack) then begin
			dep_stack := t :: !dep_stack;
			match t with
			| TInst (c,tl) -> add_dep c.cl_module tl
			| TEnum (e,tl) -> add_dep e.e_module tl
			| TType (t,tl) -> add_dep t.t_module tl
			| TMono r ->
				(match !r with
				| None -> ()
				| Some t -> loop t)
			| TLazy f ->
				loop ((!f)());
			| TDynamic t2 ->
				if t == t2 then () else loop t2
			| TAnon a ->
				PMap.iter (fun _ f -> loop f.cf_type) a.a_fields
			| TFun (args,ret) ->
				List.iter (fun (_,_,t) -> loop t) args;
				loop ret
			end
		and add_dep m tl =
			add_dependency mg m;
			List.iter loop tl
		in
		List.iter loop tl;
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
		let vars = Hashtbl.create 0 in
		let build_var v =
			try
				Hashtbl.find vars v.v_id
			with Not_found ->
				let v2 = alloc_var v.v_name (build_type v.v_type) in
				Hashtbl.add vars v.v_id v2;
				v2
		in
		let rec build_expr e = map_expr_type build_expr build_type build_var e in
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
		cg.cl_constructor <- (match c.cl_constructor, c.cl_super with
			| None, None -> None
			| Some c, _ -> Some (build_field c)
			| _ -> error "Please define a constructor for this class in order to use it as generic" c.cl_pos
		);
		cg.cl_implements <- List.map (fun (i,tl) ->
			(match follow (build_type (TInst (i, List.map build_type tl))) with
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
	add_dependency c.cl_module (create_fake_module ctx file);
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
						cf_pos = p;
						cf_doc = None;
						cf_meta = no_meta;
						cf_kind = Var { v_read = AccResolve; v_write = AccNo };
						cf_params = [];
						cf_expr = None;
						cf_overloads = [];
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
			let fields = List.map (fun f -> f.cf_name,f.cf_meta) (c.cl_ordered_fields @ (match c.cl_constructor with None -> [] | Some f -> [{ f with cf_name = "_" }])) in
			let statics =  List.map (fun f -> f.cf_name,f.cf_meta) c.cl_ordered_statics in
			(c.cl_pos, ["",c.cl_meta],fields,statics)
		| TEnumDecl e ->
			(e.e_pos, ["",e.e_meta],List.map (fun n -> n, (PMap.find n e.e_constrs).ef_meta) e.e_names, [])
		| TTypeDecl t ->
			(t.t_pos, ["",t.t_meta],(match follow t.t_type with TAnon a -> PMap.fold (fun f acc -> (f.cf_name,f.cf_meta) :: acc) a.a_fields [] | _ -> []),[])
	) in
	let filter l =
		let l = List.map (fun (n,ml) -> n, List.filter (fun (m,_,_) -> m.[0] <> ':') ml) l in
		List.filter (fun (_,ml) -> ml <> []) l
	in
	let meta, fields, statics = filter meta, filter fields, filter statics in
	let make_meta_field ml =
		let h = Hashtbl.create 0 in
		mk (TObjectDecl (List.map (fun (f,el,p) ->
			if Hashtbl.mem h f then error ("Duplicate metadata '" ^ f ^ "'") p;
			Hashtbl.add h f ();
			f, mk (match el with [] -> TConst TNull | _ -> TArrayDecl (List.map (type_constant_value com) el)) (api.tarray t_dynamic) p
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
(* MACRO TYPE *)

let build_macro_type ctx pl p =
	let path, field, args = (match pl with
		| [TInst ({ cl_kind = KExpr (ECall (e,args),_) },_)]
		| [TInst ({ cl_kind = KExpr (EArrayDecl [ECall (e,args),_],_) },_)] ->
			let rec loop e =
				match fst e with
				| EField (e,f) -> f :: loop e
				| EConst (Ident i) -> [i]
				| _ -> error "Invalid macro call" p
			in
			(match loop e with
			| meth :: cl :: path -> (List.rev path,cl), meth, args
			| _ -> error "Invalid macro call" p)
		| _ ->
			error "MacroType require a single expression call parameter" p
	) in
	let old = ctx.ret in
	let t = (match ctx.g.do_macro ctx MMacroType path field args p with
		| None -> mk_mono()
		| Some _ -> ctx.ret
	) in
	ctx.ret <- old;
	t

(* -------------------------------------------------------------------------- *)
(* API EVENTS *)

let build_instance ctx mtype p =
	match mtype with
	| TClassDecl c ->
		let ft = (fun pl ->
			match c.cl_kind with
			| KGeneric ->
				let r = exc_protect ctx (fun r ->
					let t = mk_mono() in
					r := (fun() -> t);
					unify_raise ctx (build_generic ctx c p pl) t p;
					t
				) in
				delay ctx (fun() -> ignore ((!r)()));
				TLazy r
			| KMacroType ->
				let r = exc_protect ctx (fun r ->
					let t = mk_mono() in
					r := (fun() -> t);
					unify_raise ctx (build_macro_type ctx pl p) t p;
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
		if Common.defined ctx.com "haxe3" then error ("Implementing haxe.rtti.Generic is deprecated in haxe 3, please use @:generic instead") c.cl_pos;
		if c.cl_types <> [] then c.cl_kind <- KGeneric;
		false
	| HExtends { tpackage = ["haxe";"xml"]; tname = "Proxy"; tparams = [TPExpr(EConst (String file),p);TPType t] } ->
		extend_xml_proxy ctx c t file p;
		true
	| _ ->
		true

(* -------------------------------------------------------------------------- *)
(* FINAL GENERATION *)

(*
	Adds member field initializations as assignments to the constructor
*)
let add_field_inits com c =
	let ethis = mk (TConst TThis) (TInst (c,List.map snd c.cl_types)) c.cl_pos in
	(* TODO: we have to find a variable name which is not used in any of the functions *)
	let v = alloc_var "_g" ethis.etype in
	let need_this = ref false in
	let inits,fields = List.fold_left (fun (inits,fields) cf ->
		match cf.cf_kind,cf.cf_expr with
		| Var _, Some _ ->
			if com.config.pf_can_init_member cf then (inits, cf :: fields) else (cf :: inits, cf :: fields)
		| Method MethDynamic, Some e when Common.defined com "as3" ->
			(* TODO : this would have a better place in genSWF9 I think - NC *)
			(* we move the initialization of dynamic functions to the constructor and also solve the
			   'this' problem along the way *)
			let rec use_this v e = match e.eexpr with
				| TConst TThis ->
					need_this := true;
					mk (TLocal v) v.v_type e.epos
				| _ -> Type.map_expr (use_this v) e
			in
			let e = Type.map_expr (use_this v) e in
			let cf = {cf with cf_expr = Some e} in
			(* if the method is an override, we have to remove the class field to not get invalid overrides *)
			let fields = if List.mem cf.cf_name c.cl_overrides then begin
				c.cl_fields <- PMap.remove cf.cf_name c.cl_fields;
				fields
			end else
				cf :: fields
			in
			(cf :: inits, fields)
		| _ -> (inits, cf :: fields)
	) ([],[]) c.cl_ordered_fields in
	c.cl_ordered_fields <- fields;
	match inits with
	| [] -> ()
	| _ ->
		let el = List.map (fun cf ->
			match cf.cf_expr with
			| None -> assert false
			| Some e ->
				let lhs = mk (TField(ethis,cf.cf_name)) e.etype e.epos in
				cf.cf_expr <- None;
				let eassign = mk (TBinop(OpAssign,lhs,e)) lhs.etype e.epos in
				if Common.defined com "as3" then begin
					let echeck = mk (TBinop(OpEq,lhs,(mk (TConst TNull) lhs.etype e.epos))) com.basic.tbool e.epos in
					mk (TIf(echeck,eassign,None)) eassign.etype e.epos
				end else
					eassign;
		) inits in
		let el = if !need_this then (mk (TVars([v, Some ethis])) ethis.etype ethis.epos) :: el else el in
		match c.cl_constructor with
		| None ->
			let ct = TFun([],com.basic.tvoid) in
			let ce = mk (TFunction {
				tf_args = [];
				tf_type = com.basic.tvoid;
				tf_expr = mk (TBlock el) com.basic.tvoid c.cl_pos;
			}) ct c.cl_pos in
			let ctor = mk_field "new" ct c.cl_pos in
			ctor.cf_kind <- Method MethNormal;
			c.cl_constructor <- Some { ctor with cf_expr = Some ce };
		| Some cf ->
			match cf.cf_expr with
			| Some { eexpr = TFunction f } ->
				let bl = match f.tf_expr with {eexpr = TBlock b } -> b | x -> [x] in
				let ce = mk (TFunction {f with tf_expr = mk (TBlock (el @ bl)) com.basic.tvoid c.cl_pos }) cf.cf_type cf.cf_pos in
				c.cl_constructor <- Some {cf with cf_expr = Some ce }
			| _ ->
				assert false

let has_rtti ctx c =
	let rec has_rtti_new c =
		has_meta ":rttiInfos" c.cl_meta || match c.cl_super with None -> false | Some (csup,_) -> has_rtti_new csup
	in
	let rec has_rtti_old c =
		List.exists (function (t,pl) ->
			match t, pl with
			| { cl_path = ["haxe";"rtti"],"Infos" },[] -> true
			| _ -> false
		) c.cl_implements || (match c.cl_super with None -> false | Some (c,_) -> has_rtti_old c)
	in
	if Common.defined ctx.com "haxe3" then begin
		if has_rtti_old c then error ("Implementing haxe.rtti.Infos is deprecated in haxe 3, please use @:rttiInfos instead") c.cl_pos;
		has_rtti_new c
	end else
		has_rtti_old c || has_rtti_new c

let restore c =
	let meta = c.cl_meta and path = c.cl_path and ext = c.cl_extern in
	let fl = c.cl_fields and ofl = c.cl_ordered_fields and st = c.cl_statics and ost = c.cl_ordered_statics in
	(fun() ->
		c.cl_meta <- meta;
		c.cl_extern <- ext;
		c.cl_path <- path;
		c.cl_fields <- fl;
		c.cl_ordered_fields <- ofl;
		c.cl_statics <- st;
		c.cl_ordered_statics <- ost;
	)

let on_generate ctx t =
	match t with
	| TClassDecl c ->
		if c.cl_private then begin
			let rpath = (fst c.cl_module.m_path,"_" ^ snd c.cl_module.m_path) in
			if Hashtbl.mem ctx.g.types_module rpath then error ("This private class name will clash with " ^ s_type_path rpath) c.cl_pos;
		end;
		if c.cl_kind = KGeneric && not (has_meta ":usedRecursively" c.cl_meta) then c.cl_extern <- true;
		c.cl_restore <- restore c;
		List.iter (fun m ->
			match m with
			| ":native",[Ast.EConst (Ast.String name),p],mp ->
				c.cl_meta <- (":realPath",[Ast.EConst (Ast.String (s_type_path c.cl_path)),p],mp) :: c.cl_meta;
				c.cl_path <- parse_path name;
			| _ -> ()
		) c.cl_meta;
		if has_rtti ctx c && not (PMap.mem "__rtti" c.cl_statics) then begin
			let f = mk_field "__rtti" ctx.t.tstring c.cl_pos in
			let str = Genxml.gen_type_string ctx.com t in
			f.cf_expr <- Some (mk (TConst (TString str)) f.cf_type c.cl_pos);
			c.cl_ordered_statics <- f :: c.cl_ordered_statics;
			c.cl_statics <- PMap.add f.cf_name f c.cl_statics;
		end;
		let do_remove f =
			(not ctx.in_macro && f.cf_kind = Method MethMacro) || has_meta ":extern" f.cf_meta
		in
		if not (Common.defined ctx.com "doc_gen") then begin
			c.cl_ordered_fields <- List.filter (fun f ->
				let b = do_remove f in
				if b then c.cl_fields <- PMap.remove f.cf_name c.cl_fields;
				not b
			) c.cl_ordered_fields;
			c.cl_ordered_statics <- List.filter (fun f ->
				let b = do_remove f in
				if b then c.cl_statics <- PMap.remove f.cf_name c.cl_statics;
				not b
			) c.cl_ordered_statics;
		end;
		add_field_inits ctx.com c;
		(match build_metadata ctx.com t with
		| None -> ()
		| Some e ->
			let f = mk_field "__meta__" t_dynamic c.cl_pos in
			f.cf_expr <- Some e;
			c.cl_ordered_statics <- f :: c.cl_ordered_statics;
			c.cl_statics <- PMap.add f.cf_name f c.cl_statics);
		c.cl_implements <- List.filter (fun (c,_) -> not (has_meta ":remove" c.cl_meta)) c.cl_implements;
	| TEnumDecl e ->
		List.iter (fun m ->
			match m with
			| ":native",[Ast.EConst (Ast.String name),p],mp ->
				e.e_meta <- (":realPath",[Ast.EConst (Ast.String (s_type_path e.e_path)),p],mp) :: e.e_meta;
				e.e_path <- parse_path name;
			| _ -> ()
		) e.e_meta;
	| _ ->
		()

(* -------------------------------------------------------------------------- *)
(* LOCAL VARIABLES USAGE *)

type usage =
	| Block of ((usage -> unit) -> unit)
	| Loop of ((usage -> unit) -> unit)
	| Function of ((usage -> unit) -> unit)
	| Declare of tvar
	| Use of tvar

let rec local_usage f e =
	match e.eexpr with
	| TLocal v ->
		f (Use v)
	| TVars l ->
		List.iter (fun (v,e) ->
			(match e with None -> () | Some e -> local_usage f e);
			f (Declare v);
		) l
	| TFunction tf ->
		let cc f =
			List.iter (fun (v,_) -> f (Declare v)) tf.tf_args;
			local_usage f tf.tf_expr;
		in
		f (Function cc)
	| TBlock l ->
		f (Block (fun f -> List.iter (local_usage f) l))
	| TFor (v,it,e) ->
		local_usage f it;
		f (Loop (fun f ->
			f (Declare v);
			local_usage f e;
		))
	| TWhile _ ->
		f (Loop (fun f ->
			iter (local_usage f) e
		))
	| TTry (e,catchs) ->
		local_usage f e;
		List.iter (fun (v,e) ->
			f (Block (fun f ->
				f (Declare v);
				local_usage f e;
			))
		) catchs;
	| TMatch (e,_,cases,def) ->
		local_usage f e;
		List.iter (fun (_,vars,e) ->
			let cc f =
				(match vars with
				| None -> ()
				| Some l ->	List.iter (function None -> () | Some v -> f (Declare v)) l);
				local_usage f e;
			in
			f (Block cc)
		) cases;
		(match def with None -> () | Some e -> local_usage f e);
	| _ ->
		iter (local_usage f) e

(* -------------------------------------------------------------------------- *)
(* BLOCK VARIABLES CAPTURE *)

(*
	For some platforms, it will simply mark the variables which are used in closures
	using the v_capture flag so it can be processed in a more optimized

	For Flash/JS platforms, it will ensure that variables used in loop sub-functions
	have an unique scope. It transforms the following expression :

	for( x in array )
		funs.push(function() return x++);

	Into the following :

	for( _x in array ) {
		var x = [_x];
		funs.push(function(x) { function() return x[0]++; }(x));
	}
*)

let captured_vars com e =

	let t = com.basic in

	let rec mk_init av v pos =
		mk (TVars [av,Some (mk (TArrayDecl [mk (TLocal v) v.v_type pos]) av.v_type pos)]) t.tvoid pos

	and mk_var v used =
		alloc_var v.v_name (PMap.find v.v_id used)

	and wrap used e =
		match e.eexpr with
		| TVars vl ->
			let vl = List.map (fun (v,ve) ->
				if PMap.mem v.v_id used then
					v, Some (mk (TArrayDecl (match ve with None -> [] | Some e -> [wrap used e])) v.v_type e.epos)
				else
					v, (match ve with None -> None | Some e -> Some (wrap used e))
			) vl in
			{ e with eexpr = TVars vl }
		| TLocal v when PMap.mem v.v_id used ->
			mk (TArray ({ e with etype = v.v_type },mk (TConst (TInt 0l)) t.tint e.epos)) e.etype e.epos
		| TFor (v,it,expr) when PMap.mem v.v_id used ->
			let vtmp = mk_var v used in
			let it = wrap used it in
			let expr = wrap used expr in
			mk (TFor (vtmp,it,concat (mk_init v vtmp e.epos) expr)) e.etype e.epos
		| TTry (expr,catchs) ->
			let catchs = List.map (fun (v,e) ->
				let e = wrap used e in
				try
					let vtmp = mk_var v used in
					vtmp, concat (mk_init v vtmp e.epos) e
				with Not_found ->
					v, e
			) catchs in
			mk (TTry (wrap used expr,catchs)) e.etype e.epos
		| TMatch (expr,enum,cases,def) ->
			let cases = List.map (fun (il,vars,e) ->
				let pos = e.epos in
				let e = ref (wrap used e) in
				let vars = match vars with
					| None -> None
					| Some l ->
						Some (List.map (fun v ->
							match v with
							| Some v when PMap.mem v.v_id used ->
								let vtmp = mk_var v used in
								e := concat (mk_init v vtmp pos) !e;
								Some vtmp
							| _ -> v
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
			let tmp_used = ref used in
			let rec browse = function
				| Block f | Loop f | Function f -> f browse
				| Use v ->
					if PMap.mem v.v_id !tmp_used then fused := PMap.add v.v_id v !fused;
				| Declare v ->
					tmp_used := PMap.remove v.v_id !tmp_used
			in
			local_usage browse e;
			let vars = PMap.fold (fun v acc -> v :: acc) !fused [] in

			(* in case the variable has been marked as used in a parallel scope... *)
			let fexpr = ref (wrap used f.tf_expr) in
			let fargs = List.map (fun (v,o) ->
				if PMap.mem v.v_id used then
					let vtmp = mk_var v used in
					fexpr := concat (mk_init v vtmp e.epos) !fexpr;
					vtmp, o
				else
					v, o
			) f.tf_args in
			let e = { e with eexpr = TFunction { f with tf_args = fargs; tf_expr = !fexpr } } in
			(*
				Create a new function scope to make sure that the captured loop variable
				will not be overwritten in next loop iteration
			*)
			if com.config.pf_capture_policy = CPLoopVars then
				mk (TCall (
					mk_parent (mk (TFunction {
						tf_args = List.map (fun v -> v, None) vars;
						tf_type = e.etype;
						tf_expr = mk_block (mk (TReturn (Some e)) e.etype e.epos);
					}) (TFun (List.map (fun v -> v.v_name,false,v.v_type) vars,e.etype)) e.epos),
					List.map (fun v -> mk (TLocal v) v.v_type e.epos) vars)
				) e.etype e.epos
			else
				e
		| _ ->
			map_expr (wrap used) e

	and do_wrap used e =
		if PMap.is_empty used then
			e
		else
			let used = PMap.map (fun v ->
				let vt = v.v_type in
				v.v_type <- t.tarray vt;
				v.v_capture <- true;
				vt
			) used in
			wrap used e

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
				| Declare v ->
					if in_loop then vars := PMap.add v.v_id !depth !vars;
				| Use v ->
					try
						let d = PMap.find v.v_id !vars in
						if d <> !depth then used := PMap.add v.v_id v !used;
					with Not_found ->
						()
			in
			local_usage (collect_vars false) e;
			do_wrap !used e
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
		| Declare v ->
			vars := PMap.add v.v_id !depth !vars;
		| Use v ->
			try
				let d = PMap.find v.v_id !vars in
				if d <> !depth then used := PMap.add v.v_id v !used;
			with Not_found -> ()
		in
		local_usage collect_vars e;
		!used
	in
	(* mark all capture variables - also used in rename_local_vars at later stage *)
	let captured = all_vars e in
	PMap.iter (fun _ v -> v.v_capture <- true) captured;
	match com.config.pf_capture_policy with
	| CPNone -> e
	| CPWrapRef -> do_wrap captured e
	| CPLoopVars -> out_loop e

(* -------------------------------------------------------------------------- *)
(* RENAME LOCAL VARS *)

let rename_local_vars com e =
	let cfg = com.config in
	let all_scope = (not cfg.pf_captured_scope) || (not cfg.pf_locals_scope) in
	let vars = ref PMap.empty in
	let all_vars = ref PMap.empty in
	let vtemp = alloc_var "~" t_dynamic in
	let rebuild_vars = ref false in
	let rebuild m =
		PMap.fold (fun v acc -> PMap.add v.v_name v acc) m PMap.empty
	in
	let save() =
		let old = !vars in
		if cfg.pf_unique_locals then (fun() -> ()) else (fun() -> vars := if !rebuild_vars then rebuild old else old)
	in
	let rename vars v =
		let count = ref 1 in
		while PMap.mem (v.v_name ^ string_of_int !count) vars do
			incr count;
		done;
		v.v_name <- v.v_name ^ string_of_int !count;
	in
	let declare v =
		(* chop escape char for all local variables generated *)
		if String.unsafe_get v.v_name 0 = String.unsafe_get gen_local_prefix 0 then v.v_name <- "_g" ^ String.sub v.v_name 1 (String.length v.v_name - 1);
		let look_vars = (if not cfg.pf_captured_scope && v.v_capture then !all_vars else !vars) in
		(try
			let v2 = PMap.find v.v_name look_vars in
			(*
				block_vars will create some wrapper-functions that are declaring
				the same variable twice. In that case do not perform a rename since
				we are sure it's actually the same variable
			*)
			if v == v2 then raise Not_found;
			rename look_vars v;
		with Not_found ->
			());
		vars := PMap.add v.v_name v !vars;
		if all_scope then all_vars := PMap.add v.v_name v !all_vars;
	in
	(*
		This is quite a rare case, when a local variable would otherwise prevent
		accessing a type because it masks the type value or the package name.
	*)
	let check t =
		match (t_infos t).mt_path with
		| [], name | name :: _, _ ->
			let vars = if cfg.pf_locals_scope then vars else all_vars in
			(try
				let v = PMap.find name !vars in
				if v == vtemp then raise Not_found; (* ignore *)
				rename (!vars) v;
				rebuild_vars := true;
				vars := PMap.add v.v_name v !vars
			with Not_found ->
				());
			vars := PMap.add name vtemp !vars
	in
	let check_type t =
		match follow t with
		| TInst (c,_) -> check (TClassDecl c)
		| TEnum (e,_) -> check (TEnumDecl e)
		| TType (t,_) -> check (TTypeDecl t)
		| TMono _ | TLazy _ | TAnon _ | TDynamic _ | TFun _ -> ()
	in
	let rec loop e =
		match e.eexpr with
		| TVars l ->
			List.iter (fun (v,e) ->
				if not cfg.pf_locals_scope then declare v;
				(match e with None -> () | Some e -> loop e);
				if cfg.pf_locals_scope then declare v;
			) l
		| TFunction tf ->
			let old = save() in
			List.iter (fun (v,_) -> declare v) tf.tf_args;
			loop tf.tf_expr;
			old()
		| TBlock el ->
			let old = save() in
			List.iter loop el;
			old()
		| TFor (v,it,e) ->
			loop it;
			let old = save() in
			declare v;
			loop e;
			old()
		| TTry (e,catchs) ->
			loop e;
			List.iter (fun (v,e) ->
				let old = save() in
				declare v;
				check_type v.v_type;
				loop e;
				old()
			) catchs;
		| TMatch (e,_,cases,def) ->
			loop e;
			List.iter (fun (_,vars,e) ->
				let old = save() in
				(match vars with
				| None -> ()
				| Some l ->	List.iter (function None -> () | Some v -> declare v) l);
				loop e;
				old();
			) cases;
			(match def with None -> () | Some e -> loop e);
		| TTypeExpr t ->
			check t
		| TEnumField (e,_) ->
			check (TEnumDecl e)
		| TNew (c,_,_) ->
			Type.iter loop e;
			check (TClassDecl c);
		| TCast (e,Some t) ->
			loop e;
			check t;
		| _ ->
			Type.iter loop e
	in
	loop e;
	e

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
		| TLocal v ->
			let init = (try PMap.find v.v_id !vars with Not_found -> true) in
			if not init then error ("Local variable " ^ v.v_name ^ " used without being initialized") e.epos;
		| TVars vl ->
			List.iter (fun (v,eo) ->
				match eo with
				| None ->
					declared := v.v_id :: !declared;
					vars := PMap.add v.v_id false !vars
				| Some e ->
					loop vars e
			) vl
		| TBlock el ->
			let old = !declared in
			let old_vars = !vars in
			declared := [];
			List.iter (loop vars) el;
			restore vars old_vars (List.rev !declared);
			declared := old;
		| TBinop (OpAssign,{ eexpr = TLocal v },e) when PMap.mem v.v_id !vars ->
			loop vars e;
			vars := PMap.add v.v_id true !vars
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
		| TTry (e,catches) ->
			let cvars = List.map (fun (v,e) ->
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
				loop vars e;
				restore vars old [];
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

let pp_counter = ref 1

let post_process filters t =
	(* ensure that we don't process twice the same (cached) module *)
	let m = (t_infos t).mt_module.m_extra in
	if m.m_processed = 0 then m.m_processed <- !pp_counter;
	if m.m_processed = !pp_counter then
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

let post_process_end() =
	incr pp_counter

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
	let stack_var = alloc_var stack_var st in
	let exc_var = alloc_var exc_var st in
	let pos_var = alloc_var pos_var t.tint in
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
		let tmp = alloc_var tmp_var e.etype in
		mk (TBlock [
			mk (TVars [tmp, Some e]) t.tvoid e.epos;
			stack_pop;
			mk (TReturn (Some (mk (TLocal tmp) e.etype e.epos))) e.etype e.epos
		]) e.etype e.epos
	in
	{
		stack_var = stack_var.v_name;
		stack_exc_var = exc_var.v_name;
		stack_pos_var = pos_var.v_name;
		stack_pos = p;
		stack_expr = stack_e;
		stack_pop = stack_pop;
		stack_save_pos = mk (TVars [pos_var, Some (field stack_e "length" t.tint p)]) t.tvoid p;
		stack_push = stack_push;
		stack_return = stack_return;
		stack_restore = [
			binop OpAssign exc_e (mk (TArrayDecl []) st p) st p;
			mk (TWhile (
				mk_parent (binop OpGte (field stack_e "length" t.tint p) (mk (TLocal pos_var) t.tint p) t.tbool p),
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
		let cases = List.map (fun (v,e) ->
			let e = stack_block_loop ctx e in
			let e = (match (mk_block e).eexpr with
				| TBlock l -> mk (TBlock (ctx.stack_restore @ l)) e.etype e.epos
				| _ -> assert false
			) in
			v , e
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

let rec find_field c f =
	try
		(match c.cl_super with
		| None ->
			raise Not_found
		| Some (c,_) ->
			find_field c f)
	with Not_found -> try
		let rec loop = function
			| [] ->
				raise Not_found
			| (c,_) :: l ->
				try
					find_field c f
				with
					Not_found -> loop l
		in
		loop c.cl_implements
	with Not_found ->
		let f = PMap.find f.cf_name c.cl_fields in
		(match f.cf_kind with Var { v_read = AccRequire _ } -> raise Not_found | _ -> ());
		f

let fix_override com c f fd =
	c.cl_fields <- PMap.remove f.cf_name c.cl_fields;
	let f2 = (try Some (find_field c f) with Not_found -> None) in
	let f = (match f2,fd with
		| Some (f2), Some(fd) ->
			let targs, tret = (match follow f2.cf_type with TFun (args,ret) -> args, ret | _ -> assert false) in
			let changed_args = ref [] in
			let prefix = "_tmp_" in
			let nargs = List.map2 (fun ((v,c) as cur) (_,_,t2) ->
				try
					type_eq EqStrict v.v_type t2;
					cur
				with Unify_error _ ->
					let v2 = alloc_var (prefix ^ v.v_name) t2 in
					changed_args := (v,v2) :: !changed_args;
					v2,c
			) fd.tf_args targs in
			let fd2 = {
				tf_args = nargs;
				tf_type = tret;
				tf_expr = (match List.rev !changed_args with
					| [] -> fd.tf_expr
					| args ->
						let e = fd.tf_expr in
						let el = (match e.eexpr with TBlock el -> el | _ -> [e]) in
						let p = (match el with [] -> e.epos | e :: _ -> e.epos) in
						let v = mk (TVars (List.map (fun (v,v2) ->
							(v,Some (mk (TCast (mk (TLocal v2) v2.v_type p,None)) v.v_type p))
						) args)) com.basic.tvoid p in
						{ e with eexpr = TBlock (v :: el) }
				);
			} in
			let targs = List.map (fun(v,c) -> (v.v_name, Option.is_some c, v.v_type)) nargs in
			let fde = (match f.cf_expr with None -> assert false | Some e -> e) in
			{ f with cf_expr = Some { fde with eexpr = TFunction fd2 }; cf_type = TFun(targs,tret) }
		| Some(f2), None when c.cl_interface ->
			let targs, tret = (match follow f2.cf_type with TFun (args,ret) -> args, ret | _ -> assert false) in
			{ f with cf_type = TFun(targs,tret) }
		| _ ->
			f
	) in
	c.cl_fields <- PMap.add f.cf_name f c.cl_fields;
	f

let fix_overrides com t =
	match t with
	| TClassDecl c ->
		c.cl_ordered_fields <- List.map (fun f ->
			match f.cf_expr, f.cf_kind with
			| Some { eexpr = TFunction fd }, Method (MethNormal | MethInline) ->
				fix_override com c f (Some fd)
			| None, Method (MethNormal | MethInline) when c.cl_interface ->
				fix_override com c f None
			| _ ->
				f
		) c.cl_ordered_fields
	| _ ->
		()

(*
	PHP does not allow abstract classes extending other abstract classes to override any fields, so these duplicates
	must be removed from the child interface
*)
let fix_abstract_inheritance com t =
	match t with
	| TClassDecl c when c.cl_interface ->
		c.cl_ordered_fields <- List.filter (fun f ->
			let b = try (find_field c f) == f
			with Not_found -> false in
			if not b then c.cl_fields <- PMap.remove f.cf_name c.cl_fields;
			b;
		) c.cl_ordered_fields
	| _ -> ()

(* -------------------------------------------------------------------------- *)
(* MISC FEATURES *)

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

let set_default ctx a c p =
	let t = a.v_type in
	let ve = mk (TLocal a) t p in
	let cond =  TBinop (OpEq,ve,mk (TConst TNull) t p) in
	mk (TIf (mk_parent (mk cond ctx.basic.tbool p), mk (TBinop (OpAssign,ve,mk (TConst c) t p)) t p,None)) ctx.basic.tvoid p

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
	let vtmp = alloc_var vtmp e.etype in
	let var = mk (TVars [vtmp,Some e]) api.tvoid p in
	let vexpr = mk (TLocal vtmp) e.etype p in
	let texpr = mk (TTypeExpr texpr) (mk_texpr texpr) p in
	let std = (try List.find (fun t -> t_path t = ([],"Std")) com.types with Not_found -> assert false) in
	let std = mk (TTypeExpr std) (mk_texpr std) p in
	let is = mk (TField (std,"is")) (tfun [t_dynamic;t_dynamic] api.tbool) p in
	let is = mk (TCall (is,[vexpr;texpr])) api.tbool p in
	let exc = mk (TThrow (mk (TConst (TString "Class cast error")) api.tstring p)) t p in
	let check = mk (TIf (mk_parent is,mk (TCast (vexpr,None)) t p,Some exc)) t p in
	mk (TBlock [var;check;vexpr]) t p
