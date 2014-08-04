(*
 * Copyright (C)2005-2013 Haxe Foundation
 *
 * Permission is hereby granted, free of charge, to any person obtaining a
 * copy of this software and associated documentation files (the "Software"),
 * to deal in the Software without restriction, including without limitation
 * the rights to use, copy, modify, merge, publish, distribute, sublicense,
 * and/or sell copies of the Software, and to permit persons to whom the
 * Software is furnished to do so, subject to the following conditions:
 *
 * The above copyright notice and this permission notice shall be included in
 * all copies or substantial portions of the Software.
 *
 * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
 * IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
 * FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
 * AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
 * LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING
 * FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER
 * DEALINGS IN THE SOFTWARE.
 *)

open Ast
open Type
open Common
open Typecore

(* -------------------------------------------------------------------------- *)
(* TOOLS *)

let field e name t p =
	mk (TField (e,try quick_field e.etype name with Not_found -> assert false)) t p

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

let type_constant com c p =
	let t = com.basic in
	match c with
	| Int s ->
		if String.length s > 10 && String.sub s 0 2 = "0x" then error "Invalid hexadecimal integer" p;
		(try mk (TConst (TInt (Int32.of_string s))) t.tint p
		with _ -> mk (TConst (TFloat s)) t.tfloat p)
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
		| Var { v_read = AccCall } -> true
		| Var { v_write = AccCall } -> true
		| _ when Meta.has Meta.Accessor f.cf_meta -> true
		| _ -> false
	) c.cl_ordered_fields || (match c.cl_super with Some (c,_) -> has_properties c | _ -> false)

let get_properties fields =
	List.fold_left (fun acc f ->
		if Meta.has Meta.Accessor f.cf_meta then
			(f.cf_name, f.cf_name) :: acc
		else
			let acc = (match f.cf_kind with
			| Var { v_read = AccCall } -> ("get_" ^ f.cf_name , "get_" ^ f.cf_name) :: acc
			| _ -> acc) in
			match f.cf_kind with
			| Var { v_write = AccCall } -> ("set_" ^ f.cf_name , "set_" ^ f.cf_name) :: acc
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
	add_dependency ctx.m.curmod m;
	try
		List.find (fun tdecl -> snd (t_path tdecl) = new_name) m.m_types
	with Not_found ->
		error ("Module " ^ s_type_path path ^ " does not define type " ^ t.tname) p
	) in
	match t with
	| TClassDecl c2 when c2.cl_types = [] -> c2.cl_build(); c.cl_super <- Some (c2,[]);
	| _ -> error "Remoting proxy must be a class without parameters" p

(* -------------------------------------------------------------------------- *)
(* HAXE.RTTI.GENERIC *)

exception Generic_Exception of string * Ast.pos

type generic_context = {
	ctx : typer;
	subst : (t * t) list;
	name : string;
	p : pos;
	mutable mg : module_def option;
}

let make_generic ctx ps pt p =
	let rec loop l1 l2 =
		match l1, l2 with
		| [] , [] -> []
		| (x,TLazy f) :: l1, _ -> loop ((x,(!f)()) :: l1) l2
		| (_,t1) :: l1 , t2 :: l2 -> (t1,t2) :: loop l1 l2
		| _ -> assert false
	in
	let name =
		String.concat "_" (List.map2 (fun (s,_) t ->
			let s_type_path_underscore (p,s) = match p with [] -> s | _ -> String.concat "_" p ^ "_" ^ s in
			let rec loop top t = match follow t with
				| TInst(c,tl) -> (s_type_path_underscore c.cl_path) ^ (loop_tl tl)
				| TEnum(en,tl) -> (s_type_path_underscore en.e_path) ^ (loop_tl tl)
				| TAbstract(a,tl) -> (s_type_path_underscore a.a_path) ^ (loop_tl tl)
				| _ when not top -> "_" (* allow unknown/incompatible types as type parameters to retain old behavior *)
				| TMono _ -> raise (Generic_Exception (("Could not determine type for parameter " ^ s), p))
				| t -> raise (Generic_Exception (("Type parameter must be a class or enum instance (found " ^ (s_type (print_context()) t) ^ ")"), p))
			and loop_tl tl = match tl with
				| [] -> ""
				| tl -> "_" ^ String.concat "_" (List.map (loop false) tl)
			in
			loop true t
		) ps pt)
	in
	{
		ctx = ctx;
		subst = loop ps pt;
		name = name;
		p = p;
		mg = None;
	}

let rec generic_substitute_type gctx t =
	match t with
	| TInst ({ cl_kind = KGeneric } as c2,tl2) ->
		(* maybe loop, or generate cascading generics *)
		let _, _, f = gctx.ctx.g.do_build_instance gctx.ctx (TClassDecl c2) gctx.p in
		let t = f (List.map (generic_substitute_type gctx) tl2) in
		(match follow t,gctx.mg with TInst(c,_), Some m -> add_dependency m c.cl_module | _ -> ());
		t
	| _ ->
		try
			generic_substitute_type gctx (List.assq t gctx.subst)
		with Not_found ->
			Type.map (generic_substitute_type gctx) t

let generic_substitute_expr gctx e =
	let vars = Hashtbl.create 0 in
	let build_var v =
		try
			Hashtbl.find vars v.v_id
		with Not_found ->
			let v2 = alloc_var v.v_name (generic_substitute_type gctx v.v_type) in
			v2.v_meta <- v.v_meta;
			Hashtbl.add vars v.v_id v2;
			v2
	in
	let rec build_expr e =
		match e.eexpr with
		| TField(e1, FInstance({cl_kind = KGeneric},cf)) ->
			build_expr {e with eexpr = TField(e1,quick_field_dynamic (generic_substitute_type gctx (e1.etype)) cf.cf_name)}
		| _ -> map_expr_type build_expr (generic_substitute_type gctx) build_var e
	in
	build_expr e

let has_ctor_constraint c = match c.cl_kind with
	| KTypeParameter tl ->
		List.exists (fun t -> match follow t with
			| TAnon a when PMap.mem "new" a.a_fields -> true
			| _ -> false
		) tl;
	| _ -> false

let rec build_generic ctx c p tl =
	let pack = fst c.cl_path in
	let recurse = ref false in
	let rec check_recursive t =
		match follow t with
		| TInst (c2,tl) ->
			(match c2.cl_kind with
			| KTypeParameter tl ->
				if not (Typeload.is_generic_parameter ctx c2) && has_ctor_constraint c2 then
					error "Type parameters with a constructor cannot be used non-generically" p;
				recurse := true
			| _ -> ());
			List.iter check_recursive tl;
		| _ ->
			()
	in
	List.iter check_recursive tl;
	if !recurse then begin
		TInst (c,tl) (* build a normal instance *)
	end else begin
	let gctx = make_generic ctx c.cl_types tl p in
	let name = (snd c.cl_path) ^ "_" ^ gctx.name in
	try
		Typeload.load_instance ctx { tpackage = pack; tname = name; tparams = []; tsub = None } p false
	with Error(Module_not_found path,_) when path = (pack,name) ->
		let m = (try Hashtbl.find ctx.g.modules (Hashtbl.find ctx.g.types_module c.cl_path) with Not_found -> assert false) in
		let ctx = { ctx with m = { ctx.m with module_types = m.m_types @ ctx.m.module_types } } in
		c.cl_build(); (* make sure the super class is already setup *)
		let mg = {
			m_id = alloc_mid();
			m_path = (pack,name);
			m_types = [];
			m_extra = module_extra (s_type_path (pack,name)) m.m_extra.m_sign 0. MFake;
		} in
		gctx.mg <- Some mg;
		let cg = mk_class mg (pack,name) c.cl_pos in
		mg.m_types <- [TClassDecl cg];
		Hashtbl.add ctx.g.modules mg.m_path mg;
		add_dependency mg m;
		add_dependency ctx.m.curmod mg;
		(* ensure that type parameters are set in dependencies *)
		let dep_stack = ref [] in
		let rec loop t =
			if not (List.memq t !dep_stack) then begin
			dep_stack := t :: !dep_stack;
			match t with
			| TInst (c,tl) -> add_dep c.cl_module tl
			| TEnum (e,tl) -> add_dep e.e_module tl
			| TType (t,tl) -> add_dep t.t_module tl
			| TAbstract (a,tl) -> add_dep a.a_module tl
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
		let delays = ref [] in
		let build_field f =
			let t = generic_substitute_type gctx f.cf_type in
			let f = { f with cf_type = t} in
			(* delay the expression mapping to make sure all cf_type fields are set correctly first *)
			(delays := (fun () ->
				try (match f.cf_expr with
					| None ->
						begin match f.cf_kind with
							| Method _ when not c.cl_interface && not c.cl_extern ->
								display_error ctx (Printf.sprintf "Field %s has no expression (possible typing order issue)" f.cf_name) f.cf_pos;
								display_error ctx (Printf.sprintf "While building %s" (s_type_path cg.cl_path)) p;
							| _ ->
								()
						end
					| Some e -> f.cf_expr <- Some (generic_substitute_expr gctx e)
				) with Unify_error l ->
					error (error_msg (Unify l)) f.cf_pos) :: !delays);
			f
		in
		if c.cl_init <> None || c.cl_dynamic <> None then error "This class can't be generic" p;
		if c.cl_ordered_statics <> [] then error "A generic class can't have static fields" p;
		cg.cl_super <- (match c.cl_super with
			| None -> None
			| Some (cs,pl) ->
				let find_class subst =
					let rec loop subst = match subst with
						| (TInst(c,[]),t) :: subst when c == cs -> t
						| _ :: subst -> loop subst
						| [] -> raise Not_found
					in
					try
						if pl <> [] then raise Not_found;
						let t = loop subst in
						(* extended type parameter: concrete type must have a constructor, but generic base class must not have one *)
 						begin match follow t,c.cl_constructor with
							| TInst({cl_constructor = None} as cs,_),None -> error ("Cannot use " ^ (s_type_path cs.cl_path) ^ " as type parameter because it is extended and has no constructor") p
							| _,Some cf -> error "Generics extending type parameters cannot have constructors" cf.cf_pos
							| _ -> ()
						end;
						t
					with Not_found ->
						apply_params c.cl_types tl (TInst(cs,pl))
				in
				let ts = follow (find_class gctx.subst) in
				let cs,pl = Typeload.check_extends ctx c ts p in
				match cs.cl_kind with
				| KGeneric ->
					(match build_generic ctx cs p pl with
					| TInst (cs,pl) -> Some (cs,pl)
					| _ -> assert false)
				| _ -> Some(cs,pl)
		);
		Typeload.add_constructor ctx cg false p;
		cg.cl_kind <- KGenericInstance (c,tl);
		cg.cl_interface <- c.cl_interface;
		cg.cl_constructor <- (match cg.cl_constructor, c.cl_constructor, c.cl_super with
			| _, Some c, _ -> Some (build_field c)
			| Some ctor, _, _ -> Some ctor
			| None, None, None -> None
			| _ -> error "Please define a constructor for this class in order to use it as generic" c.cl_pos
		);
		cg.cl_implements <- List.map (fun (i,tl) ->
			(match follow (generic_substitute_type gctx (TInst (i, List.map (generic_substitute_type gctx) tl))) with
			| TInst (i,tl) -> i, tl
			| _ -> assert false)
		) c.cl_implements;
		cg.cl_ordered_fields <- List.map (fun f ->
			let f = build_field f in
			cg.cl_fields <- PMap.add f.cf_name f cg.cl_fields;
			f
		) c.cl_ordered_fields;
		List.iter (fun f -> f()) !delays;
		TInst (cg,[])
	end

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
	let check_used = Common.defined ctx.com Define.CheckXmlProxy in
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
		| TAbstractDecl a ->
			(a.a_pos, ["",a.a_meta],[],[])
	) in
	let filter l =
		let l = List.map (fun (n,ml) -> n, ExtList.List.filter_map (fun (m,el,p) -> match m with Meta.Custom s when String.length s > 0 && s.[0] <> ':' -> Some (s,el,p) | _ -> None) ml) l in
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

let get_macro_path ctx e args p =
	let rec loop e =
		match fst e with
		| EField (e,f) -> f :: loop e
		| EConst (Ident i) -> [i]
		| _ -> error "Invalid macro call" p
	in
	let path = match e with
		| (EConst(Ident i)),_ ->
			let path = try
				if not (PMap.mem i ctx.curclass.cl_statics) then raise Not_found;
				ctx.curclass.cl_path
			with Not_found -> try
				(t_infos (fst (PMap.find i ctx.m.module_globals))).mt_path
			with Not_found ->
				error "Invalid macro call" p
			in
			i :: (snd path) :: (fst path)
		| _ ->
			loop e
	in
	(match path with
	| meth :: cl :: path -> (List.rev path,cl), meth, args
	| _ -> error "Invalid macro call" p)

let build_macro_type ctx pl p =
	let path, field, args = (match pl with
		| [TInst ({ cl_kind = KExpr (ECall (e,args),_) },_)]
		| [TInst ({ cl_kind = KExpr (EArrayDecl [ECall (e,args),_],_) },_)] ->
			get_macro_path ctx e args p
		| _ ->
			error "MacroType requires a single expression call parameter" p
	) in
	let old = ctx.ret in
	let t = (match ctx.g.do_macro ctx MMacroType path field args p with
		| None -> mk_mono()
		| Some _ -> ctx.ret
	) in
	ctx.ret <- old;
	t

let build_macro_build ctx c pl cfl p =
	let path, field, args = match Meta.get Meta.GenericBuild c.cl_meta with
		| _,[ECall(e,args),_],_ -> get_macro_path ctx e args p
		| _ -> error "genericBuild requires a single expression call parameter" p
	in
	let old = ctx.ret,ctx.g.get_build_infos in
	ctx.g.get_build_infos <- (fun() -> Some (TClassDecl c, pl, cfl));
	let t = (match ctx.g.do_macro ctx MMacroType path field args p with
		| None -> mk_mono()
		| Some _ -> ctx.ret
	) in
	ctx.ret <- fst old;
	ctx.g.get_build_infos <- snd old;
	t

(* -------------------------------------------------------------------------- *)
(* API EVENTS *)

let build_instance ctx mtype p =
	match mtype with
	| TClassDecl c ->
		if ctx.pass > PBuildClass then c.cl_build();
		let build f s =
			let r = exc_protect ctx (fun r ->
				let t = mk_mono() in
				r := (fun() -> t);
				unify_raise ctx (f()) t p;
				t
			) s in
			delay ctx PForce (fun() -> ignore ((!r)()));
			TLazy r
		in
		let ft = (fun pl ->
			match c.cl_kind with
			| KGeneric ->
				build (fun () -> build_generic ctx c p pl) "build_generic"
			| KMacroType ->
				build (fun () -> build_macro_type ctx pl p) "macro_type"
			| KGenericBuild cfl ->
				build (fun () -> build_macro_build ctx c pl cfl p) "generic_build"
			| _ ->
				TInst (c,pl)
		) in
		c.cl_types , c.cl_path , ft
	| TEnumDecl e ->
		e.e_types , e.e_path , (fun t -> TEnum (e,t))
	| TTypeDecl t ->
		t.t_types , t.t_path , (fun tl -> TType(t,tl))
	| TAbstractDecl a ->
		a.a_types, a.a_path, (fun tl -> TAbstract(a,tl))

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
	| HExtends { tpackage = ["haxe";"xml"]; tname = "Proxy"; tparams = [TPExpr(EConst (String file),p);TPType t] } ->
		extend_xml_proxy ctx c t file p;
		true
	| _ ->
		true

(* Promotes type parameters of abstracts to their implementation fields *)
let promote_abstract_parameters ctx t = match t with
	| TClassDecl ({cl_kind = KAbstractImpl a} as c) when a.a_types <> [] ->
		List.iter (fun f ->
			List.iter (fun (n,t) -> match t with
				| TInst({cl_kind = KTypeParameter _; cl_path=p,n} as cp,[]) when not (List.mem_assoc n f.cf_params) ->
					let path = List.rev ((snd c.cl_path) :: List.rev (fst c.cl_path)),n in
					f.cf_params <- (n,TInst({cp with cl_path = path},[])) :: f.cf_params
				| _ ->
					()
			) a.a_types;
		) c.cl_ordered_statics;
	| _ ->
		()

(* -------------------------------------------------------------------------- *)
(* ABSTRACT CASTS *)

module Abstract = struct

	let find_to ab pl b =
		if follow b == t_dynamic then
			List.find (fun (t,_) -> follow t == t_dynamic) ab.a_to
		else
			List.find (Type.unify_to_field ab pl b) ab.a_to

	let find_from ab pl a b =
		if follow a == t_dynamic then
			List.find (fun (t,_) -> follow t == t_dynamic) ab.a_from
		else
			List.find (Type.unify_from_field ab pl a b) ab.a_from

	let cast_stack = ref []
	let underlying_type_stack = ref []

	let rec get_underlying_type a pl =
		let maybe_recurse t =
			underlying_type_stack := a :: !underlying_type_stack;
			let t = match follow t with
				| TAbstract(a,tl) when not (Meta.has Meta.CoreType a.a_meta) ->
					if List.mem a !underlying_type_stack then begin
						let s = String.concat " -> " (List.map (fun a -> s_type_path a.a_path) (List.rev (a :: !underlying_type_stack))) in
						(* technically this should be done at type declaration level *)
						error ("Abstract chain detected: " ^ s) a.a_pos
					end;
					get_underlying_type a tl
				| _ ->
					t
			in
			underlying_type_stack := List.tl !underlying_type_stack;
			t
		in
		try
			if not (Meta.has Meta.MultiType a.a_meta) then raise Not_found;
			let m = mk_mono() in
			let _ = find_to a pl m in
			maybe_recurse (follow m)
		with Not_found ->
			if Meta.has Meta.CoreType a.a_meta then
				t_dynamic
			else
				maybe_recurse (apply_params a.a_types pl a.a_this)

	let make_static_call ctx c cf a pl args t p =
		make_static_call ctx c cf (apply_params a.a_types pl) args t p

	let rec do_check_cast ctx tleft eright p =
		let recurse cf f =
			if cf == ctx.curfield || List.mem cf !cast_stack then error "Recursive implicit cast" p;
			cast_stack := cf :: !cast_stack;
			let r = f() in
			cast_stack := List.tl !cast_stack;
			r
		in
		let find a tl f =
			let tcf,cfo = f() in
			let mk_cast () =
				let tcf = apply_params a.a_types tl tcf in
				if type_iseq tcf tleft then
					eright
				else
					(* TODO: causes Java overload issues *)
					(* let eright = mk (TCast(eright,None)) tleft p in *)
					do_check_cast ctx tcf eright p
			in
			match cfo,a.a_impl with
				| None,_ ->
					mk_cast();
				| Some cf,_ when Meta.has Meta.MultiType a.a_meta ->
					mk_cast();
				| Some cf,Some c ->
					recurse cf (fun () -> make_static_call ctx c cf a tl [eright] tleft p)
				| _ ->
					assert false
		in
		if type_iseq tleft eright.etype then
			eright
		else try
			begin match follow eright.etype with
				| TAbstract(a,tl) ->
					find a tl (fun () -> find_to a tl tleft)
				| _ ->
					raise Not_found
			end
		with Not_found -> try
			begin match follow tleft with
				| TAbstract(a,tl) ->
					find a tl (fun () -> find_from a tl eright.etype tleft)
				| _ ->
					raise Not_found
			end
		with Not_found ->
			eright

	let check_cast ctx tleft eright p =
		if ctx.com.display <> DMNone then eright else do_check_cast ctx tleft eright p

	let find_multitype_specialization com a pl p =
		let m = mk_mono() in
		let tl = match Meta.get Meta.MultiType a.a_meta with
			| _,[],_ -> pl
			| _,el,_ ->
				let relevant = Hashtbl.create 0 in
				List.iter (fun e -> match fst e with
					| EConst(Ident s) -> Hashtbl.replace relevant s true
					| _ -> error "Type parameter expected" (pos e)
				) el;
				let tl = List.map2 (fun (n,_) t -> if Hashtbl.mem relevant n || not (has_mono t) then t else t_dynamic) a.a_types pl in
				if com.platform = Js && a.a_path = ([],"Map") then begin match tl with
					| t1 :: _ ->
						let rec loop stack t =
							if List.exists (fun t2 -> fast_eq t t2) stack then
								t
							else begin
								let stack = t :: stack in
								match follow t with
								| TAbstract ({ a_path = [],"Class" },_) ->
									error (Printf.sprintf "Cannot use %s as key type to Map because Class<T> is not comparable" (s_type (print_context()) t1)) p;
								| TEnum(en,tl) ->
									PMap.iter (fun _ ef -> ignore(loop stack ef.ef_type)) en.e_constrs;
									Type.map (loop stack) t
								| t ->
									Type.map (loop stack) t
							end
						in
						ignore(loop [] t1)
					| _ -> assert false
				end;
				tl
		in
		let _,cfo =
			try
				find_to a tl m
			with Not_found ->
				let at = apply_params a.a_types pl a.a_this in
				let st = s_type (print_context()) at in
				if has_mono at then
					error ("Type parameters of multi type abstracts must be known (for " ^ st ^ ")") p
				else
					error ("Abstract " ^ (s_type_path a.a_path) ^ " has no @:to function that accepts " ^ st) p;
		in
		match cfo with
			| None -> assert false
			| Some cf -> cf, follow m

	let handle_abstract_casts ctx e =
		let rec loop ctx e = match e.eexpr with
			| TNew({cl_kind = KAbstractImpl a} as c,pl,el) ->
				(* a TNew of an abstract implementation is only generated if it is a multi type abstract *)
				let cf,m = find_multitype_specialization ctx.com a pl e.epos in
				let e = make_static_call ctx c cf a pl ((mk (TConst TNull) (TAbstract(a,pl)) e.epos) :: el) m e.epos in
				{e with etype = m}
			| TCall({eexpr = TField(_,FStatic({cl_path=[],"Std"},{cf_name = "string"}))},[e1]) when (match follow e1.etype with TAbstract({a_impl = Some _},_) -> true | _ -> false) ->
				begin match follow e1.etype with
					| TAbstract({a_impl = Some c} as a,tl) ->
						begin try
							let cf = PMap.find "toString" c.cl_statics in
							make_static_call ctx c cf a tl [e1] ctx.t.tstring e.epos
						with Not_found ->
							e
						end
					| _ ->
						assert false
				end
			| TCall(e1, el) ->
				begin try
					begin match e1.eexpr with
						| TField(e2,fa) ->
							begin match follow e2.etype with
								| TAbstract(a,pl) when Meta.has Meta.MultiType a.a_meta ->
									let m = get_underlying_type a pl in
									let fname = field_name fa in
									let el = List.map (loop ctx) el in
									begin try
										let ef = mk (TField({e2 with etype = m},quick_field m fname)) e1.etype e2.epos in
										make_call ctx ef el e.etype e.epos
									with Not_found ->
										(* quick_field raises Not_found if m is an abstract, we have to replicate the 'using' call here *)
										match follow m with
										| TAbstract({a_impl = Some c} as a,pl) ->
											let cf = PMap.find fname c.cl_statics in
											make_static_call ctx c cf a pl (e2 :: el) e.etype e.epos
										| _ -> raise Not_found
									end
								| _ -> raise Not_found
							end
						| _ ->
							raise Not_found
					end
				with Not_found ->
					Type.map_expr (loop ctx) e
				end
			| _ ->
				Type.map_expr (loop ctx) e
		in
		loop ctx e
end

module PatternMatchConversion = struct

 	type cctx = {
		ctx : typer;
		mutable eval_stack : ((tvar * pos) * texpr) list list;
		dt_lookup : dt array;
	}

	let is_declared cctx v =
		let rec loop sl = match sl with
			| stack :: sl ->
				List.exists (fun ((v2,_),_) -> v == v2) stack || loop sl
			| [] ->
				false
		in
		loop cctx.eval_stack

	let group_cases cases =
		let dt_eq dt1 dt2 = match dt1,dt2 with
			| DTGoto i1, DTGoto i2 when i1 = i2 -> true
			(* TODO equal bindings *)
			| _ -> false
		in
		match List.rev cases with
		| [] -> []
		| [con,dt] -> [[con],dt]
		| (con,dt) :: cases ->
			let tmp,ldt,cases = List.fold_left (fun (tmp,ldt,acc) (con,dt) ->
				if dt_eq dt ldt then
					(con :: tmp,dt,acc)
				else
					([con],dt,(tmp,ldt) :: acc)
			) ([con],dt,[]) cases in
			match tmp with
			| [] -> cases
			| tmp -> ((tmp,ldt) :: cases)

	let rec convert_dt cctx dt =
		match dt with
		| DTBind (bl,dt) ->
			cctx.eval_stack <- bl :: cctx.eval_stack;
			let e = convert_dt cctx dt in
			cctx.eval_stack <- List.tl cctx.eval_stack;
			let vl,el = List.fold_left (fun (vl,el) ((v,p),e) ->
				if is_declared cctx v then
					vl, (mk (TBinop(OpAssign,mk (TLocal v) v.v_type p,e)) e.etype e.epos) :: el
				else
					((v,Some e) :: vl), el
			) ([],[e]) bl in
			let el_v = List.map (fun (v,eo) -> mk (TVar (v,eo)) cctx.ctx.t.tvoid e.epos) vl in
			mk (TBlock (el_v @ el)) e.etype e.epos
		| DTGoto i ->
			convert_dt cctx (cctx.dt_lookup.(i))
		| DTExpr e ->
			e
		| DTGuard(e,dt1,dt2) ->
			let ethen = convert_dt cctx dt1 in
			mk (TIf(e,ethen,match dt2 with None -> None | Some dt -> Some (convert_dt cctx dt))) ethen.etype (punion e.epos ethen.epos)
		| DTSwitch({eexpr = TMeta((Meta.Exhaustive,_,_),_)},[_,dt],None) ->
			convert_dt cctx dt
		| DTSwitch(e_st,cl,dto) ->
			let def = match dto with None -> None | Some dt -> Some (convert_dt cctx dt) in
			let cases = group_cases cl in
			let cases = List.map (fun (cl,dt) -> cl,convert_dt cctx dt) cases in
			mk (TSwitch(e_st,cases,def)) (mk_mono()) e_st.epos

	let to_typed_ast ctx dt p =
		let first = dt.dt_dt_lookup.(dt.dt_first) in
		let cctx = {
			ctx = ctx;
			dt_lookup = dt.dt_dt_lookup;
			eval_stack = [];
		} in
		let e = convert_dt cctx first in
		let e = { e with epos = p; etype = dt.dt_type} in
		if dt.dt_var_init = [] then
			e
		else begin
			let el_v = List.map (fun (v,eo) -> mk (TVar (v,eo)) cctx.ctx.t.tvoid p) dt.dt_var_init in
			mk (TBlock (el_v @ [e])) dt.dt_type e.epos
		end
end

(* -------------------------------------------------------------------------- *)
(* USAGE *)

let detect_usage com =
	let usage = ref [] in
	List.iter (fun t -> match t with
		| TClassDecl c ->
			let check_constructor c p =
				try
					let _,cf = get_constructor (fun cf -> cf.cf_type) c in
					if Meta.has Meta.Usage cf.cf_meta then
						usage := p :: !usage;
				with Not_found ->
					()
			in
			let rec expr e = match e.eexpr with
				| TField(_,FEnum(_,ef)) when Meta.has Meta.Usage ef.ef_meta ->
					let p = {e.epos with pmin = e.epos.pmax - (String.length ef.ef_name)} in
					usage := p :: !usage;
					Type.iter expr e
				| TField(_,(FAnon cf | FInstance (_,cf) | FStatic (_,cf) | FClosure (_,cf))) when Meta.has Meta.Usage cf.cf_meta ->
					let p = {e.epos with pmin = e.epos.pmax - (String.length cf.cf_name)} in
					usage := p :: !usage;
					Type.iter expr e
				| TLocal v when Meta.has Meta.Usage v.v_meta ->
					usage := e.epos :: !usage
				| TVar (v,_) when com.display = DMPosition && Meta.has Meta.Usage v.v_meta ->
					raise (Typecore.DisplayPosition [e.epos])
				| TFunction tf when com.display = DMPosition && List.exists (fun (v,_) -> Meta.has Meta.Usage v.v_meta) tf.tf_args ->
					raise (Typecore.DisplayPosition [e.epos])
				| TTypeExpr mt when (Meta.has Meta.Usage (t_infos mt).mt_meta) ->
					usage := e.epos :: !usage
				| TNew (c,_,_) ->
					check_constructor c e.epos;
					Type.iter expr e;
				| TCall({eexpr = TConst TSuper},_) ->
					begin match c.cl_super with
						| Some (c,_) ->
							check_constructor c e.epos
						| _ ->
							()
					end
				| _ -> Type.iter expr e
			in
			let field cf = ignore(follow cf.cf_type); match cf.cf_expr with None -> () | Some e -> expr e in
			(match c.cl_constructor with None -> () | Some cf -> field cf);
			(match c.cl_init with None -> () | Some e -> expr e);
			List.iter field c.cl_ordered_statics;
			List.iter field c.cl_ordered_fields;
		| _ -> ()
	) com.types;
	let usage = List.sort (fun p1 p2 ->
		let c = compare p1.pfile p2.pfile in
		if c <> 0 then c else compare p1.pmin p2.pmin
	) !usage in
	raise (Typecore.DisplayPosition usage)

let update_cache_dependencies com =
	let rec check_t m t = match t with
		| TInst(c,tl) ->
			add_dependency m c.cl_module;
			List.iter (check_t m) tl;
		| TEnum(en,tl) ->
			add_dependency m en.e_module;
			List.iter (check_t m) tl;
		| TType(t,tl) ->
			add_dependency m t.t_module;
			List.iter (check_t m) tl;
		| TAbstract(a,tl) ->
			add_dependency m a.a_module;
			List.iter (check_t m) tl;
		| TFun(targs,tret) ->
			List.iter (fun (_,_,t) -> check_t m t) targs;
			check_t m tret;
		| TAnon an ->
			PMap.iter (fun _ cf -> check_field m cf) an.a_fields
		| TMono r ->
			(match !r with
			| Some t -> check_t m t
			| _ -> ())
		| TLazy f ->
			check_t m (!f())
		| TDynamic t ->
			if t == t_dynamic then
				()
			else
				check_t m t
	and check_field m cf =
		check_t m cf.cf_type
	in
	List.iter (fun t -> match t with
		| TClassDecl c ->
			List.iter (check_field c.cl_module) c.cl_ordered_statics;
			List.iter (check_field c.cl_module) c.cl_ordered_fields;
			(match c.cl_constructor with None -> () | Some cf -> check_field c.cl_module cf);
		| _ ->
			()
	) com.types

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
			mk (TVar (tmp, Some e)) t.tvoid e.epos;
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
		stack_save_pos = mk (TVar (pos_var, Some (field stack_e "length" t.tint p))) t.tvoid p;
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

let rec find_field com c f =
	try
		(match c.cl_super with
		| None ->
			raise Not_found
		| Some ( {cl_path = (["cpp"],"FastIterator")}, _ ) ->
			raise Not_found (* This is a strongly typed 'extern' and the usual rules don't apply *)
		| Some (c,_) ->
			find_field com c f)
	with Not_found -> try
		if com.platform = Cpp then (* Cpp uses delegation for interfaces *)
			raise Not_found;
		let rec loop = function
			| [] ->
				raise Not_found
			| (c,_) :: l ->
				try
					find_field com c f
				with
					Not_found -> loop l
		in
		loop c.cl_implements
	with Not_found ->
		let f = PMap.find f.cf_name c.cl_fields in
		(match f.cf_kind with Var { v_read = AccRequire _ } -> raise Not_found | _ -> ());
		f

let fix_override com c f fd =
	let f2 = (try Some (find_field com c f) with Not_found -> None) in
	match f2,fd with
		| Some (f2), Some(fd) ->
			let targs, tret = (match follow f2.cf_type with TFun (args,ret) -> args, ret | _ -> assert false) in
			let changed_args = ref [] in
			let prefix = "_tmp_" in
			let nargs = List.map2 (fun ((v,ct) as cur) (_,_,t2) ->
				try
					type_eq EqStrict (monomorphs c.cl_types (monomorphs f.cf_params v.v_type)) t2;
					(* Flash generates type parameters with a single constraint as that constraint type, so we
					   have to detect this case and change the variable (issue #2712). *)
					begin match follow v.v_type with
						| TInst({cl_kind = KTypeParameter [tc]} as cp,_) when com.platform = Flash ->
							if List.mem_assoc (snd cp.cl_path) c.cl_types then raise (Unify_error [])
						| _ ->
							()
					end;
					cur
				with Unify_error _ ->
					let v2 = alloc_var (prefix ^ v.v_name) t2 in
					changed_args := (v,v2) :: !changed_args;
					v2,ct
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
						let el_v = List.map (fun (v,v2) ->
							mk (TVar (v,Some (mk (TCast (mk (TLocal v2) v2.v_type p,None)) v.v_type p))) com.basic.tvoid p
						) args in
						{ e with eexpr = TBlock (el_v @ el) }
				);
			} in
			(* as3 does not allow wider visibility, so the base method has to be made public *)
			if Common.defined com Define.As3 && f.cf_public then f2.cf_public <- true;
			let targs = List.map (fun(v,c) -> (v.v_name, Option.is_some c, v.v_type)) nargs in
			let fde = (match f.cf_expr with None -> assert false | Some e -> e) in
			f.cf_expr <- Some { fde with eexpr = TFunction fd2 };
			f.cf_type <- TFun(targs,tret);
		| Some(f2), None when c.cl_interface ->
			let targs, tret = (match follow f2.cf_type with TFun (args,ret) -> args, ret | _ -> assert false) in
			f.cf_type <- TFun(targs,tret)
		| _ ->
			()

let fix_overrides com t =
	match t with
	| TClassDecl c ->
		(* overrides can be removed from interfaces *)
		if c.cl_interface then
			c.cl_ordered_fields <- List.filter (fun f ->
				try
					if find_field com c f == f then raise Not_found;
					c.cl_fields <- PMap.remove f.cf_name c.cl_fields;
					false;
				with Not_found ->
					true
			) c.cl_ordered_fields;
		List.iter (fun f ->
			match f.cf_expr, f.cf_kind with
			| Some { eexpr = TFunction fd }, Method (MethNormal | MethInline) ->
				fix_override com c f (Some fd)
			| None, Method (MethNormal | MethInline) when c.cl_interface ->
				fix_override com c f None
			| _ ->
				()
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
			let b = try (find_field com c f) == f
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
	let b64 = "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789+/" in
	let tbl = Array.init (String.length b64) (fun i -> String.get b64 i) in
	Base64.str_encode ~tbl data

(*
	Tells if the constructor might be called without any issue whatever its parameters
*)
let rec constructor_side_effects e =
	match e.eexpr with
	| TBinop (op,_,_) when op <> OpAssign ->
		true
	| TField (_,FEnum _) ->
		false
	| TUnop _ | TArray _ | TField _ | TEnumParameter _ | TCall _ | TNew _ | TFor _ | TWhile _ | TSwitch _ | TPatMatch _ | TReturn _ | TThrow _ ->
		true
	| TBinop _ | TTry _ | TIf _ | TBlock _ | TVar _
	| TFunction _ | TArrayDecl _ | TObjectDecl _
	| TParenthesis _ | TTypeExpr _ | TLocal _ | TMeta _
	| TConst _ | TContinue | TBreak | TCast _ ->
		try
			Type.iter (fun e -> if constructor_side_effects e then raise Exit) e;
			false;
		with Exit ->
			true

(*
	Make a dump of the full typed AST of all types
*)
let rec create_dumpfile acc = function
	| [] -> assert false
	| d :: [] ->
		let ch = open_out (String.concat "/" (List.rev (d :: acc)) ^ ".dump") in
		let buf = Buffer.create 0 in
		buf, (fun () ->
			output_string ch (Buffer.contents buf);
			close_out ch)
	| d :: l ->
		let dir = String.concat "/" (List.rev (d :: acc)) in
		if not (Sys.file_exists dir) then Unix.mkdir dir 0o755;
		create_dumpfile (d :: acc) l

let dump_types com =
	let s_type = s_type (Type.print_context()) in
	let params = function [] -> "" | l -> Printf.sprintf "<%s>" (String.concat "," (List.map (fun (n,t) -> n ^ " : " ^ s_type t) l)) in
	let s_expr = try if Common.defined_value com Define.Dump = "pretty" then Type.s_expr_pretty "\t" else Type.s_expr with Not_found -> Type.s_expr in
	List.iter (fun mt ->
		let path = Type.t_path mt in
		let buf,close = create_dumpfile [] ("dump" :: (Common.platform_name com.platform) :: fst path @ [snd path]) in
		let print fmt = Printf.kprintf (fun s -> Buffer.add_string buf s) fmt in
		(match mt with
		| Type.TClassDecl c ->
			let rec print_field stat f =
				print "\t%s%s%s%s" (if stat then "static " else "") (if f.cf_public then "public " else "") f.cf_name (params f.cf_params);
				print "(%s) : %s" (s_kind f.cf_kind) (s_type f.cf_type);
				(match f.cf_expr with
				| None -> ()
				| Some e -> print "\n\n\t = %s" (s_expr s_type e));
				print ";\n\n";
				List.iter (fun f -> print_field stat f) f.cf_overloads
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
		| Type.TAbstractDecl a ->
			print "%sabstract %s%s {}" (if a.a_private then "private " else "") (s_type_path path) (params a.a_types);
		);
		close();
	) com.types

let dump_dependencies com =
	let buf,close = create_dumpfile [] ["dump";Common.platform_name com.platform;".dependencies"] in
	let print fmt = Printf.kprintf (fun s -> Buffer.add_string buf s) fmt in
	let dep = Hashtbl.create 0 in
	List.iter (fun m ->
		print "%s:\n" m.m_extra.m_file;
		PMap.iter (fun _ m2 ->
			print "\t%s\n" (m2.m_extra.m_file);
			let l = try Hashtbl.find dep m2.m_extra.m_file with Not_found -> [] in
			Hashtbl.replace dep m2.m_extra.m_file (m :: l)
		) m.m_extra.m_deps;
	) com.Common.modules;
	close();
	let buf,close = create_dumpfile [] ["dump";Common.platform_name com.platform;".dependants"] in
	let print fmt = Printf.kprintf (fun s -> Buffer.add_string buf s) fmt in
	Hashtbl.iter (fun n ml ->
		print "%s:\n" n;
		List.iter (fun m ->
			print "\t%s\n" (m.m_extra.m_file);
		) ml;
	) dep;
	close()

(*
	Build a default safe-cast expression :
	{ var $t = <e>; if( Std.is($t,<t>) ) $t else throw "Class cast error"; }
*)
let default_cast ?(vtmp="$t") com e texpr t p =
	let api = com.basic in
	let mk_texpr = function
		| TClassDecl c -> TAnon { a_fields = PMap.empty; a_status = ref (Statics c) }
		| TEnumDecl e -> TAnon { a_fields = PMap.empty; a_status = ref (EnumStatics e) }
		| TAbstractDecl a -> TAnon { a_fields = PMap.empty; a_status = ref (AbstractStatics a) }
		| TTypeDecl _ -> assert false
	in
	let vtmp = alloc_var vtmp e.etype in
	let var = mk (TVar (vtmp,Some e)) api.tvoid p in
	let vexpr = mk (TLocal vtmp) e.etype p in
	let texpr = mk (TTypeExpr texpr) (mk_texpr texpr) p in
	let std = (try List.find (fun t -> t_path t = ([],"Std")) com.types with Not_found -> assert false) in
	let fis = (try
			let c = (match std with TClassDecl c -> c | _ -> assert false) in
			FStatic (c, PMap.find "is" c.cl_statics)
		with Not_found ->
			assert false
	) in
	let std = mk (TTypeExpr std) (mk_texpr std) p in
	let is = mk (TField (std,fis)) (tfun [t_dynamic;t_dynamic] api.tbool) p in
	let is = mk (TCall (is,[vexpr;texpr])) api.tbool p in
	let exc = mk (TThrow (mk (TConst (TString "Class cast error")) api.tstring p)) t p in
	let check = mk (TIf (mk_parent is,mk (TCast (vexpr,None)) t p,Some exc)) t p in
	mk (TBlock [var;check;vexpr]) t p

(** Overload resolution **)
module Overloads =
struct
	let rec simplify_t t = match t with
		| TInst _ | TEnum _ | TAbstract({ a_impl = None }, _) ->
			t
		| TAbstract(a,tl) -> simplify_t (Abstract.get_underlying_type a tl)
		| TType(({ t_path = [],"Null" } as t), [t2]) -> (match simplify_t t2 with
			| (TAbstract({ a_impl = None }, _) | TEnum _ as t2) -> TType(t, [simplify_t t2])
			| t2 -> t2)
		| TType(t, tl) ->
			simplify_t (apply_params t.t_types tl t.t_type)
		| TMono r -> (match !r with
			| Some t -> simplify_t t
			| None -> t_dynamic)
		| TAnon _ -> t_dynamic
		| TDynamic _ -> t
		| TLazy f -> simplify_t (!f())
		| TFun _ -> t

	(* rate type parameters *)
	let rate_tp tlfun tlarg =
		let acc = ref 0 in
		List.iter2 (fun f a -> if not (type_iseq f a) then incr acc) tlfun tlarg;
		!acc

	let rec rate_conv cacc tfun targ =
		match simplify_t tfun, simplify_t targ with
		| TInst({ cl_interface = true } as cf, tlf), TInst(ca, tla) ->
			(* breadth-first *)
			let stack = ref [0,ca,tla] in
			let cur = ref (0, ca,tla) in
			let rec loop () =
				match !stack with
				| [] -> (let acc, ca, tla = !cur in match ca.cl_super with
					| None -> raise Not_found
					| Some (sup,tls) ->
						cur := (acc+1,sup,List.map (apply_params ca.cl_types tla) tls);
						stack := [!cur];
						loop())
				| (acc,ca,tla) :: _ when ca == cf ->
					acc,tla
				| (acc,ca,tla) :: s ->
					stack := s @ List.map (fun (c,tl) -> (acc+1,c,List.map (apply_params ca.cl_types tla) tl)) ca.cl_implements;
					loop()
			in
			let acc, tla = loop() in
			(cacc + acc, rate_tp tlf tla)
		| TInst(cf,tlf), TInst(ca,tla) ->
			let rec loop acc ca tla =
				if cf == ca then
					acc, tla
				else match ca.cl_super with
				| None -> raise Not_found
				| Some(sup,stl) ->
					loop (acc+1) sup (List.map (apply_params ca.cl_types tla) stl)
			in
			let acc, tla = loop 0 ca tla in
			(cacc + acc, rate_tp tlf tla)
		| TEnum(ef,tlf), TEnum(ea, tla) ->
			if ef != ea then raise Not_found;
			(cacc, rate_tp tlf tla)
		| TDynamic _, TDynamic _ ->
			(cacc, 0)
		| TDynamic _, _ ->
			(max_int, 0) (* a function with dynamic will always be worst of all *)
		| TAbstract({ a_impl = None }, _), TDynamic _ ->
			(cacc + 2, 0) (* a dynamic to a basic type will have an "unboxing" penalty *)
		| _, TDynamic _ ->
			(cacc + 1, 0)
		| TAbstract(af,tlf), TAbstract(aa,tla) ->
			(if af == aa then
				(cacc, rate_tp tlf tla)
			else
				let ret = ref None in
				if List.exists (fun (t,_) -> try
					ret := Some (rate_conv (cacc+1) (apply_params af.a_types tlf t) targ);
					true
				with | Not_found ->
					false
				) af.a_from then
					Option.get !ret
			else
				if List.exists (fun (t,_) -> try
					ret := Some (rate_conv (cacc+1) tfun (apply_params aa.a_types tla t));
					true
				with | Not_found ->
					false
				) aa.a_to then
					Option.get !ret
			else
				raise Not_found)
		| TType({ t_path = [], "Null" }, [tf]), TType({ t_path = [], "Null" }, [ta]) ->
			rate_conv (cacc+0) tf ta
		| TType({ t_path = [], "Null" }, [tf]), ta ->
			rate_conv (cacc+1) tf ta
		| tf, TType({ t_path = [], "Null" }, [ta]) ->
			rate_conv (cacc+1) tf ta
		| TFun _, TFun _ -> (* unify will make sure they are compatible *)
			cacc,0
		| tfun,targ ->
			raise Not_found

	let is_best arg1 arg2 =
		(List.for_all2 (fun v1 v2 ->
			v1 <= v2)
		arg1 arg2) && (List.exists2 (fun v1 v2 ->
			v1 < v2)
		arg1 arg2)

	let rec rm_duplicates acc ret = match ret with
		| [] -> acc
		| ( el, t ) :: ret when List.exists (fun (_,t2) -> type_iseq t t2) acc ->
			rm_duplicates acc ret
		| r :: ret ->
			rm_duplicates (r :: acc) ret

	let s_options rated =
		String.concat ",\n" (List.map (fun ((_,t),rate) ->
			"( " ^ (String.concat "," (List.map (fun (i,i2) -> string_of_int i ^ ":" ^ string_of_int i2) rate)) ^ " ) => " ^ (s_type (print_context()) t)
		) rated)

	let count_optionals elist =
		List.fold_left (fun acc (_,is_optional) -> if is_optional then acc + 1 else acc) 0 elist

	let rec fewer_optionals acc compatible = match acc, compatible with
		| _, [] -> acc
		| [], c :: comp -> fewer_optionals [c] comp
		| (elist_acc, _) :: _, ((elist, _) as cur) :: comp ->
			let acc_opt = count_optionals elist_acc in
			let comp_opt = count_optionals elist in
			if acc_opt = comp_opt then
				fewer_optionals (cur :: acc) comp
			else if acc_opt < comp_opt then
				fewer_optionals acc comp
			else
				fewer_optionals [cur] comp

	let reduce_compatible compatible = match fewer_optionals [] (rm_duplicates [] compatible) with
		| [] -> [] | [v] -> [v]
		| compatible ->
			(* convert compatible into ( rate * compatible_type ) list *)
			let rec mk_rate acc elist args = match elist, args with
				| [], [] -> acc
				| (_,true) :: elist, _ :: args -> mk_rate acc elist args
				| (e,false) :: elist, (n,o,t) :: args ->
					mk_rate (rate_conv 0 t e.etype :: acc) elist args
				| _ -> assert false
			in

			let rated = ref [] in
			List.iter (function
				| (elist,TFun(args,ret)) -> (try
					rated := ( (elist,TFun(args,ret)), mk_rate [] elist args ) :: !rated
					with | Not_found ->  ())
				| _ -> assert false
			) compatible;

			let rec loop best rem = match best, rem with
				| _, [] -> best
				| [], r1 :: rem -> loop [r1] rem
				| (bover, bargs) :: b1, (rover, rargs) :: rem ->
					if is_best bargs rargs then
						loop best rem
					else if is_best rargs bargs then
						loop (loop b1 [rover,rargs]) rem
					else (* equally specific *)
						loop ( (rover,rargs) :: best ) rem
			in

			List.map fst (loop [] !rated)
end;;


module UnificationCallback = struct
	let tf_stack = ref []

	let check_call_params f el tl =
		let rec loop acc el tl = match el,tl with
			| e :: el, (n,_,t) :: tl ->
				loop ((f e t) :: acc) el tl
			| [], [] ->
				acc
			| [],_ ->
				acc
			| e :: el, [] ->
				loop (e :: acc) el []
		in
		List.rev (loop [] el tl)

	let check_call f el t = match follow t with
		| TFun(args,_) ->
			check_call_params f el args
		| _ ->
			el

	let rec run f e =
		let f e t =
			(* TODO: I don't think this should cause errors on Flash target *)
			(* if not (type_iseq e.etype t) then f e t else e *)
			f e t
		in
		let check e = match e.eexpr with
			| TBinop((OpAssign | OpAssignOp _ as op),e1,e2) ->
				let e2 = f e2 e1.etype in
				{e with eexpr = TBinop(op,e1,e2)}
			| TVar(v,Some e) ->
				let eo = Some (f e v.v_type) in
				{ e with eexpr = TVar(v,eo) }
			| TCall(e1,el) ->
				let el = check_call f el e1.etype in
				{e with eexpr = TCall(e1,el)}
			| TNew(c,tl,el) ->
				begin try
					let tcf,_ = get_constructor (fun cf -> apply_params c.cl_types tl cf.cf_type) c in
					let el = check_call f el tcf in
					{e with eexpr = TNew(c,tl,el)}
				with Not_found ->
					e
				end
			| TArrayDecl el ->
				begin match follow e.etype with
					| TInst({cl_path=[],"Array"},[t]) -> {e with eexpr = TArrayDecl(List.map (fun e -> f e t) el)}
					| _ -> e
				end
			| TObjectDecl fl ->
				begin match follow e.etype with
					| TAnon an ->
						let fl = List.map (fun (n,e) ->
							let e = try
								let t = (PMap.find n an.a_fields).cf_type in
								f e t
							with Not_found ->
								e
							in
							n,e
						) fl in
						{ e with eexpr = TObjectDecl fl }
					| _ -> e
				end
			| TReturn (Some e1) ->
				begin match !tf_stack with
					| tf :: _ -> { e with eexpr = TReturn (Some (f e1 tf.tf_type))}
					| _ -> e
				end
			| _ ->
				e
		in
		match e.eexpr with
			| TFunction tf ->
				tf_stack := tf :: !tf_stack;
				let etf = {e with eexpr = TFunction({tf with tf_expr = run f tf.tf_expr})} in
				tf_stack := List.tl !tf_stack;
				etf
			| _ ->
				check (Type.map_expr (run f) e)
end;;

module DeprecationCheck = struct

	let curclass = ref null_class

	let warned_positions = Hashtbl.create 0

	let print_deprecation_message com meta s p_usage =
		let s = match meta with
			| _,[EConst(String s),_],_ -> s
			| _ -> Printf.sprintf "Usage of this %s is deprecated" s
		in
		if not (Hashtbl.mem warned_positions p_usage) then begin
			Hashtbl.replace warned_positions p_usage true;
			com.warning s p_usage;
		end

	let check_meta com meta s p_usage =
		try
			print_deprecation_message com (Meta.get Meta.Deprecated meta) s p_usage;
		with Not_found ->
			()

	let check_cf com cf p = check_meta com cf.cf_meta "field" p

	let check_class com c p = if c != !curclass then check_meta com c.cl_meta "class" p

	let check_enum com en p = check_meta com en.e_meta "enum" p

	let check_ef com ef p = check_meta com ef.ef_meta "enum field" p

	let check_typedef com t p = check_meta com t.t_meta "typedef" p

	let check_module_type com mt p = match mt with
		| TClassDecl c -> check_class com c p
		| TEnumDecl en -> check_enum com en p
		| _ -> ()

	let run com =
		let rec expr e = match e.eexpr with
			| TField(e1,fa) ->
				expr e1;
				begin match fa with
					| FStatic(c,cf) | FInstance(c,cf) ->
						check_class com c e.epos;
						check_cf com cf e.epos
					| FAnon cf ->
						check_cf com cf e.epos
					| FClosure(co,cf) ->
						(match co with None -> () | Some c -> check_class com c e.epos);
						check_cf com cf e.epos
					| FEnum(en,ef) ->
						check_enum com en e.epos;
						check_ef com ef e.epos;
					| _ ->
						()
				end
			| TNew(c,_,el) ->
				List.iter expr el;
				check_class com c e.epos;
				(match c.cl_constructor with None -> () | Some cf -> check_cf com cf e.epos)
			| TTypeExpr(mt) | TCast(_,Some mt) ->
				check_module_type com mt e.epos
			| TMeta((Meta.Deprecated,_,_) as meta,e1) ->
				print_deprecation_message com meta "field" e1.epos;
				expr e1;
			| _ ->
				Type.iter expr e
		in
		List.iter (fun t -> match t with
			| TClassDecl c ->
				curclass := c;
				let field cf = match cf.cf_expr with None -> () | Some e -> expr e in
				(match c.cl_constructor with None -> () | Some cf -> field cf);
				(match c.cl_init with None -> () | Some e -> expr e);
				List.iter field c.cl_ordered_statics;
				List.iter field c.cl_ordered_fields;
			| _ ->
				()
		) com.types
end
