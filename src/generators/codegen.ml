(*
	The Haxe Compiler
	Copyright (C) 2005-2016  Haxe Foundation

	This program is free software; you can redistribute it and/or
	modify it under the terms of the GNU General Public License
	as published by the Free Software Foundation; either version 2
	of the License, or (at your option) any later version.

	This program is distributed in the hope that it will be useful,
	but WITHOUT ANY WARRANTY; without even the implied warranty of
	MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
	GNU General Public License for more details.

	You should have received a copy of the GNU General Public License
	along with this program; if not, write to the Free Software
	Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA  02110-1301, USA.
 *)

open Ast
open Type
open Common
open Typecore

(* -------------------------------------------------------------------------- *)
(* TOOLS *)

(* Collection of functions that return expressions *)
module ExprBuilder = struct
	let make_static_this c p =
		let ta = TAnon { a_fields = c.cl_statics; a_status = ref (Statics c) } in
		mk (TTypeExpr (TClassDecl c)) ta p

	let make_static_field c cf p =
		let e_this = make_static_this c p in
		mk (TField(e_this,FStatic(c,cf))) cf.cf_type p

	let make_int com i p =
		mk (TConst (TInt (Int32.of_int i))) com.basic.tint p

	let make_float com f p =
		mk (TConst (TFloat f)) com.basic.tfloat p

	let make_bool com b p =
		mk (TConst(TBool b)) com.basic.tbool p

	let make_string com s p =
		mk (TConst (TString s)) com.basic.tstring p

	let make_null t p =
		mk (TConst TNull) t p

	let make_local v p =
		mk (TLocal v) v.v_type p

	let make_const_texpr com ct p = match ct with
		| TString s -> mk (TConst (TString s)) com.basic.tstring p
		| TInt i -> mk (TConst (TInt i)) com.basic.tint p
		| TFloat f -> mk (TConst (TFloat f)) com.basic.tfloat p
		| TBool b -> mk (TConst (TBool b)) com.basic.tbool p
		| TNull -> mk (TConst TNull) (com.basic.tnull (mk_mono())) p
		| _ -> error "Unsupported constant" p
end

let field e name t p =
	mk (TField (e,try quick_field e.etype name with Not_found -> assert false)) t p

let fcall e name el ret p =
	let ft = tfun (List.map (fun e -> e.etype) el) ret in
	mk (TCall (field e name ft p,el)) ret p

let mk_parent e =
	mk (TParenthesis e) e.etype e.epos

let binop op a b t p =
	mk (TBinop (op,a,b)) t p

let index com e index t p =
	mk (TArray (e,mk (TConst (TInt (Int32.of_int index))) com.basic.tint p)) t p

let maybe_cast e t =
	try
		type_eq EqDoNotFollowNull e.etype t;
		e
	with
		Unify_error _ -> mk (TCast(e,None)) t e.epos

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
			PMap.add n cf fields,(n, ExprBuilder.make_string com v p) :: values
		) (PMap.empty,[]) props in
		let t = mk_anon fields in
		let e = mk (TObjectDecl values) t p in
		let cf = mk_field "__properties__" t p in
		cf.cf_expr <- Some e;
		c.cl_statics <- PMap.add cf.cf_name cf c.cl_statics;
		c.cl_ordered_statics <- cf :: c.cl_ordered_statics

let is_removable_field ctx f =
	Meta.has Meta.Extern f.cf_meta || Meta.has Meta.Generic f.cf_meta
	|| (match f.cf_kind with
		| Var {v_read = AccRequire (s,_)} -> true
		| Method MethMacro -> not ctx.in_macro
		| _ -> false)

let escape_res_name name allow_dirs =
	ExtString.String.replace_chars (fun chr ->
		if (chr >= 'a' && chr <= 'z') || (chr >= 'A' && chr <= 'Z') || (chr >= '0' && chr <= '9') || chr = '_' || chr = '.' then
			Char.escaped chr
		else if chr = '/' && allow_dirs then
			"/"
		else
			"-x" ^ (string_of_int (Char.code chr))) name

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
		) ml)) t_dynamic p
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
(* ABSTRACT CASTS *)

module AbstractCast = struct

	let cast_stack = ref []

	let make_static_call ctx c cf a pl args t p =
		if cf.cf_kind = Method MethMacro then begin
			match args with
				| [e] ->
					let e,f = push_this ctx e in
					ctx.with_type_stack <- (WithType t) :: ctx.with_type_stack;
					let e = match ctx.g.do_macro ctx MExpr c.cl_path cf.cf_name [e] p with
						| Some e -> type_expr ctx e Value
						| None ->  type_expr ctx (EConst (Ident "null"),p) Value
					in
					ctx.with_type_stack <- List.tl ctx.with_type_stack;
					f();
					e
				| _ -> assert false
		end else
			make_static_call ctx c cf (apply_params a.a_params pl) args t p

	let do_check_cast ctx tleft eright p =
		let recurse cf f =
			if cf == ctx.curfield || List.mem cf !cast_stack then error "Recursive implicit cast" p;
			cast_stack := cf :: !cast_stack;
			let r = f() in
			cast_stack := List.tl !cast_stack;
			r
		in
		let find a tl f =
			let tcf,cf = f() in
			if (Meta.has Meta.MultiType a.a_meta) then
				mk_cast eright tleft p
			else match a.a_impl with
				| Some c -> recurse cf (fun () ->
					let ret = make_static_call ctx c cf a tl [eright] tleft p in
					{ ret with eexpr = TMeta( (Meta.ImplicitCast,[],ret.epos), ret) }
				)
				| None -> assert false
		in
		if type_iseq tleft eright.etype then
			eright
		else begin
			let rec loop tleft tright = match follow tleft,follow tright with
			| TAbstract(a1,tl1),TAbstract(a2,tl2) ->
				begin try find a2 tl2 (fun () -> Abstract.find_to a2 tl2 tleft)
				with Not_found -> try find a1 tl1 (fun () -> Abstract.find_from a1 tl1 eright.etype tleft)
				with Not_found -> raise Not_found
				end
			| TAbstract(a,tl),_ ->
				begin try find a tl (fun () -> Abstract.find_from a tl eright.etype tleft)
				with Not_found ->
					let rec loop2 tcl = match tcl with
						| tc :: tcl ->
							if not (type_iseq tc tleft) then loop (apply_params a.a_params tl tc) tright
							else loop2 tcl
						| [] -> raise Not_found
					in
					loop2 a.a_from
				end
			| _,TAbstract(a,tl) ->
				begin try find a tl (fun () -> Abstract.find_to a tl tleft)
				with Not_found ->
					let rec loop2 tcl = match tcl with
						| tc :: tcl ->
							if not (type_iseq tc tright) then loop tleft (apply_params a.a_params tl tc)
							else loop2 tcl
						| [] -> raise Not_found
					in
					loop2 a.a_to
				end
			| _ ->
				raise Not_found
			in
			loop tleft eright.etype
		end

	let cast_or_unify_raise ctx tleft eright p =
		try
			(* can't do that anymore because this might miss macro calls (#4315) *)
			(* if ctx.com.display <> DMNone then raise Not_found; *)
			do_check_cast ctx tleft eright p
		with Not_found ->
			unify_raise ctx eright.etype tleft p;
			eright

	let cast_or_unify ctx tleft eright p =
		try
			cast_or_unify_raise ctx tleft eright p
		with Error (Unify l,p) ->
			raise_or_display ctx l p;
			eright

	let find_array_access_raise ctx a pl e1 e2o p =
		let is_set = e2o <> None in
		let ta = apply_params a.a_params pl a.a_this in
		let rec loop cfl = match cfl with
			| [] -> raise Not_found
			| cf :: cfl ->
				let monos = List.map (fun _ -> mk_mono()) cf.cf_params in
				let map t = apply_params a.a_params pl (apply_params cf.cf_params monos t) in
				let check_constraints () =
					List.iter2 (fun m (name,t) -> match follow t with
						| TInst ({ cl_kind = KTypeParameter constr },_) when constr <> [] ->
							List.iter (fun tc -> match follow m with TMono _ -> raise (Unify_error []) | _ -> Type.unify m (map tc) ) constr
						| _ -> ()
					) monos cf.cf_params;
				in
				match follow (map cf.cf_type) with
				| TFun([(_,_,tab);(_,_,ta1);(_,_,ta2)],r) as tf when is_set ->
					begin try
						Type.unify tab ta;
						let e1 = cast_or_unify_raise ctx ta1 e1 p in
						let e2o = match e2o with None -> None | Some e2 -> Some (cast_or_unify_raise ctx ta2 e2 p) in
						check_constraints();
						cf,tf,r,e1,e2o
					with Unify_error _ | Error (Unify _,_) ->
						loop cfl
					end
				| TFun([(_,_,tab);(_,_,ta1)],r) as tf when not is_set ->
					begin try
						Type.unify tab ta;
						let e1 = cast_or_unify_raise ctx ta1 e1 p in
						check_constraints();
						cf,tf,r,e1,None
					with Unify_error _ | Error (Unify _,_) ->
						loop cfl
					end
				| _ -> loop cfl
		in
		loop a.a_array

	let find_array_access ctx a tl e1 e2o p =
		try find_array_access_raise ctx a tl e1 e2o p
		with Not_found -> match e2o with
			| None ->
				error (Printf.sprintf "No @:arrayAccess function accepts argument of %s" (s_type (print_context()) e1.etype)) p
			| Some e2 ->
				error (Printf.sprintf "No @:arrayAccess function accepts arguments of %s and %s" (s_type (print_context()) e1.etype) (s_type (print_context()) e2.etype)) p

	let find_multitype_specialization com a pl p =
		let m = mk_mono() in
		let tl = match Meta.get Meta.MultiType a.a_meta with
			| _,[],_ -> pl
			| _,el,_ ->
				let relevant = Hashtbl.create 0 in
				List.iter (fun e ->
					let rec loop f e = match fst e with
						| EConst(Ident s) ->
							Hashtbl.replace relevant s f
						| EMeta((Meta.Custom ":followWithAbstracts",_,_),e1) ->
							loop Abstract.follow_with_abstracts e1;
						| _ ->
							error "Type parameter expected" (pos e)
					in
					loop (fun t -> t) e
				) el;
				let tl = List.map2 (fun (n,_) t ->
					try
						(Hashtbl.find relevant n) t
					with Not_found ->
						if not (has_mono t) then t
						else t_dynamic
				) a.a_params pl in
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
		let _,cf =
			try
				Abstract.find_to a tl m
			with Not_found ->
				let at = apply_params a.a_params pl a.a_this in
				let st = s_type (print_context()) at in
				if has_mono at then
					error ("Type parameters of multi type abstracts must be known (for " ^ st ^ ")") p
				else
					error ("Abstract " ^ (s_type_path a.a_path) ^ " has no @:to function that accepts " ^ st) p;
		in
		cf, follow m

	let handle_abstract_casts ctx e =
		let rec loop ctx e = match e.eexpr with
			| TNew({cl_kind = KAbstractImpl a} as c,pl,el) ->
				if not (Meta.has Meta.MultiType a.a_meta) then begin
					(* This must have been a @:generic expansion with a { new } constraint (issue #4364). In this case
					   let's construct the underlying type. *)
					match Abstract.get_underlying_type a pl with
					| TInst(c,tl) as t -> {e with eexpr = TNew(c,tl,el); etype = t}
					| _ -> error ("Cannot construct " ^ (s_type (print_context()) (TAbstract(a,pl)))) e.epos
				end else begin
					(* a TNew of an abstract implementation is only generated if it is a multi type abstract *)
					let cf,m = find_multitype_specialization ctx.com a pl e.epos in
					let e = make_static_call ctx c cf a pl ((mk (TConst TNull) (TAbstract(a,pl)) e.epos) :: el) m e.epos in
					{e with etype = m}
				end
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
					let rec find_abstract e = match follow e.etype,e.eexpr with
						| TAbstract(a,pl),_ when Meta.has Meta.MultiType a.a_meta -> a,pl,e
						| _,TCast(e1,None) -> find_abstract e1
						| _ -> raise Not_found
					in
					let rec find_field e1 =
						match e1.eexpr with
						| TCast(e2,None) ->
							{e1 with eexpr = TCast(find_field e2,None)}
						| TField(e2,fa) ->
							let a,pl,e2 = find_abstract e2 in
							let m = Abstract.get_underlying_type a pl in
							let fname = field_name fa in
							let el = List.map (loop ctx) el in
							begin try
								let fa = quick_field m fname in
								let get_fun_type t = match follow t with
									| TFun(_,tr) as tf -> tf,tr
									| _ -> raise Not_found
								in
								let tf,tr = match fa with
									| FStatic(_,cf) -> get_fun_type cf.cf_type
									| FInstance(c,tl,cf) -> get_fun_type (apply_params c.cl_params tl cf.cf_type)
									| FAnon cf -> get_fun_type cf.cf_type
									| _ -> raise Not_found
								in
								let ef = mk (TField({e2 with etype = m},fa)) tf e2.epos in
								let ecall = make_call ctx ef el tr e.epos in
								if not (type_iseq ecall.etype e.etype) then
									mk (TCast(ecall,None)) e.etype e.epos
								else
									ecall
							with Not_found ->
								(* quick_field raises Not_found if m is an abstract, we have to replicate the 'using' call here *)
								match follow m with
								| TAbstract({a_impl = Some c} as a,pl) ->
									let cf = PMap.find fname c.cl_statics in
									make_static_call ctx c cf a pl (e2 :: el) e.etype e.epos
								| _ -> raise Not_found
							end
						| _ ->
							raise Not_found
					in
					find_field e1
				with Not_found ->
					Type.map_expr (loop ctx) e
				end
			| _ ->
				Type.map_expr (loop ctx) e
		in
		loop ctx e
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
				| TField(_,(FAnon cf | FInstance (_,_,cf) | FStatic (_,cf) | FClosure (_,cf))) when Meta.has Meta.Usage cf.cf_meta ->
					let p = {e.epos with pmin = e.epos.pmax - (String.length cf.cf_name)} in
					usage := p :: !usage;
					Type.iter expr e
				| TLocal v when Meta.has Meta.Usage v.v_meta ->
					usage := e.epos :: !usage
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
	raise (Display.DisplayPosition usage)

let update_cache_dependencies t =
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
	match t with
		| TClassDecl c ->
			List.iter (check_field c.cl_module) c.cl_ordered_statics;
			List.iter (check_field c.cl_module) c.cl_ordered_fields;
			(match c.cl_constructor with None -> () | Some cf -> check_field c.cl_module cf);
		| _ ->
			()

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
	let stack_var = alloc_var stack_var st p in
	let exc_var = alloc_var exc_var st p in
	let pos_var = alloc_var pos_var t.tint p in
	let stack_e = mk (TLocal stack_var) st p in
	let exc_e = mk (TLocal exc_var) st p in
	let stack_pop = fcall stack_e "pop" [] t.tstring p in
	let stack_push c m =
		fcall stack_e "push" [
			if use_add then
				binop OpAdd (ExprBuilder.make_string com (s_type_path c.cl_path ^ "::") p) (ExprBuilder.make_string com m p) t.tstring p
			else
				ExprBuilder.make_string com (s_type_path c.cl_path ^ "::" ^ m) p
		] t.tvoid p
	in
	let stack_return e =
		let tmp = alloc_var tmp_var e.etype e.epos in
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
					type_eq EqStrict (monomorphs c.cl_params (monomorphs f.cf_params v.v_type)) t2;
					(* Flash generates type parameters with a single constraint as that constraint type, so we
					   have to detect this case and change the variable (issue #2712). *)
					begin match follow v.v_type with
						| TInst({cl_kind = KTypeParameter [tc]} as cp,_) when com.platform = Flash ->
							if List.mem_assoc (snd cp.cl_path) c.cl_params then raise (Unify_error [])
						| _ ->
							()
					end;
					cur
				with Unify_error _ ->
					let v2 = alloc_var (prefix ^ v.v_name) t2 v.v_pos in
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
		| _ -> is_volatile (apply_params t.t_params tl t.t_type))
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
	| TUnop _ | TArray _ | TField _ | TEnumParameter _ | TCall _ | TNew _ | TFor _ | TWhile _ | TSwitch _ | TReturn _ | TThrow _ ->
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

module Dump = struct
	let make_valid_filename s =
		let r = Str.regexp "[^A-Za-z0-9_\\-\\.,]" in
		Str.global_substitute r (fun s -> "_") s

	let rec create_file ext acc = function
		| [] -> assert false
		| d :: [] ->
			let d = make_valid_filename d in
			let ch = open_out (String.concat "/" (List.rev (d :: acc)) ^ ext) in
			ch
		| d :: l ->
			let dir = String.concat "/" (List.rev (d :: acc)) in
			if not (Sys.file_exists dir) then Unix.mkdir dir 0o755;
			create_file ext (d :: acc) l

	(*
		Make a dump of the full typed AST of all types
	*)
	let create_dumpfile acc l =
		let ch = create_file ".dump" acc l in
		let buf = Buffer.create 0 in
		buf, (fun () ->
			output_string ch (Buffer.contents buf);
			close_out ch)

	let create_dumpfile_from_path com path =
		let buf,close = create_dumpfile [] ("dump" :: (Common.platform_name com.platform) :: fst path @ [snd path]) in
		buf,close

	let dump_types com s_expr =
		let s_type = s_type (Type.print_context()) in
		let params tl = match tl with [] -> "" | l -> Printf.sprintf "<%s>" (String.concat "," (List.map (fun (n,t) -> n ^ " : " ^ s_type t) l)) in
		List.iter (fun mt ->
			let path = Type.t_path mt in
			let buf,close = create_dumpfile_from_path com path in
			let print fmt = Printf.kprintf (fun s -> Buffer.add_string buf s) fmt in
			(match mt with
			| Type.TClassDecl c ->
				let rec print_field stat f =
					print "\t%s%s%s%s" (if stat then "static " else "") (if f.cf_public then "public " else "") f.cf_name (params f.cf_params);
					print "(%s) : %s" (s_kind f.cf_kind) (s_type f.cf_type);
					(match f.cf_expr with
					| None -> ()
					| Some e -> print "\n\n\t = %s" (s_expr s_type e));
					print "\n\n";
					List.iter (fun f -> print_field stat f) f.cf_overloads
				in
				print "%s%s%s %s%s" (if c.cl_private then "private " else "") (if c.cl_extern then "extern " else "") (if c.cl_interface then "interface" else "class") (s_type_path path) (params c.cl_params);
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
				(match c.cl_init with
				| None -> ()
				| Some e ->
					print "\n\n\t__init__ = ";
					print "%s" (s_expr s_type e);
					print "}\n");
				print "}";
			| Type.TEnumDecl e ->
				print "%s%senum %s%s {\n" (if e.e_private then "private " else "") (if e.e_extern then "extern " else "") (s_type_path path) (params e.e_params);
				List.iter (fun n ->
					let f = PMap.find n e.e_constrs in
					print "\t%s : %s;\n" f.ef_name (s_type f.ef_type);
				) e.e_names;
				print "}"
			| Type.TTypeDecl t ->
				print "%stype %s%s = %s" (if t.t_private then "private " else "") (s_type_path path) (params t.t_params) (s_type t.t_type);
			| Type.TAbstractDecl a ->
				print "%sabstract %s%s {}" (if a.a_private then "private " else "") (s_type_path path) (params a.a_params);
			);
			close();
		) com.types

	let dump_record com =
		List.iter (fun mt ->
			let buf,close = create_dumpfile_from_path com (t_path mt) in
			let s = match mt with
				| TClassDecl c -> Printer.s_tclass c
				| TEnumDecl en -> Printer.s_tenum en
				| TTypeDecl t -> Printer.s_tdef "" t
				| TAbstractDecl a -> Printer.s_tabstract a
			in
			Buffer.add_string buf s;
			close();
		) com.types

	let dump_types com =
		match Common.defined_value_safe com Define.Dump with
			| "pretty" -> dump_types com (Type.s_expr_pretty false "\t")
			| "legacy" -> dump_types com Type.s_expr
			| "record" -> dump_record com
			| _ -> dump_types com (Type.s_expr_ast (not (Common.defined com Define.DumpIgnoreVarIds)) "\t")

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
end

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
	let vtmp = alloc_var vtmp e.etype e.epos in
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
		| TAbstract(a,_) when Meta.has Meta.CoreType a.a_meta ->
			t
		| TInst _ | TEnum _ ->
			t
		| TAbstract(a,tl) -> simplify_t (Abstract.get_underlying_type a tl)
		| TType(({ t_path = [],"Null" } as t), [t2]) -> (match simplify_t t2 with
			| (TAbstract(a,_) as t2) when Meta.has Meta.CoreType a.a_meta ->
				TType(t, [simplify_t t2])
			| (TEnum _ as t2) ->
				TType(t, [simplify_t t2])
			| t2 -> t2)
		| TType(t, tl) ->
			simplify_t (apply_params t.t_params tl t.t_type)
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

	(**
		The rate function returns an ( int * int ) type.
		The smaller the int, the best rated the caller argument is in comparison with the callee.

		The first int refers to how many "conversions" would be necessary to convert from the callee to the caller type, and
		the second refers to the type parameters.
	**)
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
						cur := (acc+1,sup,List.map (apply_params ca.cl_params tla) tls);
						stack := [!cur];
						loop())
				| (acc,ca,tla) :: _ when ca == cf ->
					acc,tla
				| (acc,ca,tla) :: s ->
					stack := s @ List.map (fun (c,tl) -> (acc+1,c,List.map (apply_params ca.cl_params tla) tl)) ca.cl_implements;
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
					loop (acc+1) sup (List.map (apply_params ca.cl_params tla) stl)
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
		| TAbstract(a, _), TDynamic _ when Meta.has Meta.CoreType a.a_meta ->
			(cacc + 2, 0) (* a dynamic to a basic type will have an "unboxing" penalty *)
		| _, TDynamic _ ->
			(cacc + 1, 0)
		| TAbstract(af,tlf), TAbstract(aa,tla) ->
			(if af == aa then
				(cacc, rate_tp tlf tla)
			else
				let ret = ref None in
				if List.exists (fun t -> try
					ret := Some (rate_conv (cacc+1) (apply_params af.a_params tlf t) targ);
					true
				with | Not_found ->
					false
				) af.a_from then
					Option.get !ret
			else
				if List.exists (fun t -> try
					ret := Some (rate_conv (cacc+1) tfun (apply_params aa.a_params tla t));
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
		| ( el, t, _ ) :: ret when List.exists (fun (_,t2,_) -> type_iseq t t2) acc ->
			rm_duplicates acc ret
		| r :: ret ->
			rm_duplicates (r :: acc) ret

	let s_options rated =
		String.concat ",\n" (List.map (fun ((elist,t,_),rate) ->
			"( " ^ (String.concat "," (List.map (fun(e,_) -> s_expr (s_type (print_context())) e) elist)) ^ " ) => " ^
			"( " ^ (String.concat "," (List.map (fun (i,i2) -> string_of_int i ^ ":" ^ string_of_int i2) rate)) ^ " ) => " ^ (s_type (print_context()) t)
		) rated)

	let count_optionals elist =
		List.fold_left (fun acc (_,is_optional) -> if is_optional then acc + 1 else acc) 0 elist

	let rec fewer_optionals acc compatible = match acc, compatible with
		| _, [] -> acc
		| [], c :: comp -> fewer_optionals [c] comp
		| (elist_acc, _, _) :: _, ((elist, _, _) as cur) :: comp ->
			let acc_opt = count_optionals elist_acc in
			let comp_opt = count_optionals elist in
			if acc_opt = comp_opt then
				fewer_optionals (cur :: acc) comp
			else if acc_opt < comp_opt then
				fewer_optionals acc comp
			else
				fewer_optionals [cur] comp

	let reduce_compatible compatible = match fewer_optionals [] (rm_duplicates [] compatible) with
		| [] -> []
		| [v] -> [v]
		| compatible ->
			(* convert compatible into ( rate * compatible_type ) list *)
			let rec mk_rate acc elist args = match elist, args with
				| [], [] -> acc
				| (_,true) :: elist, _ :: args -> mk_rate acc elist args
				| (e,false) :: elist, (n,o,t) :: args ->
					(* if the argument is an implicit cast, we need to start with a penalty *)
					(* The penalty should be higher than any other implicit cast - other than Dynamic *)
					(* since Dynamic has a penalty of max_int, we'll impose max_int - 1 to it *)
					(match e.eexpr with
						| TMeta( (Meta.ImplicitCast,_,_), _) ->
							mk_rate ((max_int - 1, 0) :: acc) elist args
						| _ ->
							mk_rate (rate_conv 0 t e.etype :: acc) elist args)
				| _ -> assert false
			in

			let rated = ref [] in
			List.iter (function
				| (elist,TFun(args,ret),d) -> (try
					rated := ( (elist,TFun(args,ret),d), mk_rate [] elist args ) :: !rated
					with | Not_found -> ())
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

			let r = loop [] !rated in
			List.map fst r
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
			List.map (fun e -> f e t_dynamic) el

	let rec run ff e =
		let f e t =
			if not (type_iseq e.etype t) then
				ff e t
			else
				e
		in
		let check e = match e.eexpr with
			| TBinop((OpAssign | OpAssignOp _),e1,e2) ->
				assert false; (* this trigger #4347, to be fixed before enabling
				let e2 = f e2 e1.etype in
				{e with eexpr = TBinop(op,e1,e2)} *)
			| TVar(v,Some ev) ->
				let eo = Some (f ev v.v_type) in
				{ e with eexpr = TVar(v,eo) }
			| TCall(e1,el) ->
				let el = check_call f el e1.etype in
				{e with eexpr = TCall(e1,el)}
			| TNew(c,tl,el) ->
				begin try
					let tcf,_ = get_constructor (fun cf -> apply_params c.cl_params tl cf.cf_type) c in
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
				check (Type.map_expr (run ff) e)
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
					| FStatic(c,cf) | FInstance(c,_,cf) ->
						check_class com c e.epos;
						check_cf com cf e.epos
					| FAnon cf ->
						check_cf com cf e.epos
					| FClosure(co,cf) ->
						(match co with None -> () | Some (c,_) -> check_class com c e.epos);
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

let interpolate_code com code tl f_string f_expr p =
	let exprs = Array.of_list tl in
	let i = ref 0 in
	let err msg =
		let pos = { p with pmin = p.pmin + !i } in
		com.error msg pos
	in
	let regex = Str.regexp "[{}]" in
	let rec loop m = match m with
		| [] ->
			()
		| Str.Text txt :: tl ->
			i := !i + String.length txt;
			f_string txt;
			loop tl
		| Str.Delim a :: Str.Delim b :: tl when a = b ->
			i := !i + 2;
			f_string a;
			loop tl
		| Str.Delim "{" :: Str.Text n :: Str.Delim "}" :: tl ->
			begin try
				let expr = Array.get exprs (int_of_string n) in
				f_expr expr;
			with
			| Failure "int_of_string" ->
				f_string ("{" ^ n ^ "}");
			| Invalid_argument _ ->
				err ("Out-of-bounds special parameter: " ^ n)
			end;
			i := !i + 2 + String.length n;
			loop tl
		| Str.Delim x :: tl ->
			f_string x;
			incr i;
			loop tl
	in
	loop (Str.full_split regex code)

let map_source_header com f =
	match Common.defined_value_safe com Define.SourceHeader with
	| "" -> ()
	| s -> f s


(* Static extensions for classes *)
module ExtClass = struct

	let add_cl_init c e = match c.cl_init with
			| None -> c.cl_init <- Some e
			| Some e' -> c.cl_init <- Some (concat e' e)

	let add_static_init c cf e p =
		let ethis = ExprBuilder.make_static_this c p in
		let ef1 = mk (TField(ethis,FStatic(c,cf))) cf.cf_type p in
		let e_assign = mk (TBinop(OpAssign,ef1,e)) e.etype p in
		add_cl_init c e_assign
end
