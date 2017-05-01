(*
	The Haxe Compiler
	Copyright (C) 2005-2017  Haxe Foundation

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
open OptimizerTexpr
open Error
open Globals

(* ---------------------------------------------------------------------- *)
(* API OPTIMIZATIONS *)

let mk_untyped_call name p params =
	{
		eexpr = TCall({ eexpr = TLocal(alloc_unbound_var name t_dynamic p); etype = t_dynamic; epos = p }, params);
		etype = t_dynamic;
		epos = p;
	}

let api_inline2 com c field params p =
	match c.cl_path, field, params with
	| ([],"Type"),"enumIndex",[{ eexpr = TField (_,FEnum (en,f)) }] -> (match com.platform with
		| Cs when en.e_extern && not (Meta.has Meta.HxGen en.e_meta) ->
			(* We don't want to optimize enums from external sources; as they might change unexpectedly *)
			(* and since native C# enums don't have the concept of index - they have rather a value, *)
			(* which can't be mapped to a native API - this kind of substitution is dangerous *)
			None
		| _ ->
			Some (mk (TConst (TInt (Int32.of_int f.ef_index))) com.basic.tint p))
	| ([],"Type"),"enumIndex",[{ eexpr = TCall({ eexpr = TField (_,FEnum (en,f)) },pl) }] when List.for_all (fun e -> not (has_side_effect e)) pl ->
		(match com.platform with
			| Cs when en.e_extern && not (Meta.has Meta.HxGen en.e_meta) ->
				(* see comment above *)
				None
			| _ ->
				Some (mk (TConst (TInt (Int32.of_int f.ef_index))) com.basic.tint p))
	| ([],"Std"),"int",[{ eexpr = TConst (TInt _) } as e] ->
		Some { e with epos = p }
	| ([],"String"),"fromCharCode",[{ eexpr = TConst (TInt i) }] when i > 0l && i < 128l ->
		Some (mk (TConst (TString (String.make 1 (char_of_int (Int32.to_int i))))) com.basic.tstring p)
	| ([],"Std"),"string",[{ eexpr = TCast ({ eexpr = TConst c } as e, None)}]
	| ([],"Std"),"string",[{ eexpr = TConst c } as e] ->
		(match c with
		| TString s ->
			Some { e with epos = p }
		| TInt i ->
			Some { eexpr = TConst (TString (Int32.to_string i)); epos = p; etype = com.basic.tstring }
		| TBool b ->
			Some { eexpr = TConst (TString (if b then "true" else "false")); epos = p; etype = com.basic.tstring }
		| _ ->
			None)
	| ([],"Std"),"string",[{ eexpr = TIf (_,{ eexpr = TConst (TString _)},Some { eexpr = TConst (TString _) }) } as e] ->
		Some e
	| ([],"Std"),"string",[{ eexpr = TLocal v | TField({ eexpr = TLocal v },_) } as ev] when (com.platform = Js || com.platform = Flash) && not (Meta.has Meta.CompilerGenerated v.v_meta) ->
		let pos = ev.epos in
		let stringv() =
			let to_str = mk (TBinop (Ast.OpAdd, mk (TConst (TString "")) com.basic.tstring pos, ev)) com.basic.tstring pos in
			if com.platform = Js || is_nullable ev.etype then
				let chk_null = mk (TBinop (Ast.OpEq, ev, mk (TConst TNull) t_dynamic pos)) com.basic.tbool pos in
				mk (TIf (chk_null, mk (TConst (TString "null")) com.basic.tstring pos, Some to_str)) com.basic.tstring pos
			else
				to_str
		in
		(match follow ev.etype with
		| TInst ({ cl_path = [],"String" }, []) ->
			Some (stringv())
		| TAbstract ({ a_path = [],"Float" }, []) ->
			Some (stringv())
		| TAbstract ({ a_path = [],"Int" }, []) ->
			Some (stringv())
		| TAbstract ({ a_path = [],"UInt" }, []) ->
			Some (stringv())
		| TAbstract ({ a_path = [],"Bool" }, []) ->
			Some (stringv())
		| _ ->
			None)
	| ([],"Std"),"int",[{ eexpr = TConst (TFloat f) }] ->
		let f = float_of_string f in
		(match classify_float f with
		| FP_infinite | FP_nan ->
			None
		| _ when f <= Int32.to_float Int32.min_int -. 1. || f >= Int32.to_float Int32.max_int +. 1. ->
			None (* out range, keep platform-specific behavior *)
		| _ ->
			Some { eexpr = TConst (TInt (Int32.of_float f)); etype = com.basic.tint; epos = p })
	| ([],"Math"),"ceil",[{ eexpr = TConst (TFloat f) }] ->
		let f = float_of_string f in
		(match classify_float f with
		| FP_infinite | FP_nan ->
			None
		| _ when f <= Int32.to_float Int32.min_int -. 1. || f >= Int32.to_float Int32.max_int ->
			None (* out range, keep platform-specific behavior *)
		| _ ->
			Some { eexpr = TConst (TInt (Int32.of_float (ceil f))); etype = com.basic.tint; epos = p })
	| ([],"Math"),"floor",[{ eexpr = TConst (TFloat f) }] ->
		let f = float_of_string f in
		(match classify_float f with
		| FP_infinite | FP_nan ->
			None
		| _ when f <= Int32.to_float Int32.min_int || f >= Int32.to_float Int32.max_int +. 1. ->
			None (* out range, keep platform-specific behavior *)
		| _ ->
			Some { eexpr = TConst (TInt (Int32.of_float (floor f))); etype = com.basic.tint; epos = p })
	| (["cs"],"Lib"),("fixed" | "checked" | "unsafe"),[e] ->
			Some (mk_untyped_call ("__" ^ field ^ "__") p [e])
	| (["cs"],"Lib"),("lock"),[obj;block] ->
			Some (mk_untyped_call ("__lock__") p [obj;mk_block block])
	| (["java"],"Lib"),("lock"),[obj;block] ->
			Some (mk_untyped_call ("__lock__") p [obj;mk_block block])
	| _ ->
		None

let api_inline ctx c field params p = match c.cl_path, field, params with
	| ([],"Std"),"is",[o;t] | (["js"],"Boot"),"__instanceof",[o;t] when ctx.com.platform = Js ->
		let mk_local ctx n t pos =
			mk (TLocal (try
				PMap.find n ctx.locals
			with _ ->
				let v = add_local ctx n t p in
				v.v_meta <- [Meta.Unbound,[],p];
				v
			)) t pos in

		let tstring = ctx.com.basic.tstring in
		let tbool = ctx.com.basic.tbool in
		let tint = ctx.com.basic.tint in

		let is_trivial e =
			match e.eexpr with
			| TConst _ | TLocal _ -> true
			| _ -> false
		in

		let typeof t =
			let tof = mk_local ctx "__typeof__" (tfun [o.etype] tstring) p in
			let tof = mk (TCall (tof, [o])) tstring p in
			mk (TBinop (Ast.OpEq, tof, (mk (TConst (TString t)) tstring p))) tbool p
		in

		(match t.eexpr with
		(* generate simple typeof checks for basic types *)
		| TTypeExpr (TClassDecl ({ cl_path = [],"String" })) -> Some (typeof "string")
		| TTypeExpr (TAbstractDecl ({ a_path = [],"Bool" })) -> Some (typeof "boolean")
		| TTypeExpr (TAbstractDecl ({ a_path = [],"Float" })) -> Some (typeof "number")
		| TTypeExpr (TAbstractDecl ({ a_path = [],"Int" })) when is_trivial o ->
			(* generate typeof(o) == "number" && (o|0) === o check *)
			let teq = mk_local ctx "__strict_eq__" (tfun [tint; tint] tbool) p in
			let lhs = mk (TBinop (Ast.OpOr, o, mk (TConst (TInt Int32.zero)) tint p)) tint p in
			let jscheck = mk (TCall (teq, [lhs; o])) tbool p in
			Some(mk (TBinop (Ast.OpBoolAnd, typeof "number", jscheck)) tbool p)
		| TTypeExpr (TClassDecl ({ cl_path = [],"Array" })) ->
			(* generate (o instanceof Array) && o.__enum__ == null check *)
			let iof = mk_local ctx "__instanceof__" (tfun [o.etype;t.etype] tbool) p in
			let iof = mk (TCall (iof, [o; t])) tbool p in
			let enum = mk (TField (o, FDynamic "__enum__")) (mk_mono()) p in
			let null = mk (TConst TNull) (mk_mono()) p in
			let not_enum = mk (TBinop (Ast.OpEq, enum, null)) tbool p in
			Some (mk (TBinop (Ast.OpBoolAnd, iof, not_enum)) tbool p)
		| _ ->
			None)
	| (["cs" | "java"],"Lib"),("nativeArray"),[{ eexpr = TArrayDecl args } as edecl; _]
	| (["haxe";"ds";"_Vector"],"Vector_Impl_"),("fromArrayCopy"),[{ eexpr = TArrayDecl args } as edecl] -> (try
			let platf = match ctx.com.platform with
				| Cs -> "cs"
				| Java -> "java"
				| _ -> raise Exit
			in
			let mpath = if field = "fromArrayCopy" then
				(["haxe";"ds"],"Vector")
			else
				([platf],"NativeArray")
			in

			let m = ctx.g.do_load_module ctx mpath null_pos in
			let main = List.find (function | TClassDecl _ | TAbstractDecl _ -> true | _ -> false) m.m_types in
			let t = match follow edecl.etype, main with
				| TInst({ cl_path = [],"Array" }, [t]), TClassDecl(cl) ->
					TInst(cl,[t])
				| TInst({ cl_path = [],"Array" }, [t]), TAbstractDecl(a) ->
					TAbstract(a,[t])
				| _ -> assert false
			in
			Some ({ (mk_untyped_call "__array__" p args) with etype = t })
		with | Exit ->
			None)
	| _ ->
		api_inline2 ctx.com c field params p

(* ---------------------------------------------------------------------- *)
(* INLINING *)

type in_local = {
	i_var : tvar;
	i_subst : tvar;
	i_outside : bool;
	i_abstract_this : bool;
	mutable i_captured : bool;
	mutable i_write : bool;
	mutable i_read : int;
	mutable i_force_temp : bool;
}

let inline_default_config cf t =
	(* type substitution on both class and function type parameters *)
	let rec get_params c pl =
		match c.cl_super with
		| None -> c.cl_params, pl
		| Some (csup,spl) ->
			let spl = (match apply_params c.cl_params pl (TInst (csup,spl)) with
			| TInst (_,pl) -> pl
			| _ -> assert false
			) in
			let ct, cpl = get_params csup spl in
			c.cl_params @ ct, pl @ cpl
	in
	let tparams = (match follow t with
		| TInst (c,pl) -> get_params c pl
		| _ -> ([],[]))
	in
	let pmonos = List.map (fun _ -> mk_mono()) cf.cf_params in
	let tmonos = snd tparams @ pmonos in
	let tparams = fst tparams @ cf.cf_params in
	tparams <> [], apply_params tparams tmonos

let rec type_inline ctx cf f ethis params tret config p ?(self_calling_closure=false) force =
	(* perform some specific optimization before we inline the call since it's not possible to detect at final optimization time *)
	try
		let cl = (match follow ethis.etype with
			| TInst (c,_) -> c
			| TAnon a -> (match !(a.a_status) with Statics c -> c | _ -> raise Exit)
			| _ -> raise Exit
		) in
		(match api_inline ctx cl cf.cf_name params p with
		| None -> raise Exit
		| Some e -> Some e)
	with Exit ->
	let has_params,map_type = match config with Some config -> config | None -> inline_default_config cf ethis.etype in
	(* locals substitution *)
	let locals = Hashtbl.create 0 in
	let local v =
		try
			Hashtbl.find locals v.v_id
		with Not_found ->
			let v' = alloc_var v.v_name v.v_type v.v_pos in
			if Meta.has Meta.Unbound v.v_meta then v'.v_meta <- [Meta.Unbound,[],p];
			let i = {
				i_var = v;
				i_subst = v';
				i_outside = false;
				i_abstract_this = Meta.has Meta.This v.v_meta;
				i_captured = false;
				i_write = false;
				i_force_temp = false;
				i_read = 0;
			} in
			i.i_subst.v_meta <- List.filter (fun (m,_,_) -> m <> Meta.This) v.v_meta;
			Hashtbl.add locals v.v_id i;
			Hashtbl.add locals i.i_subst.v_id i;
			i
	in
	let in_local_fun = ref false in
	let read_local v =
		let l = try
			Hashtbl.find locals v.v_id
		with Not_found ->
			(* make sure to duplicate unbound inline variable to prevent dependency leak when unifying monomorph *)
			if has_meta Meta.Unbound v.v_meta then local v else
			{
				i_var = v;
				i_subst = v;
				i_outside = true;
				i_abstract_this = Meta.has Meta.This v.v_meta;
				i_captured = false;
				i_write = false;
				i_force_temp = false;
				i_read = 0;
			}
		in
		if !in_local_fun then l.i_captured <- true;
		l
	in
	(* use default values for null/unset arguments *)
	let rec loop pl al first =
		match pl, al with
		| _, [] -> []
		| e :: pl, (v, opt) :: al ->
			(*
				if we pass a Null<T> var to an inlined method that needs a T.
				we need to force a local var to be created on some platforms.
			*)
			if ctx.com.config.pf_static && not (is_nullable v.v_type) && is_null e.etype then (local v).i_force_temp <- true;
			(*
				if we cast from Dynamic, create a local var as well to do the cast
				once and allow DCE to perform properly.
			*)
			let e = if follow v.v_type != t_dynamic && follow e.etype == t_dynamic then mk (TCast(e,None)) v.v_type e.epos else e in
			(match e.eexpr, opt with
			| TConst TNull , Some c -> mk (TConst c) v.v_type e.epos
			(*
				This is really weird and should be reviewed again. The problem is that we cannot insert a TCast here because
				the abstract `this` value could be written to, which is not possible if it is wrapped in a cast.

				The original problem here is that we do not generate a temporary variable and thus mute the type of the
				`this` variable, which leads to unification errors down the line. See issues #2236 and #3713.
			*)
			(* | _ when first && (Meta.has Meta.Impl cf.cf_meta) -> {e with etype = v.v_type} *)
			| _ -> e) :: loop pl al false
		| [], (v,opt) :: al ->
			(mk (TConst (match opt with None -> TNull | Some c -> c)) v.v_type p) :: loop [] al false
	in
	(*
		Build the expr/var subst list
	*)
	let ethis = (match ethis.eexpr with TConst TSuper -> { ethis with eexpr = TConst TThis } | _ -> ethis) in
	let vthis = alloc_var "_this" ethis.etype ethis.epos in
	let might_be_affected,collect_modified_locals = create_affection_checker() in
	let had_side_effect = ref false in
	let inlined_vars = List.map2 (fun e (v,_) ->
		let l = local v in
		if has_side_effect e then begin
			collect_modified_locals e;
			had_side_effect := true;
			l.i_force_temp <- true;
		end;
		if l.i_abstract_this then l.i_subst.v_extra <- Some ([],Some e);
		l, e
	) (ethis :: loop params f.tf_args true) ((vthis,None) :: f.tf_args) in
	List.iter (fun (l,e) ->
		if might_be_affected e then l.i_force_temp <- true;
	) inlined_vars;
	let inlined_vars = List.rev inlined_vars in
	(*
		here, we try to eliminate final returns from the expression tree.
		However, this is not entirely correct since we don't yet correctly propagate
		the type of returned expressions upwards ("return" expr itself being Dynamic).

		We also substitute variables with fresh ones that might be renamed at later stage.
	*)
	let opt f = function
		| None -> None
		| Some e -> Some (f e)
	in
	let has_vars = ref false in
	let in_loop = ref false in
	let cancel_inlining = ref false in
	let has_return_value = ref false in
	let ret_val = (match follow f.tf_type with TAbstract ({ a_path = ([],"Void") },[]) -> false | _ -> true) in
	let map_pos = if self_calling_closure then (fun e -> e) else (fun e -> { e with epos = p }) in
	let rec map term e =
		let po = e.epos in
		let e = map_pos e in
		match e.eexpr with
		| TLocal v ->
			let l = read_local v in
			l.i_read <- l.i_read + (if !in_loop then 2 else 1);
			(* never inline a function which contain a delayed macro because its bound
				to its variables and not the calling method *)
			if v.v_name = "$__delayed_call__" then cancel_inlining := true;
			let e = { e with eexpr = TLocal l.i_subst } in
			if l.i_abstract_this then mk (TCast(e,None)) v.v_type e.epos else e
		| TConst TThis ->
			let l = read_local vthis in
			l.i_read <- l.i_read + (if !in_loop then 2 else 1);
			{ e with eexpr = TLocal l.i_subst }
		| TVar (v,eo) ->
			has_vars := true;
			{ e with eexpr = TVar ((local v).i_subst,opt (map false) eo)}
		| TReturn eo when not !in_local_fun ->
			if not term then error "Cannot inline a not final return" po;
			(match eo with
			| None -> mk (TConst TNull) f.tf_type p
			| Some e ->
				has_return_value := true;
				map term e)
		| TFor (v,e1,e2) ->
			let i = local v in
			let e1 = map false e1 in
			let old = !in_loop in
			in_loop := true;
			let e2 = map false e2 in
			in_loop := old;
			{ e with eexpr = TFor (i.i_subst,e1,e2) }
		| TWhile (cond,eloop,flag) ->
			let cond = map false cond in
			let old = !in_loop in
			in_loop := true;
			let eloop = map false eloop in
			in_loop := old;
			{ e with eexpr = TWhile (cond,eloop,flag) }
		| TSwitch (e1,cases,def) when term ->
			let term = term && (def <> None || is_exhaustive e1) in
			let cases = List.map (fun (el,e) ->
				let el = List.map (map false) el in
				el, map term e
			) cases in
			let def = opt (map term) def in
			{ e with eexpr = TSwitch (map false e1,cases,def); etype = if ret_val then unify_min ctx ((List.map snd cases) @ (match def with None -> [] | Some e -> [e])) else e.etype }
		| TTry (e1,catches) ->
			{ e with eexpr = TTry (map term e1,List.map (fun (v,e) ->
				let lv = (local v).i_subst in
				let e = map term e in
				lv,e
			) catches); etype = if term && ret_val then unify_min ctx (e1::List.map snd catches) else e.etype }
		| TBlock l ->
			let old = save_locals ctx in
			let t = ref e.etype in
			let rec has_term_return e =
				let rec loop e =
					let r = match e.eexpr with
					| TReturn _ -> true
					| TFunction _ -> false
					| TIf (_,_,None) | TSwitch (_,_,None) | TFor _ | TWhile (_,_,NormalWhile) -> false (* we might not enter this code at all *)
					| TTry (a, catches) -> List.for_all has_term_return (a :: List.map snd catches)
					| TIf (cond,a,Some b) -> has_term_return cond || (has_term_return a && has_term_return b)
					| TSwitch (cond,cases,Some def) -> has_term_return cond || List.for_all has_term_return (def :: List.map snd cases)
					| TBinop (OpBoolAnd,a,b) -> has_term_return a && has_term_return b
					| _ -> Type.iter loop e; false
					in
					if r then raise Exit
				in
				try loop e; false with Exit -> true
			in
			let rec loop = function
				| [] when term ->
					t := mk_mono();
					[mk (TConst TNull) (!t) p]
				| [] -> []
				| [e] ->
					let e = map term e in
					if term then t := e.etype;
					[e]
				| ({ eexpr = TIf (cond,e1,None) } as e) :: l when term && has_term_return e1 ->
					loop [{ e with eexpr = TIf (cond,e1,Some (mk (TBlock l) e.etype e.epos)); epos = punion e.epos (match List.rev l with e :: _ -> e.epos | [] -> assert false) }]
				| e :: l ->
					let e = map false e in
					e :: loop l
			in
			let l = loop l in
			old();
			{ e with eexpr = TBlock l; etype = !t }
		| TIf (econd,eif,Some eelse) when term ->
			let econd = map false econd in
			let eif = map term eif in
			let eelse = map term eelse in
			{ e with eexpr = TIf(econd,eif,Some eelse); etype = if ret_val then unify_min ctx [eif;eelse] else e.etype }
		| TParenthesis e1 ->
			let e1 = map term e1 in
			mk (TParenthesis e1) e1.etype e.epos
		| TUnop ((Increment|Decrement) as op,flag,({ eexpr = TLocal v } as e1)) ->
			let l = read_local v in
			l.i_write <- true;
			{e with eexpr = TUnop(op,flag,{e1 with eexpr = TLocal l.i_subst})}
		| TBinop ((OpAssign | OpAssignOp _) as op,({ eexpr = TLocal v } as e1),e2) ->
			let l = read_local v in
			l.i_write <- true;
			let e2 = map false e2 in
			{e with eexpr = TBinop(op,{e1 with eexpr = TLocal l.i_subst},e2)}
		| TObjectDecl fl ->
			let fl = List.map (fun (s,e) -> s,map false e) fl in
			begin match follow e.etype with
				| TAnon an when (match !(an.a_status) with Const -> true | _ -> false) ->
					{e with eexpr = TObjectDecl fl; etype = TAnon { an with a_status = ref Closed}}
				| _ ->
					{e with eexpr = TObjectDecl fl}
			end
		| TFunction f ->
			(match f.tf_args with [] -> () | _ -> has_vars := true);
			let old = save_locals ctx and old_fun = !in_local_fun in
			let args = List.map (function(v,c) -> (local v).i_subst, c) f.tf_args in
			in_local_fun := true;
			let expr = map false f.tf_expr in
			in_local_fun := old_fun;
			old();
			{ e with eexpr = TFunction { tf_args = args; tf_expr = expr; tf_type = f.tf_type } }
		| TCall({eexpr = TConst TSuper; etype = t},el) ->
			begin match follow t with
			| TInst({ cl_constructor = Some ({cf_kind = Method MethInline; cf_expr = Some ({eexpr = TFunction tf})} as cf)} as c,_) ->
				begin match type_inline_ctor ctx c cf tf ethis el po with
				| Some e -> map term e
				| None -> error "Could not inline super constructor call" po
				end
			| _ -> error "Cannot inline function containing super" po
			end
		| TConst TSuper ->
			error "Cannot inline function containing super" po
		| TMeta(m,e1) ->
			let e1 = map term e1 in
			{e with eexpr = TMeta(m,e1)}
		| _ ->
			Type.map_expr (map false) e
	in
	let e = map true f.tf_expr in
	(*
		if variables are not written and used with a const value, let's substitute
		with the actual value, either create a temp var
	*)
	let subst = ref PMap.empty in
	let is_constant e =
		let rec loop e =
			match e.eexpr with
			| TLocal _
			| TConst TThis (* not really, but should not be move inside a function body *)
				-> raise Exit
			| TField (_,FEnum _)
			| TTypeExpr _
			| TConst _ -> ()
			| _ ->
				Type.iter loop e
		in
		try loop e; true with Exit -> false
	in
	let is_writable e =
		match e.eexpr with
		| TField _ | TEnumParameter _ | TLocal _ | TArray _ -> true
		| _  -> false
	in
	let force = ref force in
	let vars = List.fold_left (fun acc (i,e) ->
		let flag = not i.i_force_temp && (match e.eexpr with
			| TLocal _ when i.i_abstract_this -> true
			| TLocal _ | TConst _ -> not i.i_write
			| TFunction _ -> if i.i_write then error "Cannot modify a closure parameter inside inline method" p; true
			| _ -> not i.i_write && i.i_read <= 1
		) in
		let flag = flag && (not i.i_captured || is_constant e) in
		(* force inlining if we modify 'this' *)
		if i.i_write && i.i_abstract_this then force := true;
		(* force inlining of 'this' variable if it is written *)
		let flag = if not flag && i.i_abstract_this && i.i_write then begin
			if not (is_writable e) then error "Cannot modify the abstract value, store it into a local first" p;
			true
		end else flag in
		if flag then begin
			subst := PMap.add i.i_subst.v_id e !subst;
			acc
		end else begin
			(* mark the replacement local for the analyzer *)
			if i.i_read <= 1 && not i.i_write then
				i.i_subst.v_meta <- (Meta.CompilerGenerated,[],p) :: i.i_subst.v_meta;
			(i.i_subst,Some e) :: acc
		end
	) [] inlined_vars in
	let subst = !subst in
	let rec inline_params e =
		match e.eexpr with
		| TLocal v -> (try PMap.find v.v_id subst with Not_found -> e)
		| _ -> Type.map_expr inline_params e
	in
	let e = (if PMap.is_empty subst then e else inline_params e) in
	let init = match vars with [] -> None | l -> Some l in
	(*
		If we have local variables and returning a value, then this will result in
		unoptimized JS code, so let's instead skip inlining.

		This could be fixed with better post process code cleanup (planed)
	*)
	if !cancel_inlining then
		None
	else
		let wrap e =
			(* we can't mute the type of the expression because it is not correct to do so *)
			let etype = if has_params then map_type e.etype else e.etype in
			(* if the expression is "untyped" and we don't want to unify it accidentally ! *)
			try (match follow e.etype with
			| TMono _ | TInst ({cl_kind = KTypeParameter _ },_) ->
				(match follow tret with
				| TAbstract ({ a_path = [],"Void" },_) -> e
				| _ -> raise (Unify_error []))
			| _ ->
				type_eq (if ctx.com.config.pf_static then EqDoNotFollowNull else EqStrict) etype tret;
				e)
			with Unify_error _ ->
				mk (TCast (e,None)) tret e.epos
		in
		let e = (match e.eexpr, init with
			| _, None when not !has_return_value ->
				begin match e.eexpr with
					| TBlock _ -> {e with etype = tret}
					| _ -> mk (TBlock [e]) tret e.epos
				end
			| TBlock [e] , None -> wrap e
			| _ , None -> wrap e
			| TBlock l, Some vl ->
				let el_v = List.map (fun (v,eo) -> mk (TVar (v,eo)) ctx.t.tvoid e.epos) vl in
				mk (TBlock (el_v @ l)) tret e.epos
			| _, Some vl ->
				let el_v = List.map (fun (v,eo) -> mk (TVar (v,eo)) ctx.t.tvoid e.epos) vl in
				mk (TBlock (el_v @ [e])) tret e.epos
		) in
		let inline_meta e meta = match meta with
			| (Meta.Deprecated | Meta.Pure),_,_ -> mk (TMeta(meta,e)) e.etype e.epos
			| _ -> e
		in
		let e = List.fold_left inline_meta e cf.cf_meta in
		let e = Display.Diagnostics.secure_generated_code ctx e in
		if Meta.has (Meta.Custom ":inlineDebug") ctx.meta then begin
			let se t = s_expr_pretty true t true (s_type (print_context())) in
			print_endline (Printf.sprintf "Inline %s:\n\tArgs: %s\n\tExpr: %s\n\tResult: %s"
				cf.cf_name
				(String.concat "" (List.map (fun (i,e) -> Printf.sprintf "\n\t\t%s<%i> = %s" (i.i_subst.v_name) (i.i_subst.v_id) (se "\t\t" e)) inlined_vars))
				(se "\t" f.tf_expr)
				(se "\t" e)
			);
		end;
		(* we need to replace type-parameters that were used in the expression *)
		if not has_params then
			Some e
		else
			let mt = map_type cf.cf_type in
			let unify_func () = unify_raise ctx mt (TFun (List.map (fun e -> "",false,e.etype) params,tret)) p in
			(match follow ethis.etype with
			| TAnon a -> (match !(a.a_status) with
				| Statics {cl_kind = KAbstractImpl a } when Meta.has Meta.Impl cf.cf_meta ->
					if cf.cf_name <> "_new" then begin
						(* the first argument must unify with a_this for abstract implementation functions *)
						let tb = (TFun(("",false,map_type a.a_this) :: List.map (fun e -> "",false,e.etype) (List.tl params),tret)) in
						unify_raise ctx mt tb p
					end
				| _ -> unify_func())
			| _ -> unify_func());
			(*
				this is very expensive since we are building the substitution list for
				every expression, but hopefully in such cases the expression size is small
			*)
			let vars = Hashtbl.create 0 in
			let map_var v =
				if not (Hashtbl.mem vars v.v_id) then begin
					Hashtbl.add vars v.v_id ();
					if not (read_local v).i_outside then v.v_type <- map_type v.v_type;
				end;
				v
			in
			let rec map_expr_type e = Type.map_expr_type map_expr_type map_type map_var e in
			Some (map_expr_type e)

(* Same as type_inline, but modifies the function body to add field inits *)
and type_inline_ctor ctx c cf tf ethis el po =
	let field_inits =
		let cparams = List.map snd c.cl_params in
		let ethis = mk (TConst TThis) (TInst (c,cparams)) c.cl_pos in
		let el = List.fold_left (fun acc cf ->
			match cf.cf_kind,cf.cf_expr with
			| Var _,Some e ->
				let lhs = mk (TField(ethis,FInstance (c,cparams,cf))) cf.cf_type e.epos in
				let eassign = mk (TBinop(OpAssign,lhs,e)) cf.cf_type e.epos in
				eassign :: acc
			| _ -> acc
		) [] c.cl_ordered_fields in
		List.rev el
	in
	let tf =
		if field_inits = [] then tf
		else
			let bl = match tf.tf_expr with {eexpr = TBlock b } -> b | x -> [x] in
			{tf with tf_expr = mk (TBlock (field_inits @ bl)) ctx.t.tvoid c.cl_pos}
	in
	type_inline ctx cf tf ethis el ctx.t.tvoid None po true


(* ---------------------------------------------------------------------- *)
(* LOOPS *)

let rec optimize_for_loop ctx (i,pi) e1 e2 p =
	let t_void = ctx.t.tvoid in
	let t_int = ctx.t.tint in
	let lblock el = Some (mk (TBlock el) t_void p) in
	let mk_field e n =
		TField (e,try quick_field e.etype n with Not_found -> assert false)
	in
	let gen_int_iter pt f_get f_length =
		let i = add_local ctx i pt pi in
		let index = gen_local ctx t_int pi in
		index.v_meta <- (Meta.ForLoopVariable,[],null_pos) :: index.v_meta;
		let arr, avars = (match e1.eexpr with
			| TLocal _ -> e1, None
			| _ ->
				let atmp = gen_local ctx e1.etype e1.epos in
				mk (TLocal atmp) e1.etype e1.epos, (Some (atmp,Some e1))
		) in
		let iexpr = mk (TLocal index) t_int p in
		let e2 = type_expr ctx e2 NoValue in
		let aget = mk (TVar (i,Some (f_get arr iexpr pt p))) t_void pi in
		let incr = mk (TUnop (Increment,Prefix,iexpr)) t_int p in
		let block = match e2.eexpr with
			| TBlock el -> mk (TBlock (aget :: incr :: el)) t_void e2.epos
			| _ -> mk (TBlock [aget;incr;e2]) t_void p
		in
		let ivar = Some (mk (TConst (TInt 0l)) t_int p) in
		let elength = f_length arr p in
		let el = [mk (TWhile (
				mk (TBinop (OpLt, iexpr, elength)) ctx.t.tbool p,
				block,
				NormalWhile
			)) t_void p;
		] in
		let el = match avars with None -> el | Some (v,eo) -> (mk (TVar (v,eo)) t_void p) :: el in
		let el = (mk (TVar (index,ivar)) t_void p) :: el in
		lblock el
	in
	let get_next_array_element arr iexpr pt p =
		(mk (TArray (arr,iexpr)) pt p)
	in
	let get_array_length arr p =
		mk (mk_field arr "length") ctx.com.basic.tint p
	in
	match e1.eexpr, follow e1.etype with
	| TNew ({ cl_path = ([],"IntIterator") },[],[i1;i2]) , _ ->
		let max = (match i1.eexpr , i2.eexpr with
			| TConst (TInt a), TConst (TInt b) when Int32.compare b a < 0 -> error "Range operator can't iterate backwards" p
			| _, TConst _ -> None
			| _ -> Some (gen_local ctx t_int e1.epos)
		) in
		let tmp = gen_local ctx t_int pi in
		tmp.v_meta <- (Meta.ForLoopVariable,[],null_pos) :: tmp.v_meta;
		let i = add_local ctx i t_int pi in
		let rec check e =
			match e.eexpr with
			| TBinop (OpAssign,{ eexpr = TLocal l },_)
			| TBinop (OpAssignOp _,{ eexpr = TLocal l },_)
			| TUnop (Increment,_,{ eexpr = TLocal l })
			| TUnop (Decrement,_,{ eexpr = TLocal l })  when l == i ->
				error "Loop variable cannot be modified" e.epos
			| _ ->
				Type.iter check e
		in
		let e2 = type_expr ctx e2 NoValue in
		check e2;
		let etmp = mk (TLocal tmp) t_int p in
		let incr = mk (TUnop (Increment,Postfix,etmp)) t_int p in
		let init = mk (TVar (i,Some incr)) t_void pi in
		let block = match e2.eexpr with
			| TBlock el -> mk (TBlock (init :: el)) t_void e2.epos
			| _ -> mk (TBlock [init;e2]) t_void p
		in
		(*
			force locals to be of Int type (to prevent Int/UInt issues)
		*)
		let i2 = match follow i2.etype with
			| TAbstract ({ a_path = ([],"Int") }, []) -> i2
			| _ -> { i2 with eexpr = TCast(i2, None); etype = t_int }
		in
		(match max with
		| None ->
			lblock [
				mk (TVar (tmp,Some i1)) t_void p;
				mk (TWhile (
					mk (TBinop (OpLt, etmp, i2)) ctx.t.tbool p,
					block,
					NormalWhile
				)) t_void p;
			]
		| Some max ->
			lblock [
				mk (TVar (tmp,Some i1)) t_void p;
				mk (TVar (max,Some i2)) t_void p;
				mk (TWhile (
					mk (TBinop (OpLt, etmp, mk (TLocal max) t_int p)) ctx.t.tbool p,
					block,
					NormalWhile
				)) t_void p;
			])
	| TArrayDecl el, TInst({ cl_path = [],"Array" },[pt]) when false ->
		begin try
			let num_expr = ref 0 in
			let rec loop e = match fst e with
				| EContinue | EBreak ->
					raise Exit
				| _ ->
					incr num_expr;
					Ast.map_expr loop e
			in
			ignore(loop e2);
			let v = add_local ctx i pt p in
			let e2 = type_expr ctx e2 NoValue in
			let cost = (List.length el) * !num_expr in
			let max_cost = try
				int_of_string (Common.defined_value ctx.com Define.LoopUnrollMaxCost)
			with Not_found ->
				250
			in
			if cost > max_cost then raise Exit;
			let eloc = mk (TLocal v) v.v_type p in
			let el = List.map (fun e ->
				let e_assign = mk (TBinop(OpAssign,eloc,e)) e.etype e.epos in
				concat e_assign e2
			) el in
			let ev = mk (TVar(v, None)) ctx.t.tvoid p in
			Some (mk (TBlock (ev :: el)) ctx.t.tvoid p)
		with Exit ->
			gen_int_iter pt get_next_array_element get_array_length
		end
	| _ , TInst({ cl_path = [],"Array" },[pt])
	| _ , TInst({ cl_path = ["flash"],"Vector" },[pt]) ->
		gen_int_iter pt get_next_array_element get_array_length
	| _ , TInst({ cl_array_access = Some pt } as c,pl) when (try match follow (PMap.find "length" c.cl_fields).cf_type with TAbstract ({ a_path = [],"Int" },[]) -> true | _ -> false with Not_found -> false) && not (PMap.mem "iterator" c.cl_fields) ->
		gen_int_iter (apply_params c.cl_params pl pt) get_next_array_element get_array_length
	| _, TAbstract({a_impl = Some c} as a,tl) ->
		begin try
			let cf_length = PMap.find "get_length" c.cl_statics in
			let get_length e p =
				make_static_call ctx c cf_length (apply_params a.a_params tl) [e] ctx.com.basic.tint p
			in
			begin match follow cf_length.cf_type with
				| TFun(_,tr) ->
					begin match follow tr with
						| TAbstract({a_path = [],"Int"},_) -> ()
						| _ -> raise Not_found
					end
				| _ ->
					raise Not_found
			end;
			begin try
				(* first try: do we have an @:arrayAccess getter field? *)
				let todo = mk (TConst TNull) ctx.t.tint p in
				let cf,_,r,_,_ = (!find_array_access_raise_ref) ctx a tl todo None p in
				let get_next e_base e_index t p =
					make_static_call ctx c cf (apply_params a.a_params tl) [e_base;e_index] r p
				in
				gen_int_iter r get_next get_length
			with Not_found ->
				(* second try: do we have @:arrayAccess on the abstract itself? *)
				if not (Meta.has Meta.ArrayAccess a.a_meta) then raise Not_found;
				(* let's allow this only for core-type abstracts *)
				if not (Meta.has Meta.CoreType a.a_meta) then raise Not_found;
				(* in which case we assume that a singular type parameter is the element type *)
				let t = match tl with [t] -> t | _ -> raise Not_found in
				gen_int_iter t get_next_array_element get_length
		end with Not_found ->
			None
		end
	| _ , TInst ({ cl_kind = KGenericInstance ({ cl_path = ["haxe";"ds"],"GenericStack" },[t]) } as c,[]) ->
		let tcell = (try (PMap.find "head" c.cl_fields).cf_type with Not_found -> assert false) in
		let i = add_local ctx i t p in
		let cell = gen_local ctx tcell p in
		let cexpr = mk (TLocal cell) tcell p in
		let e2 = type_expr ctx e2 NoValue in
		let evar = mk (TVar (i,Some (mk (mk_field cexpr "elt") t p))) t_void pi in
		let enext = mk (TBinop (OpAssign,cexpr,mk (mk_field cexpr "next") tcell p)) tcell p in
		let block = match e2.eexpr with
			| TBlock el -> mk (TBlock (evar :: enext :: el)) t_void e2.epos
			| _ -> mk (TBlock [evar;enext;e2]) t_void p
		in
		lblock [
			mk (TVar (cell,Some (mk (mk_field e1 "head") tcell p))) t_void p;
			mk (TWhile (
				mk (TBinop (OpNotEq, cexpr, mk (TConst TNull) tcell p)) ctx.t.tbool p,
				block,
				NormalWhile
			)) t_void p
		]
	| _ ->
		None

let optimize_for_loop_iterator ctx v e1 e2 p =
	let c,tl = (match follow e1.etype with TInst (c,pl) -> c,pl | _ -> raise Exit) in
	let _, _, fhasnext = (try raw_class_field (fun cf -> apply_params c.cl_params tl cf.cf_type) c tl "hasNext" with Not_found -> raise Exit) in
	if fhasnext.cf_kind <> Method MethInline then raise Exit;
	let tmp = gen_local ctx e1.etype e1.epos in
	let eit = mk (TLocal tmp) e1.etype p in
	let ehasnext = make_call ctx (mk (TField (eit,FInstance (c, tl, fhasnext))) (TFun([],ctx.t.tbool)) p) [] ctx.t.tbool p in
	let enext = mk (TVar (v,Some (make_call ctx (mk (TField (eit,quick_field_dynamic eit.etype "next")) (TFun ([],v.v_type)) p) [] v.v_type p))) ctx.t.tvoid p in
	let eblock = (match e2.eexpr with
		| TBlock el -> { e2 with eexpr = TBlock (enext :: el) }
		| _ -> mk (TBlock [enext;e2]) ctx.t.tvoid p
	) in
	mk (TBlock [
		mk (TVar (tmp,Some e1)) ctx.t.tvoid p;
		mk (TWhile (ehasnext,eblock,NormalWhile)) ctx.t.tvoid p
	]) ctx.t.tvoid p


(* ---------------------------------------------------------------------- *)
(* SANITIZE *)

(*
	makes sure that when an AST get generated to source code, it will not
	generate expressions that evaluate differently. It is then necessary to
	add parenthesises around some binary expressions when the AST does not
	correspond to the natural operand priority order for the platform
*)

(*
	this is the standard C++ operator precedence, which is also used by both JS and PHP
*)
let standard_precedence op =
	let left = true and right = false in
	match op with
	| OpMult | OpDiv | OpMod -> 5, left
	| OpAdd | OpSub -> 6, left
	| OpShl | OpShr | OpUShr -> 7, left
	| OpLt | OpLte | OpGt | OpGte -> 8, left
	| OpEq | OpNotEq -> 9, left
	| OpAnd -> 10, left
	| OpXor -> 11, left
	| OpOr -> 12, left
	| OpInterval -> 13, right (* haxe specific *)
	| OpBoolAnd -> 14, left
	| OpBoolOr -> 15, left
	| OpArrow -> 16, left
	| OpAssignOp OpAssign -> 17, right (* mimics ?: *)
	| OpAssign | OpAssignOp _ -> 18, right

let rec need_parent e =
	match e.eexpr with
	| TConst _ | TLocal _ | TArray _ | TField _ | TEnumParameter _ | TParenthesis _ | TCall _ | TNew _ | TTypeExpr _ | TObjectDecl _ | TArrayDecl _ -> false
	| TCast (e,None) | TMeta(_,e) -> need_parent e
	| TCast _ | TThrow _ | TReturn _ | TTry _ | TSwitch _ | TFor _ | TIf _ | TWhile _ | TBinop _ | TContinue | TBreak
	| TBlock _ | TVar _ | TFunction _ | TUnop _ -> true

let sanitize_expr com e =
	let parent e =
		match e.eexpr with
		| TParenthesis _ -> e
		| _ -> mk (TParenthesis e) e.etype e.epos
	in
	let block e =
		match e.eexpr with
		| TBlock _ -> e
		| _ -> mk (TBlock [e]) e.etype e.epos
	in
	let complex e =
		(* complex expressions are the one that once generated to source consists in several expressions  *)
		match e.eexpr with
		| TVar _	(* needs to be put into blocks *)
		| TFor _	(* a temp var is needed for holding iterator *)
		| TCall ({ eexpr = TLocal { v_name = "__js__" } },_) (* we never know *)
			-> block e
		| _ -> e
	in
	(* tells if the printed expresssion ends with an if without else *)
	let rec has_if e =
		match e.eexpr with
		| TIf (_,_,None) -> true
		| TWhile (_,e,NormalWhile) -> has_if e
		| TFor (_,_,e) -> has_if e
		| _ -> false
	in
	match e.eexpr with
	| TConst TNull ->
		if com.config.pf_static && not (is_nullable e.etype) then begin
			let rec loop t = match follow t with
				| TMono _ -> () (* in these cases the null will cast to default value *)
				| TFun _ -> () (* this is a bit a particular case, maybe flash-specific actually *)
				(* TODO: this should use get_underlying_type, but we do not have access to Codegen here.  *)
				| TAbstract(a,tl) when not (Meta.has Meta.CoreType a.a_meta) -> loop (apply_params a.a_params tl a.a_this)
				| _ -> com.error ("On static platforms, null can't be used as basic type " ^ s_type (print_context()) e.etype) e.epos
			in
			loop e.etype
		end;
		e
	| TBinop (op,e1,e2) ->
		let swap op1 op2 =
			let p1, left1 = standard_precedence op1 in
			let p2, _ = standard_precedence op2 in
			left1 && p1 <= p2
		in
		let rec loop ee left =
			match ee.eexpr with
			| TBinop (op2,_,_) -> if left then not (swap op2 op) else swap op op2
			| TIf _ -> if left then not (swap (OpAssignOp OpAssign) op) else swap op (OpAssignOp OpAssign)
			| TCast (e,None) | TMeta (_,e) -> loop e left
			| _ -> false
		in
		let e1 = if loop e1 true then parent e1 else e1 in
		let e2 = if loop e2 false then parent e2 else e2 in
		{ e with eexpr = TBinop (op,e1,e2) }
	| TUnop (op,mode,e1) ->
		let rec loop ee =
			match ee.eexpr with
			| TBinop _ | TIf _ | TUnop _ -> parent e1
			| TCast (e,None) | TMeta (_, e) -> loop e
			| _ -> e1
		in
		{ e with eexpr = TUnop (op,mode,loop e1)}
	| TIf (e1,e2,eelse) ->
		let e1 = parent e1 in
		let e2 = (if (eelse <> None && has_if e2) || (match e2.eexpr with TIf _ -> true | _ -> false) then block e2 else complex e2) in
		let eelse = (match eelse with None -> None | Some e -> Some (complex e)) in
		{ e with eexpr = TIf (e1,e2,eelse) }
	| TWhile (e1,e2,flag) ->
		let e1 = parent e1 in
		let e2 = complex e2 in
		{ e with eexpr = TWhile (e1,e2,flag) }
	| TFor (v,e1,e2) ->
		let e2 = complex e2 in
		{ e with eexpr = TFor (v,e1,e2) }
	| TFunction f ->
		let f = (match f.tf_expr.eexpr with
			| TBlock _ -> f
			| _ -> { f with tf_expr = block f.tf_expr }
		) in
		{ e with eexpr = TFunction f }
	| TCall (e2,args) ->
		if need_parent e2 then { e with eexpr = TCall(parent e2,args) } else e
	| TEnumParameter (e2,ef,i) ->
		if need_parent e2 then { e with eexpr = TEnumParameter(parent e2,ef,i) } else e
	| TField (e2,f) ->
		if need_parent e2 then { e with eexpr = TField(parent e2,f) } else e
	| TArray (e1,e2) ->
		if need_parent e1 then { e with eexpr = TArray(parent e1,e2) } else e
	| TTry (e1,catches) ->
		let e1 = block e1 in
		let catches = List.map (fun (v,e) -> v, block e) catches in
		{ e with eexpr = TTry (e1,catches) }
	| TSwitch (e1,cases,def) ->
		let e1 = parent e1 in
		let cases = List.map (fun (el,e) -> el, complex e) cases in
		let def = (match def with None -> None | Some e -> Some (complex e)) in
		{ e with eexpr = TSwitch (e1,cases,def) }
	| _ ->
		e

let reduce_expr com e =
	match e.eexpr with
	| TSwitch (_,cases,_) ->
		List.iter (fun (cl,_) ->
			List.iter (fun e ->
				match e.eexpr with
				| TCall ({ eexpr = TField (_,FEnum _) },_) -> error "Not-constant enum in switch cannot be matched" e.epos
				| _ -> ()
			) cl
		) cases;
		e
	| TBlock l ->
		(match List.rev l with
		| [] -> e
		| ec :: l ->
			(* remove all no-ops : not-final constants in blocks *)
			match List.filter (fun e -> match e.eexpr with
				| TConst _
				| TBlock []
				| TObjectDecl [] ->
					false
				| _ ->
					true
			) l with
			| [] -> ec
			| l -> { e with eexpr = TBlock (List.rev (ec :: l)) })
	| TParenthesis ec ->
		{ ec with epos = e.epos }
	| TTry (e,[]) ->
		e
	| _ ->
		e

let rec sanitize com e =
	sanitize_expr com (reduce_expr com (Type.map_expr (sanitize com) e))

(* ---------------------------------------------------------------------- *)
(* REDUCE *)

let reduce_control_flow ctx e = match e.eexpr with
	| TIf ({ eexpr = TConst (TBool t) },e1,e2) ->
		(if t then e1 else match e2 with None -> { e with eexpr = TBlock [] } | Some e -> e)
	| TWhile ({ eexpr = TConst (TBool false) },sub,flag) ->
		(match flag with
		| NormalWhile -> { e with eexpr = TBlock [] } (* erase sub *)
		| DoWhile -> e) (* we cant remove while since sub can contain continue/break *)
	| TSwitch (e1,cases,def) ->
		let e = match Texpr.skip e1 with
			| {eexpr = TConst ct} as e1 ->
				let rec loop cases = match cases with
					| (el,e) :: cases ->
						if List.exists (Texpr.equal e1) el then e
						else loop cases
					| [] ->
						begin match def with
						| None -> e
						| Some e -> e
						end
				in
				loop cases
			| _ ->
				e
		in
		e
	| TBinop (op,e1,e2) ->
		optimize_binop e op e1 e2
	| TUnop (op,flag,esub) ->
		optimize_unop e op flag esub
	| TCall ({ eexpr = TField (o,FClosure (c,cf)) } as f,el) ->
		let fmode = (match c with None -> FAnon cf | Some (c,tl) -> FInstance (c,tl,cf)) in
		{ e with eexpr = TCall ({ f with eexpr = TField (o,fmode) },el) }
	| _ ->
		e

let rec reduce_loop ctx e =
	let e = Type.map_expr (reduce_loop ctx) e in
	sanitize_expr ctx.com (match e.eexpr with
	| TCall ({ eexpr = TField ({ eexpr = TTypeExpr (TClassDecl c) },field) },params) ->
		(match api_inline ctx c (field_name field) params e.epos with
		| None -> reduce_expr ctx e
		| Some e -> reduce_loop ctx e)
	| TCall ({ eexpr = TFunction func } as ef,el) ->
		let cf = mk_field "" ef.etype e.epos null_pos in
		let ethis = mk (TConst TThis) t_dynamic e.epos in
		let rt = (match follow ef.etype with TFun (_,rt) -> rt | _ -> assert false) in
		let inl = (try type_inline ctx cf func ethis el rt None e.epos ~self_calling_closure:true false with Error (Custom _,_) -> None) in
		(match inl with
		| None -> reduce_expr ctx e
		| Some e -> reduce_loop ctx e)
	| _ ->
		reduce_expr ctx (reduce_control_flow ctx e))

let reduce_expression ctx e =
	if ctx.com.foptimize then reduce_loop ctx e else e

let rec make_constant_expression ctx ?(concat_strings=false) e =
	let e = reduce_loop ctx e in
	match e.eexpr with
	| TConst _ -> Some e
	| TBinop ((OpAdd|OpSub|OpMult|OpDiv|OpMod|OpShl|OpShr|OpUShr|OpOr|OpAnd|OpXor) as op,e1,e2) -> (match make_constant_expression ctx e1,make_constant_expression ctx e2 with
		| Some ({eexpr = TConst (TString s1)}), Some ({eexpr = TConst (TString s2)}) when concat_strings ->
			Some (mk (TConst (TString (s1 ^ s2))) ctx.com.basic.tstring (punion e1.epos e2.epos))
		| Some e1, Some e2 -> Some (mk (TBinop(op, e1, e2)) e.etype e.epos)
		| _ -> None)
	| TUnop((Neg | NegBits) as op,Prefix,e1) -> (match make_constant_expression ctx e1 with
		| Some e1 -> Some (mk (TUnop(op,Prefix,e1)) e.etype e.epos)
		| None -> None)
	| TCast (e1, None) ->
		(match make_constant_expression ctx e1 with
		| None -> None
		| Some e1 -> Some {e with eexpr = TCast(e1,None)})
	| TParenthesis e1 ->
		begin match make_constant_expression ctx ~concat_strings e1 with
			| None -> None
			| Some e1 -> Some {e with eexpr = TParenthesis e1}
		end
	| TMeta(m,e1) ->
		begin match make_constant_expression ctx ~concat_strings e1 with
			| None -> None
			| Some e1 -> Some {e with eexpr = TMeta(m,e1)}
		end
	| TTypeExpr _ -> Some e
	(* try to inline static function calls *)
	(* Disabled for now, see #4254. *)
(* 	| TCall ({ etype = TFun(_,ret); eexpr = TField (_,FStatic (c,cf)) },el) ->
		(try
			let func = match cf.cf_expr with Some ({eexpr = TFunction func}) -> func | _ -> raise Not_found in
			let ethis = mk (TConst TThis) t_dynamic e.epos in
			let inl = (try type_inline ctx cf func ethis el ret None e.epos false with Error (Custom _,_) -> None) in
			(match inl with
			| None -> None
			| Some e -> make_constant_expression ctx e)
		with Not_found -> None) *)
	| _ -> None

(* ---------------------------------------------------------------------- *)
(* INLINE CONSTRUCTORS *)
(* This version is disabled by default, use -D old-constructor-inline to use this *)

(*
	First pass :
	We will look at local variables in the form   var v = new ....
	we only capture the ones which have constructors marked as inlined
	then we make sure that these locals are no more referenced except for fields accesses

	Second pass :
	We replace the variables by their fields lists, and the corresponding fields accesses as well
*)

type inline_info_kind =
	| IKCtor of tclass_field * bool
	| IKStructure
	| IKArray of int

type inline_info = {
	ii_var : tvar;
	ii_expr : texpr;
	ii_kind : inline_info_kind;
	mutable ii_fields : (string,tvar) PMap.t;
}

let inline_constructors ctx e =
	let vars = ref IntMap.empty in
	let is_valid_ident s =
		try
			if String.length s = 0 then raise Exit;
			begin match String.unsafe_get s 0 with
				| 'a'..'z' | 'A'..'Z' | '_' -> ()
				| _ -> raise Exit
			end;
			for i = 1 to String.length s - 1 do
				match String.unsafe_get s i with
				| 'a'..'z' | 'A'..'Z' | '_' -> ()
				| '0'..'9' when i > 0 -> ()
				| _ -> raise Exit
			done;
			true
		with Exit ->
			false
	in
	let cancel v p =
		try
			let ii = IntMap.find v.v_id !vars in
			vars := IntMap.remove v.v_id !vars;
			v.v_id <- -v.v_id;
			begin match ii.ii_kind with
				| IKCtor(cf,true) ->
					display_error ctx "Extern constructor could not be inlined" p;
					error "Variable is used here" p;
				| _ ->
					()
			end;
		with Not_found ->
			()
	in
	let add v e kind =
		let ii = {
			ii_var = v;
			ii_fields = PMap.empty;
			ii_expr = e;
			ii_kind = kind
		} in
		v.v_id <- -v.v_id;
		vars := IntMap.add v.v_id ii !vars;
	in
	let get_field_var v s =
		let ii = IntMap.find v.v_id !vars in
		PMap.find s ii.ii_fields
	in
	let add_field_var v s t =
		let ii = IntMap.find v.v_id !vars in
		let v' = alloc_var (Printf.sprintf "%s_%s" v.v_name s) t v.v_pos in
		v'.v_meta <- (Meta.InlineConstructorVariable,[],v.v_pos) :: v'.v_meta;
		ii.ii_fields <- PMap.add s v' ii.ii_fields;
		v'
	in
	let int_field_name i =
		if i < 0 then "n" ^ (string_of_int (-i))
		else (string_of_int i)
	in
	let is_extern_ctor c cf = c.cl_extern || Meta.has Meta.Extern cf.cf_meta in
	let rec find_locals e = match e.eexpr with
		| TVar(v,Some e1) ->
			find_locals e1;
			let rec loop el_init e1 = match e1.eexpr with
				| TBlock el ->
					begin match List.rev el with
					| e1 :: el ->
						loop (el @ el_init) e1
					| [] ->
						()
					end
				| TNew({ cl_constructor = Some ({cf_kind = Method MethInline; cf_expr = Some ({eexpr = TFunction tf})} as cf)} as c,tl,pl) when type_iseq v.v_type e1.etype ->
					begin match type_inline_ctor ctx c cf tf (mk (TLocal v) (TInst (c,tl)) e1.epos) pl e1.epos with
					| Some e ->
						let e' = match el_init with
							| [] -> e
							| _ -> mk (TBlock (List.rev (e :: el_init))) e.etype e.epos
						in
						add v e' (IKCtor(cf,is_extern_ctor c cf));
						find_locals e
					| None ->
						()
					end
				| TObjectDecl fl when fl <> [] ->
					begin try
						let ev = mk (TLocal v) v.v_type e.epos in
						let el = List.fold_left (fun acc (s,e) ->
							if not (is_valid_ident s) then raise Exit;
							let ef = mk (TField(ev,FDynamic s)) e.etype e.epos in
							let e = mk (TBinop(OpAssign,ef,e)) e.etype e.epos in
							e :: acc
						) el_init fl in
						let e = mk (TBlock (List.rev el)) ctx.t.tvoid e.epos in
						add v e IKStructure
					with Exit ->
						()
					end
				| TArrayDecl el ->
					let ev = mk (TLocal v) v.v_type e.epos in
					let el,_ = List.fold_left (fun (acc,i) e ->
						let ef = mk (TField(ev,FDynamic (string_of_int i))) e.etype e.epos in
						let e = mk (TBinop(OpAssign,ef,e)) e.etype e.epos in
						e :: acc,i + 1
					) (el_init,0) el in
					let e = mk (TBlock (List.rev el)) ctx.t.tvoid e.epos in
					add v e (IKArray (List.length el))
				| TCast(e1,None) | TParenthesis e1 ->
					loop el_init e1
				| _ ->
					()
			in
			loop [] e1
		| TBinop(OpAssign,({eexpr = TField({eexpr = TLocal v},fa)} as e1),e2) when v.v_id < 0 ->
			let s = field_name fa in
			(try ignore(get_field_var v s) with Not_found -> ignore(add_field_var v s e1.etype));
			find_locals e2
		| TField({eexpr = TLocal v},fa) when v.v_id < 0 ->
			begin match extract_field fa with
			| Some ({cf_kind = Var _} as cf) ->
				(* Arrays are not supposed to have public var fields, besides "length" (which we handle when inlining),
				   however, its inlined methods may generate access to private implementation fields (such as internal
				   native array), in this case we have to cancel inlining.
				*)
				if cf.cf_name <> "length" then
					begin match (IntMap.find v.v_id !vars).ii_kind with
					| IKArray _ -> cancel v e.epos
					| _ -> (try ignore(get_field_var v cf.cf_name) with Not_found -> ignore(add_field_var v cf.cf_name e.etype));
					end
			| _ -> cancel v e.epos
			end
		| TArray({eexpr = TLocal v},{eexpr = TConst (TInt i)}) when v.v_id < 0 ->
			let i = Int32.to_int i in
			begin try
				let ii = IntMap.find v.v_id !vars in
				let l = match ii.ii_kind with
					| IKArray l -> l
					| _ -> raise Not_found
				in
				if i < 0 || i >= l then raise Not_found;
			with Not_found ->
				cancel v e.epos
			end
		| TLocal v when v.v_id < 0 ->
			cancel v e.epos;
		| _ ->
			Type.iter find_locals e
	in
	find_locals e;
	(* Pass 2 *)
	let inline v p =
		try
			let ii = IntMap.find v.v_id !vars in
			let el = PMap.fold (fun v acc -> (mk (TVar(v,None)) ctx.t.tvoid p) :: acc) ii.ii_fields [] in
			let e = {ii.ii_expr with eexpr = TBlock (el @ [ii.ii_expr])} in
			Some e
		with Not_found ->
			None
	in
	let assign_or_declare v name e2 t p =
		 try
			let v = get_field_var v name in
			let e1 = mk (TLocal v) t p in
			mk (TBinop(OpAssign,e1,e2)) e1.etype p
		with Not_found ->
			let v = add_field_var v name t in
			mk (TVar(v,Some e2)) ctx.t.tvoid e.epos
	in
	let use_local_or_null v name t p =
		try
			let v' = get_field_var v name in
			mk (TLocal v') t p
		with Not_found -> try
			if name <> "length" then raise Not_found;
			let ii = IntMap.find v.v_id !vars in
			begin match ii.ii_kind with
			| IKArray l -> mk (TConst (TInt (Int32.of_int l))) ctx.t.tint p
			| _ -> raise Not_found
			end
		with Not_found ->
			mk (TConst TNull) t p
	in
	let flatten e =
		let el = ref [] in
		let rec loop e = match e.eexpr with
			| TBlock el ->
				List.iter loop el
			| _ ->
				el := e :: !el
		in
		loop e;
		let e = mk (TBlock (List.rev !el)) e.etype e.epos in
		mk (TMeta((Meta.MergeBlock,[],e.epos),e)) e.etype e.epos
	in
	let rec loop e = match e.eexpr with
		| TVar(v,_) when v.v_id < 0 ->
			begin match inline v e.epos with
			| Some e ->
				let e = flatten e in
				loop e
			| None ->
				cancel v e.epos;
				e
			end
		| TBinop(OpAssign,({eexpr = TField({eexpr = TLocal v},fa)} as e1),e2) when v.v_id < 0 ->
			let e2 = loop e2 in
			assign_or_declare v (field_name fa) e2 e1.etype e.epos
		| TField({eexpr = TLocal v},fa) when v.v_id < 0 ->
			use_local_or_null v (field_name fa) e.etype e.epos
		| TBinop(OpAssign,({eexpr = TArray({eexpr = TLocal v},{eexpr = TConst (TInt i)})} as e1),e2) when v.v_id < 0 ->
			let e2 = loop e2 in
			let name = int_field_name (Int32.to_int i) in
			assign_or_declare v name e2 e1.etype e.epos
		| TArray({eexpr = TLocal v},{eexpr = TConst (TInt i)}) when v.v_id < 0	->
			use_local_or_null v (int_field_name (Int32.to_int i)) e.etype e.epos
		| TBlock el ->
			let rec block acc el = match el with
				| e1 :: el ->
					begin match loop e1 with
					| {eexpr = TMeta((Meta.MergeBlock,_,_),{eexpr = TBlock el2})} ->
						let acc = block acc el2 in
						block acc el
					| e -> block (e :: acc) el
					end
				| [] ->
					acc
			in
			let el = block [] el in
			mk (TBlock (List.rev el)) e.etype e.epos
		| TNew({ cl_constructor = Some ({cf_kind = Method MethInline; cf_expr = Some ({eexpr = TFunction _})} as cf)} as c,_,_) when is_extern_ctor c cf ->
			display_error ctx "Extern constructor could not be inlined" e.epos;
			Type.map_expr loop e
		| _ ->
			Type.map_expr loop e
	in
	loop e

(* ---------------------------------------------------------------------- *)
(* COMPLETION *)

exception Return of Ast.expr

type compl_locals = {
	mutable r : (string, (complex_type option * (int * Ast.expr * compl_locals) option)) PMap.t;
}

let optimize_completion_expr e =
	let iid = ref 0 in
	let typing_side_effect = ref false in
	let locals : compl_locals = { r = PMap.empty } in
	let save() = let old = locals.r in (fun() -> locals.r <- old) in
	let get_local n = PMap.find n locals.r in
	let maybe_typed e =
		match fst e with
		| EConst (Ident "null") -> false
		| _ -> true
	in
	let decl n t e =
		typing_side_effect := true;
		locals.r <- PMap.add n (t,(match e with Some e when maybe_typed e -> incr iid; Some (!iid,e,{ r = locals.r }) | _ -> None)) locals.r
	in
	let rec loop e =
		let p = snd e in
		match fst e with
		| EConst (Ident n) ->
			(try
				(match get_local n with
				| Some _ , _ -> ()
				| _ -> typing_side_effect := true)
			with Not_found ->
				());
			e
		| EBinop (OpAssign,(EConst (Ident n),_),esub) ->
			(try
				(match get_local n with
				| None, None when maybe_typed esub -> decl n None (Some esub)
				| _ -> ())
			with Not_found ->
				());
			map e
		| EVars vl ->
			let vl = List.map (fun ((v,pv),t,e) ->
				let e = (match e with None -> None | Some e -> Some (loop e)) in
				decl v (Option.map fst t) e;
				((v,pv),t,e)
			) vl in
			(EVars vl,p)
		| EBlock el ->
			let old = save() in
			let told = ref (!typing_side_effect) in
			let el = List.fold_left (fun acc e ->
				typing_side_effect := false;
				let e = loop e in
				if !typing_side_effect || Display.is_display_position (pos e) then begin told := true; e :: acc end else acc
			) [] el in
			old();
			typing_side_effect := !told;
			(EBlock (List.rev el),p)
		| EFunction (v,f) ->
			(match v with
			| None -> ()
			| Some name ->
				decl name None (Some e));
			let old = save() in
			List.iter (fun ((n,_),_,_,t,e) -> decl n (Option.map fst t) e) f.f_args;
			let e = map e in
			old();
			e
		| EFor ((EIn ((EConst (Ident n),_) as id,it),p),efor) ->
			let it = loop it in
			let old = save() in
			let etmp = (EConst (Ident "$tmp"),p) in
			decl n None (Some (EBlock [
				(EVars [("$tmp",null_pos),None,None],p);
				(EFor ((EIn (id,it),p),(EBinop (OpAssign,etmp,(EConst (Ident n),p)),p)),p);
				etmp
			],p));
			let efor = loop efor in
			old();
			(EFor ((EIn (id,it),p),efor),p)
		| EReturn _ ->
			typing_side_effect := true;
			map e
		| ESwitch (e,cases,def) ->
			let e = loop e in
			let cases = List.map (fun (el,eg,eo,p) -> match eo with
				| None ->
					el,eg,eo,p
				| Some e ->
					let el = List.map loop el in
					let old = save() in
					List.iter (fun e ->
						match fst e with
						| ECall (_,pl) ->
							List.iter (fun p ->
								match fst p with
								| EConst (Ident i) -> decl i None None (* sadly *)
								| _ -> ()
							) pl
						| _ -> ()
					) el;
					let e = loop e in
					old();
					el, eg, Some e, p
			) cases in
			let def = match def with
				| None -> None
				| Some (None,p) -> Some (None,p)
				| Some (Some e,p) -> Some (Some (loop e),p)
			in
			(ESwitch (e,cases,def),p)
		| ETry (et,cl) ->
			let et = loop et in
			let cl = List.map (fun ((n,pn),(t,pt),e,p) ->
				let old = save() in
				decl n (Some t) None;
				let e = loop e in
				old();
				(n,pn), (t,pt), e, p
			) cl in
			(ETry (et,cl),p)
		| EDisplay (s,call) ->
			typing_side_effect := true;
			let tmp_locals = ref [] in
			let tmp_hlocals = ref PMap.empty in
			let rec subst_locals locals e =
				match fst e with
				| EConst (Ident n) ->
					let p = snd e in
					(try
						(match PMap.find n locals.r with
						| Some t , _ -> (ECheckType ((EConst (Ident "null"),p),(t,p)),p)
						| _, Some (id,e,lc) ->
							let name = (try
								PMap.find id (!tmp_hlocals)
							with Not_found ->
								let e = subst_locals lc e in
								let name = "$tmp_" ^ string_of_int id in
								tmp_locals := ((name,null_pos),None,Some e) :: !tmp_locals;
								tmp_hlocals := PMap.add id name !tmp_hlocals;
								name
							) in
							(EConst (Ident name),p)
						| None, None ->
							(* we can't replace the var *)
							raise Exit)
					with Not_found ->
						(* not found locals are most likely to be member/static vars *)
						e)
				| EFunction (_,f) ->
					Ast.map_expr (subst_locals { r = PMap.foldi (fun n i acc -> if List.exists (fun ((a,_),_,_,_,_) -> a = n) f.f_args then acc else PMap.add n i acc) locals.r PMap.empty }) e
				| EObjectDecl [] ->
					(* this probably comes from { | completion so we need some context} *)
					raise Exit
				| _ ->
					Ast.map_expr (subst_locals locals) e
			in
			(try
				let e = subst_locals locals s in
				let e = (EBlock [(EVars (List.rev !tmp_locals),p);(EDisplay (e,call),p)],p) in
				raise (Return e)
			with Exit ->
				map e)
		| EDisplayNew _ ->
			raise (Return e)
		| _ ->
			map e
	and map e =
		Ast.map_expr loop e
	in
	(try loop e with Return e -> e)

(* ---------------------------------------------------------------------- *)
