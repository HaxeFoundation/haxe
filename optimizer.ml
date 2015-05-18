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

(* ---------------------------------------------------------------------- *)
(* API OPTIMIZATIONS *)

(* tells if an expression causes side effects. This does not account for potential null accesses (fields/arrays/ops) *)
let has_side_effect e =
	let rec loop e =
		match e.eexpr with
		| TConst _ | TLocal _ | TTypeExpr _ | TFunction _ -> ()
		| TCall ({ eexpr = TField(_,FStatic({ cl_path = ([],"Std") },{ cf_name = "string" })) },args) -> Type.iter loop e
		| TNew _ | TCall _ | TBinop ((OpAssignOp _ | OpAssign),_,_) | TUnop ((Increment|Decrement),_,_) -> raise Exit
		| TReturn _ | TBreak | TContinue | TThrow _ | TCast (_,Some _) -> raise Exit
		| TArray _ | TEnumParameter _ | TCast (_,None) | TBinop _ | TUnop _ | TParenthesis _ | TMeta _ | TWhile _ | TFor _
		| TField _ | TIf _ | TTry _ | TSwitch _ | TArrayDecl _ | TBlock _ | TObjectDecl _ | TVar _ -> Type.iter loop e
	in
	try
		loop e; false
	with Exit ->
		true

let mk_untyped_call name p params =
	{
		eexpr = TCall({ eexpr = TLocal(alloc_unbound_var name t_dynamic); etype = t_dynamic; epos = p }, params);
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
				let v = add_local ctx n t in
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
			(* generate (o|0) === o check *)
			let teq = mk_local ctx "__strict_eq__" (tfun [tint; tint] tbool) p in
			let lhs = mk (TBinop (Ast.OpOr, o, mk (TConst (TInt Int32.zero)) tint p)) tint p in
			Some (mk (TCall (teq, [lhs; o])) tbool p)
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
			let v' = alloc_var v.v_name v.v_type in
			if Meta.has Meta.Unbound v.v_meta then v'.v_meta <- [Meta.Unbound,[],p];
			let i = {
				i_var = v;
				i_subst = v';
				i_captured = false;
				i_write = false;
				i_force_temp = false;
				i_read = 0;
			} in
			i.i_subst.v_meta <- v.v_meta;
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
			if v.v_type != t_dynamic && follow e.etype == t_dynamic then (local v).i_write <- true;
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
	let vthis = alloc_var "_this" ethis.etype in
	let inlined_vars = List.map2 (fun e (v,_) ->
		let l = local v in
		if has_side_effect e then l.i_force_temp <- true; (* force tmp var *)
		l, e
	) (ethis :: loop params f.tf_args true) ((vthis,None) :: f.tf_args) in
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
			if v.v_name = "__dollar__delay_call" then cancel_inlining := true;
			let e = { e with eexpr = TLocal l.i_subst } in
			if Meta.has Meta.This v.v_meta then mk (TCast(e,None)) v.v_type e.epos else e
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
			let term = term && def <> None in
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
			let has_return e =
				let rec loop e = match e.eexpr with
					| TReturn _ -> raise Exit
					| _ -> Type.iter loop e
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
				| ({ eexpr = TIf (cond,e1,None) } as e) :: l when term && has_return e1 ->
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
(* 		| TCall({eexpr = TLocal v} as e1,el) ->
			let el = List.map (map false) el in
			let l = read_local v in
			let edef() = {e with eexpr = TCall({e1 with eexpr = TLocal l.i_subst},el)} in
			begin try
				begin match List.assq l inlined_vars with
				| {eexpr = TField(_, (FStatic(_,cf) | FInstance(_,_,cf)))} as e' when cf.cf_kind = Method MethInline ->
					make_call ctx e' el e.etype e.epos
				| _ ->
					edef()
				end
			with Not_found ->
				edef()
			end *)
		| TFunction f ->
			(match f.tf_args with [] -> () | _ -> has_vars := true);
			let old = save_locals ctx and old_fun = !in_local_fun in
			let args = List.map (function(v,c) -> (local v).i_subst, c) f.tf_args in
			in_local_fun := true;
			let expr = map false f.tf_expr in
			in_local_fun := old_fun;
			old();
			{ e with eexpr = TFunction { tf_args = args; tf_expr = expr; tf_type = f.tf_type } }
		| TConst TSuper ->
			error "Cannot inline function containing super" po
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
			| TLocal v when Meta.has Meta.This v.v_meta -> true
			| TLocal _ | TConst _ -> not i.i_write
			| TFunction _ -> if i.i_write then error "Cannot modify a closure parameter inside inline method" p; true
			| _ -> not i.i_write && i.i_read <= 1
		) in
		let flag = flag && (not i.i_captured || is_constant e) in
		(* force inlining if we modify 'this' *)
		if i.i_write && (Meta.has Meta.This i.i_var.v_meta) then force := true;
		(* force inlining of 'this' variable if it is written *)
		let flag = if not flag && (Meta.has Meta.This i.i_var.v_meta) && i.i_write then begin
			if not (is_writable e) then error "Cannot modify the abstract value, store it into a local first" p;
			true
		end else flag in
		if flag then begin
			subst := PMap.add i.i_subst.v_id e !subst;
			acc
		end else
			(i.i_subst,Some e) :: acc
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
	if !cancel_inlining || (not (Common.defined ctx.com Define.Analyzer) && Common.platform ctx.com Js && not !force && (init <> None || !has_vars)) then
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
				{e with etype = tret}
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
			| Meta.Deprecated,_,_ -> mk (TMeta(meta,e)) e.etype e.epos
			| _ -> e
		in
		let e = List.fold_left inline_meta e cf.cf_meta in
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
					v.v_type <- map_type v.v_type;
				end;
				v
			in
			let rec map_expr_type e = Type.map_expr_type map_expr_type map_type map_var e in
			Some (map_expr_type e)


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
		let i = add_local ctx i pt in
		let index = gen_local ctx t_int in
		let arr, avars = (match e1.eexpr with
			| TLocal _ -> e1, None
			| _ ->
				let atmp = gen_local ctx e1.etype in
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
			| _, TConst _ | _ , TLocal _ -> None
			| _ -> Some (gen_local ctx t_int)
		) in
		let tmp = gen_local ctx t_int in
		let i = add_local ctx i t_int in
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
			let v = add_local ctx i pt in
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
		let i = add_local ctx i t in
		let cell = gen_local ctx tcell in
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
	let tmp = gen_local ctx e1.etype in
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
	| TConst _ | TLocal _ | TArray _ | TField _ | TEnumParameter _ | TParenthesis _ | TMeta _ | TCall _ | TNew _ | TTypeExpr _ | TObjectDecl _ | TArrayDecl _ -> false
	| TCast (e,None) -> need_parent e
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
			| TCast (e,None) -> loop e left
			| _ -> false
		in
		let e1 = if loop e1 true then parent e1 else e1 in
		let e2 = if loop e2 false then parent e2 else e2 in
		{ e with eexpr = TBinop (op,e1,e2) }
	| TUnop (op,mode,e1) ->
		let rec loop ee =
			match ee.eexpr with
			| TBinop _ | TIf _ | TUnop _ -> parent e1
			| TCast (e,None) -> loop e
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

let optimize_binop e op e1 e2 =
	let is_float t =
		match follow t with
		| TAbstract({ a_path = [],"Float" },_) -> true
		| _ -> false
	in
	let is_numeric t =
		match follow t with
		| TAbstract({ a_path = [],("Float"|"Int") },_) -> true
		| _ -> false
	in
	let check_float op f1 f2 =
		let f = op f1 f2 in
		let fstr = float_repres f in
		if (match classify_float f with FP_nan | FP_infinite -> false | _ -> float_of_string fstr = f) then { e with eexpr = TConst (TFloat fstr) } else e
	in
	(match e1.eexpr, e2.eexpr with
	| TConst (TInt 0l) , _ when op = OpAdd && is_numeric e2.etype -> e2
	| TConst (TInt 1l) , _ when op = OpMult -> e2
	| TConst (TFloat v) , _ when op = OpAdd && float_of_string v = 0. && is_float e2.etype -> e2
	| TConst (TFloat v) , _ when op = OpMult && float_of_string v = 1. && is_float e2.etype -> e2
	| _ , TConst (TInt 0l) when (match op with OpAdd -> is_numeric e1.etype | OpSub | OpShr | OpShl -> true | _ -> false) -> e1 (* bits operations might cause overflow *)
	| _ , TConst (TInt 1l) when op = OpMult -> e1
	| _ , TConst (TFloat v) when (match op with OpAdd | OpSub -> float_of_string v = 0. && is_float e1.etype | _ -> false) -> e1 (* bits operations might cause overflow *)
	| _ , TConst (TFloat v) when op = OpMult && float_of_string v = 1. && is_float e1.etype -> e1
	| TConst TNull, TConst TNull ->
		(match op with
		| OpEq -> { e with eexpr = TConst (TBool true) }
		| OpNotEq -> { e with eexpr = TConst (TBool false) }
		| _ -> e)
	| TFunction _, TConst TNull ->
		(match op with
		| OpEq -> { e with eexpr = TConst (TBool false) }
		| OpNotEq -> { e with eexpr = TConst (TBool true) }
		| _ -> e)
	| TConst TNull, TFunction _ ->
		(match op with
		| OpEq -> { e with eexpr = TConst (TBool false) }
		| OpNotEq -> { e with eexpr = TConst (TBool true) }
		| _ -> e)
	| TConst (TInt a), TConst (TInt b) ->
		let opt f = try { e with eexpr = TConst (TInt (f a b)) } with Exit -> e in
		let check_overflow f =
			opt (fun a b ->
				let v = f (Int64.of_int32 a) (Int64.of_int32 b) in
				let iv = Int64.to_int32 v in
				if Int64.compare (Int64.of_int32 iv) v <> 0 then raise Exit;
				iv
			)
		in
		let ebool t =
			{ e with eexpr = TConst (TBool (t (Int32.compare a b) 0)) }
		in
		(match op with
		| OpAdd -> check_overflow Int64.add
		| OpSub -> check_overflow Int64.sub
		| OpMult -> check_overflow Int64.mul
		| OpDiv -> check_float ( /. ) (Int32.to_float a) (Int32.to_float b)
		| OpAnd -> opt Int32.logand
		| OpOr -> opt Int32.logor
		| OpXor -> opt Int32.logxor
		| OpShl -> opt (fun a b -> Int32.shift_left a (Int32.to_int b))
		| OpShr -> opt (fun a b -> Int32.shift_right a (Int32.to_int b))
		| OpUShr -> opt (fun a b -> Int32.shift_right_logical a (Int32.to_int b))
		| OpEq -> ebool (=)
		| OpNotEq -> ebool (<>)
		| OpGt -> ebool (>)
		| OpGte -> ebool (>=)
		| OpLt -> ebool (<)
		| OpLte -> ebool (<=)
		| _ -> e)
	| TConst ((TFloat _ | TInt _) as ca), TConst ((TFloat _ | TInt _) as cb) ->
		let fa = (match ca with
			| TFloat a -> float_of_string a
			| TInt a -> Int32.to_float a
			| _ -> assert false
		) in
		let fb = (match cb with
			| TFloat b -> float_of_string b
			| TInt b -> Int32.to_float b
			| _ -> assert false
		) in
		let fop op = check_float op fa fb in
		let ebool t =
			{ e with eexpr = TConst (TBool (t (compare fa fb) 0)) }
		in
		(match op with
		| OpAdd -> fop (+.)
		| OpDiv -> fop (/.)
		| OpSub -> fop (-.)
		| OpMult -> fop ( *. )
		| OpEq -> ebool (=)
		| OpNotEq -> ebool (<>)
		| OpGt -> ebool (>)
		| OpGte -> ebool (>=)
		| OpLt -> ebool (<)
		| OpLte -> ebool (<=)
		| _ -> e)
	| TConst (TBool a), TConst (TBool b) ->
		let ebool f =
			{ e with eexpr = TConst (TBool (f a b)) }
		in
		(match op with
		| OpEq -> ebool (=)
		| OpNotEq -> ebool (<>)
		| OpBoolAnd -> ebool (&&)
		| OpBoolOr -> ebool (||)
		| _ -> e)
	| TConst a, TConst b when op = OpEq || op = OpNotEq ->
		let ebool b =
			{ e with eexpr = TConst (TBool (if op = OpEq then b else not b)) }
		in
		(match a, b with
		| TInt a, TFloat b | TFloat b, TInt a -> ebool (Int32.to_float a = float_of_string b)
		| _ -> ebool (a = b))
	| TConst (TBool a), _ ->
		(match op with
		| OpBoolAnd -> if a then e2 else { e with eexpr = TConst (TBool false) }
		| OpBoolOr -> if a then { e with eexpr = TConst (TBool true) } else e2
		| _ -> e)
	| _ , TConst (TBool a) ->
		(match op with
		| OpBoolAnd when a  -> e1
		| OpBoolOr when not a -> e1
		| _ -> e)
	| TField (_,FEnum (e1,f1)), TField (_,FEnum (e2,f2)) when e1 == e2 ->
		(match op with
		| OpEq -> { e with eexpr = TConst (TBool (f1 == f2)) }
		| OpNotEq -> { e with eexpr = TConst (TBool (f1 != f2)) }
		| _ -> e)
	| _, TCall ({ eexpr = TField (_,FEnum _) },_) | TCall ({ eexpr = TField (_,FEnum _) },_), _ ->
		(match op with
		| OpAssign -> e
		| _ ->
			error "You cannot directly compare enums with arguments. Use either 'switch' or 'Type.enumEq'" e.epos)
	| _ ->
		e)

let optimize_unop e op flag esub =
	match op, esub.eexpr with
		| Not, (TConst (TBool f) | TParenthesis({eexpr = TConst (TBool f)})) -> { e with eexpr = TConst (TBool (not f)) }
		| Neg, TConst (TInt i) -> { e with eexpr = TConst (TInt (Int32.neg i)) }
		| NegBits, TConst (TInt i) -> { e with eexpr = TConst (TInt (Int32.lognot i)) }
		| Neg, TConst (TFloat f) ->
			let v = 0. -. float_of_string f in
			let vstr = float_repres v in
			if float_of_string vstr = v then
				{ e with eexpr = TConst (TFloat vstr) }
			else
				e
		| _ -> e

let rec reduce_loop ctx e =
	let e = Type.map_expr (reduce_loop ctx) e in
	sanitize_expr ctx.com (match e.eexpr with
	| TIf ({ eexpr = TConst (TBool t) },e1,e2) ->
		(if t then e1 else match e2 with None -> { e with eexpr = TBlock [] } | Some e -> e)
	| TWhile ({ eexpr = TConst (TBool false) },sub,flag) ->
		(match flag with
		| NormalWhile -> { e with eexpr = TBlock [] } (* erase sub *)
		| DoWhile -> e) (* we cant remove while since sub can contain continue/break *)
	| TBinop (op,e1,e2) ->
		optimize_binop e op e1 e2
	| TUnop (op,flag,esub) ->
		optimize_unop e op flag esub
	| TCall ({ eexpr = TField ({ eexpr = TTypeExpr (TClassDecl c) },field) },params) ->
		(match api_inline ctx c (field_name field) params e.epos with
		| None -> reduce_expr ctx e
		| Some e -> reduce_loop ctx e)
	| TCall ({ eexpr = TFunction func } as ef,el) ->
		let cf = mk_field "" ef.etype e.epos in
		let ethis = mk (TConst TThis) t_dynamic e.epos in
		let rt = (match follow ef.etype with TFun (_,rt) -> rt | _ -> assert false) in
		let inl = (try type_inline ctx cf func ethis el rt None e.epos ~self_calling_closure:true false with Error (Custom _,_) -> None) in
		(match inl with
		| None -> reduce_expr ctx e
		| Some e -> reduce_loop ctx e)
	| TCall ({ eexpr = TField (o,FClosure (c,cf)) } as f,el) ->
		let fmode = (match c with None -> FAnon cf | Some (c,tl) -> FInstance (c,tl,cf)) in
		{ e with eexpr = TCall ({ f with eexpr = TField (o,fmode) },el) }
	| TSwitch (e1,[[{eexpr = TConst (TBool true)}],{eexpr = TConst (TBool true)}],Some ({eexpr = TConst (TBool false)})) ->
		(* introduced by extractors in some cases *)
		e1
	| _ ->
		reduce_expr ctx e)

let reduce_expression ctx e =
	if ctx.com.foptimize then reduce_loop ctx e else e

let rec make_constant_expression ctx ?(concat_strings=false) e =
	let e = reduce_loop ctx e in
	match e.eexpr with
	| TConst _ -> Some e
	| TBinop ((OpAdd|OpSub|OpMult|OpDiv|OpMod) as op,e1,e2) -> (match make_constant_expression ctx e1,make_constant_expression ctx e2 with
		| Some ({eexpr = TConst (TString s1)}), Some ({eexpr = TConst (TString s2)}) when concat_strings ->
			Some (mk (TConst (TString (s1 ^ s2))) ctx.com.basic.tstring (punion e1.epos e2.epos))
		| Some e1, Some e2 -> Some (mk (TBinop(op, e1, e2)) e.etype e.epos)
		| _ -> None)
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
	| TCall ({ etype = TFun(_,ret); eexpr = TField (_,FStatic (c,cf)) },el) ->
		(try
			let func = match cf.cf_expr with Some ({eexpr = TFunction func}) -> func | _ -> raise Not_found in
			let ethis = mk (TConst TThis) t_dynamic e.epos in
			let inl = (try type_inline ctx cf func ethis el ret None e.epos false with Error (Custom _,_) -> None) in
			(match inl with
			| None -> None
			| Some e -> make_constant_expression ctx e)
		with Not_found -> None)
	| _ -> None

(* ---------------------------------------------------------------------- *)
(* INLINE CONSTRUCTORS *)

(*
	First pass :
	We will look at local variables in the form   var v = new ....
	we only capture the ones which have constructors marked as inlined
	then we make sure that these locals are no more referenced except for fields accesses

	Second pass :
	We replace the variables by their fields lists, and the corresponding fields accesses as well
*)

type inline_kind =
	| IKCtor of tfunc * tclass_field * tclass * t list * texpr list * texpr list
	| IKArray of texpr list * t
	| IKStructure of (string * texpr) list
	| IKNone

let inline_constructors ctx e =
	let vars = ref PMap.empty in
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
	let rec get_inline_ctor_info e = match e.eexpr with
		| TNew ({ cl_constructor = Some ({ cf_kind = Method MethInline; cf_expr = Some { eexpr = TFunction f } } as cst) } as c,tl,pl) ->
			IKCtor (f,cst,c,tl,pl,[])
		| TObjectDecl [] | TArrayDecl [] ->
			IKNone
		| TArrayDecl el ->
			begin match follow e.etype with
				| TInst({cl_path = [],"Array"},[t]) ->
					IKArray(el,t)
				| _ ->
					IKNone
			end
		| TObjectDecl fl ->
			if (List.exists (fun (s,_) -> not (is_valid_ident s)) fl) then
				IKNone
			else
				IKStructure fl
		| TCast(e,None) | TParenthesis e ->
			get_inline_ctor_info e
		| TBlock el ->
			begin match List.rev el with
				| e :: el ->
					begin match get_inline_ctor_info e with
						| IKCtor(f,cst,c,tl,pl,e_init) ->
							IKCtor(f,cst,c,tl,pl,(List.rev el) @ e_init)
						| _ ->
							IKNone
					end
				| [] ->
					IKNone
			end
		| _ ->
			IKNone
	in
	let check_field v s e t =
		let (a,b,fields,c,d) = PMap.find (-v.v_id) !vars in
		if not (List.exists (fun (s2,_,_) -> s = s2) fields) then
			vars := PMap.add (-v.v_id) (a,b,(s,e,t) :: fields,c,d) !vars
	in
	let cancel v =
		v.v_id <- -v.v_id;
		(* error if the constructor is extern *)
		(match PMap.find v.v_id !vars with
		| _,_,_,true,p ->
			display_error ctx "Extern constructor could not be inlined" p;
			error "Variable is used here" e.epos
		| _ -> ());
		vars := PMap.remove v.v_id !vars;
	in
	let rec skip_to_var e = match e.eexpr with
		| TLocal v when v.v_id < 0 -> Some v
		(* | TCast(e1,None) | TMeta(_,e1) | TParenthesis(e1) -> skip_to_var e1 *)
		| _ -> None
	in
	let rec find_locals e =
		match e.eexpr with
		| TVar (v,eo) ->
			Type.iter find_locals e;
			begin match eo with
				| Some n ->
					begin match get_inline_ctor_info n with
					| IKCtor (f,cst,c,tl,pl,el_init) when type_iseq v.v_type n.etype ->
						(* inline the constructor *)
						(match (try type_inline ctx cst f (mk (TLocal v) (TInst (c,tl)) n.epos) pl ctx.t.tvoid None n.epos true with Error (Custom _,_) -> None) with
						| None -> ()
						| Some ecst ->
							let assigns = ref [] in
							(* add field inits here because the filter has not run yet (issue #2336) *)
							List.iter (fun cf -> match cf.cf_kind,cf.cf_expr with
								| Var _,Some e -> assigns := (cf.cf_name,e,cf.cf_type) :: !assigns
								| _ -> ()
							) c.cl_ordered_fields;
							(* make sure we only have v.field = expr calls *)
							let rec get_assigns e =
								match e.eexpr with
								| TBlock el ->
									List.iter get_assigns el
								| TBinop (OpAssign, { eexpr = TField ({ eexpr = TLocal vv },FInstance(_,_,cf)); etype = t }, e) when v == vv ->
									assigns := (cf.cf_name,e,t) :: !assigns
								| _ ->
									raise Exit
							in
							try
								get_assigns ecst;
								(* mark variable as candidate for inlining *)
								vars := PMap.add v.v_id (v,el_init,List.rev !assigns,c.cl_extern || Meta.has Meta.Extern cst.cf_meta,n.epos) !vars;
								v.v_id <- -v.v_id; (* mark *)
								(* recurse with the constructor code which will be inlined here *)
								find_locals ecst
							with Exit ->
								())
					| IKArray (el,t) ->
						vars := PMap.add v.v_id (v,[],ExtList.List.mapi (fun i e -> string_of_int i,e,t) el, false, n.epos) !vars;
						v.v_id <- -v.v_id;
					| IKStructure fl ->
						vars := PMap.add v.v_id (v,[],List.map (fun (s,e) -> s,e,e.etype) fl, false, n.epos) !vars;
						v.v_id <- -v.v_id;
					| _ ->
						()
					end
				| _ -> ()
			end
		| TField(e1, (FInstance(_, _, {cf_kind = Var _; cf_name = s}) | FAnon({cf_kind = Var _; cf_name = s}))) ->
			(match skip_to_var e1 with None -> find_locals e1 | Some _ -> ())
		| TArray (e1,{eexpr = TConst (TInt i)}) ->
			begin match skip_to_var e1 with
				| None -> find_locals e1
				| Some v ->
					let (_,_,fields,_,_) = PMap.find (-v.v_id) !vars in
					let i = Int32.to_int i in
					if i < 0 || i >= List.length fields then cancel v
			end
		| TBinop((OpAssign | OpAssignOp _),e1,e2) ->
			begin match e1.eexpr with
	 			| TArray ({eexpr = TLocal v},{eexpr = TConst (TInt i)}) when v.v_id < 0 ->
					check_field v (Int32.to_string i) e2 e2.etype
				| TField({eexpr = TLocal v}, (FInstance(_, _, {cf_kind = Var _; cf_name = s}) | FAnon({cf_kind = Var _; cf_name = s}))) when v.v_id < 0 ->
					check_field v s e2 e2.etype
				| _ ->
					find_locals e1
			end;
			find_locals e2
		| TLocal v when v.v_id < 0 ->
			cancel v
		| _ ->
			Type.iter find_locals e
	in
	find_locals e;
	let vars = !vars in
	if PMap.is_empty vars then
		e
	else begin
		let vfields = PMap.map (fun (v,el_init,assigns,_,_) ->
			(List.fold_left (fun (acc,map) (name,e,t) ->
				let vf = alloc_var (v.v_name ^ "_" ^ name) t in
				((vf,e) :: acc, PMap.add name vf map)
			) ([],PMap.empty) assigns),el_init
		) vars in
		let el_b = ref [] in
		let append e = el_b := e :: !el_b in
		let inline_field c cf v =
			let (_, vars),el_init = PMap.find (-v.v_id) vfields in
			(try
				let v = PMap.find cf.cf_name vars in
				mk (TLocal v) v.v_type e.epos
			with Not_found ->
				if (c.cl_path = ([],"Array") && cf.cf_name = "length") then begin
					(* this can only occur for inlined array declarations, so we can use the statically known length here (issue #2568)*)
					let l = PMap.fold (fun _ i -> i + 1) vars 0 in
					mk (TConst (TInt (Int32.of_int l))) ctx.t.tint e.epos
				end else
					(* the variable was not set in the constructor, assume null *)
					mk (TConst TNull) e.etype e.epos)
		in
		let inline_anon_field cf v =
			let (_, vars),_ = PMap.find (-v.v_id) vfields in
			(try
				let v = PMap.find cf.cf_name vars in
				mk (TLocal v) v.v_type e.epos
			with Not_found ->
				(* this could happen in untyped code, assume null *)
				mk (TConst TNull) e.etype e.epos)
		in
		let inline_array_access i v =
			let (_, vars),_ = PMap.find (-v.v_id) vfields in
			(try
				let v = PMap.find (Int32.to_string i) vars in
				mk (TLocal v) v.v_type e.epos
			with Not_found ->
				(* probably out-of-bounds, assume null *)
				mk (TConst TNull) e.etype e.epos)
		in
		let rec subst e =
			match e.eexpr with
			| TBlock el ->
				let old = !el_b in
				el_b := [];
				List.iter (fun e -> append (subst e)) el;
				let n = !el_b in
				el_b := old;
				{e with eexpr = TBlock (List.rev n)}
			| TVar (v,Some e) when v.v_id < 0 ->
				let (vars, _),el_init = PMap.find (-v.v_id) vfields in
				List.iter (fun e ->
					append (subst e)
				) el_init;
				let (v_first,e_first),vars = match vars with
					| v :: vl -> v,vl
					| [] -> assert false
				in
				List.iter (fun (v,e) -> append (mk (TVar(v,Some (subst e))) ctx.t.tvoid e.epos)) (List.rev vars);
				mk (TVar (v_first, Some (subst e_first))) ctx.t.tvoid e.epos
			| TField (e1,FInstance (c,_,cf)) ->
				begin match skip_to_var e1 with
					| None -> Type.map_expr subst e
					| Some v -> inline_field c cf v
				end
			| TArray (e1,{eexpr = TConst (TInt i)}) ->
				begin match skip_to_var e1 with
					| None -> Type.map_expr subst e
					| Some v -> inline_array_access i v
				end
			| TField (e1,FAnon(cf)) ->
				begin match skip_to_var e1 with
					| None -> Type.map_expr subst e
					| Some v -> inline_anon_field cf v
				end
			| _ ->
				Type.map_expr subst e
		in
		let e = (try subst e with Not_found -> assert false) in
		PMap.iter (fun _ (v,_,_,_,_) -> v.v_id <- -v.v_id) vars;
		e
	end

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
			let vl = List.map (fun (v,t,e) ->
				let e = (match e with None -> None | Some e -> Some (loop e)) in
				decl v t e;
				(v,t,e)
			) vl in
			(EVars vl,p)
		| EBlock el ->
			let old = save() in
			let told = ref (!typing_side_effect) in
			let el = List.fold_left (fun acc e ->
				typing_side_effect := false;
				let e = loop e in
				if !typing_side_effect then begin told := true; e :: acc end else acc
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
			List.iter (fun (n,_,t,e) -> decl n t e) f.f_args;
			let e = map e in
			old();
			e
		| EFor ((EIn ((EConst (Ident n),_) as id,it),p),efor) ->
			let it = loop it in
			let old = save() in
			let etmp = (EConst (Ident "$tmp"),p) in
			decl n None (Some (EBlock [
				(EVars ["$tmp",None,None],p);
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
			let cases = List.map (fun (el,eg,eo) -> match eo with
				| None ->
					el,eg,eo
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
					el, eg, Some e
			) cases in
			let def = match def with
				| None -> None
				| Some None -> Some None
				| Some (Some e) -> Some (Some (loop e))
			in
			(ESwitch (e,cases,def),p)
		| ETry (et,cl) ->
			let et = loop et in
			let cl = List.map (fun (n,t,e) ->
				let old = save() in
				decl n (Some t) None;
				let e = loop e in
				old();
				n, t, e
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
						| Some t , _ -> (ECheckType ((EConst (Ident "null"),p),t),p)
						| _, Some (id,e,lc) ->
							let name = (try
								PMap.find id (!tmp_hlocals)
							with Not_found ->
								let e = subst_locals lc e in
								let name = "$tmp_" ^ string_of_int id in
								tmp_locals := (name,None,Some e) :: !tmp_locals;
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
					Ast.map_expr (subst_locals { r = PMap.foldi (fun n i acc -> if List.exists (fun (a,_,_,_) -> a = n) f.f_args then acc else PMap.add n i acc) locals.r PMap.empty }) e
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
