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

(* ---------------------------------------------------------------------- *)
(* INLINING *)

type in_local = {
	i_var : tvar;
	i_subst : tvar;
	mutable i_write : bool;
	mutable i_read : int;
}

let rec type_inline ctx cf f ethis params tret p =
	(* type substitution on both class and function type parameters *)
	let has_params, map_type =
		let rec get_params c pl =
			match c.cl_super with
			| None -> c.cl_types, pl
			| Some (csup,spl) ->
				let spl = (match apply_params c.cl_types pl (TInst (csup,spl)) with
				| TInst (_,pl) -> pl
				| _ -> assert false
				) in
				let ct, cpl = get_params csup spl in
				c.cl_types @ ct, pl @ cpl
		in
		let tparams = (match follow ethis.etype with TInst (c,pl) -> get_params c pl | _ -> ([],[])) in
		let pmonos = List.map (fun _ -> mk_mono()) cf.cf_params in
		let tmonos = snd tparams @ pmonos in
		let tparams = fst tparams @ cf.cf_params in
		tparams <> [], apply_params tparams tmonos
	in
	(* locals substitution *)
	let locals = Hashtbl.create 0 in
	let local v =
		try
			Hashtbl.find locals v.v_id
		with Not_found ->
			let i = {
				i_var = v;
				i_subst = alloc_var v.v_name v.v_type;
				i_write = false;
				i_read = 0;
			} in
			Hashtbl.add locals v.v_id i;
			Hashtbl.add locals i.i_subst.v_id i;
			i
	in
	let read_local v =
		try
			Hashtbl.find locals v.v_id
		with Not_found ->
			{
				i_var = v;
				i_subst = v;
				i_write = false;
				i_read = 0;
			}
	in
	(* use default values for null/unset arguments *)
	let rec loop pl al =
		match pl, al with
		| [], [] -> []
		| e :: pl, (v, opt) :: al ->
			(*
				if we pass a Null<T> var to an inlined method that needs a T.
				we need to force a local var to be created on some platforms.
			*)
			if is_nullable v.v_type && is_null e.etype && (match ctx.com.platform with Flash9 | Cpp -> true | _ -> false) then (local v).i_write <- true;
			(match e.eexpr, opt with
			| TConst TNull , Some c -> mk (TConst c) v.v_type e.epos
			| _ -> e) :: loop pl al
		| [], (v,opt) :: al ->
			(match opt with
			| None -> assert false
			| Some c -> mk (TConst c) v.v_type p) :: loop [] al
		| _ :: _, [] ->
			assert false
	in
	(*
		Build the expr/var subst list
	*)
	let ethis = (match ethis.eexpr with TConst TSuper -> { ethis with eexpr = TConst TThis } | _ -> ethis) in
	let vthis = alloc_var "_this" ethis.etype in
	let inlined_vars = List.map2 (fun e (v,_) -> local v, e) (ethis :: loop params f.tf_args) ((vthis,None) :: f.tf_args) in
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
	let rec map term e =
		let po = e.epos in
		let e = { e with epos = p } in
		match e.eexpr with
		| TLocal v ->
			let l = read_local v in
			l.i_read <- l.i_read + (if !in_loop then 2 else 1);
			{ e with eexpr = TLocal l.i_subst }
		| TConst TThis ->
			let l = read_local vthis in
			l.i_read <- l.i_read + (if !in_loop then 2 else 1);
			{ e with eexpr = TLocal l.i_subst }
		| TVars vl ->
			has_vars := true;
			let vl = List.map (fun (v,e) ->
				(local v).i_subst,opt (map false) e
			) vl in
			{ e with eexpr = TVars vl }
		| TReturn eo ->
			if not term then error "Cannot inline a not final return" po;
			(match eo with
			| None -> mk (TConst TNull) f.tf_type p
			| Some e -> map term e)
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
		| TMatch (v,en,cases,def) ->
			let term, t = (match def with Some d when term -> true, ref d.etype | _ -> false, ref e.etype) in
			let cases = List.map (fun (i,vl,e) ->
				let vl = opt (List.map (fun v -> opt (fun v -> (local v).i_subst) v)) vl in
				let e = map term e in
				if is_null e.etype then t := e.etype;
				i, vl, e
			) cases in
			{ e with eexpr = TMatch (map false v,en,cases,opt (map term) def); etype = !t }
		| TTry (e1,catches) ->
			{ e with eexpr = TTry (map term e1,List.map (fun (v,e) ->
				let e = map term e in
				(local v).i_subst,e
			) catches) }
		| TBlock l ->
			let old = save_locals ctx in
			let t = ref e.etype in
			let rec loop = function
				| [] when term ->
					t := mk_mono();
					[mk (TConst TNull) (!t) p]
				| [] -> []
				| [e] ->
					let e = map term e in
					if term then t := e.etype;
					[e]
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
			{ e with eexpr = TIf(econd,eif,Some eelse); etype = if is_null eif.etype then eif.etype else eelse.etype }
		| TParenthesis _ | TIf (_,_,Some _) | TSwitch (_,_,Some _) ->
			Type.map_expr (map term) e
		| TUnop ((Increment|Decrement),_,{ eexpr = TLocal v }) ->
			(read_local v).i_write <- true;
			Type.map_expr (map false) e
		| TBinop ((OpAssign | OpAssignOp _),{ eexpr = TLocal v },_) ->
			(read_local v).i_write <- true;
			Type.map_expr (map false) e;
		| TConst TSuper ->
			error "Cannot inline function containing super" po
		| TFunction _ ->
			error "Cannot inline functions containing closures" po
		| _ ->
			Type.map_expr (map false) e
	in
	let e = map true f.tf_expr in
	(*
		if variables are not written and used with a const value, let's substitute
		with the actual value, either create a temp var
	*)
	let subst = ref PMap.empty in
	let vars = List.fold_left (fun acc (i,e) ->
		let flag = (match e.eexpr with
			| TLocal _ | TConst _ -> not i.i_write
			| TFunction _ -> if i.i_write then error "Cannot modify a closure parameter inside inline method" p; true
			| _ -> not i.i_write && i.i_read <= 1
		) in
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
	let init = (match vars with [] -> None | l -> Some (mk (TVars (List.rev l)) ctx.t.tvoid p)) in
	(*
		If we have local variables and returning a value, then this will result in
		unoptimized JS code, so let's instead skip inlining.

		This could be fixed with better post process code cleanup (planed)
	*)
	if Common.platform ctx.com Js && (init <> None || !has_vars) then begin
		None
	end else
		let wrap e =
			(* we can't mute the type of the expression because it is not correct to do so *)
			if e.etype == tret then
				e
			else
				mk (TCast (e,None)) tret e.epos
		in
		let e = (match e.eexpr, init with
			| TBlock [e] , None -> wrap e
			| _ , None -> wrap e
			| TBlock l, Some init -> mk (TBlock (init :: l)) tret e.epos
			| _, Some init -> mk (TBlock [init;e]) tret e.epos
		) in
		(* we need to replace type-parameters that were used in the expression *)
		if not has_params then
			Some e
		else
			let mt = map_type cf.cf_type in
			unify_raise ctx mt (TFun (List.map (fun e -> "",false,e.etype) params,tret)) p;
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

let optimize_for_loop ctx i e1 e2 p =
	let t_void = ctx.t.tvoid in
	let t_int = ctx.t.tint in
	let lblock el = Some (mk (TBlock el) t_void p) in
	match e1.eexpr, follow e1.etype with
	| TNew ({ cl_path = ([],"IntIter") },[],[i1;i2]) , _ ->
		let max = (match i1.eexpr , i2.eexpr with
			| TConst (TInt a), TConst (TInt b) when Int32.compare b a < 0 -> error "Range operate can't iterate backwards" p
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
		let e2 = type_expr ctx e2 false in
		check e2;
		let etmp = mk (TLocal tmp) t_int p in
		let incr = mk (TUnop (Increment,Postfix,etmp)) t_int p in
		let init = mk (TVars [i,Some incr]) t_void p in
		let block = match e2.eexpr with
			| TBlock el -> mk (TBlock (init :: el)) t_void e2.epos
			| _ -> mk (TBlock [init;e2]) t_void p
		in
		(*
			force locals to be of Int type (to prevent Int/UInt issues)
		*)
		(match max with
		| None ->
			lblock [
				mk (TVars [tmp,Some i1]) t_void p;
				mk (TWhile (
					mk (TBinop (OpLt, etmp, { i2 with etype = t_int })) ctx.t.tbool p,
					block,
					NormalWhile
				)) t_void p;
			]
		| Some max ->
			lblock [
				mk (TVars [tmp,Some i1;max,Some i2]) t_void p;
				mk (TWhile (
					mk (TBinop (OpLt, etmp, mk (TLocal max) t_int p)) ctx.t.tbool p,
					block,
					NormalWhile
				)) t_void p;
			])
	| _ , TInst({ cl_path = [],"Array" },[pt])
	| _ , TInst({ cl_path = ["flash"],"Vector" },[pt]) ->
		let i = add_local ctx i pt in
		let index = gen_local ctx t_int in
		let arr, avars = (match e1.eexpr with
			| TLocal _ -> e1, []
			| _ ->
				let atmp = gen_local ctx e1.etype in
				mk (TLocal atmp) e1.etype e1.epos, [atmp,Some e1]
		) in
		let iexpr = mk (TLocal index) t_int p in
		let e2 = type_expr ctx e2 false in
		let aget = mk (TVars [i,Some (mk (TArray (arr,iexpr)) pt p)]) t_void p in
		let incr = mk (TUnop (Increment,Prefix,iexpr)) t_int p in
		let block = match e2.eexpr with
			| TBlock el -> mk (TBlock (aget :: incr :: el)) t_void e2.epos
			| _ -> mk (TBlock [aget;incr;e2]) t_void p
		in
		let ivar = index, Some (mk (TConst (TInt 0l)) t_int p) in
		lblock [
			mk (TVars (ivar :: avars)) t_void p;
			mk (TWhile (
				mk (TBinop (OpLt, iexpr, mk (TField (arr,"length")) t_int p)) ctx.t.tbool p,
				block,
				NormalWhile
			)) t_void p;
		]
	| _ , TInst ({ cl_kind = KGenericInstance ({ cl_path = ["haxe"],"FastList" },[t]) } as c,[]) ->
		let tcell = (try (PMap.find "head" c.cl_fields).cf_type with Not_found -> assert false) in
		let i = add_local ctx i t in
		let cell = gen_local ctx tcell in
		let cexpr = mk (TLocal cell) tcell p in
		let e2 = type_expr ctx e2 false in
		let evar = mk (TVars [i,Some (mk (TField (cexpr,"elt")) t p)]) t_void p in
		let enext = mk (TBinop (OpAssign,cexpr,mk (TField (cexpr,"next")) tcell p)) tcell p in
		let block = match e2.eexpr with
			| TBlock el -> mk (TBlock (evar :: enext :: el)) t_void e2.epos
			| _ -> mk (TBlock [evar;enext;e2]) t_void p
		in
		lblock [
			mk (TVars [cell,Some (mk (TField (e1,"head")) tcell p)]) t_void p;
			mk (TWhile (
				mk (TBinop (OpNotEq, cexpr, mk (TConst TNull) tcell p)) ctx.t.tbool p,
				block,
				NormalWhile
			)) t_void p
		]
	| _ ->
		None

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
	| OpAssignOp OpAssign -> 16, right (* mimics ?: *)
	| OpAssign | OpAssignOp _ -> 17, right

let rec need_parent e =
	match e.eexpr with
	| TConst _ | TLocal _ | TEnumField _ | TArray _ | TField _ | TParenthesis _ | TCall _ | TClosure _ | TNew _ | TTypeExpr _ | TObjectDecl _ | TArrayDecl _ -> false
	| TCast (e,None) -> need_parent e
	| TCast _ | TThrow _ | TReturn _ | TTry _ | TMatch _ | TSwitch _ | TFor _ | TIf _ | TWhile _ | TBinop _ | TContinue | TBreak
	| TBlock _ | TVars _ | TFunction _ | TUnop _ -> true

let sanitize_expr e =
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
		| TVars _	(* needs to be put into blocks *)
		| TFor _	(* a temp var is needed for holding iterator *)
		| TMatch _	(* a temp var is needed for holding enum *)
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
	| TUnop (op,mode,e2) ->
		let rec loop ee =
			match ee.eexpr with
			| TBinop _ -> parent e2
			| TCast (e,None) -> loop e
			| _ -> e2
		in
		{ e with eexpr = TUnop (op,mode,loop e2) }
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
		(match f.tf_expr.eexpr with
		| TBlock _ -> e
		| _ -> { e with eexpr = TFunction { f with tf_expr = block f.tf_expr } })
	| TCall (e2,args) ->
		if need_parent e2 then { e with eexpr = TCall(parent e2,args) } else e
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
	| TMatch (e1, en, cases, def) ->
		let e1 = parent e1 in
		let cases = List.map (fun (el,vars,e) -> el, vars, complex e) cases in
		let def = (match def with None -> None | Some e -> Some (complex e)) in
		{ e with eexpr = TMatch (e1,en,cases,def) }
	| _ ->
		e

let reduce_expr ctx e =
	match e.eexpr with
	| TSwitch (_,cases,_) ->
		List.iter (fun (cl,_) ->
			List.iter (fun e ->
				match e.eexpr with
				| TCall ({ eexpr = TEnumField _ },_) -> error "Not-constant enum in switch cannot be matched" e.epos
				| _ -> ()
			) cl
		) cases;
		e
	| TBlock l ->
		(match List.rev l with
		| [] -> e
		| ec :: l ->
			(* remove all no-ops : not-final constants in blocks *)
			match List.filter (fun e -> match e.eexpr with TConst _ -> false | _ -> true) l with
			| [] -> { ec with epos = e.epos }
			| l -> { e with eexpr = TBlock (List.rev (ec :: l)) })
	| TParenthesis ec ->
		{ ec with epos = e.epos }
	| _ ->
		e

let rec sanitize ctx e =
	sanitize_expr (reduce_expr ctx (Type.map_expr (sanitize ctx) e))

(* ---------------------------------------------------------------------- *)
(* REDUCE *)

let rec reduce_loop ctx e =
	let is_float t =
		match follow t with
		| TInst ({ cl_path = ([],"Float") },_) -> true
		| _ -> false
	in
	let is_numeric t =
		match follow t with
		| TInst ({ cl_path = ([],("Float" | "Int")) },_) -> true
		| _ -> false
	in
	let e = Type.map_expr (reduce_loop ctx) e in
	sanitize_expr (match e.eexpr with
	| TIf ({ eexpr = TConst (TBool t) },e1,e2) ->
		(if t then e1 else match e2 with None -> { e with eexpr = TBlock [] } | Some e -> e)
	| TWhile ({ eexpr = TConst (TBool false) },sub,flag) ->
		(match flag with
		| NormalWhile -> { e with eexpr = TBlock [] } (* erase sub *)
		| DoWhile -> e) (* we cant remove while since sub can contain continue/break *)
	| TBinop (op,e1,e2) ->
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
				{ e with eexpr = TConst (TBool (t (Int32.compare b a))) }
			in
			(match op with
			| OpAdd -> check_overflow Int64.add
			| OpSub -> check_overflow Int64.sub
			| OpMult -> check_overflow Int64.mul
			| OpAnd -> opt Int32.logand
			| OpOr -> opt Int32.logor
			| OpXor -> opt Int32.logxor
			| OpShl -> opt (fun a b -> Int32.shift_left a (Int32.to_int b))
			| OpShr -> opt (fun a b -> Int32.shift_right a (Int32.to_int b))
			| OpUShr -> opt (fun a b -> Int32.shift_right_logical a (Int32.to_int b))
			| OpEq -> ebool ((=) 0)
			| OpNotEq -> ebool ((<>) 0)
			| OpGt -> ebool ((>) 0)
			| OpGte -> ebool ((>=) 0)
			| OpLt -> ebool ((<) 0)
			| OpLte -> ebool ((<=) 0)
			| _ -> e)
		| TConst (TFloat a), TConst (TFloat b) ->
			let fop f =
				let v = f (float_of_string a) (float_of_string b) in
				let vstr = string_of_float v in
				if v = float_of_string vstr then
					{ e with eexpr = TConst (TFloat vstr) }
				else
					e
			in
			let ebool t =
				{ e with eexpr = TConst (TBool (t (compare (float_of_string b) (float_of_string a)))) }
			in
			(match op with
			| OpAdd -> fop (+.)
			| OpSub -> fop (-.)
			| OpMult -> fop ( *. )
			| OpEq -> ebool ((=) 0)
			| OpNotEq -> ebool ((<>) 0)
			| OpGt -> ebool ((>) 0)
			| OpGte -> ebool ((>=) 0)
			| OpLt -> ebool ((<) 0)
			| OpLte -> ebool ((<=) 0)
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
		| TEnumField (e1,f1), TEnumField (e2,f2) when e1 == e2 ->
			(match op with
			| OpEq -> { e with eexpr = TConst (TBool (f1 = f2)) }
			| OpNotEq -> { e with eexpr = TConst (TBool (f1 <> f2)) }
			| _ -> e)
		| _, TCall ({ eexpr = TEnumField _ },_) | TCall ({ eexpr = TEnumField _ },_), _ ->
			(match op with
			| OpAssign -> e
			| _ ->
				error "You cannot directly compare enums with arguments. Use either 'switch' or 'Type.enumEq'" e.epos)
		| _ ->
			e)
	| TUnop (op,flag,esub) ->
		(match op, esub.eexpr with
		| Not, TConst (TBool f) -> { e with eexpr = TConst (TBool (not f)) }
		| Neg, TConst (TInt i) -> { e with eexpr = TConst (TInt (Int32.neg i)) }
		| NegBits, TConst (TInt i) -> { e with eexpr = TConst (TInt (Int32.lognot i)) }
		| Neg, TConst (TFloat f) ->
			let v = 0. -. float_of_string f in
			let vstr = string_of_float v in
			if float_of_string vstr = v then
				{ e with eexpr = TConst (TFloat vstr) }
			else
				e
		| _ -> e
		)
	| TCall ({ eexpr = TFunction func } as ef,el) ->
		let cf = mk_field "" ef.etype e.epos in
		let ethis = mk (TConst TThis) t_dynamic e.epos in
		let rt = (match follow ef.etype with TFun (_,rt) -> rt | _ -> assert false) in
		let inl = (try type_inline ctx cf func ethis el rt e.epos with Error (Custom _,_) -> None) in
		(match inl with
		| None -> reduce_expr ctx e
		| Some e -> reduce_loop ctx e)
	| _ ->
		reduce_expr ctx e)

let reduce_expression ctx e =
	if ctx.com.foptimize then reduce_loop ctx e else e

(* ---------------------------------------------------------------------- *)
(* ELIMINATE DEAD CODE *)

(*
	if dead code elimination is on, any class without fields is eliminated from the output.
*)

let filter_dead_code com =
	let s_class c = s_type_path c.cl_path in
	com.types <- List.filter (fun t ->
		match t with
		| TClassDecl c ->
			if (c.cl_extern or has_meta ":keep" c.cl_meta) then
				true
			else (
				match (c.cl_ordered_statics, c.cl_ordered_fields, c.cl_constructor) with
				| ([], [], None) ->
					if com.verbose then print_endline ("Remove class " ^ s_class c);
					false
				| _ ->
					true)
		| _ ->
			true
	) com.types
