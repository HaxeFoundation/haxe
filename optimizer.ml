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

let type_inline ctx cf f ethis params tret p =
	let locals = save_locals ctx in
	let hcount = Hashtbl.create 0 in
	let lsets = Hashtbl.create 0 in
	let pnames = List.map (fun (name,_,t) ->
		let name = add_local ctx name t in
		Hashtbl.add hcount name (ref 0);
		(name,t)
	) f.tf_args in
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
	(* use default values for null/unset arguments *)
	let rec loop pl al =
		match pl, al with
		| [], [] -> []
		| e :: pl, (name, opt, t) :: al ->
			if is_nullable t && is_null e.etype then Hashtbl.add lsets name (); (* force coerce *)
			(match e.eexpr, opt with
			| TConst TNull , Some c -> mk (TConst c) (map_type t) e.epos
			| _ -> e) :: loop pl al
		| [], (_,opt,t) :: al ->
			(match opt with
			| None -> assert false
			| Some c -> mk (TConst c) (map_type t) p) :: loop [] al
		| _ :: _, [] ->
			assert false
	in
	let params = loop params f.tf_args in
	let ethis = (match ethis.eexpr with TConst TSuper -> { ethis with eexpr = TConst TThis } | _ -> ethis) in
	let vthis = gen_local ctx ethis.etype in
	let this_count = ref 0 in
	let local i =
		let i = (try PMap.find i ctx.locals_map with Not_found -> i) in
		(try incr (Hashtbl.find hcount i) with Not_found -> ());
		i
	in
	let opt f = function
		| None -> None
		| Some e -> Some (f e)
	in
	let has_vars = ref false in
	(*
		here, we try to eliminate final returns from the expression tree.
		However, this is not entirely correct since we don't yet correctly propagate
		the type of returned expressions upwards ("return" expr itself being Dynamic)
	*)
	let rec map term e =
		let e = { e with epos = p } in
		match e.eexpr with
		| TLocal s ->
			{ e with eexpr = TLocal (local s) }
		| TConst TThis ->
			incr this_count;
			{ e with eexpr = TLocal vthis }
		| TVars vl ->
			has_vars := true;
			let vl = List.map (fun (v,t,e) ->
				let e = opt (map false) e in
				add_local ctx v t,t,e
			) vl in
			{ e with eexpr = TVars vl }
		| TReturn eo ->
			if not term then error "Cannot inline a not final return" e.epos;
			(match eo with
			| None -> mk (TConst TNull) (mk_mono()) p
			| Some e -> map term e)
		| TFor (v,t,e1,e2) ->
			let e1 = map false e1 in
			let old = save_locals ctx in
			let v = add_local ctx v t in
			let e2 = map false e2 in
			old();
			{ e with eexpr = TFor (v,t,e1,e2) }
		| TMatch (e,en,cases,def) ->
			let term, t = (match def with Some d when term -> true, ref d.etype | _ -> false, ref e.etype) in
			let cases = List.map (fun (i,vl,e) ->
				let old = save_locals ctx in
				let vl = opt (List.map (fun (n,t) -> opt (fun n -> add_local ctx n t) n, t)) vl in
				let e = map term e in
				if is_null e.etype then t := e.etype;
				old();
				i, vl, e
			) cases in
			{ e with eexpr = TMatch (map false e,en,cases,opt (map term) def); etype = !t }
		| TTry (e1,catches) ->
			{ e with eexpr = TTry (map term e1,List.map (fun (v,t,e) ->
				let old = save_locals ctx in
				let v = add_local ctx v t in
				let e = map term e in
				old();
				v,t,e
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
		| TUnop (op,pref,({ eexpr = TLocal s } as e1)) ->
			(match op with
			| Increment | Decrement -> Hashtbl.add lsets (local s) ()
			| _ -> ());
			{ e with eexpr = TUnop (op,pref,map false e1) }
		| TBinop (op,({ eexpr = TLocal s } as e1),e2) ->
			(match op with
			| OpAssign | OpAssignOp _ -> Hashtbl.add lsets (local s) ()
			| _ -> ());
			{ e with eexpr = TBinop (op,map false e1,map false e2) }
		| TConst TSuper ->
			error "Cannot inline function containing super" e.epos
		| TFunction _ ->
			error "Cannot inline functions containing closures" p
		| _ ->
			Type.map_expr (map false) e
	in
	let e = map true f.tf_expr in
	locals();
	let subst = ref PMap.empty in
	Hashtbl.add hcount vthis this_count;
	let vars = List.map2 (fun (n,t) e ->
		let flag = not (Hashtbl.mem lsets n) && (match e.eexpr with
			| TLocal _ | TConst _ | TFunction _ -> true
			| _ ->
				let used = !(Hashtbl.find hcount n) in
				used <= 1
		) in
		(n,t,e,flag)
	) ((vthis,ethis.etype) :: pnames) (ethis :: params) in
	let vars = List.fold_left (fun acc (n,t,e,flag) ->
		if flag then begin
			subst := PMap.add n e !subst;
			acc
		end else
			(n,t,Some e) :: acc
	) [] vars in
	let subst = !subst in
	let rec inline_params e =
		match e.eexpr with
		| TLocal s -> (try PMap.find s subst with Not_found -> e)
		| _ -> Type.map_expr inline_params e
	in
	let e = (if PMap.is_empty subst then e else inline_params e) in
	let init = (match vars with [] -> None | l -> Some (mk (TVars (List.rev l)) ctx.t.tvoid p)) in
	if Common.defined ctx.com "js" && (init <> None || !has_vars) then
		None
	else
		let wrap e =
			(* we can't mute the type of the expression because it is not correct to do so *)
			if e.etype == tret then
				e
			else
				mk (TParenthesis e) tret e.epos
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
			let rec map_expr_type e = Type.map_expr_type map_expr_type map_type e in
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
			| TUnop (Decrement,_,{ eexpr = TLocal l })  when l = i ->
				error "Loop variable cannot be modified" e.epos
			| TFunction f when List.exists (fun (l,_,_) -> l = i) f.tf_args ->
				e
			| TFor (k,_,_,_) when k = i ->
				e
			| _ ->
				Type.map_expr check e
		in
		let e2 = check (type_expr ctx e2 false) in
		let etmp = mk (TLocal tmp) t_int p in
		let incr = mk (TUnop (Increment,Postfix,etmp)) t_int p in
		let init = mk (TVars [i,t_int,Some incr]) t_void p in
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
				mk (TVars [tmp,t_int,Some i1]) t_void p;
				mk (TWhile (
					mk (TBinop (OpLt, etmp, { i2 with etype = t_int })) ctx.t.tbool p,
					block,
					NormalWhile
				)) t_void p;
			]
		| Some max ->
			lblock [
				mk (TVars [tmp,t_int,Some i1;max,t_int,Some i2]) t_void p;
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
				mk (TLocal atmp) e1.etype e1.epos, [atmp,e1.etype,Some e1]
		) in
		let iexpr = mk (TLocal index) t_int p in
		let e2 = type_expr ctx e2 false in
		let aget = mk (TVars [i,pt,Some (mk (TArray (arr,iexpr)) pt p)]) t_void p in
		let incr = mk (TUnop (Increment,Prefix,iexpr)) t_int p in
		let block = match e2.eexpr with
			| TBlock el -> mk (TBlock (aget :: incr :: el)) t_void e2.epos
			| _ -> mk (TBlock [aget;incr;e2]) t_void p
		in
		let ivar = index, t_int, Some (mk (TConst (TInt 0l)) t_int p) in
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
		let evar = mk (TVars [i,t,Some (mk (TField (cexpr,"elt")) t p)]) t_void p in
		let enext = mk (TBinop (OpAssign,cexpr,mk (TField (cexpr,"next")) tcell p)) tcell p in
		let block = match e2.eexpr with
			| TBlock el -> mk (TBlock (evar :: enext :: el)) t_void e2.epos
			| _ -> mk (TBlock [evar;enext;e2]) t_void p
		in
		lblock [
			mk (TVars [cell,tcell,Some (mk (TField (e1,"head")) tcell p)]) t_void p;
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

let sanitize_expr e =
	let parent e =
		mk (TParenthesis e) e.etype e.epos
	in
	let block e =
		mk (TBlock [e]) e.etype e.epos
	in
	let need_parent e =
		match e.eexpr with
		| TConst _ | TLocal _ | TEnumField _ | TArray _ | TField _ | TParenthesis _ | TCall _ | TClosure _ | TNew _ | TTypeExpr _ | TObjectDecl _ | TArrayDecl _ -> false
		| TCast _ | TThrow _ | TReturn _ | TTry _ | TMatch _ | TSwitch _ | TFor _ | TIf _ | TWhile _ | TBinop _ | TContinue | TBreak
		| TBlock _ | TVars _ | TFunction _ | TUnop _ -> true
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
		let e1 = (match e1.eexpr with
			| TParenthesis _ -> e1
			| _ -> parent e1
		) in
		let e2 = (match e2.eexpr, eelse with
			| TIf (_,_,Some _) , _ | TIf (_,_,None), Some _ -> block e2
			| _ -> e2
		) in
		{ e with eexpr = TIf (e1,e2,eelse) }
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
	| TBlock [{ eexpr = TConst _ } as ec] ->
		{ ec with epos = e.epos }
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
		| TConst (TInt 0l) , _ when op = OpAdd -> e2
		| TConst (TInt 1l) , _ when op = OpMult -> e2
		| TConst (TFloat v) , _ when op = OpAdd && float_of_string v = 0. && is_float e2.etype -> e2
		| TConst (TFloat v) , _ when op = OpMult && float_of_string v = 1. && is_float e2.etype -> e2
		| _ , TConst (TInt 0l) when (match op with OpAdd | OpSub | OpShr | OpShl -> true | _ -> false) -> e1 (* bits operations might cause overflow *)
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
				{ e with eexpr = TConst (TBool (t (compare b a))) }
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
		(match follow ef.etype with
		| TFun (_,rt) ->
			let cf = { cf_name = ""; cf_params = []; cf_type = ef.etype; cf_public = true; cf_doc = None; cf_meta = no_meta; cf_kind = Var { v_read = AccNormal; v_write = AccNo }; cf_expr = None } in
			let inl = (try type_inline ctx cf func (mk (TConst TNull) (mk_mono()) e.epos) el rt e.epos with Error (Custom _,_) -> None) in
			(match inl with
			| None -> e
			| Some e -> e)
		| _ ->
			e)
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
