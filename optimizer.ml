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
	(* use default values for null/unset arguments *)
	let rec loop pl al =
		match pl, al with
		| [], [] -> []
		| e :: pl, (name, opt, t) :: al ->
			if is_nullable t && is_null e.etype then Hashtbl.add lsets name (); (* force coerce *)
			(match e.eexpr, opt with
			| TConst TNull , Some c -> mk (TConst c) t e.epos
			| _ -> e) :: loop pl al
		| [], (_,opt,t) :: al ->
			(match opt with
			| None -> assert false
			| Some c -> mk (TConst c) t p) :: loop [] al
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
			{ e with eexpr = TFor (local v,t,map false e1,map false e2) }
		| TMatch (e,en,cases,def) ->
			let term, t = (match def with Some d when term -> true, d.etype | _ -> false, e.etype) in
			let cases = List.map (fun (i,vl,e) ->
				i, opt (List.map (fun (n,t) -> opt local n, t)) vl, map term e
			) cases in
			{ e with eexpr = TMatch (map false e,en,cases,opt (map term) def); etype = t }
		| TTry (e1,catches) ->
			{ e with eexpr = TTry (map term e1,List.map (fun (v,t,e) -> local v,t,map term e) catches) }
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
			{ e with eexpr = TIf(econd,eif,Some eelse); etype = eif.etype }
		| TParenthesis _ | TIf (_,_,Some _) | TSwitch (_,_,Some _) ->
			Type.map_expr (map term) e
		| TUnop (op,pref,({ eexpr = TLocal s } as e1)) ->
			(match op with
			| Increment | Decrement -> Hashtbl.add lsets s ()
			| _ -> ());
			{ e with eexpr = TUnop (op,pref,map false e1) }
		| TBinop (op,({ eexpr = TLocal s } as e1),e2) ->
			(match op with
			| OpAssign | OpAssignOp _ -> Hashtbl.add lsets s ()
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
			| TLocal _ | TConst _ -> true
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
	let init = (match vars with [] -> None | l -> Some (mk (TVars (List.rev l)) ctx.api.tvoid p)) in
	if Common.defined ctx.com "js" && (init <> None || !has_vars) then
		None
	else
		let e = (match e.eexpr, init with
			| TBlock [e] , None -> { e with etype = tret; }
			| _ , None -> { e with etype = tret; }
			| TBlock l, Some init -> mk (TBlock (init :: l)) tret e.epos
			| _, Some init -> mk (TBlock [init;e]) tret e.epos
		) in
		(* we need to replace type-parameters that were used in the expression *)
		let tparams = (match follow ethis.etype with TInst (c,pl) -> (c.cl_types,pl) | _ -> ([],[])) in
		match cf.cf_params, tparams with
		| [], ([],_) -> Some e
		| _ ->
			let pmonos = List.map (fun _ -> mk_mono()) cf.cf_params in
			let tmonos = snd tparams @ pmonos in
			let tparams = fst tparams @ cf.cf_params in
			let mt = apply_params tparams tmonos cf.cf_type in
			unify_raise ctx mt (TFun (List.map (fun e -> "",false,e.etype) params,tret)) p;
			(*
				this is very expensive since we are building the substitution list for
				every expression, but hopefully in such cases the expression size is small
			*)
			let map_type t = apply_params tparams tmonos t in
			let rec map_expr_type e = Type.map_expr_type map_expr_type map_type e in
			Some (map_expr_type e)

(* ---------------------------------------------------------------------- *)
(* LOOPS *)

let optimize_for_loop ctx i e1 e2 p =
	let t_void = ctx.api.tvoid in
	let t_int = ctx.api.tint in
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
		(match max with
		| None ->
			lblock [
				mk (TVars [tmp,i1.etype,Some i1]) t_void p;
				mk (TWhile (
					mk (TBinop (OpLt, etmp, i2)) ctx.api.tbool p,
					block,
					NormalWhile
				)) t_void p;
			]
		| Some max ->
			lblock [
				mk (TVars [tmp,i1.etype,Some i1;max,i2.etype,Some i2]) t_void p;
				mk (TWhile (
					mk (TBinop (OpLt, etmp, mk (TLocal max) i2.etype p)) ctx.api.tbool p,
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
				mk (TBinop (OpLt, iexpr, mk (TField (arr,"length")) t_int p)) ctx.api.tbool p,
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
				mk (TBinop (OpNotEq, cexpr, mk (TConst TNull) tcell p)) ctx.api.tbool p,
				block,
				NormalWhile
			)) t_void p
		]
	| _ ->
		None

(* ---------------------------------------------------------------------- *)
(* REDUCE *)

let rec reduce_loop ctx is_sub e =
	let is_float t =
		match follow t with
		| TInst ({ cl_path = ([],"Float") },_) -> true
		| _ -> false
	in
	let e = Type.map_expr (reduce_loop ctx (match e.eexpr with TBlock _ -> false | _ -> true)) e in
	match e.eexpr with
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
		| _ -> e)
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
			let cf = { cf_name = ""; cf_params = []; cf_type = ef.etype; cf_public = true; cf_doc = None; cf_get = NormalAccess; cf_set = NoAccess; cf_expr = None } in
			let inl = (try type_inline ctx cf func (mk (TConst TNull) (mk_mono()) e.epos) el rt e.epos with Error (Custom _,_) -> None) in
			(match inl with
			| None -> e
			| Some e -> e)
		| _ -> 
			e)
	| TParenthesis ({ eexpr = TConst _ } as ec) | TBlock [{ eexpr = TConst _ } as ec] ->
		{ ec with epos = e.epos }
	| TSwitch (_,cases,_) ->
		List.iter (fun (cl,_) ->
			List.iter (fun e ->
				match e.eexpr with
				| TCall ({ eexpr = TEnumField _ },_) -> error "Not-constant enum in switch cannot be matched" e.epos
				| _ -> ()
			) cl
		) cases;
		e
	| _ ->
		e

let reduce_expression ctx e =
	if ctx.com.foptimize then reduce_loop ctx false e else e

(* ---------------------------------------------------------------------- *)
(* SANITIZE *)

(*
	makes sure that when an AST get generated to source code, it will not
	generate expressions that evaluate differently. It is then necessary to
	add parenthesises around some binary expressions when the AST does not
	correspond to the natural operand priority order for the platform
*)

let rec sanitize e =	
	match e.eexpr with
	| TBinop (op,e1,e2) ->
		let parent e = mk (TParenthesis e) e.etype e.epos in
		let e1 = sanitize e1 in
		let e2 = sanitize e2 in
		let e1 = (match e1.eexpr with
			| TBinop (op2,_,_) when Parser.swap op2 op -> parent e1
			| _ -> e1
		) in
		let e2 = (match e2.eexpr with
			| TBinop (op2,_,_) when Parser.swap op2 op -> parent e2
			| _ -> e2
		) in
		{ e with eexpr = TBinop (op,e1,e2) }
	| _ -> Type.map_expr sanitize e

