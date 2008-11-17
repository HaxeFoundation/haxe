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
		name
	) f.tf_args in
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
	let rec map term e =
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
			let term = (match def with None -> false | Some _ -> term) in
			let cases = List.map (fun (i,vl,e) ->
				i, opt (List.map (fun (n,t) -> opt local n, t)) vl, map term e
			) cases in
			{ e with eexpr = TMatch (map false e,en,cases,opt (map term) def) }
		| TTry (e1,catches) ->
			{ e with eexpr = TTry (map term e1,List.map (fun (v,t,e) -> local v,t,map term e) catches) }
		| TBlock l ->
			let old = save_locals ctx in
			let rec loop = function
				| [] -> []
				| [e] -> [map term e]
				| e :: l ->
					let e = map false e in
					e :: loop l
			in
			let l = loop l in
			old();
			{ e with eexpr = TBlock l }
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
	let vars = List.map2 (fun n e ->
		let flag = not (Hashtbl.mem lsets n) && (match e.eexpr with
			| TLocal _ | TConst _ -> true
			| _ ->
				let used = !(Hashtbl.find hcount n) in
				used <= 1
		) in
		(n,e.etype,e,flag)
	) (vthis :: pnames) (ethis :: params) in
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
			let rec map_type e = 
				let e = { e with etype = apply_params tparams tmonos e.etype } in
				Type.map_expr map_type e
			in
			Some (map_type e)

(* ---------------------------------------------------------------------- *)
(* LOOPS *)

let optimize_for_loop ctx i e1 e2 p =
	let t_void = ctx.api.tvoid in
	let t_int = ctx.api.tint in
	let lblock el = Some (mk (TBlock el) t_void p) in
	match e1.eexpr, follow e1.etype with
	| TNew ({ cl_path = ([],"IntIter") },[],[i1;i2]) , _ ->
		let max = (match i1.eexpr , i2.eexpr with
			| TConst (TInt a), TConst (TInt b) when Int32.compare b a <= 0 -> error "Range operate can't iterate backwards" p
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
