(*
 *  Haxe Compiler
 *  Copyright (c)2005 Nicolas Cannasse
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

let rec iter f e =
	match e.eexpr with
	| TConst _
	| TLocal _
	| TEnumField _
	| TBreak
	| TContinue
	| TTypeExpr _ -> ()
	| TArray (e1,e2)
	| TBinop (_,e1,e2)
	| TFor (_,_,e1,e2)
	| TWhile (e1,e2,_)
		-> f e1; f e2
	| TThrow e
	| TField (e,_)
	| TParenthesis e
	| TUnop (_,_,e)
	| TFunction { tf_expr = e }
		-> f e
	| TArrayDecl el 
	| TNew (_,_,el)
	| TBlock el
		-> List.iter f el
	| TObjectDecl el -> List.iter (fun (_,e) -> f e) el
	| TCall (e,el) -> f e; List.iter f el
	| TVars vl -> List.iter (fun (_,_,eo) -> match eo with None -> () | Some e -> f e) vl
	| TIf (e,e1,e2) -> f e; f e1; (match e2 with None -> () | Some e -> f e)
	| TSwitch (e,cases,def) -> f e; List.iter (fun (el,e) -> List.iter f el; f e) cases; (match def with None -> () | Some e -> f e)
	| TMatch (e,_,cases,def) -> f e; List.iter (fun (_,_,e) -> f e) cases; (match def with None -> () | Some e -> f e)
	| TTry (e,catches) -> f e; List.iter (fun (_,_,e) -> f e) catches
	| TReturn eo -> (match eo with None -> () | Some e -> f e)

let rec map f e =
	match e.eexpr with
	| TConst _
	| TLocal _
	| TEnumField _
	| TBreak
	| TContinue
	| TTypeExpr _ ->
		e
	| TArray (e1,e2) ->
		{ e with eexpr = TArray (f e1,f e2) }
	| TBinop (op,e1,e2) ->
		{ e with eexpr = TBinop (op,f e1,f e2) }
	| TFor (v,t,e1,e2) ->
		{ e with eexpr = TFor (v,t,f e1,f e2) }
	| TWhile (e1,e2,flag) ->
		{ e with eexpr = TWhile (f e1,f e2,flag) }
	| TThrow e1 ->
		{ e with eexpr = TThrow (f e1) }
	| TField (e1,v) ->
		{ e with eexpr = TField (f e1,v) }
	| TParenthesis e1 ->
		{ e with eexpr = TParenthesis (f e1) }
	| TUnop (op,pre,e1) ->
		{ e with eexpr = TUnop (op,pre,f e1) }
	| TArrayDecl el ->
		{ e with eexpr = TArrayDecl (List.map f el) }
	| TNew (t,pl,el) ->
		{ e with eexpr = TNew (t,pl,List.map f el) }
	| TBlock el ->
		{ e with eexpr = TBlock (List.map f el) }
	| TObjectDecl el ->
		{ e with eexpr = TObjectDecl (List.map (fun (v,e) -> v, f e) el) }
	| TCall (e1,el) ->
		{ e with eexpr = TCall (f e1, List.map f el) }
	| TVars vl ->
		{ e with eexpr = TVars (List.map (fun (v,t,e) -> v , t , match e with None -> None | Some e -> Some (f e)) vl) }
	| TFunction fu ->
		{ e with eexpr = TFunction { fu with tf_expr = f fu.tf_expr } }
	| TIf (ec,e1,e2) ->
		{ e with eexpr = TIf (f ec,f e1,match e2 with None -> None | Some e -> Some (f e)) }
	| TSwitch (e1,cases,def) ->
		{ e with eexpr = TSwitch (f e1, List.map (fun (el,e2) -> List.map f el, f e2) cases, match def with None -> None | Some e -> Some (f e)) }
	| TMatch (e1,t,cases,def) ->
		{ e with eexpr = TMatch (f e1, t, List.map (fun (cl,params,e) -> cl, params, f e) cases, match def with None -> None | Some e -> Some (f e)) }
	| TTry (e1,catches) ->
		{ e with eexpr = TTry (f e1, List.map (fun (v,t,e) -> v, t, f e) catches) }
	| TReturn eo ->
		{ e with eexpr = TReturn (match eo with None -> None | Some e -> Some (f e)) }

let local_find flag vname e =
	let rec loop2 e =
		match e.eexpr with
		| TFunction f ->
			if not flag && not (List.exists (fun (a,_,_) -> a = vname) f.tf_args) then loop2 f.tf_expr
		| TBlock _ ->
			(try
				iter loop2 e;
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

let block_vars e =
	let add_var map v d = map := PMap.add v d (!map) in
	let wrap e used =
		match PMap.foldi (fun v _ acc -> v :: acc) used [] with
		| [] -> e
		| vars ->
			mk (TCall (
				(mk (TFunction {
					tf_args = List.map (fun v -> v , false, t_dynamic) vars;
					tf_type = t_dynamic;
					tf_expr = mk (TReturn (Some e)) t_dynamic e.epos;
				}) t_dynamic e.epos),
				List.map (fun v -> mk (TLocal v) t_dynamic e.epos) vars)
			) t_dynamic e.epos
	in
	let rec in_fun vars depth used_locals e =
		match e.eexpr with
		| TLocal v ->
			(try
				if PMap.find v vars = depth then add_var used_locals v depth;				
			with
				Not_found -> ())
		| _ ->
			iter (in_fun vars depth used_locals) e

	and in_loop vars depth e =
		match e.eexpr with
		| TVars l ->
			{ e with eexpr = TVars (List.map (fun (v,t,e) ->
				let e = (match e with None -> None | Some e -> Some (in_loop vars depth e)) in
				add_var vars v depth;
				v, t, e
			) l) }
		| TFor (v,t,i,e1) ->
			let new_vars = PMap.add v depth (!vars) in
			{ e with eexpr = TFor (v,t,in_loop vars depth i,in_loop (ref new_vars) depth e1) }
		| TTry (e1,cases) ->
			let e1 = in_loop vars depth e1 in
			let cases = List.map (fun (v,t,e) ->
				let new_vars = PMap.add v depth (!vars) in
				v , t, in_loop (ref new_vars) depth e
			) cases in
			{ e with eexpr = TTry (e1,cases) }
		| TMatch (e1,t,cases,def) ->
			let e1 = in_loop vars depth e1 in
			let cases = List.map (fun (cl,params,e) ->
				let e = (match params with
					| None -> in_loop vars depth e
					| Some l ->
						let new_vars = List.fold_left (fun acc (v,t) ->
							match v with
							| None -> acc
							| Some name -> PMap.add name depth acc
						) (!vars) l in
						in_loop (ref new_vars) depth e
				) in
				cl , params, e
			) cases in
			let def = (match def with None -> None | Some e -> Some (in_loop vars depth e)) in
			{ e with eexpr = TMatch (e1, t, cases, def) }
		| TBlock l ->
			let new_vars = (ref !vars) in
			map (in_loop new_vars depth) e
		| TFunction _ ->
			let new_vars = !vars in
			let used = ref PMap.empty in
			iter (in_fun new_vars depth used) e;
			let e = wrap e (!used) in
			let new_vars = ref (PMap.foldi (fun v _ acc -> PMap.remove v acc) (!used) new_vars) in
			map (in_loop new_vars (depth + 1)) e
		| _ ->
			map (in_loop vars depth) e
	and out_loop e =
		match e.eexpr with
		| TFor _ | TWhile _ ->
			in_loop (ref PMap.empty) 0 e
		| _ ->
			map out_loop e
	in
	out_loop e

let emk e = mk e (mk_mono()) null_pos

let block e =
	match e.eexpr with
	| TBlock (_ :: _) -> e
	| _ -> mk (TBlock [e]) e.etype e.epos

let stack_var = "$s"
let exc_stack_var = "$e"
let stack_var_pos = "$spos"
let stack_e = emk (TLocal stack_var)
let stack_pop = emk (TCall (emk (TField (stack_e,"pop")),[]))

let stack_push useadd (c,m) =
	emk (TCall (emk (TField (stack_e,"push")),[
		if useadd then
			emk (TBinop (
				OpAdd,
				emk (TConst (TString (s_type_path c.cl_path ^ "::"))),
				emk (TConst (TString m))
			))
		else
			emk (TConst (TString (s_type_path c.cl_path ^ "::" ^ m)))
	]))

let stack_save_pos =
	emk (TVars [stack_var_pos, t_dynamic, Some (emk (TField (stack_e,"length")))])

let stack_restore_pos =
	let ev = emk (TLocal exc_stack_var) in
	[
	emk (TBinop (OpAssign, ev, emk (TArrayDecl [])));
	emk (TWhile (
		emk (TBinop (OpGte,
			emk (TField (stack_e,"length")),
			emk (TLocal stack_var_pos)
		)),
		emk (TCall (
			emk (TField (ev,"unshift")),
			[emk (TCall (
				emk (TField (stack_e,"pop")),
				[]
			))]
		)),
		NormalWhile
	));
	emk (TCall (emk (TField (stack_e,"push")),[ emk (TArray (ev,emk (TConst (TInt 0l)))) ]))
	]

let stack_block ?(useadd=false) ctx e =
	let rec loop e =
		match e.eexpr with
		| TFunction _ ->
			e
		| TReturn (Some e) ->
			mk (TBlock [
				mk (TVars ["$tmp", t_dynamic, Some (loop e)]) t_dynamic e.epos;
				stack_pop;
				mk (TReturn (Some (mk (TLocal "$tmp") t_dynamic e.epos))) t_dynamic e.epos
			]) e.etype e.epos
		| TTry (v,cases) ->
			let v = loop v in
			let cases = List.map (fun (n,t,e) ->
				let e = loop e in
				let e = (match (block e).eexpr with
					| TBlock l -> mk (TBlock (stack_restore_pos @ l)) e.etype e.epos
					| _ -> assert false
				) in
				n , t , e
			) cases in
			mk (TTry (v,cases)) e.etype e.epos
		| _ ->
			map loop e
	in
	match (block e).eexpr with
	| TBlock l -> mk (TBlock (stack_push useadd ctx :: stack_save_pos :: List.map loop l @ [stack_pop])) e.etype e.epos
	| _ -> assert false

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
