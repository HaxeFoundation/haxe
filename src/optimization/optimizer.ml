(*
	The Haxe Compiler
	Copyright (C) 2005-2019  Haxe Foundation

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
open Inline

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
	| OpIn -> 4, right
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
	| OpAssignOp OpAssign -> 18, right (* mimics ?: *)
	| OpAssign | OpAssignOp _ -> 19, right

let rec need_parent e =
	match e.eexpr with
	| TConst _ | TLocal _ | TArray _ | TField _ | TEnumParameter _ | TEnumIndex _ | TParenthesis _
	| TCall _ | TNew _ | TTypeExpr _ | TObjectDecl _ | TArrayDecl _ | TIdent _ -> false
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
		| TCall ({ eexpr = TIdent "__js__" },_) (* we never know *)
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
			| TConst (TInt i) when not left ->
				(match op with
					| OpAdd | OpSub -> (Int32.to_int i) < 0
					| _ -> false
				)
			| TConst (TFloat flt) when not left ->
				(match op with
					| OpAdd | OpSub -> String.get flt 0 = '-'
					| _ -> false
				)
			| _ -> false
		in
		let e1 = if loop e1 true then parent e1 else e1 in
		let e2 = if loop e2 false then parent e2 else e2 in
		{ e with eexpr = TBinop (op,e1,e2) }
	| TUnop (Not,Prefix,{ eexpr = (TUnop (Not,Prefix,e1)) | (TParenthesis { eexpr = TUnop (Not,Prefix,e1) }) })
		when ExtType.is_bool (Abstract.follow_with_abstracts_without_null e1.etype) ->
		e1
	| TUnop (op,mode,e1) ->
		let rec loop ee =
			match ee.eexpr with
			| TConst (TInt i) when op = Neg && (Int32.to_int i) < 0 -> parent e1
			| TConst (TFloat flt) when op = Neg && String.get flt 0 = '-' -> parent e1
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
			| TBlock exprs ->
				if ExtType.is_void (follow f.tf_type) then
					match List.rev exprs with
					| { eexpr = TReturn None } :: rest -> { f with tf_expr = { f.tf_expr with eexpr = TBlock (List.rev rest) } }
					| _ -> f
				else
					f
			| _ -> { f with tf_expr = block f.tf_expr }
		) in
		{ e with eexpr = TFunction f }
	| TCall (e2,args) ->
		if need_parent e2 then { e with eexpr = TCall(parent e2,args) } else e
	| TEnumParameter (e2,ef,i) ->
		if need_parent e2 then { e with eexpr = TEnumParameter(parent e2,ef,i) } else e
	| TEnumIndex e2 ->
		if need_parent e2 then { e with eexpr = TEnumIndex(parent e2) } else e
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

let check_enum_construction_args el i =
	let b,_ = List.fold_left (fun (b,i') e ->
		(b && (i' = i || not (has_side_effect e))),i' + 1
	) (true,0) el in
	b

let check_constant_switch e1 cases def =
	let rec loop e1 cases = match cases with
		| (el,e) :: cases ->
			if List.exists (Texpr.equal e1) el then Some e
			else loop e1 cases
		| [] ->
			begin match def with
			| None -> None
			| Some e -> Some e
			end
	in
	match Texpr.skip e1 with
		| {eexpr = TConst ct} as e1 when (match ct with TSuper | TThis -> false | _ -> true) ->
			loop e1 cases
		| _ ->
			None

let reduce_control_flow ctx e = match e.eexpr with
	| TIf ({ eexpr = TConst (TBool t) },e1,e2) ->
		(if t then e1 else match e2 with None -> { e with eexpr = TBlock [] } | Some e -> e)
	| TWhile ({ eexpr = TConst (TBool false) },sub,flag) ->
		(match flag with
		| NormalWhile -> { e with eexpr = TBlock [] } (* erase sub *)
		| DoWhile -> e) (* we cant remove while since sub can contain continue/break *)
	| TSwitch (e1,cases,def) ->
		begin match check_constant_switch e1 cases def with
		| Some e -> e
		| None -> e
		end
	| TBinop (op,e1,e2) ->
		optimize_binop e op e1 e2
	| TUnop (op,flag,esub) ->
		optimize_unop e op flag esub
	| TCall ({ eexpr = TField (o,FClosure (c,cf)) } as f,el) ->
		let fmode = (match c with None -> FAnon cf | Some (c,tl) -> FInstance (c,tl,cf)) in
		{ e with eexpr = TCall ({ f with eexpr = TField (o,fmode) },el) }
	| TEnumParameter({eexpr = TCall({eexpr = TField(_,FEnum(_,ef1))},el)},ef2,i)
	| TEnumParameter({eexpr = TParenthesis {eexpr = TCall({eexpr = TField(_,FEnum(_,ef1))},el)}},ef2,i)
		when ef1 == ef2 && check_enum_construction_args el i ->
		(try List.nth el i with Failure _ -> e)
	| TCast(e1,None) ->
		(* TODO: figure out what's wrong with these targets *)
		let require_cast = match ctx.com.platform with
			| Cpp | Flash -> true
			| Java -> defined ctx.com Define.Jvm
			| Cs -> defined ctx.com Define.EraseGenerics || defined ctx.com Define.FastCast
			| _ -> false
		in
		Texpr.reduce_unsafe_casts ~require_cast e e.etype
	| _ ->
		e

let inline_stack = new_rec_stack()

let rec reduce_loop ctx e =
	let e = Type.map_expr (reduce_loop ctx) e in
	sanitize_expr ctx.com (match e.eexpr with
	| TCall(e1,el) ->
		begin match Texpr.skip e1 with
			| { eexpr = TFunction func } as ef ->
				let cf = mk_field "" ef.etype e.epos null_pos in
				let ethis = mk (TConst TThis) t_dynamic e.epos in
				let rt = (match follow ef.etype with TFun (_,rt) -> rt | _ -> die "" __LOC__) in
				let inl = (try type_inline ctx cf func ethis el rt None e.epos ~self_calling_closure:true false with Error (Custom _,_) -> None) in
				(match inl with
				| None -> reduce_expr ctx e
				| Some e -> reduce_loop ctx e)
			| {eexpr = TField(ef,(FStatic(cl,cf) | FInstance(cl,_,cf)))} when needs_inline ctx (has_class_flag cl CExtern) cf && not (rec_stack_memq cf inline_stack) ->
				begin match cf.cf_expr with
				| Some {eexpr = TFunction tf} ->
					let config = inline_config (Some cl) cf el e.etype in
					let rt = (match follow e1.etype with TFun (_,rt) -> rt | _ -> die "" __LOC__) in
					let inl = (try type_inline ctx cf tf ef el rt config e.epos false with Error (Custom _,_) -> None) in
					(match inl with
					| None -> reduce_expr ctx e
					| Some e ->
						rec_stack_default inline_stack cf (fun cf' -> cf' == cf) (fun () -> reduce_loop ctx e) e)
				| _ ->
					reduce_expr ctx e
				end
			| { eexpr = TField ({ eexpr = TTypeExpr (TClassDecl c) },field) } ->
				(match api_inline ctx c (field_name field) el e.epos with
				| None -> reduce_expr ctx e
				| Some e -> reduce_loop ctx e)
			| _ ->
				reduce_expr ctx e
		end
	| _ ->
		reduce_expr ctx (reduce_control_flow ctx e))

let reduce_expression ctx e =
	if ctx.com.foptimize then
		(* We go through rec_stack_default here so that the current field is on inline_stack. This prevents self-recursive
		   inlining (#7569). *)
		rec_stack_default inline_stack ctx.curfield (fun cf' -> cf' == ctx.curfield) (fun () -> reduce_loop ctx e) e
	else
		e

let rec make_constant_expression ctx ?(concat_strings=false) e =
	let e = reduce_loop ctx e in
	match e.eexpr with
	| TConst _ -> Some e
	| TField({eexpr = TTypeExpr _},FEnum _) -> Some e
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