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
			| TBlock _ -> f
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
				let rt = (match follow ef.etype with TFun (_,rt) -> rt | _ -> assert false) in
				let inl = (try type_inline ctx cf func ethis el rt None e.epos ~self_calling_closure:true false with Error (Custom _,_) -> None) in
				(match inl with
				| None -> reduce_expr ctx e
				| Some e -> reduce_loop ctx e)
			| {eexpr = TField(ef,(FStatic(_,cf) | FInstance(_,_,cf)))} when cf.cf_kind = Method MethInline && not (rec_stack_memq cf inline_stack) ->
				begin match cf.cf_expr with
				| Some {eexpr = TFunction tf} ->
					let rt = (match follow e1.etype with TFun (_,rt) -> rt | _ -> assert false) in
					let inl = (try type_inline ctx cf tf ef el rt None e.epos false with Error (Custom _,_) -> None) in
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
		let v' = alloc_var VInlinedConstructorVariable (Printf.sprintf "%s_%s" v.v_name s) t v.v_pos in
		ii.ii_fields <- PMap.add s v' ii.ii_fields;
		v'
	in
	let int_field_name i =
		if i < 0 then "n" ^ (string_of_int (-i))
		else (string_of_int i)
	in
	let is_extern_ctor c cf = c.cl_extern || has_class_field_flag cf CfExtern in
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
						let el = List.fold_left (fun acc ((s,_,_),e) ->
							if not (Lexer.is_valid_identifier s) then raise Exit;
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

let optimize_completion_expr e args =
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
	let rec hunt_idents e = match fst e with
		| EConst (Ident i) -> decl i None None
		| _ -> Ast.iter_expr hunt_idents e
	in
	let e0 = e in
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
			let vl = List.map (fun ((v,pv),final,t,e) ->
				let e = (match e with None -> None | Some e -> Some (loop e)) in
				decl v (Option.map fst t) e;
				((v,pv),final,t,e)
			) vl in
			(EVars vl,p)
		| EBlock el ->
			let old = save() in
			let told = ref (!typing_side_effect) in
			let el = List.fold_left (fun acc e ->
				typing_side_effect := false;
				let e = loop e in
				if !typing_side_effect || DisplayPosition.display_position#enclosed_in (pos e) then begin told := true; e :: acc end else acc
			) [] el in
			old();
			typing_side_effect := !told;
			(EBlock (List.rev el),p)
		| EFunction (kind,f) ->
			(match kind with
			| FKNamed ((name,_),_) ->
				decl name None (Some e)
			| _ -> ());
			let old = save() in
			List.iter (fun ((n,_),_,_,t,e) -> decl n (Option.map fst t) e) f.f_args;
			let e = map e in
			old();
			e
		| EFor (header,body) ->
			let idents = ref []
			and has_in = ref false in
			let rec collect_idents e =
				match e with
					| EConst (Ident name), p ->
						idents := (name,p) :: !idents;
						e
					| EBinop (OpIn, e, it), p ->
						has_in := true;
						(EBinop (OpIn, collect_idents e, loop it), p)
					| _ ->
						Ast.map_expr collect_idents e
			in
			let header = collect_idents header in
			(match !idents,!has_in with
				| [],_ | _,false -> map e
				| idents,true ->
					let old = save() in
					List.iter
						(fun (name, pos) ->
							let etmp = (EConst (Ident "`tmp"),pos) in
							decl name None (Some (EBlock [
								(EVars [("`tmp",null_pos),false,None,None],p);
								(EFor(header,(EBinop (OpAssign,etmp,(EConst (Ident name),p)),p)), p);
								etmp
							],p));
						)
						idents;
					let body = loop body in
					old();
					(EFor(header,body),p)
			)
		| EReturn _ ->
			typing_side_effect := true;
			map e
		| ESwitch (e1,cases,def) when DisplayPosition.display_position#enclosed_in p ->
			let e1 = loop e1 in
			hunt_idents e1;
			(* Prune all cases that aren't our display case *)
			let cases = List.filter (fun (_,_,_,p) -> DisplayPosition.display_position#enclosed_in p) cases in
			(* Don't throw away the switch subject when we optimize in a case expression because we might need it *)
			let cases = List.map (fun (el,eg,eo,p) ->
				List.iter hunt_idents el;
				el,eg,(try Option.map loop eo with Return e -> Some e),p
			) cases in
			let def = match def with
				| None -> None
				| Some (None,p) -> Some (None,p)
				| Some (Some e,p) -> Some (Some (loop e),p)
			in
			(ESwitch (e1,cases,def),p)
		| ESwitch (e,cases,def) ->
			let e = loop e in
			let cases = List.map (fun (el,eg,eo,p) -> match eo with
				| None ->
					el,eg,eo,p
				| Some e ->
					let el = List.map loop el in
					let old = save() in
					List.iter hunt_idents el;
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
		| ECall(e1,el) when DisplayPosition.display_position#enclosed_in p ->
			let e1 = loop e1 in
			let el = List.map (fun e ->
				if DisplayPosition.display_position#enclosed_in (pos e) then
					(try loop e with Return e -> e)
				else
					(EConst (Ident "null"),(pos e))
			) el in
			(ECall(e1,el),p)
		| ECheckType(e1,th) ->
			typing_side_effect := true;
			let e1 = loop e1 in
			(ECheckType(e1,th),p)
		| EMeta(m,e1) ->
			begin try
				let e1 = loop e1 in
				(EMeta(m,e1),(pos e))
			with Return e1 ->
				let e1 = (EMeta(m,e1),(pos e)) in
				raise (Return e1)
			end
		| EDisplay(_,DKStructure) ->
			raise (Return e0)
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
								let name = "`tmp_" ^ string_of_int id in
								tmp_locals := ((name,null_pos),false,None,Some e) :: !tmp_locals;
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
	List.iter (fun ((n,_),_,_,t,e) -> decl n (Option.map fst t) e) args;
	(try loop e with Return e -> e)

(* ---------------------------------------------------------------------- *)
