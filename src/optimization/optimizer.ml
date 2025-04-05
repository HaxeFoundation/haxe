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
open SafeCom
open OptimizerTexpr
open Error
open Globals
open Inline

(* ---------------------------------------------------------------------- *)
(* REDUCE *)

let check_enum_construction_args el i =
	let b,_ = List.fold_left (fun (b,i') e ->
		(b && (i' = i || not (has_side_effect e))),i' + 1
	) (true,0) el in
	b

let rec extract_constant_value e = match e.eexpr with
	| TConst (TInt _ | TFloat _ | TString _ | TBool _ | TNull) ->
		Some e
	| TConst (TThis | TSuper) ->
		None
	| TField(_,FStatic(c,({cf_kind = Var {v_write = AccNever}} as cf))) ->
		begin match cf.cf_expr with
		| Some e ->
			(* Don't care about inline, if we know the value it makes no difference. *)
			extract_constant_value e
		| None ->
			None
		end
	| TField(_,FEnum _) ->
		Some e
	| TParenthesis e1 ->
		extract_constant_value e1
	| _ ->
		None

let check_constant_switch switch =
	let rec loop e1 cases = match cases with
		| case :: cases ->
			(* Map everything first so that we find unknown things eagerly. *)
			let el = List.map (fun e2 -> match extract_constant_value e2 with
				| Some e2 -> e2
				| None -> raise Exit
			) case.case_patterns in
			if List.exists (fun e2 ->
				Texpr.equal e1 e2
			) el then
				Some case.case_expr
			else
				loop e1 cases
		| [] ->
			begin match switch.switch_default with
			| None -> None
			| Some e -> Some e
			end
	in
	let is_empty e = match e.eexpr with
		| TBlock [] -> true
		| _ -> false
	in
	let is_empty_def () = match switch.switch_default with
		| None -> true
		| Some e -> is_empty e
in
	match Texpr.skip switch.switch_subject with
		| {eexpr = TConst ct} as e1 when (match ct with TSuper | TThis -> false | _ -> true) ->
			begin try
				loop e1 switch.switch_cases
			with Exit ->
				None
			end
		| _ ->
			if List.for_all (fun case -> is_empty case.case_expr) switch.switch_cases && is_empty_def() then
				Some switch.switch_subject
			else
				None

let reduce_control_flow platform e = match e.eexpr with
	| TIf ({ eexpr = TConst (TBool t) },e1,e2) ->
		(if t then e1 else match e2 with None -> { e with eexpr = TBlock [] } | Some e -> e)
	| TWhile ({ eexpr = TConst (TBool false) },sub,flag) ->
		(match flag with
		| NormalWhile -> { e with eexpr = TBlock [] } (* erase sub *)
		| DoWhile -> e) (* we cant remove while since sub can contain continue/break *)
	| TSwitch switch ->
		begin match check_constant_switch switch with
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
		let require_cast = match platform with
			| Cpp | Flash -> true
			| Jvm -> true
			| _ -> false
		in
		Texpr.reduce_unsafe_casts ~require_cast e e.etype
	| _ ->
		e

let rec reduce_loop (scom : SafeCom.t) stack e =
	let e = Type.map_expr (reduce_loop scom stack) e in
	let reduce_expr = Sanitize.reduce_expr in
	Sanitize.sanitize_expr scom.platform_config (match e.eexpr with
	| TCall(e1,el) ->
		begin match Texpr.skip e1 with
			| { eexpr = TFunction func } as ef ->
				let cf = mk_field "" ef.etype e.epos null_pos in
				let ethis = mk (TConst TThis) t_dynamic e.epos in
				let rt = (match follow ef.etype with TFun (_,rt) -> rt | _ -> die "" __LOC__) in
				begin try
					let e = type_inline (context_of_scom scom) cf func ethis el rt None e.epos ~self_calling_closure:true false in
					reduce_loop scom stack e
				with Error { err_message = Custom _ } ->
					reduce_expr scom e
				end;
			| {eexpr = TField(ef,(FStatic(cl,cf) | FInstance(cl,_,cf)))} when SafeCom.needs_inline scom (Some cl) cf && not (rec_stack_memq cf stack) ->
				begin match cf.cf_expr with
				| Some {eexpr = TFunction tf} ->
					let config = inline_config (Some cl) cf el e.etype in
					let rt = (match Abstract.follow_with_abstracts e1.etype with TFun (_,rt) -> rt | _ -> die "" __LOC__) in
					begin try
						let e = type_inline (context_of_scom scom) cf tf ef el rt config e.epos false in
						rec_stack_default stack cf (fun cf' -> cf' == cf) (fun () -> reduce_loop scom stack e) e
					with Error { err_message = Custom _ } ->
						reduce_expr scom e
					end
				| _ ->
					reduce_expr scom e
				end
			| { eexpr = TField ({ eexpr = TTypeExpr (TClassDecl c) },field) } ->
				(match api_inline scom c (field_name field) el e.epos with
				| None -> reduce_expr scom e
				| Some e -> reduce_loop scom stack e)
			| _ ->
				reduce_expr scom e
		end
	| _ ->
		reduce_expr scom (reduce_control_flow scom.platform e))

let reduce_expression scom e =
	if scom.foptimize then begin
		(* We go through rec_stack_default here so that the current field is on inline_stack. This prevents self-recursive
		   inlining (#7569). *)
		let stack = new_rec_stack() in
		rec_stack_default stack scom.curfield (fun cf' -> cf' == scom.curfield) (fun () -> reduce_loop scom stack e) e
	end else
		e

let rec make_constant_expression scom stack ?(concat_strings=false) e =
	let e = reduce_loop scom stack e in
	match e.eexpr with
	| TConst _ -> Some e
	| TField({eexpr = TTypeExpr _},FEnum _) -> Some e
	| TBinop ((OpAdd|OpSub|OpMult|OpDiv|OpMod|OpShl|OpShr|OpUShr|OpOr|OpAnd|OpXor) as op,e1,e2) -> (match make_constant_expression scom stack e1,make_constant_expression scom stack e2 with
		| Some ({eexpr = TConst (TString s1)}), Some ({eexpr = TConst (TString s2)}) when concat_strings ->
			Some (mk (TConst (TString (s1 ^ s2))) scom.basic.tstring (punion e1.epos e2.epos))
		| Some e1, Some e2 -> Some (mk (TBinop(op, e1, e2)) e.etype e.epos)
		| _ -> None)
	| TUnop((Neg | NegBits) as op,Prefix,e1) -> (match make_constant_expression scom stack e1 with
		| Some e1 -> Some (mk (TUnop(op,Prefix,e1)) e.etype e.epos)
		| None -> None)
	| TCast (e1, None) ->
		(match make_constant_expression scom stack e1 with
		| None -> None
		| Some e1 -> Some {e with eexpr = TCast(e1,None)})
	| TParenthesis e1 ->
		begin match make_constant_expression scom stack ~concat_strings e1 with
			| None -> None
			| Some e1 -> Some {e with eexpr = TParenthesis e1}
		end
	| TMeta(m,e1) ->
		begin match make_constant_expression scom stack ~concat_strings e1 with
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

let make_constant_expression ctx ?(concat_strings=false) e =
	make_constant_expression ctx (new_rec_stack()) ~concat_strings e