(*
	The Haxe Compiler
	Copyright (C) 2005-2016  Haxe Foundation

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

let s_expr_pretty e = s_expr_pretty false "" (s_type (print_context())) e

let rec is_true_expr e1 = match e1.eexpr with
	| TConst(TBool true) -> true
	| TParenthesis e1 -> is_true_expr e1
	| _ -> false

let map_values ?(allow_control_flow=true) f e =
	let branching = ref false in
	let efinal = ref None in
	let f e =
		if !branching then
			f e
		else begin
			efinal := Some e;
			mk (TConst TNull) e.etype e.epos
		end
	in
	let rec loop complex e = match e.eexpr with
		| TIf(e1,e2,Some e3) ->
			branching := true;
			let e2 = loop true e2 in
			let e3 = loop true e3 in
			{e with eexpr = TIf(e1,e2,Some e3)}
		| TSwitch(e1,cases,edef) ->
			branching := true;
			let cases = List.map (fun (el,e) -> el,loop true e) cases in
			let edef = Option.map (loop true) edef in
			{e with eexpr = TSwitch(e1,cases,edef)}
		| TBlock [e1] ->
			loop complex e1
		| TBlock el ->
			begin match List.rev el with
			| e1 :: el ->
				let e1 = loop true e1 in
				let e = {e with eexpr = TBlock (List.rev (e1 :: el))} in
				{e with eexpr = TMeta((Meta.MergeBlock,[],e.epos),e)}
			| [] ->
				f e
			end
		| TTry(e1,catches) ->
			branching := true;
			let e1 = loop true e1 in
			let catches = List.map (fun (v,e) -> v,loop true e) catches in
			{e with eexpr = TTry(e1,catches)}
		| TMeta(m,e1) ->
			{e with eexpr = TMeta(m,loop complex e1)}
		| TParenthesis e1 ->
			{e with eexpr = TParenthesis (loop complex e1)}
		| TBreak | TContinue | TThrow _ | TReturn _ ->
			if not allow_control_flow then raise Exit;
			e
		| _ ->
			if not complex then raise Exit;
			f e
	in
	let e = loop false e in
	e,!efinal

let can_throw e =
	let rec loop e = match e.eexpr with
		| TConst _ | TLocal _ | TTypeExpr _ | TFunction _ | TBlock _ -> ()
		| TCall _ | TNew _ | TThrow _ | TCast(_,Some _) -> raise Exit
		| TField _ | TArray _ -> raise Exit (* sigh *)
		| _ -> Type.iter loop e
	in
	try
		loop e; false
	with Exit ->
		true

let rec can_be_inlined e = match e.eexpr with
	| TConst _ -> true
	| TParenthesis e1 | TMeta(_,e1) -> can_be_inlined e1
	| _ -> false

let target_handles_unops com = match com.platform with
	| Lua | Python -> false
	| _ -> true

let target_handles_assign_ops com = match com.platform with
	| Lua -> false
	| _ -> true

let rec can_be_used_as_value com e =
	let rec loop e = match e.eexpr with
		| TBlock [e] -> loop e
		| TBlock _ | TSwitch _ | TTry _ -> raise Exit
		| TCall({eexpr = TConst (TString "phi")},_) -> raise Exit
		(* | TCall _ | TNew _ when (match com.platform with Cpp | Php -> true | _ -> false) -> raise Exit *)
		| TReturn _ | TThrow _ | TBreak | TContinue -> raise Exit
		| TUnop((Increment | Decrement),_,_) when not (target_handles_unops com) -> raise Exit
		| TNew _ when com.platform = Php -> raise Exit
		| TFunction _ -> ()
		| TConst TNull when (match com.platform with Cs | Cpp | Java | Flash -> true | _ -> false) -> raise Exit
		| _ -> Type.iter loop e
	in
	try
		loop e;
		true
	with Exit ->
		false

let has_pure_meta meta = Meta.has Meta.Pure meta

let is_pure c cf = has_pure_meta c.cl_meta || has_pure_meta cf.cf_meta

let wrap_meta s e =
	mk (TMeta((Meta.Custom s,[],e.epos),e)) e.etype e.epos

let is_really_unbound v =
	v.v_name <> "`trace" && is_unbound v

let r = Str.regexp "^\\([A-Za-z0-9_]\\)+$"
let is_unbound_call_that_might_have_side_effects v el = match v.v_name,el with
	| "__js__",[{eexpr = TConst (TString s)}] when Str.string_match r s 0 -> false
	| _ -> true

let is_ref_type = function
	| TType({t_path = ["cs"],("Ref" | "Out")},_) -> true
	| TAbstract({a_path=["hl";"types"],"Ref"},_) -> true
	| _ -> false

let type_change_ok com t1 t2 =
	if t1 == t2 then
		true
	else begin
		let rec map t = match t with
			| TMono r -> (match !r with None -> t_dynamic | Some t -> map t)
			| _ -> Type.map map t
		in
		let t1 = map t1 in
		let t2 = map t2 in
		let rec is_nullable_or_whatever = function
			| TMono r ->
				(match !r with None -> false | Some t -> is_nullable_or_whatever t)
			| TType ({ t_path = ([],"Null") },[_]) ->
				true
			| TLazy f ->
				is_nullable_or_whatever (!f())
			| TType (t,tl) ->
				is_nullable_or_whatever (apply_params t.t_params tl t.t_type)
			| TFun _ ->
				false
			| TInst ({ cl_kind = KTypeParameter _ },_) ->
				false
			| TAbstract (a,_) when Meta.has Meta.CoreType a.a_meta ->
				not (Meta.has Meta.NotNull a.a_meta)
			| TAbstract (a,tl) ->
				not (Meta.has Meta.NotNull a.a_meta) && is_nullable_or_whatever (apply_params a.a_params tl a.a_this)
			| _ ->
				true
		in
		(* Check equality again to cover cases where TMono became t_dynamic *)
		t1 == t2 || match follow t1,follow t2 with
			| TDynamic _,_ | _,TDynamic _ -> false
			| _ ->
				if com.config.pf_static && is_nullable_or_whatever t1 <> is_nullable_or_whatever t2 then false
				else type_iseq t1 t2
	end

let dynarray_map f d =
	DynArray.iteri (fun i e -> DynArray.unsafe_set d i (f e)) d

let dynarray_mapi f d =
	DynArray.iteri (fun i e -> DynArray.unsafe_set d i (f i e)) d

(*
	This module rewrites some expressions to reduce the amount of special cases for subsequent analysis. After analysis
	it restores some of these expressions back to their original form.

	The following expressions are removed from the AST after `apply` has run:
	- OpBoolAnd and OpBoolOr binary operations are rewritten to TIf
	- OpAssignOp on a variable is rewritten to OpAssign
	- Prefix increment/decrement operations are rewritten to OpAssign
	- Postfix increment/decrement operations are rewritten to a TBlock with OpAssign and OpAdd/OpSub
	- `do {} while(true)` is rewritten to `while(true) {}`
	- TWhile expressions are rewritten to `while (true)` with appropriate conditional TBreak
	- TFor is rewritten to TWhile
*)
module TexprFilter = struct
	let apply com e =
		let rec loop e = match e.eexpr with
		| TBinop(OpBoolAnd | OpBoolOr as op,e1,e2) ->
			let e_then = e2 in
			let e_if,e_else = if op = OpBoolOr then
				mk (TUnop(Not,Prefix,e1)) com.basic.tbool e.epos,mk (TConst (TBool(true))) com.basic.tbool e.epos
			else
				e1,mk (TConst (TBool(false))) com.basic.tbool e.epos
			in
			loop (mk (TIf(e_if,e_then,Some e_else)) e.etype e.epos)
		| TBinop(OpAssignOp op,({eexpr = TLocal _} as e1),e2) ->
			let e = {e with eexpr = TBinop(op,e1,e2)} in
			loop {e with eexpr = TBinop(OpAssign,e1,e)}
		| TUnop((Increment | Decrement as op),flag,({eexpr = TLocal _} as e1)) ->
			let e_one = mk (TConst (TInt (Int32.of_int 1))) com.basic.tint e1.epos in
			let e = {e with eexpr = TBinop(OpAssignOp (if op = Increment then OpAdd else OpSub),e1,e_one)} in
			let e = if flag = Prefix then
				e
			else
				mk (TBlock [
					{e with eexpr = TBinop(OpAssignOp (if op = Increment then OpAdd else OpSub),e1,e_one)};
					{e with eexpr = TBinop((if op = Increment then OpSub else OpAdd),e1,e_one)};
				]) e.etype e.epos
			in
			loop e
		| TWhile(e1,e2,DoWhile) when is_true_expr e1 ->
			loop {e with eexpr = TWhile(e1,e2,NormalWhile)}
		| TWhile(e1,e2,flag) when not (is_true_expr e1) ->
			let p = e.epos in
			let e_break = mk TBreak t_dynamic p in
			let e_not = mk (TUnop(Not,Prefix,Codegen.mk_parent e1)) e1.etype e1.epos in
			let e_if eo = mk (TIf(e_not,e_break,eo)) com.basic.tvoid p in
			let rec map_continue e = match e.eexpr with
				| TContinue ->
					Texpr.duplicate_tvars (e_if (Some e))
				| TWhile _ | TFor _ ->
					e
				| _ ->
					Type.map_expr map_continue e
			in
			let e2 = if flag = NormalWhile then e2 else map_continue e2 in
			let e_if = e_if None in
			let e_block = if flag = NormalWhile then Type.concat e_if e2 else Type.concat e2 e_if in
			let e_true = mk (TConst (TBool true)) com.basic.tbool p in
			let e = mk (TWhile(Codegen.mk_parent e_true,e_block,NormalWhile)) e.etype p in
			loop e
		| TFor(v,e1,e2) ->
			let v' = alloc_var "tmp" e1.etype e1.epos in
			let ev' = mk (TLocal v') e1.etype e1.epos in
			let t1 = (Abstract.follow_with_abstracts e1.etype) in
			let ehasnext = mk (TField(ev',quick_field t1 "hasNext")) (tfun [] com.basic.tbool) e1.epos in
			let ehasnext = mk (TCall(ehasnext,[])) com.basic.tbool ehasnext.epos in
			let enext = mk (TField(ev',quick_field t1 "next")) (tfun [] v.v_type) e1.epos in
			let enext = mk (TCall(enext,[])) v.v_type e1.epos in
			let eassign = mk (TVar(v,Some enext)) com.basic.tvoid e.epos in
			let ebody = Type.concat eassign e2 in
			let e = mk (TBlock [
				mk (TVar (v',Some e1)) com.basic.tvoid e1.epos;
				mk (TWhile((mk (TParenthesis ehasnext) ehasnext.etype ehasnext.epos),ebody,NormalWhile)) com.basic.tvoid e1.epos;
			]) com.basic.tvoid e.epos in
			loop e
		| _ ->
			Type.map_expr loop e
		in
		loop e
end

module VarLazifier = struct
	let apply com e =
		let rec loop var_inits e = match e.eexpr with
			| TVar(v,Some e1) when (Meta.has (Meta.Custom ":extractorVariable") v.v_meta) ->
				let var_inits,e1 = loop var_inits e1 in
				let var_inits = PMap.add v.v_id e1 var_inits in
				var_inits,{e with eexpr = TVar(v,None)}
			| TLocal v ->
				begin try
					let e_init = PMap.find v.v_id var_inits in
					let e = {e with eexpr = TBinop(OpAssign,e,e_init)} in
					let e = {e with eexpr = TParenthesis e} in
					let var_inits = PMap.remove v.v_id var_inits in
					var_inits,e
				with Not_found ->
					var_inits,e
				end
			| TIf(e1,e2,eo) ->
				let var_inits,e1 = loop var_inits e1 in
				let _,e2 = loop var_inits e2 in
				let eo = match eo with None -> None | Some e -> Some (snd (loop var_inits e)) in
				var_inits,{e with eexpr = TIf(e1,e2,eo)}
			| TSwitch(e1,cases,edef) ->
				let var_inits,e1 = loop var_inits e1 in
				let cases = List.map (fun (el,e) ->
					let _,e = loop var_inits e in
					el,e
				) cases in
				let edef = match edef with None -> None | Some e -> Some (snd (loop var_inits e)) in
				var_inits,{e with eexpr = TSwitch(e1,cases,edef)}
			| _ ->
				Texpr.foldmap loop var_inits e
		in
		snd (loop PMap.empty e)
end

module Fusion = struct

	open AnalyzerConfig

	let get_interference_kind e =
		let vars = ref [] in
		let has_side_effect = ref false in
		let rec loop e = match e.eexpr with
			| TMeta((Meta.Pure,_,_),_) ->
				()
			| TUnop((Increment | Decrement),_,{eexpr = TLocal v}) ->
				vars := v :: !vars
			| TBinop((OpAssign | OpAssignOp _),{eexpr = TLocal v},e2) ->
				vars := v :: !vars;
				loop e2
			| TBinop((OpAssign | OpAssignOp _),e1,e2) ->
				has_side_effect := true;
				loop e1;
				loop e2;
			| TUnop((Increment | Decrement),_,e1) ->
				has_side_effect := true;
				loop e1
			| TCall({eexpr = TLocal v},el) when not (is_unbound_call_that_might_have_side_effects v el) ->
				List.iter loop el
			| TCall({eexpr = TField(_,FStatic(c,cf))},el) when is_pure c cf ->
				List.iter loop el
			| TNew(c,_,el) when (match c.cl_constructor with Some cf when is_pure c cf -> true | _ -> false) ->
				List.iter loop el;
			| TCall(e1,el) ->
				has_side_effect := true;
				loop e1;
				List.iter loop el
			| TNew(_,_,el) ->
				has_side_effect := true;
				List.iter loop el;
			| _ ->
				Type.iter loop e
		in
		loop e;
		!has_side_effect,!vars

	let apply com config e =
		let rec block_element acc el = match el with
			| {eexpr = TBinop((OpAssign | OpAssignOp _),_,_) | TUnop((Increment | Decrement),_,_)} as e1 :: el ->
				block_element (e1 :: acc) el
			| {eexpr = TLocal _} as e1 :: el when not config.local_dce ->
				block_element (e1 :: acc) el
			(* no-side-effect *)
			| {eexpr = TEnumParameter _ | TFunction _ | TConst _ | TTypeExpr _ | TLocal _} :: el ->
				block_element acc el
			(* no-side-effect composites *)
			| {eexpr = TParenthesis e1 | TMeta(_,e1) | TCast(e1,None) | TField(e1,_) | TUnop(_,_,e1)} :: el ->
				block_element acc (e1 :: el)
			| {eexpr = TArray(e1,e2) | TBinop(_,e1,e2)} :: el ->
				block_element acc (e1 :: e2 :: el)
			| {eexpr = TArrayDecl el1 | TCall({eexpr = TField(_,FEnum _)},el1)} :: el2 -> (* TODO: check e1 of FEnum *)
				block_element acc (el1 @ el2)
			| {eexpr = TObjectDecl fl} :: el ->
				block_element acc ((List.map snd fl) @ el)
			| {eexpr = TIf(e1,{eexpr = TBlock []},(Some {eexpr = TBlock []} | None))} :: el ->
				block_element acc (e1 :: el)
			| {eexpr = TBlock [e1]} :: el ->
				block_element acc (e1 :: el)
			| {eexpr = TBlock []} :: el ->
				block_element acc el
			| e1 :: el ->
				block_element (e1 :: acc) el
			| [] ->
				acc
		in
		let changed = ref false in
		let var_uses = Hashtbl.create 0 in
		let var_writes = Hashtbl.create 0 in
		let get_num_uses v =
			try Hashtbl.find var_uses v.v_id with Not_found -> 0
		in
		let get_num_writes v =
			try Hashtbl.find var_writes v.v_id with Not_found -> 0
		in
		let change map v delta =
			Hashtbl.replace map v.v_id ((try Hashtbl.find map v.v_id with Not_found -> 0) + delta);
		in
		let change_num_uses v delta =
			change var_uses v delta
		in
		let change_num_writes v delta =
			change var_writes v delta
		in
		let rec loop e = match e.eexpr with
			| TLocal v ->
				change_num_uses v 1;
			| TBinop(OpAssign,{eexpr = TLocal v},e2) ->
				change_num_writes v 1;
				loop e2
			| _ ->
				Type.iter loop e
		in
		loop e;
		let can_be_fused v e =
			let b = get_num_uses v <= 1 &&
			        get_num_writes v = 0 &&
			        can_be_used_as_value com e &&
			        (Meta.has Meta.CompilerGenerated v.v_meta || config.optimize && config.fusion && config.user_var_fusion && v.v_extra = None)
			in
(* 			let st = s_type (print_context()) in
			if e.epos.pfile = "src/Main.hx" then
				print_endline (Printf.sprintf "%s(%s) -> %s: #uses=%i && #writes=%i && used_as_value=%b && (compiler-generated=%b || optimize=%b && fusion=%b && user_var_fusion=%b && type_change_ok=%b && v_extra=%b) -> %b"
					v.v_name (st v.v_type) (st e.etype)
					(get_num_uses v) (get_num_writes v) (can_be_used_as_value com e)
					(Meta.has Meta.CompilerGenerated v.v_meta) config.optimize config.fusion
					config.user_var_fusion (type_change_ok com v.v_type e.etype) (v.v_extra = None) b); *)
			b
		in
		let rec fuse acc el = match el with
			| ({eexpr = TVar(v1,None)} as e1) :: {eexpr = TBinop(OpAssign,{eexpr = TLocal v2},e2)} :: el when v1 == v2 ->
				changed := true;
				let e1 = {e1 with eexpr = TVar(v1,Some e2)} in
				change_num_writes v1 (-1);
				fuse (e1 :: acc) el
			| ({eexpr = TVar(v1,None)} as e1) :: ({eexpr = TIf(eif,_,Some _)} as e2) :: el when can_be_used_as_value com e2 && (match com.platform with Php -> false | Cpp when not (Common.defined com Define.Cppia) -> false | _ -> true) ->
				begin try
					let i = ref 0 in
					let check_assign e = match e.eexpr with
						| TBinop(OpAssign,{eexpr = TLocal v2},e2) when v1 == v2 -> incr i; e2
						| _ -> raise Exit
					in
					let e,_ = map_values ~allow_control_flow:false check_assign e2 in
					let e = match follow e.etype with
						| TAbstract({a_path=[],"Void"},_) -> {e with etype = v1.v_type}
						| _ -> e
					in
					let e1 = {e1 with eexpr = TVar(v1,Some e)} in
					changed := true;
					change_num_writes v1 (- !i);
					fuse (e1 :: acc) el
				with Exit ->
					fuse (e1 :: acc) (e2 :: el)
				end
			| ({eexpr = TVar(v1,Some e1)} as ev) :: e2 :: el when can_be_fused v1 e1 ->
				let found = ref false in
				let affected = ref false in
				let ik1 = get_interference_kind e1 in
				let check_interference e2 =
					let check (has_side_effect,modified_vars) e2 =
						if has_side_effect then begin
							let rec loop e = match e.eexpr with
								| TMeta((Meta.Pure,_,_),_) ->
									()
								| TArray _ ->
									raise Exit
								| TField _ when Optimizer.is_affected_type e.etype ->
									raise Exit
								| TCall({eexpr = TField(_,FStatic(c,cf))},el) when is_pure c cf ->
									List.iter loop el
								| TNew(c,_,el) when (match c.cl_constructor with Some cf when is_pure c cf -> true | _ -> false) ->
									List.iter loop el
								| TCall _ | TNew _ | TBinop((OpAssign | OpAssignOp _),_,_) | TUnop((Increment | Decrement),_,_) ->
									raise Exit
								| _ ->
									Type.iter loop e
							in
							loop e2
						end;
						if modified_vars <> [] then begin
							let rec loop e = match e.eexpr with
								| TLocal v when List.memq v modified_vars -> raise Exit
								| _ -> Type.iter loop e
							in
							loop e2
						end
					in
					try
						check ik1 e2;
						check (get_interference_kind e2) e1
					with Exit -> match com.platform with
						| Cpp when not (Common.defined com Define.Cppia) -> raise Exit
						| Php -> raise Exit (* They don't define evaluation order, so let's exit *)
						| _ -> affected := true;
				in
				let rec replace e =
					let e = match e.eexpr with
						| TWhile _ | TFunction _ ->
							e
						| TIf(e1,e2,eo) ->
							let e1 = replace e1 in
							{e with eexpr = TIf(e1,e2,eo)}
						| TSwitch(e1,cases,edef) ->
							let e1 = match com.platform with
								| Lua | Python -> e1
								| _ -> replace e1
							in
							{e with eexpr = TSwitch(e1,cases,edef)}
						| TLocal v2 when v1 == v2 && not !affected ->
							found := true;
							if type_change_ok com v1.v_type e1.etype then e1 else mk (TCast(e1,None)) v1.v_type e.epos
						| TBinop((OpAssign | OpAssignOp _ as op),({eexpr = TArray(e1,e2)} as ea),e3) ->
							let e1 = replace e1 in
							let e2 = replace e2 in
							let ea = {ea with eexpr = TArray(e1,e2)} in
							let e3 = replace e3 in
							{e with eexpr = TBinop(op,ea,e3)}
						| TBinop((OpAssign | OpAssignOp _ as op),e1,e2) ->
							let e2 = replace e2 in
							let e1 = match e1.eexpr with TLocal _ -> e1 | _ -> replace e1 in
							{e with eexpr = TBinop(op,e1,e2)}
						| TUnop((Increment | Decrement),_,{eexpr = TLocal _}) ->
							e
						| TCall({eexpr = TLocal v},_) when is_really_unbound v ->
							e
						(* TODO: this is a pretty outrageous hack for https://github.com/HaxeFoundation/haxe/issues/5366 *)
						| TCall({eexpr = TField(_,FStatic({cl_path=["python"],"Syntax"},{cf_name="arraySet"}))} as ef,[e1;e2;e3]) ->
							let e3 = replace e3 in
							let e1 = replace e1 in
							let e2 = replace e2 in
							{e with eexpr = TCall(ef,[e1;e2;e3])}
						| TCall(e1,el) when com.platform = Neko ->
							(* Neko has this reversed at the moment (issue #4787) *)
							let el = List.map replace el in
							let e1 = replace e1 in
							{e with eexpr = TCall(e1,el)}
						| _ ->
							Type.map_expr replace e
					in
					check_interference e;
					e
				in
				begin try
					let e = replace e2 in
					if not !found then raise Exit;
					changed := true;
					change_num_uses v1 (-1);
					fuse (e :: acc) el
				with Exit ->
					fuse (ev :: acc) (e2 :: el)
				end
			| {eexpr = TUnop((Increment | Decrement as op,Prefix,({eexpr = TLocal v} as ev)))} as e1 :: e2 :: el ->
				begin try
					let e2,f = match e2.eexpr with
						| TReturn (Some e2) -> e2,(fun e -> {e2 with eexpr = TReturn (Some e)})
						| TBinop(OpAssign,e21,e22) -> e22,(fun e -> {e2 with eexpr = TBinop(OpAssign,e21,e)})
						| TVar(v,Some e2) -> e2,(fun e -> {e2 with eexpr = TVar(v,Some e)})
						| _ -> raise Exit
					in
					let ops_match op1 op2 = match op1,op2 with
						| Increment,OpSub
						| Decrement,OpAdd ->
							true
						| _ ->
							false
					in
					begin match e2.eexpr with
						| TBinop(op2,{eexpr = TLocal v2},{eexpr = TConst (TInt i32)}) when v == v2 && Int32.to_int i32 = 1 && ops_match op op2 ->
							changed := true;
							change_num_uses v2 (-1);
							let e = (f {e1 with eexpr = TUnop(op,Postfix,ev)}) in
							fuse (e :: acc) el
						| TLocal v2 when v == v2 ->
							changed := true;
							change_num_uses v2 (-1);
							let e = (f {e1 with eexpr = TUnop(op,Prefix,ev)}) in
							fuse (e :: acc) el
						| _ ->
							raise Exit
					end
				with Exit ->
					fuse (e1 :: acc) (e2 :: el)
				end
			| e1 :: el ->
				fuse (e1 :: acc) el
			| [] ->
				acc
		in
		let rec loop e = match e.eexpr with
			| TBlock el ->
				let el = List.map loop el in
				(* fuse flips element order, but block_element doesn't care and flips it back *)
				let el = fuse [] el in
				let el = block_element [] el in
				let rec fuse_loop el =
					changed := false;
					let el = fuse [] el in
					let el = block_element [] el in
					if !changed then fuse_loop el else el
				in
				let el = fuse_loop el in
				{e with eexpr = TBlock el}
			| TCall({eexpr = TLocal v},_) when is_really_unbound v ->
				e
			| _ ->
				Type.map_expr loop e
		in
		let e = loop e in
		e
end

module Cleanup = struct
	let apply com e =
		let if_or_op e e1 e2 e3 = match (Texpr.skip e1).eexpr,(Texpr.skip e3).eexpr with
			| TUnop(Not,Prefix,e1),TConst (TBool true) -> Optimizer.optimize_binop {e with eexpr = TBinop(OpBoolOr,e1,e2)} OpBoolOr e1 e2
			| _,TConst (TBool false) -> Optimizer.optimize_binop {e with eexpr = TBinop(OpBoolAnd,e1,e2)} OpBoolAnd e1 e2
			| _,TBlock [] -> {e with eexpr = TIf(e1,e2,None)}
			| _ -> match (Texpr.skip e2).eexpr with
				| TBlock [] when com.platform <> Cs ->
					let e1' = mk (TUnop(Not,Prefix,e1)) e1.etype e1.epos in
					let e1' = Optimizer.optimize_unop e1' Not Prefix e1 in
					{e with eexpr = TIf(e1',e3,None)}
				| _ ->
					{e with eexpr = TIf(e1,e2,Some e3)}
		in
		let rec loop e = match e.eexpr with
			| TIf(e1,e2,Some e3) ->
				let e1 = loop e1 in
				let e2 = loop e2 in
				let e3 = loop e3 in
				if_or_op e e1 e2 e3;
			| TCall({eexpr = TLocal v},_) when is_really_unbound v ->
				e
			| TBlock el ->
				let el = List.map (fun e ->
					let e = loop e in
					match e.eexpr with
					| TIf _ -> {e with etype = com.basic.tvoid}
					| _ -> e
				) el in
				{e with eexpr = TBlock el}
			| TWhile(e1,e2,NormalWhile) ->
				let e1 = loop e1 in
				let e2 = loop e2 in
				begin match e2.eexpr with
					| TBlock ({eexpr = TIf(e1,({eexpr = TBlock[{eexpr = TBreak}]} as eb),None)} :: el2) ->
						let e1 = Texpr.skip e1 in
						let e1 = match e1.eexpr with TUnop(_,_,e1) -> e1 | _ -> {e1 with eexpr = TUnop(Not,Prefix,e1)} in
						{e with eexpr = TWhile(e1,{eb with eexpr = TBlock el2},NormalWhile)}
					| TBlock el ->
						let rec loop2 el = match el with
							| {eexpr = TBreak | TContinue | TReturn _ | TThrow _} as e :: el ->
								[e]
							| e :: el ->
								e :: (loop2 el)
							| [] ->
								[]
						in
						let el = loop2 el in
						{e with eexpr = TWhile(e1,{e2 with eexpr = TBlock el},NormalWhile)}
					| _ ->
						{e with eexpr = TWhile(e1,e2,NormalWhile)}
				end
			| _ ->
				Type.map_expr loop e
		in
		loop e

	let rec reduce_control_flow ctx e =
		Type.map_expr (reduce_control_flow ctx) (Optimizer.reduce_control_flow ctx e)
end

module Purity = struct
	type purity =
		| Pure
		| NotPure
		| MaybePure

	type purity_node = {
		pn_field : tclass_field;
		mutable pn_purity : purity;
		mutable pn_dependents : purity_node list;
	}

	let node_lut = Hashtbl.create 0

	let get_field_id c cf = Printf.sprintf "%s.%s" (s_type_path c.cl_path) cf.cf_name

	let get_node c cf =
		try
			Hashtbl.find node_lut (get_field_id c cf)
		with Not_found ->
			let node = {
				pn_field = cf;
				pn_purity = MaybePure;
				pn_dependents = []
			} in
			Hashtbl.replace node_lut (get_field_id c cf) node;
			node

	let apply_to_field com is_ctor c cf =
		let node = get_node c cf in
		let rec taint node =
			if node.pn_purity <> NotPure then begin
				node.pn_purity <- NotPure;
				List.iter taint node.pn_dependents
			end
		in
		let taint_raise node =
			taint node;
			raise Exit;
		in
		let check_field c cf =
			let node' = get_node c cf in
			match node'.pn_purity with
				| Pure -> ()
				| NotPure -> taint_raise node;
				| MaybePure -> node'.pn_dependents <- node :: node'.pn_dependents
		in
		let rec check_write e1 =
			begin match e1.eexpr with
				| TLocal v ->
					() (* Writing to locals does not violate purity. *)
				| TField({eexpr = TConst TThis},_) when is_ctor ->
					() (* A constructor can write to its own fields without violating purity. *)
				| _ ->
					taint_raise node
			end
		and loop e = match e.eexpr with
			| TMeta((Meta.Pure,_,_),_) ->
				()
			| TThrow _ ->
				taint_raise node;
			| TBinop((OpAssign | OpAssignOp _),e1,e2) ->
				check_write e1;
				loop e2;
			| TUnop((Increment | Decrement),_,e1) ->
				check_write e1;
			| TCall({eexpr = TField(_,FStatic(c,cf))},el) ->
				List.iter loop el;
				check_field c cf;
			| TNew(c,_,el) ->
				List.iter loop el;
				begin match c.cl_constructor with
					| Some cf -> check_field c cf
					| None -> taint_raise node
				end
			| TCall({eexpr = TLocal v},el) when not (is_unbound_call_that_might_have_side_effects v el) ->
				List.iter loop el;
			| TCall _ ->
				taint_raise node
			| _ ->
				Type.iter loop e
		in
		match cf.cf_expr with
		| None ->
			taint node
		| Some e ->
			try
				if (Meta.has (Meta.Custom ":impure")) cf.cf_meta then taint_raise node;
				if is_pure c cf then raise Exit;
				loop e;
				node.pn_purity <- Pure;
			with Exit ->
				()

	let apply_to_class com c =
		List.iter (apply_to_field com false c) c.cl_ordered_fields;
		List.iter (apply_to_field com false c) c.cl_ordered_statics;
		(match c.cl_constructor with Some cf -> apply_to_field com true c cf | None -> ())

	let infer com =
		Hashtbl.clear node_lut;
		List.iter (fun mt -> match mt with
			| TClassDecl c -> apply_to_class com c
			| _ -> ()
		) com.types;
		Hashtbl.fold (fun _ node acc ->
			if node.pn_purity = Pure then begin
				node.pn_field.cf_meta <- (Meta.Pure,[],node.pn_field.cf_pos) :: node.pn_field.cf_meta;
				node.pn_field :: acc
			end else acc
		) node_lut [];
end