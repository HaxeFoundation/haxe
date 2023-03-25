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

open Globals
open Ast
open Type
open Common
open AnalyzerConfig
open AnalyzerTypes
open AnalyzerTypes.BasicBlock
open AnalyzerTypes.Graph
open AnalyzerTexpr
open OptimizerTexpr

(*
	Transforms an expression to a graph, and a graph back to an expression. This module relies on TexprFilter being
	run first.

	The created graph is intact and can immediately be transformed back to an expression, or used for analysis first.
*)

let rec func ctx bb tf t p =
	let g = ctx.graph in
	let create_node kind t p =
		let bb = Graph.create_node g kind t p in
		bb.bb_loop_groups <- ctx.loop_stack;
		bb
	in
	let bb_root = create_node (BKFunctionBegin tf) tf.tf_expr.etype tf.tf_expr.epos in
	let bb_exit = create_node BKFunctionEnd tf.tf_expr.etype tf.tf_expr.epos in
	add_function g tf t p bb_root;
	add_cfg_edge bb bb_root CFGFunction;
	let bb_breaks = ref [] in
	let bb_continue = ref None in
	let b_try_stack = ref [] in
	let begin_loop bb_loop_pre bb_continue' =
		let old = !bb_breaks,!bb_continue in
		bb_breaks := [];
		bb_continue := Some bb_continue';
		let id = ctx.loop_counter in
		g.g_loops <- IntMap.add id bb_loop_pre g.g_loops;
		ctx.loop_stack <- id :: ctx.loop_stack;
		bb_continue'.bb_loop_groups <- id :: bb_continue'.bb_loop_groups;
		ctx.loop_counter <- id + 1;
		(fun () ->
			let breaks = !bb_breaks in
			bb_breaks := fst old;
			bb_continue := snd old;
			ctx.loop_stack <- List.tl ctx.loop_stack;
			breaks;
		)
	in
	let begin_try b =
		b_try_stack := b :: !b_try_stack;
		(fun () ->
			b_try_stack := List.tl !b_try_stack
		)
	in
	let add_terminator bb term =
		bb.bb_terminator <- term;
		close_node bb;
		g.g_unreachable
	in
	let check_unbound_call s el =
		if s = "$ref" then begin match el with
			| [{eexpr = TLocal v}] -> add_var_flag v VCaptured
			| _ -> ()
		end;
		if is_unbound_call_that_might_have_side_effects s el then ctx.has_unbound <- true;
	in
	let no_void t p =
		if ExtType.is_void (follow t) then Error.typing_error "Cannot use Void as value" p
	in
	let push_name s =
		ctx.name_stack <- s :: ctx.name_stack;
		(fun () -> ctx.name_stack <- List.tl ctx.name_stack)
	in
	let check_ref v e = if ExtType.has_reference_semantics v.v_type then match (Texpr.skip e).eexpr with
		| TLocal v' -> add_var_flag v' VCaptured
		| _ -> ()
	in
	let rec value' bb e = match e.eexpr with
		| TLocal _ | TIdent _ ->
			bb,e
		| TBinop(OpAssign,({eexpr = TLocal v} as e1),e2) ->
			block_element bb e,e1
		| TBlock [e1] ->
			value bb e1
		| TBlock _ | TIf _ | TSwitch _ | TTry _ ->
			bind_to_temp bb false e
		| TCall({eexpr = TIdent s},el) when is_really_unbound s ->
			check_unbound_call s el;
			bb,e
		| TCall(e1,el) ->
			call bb e e1 el
		| TBinop(OpAssignOp op,({eexpr = TArray(e1,e2)} as ea),e3) ->
			array_assign_op bb op e ea e1 e2 e3
		| TBinop(OpAssignOp op,({eexpr = TField(e1,fa)} as ef),e2) ->
			field_assign_op bb op e ef e1 fa e2
		| TBinop((OpAssign | OpAssignOp _) as op,e1,e2) ->
			let bb,e1 = value bb e1 in
			let bb,e2 = value bb e2 in
			bb,{e with eexpr = TBinop(op,e1,e2)}
		| TBinop(op,e1,e2) ->
			let bb,e1,e2 = match ordered_value_list bb [e1;e2] with
				| bb,[e1;e2] -> bb,e1,e2
				| _ -> die "" __LOC__
			in
			bb,{e with eexpr = TBinop(op,e1,e2)}
		| TUnop(op,flag,e1) ->
			let bb,e1 = value bb e1 in
			bb,{e with eexpr = TUnop(op,flag,e1)}
		| TArrayDecl el ->
			let bb,el = ordered_value_list bb el in
			bb,{e with eexpr = TArrayDecl el}
		| TObjectDecl fl ->
			let el = List.map snd fl in
			let bb,el = ordered_value_list bb el in
			bb,{e with eexpr = TObjectDecl (List.map2 (fun (s,_) e -> s,e) fl el)}
		| TField({eexpr = TTypeExpr _},fa) ->
			bb,e
		| TField(e1,fa) ->
			let bb,e1 = value bb e1 in
			bb,{e with eexpr = TField(e1,fa)}
		| TArray(e1,e2) ->
			let bb,e1,e2 = match ordered_value_list bb [e1;e2] with
				| bb,[e1;e2] -> bb,e1,e2
				| _ -> die "" __LOC__
			in
			bb,{e with eexpr = TArray(e1,e2)}
		| TMeta(m,e1) ->
			let bb,e1 = value bb e1 in
			bb,{e with eexpr = TMeta(m,e1)}
		| TParenthesis e1 ->
			let bb,e1 = value bb e1 in
			bb,{e with eexpr = TParenthesis e1}
		| TCast(e1,mto) ->
			let bb,e1 = value bb e1 in
			bb,{e with eexpr = TCast(e1,mto)}
		| TNew(c,tl,el) ->
			let bb,el = ordered_value_list bb el in
			bb,{e with eexpr = TNew(c,tl,el)}
		| TEnumParameter(e1,ef,ei) ->
			let bb,e1 = value bb e1 in
			bb,{e with eexpr = TEnumParameter(e1,ef,ei)}
		| TEnumIndex e1 ->
			let bb,e1 = value bb e1 in
			bb,{e with eexpr = TEnumIndex e1}
		| TFunction tf ->
			let bb_func,bb_func_end = func ctx bb tf e.etype e.epos in
			let e_fun = mk (TConst (TString "fun")) t_dynamic p in
			let econst = mk (TConst (TInt (Int32.of_int bb_func.bb_id))) ctx.com.basic.tint e.epos in
			let ec = mk (TCall(e_fun,[econst])) t_dynamic p in
			let bb_next = create_node BKNormal bb.bb_type bb.bb_pos in
			add_cfg_edge bb bb_next CFGGoto;
			set_syntax_edge bb (SEMerge bb_next);
			close_node bb;
			add_cfg_edge bb_func_end bb_next CFGGoto;
			bb_next,ec
		| TConst _ | TTypeExpr _ ->
			bb,e
		| TThrow _ | TReturn _ | TBreak | TContinue ->
			let bb = block_element bb e in
			bb,mk (TConst TNull) t_dynamic e.epos
		| TVar _ | TFor _ | TWhile _ ->
			Error.typing_error "Cannot use this expression as value" e.epos
	and value bb e =
		let bb,e = value' bb e in
		no_void e.etype e.epos;
		bb,e
	and ordered_value_list bb el =
		let might_be_affected,collect_modified_locals = create_affection_checker() in
		let rec can_be_optimized e = match e.eexpr with
			| TBinop _ | TArray _ | TCall _ -> true
			| TParenthesis e1 -> can_be_optimized e1
			| _ -> false
		in
		let _,el = List.fold_left (fun (had_side_effect,acc) e ->
			if had_side_effect then
				(true,(might_be_affected e || has_side_effect e,can_be_optimized e,e) :: acc)
			else begin
				let had_side_effect = has_side_effect e in
				if had_side_effect then collect_modified_locals e;
				let opt = can_be_optimized e in
				(had_side_effect || opt,(false,opt,e) :: acc)
			end
		) (false,[]) (List.rev el) in
		let bb,values = List.fold_left (fun (bb,acc) (aff,opt,e) ->
			if bb == g.g_unreachable then
				bb,acc
			else begin
				let bb,value = if aff || opt then bind_to_temp bb aff e else value bb e in
				bb,(value :: acc)
			end
		) (bb,[]) el in
		bb,List.rev values
	and bind_to_temp ?(v=None) bb sequential e =
		let is_probably_not_affected e e1 fa = match fa with
			| FAnon cf | FInstance (_,_,cf) | FStatic (_,cf) | FClosure (_,cf) when cf.cf_kind = Method MethNormal -> true
			| FStatic(_,{cf_kind = Method MethDynamic}) -> false
			| FEnum _ -> true
			| FDynamic ("cca" | "__Index" | "__s") -> true (* This is quite retarded, but we have to deal with this somehow... *)
			| _ -> match follow e.etype,follow e1.etype with
				| TFun _,TInst _ -> false
				| TFun _,_ -> true (* We don't know what's going on here, don't create a temp var (see #5082). *)
				| _ -> false
		in
		let rec loop fl e = match e.eexpr with
			| TField(e1,fa) when is_probably_not_affected e e1 fa ->
				loop ((fun e' -> {e with eexpr = TField(e',fa)}) :: fl) e1
			| TField(e1,fa) ->
				let fa = match fa with
					| FInstance(c,tl,({cf_kind = Method _ } as cf)) -> FClosure(Some(c,tl),cf)
					| _ -> fa
				in
				fl,{e with eexpr = TField(e1,fa)}
			| _ ->
				fl,e
		in
		let fl,e = loop [] e in
		let rec loop e = match e.eexpr with
			| TLocal v -> v.v_name
			| TArray(e1,_) | TField(e1,_) | TParenthesis e1 | TCast(e1,None) | TMeta(_,e1) -> loop e1
			| _ -> match ctx.name_stack with
				| s :: _ -> s
				| [] -> ctx.temp_var_name
		in
		let v = match v with Some v -> v | None -> alloc_var VGenerated (loop e) e.etype e.epos in
		let bb = declare_var_and_assign bb v e e.epos in
		let e = {e with eexpr = TLocal v} in
		let e = List.fold_left (fun e f -> f e) e fl in
		bb,e
	and declare_var_and_assign bb v e p =
		no_void v.v_type p;
		(* TODO: this section shouldn't be here because it can be handled as part of the normal value processing *)
		let rec loop bb e = match e.eexpr with
			| TParenthesis e1 ->
				loop bb e1
			| TBlock el ->
				let rec loop2 bb el = match el with
					| [e] ->
						bb,e
					| e1 :: el ->
						let bb = block_element bb e1 in
						if bb == g.g_unreachable then raise Exit;
						loop2 bb el
					| [] ->
						die "" __LOC__
				in
				let bb,e = loop2 bb el in
				loop bb e
			| _ ->
				bb,e
		in
		let generate bb e =
			let ev = mk (TLocal v) v.v_type p in
			let was_assigned = ref false in
			let assign e =
				if not !was_assigned then begin
					was_assigned := true;
					add_texpr bb (mk (TVar(v,None)) ctx.com.basic.tvoid ev.epos);
				end;
				mk (TBinop(OpAssign,ev,e)) ev.etype ev.epos
			in
			let close = push_name v.v_name in
			let bb = try
				block_element_plus bb (map_values assign e) (fun e -> mk (TVar(v,Some e)) ctx.com.basic.tvoid ev.epos)
			with Exit ->
				let bb,e = value bb e in
				add_texpr bb (mk (TVar(v,Some e)) ctx.com.basic.tvoid ev.epos);
				bb
			in
			close();
			bb
		in
		try
			let bb,e = loop bb e in
			generate bb e
		with Exit ->
			g.g_unreachable
	and block_element_plus bb (e,efinal) f =
		let bb = block_element bb e in
		let bb = match efinal with
			| Some e when bb != g.g_unreachable -> block_element bb (f e)
			| _ -> bb
		in
		bb
	and block_element_value bb e f =
		let e,efinal = map_values f e in
		block_element_plus bb (e,efinal) f
	and call bb e e1 el =
		let bb = ref bb in
		let check e t = match e.eexpr with
			| TLocal v when ExtType.has_reference_semantics t ->
				add_var_flag v VCaptured;
				e
			| _ ->
				if ExtType.has_variable_semantics t then begin
					let v = alloc_var VGenerated "tmp" t e.epos in
					let bb',e = bind_to_temp ~v:(Some v) !bb false e in
					bb := bb';
					e
				end else
					e
		in
		let el = Codegen.UnificationCallback.check_call check el e1.etype in
		let bb,el = ordered_value_list !bb (e1 :: el) in
		match el with
			| e1 :: el -> bb,{e with eexpr = TCall(e1,el)}
			| _ -> die "" __LOC__
	and array_assign_op bb op e ea e1 e2 e3 =
		let bb,e1 = bind_to_temp bb false e1 in
		let bb,e2 = bind_to_temp bb false e2 in
		let ea = {ea with eexpr = TArray(e1,e2)} in
		let bb,e4 = bind_to_temp bb false ea in
		let bb,e3 = bind_to_temp bb false e3 in
		let eop = {e with eexpr = TBinop(op,e4,e3)} in
		add_texpr bb {e with eexpr = TBinop(OpAssign,ea,eop)};
		bb,ea
	and field_assign_op bb op e ef e1 fa e2 =
		let bb,e1 = match fa with
			| FInstance(c,_,_) | FClosure(Some(c,_),_) when is_stack_allocated c -> bb,e1
			| _ -> bind_to_temp bb false e1
		in
		let ef = {ef with eexpr = TField(e1,fa)} in
		let bb,e3 = bind_to_temp bb false ef in
		let bb,e2 = bind_to_temp bb false e2 in
		let eop = {e with eexpr = TBinop(op,e3,e2)} in
		add_texpr bb {e with eexpr = TBinop(OpAssign,ef,eop)};
		bb,ef
	and block_element bb e = match e.eexpr with
		(* variables *)
		| TVar(v,None) ->
			add_texpr bb e;
			bb
		| TVar(v,Some e1) ->
			check_ref v e1;
			declare_var_and_assign bb v e1 e.epos
		| TBinop(OpAssign,({eexpr = TLocal v} as e1),e2) ->
			check_ref v e2;
			let assign e =
				mk (TBinop(OpAssign,e1,e)) e.etype e.epos
			in
			let close = push_name v.v_name in
			let bb = try
				block_element_value bb e2 assign
			with Exit ->
				let bb,e2 = value bb e2 in
				add_texpr bb {e with eexpr = TBinop(OpAssign,e1,e2)};
				bb
			in
			close();
			bb
		(* branching *)
		| TMeta((Meta.MergeBlock,_,_),{eexpr = TBlock el}) ->
			block_el bb el
		| TBlock [] when (ExtType.is_void (follow e.etype)) ->
			bb
		| TBlock el ->
			let bb_sub = create_node BKSub e.etype e.epos in
			add_cfg_edge bb bb_sub CFGGoto;
			close_node bb;
			let bb_sub_next = block_el bb_sub el in
			if bb_sub_next != g.g_unreachable then begin
				let bb_next = create_node BKNormal bb.bb_type bb.bb_pos in
				set_syntax_edge bb (SESubBlock(bb_sub,bb_next));
				add_cfg_edge bb_sub_next bb_next CFGGoto;
				close_node bb_sub_next;
				bb_next;
			end else begin
				set_syntax_edge bb (SEMerge bb_sub);
				close_node bb_sub_next;
				bb_sub_next
			end
		| TIf(e1,e2,None) ->
			let bb,e1 = bind_to_temp bb false e1 in
			if bb == g.g_unreachable then
				bb
			else begin
				let bb_then = create_node BKConditional e2.etype e2.epos in
				bb.bb_terminator <- TermCondBranch e1;
				add_cfg_edge bb bb_then (CFGCondBranch (mk (TConst (TBool true)) ctx.com.basic.tbool e2.epos));
				let bb_then_next = block bb_then e2 in
				let bb_next = create_node BKNormal bb.bb_type bb.bb_pos in
				set_syntax_edge bb (SEIfThen(bb_then,bb_next,e.epos));
				add_cfg_edge bb bb_next CFGCondElse;
				close_node bb;
				add_cfg_edge bb_then_next bb_next CFGGoto;
				close_node bb_then_next;
				bb_next
			end
		| TIf(e1,e2,Some e3) ->
			let bb,e1 = bind_to_temp bb false e1 in
			if bb == g.g_unreachable then
				bb
			else begin
				let bb_then = create_node BKConditional e2.etype e2.epos in
				let bb_else = create_node BKConditional e3.etype e3.epos in
				bb.bb_terminator <- TermCondBranch e1;
				add_cfg_edge bb bb_then (CFGCondBranch (mk (TConst (TBool true)) ctx.com.basic.tbool e2.epos));
				add_cfg_edge bb bb_else CFGCondElse;
				close_node bb;
				let bb_then_next = block bb_then e2 in
				let bb_else_next = block bb_else e3 in
				if bb_then_next == g.g_unreachable && bb_else_next == g.g_unreachable then begin
					set_syntax_edge bb (SEIfThenElse(bb_then,bb_else,g.g_unreachable,e.etype,e.epos));
					g.g_unreachable
				end else begin
					let bb_next = create_node BKNormal bb.bb_type bb.bb_pos in
					set_syntax_edge bb (SEIfThenElse(bb_then,bb_else,bb_next,e.etype,e.epos));
					add_cfg_edge bb_then_next bb_next CFGGoto;
					add_cfg_edge bb_else_next bb_next CFGGoto;
					close_node bb_then_next;
					close_node bb_else_next;
					bb_next
				end
			end
		| TSwitch(e1,cases,edef) ->
			let is_exhaustive = is_exhaustive e1 edef in
			let bb,e1 = bind_to_temp bb false e1 in
			bb.bb_terminator <- TermCondBranch e1;
			let reachable = ref [] in
			let make_case e =
				let bb_case = create_node BKConditional e.etype e.epos in
				let bb_case_next = block bb_case e in
				if bb_case_next != g.g_unreachable then
					reachable := bb_case_next :: !reachable;
				close_node bb_case_next;
				bb_case
			in
			let cases = List.map (fun (el,e) ->
				let bb_case = make_case e in
				List.iter (fun e -> add_cfg_edge bb bb_case (CFGCondBranch e)) el;
				el,bb_case
			) cases in
			let def = match edef with
				| None ->
					None
				| Some e ->
					let bb_case = make_case e in
					add_cfg_edge bb bb_case (CFGCondElse);
					Some (bb_case)
			in
			if is_exhaustive && !reachable = [] then begin
				set_syntax_edge bb (SESwitch(cases,def,g.g_unreachable,e.epos));
				close_node bb;
				g.g_unreachable;
			end else begin
				let bb_next = create_node BKNormal bb.bb_type bb.bb_pos in
				if not is_exhaustive then add_cfg_edge bb bb_next CFGGoto;
				List.iter (fun bb -> add_cfg_edge bb bb_next CFGGoto) !reachable;
				set_syntax_edge bb (SESwitch(cases,def,bb_next,e.epos));
				close_node bb;
				bb_next
			end
		| TWhile(e1,e2,NormalWhile) ->
			let bb_loop_pre = create_node BKLoopHead e1.etype e1.epos in
			add_cfg_edge bb bb_loop_pre CFGGoto;
			set_syntax_edge bb (SEMerge bb_loop_pre);
			close_node bb;
			let close = begin_loop bb bb_loop_pre in
			let bb_loop_body = create_node BKNormal e2.etype e2.epos in
			let bb_loop_body_next = block bb_loop_body e2 in
			let bb_breaks = close() in
			let bb_next = if bb_breaks = [] then begin
				(* The loop appears to be infinite, let's assume that something within it throws.
				   Otherwise DCE's mark-pass won't see its body and removes everything. *)
				add_cfg_edge bb_loop_body bb_exit CFGMaybeThrow;
				g.g_unreachable
			end else
				create_node BKNormal bb.bb_type bb.bb_pos
			in
			List.iter (fun bb -> add_cfg_edge bb bb_next CFGGoto) bb_breaks;
			set_syntax_edge bb_loop_pre (SEWhile(bb_loop_body,bb_next,e.epos));
			bb_loop_pre.bb_terminator <- TermCondBranch e1;
			if bb_loop_body_next != g.g_unreachable then add_cfg_edge bb_loop_body_next bb_loop_pre CFGGoto;
			add_cfg_edge bb_loop_pre bb_loop_body CFGGoto;
			close_node bb_loop_body_next;
			close_node bb_loop_pre;
			bb_next;
		| TTry(e1,catches) ->
			let bb_try = create_node BKNormal e1.etype e1.epos in
			let bb_exc = create_node BKException t_dynamic e.epos in
			add_cfg_edge bb bb_try CFGGoto;
			let close = begin_try bb_exc in
			let bb_try_next = block bb_try e1 in
			close();
			(* We always want to keep catch-blocks, so let's add a pseudo CFG edge if it's unreachable. *)
			if bb_exc.bb_incoming = [] then add_cfg_edge (if bb_try_next == g.g_unreachable then bb_try else bb_try_next) bb_exc CFGMaybeThrow;
			let is_reachable = ref (not (bb_try_next == g.g_unreachable)) in
			let catches = List.map (fun (v,e) ->
				let bb_catch = create_node (BKCatch v) e.etype e.epos in
				add_cfg_edge bb_exc bb_catch CFGGoto;
				let bb_catch_next = block bb_catch e in
				is_reachable := !is_reachable || (not (bb_catch_next == g.g_unreachable));
				v,bb_catch,bb_catch_next
			) catches in
			let bb_next = if !is_reachable then create_node BKNormal bb.bb_type bb.bb_pos else g.g_unreachable in
			let catches = List.map (fun (v,bb_catch,bb_catch_next) ->
				if bb_catch_next != g.g_unreachable then add_cfg_edge bb_catch_next bb_next CFGGoto;
				close_node bb_catch_next;
				v,bb_catch
			) catches in
			set_syntax_edge bb (SETry(bb_try,bb_exc,catches,bb_next,e.epos));
			if bb_try_next != g.g_unreachable then add_cfg_edge bb_try_next bb_next CFGGoto;
			close_node bb_try_next;
			close_node bb_exc;
			close_node bb;
			bb_next
		(* control flow *)
		| TReturn None ->
			add_cfg_edge bb bb_exit CFGGoto;
			add_terminator bb (TermReturn e.epos)
		| TReturn (Some e1) when ExtType.is_void (follow e1.etype) ->
			let bb = block_element bb e1 in
			block_element bb (mk (TReturn None) t_dynamic e.epos)
		| TReturn (Some e1) ->
			begin try
				let mk_return e1 = mk (TReturn (Some e1)) t_dynamic e1.epos in
				block_element_value bb e1 mk_return
			with Exit ->
				let bb,e1 = value bb e1 in
				add_cfg_edge bb bb_exit CFGGoto;
				add_terminator bb (TermReturnValue(e1,e.epos))
			end
		| TBreak ->
			bb_breaks := bb :: !bb_breaks;
			add_terminator bb (TermBreak e.epos)
		| TContinue ->
			begin match !bb_continue with
				| Some bb_continue -> add_cfg_edge bb bb_continue CFGGoto
				| _ -> die "" __LOC__
			end;
			add_terminator bb (TermContinue e.epos)
		| TThrow e1 ->
			begin try
				let mk_throw e1 =
					mk (TThrow e1) t_dynamic e.epos
				in
				block_element_value bb e1 mk_throw
			with Exit ->
				let bb,e1 = value bb e1 in
				begin match !b_try_stack with
					| [] -> add_cfg_edge bb bb_exit CFGGoto
					| _ -> List.iter (fun bb_exc -> add_cfg_edge bb bb_exc CFGGoto) !b_try_stack;
				end;
				add_terminator bb (TermThrow(e1,e.epos))
			end
		(* side_effects *)
		| TCall({eexpr = TIdent s},el) when is_really_unbound s ->
			check_unbound_call s el;
			add_texpr bb e;
			bb
		| TCall(e1,el) ->
			let bb,e = call bb e e1 el in
			add_texpr bb e;
			bb
		| TNew(c,tl,el) ->
			let bb,el = ordered_value_list bb el in
			add_texpr bb {e with eexpr = TNew(c,tl,el)};
			bb
		| TCast(e1,Some mt) ->
			let bb,e1 = value bb e1 in
			add_texpr bb {e with eexpr = TCast(e1,Some mt)};
			bb
		| TBinop(OpAssignOp op,({eexpr = TArray(e1,e2)} as ea),e3) ->
			let bb,_ = array_assign_op bb op e ea e1 e2 e3 in
			bb
		| TBinop(OpAssignOp op,({eexpr = TField(e1,fa)} as ef),e2) ->
			let bb,_ = field_assign_op bb op e ef e1 fa e2 in
			bb
		| TBinop(OpAssign,({eexpr = TArray(e1,e2)} as ea),e3) ->
			let bb,e1,e2,e3 = match ordered_value_list bb [e1;e2;e3] with
				| bb,[e1;e2;e3] -> bb,e1,e2,e3
				| _ -> die "" __LOC__
			in
			add_texpr bb {e with eexpr = TBinop(OpAssign,{ea with eexpr = TArray(e1,e2)},e3)};
			bb
		| TBinop((OpAssign | OpAssignOp _ as op),e1,e2) ->
			let bb,e1 = value bb e1 in
			let bb,e2 = value bb e2 in
			add_texpr bb {e with eexpr = TBinop(op,e1,e2)};
			bb
		| TUnop((Increment | Decrement as op),flag,e1) ->
			let bb,e1 = value bb e1 in
			add_texpr bb {e with eexpr = TUnop(op,flag,e1)};
			bb
		| TLocal _ when not ctx.config.AnalyzerConfig.local_dce ->
			add_texpr bb e;
			bb
		| TField (e1,fa) when has_side_effect e ->
			let bb,e1 = value bb e1 in
			add_texpr bb {e with eexpr = TField(e1,fa)};
			bb
		(* no-side-effect *)
		| TEnumParameter _ | TEnumIndex _ | TFunction _ | TConst _ | TTypeExpr _ | TLocal _ | TIdent _ ->
			bb
		(* no-side-effect composites *)
		| TParenthesis e1 | TMeta(_,e1) | TCast(e1,None) | TField(e1,_) | TUnop(_,_,e1) ->
			block_element bb e1
		| TArray(e1,e2) | TBinop(_,e1,e2) ->
			let bb = block_element bb e1 in
			block_element bb e2
		| TArrayDecl el ->
			block_el bb el
		| TObjectDecl fl ->
			block_el bb (List.map snd fl)
		| TFor _ | TWhile(_,_,DoWhile) ->
			die "" __LOC__
	and block_el bb el =
		match !b_try_stack with
		| [] ->
			let rec loop bb el = match el with
				| [] -> bb
				| e :: el ->
					let bb = block_element bb e in
					if bb == g.g_unreachable then bb else loop bb el
			in
			loop bb el
		| bbl ->
			let rec loop bb el = match el with
				| [] -> bb
				| e :: el ->
					let bb = if not (can_throw e) then
						block_element bb e
					else begin
						let bb' = create_node BKNormal e.etype e.epos in
						add_cfg_edge bb bb' CFGGoto;
						List.iter (fun bb_exc -> add_cfg_edge bb bb_exc CFGMaybeThrow) bbl;
						set_syntax_edge bb (SEMerge bb');
						close_node bb;
						block_element bb' e
					end in
					if bb == g.g_unreachable then bb else loop bb el
			in
			loop bb el
	and block bb e =
		let el = match e.eexpr with
			| TBlock el -> el
			| _ -> [e]
		in
		block_el bb el
	in
	let bb_last = block bb_root tf.tf_expr in
	close_node bb_last;
	add_cfg_edge bb_last bb_exit CFGGoto; (* implied return *)
	close_node bb_exit;
	bb_root,bb_exit

let from_tfunction ctx tf t p =
	let g = ctx.graph in
	let bb_func,bb_exit = func ctx g.g_root tf t p in
	ctx.entry <- bb_func;
	close_node g.g_root;
	g.g_exit <- bb_exit

let terminator_to_texpr_maybe = function
	| TermReturn p -> Some (mk (TReturn None) t_dynamic p)
	| TermBreak p -> Some (mk TBreak t_dynamic p)
	| TermContinue p -> Some (mk TContinue t_dynamic p)
	| TermReturnValue(e1,p) -> Some (mk (TReturn (Some e1)) t_dynamic p)
	| TermThrow(e1,p) -> Some (mk (TThrow e1) t_dynamic p)
	| TermCondBranch e1 -> Some e1 (* TODO: this shouldn't be here *)
	| _ -> None

let rec block_to_texpr_el ctx bb =
	if bb.bb_dominator == ctx.graph.g_unreachable then
		[]
	else begin
		let block bb = block_to_texpr ctx bb in
		let live bb = not ctx.did_optimize || not ctx.config.local_dce || has_block_flag bb BlockDce in
		let if_live bb = if live bb then Some bb else None in
		let rec loop bb se =
			let get_terminator() = match bb.bb_terminator with
				| TermCondBranch e1 -> e1
				| _ -> die "" __LOC__
			in
			match se with
			| SESubBlock(bb_sub,bb_next) ->
				Some bb_next,Some (block bb_sub)
			| SEMerge bb_next ->
				Some bb_next,None
			| SENone ->
				None,terminator_to_texpr_maybe bb.bb_terminator
			| SETry(bb_try,_,bbl,bb_next,p) ->
				if_live bb_next,Some (mk (TTry(block bb_try,List.map (fun (v,bb) -> v,block bb) bbl)) ctx.com.basic.tvoid p)
			| SEIfThen(bb_then,bb_next,p) ->
				if_live bb_next,Some (mk (TIf(get_terminator(),block bb_then,None)) ctx.com.basic.tvoid p)
			| SEIfThenElse(bb_then,bb_else,bb_next,t,p) ->
				if_live bb_next,Some (mk (TIf(get_terminator(),block bb_then,Some (block bb_else))) t p)
			| SEWhile(bb_body,bb_next,p) ->
				let e2 = block bb_body in
				if_live bb_next,Some (mk (TWhile(get_terminator(),e2,NormalWhile)) ctx.com.basic.tvoid p)
			| SESwitch(bbl,bo,bb_next,p) ->
				Some bb_next,Some (mk (TSwitch(get_terminator(),List.map (fun (el,bb) -> el,block bb) bbl,Option.map block bo)) ctx.com.basic.tvoid p)
		in
		let bb_next,e_term = loop bb bb.bb_syntax_edge in
		let el = DynArray.to_list bb.bb_el in
		let el = match e_term with
			| None -> el
			| Some e -> el @ [e]
		in
		let el = match bb_next with
			| None -> el
			| Some bb -> el @ (block_to_texpr_el ctx bb)
		in
		el
	end

and block_to_texpr ctx bb =
	assert(bb.bb_closed);
	let el = block_to_texpr_el ctx bb in
	let e = mk (TBlock el) bb.bb_type bb.bb_pos in
	e

and func ctx i =
	let bb,t,p,tf = Hashtbl.find ctx.graph.g_functions i in
	let e = block_to_texpr ctx bb in
	let rec loop e = match e.eexpr with
		| TLocal v ->
			{e with eexpr = TLocal (get_var_origin ctx.graph v)}
		| TVar(v,eo) ->
			let eo = Option.map loop eo in
			let v' = get_var_origin ctx.graph v in
			{e with eexpr = TVar(v',eo)}
		| TBinop(OpAssign,e1,({eexpr = TBinop(op,e2,e3)} as e4)) when target_handles_assign_ops ctx.com e3 ->
			let e1 = loop e1 in
			let e2 = loop e2 in
			let e3 = loop e3 in
			let is_valid_assign_op = function
				| OpAdd | OpMult | OpDiv | OpSub | OpAnd
				| OpOr | OpXor | OpShl | OpShr | OpUShr | OpMod ->
					true
				| OpAssignOp _ | OpInterval | OpArrow | OpIn | OpNullCoal | OpAssign | OpEq
				| OpNotEq | OpGt | OpGte | OpLt | OpLte | OpBoolAnd | OpBoolOr ->
					false
			in
			begin match e1.eexpr,e2.eexpr with
				| TLocal v1,TLocal v2 when v1 == v2 && not (has_var_flag v1 VCaptured) && is_valid_assign_op op ->
					begin match op,e3.eexpr with
						| (OpAdd|OpSub) as op,TConst (TInt i32) when Int32.to_int i32 = 1 && ExtType.is_numeric (Abstract.follow_with_abstracts v1.v_type) ->
							let op = match op with
								| OpAdd -> Increment
								| OpSub -> Decrement
								| _ -> die "" __LOC__
							in
							{e with eexpr = TUnop(op,Prefix,e1)}
						| _ -> {e with eexpr = TBinop(OpAssignOp op,e1,e3)}
					end
				| _ ->
					{e with eexpr = TBinop(OpAssign,e1,{e4 with eexpr = TBinop(op,e2,e3)})}
			end
		| TCall({eexpr = TConst (TString "fun")},[{eexpr = TConst (TInt i32)}]) ->
			func ctx (Int32.to_int i32)
		| TCall({eexpr = TIdent s},_) when is_really_unbound s ->
			e
		| _ ->
			Type.map_expr loop e
	in
	let e = loop e in
	mk (TFunction {tf with tf_expr = e}) t p

let to_texpr ctx =
	func ctx ctx.entry.bb_id
