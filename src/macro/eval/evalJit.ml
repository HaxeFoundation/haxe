(*
	The Haxe Compiler
	Copyright (C) 2005-2017  Haxe Foundation

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
open EvalValue
open EvalContext
open EvalHash
open EvalEmitter

(* Helper *)

let rope_path t = match follow t with
	| TInst({cl_path=path},_) | TEnum({e_path=path},_) | TAbstract({a_path=path},_) -> Rope.of_string (s_type_path path)
	| TDynamic _ -> Rope.of_string "Dynamic"
	| TFun _ | TAnon _ | TMono _ | TType _ | TLazy _ -> assert false

let eone = mk (TConst(TInt (Int32.one))) t_dynamic null_pos

let eval_const = function
	| TString s -> vstring (Rope.of_string s)
	| TInt i32 -> vint32 i32
	| TFloat f -> vfloat (float_of_string f)
	| TBool b -> vbool b
	| TNull -> vnull
	| TThis | TSuper -> assert false

let get_binop_fun op p = match op with
	| OpAdd -> op_add
	| OpMult -> op_mult p
	| OpDiv -> op_div p
	| OpSub -> op_sub p
	| OpEq -> op_eq
	| OpNotEq -> op_not_eq
	| OpGt -> op_gt
	| OpGte -> op_gte
	| OpLt -> op_lt
	| OpLte -> op_lte
	| OpAnd -> op_and p
	| OpOr -> op_or p
	| OpXor -> op_xor p
	| OpShl -> op_shl p
	| OpShr -> op_shr p
	| OpUShr -> op_ushr p
	| OpMod -> op_mod p
	| OpAssign | OpBoolAnd | OpBoolOr | OpAssignOp _ | OpInterval | OpArrow -> assert false

open EvalJitContext

let rec op_assign ctx jit e1 e2 = match e1.eexpr with
	| TLocal var ->
		let exec = jit_expr false jit e2 in
		if var.v_capture then emit_capture_write (get_capture_slot jit var.v_id) exec
		else emit_local_write (get_slot jit var.v_id e1.epos) exec
	| TField(e1,fa) ->
		let name = hash_s (field_name fa) in
		let exec1 = jit_expr false jit e1 in
		let exec2 = jit_expr false jit e2 in
		begin match fa with
			| FStatic({cl_path=path},_) | FEnum({e_path=path},_) ->
				let proto = get_static_prototype jit.ctx (path_hash path) e1.epos in
				emit_proto_field_write proto (get_proto_field_index proto name) exec2
			| FInstance({cl_path=([],"Array")},_,{cf_name="length"}) ->
				emit_array_length_write exec1 exec2
			| FInstance(c,_,_) when not c.cl_interface ->
				let proto = get_instance_prototype jit.ctx (path_hash c.cl_path) e1.epos in
				let i = get_instance_field_index proto name in
				emit_instance_field_write exec1 i exec2
			| FAnon cf ->
				begin match follow e1.etype with
					| TAnon an ->
						let l = PMap.foldi (fun k _ acc -> (hash_s k,()) :: acc) an.a_fields [] in
						let proto,_ = ctx.get_object_prototype ctx l in
						let i = get_instance_field_index proto name in
						emit_anon_field_write exec1 proto i name exec2
					| _ ->
						emit_field_write exec1 name exec2
				end
			| _ ->
				emit_field_write exec1 name exec2
		end
	| TArray(ea1,ea2) ->
		begin match ea1.eexpr with
		| TLocal var when not var.v_capture ->
			let exec2 = jit_expr false jit ea2 in
			let exec3 = jit_expr false jit e2 in
			emit_array_local_write (get_slot jit var.v_id ea1.epos) exec2 exec3 ea2.epos
		| _ ->
			let exec1 = jit_expr false jit ea1 in
			let exec2 = jit_expr false jit ea2 in
			let exec3 = jit_expr false jit e2 in
			emit_array_write exec1 exec2 exec3 ea2.epos
		end
	| _ ->
		assert false

and op_assign_op jit op e1 e2 prefix = match e1.eexpr with
	| TLocal var ->
		let exec = jit_expr false jit e2 in
		if var.v_capture then emit_capture_read_write (get_capture_slot jit var.v_id) exec op prefix
		else emit_local_read_write (get_slot jit var.v_id e1.epos) exec op prefix
	| TField(e1,fa) ->
		let name = hash_s (field_name fa) in
		let exec1 = jit_expr false jit e1 in
		let exec2 = jit_expr false jit e2 in
		begin match fa with
			| FStatic({cl_path=path},_) ->
				let proto = get_static_prototype jit.ctx (path_hash path) e1.epos in
				emit_proto_field_read_write proto (get_proto_field_index proto name) exec2 op prefix
			| FInstance(c,_,_) when not c.cl_interface ->
				let proto = get_instance_prototype jit.ctx (path_hash c.cl_path) e1.epos in
				let i = get_instance_field_index proto name in
				emit_instance_field_read_write exec1 i exec2 op prefix
			| _ ->
				emit_field_read_write exec1 name exec2 op prefix
		end
	| TArray(ea1,ea2) ->
		begin match ea1.eexpr with
			| TLocal var when not var.v_capture ->
				let exec2 = jit_expr false jit ea2 in
				let exec3 = jit_expr false jit e2 in
				emit_array_local_read_write (get_slot jit var.v_id ea1.epos) exec2 exec3 op prefix ea2.epos
			| _ ->
				let exec1 = jit_expr false jit ea1 in
				let exec2 = jit_expr false jit ea2 in
				let exec3 = jit_expr false jit e2 in
				emit_array_read_write exec1 exec2 exec3 op prefix ea2.epos
		end
	| _ ->
		assert false

and op_incr jit e1 prefix p = match e1.eexpr with
	| TLocal var ->
		begin match var.v_capture,prefix with
			| true,true -> emit_capture_incr_prefix (get_capture_slot jit var.v_id)
			| true,false -> emit_capture_incr_postfix (get_capture_slot jit var.v_id)
			| false,true -> emit_local_incr_prefix (get_slot jit var.v_id e1.epos)
			| false,false -> emit_local_incr_postfix (get_slot jit var.v_id e1.epos)
		end
	| _ ->
		op_assign_op jit (get_binop_fun OpAdd p) e1 eone prefix

and op_decr jit e1 prefix p = match e1.eexpr with
	| TLocal var ->
		begin match var.v_capture,prefix with
			| true,true -> emit_capture_decr_prefix (get_capture_slot jit var.v_id)
			| true,false -> emit_capture_decr_postfix (get_capture_slot jit var.v_id)
			| false,true -> emit_local_decr_prefix (get_slot jit var.v_id e1.epos)
			| false,false -> emit_local_decr_postfix (get_slot jit var.v_id e1.epos)
		end
	| _ ->
		op_assign_op jit (get_binop_fun OpSub p) e1 eone prefix

and unop jit op flag e1 p =
	match op with
	| Not ->
		let exec = jit_expr false jit e1 in
		emit_not exec
	| Neg ->
		let exec = jit_expr false jit e1 in
		emit_neg exec p
	| NegBits ->
		let exec = jit_expr false jit e1 in
		emit_op_sub p (fun _ -> vint32 (Int32.minus_one)) exec
	| Increment ->
		op_incr jit e1 (flag = Prefix) p
	| Decrement ->
		op_decr jit e1 (flag = Prefix) p

(*
	This is the main jit function. It turns expression [e] into a function, which can be
	executed int an environment of type [EvalContext.env].
*)
and jit_expr return jit e =
	let ctx = jit.ctx in
	let rec loop e = match e.eexpr with
	(* objects and values *)
	| TVar(var,eo) ->
		let varacc = declare_local jit var in
		let exec = match eo with
			| None -> emit_null
			| Some e -> jit_expr false jit e
		in
		begin match varacc with
			| Local slot -> emit_local_declaration slot exec
			| Env slot -> emit_capture_declaration slot exec
		end
	| TConst TThis ->
		emit_local_read (get_slot jit 0 e.epos)
	| TConst ct ->
		emit_const (eval_const ct)
	| TObjectDecl fl ->
		let fl = List.map (fun (s,e) -> hash_s s,jit_expr false jit e) fl in
		let proto,_ = ctx.get_object_prototype ctx fl in
		let fl = List.map (fun (s,exec) -> get_instance_field_index proto s,exec) fl in
		let fa = Array.of_list fl in
		emit_object_declaration proto fa
	| TArrayDecl el ->
		let execs = List.map (jit_expr false jit) el in
		let execs = Array.of_list execs in
		emit_array_declaration execs
	| TTypeExpr mt ->
		let key = path_hash (t_infos mt).mt_path in
		let proto = get_static_prototype_as_value jit.ctx key e.epos in
		emit_type_expr proto
	| TFunction tf ->
		let jit_closure = EvalJitContext.create ctx jit.file_key in
		jit_closure.captures <- jit.captures;
		push_scope jit_closure;
		let varaccs = List.map (fun (var,_) -> declare_local jit_closure var) tf.tf_args in
		let exec = jit_expr true jit_closure tf.tf_expr in
		pop_scope jit_closure;
		let args = List.map (fun (_,cto) -> Option.map_default eval_const vnull cto) tf.tf_args in
		jit.num_closures <- jit.num_closures + 1;
		let num_captures = Hashtbl.length jit.captures in
		let info = create_env_info (hash_s e.epos.pfile) (EKLocalFunction jit.num_closures) jit_closure.caught_types in
		emit_closure jit.ctx info jit_closure.has_nonfinal_return jit_closure.max_local_count num_captures varaccs args exec
	(* branching *)
	| TIf(e1,e2,eo) ->
		let exec_cond = jit_expr false jit e1 in
		let exec_then = jit_expr return jit e2 in
		let exec_else = match eo with
			| None -> emit_null
			| Some e -> jit_expr return jit e
		in
		emit_if exec_cond exec_then exec_else
	| TSwitch(e1,cases,def) ->
		let exec = jit_expr false jit e1 in
		let execs = DynArray.create () in
		let constants = DynArray.create () in
		let patterns = DynArray.create () in
		let is_complex = ref false in
		(* This is slightly insane... *)
		List.iter (fun (el,e) ->
			push_scope jit;
			begin try
				if !is_complex then raise Exit;
				let el = List.map (fun e -> match e.eexpr with
					| TConst ct -> eval_const ct
					| _ -> raise Exit
				) el in
				DynArray.add constants el
			with Exit ->
				is_complex := true;
				let el = List.map (jit_expr false jit) el in
				DynArray.add patterns el
			end;
			DynArray.add execs (jit_expr return jit e);
			pop_scope jit;
		) cases;
		let exec_def = match def with
			| None ->
				emit_null
			| Some e ->
				push_scope jit;
				let exec = jit_expr return jit e in
				pop_scope jit;
				exec
		in
		if !is_complex then begin
			let l = DynArray.length constants in
			let all_patterns = Array.init (l + DynArray.length patterns) (fun i ->
				if i >= l then DynArray.get patterns (i - l) else (List.map (fun ct -> fun _ -> ct) (DynArray.get constants i))
			) in
			emit_switch exec (DynArray.to_array execs) all_patterns exec_def
		end else begin
			emit_constant_switch exec (DynArray.to_array execs) (DynArray.to_array constants) exec_def
		end
	| TWhile({eexpr = TParenthesis e1},e2,flag) ->
		loop {e with eexpr = TWhile(e1,e2,flag)}
	| TWhile({eexpr = TBinop(OpLt,{eexpr = TLocal v;epos=pv},eto)},e2,NormalWhile) when (Meta.has Meta.ForLoopVariable v.v_meta) ->
		let has_break = ref false in
		let has_continue = ref false in
		let rec loop e = match e.eexpr with
			| TUnop(Increment,_,({eexpr = TLocal v'} as e1)) when v == v' -> e1
			| TWhile _ | TFor _ -> e
			| TBreak -> has_break := true; e
			| TContinue -> has_continue := true; e
			| _ -> Type.map_expr loop e
		in
		let e2 = loop e2 in
		let slot = get_slot jit v.v_id pv in
		let exec1 = jit_expr false jit eto in
		let exec2 = jit_expr false jit e2 in
		begin match !has_break,!has_continue with
			| false,false -> emit_int_iterator slot exec1 exec2
			| true,false -> emit_int_iterator_break slot exec1 exec2
			| false,true -> emit_int_iterator_continue slot exec1 exec2
			| true,true -> emit_int_iterator_break_continue slot exec1 exec2
		end
	| TWhile(e1,e2,flag) ->
		let has_break = ref false in
		let has_continue = ref false in
		let rec loop e = match e.eexpr with
			| TContinue -> has_continue := true; if !has_break then raise Exit
			| TBreak -> has_break := true; if !has_continue then raise Exit
			| TFunction _ | TWhile _ | TFor _ -> ()
			| _ -> Type.iter loop e
		in
		(try loop e2 with Exit -> ());
		begin match e1.eexpr with
			| TBinop(OpGte,e1,{eexpr = TConst (TFloat s)}) when not !has_break && not !has_continue && flag = NormalWhile ->
				let f = float_of_string s in
				let exec1 = jit_expr false jit e1 in
				let exec2 = jit_expr false jit e2 in
				emit_while_gte exec1 f exec2
			| _ ->
				let exec_cond = jit_expr false jit e1 in
				let exec_body = jit_expr false jit e2 in
				(* This is a bit moronic, but it does avoid run-time branching and setting up some exception
					handlers for break/continue, so it might be worth it... *)
				begin match flag,!has_break,!has_continue with
					| NormalWhile,false,false -> emit_while exec_cond exec_body
					| NormalWhile,true,false -> emit_while_break exec_cond exec_body
					| NormalWhile,false,true -> emit_while_continue exec_cond exec_body
					| NormalWhile,true,true -> emit_while_break_continue exec_cond exec_body
					| DoWhile,false,false -> emit_do_while exec_cond exec_body
					| DoWhile,true,false -> emit_do_while_break exec_cond exec_body
					| DoWhile,false,true -> emit_do_while_continue exec_cond exec_body
					| DoWhile,true,true -> emit_do_while_break_continue exec_cond exec_body
				end
		end
	| TTry(e1,catches) ->
		let exec = jit_expr return jit e1 in
		let catches = List.map (fun (var,e) ->
			push_scope jit;
			let varacc = declare_local jit var in
			let exec = jit_expr return jit e in
			pop_scope jit;
			let key = hash (rope_path var.v_type) in
			if ctx.debug.support_debugger then Hashtbl.replace jit.caught_types key true;
			exec,key,varacc
		) catches in
		emit_try exec catches
	(* control flow *)
	| TBlock [] ->
		emit_null
	| TBlock [e1] ->
		loop e1
	| TBlock [e1;e2] ->
		push_scope jit;
		let exec1 = jit_expr false jit e1 in
		let exec2 = jit_expr return jit e2 in
		pop_scope jit;
		emit_block2 exec1 exec2
	| TBlock [e1;e2;e3] ->
		push_scope jit;
		let exec1 = jit_expr false jit e1 in
		let exec2 = jit_expr false jit e2 in
		let exec3 = jit_expr return jit e3 in
		pop_scope jit;
		emit_block3 exec1 exec2 exec3
	| TBlock [e1;e2;e3;e4] ->
		push_scope jit;
		let exec1 = jit_expr false jit e1 in
		let exec2 = jit_expr false jit e2 in
		let exec3 = jit_expr false jit e3 in
		let exec4 = jit_expr return jit e4 in
		pop_scope jit;
		emit_block4 exec1 exec2 exec3 exec4
	| TBlock [e1;e2;e3;e4;e5] ->
		push_scope jit;
		let exec1 = jit_expr false jit e1 in
		let exec2 = jit_expr false jit e2 in
		let exec3 = jit_expr false jit e3 in
		let exec4 = jit_expr false jit e4 in
		let exec5 = jit_expr return jit e5 in
		pop_scope jit;
		emit_block5 exec1 exec2 exec3 exec4 exec5
	| TBlock el ->
		let d = DynArray.create () in
		let add = DynArray.add d in
		let rec loop el = match el with
			| e1 :: e2 :: e3 :: e4 :: e5 :: el ->
				let exec1 = jit_expr false jit e1 in
				let exec2 = jit_expr false jit e2 in
				let exec3 = jit_expr false jit e3 in
				let exec4 = jit_expr false jit e4 in
				let exec5 = jit_expr (return && el = []) jit e5 in
				add (emit_block5 exec1 exec2 exec3 exec4 exec5);
				loop el
			| e1 :: e2 :: e3 :: e4 :: el ->
				let exec1 = jit_expr false jit e1 in
				let exec2 = jit_expr false jit e2 in
				let exec3 = jit_expr false jit e3 in
				let exec4 = jit_expr (return && el = []) jit e4 in
				add (emit_block4 exec1 exec2 exec3 exec4);
				loop el
			| e1 :: e2 :: e3 :: el ->
				let exec1 = jit_expr false jit e1 in
				let exec2 = jit_expr false jit e2 in
				let exec3 = jit_expr (return && el = []) jit e3 in
				add (emit_block3 exec1 exec2 exec3);
				loop el
			| e1 :: e2 :: el ->
				let exec1 = jit_expr false jit e1 in
				let exec2 = jit_expr (return && el = []) jit e2 in
				add (emit_block2 exec1 exec2);
				loop el
			| [e1] ->
				let exec1 = jit_expr return jit e1 in
				add (emit_block1 exec1);
			| [] ->
				()
		in
		push_scope jit;
		loop el;
		pop_scope jit;
		emit_block (DynArray.to_array d)
	| TReturn None ->
		if return then emit_null
		else begin
			jit.has_nonfinal_return <- true;
			emit_return_null
		end
	| TReturn (Some e1) ->
		let exec = jit_expr false jit e1 in
		if return then emit_value exec
		else begin
			jit.has_nonfinal_return <- true;
			emit_return_value exec
		end
	| TBreak ->
		emit_break
	| TContinue ->
		emit_continue
	| TThrow e1 ->
		let exec = jit_expr false jit e1 in
		emit_throw exec e.epos
	| TCast(e1,Some mt) ->
		let exec = jit_expr false jit e1 in
		let t = type_of_module_type mt in
		emit_safe_cast exec (hash (rope_path t)) e.epos
	(* calls *)
	| TCall(e1,el) ->
		begin match e1.eexpr with
		| TField({eexpr = TConst TSuper;epos=pv},FInstance(c,_,cf)) ->
			let proto = get_instance_prototype ctx (path_hash c.cl_path) e1.epos in
			let name = hash_s cf.cf_name in
			let i = get_proto_field_index proto name in
			let slot = get_slot jit 0 pv in
			let execs = List.map (jit_expr false jit) el in
			emit_super_field_call slot proto i execs e.epos
		| TField(ef,fa) ->
			let name = hash_s (field_name fa) in
			let execs = List.map (jit_expr false jit) el in
			let is_overridden c s_name =
				try
					Hashtbl.find ctx.overrides (c.cl_path,s_name)
				with Not_found ->
					false
			in
			let is_proper_method cf = match cf.cf_kind with
				| Method MethDynamic -> false
				| Method _ -> true
				| Var _ -> false
			in
			begin match fa with
				| FStatic({cl_path=[],"Type"},{cf_name="enumIndex"}) ->
					begin match execs with
						| [exec] -> emit_enum_index exec
						| _ -> assert false
					end
				| FEnum({e_path=path},ef) ->
					let key = path_hash path in
					let pos = Some ef.ef_pos in
					begin match execs with
						| [] -> emit_enum_construction0 key ef.ef_index pos
						| [exec1] -> emit_enum_construction1 key ef.ef_index exec1 pos
						| [exec1;exec2] -> emit_enum_construction2 key ef.ef_index exec1 exec2 pos
						| [exec1;exec2;exec3] -> emit_enum_construction3 key ef.ef_index exec1 exec2 exec3 pos
						| [exec1;exec2;exec3;exec4] -> emit_enum_construction4 key ef.ef_index exec1 exec2 exec3 exec4 pos
						| [exec1;exec2;exec3;exec4;exec5] -> emit_enum_construction5 key ef.ef_index exec1 exec2 exec3 exec4 exec5 pos
						| _ -> emit_enum_construction key ef.ef_index (Array.of_list execs) pos
					end
				| FStatic({cl_path=path},cf) when is_proper_method cf ->
					let proto = get_static_prototype ctx (path_hash path) ef.epos in
					let i = get_proto_field_index proto name in
					emit_proto_field_call proto i execs e.epos
				| FInstance(c,_,cf) when is_proper_method cf && not (is_overridden c cf.cf_name) && not c.cl_interface ->
					let exec = jit_expr false jit ef in
					let proto = get_instance_prototype ctx (path_hash c.cl_path) ef.epos in
					let i = get_proto_field_index proto name in
					emit_proto_field_call proto i (exec :: execs) e.epos
				| FInstance(_,_,cf) when is_proper_method cf ->
					let exec = jit_expr false jit ef in
					begin match execs with
						| [] -> emit_method_call0 exec name e.epos
						| [exec1] -> emit_method_call1 exec name exec1 e.epos
						| [exec1;exec2] -> emit_method_call2 exec name exec1 exec2 e.epos
						| [exec1;exec2;exec3] -> emit_method_call3 exec name exec1 exec2 exec3 e.epos
						| [exec1;exec2;exec3;exec4] -> emit_method_call4 exec name exec1 exec2 exec3 exec4 e.epos
						| _ -> emit_method_call exec name execs e.epos
					end
				| _ ->
					let exec = jit_expr false jit ef in
					emit_field_call exec name execs e.epos
			end
		| TConst TSuper ->
			begin match follow e1.etype with
			| TInst(c,_) ->
				let key = (path_hash c.cl_path) in
				let execs = List.map (jit_expr false jit) el in
				let fnew = get_instance_constructor jit.ctx key e1.epos in
				emit_super_call fnew execs e.epos
			| _ -> assert false
			end
		| _ ->
			match e1.eexpr,el with
			| TLocal({v_name = "$__mk_pos__"}),[file;min;max] ->
				let exec1 = jit_expr false jit file in
				let exec2 = jit_expr false jit min in
				let exec3 = jit_expr false jit max in
				emit_mk_pos exec1 exec2 exec3
			| TLocal({v_name = "$__delayed_call__"}),[{eexpr = TConst(TInt i)}] ->
				let f = ctx.curapi.MacroApi.delayed_macro (Int32.to_int i) in
				(fun env ->
					let f = f() in
					f()
				)
			| _ ->
				let exec = jit_expr false jit e1 in
				let execs = List.map (jit_expr false jit) el in
				begin match execs with
					| [] -> emit_call0 exec e.epos
					| [exec1] -> emit_call1 exec exec1 e.epos
					| [exec1;exec2] -> emit_call2 exec exec1 exec2 e.epos
					| [exec1;exec2;exec3] -> emit_call3 exec exec1 exec2 exec3 e.epos
					| [exec1;exec2;exec3;exec4] -> emit_call4 exec exec1 exec2 exec3 exec4 e.epos
					| _ -> emit_call exec execs e.epos
				end
		end
	| TNew({cl_path=[],"Array"},_,_) ->
		emit_new_array
	| TNew(c,_,el) ->
		let execs = List.map (jit_expr false jit) el in
		let key = path_hash c.cl_path in
		begin try
			let f = get_special_instance_constructor_raise ctx key in
			emit_special_instance f execs
		with Not_found ->
			let fnew = get_instance_constructor jit.ctx key e.epos in
			let proto = get_instance_prototype jit.ctx key e.epos in
			emit_constructor_call proto fnew execs e.epos
		end
	(* read *)
	| TLocal var ->
		if var.v_capture then emit_capture_read (get_capture_slot jit var.v_id)
		else emit_local_read (get_slot jit var.v_id e.epos)
	| TField(e1,fa) ->
		let name = hash_s (field_name fa) in
		begin match fa with
			| FInstance({cl_path=([],"Array")},_,{cf_name="length"}) -> emit_array_length_read (jit_expr false jit e1)
			| FInstance({cl_path=(["haxe";"io"],"Bytes")},_,{cf_name="length"}) -> emit_bytes_length_read (jit_expr false jit e1)
			| FStatic({cl_path=path},_) | FEnum({e_path=path},_) ->
				let proto = get_static_prototype ctx (path_hash path) e1.epos in
				emit_proto_field_read proto (get_proto_field_index proto name)
			| FInstance(c,_,_) when not c.cl_interface ->
				let proto = get_instance_prototype ctx (path_hash c.cl_path) e1.epos in
				let i = get_instance_field_index proto name in
				begin match e1.eexpr with
					| TLocal var when not var.v_capture -> emit_instance_local_field_read (get_slot jit var.v_id e1.epos) i
					| _ -> emit_instance_field_read (jit_expr false jit e1) i
				end
			| FAnon _ ->
				begin match follow e1.etype with
					| TAnon an ->
						let l = PMap.foldi (fun k _ acc -> (hash_s k,()) :: acc) an.a_fields [] in
						let proto,_ = ctx.get_object_prototype ctx l in
						let i = get_instance_field_index proto name in
						begin match e1.eexpr with
							| TLocal var when not var.v_capture -> emit_anon_local_field_read (get_slot jit var.v_id e1.epos) proto i name e1.epos
							| _ -> emit_anon_field_read (jit_expr false jit e1) proto i name e1.epos
						end
					| _ ->
						emit_field_read (jit_expr false jit e1) name e1.epos
				end
			| FClosure _ | FDynamic _ ->
				let exec = jit_expr false jit e1 in
				emit_field_closure exec name
			| _ ->
				let exec = jit_expr false jit e1 in
				emit_field_read exec name e1.epos
		end
	| TArray(e1,e2) ->
		begin match e1.eexpr with
			| TLocal var when not var.v_capture ->
				emit_array_local_read (get_slot jit var.v_id e1.epos) (jit_expr false jit e2)
			| _ ->
				let exec1 = jit_expr false jit e1 in
				let exec2 = jit_expr false jit e2 in
				emit_array_read exec1 exec2
		end
	| TEnumParameter(e1,_,i) ->
		let exec = jit_expr false jit e1 in
		emit_enum_parameter_read exec i
	(* ops *)
	| TBinop(op,e1,e2) ->
		begin match op with
		| OpAssign ->
			op_assign ctx jit e1 e2
		| OpAssignOp op ->
			let f = get_binop_fun op e.epos in
			op_assign_op jit f e1 e2 true
		| OpBoolAnd ->
			let exec1 = jit_expr false jit e1 in
			let exec2 = jit_expr false jit e2 in
			emit_bool_and exec1 exec2
		| OpBoolOr ->
			let exec1 = jit_expr false jit e1 in
			let exec2 = jit_expr false jit e2 in
			emit_bool_or exec1 exec2
		| _ ->
			let exec1 = jit_expr false jit e1 in
			let exec2 = jit_expr false jit e2 in
			begin match op with
				| OpAdd -> emit_op_add exec1 exec2
				| OpMult -> emit_op_mult e.epos exec1 exec2
				| OpDiv -> emit_op_div e.epos exec1 exec2
				| OpSub -> emit_op_sub e.epos exec1 exec2
				| OpEq -> emit_op_eq exec1 exec2
				| OpNotEq -> emit_op_not_eq exec1 exec2
				| OpGt -> emit_op_gt exec1 exec2
				| OpGte -> emit_op_gte exec1 exec2
				| OpLt -> emit_op_lt exec1 exec2
				| OpLte -> emit_op_lte exec1 exec2
				| OpAnd -> emit_op_and e.epos exec1 exec2
				| OpOr -> emit_op_or e.epos exec1 exec2
				| OpXor -> emit_op_xor e.epos exec1 exec2
				| OpShl -> emit_op_shl e.epos exec1 exec2
				| OpShr -> emit_op_shr e.epos exec1 exec2
				| OpUShr -> emit_op_ushr e.epos exec1 exec2
				| OpMod -> emit_op_mod e.epos exec1 exec2
				| _ -> assert false
			end
		end
	| TUnop(op,flag,v1) ->
		unop jit op flag v1 e.epos
	(* rewrites/skips *)
	| TFor(v,e1,e2) ->
		loop (Codegen.for_remap (ctx.curapi.MacroApi.get_com()) v e1 e2 e.epos)
	| TParenthesis e1 | TMeta(_,e1) | TCast(e1,None) ->
		loop e1
	in
	let f = loop e in
	if ctx.debug.support_debugger then begin match e.eexpr with
		| TConst _ | TLocal _ | TTypeExpr _ -> f
		| _ -> EvalDebug.debug_loop jit e f
	end else
		f

(* Creates a [EvalValue.vfunc] of function [tf], which can be [static] or not. *)
let jit_tfunction ctx key_type key_field tf static =
	let t = Common.timer [(if ctx.is_macro then "macro" else "interp");"jit"] in
	(* Create a new JitContext with an initial scope *)
	let jit = EvalJitContext.create ctx (hash_s (Path.unique_full_path tf.tf_expr.epos.pfile)) in
	push_scope jit;
	(* Declare `this` (if not static) and function arguments as local variables. *)
	let varaccs = if static then [] else [declare_local_this jit] in
	let varaccs = varaccs @ List.map (fun (var,_) -> declare_local jit var) tf.tf_args in
	(* Jit the function expression and pop the scope. *)
	let exec = jit_expr true jit tf.tf_expr in
	pop_scope jit;
	(* Create a list of default values for the function arguments. *)
	let args = List.map (fun (_,cto) -> Option.map_default eval_const vnull cto) tf.tf_args in
	let args = if static then args else vnull :: args in
	t();
	(* Create the [vfunc] instance depending on the number of arguments. *)
	let local_count = jit.max_local_count in
	let capture_count = Hashtbl.length jit.captures in
	let hasret = jit.has_nonfinal_return in
	let info = create_env_info (hash_s tf.tf_expr.epos.pfile) (EKMethod(key_type,key_field)) jit.caught_types in
	match args,varaccs with
	| [],[] -> Fun0 (emit_tfunction0 ctx info hasret local_count capture_count exec)
	| [arg1],[varacc1] -> Fun1 (emit_tfunction1 ctx info hasret local_count capture_count arg1 varacc1 exec)
	| [arg1;arg2],[varacc1;varacc2] -> Fun2 (emit_tfunction2 ctx info hasret local_count capture_count arg1 varacc1 arg2 varacc2 exec)
	| [arg1;arg2;arg3],[varacc1;varacc2;varacc3] -> Fun3 (emit_tfunction3 ctx info hasret local_count capture_count arg1 varacc1 arg2 varacc2 arg3 varacc3 exec)
	| [arg1;arg2;arg3;arg4],[varacc1;varacc2;varacc3;varacc4] -> Fun4 (emit_tfunction4 ctx info hasret local_count capture_count arg1 varacc1 arg2 varacc2 arg3 varacc3 arg4 varacc4 exec)
	| [arg1;arg2;arg3;arg4;arg5],[varacc1;varacc2;varacc3;varacc4;varacc5] -> Fun5 (emit_tfunction5 ctx info hasret local_count capture_count arg1 varacc1 arg2 varacc2 arg3 varacc3 arg4 varacc4 arg5 varacc5 exec)
	| _ -> FunN (emit_tfunction ctx info hasret local_count capture_count args varaccs exec)

(* JITs expression [e] to a function. This is used for expressions that are not in a method. *)
let jit_expr ctx e =
	let t = Common.timer [(if ctx.is_macro then "macro" else "interp");"jit"] in
	let jit = EvalJitContext.create ctx (hash_s (Path.unique_full_path e.epos.pfile)) in
	let f = jit_expr false jit (mk_block e) in
	t();
	jit,f