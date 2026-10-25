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
open EvalValue
open EvalContext
open EvalHash
open EvalEmitter
open EvalMisc

(* Helper *)

let rope_path t = match follow t with
	| TInst({cl_path=path},_) | TEnum({e_path=path},_) | TAbstract({a_path=path},_) -> s_type_path path
	| TDynamic _ -> "Dynamic"
	| TFun _ | TAnon _ | TMono _ | TType _ | TLazy _ -> die "" __LOC__

let eone = mk (TConst(TInt (Int32.one))) t_dynamic null_pos

let eval_const = function
	| TString s -> EvalString.create_unknown s
	| TInt i32 -> vint32 i32
	| TFloat f -> vfloat (float_of_string f)
	| TBool b -> vbool b
	| TNull -> vnull
	| TThis | TSuper -> die "" __LOC__

let is_int t = match follow t with
	| TAbstract({a_path=[],"Int"},_) -> true
	| _ -> false

let is_string t = match follow t with
	| TInst({cl_path=[],"String"},_) -> true
	| _ -> false

let is_const_int_pattern case =
	List.for_all (fun e -> match e.eexpr with
		| TConst (TInt _) -> true
		| _ -> false
	) case.case_patterns

open EvalJitContext

type operand =
	| OLocal of int
	| OConst of value
	| OExec of (env -> value)

(* The slot of [e] when it reads a local variable that is not captured. *)
let local_slot jit e = match e.eexpr with
	| TLocal var when not (has_var_flag var VCaptured) -> Some (get_slot jit var.v_id e.epos)
	| TConst TThis -> Some (get_slot jit 0 e.epos)
	| _ -> None

(* [exec] reading or changing a static variable (see [EvalValue.on_shared_state]) *)
let shared_state exec env =
	!on_shared_state (fun () -> "static");
	exec env

(* [exec] reading or changing the static variable at slot [i] of [proto] *)
let static_state proto i exec env =
	if is_shared_static proto i then !on_shared_state (fun () -> static_name proto i);
	fresh_statics proto;
	exec env

let rec op_assign ctx jit e1 e2 = match e1.eexpr with
	| TLocal var ->
		let exec = jit_expr jit false e2 in
		if has_var_flag var VCaptured then emit_capture_write (get_capture_slot jit var) exec
		else emit_local_write (get_slot jit var.v_id e1.epos) exec
	| TField(ef,fa) ->
		let name = hash (field_name fa) in
		let exec1 = jit_expr jit false ef in
		let exec2 = jit_expr jit false e2 in
		begin match fa with
			| FInstance({cl_path=(["haxe";"io"],"Bytes")},_,{cf_name="length"}) ->
				emit_bytes_length_write exec1 exec2
			| FStatic({cl_path=path},_) | FEnum({e_path=path},_) ->
				let proto = get_static_prototype jit.ctx (path_hash path) ef.epos in
				let i = get_proto_field_index proto name in
				static_state proto i (emit_proto_field_write proto i exec2)
			| FInstance(c,_,_) when not (has_class_flag c CInterface) ->
				let proto = get_instance_prototype jit.ctx (path_hash c.cl_path) ef.epos in
				let i = get_instance_field_index proto name ef.epos in
				begin match local_slot jit ef with
				| Some slot -> emit_local_instance_field_write slot ef.epos i exec2
				| None -> emit_instance_field_write exec1 ef.epos i exec2
				end
			| FAnon cf ->
				begin match follow ef.etype with
					| TAnon an ->
						let l = PMap.foldi (fun k _ acc -> (hash k,()) :: acc) an.a_fields [] in
						let proto,_ = ctx.get_object_prototype ctx l in
						let i = get_instance_field_index proto name ef.epos in
						emit_anon_field_write exec1 ef.epos proto i name exec2
					| _ ->
						emit_field_write exec1 e1.epos name exec2
				end
			| _ ->
				emit_field_write exec1 e1.epos name exec2
		end
	| TArray(ea1,ea2) ->
		begin match (follow ea1.etype) with
			| TInst({cl_path=(["eval"],"Vector")}, _) ->
				let exec1 = jit_expr jit false ea1 in
				let exec2 = jit_expr jit false ea2 in
				let exec3 = jit_expr jit false e2 in
				emit_vector_write exec1 ea1.epos exec2 ea2.epos exec3 ea2.epos
			| _ ->
				let exec1 = jit_expr jit false ea1 in
				let exec2 = jit_expr jit false ea2 in
				let exec3 = jit_expr jit false e2 in
				emit_array_write exec1 ea1.epos exec2 ea2.epos exec3 ea2.epos
		end

	| _ ->
		die "" __LOC__

and op_assign_op ?binop jit op e1 e2 prefix = match e1.eexpr with
	| TLocal var ->
		let exec = jit_expr jit false e2 in
		if has_var_flag var VCaptured then emit_capture_read_write (get_capture_slot jit var) exec op prefix
		else begin
			let slot = get_slot jit var.v_id e1.epos in
			match binop with
			| Some (OpAdd,p) when prefix -> emit_local_assign_add p slot exec
			| Some (OpSub,p) when prefix -> emit_local_assign_sub p slot exec
			| Some (OpMult,p) when prefix -> emit_local_assign_mult p slot exec
			| Some (OpAnd,p) when prefix -> emit_local_assign_and p slot exec
			| Some (OpOr,p) when prefix -> emit_local_assign_or p slot exec
			| Some (OpXor,p) when prefix -> emit_local_assign_xor p slot exec
			| _ -> emit_local_read_write slot exec op prefix
		end
	| TField(ef,fa) ->
		let name = hash (field_name fa) in
		let exec1 = jit_expr jit false ef in
		let exec2 = jit_expr jit false e2 in
		begin match fa with
			| FStatic({cl_path=path},_) ->
				let proto = get_static_prototype jit.ctx (path_hash path) ef.epos in
				let i = get_proto_field_index proto name in
				static_state proto i (emit_proto_field_read_write proto i exec2 op prefix)
			| FInstance(c,_,_) when not (has_class_flag c CInterface) ->
				let proto = get_instance_prototype jit.ctx (path_hash c.cl_path) ef.epos in
				let i = get_instance_field_index proto name ef.epos in
				emit_instance_field_read_write exec1 ef.epos i exec2 op prefix
			| _ ->
				emit_field_read_write exec1 e1.epos name exec2 op prefix
		end
	| TArray(ea1,ea2) ->
		begin match (follow ea1.etype) with
			| TInst({cl_path=(["eval"],"Vector")}, _) ->
				let exec1 = jit_expr jit false ea1 in
				let exec2 = jit_expr jit false ea2 in
				let exec3 = jit_expr jit false e2 in
				emit_vector_read_write exec1 ea1.epos exec2 ea2.epos exec3 op prefix
			| _ ->
				let exec1 = jit_expr jit false ea1 in
				let exec2 = jit_expr jit false ea2 in
				let exec3 = jit_expr jit false e2 in
				emit_array_read_write exec1 ea1.epos exec2 ea2.epos exec3 op prefix
		end
	| _ ->
		die "" __LOC__

and op_incr jit e1 prefix p =
	op_assign_op jit (get_binop_fun OpAdd p) e1 eone prefix

and op_decr jit e1 prefix p =
	op_assign_op jit (get_binop_fun OpSub p) e1 eone prefix

and unop jit op flag e1 p =
	match op with
	| Not ->
		let exec = jit_expr jit false e1 in
		emit_not exec
	| Neg ->
		let exec = jit_expr jit false e1 in
		emit_neg exec p
	| NegBits ->
		let exec = jit_expr jit false e1 in
		emit_op_sub p (fun _ -> vint32 (Int32.minus_one)) exec
	| Increment ->
		begin match Texpr.skip e1 with
		| {eexpr = TLocal v} when not (has_var_flag v VCaptured) ->
			let slot = get_slot jit v.v_id e1.epos in
			if flag = Prefix then emit_local_incr_prefix slot e1.epos
			else emit_local_incr_postfix slot e1.epos
		| _ ->
			op_incr jit e1 (flag = Prefix) p
		end
	| Decrement ->
		op_decr jit e1 (flag = Prefix) p
	| Spread ->
		match flag with
		| Postfix -> die ~p:p "Postfix spread operator is not supported" __LOC__
		| Prefix -> jit_expr jit false e1

(* A binary operation, read directly from a local or constant operand. *)
and arith jit ee le lc ll ec p e1 e2 =
	match jit_operand jit e1 with
	| OLocal slot1 ->
		begin match jit_operand jit e2 with
		| OLocal slot2 -> ll p slot1 slot2
		| OConst v2 -> lc p slot1 v2
		| OExec exec2 -> le p slot1 exec2
		end
	| OConst v1 ->
		ee p (emit_const v1) (jit_expr jit false e2)
	| OExec exec1 ->
		begin match jit_operand jit e2 with
		| OConst v2 -> ec p exec1 v2
		| OLocal slot2 -> ee p exec1 (emit_local_read slot2)
		| OExec exec2 -> ee p exec1 exec2
		end

(* An operand: a local that is not captured, a constant or an expression. *)
and jit_operand jit e = match e.eexpr with
	| TLocal var when not (has_var_flag var VCaptured) -> OLocal (get_slot jit var.v_id e.epos)
	| TConst TThis -> OLocal (get_slot jit 0 e.epos)
	| TConst (TString _ | TInt _ | TFloat _ | TBool _ | TNull as ct) -> OConst (eval_const ct)
	| _ -> OExec (jit_expr jit false e)

(* [e] as a condition (see [EvalEmitter.emit_cond_value]). *)
and jit_cond jit e =
	let generic () = emit_cond_value (jit_expr jit false e) in
	let is_null e = match e.eexpr with TConst TNull -> true | _ -> false in
	(* the conditions always giving a Bool value *)
	let rec is_bool_cond e = match e.eexpr with
		| TParenthesis e1 | TMeta(_,e1) -> is_bool_cond e1
		| TBinop((OpLt | OpLte | OpGt | OpGte | OpEq | OpNotEq),_,_) -> true
		| TUnop(Not,Prefix,_) -> true
		| _ -> false
	in
	let operand = jit_operand jit in
	let compare ee le lc ll ec e1 e2 =
		match operand e1 with
		| OLocal slot1 ->
			begin match operand e2 with
			| OLocal slot2 -> ll slot1 slot2
			| OConst v2 -> lc slot1 v2
			| OExec exec2 -> le slot1 exec2
			end
		| OConst v1 ->
			ee (emit_const v1) (jit_expr jit false e2)
		| OExec exec1 ->
			begin match operand e2 with
			| OConst v2 -> ec exec1 v2
			| OLocal slot2 -> ee exec1 (emit_local_read slot2)
			| OExec exec2 -> ee exec1 exec2
			end
	in
	(* the debugger steps through the expressions jit_expr gives *)
	if Option.is_some jit.ctx.debug.debug_socket then generic () else
	match e.eexpr with
	| TParenthesis e1 | TMeta(_,e1) ->
		jit_cond jit e1
	| TBinop(OpEq,e1,e2) when is_null e2 || is_null e1 ->
		let e1 = if is_null e2 then e1 else e2 in
		begin match operand e1 with
		| OLocal slot -> emit_cond_local_eq_null slot
		| _ -> emit_cond_eq_null (jit_expr jit false e1)
		end
	| TBinop(OpNotEq,e1,e2) when is_null e2 || is_null e1 ->
		let e1 = if is_null e2 then e1 else e2 in
		begin match operand e1 with
		| OLocal slot -> emit_cond_local_not_eq_null slot
		| _ -> emit_cond_not_eq_null (jit_expr jit false e1)
		end
	| TBinop(OpLt,e1,e2) -> compare emit_cond_lt emit_cond_lt_le emit_cond_lt_lc emit_cond_lt_ll emit_cond_lt_ec e1 e2
	| TBinop(OpLte,e1,e2) -> compare emit_cond_lte emit_cond_lte_le emit_cond_lte_lc emit_cond_lte_ll emit_cond_lte_ec e1 e2
	| TBinop(OpGt,e1,e2) -> compare emit_cond_gt emit_cond_gt_le emit_cond_gt_lc emit_cond_gt_ll emit_cond_gt_ec e1 e2
	| TBinop(OpGte,e1,e2) -> compare emit_cond_gte emit_cond_gte_le emit_cond_gte_lc emit_cond_gte_ll emit_cond_gte_ec e1 e2
	| TBinop(OpEq,e1,e2) -> compare emit_cond_eq emit_cond_eq_le emit_cond_eq_lc emit_cond_eq_ll emit_cond_eq_ec e1 e2
	| TBinop(OpNotEq,e1,e2) -> compare emit_cond_neq emit_cond_neq_le emit_cond_neq_lc emit_cond_neq_ll emit_cond_neq_ec e1 e2
	| TBinop(OpBoolAnd,e1,e2) ->
		let cond1 = jit_cond jit e1 in
		let cond2 = jit_cond jit e2 in
		emit_cond_and cond1 cond2
	| TBinop(OpBoolOr,e1,e2) ->
		let cond1 = jit_cond jit e1 in
		let cond2 = jit_cond jit e2 in
		emit_cond_or cond1 cond2
	| TUnop(Not,Prefix,e1) ->
		if is_bool_cond e1 then emit_cond_not_cond (jit_cond jit e1)
		else emit_cond_not (jit_expr jit false e1)
	| _ ->
		generic ()

and jit_default jit return def =
	match def with
	| None ->
		emit_null
	| Some e ->
		push_scope jit e.epos;
		let exec = jit_expr jit return e in
		pop_scope jit;
		exec

(*
	This is the main jit function. It turns expression [e] into a function, which can be
	executed int an environment of type [EvalContext.env].
*)
and jit_expr jit return e =
	let ctx = jit.ctx in
	let rec loop e = match e.eexpr with
	(* objects and values *)
	| TVar(var,eo) ->
		let varacc = declare_local jit var in
		let exec = match eo with
			| None -> emit_null
			| Some e -> jit_expr jit false e
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
		let fl = List.map (fun ((s,_,_),e) -> hash s,jit_expr jit false e) fl in
		let proto,_ = ctx.get_object_prototype ctx fl in
		let fl = List.map (fun (s,exec) -> get_instance_field_index proto s e.epos,exec) fl in
		let fa = Array.of_list fl in
		emit_object_declaration proto fa
	| TArrayDecl el ->
		let execs = List.map (jit_expr jit false) el in
		let execs = Array.of_list execs in
		emit_array_declaration execs
	| TTypeExpr mt ->
		let key = path_hash (t_infos mt).mt_path in
		let proto = get_static_prototype_as_value jit.ctx key e.epos in
		emit_type_expr proto
	| TFunction tf ->
		let jit_closure = EvalJitContext.create ctx in
		jit.num_closures <- jit.num_closures + 1;
		let fl,exec = jit_tfunction jit_closure true e.epos tf in
		let hasret = jit_closure.has_nonfinal_return in
		let eci = get_env_creation jit_closure false (Pos.file tf.tf_expr.epos) (EKLocalFunction jit.num_closures) in
		let captures = IntHashtbl.fold (fun vid (i,declared) acc -> (i,vid,declared) :: acc) jit_closure.captures [] in
		let captures = List.sort (fun (i1,_,_) (i2,_,_) -> Stdlib.compare i1 i2) captures in
		(* Check if the out-of-scope var is in the outer scope because otherwise we have to promote outwards. *)
		List.iter (fun var -> ignore(get_capture_slot jit var)) jit_closure.captures_outside_scope;
		let captures = ExtList.List.filter_map (fun (i,vid,declared) ->
			if declared then None
			else Some (i,fst (try IntHashtbl.find jit.captures vid with Not_found -> Error.raise_typing_error (Printf.sprintf "Could not find capture variable %i" vid) e.epos))
		) captures in
		let mapping = Array.of_list captures in
		emit_closure ctx mapping eci hasret exec fl
	(* branching *)
	| TIf(e1,e2,eo) ->
		let cond = jit_cond jit e1 in
		let exec_then = jit_expr jit return e2 in
		let exec_else = match eo with
			| None -> emit_null
			| Some e -> jit_expr jit return e
		in
		emit_if_cond cond exec_then exec_else
	| TSwitch switch when is_int switch.switch_subject.etype && List.for_all is_const_int_pattern switch.switch_cases ->
		let exec = jit_expr jit false switch.switch_subject in
		let h = ref IntMap.empty in
		let max = ref 0 in
		let min = ref max_int in
		List.iter (fun {case_patterns = el;case_expr = e} ->
			push_scope jit e.epos;
			let exec = jit_expr jit return e in
			List.iter (fun e -> match e.eexpr with
				| TConst (TInt i32) ->
					let i = Int32.to_int i32 in
					h := IntMap.add i exec !h;
					if i > !max then max := i;
					if i < !min then min := i;
				| _ -> die "" __LOC__
			) el;
			pop_scope jit;
		) switch.switch_cases;
		let exec_def = jit_default jit return switch.switch_default in
		let l = !max - !min + 1 in
		if l > 0 && l < 256 then begin
			let cases = Array.init l (fun i -> try IntMap.find (i + !min) !h with Not_found -> exec_def) in
			emit_int_switch_array (- !min) exec cases exec_def switch.switch_subject.epos
		end else
			emit_int_switch_map exec !h exec_def switch.switch_subject.epos
	(* | TSwitch(e1,cases,def) when is_string e1.etype ->
		let exec = jit_expr jit false e1 in
		let h = ref PMap.empty in
		List.iter (fun (el,e) ->
			push_scope jit e.epos;
			let exec = jit_expr jit return e in
			List.iter (fun e -> match e.eexpr with
				| TConst (TString s) -> h := PMap.add s exec !h;
				| _ -> die "" __LOC__
			) el;
			pop_scope jit;
		) cases;
		let exec_def = jit_default jit return def in
		emit_string_switch_map exec !h exec_def e1.epos *)
	| TSwitch switch ->
		let exec = jit_expr jit false switch.switch_subject in
		let execs = DynArray.create () in
		let patterns = List.map (fun {case_patterns = el;case_expr = e}  ->
			push_scope jit e.epos;
			let el = List.map (jit_expr jit false) el in
			DynArray.add execs (jit_expr jit return e);
			pop_scope jit;
			el
		) switch.switch_cases in
		let exec_def = jit_default jit return switch.switch_default in
		emit_switch exec (DynArray.to_array execs) (Array.of_list patterns) exec_def
	| TWhile({eexpr = TParenthesis e1},e2,flag) ->
		loop {e with eexpr = TWhile(e1,e2,flag)}
	| TWhile(e1,e2,flag) ->
		let cond = jit_cond jit e1 in
		let exec_body = jit_expr jit false e2 in
		begin match flag with
			| NormalWhile -> emit_while_cond cond exec_body
			| DoWhile -> emit_do_while_cond cond exec_body
		end
	| TTry(e1,catches) ->
		let exec = jit_expr jit return e1 in
		let catches = List.map (fun (var,e) ->
			push_scope jit e.epos;
			let varacc = declare_local jit var in
			let exec = jit_expr jit return e in
			pop_scope jit;
			let key = hash (rope_path var.v_type) in
			let f = match varacc with
				| Local slot -> emit_local_write slot
				| Env slot -> emit_capture_write slot
			in
			exec,key,f
		) catches in
		emit_try exec catches
	(* control flow *)
	| TBlock [] ->
		emit_null
	| TBlock [e1] ->
		push_scope jit e.epos;
		let exec = jit_expr jit return e1 in
		pop_scope jit;
		exec
	| TBlock el ->
		push_scope jit e.epos;
		let rec loop acc el = match el with
			| [e] ->
				List.rev ((jit_expr jit return e) :: acc)
			| e :: el ->
				loop (jit_expr jit false e :: acc) el
			| [] ->
				die "" __LOC__
		in
		let el = loop [] el in
		pop_scope jit;
		let rec step el =
			let a = Array.of_list el in
			let rec loop i acc =
				if i >= 7 then begin
					let f8 = emit_seq8 a.(i - 7) a.(i - 6) a.(i - 5) a.(i - 4) a.(i - 3) a.(i - 2) a.(i - 1) a.(i) in
					loop (i - 8) (f8 :: acc)
				end else if i >= 3 then begin
					let f4 = emit_seq4 a.(i - 3) a.(i - 2) a.(i - 1) a.(i) in
					loop (i - 4) (f4 :: acc)
				end else if i >= 1 then begin
					let f2 = emit_seq2 a.(i - 1) a.(i) in
					loop (i - 2) (f2 :: acc)
				end else if i = 0 then
					((a.(i)) :: acc)
				else
					acc
			in
			let length = Array.length a in
			match loop (length - 1) [] with
			| [] -> die "" __LOC__
			| [f] -> f
			| fl -> step fl
		in
		step el
	| TReturn None ->
		if return && not jit.ctx.debug.support_debugger then
			emit_null
		else begin
			jit.has_nonfinal_return <- true;
			emit_return_null
		end
	| TReturn (Some e1) ->
		let exec = jit_expr jit false e1 in
		if return && not jit.ctx.debug.support_debugger then
			emit_value exec
		else begin
			jit.has_nonfinal_return <- true;
			emit_return_value exec
		end
	| TBreak ->
		emit_break
	| TContinue ->
		emit_continue
	| TThrow e1 ->
		let exec = jit_expr jit false e1 in
		emit_throw exec e.epos
	| TCast(e1,Some mt) ->
		let exec = jit_expr jit false e1 in
		let t = type_of_module_type mt in
		emit_safe_cast exec (hash (rope_path t)) e.epos
	(* calls *)
	| TCall(e1,el) ->
		begin match e1.eexpr with
		| TField({eexpr = TConst TSuper;epos=pv},FInstance(c,_,cf)) ->
			let proto = get_instance_prototype ctx (path_hash c.cl_path) e1.epos in
			let name = hash cf.cf_name in
			let i = get_proto_field_index proto name in
			let slot = get_slot jit 0 pv in
			let execs = List.map (jit_expr jit false) el in
			emit_super_field_call ctx.max_stack_depth slot proto i execs e.epos
		| TField(ef,fa) ->
			let name = hash (field_name fa) in
			let execs = List.map (jit_expr jit false) el in
			let is_final c cf =
				has_class_flag c CFinal || (has_class_field_flag cf CfFinal) ||
				(* In interp mode we can assume that a field is final if it is not overridden.
				   We cannot do that in macro mode because overriding fields might be added
				   after jitting this call. *)
				(not ctx.is_macro && not (Hashtbl.mem ctx.overrides (c.cl_path,cf.cf_name)))
			in
			let is_proper_method cf = match cf.cf_kind with
				| Method MethDynamic -> false
				| Method _ -> true
				| Var _ -> false
			in
			let lazy_proto_field proto =
				let i = get_proto_field_index proto name in
				AtomicLazy.from_fun (fun () -> match proto.pfields.(i) with VFunction (f,_) -> f | v -> cannot_call v e.epos)
			in
			let jit_with_null_check ef = match ef.eexpr with
				| TConst TThis ->
					(* [this] is never null *)
					jit_expr jit false ef
				| _ ->
					match local_slot jit ef with
					| Some slot -> emit_local_null_check slot ef.epos
					| None -> emit_null_check (jit_expr jit false ef) ef.epos
			in
			let instance_call c =
				let exec = jit_with_null_check ef in
				let proto = get_instance_prototype ctx (path_hash c.cl_path) ef.epos in
				let v = lazy_proto_field proto in
				emit_proto_field_call ctx.max_stack_depth v (exec :: execs) e.epos
			in
			let default () =
				let exec = jit_with_null_check ef in
				emit_method_call ctx.max_stack_depth exec name execs e.epos
			in
			begin match fa with
				| FStatic({cl_path=[],"StringTools"},{cf_name="fastCodeAt"}) ->
					begin match execs with
						| [exec1;exec2] -> emit_string_cca exec1 exec2 e.epos
						| _ -> die "" __LOC__
					end
				| FStatic({cl_path=[],"StringTools"},{cf_name="unsafeCodeAt"}) ->
					begin match execs with
						| [exec1;exec2] -> emit_string_cca_unsafe exec1 exec2 e.epos
						| _ -> die "" __LOC__
					end
				| FEnum({e_path=path},ef) ->
					let key = path_hash path in
					let pos = Some e.epos in
					emit_enum_construction key ef.ef_index (Array.of_list execs) pos
				| FStatic({cl_path=path},cf) when is_proper_method cf ->
					let proto = get_static_prototype ctx (path_hash path) ef.epos in
					let v = lazy_proto_field proto in
					emit_proto_field_call ctx.max_stack_depth v execs e.epos
				| FInstance(c,_,cf) when is_proper_method cf ->
					if not (is_final c cf) then
						default()
					else if not (has_class_flag c CInterface) then
						instance_call c
					(* If we have exactly one implementer, use it instead of the super class/interface. *)
					else if not ctx.is_macro && c.cl_implements = [] && c.cl_super = None then begin match c.cl_descendants with
						| [c'] when not (has_class_flag c' CInterface) && is_final c' cf ->
							instance_call c'
						| _ ->
							default()
					end else
						default()
				| _ ->
					let exec = jit_with_null_check ef in
					emit_field_call ctx.max_stack_depth exec name execs e.epos
			end
		| TConst TSuper ->
			begin match follow e1.etype with
			| TInst(c,_) ->
				let key = (path_hash c.cl_path) in
				let execs = List.map (jit_expr jit false) el in
				begin try
					let f = get_special_instance_constructor_raise ctx key in
					emit_special_super_call ctx.max_stack_depth f execs
				with Not_found ->
					let fnew = get_instance_constructor jit.ctx key e1.epos in
					let v = AtomicLazy.from_fun (fun () -> match AtomicLazy.force fnew with VFunction (f,_) -> f | v -> cannot_call v e.epos) in
					emit_super_call ctx.max_stack_depth v execs e.epos
				end
			| _ -> die "" __LOC__
			end
		| _ ->
			let exec = jit_expr jit false e1 in
			let execs = List.map (jit_expr jit false) el in
			emit_call ctx.max_stack_depth exec execs e.epos
		end
	| TNew({cl_path=[],"Array"},_,_) ->
		emit_new_array
	| TNew({cl_path=["eval"],"Vector"},_,[e1]) ->
		begin match e1.eexpr with
			| TConst (TInt i32) ->
				emit_new_vector_int (Int32.to_int i32) e1.epos
			| _ ->
				let exec1 = jit_expr jit false e1 in
				emit_new_vector exec1 e1.epos
		end
	| TNew(c,_,el) ->
		let execs = List.map (jit_expr jit false) el in
		let key = path_hash c.cl_path in
		begin try
			let f = get_special_instance_constructor_raise ctx key in
			emit_special_instance f execs
		with Not_found ->
			let fnew = get_instance_constructor jit.ctx key e.epos in
			let proto = get_instance_prototype jit.ctx key e.epos in
			let v = AtomicLazy.from_fun (fun () -> match AtomicLazy.force fnew with VFunction (f,_) -> f | v -> cannot_call v e.epos) in
			emit_constructor_call ctx.max_stack_depth proto v execs e.epos
		end
	(* read *)
	| TLocal var ->
		if has_var_flag var VCaptured then emit_capture_read (get_capture_slot jit var)
		else emit_local_read (get_slot jit var.v_id e.epos)
	| TField(e1,fa) ->
		let name = hash (field_name fa) in
		begin match fa with
			| FInstance({cl_path=([],"Array")},_,{cf_name="length"}) ->
				begin match local_slot jit e1 with
				| Some slot -> emit_local_array_length_read slot e1.epos
				| None -> emit_array_length_read (jit_expr jit false e1) e1.epos
				end
			| FInstance({cl_path=(["eval"],"Vector")},_,{cf_name="length"}) -> emit_vector_length_read (jit_expr jit false e1) e1.epos
			| FInstance({cl_path=(["haxe";"io"],"Bytes")},_,{cf_name="length"}) -> emit_bytes_length_read (jit_expr jit false e1) e1.epos
			| FStatic({cl_path=path},cf) ->
				let proto = get_static_prototype ctx (path_hash path) e1.epos in
				let i = get_proto_field_index proto name in
				let exec = emit_proto_field_read proto i in
				begin match cf.cf_kind with
					| Method (MethNormal | MethInline | MethMacro) -> exec
					| _ -> static_state proto i exec
				end
			| FEnum({e_path=path},_)
			| FInstance({cl_path=path},_,{cf_kind = Method (MethNormal | MethInline)}) ->
				let proto = get_static_prototype ctx (path_hash path) e1.epos in
				emit_proto_field_read proto (get_proto_field_index proto name)
			| FInstance(c,_,_) when not (has_class_flag c CInterface) ->
				let proto = get_instance_prototype ctx (path_hash c.cl_path) e1.epos in
				let i = get_instance_field_index proto name e1.epos in
				begin match e1.eexpr with
					| TConst TThis -> emit_this_field_read (get_slot jit 0 e.epos) i
					| _ ->
						match local_slot jit e1 with
						| Some slot -> emit_local_instance_field_read slot e1.epos i
						| None -> emit_instance_field_read (jit_expr jit false e1) e1.epos i
				end
			| FAnon _ ->
				begin match follow e1.etype with
					| TAnon an ->
						let l = PMap.foldi (fun k _ acc -> (hash k,()) :: acc) an.a_fields [] in
						let proto,_ = ctx.get_object_prototype ctx l in
						let i = get_instance_field_index proto name e1.epos in
						begin match local_slot jit e1 with
						| Some slot -> emit_local_anon_field_read slot proto i name e1.epos
						| None -> emit_anon_field_read (jit_expr jit false e1) proto i name e1.epos
						end
					| _ ->
						emit_field_read (jit_expr jit false e1) name e1.epos
				end
			| FClosure _ | FDynamic _ ->
				let exec = jit_expr jit false e1 in
				emit_field_closure exec name
			| _ ->
				let exec = jit_expr jit false e1 in
				emit_field_read exec name e1.epos
		end
	| TArray(e1,e2) ->
		begin match (follow e1.etype) with
			| TInst({cl_path=(["eval"],"Vector")}, _) ->
				let exec1 = jit_expr jit false e1 in
				let exec2 = jit_expr jit false e2 in
				emit_vector_read exec1 e1.epos exec2 e2.epos
			| _ ->
				begin match local_slot jit e1 with
				| Some slot1 ->
					begin match local_slot jit e2 with
					| Some slot2 -> emit_array_read_ll slot1 e1.epos slot2 e2.epos
					| None -> emit_array_read_le slot1 e1.epos (jit_expr jit false e2) e2.epos
					end
				| None ->
					let exec1 = jit_expr jit false e1 in
					let exec2 = jit_expr jit false e2 in
					emit_array_read exec1 e1.epos exec2 e2.epos
				end
		end
	| TEnumParameter(e1,_,i) ->
		begin match local_slot jit e1 with
		| Some slot -> emit_local_enum_parameter_read slot i
		| None -> emit_enum_parameter_read (jit_expr jit false e1) i
		end
	| TEnumIndex e1 ->
		begin match local_slot jit e1 with
		| Some slot -> emit_local_enum_index slot e1.epos
		| None -> emit_enum_index (jit_expr jit false e1) e1.epos
		end
	(* ops *)
	| TBinop(OpEq,e1,{eexpr = TConst TNull}) | TBinop(OpEq,{eexpr = TConst TNull},e1) ->
		let exec = jit_expr jit false e1 in
		emit_eq_null exec
	| TBinop(OpNotEq,e1,{eexpr = TConst TNull}) | TBinop(OpNotEq,{eexpr = TConst TNull},e1) ->
		let exec = jit_expr jit false e1 in
		emit_not_eq_null exec
	| TBinop(op,e1,e2) ->
		begin match op with
		| OpAssign ->
			op_assign ctx jit e1 e2
		| OpAssignOp op ->
			let f = get_binop_fun op e.epos in
			op_assign_op ~binop:(op,e.epos) jit f e1 e2 true
		| OpBoolAnd ->
			let exec1 = jit_expr jit false e1 in
			let exec2 = jit_expr jit false e2 in
			emit_bool_and exec1 exec2
		| OpBoolOr ->
			let exec1 = jit_expr jit false e1 in
			let exec2 = jit_expr jit false e2 in
			emit_bool_or exec1 exec2
		| OpAdd -> arith jit emit_op_add emit_op_add_le emit_op_add_lc emit_op_add_ll emit_op_add_ec e.epos e1 e2
		| OpSub -> arith jit emit_op_sub emit_op_sub_le emit_op_sub_lc emit_op_sub_ll emit_op_sub_ec e.epos e1 e2
		| OpMult -> arith jit emit_op_mult emit_op_mult_le emit_op_mult_lc emit_op_mult_ll emit_op_mult_ec e.epos e1 e2
		| OpDiv -> arith jit emit_op_div emit_op_div_le emit_op_div_lc emit_op_div_ll emit_op_div_ec e.epos e1 e2
		| OpAnd -> arith jit emit_op_and emit_op_and_le emit_op_and_lc emit_op_and_ll emit_op_and_ec e.epos e1 e2
		| OpOr -> arith jit emit_op_or emit_op_or_le emit_op_or_lc emit_op_or_ll emit_op_or_ec e.epos e1 e2
		| OpXor -> arith jit emit_op_xor emit_op_xor_le emit_op_xor_lc emit_op_xor_ll emit_op_xor_ec e.epos e1 e2
		| OpShl -> arith jit emit_op_shl emit_op_shl_le emit_op_shl_lc emit_op_shl_ll emit_op_shl_ec e.epos e1 e2
		| OpShr -> arith jit emit_op_shr emit_op_shr_le emit_op_shr_lc emit_op_shr_ll emit_op_shr_ec e.epos e1 e2
		| OpUShr -> arith jit emit_op_ushr emit_op_ushr_le emit_op_ushr_lc emit_op_ushr_ll emit_op_ushr_ec e.epos e1 e2
		| OpMod -> arith jit emit_op_mod emit_op_mod_le emit_op_mod_lc emit_op_mod_ll emit_op_mod_ec e.epos e1 e2
		| _ ->
			let exec1 = jit_expr jit false e1 in
			let exec2 = jit_expr jit false e2 in
			begin match op with
				| OpEq -> emit_op_eq exec1 exec2
				| OpNotEq -> emit_op_not_eq exec1 exec2
				| OpGt -> emit_op_gt exec1 exec2
				| OpGte -> emit_op_gte exec1 exec2
				| OpLt -> emit_op_lt exec1 exec2
				| OpLte -> emit_op_lte exec1 exec2
				| _ -> die "" __LOC__
			end
		end
	| TUnop(op,flag,v1) ->
		unop jit op flag v1 e.epos
	(* rewrites/skips *)
	| TParenthesis e1 | TMeta(_,e1) | TCast(e1,None) ->
		loop e1
	| TIdent "$__mk_pos__" ->
		emit_mk_pos e.epos
	| TIdent s ->
		Error.raise_typing_error ("Unknown identifier: " ^ s) e.epos
	in
	let f = loop e in
	begin match ctx.debug.debug_socket with
		| None ->
			f
		| Some socket ->
			let wrap () =
				EvalDebug.debug_loop jit socket.connection e f
			in
			begin match e.eexpr with
			| TCall _ | TNew _
			| TVar({v_kind = VUser _},_)
			| TIf _ | TWhile _ | TSwitch _ | TTry _
			| TReturn _ | TBreak | TContinue | TThrow _ | TCast(_,Some _) ->
				wrap()
			| TUnop((Increment | Decrement),_,e1) | TBinop((OpAssign | OpAssignOp _),e1,_) ->
				begin match (Texpr.skip e1).eexpr with
				| TLocal {v_kind = VGenerated | VInlined | VInlinedConstructorVariable _ | VExtractorVariable} ->
					f
				| _ ->
					wrap()
				end
			| _ ->
				f
		end
	end

and jit_tfunction jit static pos tf =
	let ctx = jit.ctx in
	push_scope jit pos;
	(* Declare `this` (if not static) and function arguments as local variables. *)
	if not static then ignore(declare_local_this jit);
	let fl = List.map (fun (var,_) ->
		let varacc = declare_local jit var in
		match varacc with
		| Env slot -> param_capture slot
		| Local slot -> param_local slot
	) tf.tf_args in
	let fl = Array.of_list (if static then fl else (param_local 0) :: fl) in
	let e = List.fold_right (fun (v,cto) e -> match cto with
		| None -> e
		| Some ct -> concat (Texpr.set_default ((EvalContext.curapi ctx).MacroApi.get_com()).Common.basic v ct e.epos) e
	) tf.tf_args tf.tf_expr in
	let has_final_return el = match List.rev el with
		| {eexpr = TReturn _} :: _ -> true
		| _ -> false
	in
	let e = match e.eexpr with
		| TBlock el when ctx.debug.support_debugger && (ExtType.is_void (follow tf.tf_type)) && not (has_final_return el) ->
			{e with eexpr = TBlock (el @ [mk (TReturn None) t_dynamic (Pos.with_min pos ((Pos.max pos)))])}
		| _ ->
			e
	in
	(* Jit the function expression. *)
	let exec = jit_expr jit true e in
	pop_scope jit;
	fl,exec

and get_env_creation jit static file info =
	create_env_info static file (jit.ctx.file_keys#get file) info jit.capture_infos jit.max_num_locals (IntHashtbl.length jit.captures)

let jit_timer ctx f =
	Timer.time ctx.timer_ctx [(if ctx.is_macro then "macro" else "interp");"jit"] f ()

(* Creates a [EvalValue.vfunc] of function [tf], which can be [static] or not. *)
let jit_tfunction ctx key_type key_field tf static pos =
	let f () =
		(* Create a new JitContext with an initial scope *)
		let jit = EvalJitContext.create ctx in
		let fl,exec = jit_tfunction jit static pos tf in
		(* Create the [vfunc] instance depending on the number of arguments. *)
		let hasret = jit.has_nonfinal_return in
		let eci = get_env_creation jit static (Pos.file tf.tf_expr.epos) (EKMethod(key_type,key_field)) in
		if hasret then create_function ctx eci exec fl else create_function_noret ctx eci exec fl
	in
	jit_timer ctx f

(* The same, jitted on the first call of the function: the prototypes of the
   macro context hold every method of its types, most of which are never
   called. The function keeps its identity (the method value). *)
let jit_tfunction_lazy ctx key_type key_field tf static pos =
	let jitted = AtomicLazy.from_fun (fun () -> jit_tfunction ctx key_type key_field tf static pos) in
	let cache = ref None in
	(fun vl -> match !cache with
		| Some f -> f vl
		| None ->
			let f = AtomicLazy.force jitted in
			cache := Some f;
			f vl
	)

(* JITs expression [e] to a function. This is used for expressions that are not in a method. *)
let jit_expr ctx e =
	let f () =
		let jit = EvalJitContext.create ctx in
		let f = jit_expr jit false (mk_block e) in
		jit,f
	in
	jit_timer ctx f
