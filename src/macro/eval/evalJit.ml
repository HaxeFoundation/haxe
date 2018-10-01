(*
	The Haxe Compiler
	Copyright (C) 2005-2018  Haxe Foundation

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
	| TFun _ | TAnon _ | TMono _ | TType _ | TLazy _ -> assert false

let eone = mk (TConst(TInt (Int32.one))) t_dynamic null_pos

let eval_const = function
	| TString s -> EvalString.create_unknown s
	| TInt i32 -> vint32 i32
	| TFloat f -> vfloat (float_of_string f)
	| TBool b -> vbool b
	| TNull -> vnull
	| TThis | TSuper -> assert false

let is_int t = match follow t with
	| TAbstract({a_path=[],"Int"},_) -> true
	| _ -> false

let is_string t = match follow t with
	| TInst({cl_path=[],"String"},_) -> true
	| _ -> false

open EvalJitContext

let rec op_assign ctx jit e1 e2 = match e1.eexpr with
	| TLocal var ->
		let exec = jit_expr jit false e2 in
		if var.v_capture then emit_capture_write (get_capture_slot jit var.v_id) exec
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
				emit_proto_field_write proto (get_proto_field_index proto name) exec2
			| FInstance(c,_,_) when not c.cl_interface ->
				let proto = get_instance_prototype jit.ctx (path_hash c.cl_path) ef.epos in
				let i = get_instance_field_index proto name ef.epos in
				emit_instance_field_write exec1 i exec2
			| FAnon cf ->
				begin match follow ef.etype with
					| TAnon an ->
						let l = PMap.foldi (fun k _ acc -> (hash k,()) :: acc) an.a_fields [] in
						let proto,_ = ctx.get_object_prototype ctx l in
						let i = get_instance_field_index proto name ef.epos in
						emit_anon_field_write exec1 proto i name exec2
					| _ ->
						emit_field_write exec1 name exec2 e1.epos
				end
			| _ ->
				emit_field_write exec1 name exec2 e1.epos
		end
	| TArray(ea1,ea2) ->
		begin match (follow ea1.etype) with
			| TInst({cl_path=(["eval"],"Vector")}, _) ->
				let exec1 = jit_expr jit false ea1 in
				let exec2 = jit_expr jit false ea2 in
				let exec3 = jit_expr jit false e2 in
				emit_vector_write exec1 exec2 exec3 ea2.epos
			| _ ->
				let exec1 = jit_expr jit false ea1 in
				let exec2 = jit_expr jit false ea2 in
				let exec3 = jit_expr jit false e2 in
				emit_array_write exec1 exec2 exec3 ea2.epos
		end

	| _ ->
		assert false

and op_assign_op jit op e1 e2 prefix = match e1.eexpr with
	| TLocal var ->
		let exec = jit_expr jit false e2 in
		if var.v_capture then emit_capture_read_write (get_capture_slot jit var.v_id) exec op prefix
		else emit_local_read_write (get_slot jit var.v_id e1.epos) exec op prefix
	| TField(ef,fa) ->
		let name = hash (field_name fa) in
		let exec1 = jit_expr jit false ef in
		let exec2 = jit_expr jit false e2 in
		begin match fa with
			| FStatic({cl_path=path},_) ->
				let proto = get_static_prototype jit.ctx (path_hash path) ef.epos in
				emit_proto_field_read_write proto (get_proto_field_index proto name) exec2 op prefix
			| FInstance(c,_,_) when not c.cl_interface ->
				let proto = get_instance_prototype jit.ctx (path_hash c.cl_path) ef.epos in
				let i = get_instance_field_index proto name ef.epos in
				emit_instance_field_read_write exec1 i exec2 op prefix
			| _ ->
				emit_field_read_write exec1 name exec2 op prefix e1.epos
		end
	| TArray(ea1,ea2) ->
		begin match (follow ea1.etype) with
			| TInst({cl_path=(["eval"],"Vector")}, _) ->
				let exec1 = jit_expr jit false ea1 in
				let exec2 = jit_expr jit false ea2 in
				let exec3 = jit_expr jit false e2 in
				emit_vector_read_write exec1 exec2 exec3 op prefix ea2.epos
			| _ ->
				let exec1 = jit_expr jit false ea1 in
				let exec2 = jit_expr jit false ea2 in
				let exec3 = jit_expr jit false e2 in
				emit_array_read_write exec1 exec2 exec3 op prefix ea2.epos
		end
	| _ ->
		assert false

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
		op_incr jit e1 (flag = Prefix) p
	| Decrement ->
		op_decr jit e1 (flag = Prefix) p

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
		jit_closure.captures <- jit.captures;
		jit_closure.capture_infos <- jit.capture_infos;
		jit.num_closures <- jit.num_closures + 1;
		let exec = jit_tfunction jit_closure true e.epos tf in
		let num_captures = Hashtbl.length jit.captures in
		let hasret = jit_closure.has_nonfinal_return in
		let get_env = get_env jit_closure false (file_hash tf.tf_expr.epos.pfile) (EKLocalFunction jit.num_closures) in
		emit_closure ctx num_captures get_env hasret exec
	(* branching *)
	| TIf(e1,e2,eo) ->
		let exec_cond = jit_expr jit false e1 in
		let exec_then = jit_expr jit return e2 in
		let exec_else = match eo with
			| None -> emit_null
			| Some e -> jit_expr jit return e
		in
		emit_if exec_cond exec_then exec_else
	| TSwitch(e1,cases,def) when is_int e1.etype ->
		let exec = jit_expr jit false e1 in
		let h = ref IntMap.empty in
		let max = ref 0 in
		let min = ref max_int in
		List.iter (fun (el,e) ->
			push_scope jit e.epos;
			let exec = jit_expr jit return e in
			List.iter (fun e -> match e.eexpr with
				| TConst (TInt i32) ->
					let i = Int32.to_int i32 in
					h := IntMap.add i exec !h;
					if i > !max then max := i;
					if i < !min then min := i;
				| _ -> assert false
			) el;
			pop_scope jit;
		) cases;
		let exec_def = jit_default jit return def in
		let l = !max - !min + 1 in
		if l < 256 then begin
			let cases = Array.init l (fun i -> try IntMap.find (i + !min) !h with Not_found -> exec_def) in
			emit_int_switch_array (- !min) exec cases exec_def e1.epos
		end else
			emit_int_switch_map exec !h exec_def e1.epos
	(* | TSwitch(e1,cases,def) when is_string e1.etype ->
		let exec = jit_expr jit false e1 in
		let h = ref PMap.empty in
		List.iter (fun (el,e) ->
			push_scope jit e.epos;
			let exec = jit_expr jit return e in
			List.iter (fun e -> match e.eexpr with
				| TConst (TString s) -> h := PMap.add s exec !h;
				| _ -> assert false
			) el;
			pop_scope jit;
		) cases;
		let exec_def = jit_default jit return def in
		emit_string_switch_map exec !h exec_def e1.epos *)
	| TSwitch(e1,cases,def) ->
		let exec = jit_expr jit false e1 in
		let execs = DynArray.create () in
		let patterns = List.map (fun (el,e) ->
			push_scope jit e.epos;
			let el = List.map (jit_expr jit false) el in
			DynArray.add execs (jit_expr jit return e);
			pop_scope jit;
			el
		) cases in
		let exec_def = jit_default jit return def in
		emit_switch exec (DynArray.to_array execs) (Array.of_list patterns) exec_def
	| TWhile({eexpr = TParenthesis e1},e2,flag) ->
		loop {e with eexpr = TWhile(e1,e2,flag)}
	| TWhile(e1,e2,flag) ->
		let exec_cond = jit_expr jit false e1 in
		let exec_body = jit_expr jit false e2 in
		begin match flag with
			| NormalWhile -> emit_while_break_continue exec_cond exec_body
			| DoWhile -> emit_do_while_break_continue exec_cond exec_body
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
	| TBlock (e1 :: el) ->
		let rec loop f el =
			match el with
				| [e] ->
					let f' = jit_expr jit return e in
					emit_seq f f'
				| e1 :: el ->
					let f' = jit_expr jit false e1 in
					let f = emit_seq f f' in
					loop f el
				| [] ->
					assert false
		in
		push_scope jit e.epos;
		let f0 = jit_expr jit false e1 in
		let f = loop f0 el in
		pop_scope jit;
		f
	| TReturn None ->
		if return then emit_null
		else begin
			jit.has_nonfinal_return <- true;
			emit_return_null
		end
	| TReturn (Some e1) ->
		let exec = jit_expr jit false e1 in
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
			emit_super_field_call slot proto i execs e.epos
		| TField(ef,fa) ->
			let name = hash (field_name fa) in
			let execs = List.map (jit_expr jit false) el in
			let is_final c cf =
				c.cl_final || cf.cf_final ||
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
			let instance_call c =
				let exec = jit_expr jit false ef in
				let proto = get_instance_prototype ctx (path_hash c.cl_path) ef.epos in
				let i = get_proto_field_index proto name in
				emit_proto_field_call proto i (exec :: execs) e.epos
			in
			let default () =
				let exec = jit_expr jit false ef in
				emit_method_call exec name execs e.epos
			in
			begin match fa with
				| FStatic({cl_path=[],"StringTools"},{cf_name="fastCodeAt"}) ->
					begin match execs with
						| [exec1;exec2] -> emit_string_cca exec1 exec2 e.epos
						| _ -> assert false
					end
				| FEnum({e_path=path},ef) ->
					let key = path_hash path in
					let pos = Some ef.ef_pos in
					emit_enum_construction key ef.ef_index (Array.of_list execs) pos
				| FStatic({cl_path=path},cf) when is_proper_method cf ->
					let proto = get_static_prototype ctx (path_hash path) ef.epos in
					let i = get_proto_field_index proto name in
					emit_proto_field_call proto i execs e.epos
				| FInstance(c,_,cf) when is_proper_method cf ->
					if not (is_final c cf) then
						default()
					else if not c.cl_interface then
						instance_call c
					(* If we have exactly one implementer, use it instead of the super class/interface. *)
					else if not ctx.is_macro && c.cl_implements = [] && c.cl_super = None then begin match c.cl_descendants with
						| [c'] when not c'.cl_interface && is_final c' cf ->
							instance_call c'
						| _ ->
							default()
					end else
						default()
				| _ ->
					let exec = jit_expr jit false ef in
					emit_field_call exec name execs e.epos
			end
		| TConst TSuper ->
			begin match follow e1.etype with
			| TInst(c,_) ->
				let key = (path_hash c.cl_path) in
				let execs = List.map (jit_expr jit false) el in
				begin try
					let f = get_special_instance_constructor_raise ctx key in
					emit_special_super_call f execs
				with Not_found ->
					let fnew = get_instance_constructor jit.ctx key e1.epos in
					emit_super_call fnew execs e.epos
				end
			| _ -> assert false
			end
		| _ ->
			match e1.eexpr,el with
			| TIdent "$__mk_pos__",[file;min;max] ->
				let exec1 = jit_expr jit false file in
				let exec2 = jit_expr jit false min in
				let exec3 = jit_expr jit false max in
				emit_mk_pos exec1 exec2 exec3
			| TIdent "$__delayed_call__",[{eexpr = TConst(TInt i)}] ->
				let f = ctx.curapi.MacroApi.delayed_macro (Int32.to_int i) in
				(fun env ->
					let f = f() in
					f()
				)
			| _ ->
				let exec = jit_expr jit false e1 in
				let execs = List.map (jit_expr jit false) el in
				emit_call exec execs e.epos
		end
	| TNew({cl_path=[],"Array"},_,_) ->
		emit_new_array
	| TNew({cl_path=["eval"],"Vector"},_,[e1]) ->
		begin match e1.eexpr with
			| TConst (TInt i32) ->
				emit_new_vector_int (Int32.to_int i32)
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
			emit_constructor_call proto fnew execs e.epos
		end
	(* read *)
	| TLocal var ->
		if var.v_capture then emit_capture_read (get_capture_slot jit var.v_id)
		else emit_local_read (get_slot jit var.v_id e.epos)
	| TField(e1,fa) ->
		let name = hash (field_name fa) in
		begin match fa with
			| FInstance({cl_path=([],"Array")},_,{cf_name="length"}) -> emit_array_length_read (jit_expr jit false e1)
			| FInstance({cl_path=(["eval"],"Vector")},_,{cf_name="length"}) -> emit_vector_length_read (jit_expr jit false e1)
			| FInstance({cl_path=(["haxe";"io"],"Bytes")},_,{cf_name="length"}) -> emit_bytes_length_read (jit_expr jit false e1)
			| FStatic({cl_path=path},_) | FEnum({e_path=path},_)
			| FInstance({cl_path=path},_,{cf_kind = Method (MethNormal | MethInline)}) ->
				let proto = get_static_prototype ctx (path_hash path) e1.epos in
				emit_proto_field_read proto (get_proto_field_index proto name)
			| FInstance(c,_,_) when not c.cl_interface ->
				let proto = get_instance_prototype ctx (path_hash c.cl_path) e1.epos in
				let i = get_instance_field_index proto name e1.epos in
				begin match e1.eexpr with
					| TConst TThis -> emit_this_field_read (get_slot jit 0 e.epos) i
					| _ -> emit_instance_field_read (jit_expr jit false e1) i
				end
			| FAnon _ ->
				begin match follow e1.etype with
					| TAnon an ->
						let l = PMap.foldi (fun k _ acc -> (hash k,()) :: acc) an.a_fields [] in
						let proto,_ = ctx.get_object_prototype ctx l in
						let i = get_instance_field_index proto name e1.epos in
						emit_anon_field_read (jit_expr jit false e1) proto i name e1.epos
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
				emit_vector_read exec1 exec2 e2.epos
			| _ ->
				let exec1 = jit_expr jit false e1 in
				let exec2 = jit_expr jit false e2 in
				emit_array_read exec1 exec2 e2.epos
		end
	| TEnumParameter(e1,_,i) ->
		let exec = jit_expr jit false e1 in
		emit_enum_parameter_read exec i
	| TEnumIndex e1 ->
		let exec = jit_expr jit false e1 in
		emit_enum_index exec
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
			op_assign_op jit f e1 e2 true
		| OpBoolAnd ->
			let exec1 = jit_expr jit false e1 in
			let exec2 = jit_expr jit false e2 in
			emit_bool_and exec1 exec2
		| OpBoolOr ->
			let exec1 = jit_expr jit false e1 in
			let exec2 = jit_expr jit false e2 in
			emit_bool_or exec1 exec2
		| _ ->
			let exec1 = jit_expr jit false e1 in
			let exec2 = jit_expr jit false e2 in
			begin match op with
				| OpAdd -> emit_op_add e.epos exec1 exec2
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
		loop (Texpr.for_remap (ctx.curapi.MacroApi.get_com()).Common.basic v e1 e2 e.epos)
	| TParenthesis e1 | TMeta(_,e1) | TCast(e1,None) ->
		loop e1
	| TIdent s ->
		Error.error ("Unknown identifier: " ^ s) e.epos
	in
	let f = loop e in
	begin match ctx.debug.debug_socket with
		| None ->
			f
		| Some socket -> begin match e.eexpr with
			| TConst _ | TLocal _ | TTypeExpr _ | TBlock _ | TField _ -> f
			| _ -> EvalDebug.debug_loop jit socket.connection e f
		end
	end

and jit_tfunction jit static pos tf =
	let ctx = jit.ctx in
	push_scope jit pos;
	(* Declare `this` (if not static) and function arguments as local variables. *)
	if not static then ignore(declare_local_this jit);
	let varaccs = ExtList.List.filter_map (fun (var,_) ->
		let slot = add_local jit var in
		if var.v_capture then Some (slot,add_capture jit var) else None
	) tf.tf_args in
	(* Add conditionals for default values. *)
	let e = List.fold_left (fun e (v,cto) -> match cto with
		| None -> e
		| Some ct -> concat (Texpr.set_default (ctx.curapi.MacroApi.get_com()).Common.basic v ct e.epos) e
	) tf.tf_expr tf.tf_args in
	(* Jit the function expression. *)
	let exec = jit_expr jit true e in
	(* Deal with captured arguments, if necessary. *)
	let exec = match varaccs with
		| [] -> exec
		| _ -> handle_capture_arguments exec varaccs
	in
	pop_scope jit;
	exec

and get_env jit static file info =
	let ctx = jit.ctx in
	let num_locals = jit.max_num_locals in
	let num_captures = Hashtbl.length jit.captures in
	let info = create_env_info static file info jit.capture_infos in
	match info.kind with
	| EKLocalFunction _ -> get_closure_env ctx info num_locals num_captures
	| _ -> get_normal_env ctx info num_locals num_captures

(* Creates a [EvalValue.vfunc] of function [tf], which can be [static] or not. *)
let jit_tfunction ctx key_type key_field tf static pos =
	let t = Timer.timer [(if ctx.is_macro then "macro" else "interp");"jit"] in
	(* Create a new JitContext with an initial scope *)
	let jit = EvalJitContext.create ctx in
	let exec = jit_tfunction jit static pos tf in
	(* Create the [vfunc] instance depending on the number of arguments. *)
	let hasret = jit.has_nonfinal_return in
	let get_env = get_env jit static (file_hash tf.tf_expr.epos.pfile) (EKMethod(key_type,key_field)) in
	let f = create_function ctx get_env hasret empty_array exec in
	t();
	f

(* JITs expression [e] to a function. This is used for expressions that are not in a method. *)
let jit_expr ctx e =
	let t = Timer.timer [(if ctx.is_macro then "macro" else "interp");"jit"] in
	let jit = EvalJitContext.create ctx in
	let f = jit_expr jit false (mk_block e) in
	t();
	jit,f