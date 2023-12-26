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
				emit_proto_field_write proto (get_proto_field_index proto name) exec2
			| FInstance(c,_,_) when not (has_class_flag c CInterface) ->
				let proto = get_instance_prototype jit.ctx (path_hash c.cl_path) ef.epos in
				let i = get_instance_field_index proto name ef.epos in
				emit_instance_field_write exec1 ef.epos i exec2
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

and op_assign_op jit op e1 e2 prefix = match e1.eexpr with
	| TLocal var ->
		let exec = jit_expr jit false e2 in
		if has_var_flag var VCaptured then emit_capture_read_write (get_capture_slot jit var) exec op prefix
		else emit_local_read_write (get_slot jit var.v_id e1.epos) exec op prefix
	| TField(ef,fa) ->
		let name = hash (field_name fa) in
		let exec1 = jit_expr jit false ef in
		let exec2 = jit_expr jit false e2 in
		begin match fa with
			| FStatic({cl_path=path},_) ->
				let proto = get_static_prototype jit.ctx (path_hash path) ef.epos in
				emit_proto_field_read_write proto (get_proto_field_index proto name) exec2 op prefix
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
		let eci = get_env_creation jit_closure false tf.tf_expr.epos.pfile (EKLocalFunction jit.num_closures) in
		let captures = Hashtbl.fold (fun vid (i,declared) acc -> (i,vid,declared) :: acc) jit_closure.captures [] in
		let captures = List.sort (fun (i1,_,_) (i2,_,_) -> Stdlib.compare i1 i2) captures in
		(* Check if the out-of-scope var is in the outer scope because otherwise we have to promote outwards. *)
		List.iter (fun var -> ignore(get_capture_slot jit var)) jit_closure.captures_outside_scope;
		let captures = ExtList.List.filter_map (fun (i,vid,declared) ->
			if declared then None
			else Some (i,fst (try Hashtbl.find jit.captures vid with Not_found -> Error.raise_typing_error "Something went wrong" e.epos))
		) captures in
		let mapping = Array.of_list captures in
		emit_closure ctx mapping eci hasret exec fl
	(* branching *)
	| TIf(e1,e2,eo) ->
		let exec_cond = jit_expr jit false e1 in
		let exec_then = jit_expr jit return e2 in
		let exec_else = match eo with
			| None -> emit_null
			| Some e -> jit_expr jit return e
		in
		emit_if exec_cond exec_then exec_else
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
		let rec has_continue e = match e.eexpr with
			| TContinue -> true
			| TWhile _ | TFor _ | TFunction _ -> false
			| _ -> check_expr has_continue e
		in
		let exec_cond = jit_expr jit false e1 in
		let exec_body = jit_expr jit false e2 in
		begin match flag with
			| NormalWhile ->
				if has_continue e2 then emit_while_break_continue exec_cond exec_body
				else emit_while_break exec_cond exec_body
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
			emit_super_field_call slot proto i execs e.epos
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
				lazy (match proto.pfields.(i) with VFunction (f,_) -> f | v -> cannot_call v e.epos)
			in
			let jit_with_null_check ef =
				let exec = jit_expr jit false ef in
				emit_null_check exec ef.epos
			in
			let instance_call c =
				let exec = jit_with_null_check ef in
				let proto = get_instance_prototype ctx (path_hash c.cl_path) ef.epos in
				let v = lazy_proto_field proto in
				emit_proto_field_call v (exec :: execs) e.epos
			in
			let default () =
				let exec = jit_with_null_check ef in
				emit_method_call exec name execs e.epos
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
					emit_proto_field_call v execs e.epos
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
					let v = lazy (match Lazy.force fnew with VFunction (f,_) -> f | v -> cannot_call v e.epos) in
					emit_super_call v execs e.epos
				end
			| _ -> die "" __LOC__
			end
		| _ ->
			match e1.eexpr,el with
			| TIdent "$__mk_pos__",[file;min;max] ->
				let exec1 = jit_expr jit false file in
				let exec2 = jit_expr jit false min in
				let exec3 = jit_expr jit false max in
				emit_mk_pos exec1 exec2 exec3
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
			let v = lazy (match Lazy.force fnew with VFunction (f,_) -> f | v -> cannot_call v e.epos) in
			emit_constructor_call proto v execs e.epos
		end
	(* read *)
	| TLocal var ->
		if has_var_flag var VCaptured then emit_capture_read (get_capture_slot jit var)
		else emit_local_read (get_slot jit var.v_id e.epos)
	| TField(e1,fa) ->
		let name = hash (field_name fa) in
		begin match fa with
			| FInstance({cl_path=([],"Array")},_,{cf_name="length"}) -> emit_array_length_read (jit_expr jit false e1) e1.epos
			| FInstance({cl_path=(["eval"],"Vector")},_,{cf_name="length"}) -> emit_vector_length_read (jit_expr jit false e1) e1.epos
			| FInstance({cl_path=(["haxe";"io"],"Bytes")},_,{cf_name="length"}) -> emit_bytes_length_read (jit_expr jit false e1) e1.epos
			| FStatic({cl_path=path},_) | FEnum({e_path=path},_)
			| FInstance({cl_path=path},_,{cf_kind = Method (MethNormal | MethInline)}) ->
				let proto = get_static_prototype ctx (path_hash path) e1.epos in
				emit_proto_field_read proto (get_proto_field_index proto name)
			| FInstance(c,_,_) when not (has_class_flag c CInterface) ->
				let proto = get_instance_prototype ctx (path_hash c.cl_path) e1.epos in
				let i = get_instance_field_index proto name e1.epos in
				begin match e1.eexpr with
					| TConst TThis -> emit_this_field_read (get_slot jit 0 e.epos) i
					| _ -> emit_instance_field_read (jit_expr jit false e1) e1.epos i
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
				emit_vector_read exec1 e1.epos exec2 e2.epos
			| _ ->
				let exec1 = jit_expr jit false e1 in
				let exec2 = jit_expr jit false e2 in
				emit_array_read exec1 e1.epos exec2 e2.epos
		end
	| TEnumParameter(e1,_,i) ->
		let exec = jit_expr jit false e1 in
		emit_enum_parameter_read exec i
	| TEnumIndex e1 ->
		let exec = jit_expr jit false e1 in
		emit_enum_index exec e1.epos
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
				| _ -> die "" __LOC__
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
			| TFor _ | TIf _ | TWhile _ | TSwitch _ | TTry _
			| TReturn _ | TBreak | TContinue | TThrow _ | TCast(_,Some _) ->
				wrap()
			| TUnop((Increment | Decrement),_,e1) | TBinop((OpAssign | OpAssignOp _),e1,_) ->
				begin match (Texpr.skip e1).eexpr with
				| TLocal {v_kind = VGenerated | VInlined | VInlinedConstructorVariable | VExtractorVariable} ->
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
		| Env slot -> execute_set_capture slot
		| Local slot -> execute_set_local slot
	) tf.tf_args in
	let fl = if static then fl else (execute_set_local 0) :: fl in
	(* Add conditionals for default values. *)
	let e = List.fold_left (fun e (v,cto) -> match cto with
		| None -> e
		| Some ct -> concat (Texpr.set_default (ctx.curapi.MacroApi.get_com()).Common.basic v ct e.epos) e
	) tf.tf_expr tf.tf_args in
	let has_final_return el = match List.rev el with
		| {eexpr = TReturn _} :: _ -> true
		| _ -> false
	in
	let e = match e.eexpr with
		| TBlock el when ctx.debug.support_debugger && (ExtType.is_void (follow tf.tf_type)) && not (has_final_return el) ->
			{e with eexpr = TBlock (el @ [mk (TReturn None) t_dynamic {pos with pmin = pos.pmax}])}
		| _ ->
			e
	in
	(* Jit the function expression. *)
	let exec = jit_expr jit true e in
	pop_scope jit;
	fl,exec

and get_env_creation jit static file info =
	create_env_info static file (jit.ctx.file_keys#get file) info jit.capture_infos jit.max_num_locals (Hashtbl.length jit.captures)

let jit_timer ctx f =
	Std.finally (Timer.timer [(if ctx.is_macro then "macro" else "interp");"jit"]) f ()

(* Creates a [EvalValue.vfunc] of function [tf], which can be [static] or not. *)
let jit_tfunction ctx key_type key_field tf static pos =
	let f () =
		(* Create a new JitContext with an initial scope *)
		let jit = EvalJitContext.create ctx in
		let fl,exec = jit_tfunction jit static pos tf in
		(* Create the [vfunc] instance depending on the number of arguments. *)
		let hasret = jit.has_nonfinal_return in
		let eci = get_env_creation jit static tf.tf_expr.epos.pfile (EKMethod(key_type,key_field)) in
		if hasret then create_function ctx eci exec fl else create_function_noret ctx eci exec fl
	in
	jit_timer ctx f

(* JITs expression [e] to a function. This is used for expressions that are not in a method. *)
let jit_expr ctx e =
	let f () =
		let jit = EvalJitContext.create ctx in
		let f = jit_expr jit false (mk_block e) in
		jit,f
	in
	jit_timer ctx f
