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
open EvalHash
open EvalValue
open EvalEncode
open EvalDecode
open EvalContext
open EvalPrinting
open EvalExceptions
open EvalField
open EvalMisc

type varacc =
	| Local of int
	| Env of int

(* Helper *)

let throw_string s p =
	throw (encode_string s) p

let invalid_binop op v1 v2 p =
	throw_string (Printf.sprintf "Invalid operation: %s %s %s" (value_string v1) (s_binop op) (value_string v2)) p

let unexpected_value_p v s p =
	let str = Printf.sprintf "Unexpected value %s, expected %s" (value_string v) s in
	throw_string str p

let cannot_call v p =
	throw (encode_string ("Cannot call " ^ (value_string v))) p

let decode_int_p v p = match v with
	| VInt32 i -> Int32.to_int i
	| VFloat f -> int_of_float f
	| _ -> unexpected_value_p v "int" p

(* Emitter *)

let apply env exec =
	exec env

(* Objects and values *)

let emit_null _ = vnull

let emit_local_declaration i exec env =
	env.env_locals.(i) <- exec env;
	vnull

let emit_capture_declaration i exec env =
	env.env_captures.(i) <- ref (exec env);
	vnull

let emit_const v _ = v

let emit_new_array env =
	encode_array_instance (EvalArray.create [||])

let emit_new_vector_int i env =
	encode_vector_instance (Array.make i vnull)

let emit_new_vector exec p env =
	encode_vector_instance (Array.make (decode_int_p (exec env) p) vnull)

let emit_special_instance f execs env =
	let vl = List.map (apply env) execs in
	f vl

let emit_object_declaration proto fa env =
	let a = Array.make (Array.length fa) vnull in
	Array.iter (fun (i,exec) -> a.(i) <- exec env) fa;
	vobject {
		ofields = a;
		oproto = proto;
		oextra = IntMap.empty;
		oremoved = IntMap.empty;
	}

let emit_array_declaration execs env =
	let vl = Array.map (apply env) execs in
	encode_array_instance (EvalArray.create vl)

let emit_type_expr proto env = proto

let emit_mk_pos exec1 exec2 exec3 env =
	let file = exec1 env in
	let min = exec2 env in
	let max = exec3 env in
	encode_pos { pfile = decode_string file; pmin = decode_int min; pmax = decode_int max }

let emit_enum_construction0 key i p env =
	encode_enum_value key i [||] p

let emit_enum_construction1 key i exec1 p env =
	let v1 = exec1 env in
	encode_enum_value key i [|v1|] p

let emit_enum_construction2 key i exec1 exec2 p env =
	let v1 = exec1 env in
	let v2 = exec2 env in
	encode_enum_value key i [|v1;v2|] p

let emit_enum_construction3 key i exec1 exec2 exec3 p env =
	let v1 = exec1 env in
	let v2 = exec2 env in
	let v3 = exec3 env in
	encode_enum_value key i [|v1;v2;v3|] p

let emit_enum_construction4 key i exec1 exec2 exec3 exec4 p env =
	let v1 = exec1 env in
	let v2 = exec2 env in
	let v3 = exec3 env in
	let v4 = exec4 env in
	encode_enum_value key i [|v1;v2;v3;v4|] p

let emit_enum_construction5 key i exec1 exec2 exec3 exec4 exec5 p env =
	let v1 = exec1 env in
	let v2 = exec2 env in
	let v3 = exec3 env in
	let v4 = exec4 env in
	let v5 = exec5 env in
	encode_enum_value key i [|v1;v2;v3;v4;v5|] p

let emit_enum_construction key i execs p env =
	encode_enum_value key i (Array.map (fun exec -> exec env) execs) p

(* Branching *)

let emit_if exec_cond exec_then exec_else env =
	match exec_cond env with
	| VTrue -> exec_then env
	| _ -> exec_else env

let emit_enum_switch_array exec cases exec_def p env = match exec env with
	| VEnumValue ev ->
		let i = ev.eindex in
		if i >= Array.length cases || i < 0 then exec_def env
		else (Array.unsafe_get cases i) env
	| v ->
		unexpected_value_p v "enum value" p

let emit_int_switch_array exec cases exec_def p env = match exec env with
	| VInt32 i32 ->
		let i = Int32.to_int i32 in
		if i >= Array.length cases || i < 0 then exec_def env
		else (Array.unsafe_get cases i) env
	| VNull ->
		exec_def env
	| v ->
		unexpected_value_p v "int" p

let emit_int_switch_array_shift shift exec cases exec_def p env = match exec env with
	| VInt32 i32 ->
		let i = Int32.to_int i32 + shift in
		if i >= Array.length cases || i < 0 then exec_def env
		else (Array.unsafe_get cases i) env
	| VNull ->
		exec_def env
	| v ->
		unexpected_value_p v "int" p

let emit_int_switch_map exec cases exec_def p env = match exec env with
	| VInt32 i32 ->
		let i = Int32.to_int i32 in
		begin try
			(IntMap.find i cases) env
		with Not_found ->
			exec_def env
		end
	| v ->
		unexpected_value_p v "int" p

let emit_constant_switch exec execs constants exec_def env =
	let v1 = exec env in
	let rec loop v1 i =
		if i >= Array.length constants then exec_def env
		else if List.exists (fun v2 -> equals v1 v2) (Array.unsafe_get constants i) then
			(Array.unsafe_get execs i) env
		else
			loop v1 (i + 1)
	in
	loop v1 0

let emit_switch exec execs patterns exec_def env =
	let v1 = exec env in
	let rec loop v1 i =
		if i >= Array.length patterns then exec_def env
		else if List.exists (fun exec -> equals v1 (exec env)) patterns.(i) then
			execs.(i) env
		else
			loop v1 (i + 1)
	in
	loop v1 0

let emit_int_iterator slot exec1 exec2 p1 p2 env =
	let i1 = decode_int_p (env.env_locals.(slot)) p1 in
	let i2 = decode_int_p (exec1 env) p2 in
	for i = i1 to i2 - 1 do
		env.env_locals.(slot) <- vint i;
		ignore(exec2 env);
	done;
	vnull

let emit_int_iterator_continue slot exec1 exec2 p1 p2 env =
	let i1 = decode_int_p (env.env_locals.(slot)) p1 in
	let i2 = decode_int_p (exec1 env) p2 in
	for i = i1 to i2 - 1 do
		env.env_locals.(slot) <- vint i;
		(try ignore(exec2 env) with Continue -> ())
	done;
	vnull

let emit_int_iterator_break slot exec1 exec2 p1 p2 env =
	let i1 = decode_int_p (env.env_locals.(slot)) p1 in
	let i2 = decode_int_p (exec1 env) p2 in
	begin try
		for i = i1 to i2 - 1 do
			env.env_locals.(slot) <- vint i;
			ignore(exec2 env);
		done;
	with Break ->
		()
	end;
	vnull

let emit_int_iterator_break_continue slot exec1 exec2 p1 p2 env =
	let i1 = decode_int_p (env.env_locals.(slot)) p1 in
	let i2 = decode_int_p (exec1 env) p1 in
	begin try
		for i = i1 to i2 - 1 do
			env.env_locals.(slot) <- vint i;
			(try ignore(exec2 env) with Continue -> ())
		done;
	with Break ->
		()
	end;
	vnull

let emit_while_gte exec1 f exec2 env =
	while (num (exec1 env) >= f) do exec2 env done;
	vnull

let rec run_while_continue exec_cond exec_body env =
	try
		while is_true (exec_cond env) do exec_body env done;
	with Continue ->
		run_while_continue exec_cond exec_body env

let emit_while exec_cond exec_body env =
	while is_true (exec_cond env) do exec_body env done;
	vnull

let emit_while_break exec_cond exec_body env =
	begin try
		while is_true (exec_cond env) do exec_body env done;
	with Break ->
		()
	end;
	vnull

let emit_while_continue exec_cond exec_body env =
	run_while_continue exec_cond exec_body env;
	vnull

let emit_while_break_continue exec_cond exec_body env =
	(try run_while_continue exec_cond exec_body env with Break -> ());
	vnull

let emit_do_while exec_cond exec_body env =
	ignore(exec_body env);
	while is_true (exec_cond env) do exec_body env done;
	vnull

let emit_do_while_break exec_cond exec_body env =
	begin try
		ignore(exec_body env);
		while is_true (exec_cond env) do exec_body env done;
	with Break ->
		()
	end;
	vnull

let emit_do_while_continue exec_cond exec_body env =
	(try ignore(exec_body env) with Continue -> ());
	run_while_continue exec_cond exec_body env;
	vnull

let emit_do_while_break_continue exec_cond exec_body env =
	begin try
		ignore(exec_body env); run_while_continue exec_cond exec_body env
	with
		| Break -> ()
		| Continue -> try run_while_continue exec_cond exec_body env with Break -> ()
	end;
	vnull

let emit_try exec catches env =
	let ctx = get_ctx() in
	let eval = get_eval ctx in
	let environment_offset = eval.environment_offset in
	if ctx.debug.support_debugger then begin
		List.iter (fun (_,path,_) -> Hashtbl.add ctx.debug.caught_types path true) catches
	end;
	let restore () =
		List.iter (fun (_,path,_) -> Hashtbl.remove ctx.debug.caught_types path) catches
	in
	let v = try
		let v = exec env in
		restore();
		v
	with RunTimeException(v,_,_) as exc ->
		restore();
		build_exception_stack ctx environment_offset;
		eval.environment_offset <- environment_offset;
		let exec,_,varacc =
			try
				List.find (fun (_,path,i) -> is v path) catches
			with Not_found ->
				raise exc
		in
		begin match varacc with
			| Local slot -> env.env_locals.(slot) <- v
			| Env slot -> env.env_captures.(slot) <- ref v
		end;
		exec env
	in
	v

(* Control flow *)

let emit_block1 exec1 env =
	exec1 env

let emit_block2 exec1 exec2 env =
	ignore(exec1 env);
	exec2 env

let emit_block3 exec1 exec2 exec3 env =
	ignore(exec1 env);
	ignore(exec2 env);
	exec3 env

let emit_block4 exec1 exec2 exec3 exec4 env =
	ignore(exec1 env);
	ignore(exec2 env);
	ignore(exec3 env);
	exec4 env

let emit_block5 exec1 exec2 exec3 exec4 exec5 env =
	ignore(exec1 env);
	ignore(exec2 env);
	ignore(exec3 env);
	ignore(exec4 env);
	exec5 env

let emit_block execs env =
	let l = Array.length execs in
	for i = 0 to l - 2 do
		ignore((Array.unsafe_get execs i) env)
	done;
	(Array.unsafe_get execs (l -1)) env

let emit_value exec env =
	exec env

let emit_return_null _ = raise (Return vnull)

let emit_return_value exec env = raise (Return (exec env))

let emit_break env = raise Break

let emit_continue env = raise Continue

let emit_throw exec p env = throw (exec env) p

let emit_safe_cast exec t p env =
	let v1 = exec env in
	if not (is v1 t) then throw_string "Class cast error" p;
	v1

(* Calls *)

(* super.call() - immediate *)

let emit_super_field_call slot proto i execs p env =
	let vthis = env.env_locals.(slot) in
	let vf = proto.pfields.(i) in
	let vl = List.map (fun f -> f env) execs in
	call_value_on vthis vf vl

(* Type.call() - immediate *)

let call0 v p env =
	env.env_leave_pmin <- p.pmin;
	env.env_leave_pmax <- p.pmax;
	match v with
	| VFunction (Fun0 f,_) -> f ()
	| VFunction (FunN f,_) -> f []
	| VFieldClosure(v0,f) -> call_function f [v0]
	| VInstance {ikind = ILazyType(_,get)} -> get()
	| _ -> cannot_call v p

let call1 v v1 p env =
	env.env_leave_pmin <- p.pmin;
	env.env_leave_pmax <- p.pmax;
	match v with
	| VFunction (Fun1 f,_) -> f v1
	| VFunction (FunN f,_) -> f [v1]
	| VFieldClosure(v0,f) -> call_function f [v0;v1]
	| _ -> cannot_call v p

let call2 v v1 v2 p env =
	env.env_leave_pmin <- p.pmin;
	env.env_leave_pmax <- p.pmax;
	match v with
	| VFunction (Fun2 f,_) -> f v1 v2
	| VFunction (FunN f,_) -> f [v1;v2]
	| VFieldClosure(v0,f) -> call_function f [v0;v1;v2]
	| _ -> cannot_call v p

let call3 v v1 v2 v3 p env =
	env.env_leave_pmin <- p.pmin;
	env.env_leave_pmax <- p.pmax;
	match v with
	| VFunction (Fun3 f,_) -> f v1 v2 v3
	| VFunction (FunN f,_) -> f [v1;v2;v3]
	| VFieldClosure(v0,f) -> call_function f [v0;v1;v2;v3]
	| _ -> cannot_call v p

let call4 v v1 v2 v3 v4 p env =
	env.env_leave_pmin <- p.pmin;
	env.env_leave_pmax <- p.pmax;
	match v with
	| VFunction (Fun4 f,_) -> f v1 v2 v3 v4
	| VFunction (FunN f,_) -> f [v1;v2;v3;v4]
	| VFieldClosure(v0,f) -> call_function f [v0;v1;v2;v3;v4]
	| _ -> cannot_call v p

let call5 v v1 v2 v3 v4 v5 p env =
	env.env_leave_pmin <- p.pmin;
	env.env_leave_pmax <- p.pmax;
	match v with
	| VFunction (Fun5 f,_) -> f v1 v2 v3 v4 v5
	| VFunction (FunN f,_) -> f [v1;v2;v3;v4;v5]
	| VFieldClosure(v0,f) -> call_function f [v0;v1;v2;v3;v4;v5]
	| _ -> cannot_call v p

let emit_proto_field_call proto i execs p =
	match execs with
		| [] ->
			let vf = lazy (match proto.pfields.(i) with VFunction (Fun0 f,_) -> f | v -> cannot_call v p) in
			(fun env ->
				env.env_leave_pmin <- p.pmin;
				env.env_leave_pmax <- p.pmax;
				(Lazy.force vf) ()
			)
		| [exec1] ->
			let vf = lazy (match proto.pfields.(i) with VFunction (Fun1 f,_) -> f | v -> cannot_call v p) in
			(fun env ->
				let f = Lazy.force vf in
				let v1 = exec1 env in
				env.env_leave_pmin <- p.pmin;
				env.env_leave_pmax <- p.pmax;
				f v1
			)
		| [exec1;exec2] ->
			let vf = lazy (match proto.pfields.(i) with VFunction (Fun2 f,_) -> f | v -> cannot_call v p) in
			(fun env ->
				let f = Lazy.force vf in
				let v1 = exec1 env in
				let v2 = exec2 env in
				env.env_leave_pmin <- p.pmin;
				env.env_leave_pmax <- p.pmax;
				f v1 v2
			)
		| [exec1;exec2;exec3] ->
			let vf = lazy (match proto.pfields.(i) with VFunction (Fun3 f,_) -> f | v -> cannot_call v p) in
			(fun env ->
				let f = Lazy.force vf in
				let v1 = exec1 env in
				let v2 = exec2 env in
				let v3 = exec3 env in
				env.env_leave_pmin <- p.pmin;
				env.env_leave_pmax <- p.pmax;
				f v1 v2 v3
			)
		| [exec1;exec2;exec3;exec4] ->
			let vf = lazy (match proto.pfields.(i) with VFunction (Fun4 f,_) -> f | v -> cannot_call v p) in
			(fun env ->
				let f = Lazy.force vf in
				let v1 = exec1 env in
				let v2 = exec2 env in
				let v3 = exec3 env in
				let v4 = exec4 env in
				env.env_leave_pmin <- p.pmin;
				env.env_leave_pmax <- p.pmax;
				f v1 v2 v3 v4
			)
		| [exec1;exec2;exec3;exec4;exec5] ->
			let vf = lazy (match proto.pfields.(i) with VFunction (Fun5 f,_) -> f | v -> cannot_call v p) in
			(fun env ->
				let f = Lazy.force vf in
				let v1 = exec1 env in
				let v2 = exec2 env in
				let v3 = exec3 env in
				let v4 = exec4 env in
				let v5 = exec5 env in
				env.env_leave_pmin <- p.pmin;
				env.env_leave_pmax <- p.pmax;
				f v1 v2 v3 v4 v5
			)
		| _ ->
			let vf = lazy (match proto.pfields.(i) with VFunction (FunN f,_) -> f | v -> cannot_call v p) in
			(fun env ->
				let f = Lazy.force vf in
				let vl = List.map (fun exec -> exec env) execs in
				env.env_leave_pmin <- p.pmin;
				env.env_leave_pmax <- p.pmax;
				f vl
			)

(* instance.call() where call is overridden - dynamic dispatch *)

let emit_method_call exec name execs p =
	let vf vthis = match vthis with
		| VInstance {iproto = proto} | VPrototype proto -> proto_field_raise proto name
		| VString _ -> proto_field_raise (get_ctx()).string_prototype name
		| _ -> unexpected_value_p vthis "instance" p
	in
	match execs with
		| [] ->
			(fun env ->
				let vthis = exec env in
				let vf = vf vthis in
				call1 vf vthis p env
			)
		| [exec1] ->
			(fun env ->
				let vthis = exec env in
				let vf = vf vthis in
				let v1 = exec1 env in
				call2 vf vthis v1 p env
			)
		| [exec1;exec2] ->
			(fun env ->
				let vthis = exec env in
				let vf = vf vthis in
				let v1 = exec1 env in
				let v2 = exec2 env in
				call3 vf vthis v1 v2 p env
			)
		| [exec1;exec2;exec3] ->
			(fun env ->
				let vthis = exec env in
				let vf = vf vthis in
				let v1 = exec1 env in
				let v2 = exec2 env in
				let v3 = exec3 env in
				call4 vf vthis v1 v2 v3 p env
			)
		| [exec1;exec2;exec3;exec4] ->
			(fun env ->
				let vthis = exec env in
				let vf = vf vthis in
				let v1 = exec1 env in
				let v2 = exec2 env in
				let v3 = exec3 env in
				let v4 = exec4 env in
				call5 vf vthis v1 v2 v3 v4 p env
			)
		| _ ->
			(fun env ->
				let vthis = exec env in
				let vf = vf vthis in
				let vl = List.map (apply env) execs in
				env.env_leave_pmin <- p.pmin;
				env.env_leave_pmax <- p.pmax;
				call_value_on vthis vf vl
			)

(* instance.call() where call is not a method - lookup + this-binding *)

let emit_field_call exec name execs p env =
	let vthis = exec env in
	let vf = field vthis name in
	env.env_leave_pmin <- p.pmin;
	env.env_leave_pmax <- p.pmax;
	call_value_on vthis vf (List.map (apply env) execs)

(* new() - immediate + this-binding *)

let emit_constructor_call0 proto vf p env =
	let vthis = create_instance_direct proto in
	env.env_leave_pmin <- p.pmin;
	env.env_leave_pmax <- p.pmax;
	ignore((Lazy.force vf) vthis);
	vthis

let emit_constructor_call1 proto vf exec1 p env =
	let f = Lazy.force vf in
	let vthis = create_instance_direct proto in
	let v1 = exec1 env in
	env.env_leave_pmin <- p.pmin;
	env.env_leave_pmax <- p.pmax;
	ignore(f vthis v1);
	vthis

let emit_constructor_call2 proto vf exec1 exec2 p env =
	let f = Lazy.force vf in
	let vthis = create_instance_direct proto in
	let v1 = exec1 env in
	let v2 = exec2 env in
	env.env_leave_pmin <- p.pmin;
	env.env_leave_pmax <- p.pmax;
	ignore(f vthis v1 v2);
	vthis

let emit_constructor_call3 proto vf exec1 exec2 exec3 p env =
	let f = Lazy.force vf in
	let vthis = create_instance_direct proto in
	let v1 = exec1 env in
	let v2 = exec2 env in
	let v3 = exec3 env in
	env.env_leave_pmin <- p.pmin;
	env.env_leave_pmax <- p.pmax;
	ignore(f vthis v1 v2 v3);
	vthis

let emit_constructor_call4 proto vf exec1 exec2 exec3 exec4 p env =
	let f = Lazy.force vf in
	let vthis = create_instance_direct proto in
	let v1 = exec1 env in
	let v2 = exec2 env in
	let v3 = exec3 env in
	let v4 = exec4 env in
	env.env_leave_pmin <- p.pmin;
	env.env_leave_pmax <- p.pmax;
	ignore(f vthis v1 v2 v3 v4);
	vthis

let emit_constructor_callN proto vf execs p env =
	let f = Lazy.force vf in
	let vthis = create_instance_direct proto in
	let vl = List.map (fun exec -> exec env) execs in
	env.env_leave_pmin <- p.pmin;
	env.env_leave_pmax <- p.pmax;
	ignore(f (vthis :: vl));
	vthis

let emit_constructor_call proto fnew execs p =
	match execs with
		| [] ->
			let vf = lazy (match Lazy.force fnew with VFunction (Fun1 f,_) -> f | v -> cannot_call v p) in
			emit_constructor_call0 proto vf p
		| [exec1] ->
			let vf = lazy (match Lazy.force fnew with VFunction (Fun2 f,_) -> f | v -> cannot_call v p) in
			emit_constructor_call1 proto vf exec1 p
		| [exec1;exec2] ->
			let vf = lazy (match Lazy.force fnew with VFunction (Fun3 f,_) -> f | v -> cannot_call v p) in
			emit_constructor_call2 proto vf exec1 exec2 p
		| [exec1;exec2;exec3] ->
			let vf = lazy (match Lazy.force fnew with VFunction (Fun4 f,_) -> f | v -> cannot_call v p) in
			emit_constructor_call3 proto vf exec1 exec2 exec3 p
		| [exec1;exec2;exec3;exec4] ->
			let vf = lazy (match Lazy.force fnew with VFunction (Fun5 f,_) -> f | v -> cannot_call v p) in
			emit_constructor_call4 proto vf exec1 exec2 exec3 exec4 p
		| _ ->
			let vf = lazy (match Lazy.force fnew with VFunction (FunN f,_) -> f | v -> cannot_call v p) in
			emit_constructor_callN proto vf execs p

(* super() - immediate + this-binding *)

let emit_super_call0 vf p env =
	let vthis = env.env_locals.(0) in
	env.env_leave_pmin <- p.pmin;
	env.env_leave_pmax <- p.pmax;
	ignore((Lazy.force vf) vthis);
	vthis

let emit_super_call1 vf exec1 p env =
	let f = Lazy.force vf in
	let vthis = env.env_locals.(0) in
	let v1 = exec1 env in
	env.env_leave_pmin <- p.pmin;
	env.env_leave_pmax <- p.pmax;
	ignore(f vthis v1);
	vthis

let emit_super_call2 vf exec1 exec2 p env =
	let f = Lazy.force vf in
	let vthis = env.env_locals.(0) in
	let v1 = exec1 env in
	let v2 = exec2 env in
	env.env_leave_pmin <- p.pmin;
	env.env_leave_pmax <- p.pmax;
	ignore(f vthis v1 v2);
	vthis

let emit_super_call3 vf exec1 exec2 exec3 p env =
	let f = Lazy.force vf in
	let vthis = env.env_locals.(0) in
	let v1 = exec1 env in
	let v2 = exec2 env in
	let v3 = exec3 env in
	env.env_leave_pmin <- p.pmin;
	env.env_leave_pmax <- p.pmax;
	ignore(f vthis v1 v2 v3);
	vthis

let emit_super_call4 vf exec1 exec2 exec3 exec4 p env =
	let f = Lazy.force vf in
	let vthis = env.env_locals.(0) in
	let v1 = exec1 env in
	let v2 = exec2 env in
	let v3 = exec3 env in
	let v4 = exec4 env in
	env.env_leave_pmin <- p.pmin;
	env.env_leave_pmax <- p.pmax;
	ignore(f vthis v1 v2 v3 v4);
	vthis

let emit_super_callN vf execs p env =
	let f = Lazy.force vf in
	let vthis = env.env_locals.(0) in
	let vl = List.map (fun exec -> exec env) execs in
	env.env_leave_pmin <- p.pmin;
	env.env_leave_pmax <- p.pmax;
	ignore(f (vthis :: vl));
	vthis

let emit_special_super_call fnew execs env =
	let vl = List.map (apply env) execs in
	let vi' = fnew vl in
	let vthis = env.env_locals.(0) in
	(* This isn't very elegant, but it's probably a rare case to extend these types. *)
	begin match vthis,vi' with
		| VInstance vi,VInstance vi' -> vi.ikind <- vi'.ikind
		| _ -> assert false
	end;
	vnull

let emit_super_call fnew execs p =
	match execs with
		| [] ->
			let vf = lazy (match Lazy.force fnew with VFunction (Fun1 f,_) -> f | v -> cannot_call v p) in
			emit_super_call0 vf p
		| [exec1] ->
			let vf = lazy (match Lazy.force fnew with VFunction (Fun2 f,_) -> f | v -> cannot_call v p) in
			emit_super_call1 vf exec1 p
		| [exec1;exec2] ->
			let vf = lazy (match Lazy.force fnew with VFunction (Fun3 f,_) -> f | v -> cannot_call v p) in
			emit_super_call2 vf exec1 exec2 p
		| [exec1;exec2;exec3] ->
			let vf = lazy (match Lazy.force fnew with VFunction (Fun4 f,_) -> f | v -> cannot_call v p) in
			emit_super_call3 vf exec1 exec2 exec3 p
		| [exec1;exec2;exec3;exec4] ->
			let vf = lazy (match Lazy.force fnew with VFunction (Fun5 f,_) -> f | v -> cannot_call v p) in
			emit_super_call4 vf exec1 exec2 exec3 exec4 p
		| _ ->
			let vf = lazy (match Lazy.force fnew with VFunction (FunN f,_) -> f | v -> cannot_call v p) in
			emit_super_callN vf execs p

(* unknown call - full lookup *)

let emit_call0 exec p env =
	call0 (exec env) p env

let emit_call1 exec exec1 p env =
	let v0 = exec env in
	let v1 = exec1 env in
	call1 v0 v1 p env

let emit_call2 exec exec1 exec2 p env =
	let v0 = exec env in
	let v1 = exec1 env in
	let v2 = exec2 env in
	call2 v0 v1 v2 p env

let emit_call3 exec exec1 exec2 exec3 p env =
	let v0 = exec env in
	let v1 = exec1 env in
	let v2 = exec2 env in
	let v3 = exec3 env in
	call3 v0 v1 v2 v3 p env

let emit_call4 exec exec1 exec2 exec3 exec4 p env =
	let v0 = exec env in
	let v1 = exec1 env in
	let v2 = exec2 env in
	let v3 = exec3 env in
	let v4 = exec4 env in
	call4 v0 v1 v2 v3 v4 p env

let emit_call5 exec exec1 exec2 exec3 exec4 exec5 p env =
	let v0 = exec env in
	let v1 = exec1 env in
	let v2 = exec2 env in
	let v3 = exec3 env in
	let v4 = exec4 env in
	let v5 = exec5 env in
	call5 v0 v1 v2 v3 v4 v5 p env

let emit_call exec execs p env =
	let v1 = exec env in
	env.env_leave_pmin <- p.pmin;
	env.env_leave_pmax <- p.pmax;
	call_value v1 (List.map (apply env) execs)

(* Read *)

let emit_local_read i env = env.env_locals.(i)

let emit_capture_read i env = !(env.env_captures.(i))

let emit_array_length_read exec env = match exec env with
	| VArray va -> vint (va.alength)
	| v -> unexpected_value v "Array"

let emit_vector_length_read exec env = match exec env with
	| VVector vv -> vint (Array.length vv)
	| v -> unexpected_value v "Vector"

let emit_bytes_length_read exec env = match exec env with
	| VInstance {ikind = IBytes s} -> vint (Bytes.length s)
	| v -> unexpected_value v "Bytes"

let emit_proto_field_read proto i env =
	proto.pfields.(i)

let emit_instance_local_field_read iv i env = match env.env_locals.(iv) with
	| VInstance vi -> vi.ifields.(i)
	| VString(_,s) -> vint (String.length (Lazy.force s))
	| v -> unexpected_value v "instance"

let emit_instance_field_read exec i env = match exec env with
	| VInstance vi -> vi.ifields.(i)
	| VString(_,s) -> vint (String.length (Lazy.force s))
	| v -> unexpected_value v "instance"

let emit_field_closure exec name env =
	let v = exec env in
	dynamic_field v name

let emit_anon_local_field_read iv proto i name p env =
	match env.env_locals.(iv) with
	| VObject o ->
		if proto == o.oproto then o.ofields.(i)
		else object_field o name
	| VNull -> throw_string "field access on null" p
	| v -> field v name

let emit_anon_field_read exec proto i name p env =
	match exec env with
	| VObject o ->
		if proto == o.oproto then o.ofields.(i)
		else object_field o name
	| VNull -> throw_string "field access on null" p
	| v -> field v name

let emit_field_read exec name p env = match exec env with
	| VNull -> throw_string "field access on null" p
	| v -> field v name

let emit_array_local_read i exec2 p env =
	let va = env.env_locals.(i) in
	let vi = exec2 env in
	let i = decode_int_p vi p in
	if i < 0 then vnull
	else EvalArray.get (decode_varray va) i

let emit_array_read exec1 exec2 p env =
	let va = exec1 env in
	let vi = exec2 env in
	let i = decode_int_p vi p in
	if i < 0 then vnull
	else EvalArray.get (decode_varray va) i

let emit_vector_local_read i exec2 p env =
	let vv = env.env_locals.(i) in
	let vi = exec2 env in
	let i = decode_int_p vi p in
	if i < 0 then vnull
	else Array.unsafe_get (decode_vector vv) i

let emit_vector_read exec1 exec2 p env =
	let vv = exec1 env in
	let vi = exec2 env in
	let i = decode_int_p vi p in
	if i < 0 then vnull
	else Array.unsafe_get (decode_vector vv) i

let emit_enum_index exec env = match exec env with
	| VEnumValue ev -> vint ev.eindex
	| v -> unexpected_value v "enum value"

let emit_enum_parameter_read exec i env = match exec env with
	| VEnumValue ev -> (try ev.eargs.(i) with Not_found -> vnull)
	| v1 -> unexpected_value v1 "enum value"

let emit_string_cca exec1 exec2 p env =
	let s = decode_string (exec1 env) in
	let index = decode_int_p (exec2 env) p in
	if index >= String.length s then vnull
	else vint (int_of_char s.[index])

(* Write *)

let emit_local_write slot exec env =
	let v = exec env in
	env.env_locals.(slot) <- v;
	v

let emit_capture_write slot exec env =
	let v = exec env in
	env.env_captures.(slot) := v;
	v

let emit_proto_field_write proto i exec2 env =
	let v = exec2 env in
	proto.pfields.(i) <- v;
	v

let emit_instance_field_write exec1 i exec2 env = match exec1 env with
	| VInstance vi ->
		let v = exec2 env in
		vi.ifields.(i) <- v;
		v
	| v -> unexpected_value v "instance"

let emit_anon_field_write exec1 proto i name exec2 env =
	let v1 = exec1 env in
	let v2 = exec2 env in
	begin match v1 with
		| VObject o ->
			if proto == o.oproto then begin
				o.ofields.(i) <- v2;
				o.oremoved <- IntMap.remove name o.oremoved;
			end else set_object_field o name v2
		| _ ->
			set_field v1 name v2;
	end;
	v2

let emit_field_write exec1 name exec2 env =
	let v1 = exec1 env in
	let v2 = exec2 env in
	set_field v1 name v2;
	v2

let emit_array_local_write i exec2 exec3 p env =
	let va = env.env_locals.(i) in
	let vi = exec2 env in
	let v3 = exec3 env in
	let i = decode_int_p vi p in
	if i < 0 then throw_string (Printf.sprintf "Negative array index: %i" i) p;
	EvalArray.set (decode_varray va) i v3;
	v3

let emit_array_write exec1 exec2 exec3 p env =
	let va = exec1 env in
	let vi = exec2 env in
	let v3 = exec3 env in
	let i = decode_int_p vi p in
	if i < 0 then throw_string (Printf.sprintf "Negative array index: %i" i) p;
	EvalArray.set (decode_varray va) i v3;
	v3

let emit_vector_local_write i exec2 exec3 p env =
	let vv = env.env_locals.(i) in
	let vi = exec2 env in
	let v3 = exec3 env in
	let i = decode_int_p vi p in
	if i < 0 then throw_string (Printf.sprintf "Negative vector index: %i" i) p;
	Array.unsafe_set (decode_vector vv) i v3;
	v3

let emit_vector_write exec1 exec2 exec3 p env =
	let vv = exec1 env in
	let vi = exec2 env in
	let v3 = exec3 env in
	let i = decode_int_p vi p in
	if i < 0 then throw_string (Printf.sprintf "Negative vector index: %i" i) p;
	Array.unsafe_set (decode_vector vv) i v3;
	v3

(* Read + write *)

let emit_local_read_write slot exec fop prefix env =
	let v1 = env.env_locals.(slot) in
	let v2 = exec env in
	let v = fop v1 v2 in
	env.env_locals.(slot) <- v;
	if prefix then v else v1

let emit_local_incr_postfix slot env =
	let vi = env.env_locals.(slot) in
	env.env_locals.(slot) <- vint32 (Int32.succ (decode_i32 vi));
	vi

let emit_local_incr_prefix slot env =
	let vi = env.env_locals.(slot) in
	let v = vint32 (Int32.succ (decode_i32 vi)) in
	env.env_locals.(slot) <- v;
	v

let emit_local_decr_postfix slot env =
	let vi = env.env_locals.(slot) in
	env.env_locals.(slot) <- vint32 (Int32.pred (decode_i32 vi));
	vi

let emit_local_decr_prefix slot env =
	let vi = env.env_locals.(slot) in
	let v = vint32 (Int32.pred (decode_i32 vi)) in
	env.env_locals.(slot) <- v;
	v

let emit_capture_read_write slot exec fop prefix env =
	let v1 = !(env.env_captures.(slot)) in
	let v2 = exec env in
	let v = fop v1 v2 in
	env.env_captures.(slot) := v;
	if prefix then v else v1

let emit_capture_incr_postfix slot env =
	let vi = !(env.env_captures.(slot)) in
	env.env_captures.(slot) := vint32 (Int32.succ (decode_i32 vi));
	vi

let emit_capture_incr_prefix slot env =
	let vi = !(env.env_captures.(slot)) in
	let v = vint32 (Int32.succ (decode_i32 vi)) in
	env.env_captures.(slot) := v;
	v

let emit_capture_decr_postfix slot env =
	let vi = !(env.env_captures.(slot)) in
	env.env_captures.(slot) := vint32 (Int32.pred (decode_i32 vi));
	vi

let emit_capture_decr_prefix slot env =
	let vi = !(env.env_captures.(slot)) in
	let v = vint32 (Int32.pred (decode_i32 vi)) in
	env.env_captures.(slot) := v;
	v

let emit_proto_field_read_write proto i exec2 fop prefix env =
	let vf = proto.pfields.(i) in
	let v2 = exec2 env in
	let v = fop vf v2 in
	proto.pfields.(i) <- v;
	if prefix then v else vf

let instance_field_read_write vi i exec2 fop prefix env =
	let vf = vi.ifields.(i) in
	let v2 = exec2 env in
	let v = fop vf v2 in
	vi.ifields.(i) <- v;
	if prefix then v else vf

let emit_instance_field_read_write exec1 i exec2 fop prefix env = match exec1 env with
	| VInstance vi -> instance_field_read_write vi i exec2 fop prefix env
	| v -> unexpected_value v "instance"

let emit_field_read_write exec1 name exec2 fop prefix env =
	let v1 = exec1 env in
	match v1 with
	| VObject o ->
		let vf = object_field o name in
		let v2 = exec2 env in
		let v = fop vf v2 in
		set_object_field o name v;
		if prefix then v else vf
	| VInstance vi ->
		let i = get_instance_field_index vi.iproto name in
		instance_field_read_write vi i exec2 fop prefix env
	| VPrototype proto ->
		let i = get_proto_field_index proto name in
		emit_proto_field_read_write proto i exec2 fop prefix env
	| _ ->
		let vf = field v1 name in
		let v2 = exec2 env in
		let v = fop vf v2 in
		set_field v1 name v;
		if prefix then v else vf

let emit_array_local_read_write i exec2 exec3 fop prefix p env =
	let va1 = env.env_locals.(i) in
	let va2 = exec2 env in
	let va = decode_varray va1 in
	let i = decode_int_p va2 p in
	if i < 0 then throw_string (Printf.sprintf "Negative array index: %i" i) p;
	let v = EvalArray.get va i in
	let v2 = exec3 env in
	let v3 = fop v v2 in
	EvalArray.set va i v3;
	if prefix then v3 else v

let emit_array_read_write exec1 exec2 exec3 fop prefix p env =
	let va1 = exec1 env in
	let va2 = exec2 env in
	let va = decode_varray va1 in
	let i = decode_int_p va2 p in
	if i < 0 then throw_string (Printf.sprintf "Negative array index: %i" i) p;
	let v = EvalArray.get va i in
	let v2 = exec3 env in
	let v3 = fop v v2 in
	EvalArray.set va i v3;
	if prefix then v3 else v

let emit_vector_local_read_write i exec2 exec3 fop prefix p env =
	let va1 = env.env_locals.(i) in
	let va2 = exec2 env in
	let va = decode_vector va1 in
	let i = decode_int_p va2 p in
	if i < 0 then throw_string (Printf.sprintf "Negative vector index: %i" i) p;
	let v = Array.unsafe_get va i in
	let v2 = exec3 env in
	let v3 = fop v v2 in
	Array.unsafe_set va i v3;
	if prefix then v3 else v

let emit_vector_read_write exec1 exec2 exec3 fop prefix p env =
	let va1 = exec1 env in
	let va2 = exec2 env in
	let va = decode_vector va1 in
	let i = decode_int_p va2 p in
	if i < 0 then throw_string (Printf.sprintf "Negative vector index: %i" i) p;
	let v = Array.unsafe_get va i in
	let v2 = exec3 env in
	let v3 = fop v v2 in
	Array.unsafe_set va i v3;
	if prefix then v3 else v

(* Ops *)

let emit_eq_null exec env = match exec env with
	| VNull -> VTrue
	| _ -> VFalse

let emit_not_eq_null exec env = match exec env with
	| VNull -> VFalse
	| _ -> VTrue

let op_add v1 v2 = match v1,v2 with
	| VInt32 i1,VInt32 i2 -> vint32 (Int32.add i1 i2)
	| VFloat f1,VFloat f2 -> vfloat (f1 +. f2)
	| VInt32 i,VFloat f | VFloat f,VInt32 i -> vfloat ((Int32.to_float i) +. f)
	| VString(s1,_),VString(s2,_) -> encode_rope (Rope.concat2 s1 s2)
	| VString(s1,_),v2 -> encode_rope (Rope.concat2 s1 (s_value 0 v2))
	| v1,VString(s2,_) -> encode_rope (Rope.concat2 (s_value 0 v1) s2)
	| v1,v2 -> encode_rope (Rope.concat2 (s_value 0 v1) (s_value 0 v2))

let op_mult p v1 v2 = match v1,v2 with
	| VInt32 i1,VInt32 i2 -> vint32 (Int32.mul i1 i2)
	| VFloat f1,VFloat f2 -> vfloat (f1 *. f2)
	| VInt32 i,VFloat f | VFloat f,VInt32 i -> vfloat ((Int32.to_float i) *. f)
	| _ -> invalid_binop OpMult v1 v2 p

let op_div p v1 v2 = match v1,v2 with
	| VInt32 i1,VInt32 i2 -> vfloat ((Int32.to_float i1) /. (Int32.to_float i2))
	| VFloat f1,VFloat f2 -> vfloat (f1 /. f2)
	| VInt32 i1,VFloat f2 -> vfloat ((Int32.to_float i1) /. f2)
	| VFloat f1,VInt32 i2 -> vfloat (f1 /. (Int32.to_float i2))
	| _ -> invalid_binop OpDiv v1 v2 p

let op_sub p v1 v2 = match v1,v2 with
	| VInt32 i1,VInt32 i2 -> vint32 (Int32.sub i1 i2)
	| VFloat f1,VFloat f2 -> vfloat (f1 -. f2)
	| VInt32 i1,VFloat f2 -> vfloat ((Int32.to_float i1) -. f2)
	| VFloat f1,VInt32 i2 -> vfloat (f1 -. (Int32.to_float i2))
	| _ -> invalid_binop OpSub v1 v2 p

let op_eq v1 v2 = vbool (equals v1 v2)

let op_not_eq v1 v2 = vbool (not (equals v1 v2))

let op_gt v1 v2 = vbool (compare v1 v2 = CSup)

let op_gte v1 v2 = vbool (match compare v1 v2 with CSup | CEq -> true | _ -> false)

let op_lt v1 v2 = vbool (compare v1 v2 = CInf)

let op_lte v1 v2 = vbool (match compare v1 v2 with CInf | CEq -> true | _ -> false)

let op_and p v1 v2 = match v1,v2 with
	| VInt32 i1,VInt32 i2 -> vint32 (Int32.logand i1 i2)
	| _ -> invalid_binop OpAnd v1 v2 p

let op_or p v1 v2 = match v1,v2 with
	| VInt32 i1,VInt32 i2 -> vint32 (Int32.logor i1 i2)
	| _ -> invalid_binop OpOr v1 v2 p

let op_xor p v1 v2 = match v1,v2 with
	| VInt32 i1,VInt32 i2 -> vint32 (Int32.logxor i1 i2)
	| _ -> invalid_binop OpXor v1 v2 p

let op_shl p v1 v2 = match v1,v2 with
	| VInt32 i1,VInt32 i2 -> vint32 (Int32.shift_left i1 (Int32.to_int i2))
	| _ -> invalid_binop OpShl v1 v2 p

let op_shr p v1 v2 = match v1,v2 with
	| VInt32 i1,VInt32 i2 -> vint32 (Int32.shift_right i1 (Int32.to_int i2))
	| _ -> invalid_binop OpShr v1 v2 p

let op_ushr p v1 v2 = match v1,v2 with
	| VInt32 i1,VInt32 i2 -> vint32 (Int32.shift_right_logical i1 (Int32.to_int i2))
	| _ -> invalid_binop OpUShr v1 v2 p

let op_mod p v1 v2 = match v1,v2 with
	| VInt32 i1,VInt32 i2 -> vint32 (Int32.rem i1 i2)
	| VFloat f1,VFloat f2 -> vfloat (mod_float f1 f2)
	| VInt32 i1,VFloat f2 -> vfloat (mod_float (Int32.to_float i1) f2)
	| VFloat f1,VInt32 i2 -> vfloat (mod_float f1 (Int32.to_float i2))
	| _ -> invalid_binop OpMod v1 v2 p

let emit_op_add exec1 exec2 env =
	let v1 = exec1 env in
	let v2 = exec2 env in
	op_add v1 v2

let emit_op_mult p exec1 exec2 env =
	let v1 = exec1 env in
	let v2 = exec2 env in
	op_mult p v1 v2

let emit_op_div p exec1 exec2 env =
	let v1 = exec1 env in
	let v2 = exec2 env in
	op_div p v1 v2

let emit_op_sub p exec1 exec2 env =
	let v1 = exec1 env in
	let v2 = exec2 env in
	op_sub p v1 v2

let emit_op_eq exec1 exec2 env =
	let v1 = exec1 env in
	let v2 = exec2 env in
	vbool (equals v1 v2)

let emit_op_not_eq exec1 exec2 env =
	let v1 = exec1 env in
	let v2 = exec2 env in
	vbool  (not (equals v1 v2))

let emit_op_gt exec1 exec2 env =
	let v1 = exec1 env in
	let v2 = exec2 env in
	vbool (compare v1 v2 = CSup)

let emit_op_gte exec1 exec2 env =
	let v1 = exec1 env in
	let v2 = exec2 env in
	vbool (match compare v1 v2 with CSup | CEq -> true | _ -> false)

let emit_op_lt exec1 exec2 env =
	let v1 = exec1 env in
	let v2 = exec2 env in
	vbool (compare v1 v2 = CInf)

let emit_op_lte exec1 exec2 env =
	let v1 = exec1 env in
	let v2 = exec2 env in
	vbool (match compare v1 v2 with CInf | CEq -> true | _ -> false)

let emit_op_and p exec1 exec2 env =
	let v1 = exec1 env in
	let v2 = exec2 env in
	op_and p v1 v2

let emit_op_or p exec1 exec2 env =
	let v1 = exec1 env in
	let v2 = exec2 env in
	op_or p v1 v2

let emit_op_xor p exec1 exec2 env =
	let v1 = exec1 env in
	let v2 = exec2 env in
	op_xor p v1 v2

let emit_op_shl p exec1 exec2 env =
	let v1 = exec1 env in
	let v2 = exec2 env in
	op_shl p v1 v2

let emit_op_shr p exec1 exec2 env =
	let v1 = exec1 env in
	let v2 = exec2 env in
	op_shr p v1 v2

let emit_op_ushr p exec1 exec2 env =
	let v1 = exec1 env in
	let v2 = exec2 env in
	op_ushr p v1 v2

let emit_op_mod p exec1 exec2 env =
	let v1 = exec1 env in
	let v2 = exec2 env in
	op_mod p v1 v2

let emit_not exec env = match exec env with
	| VNull | VFalse -> VTrue
	| _ -> VFalse

let emit_bool_and exec1 exec2 env =
	if is_true (exec1 env) then exec2 env
	else VFalse

let emit_bool_or exec1 exec2 env =
	if is_true (exec1 env) then VTrue
	else exec2 env

let emit_neg exec p env = match exec env with
	| VFloat f -> vfloat (-.f)
	| VInt32 i -> vint32 (Int32.neg i)
	| _ -> throw_string "Invalid operation" p

(* Function *)

let handle_capture_arguments exec varaccs env =
	List.iter (fun (slot,i) ->
		env.env_captures.(i) <- ref env.env_locals.(slot)
	) varaccs;
	exec env

let run_function ctx exec env =
	let v = try
		exec env
	with
		| Return v -> v
	in
	env.env_in_use <- false;
	pop_environment ctx env;
	v
[@@inline]

let run_function_noret ctx exec env =
	let v = exec env in
	env.env_in_use <- false;
	pop_environment ctx env;
	v
[@@inline]

let get_normal_env ctx info num_locals num_captures _ =
	push_environment ctx info num_locals num_captures

let get_closure_env ctx info num_locals num_captures refs =
	let env = push_environment ctx info num_locals num_captures in
	Array.iteri (fun i vr -> env.env_captures.(i) <- vr) refs;
	env

let get_normal_env_opt ctx default_env info num_locals num_captures _ =
	if default_env.env_in_use then begin
		push_environment ctx info num_locals num_captures
	end else begin
		default_env.env_in_use <- true;
		default_env
	end

let get_closure_env_opt ctx default_env info num_locals num_captures refs =
	let env = if default_env.env_in_use then begin
		push_environment ctx info num_locals num_captures
	end else begin
		default_env.env_in_use <- true;
		default_env
	end in
	Array.iteri (fun i vr -> env.env_captures.(i) <- vr) refs;
	env

let create_function ctx num_args get_env hasret refs exec =
	match num_args with
	| 0 ->
		if hasret then Fun0 (fun () ->
			let env = get_env refs in
			run_function ctx exec env
		)
		else Fun0 (fun () ->
			let env = get_env refs in
			run_function_noret ctx exec env
		)
	| 1 ->
		if hasret then Fun1 (fun v1 ->
			let env = get_env refs in
			env.env_locals.(0) <- v1;
			run_function ctx exec env
		)
		else Fun1 (fun v1 ->
			let env = get_env refs in
			env.env_locals.(0) <- v1;
			run_function_noret ctx exec env
		)
	| 2 ->
		let run v1 v2 =
			let env = get_env refs in
			env.env_locals.(0) <- v1;
			env.env_locals.(1) <- v2;
			env
		in
		if hasret then Fun2 (fun v1 v2 ->
			let env = run v1 v2 in
			run_function ctx exec env
		)
		else Fun2 (fun v1 v2 ->
			let env = run v1 v2 in
			run_function_noret ctx exec env
		)
	| 3 ->
		let run v1 v2 v3 =
			let env = get_env refs in
			env.env_locals.(0) <- v1;
			env.env_locals.(1) <- v2;
			env.env_locals.(2) <- v3;
			env
		in
		if hasret then Fun3 (fun v1 v2 v3 ->
			let env = run v1 v2 v3 in
			run_function ctx exec env
		)
		else Fun3 (fun v1 v2 v3 ->
			let env = run v1 v2 v3 in
			run_function_noret ctx exec env
		)
	| 4 ->
		let run v1 v2 v3 v4 =
			let env = get_env refs in
			env.env_locals.(0) <- v1;
			env.env_locals.(1) <- v2;
			env.env_locals.(2) <- v3;
			env.env_locals.(3) <- v4;
			env
		in
		if hasret then Fun4 (fun v1 v2 v3 v4 ->
			let env = run v1 v2 v3 v4 in
			run_function ctx exec env
		)
		else Fun4 (fun v1 v2 v3 v4 ->
			let env = run v1 v2 v3 v4 in
			run_function_noret ctx exec env
		)
	| 5 ->
		let run v1 v2 v3 v4 v5 =
			let env = get_env refs in
			env.env_locals.(0) <- v1;
			env.env_locals.(1) <- v2;
			env.env_locals.(2) <- v3;
			env.env_locals.(3) <- v4;
			env.env_locals.(4) <- v5;
			env
		in
		if hasret then Fun5 (fun v1 v2 v3 v4 v5 ->
			let env = run v1 v2 v3 v4 v5 in
			run_function ctx exec env
		)
		else Fun5 (fun v1 v2 v3 v4 v5 ->
			let env = run v1 v2 v3 v4 v5 in
			run_function_noret ctx exec env
		)
	| _ ->
		if hasret then FunN (fun vl ->
			let env = get_env refs in
			List.iteri (fun i v ->
				env.env_locals.(i) <- v
			) vl;
			run_function ctx exec env
		)
		else FunN (fun vl ->
			let env = get_env refs in
			List.iteri (fun i v ->
				env.env_locals.(i) <- v
			) vl;
			run_function_noret ctx exec env
		)

let emit_closure ctx num_captures num_args get_env hasret exec env =
	let refs = Array.sub env.env_captures 0 num_captures in
	let f = create_function ctx num_args get_env hasret refs exec in
	vstatic_function f