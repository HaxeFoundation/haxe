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
open EvalHash
open EvalValue
open EvalEncode
open EvalDecode
open EvalContext
open EvalPrinting
open EvalExceptions
open EvalField
open EvalMisc

(* Helper *)

let unexpected_value_p v s p =
	let str = match v with
		| VNull -> "Null Access"
		| _ -> Printf.sprintf "Unexpected value %s, expected %s" (value_string v) s
	in
	throw_string str p

let as_array p = function
	| VArray va -> va
	| v -> unexpected_value_p v "Array" p

let as_bytes p = function
	| VInstance {ikind = IBytes s} -> s
	| v -> unexpected_value_p v "Bytes" p

let as_enum_value p = function
	| VEnumValue ve -> ve
	| v -> unexpected_value_p v "enum value" p

let as_int p = function
	| VInt32 i -> Int32.to_int i
	| VFloat f -> int_of_float f
	| v -> unexpected_value_p v "int" p

let as_vector p = function
	| VVector vv -> vv
	| v -> unexpected_value_p v "Vector" p

let cannot_call v p =
	throw (EvalString.create_unknown ("Cannot call " ^ (value_string v))) p

let decode_int_p v p = match v with
	| VInt32 i -> Int32.to_int i
	| VFloat f -> int_of_float f
	| _ -> unexpected_value_p v "int" p

let check_stack_depth env =
	if env.env_stack_depth > (get_ctx()).max_stack_depth then
		exc_string "Stack overflow"

(* Emitter *)

let apply env exec =
	exec env

(* Objects and values *)

let emit_null _ = vnull

let emit_local_declaration i exec env =
	env.env_locals.(i) <- exec env;
	vnull

let emit_capture_declaration i exec env =
	env.env_captures.(i) <- exec env;
	vnull

let emit_const v _ = v

let emit_new_array env =
	encode_array_instance (EvalArray.create [||])

let emit_new_vector_int i p env =
	if i < 0 then exc_string_p "Vector size must be >= 0" p;
	let a = try
		Array.make i vnull
	with Invalid_argument _ ->
		exc_string_p (Printf.sprintf "Not enough memory to allocate Vector of size %i" i) p;
	in
	encode_vector_instance a

let emit_new_vector exec p env =
	let i = decode_int_p (exec env) p in
	emit_new_vector_int i p env

let emit_special_instance f execs env =
	let vl = List.map (apply env) execs in
	f vl

let emit_object_declaration proto fa env =
	let a = Array.make (Array.length fa) vnull in
	Array.iter (fun (i,exec) -> a.(i) <- exec env) fa;
	vobject {
		ofields = a;
		oproto = OProto proto;
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

let emit_enum_construction key i execs p env =
	encode_enum_value key i (Array.map (apply env) execs) p

(* Branching *)

let emit_if exec_cond exec_then exec_else env =
	match exec_cond env with
	| VTrue -> exec_then env
	| _ -> exec_else env

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

let emit_int_switch_map exec cases exec_def p env = match exec env with
	| VInt32 i32 ->
		let i = Int32.to_int i32 in
		begin try
			(IntMap.find i cases) env
		with Not_found ->
			exec_def env
		end
	| VNull ->
		exec_def env
	| v ->
		unexpected_value_p v "int" p

let emit_string_switch_map exec cases exec_def p env = match exec env with
	| VString s ->
		begin try
			(PMap.find s.sstring cases) env
		with Not_found ->
			exec_def env
		end
	| VNull ->
		exec_def env
	| v ->
		unexpected_value_p v "string" p

let emit_int_switch_array shift exec cases exec_def p env = match exec env with
	| VInt32 i32 ->
		let i = Int32.to_int i32 + shift in
		if i >= Array.length cases || i < 0 then exec_def env
		else (Array.unsafe_get cases i) env
	| VNull ->
		exec_def env
	| v ->
		unexpected_value_p v "int" p

let rec run_while_continue exec_cond exec_body env =
	try
		while is_true (exec_cond env) do exec_body env done;
	with Continue ->
		run_while_continue exec_cond exec_body env

let rec run_while exec_cond exec_body env =
	while is_true (exec_cond env) do exec_body env done

let emit_while_break exec_cond exec_body env =
	(try run_while_continue exec_cond exec_body env with Break -> ());
	vnull

let emit_while_break_continue exec_cond exec_body env =
	(try run_while_continue exec_cond exec_body env with Break -> ());
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
	let eval = env.env_eval in
	if ctx.debug.support_debugger then begin
		List.iter (fun (_,path,_) -> Hashtbl.add eval.caught_types path true) catches
	end;
	let restore () =
		List.iter (fun (_,path,_) -> Hashtbl.remove eval.caught_types path) catches
	in
	let v = try
		let v = handle_stack_overflow eval (fun() -> exec env) in
		restore();
		v
	with RunTimeException(v,_,_) as exc ->
		eval.caught_exception <- vnull;
		restore();
		build_exception_stack ctx env;
		let rec loop () = match eval.env with
			| Some env' when env' != env ->
				pop_environment ctx env';
				loop();
			| _ ->
				()
		in
		loop();
		let exec,_,varacc =
			try
				List.find (fun (_,path,i) -> path = key_Dynamic || is v path) catches
			with Not_found ->
				raise_notrace exc
		in
		varacc (fun _ -> v) env;
		exec env
	in
	v

(* Control flow *)

let emit_value exec env =
	exec env

let emit_seq2 exec1 exec2 env =
	ignore(exec1 env);
	exec2 env

let emit_seq4 exec1 exec2 exec3 exec4 env =
	ignore (exec1 env);
	ignore (exec2 env);
	ignore (exec3 env);
	exec4 env

let emit_seq8 exec1 exec2 exec3 exec4 exec5 exec6 exec7 exec8 env =
	ignore (exec1 env);
	ignore (exec2 env);
	ignore (exec3 env);
	ignore (exec4 env);
	ignore (exec5 env);
	ignore (exec6 env);
	ignore (exec7 env);
	exec8 env

let emit_return_null _ = raise_notrace (Return vnull)

let emit_return_value exec env = raise_notrace (Return (exec env))

let emit_break env = raise_notrace Break

let emit_continue env = raise_notrace Continue

let emit_throw exec p env = throw (exec env) p

let emit_safe_cast exec t p env =
	let v1 = exec env in
	match vresolve v1 with
	| VNull -> v1
	| _ -> if not (is v1 t) then throw_string "Class cast error" p else v1

(* Calls *)

(* super.call() - immediate *)

let emit_super_field_call slot proto i execs p env =
	check_stack_depth env;
	let vthis = env.env_locals.(slot) in
	let vf = proto.pfields.(i) in
	let vl = List.map (apply env) execs in
	call_value_on vthis vf vl

(* Type.call() - immediate *)

let emit_proto_field_call v execs p env =
	check_stack_depth env;
	let f = Lazy.force v in
	let vl = List.map (apply env) execs in
	env.env_leave_pmin <- p.pmin;
	env.env_leave_pmax <- p.pmax;
	f vl

(* instance.call() where call is overridden - dynamic dispatch *)

let get_prototype v p = match vresolve v with
	| VInstance {iproto = proto} | VPrototype proto -> proto
	| VString _ -> (get_ctx()).string_prototype
	| VArray _ -> (get_ctx()).array_prototype
	| VVector _ -> (get_ctx()).vector_prototype
	| _ -> unexpected_value_p v "instance" p

let emit_method_call exec name execs p env =
	check_stack_depth env;
	let vthis = exec env in
	let proto = get_prototype vthis p in
	let vf = try proto_field_raise proto name with Not_found -> throw_string (Printf.sprintf "Field %s not found on prototype %s" (rev_hash name) (rev_hash proto.ppath)) p in
	let vl = List.map (apply env) execs in
	env.env_leave_pmin <- p.pmin;
	env.env_leave_pmax <- p.pmax;
	call_value_on vthis vf vl

(* instance.call() where call is not a method - lookup + this-binding *)

let emit_field_call exec name execs p env =
	check_stack_depth env;
	let vthis = exec env in
	let vf = field vthis name in
	env.env_leave_pmin <- p.pmin;
	env.env_leave_pmax <- p.pmax;
	call_value_on vthis vf (List.map (apply env) execs)

(* new() - immediate + this-binding *)

let emit_constructor_call proto v execs p env =
	check_stack_depth env;
	let f = Lazy.force v in
	let vthis = create_instance_direct proto INormal in
	let vl = List.map (apply env) execs in
	env.env_leave_pmin <- p.pmin;
	env.env_leave_pmax <- p.pmax;
	ignore(f (vthis :: vl));
	vthis

(* super() - immediate + this-binding *)

let emit_special_super_call fnew execs env =
	check_stack_depth env;
	let vl = List.map (apply env) execs in
	let vi' = fnew vl in
	let vthis = env.env_locals.(0) in
	(* This isn't very elegant, but it's probably a rare case to extend these types. *)
	begin match vthis,vi' with
		| VInstance vi,VInstance vi' -> vi.ikind <- vi'.ikind
		| _ -> die "" __LOC__
	end;
	vnull

let emit_super_call v execs p env =
	check_stack_depth env;
	let f = Lazy.force v in
	let vthis = env.env_locals.(0) in
	let vl = List.map (apply env) execs in
	env.env_leave_pmin <- p.pmin;
	env.env_leave_pmax <- p.pmax;
	ignore(f (vthis :: vl));
	vthis

(* unknown call - full lookup *)

let emit_call exec execs p env =
	check_stack_depth env;
	let v1 = exec env in
	env.env_leave_pmin <- p.pmin;
	env.env_leave_pmax <- p.pmax;
	call_value v1 (List.map (apply env) execs)

(* Read *)

let emit_local_read i env = env.env_locals.(i)

let emit_capture_read i env = env.env_captures.(i)

let emit_array_length_read exec p env = vint (as_array p (exec env)).alength

let emit_vector_length_read exec p env = vint (Array.length (as_vector p (exec env)))

let emit_bytes_length_read exec p env = vint (Bytes.length (as_bytes p (exec env)))

let emit_proto_field_read proto i env =
	proto.pfields.(i)

let emit_instance_field_read exec p i env = match exec env with
	| VInstance vi -> vi.ifields.(i)
	| VString s -> vint (s.slength)
	| VNull -> throw_string "field access on null" p
	| v -> unexpected_value_p v "instance" p

let emit_this_field_read iv i env = match env.env_locals.(iv) with
	| VInstance vi -> vi.ifields.(i)
	| v -> unexpected_value v "instance"

let emit_field_closure exec name env =
	let v = exec env in
	dynamic_field v name

let emit_anon_field_read exec proto i name p env =
	match vresolve (exec env) with
	| VObject o ->
		begin match o.oproto with
		| OProto proto' when proto' == proto ->
			o.ofields.(i)
		| _ ->
			object_field o name
		end
	| VNull -> throw_string "field access on null" p
	| v -> field v name

let emit_field_read exec name p env = match exec env with
	| VNull -> throw_string "field access on null" p
	| v -> field v name

let emit_array_read exec1 p1 exec2 p2 env =
	let a = as_array p1 (exec1 env) in
	let i = as_int p2 (exec2 env) in
	if i < 0 then vnull
	else EvalArray.get a i

let emit_vector_read exec1 p1 exec2 p2 env =
	let v = as_vector p1 (exec1 env) in
	let i = as_int p2 (exec2 env) in
	if i < 0 then vnull
	else Array.unsafe_get v i

let emit_enum_index exec p env = vint (as_enum_value p (exec env)).eindex

let emit_enum_parameter_read exec i env = match exec env with
	| VEnumValue ev -> (try ev.eargs.(i) with Not_found -> vnull)
	| v1 -> unexpected_value v1 "enum value"

let emit_string_cca exec1 exec2 p env =
	let s = decode_vstring (exec1 env) in
	let index = decode_int_p (exec2 env) p in
	if index < 0 || index >= s.slength then vnull
	else vint (EvalString.char_at s index)

let emit_string_cca_unsafe exec1 exec2 p env =
	let s = decode_vstring (exec1 env) in
	let index = decode_int_p (exec2 env) p in
	vint (EvalString.char_at s index)

(* Write *)

let emit_bytes_length_write exec1 exec2 env =
	let v1 = exec1 env in
	let v2 = exec2 env in
	set_bytes_length_field v1 v2;
	v2

let emit_local_write slot exec env =
	let v = exec env in
	env.env_locals.(slot) <- v;
	v

let emit_capture_write slot exec env =
	let v = exec env in
	env.env_captures.(slot) <- v;
	v

let emit_proto_field_write proto i exec2 env =
	let v = exec2 env in
	proto.pfields.(i) <- v;
	v

let emit_instance_field_write exec1 p i exec2 env = match exec1 env with
	| VInstance vi ->
		let v = exec2 env in
		vi.ifields.(i) <- v;
		v
	| v -> unexpected_value_p v "instance" p

let emit_anon_field_write exec1 p proto i name exec2 env =
	let v1 = exec1 env in
	let v2 = exec2 env in
	begin match vresolve v1 with
		| VObject o ->
			begin match o.oproto with
			| OProto proto' when proto' == proto ->
				o.ofields.(i) <- v2;
			| _ ->
				set_object_field o name v2
			end
		| VNull ->
			throw_string "field access on null" p
		| _ ->
			set_field v1 name v2;
	end;
	v2

let emit_field_write exec1 p1 name exec2 env =
	let v1 = exec1 env in
	let v2 = exec2 env in
	(try set_field v1 name v2 with RunTimeException(v,stack,_) -> raise_notrace (RunTimeException(v,stack,p1)));
	v2

let emit_array_write exec1 p1 exec2 p2 exec3 p env =
	let a = as_array p1 (exec1 env) in
	let i = as_int p2 (exec2 env) in
	let v3 = exec3 env in
	if i < 0 then throw_string (Printf.sprintf "Negative array index: %i" i) p;
	EvalArray.set a i v3;
	v3

let emit_vector_write exec1 p1 exec2 p2 exec3 p env =
	let vv = as_vector p1 (exec1 env) in
	let i = as_int p2 (exec2 env) in
	let v3 = exec3 env in
	if i < 0 then throw_string (Printf.sprintf "Negative vector index: %i" i) p;
	Array.unsafe_set vv i v3;
	v3

(* Read + write *)

let do_incr v p = match v with
	| VInt32 i32 -> vint32 (Int32.add i32 Int32.one)
	| VFloat f -> vfloat (f +. 1.)
	| v -> unexpected_value_p v "number" p

let emit_local_incr_prefix slot p env =
	let v0 = env.env_locals.(slot) in
	let v = do_incr v0 p in
	env.env_locals.(slot) <- v;
	v

let emit_local_incr_postfix slot p env =
	let v0 = env.env_locals.(slot) in
	let v = do_incr v0 p in
	env.env_locals.(slot) <- v;
	v0

let emit_local_read_write slot exec fop prefix env =
	let v1 = env.env_locals.(slot) in
	let v2 = exec env in
	let v = fop v1 v2 in
	env.env_locals.(slot) <- v;
	if prefix then v else v1

let emit_capture_read_write slot exec fop prefix env =
	let v1 = (env.env_captures.(slot)) in
	let v2 = exec env in
	let v = fop v1 v2 in
	env.env_captures.(slot) <- v;
	if prefix then v else v1

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

let emit_instance_field_read_write exec1 p1 i exec2 fop prefix env = match exec1 env with
	| VInstance vi -> instance_field_read_write vi i exec2 fop prefix env
	| v -> unexpected_value_p v "instance" p1

let emit_field_read_write exec1 p1 name exec2 fop prefix env =
	let v1 = exec1 env in
	match vresolve v1 with
	| VObject o ->
		let vf = object_field o name in
		let v2 = exec2 env in
		let v = fop vf v2 in
		set_object_field o name v;
		if prefix then v else vf
	| VInstance vi ->
		let i = get_instance_field_index vi.iproto name null_pos in
		instance_field_read_write vi i exec2 fop prefix env
	| VPrototype proto ->
		let i = get_proto_field_index proto name in
		emit_proto_field_read_write proto i exec2 fop prefix env
	| _ ->
		let vf = field v1 name in
		let v2 = exec2 env in
		let v = fop vf v2 in
		(try set_field v1 name v with RunTimeException(v,stack,_) -> raise_notrace (RunTimeException(v,stack,p1)));
		if prefix then v else vf

let emit_array_read_write exec1 p1 exec2 p2 exec3 fop prefix env =
	let va = as_array p1 (exec1 env) in
	let i = as_int p2 (exec2 env) in
	if i < 0 then throw_string (Printf.sprintf "Negative array index: %i" i) p2;
	let v = EvalArray.get va i in
	let v2 = exec3 env in
	let v3 = fop v v2 in
	EvalArray.set va i v3;
	if prefix then v3 else v

let emit_vector_read_write exec1 p1 exec2 p2 exec3 fop prefix env =
	let va = as_vector p1 (exec1 env) in
	let i = as_int p2 (exec2 env) in
	if i < 0 then throw_string (Printf.sprintf "Negative vector index: %i" i) p2;
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

let emit_op_add p exec1 exec2 env =
	let v1 = exec1 env in
	let v2 = exec2 env in
	op_add p v1 v2

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

let execute_set_local i env v =
	env.env_locals.(i) <- v

let execute_set_capture i env v =
	env.env_captures.(i) <- v

let process_arguments fl vl env =
	let rec loop fl vl = match fl,vl with
		| f :: fl,v :: vl ->
			f env v;
			loop fl vl
		| f :: fl,[] ->
			f env vnull;
			loop fl []
		| [],[] ->
			()
		| _ ->
			exc_string "Something went wrong"
	in
	loop fl vl
[@@inline]

let create_function_noret ctx eci exec fl vl =
	let env = push_environment ctx eci in
	process_arguments fl vl env;
	let v = exec env in
	pop_environment ctx env;
	v

let create_function ctx eci exec fl vl =
	let env = push_environment ctx eci in
	process_arguments fl vl env;
	let v = try exec env with Return v -> v in
	pop_environment ctx env;
	v

let create_closure_noret ctx eci refs exec fl vl =
	let env = push_environment ctx eci in
	Array.iter (fun (i,vr) -> env.env_captures.(i) <- vr) refs;
	process_arguments fl vl env;
	let v = exec env in
	pop_environment ctx env;
	v

let create_closure refs ctx eci exec fl vl =
	let env = push_environment ctx eci in
	Array.iter (fun (i,vr) -> env.env_captures.(i) <- vr) refs;
	process_arguments fl vl env;
	let v = try exec env with Return v -> v in
	pop_environment ctx env;
	v

let emit_closure ctx mapping eci hasret exec fl env =
	let refs = Array.map (fun (i,slot) -> i,emit_capture_read slot env) mapping in
	let create = match hasret,eci.num_captures with
		| true,0 -> create_function
		| false,0 -> create_function_noret
		| _ -> create_closure refs
	in
	let f = create ctx eci exec fl in
	vstatic_function f