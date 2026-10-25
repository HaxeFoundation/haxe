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
	| VInt i -> i
	| VFloat f -> int_of_float f
	| v -> unexpected_value_p v "int" p

let as_vector p = function
	| VVector vv -> vv
	| v -> unexpected_value_p v "Vector" p

let cannot_call v p =
	throw (EvalString.create_unknown ("Cannot call " ^ (value_string v))) p

let decode_int_p v p = match v with
	| VInt i -> i
	| VFloat f -> int_of_float f
	| _ -> unexpected_value_p v "int" p

let check_stack_depth max_depth env =
	if env.env_stack_depth > max_depth then
		exc_string "Stack overflow"

(* Emitter *)

let apply env exec =
	exec env

(* The arguments of a call, evaluated from left to right. *)
let emit_args execs = match execs with
	| [] ->
		(fun _ -> [])
	| [exec1] ->
		(fun env -> [exec1 env])
	| [exec1;exec2] ->
		(fun env ->
			let v1 = exec1 env in
			let v2 = exec2 env in
			[v1;v2]
		)
	| [exec1;exec2;exec3] ->
		(fun env ->
			let v1 = exec1 env in
			let v2 = exec2 env in
			let v3 = exec3 env in
			[v1;v2;v3]
		)
	| [exec1;exec2;exec3;exec4] ->
		(fun env ->
			let v1 = exec1 env in
			let v2 = exec2 env in
			let v3 = exec3 env in
			let v4 = exec4 env in
			[v1;v2;v3;v4]
		)
	| _ ->
		let rec loop env execs = match execs with
			| [] -> []
			| exec :: execs ->
				let v = exec env in
				v :: loop env execs
		in
		(fun env -> loop env execs)

(* The values of [execs], evaluated in order. *)
let eval_array execs env =
	let n = Array.length execs in
	if n = 0 then [||]
	else begin
		let a = make_null_array n in
		for i = 0 to n - 1 do
			Array.unsafe_set a i ((Array.unsafe_get execs i) env)
		done;
		a
	end

(* Objects and values *)

let emit_null _ = vnull

let emit_local_declaration i exec = fun env ->
	env.env_locals.(i) <- exec env;
	vnull

let emit_capture_declaration i exec = fun env ->
	env.env_captures.(i) <- exec env;
	vnull

let emit_const v = fun _ -> v

let emit_null_check exec p = fun env -> match exec env with
	| VNull -> throw_string "Null Access" p
	| v -> v

let emit_local_null_check slot p = fun env -> match env.env_locals.(slot) with
	| VNull -> throw_string "Null Access" p
	| v -> v

let emit_new_array env =
	encode_array_instance (EvalArray.create [||])

let new_vector i p =
	if i < 0 then exc_string_p "Vector size must be >= 0" p;
	let a = try
		Array.make i vnull
	with Invalid_argument _ ->
		exc_string_p (Printf.sprintf "Not enough memory to allocate Vector of size %i" i) p;
	in
	encode_vector_instance a

let emit_new_vector_int i p = fun env ->
	new_vector i p

let emit_new_vector exec p = fun env ->
	let i = decode_int_p (exec env) p in
	new_vector i p

let emit_special_instance f execs =
	let args = emit_args execs in
	(fun env -> f (args env))

let emit_object_declaration proto fa =
	let n = Array.length fa in
	let slots = Array.map fst fa in
	let execs = Array.map snd fa in
	(fun env ->
		let a = Array.make n vnull in
		for k = 0 to n - 1 do
			a.(Array.unsafe_get slots k) <- (Array.unsafe_get execs k) env
		done;
		vobject {
			ofields = a;
			oproto = OProto proto;
		}
	)

let emit_array_declaration execs = fun env ->
	encode_array_instance (EvalArray.create (eval_array execs env))

let emit_type_expr proto = fun env -> proto

let emit_mk_pos p = fun env ->
	encode_pos p

let emit_enum_construction key i execs p = fun env ->
	encode_enum_value key i (eval_array execs env) p

(* Branching *)

let rec switch_case_matches v1 patterns env = match patterns with
	| [] -> false
	| exec :: patterns -> equals v1 (exec env) || switch_case_matches v1 patterns env

let rec switch_from v1 execs patterns exec_def i env =
	if i >= Array.length patterns then exec_def env
	else if switch_case_matches v1 (Array.unsafe_get patterns i) env then
		execs.(i) env
	else
		switch_from v1 execs patterns exec_def (i + 1) env

let emit_switch exec execs patterns exec_def = fun env ->
	let v1 = exec env in
	switch_from v1 execs patterns exec_def 0 env

let emit_int_switch_map exec cases exec_def p = fun env -> match exec env with
	| VInt i ->
		begin try
			(IntMap.find i cases) env
		with Not_found ->
			exec_def env
		end
	| VNull ->
		exec_def env
	| v ->
		unexpected_value_p v "int" p

let emit_string_switch_map exec cases exec_def p = fun env -> match exec env with
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

let emit_int_switch_array shift exec cases exec_def p = fun env -> match exec env with
	| VInt i ->
		let i = i + shift in
		if i >= Array.length cases || i < 0 then exec_def env
		else (Array.unsafe_get cases i) env
	| VNull ->
		exec_def env
	| v ->
		unexpected_value_p v "int" p

let emit_try exec catches = fun env ->
	let ctx = get_ctx() in
	let eval = env.env_eval in
	(* the caught types are only registered for the debugger *)
	let debugger = ctx.debug.support_debugger in
	if debugger then begin
		List.iter (fun (_,path,_) -> IntHashtbl.add eval.caught_types path true) catches
	end;
	let restore () =
		if debugger then List.iter (fun (_,path,_) -> IntHashtbl.remove eval.caught_types path) catches
	in
	let v = try
		let v = try exec env with Stack_overflow -> exc_string "Stack overflow" in
		restore();
		v
	with RunTimeException(v,_,_) as exc ->
		eval.caught_exception <- vnull;
		restore();
		build_exception_stack eval env;
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
		ignore(varacc (fun _ -> v) env);
		exec env
	in
	v

(* Control flow *)

let emit_value exec = fun env ->
	exec env

let emit_seq2 exec1 exec2 = fun env ->
	ignore(exec1 env);
	exec2 env

let emit_seq4 exec1 exec2 exec3 exec4 = fun env ->
	ignore (exec1 env);
	ignore (exec2 env);
	ignore (exec3 env);
	exec4 env

let emit_seq8 exec1 exec2 exec3 exec4 exec5 exec6 exec7 exec8 = fun env ->
	ignore (exec1 env);
	ignore (exec2 env);
	ignore (exec3 env);
	ignore (exec4 env);
	ignore (exec5 env);
	ignore (exec6 env);
	ignore (exec7 env);
	exec8 env

let emit_return_null _ = raise_notrace (Return vnull)

let emit_return_value exec = fun env -> raise_notrace (Return (exec env))

let emit_break env = raise_notrace Break

let emit_continue env = raise_notrace Continue

let emit_throw exec p = fun env -> throw (exec env) p

let emit_safe_cast exec t p = fun env ->
	let v1 = exec env in
	match vresolve v1 with
	| VNull -> v1
	| _ -> if not (is v1 t) then throw_string "Class cast error" p else v1

(* Calls *)

(* The function [v] gives, forced once by the call site (the cache is only
   written with the forced value, which never changes). *)
let force_function cache v = match !cache with
	| Some f -> f
	| None ->
		let f = AtomicLazy.force v in
		cache := Some f;
		f
[@@inline]

(* super.call() - immediate *)

let emit_super_field_call max_depth slot proto i execs p =
	let args = emit_args execs in
	(fun env ->
		check_stack_depth max_depth env;
		let vthis = env.env_locals.(slot) in
		let vf = proto.pfields.(i) in
		let vl = args env in
		call_value_on vthis vf vl
	)

(* Type.call() - immediate *)

let emit_proto_field_call max_depth v execs p =
	let args = emit_args execs in
	let fcache = ref None in
	let pmin = Pos.min p and pmax = Pos.max p in
	(fun env ->
		check_stack_depth max_depth env;
		let f = force_function fcache v in
		let vl = args env in
		env.env_leave_pmin <- pmin;
		env.env_leave_pmax <- pmax;
		f vl
	)

(* instance.call() where call is overridden - dynamic dispatch *)

let get_prototype v p = match vresolve v with
	| VInstance {iproto = proto} | VPrototype proto -> proto
	| VString _ -> (get_ctx()).string_prototype
	| VArray _ -> (get_ctx()).array_prototype
	| VVector _ -> (get_ctx()).vector_prototype
	| _ -> unexpected_value_p v "instance" p

(* The prototype a call site last saw, with the prototype declaring the method
   and its slot (a monomorphic inline cache). An entry is immutable: a domain
   reading the cache while another one changes it sees either entry. *)
type method_cache = {
	mc_proto : vprototype;
	mc_owner : vprototype;
	mc_slot : int;
}

let rec find_method proto name =
	match IntMap.find_opt name proto.pnames with
	| Some i -> proto,i
	| None -> match proto.pparent with
		| Some proto -> find_method proto name
		| None -> raise Not_found

let emit_method_call max_depth exec name execs p =
	let args = emit_args execs in
	let pmin = Pos.min p and pmax = Pos.max p in
	let cache = ref None in
	let lookup proto =
		let owner,i = try find_method proto name with Not_found -> throw_string (Printf.sprintf "Field %s not found on prototype %s" (rev_hash name) (rev_hash proto.ppath)) p in
		cache := Some {mc_proto = proto; mc_owner = owner; mc_slot = i};
		get_static owner i
	in
	(fun env ->
		check_stack_depth max_depth env;
		let vthis = exec env in
		let proto = get_prototype vthis p in
		let vf = match !cache with
			| Some c when c.mc_proto == proto -> get_static c.mc_owner c.mc_slot
			| _ -> lookup proto
		in
		let vl = args env in
		env.env_leave_pmin <- pmin;
		env.env_leave_pmax <- pmax;
		call_value_on vthis vf vl
	)

(* instance.call() where call is not a method - lookup + this-binding *)

let emit_field_call max_depth exec name execs p =
	let args = emit_args execs in
	let pmin = Pos.min p and pmax = Pos.max p in
	(fun env ->
		check_stack_depth max_depth env;
		let vthis = exec env in
		let vf = field vthis name in
		env.env_leave_pmin <- pmin;
		env.env_leave_pmax <- pmax;
		call_value_on vthis vf (args env)
	)

(* new() - immediate + this-binding *)

let emit_constructor_call max_depth proto v execs p =
	let args = emit_args execs in
	let fcache = ref None in
	let pmin = Pos.min p and pmax = Pos.max p in
	(fun env ->
		check_stack_depth max_depth env;
		let f = force_function fcache v in
		let vthis = create_instance_direct proto INormal in
		let vl = args env in
		env.env_leave_pmin <- pmin;
		env.env_leave_pmax <- pmax;
		ignore(f (vthis :: vl));
		vthis
	)

(* super() - immediate + this-binding *)

let emit_special_super_call max_depth fnew execs =
	let args = emit_args execs in
	(fun env ->
		check_stack_depth max_depth env;
		let vl = args env in
		let vi' = fnew vl in
		let vthis = env.env_locals.(0) in
		(* This isn't very elegant, but it's probably a rare case to extend these types. *)
		begin match vthis,vi' with
			| VInstance vi,VInstance vi' -> vi.ikind <- vi'.ikind
			| _ -> die "" __LOC__
		end;
		vnull
	)

let emit_super_call max_depth v execs p =
	let args = emit_args execs in
	let fcache = ref None in
	let pmin = Pos.min p and pmax = Pos.max p in
	(fun env ->
		check_stack_depth max_depth env;
		let f = force_function fcache v in
		let vthis = env.env_locals.(0) in
		let vl = args env in
		env.env_leave_pmin <- pmin;
		env.env_leave_pmax <- pmax;
		ignore(f (vthis :: vl));
		vthis
	)

(* unknown call - full lookup *)

let emit_call max_depth exec execs p =
	let args = emit_args execs in
	let pmin = Pos.min p and pmax = Pos.max p in
	(fun env ->
		check_stack_depth max_depth env;
		let v1 = exec env in
		env.env_leave_pmin <- pmin;
		env.env_leave_pmax <- pmax;
		call_value v1 (args env)
	)

(* Read *)

let emit_local_read i = fun env -> env.env_locals.(i)

let emit_capture_read i = fun env -> env.env_captures.(i)

let emit_array_length_read exec p = fun env -> vint (as_array p (exec env)).alength

let emit_local_array_length_read slot p = fun env -> vint (as_array p env.env_locals.(slot)).alength

let emit_vector_length_read exec p = fun env -> vint (Array.length (as_vector p (exec env)))

let emit_bytes_length_read exec p = fun env -> vint (Bytes.length (as_bytes p (exec env)))

let emit_proto_field_read proto i = fun env ->
	get_static proto i

let emit_instance_field_read exec p i = fun env -> match exec env with
	| VInstance vi -> vi.ifields.(i)
	| VString s -> vint (s.slength)
	| VNull -> throw_string "field access on null" p
	| v -> unexpected_value_p v "instance" p

let emit_local_instance_field_read slot p i = fun env -> match env.env_locals.(slot) with
	| VInstance vi -> vi.ifields.(i)
	| VString s -> vint (s.slength)
	| VNull -> throw_string "field access on null" p
	| v -> unexpected_value_p v "instance" p

let emit_this_field_read iv i = fun env -> match env.env_locals.(iv) with
	| VInstance vi -> vi.ifields.(i)
	| v -> unexpected_value v "instance"

let emit_field_closure exec name = fun env ->
	let v = exec env in
	dynamic_field v name

let anon_field_read proto i name p v = match vresolve v with
	| VObject o ->
		begin match o.oproto with
		| OProto proto' when proto' == proto ->
			object_field_at o i
		| _ ->
			object_field o name
		end
	| VNull -> throw_string "field access on null" p
	| v -> field v name
[@@inline]

let emit_anon_field_read exec proto i name p = fun env ->
	anon_field_read proto i name p (exec env)

let emit_local_anon_field_read slot proto i name p = fun env ->
	anon_field_read proto i name p env.env_locals.(slot)

let emit_field_read exec name p = fun env -> match exec env with
	| VNull -> throw_string "field access on null" p
	| v -> field v name

let emit_array_read exec1 p1 exec2 p2 = fun env ->
	let a = as_array p1 (exec1 env) in
	let i = as_int p2 (exec2 env) in
	if i < 0 then vnull
	else EvalArray.get a i

let emit_array_read_ll slot1 p1 slot2 p2 = fun env ->
	let a = as_array p1 env.env_locals.(slot1) in
	let i = as_int p2 env.env_locals.(slot2) in
	if i < 0 then vnull
	else EvalArray.get a i

let emit_array_read_le slot1 p1 exec2 p2 = fun env ->
	let a = as_array p1 env.env_locals.(slot1) in
	let i = as_int p2 (exec2 env) in
	if i < 0 then vnull
	else EvalArray.get a i

let emit_vector_read exec1 p1 exec2 p2 = fun env ->
	let v = as_vector p1 (exec1 env) in
	let i = as_int p2 (exec2 env) in
	if i < 0 then vnull
	else Array.unsafe_get v i

let emit_enum_index exec p = fun env -> vint (as_enum_value p (exec env)).eindex

let emit_enum_parameter_read exec i = fun env -> match exec env with
	| VEnumValue ev -> ev.eargs.(i)
	| v1 -> unexpected_value v1 "enum value"

let emit_local_enum_index slot p = fun env -> vint (as_enum_value p env.env_locals.(slot)).eindex

let emit_local_enum_parameter_read slot i = fun env -> match env.env_locals.(slot) with
	| VEnumValue ev -> ev.eargs.(i)
	| v1 -> unexpected_value v1 "enum value"

let emit_string_cca exec1 exec2 p = fun env ->
	let s = decode_vstring (exec1 env) in
	let index = decode_int_p (exec2 env) p in
	if index < 0 || index >= s.slength then vnull
	else vint (EvalString.char_at s index)

let emit_string_cca_unsafe exec1 exec2 p = fun env ->
	let s = decode_vstring (exec1 env) in
	let index = decode_int_p (exec2 env) p in
	vint (EvalString.char_at s index)

(* Write *)

let emit_bytes_length_write exec1 exec2 = fun env ->
	let v1 = exec1 env in
	let v2 = exec2 env in
	set_bytes_length_field v1 v2;
	v2

let emit_local_write slot exec = fun env ->
	let v = exec env in
	env.env_locals.(slot) <- v;
	v

let emit_capture_write slot exec = fun env ->
	let v = exec env in
	env.env_captures.(slot) <- v;
	v

let emit_proto_field_write proto i exec2 = fun env ->
	let v = exec2 env in
	set_static proto i v;
	v

let emit_instance_field_write exec1 p i exec2 = fun env -> match exec1 env with
	| VInstance vi ->
		let v = exec2 env in
		vi.ifields.(i) <- v;
		v
	| v -> unexpected_value_p v "instance" p

let emit_local_instance_field_write slot p i exec2 = fun env -> match env.env_locals.(slot) with
	| VInstance vi ->
		let v = exec2 env in
		vi.ifields.(i) <- v;
		v
	| v -> unexpected_value_p v "instance" p

let emit_anon_field_write exec1 p proto i name exec2 = fun env ->
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

let emit_field_write exec1 p1 name exec2 = fun env ->
	let v1 = exec1 env in
	let v2 = exec2 env in
	(try set_field v1 name v2 with RunTimeException(v,stack,_) -> raise_notrace (RunTimeException(v,stack,p1)));
	v2

let emit_array_write exec1 p1 exec2 p2 exec3 p = fun env ->
	let a = as_array p1 (exec1 env) in
	let i = as_int p2 (exec2 env) in
	let v3 = exec3 env in
	if i < 0 then throw_string (Printf.sprintf "Negative array index: %i" i) p;
	EvalArray.set a i v3;
	v3

let emit_vector_write exec1 p1 exec2 p2 exec3 p = fun env ->
	let vv = as_vector p1 (exec1 env) in
	let i = as_int p2 (exec2 env) in
	let v3 = exec3 env in
	if i < 0 then throw_string (Printf.sprintf "Negative vector index: %i" i) p;
	Array.unsafe_set vv i v3;
	v3

(* Read + write *)

let do_incr v p = match v with
	| VInt i -> vint (i + 1)
	| VInt64 i64 -> vint64 (Int64.succ i64)
	| VFloat f -> vfloat (f +. 1.)
	| v -> unexpected_value_p v "number" p

let emit_local_incr_prefix slot p = fun env ->
	let v0 = env.env_locals.(slot) in
	let v = do_incr v0 p in
	env.env_locals.(slot) <- v;
	v

let emit_local_incr_postfix slot p = fun env ->
	let v0 = env.env_locals.(slot) in
	let v = do_incr v0 p in
	env.env_locals.(slot) <- v;
	v0

let emit_local_read_write slot exec fop prefix = fun env ->
	let v1 = env.env_locals.(slot) in
	let v2 = exec env in
	let v = fop v1 v2 in
	env.env_locals.(slot) <- v;
	if prefix then v else v1

let emit_capture_read_write slot exec fop prefix = fun env ->
	let v1 = (env.env_captures.(slot)) in
	let v2 = exec env in
	let v = fop v1 v2 in
	env.env_captures.(slot) <- v;
	if prefix then v else v1

let proto_field_read_write proto i exec2 fop prefix env =
	let vf = get_static proto i in
	let v2 = exec2 env in
	let v = fop vf v2 in
	set_static proto i v;
	if prefix then v else vf

let emit_proto_field_read_write proto i exec2 fop prefix = fun env ->
	proto_field_read_write proto i exec2 fop prefix env

let instance_field_read_write vi i exec2 fop prefix env =
	let vf = vi.ifields.(i) in
	let v2 = exec2 env in
	let v = fop vf v2 in
	vi.ifields.(i) <- v;
	if prefix then v else vf

let emit_instance_field_read_write exec1 p1 i exec2 fop prefix = fun env -> match exec1 env with
	| VInstance vi -> instance_field_read_write vi i exec2 fop prefix env
	| v -> unexpected_value_p v "instance" p1

let emit_field_read_write exec1 p1 name exec2 fop prefix = fun env ->
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
		if is_shared_static proto i then !on_shared_state ();
		fresh_statics proto;
		proto_field_read_write proto i exec2 fop prefix env
	| _ ->
		let vf = field v1 name in
		let v2 = exec2 env in
		let v = fop vf v2 in
		(try set_field v1 name v with RunTimeException(v,stack,_) -> raise_notrace (RunTimeException(v,stack,p1)));
		if prefix then v else vf

let emit_array_read_write exec1 p1 exec2 p2 exec3 fop prefix = fun env ->
	let va = as_array p1 (exec1 env) in
	let i = as_int p2 (exec2 env) in
	if i < 0 then throw_string (Printf.sprintf "Negative array index: %i" i) p2;
	let v = EvalArray.get va i in
	let v2 = exec3 env in
	let v3 = fop v v2 in
	EvalArray.set va i v3;
	if prefix then v3 else v

let emit_vector_read_write exec1 p1 exec2 p2 exec3 fop prefix = fun env ->
	let va = as_vector p1 (exec1 env) in
	let i = as_int p2 (exec2 env) in
	if i < 0 then throw_string (Printf.sprintf "Negative vector index: %i" i) p2;
	let v = Array.unsafe_get va i in
	let v2 = exec3 env in
	let v3 = fop v v2 in
	Array.unsafe_set va i v3;
	if prefix then v3 else v

(* Ops *)

let emit_eq_null exec = fun env -> match vresolve (exec env) with
	| VNull -> VTrue
	| _ -> VFalse

let emit_not_eq_null exec = fun env -> match vresolve (exec env) with
	| VNull -> VFalse
	| _ -> VTrue

(* Arithmetic, with the same operand variants as the conditions (below). *)

let arith_add p v1 v2 = match v1,v2 with
	| VInt a,VInt b -> vint (a + b)
	| VFloat a,VFloat b -> vfloat (a +. b)
	| _ -> op_add p v1 v2
[@@inline]

let emit_op_add p exec1 exec2 = fun env ->
	let v1 = exec1 env in
	let v2 = exec2 env in
	arith_add p v1 v2

let emit_op_add_le p slot exec2 = fun env ->
	let v1 = env.env_locals.(slot) in
	let v2 = exec2 env in
	arith_add p v1 v2

let emit_op_add_lc p slot v2 = fun env ->
	arith_add p env.env_locals.(slot) v2

let emit_op_add_ll p slot1 slot2 = fun env ->
	arith_add p env.env_locals.(slot1) env.env_locals.(slot2)

let emit_op_add_ec p exec1 v2 = fun env ->
	arith_add p (exec1 env) v2

(* [local op= exec] *)
let emit_local_assign_add p slot exec = fun env ->
	let v1 = env.env_locals.(slot) in
	let v2 = exec env in
	let v = arith_add p v1 v2 in
	env.env_locals.(slot) <- v;
	v

let arith_sub p v1 v2 = match v1,v2 with
	| VInt a,VInt b -> vint (a - b)
	| VFloat a,VFloat b -> vfloat (a -. b)
	| _ -> op_sub p v1 v2
[@@inline]

let emit_op_sub p exec1 exec2 = fun env ->
	let v1 = exec1 env in
	let v2 = exec2 env in
	arith_sub p v1 v2

let emit_op_sub_le p slot exec2 = fun env ->
	let v1 = env.env_locals.(slot) in
	let v2 = exec2 env in
	arith_sub p v1 v2

let emit_op_sub_lc p slot v2 = fun env ->
	arith_sub p env.env_locals.(slot) v2

let emit_op_sub_ll p slot1 slot2 = fun env ->
	arith_sub p env.env_locals.(slot1) env.env_locals.(slot2)

let emit_op_sub_ec p exec1 v2 = fun env ->
	arith_sub p (exec1 env) v2

(* [local op= exec] *)
let emit_local_assign_sub p slot exec = fun env ->
	let v1 = env.env_locals.(slot) in
	let v2 = exec env in
	let v = arith_sub p v1 v2 in
	env.env_locals.(slot) <- v;
	v

let arith_mult p v1 v2 = match v1,v2 with
	| VInt a,VInt b -> vint (a * b)
	| VFloat a,VFloat b -> vfloat (a *. b)
	| _ -> op_mult p v1 v2
[@@inline]

let emit_op_mult p exec1 exec2 = fun env ->
	let v1 = exec1 env in
	let v2 = exec2 env in
	arith_mult p v1 v2

let emit_op_mult_le p slot exec2 = fun env ->
	let v1 = env.env_locals.(slot) in
	let v2 = exec2 env in
	arith_mult p v1 v2

let emit_op_mult_lc p slot v2 = fun env ->
	arith_mult p env.env_locals.(slot) v2

let emit_op_mult_ll p slot1 slot2 = fun env ->
	arith_mult p env.env_locals.(slot1) env.env_locals.(slot2)

let emit_op_mult_ec p exec1 v2 = fun env ->
	arith_mult p (exec1 env) v2

(* [local op= exec] *)
let emit_local_assign_mult p slot exec = fun env ->
	let v1 = env.env_locals.(slot) in
	let v2 = exec env in
	let v = arith_mult p v1 v2 in
	env.env_locals.(slot) <- v;
	v

let arith_div p v1 v2 = match v1,v2 with
	| _ -> op_div p v1 v2
[@@inline]

let emit_op_div p exec1 exec2 = fun env ->
	let v1 = exec1 env in
	let v2 = exec2 env in
	arith_div p v1 v2

let emit_op_div_le p slot exec2 = fun env ->
	let v1 = env.env_locals.(slot) in
	let v2 = exec2 env in
	arith_div p v1 v2

let emit_op_div_lc p slot v2 = fun env ->
	arith_div p env.env_locals.(slot) v2

let emit_op_div_ll p slot1 slot2 = fun env ->
	arith_div p env.env_locals.(slot1) env.env_locals.(slot2)

let emit_op_div_ec p exec1 v2 = fun env ->
	arith_div p (exec1 env) v2

let arith_and p v1 v2 = match v1,v2 with
	| VInt a,VInt b -> VInt (a land b)
	| _ -> op_and p v1 v2
[@@inline]

let emit_op_and p exec1 exec2 = fun env ->
	let v1 = exec1 env in
	let v2 = exec2 env in
	arith_and p v1 v2

let emit_op_and_le p slot exec2 = fun env ->
	let v1 = env.env_locals.(slot) in
	let v2 = exec2 env in
	arith_and p v1 v2

let emit_op_and_lc p slot v2 = fun env ->
	arith_and p env.env_locals.(slot) v2

let emit_op_and_ll p slot1 slot2 = fun env ->
	arith_and p env.env_locals.(slot1) env.env_locals.(slot2)

let emit_op_and_ec p exec1 v2 = fun env ->
	arith_and p (exec1 env) v2

(* [local op= exec] *)
let emit_local_assign_and p slot exec = fun env ->
	let v1 = env.env_locals.(slot) in
	let v2 = exec env in
	let v = arith_and p v1 v2 in
	env.env_locals.(slot) <- v;
	v

let arith_or p v1 v2 = match v1,v2 with
	| VInt a,VInt b -> VInt (a lor b)
	| _ -> op_or p v1 v2
[@@inline]

let emit_op_or p exec1 exec2 = fun env ->
	let v1 = exec1 env in
	let v2 = exec2 env in
	arith_or p v1 v2

let emit_op_or_le p slot exec2 = fun env ->
	let v1 = env.env_locals.(slot) in
	let v2 = exec2 env in
	arith_or p v1 v2

let emit_op_or_lc p slot v2 = fun env ->
	arith_or p env.env_locals.(slot) v2

let emit_op_or_ll p slot1 slot2 = fun env ->
	arith_or p env.env_locals.(slot1) env.env_locals.(slot2)

let emit_op_or_ec p exec1 v2 = fun env ->
	arith_or p (exec1 env) v2

(* [local op= exec] *)
let emit_local_assign_or p slot exec = fun env ->
	let v1 = env.env_locals.(slot) in
	let v2 = exec env in
	let v = arith_or p v1 v2 in
	env.env_locals.(slot) <- v;
	v

let arith_xor p v1 v2 = match v1,v2 with
	| VInt a,VInt b -> VInt (a lxor b)
	| _ -> op_xor p v1 v2
[@@inline]

let emit_op_xor p exec1 exec2 = fun env ->
	let v1 = exec1 env in
	let v2 = exec2 env in
	arith_xor p v1 v2

let emit_op_xor_le p slot exec2 = fun env ->
	let v1 = env.env_locals.(slot) in
	let v2 = exec2 env in
	arith_xor p v1 v2

let emit_op_xor_lc p slot v2 = fun env ->
	arith_xor p env.env_locals.(slot) v2

let emit_op_xor_ll p slot1 slot2 = fun env ->
	arith_xor p env.env_locals.(slot1) env.env_locals.(slot2)

let emit_op_xor_ec p exec1 v2 = fun env ->
	arith_xor p (exec1 env) v2

(* [local op= exec] *)
let emit_local_assign_xor p slot exec = fun env ->
	let v1 = env.env_locals.(slot) in
	let v2 = exec env in
	let v = arith_xor p v1 v2 in
	env.env_locals.(slot) <- v;
	v

let arith_shl p v1 v2 = match v1,v2 with
	| _ -> op_shl p v1 v2
[@@inline]

let emit_op_shl p exec1 exec2 = fun env ->
	let v1 = exec1 env in
	let v2 = exec2 env in
	arith_shl p v1 v2

let emit_op_shl_le p slot exec2 = fun env ->
	let v1 = env.env_locals.(slot) in
	let v2 = exec2 env in
	arith_shl p v1 v2

let emit_op_shl_lc p slot v2 = fun env ->
	arith_shl p env.env_locals.(slot) v2

let emit_op_shl_ll p slot1 slot2 = fun env ->
	arith_shl p env.env_locals.(slot1) env.env_locals.(slot2)

let emit_op_shl_ec p exec1 v2 = fun env ->
	arith_shl p (exec1 env) v2

let arith_shr p v1 v2 = match v1,v2 with
	| _ -> op_shr p v1 v2
[@@inline]

let emit_op_shr p exec1 exec2 = fun env ->
	let v1 = exec1 env in
	let v2 = exec2 env in
	arith_shr p v1 v2

let emit_op_shr_le p slot exec2 = fun env ->
	let v1 = env.env_locals.(slot) in
	let v2 = exec2 env in
	arith_shr p v1 v2

let emit_op_shr_lc p slot v2 = fun env ->
	arith_shr p env.env_locals.(slot) v2

let emit_op_shr_ll p slot1 slot2 = fun env ->
	arith_shr p env.env_locals.(slot1) env.env_locals.(slot2)

let emit_op_shr_ec p exec1 v2 = fun env ->
	arith_shr p (exec1 env) v2

let arith_ushr p v1 v2 = match v1,v2 with
	| _ -> op_ushr p v1 v2
[@@inline]

let emit_op_ushr p exec1 exec2 = fun env ->
	let v1 = exec1 env in
	let v2 = exec2 env in
	arith_ushr p v1 v2

let emit_op_ushr_le p slot exec2 = fun env ->
	let v1 = env.env_locals.(slot) in
	let v2 = exec2 env in
	arith_ushr p v1 v2

let emit_op_ushr_lc p slot v2 = fun env ->
	arith_ushr p env.env_locals.(slot) v2

let emit_op_ushr_ll p slot1 slot2 = fun env ->
	arith_ushr p env.env_locals.(slot1) env.env_locals.(slot2)

let emit_op_ushr_ec p exec1 v2 = fun env ->
	arith_ushr p (exec1 env) v2

let arith_mod p v1 v2 = match v1,v2 with
	| _ -> op_mod p v1 v2
[@@inline]

let emit_op_mod p exec1 exec2 = fun env ->
	let v1 = exec1 env in
	let v2 = exec2 env in
	arith_mod p v1 v2

let emit_op_mod_le p slot exec2 = fun env ->
	let v1 = env.env_locals.(slot) in
	let v2 = exec2 env in
	arith_mod p v1 v2

let emit_op_mod_lc p slot v2 = fun env ->
	arith_mod p env.env_locals.(slot) v2

let emit_op_mod_ll p slot1 slot2 = fun env ->
	arith_mod p env.env_locals.(slot1) env.env_locals.(slot2)

let emit_op_mod_ec p exec1 v2 = fun env ->
	arith_mod p (exec1 env) v2

let emit_op_eq exec1 exec2 = fun env ->
	let v1 = exec1 env in
	let v2 = exec2 env in
	match v1,v2 with
	| VInt a,VInt b -> vbool (a = b)
	| _ -> vbool (equals v1 v2)

let emit_op_not_eq exec1 exec2 = fun env ->
	let v1 = exec1 env in
	let v2 = exec2 env in
	match v1,v2 with
	| VInt a,VInt b -> vbool (a <> b)
	| _ -> vbool  (not (equals v1 v2))

let emit_op_gt exec1 exec2 = fun env ->
	let v1 = exec1 env in
	let v2 = exec2 env in
	match v1,v2 with
	| VInt a,VInt b -> vbool (a > b)
	| VFloat a,VFloat b -> vbool (a > b)
	| _ -> vbool (compare v1 v2 = CSup)

let emit_op_gte exec1 exec2 = fun env ->
	let v1 = exec1 env in
	let v2 = exec2 env in
	match v1,v2 with
	| VInt a,VInt b -> vbool (a >= b)
	| VFloat a,VFloat b -> vbool (a >= b)
	| _ -> vbool (match compare v1 v2 with CSup | CEq -> true | _ -> false)

let emit_op_lt exec1 exec2 = fun env ->
	let v1 = exec1 env in
	let v2 = exec2 env in
	match v1,v2 with
	| VInt a,VInt b -> vbool (a < b)
	| VFloat a,VFloat b -> vbool (a < b)
	| _ -> vbool (compare v1 v2 = CInf)

let emit_op_lte exec1 exec2 = fun env ->
	let v1 = exec1 env in
	let v2 = exec2 env in
	match v1,v2 with
	| VInt a,VInt b -> vbool (a <= b)
	| VFloat a,VFloat b -> vbool (a <= b)
	| _ -> vbool (match compare v1 v2 with CInf | CEq -> true | _ -> false)

let emit_not exec = fun env -> match exec env with
	| VNull | VFalse -> VTrue
	| _ -> VFalse

let emit_bool_and exec1 exec2 = fun env ->
	if is_true (exec1 env) then exec2 env
	else VFalse

let emit_bool_or exec1 exec2 = fun env ->
	if is_true (exec1 env) then VTrue
	else exec2 env

let emit_neg exec p = fun env -> match exec env with
	| VFloat f -> vfloat (-.f)
	| VInt i -> vint (-i)
	| VInt64 i -> vint64 (Int64.neg i)
	| _ -> throw_string "Invalid operation" p

(* Conditions

   A condition is compiled to a closure giving a bool: [cond e] is
   [is_true (exec e)], with the same evaluation, without building and matching
   a Bool value. Comparisons have variants for a local operand ([l]) and a
   constant one ([c]), read without calling a closure. *)

let emit_cond_value exec = fun env -> is_true (exec env)

let emit_cond_not exec = fun env -> match exec env with
	| VNull | VFalse -> true
	| _ -> false

let emit_cond_not_cond cond = fun env -> not (cond env)

let emit_cond_and cond1 cond2 = fun env -> cond1 env && cond2 env

let emit_cond_or cond1 cond2 = fun env -> cond1 env || cond2 env

let emit_cond_eq_null exec = fun env -> match vresolve (exec env) with
	| VNull -> true
	| _ -> false

let emit_cond_not_eq_null exec = fun env -> match vresolve (exec env) with
	| VNull -> false
	| _ -> true

let emit_cond_local_eq_null slot = fun env -> match vresolve env.env_locals.(slot) with
	| VNull -> true
	| _ -> false

let emit_cond_local_not_eq_null slot = fun env -> match vresolve env.env_locals.(slot) with
	| VNull -> false
	| _ -> true

let cond_lt v1 v2 = match v1,v2 with
	| VInt a,VInt b -> a < b
	| VFloat a,VFloat b -> a < b
	| _ -> compare v1 v2 = CInf
[@@inline]

let emit_cond_lt exec1 exec2 = fun env ->
	let v1 = exec1 env in
	let v2 = exec2 env in
	cond_lt v1 v2

let emit_cond_lt_le slot exec2 = fun env ->
	let v1 = env.env_locals.(slot) in
	let v2 = exec2 env in
	cond_lt v1 v2

let emit_cond_lt_lc slot v2 = fun env ->
	cond_lt env.env_locals.(slot) v2

let emit_cond_lt_ll slot1 slot2 = fun env ->
	cond_lt env.env_locals.(slot1) env.env_locals.(slot2)

let emit_cond_lt_ec exec1 v2 = fun env ->
	cond_lt (exec1 env) v2

let cond_lte v1 v2 = match v1,v2 with
	| VInt a,VInt b -> a <= b
	| VFloat a,VFloat b -> a <= b
	| _ -> (match compare v1 v2 with CInf | CEq -> true | _ -> false)
[@@inline]

let emit_cond_lte exec1 exec2 = fun env ->
	let v1 = exec1 env in
	let v2 = exec2 env in
	cond_lte v1 v2

let emit_cond_lte_le slot exec2 = fun env ->
	let v1 = env.env_locals.(slot) in
	let v2 = exec2 env in
	cond_lte v1 v2

let emit_cond_lte_lc slot v2 = fun env ->
	cond_lte env.env_locals.(slot) v2

let emit_cond_lte_ll slot1 slot2 = fun env ->
	cond_lte env.env_locals.(slot1) env.env_locals.(slot2)

let emit_cond_lte_ec exec1 v2 = fun env ->
	cond_lte (exec1 env) v2

let cond_gt v1 v2 = match v1,v2 with
	| VInt a,VInt b -> a > b
	| VFloat a,VFloat b -> a > b
	| _ -> compare v1 v2 = CSup
[@@inline]

let emit_cond_gt exec1 exec2 = fun env ->
	let v1 = exec1 env in
	let v2 = exec2 env in
	cond_gt v1 v2

let emit_cond_gt_le slot exec2 = fun env ->
	let v1 = env.env_locals.(slot) in
	let v2 = exec2 env in
	cond_gt v1 v2

let emit_cond_gt_lc slot v2 = fun env ->
	cond_gt env.env_locals.(slot) v2

let emit_cond_gt_ll slot1 slot2 = fun env ->
	cond_gt env.env_locals.(slot1) env.env_locals.(slot2)

let emit_cond_gt_ec exec1 v2 = fun env ->
	cond_gt (exec1 env) v2

let cond_gte v1 v2 = match v1,v2 with
	| VInt a,VInt b -> a >= b
	| VFloat a,VFloat b -> a >= b
	| _ -> (match compare v1 v2 with CSup | CEq -> true | _ -> false)
[@@inline]

let emit_cond_gte exec1 exec2 = fun env ->
	let v1 = exec1 env in
	let v2 = exec2 env in
	cond_gte v1 v2

let emit_cond_gte_le slot exec2 = fun env ->
	let v1 = env.env_locals.(slot) in
	let v2 = exec2 env in
	cond_gte v1 v2

let emit_cond_gte_lc slot v2 = fun env ->
	cond_gte env.env_locals.(slot) v2

let emit_cond_gte_ll slot1 slot2 = fun env ->
	cond_gte env.env_locals.(slot1) env.env_locals.(slot2)

let emit_cond_gte_ec exec1 v2 = fun env ->
	cond_gte (exec1 env) v2

let cond_eq v1 v2 = match v1,v2 with
	| VInt a,VInt b -> a = b
	| _ -> equals v1 v2
[@@inline]

let emit_cond_eq exec1 exec2 = fun env ->
	let v1 = exec1 env in
	let v2 = exec2 env in
	cond_eq v1 v2

let emit_cond_eq_le slot exec2 = fun env ->
	let v1 = env.env_locals.(slot) in
	let v2 = exec2 env in
	cond_eq v1 v2

let emit_cond_eq_lc slot v2 = fun env ->
	cond_eq env.env_locals.(slot) v2

let emit_cond_eq_ll slot1 slot2 = fun env ->
	cond_eq env.env_locals.(slot1) env.env_locals.(slot2)

let emit_cond_eq_ec exec1 v2 = fun env ->
	cond_eq (exec1 env) v2

let cond_neq v1 v2 = match v1,v2 with
	| VInt a,VInt b -> a <> b
	| _ -> not (equals v1 v2)
[@@inline]

let emit_cond_neq exec1 exec2 = fun env ->
	let v1 = exec1 env in
	let v2 = exec2 env in
	cond_neq v1 v2

let emit_cond_neq_le slot exec2 = fun env ->
	let v1 = env.env_locals.(slot) in
	let v2 = exec2 env in
	cond_neq v1 v2

let emit_cond_neq_lc slot v2 = fun env ->
	cond_neq env.env_locals.(slot) v2

let emit_cond_neq_ll slot1 slot2 = fun env ->
	cond_neq env.env_locals.(slot1) env.env_locals.(slot2)

let emit_cond_neq_ec exec1 v2 = fun env ->
	cond_neq (exec1 env) v2

let emit_if_cond cond exec_then exec_else = fun env ->
	if cond env then exec_then env else exec_else env

let rec run_while_cond_continue cond exec_body env =
	try
		while cond env do ignore(exec_body env) done;
	with Continue ->
		run_while_cond_continue cond exec_body env

let emit_while_cond cond exec_body = fun env ->
	(try run_while_cond_continue cond exec_body env with Break -> ());
	vnull

let emit_do_while_cond cond exec_body = fun env ->
	begin try
		ignore(exec_body env); run_while_cond_continue cond exec_body env
	with
		| Break -> ()
		| Continue -> try run_while_cond_continue cond exec_body env with Break -> ()
	end;
	vnull

(* Function *)

(* The parameters of a function are the slots of its arguments: a local slot,
   or [-1 - slot] for a captured one. *)
let param_local slot = slot
let param_capture slot = -1 - slot

(* (top-level functions: local ones would be closures allocated at every call) *)
let set_argument params k env v =
	let slot = Array.unsafe_get params k in
	if slot >= 0 then env.env_locals.(slot) <- v
	else env.env_captures.(-1 - slot) <- v

let rec set_arguments params n k vl env =
	if k < n then begin match vl with
		| v :: vl ->
			set_argument params k env v;
			set_arguments params n (k + 1) vl env
		| [] ->
			set_argument params k env vnull;
			set_arguments params n (k + 1) [] env
	end else match vl with
		| [] -> ()
		| _ -> exc_string (Printf.sprintf "Bad number of arguments: %i vs. %i" 0 (List.length vl))

let process_arguments params vl env =
	set_arguments params (Array.length params) 0 vl env

(* The functions run their body in an environment of their own, popped however
   the body ends (without allocating a closure for it, as [Std.finally] does). *)

let create_function_noret ctx eci exec fl = fun vl ->
	let env = push_environment ctx eci in
	match
		process_arguments fl vl env;
		exec env
	with
	| v -> pop_environment ctx env; v
	| exception exc -> pop_environment ctx env; raise exc

let create_function ctx eci exec fl = fun vl ->
	let env = push_environment ctx eci in
	match
		process_arguments fl vl env;
		exec env
	with
	| v -> pop_environment ctx env; v
	| exception Return v -> pop_environment ctx env; v
	| exception exc -> pop_environment ctx env; raise exc

let set_captures env refs =
	for k = 0 to Array.length refs - 1 do
		let (i,vr) = Array.unsafe_get refs k in
		env.env_captures.(i) <- vr
	done

let create_closure_noret ctx eci refs exec fl = fun vl ->
	let env = push_environment ctx eci in
	match
		set_captures env refs;
		process_arguments fl vl env;
		exec env
	with
	| v -> pop_environment ctx env; v
	| exception exc -> pop_environment ctx env; raise exc

let create_closure refs ctx eci exec fl = fun vl ->
	let env = push_environment ctx eci in
	match
		set_captures env refs;
		process_arguments fl vl env;
		exec env
	with
	| v -> pop_environment ctx env; v
	| exception Return v -> pop_environment ctx env; v
	| exception exc -> pop_environment ctx env; raise exc

let emit_closure ctx mapping eci hasret exec fl = fun env ->
	let refs = Array.map (fun (i,slot) -> i,env.env_captures.(slot)) mapping in
	let create = match hasret,eci.num_captures with
		| true,0 -> create_function
		| false,0 -> create_function_noret
		| _ -> create_closure refs
	in
	let f = create ctx eci exec fl in
	vstatic_function f
