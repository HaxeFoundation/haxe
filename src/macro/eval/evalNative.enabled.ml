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

(*
	The native tier of eval (docs/macro-jit-native.md): the body of a function
	is compiled to native code by the OCaml backend (libs/jit), in place of the
	tree of closures of EvalJit.

	The native code is the [exec : env -> value] of the function: the
	environment, the arguments and the call stack are handled by
	[EvalEmitter.create_function] as for closures, and locals live in
	[env.env_locals], at the slots given by the same [EvalJitContext]. An
	expression that is not translated is compiled by EvalJit in that context
	and called from the native code, so that both tiers mix at the level of
	expressions. Operations call the OCaml functions that the emitters call,
	in the same order, through a table of constants of the unit.
*)

open Globals
open Ast
open Type
open EvalValue
open EvalContext
open EvalHash
open EvalEmitter
open EvalMisc
open EvalField
open EvalPrinting
open EvalJitContext
open Ocaml_backend
open Cmm

exception Unsupported of string

let dbg = Debuginfo.none

(* Layout of the values the native code reads *)

let raw_immediate (v : value) =
	let n : int = Obj.magic v in
	2 * n + 1

let raw_null = raw_immediate VNull
let raw_true = raw_immediate VTrue
let raw_false = raw_immediate VFalse

(* Indices of the fields of [env], found on a sample record *)
let env_field_locals,env_field_captures,env_field_leave_pmin,env_field_leave_pmax =
	let locals = [|VNull|] and captures = [|VTrue|] in
	let env = {
		env_info = Obj.magic 0;
		env_debug = Obj.magic 0;
		env_leave_pmin = 0x1234;
		env_leave_pmax = 0x4321;
		env_locals = locals;
		env_captures = captures;
		env_extra_locals = IntMap.empty;
		env_parent = None;
		env_stack_depth = 0;
		env_eval = Obj.magic 0;
	} in
	let r = Obj.repr env in
	let find p =
		let rec loop i = if i >= Obj.size r then die "" __LOC__ else if p (Obj.field r i) then i else loop (i + 1) in
		loop 0
	in
	find (fun f -> f == Obj.repr locals),
	find (fun f -> f == Obj.repr captures),
	find (fun f -> Obj.is_int f && (Obj.obj f : int) = 0x1234),
	find (fun f -> Obj.is_int f && (Obj.obj f : int) = 0x4321)

(* Cmm *)

let int n = Cconst_int(n,dbg)
let op o args = Cop(o,args,dbg)
let ifte c a b = Cifthenelse(c,dbg,a,dbg,b,dbg)
let load e i = Cmm_helpers.get_field_gen Asttypes.Mutable e i dbg
let store e i v = Cmm_helpers.setfield i Lambda.Pointer Lambda.Assignment e v dbg
let store_int e i n = Cmm_helpers.setfield i Lambda.Immediate Lambda.Assignment e (int (2 * n + 1)) dbg
let eq a b = op (Ccmpi Ceq) [a;b]

let var name =
	let v = Backend_var.create_local name in
	v,Backend_var.With_provenance.create v

(* [k] applied to the value of [e], bound to a variable: forces the order of
   evaluation *)
let bind name e k = match e with
	| Cvar _ | Cconst_int _ | Cconst_symbol _ -> k e
	| _ ->
		let v,vp = var name in
		Clet(vp,e,k (Cvar v))

let rec bind_list name el k = match el with
	| [] -> k []
	| e :: el -> bind name e (fun e -> bind_list name el (fun el -> k (e :: el)))

(* An OCaml list of values *)
let make_list el =
	List.fold_right (fun e acc -> Cmm_helpers.make_alloc dbg 0 [e;acc]) el (int 1)

let is_true e = eq e (int raw_true)

(* Translation context of a function *)

type t = {
	jit : EvalJitContext.t;
	(* The table of constants of the unit: values and the OCaml functions the
	   code calls *)
	consts : Obj.t DynArray.t;
	consts_symbol : string;
	env : Backend_var.t;
	(* The exit of the function, with the returned value *)
	return_label : int;
	(* Exits of the enclosing loops: break, continue *)
	mutable loops : (int * int) list;
}

let const t (v : Obj.t) =
	let i = DynArray.length t.consts in
	DynArray.add t.consts v;
	load (Cconst_symbol(t.consts_symbol,dbg)) i

let value_const t (v : value) = match v with
	| VNull | VTrue | VFalse -> int (raw_immediate v)
	| _ -> const t (Obj.repr v)

(* Call of the OCaml function [f] *)
let call t (f : Obj.t) args =
	Cmm_helpers.generic_apply Asttypes.Mutable (const t f) args dbg

let env t = Cvar t.env
let locals t = load (env t) env_field_locals
let captures t = load (env t) env_field_captures

let unit_value = int 1

(* The value of [e], then [vnull] *)
let seq_null e = Csequence(e,int raw_null)

(* Expressions compiled by EvalJit *)

let fallback t return e =
	let exec = EvalJit.jit_subexpr t.jit return e in
	call t (Obj.repr exec) [env t]

(* Whether [e] contains a [break] or [continue] of an enclosing loop *)
let rec has_free_break_continue e = match e.eexpr with
	| TBreak | TContinue -> true
	| TWhile _ | TFunction _ -> false
	| _ -> check_expr has_free_break_continue e

let strip_parenthesis e = match e.eexpr with
	| TParenthesis e -> e
	| _ -> e

(* Whether [e] is translated to native code (otherwise compiled by EvalJit).
   Must match [expr]. *)
let rec is_native ctx e = match e.eexpr with
	| TBlock _ | TVar _ | TLocal _ | TConst _ | TIf _ | TReturn _
	| TBreak | TContinue | TParenthesis _ | TMeta _ | TCast(_,None) ->
		true
	| TWhile(_,body,_) ->
		not (loop_needs_fallback ctx body)
	| TBinop(OpEq,_,{eexpr = TConst TNull}) | TBinop(OpEq,{eexpr = TConst TNull},_)
	| TBinop(OpNotEq,_,{eexpr = TConst TNull}) | TBinop(OpNotEq,{eexpr = TConst TNull},_) ->
		true
	| TBinop(OpAssign,{eexpr = TLocal _},_) ->
		true
	| TBinop((OpAdd | OpMult | OpDiv | OpSub | OpEq | OpNotEq | OpGt | OpGte | OpLt | OpLte
		| OpAnd | OpOr | OpXor | OpShl | OpShr | OpUShr | OpMod | OpBoolAnd | OpBoolOr),_,_) ->
		true
	| TUnop(Not,_,_) ->
		true
	| TCall(e1,_) ->
		is_native_call e1
	| _ ->
		false

and is_native_call e1 = match e1.eexpr with
	| TField({eexpr = TConst TSuper},_) -> false
	| TField(_,FStatic({cl_path=[],"StringTools"},{cf_name=("fastCodeAt" | "unsafeCodeAt")})) -> false
	| TField(_,FEnum _) -> false
	| TField _ -> true
	| TConst TSuper -> false
	| _ -> true

(* A loop whose body has a [break] or [continue] in an expression compiled by
   EvalJit is compiled by EvalJit: these raise exceptions that only its loops
   catch *)
and loop_needs_fallback ctx body =
	let rec loop e =
		if is_native ctx e then match e.eexpr with
			| TWhile _ | TFunction _ -> false
			| _ -> check_expr loop e
		else
			has_free_break_continue e
	in
	loop body

(* Translation *)

let rec expr t return e =
	if not (is_native t.jit.ctx e) then fallback t return e
	else match e.eexpr with
	| TParenthesis e1 | TMeta(_,e1) | TCast(e1,None) ->
		expr t return e1
	| TBlock [] ->
		int raw_null
	| TBlock [e1] ->
		push_scope t.jit e.epos;
		let c = expr t return e1 in
		pop_scope t.jit;
		c
	| TBlock el ->
		push_scope t.jit e.epos;
		let rec loop el = match el with
			| [e] -> expr t return e
			| e :: el ->
				let c = expr t false e in
				Csequence(c,loop el)
			| [] -> die "" __LOC__
		in
		let c = loop el in
		pop_scope t.jit;
		c
	| TVar(var,eo) ->
		let varacc = declare_local t.jit var in
		let c = match eo with
			| None -> int raw_null
			| Some e -> expr t false e
		in
		begin match varacc with
			| Local slot -> seq_null (store (locals t) slot c)
			| Env slot -> seq_null (store (captures t) slot c)
		end
	| TConst TThis ->
		load (locals t) (get_slot t.jit 0 e.epos)
	| TConst ct ->
		value_const t (EvalJit.eval_const ct)
	| TLocal var ->
		if has_var_flag var VCaptured then load (captures t) (get_capture_slot t.jit var)
		else load (locals t) (get_slot t.jit var.v_id e.epos)
	| TIf(e1,e2,eo) ->
		let c1 = expr t false e1 in
		let c2 = expr t return e2 in
		let c3 = match eo with
			| None -> int raw_null
			| Some e -> expr t return e
		in
		ifte (is_true c1) c2 c3
	| TWhile(e1,e2,flag) ->
		let e1 = strip_parenthesis e1 in
		let lbl_break = Lambda.next_raise_count () in
		let lbl_continue = Lambda.next_raise_count () in
		let lbl_loop = Lambda.next_raise_count () in
		let c1 = expr t false e1 in
		t.loops <- (lbl_break,lbl_continue) :: t.loops;
		let c2 = expr t false e2 in
		t.loops <- List.tl t.loops;
		let body = Ccatch(Nonrecursive,[lbl_continue,[],int raw_null,dbg],c2) in
		let next = Csequence(body,Cexit(lbl_loop,[])) in
		let handler = match flag with
			| NormalWhile -> ifte (is_true c1) next (Cexit(lbl_break,[]))
			| DoWhile -> Csequence(body,ifte (is_true c1) (Cexit(lbl_loop,[])) (Cexit(lbl_break,[])))
		in
		Ccatch(Nonrecursive,[lbl_break,[],int raw_null,dbg],
			Ccatch(Recursive,[lbl_loop,[],handler,dbg],Cexit(lbl_loop,[])))
	| TBreak ->
		begin match t.loops with
			| (lbl,_) :: _ -> Cexit(lbl,[])
			| [] -> raise (Unsupported "break outside of a loop")
		end
	| TContinue ->
		begin match t.loops with
			| (_,lbl) :: _ -> Cexit(lbl,[])
			| [] -> raise (Unsupported "continue outside of a loop")
		end
	| TReturn None ->
		Cexit(t.return_label,[int raw_null])
	| TReturn (Some e1) ->
		Cexit(t.return_label,[expr t false e1])
	| TBinop((OpEq | OpNotEq) as op,e1,{eexpr = TConst TNull}) | TBinop((OpEq | OpNotEq) as op,{eexpr = TConst TNull},e1) ->
		let f = if op = OpEq then (fun v -> match vresolve v with VNull -> VTrue | _ -> VFalse)
			else (fun v -> match vresolve v with VNull -> VFalse | _ -> VTrue) in
		call t (Obj.repr f) [expr t false e1]
	| TBinop(OpAssign,({eexpr = TLocal var} as e1),e2) ->
		bind "v" (expr t false e2) (fun v ->
			let store =
				if has_var_flag var VCaptured then store (captures t) (get_capture_slot t.jit var) v
				else store (locals t) (get_slot t.jit var.v_id e1.epos) v
			in
			Csequence(store,v))
	| TBinop(OpBoolAnd,e1,e2) ->
		let c1 = expr t false e1 in
		let c2 = expr t false e2 in
		ifte (is_true c1) c2 (int raw_false)
	| TBinop(OpBoolOr,e1,e2) ->
		let c1 = expr t false e1 in
		let c2 = expr t false e2 in
		ifte (is_true c1) (int raw_true) c2
	| TBinop(op,e1,e2) ->
		let f = get_binop_fun op e.epos in
		let c1 = expr t false e1 in
		let c2 = expr t false e2 in
		bind "v1" c1 (fun v1 -> bind "v2" c2 (fun v2 -> call t (Obj.repr f) [v1;v2]))
	| TUnop(Not,_,e1) ->
		bind "v" (expr t false e1) (fun v ->
			ifte (eq v (int raw_null)) (int raw_true)
				(ifte (eq v (int raw_false)) (int raw_true) (int raw_false)))
	| TCall(e1,el) ->
		call_expr t e e1 el
	| _ ->
		die "" __LOC__

(* Calls, as in EvalJit and EvalEmitter: same checks, same order of
   evaluation, same positions recorded for the call stack *)
and call_expr t e e1 el =
	let ctx = t.jit.ctx in
	let p = e.epos in
	let check_stack = call t (Obj.repr check_stack_depth) [env t] in
	let set_pos = Csequence(store_int (env t) env_field_leave_pmin (Pos.min p),
		store_int (env t) env_field_leave_pmax (Pos.max p)) in
	let args el k =
		let cl = List.map (expr t false) el in
		bind_list "arg" cl k
	in
	let null_check ef =
		let c = expr t false ef in
		let p = ef.epos in
		bind "this" c (fun v ->
			ifte (eq v (int raw_null)) (call t (Obj.repr (fun () -> throw_string "Null Access" p)) [unit_value]) v)
	in
	let lazy_proto_field proto name =
		let i = get_proto_field_index proto name in
		AtomicLazy.from_fun (fun () -> match proto.pfields.(i) with VFunction (f,_) -> f | v -> cannot_call v p)
	in
	(* emit_proto_field_call *)
	let proto_field_call v this =
		let force = call t (Obj.repr (fun () -> AtomicLazy.force v)) [unit_value] in
		Csequence(check_stack,
			bind "f" force (fun f ->
				let k vl = Csequence(set_pos,Cmm_helpers.generic_apply Asttypes.Mutable f [make_list vl] dbg) in
				match this with
				| None -> args el k
				| Some ef -> bind "this" (null_check ef) (fun vthis -> args el (fun vl -> k (vthis :: vl)))))
	in
	match e1.eexpr with
	| TField(ef,fa) ->
		let name = hash (field_name fa) in
		let is_final c cf =
			has_class_flag c CFinal || (has_class_field_flag cf CfFinal) ||
			(not ctx.is_macro && not (Hashtbl.mem ctx.overrides (c.cl_path,cf.cf_name)))
		in
		let is_proper_method cf = match cf.cf_kind with
			| Method MethDynamic -> false
			| Method _ -> true
			| Var _ -> false
		in
		let instance_call c =
			let proto = get_instance_prototype ctx (path_hash c.cl_path) ef.epos in
			proto_field_call (lazy_proto_field proto name) (Some ef)
		in
		(* emit_method_call *)
		let default () =
			let lookup vthis =
				let proto = get_prototype vthis p in
				try proto_field_raise proto name with Not_found -> throw_string (Printf.sprintf "Field %s not found on prototype %s" (rev_hash name) (rev_hash proto.ppath)) p
			in
			Csequence(check_stack,
				bind "this" (null_check ef) (fun vthis ->
					bind "f" (call t (Obj.repr lookup) [vthis]) (fun vf ->
						args el (fun vl ->
							Csequence(set_pos,call t (Obj.repr call_value_on) [vthis;vf;make_list vl])))))
		in
		begin match fa with
			| FStatic({cl_path=path},cf) when is_proper_method cf ->
				let proto = get_static_prototype ctx (path_hash path) ef.epos in
				proto_field_call (lazy_proto_field proto name) None
			| FInstance(c,_,cf) when is_proper_method cf ->
				if not (is_final c cf) then
					default()
				else if not (has_class_flag c CInterface) then
					instance_call c
				else if not ctx.is_macro && c.cl_implements = [] && c.cl_super = None then begin match c.cl_descendants with
					| [c'] when not (has_class_flag c' CInterface) && is_final c' cf ->
						instance_call c'
					| _ ->
						default()
				end else
					default()
			| _ ->
				(* emit_field_call *)
				Csequence(check_stack,
					bind "this" (null_check ef) (fun vthis ->
						bind "f" (call t (Obj.repr (fun v -> field v name)) [vthis]) (fun vf ->
							Csequence(set_pos,
								args el (fun vl -> call t (Obj.repr call_value_on) [vthis;vf;make_list vl])))))
		end
	| _ ->
		(* emit_call *)
		Csequence(check_stack,
			bind "f" (expr t false e1) (fun v1 ->
				Csequence(set_pos,
					args el (fun vl -> call t (Obj.repr call_value) [v1;make_list vl]))))

(* Functions *)

let enabled ctx =
	not ctx.debug.support_debugger && ctx.debug.debug_socket = None
	&& Common.defined ((EvalContext.curapi ctx).MacroApi.get_com()) Define.EvalNative

let jit_timer ctx f =
	Timer.time ctx.timer_ctx [(if ctx.is_macro then "macro" else "interp");"native"] f ()

(* As [EvalJit.jit_tfunction], with a native [exec] *)
let compile_function ctx key_type key_field tf static pos =
	let consts = DynArray.create () in
	let result = ref None in
	let c = Jit.Jit_backend.compile (fun symbol ->
		let jit = EvalJitContext.create ctx in
		let env_var,env_vp = var "env" in
		let t = {
			jit;
			consts;
			consts_symbol = symbol "consts";
			env = env_var;
			return_label = Lambda.next_raise_count ();
			loops = [];
		} in
		push_scope jit pos;
		if not static then ignore(declare_local_this jit);
		let fl = List.map (fun (var,_) ->
			match declare_local jit var with
			| Env slot -> execute_set_capture slot
			| Local slot -> execute_set_local slot
		) tf.tf_args in
		let fl = if static then fl else (execute_set_local 0) :: fl in
		let e = List.fold_right (fun (v,cto) e -> match cto with
			| None -> e
			| Some ct -> concat (Texpr.set_default ((EvalContext.curapi ctx).MacroApi.get_com()).Common.basic v ct e.epos) e
		) tf.tf_args tf.tf_expr in
		let body = expr t true e in
		pop_scope jit;
		let rv,rvp = var "result" in
		let body = Ccatch(Nonrecursive,[t.return_label,[rvp,typ_val],Cvar rv,dbg],body) in
		let _,clos_vp = var "clos" in
		let fn = Cfunction {
			fun_name = symbol "exec";
			fun_args = [env_vp,typ_val;clos_vp,typ_val];
			fun_body = body;
			fun_codegen_options = [];
			fun_poll = Lambda.Default_poll;
			fun_dbg = dbg;
		} in
		let closure = Cdata [
			Cint (Cmm_helpers.black_closure_header 2);
			Cdefine_symbol (symbol "exec_closure");
			Csymbol_address (symbol "exec");
			Cint (Cmm_helpers.closure_info ~arity:1 ~startenv:2);
		] in
		let n = DynArray.length consts in
		let data = Cdata (
			[Cint (Cmm_helpers.black_block_header 0 (max n 1));
			 Cdefine_symbol (symbol "consts")]
			@ List.init (max n 1) (fun _ -> Cint 1n)
			@ [Calign 8;
			   Cdefine_symbol (symbol "gc_roots");
			   Csymbol_address (symbol "consts");
			   Cint 0n]
		) in
		result := Some (jit,fl);
		[data;fn;closure]
	) in
	let _,l = Jit.Jit_backend.load ~gc_roots:"gc_roots" c in
	let table = Jit.Jit_backend.value c l "consts" in
	DynArray.iteri (fun i v -> Obj.set_field table i v) consts;
	let exec : env -> value = Obj.obj (Jit.Jit_backend.value c l "exec_closure") in
	let jit,fl = Option.get !result in
	let hasret = jit.has_nonfinal_return in
	let eci = EvalJit.get_env_creation jit static (Pos.file tf.tf_expr.epos) (EKMethod(key_type,key_field)) in
	if hasret then create_function ctx eci exec fl else create_function_noret ctx eci exec fl

(* Number of nodes of [e] *)
let size e =
	let n = ref 0 in
	let rec loop e = incr n; Type.iter loop e in
	loop e;
	!n

(* Above this size (in nodes of the typed tree), a function is compiled to
   closures: the time of the backend grows faster than the size of the code,
   and such functions (initializations, generated code) usually run once *)
let max_size = try int_of_string (Sys.getenv "HAXE_NATIVE_MAX_SIZE") with _ -> 2000

(* Why functions are not compiled natively, printed at exit with HAXE_JIT_STATS *)
let fallbacks = Hashtbl.create 8
let fallback_function reason =
	if Jit.Jit_backend.stats_enabled then begin
		Mutex.protect Jit.Jit_backend.lock (fun () ->
			Hashtbl.replace fallbacks reason (1 + try Hashtbl.find fallbacks reason with Not_found -> 0))
	end

let () =
	if Jit.Jit_backend.stats_enabled then
		at_exit (fun () -> Hashtbl.iter (fun r n -> Printf.eprintf "native fallback %-30s %d\n" r n) fallbacks)

let jit_tfunction ctx key_type key_field tf static pos =
	let closures reason =
		fallback_function reason;
		EvalJit.jit_tfunction ctx key_type key_field tf static pos
	in
	if not (enabled ctx) then EvalJit.jit_tfunction ctx key_type key_field tf static pos
	else if size tf.tf_expr > max_size then closures "size"
	else
		try
			jit_timer ctx (fun () -> compile_function ctx key_type key_field tf static pos)
		with
		| Unsupported s -> closures ("unsupported: " ^ s)
		| Jit.Jit_backend.Busy -> closures "busy"
		| Jit.X86_encode.Unsupported s -> closures ("encoder: " ^ s)
		| Jit.Jit_link.Link_error s -> closures ("link: " ^ s)
