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
open EvalEncode
open EvalDecode
open EvalExceptions
open EvalJitContext
open Ocaml_backend
open Cmm

exception Unsupported of string

module IntSet = Set.Make(Int)

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
	(* Locals of type Int kept unboxed in Cmm variables (see [int_locals]) *)
	int_vars : IntSet.t;
	int_cvars : (int,Backend_var.t) Hashtbl.t;
}

let const t (v : Obj.t) =
	let i = DynArray.length t.consts in
	DynArray.add t.consts v;
	load (Cconst_symbol(t.consts_symbol,dbg)) i

let value_const t (v : value) = match v with
	| VNull | VTrue | VFalse -> int (raw_immediate v)
	| _ -> const t (Obj.repr v)

(* The kind of the expression being translated, for the statistics of the
   calls generated (HAXE_JIT_STATS) *)
let current_kind = ref ""
let call_sites = Hashtbl.create 0

(* Call of the OCaml function [f] *)
let call t (f : Obj.t) args =
	if Jit.Jit_backend.stats_enabled then
		Hashtbl.replace call_sites !current_kind (1 + try Hashtbl.find call_sites !current_kind with Not_found -> 0);
	Cmm_helpers.generic_apply Asttypes.Mutable (const t f) args dbg

let env t = Cvar t.env
let locals t = load (env t) env_field_locals
let captures t = load (env t) env_field_captures

let unit_value = int 1

(* The value of [e], then [vnull] *)
let seq_null e = Csequence(e,int raw_null)

(* Expressions compiled by EvalJit *)

let fallback_counter = ref (fun (_ : texpr) -> ())

let fallback t return e =
	!fallback_counter e;
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

(* Tags of the constructors of [value] *)
let tag_of (v : value) = Obj.tag (Obj.repr v)
let tag_instance = tag_of (VInstance (Obj.magic 0))

(* The value of an expression that has no side effect and cannot fail: it can
   be evaluated later than in EvalJit without any difference *)
let rec is_simple e = match e.eexpr with
	| TLocal _ | TConst _ -> true
	| TParenthesis e | TMeta(_,e) | TCast(e,None) -> is_simple e
	| _ -> false

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
	| TBinop(OpAssign,e1,_) ->
		is_native_assign e1
	| TBinop(OpAssignOp _,e1,e2) ->
		is_native_assign_op e1 e2
	| TBinop((OpAdd | OpMult | OpDiv | OpSub | OpEq | OpNotEq | OpGt | OpGte | OpLt | OpLte
		| OpAnd | OpOr | OpXor | OpShl | OpShr | OpUShr | OpMod | OpBoolAnd | OpBoolOr),_,_) ->
		true
	| TUnop((Not | Neg | NegBits),_,_) | TUnop(Spread,Prefix,_) ->
		true
	| TUnop((Increment | Decrement),_,e1) ->
		is_native_assign_op e1 (mk (TConst (TInt Int32.one)) t_dynamic null_pos)
	| TField _ | TArray _ | TEnumParameter _ | TEnumIndex _ | TTypeExpr _
	| TArrayDecl _ | TObjectDecl _ | TThrow _ | TCast(_,Some _) ->
		true
	| TNew({cl_path=["eval"],"Vector"},_,_) ->
		false
	| TNew _ ->
		true
	| TCall(e1,_) ->
		is_native_call e1
	| _ ->
		false

and is_native_assign e1 = match e1.eexpr with
	| TLocal _ | TField _ | TArray _ -> true
	| _ -> false

and is_native_assign_op e1 e2 = match e1.eexpr with
	| TLocal _ -> true
	| TField(_,(FStatic _ | FInstance _)) -> true
	| TArray(_,ea2) -> is_simple ea2 && is_simple e2
	| _ -> false

and is_native_call e1 = match e1.eexpr with
	| TConst TSuper -> true
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

(* Specialization of Int

   A value of type Int is a [VInt32] in most cases, but not always (a Dynamic
   assigned to an Int, a field written by another tier): only the values whose
   representation is known are kept unboxed, as native integers holding the
   32-bit value sign-extended. They are: Int constants, the results of the
   operations of eval that give a [VInt32] for two [VInt32] (+ - * & | ^ <<
   >> >>>, negations, increments), and the locals all of whose assignments
   are such values ([int_locals]). They are boxed where they meet the rest of
   the code. *)

let tag_int32 = tag_of (VInt32 0)

let is_int_type t = match follow t with
	| TAbstract({a_path=[],"Int"},_) -> true
	| _ -> false

let rec skip e = match e.eexpr with
	| TParenthesis e1 | TMeta(_,e1) | TCast(e1,None) -> skip e1
	| _ -> e

let is_int_binop = function
	| OpAdd | OpSub | OpMult | OpAnd | OpOr | OpXor | OpShl | OpShr | OpUShr -> true
	| _ -> false

let is_comparison = function
	| OpEq | OpNotEq | OpGt | OpGte | OpLt | OpLte -> true
	| _ -> false

(* Whether the value of [e] is known to be a [VInt32], [vars] being the
   unboxed locals *)
let rec is_known_int vars e = match e.eexpr with
	| TConst (TInt _) -> true
	| TLocal v -> IntSet.mem v.v_id vars
	| TParenthesis e1 | TMeta(_,e1) | TCast(e1,None) -> is_known_int vars e1
	| TBinop(op,e1,e2) when is_int_binop op -> is_known_int vars e1 && is_known_int vars e2
	| TBinop(OpAssign,{eexpr = TLocal v},e2) -> IntSet.mem v.v_id vars && is_known_int vars e2
	| TBinop(OpAssignOp op,{eexpr = TLocal v},e2) when is_int_binop op -> IntSet.mem v.v_id vars && is_known_int vars e2
	| TUnop((Neg | NegBits),_,e1) -> is_known_int vars e1
	| TUnop((Increment | Decrement),_,e1) ->
		begin match (skip e1).eexpr with
			| TLocal v -> IntSet.mem v.v_id vars
			| _ -> false
		end
	| _ -> false

(* The locals of [body] that can be kept unboxed: declared with a value in a
   block, of type Int, not captured, never read by an expression compiled by
   EvalJit (which reads [env.env_locals]), and only assigned known integers *)
let int_locals ctx body =
	let candidates = ref IntSet.empty in
	let excluded = ref IntSet.empty in
	let assigned = Hashtbl.create 0 in
	let exclude_all e =
		let rec loop e = match e.eexpr with
			| TLocal v | TVar(v,_) -> excluded := IntSet.add v.v_id !excluded; Type.iter loop e
			| _ -> Type.iter loop e
		in
		loop e
	in
	let rec walk e =
		if not (is_native ctx e) then exclude_all e
		else match e.eexpr with
		| TBlock el ->
			List.iter (fun e -> match e.eexpr with
				| TVar(v,Some init) when not (has_var_flag v VCaptured) && is_int_type v.v_type ->
					candidates := IntSet.add v.v_id !candidates;
					Hashtbl.add assigned v.v_id init;
					walk init
				| _ ->
					walk e
			) el
		| TVar(v,eo) ->
			excluded := IntSet.add v.v_id !excluded;
			(match eo with Some e -> walk e | None -> ())
		| TBinop(OpAssign,{eexpr = TLocal v},e2) ->
			Hashtbl.add assigned v.v_id e2;
			walk e2
		| TBinop(OpAssignOp op,{eexpr = TLocal v},e2) ->
			if is_int_binop op then Hashtbl.add assigned v.v_id e2
			else excluded := IntSet.add v.v_id !excluded;
			walk e2
		| TBinop(OpAssignOp _,{eexpr = TField(_,FInstance(c,_,_))},e2) when has_class_flag c CInterface ->
			(* compiled by EvalJit (see assign_op) *)
			exclude_all e;
		| TBinop(OpAssignOp _,{eexpr = TField(_,fa)},e2) when (match fa with FStatic _ | FInstance _ -> false | _ -> true) ->
			exclude_all e;
		| _ ->
			Type.iter walk e
	in
	walk body;
	let vars = ref (IntSet.diff !candidates !excluded) in
	let changed = ref true in
	while !changed do
		changed := false;
		IntSet.iter (fun id ->
			if List.exists (fun e -> not (is_known_int !vars e)) (Hashtbl.find_all assigned id) then begin
				vars := IntSet.remove id !vars;
				changed := true
			end
		) !vars
	done;
	!vars

(* Native integers *)

let sign_extend c = Cmm_helpers.sign_extend_32 dbg c

let int_op op a b = match op with
	| OpAdd -> sign_extend (Cop(Caddi,[a;b],dbg))
	| OpSub -> sign_extend (Cop(Csubi,[a;b],dbg))
	| OpMult -> sign_extend (Cop(Cmuli,[a;b],dbg))
	| OpAnd -> Cop(Cand,[a;b],dbg)
	| OpOr -> Cop(Cor,[a;b],dbg)
	| OpXor -> Cop(Cxor,[a;b],dbg)
	| OpShl -> sign_extend (Cop(Clsl,[a;Cop(Cand,[b;int 31],dbg)],dbg))
	| OpShr -> Cop(Casr,[a;Cop(Cand,[b;int 31],dbg)],dbg)
	| OpUShr -> sign_extend (Cop(Clsr,[Cop(Cand,[a;Cconst_natint(0xFFFFFFFFn,dbg)],dbg);Cop(Cand,[b;int 31],dbg)],dbg))
	| _ -> die "" __LOC__

let int_cmp = function
	| OpEq -> Ceq | OpNotEq -> Cne | OpGt -> Cgt | OpGte -> Cge | OpLt -> Clt | OpLte -> Cle
	| _ -> die "" __LOC__

(* The [VInt32] of a native integer *)
let box_int c =
	Cmm_helpers.make_alloc dbg tag_int32 [Cmm_helpers.tag_int c dbg]

(* The native integer of a [VInt32] *)
let unbox_int v = Cmm_helpers.untag_int (load v 0) dbg

(* [k] applied to a variable holding the value of [e], always bound: [e] may
   be a mutable variable *)
let bind_copy name e k =
	let v,vp = var name in
	Clet(vp,e,k (Cvar v))

(* [fast v] if [v] is an instance, [slow v] otherwise *)
let if_instance v fast slow =
	let lbl = Lambda.next_raise_count () in
	Ccatch(Nonrecursive,[lbl,[],slow,dbg],
		ifte (op Cand [v;int 1]) (Cexit(lbl,[]))
			(ifte (eq (Cmm_helpers.get_tag v dbg) (int tag_instance)) fast (Cexit(lbl,[]))))

let dummy_env : env = Obj.magic 0

(* Whether the value [v] is a block of tag [tag] *)
let op_tag_is v tag =
	ifte (op Cand [v;int 1]) (int 0) (eq (Cmm_helpers.get_tag v dbg) (int tag))

(* The fields of the instance [v] (the [ifields] of its [vinstance]) *)
let instance_fields v = load (load v 0) 0

let rec expr t return e =
	if Jit.Jit_backend.stats_enabled then begin
		let saved = !current_kind in
		current_kind := (match e.eexpr with
			| TField(_,fa) -> "TField " ^ (match fa with FInstance _ -> "instance" | FStatic _ -> "static" | FAnon _ -> "anon" | FDynamic _ -> "dynamic" | FClosure _ -> "closure" | FEnum _ -> "enum")
			| TBinop(op,_,_) -> "TBinop " ^ s_binop op
			| TUnop(op,_,_) -> "TUnop " ^ s_unop op
			| TCall({eexpr = TField(_,fa)},_) -> "TCall " ^ (match fa with FInstance _ -> "instance" | FStatic _ -> "static" | FAnon _ -> "anon" | FDynamic _ -> "dynamic" | FClosure _ -> "closure" | FEnum _ -> "enum")
			| TCall _ -> "TCall other"
			| _ -> s_expr_kind e);
		let r = expr_kind t return e in
		current_kind := saved;
		r
	end else
		expr_kind t return e

and expr_kind t return e =
	if not (is_native t.jit.ctx e) then fallback t return e
	else if is_known_int t.int_vars e && (match (skip e).eexpr with TConst _ -> false | _ -> true) then
		box_int (int_expr t e)
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
			| {eexpr = TVar(v,Some init)} :: el when IntSet.mem v.v_id t.int_vars ->
				(* an unboxed local: a Cmm variable for the rest of the block *)
				ignore (declare_local t.jit v);
				let c = int_expr t init in
				let cv,cvp = var v.v_name in
				Hashtbl.replace t.int_cvars v.v_id cv;
				Clet_mut(cvp,typ_int,c,if el = [] then int raw_null else loop el)
			| [e] -> expr t return e
			| e :: el ->
				let c = statement t e in
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
	| TTypeExpr mt ->
		let key = path_hash (t_infos mt).mt_path in
		value_const t (get_static_prototype_as_value t.jit.ctx key e.epos)
	| TIf(e1,e2,eo) ->
		let c1 = cond t e1 in
		let c2 = expr t return e2 in
		let c3 = match eo with
			| None -> int raw_null
			| Some e -> expr t return e
		in
		ifte c1 c2 c3
	| TWhile(e1,e2,flag) ->
		let e1 = strip_parenthesis e1 in
		let lbl_break = Lambda.next_raise_count () in
		let lbl_continue = Lambda.next_raise_count () in
		let lbl_loop = Lambda.next_raise_count () in
		let c1 = cond t e1 in
		t.loops <- (lbl_break,lbl_continue) :: t.loops;
		let c2 = expr t false e2 in
		t.loops <- List.tl t.loops;
		let body = Ccatch(Nonrecursive,[lbl_continue,[],int raw_null,dbg],c2) in
		let next = Csequence(body,Cexit(lbl_loop,[])) in
		let handler = match flag with
			| NormalWhile -> ifte c1 next (Cexit(lbl_break,[]))
			| DoWhile -> Csequence(body,ifte c1 (Cexit(lbl_loop,[])) (Cexit(lbl_break,[])))
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
	| TThrow e1 ->
		let p = e.epos in
		call t (Obj.repr (fun v -> emit_throw (fun _ -> v) p dummy_env)) [expr t false e1]
	| TCast(e1,Some mt) ->
		let c = expr t false e1 in
		let key = hash (EvalJit.rope_path (type_of_module_type mt)) in
		let p = e.epos in
		call t (Obj.repr (fun v -> emit_safe_cast (fun _ -> v) key p dummy_env)) [c]
	(* objects and arrays *)
	| TObjectDecl fl ->
		let fl = List.map (fun ((s,_,_),e) -> hash s,expr t false e) fl in
		let proto,_ = t.jit.ctx.get_object_prototype t.jit.ctx fl in
		let indices = Array.of_list (List.map (fun (s,_) -> get_instance_field_index proto s e.epos) fl) in
		let create vl =
			let a = Array.make (Array.length indices) vnull in
			List.iteri (fun k v -> a.(indices.(k)) <- v) vl;
			vobject { ofields = a; oproto = OProto proto }
		in
		bind_list "field" (List.map snd fl) (fun vl -> call t (Obj.repr create) [make_list vl])
	| TArrayDecl el ->
		let cl = List.map (expr t false) el in
		let create vl = encode_array_instance (EvalArray.create (Array.of_list vl)) in
		bind_list "elt" cl (fun vl -> call t (Obj.repr create) [make_list vl])
	| TNew({cl_path=[],"Array"},_,_) ->
		call t (Obj.repr (fun () -> emit_new_array dummy_env)) [unit_value]
	| TNew(c,_,el) ->
		new_expr t e c el
	(* reads *)
	| TField(e1,fa) ->
		field_expr t e e1 fa
	| TArray(e1,e2) ->
		let vector = match follow e1.etype with TInst({cl_path=(["eval"],"Vector")},_) -> true | _ -> false in
		let p1 = e1.epos and p2 = e2.epos in
		let c1 = expr t false e1 in
		let c2 = expr t false e2 in
		(* as in emit_array_read: the array is checked before the index is evaluated *)
		let check1 = if vector then (fun v -> ignore (as_vector p1 v); v) else (fun v -> ignore (as_array p1 v); v) in
		let read = if vector then (fun v1 v2 -> emit_vector_read (fun _ -> v1) p1 (fun _ -> v2) p2 dummy_env)
			else (fun v1 v2 -> emit_array_read (fun _ -> v1) p1 (fun _ -> v2) p2 dummy_env) in
		bind "a" c1 (fun v1 ->
			let k v1 = bind "i" c2 (fun v2 -> call t (Obj.repr read) [v1;v2]) in
			if is_simple e2 then k v1 else bind "a" (call t (Obj.repr check1) [v1]) k)
	| TEnumParameter(e1,_,i) ->
		call t (Obj.repr (fun v -> emit_enum_parameter_read (fun _ -> v) i dummy_env)) [expr t false e1]
	| TEnumIndex e1 ->
		let p = e1.epos in
		call t (Obj.repr (fun v -> emit_enum_index (fun _ -> v) p dummy_env)) [expr t false e1]
	(* operators *)
	| TBinop((OpEq | OpNotEq) as op,e1,{eexpr = TConst TNull}) | TBinop((OpEq | OpNotEq) as op,{eexpr = TConst TNull},e1) ->
		let f = if op = OpEq then (fun v -> match vresolve v with VNull -> VTrue | _ -> VFalse)
			else (fun v -> match vresolve v with VNull -> VFalse | _ -> VTrue) in
		call t (Obj.repr f) [expr t false e1]
	| TBinop(OpAssign,e1,e2) ->
		assign t e1 e2
	| TBinop(OpAssignOp op,e1,e2) ->
		assign_op t (get_binop_fun op e.epos) e1 e2 true
	| TBinop(OpBoolAnd,e1,e2) ->
		let c1 = expr t false e1 in
		let c2 = expr t false e2 in
		ifte (is_true c1) c2 (int raw_false)
	| TBinop(OpBoolOr,e1,e2) ->
		let c1 = expr t false e1 in
		let c2 = expr t false e2 in
		ifte (is_true c1) (int raw_true) c2
	| TBinop(op,e1,e2) when is_comparison op && (is_known_int t.int_vars e1 || is_known_int t.int_vars e2) ->
		ifte (cond t e) (int raw_true) (int raw_false)
	| TBinop(op,e1,e2) ->
		let f = get_binop_fun op e.epos in
		let c1 = expr t false e1 in
		let c2 = expr t false e2 in
		bind "v1" c1 (fun v1 -> bind "v2" c2 (fun v2 -> call t (Obj.repr f) [v1;v2]))
	| TUnop(Not,_,e1) ->
		bind "v" (expr t false e1) (fun v ->
			ifte (eq v (int raw_null)) (int raw_true)
				(ifte (eq v (int raw_false)) (int raw_true) (int raw_false)))
	| TUnop(Neg,_,e1) ->
		let p = e.epos in
		call t (Obj.repr (fun v -> emit_neg (fun _ -> v) p dummy_env)) [expr t false e1]
	| TUnop(NegBits,_,e1) ->
		let p = e.epos in
		call t (Obj.repr (fun v -> op_sub p (vint32 Int32.minus_one) v)) [expr t false e1]
	| TUnop(Spread,_,e1) ->
		expr t false e1
	| TUnop(Increment,flag,e1) ->
		begin match Texpr.skip e1 with
		| {eexpr = TLocal v} when not (has_var_flag v VCaptured) ->
			(* emit_local_incr_prefix/postfix *)
			let slot = get_slot t.jit v.v_id e1.epos in
			let p = e1.epos in
			bind "v0" (load (locals t) slot) (fun v0 ->
				bind "v" (call t (Obj.repr (fun v -> do_incr v p)) [v0]) (fun v ->
					Csequence(store (locals t) slot v,if flag = Prefix then v else v0)))
		| _ ->
			assign_op t (get_binop_fun OpAdd e.epos) e1 EvalJit.eone (flag = Prefix)
		end
	| TUnop(Decrement,flag,e1) ->
		assign_op t (get_binop_fun OpSub e.epos) e1 EvalJit.eone (flag = Prefix)
	| TCall(e1,el) ->
		call_expr t e e1 el
	| _ ->
		die "" __LOC__

(* A statement: an expression whose value is not used *)
and statement t e =
	if is_native t.jit.ctx e && is_known_int t.int_vars e then Csequence(int_expr t e,int raw_null)
	else expr t false e

(* The native integer of [e], a known integer (see [is_known_int]) *)
and int_expr t e = match e.eexpr with
	| TConst (TInt i) ->
		Cconst_natint(Nativeint.of_int32 i,dbg)
	| TLocal v ->
		Cvar (Hashtbl.find t.int_cvars v.v_id)
	| TParenthesis e1 | TMeta(_,e1) | TCast(e1,None) ->
		int_expr t e1
	| TBinop(op,e1,e2) when is_int_binop op ->
		let c1 = int_expr t e1 in
		let c2 = int_expr t e2 in
		bind_copy "a" c1 (fun a -> bind "b" c2 (fun b -> int_op op a b))
	| TBinop(OpAssign,{eexpr = TLocal v},e2) ->
		let cv = Hashtbl.find t.int_cvars v.v_id in
		bind "v" (int_expr t e2) (fun x -> Csequence(Cassign(cv,x),x))
	| TBinop(OpAssignOp op,{eexpr = TLocal v},e2) ->
		(* emit_local_read_write: the local is read before [e2] is evaluated *)
		let cv = Hashtbl.find t.int_cvars v.v_id in
		bind_copy "v1" (Cvar cv) (fun v1 -> bind "v2" (int_expr t e2) (fun v2 ->
			bind "v" (int_op op v1 v2) (fun x -> Csequence(Cassign(cv,x),x))))
	| TUnop(Neg,_,e1) ->
		sign_extend (Cop(Csubi,[int 0;int_expr t e1],dbg))
	| TUnop(NegBits,_,e1) ->
		sign_extend (Cop(Csubi,[int (-1);int_expr t e1],dbg))
	| TUnop((Increment | Decrement) as op,flag,e1) ->
		let v = match (skip e1).eexpr with TLocal v -> v | _ -> die "" __LOC__ in
		let cv = Hashtbl.find t.int_cvars v.v_id in
		bind_copy "v0" (Cvar cv) (fun v0 ->
			bind "v" (sign_extend (Cop((if op = Increment then Caddi else Csubi),[v0;int 1],dbg))) (fun x ->
				Csequence(Cassign(cv,x),if flag = Prefix then x else v0)))
	| _ ->
		die "" __LOC__

(* [e] as a condition: a Cmm integer, non-zero if the value of [e] is [true]
   (as EvalMisc.is_true) *)
and cond t e = match e.eexpr with
	| TParenthesis e1 | TMeta(_,e1) | TCast(e1,None) ->
		cond t e1
	| TConst (TBool b) ->
		int (if b then 1 else 0)
	| TBinop(op,e1,e2) when is_comparison op && is_native t.jit.ctx e ->
		let k1 = is_known_int t.int_vars e1 and k2 = is_known_int t.int_vars e2 in
		let p = e.epos in
		let cmp = int_cmp op in
		(* a known integer against any value: compared as integers if it is a
		   [VInt32], by the operator of eval otherwise *)
		let mixed a v ~int_first =
			let generic = is_true (call t (Obj.repr (get_binop_fun op p))
				(if int_first then [box_int a;v] else [v;box_int a])) in
			let lbl = Lambda.next_raise_count () in
			Ccatch(Nonrecursive,[lbl,[],generic,dbg],
				ifte (op_tag_is v tag_int32)
					(bind "b" (unbox_int v) (fun b -> Cop(Ccmpi cmp,(if int_first then [a;b] else [b;a]),dbg)))
					(Cexit(lbl,[])))
		in
		if k1 && k2 then begin
			let c1 = int_expr t e1 in
			let c2 = int_expr t e2 in
			bind_copy "a" c1 (fun a -> bind "b" c2 (fun b -> Cop(Ccmpi cmp,[a;b],dbg)))
		end else if k1 then begin
			let c1 = int_expr t e1 in
			let c2 = expr t false e2 in
			bind_copy "a" c1 (fun a -> bind "v" c2 (fun v -> mixed a v ~int_first:true))
		end else if k2 then begin
			let c1 = expr t false e1 in
			let c2 = int_expr t e2 in
			bind "v" c1 (fun v -> bind_copy "b" c2 (fun b -> mixed b v ~int_first:false))
		end else
			is_true (expr t false e)
	| TBinop(OpBoolAnd,e1,e2) when is_native t.jit.ctx e ->
		let c1 = cond t e1 in
		let c2 = cond t e2 in
		ifte c1 c2 (int 0)
	| TBinop(OpBoolOr,e1,e2) when is_native t.jit.ctx e ->
		let c1 = cond t e1 in
		let c2 = cond t e2 in
		ifte c1 (int 1) c2
	| _ ->
		is_true (expr t false e)

(* Reads of fields, as in EvalJit *)
and field_expr t e e1 fa =
	let ctx = t.jit.ctx in
	let name = hash (field_name fa) in
	let p1 = e1.epos in
	let on v f = call t (Obj.repr f) [v] in
	match fa with
	| FInstance({cl_path=([],"Array")},_,{cf_name="length"}) ->
		on (expr t false e1) (fun v -> emit_array_length_read (fun _ -> v) p1 dummy_env)
	| FInstance({cl_path=(["eval"],"Vector")},_,{cf_name="length"}) ->
		on (expr t false e1) (fun v -> emit_vector_length_read (fun _ -> v) p1 dummy_env)
	| FInstance({cl_path=(["haxe";"io"],"Bytes")},_,{cf_name="length"}) ->
		on (expr t false e1) (fun v -> emit_bytes_length_read (fun _ -> v) p1 dummy_env)
	| FStatic({cl_path=path},cf) ->
		let proto = get_static_prototype ctx (path_hash path) e1.epos in
		let i = get_proto_field_index proto name in
		let read = match cf.cf_kind with
			| Method (MethNormal | MethInline | MethMacro) -> (fun () -> get_static proto i)
			| _ -> (fun () -> EvalJit.static_state proto i (fun _ -> get_static proto i) dummy_env)
		in
		call t (Obj.repr read) [unit_value]
	| FEnum({e_path=path},_)
	| FInstance({cl_path=path},_,{cf_kind = Method (MethNormal | MethInline)}) ->
		let proto = get_static_prototype ctx (path_hash path) e1.epos in
		let i = get_proto_field_index proto name in
		call t (Obj.repr (fun () -> get_static proto i)) [unit_value]
	| FInstance(c,_,_) when not (has_class_flag c CInterface) ->
		let proto = get_instance_prototype ctx (path_hash c.cl_path) e1.epos in
		let i = get_instance_field_index proto name e1.epos in
		begin match e1.eexpr with
			| TConst TThis ->
				(* emit_this_field_read *)
				bind "this" (load (locals t) (get_slot t.jit 0 e.epos)) (fun v ->
					if_instance v (load (instance_fields v) i)
						(on v (fun v -> match v with VInstance vi -> vi.ifields.(i) | v -> unexpected_value v "instance")))
			| _ ->
				(* emit_instance_field_read *)
				bind "obj" (expr t false e1) (fun v ->
					if_instance v (load (instance_fields v) i)
						(on v (fun v -> emit_instance_field_read (fun _ -> v) p1 i dummy_env)))
		end
	| FAnon _ ->
		begin match follow e1.etype with
			| TAnon an ->
				let l = PMap.foldi (fun k _ acc -> (hash k,()) :: acc) an.a_fields [] in
				let proto,_ = ctx.get_object_prototype ctx l in
				let i = get_instance_field_index proto name e1.epos in
				on (expr t false e1) (fun v -> emit_anon_field_read (fun _ -> v) proto i name p1 dummy_env)
			| _ ->
				on (expr t false e1) (fun v -> emit_field_read (fun _ -> v) name p1 dummy_env)
		end
	| FClosure _ | FDynamic _ ->
		on (expr t false e1) (fun v -> dynamic_field v name)
	| _ ->
		on (expr t false e1) (fun v -> emit_field_read (fun _ -> v) name p1 dummy_env)

(* Assignments, as in EvalJit.op_assign *)
and assign t e1 e2 =
	let ctx = t.jit.ctx in
	match e1.eexpr with
	| TLocal var ->
		bind "v" (expr t false e2) (fun v ->
			let store =
				if has_var_flag var VCaptured then store (captures t) (get_capture_slot t.jit var) v
				else store (locals t) (get_slot t.jit var.v_id e1.epos) v
			in
			Csequence(store,v))
	| TField(ef,fa) ->
		let name = hash (field_name fa) in
		let p1 = ef.epos in
		begin match fa with
			| FInstance({cl_path=(["haxe";"io"],"Bytes")},_,{cf_name="length"}) ->
				let c1 = expr t false ef in
				let c2 = expr t false e2 in
				bind "obj" c1 (fun v1 -> bind "v" c2 (fun v2 ->
					call t (Obj.repr (fun v1 v2 -> emit_bytes_length_write (fun _ -> v1) (fun _ -> v2) dummy_env)) [v1;v2]))
			| FStatic({cl_path=path},_) | FEnum({e_path=path},_) ->
				(* the object (a type) is not evaluated *)
				let proto = get_static_prototype ctx (path_hash path) ef.epos in
				let i = get_proto_field_index proto name in
				let c2 = expr t false e2 in
				let write v = EvalJit.static_state proto i (fun _ -> set_static proto i v; v) dummy_env in
				call t (Obj.repr write) [c2]
			| FInstance(c,_,_) when not (has_class_flag c CInterface) ->
				(* emit_instance_field_write: the object is checked before the value is evaluated *)
				let proto = get_instance_prototype ctx (path_hash c.cl_path) ef.epos in
				let i = get_instance_field_index proto name ef.epos in
				let c1 = expr t false ef in
				let c2 = expr t false e2 in
				bind "obj" c1 (fun v1 ->
					if_instance v1
						(bind "v" c2 (fun v2 -> Csequence(store (instance_fields v1) i v2,v2)))
						(call t (Obj.repr (fun v -> unexpected_value_p v "instance" p1)) [v1]))
			| FAnon _ when (match follow ef.etype with TAnon _ -> true | _ -> false) ->
				let an = match follow ef.etype with TAnon an -> an | _ -> die "" __LOC__ in
				let l = PMap.foldi (fun k _ acc -> (hash k,()) :: acc) an.a_fields [] in
				let proto,_ = ctx.get_object_prototype ctx l in
				let i = get_instance_field_index proto name ef.epos in
				let c1 = expr t false ef in
				let c2 = expr t false e2 in
				bind "obj" c1 (fun v1 -> bind "v" c2 (fun v2 ->
					call t (Obj.repr (fun v1 v2 -> emit_anon_field_write (fun _ -> v1) p1 proto i name (fun _ -> v2) dummy_env)) [v1;v2]))
			| _ ->
				let p = e1.epos in
				let c1 = expr t false ef in
				let c2 = expr t false e2 in
				bind "obj" c1 (fun v1 -> bind "v" c2 (fun v2 ->
					call t (Obj.repr (fun v1 v2 -> emit_field_write (fun _ -> v1) p name (fun _ -> v2) dummy_env)) [v1;v2]))
		end
	| TArray(ea1,ea2) ->
		let vector = match follow ea1.etype with TInst({cl_path=(["eval"],"Vector")},_) -> true | _ -> false in
		let p1 = ea1.epos and p2 = ea2.epos in
		let c1 = expr t false ea1 in
		let c2 = expr t false ea2 in
		let c3 = expr t false e2 in
		let check1 = if vector then (fun v -> ignore (as_vector p1 v); v) else (fun v -> ignore (as_array p1 v); v) in
		let check2 v = ignore (as_int p2 v); v in
		let write = if vector then (fun v1 v2 v3 -> emit_vector_write (fun _ -> v1) p1 (fun _ -> v2) p2 (fun _ -> v3) p2 dummy_env)
			else (fun v1 v2 v3 -> emit_array_write (fun _ -> v1) p1 (fun _ -> v2) p2 (fun _ -> v3) p2 dummy_env) in
		(* as in emit_array_write: the array, then the index are checked before
		   the next expression is evaluated *)
		bind "a" c1 (fun v1 ->
			let k3 v1 v2 = bind "v" c3 (fun v3 -> call t (Obj.repr write) [v1;v2;v3]) in
			let k2 v1 = bind "i" c2 (fun v2 ->
				if is_simple e2 then k3 v1 v2 else bind "i" (call t (Obj.repr check2) [v2]) (k3 v1)) in
			if is_simple ea2 && is_simple e2 then k2 v1 else bind "a" (call t (Obj.repr check1) [v1]) k2)
	| _ ->
		die "" __LOC__

(* Operations with assignment ([+=], [++]...), as in EvalJit.op_assign_op *)
and assign_op t fop e1 e2 prefix =
	let ctx = t.jit.ctx in
	let result v1 v = if prefix then v else v1 in
	match e1.eexpr with
	| TLocal var ->
		let slot,read,write =
			if has_var_flag var VCaptured then let s = get_capture_slot t.jit var in s,load (captures t) s,store (captures t) s
			else let s = get_slot t.jit var.v_id e1.epos in s,load (locals t) s,store (locals t) s
		in
		ignore slot;
		let c2 = expr t false e2 in
		bind "v1" read (fun v1 -> bind "v2" c2 (fun v2 ->
			bind "v" (call t (Obj.repr fop) [v1;v2]) (fun v -> Csequence(write v,result v1 v))))
	| TField(ef,FStatic({cl_path=path},_)) ->
		let name = hash (field_name (match e1.eexpr with TField(_,fa) -> fa | _ -> die "" __LOC__)) in
		let proto = get_static_prototype ctx (path_hash path) ef.epos in
		let i = get_proto_field_index proto name in
		let c2 = expr t false e2 in
		(* static_state, then emit_proto_field_read_write *)
		let read () = EvalJit.static_state proto i (fun _ -> get_static proto i) dummy_env in
		let write v = set_static proto i v in
		bind "v1" (call t (Obj.repr read) [unit_value]) (fun v1 -> bind "v2" c2 (fun v2 ->
			bind "v" (call t (Obj.repr fop) [v1;v2]) (fun v ->
				Csequence(call t (Obj.repr write) [v],result v1 v))))
	| TField(ef,(FInstance(c,_,_) as fa)) when not (has_class_flag c CInterface) ->
		let name = hash (field_name fa) in
		let proto = get_instance_prototype ctx (path_hash c.cl_path) ef.epos in
		let i = get_instance_field_index proto name ef.epos in
		let p1 = ef.epos in
		let c1 = expr t false ef in
		let c2 = expr t false e2 in
		bind "obj" c1 (fun vo ->
			if_instance vo
				(bind "v1" (load (instance_fields vo) i) (fun v1 -> bind "v2" c2 (fun v2 ->
					bind "v" (call t (Obj.repr fop) [v1;v2]) (fun v ->
						Csequence(store (instance_fields vo) i v,result v1 v)))))
				(call t (Obj.repr (fun v -> unexpected_value_p v "instance" p1)) [vo]))
	| TField(ef,fa) ->
		(* emit_field_read_write *)
		let name = hash (field_name fa) in
		let p1 = e1.epos in
		let c1 = expr t false ef in
		let exec2 = EvalJit.jit_subexpr t.jit false e2 in
		bind "obj" c1 (fun v1 ->
			call t (Obj.repr (fun v1 env -> emit_field_read_write (fun _ -> v1) p1 name exec2 fop prefix env)) [v1;env t])
	| TArray(ea1,ea2) ->
		(* only with a simple index and value (see is_native_assign_op) *)
		let vector = match follow ea1.etype with TInst({cl_path=(["eval"],"Vector")},_) -> true | _ -> false in
		let p1 = ea1.epos and p2 = ea2.epos in
		let c1 = expr t false ea1 in
		let c2 = expr t false ea2 in
		let c3 = expr t false e2 in
		let f = if vector then (fun v1 v2 v3 -> emit_vector_read_write (fun _ -> v1) p1 (fun _ -> v2) p2 (fun _ -> v3) fop prefix dummy_env)
			else (fun v1 v2 v3 -> emit_array_read_write (fun _ -> v1) p1 (fun _ -> v2) p2 (fun _ -> v3) fop prefix dummy_env) in
		bind "a" c1 (fun v1 -> bind "i" c2 (fun v2 -> bind "v" c3 (fun v3 -> call t (Obj.repr f) [v1;v2;v3])))
	| _ ->
		die "" __LOC__

(* [new], as in EvalJit *)
and new_expr t e c el =
	let ctx = t.jit.ctx in
	let p = e.epos in
	let cl = List.map (expr t false) el in
	let key = path_hash c.cl_path in
	match get_special_instance_constructor_raise ctx key with
	| f ->
		(* emit_special_instance *)
		bind_list "arg" cl (fun vl -> call t (Obj.repr f) [make_list vl])
	| exception Not_found ->
		(* emit_constructor_call *)
		let fnew = get_instance_constructor ctx key p in
		let proto = get_instance_prototype ctx key p in
		let v = AtomicLazy.from_fun (fun () -> match AtomicLazy.force fnew with VFunction (f,_) -> f | v -> cannot_call v p) in
		Csequence(call t (Obj.repr check_stack_depth) [env t],
			bind "f" (call t (Obj.repr (fun () -> AtomicLazy.force v)) [unit_value]) (fun f ->
				bind "this" (call t (Obj.repr (fun () -> create_instance_direct proto INormal)) [unit_value]) (fun vthis ->
					bind_list "arg" cl (fun vl ->
						Csequence(set_pos t p,
							Csequence(Cmm_helpers.generic_apply Asttypes.Mutable f [make_list (vthis :: vl)] dbg,vthis))))))

and set_pos t p =
	Csequence(store_int (env t) env_field_leave_pmin (Pos.min p),
		store_int (env t) env_field_leave_pmax (Pos.max p))

(* Calls, as in EvalJit and EvalEmitter: same checks, same order of
   evaluation, same positions recorded for the call stack *)
and call_expr t e e1 el =
	let ctx = t.jit.ctx in
	let p = e.epos in
	let check_stack = call t (Obj.repr check_stack_depth) [env t] in
	let set_pos = set_pos t p in
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
	(* When the object and the arguments are simple (see [is_simple]), the
	   whole sequence of a call is one OCaml function: the order of evaluation
	   does not matter for them. [body env this args] for an object. *)
	let simple_args = List.for_all is_simple el in
	let merged this (body : env -> value -> value list -> value) =
		let n = List.length el in
		let f : Obj.t option = match n with
			| 0 -> Some (Obj.repr (fun env vthis -> body env vthis []))
			| 1 -> Some (Obj.repr (fun env vthis a -> body env vthis [a]))
			| 2 -> Some (Obj.repr (fun env vthis a b -> body env vthis [a;b]))
			| 3 -> Some (Obj.repr (fun env vthis a b c -> body env vthis [a;b;c]))
			| 4 -> Some (Obj.repr (fun env vthis a b c d -> body env vthis [a;b;c;d]))
			| _ -> None
		in
		match f with
		| Some f when simple_args && (match this with Some e -> is_simple e | None -> true) ->
			let cthis = match this with Some e -> expr t false e | None -> int raw_null in
			let cl = List.map (expr t false) el in
			Some (call t f (env t :: cthis :: cl))
		| _ ->
			None
	in
	let leave env =
		env.env_leave_pmin <- Pos.min p;
		env.env_leave_pmax <- Pos.max p
	in
	let null_access ef vthis = if vthis == VNull then throw_string "Null Access" ef.epos in
	(* emit_proto_field_call *)
	let proto_field_call v this =
		match merged this (fun env vthis vl ->
			check_stack_depth env;
			let f = AtomicLazy.force v in
			match this with
			| None -> leave env; f vl
			| Some ef -> null_access ef vthis; leave env; f (vthis :: vl)
		) with
		| Some c -> c
		| None ->
		let force = call t (Obj.repr (fun () -> AtomicLazy.force v)) [unit_value] in
		Csequence(check_stack,
			bind "f" force (fun f ->
				let k vl = Csequence(set_pos,Cmm_helpers.generic_apply Asttypes.Mutable f [make_list vl] dbg) in
				match this with
				| None -> args el k
				| Some ef -> bind "this" (null_check ef) (fun vthis -> args el (fun vl -> k (vthis :: vl)))))
	in
	match e1.eexpr with
	| TField({eexpr = TConst TSuper;epos=pv},FInstance(c,_,cf)) ->
		(* emit_super_field_call *)
		let proto = get_instance_prototype ctx (path_hash c.cl_path) e1.epos in
		let i = get_proto_field_index proto (hash cf.cf_name) in
		let slot = get_slot t.jit 0 pv in
		Csequence(check_stack,
			bind "this" (load (locals t) slot) (fun vthis ->
				bind "f" (call t (Obj.repr (fun () -> proto.pfields.(i))) [unit_value]) (fun vf ->
					args el (fun vl -> call t (Obj.repr call_value_on) [vthis;vf;make_list vl]))))
	| TField(ef,FStatic({cl_path=[],"StringTools"},{cf_name=("fastCodeAt" | "unsafeCodeAt") as name})) ->
		(* emit_string_cca(_unsafe): the string is decoded before the index is evaluated *)
		begin match el with
			| [e1;e2] ->
				let c1 = expr t false e1 in
				let c2 = expr t false e2 in
				let check v = ignore (decode_vstring v); v in
				let f = if name = "fastCodeAt" then (fun v1 v2 -> emit_string_cca (fun _ -> v1) (fun _ -> v2) p dummy_env)
					else (fun v1 v2 -> emit_string_cca_unsafe (fun _ -> v1) (fun _ -> v2) p dummy_env) in
				bind "s" c1 (fun v1 ->
					let k v1 = bind "i" c2 (fun v2 -> call t (Obj.repr f) [v1;v2]) in
					if is_simple e2 then k v1 else bind "s" (call t (Obj.repr check) [v1]) k)
			| _ -> die "" __LOC__
		end
	| TField(_,FEnum({e_path=path},ef)) ->
		(* emit_enum_construction *)
		let key = path_hash path in
		let i = ef.ef_index in
		let pos = Some p in
		args el (fun vl -> call t (Obj.repr (fun vl -> encode_enum_value key i (Array.of_list vl) pos)) [make_list vl])
	| TConst TSuper ->
		begin match follow e1.etype with
		| TInst(c,_) ->
			let key = path_hash c.cl_path in
			begin match get_special_instance_constructor_raise ctx key with
			| f ->
				(* emit_special_super_call *)
				let super vl env = emit_special_super_call f (List.map (fun v _ -> v) vl) env in
				Csequence(check_stack,args el (fun vl -> call t (Obj.repr super) [make_list vl;env t]))
			| exception Not_found ->
				(* emit_super_call *)
				let fnew = get_instance_constructor ctx key e1.epos in
				let v = AtomicLazy.from_fun (fun () -> match AtomicLazy.force fnew with VFunction (f,_) -> f | v -> cannot_call v p) in
				Csequence(check_stack,
					bind "f" (call t (Obj.repr (fun () -> AtomicLazy.force v)) [unit_value]) (fun f ->
						bind "this" (load (locals t) 0) (fun vthis ->
							args el (fun vl ->
								Csequence(set_pos,
									Csequence(Cmm_helpers.generic_apply Asttypes.Mutable f [make_list (vthis :: vl)] dbg,vthis))))))
			end
		| _ -> die "" __LOC__
		end
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
			match merged (Some ef) (fun env vthis vl ->
				check_stack_depth env;
				null_access ef vthis;
				let vf = lookup vthis in
				leave env;
				call_value_on vthis vf vl
			) with
			| Some c -> c
			| None ->
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
				match merged (Some ef) (fun env vthis vl ->
					check_stack_depth env;
					null_access ef vthis;
					let vf = field vthis name in
					leave env;
					call_value_on vthis vf vl
				) with
				| Some c -> c
				| None ->
				Csequence(check_stack,
					bind "this" (null_check ef) (fun vthis ->
						bind "f" (call t (Obj.repr (fun v -> field v name)) [vthis]) (fun vf ->
							Csequence(set_pos,
								args el (fun vl -> call t (Obj.repr call_value_on) [vthis;vf;make_list vl])))))
		end
	| _ ->
		(* emit_call *)
		match merged (Some e1) (fun env v1 vl ->
			check_stack_depth env;
			leave env;
			call_value v1 vl
		) with
		| Some c -> c
		| None ->
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

(* A function to compile *)
type item = {
	i_key_type : int;
	i_key_field : int;
	i_tf : tfunc;
	i_static : bool;
	i_pos : pos;
}

(* The Cmm of the function [n] of a unit, as [EvalJit.jit_tfunction] with a
   native [exec]. Returns the phrases and what [create_function] needs. *)
let translate_function ctx consts symbol n item =
	let jit = EvalJitContext.create ctx in
	let env_var,env_vp = var "env" in
	let t = {
		jit;
		consts;
		consts_symbol = symbol "consts";
		env = env_var;
		return_label = Lambda.next_raise_count ();
		loops = [];
		int_vars = int_locals ctx item.i_tf.tf_expr;
		int_cvars = Hashtbl.create 0;
	} in
	let tf = item.i_tf and static = item.i_static in
	push_scope jit item.i_pos;
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
	let name = Printf.sprintf "exec%d" n in
	let fn = Cfunction {
		fun_name = symbol name;
		fun_args = [env_vp,typ_val;clos_vp,typ_val];
		fun_body = body;
		fun_codegen_options = [];
		fun_poll = Lambda.Default_poll;
		fun_dbg = dbg;
	} in
	let closure = Cdata [
		Cint (Cmm_helpers.black_closure_header 2);
		Cdefine_symbol (symbol (name ^ "_closure"));
		Csymbol_address (symbol name);
		Cint (Cmm_helpers.closure_info ~arity:1 ~startenv:2);
	] in
	[fn;closure],(jit,fl)

(* Compiles [items] in one unit. Returns, for each item, its function or the
   reason why it could not be compiled. *)
let compile_items ctx items =
	let consts = DynArray.create () in
	let results = ref [] in
	let c = Jit.Jit_backend.compile (fun symbol ->
		let phrases = List.concat (List.mapi (fun n item ->
			match translate_function ctx consts symbol n item with
			| phrases,info ->
				results := (item,Ok (n,info)) :: !results;
				phrases
			| exception Unsupported s ->
				results := (item,Error ("unsupported: " ^ s)) :: !results;
				[]
		) items) in
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
		data :: phrases
	) in
	let _,l = Jit.Jit_backend.load ~gc_roots:"gc_roots" c in
	let table = Jit.Jit_backend.value c l "consts" in
	DynArray.iteri (fun i v -> Obj.set_field table i v) consts;
	List.rev_map (fun (item,r) -> item,match r with
		| Error s -> Error s
		| Ok (n,(jit,fl)) ->
			let exec : env -> value = Obj.obj (Jit.Jit_backend.value c l (Printf.sprintf "exec%d_closure" n)) in
			let hasret = jit.has_nonfinal_return in
			let eci = EvalJit.get_env_creation jit item.i_static (Pos.file item.i_tf.tf_expr.epos) (EKMethod(item.i_key_type,item.i_key_field)) in
			Ok (if hasret then create_function ctx eci exec fl else create_function_noret ctx eci exec fl)
	) !results

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

(* Statistics, printed at exit with HAXE_JIT_STATS *)
let stats_lock = Mutex.create ()
let counters = Hashtbl.create 8
let count name n =
	if Jit.Jit_backend.stats_enabled then
		Mutex.protect stats_lock (fun () ->
			Hashtbl.replace counters name (n + try Hashtbl.find counters name with Not_found -> 0))

let () =
	if Jit.Jit_backend.stats_enabled then begin
		fallback_counter := (fun e ->
			let kind = match e.eexpr with
				| TField(_,fa) -> "TField " ^ (match fa with FInstance _ -> "instance" | FStatic _ -> "static" | FAnon _ -> "anon" | FDynamic _ -> "dynamic" | FClosure _ -> "closure" | FEnum _ -> "enum")
				| TBinop(op,_,_) -> "TBinop " ^ s_binop op
				| TUnop(op,_,_) -> "TUnop " ^ s_unop op
				| TCall({eexpr = TField(_,FEnum _)},_) -> "TCall enum"
				| TCall _ -> "TCall other"
				| _ -> let s = s_expr_kind e in s
			in
			count ("fallback " ^ kind) 1);
		at_exit (fun () ->
			let l = Hashtbl.fold (fun r n acc -> (n,"calls from " ^ r) :: acc) call_sites [] in
			List.iter (fun (n,r) -> Printf.eprintf "native %-40s %d\n" r n) (List.sort Stdlib.compare l);
			let l = Hashtbl.fold (fun r n acc -> (n,r) :: acc) counters [] in
			List.iter (fun (n,r) -> Printf.eprintf "native %-40s %d\n" r n) (List.sort Stdlib.compare l))
	end

(*
	Functions are compiled by groups: the methods of a type (see
	[EvalPrototype]) are registered when its prototypes are created, and the
	first time one of them is needed, all those of its group not yet compiled
	are compiled together, in one unit: the fixed costs of a unit (loading,
	registration with the runtime, which stops all domains) are paid once.
*)
type group = {
	g_lock : Mutex.t;
	mutable g_pending : item list;
	(* compiled functions, by physical identity of their [tfunc] *)
	mutable g_compiled : (tfunc * (vfunc,string) result) list;
}

let groups : (int,group) Hashtbl.t = Hashtbl.create 0
let groups_lock = Mutex.create ()

let get_group key =
	Mutex.protect groups_lock (fun () ->
		try Hashtbl.find groups key with Not_found ->
			let g = { g_lock = Mutex.create (); g_pending = []; g_compiled = [] } in
			Hashtbl.add groups key g;
			g
	)

(* Adds a function to the pending ones of [g] (under [g.g_lock]) *)
let add_pending g key_type key_field tf static pos =
	let item = { i_key_type = key_type; i_key_field = key_field; i_tf = tf; i_static = static; i_pos = pos } in
	g.g_compiled <- List.filter (fun (tf',_) -> tf' != tf) g.g_compiled;
	g.g_pending <- item :: List.filter (fun i -> i.i_tf != tf) g.g_pending

(* Registers method [key_field] of type [key_type], to be compiled with its
   group *)
let register ctx key_type key_field tf static pos =
	if enabled ctx && size tf.tf_expr <= max_size then begin
		let g = get_group key_type in
		Mutex.protect g.g_lock (fun () -> add_pending g key_type key_field tf static pos)
	end

let compile_group ctx g =
	let items = List.rev g.g_pending in
	g.g_pending <- [];
	count "groups" 1;
	count "functions compiled" (List.length items);
	let results =
		try
			jit_timer ctx (fun () -> compile_items ctx items)
		with
		| Jit.X86_encode.Unsupported s -> List.map (fun i -> i,Error ("encoder: " ^ s)) items
		| Jit.Jit_link.Link_error s -> List.map (fun i -> i,Error ("link: " ^ s)) items
	in
	List.iter (fun (item,r) -> g.g_compiled <- (item.i_tf,r) :: g.g_compiled) results

let jit_tfunction ctx key_type key_field tf static pos =
	let closures reason =
		count ("closures: " ^ reason) 1;
		EvalJit.jit_tfunction ctx key_type key_field tf static pos
	in
	if not (enabled ctx) then EvalJit.jit_tfunction ctx key_type key_field tf static pos
	else if size tf.tf_expr > max_size then closures "size"
	(* a function needed while this domain compiles (see [Jit_backend.Busy]) *)
	else if Domain.DLS.get Jit.Jit_backend.compiling then closures "busy"
	else begin
		let g = get_group key_type in
		let r = Mutex.protect g.g_lock (fun () ->
			let find () = List.find_opt (fun (tf',_) -> tf' == tf) g.g_compiled in
			match find () with
			| Some (_,r) -> Some r
			| None ->
				if not (List.exists (fun i -> i.i_tf == tf) g.g_pending) then
					add_pending g key_type key_field tf static pos;
				compile_group ctx g;
				Option.map snd (find ())
		) in
		count "functions used" 1;
		match r with
		| Some (Ok f) -> f
		| Some (Error s) -> closures s
		| None -> closures "not compiled"
	end
