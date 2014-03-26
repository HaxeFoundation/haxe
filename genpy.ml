open Ast
open Type
open Common

let bootcode = "import builtins as _hx_builtin

_hx_classes = dict()

class _hx_AnonObject(object):
    def __init__(self, **kwargs):
        self.__dict__.update(kwargs)

_hx_c = _hx_AnonObject()

_hx_c._hx_AnonObject = _hx_AnonObject


import functools as _hx_functools

class Enum:
    # String tag;
    # int index;
    # List params;
    def __init__(self, tag, index, params):
        self.tag = tag
        self.index = index
        self.params = params

    def __str__(self):
        if self.params == None:
            res = self.tag
        else:
            res = self.tag + '(' + ','.join(self.params) + ')'
        res

Enum._hx_class_name = 'Enum'
Enum._hx_class = Enum
_hx_classes['Enum'] = Enum
_hx_c.Enum = Enum

class _HxException(Exception):
    # String tag;
    # int index;
    # List params;
    def __init__(self, val):
        try:
            message = _hx_c.Std.string(val)
        except Exception:
            message = '_HxException'
        Exception.__init__(self, message)
        self.val = val

class Int:
    pass

Int._hx_class_name = 'Int'
Int._hx_class = Int
_hx_classes['Int'] = Int
_hx_c.Int = Int

class Bool:
    pass

Bool._hx_class_name = 'Bool'
Bool._hx_class = Bool

_hx_classes['Bool'] = Bool
_hx_c.Bool = Bool

class Float:
    pass

Float._hx_class_name = 'Float'
Float._hx_class = Float
_hx_classes['Float'] = Float
_hx_c.Float = Float

class Dynamic:
    pass

Dynamic._hx_class_name = 'Dynamic'
Dynamic._hx_class = Dynamic
_hx_classes['Dynamic'] = Dynamic
_hx_c.Dynamic = Dynamic

class Class:
    pass

Class._hx_class_name = 'Class'
Class._hx_class = Class
_hx_classes['Class'] = Class
_hx_c.Class = Class

def _hx_rshift(val, n):
    return (val % 0x100000000) >> n
def _hx_modf(a,b):
    return float('nan') if (b == 0.0) else a % b if a > 0 else -(-a % b)
def _hx_array_get(a,i):
    return a[i] if (i < len(a) and i > -1) else None
def _hx_array_set(a,i,v):
    l = len(a)
    while l < i:
        a.append(None)
        l+=1
    if l == i:
        a.append(v)
    else:
        a[i] = v
    return v

def _hx_toUpperCase (x):
    if isinstance(x, str):
        return x.upper()
    return x.toUpperCase()

import math as _hx_math


def HxOverrides_iterator(x):
    if isinstance(x, list):
        return _hx_c.python_internal_ArrayImpl.iterator(x)
    else:
        return x.iterator()

def HxOverrides_shift(x):
    if isinstance(x, list):
        return _hx_c.python_internal_ArrayImpl.shift(x)
    else:
        return x.shift()

def HxOverrides_filter(x, f):
    if isinstance(x, list):
        return _hx_c.python_internal_ArrayImpl.filter(x,f)
    else:
        return x.filter(f)

def HxOverrides_map(x, f):
    if isinstance(x, list):
        return _hx_c.python_internal_ArrayImpl.map(x,f)
    else:
        return x.filter(f)

def HxOverrides_length(x):
    if isinstance(x, list) or isinstance(x, str):
        return _hx_builtin.len(x)
    else:
        return x.length
"

module KeywordHandler = struct
	let kwds =
		let h = Hashtbl.create 0 in
		List.iter (fun s -> Hashtbl.add h s ()) [
			"and"; "as"; "assert"; "break"; "class"; "continue"; "def"; "del"; "elif"; "else"; "except"; "exec"; "finally"; "for";
			"from"; "global"; "if"; "import"; "in"; "is"; "lambda"; "not"; "or"; "pass"; "print";" raise"; "return"; "try"; "while";
			"with"; "yield"; "float"; "None"
		];
		h

	let handle_keywords s =
		if Hashtbl.mem kwds s then "_hx_" ^ s else s

	let unhandle_keywords s =
		if String.length s > 4 && String.sub s 0 4 = "_hx_" then
			String.sub s 4 (String.length s - 4)
		else
			s
end

module Transformer = struct
	type adjusted_expr = {
		a_expr : texpr;
		a_blocks : texpr list;
		a_next_id : unit -> string;
		a_is_value : bool;
	}

	let t_bool = ref t_dynamic
	let t_void = ref t_dynamic

	let init com =
		t_bool := com.basic.tbool;
		t_void := com.basic.tvoid

	and debug_expr e =
		let s_type = Type.s_type (print_context()) in
		let s = Type.s_expr_pretty "\t" s_type e in
		Printf.printf "%s\n" s

	let new_counter () =
		let n = ref (-1) in
		(fun () ->
			incr n;
			Printf.sprintf "_hx_local_%i" !n
		)

	let to_expr ae =
		match ae.a_blocks with
			| [] ->
				ae.a_expr
			| el ->
				match ae.a_expr.eexpr with
					| TBlock el2 ->
						{ ae.a_expr with eexpr = TBlock (el @ el2) }
					| _ ->
						{ ae.a_expr with eexpr = TBlock (el @ [ae.a_expr])}

	let lift_expr ?(is_value = false) ?(next_id = None) ?(blocks = []) e =
		let next_id = match next_id with
			| None ->
				new_counter()
			| Some f ->
				f
		in
		{
			a_expr = e;
			a_blocks = blocks;
			a_next_id = next_id;
			a_is_value = is_value
		}
	let lift_expr1 is_value next_id blocks e =
		lift_expr ~is_value:is_value ~next_id:(Some next_id) ~blocks:blocks e

	let to_tvar ?(capture = false) n t =
		{ v_name = n; v_type = t; v_id = 0; v_capture = capture; v_extra = None; v_meta = [] }

	let create_non_local n pos =
		let s = "nonlocal " ^ n in
		let id = mk (TLocal (to_tvar "__python__" t_dynamic ) ) !t_void pos in
		let id2 = mk (TLocal( to_tvar s t_dynamic )) !t_void pos in
		mk (TCall(id, [id2])) t_dynamic pos

	let to_tlocal_expr ?(capture = false) n t p =
		mk (TLocal (to_tvar ~capture:capture n t)) t p

	let add_non_locals_to_func e =
		match e.eexpr with
		| TFunction f ->

			let local_vars =
				let h = Hashtbl.create 0 in
				let fn (tvar, _) =
					let x = tvar.v_name in
					Hashtbl.add h x x;
				in
				List.iter fn f.tf_args;
				h
			in
			let non_locals = Hashtbl.create 0 in

			let rec it lv e =
				let maybe_continue x =
					match x.eexpr with
					| TFunction(_) -> ()
					| _ ->
						Type.iter (it (Hashtbl.copy lv)) x;
						()
				in
				match e.eexpr with
				| TVar(v,expr) ->
					(match expr with
					| Some x -> maybe_continue x; ()
					| None -> ());
					Hashtbl.add lv v.v_name v.v_name;
					()
				| TBinop( (OpAssign | OpAssignOp(_)) , { eexpr = TLocal( { v_name = x })}, e2) ->
					if not (Hashtbl.mem lv x) then
						Hashtbl.add non_locals x x;
					maybe_continue e2;
					()
				| TFunction(_) -> ()
				| _ -> Type.iter (it (Hashtbl.copy lv)) e; ()
			in

			Type.iter (it local_vars) f.tf_expr;
			let keys = Hashtbl.fold (fun k _ acc -> k :: acc) non_locals [] in
			let non_local_exprs = List.map (fun (k) -> create_non_local k f.tf_expr.epos) keys in
			let new_exprs = List.append non_local_exprs [f.tf_expr] in
			let f = { f with tf_expr = { f.tf_expr with eexpr = TBlock(new_exprs)}} in
			{e with eexpr = TFunction f }
		| _ -> assert false

	let rec transform_function tf ae is_value =
		let p = tf.tf_expr.epos in
		let assigns = List.fold_left (fun acc (v,value) -> match value with
			| None ->
				acc
			| Some ct ->
				let a_local = mk (TLocal v) v.v_type p in
				let a_null = mk (TConst TNull) v.v_type p in
				let a_cmp = mk (TBinop(OpEq,a_local,a_null)) !t_bool p in
				let a_value = mk (TConst(ct)) v.v_type p in
				let a_assign = mk (TBinop(OpAssign,a_local,a_value)) v.v_type p in
				let a_if = mk (TIf(a_cmp,a_assign,None)) !t_void p in
				a_if :: acc
		) [] tf.tf_args in
		let body = match assigns with
			| [] ->
				tf.tf_expr
			| _ ->
				let eb = mk (TBlock (List.rev assigns)) t_dynamic p in
				Codegen.concat tf.tf_expr eb
		in
		let e1 = to_expr (transform_expr ~next_id:(Some ae.a_next_id) body) in
		let fn = mk (TFunction({
			tf_expr = e1;
			tf_args = tf.tf_args;
			tf_type = tf.tf_type;
		})) ae.a_expr.etype p in
		let fn = add_non_locals_to_func fn in
		if is_value then begin
			let new_name = ae.a_next_id() in
			let new_var = alloc_var new_name tf.tf_type in
			let new_local = mk (TLocal new_var) fn.etype p in
			let def = mk (TVar(new_var,Some fn)) fn.etype p in
			lift_expr1 false ae.a_next_id [def] new_local
		end else
			lift_expr fn

	and transform_var_expr ae eo v =
		let b,new_expr = match eo with
			| None ->
				[],None
			| Some e1 ->
				let f = transform_expr1 true ae.a_next_id [] e1 in
				let b = f.a_blocks in
				b,Some(f.a_expr)
		in
		let e = mk (TVar(v,new_expr)) ae.a_expr.etype ae.a_expr.epos in
		lift_expr ~next_id:(Some ae.a_next_id) ~blocks:b e

	and transform_expr ?(is_value = false) ?(next_id = None) ?(blocks = []) (e : texpr) : adjusted_expr =
		transform1 (lift_expr ~is_value ~next_id ~blocks e)

	and transform_expr1 is_value next_id blocks e =
		transform_expr ~is_value ~next_id:(Some next_id) ~blocks e

	and transform_exprs_to_block el tb is_value p next_id =
		match el with
			| [e] ->
				transform_expr ~is_value ~next_id:(Some next_id) e
			| _ ->
				let res = DynArray.create () in
				List.iter (fun e ->
					let ae = transform_expr ~is_value ~next_id:(Some next_id) e in
					List.iter (DynArray.add res) ae.a_blocks;
					DynArray.add res ae.a_expr
				) el;
				lift_expr (mk (TBlock (DynArray.to_list res)) tb p)

	and transform_switch ae is_value e1 cases edef =
		let case_functions = ref [] in
		let case_to_if (el,e) eelse =
			let val_reversed = List.rev el in
			let mk_eq e = mk (TBinop(OpEq,e1,e)) !t_bool (punion e1.epos e.epos) in
			let cond = match val_reversed with
				| [] ->
					assert false
				| [e] ->
					mk_eq e
				| e :: el ->
					List.fold_left (fun eelse e -> mk (TBinop(OpOr,eelse,mk_eq e)) !t_bool (punion eelse.epos e.epos)) (mk_eq e) el
			in
			let eif = if is_value then begin
				let name = ae.a_next_id() in
				let func = exprs_to_func [e] name ae in
				case_functions := !case_functions @ func.a_blocks;
				let call = func.a_expr in
				mk (TIf(cond,call,eelse)) ae.a_expr.etype ae.a_expr.epos
			end else
				mk (TIf(cond,e,eelse)) ae.a_expr.etype e.epos
			in
			eif
		in
		let rev_cases = List.rev cases in
		let edef = Some (match edef with
			| None ->
				mk (TBlock []) ae.a_expr.etype ae.a_expr.epos
			| Some e ->
				e)
		in
		let res = match rev_cases with
			| [] ->
				(* TODO: this can be handled better *)
				null ae.a_expr.etype ae.a_expr.epos
			| [case] ->
				case_to_if case edef
			| case :: cases ->
				List.fold_left (fun acc case -> case_to_if case (Some acc)) (case_to_if case edef) cases
		in
		let res = if is_value then
			mk (TBlock ((List.rev (res :: !case_functions)))) res.etype res.epos
		else
			res
		in
		forward_transform res ae

	and transform_op_assign_op ae e1 op one is_value post =
		let e1_ = transform_expr e1 ~is_value:true ~next_id:(Some ae.a_next_id) in
		let handle_as_local temp_local =
			let ex = ae.a_expr in
			let res_var = alloc_var (ae.a_next_id()) ex.etype in
			let res_local = {ex with eexpr = TLocal res_var} in
			let plus = {ex with eexpr = TBinop(op,temp_local,one)} in
			let var_expr = {ex with eexpr = TVar(res_var,Some temp_local)} in
			let assign_expr = {ex with eexpr = TBinop(OpAssign,e1_.a_expr,plus)} in
			let blocks = if post then
				[var_expr;assign_expr;res_local]
			else
				[assign_expr;temp_local]
			in
			(* TODO: block is ignored in the else case? *)
			let block = e1_.a_blocks @ blocks in
			if is_value then begin
				let f = exprs_to_func block (ae.a_next_id()) ae in
				lift_expr f.a_expr ~is_value:true ~next_id:(Some ae.a_next_id) ~blocks:f.a_blocks
			end else begin
				let block = e1_.a_blocks @ [assign_expr] in
				transform_exprs_to_block block ex.etype false ex.epos ae.a_next_id
			end
		in
		match e1_.a_expr.eexpr with
			| TArray({eexpr = TLocal _},{eexpr = TLocal _})
			| TField({eexpr = TLocal _},_)
			| TLocal _ ->
				handle_as_local e1_.a_expr
			| TArray(e1,e2) ->
				let id = ae.a_next_id() in
				let temp_var_l = alloc_var id e1.etype in
				let temp_local_l = {e1 with eexpr = TLocal temp_var_l} in
				let temp_var_l = {e1 with eexpr = TVar(temp_var_l,Some e1)} in

				let id = ae.a_next_id() in
				let temp_var_r = alloc_var id e2.etype in
				let temp_local_r = {e2 with eexpr = TLocal temp_var_r} in
				let temp_var_r = {e2 with eexpr = TVar(temp_var_r,Some e2)} in

				let id = ae.a_next_id() in
				let temp_var = alloc_var id e1_.a_expr.etype in
				let temp_local = {e1_.a_expr with eexpr = TLocal temp_var} in
				let temp_var_expr = {e1_.a_expr with eexpr = TArray(temp_local_l,temp_local_r)} in
				let temp_var = {e1_.a_expr with eexpr = TVar(temp_var,Some temp_var_expr)} in

				let plus = {ae.a_expr with eexpr = TBinop(op,temp_local,one)} in
				let assign_expr = {ae.a_expr with eexpr = TBinop(OpAssign,temp_var_expr,plus)} in
				let block = e1_.a_blocks @ [temp_var_l;temp_var_r;temp_var;assign_expr;if post then temp_local else temp_var_expr] in
				if is_value then begin
					let f = exprs_to_func block (ae.a_next_id()) ae in
					lift_expr f.a_expr ~is_value:true ~next_id:(Some ae.a_next_id) ~blocks:f.a_blocks
				end else
					transform_exprs_to_block block ae.a_expr.etype false ae.a_expr.epos ae.a_next_id
			| TField(e1,fa) ->
				let temp_var_l = alloc_var (ae.a_next_id()) e1.etype in
				let temp_local_l = {e1 with eexpr = TLocal temp_var_l} in
				let temp_var_l = {e1 with eexpr = TVar(temp_var_l,Some e1)} in

				let temp_var = alloc_var (ae.a_next_id()) e1_.a_expr.etype in
				let temp_local = {e1_.a_expr with eexpr = TLocal temp_var} in
				let temp_var_expr = {e1_.a_expr with eexpr = TField(temp_local_l,fa)} in
				let temp_var = {e1_.a_expr with eexpr = TVar(temp_var,Some temp_var_expr)} in

				let plus = {ae.a_expr with eexpr = TBinop(op,temp_local,one)} in
				let assign_expr = {ae.a_expr with eexpr = TBinop(OpAssign,temp_var_expr,plus)} in
				let block = e1_.a_blocks @ [temp_var_l;temp_var;assign_expr;if post then temp_local else temp_var_expr] in
				if is_value then begin
					let f = exprs_to_func block (ae.a_next_id()) ae in
					lift_expr f.a_expr ~is_value:true ~next_id:(Some ae.a_next_id) ~blocks:f.a_blocks
				end else
					transform_exprs_to_block block ae.a_expr.etype false ae.a_expr.epos ae.a_next_id
			| _ ->
				assert false

	and var_to_treturn_expr ?(capture = false) n t p =
		let x = mk (TLocal (to_tvar ~capture:capture n t)) t p in
		mk (TReturn (Some x)) t p

	and exprs_to_func exprs name base =
		let convert_return_expr (expr:texpr) =
			match expr.eexpr with
			| TFunction(f) ->
				let ret = var_to_treturn_expr name f.tf_type f.tf_expr.epos in
				[expr;ret]
			| TBinop(OpAssign, l, r) ->
				let r = { l with eexpr = TReturn(Some l) } in
				[expr; r]
			| x ->
				let ret_expr = { expr with eexpr = TReturn( Some(expr) )} in
				[ret_expr]
		in
		let def =
			(let ex = match exprs with
			| [] -> assert false
			| [x] ->
				(let exs = convert_return_expr x in
				match exs with
				| [] -> assert false
				| [x] -> x
				| x ->
					match List.rev x with
					| x::xs ->
						mk (TBlock exs) x.etype base.a_expr.epos
					| _ -> assert false)

			| x ->
				match List.rev x with
				| x::xs ->
					(let ret = x in
					let tail = List.rev xs in
					let block = tail @ (convert_return_expr ret) in
					match List.rev block with
					| x::_ ->
						mk (TBlock block) x.etype base.a_expr.epos
					| _ -> assert false)
				| _ -> assert false
			in
			let f1 = { tf_args = []; tf_type = TFun([],ex.etype); tf_expr = ex} in
			let fexpr = mk (TFunction f1) ex.etype ex.epos in
			let fvar = to_tvar name fexpr.etype in
			let f = add_non_locals_to_func fexpr in
			let assign = { ex with eexpr = TVar(fvar, Some(f))} in
			let call_expr = (mk (TLocal fvar) fexpr.etype ex.epos ) in
			let substitute = mk (TCall(call_expr, [])) ex.etype ex.epos in
			lift_expr ~blocks:[assign] substitute)
		in
		match exprs with
		| [{ eexpr = TFunction({ tf_args = []} as f) } as x] ->
			let l = to_tlocal_expr name f.tf_type f.tf_expr.epos in
			let substitute = mk (TCall(l, [])) f.tf_type f.tf_expr.epos in
			lift_expr ~blocks:[x] substitute
		| _ -> def



	and transform1 ae : adjusted_expr =

		let trans is_value blocks e = transform_expr1 is_value ae.a_next_id blocks e in
		let lift is_value blocks e = lift_expr1 is_value ae.a_next_id blocks e in
		let a_expr = ae.a_expr in
		match ae.a_is_value,ae.a_expr.eexpr with
		| (is_value,TBlock [x]) ->
			trans is_value [] x
		| (_,TBlock []) ->
			lift_expr (mk (TConst TNull) ae.a_expr.etype ae.a_expr.epos)
		| (false,TBlock el) ->
			transform_exprs_to_block el ae.a_expr.etype false ae.a_expr.epos ae.a_next_id
		| (true,TBlock el) ->
			let name = ae.a_next_id() in
			let block,tr = match List.rev el with
				| e :: el ->
					List.rev ((mk (TReturn (Some e)) t_dynamic e.epos) :: el),e.etype
				| [] ->
					assert false
			in
			let my_block = transform_exprs_to_block block tr false ae.a_expr.epos ae.a_next_id in
			let fn = mk (TFunction {
				tf_args = [];
				tf_type = tr;
				tf_expr = my_block.a_expr;
			}) ae.a_expr.etype ae.a_expr.epos in
			let t_var = alloc_var name ae.a_expr.etype in
			let f = add_non_locals_to_func fn in
			let fn_assign = mk (TVar (t_var,Some f)) ae.a_expr.etype ae.a_expr.epos in
			let ev = mk (TLocal t_var) ae.a_expr.etype ae.a_expr.epos in
			let substitute = mk (TCall(ev,[])) ae.a_expr.etype ae.a_expr.epos in
			lift_expr ~blocks:[fn_assign] substitute
		| (is_value,TFunction(f)) ->
			transform_function f ae is_value
		| (_,TVar(v,None)) ->
			transform_var_expr ae None v
		| (false, TVar(v,Some({ eexpr = TUnop((Increment | Decrement as unop),post_fix,({eexpr = TLocal _ | TField({eexpr = TConst TThis},_)} as ve))} as e1))) ->
			let one = {e1 with eexpr = TConst (TInt (Int32.of_int 1))} in
			let op = if unop = Increment then OpAdd else OpSub in
			let inc = {e1 with eexpr = TBinop(op,ve,one)} in
			let inc_assign = {e1 with eexpr = TBinop(OpAssign,ve,inc)} in
			let var_assign = {e1 with eexpr = TVar(v,Some ve)} in
			let block = if post_fix = Postfix then [var_assign;inc_assign] else [inc_assign;var_assign] in
			transform_exprs_to_block block ae.a_expr.etype false ae.a_expr.epos ae.a_next_id
		| (_,TVar(v,eo)) ->
			transform_var_expr ae eo v
		| (_,TFor(v,e1,e2)) ->
			let e1 = trans true [] e1 in
			let e2 = to_expr (trans false [] e2) in
			let new_expr = mk (TFor(v,e1.a_expr,e2)) ae.a_expr.etype ae.a_expr.epos in
			lift_expr ~blocks: e1.a_blocks new_expr
		| (_,TReturn None) ->
			ae
		| (_,TReturn (Some ({eexpr = TFunction f} as ef))) ->
			let n = ae.a_next_id() in
			let e1 = to_expr (trans false [] f.tf_expr) in
			let f = mk (TFunction {
				tf_args = f.tf_args;
				tf_type = f.tf_type;
				tf_expr = e1;
			}) ef.etype ef.epos in
			let f1 = add_non_locals_to_func f in
			let var_n = alloc_var n ef.etype in
			let f1_assign = mk (TVar(var_n,Some f1)) !t_void f1.epos in
			let var_local = mk (TLocal var_n) ef.etype f1.epos in
			let er = mk (TReturn (Some var_local)) t_dynamic  ae.a_expr.epos in
			lift true [f1_assign] er

		| (_,TReturn Some(x)) ->
			let x1 = trans true [] x in
			(match x1.a_blocks with
				| [] ->
					lift true [] { ae.a_expr with eexpr = TReturn(Some x1.a_expr) }
				| blocks ->
					let f = exprs_to_func (blocks @ [x1.a_expr]) (ae.a_next_id()) ae in
					lift true f.a_blocks {a_expr with eexpr = TReturn (Some f.a_expr)})
		| (_, TParenthesis(e1)) ->
			let e1 = trans true [] e1 in
			let p = { ae.a_expr with eexpr = TParenthesis(e1.a_expr)} in
			lift true e1.a_blocks p
		| (true, TIf(econd, eif, eelse)) ->
			(let econd1 = trans true [] econd in
			let eif1 = trans true [] eif in
			let eelse1 = match eelse with
				| Some x -> Some(trans true [] x)
				| None -> None
			in
			let blocks = [] in
			let eif2, blocks =
				match eif1.a_blocks with
				| [] -> eif1.a_expr, blocks
				| x ->
					let regular =
						let fname = eif1.a_next_id () in
						let f = exprs_to_func (List.append eif1.a_blocks [eif1.a_expr]) fname ae in
						f.a_expr, List.append blocks f.a_blocks
					in
					match eif1.a_blocks with
					| [{ eexpr = TVar(_, Some({ eexpr = TFunction(_)}))} as b] ->
						eif1.a_expr, List.append blocks [b]
					| _ -> regular
			in
			let eelse2, blocks =
				match eelse1 with
				| None -> None, blocks
				| Some({ a_blocks = []} as x) -> Some(x.a_expr), blocks
				| Some({ a_blocks = b} as eelse1) ->
					let regular =
						let fname = eelse1.a_next_id () in
						let f = exprs_to_func (List.append eelse1.a_blocks [eelse1.a_expr]) fname ae in
						Some(f.a_expr), List.append blocks f.a_blocks
					in
					match b with
					| [{ eexpr = TVar(_, Some({ eexpr = TFunction(f)}))} as b] ->
						Some(eelse1.a_expr), List.append blocks [b]
					| _ -> regular
			in
			let blocks = List.append econd1.a_blocks blocks in
			let new_if = { ae.a_expr with eexpr = TIf(econd1.a_expr, eif2, eelse2) } in
			match blocks with
			| [] ->
				let meta = Meta.Custom(":ternaryIf"), [], ae.a_expr.epos in
				let ternary = { ae.a_expr with eexpr = TMeta(meta, new_if) } in
				lift_expr ~blocks:blocks ternary
			| b ->
				let f = exprs_to_func (List.append blocks [new_if]) (ae.a_next_id ()) ae in
				lift_expr ~blocks:f.a_blocks f.a_expr)
		| (false, TIf(econd, eif, eelse)) ->
			let econd = trans true [] econd in
			let eif = to_expr (trans false [] eif) in
			let eelse = match eelse with
			| Some(x) -> Some(to_expr (trans false [] x))
			| None -> None
			in
			let new_if = { ae.a_expr with eexpr = TIf(econd.a_expr, eif, eelse) } in
			lift false econd.a_blocks new_if
		| (false, TWhile(econd, e1, NormalWhile)) ->
			let econd1 = trans true [] econd in
			let e11 = to_expr (trans false [] e1) in
			let new_while = mk (TWhile(econd1.a_expr,e11,NormalWhile)) a_expr.etype a_expr.epos in
			lift false econd1.a_blocks new_while
		| (true, TWhile(econd, ebody, NormalWhile)) ->
			let econd = trans true [] econd in
			let ebody = to_expr (trans false [] ebody) in
			let ewhile = { ae.a_expr with eexpr = TWhile(econd.a_expr, ebody, NormalWhile) } in
			let eval = { ae.a_expr with eexpr = TConst(TNull) } in
			let f = exprs_to_func (List.append econd.a_blocks [ewhile; eval]) (ae.a_next_id ()) ae in
			lift true f.a_blocks f.a_expr
		| (false, TWhile(econd, ebody, DoWhile)) ->
			let not_expr = { econd with eexpr = TUnop(Not, Prefix, econd) } in
			let break_expr = mk TBreak !t_void econd.epos in
			let if_expr = mk (TIf(not_expr, break_expr, None)) (!t_void) econd.epos in
			let new_e = match ebody.eexpr with
			| TBlock(exprs) -> { econd with eexpr = TBlock( List.append exprs [if_expr]) }
			| _ -> { econd with eexpr = TBlock( List.append [if_expr] [ebody]) } in
			let true_expr = mk (TConst(TBool(true))) econd.etype ae.a_expr.epos in
			let new_expr = { ae.a_expr with eexpr = TWhile( true_expr, new_e, NormalWhile) } in
			forward_transform new_expr ae

		| (is_value, TSwitch(e, cases, edef)) ->
			transform_switch ae is_value e cases edef
		| (is_value, TUnop( (Increment | Decrement) as unop, op, e)) ->
			let one = { ae.a_expr with eexpr = TConst(TInt(Int32.of_int(1)))} in
			let is_postfix = match op with
			| Postfix -> true
			| Prefix -> false in
			let op = match unop with
			| Increment -> OpAdd
			| Decrement -> OpSub
			| _ -> assert false in
			transform_op_assign_op ae e op one is_value is_postfix
		| (_, TUnop(op, Prefix, e)) ->
			let e1 = trans true [] e in
			let r = { a_expr with eexpr = TUnop(op, Prefix, e1.a_expr) } in
			lift_expr ~blocks:e1.a_blocks r

		| (is_value, TBinop(OpAssign, left, right))->
			(let left = trans true [] left in
			let right = trans true [] right in
			let r = { a_expr with eexpr = TBinop(OpAssign, left.a_expr, right.a_expr)} in
			if is_value then
				(let blocks = List.concat [left.a_blocks; right.a_blocks; [r]] in
				let f = exprs_to_func blocks (ae.a_next_id ()) ae in
				lift true f.a_blocks f.a_expr)
			else
				lift false (List.append left.a_blocks right.a_blocks) r)
		| (is_value, TBinop(OpAssignOp(x), left, right)) ->
			let right = trans true [] right in
			let v = right.a_expr in
			let res = transform_op_assign_op ae left x v is_value false in
			lift true (List.append right.a_blocks res.a_blocks) res.a_expr
		| (_, TBinop(op, left, right))->
			(let left = trans true [] left in
			let right = trans true [] right in
			let r = { a_expr with eexpr = TBinop(op, left.a_expr, right.a_expr)} in
			lift false (List.append left.a_blocks right.a_blocks) r)

		| (true, TThrow(x)) ->
			let block = TBlock([a_expr; { a_expr with eexpr = TConst(TNull) }]) in
			let r = { a_expr with eexpr = block } in
			forward_transform r ae
		| (false, TThrow(x)) ->
			let x = trans true [] x in
			let r = { a_expr with eexpr = TThrow(x.a_expr)} in
			lift false x.a_blocks r
		| (_, TNew(c, tp, params)) ->
			let params = List.map (trans true []) params in
			let blocks = List.flatten (List.map (fun (p) -> p.a_blocks) params) in
			let params = List.map (fun (p) -> p.a_expr) params in
			let e = { a_expr with eexpr = TNew(c, tp, params) } in
			lift false blocks e
		| (_, TCall({ eexpr = TLocal({v_name = "__python_for__" })} as x, [param])) ->
			let param = trans false [] param in
			let call = { a_expr with eexpr = TCall(x, [param.a_expr])} in
			lift_expr call
		| (_, TCall(e, params)) ->
			let e = trans true [] e in
			let params = List.map (trans true []) params in
			let blocks = List.flatten (List.map (fun (p) -> p.a_blocks) params) in
			let params = List.map (fun (p) -> p.a_expr) params in
			let e = { a_expr with eexpr = TCall(e.a_expr, params) } in
			lift_expr ~blocks:blocks e

		| (true, TArray(e1, e2)) ->
			let e1 = trans true [] e1 in
			let e2 = trans true [] e2 in
			let r = { a_expr with eexpr = TArray(e1.a_expr, e2.a_expr)} in
			let blocks = List.append e1.a_blocks e2.a_blocks in
			lift_expr ~blocks:blocks r
		| (false, TTry(etry, catches)) ->
			let etry = trans false [] etry in
			let catches = List.map (fun(v,e) -> v, trans false [] e) catches in
			let blocks = List.flatten (List.map (fun (_,e) -> e.a_blocks) catches) in
			let catches = List.map (fun(v,e) -> v, e.a_expr) catches in
			let r = { a_expr with eexpr = TTry(etry.a_expr, catches)} in
			let blocks = List.append etry.a_blocks blocks in
			lift false blocks r
		| (true, TTry(etry, catches)) ->

			let id = ae.a_next_id () in
			let temp_var = to_tvar id a_expr.etype in
			let temp_var_def = { a_expr with eexpr = TVar(temp_var, None) } in
			let temp_local = { a_expr with eexpr = TLocal(temp_var)} in
			let mk_temp_assign right = { a_expr with eexpr = TBinop(OpAssign, temp_local, right)} in
			let etry = mk_temp_assign etry in
			let catches = List.map (fun (v,e)-> v, mk_temp_assign e) catches in
			let new_try = { a_expr with eexpr = TTry(etry, catches)} in
			let block = [temp_var_def; new_try; temp_local] in
			let new_block = { a_expr with eexpr = TBlock(block)} in
			forward_transform new_block ae
		| (_, TObjectDecl(fields)) ->
			let fields = List.map (fun (name,ex) -> name, trans true [] ex) fields in
			let blocks = List.flatten (List.map (fun (_,ex) -> ex.a_blocks) fields) in
			let fields = List.map (fun (name,ex) -> name, ex.a_expr) fields in
			let r = { a_expr with eexpr = (TObjectDecl(fields) )} in
			lift_expr ~blocks r
		| (_, TArrayDecl(values)) ->
			let values = List.map (trans true []) values in
			let blocks = List.flatten (List.map (fun (v) -> v.a_blocks) values) in
			let exprs = List.map (fun (v) -> v.a_expr) values in
			let r = { a_expr with eexpr = TArrayDecl exprs } in
			lift_expr ~blocks:blocks r
		| (_, TCast(e,t)) ->
			let e = trans true [] e in
			let r = { a_expr with eexpr = TCast(e.a_expr, t) } in
			lift_expr ~blocks:e.a_blocks r
		| (_, TField(e,f)) ->
			let e = trans true [] e in
			let r = { a_expr with eexpr = TField(e.a_expr, f) } in
			lift_expr ~blocks:e.a_blocks r
		| (is_value, TMeta(m,e)) ->
			let e = trans is_value [] e in
			let r = { a_expr with eexpr = TMeta(m, e.a_expr) } in
			lift_expr ~blocks:e.a_blocks r
		| _ ->
			lift_expr ae.a_expr

	and transform e =
		to_expr (transform1 (lift_expr e))

	and forward_transform e base =
		transform1 (lift_expr1 base.a_is_value base.a_next_id base.a_blocks e)

	let transform_to_value e =
		to_expr (transform1 (lift_expr e ~is_value:true))

end

module Printer = struct

	type print_context = {
		pc_indent : string;
		pc_next_anon_func : unit -> string;
	}

	let create_context =
		let n = ref (-1) in
		(fun indent -> {
				pc_indent = indent;
				pc_next_anon_func = (fun () -> incr n; Printf.sprintf "anon_%i" !n);
			}
		)

	let tabs = ref ""

	let opt o f s = match o with
		| None -> ""
		| Some v -> s ^ (f v)

	(* TODO: both of these are crazy *)

	let is_type p t =
		(fun r ->
			let x = t_infos r in
			(String.concat "." (fst x.mt_path)) = p && (snd x.mt_path) = t
		)

	let is_type1 p s =
		(fun t -> match follow t with
			| TInst(c,_) -> (is_type p s)(TClassDecl c)
			| TAbstract(a,_) -> (is_type p s)(TAbstractDecl a)
			| TEnum(en,_) -> (is_type p s)(TEnumDecl en)
			| _ -> false
		)

	let is_underlying_string t = match follow t with
		| TAbstract(a,tl) -> (is_type1 "" "String")(Codegen.Abstract.get_underlying_type a tl)
		| _ -> false

	let handle_keywords s =
		KeywordHandler.handle_keywords s

	let print_unop = function
		| Increment | Decrement -> assert false
		| Not -> "not "
		| Neg -> "-";
		| NegBits -> "~"

	let print_binop = function
		| OpAdd -> "+"
		| OpSub -> "-"
		| OpMult -> "*"
		| OpDiv -> "/"
		| OpAssign -> "="
		| OpEq -> "=="
		| OpNotEq -> "!="
		| OpGt -> ">"
		| OpGte -> ">="
		| OpLt -> "<"
		| OpLte -> "<="
		| OpAnd -> "&"
		| OpOr -> "|"
		| OpXor -> "^"
		| OpBoolAnd -> "and"
		| OpBoolOr -> "or"
		| OpShl -> "<<"
		| OpShr -> ">>"
		| OpUShr -> ">>"
		| OpMod -> "%"
		| OpInterval | OpArrow | OpAssignOp _ -> assert false

	let print_string s =
		Printf.sprintf "\"%s\"" (Ast.s_escape s)

	let print_constant = function
		| TThis -> "self"
		| TNull -> "None"
		| TBool(true) -> "True"
		| TBool(false) -> "False"
		| TString(s) -> print_string s
		| TInt(i) -> Int32.to_string i
		| TFloat s -> s
		| TSuper -> "super"

	let print_base_type tp =
		try
			begin match Meta.get Meta.Native tp.mt_meta with
				| _,[EConst(String s),_],_ -> s
				| _ -> raise Not_found
			end
		with Not_found ->
			let pack,name = tp.mt_path in
			(String.concat "_" pack) ^ (if pack = [] then name else "_" ^ name)

	let print_module_type mt = print_base_type (t_infos mt)

	let print_metadata (name,_,_) =
		Printf.sprintf "@%s" name

	let print_args args =
		let sl = List.map (fun (v,cto) ->
			let name = handle_keywords v.v_name in
			let arg_string = match follow v.v_type with
				| TAbstract({a_path = [],"KwdArgs"},_) -> "**" ^ name
				| _ -> name
			in
			let arg_value = match cto with
				| None -> ""
				| Some ct -> Printf.sprintf " = %s" (print_constant ct)
			in
			Printf.sprintf "%s%s" arg_string arg_value
		) args in
		String.concat "," sl

	let rec print_op_assign_right pctx e =
		match e.eexpr with
			| TIf({eexpr = TParenthesis econd},eif,Some eelse)
			| TIf(econd,eif,Some eelse) ->
				Printf.sprintf "%s if %s else %s" (print_expr pctx eif) (print_expr pctx econd) (print_expr pctx eelse)
			| _ ->
				print_expr pctx e

	and print_var pctx v eo =
		match eo with
			| Some {eexpr = TFunction tf} ->
				print_function pctx tf (Some v.v_name)
			| _ ->
				let s_init = match eo with
					| None -> "None"
					| Some e -> print_op_assign_right pctx e
				in
				Printf.sprintf "%s = %s" (handle_keywords v.v_name) s_init

	and print_function pctx tf name =
		let s_name = match name with
			| None -> pctx.pc_next_anon_func()
			| Some s -> handle_keywords s
		in
		let s_args = print_args tf.tf_args in
		let s_expr = print_expr {pctx with pc_indent = "\t" ^ pctx.pc_indent} tf.tf_expr in
		Printf.sprintf "def %s(%s):\n%s\t%s" s_name s_args pctx.pc_indent s_expr

	and print_expr pctx e =
		let indent = pctx.pc_indent in
		let print_expr_indented e = print_expr {pctx with pc_indent = "\t" ^ pctx.pc_indent} e in
		match e.eexpr with
			| TConst ct ->
				print_constant ct
			| TTypeExpr mt ->
				print_module_type mt
			| TLocal v ->
				handle_keywords v.v_name
			| TEnumParameter(e1,_,index) ->
				Printf.sprintf "%s.params[%i]" (print_expr pctx e1) index
			| TArray(e1,e2) ->
				Printf.sprintf "_hx_array_get(%s, %s)" (print_expr pctx e1) (print_expr pctx e2)
			| TBinop(OpAssign,{eexpr = TArray(e1,e2)},e3) ->
				Printf.sprintf "_hx_array_set(%s,%s,%s)" (print_expr pctx e1) (print_expr pctx e2) (print_expr pctx e3)
			| TBinop(OpAssign,{eexpr = TField(ef1,fa)},e2) ->
				Printf.sprintf "%s = %s" (print_field pctx ef1 fa true) (print_op_assign_right pctx e2)
			| TBinop(OpAssign,e1,e2) ->
				Printf.sprintf "%s = %s" (print_expr pctx e1) (print_expr pctx e2)
			| TBinop(OpEq,{eexpr = TCall({eexpr = TLocal {v_name = "__typeof__"}},[e1])},e2) ->
				begin match e2.eexpr with
					| TConst(TString s) ->
						begin match s with
							| "string" -> Printf.sprintf "_hx_c.Std._hx_is(%s, _hx_builtin.str)" (print_expr pctx e1)
							| "boolean" -> Printf.sprintf "_hx_c.Std._hx_is(%s, _hx_builtin.bool)" (print_expr pctx e1)
							| "number" -> Printf.sprintf "_hx_c.Std._hx_is(%s, _hx_builtin.float)" (print_expr pctx e1)
							| _ -> assert false
						end
					| _ ->
						assert false
				end
			| TBinop(OpEq,e1,({eexpr = TConst TNull} as e2)) ->
				Printf.sprintf "%s is %s" (print_expr pctx e1) (print_expr pctx e2)
			| TBinop(OpNotEq,e1,({eexpr = TConst TNull} as e2)) ->
				Printf.sprintf "%s is not %s" (print_expr pctx e1) (print_expr pctx e2)
			| TBinop(OpMod,e1,e2) when (is_type1 "" "Int")(e1.etype) && (is_type1 "" "Int")(e2.etype) ->
				Printf.sprintf "%s %% %s" (print_expr pctx e1) (print_expr pctx e2)
			| TBinop(OpMod,e1,e2) ->
				Printf.sprintf "_hx_modf(%s, %s)" (print_expr pctx e1) (print_expr pctx e1)
			| TBinop(OpUShr,e1,e2) ->
				Printf.sprintf "_hx_rshift(%s, %s)" (print_expr pctx e1) (print_expr pctx e2)
			| TBinop(OpAdd,e1,e2) when (is_type1 "" "String")(e.etype) || is_underlying_string e.etype ->
				let safe_string ex =
					match ex.eexpr with
						| TConst(TString _) -> print_expr pctx ex
						| _ -> Printf.sprintf "Std.string(%s)" (print_expr pctx ex)
				in
				let e1_str = safe_string e1 in
				let e2_str = safe_string e2 in
				Printf.sprintf "%s + %s" e1_str e2_str
			| TBinop(OpAdd,e1,e2) when (match follow e.etype with TDynamic _ -> true | _ -> false) ->
				Printf.sprintf "python_Boot._add_dynamic(%s,%s)" (print_expr pctx e1) (print_expr pctx e2);
			| TBinop(op,e1,e2) ->
				Printf.sprintf "%s %s %s" (print_expr pctx e1) (print_binop op) (print_expr pctx e2)
			| TField(e1,fa) ->
				print_field pctx e1 fa false
			| TParenthesis e1 ->
				Printf.sprintf "(%s)" (print_expr pctx e1)
			| TObjectDecl fl ->
				Printf.sprintf "_hx_c._hx_AnonObject(%s)" (print_exprs_named pctx ", " fl)
			| TArrayDecl el ->
				Printf.sprintf "[%s]" (print_exprs pctx ", " el)
			(* TODO: toUpperCase special case?! *)
			| TCall(e1,el) ->
				print_call pctx e1 el
			| TNew(c,_,el) ->
				let id = print_base_type (t_infos (TClassDecl c)) in
				Printf.sprintf "%s(%s)" id (print_exprs pctx ", " el)
			| TUnop(op,Postfix,e1) ->
				Printf.sprintf "%s%s" (print_expr pctx e1) (print_unop op)
			| TUnop(op,Prefix,e1) ->
				Printf.sprintf "%s%s" (print_unop op) (print_expr pctx e1)
			| TFunction tf ->
				print_function pctx tf None
			| TVar (v,eo) ->
				print_var pctx v eo
			| TBlock [] ->
				Printf.sprintf "pass\n%s" indent
			| TBlock el ->
				let old = !tabs in
				tabs := pctx.pc_indent;
				let s = print_exprs pctx ("\n" ^ !tabs) el in
				tabs := old;
				Printf.sprintf "%s\n%s" s !tabs
			| TFor(v,e1,e2) ->
				let pctx2 = {pctx with pc_indent = "\t" ^ pctx.pc_indent} in
				let ind1 = pctx.pc_indent in
				let ind2 = pctx2.pc_indent in
				Printf.sprintf "_it = %s\n%swhile _it.hasNext():\n%s%s = _it.next()\n%s%s" (print_expr pctx e1) ind1 ind2 v.v_name ind2 (print_expr pctx2 e2)
			| TIf(econd,eif,(Some {eexpr = TIf _} as eelse)) ->
				print_if_else pctx econd eif eelse true
			| TIf(econd,eif,eelse) ->
				print_if_else pctx econd eif eelse false
			| TWhile(econd,e1,NormalWhile) ->
				Printf.sprintf "while %s:\n%s\t%s" (print_expr pctx econd) indent (print_expr_indented e1)
			| TWhile(econd,e1,DoWhile) ->
				error "Currently not supported" e.epos
			| TTry(e1,catches) ->
				print_try pctx e1 catches
			| TReturn eo ->
				Printf.sprintf "return%s" (opt eo (print_op_assign_right pctx) " ")
			| TBreak ->
				"break"
			| TContinue ->
				"continue"
			| TThrow e1 ->
				Printf.sprintf "raise _HxException(%s)" (print_expr pctx e1)
			| TCast(e1,_) ->
				(* TODO: safe cast *)
				print_expr pctx e1
			| TMeta((Meta.Custom ":ternaryIf",_,_),{eexpr = TIf(econd,eif,Some eelse)}) ->
				Printf.sprintf "%s if %s else %s" (print_expr pctx eif) (print_expr pctx econd) (print_expr pctx eelse)
			| TMeta(_,e1) ->
				print_expr pctx e1
			| TPatMatch _ | TSwitch _ ->
				assert false

	and print_if_else pctx econd eif eelse as_elif =
		let econd1 = match econd.eexpr with
			| TParenthesis e -> e
			| _ -> econd
		in
		let if_str = print_expr {pctx with pc_indent = "\t" ^ pctx.pc_indent} eif in
		let indent = pctx.pc_indent in
		(* TODO: double check this *)
		(* TODO: triple check it *)
		let else_str = if as_elif then
			opt eelse (print_expr pctx) "el"
		else
			opt eelse (print_expr {pctx with pc_indent = "\t" ^ pctx.pc_indent}) (Printf.sprintf "else:\n%s\t" indent)
		in
		Printf.sprintf "if %s:\n%s\t%s\n%s%s" (print_expr pctx econd1) indent if_str indent else_str

	and print_field pctx e1 fa is_assign =
		let obj = match e1.eexpr with
			| TConst TSuper -> "super()"
			| _ -> print_expr pctx e1
		in
		let name = field_name fa in
		let do_default () =
			Printf.sprintf "%s.%s" obj (handle_keywords name)
		in
		match fa with
			| FInstance(c,{cf_name = "length" | "get_length"}) when (is_type "" "list")(TClassDecl c) ->
				Printf.sprintf "_hx_builtin.len(%s)" (print_expr pctx e1)
			| FInstance(c,{cf_name = "toUpperCase"}) when (is_type "" "String")(TClassDecl c) ->
				Printf.sprintf "%s.toUpper" (print_expr pctx e1)
			| FInstance(c,{cf_name = "toLowerCase"}) when (is_type "" "String")(TClassDecl c) ->
				Printf.sprintf "%s.toLower" (print_expr pctx e1)
			| FInstance _ | FStatic _ ->
				do_default ()
			| FAnon cf when name = "iterator" && not is_assign ->
				begin match follow cf.cf_type with
					| TFun([],_) ->
						Printf.sprintf "_hx_functools.partial(HxOverrides_iterator, %s)" obj
					| _ ->
						do_default()
				end
			| FAnon cf when name = "shift" && not is_assign ->
				begin match follow cf.cf_type with
					| TFun([],_) ->
						Printf.sprintf "_hx_functools.partial(HxOverrides_shift, %s)" obj
					| _ ->
						do_default()
				end
			| FAnon _ ->
				do_default()
			| FDynamic "iterator" ->
				Printf.sprintf "_hx_functools.partial(HxOverrides_iterator, %s)" obj
			| FDynamic "length" when not is_assign ->
				Printf.sprintf "HxOverrides_length(%s)" obj
			| FDynamic "filter" when not is_assign ->
				Printf.sprintf "_hx_functools.partial(HxOverrides_filter, %s)" obj
			| FDynamic "map" when not is_assign ->
				Printf.sprintf "_hx_functools.partial(HxOverrides_map, %s)" obj
			| _ ->
				do_default()

	and print_try pctx e1 catches =
		let print_catch pctx i (v,e) =
			let indent = pctx.pc_indent in
			let handle_base_type bt =
				let t = print_base_type bt in
				let res = if t = "String" then
					Printf.sprintf "if isinstance(_hx_e1, str):\n%s\t%s = _hx_e1\n%s\t%s" indent v.v_name indent (print_expr {pctx with pc_indent = "\t" ^ pctx.pc_indent} e)
				else
					Printf.sprintf "if isinstance(_hx_e1, %s):\n%s\t%s = _hx_e1\n%s\t%s" t indent v.v_name indent (print_expr {pctx with pc_indent = "\t" ^ pctx.pc_indent} e)
				in
				if i > 0 then
					indent ^ "el" ^ res
				else
					res
			in
			match follow v.v_type with
				| TDynamic _ ->
					Printf.sprintf "%sif True:\n%s\t%s = _hx_e1\n%s\t%s" (if i > 0 then indent else "") indent v.v_name indent (print_expr {pctx with pc_indent = "\t" ^ pctx.pc_indent} e)
				| TInst(c,_) ->
					handle_base_type (t_infos (TClassDecl c))
				| TEnum(en,_) ->
					handle_base_type (t_infos (TEnumDecl en))
				| _ ->
					assert false
		in
		let indent = pctx.pc_indent in
		let print_expr_indented e = print_expr {pctx with pc_indent = "\t" ^ pctx.pc_indent} e in
		let try_str = Printf.sprintf "try:\n%s\t%s\n%s" indent (print_expr_indented e1) indent in
		let except = Printf.sprintf "except Exception as _hx_e:\n%s\t_hx_e1 = _hx_e.val if isinstance(_hx_e, _HxException) else _hx_e\n%s\t" indent indent in
		let catch_str = String.concat (Printf.sprintf "\n%s\n" indent) (ExtList.List.mapi (fun i catch -> print_catch {pctx with pc_indent = "\t" ^ pctx.pc_indent} i catch) catches) in
		let except_end = Printf.sprintf "\n%s\telse:\n%s\t\traise _hx_e" indent indent in
		Printf.sprintf "%s%s%s%s" try_str except catch_str except_end

	and print_call pctx e1 el =
		let id = print_expr pctx e1 in
		match id with
			| "super" ->
				let s_el = print_exprs pctx ", " el in
				Printf.sprintf "super().__init__(%s)" s_el
			| "__python_kwargs__" ->
				"**" ^ (print_expr pctx (List.hd el))
			| "__python_varargs__" ->
				"*" ^ (print_expr pctx (List.hd el))
			| "__python__" ->
				begin match (List.hd el) with
					| {eexpr = TConst (TString s)} -> s
					| e -> print_expr pctx e
				end
			| "__named_arg__" ->
				let name,e2 = match el with
					| [{eexpr = TConst (TString s)};e2] -> s,e2
					| e -> assert false
				in
				Printf.sprintf "%s=%s" name (print_expr pctx e2)
			| "__feature__" ->
				""
			| "__named__" ->
				let res,fields = match List.rev el with
					| {eexpr = TObjectDecl fields} :: el ->
						List.rev el,fields
					| _ ->
						assert false
				in
				begin match res with
					| e1 :: el ->
						Printf.sprintf "%s(%s, %s)" (print_expr pctx e1) (print_exprs pctx ", " el) (print_exprs_named pctx ", " fields)
					| [] ->
						Printf.sprintf "%s(%s)" (print_expr pctx e1) (print_exprs_named pctx ", " fields)
				end
			| "__define_feature__" ->
				print_expr pctx (List.hd el)
			| "__call__" ->
				begin match el with
					| e1 :: el ->
						Printf.sprintf "%s(%s)" (print_expr pctx e1) (print_exprs pctx ", " el)
					| _ ->
						assert false
				end
			| "__field__" ->
				begin match el with
					| [e1;{eexpr = TConst(TString id)}] ->
						Printf.sprintf "%s.%s" (print_expr pctx e1) id
					| _ ->
						assert false
				end
			| "__python_tuple__" ->
				Printf.sprintf "(%s)" (print_exprs pctx ", " el)
			| "__python_array_get__" ->
				let e1, tail = match el with
					| e1::tail -> e1,tail
					| _ -> assert false
				in
				Printf.sprintf "%s[%s]" (print_expr pctx e1) (print_exprs pctx ":" tail)
			| "__python_in__" ->
				begin match el with
					| [e1;e2] ->
						Printf.sprintf "%s in %s" (print_expr pctx e1) (print_expr pctx e2)
					| _ ->
						assert false
				end
			| "__python_for__" ->
				begin match el with
					| [{eexpr = TBlock [{eexpr = TVar(v1,_)};e2;block]}] ->
						let f1 = v1.v_name in
						let pctx = {pctx with pc_indent = "\t" ^ pctx.pc_indent} in
						let i = pctx.pc_indent in
						Printf.sprintf "for %s in %s:\n%s%s" f1 (print_expr pctx e2) i (print_expr pctx block)
					| _ ->
						assert false
				end
			| "__python_del__" ->
				Printf.sprintf "del %s" (print_expr pctx (List.hd el))
			| "__python_binop__" ->
				begin match el with
					| [e0;{eexpr = TConst(TString id)};e2] ->
						Printf.sprintf "%s %s %s" (print_expr pctx e0) id (print_expr pctx e2)
					| _ ->
						assert false
				end
			| "__python_array_set__" ->
				begin match el with
					| [e1;e2;e3] ->
						Printf.sprintf "%s[%s] = %s" (print_expr pctx e1) (print_expr pctx e2) (print_expr pctx e3)
					| _ ->
						assert false
				end
			| "__assert__" ->
				Printf.sprintf "assert(%s)" (print_exprs pctx ", " el)
			| "__new_named__" ->
				begin match el with
					| e1 :: el ->
						Printf.sprintf "new %s(%s)" (print_expr pctx e1) (print_exprs pctx ", " el)
					| _ ->
						assert false
				end
			| "__new__" ->
				begin match el with
					| e1 :: el ->
						Printf.sprintf "%s(%s)" (print_expr pctx e1) (print_exprs pctx ", " el)
					| _ ->
						assert false
				end
			| "__call_global__" ->
				begin match el with
					| {eexpr = TConst(TString s)} :: el ->
						Printf.sprintf "%s(%s)" s (print_exprs pctx ", " el)
					| _ ->
						assert false
				end
			| "__is__" ->
				begin match el with
					| [e1;e2] ->
						Printf.sprintf "%s is %s" (print_expr pctx e1) (print_expr pctx e2)
					| _ ->
						assert false
				end
			| "__as__" ->
				begin match el with
					| [e1;e2] ->
						Printf.sprintf "%s as %s" (print_expr pctx e1) (print_expr pctx e2)
					| _ ->
						assert false
				end
			| "__int_parse__" ->
				Printf.sprintf "int.parse(%s)" (print_expr pctx (List.hd el))
			| "__double_parse__" ->
				Printf.sprintf "double.parse(%s)" (print_expr pctx (List.hd el))
			| "__instanceof__" ->
				begin match el with
					| [e1;e2] ->
						Printf.sprintf "_hx_c.Std._hx_is%s,%s" (print_expr pctx e1) (print_expr pctx e2)
					| _ ->
						assert false
				end
			| "__strict_eq__" ->
				begin match el with
					| [e2;e3] ->
						let e2 = match e2.eexpr with
							| TBinop(OpOr,a,_) -> a
							| _ -> e2
						in
						print_expr pctx {e1 with eexpr = TBinop(OpEq,e2,e3)}
					| _ ->
						assert false
				end
			| _ ->
				Printf.sprintf "%s(%s)" id (print_exprs pctx ", " el)

	and print_exprs pctx sep el =
		String.concat sep (List.map (print_expr pctx) el)

	and print_exprs_named pctx sep fl =
		String.concat sep (List.map (fun (s,e) -> Printf.sprintf "%s = %s" (handle_keywords s) (print_expr pctx e)) fl)

	let handle_keywords s =
		KeywordHandler.handle_keywords s
end

module Generator = struct
	type context = {
		com : Common.context;
		buf : Buffer.t;
		packages : (string,int) Hashtbl.t;
		mutable static_inits : (unit -> unit) list;
		mutable indent_count : int;
		transform_time : float;
		print_time : float;
	}

	type class_field_infos = {
		cfd_fields : string list;
		cfd_props : string list;
		cfd_methods : string list;
	}

	let mk_context com = {
		com = com;
		buf = Buffer.create 16000;
		packages = Hashtbl.create 0;
		static_inits = [];
		indent_count = 0;
		transform_time = 0.;
		print_time = 0.;
	}

	(* Transformer interface *)

	let transform_expr e =
		Transformer.transform e

	let transform_to_value e =
		Transformer.transform_to_value e

	(* Printer interface *)

	let get_path mt =
		Printer.print_base_type mt

	let tfunc_str f pctx name =
		Printer.print_function pctx f name

	let texpr_str e pctx =
		Printer.print_expr pctx e

	let handle_keywords s =
		Printer.handle_keywords s

	(* Helper *)

	let get_full_name mt =
		(* TODO: haxe source is crazy *)
		s_type_path mt.mt_path

	let collect_class_field_data cfl =
		let fields = DynArray.create () in
		let props = DynArray.create () in
		let methods = DynArray.create () in
		List.iter (fun cf ->
			match cf.cf_kind with
				| Var({v_read = AccResolve}) ->
					()
				| Var({v_read = AccCall}) ->
					if Meta.has Meta.IsVar cf.cf_meta then
						DynArray.add fields cf.cf_name
					else
						DynArray.add props cf.cf_name
				| Var _ ->
					DynArray.add fields cf.cf_name
				| _ ->
					DynArray.add methods cf.cf_name
		) cfl;
		{
			cfd_fields = DynArray.to_list fields;
			cfd_props = DynArray.to_list props;
			cfd_methods = DynArray.to_list methods;
		}


	let filter_py_metas metas =
		List.filter (fun (n,_,_) -> match n with Meta.Custom ":python" -> true | _ -> false) metas

	let get_members_with_init_expr c =
		List.filter (fun cf -> match cf.cf_kind with
			| Var({v_read = AccResolve | AccCall _}) -> false
			| Var _ when cf.cf_expr = None -> true
			| _ -> false
		) c.cl_ordered_fields

	let get_meta_entries meta =
		(* TODO *)
		""

	(* Printing *)

	let spr ctx s =
		Buffer.add_string ctx.buf s

	let spr_line ctx s =
		Buffer.add_string ctx.buf s;
		Buffer.add_string ctx.buf "\n"

	let print ctx =
		Printf.kprintf (fun s -> begin
			Buffer.add_string ctx.buf s
		end)

	let newline ctx =
		spr ctx "\n"

	let open_block ctx =
		newline ctx;
		ctx.indent_count <- ctx.indent_count + 1;
		newline ctx

	let close_block ctx =
		ctx.indent_count <- ctx.indent_count - 1;
		newline ctx;
		newline ctx;
		newline ctx

	(* Generating functions *)

	let gen_pre_code_meta ctx metadata =
		try
			begin match Meta.get (Meta.Custom ":preCode") metadata with
				| _,[(EConst(String s)),_],_ -> spr ctx s
				| _ -> raise Not_found
			end
		with Not_found ->
			()

	let gen_py_metas ctx metas indent =
		List.iter (fun (n,el,_) ->
			match el with
				| [EConst(String s),_] ->
					print ctx "%s@%s\n" indent s
				| _ ->
					assert false
		) metas

	let gen_expr ctx e field indent =
		let pctx = Printer.create_context ("\t" ^ indent) in
		let e = match e.eexpr with
			| TFunction(f) ->
				{e with eexpr = TBlock [e]}
			| _ ->
				e
		in
		let expr2 = transform_to_value e in
		let name = "_hx_init_" ^ (String.concat "_" (ExtString.String.nsplit field ".")) in
		let maybe_split_expr expr2 = match expr2.eexpr with
			| TBlock es when es <> [] && field <> "" ->
				begin match List.rev es with
					| e_last :: el ->
						let new_last = {e_last with eexpr = TReturn (Some e_last)} in
						let new_block = {expr2 with eexpr = TBlock (List.rev (new_last :: el))} in
						let v_name = alloc_var name (tfun [] e_last.etype) in
						let f_name = mk (TLocal v_name) v_name.v_type e_last.epos in
						let call_f = mk (TCall(f_name,[])) e_last.etype e_last.epos in
						Some new_block,call_f
					| _ ->
						assert false
				end
			| _ ->
				None,expr2
		in
		let r = maybe_split_expr expr2 in
		match r with
			| Some e1,e2 ->
				let expr_string_1 = texpr_str e1 pctx in
				let expr_string_2 = texpr_str e2 pctx in
				print ctx "%sdef %s():\n\t%s\n" indent name expr_string_1;
				print ctx "%s%s = %s" indent field expr_string_2;
			| None,e2 ->
				let expr_string_2 = texpr_str e2 pctx in
				if field = "" then
					spr ctx expr_string_2
				else
					print ctx "%s%s = %s" indent field expr_string_2

	let gen_func_expr ctx e c name metas extra_args indent stat =
		let pctx = Printer.create_context indent in
		let e = match e.eexpr with
			| TFunction(f) ->
				let args = List.map (fun s ->
					alloc_var s t_dynamic,None
				) extra_args in
				{e with eexpr = TFunction {f with tf_args = args @ f.tf_args}}
			| _ ->
				e
		in
		let expr1 = transform_expr e in
		let field_name = if stat then
			Printf.sprintf "%s_statics_%s" (snd c.cl_path) name
		else
			name
		in
		let expr_string = match expr1.eexpr with
			| TFunction f ->
				tfunc_str f pctx (Some field_name)
			| _ ->
				Printf.sprintf "%s = %s" field_name (texpr_str expr1 pctx)
		in
		gen_py_metas ctx metas indent;
		spr ctx indent;
		spr ctx expr_string;
		if stat then begin
			newline ctx;
			print ctx "%s.%s = %s" (get_path (t_infos (TClassDecl c))) name field_name
		end

	let gen_class_constructor ctx c = match c.cl_constructor with
		| None ->
			()
		| Some cf ->
			let member_inits = get_members_with_init_expr c in
			newline ctx;
			let py_metas = filter_py_metas cf.cf_meta in
			begin match member_inits,cf.cf_expr with
				| _,Some ({eexpr = TFunction f} as ef) ->
					let ethis = mk (TConst TThis) (TInst(c,List.map snd c.cl_types)) cf.cf_pos in
					let member_data = List.map (fun cf ->
						let ef = mk (TField(ethis,FDynamic cf.cf_name)) cf.cf_type cf.cf_pos in
						mk (TBinop(OpAssign,ef,null ef.etype ef.epos)) ef.etype ef.epos
					) member_inits in
					let e = {f.tf_expr with eexpr = TBlock (member_data @ [f.tf_expr])} in
					cf.cf_expr <- Some {ef with eexpr = TFunction {f with tf_expr = e}};
				| _ ->
					(* TODO: is this correct? *)
					()
			end;
			gen_func_expr ctx (match cf.cf_expr with None -> assert false | Some e -> e) c "__init__" py_metas ["self"] "\t" false;
			newline ctx

	let gen_class_field ctx c p cf =
		let field = handle_keywords cf.cf_name in
		begin match cf.cf_expr with
			| None ->
				print ctx "\t# var %s" field
			| Some e ->
				match cf.cf_kind with
					| Method _ ->
						let py_metas = filter_py_metas cf.cf_meta in
						gen_func_expr ctx e c field py_metas ["self"] "\t" false;
						newline ctx
					| _ ->
						gen_expr ctx e (Printf.sprintf "# var %s" field) "\t";
						newline ctx
		end;
		newline ctx

	let gen_static_field ctx c p cf =
		let p = get_path (t_infos (TClassDecl c)) in
		let field = handle_keywords cf.cf_name in
		match cf.cf_expr with
			| None ->
				print ctx "%s.%s = None;\n" p field
			| Some e ->
				match cf.cf_kind with
					| Method _ ->
						let py_metas = filter_py_metas cf.cf_meta in
						gen_func_expr ctx e c field py_metas [] "" true;
						newline ctx
					| _ ->
						gen_expr ctx e (Printf.sprintf "%s.%s" p field) "";
						newline ctx

	let gen_class_data ctx c cfd p_super p_interfaces p p_name =
		let field_str = String.concat "," (List.map (fun s -> "\"" ^ s ^ "\"") cfd.cfd_fields) in
		let props_str = String.concat "," (List.map (fun s -> "\"" ^ s ^ "\"") cfd.cfd_props) in
		let method_str = String.concat "," (List.map (fun s -> "\"" ^ s ^ "\"") cfd.cfd_methods) in
		newline ctx;
		print ctx "%s._hx_class = %s\n" p p;
		print ctx "%s._hx_class_name = \"%s\"\n" p p_name;
		print ctx "_hx_classes[\"%s\"] = %s\n" p_name p;
		print ctx "_hx_c.%s = %s\n" p p;
		print ctx "%s._hx_fields = [%s]\n" p field_str;
		print ctx "%s._hx_props = [%s]\n" p props_str;
		print ctx "%s._hx_methods = [%s]\n" p method_str;
		(* TODO: statics *)
		print ctx "%s._hx_interfaces = [%s]\n" p (String.concat "," p_interfaces);
		match p_super with
			| None ->
				()
			| Some ps ->
				print ctx "%s._hx_super = %s\n" p ps

	let gen_meta_members ctx fields =
		spr ctx "_hx_c._hx_AnonObject(";
		List.iter (fun cf ->
			()
		) fields;
		spr ctx ")"

	let gen_class_metadata ctx c p =
		let meta = Codegen.build_metadata ctx.com (TClassDecl c) in
		match meta with
			| None ->
				()
			| Some e ->
				gen_expr ctx e "" "";
				newline ctx

	let gen_enum_metadata ctx en p =
		let meta = Codegen.build_metadata ctx.com (TEnumDecl en) in
		match meta with
			| None ->
				()
			| Some e ->
				gen_expr ctx e "" "";
				newline ctx

	let gen_class_empty_constructor ctx p cfl =
		let s_name = p ^ "_hx_empty_init" in
		print ctx "def %s (_hx_o):\n" s_name;
		let found_fields = ref false in
		List.iter (fun cf -> match cf.cf_kind with
				| Var ({v_read = AccResolve | AccCall}) ->
					()
				| Var _ ->
					found_fields := true;
					print ctx "\t_hx_o.%s = None\n" (handle_keywords cf.cf_name)
				| _ ->
					()
		) cfl;
		if not !found_fields then
			spr ctx "\tpass\n";
		print ctx "%s._hx_empty_init = %s\n" p s_name

	let gen_class_statics ctx c p =
		let f = fun () ->
			List.iter (fun cf -> gen_static_field ctx c p cf) c.cl_ordered_statics;
			spr ctx "\n";
		in
		ctx.static_inits <- f :: ctx.static_inits

	let gen_class_init ctx c =
		match c.cl_init with
			| None ->
				()
			| Some e ->
				let f = fun () ->
					let e = transform_expr e in
					spr_line ctx (texpr_str e (Printer.create_context ""));
				in
				ctx.static_inits <- f :: ctx.static_inits

	let gen_class ctx c =
		gen_pre_code_meta ctx c.cl_meta;
		print ctx "# print %s.%s\n" (s_type_path c.cl_module.m_path) (snd c.cl_path);
		if not c.cl_extern then begin
			let mt = (t_infos (TClassDecl c)) in
			let p = get_path mt in
			let p_name = get_full_name mt in
			print ctx "class %s" p;
			let p_super = match c.cl_super with
				| None ->
					None
				| Some (csup,_) ->
					let p = get_path (t_infos (TClassDecl csup)) in
					print ctx "(%s)" p;
					Some p
			in
			let p_interfaces = List.map (fun (c,tl) ->
				get_path (t_infos (TClassDecl c))
			) c.cl_implements in
			spr ctx ":";
			open_block ctx;
			gen_class_constructor ctx c;
			List.iter (fun cf -> gen_class_field ctx c p cf) c.cl_ordered_fields;
			let x = collect_class_field_data c.cl_ordered_fields in
			let use_pass = match x.cfd_methods with
				| [] -> c.cl_constructor = None
				| _ -> c.cl_interface
			in
			if use_pass then spr_line ctx "\tpass";
			close_block ctx;
			gen_class_data ctx c x p_super p_interfaces p p_name;
			gen_class_metadata ctx c p;
			gen_class_empty_constructor ctx p c.cl_ordered_fields;
			gen_class_statics ctx c p;
		end;
		gen_class_init ctx c

	let gen_enum ctx en =
		let mt = (t_infos (TEnumDecl en)) in
		let p = get_path mt in
		let p_name = get_full_name mt in
		print ctx "class %s(_hx_c.Enum):\n" p;
		spr ctx "\tdef __init__(self, t, i, p):\n";
		print ctx "\t\tsuper(%s,self).__init__(t, i, p)" p;
		newline ctx;
		let enum_constructs = PMap.foldi (fun k ef acc ->
			let f = handle_keywords ef.ef_name in
			begin match follow ef.ef_type with
				| TFun(args,_) ->
					let param_str = (String.concat "," (List.map (fun (n,o,_) -> Printf.sprintf "%s%s" (handle_keywords n) (if o then " = None" else "")) args)) in
					let args_str = String.concat "," (List.map (fun (n,_,_) -> handle_keywords n) args) in
					print ctx "def _%s_statics_%s (%s):\n" p f param_str;
					print ctx "\treturn %s(\"%s\", %i, [%s])\n" p ef.ef_name ef.ef_index args_str;
					print ctx "%s.%s = _%s_statics_%s\n" p f p f;
				| _ ->
					(* TODO: haxe source has api.quoteString for ef.ef_name *)
					print ctx "%s.%s = %s(\"%s\", %i, list())\n" p f p ef.ef_name ef.ef_index
			end;
			newline ctx;
			ef :: acc
		) en.e_constrs [] in
		let fix = match enum_constructs with [] -> "" | _ -> "\"" in
		let enum_constructs = List.sort (fun a b -> if a.ef_index < b.ef_index then -1 else if a.ef_index > b.ef_index then 1 else 0) enum_constructs in
		let enum_constructs_str = fix ^ (String.concat ("\",\"") (List.map (fun ef -> ef.ef_name) enum_constructs)) ^ fix in
		print ctx "%s._hx_constructs = [%s]\n" p enum_constructs_str;
		print ctx "%s._hx_class = %s\n" p p;
		print ctx "%s._hx_class_name = \"%s\"\n" p p_name;
		print ctx "_hx_classes[\"%s\"] = %s\n" p_name p;
		print ctx "_hx_c.%s = %s\n" p p;
		gen_enum_metadata ctx en p

	let gen_type ctx mt = match mt with
		| TClassDecl c -> gen_class ctx c
		| TEnumDecl en -> gen_enum ctx en
		| _ -> ()

	(* Generator parts *)

	let gen_resources ctx =
		if Hashtbl.length ctx.com.resources > 0 then begin
			spr ctx "def _hx_resources__():\n\treturn {";
			let first = ref true in
			Hashtbl.iter (fun k v ->
				let prefix = if !first then begin
					first := true;
					"";
				end else
					","
				in
				print ctx "%s'%s':'%s'" prefix k v
			) ctx.com.resources;
			spr ctx "}\n"
		end

	let gen_boot_code ctx =
		(* TODO: ... *)
		spr ctx bootcode

	let gen_boot_class ctx =
		let boot = List.find (fun mt -> match mt with
			| TClassDecl {cl_path = ["python"],"Boot"} -> true
			| _ -> false
		) ctx.com.types in
		gen_type ctx boot

	let gen_types ctx =
		List.iter (fun mt -> match mt with
			| TClassDecl {cl_path = ["python"],"Boot"} ->
				()
			| _ ->
				gen_type ctx mt
		) ctx.com.types

	let gen_static_inits ctx =
		List.iter (fun f -> f()) (List.rev ctx.static_inits)

	let gen_main ctx =
		match ctx.com.main with
			| None ->
				()
			| Some e ->
				gen_expr ctx e "" ""

	(* Entry point *)

	let run com =
		let ctx = mk_context com in
		gen_resources ctx;
		gen_boot_code ctx;
		gen_boot_class ctx;
		gen_types ctx;
		gen_static_inits ctx;
		gen_main ctx;

		let ch = open_out_bin com.file in
		output_string ch (Buffer.contents ctx.buf);
		close_out ch
end

let generate com =
	Generator.run com