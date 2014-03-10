open Ast
open Type
open Common

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

	let add_non_locals_to_func e =
		e

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
		if is_value then begin
			let new_name = ae.a_next_id() in
			let new_var = alloc_var new_name tf.tf_type in
			let new_local = mk (TLocal new_var) fn.etype p in
			let def = mk (TVar(new_var,Some fn)) fn.etype p in
			lift_expr ~next_id:(Some ae.a_next_id) ~blocks:[def] new_local
		end else
			lift_expr fn

	and transform_var_expr ae eo v =
		let b,new_expr = match eo with
			| None ->
				[],None
			| Some e1 ->
				let f = transform_expr ~is_value:true ~next_id:(Some ae.a_next_id) e1 in
				let b = f.a_blocks in
				b,Some(f.a_expr)
		in
		let e = mk (TVar(v,new_expr)) ae.a_expr.etype ae.a_expr.epos in
		lift_expr ~next_id:(Some ae.a_next_id) ~blocks:b e

	and transform1 ae : adjusted_expr = match ae.a_is_value,ae.a_expr.eexpr with
		| (is_value,TBlock [x]) ->
			transform_expr ~is_value:is_value ~next_id:(Some ae.a_next_id) x
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
			let e1 = transform_expr ~is_value:true ~next_id:(Some ae.a_next_id) e1 in
			let e2 = to_expr (transform_expr ~is_value:false ~next_id:(Some ae.a_next_id) e2) in
			let new_expr = mk (TFor(v,e1.a_expr,e2)) ae.a_expr.etype ae.a_expr.epos in
			lift_expr ~blocks:e1.a_blocks new_expr
		| (_,TReturn None) ->
			ae
		| (_,TReturn (Some ({eexpr = TFunction f} as ef))) ->
			let n = ae.a_next_id() in
			let e1 = to_expr (transform_expr ~next_id:(Some ae.a_next_id) f.tf_expr) in
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
			lift_expr ~is_value:true ~next_id:(Some ae.a_next_id) ~blocks:[f1_assign] er

	and transform_expr ?(is_value = false) ?(next_id = None) ?(blocks = []) (e : texpr) : adjusted_expr =
		transform1 (lift_expr ~is_value ~next_id ~blocks e)

	and transform_exprs_to_block el tb is_value p next_id =
		match el with
			| [e] ->
				transform_expr ~is_value ~next_id:(Some next_id) e
			| _ ->
				let res = ref [] in
				List.iter (fun e ->
					(* TODO: urgh *)
					let ae = transform_expr ~is_value ~next_id:(Some next_id) e in
					res := ae.a_expr :: !res;
					res := ae.a_blocks @ !res;
				) el;
				lift_expr (mk (TBlock (List.rev !res)) tb p)

	let transform e =
		to_expr (transform1 (lift_expr e))

end

let generate com =
	()