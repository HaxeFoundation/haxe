open Globals
open Type

let fix_return_dynamic_from_void_function _ e =
	let rec loop return_is_void e = match e.eexpr with
		| TFunction fn ->
			let is_void = ExtType.is_void (follow fn.tf_type) in
			let body = loop is_void fn.tf_expr in
			{ e with eexpr = TFunction { fn with tf_expr = body } }
		| TReturn (Some return_expr) when return_is_void && t_dynamic == follow return_expr.etype ->
			let return_pos = { e.epos with pmax = return_expr.epos.pmin - 1 } in
			let exprs = [
				loop return_is_void return_expr;
				{ e with eexpr = TReturn None; epos = return_pos };
			] in
			{ e with
				eexpr = TMeta (
					(Meta.MergeBlock, [], null_pos),
					mk (TBlock exprs) e.etype e.epos
				);
			}
		| _ -> Type.map_expr (loop return_is_void) e
	in
	loop true e

let check_abstract_as_value _ e =
	let rec loop e =
		match e.eexpr with
		| TField ({ eexpr = TTypeExpr _ }, _) -> ()
		| TTypeExpr(TClassDecl {cl_kind = KAbstractImpl a}) when not (Meta.has Meta.RuntimeValue a.a_meta) ->
			Error.raise_typing_error "Cannot use abstract as value" e.epos
		| _ -> Type.iter loop e
	in
	loop e;
	e

let mark_switch_break_loops _ e =
	let add_loop_label n e =
		{ e with eexpr = TMeta ((Meta.LoopLabel,[(EConst(Int(string_of_int n, None)),e.epos)],e.epos), e) }
	in
	let in_switch = ref false in
	let did_found = ref (-1) in
	let num = ref 0 in
	let cur_num = ref 0 in
	let rec run e =
		match e.eexpr with
		| TFunction _ ->
			let old_num = !num in
			num := 0;
				let ret = Type.map_expr run e in
			num := old_num;
			ret
		| TWhile _ ->
			let last_switch = !in_switch in
			let last_found = !did_found in
			let last_num = !cur_num in
			in_switch := false;
			incr num;
			cur_num := !num;
			did_found := -1;
				let new_e = Type.map_expr run e in (* assuming that no loop will be found in the condition *)
				let new_e = if !did_found <> -1 then add_loop_label !did_found new_e else new_e in
			did_found := last_found;
			in_switch := last_switch;
			cur_num := last_num;

			new_e
		| TSwitch _ ->
			let last_switch = !in_switch in
			in_switch := true;
				let new_e = Type.map_expr run e in
			in_switch := last_switch;
			new_e
		| TBreak ->
			if !in_switch then (
				did_found := !cur_num;
				add_loop_label !cur_num e
			) else
				e
		| _ -> Type.map_expr run e
	in
	run e