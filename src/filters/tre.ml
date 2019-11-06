open Type
open Typecore
open Globals

let null_var = {
	v_id = -1;
	v_name = "null_var";
	v_type = t_dynamic;
	v_kind = VGenerated;
	v_capture = false;
	v_final = false;
	v_extra = None;
	v_meta = [];
	v_pos = null_pos;
}

let rec collect_new_args_values ctx args declarations values n =
	match args with
	| [] -> declarations, values
	| arg :: rest ->
		let v = alloc_var VGenerated ("`tmp" ^ (string_of_int n)) arg.etype arg.epos in
		let decl = { eexpr = TVar (v, Some arg); etype = ctx.t.tvoid; epos = v.v_pos }
		and value = { arg with eexpr = TLocal v } in
		collect_new_args_values ctx rest (decl :: declarations) (value :: values) (n + 1)

let rec assign_args vars exprs =
	match vars, exprs with
	| [], [] -> []
	| (v, _) :: rest_vars, e :: rest_exprs
	| (v, Some e) :: rest_vars, rest_exprs ->
		let arg = { e with eexpr = TLocal v } in
		{ e with eexpr = TBinop (OpAssign, arg, e) } :: assign_args rest_vars rest_exprs
	| _ -> assert false

let replacement_for_TReturn ctx fn args add_continue p =
	let temps_rev, args_rev = collect_new_args_values ctx args [] [] 0
	and continue = if add_continue then [mk TContinue ctx.t.tvoid Globals.null_pos] else [] in
	{
		etype = ctx.t.tvoid;
		epos = p;
		eexpr = TMeta ((Meta.TailRecursion, [], null_pos), {
			eexpr = TBlock ((List.rev temps_rev) @ (assign_args fn.tf_args (List.rev args_rev)) @ continue);
			etype = ctx.t.tvoid;
			epos = p;
		});
	}

let collect_captured_args args e =
	let result = ref [] in
	let rec loop in_closure e =
		match e.eexpr with
		| TLocal ({ v_kind = VUser TVOArgument } as v) when in_closure && not (List.memq v !result) && List.memq v args ->
			result := v :: !result
		| TFunction { tf_expr = e } ->
			loop true e
		| _ ->
			iter (loop in_closure) e
	in
	loop false e;
	!result

let rec redeclare_vars ctx vars declarations replace_list =
	match vars with
	| [] -> declarations, replace_list
	| v :: rest ->
		let new_v = alloc_var VGenerated ("`" ^ v.v_name) v.v_type v.v_pos in
		let decl =
			{
				eexpr = TVar (new_v, Some { eexpr = TLocal v; etype = v.v_type; epos = v.v_pos; });
				etype = ctx.t.tvoid;
				epos = v.v_pos;
			}
		in
		redeclare_vars ctx rest (decl :: declarations) ((v, new_v) :: replace_list)

let rec replace_vars replace_list in_tail_recursion e =
	match e.eexpr with
	| TBinop (OpAssign, ({ eexpr = TLocal { v_kind = VUser TVOArgument } } as arg), value) when in_tail_recursion ->
		let value = replace_vars replace_list in_tail_recursion value in
		{ e with eexpr = TBinop (OpAssign, arg, value) }
	| TLocal v ->
		(try
			let v = List.assq v replace_list in
			{ e with eexpr = TLocal v }
		with Not_found ->
			e
		)
	| TMeta ((Meta.TailRecursion, _, _), _) -> map_expr (replace_vars replace_list true) e
	| _ -> map_expr (replace_vars replace_list in_tail_recursion) e

let wrap_loop ctx args body =
	let wrap e =
		let cond = mk (TConst (TBool true)) ctx.t.tbool Globals.null_pos in
		{ e with eexpr = TWhile (cond, e, Ast.NormalWhile) }
	in
	match collect_captured_args args body with
	| [] -> wrap body
	| captured_args ->
		let declarations, replace_list = redeclare_vars ctx captured_args [] [] in
		wrap { body with eexpr = TBlock (declarations @ [replace_vars replace_list false body]) }

let fn_args_vars fn = List.map (fun (v,_) -> v) fn.tf_args

let is_recursive_named_local_call fn_var callee args =
	match callee.eexpr with
	(* named local function*)
	| TLocal v ->
		v == fn_var
	| _ -> false

let is_recursive_method_call field callee args =
	match callee.eexpr, args with
	(* member abstract function*)
	| TField (_, FStatic (_, cf)), { eexpr = TLocal v } :: _ when has_meta Meta.Impl cf.cf_meta ->
		cf == field && has_meta Meta.This v.v_meta
	(* static method *)
	| TField (_, FStatic (_, cf)), _
	(* instance method *)
	| TField ({ eexpr = TConst TThis }, FInstance (_, _, cf)), _ ->
		cf == field
	| _ -> false

let rec transform_function ctx is_recursive_call fn =
	let add_loop = ref false in
	let rec transform_expr in_loop function_end force_continue e =
		match e.eexpr with
		| TWhile _ | TFor _ ->
			map_expr (transform_expr true false false) e
		(* named local function *)
		| TBinop (OpAssign, ({ eexpr = TLocal ({ v_kind = VUser TVOLocalFunction } as v) } as e_var), ({ eexpr = TFunction fn } as e_fn)) ->
			let fn = transform_function ctx (is_recursive_named_local_call v) fn in
			{ e with eexpr = TBinop (OpAssign, e_var, { e_fn with eexpr = TFunction fn }) }
		(* anonymous function *)
		| TFunction _ ->
			e
		(* return a recursive call to current function *)
		| TReturn (Some { eexpr = TCall (callee, args) }) when not in_loop && is_recursive_call callee args ->
			add_loop := true;
			replacement_for_TReturn ctx fn args (not function_end || force_continue) e.epos
		| TReturn (Some e_return) ->
			{ e with eexpr = TReturn (Some (transform_expr in_loop function_end true e_return)) }
		| TBlock exprs ->
			let rec loop exprs =
				match exprs with
				| [] -> []
				| [{ eexpr = TCall (callee, args) } as e] when not in_loop && function_end && is_recursive_call callee args ->
					add_loop := true;
					[replacement_for_TReturn ctx fn args (not function_end || force_continue) e.epos]
				| { eexpr = TCall (callee, args) } :: [{ eexpr = TReturn None }] when not in_loop && is_recursive_call callee args ->
					add_loop := true;
					[replacement_for_TReturn ctx fn args (not function_end || force_continue) e.epos]
				| e :: rest ->
					let function_end = function_end && rest = [] in
					transform_expr in_loop function_end force_continue e :: loop rest
			in
			{ e with eexpr = TBlock (loop exprs) }
		| _ ->
			map_expr (transform_expr in_loop function_end force_continue) e
	in
	let body = transform_expr false true false fn.tf_expr in
	let body = if !add_loop then wrap_loop ctx (fn_args_vars fn) body else body in
	{ fn with tf_expr = body }

let rec has_tail_recursion is_recursive_call in_loop function_end e =
	match e.eexpr with
	| TFor _ | TWhile _ ->
		check_expr (has_tail_recursion is_recursive_call true false) e
	(* named local function *)
	| TBinop (OpAssign, { eexpr = TLocal ({ v_kind = VUser TVOLocalFunction } as v) }, { eexpr = TFunction fn }) ->
		has_tail_recursion (is_recursive_named_local_call v) false true fn.tf_expr
	(* anonymous function *)
	| TFunction _ ->
		false
	| TReturn (Some { eexpr = TCall (callee, args)}) ->
		not in_loop && is_recursive_call callee args
	| TBlock exprs ->
		let rec loop exprs =
			match exprs with
			| [] -> false
			| [{ eexpr = TCall (callee, args) }] when not in_loop && function_end ->
				is_recursive_call callee args
			| { eexpr = TCall (callee, args) } :: [{ eexpr = TReturn None }] when not in_loop ->
				is_recursive_call callee args
			| e :: rest ->
				let function_end = function_end && rest = [] in
				has_tail_recursion is_recursive_call in_loop function_end e
				|| loop rest
		in
		loop exprs
	| _ ->
		check_expr (has_tail_recursion is_recursive_call in_loop function_end) e

let run ctx e =
	match e.eexpr with
	| TFunction fn when has_tail_recursion (is_recursive_method_call ctx.curfield) false true fn.tf_expr ->
		(* transform_method ctx fn e *)
		let fn = transform_function ctx (is_recursive_method_call ctx.curfield) fn in
		{ e with eexpr = TFunction fn }
	| _ -> e