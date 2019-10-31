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

let replacement_for_TReturn ctx fn args p =
	let temps_rev, args_rev = collect_new_args_values ctx args [] [] 0
	and continue = mk TContinue ctx.t.tvoid Globals.null_pos in
	{
		etype = ctx.t.tvoid;
		epos = p;
		eexpr = TMeta ((Meta.TailRecursion, [], null_pos), {
			eexpr = TBlock ((List.rev temps_rev) @ (assign_args fn.tf_args (List.rev args_rev)) @ [continue]);
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

let rec transform_named_local_function ctx e fn_var fn =
	let add_loop = ref false in
	let rec transform e =
		match e.eexpr with
		(* named local function *)
		| TBinop (OpAssign, { eexpr = TLocal ({ v_kind = VUser TVOLocalFunction } as v) }, { eexpr = TFunction fn }) ->
			transform_named_local_function ctx e v fn
		(* anonymous function *)
		| TFunction _ ->
			e
		(* return a call to this local function *)
		| TReturn (Some { eexpr = TCall ({ eexpr = TLocal v }, args) }) when v == fn_var ->
			add_loop := true;
			replacement_for_TReturn ctx fn args e.epos
		| _ ->
			map_expr transform e
	in
	let body = transform fn.tf_expr in
	let body = if !add_loop then wrap_loop ctx (fn_args_vars fn) body else body in
	{ e with
		eexpr = TBinop (
			OpAssign,
			{ e with eexpr = TLocal fn_var },
			{ e with eexpr = TFunction { fn with tf_expr = body } }
		)
	}

let transform_method ctx fn field_expr =
	let field = ctx.curfield in
	let add_loop = ref false in
	let rec transform in_loop e =
		match e.eexpr with
		| TFor _ | TWhile _ ->
			map_expr (transform true) e
		(* named local function *)
		| TBinop (OpAssign, { eexpr = TLocal ({ v_kind = VUser TVOLocalFunction } as v) }, { eexpr = TFunction fn }) ->
			transform_named_local_function ctx e v fn
		(* anonymous function *)
		| TFunction _ ->
			e
		(* return a call to a member abstract function*)
		| TReturn (Some { eexpr = TCall ({ eexpr = TField (_, FStatic (_, f)) }, ({ eexpr = TLocal v } :: _ as args)) })
		when f == field && not in_loop && has_meta Meta.Impl f.cf_meta ->
			if has_meta Meta.This v.v_meta then begin
				add_loop := true;
				replacement_for_TReturn ctx fn args e.epos
			end else
				e
		(* return a call to an instance method *)
		| TReturn (Some { eexpr = TCall ({ eexpr = TField ({ eexpr = TConst TThis }, FInstance (_, _, f)) }, args) })
		(* return a call to a static method *)
		| TReturn (Some { eexpr = TCall ({ eexpr = TField (_, FStatic (_, f)) }, args) })
		when f == field && not in_loop ->
			add_loop := true;
			replacement_for_TReturn ctx fn args e.epos
		| _ ->
			map_expr (transform in_loop) e
	in
	let body = transform false fn.tf_expr in
	let body = if !add_loop then wrap_loop ctx (fn_args_vars fn) body else body in
	{ field_expr with eexpr = TFunction { fn with tf_expr = body } }

let rec has_tail_recursion ctx field fn_var in_loop e =
	match e.eexpr with
	| TFor _ | TWhile _ ->
		check_expr (has_tail_recursion ctx field fn_var true) e
	(* named local function *)
	| TBinop (OpAssign, { eexpr = TLocal ({ v_kind = VUser TVOLocalFunction } as v) }, { eexpr = TFunction fn }) ->
		has_tail_recursion ctx null_field v false fn.tf_expr
	(* anonymous function *)
	| TFunction _ ->
		false
	(* return a call to a named local function*)
	| TReturn (Some { eexpr = TCall ({ eexpr = TLocal v }, _) }) -> v == fn_var && not in_loop
	(* return a call to a member abstract function*)
	| TReturn (Some { eexpr = TCall ({ eexpr = TField (_, FStatic (_, f)) }, { eexpr = TLocal v } :: _) })
	when has_meta Meta.Impl f.cf_meta ->
		f == field && not in_loop && has_meta Meta.This v.v_meta
	(* return a call to an instance method *)
	| TReturn (Some { eexpr = TCall ({ eexpr = TField ({ eexpr = TConst TThis }, FInstance (_, _, f)) }, _) })
	(* return a call to a static method *)
	| TReturn (Some { eexpr = TCall ({ eexpr = TField (_, FStatic (_, f)) }, _) }) ->
		f == field && not in_loop
	| _ ->
		check_expr (has_tail_recursion ctx field fn_var in_loop) e

let run ctx e =
	match e.eexpr with
	| TFunction fn when has_tail_recursion ctx ctx.curfield null_var false fn.tf_expr ->
		transform_method ctx fn e
	| _ -> e