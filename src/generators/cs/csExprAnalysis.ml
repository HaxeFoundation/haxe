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

(* Expression and control flow analysis utilities for C# code generation.
   These functions analyze Haxe expressions and C# AST nodes for various
   purposes like detecting `this` usage, finding return types, checking
   for side effects, and analyzing control flow. *)

open Globals
open Type
open CsAst
open CsGlobals

(* ============================================================
   Haxe Expression Analysis
   ============================================================ *)

(* Check if a Haxe expression contains a reference to `this`.
   Used to detect when field initializers need to be moved to constructors,
   since C# doesn't allow `this` in field initializers. *)
let rec expr_contains_this e =
	match e.eexpr with
	| TConst TThis -> true
	| TLocal _ | TConst _ | TTypeExpr _ | TIdent _ -> false
	| TArray (e1, e2) ->
		expr_contains_this e1 || expr_contains_this e2
	| TBinop (_, e1, e2) ->
		expr_contains_this e1 || expr_contains_this e2
	| TUnop (_, _, e1) -> expr_contains_this e1
	| TField (e1, _) -> expr_contains_this e1
	| TParenthesis e1 -> expr_contains_this e1
	| TMeta (_, e1) -> expr_contains_this e1
	| TCast (e1, _) -> expr_contains_this e1
	| TEnumParameter (e1, _, _) -> expr_contains_this e1
	| TEnumIndex e1 -> expr_contains_this e1
	| TCall (e1, el) ->
		expr_contains_this e1 || List.exists expr_contains_this el
	| TNew (_, _, el) ->
		List.exists expr_contains_this el
	| TObjectDecl fields ->
		List.exists (fun (_, e1) -> expr_contains_this e1) fields
	| TArrayDecl el ->
		List.exists expr_contains_this el
	| TBlock el ->
		List.exists expr_contains_this el
	| TIf (e1, e2, e3_opt) ->
		expr_contains_this e1 || expr_contains_this e2 ||
		(match e3_opt with Some e3 -> expr_contains_this e3 | None -> false)
	| TWhile (e1, e2, _) ->
		expr_contains_this e1 || expr_contains_this e2
	| TSwitch sw ->
		expr_contains_this sw.switch_subject ||
		List.exists (fun case -> List.exists expr_contains_this case.case_patterns || expr_contains_this case.case_expr) sw.switch_cases ||
		(match sw.switch_default with Some e1 -> expr_contains_this e1 | None -> false)
	| TTry (e1, catches) ->
		expr_contains_this e1 ||
		List.exists (fun (_, e1) -> expr_contains_this e1) catches
	| TVar (_, init_opt) ->
		(match init_opt with Some e1 -> expr_contains_this e1 | None -> false)
	| TReturn e_opt ->
		(match e_opt with Some e1 -> expr_contains_this e1 | None -> false)
	| TThrow e1 -> expr_contains_this e1
	| TBreak | TContinue -> false
	| TFunction tf ->
		expr_contains_this tf.tf_expr

(* Find the type of the first non-void return expression inside an expression tree.
   Returns None if no return with value is found, Some type if found.
   This is used to determine if an IIFE should use Func<T> instead of Action. *)
let rec find_return_type e =
	match e.eexpr with
	| TReturn (Some ret_e) when not (ExtType.is_void (follow ret_e.etype)) ->
		Some ret_e.etype
	| TReturn _ -> None
	| TFunction _ -> None  (* Don't recurse into nested functions *)
	| TBlock el ->
		List.fold_left (fun acc e1 ->
			match acc with Some _ -> acc | None -> find_return_type e1
		) None el
	| TIf (_, e1, e2_opt) ->
		begin match find_return_type e1 with
		| Some t -> Some t
		| None ->
			match e2_opt with
			| Some e2 -> find_return_type e2
			| None -> None
		end
	| TWhile (_, body, _) -> find_return_type body
	| TSwitch sw ->
		let check_case acc case =
			match acc with Some _ -> acc | None -> find_return_type case.case_expr
		in
		let result = List.fold_left check_case None sw.switch_cases in
		begin match result with
		| Some _ -> result
		| None ->
			match sw.switch_default with
			| Some def -> find_return_type def
			| None -> None
		end
	| TTry (e1, catches) ->
		begin match find_return_type e1 with
		| Some t -> Some t
		| None ->
			List.fold_left (fun acc (_, catch_e) ->
				match acc with Some _ -> acc | None -> find_return_type catch_e
			) None catches
		end
	| _ -> None

(* Check if expression contains statements (TVar, TBlock, etc.) that can't appear in C# base() call.
   In C#, the base() call in `: base(args)` can only contain expressions, not statements.
   If super() args contain TVar declarations or blocks, we need two-phase construction. *)
let rec expr_contains_statements e =
	match e.eexpr with
	| TVar _ -> true  (* Local var declaration is a statement *)
	| TBlock (_ :: _) -> true  (* Non-empty block contains statements *)
	| TWhile _ | TTry _ | TSwitch _ | TIf _ -> true  (* Control flow is statement-like *)
	| TConst _ | TLocal _ | TTypeExpr _ | TIdent _ -> false
	| TArray (e1, e2) -> expr_contains_statements e1 || expr_contains_statements e2
	| TBinop (_, e1, e2) -> expr_contains_statements e1 || expr_contains_statements e2
	| TField (e1, _) | TParenthesis e1 | TMeta (_, e1) | TCast (e1, _) | TUnop (_, _, e1) ->
		expr_contains_statements e1
	| TCall (e1, args) -> expr_contains_statements e1 || List.exists expr_contains_statements args
	| TNew (_, _, args) -> List.exists expr_contains_statements args
	| TArrayDecl el -> List.exists expr_contains_statements el
	| TEnumParameter (e1, _, _) -> expr_contains_statements e1
	| TObjectDecl fields -> List.exists (fun (_, e1) -> expr_contains_statements e1) fields
	| TFunction _ -> false  (* Lambda itself is just a value *)
	| TThrow e1 -> expr_contains_statements e1
	| TReturn (Some e1) -> expr_contains_statements e1
	| TReturn None | TBreak | TContinue -> false
	| TBlock [] -> false
	| TEnumIndex e1 -> expr_contains_statements e1

(* Collect all local variable IDs used in an expression *)
let collect_locals_used exprs =
	let ids = ref [] in
	let rec loop e =
		match e.eexpr with
		| TLocal v -> ids := v.v_id :: !ids
		| _ -> Type.iter loop e
	in
	List.iter loop exprs;
	!ids

(* Collect all local variable IDs defined in an expression (TVar declarations) *)
let collect_locals_defined e =
	let ids = ref [] in
	let rec loop e =
		match e.eexpr with
		| TVar (v, _) -> ids := v.v_id :: !ids; Type.iter loop e
		| _ -> Type.iter loop e
	in
	loop e;
	!ids

(* Check if super_args reference locals that are defined in body_expr.
   This happens with map literals: super(["k" => v]) becomes:
   TBlock [ TVar _g = new Map; _g.set("k", v); TCall(TSuper, [TLocal _g]) ]
   The super_args is [TLocal _g], but _g is defined in the body. In C#, we can't
   use `: base(_g)` because _g isn't defined yet at that point. *)
let super_args_reference_body_locals super_args body_expr =
	let locals_used_in_args = collect_locals_used super_args in
	let locals_defined_in_body = match body_expr with
		| Some body -> collect_locals_defined body
		| None -> []
	in
	(* Check if any local used in args is defined in body *)
	List.exists (fun id -> List.mem id locals_defined_in_body) locals_used_in_args

(* Extract statements from body that are needed for a local variable.
   Returns (dependency_stmts, remaining_body) where:
   - dependency_stmts: statements that define/use the local before the arg value
   - remaining_body: rest of body that should go in constructor *)
let extract_local_dependencies body_expr local_ids =
	(* For now, simple heuristic: take all statements from the body that reference
	   any of the local IDs. A more precise analysis could trace data dependencies. *)
	match body_expr with
	| None -> ([], None)
	| Some body ->
		let stmts = match body.eexpr with
			| TBlock el -> el
			| _ -> [body]
		in
		(* Split into: statements that touch our locals, and the rest *)
		let rec split_stmts touched remaining = function
			| [] -> (List.rev touched, List.rev remaining)
			| stmt :: rest ->
				let stmt_locals_used = collect_locals_used [stmt] in
				let stmt_locals_defined = collect_locals_defined stmt in
				let touches_our_locals =
					List.exists (fun id -> List.mem id local_ids) stmt_locals_used ||
					List.exists (fun id -> List.mem id local_ids) stmt_locals_defined
				in
				if touches_our_locals then
					split_stmts (stmt :: touched) remaining rest
				else
					split_stmts touched (stmt :: remaining) rest
		in
		let (dependency_stmts, remaining_stmts) = split_stmts [] [] stmts in
		let remaining_body = match remaining_stmts with
			| [] -> None
			| [e] -> Some e
			| el -> Some { body with eexpr = TBlock el }
		in
		(dependency_stmts, remaining_body)

(* Check if an expression is "pure" (no side effects) and can be safely dropped
   when used as a statement. In C#, bare constants/locals can't be statements. *)
let rec is_pure_expr e =
	match e.eexpr with
	| TConst _ -> true
	| TLocal _ -> true
	| TTypeExpr _ -> true
	| TIdent _ -> true
	| TParenthesis e1 -> is_pure_expr e1
	| TCast (e1, None) -> is_pure_expr e1
	| TMeta (_, e1) -> is_pure_expr e1
	| TField (e1, _) -> is_pure_expr e1  (* Field access without call is pure *)
	| TEnumIndex e1 -> is_pure_expr e1
	| TEnumParameter (e1, _, _) -> is_pure_expr e1
	| _ -> false

(* Check if a Haxe expression contains a call to a virtual method on 'this'.
   A virtual method is: not static, not final. This is used to detect constructors
   that may call overridable methods, which affects field initialization order in C#. *)
let rec expr_calls_virtual_method_on_this e =
	match e.eexpr with
	| TCall ({ eexpr = TField ({ eexpr = TConst TThis }, FInstance (_, _, cf)) }, _) ->
		(* Check if the method is virtual (can be overridden) *)
		begin match cf.cf_kind with
		| Method (MethNormal | MethInline) ->
			(* Virtual if not final *)
			not (has_class_field_flag cf CfFinal)
		| _ -> false
		end
	| TFunction _ ->
		(* Don't recurse into nested functions - they have their own 'this' *)
		false
	| _ ->
		(* Recursively check subexpressions *)
		let found = ref false in
		Type.iter (fun sub -> if expr_calls_virtual_method_on_this sub then found := true) e;
		!found

(* Check if a Haxe expression contains a TBreak that's inside a TSwitch but not inside a nested loop.
   This helps determine if we need a break label for the enclosing loop. *)
let rec has_break_in_switch ?(in_switch=false) e =
	match e.eexpr with
	| TBreak -> in_switch  (* Found break - return true only if we're inside a switch *)
	| TSwitch sw ->
		(* Enter switch context - any break in here is "in switch" *)
		let in_cases = List.exists (fun c -> has_break_in_switch ~in_switch:true c.case_expr) sw.switch_cases in
		let in_default = match sw.switch_default with
			| Some d -> has_break_in_switch ~in_switch:true d
			| None -> false
		in
		in_cases || in_default
	| TWhile _ ->
		(* Nested loop - don't look inside, breaks there are for that loop *)
		false
	| TFunction _ ->
		(* Don't look inside nested functions *)
		false
	| _ ->
		(* Recurse into sub-expressions *)
		let found = ref false in
		Type.iter (fun sub ->
			if has_break_in_switch ~in_switch sub then found := true
		) e;
		!found

(* ============================================================
   C# AST Analysis
   ============================================================ *)

(* Check if a CS expression has side effects and should not be evaluated multiple times.
   Returns true for method calls, new expressions, assignments, etc. *)
let rec cs_expr_has_side_effects cs_e =
	match cs_e with
	| CsCall _ | CsStaticCall _ | CsCallGeneric _ | CsStaticCallGeneric _ -> true
	| CsNew _ | CsNewArray _ | CsNewArraySize _ -> true
	| CsBinop (CsOpAssign, _, _) -> true
	| CsUnop (CsOpIncrement, _, _) | CsUnop (CsOpDecrement, _, _) -> true
	| CsCast (_, inner) | CsAs (inner, _) | CsParens inner | CsUnchecked inner -> cs_expr_has_side_effects inner
	| CsTernary (c, t, e) -> cs_expr_has_side_effects c || cs_expr_has_side_effects t || cs_expr_has_side_effects e
	| CsField (obj, _) -> cs_expr_has_side_effects obj
	| CsArrayAccess (arr, idx) -> cs_expr_has_side_effects arr || cs_expr_has_side_effects idx
	| _ -> false

(* Check if expression is trivial enough to safely duplicate without performance concerns.
   More conservative than cs_expr_has_side_effects - returns true only for truly cheap operations.
   Used when we need to use an expression multiple times and want to avoid redundant computation. *)
let rec cs_expr_is_trivial = function
	| CsLocal _ -> true           (* Variable lookup - trivial *)
	| CsConst _ -> true           (* Constant value - trivial *)
	| CsThis | CsBase -> true     (* Keywords - trivial *)
	| CsNull -> true              (* null literal - trivial *)
	| CsDefault _ -> true         (* default(T) - trivial *)
	| CsStaticField _ -> true     (* Static field read - cheap *)
	| CsParens e -> cs_expr_is_trivial e
	| _ -> false                  (* Everything else: calls, field chains, casts, etc. - may be expensive *)

(* Detect single-arg lambda IIFE pattern: ((Func<T,R>)(param => body))(arg)
   Returns Some (prefix_stmts, simplified_expr) if optimizable, None otherwise.
   This avoids lambda allocation overhead in statement contexts by converting to:
   var param = arg; then use body directly. *)
let optimize_single_arg_iife cs_expr =
	match cs_expr with
	| CsCall (CsCast (CsTypeFunc ([param_type], _), CsLambda ([param], CsLambdaExpr body)), [arg]) ->
		(* Convert to: var param = arg; then use body directly *)
		let prefix_stmt = CsVarDecl (param.p_name, Some param_type, Some arg) in
		Some ([prefix_stmt], body)
	| CsCall (CsParens (CsCast (CsTypeFunc ([param_type], _), CsLambda ([param], CsLambdaExpr body))), [arg]) ->
		(* Same with extra parens *)
		let prefix_stmt = CsVarDecl (param.p_name, Some param_type, Some arg) in
		Some ([prefix_stmt], body)
	| _ -> None

(* Check if a C# statement terminates with a HARD terminator (return, throw).
   Used to avoid generating unreachable 'break' statements after terminators in switch cases.
   IMPORTANT: We only consider return/throw as terminators, NOT break/continue/goto,
   because those only terminate local control flow (loops/switches) but not the enclosing case.
   A nested switch with break doesn't terminate the outer case - it just exits the inner switch. *)
let rec stmt_terminates stmt =
	match stmt with
	| CsReturn _ | CsThrowStmt _ -> true  (* Hard terminators *)
	| CsBreak | CsContinue | CsGoto _ -> false  (* Local control flow - doesn't terminate enclosing context *)
	| CsBlock stmts | CsStmtList stmts ->
		(* A block terminates if its last statement terminates *)
		begin match List.rev stmts with
		| [] -> false
		| last :: _ -> stmt_terminates last
		end
	| CsIf (_, then_branch, Some else_branch) ->
		(* If-else terminates if BOTH branches terminate *)
		stmt_terminates then_branch && stmt_terminates else_branch
	| CsIf (_, _, None) ->
		(* If without else doesn't guarantee termination *)
		false
	| CsSwitch (_, sections) ->
		(* A nested switch only terminates the outer context if ALL cases return/throw.
		   If any case uses break (to exit the inner switch), control continues in outer context. *)
		List.for_all (fun section ->
			match List.rev section.sw_body with
			| [] -> false
			| last :: _ -> stmt_terminates last
		) sections
	| CsTry (body, catches, finally) ->
		(* Try-catch only terminates if BOTH:
		   1. The try body terminates (returns/throws) - so normal completion doesn't happen
		   2. All catch blocks terminate
		   If try completes normally, control falls through even if catches throw.
		   finally doesn't affect termination analysis. *)
		let _ = finally in (* suppress unused warning *)
		stmt_terminates body &&
		List.for_all (fun c -> stmt_terminates c.catch_body) catches
	| CsUncheckedStmt inner -> stmt_terminates inner
	| CsWhile _ | CsDoWhile _ | CsFor _ | CsForeach _ ->
		(* Loops don't terminate the enclosing context even if they have breaks/continues inside.
		   A loop might complete normally after iterations, so we need a break after. *)
		false
	| _ -> false

(* Check if a C# statement exits a switch case (prevents fallthrough).
   Unlike stmt_terminates, this returns true for break/continue/goto since those
   prevent switch fallthrough even though they don't terminate the method context. *)
let rec stmt_exits_case stmt =
	match stmt with
	| CsReturn _ | CsThrowStmt _ | CsBreak | CsContinue | CsGoto _ -> true
	| CsBlock stmts | CsStmtList stmts ->
		begin match List.rev stmts with
		| [] -> false
		| last :: _ -> stmt_exits_case last
		end
	| CsIf (_, then_branch, Some else_branch) ->
		stmt_exits_case then_branch && stmt_exits_case else_branch
	| CsIf (_, _, None) -> false
	| CsUncheckedStmt inner -> stmt_exits_case inner
	| _ -> false

(* Check if a statement definitely returns (ends with a return statement).
   This is a simpler check than stmt_terminates - only looks for explicit returns. *)
let rec stmt_has_return stmt =
	match stmt with
	| CsReturn _ -> true
	| CsThrowStmt _ -> true
	| CsBlock stmts -> (match List.rev stmts with [] -> false | last :: _ -> stmt_has_return last)
	| CsStmtList stmts -> (match List.rev stmts with [] -> false | last :: _ -> stmt_has_return last)
	| _ -> false

(* Check if a statement list ends with a return *)
let stmts_end_with_return stmts =
	match List.rev stmts with
	| [] -> false
	| last :: _ -> stmt_has_return last

(* Transform void returns (CsReturn None) to null returns (CsReturn (Some CsNull)).
   Used when a void-returning Haxe closure shadows a base class method that returns object. *)
let rec transform_void_returns_to_null stmt =
	match stmt with
	| CsReturn None -> CsReturn (Some CsNull)
	| CsBlock stmts -> CsBlock (List.map transform_void_returns_to_null stmts)
	| CsStmtList stmts -> CsStmtList (List.map transform_void_returns_to_null stmts)
	| CsIf (cond, then_stmt, else_opt) ->
		CsIf (cond, transform_void_returns_to_null then_stmt,
			Option.map transform_void_returns_to_null else_opt)
	| CsSwitch (expr, sections) ->
		let sections' = List.map (fun s ->
			{ s with sw_body = List.map transform_void_returns_to_null s.sw_body }
		) sections in
		CsSwitch (expr, sections')
	| CsWhile (cond, body) ->
		CsWhile (cond, transform_void_returns_to_null body)
	| CsDoWhile (body, cond) ->
		CsDoWhile (transform_void_returns_to_null body, cond)
	| CsFor (init, cond, iter, body) ->
		CsFor (init, cond, iter, transform_void_returns_to_null body)
	| CsForeach (t, name, expr, body) ->
		CsForeach (t, name, expr, transform_void_returns_to_null body)
	| CsTry (body, catches, finally_opt) ->
		let catches' = List.map (fun c ->
			{ c with catch_body = transform_void_returns_to_null c.catch_body }
		) catches in
		CsTry (transform_void_returns_to_null body, catches',
			Option.map transform_void_returns_to_null finally_opt)
	| CsUsing (decls, body) ->
		CsUsing (decls, transform_void_returns_to_null body)
	| CsLock (expr, body) ->
		CsLock (expr, transform_void_returns_to_null body)
	| _ -> stmt
