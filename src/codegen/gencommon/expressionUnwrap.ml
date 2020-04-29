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
open Common
open Ast
open Type
open Gencommon

(*
	This is the most important module for source-code based targets. It will follow a convention of what's an expression and what's a statement,
	and will unwrap statements where expressions are expected, and vice-versa.

	It should be one of the first syntax filters to be applied. As a consequence, it's applied after all filters that add code to the AST, and by being
	the first of the syntax filters, it will also have the AST retain most of the meaning of normal Haxe code. So it's easier to detect cases which are
	side-effects free, for example

	Any target can make use of this, but there is one requirement: The target must accept null to be set to any kind of variable. For example,
	var i:Int = null; must be accepted. The best way to deal with this is to (like it's done in C#) make null equal to "default(Type)"

	dependencies:
		While it's best for Expression Unwrap to delay its execution as much as possible, since theoretically any
		filter can return an expression that needs to be unwrapped, it is also desirable for ExpresionUnwrap to have
		the AST as close as possible as Haxe's, so it can make some correct predictions (for example, so it can
		more accurately know what can be side-effects-free and what can't).
		This way, it will run slightly after the Normal priority, so if you don't say that a syntax filter must run
		before Expression Unwrap, it will run after it.

	TODO : While statement must become do / while, with the actual block inside an if for the condition, and else for 'break'
*)

(* priority: first syntax filter *)
let priority = -10.0

(*
	We always need to rely on Blocks to be able to unwrap expressions correctly.
	So the the standard traverse will always be based on blocks.
	Normal block statements, like for(), while(), if(), ... will be mk_block'ed so there is always a block inside of them.

		At the block level, we'll define an "add_statement" function, which will allow the current expression to
		add statements to the block. This statement may or may not contain statements as expressions, so the texpr will be evaluated recursively before being added.

		- traverse will always evaluate TBlocks
		- for each texpr in a TBlock list,
			check shallow type
				if type is Statement or Both when it has problematic expression (var problematic_expr = count_problematic_expressions),
					if we can eagerly call unwrap_statement on the whole expression (try_call_unwrap_statement), use the return expression
					else
						check expr_type of each underlying type (with expr_stat_map)
							if it has ExprWithStatement or Statement,
								call problematic_expression_unwrap in it
								problematic_expr--
							else if problematic_expr == 0, just add the unchanged expression
							else if NoSideEffects and doesn't have short-circuit, just add the unchanged expression
							else call problematic_expression_unwrap in it
				if type is Expression, check if there are statements or Both inside.
					if there are, problematic_expression_unwrap in it
				aftewards, use on_expr_as_statement to get it

	helpers:
		try_call_unwrap_statement: (returns texpr option)
			if underlying statement is TBinop(OpAssign/OpAssignOp), or TVar, with the right side being a Statement or a short circuit op, we can call apply_assign.

		apply_assign:
			if is TVar, first declare the tvar with default expression = null;
			will receive the left and right side of the assignment; right-side must be Statement
			see if right side is a short-circuit operation, call short_circuit_op_unwrap
			else see eexpr of the right side
				if it's void, just add the statement with add_statement, and set the right side as null;
				if not, it will have a block inside. set the left side = to the last expression on each block inside. add_statement for it.

		short_circuit_op_unwrap: x() && (1 + {var x = 0; x + 1;} == 2) && z()
			-> var x = x();
					var y = false;
					var z = false;
					if (x) //for &&, neg for ||
					{
					var temp = null;
					{
						var x = 0;
						temp = x + 1;
					}

					y = (1 + temp) == 2;
					if (y)
					{
						z = z();
					}
					}
			expects to receive a texpr with TBinop(OpBoolAnd/OpBoolOr)
			will traverse the AST while there is a TBinop(OpBoolAnd/OpBoolOr) as a right-side expr, and declare new temp vars in the	for each found.
			will collect the return value, a mapped expr with all exprs as TLocal of the temp vars created


		problematic_expression_unwrap:
			check expr_kind:
				if it is NoSideEffects and not short-circuit, leave it there
				if it is ExprWithStatement and not short-circuit, call Type.map_expr problematic_expression_unwrap
				if it is Statement or Expression or short-circuit expr, call add_assign for this expression

		add_assign:
			see if the type is void. If it is, just add_statement the expression argument, and return a null value
			else create a new variable, set TVar with Some() with the expression argument, add TVar with add_statement, and return the TLocal of this expression.

		map_problematic_expr:
			call expr_stat_map on statement with problematic_expression_unwrap

	types:
		type shallow_expr_type = | Statement | Expression | Both (* shallow expression classification. Both means that they can be either Statements as Expressions *)

		type expr_kind = | NormalExpr | ExprNoSideEffects (* -> short-circuit is considered side-effects *) | ExprWithStatement | Statement
			evaluates an expression (as in not a statement) type. If it is ExprWithStatement or Statement, it means it contains errors

	functions:
		shallow_expr_type (expr:texpr) : shallow_expr_type

		expr_kind (expr:texpr) : expr_kind
			deeply evaluates an expression type

		expr_stat_map (fn:texpr->texpr) (expr:texpr) : texpr
			it will traverse the AST looking for places where an expression is expected, and map the value according to fn

		aggregate_expr_type (is_side_effects_free:bool) (children:expr_type list) : expr_type
			helper function to deal with expr_type aggregation (e.g. an Expression + a Statement as a children, is a ExprWithStatement)

		check_statement_in_expression (expr:texpr) : texpr option :
			will check

*)

type shallow_expr_type = | Statement | Expression of texpr | Both of texpr (* shallow expression classification. Both means that they can be either Statements as Expressions *)

type expr_kind = | KNormalExpr | KNoSideEffects (* -> short-circuit is considered side-effects *) | KExprWithStatement | KStatement

let rec no_paren e =
	match e.eexpr with
		| TParenthesis e -> no_paren e
		| _ -> e

(* must be called in a statement. Will execute fn whenever an expression (not statement) is expected *)
let rec expr_stat_map fn (expr:texpr) =
	match (no_paren expr).eexpr with
		| TBinop ( (OpAssign as op), left_e, right_e )
		| TBinop ( (OpAssignOp _ as op), left_e, right_e ) ->
			{ expr with eexpr = TBinop(op, fn left_e, fn right_e) }
		| TParenthesis _ -> Globals.die "" __LOC__
		| TCall(left_e, params) ->
			{ expr with eexpr = TCall(fn left_e, List.map fn params) }
		| TNew(cl, tparams, params) ->
			{ expr with eexpr = TNew(cl, tparams, List.map fn params) }
		| TVar(v,eopt) ->
			{ expr with eexpr = TVar(v, Option.map fn eopt) }
		| TFor (v,cond,block) ->
			{ expr with eexpr = TFor(v, fn cond, block) }
		| TIf(cond,eif,eelse) ->
			{ expr with eexpr = TIf(fn cond, eif, eelse) }
		| TWhile(cond, block, flag) ->
			{ expr with eexpr = TWhile(fn cond, block, flag) }
		| TSwitch(cond, el_block_l, default) ->
			{ expr with eexpr = TSwitch( fn cond, List.map (fun (el,block) -> (List.map fn el, block)) el_block_l, default ) }
		| TReturn(eopt) ->
			{ expr with eexpr = TReturn(Option.map fn eopt) }
		| TThrow (texpr) ->
			{ expr with eexpr = TThrow(fn texpr) }
		| TBreak
		| TContinue
		| TTry _
		| TUnop (Increment, _, _)
		| TUnop (Decrement, _, _) (* unop is a special case because the haxe compiler won't let us generate complex expressions with Increment/Decrement *)
		| TBlock _ -> expr (* there is no expected expression here. Only statements *)
		| TMeta(m,e) ->
			{ expr with eexpr = TMeta(m,expr_stat_map fn e) }
		| _ -> Globals.die "" __LOC__ (* we only expect valid statements here. other expressions aren't valid statements *)

let is_expr = function | Expression _ -> true | _ -> false

let aggregate_expr_type map_fn side_effects_free children =
	let rec loop acc children =
		match children with
			| [] -> acc
			| hd :: children ->
				match acc, map_fn hd with
					| _, KExprWithStatement
					| _, KStatement
					| KExprWithStatement, _
					| KStatement, _ -> KExprWithStatement
					| KNormalExpr, KNoSideEffects
					| KNoSideEffects, KNormalExpr
					| KNormalExpr, KNormalExpr -> loop KNormalExpr children
					| KNoSideEffects, KNoSideEffects -> loop KNoSideEffects children
	in
	loop (if side_effects_free then KNoSideEffects else KNormalExpr) children

(* statements: *)
(* Error CS0201: Only assignment, call, increment,					 *)
(* decrement, and new object expressions can be used as a		 *)
(* statement (CS0201). *)
let rec shallow_expr_type expr : shallow_expr_type =
	match expr.eexpr with
		| TCall _ when not (ExtType.is_void expr.etype) -> Both expr
		| TNew _
		| TUnop (Increment, _, _)
		| TUnop (Decrement, _, _)
		| TBinop (OpAssign, _, _)
		| TBinop (OpAssignOp _, _, _) -> Both expr
		| TIf (cond, eif, Some(eelse)) -> (match aggregate_expr_type expr_kind true [cond;eif;eelse] with
			| KExprWithStatement -> Statement
			| _ -> Both expr)
		| TConst _
		| TLocal _
		| TIdent _
		| TArray _
		| TBinop _
		| TField _
		| TEnumParameter _
		| TEnumIndex _
		| TTypeExpr _
		| TObjectDecl _
		| TArrayDecl _
		| TFunction _
		| TCast _
		| TUnop _ -> Expression (expr)
		| TParenthesis p | TMeta(_,p) -> shallow_expr_type p
		| TBlock ([e]) -> shallow_expr_type e
		| TCall _
		| TVar _
		| TBlock _
		| TFor _
		| TWhile _
		| TSwitch _
		| TTry _
		| TReturn _
		| TBreak
		| TContinue
		| TIf _
		| TThrow _ -> Statement

and expr_kind expr =
	match shallow_expr_type expr with
	| Statement -> KStatement
	| Both expr | Expression expr ->
		let aggregate = aggregate_expr_type expr_kind in
		match expr.eexpr with
		| TConst _
		| TLocal _
		| TFunction _
		| TTypeExpr _
		| TIdent _ ->
			KNoSideEffects
		| TCall (ecall, params) ->
			aggregate false (ecall :: params)
		| TNew (_,_,params) ->
			aggregate false params
		| TUnop (Increment,_,e)
		| TUnop (Decrement,_,e) ->
			aggregate false [e]
		| TUnop (_,_,e) ->
			aggregate true [e]
		| TBinop (OpBoolAnd, e1, e2)
		| TBinop (OpBoolOr, e1, e2) ->	(* TODO: should OpBool never be side-effects free? *)
			aggregate true [e1;e2]
		| TBinop (OpAssign, e1, e2)
		| TBinop (OpAssignOp _, e1, e2) ->
			aggregate false [e1;e2]
		| TBinop (_, e1, e2) ->
			aggregate true [e1;e2]
		| TIf (cond, eif, Some(eelse)) -> (match aggregate true [cond;eif;eelse] with
			| KExprWithStatement -> KStatement
			| k -> k)
		| TArray (e1,e2) ->
			aggregate true [e1;e2]
		| TParenthesis e
		| TMeta(_,e)
		| TField (e,_) ->
			aggregate true [e]
		| TArrayDecl (el) ->
			aggregate true el
		| TObjectDecl (sel) ->
			aggregate true (List.map snd sel)
		| TCast (e,_) ->
			aggregate false [e]
		| _ -> trace (debug_expr expr); Globals.die "" __LOC__ (* should have been read as Statement by shallow_expr_type *)

let get_kinds (statement:texpr) =
	let kinds = ref [] in
	ignore (expr_stat_map (fun e ->
		kinds := (expr_kind e) :: !kinds;
		e
	) statement);
	List.rev !kinds

let has_problematic_expressions (kinds:expr_kind list) =
	let rec loop kinds =
		match kinds with
			| [] -> false
			| KStatement :: _
			| KExprWithStatement :: _ -> true
			| _ :: tl -> loop tl
	in
	loop kinds

let count_problematic_expressions (statement:texpr) =
	let count = ref 0 in
	ignore (expr_stat_map (fun e ->
		(match expr_kind e with
			| KStatement | KExprWithStatement -> incr count
			| _ -> ()
		);
		e
	) statement);
	!count

let apply_assign_block assign_fun elist =
	let rec assign acc elist =
		match elist with
			| [] -> acc
			| last :: [] ->
				(assign_fun last) :: acc
			| hd :: tl ->
				assign (hd :: acc) tl
	in
	List.rev (assign [] elist)

let mk_get_block assign_fun e =
	match e.eexpr with
		| TBlock [] -> e
		| TBlock (el) ->
			{ e with eexpr = TBlock(apply_assign_block assign_fun el) }
		| _ ->
			{ e with eexpr = TBlock([ assign_fun e ]) }

let add_assign add_statement expr =
	match expr.eexpr, follow expr.etype with
		| _, TAbstract ({ a_path = ([],"Void") },[])
		| TThrow _, _ ->
			add_statement expr;
			null expr.etype expr.epos
		| _ ->
			let var = mk_temp "stmt" expr.etype in
			let tvars = { expr with eexpr = TVar(var,Some(expr)) } in
			let local = { expr with eexpr = TLocal(var) } in
			add_statement tvars;
			local

(* requirement: right must be a statement *)
let rec apply_assign assign_fun right =
	match right.eexpr with
		| TBlock el ->
			{ right with eexpr = TBlock(apply_assign_block assign_fun el) }
		| TSwitch (cond, elblock_l, default) ->
			{ right with eexpr = TSwitch(cond, List.map (fun (el,block) -> (el, mk_get_block assign_fun block)) elblock_l, Option.map (mk_get_block assign_fun) default) }
		| TTry (block, catches) ->
			{ right with eexpr = TTry(mk_get_block assign_fun block, List.map (fun (v,block) -> (v,mk_get_block assign_fun block) ) catches) }
		| TIf (cond,eif,eelse) ->
			{ right with eexpr = TIf(cond, mk_get_block assign_fun eif, Option.map (mk_get_block assign_fun) eelse) }
		| TThrow _
		| TWhile _
		| TFor _
		| TReturn _
		| TBreak
		| TContinue -> right
		| TParenthesis p | TMeta(_,p) ->
			apply_assign assign_fun p
		| TVar _ ->
			right
		| _ ->
			match follow right.etype with
				| TAbstract ({ a_path = ([], "Void") },[]) ->
					right
				| _ -> trace (debug_expr right); Globals.die "" __LOC__ (* a statement is required *)

let short_circuit_op_unwrap com add_statement expr :texpr =
	let do_not expr =
		{ expr with eexpr = TUnop(Not, Prefix, expr) }
	in

	(* loop will always return its own TBlock, and the mapped expression *)
	let rec loop acc expr =
		match expr.eexpr with
			| TBinop ( (OpBoolAnd as op), left, right) ->
				let var = mk_temp "boolv" right.etype in
				let tvars = { right with eexpr = TVar(var, Some( { right with eexpr = TConst(TBool false); etype = com.basic.tbool } )); etype = com.basic.tvoid } in
				let local = { right with eexpr = TLocal(var) } in

				let mapped_left, ret_acc = loop ( (local, { right with eexpr = TBinop(OpAssign, local, right) } ) :: acc) left in

				add_statement tvars;
				({ expr with eexpr = TBinop(op, mapped_left, local) }, ret_acc)
			(* we only accept OpBoolOr when it's the first to be evaluated *)
			| TBinop ( (OpBoolOr as op), left, right) when acc = [] ->
				let left = match left.eexpr with
					| TLocal _ | TConst _ -> left
					| _ -> add_assign add_statement left
				in

				let var = mk_temp "boolv" right.etype in
				let tvars = { right with eexpr = TVar(var, Some( { right with eexpr = TConst(TBool false); etype = com.basic.tbool } )); etype = com.basic.tvoid } in
				let local = { right with eexpr = TLocal(var) } in
				add_statement tvars;

				({ expr with eexpr = TBinop(op, left, local) }, [ do_not left, { right with eexpr = TBinop(OpAssign, local, right) } ])
			| _ when acc = [] -> Globals.die "" __LOC__
			| _ ->
				let var = mk_temp "boolv" expr.etype in
				let tvars = { expr with eexpr = TVar(var, Some( { expr with etype = com.basic.tbool } )); etype = com.basic.tvoid } in
				let local = { expr with eexpr = TLocal(var) } in

				let last_local = ref local in
				let acc = List.map (fun (local, assign) ->
					let l = !last_local in
					last_local := local;
					(l, assign)
				) acc in

				add_statement tvars;
				(local, acc)
	in

	let mapped_expr, local_assign_list = loop [] expr in

	let rec loop local_assign_list : texpr =
		match local_assign_list with
			| [local, assign] ->
				{ eexpr = TIf(local, assign, None); etype = com.basic.tvoid; epos = assign.epos }
			| (local, assign) :: tl ->
				{ eexpr = TIf(local,
					{
						eexpr = TBlock ( assign :: [loop tl] );
						etype = com.basic.tvoid;
						epos = assign.epos;
					},
				None); etype = com.basic.tvoid; epos = assign.epos }
			| [] -> Globals.die "" __LOC__
	in

	add_statement (loop local_assign_list);
	mapped_expr

let twhile_with_condition_statement com add_statement twhile cond e1 flag =
	(* when a TWhile is found with a problematic condition *)
	let block =
		if flag = NormalWhile then
			{ e1 with eexpr = TIf(cond, e1, Some({ e1 with eexpr = TBreak; etype = com.basic.tvoid })) }
		else
			Type.concat e1 { e1 with
				eexpr = TIf({
					eexpr = TUnop(Not, Prefix, mk_paren cond);
					etype = com.basic.tbool;
					epos = cond.epos
				}, { e1 with eexpr = TBreak; etype = com.basic.tvoid }, None);
				etype = com.basic.tvoid
			}
	in
	add_statement { twhile with
		eexpr = TWhile(
			{ eexpr = TConst(TBool true); etype = com.basic.tbool; epos = cond.epos },
			block,
			DoWhile
		);
	}

let try_call_unwrap_statement com handle_cast problematic_expression_unwrap (add_statement:texpr->unit) (expr:texpr) : texpr option =
	let check_left left =
		match expr_kind left with
			| KExprWithStatement ->
				problematic_expression_unwrap add_statement left KExprWithStatement
			| KStatement -> Globals.die "" __LOC__ (* doesn't make sense a KStatement as a left side expression *)
			| _ -> left
	in

	let handle_assign op left right =
		let left = check_left left in
		Some (apply_assign (fun e -> { e with eexpr = TBinop(op, left, if ExtType.is_void left.etype then e else handle_cast left.etype e.etype e) }) right )
	in

	let handle_return e =
		Some( apply_assign (fun e ->
			match e.eexpr with
				| TThrow _ -> e
				| _ when ExtType.is_void e.etype ->
					{ e with eexpr = TBlock([e; { e with eexpr = TReturn None }]) }
				| _ ->
					Texpr.Builder.mk_return e
		) e )
	in

	let is_problematic_if right =
		match expr_kind right with
			| KStatement | KExprWithStatement -> true
			| _ -> false
	in

	match expr.eexpr with
		| TBinop((OpAssign as op),left,right)
		| TBinop((OpAssignOp _ as op),left,right) when shallow_expr_type right = Statement ->
			handle_assign op left right
		| TReturn( Some right ) when shallow_expr_type right = Statement ->
			handle_return right
		| TBinop((OpAssign as op),left, ({ eexpr = TBinop(OpBoolAnd,_,_) } as right) )
		| TBinop((OpAssign as op),left,({ eexpr = TBinop(OpBoolOr,_,_) } as right))
		| TBinop((OpAssignOp _ as op),left,({ eexpr = TBinop(OpBoolAnd,_,_) } as right) )
		| TBinop((OpAssignOp _ as op),left,({ eexpr = TBinop(OpBoolOr,_,_) } as right) ) ->
			let right = short_circuit_op_unwrap com add_statement right in
			Some { expr with eexpr = TBinop(op, check_left left, right) }
		| TVar(v,Some({ eexpr = TBinop(OpBoolAnd,_,_) } as right))
		| TVar(v,Some({ eexpr = TBinop(OpBoolOr,_,_) } as right)) ->
			let right = short_circuit_op_unwrap com add_statement right in
			Some { expr with eexpr = TVar(v, Some(right)) }
		| TVar(v,Some(right)) when shallow_expr_type right = Statement ->
			add_statement ({ expr with eexpr = TVar(v, Some(null right.etype right.epos)) });
			handle_assign OpAssign { expr with eexpr = TLocal(v); etype = v.v_type } right
		(* TIf handling *)
		| TBinop((OpAssign as op),left, ({ eexpr = TIf _ } as right))
		| TBinop((OpAssignOp _ as op),left,({ eexpr = TIf _ } as right)) when is_problematic_if right ->
			handle_assign op left right
		| TVar(v,Some({ eexpr = TIf _ } as right)) when is_problematic_if right ->
			add_statement ({ expr with eexpr = TVar(v, Some(null right.etype right.epos)) });
			handle_assign OpAssign { expr with eexpr = TLocal(v); etype = v.v_type } right
		| TWhile(cond, e1, flag) when is_problematic_if cond ->
			twhile_with_condition_statement com add_statement expr cond e1 flag;
			Some (null expr.etype expr.epos)
		| _ -> None

let problematic_expression_unwrap add_statement expr e_type =
	let rec problematic_expression_unwrap is_first expr e_type =
		match e_type, expr.eexpr with
			| _, TBinop(OpBoolAnd, _, _)
			| _, TBinop(OpBoolOr, _, _) -> add_assign add_statement expr (* add_assign so try_call_unwrap_expr *)
			| KNoSideEffects, _ -> expr
			| KStatement, _
			| KNormalExpr, _ -> add_assign add_statement expr
			| KExprWithStatement, TCall _
			| KExprWithStatement, TNew _
			| KExprWithStatement, TBinop (OpAssign,_,_)
			| KExprWithStatement, TBinop (OpAssignOp _,_,_)
			| KExprWithStatement, TUnop (Increment,_,_) (* all of these may have side-effects, so they must also be add_assign'ed . is_first avoids infinite loop *)
			| KExprWithStatement, TUnop (Decrement,_,_) when not is_first -> add_assign add_statement expr

			(* bugfix: Type.map_expr doesn't guarantee the correct order of execution *)
			| KExprWithStatement, TBinop(op,e1,e2) ->
				let e1 = problematic_expression_unwrap false e1 (expr_kind e1) in
				let e2 = problematic_expression_unwrap false e2 (expr_kind e2) in
				{ expr with eexpr = TBinop(op, e1, e2) }
			| KExprWithStatement, TArray(e1,e2) ->
				let e1 = problematic_expression_unwrap false e1 (expr_kind e1) in
				let e2 = problematic_expression_unwrap false e2 (expr_kind e2) in
				{ expr with eexpr = TArray(e1, e2) }
			(* bugfix: calls should not be transformed into closure calls *)
			| KExprWithStatement, TCall(( { eexpr = TField (ef_left, f) } as ef ), eargs) ->
				{ expr with eexpr = TCall(
					{ ef with eexpr = TField(problematic_expression_unwrap false ef_left (expr_kind ef_left), f) },
					List.map (fun e -> problematic_expression_unwrap false e (expr_kind e)) eargs)
				}
			| KExprWithStatement, _ -> Type.map_expr (fun e -> problematic_expression_unwrap false e (expr_kind e)) expr
	in
	problematic_expression_unwrap true expr e_type

let configure gen =
	let rec traverse e =
		match e.eexpr with
		| TBlock el ->
			let new_block = ref [] in
			let rec process_statement e =
				let e = no_paren e in
				match e.eexpr, shallow_expr_type e with
				| TCall( { eexpr = TIdent s } as elocal, elist ), _ when String.get s 0 = '_' && Hashtbl.mem gen.gspecial_vars s ->
					new_block := { e with eexpr = TCall( elocal, List.map (fun e ->
						match e.eexpr with
							| TBlock _ -> traverse e
							| _ -> e
					) elist ) } :: !new_block
				| _, Statement | _, Both _ ->
					let e = match e.eexpr with TReturn (Some ({ eexpr = TThrow _ } as ethrow)) -> ethrow | _ -> e in
					let kinds = get_kinds e in
					if has_problematic_expressions kinds then begin
						match try_call_unwrap_statement gen.gcon gen.ghandle_cast problematic_expression_unwrap process_statement e with
							| Some { eexpr = TConst(TNull) } (* no op *)
							| Some { eexpr = TBlock [] } -> ()
							| Some e ->
								if has_problematic_expressions (get_kinds e) then begin
									process_statement e
								end else
									new_block := (traverse e) :: !new_block
							| None ->
							(
								let acc = ref kinds in
								let new_e = expr_stat_map (fun e ->
									match !acc with
										| hd :: tl ->
											acc := tl;
											if has_problematic_expressions (hd :: tl) then begin
												problematic_expression_unwrap process_statement e hd
											end else
												e
										| [] -> Globals.die "" __LOC__
								) e in

								new_block := (traverse new_e) :: !new_block
							)
					end else begin new_block := (traverse e) :: !new_block end
				| _, Expression e ->
					let e = mk (TVar (mk_temp "expr" e.etype, Some e)) gen.gcon.basic.tvoid e.epos in
					process_statement e
			in
			List.iter process_statement el;
			let block = List.rev !new_block in
			{ e with eexpr = TBlock block }
		| TTry (block, catches) ->
			{ e with eexpr = TTry(traverse (mk_block block), List.map (fun (v,block) -> (v, traverse (mk_block block))) catches) }
		| TSwitch (cond,el_e_l, default) ->
			{ e with eexpr = TSwitch(cond, List.map (fun (el,e) -> (el, traverse (mk_block e))) el_e_l, Option.map (fun e -> traverse (mk_block e)) default) }
		| TWhile (cond,block,flag) ->
			{e with eexpr = TWhile(cond,traverse (mk_block block), flag) }
		| TIf (cond, eif, eelse) ->
			{ e with eexpr = TIf(cond, traverse (mk_block eif), Option.map (fun e -> traverse (mk_block e)) eelse) }
		| TFor (v,it,block) ->
			{ e with eexpr = TFor(v,it, traverse (mk_block block)) }
		| TFunction (tfunc) ->
			{ e with eexpr = TFunction({ tfunc with tf_expr = traverse (mk_block tfunc.tf_expr) }) }
		| TMeta (m, e2) ->
			{ e with eexpr = TMeta (m, traverse e2)}
		| _ -> e (* if expression doesn't have a block, we will exit *)
	in
	gen.gsyntax_filters#add "expression_unwrap" (PCustom priority) traverse
