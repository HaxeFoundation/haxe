(**
	Parser Hook Example Plugin

	This plugin demonstrates how a parser plugin can be created.
	It implements a simple version of a forward pipe operator `|>`.
*)

open Ast
open Globals
open EvalValue
open Grammar

;;

(**
	Plugins could be designed to be used with both `eval.vm.Context.loadPlugin` and `--load-plugin`.
	However, this plugin should only be used with `--load-plugin`.
*)
if not !EvalStdLib.StdContext.is_init then
	(failwith "parser_hooks - This plugin must be initialized using --load-plugin");

(**
	Finds the last element of a list and returns it with a list of the other elements.
*)
let rec find_last_element acc lst =
	match lst with
	| [x] -> Some (acc, x)
	| [] -> None
	| f :: lst -> find_last_element (acc @ [f]) lst
in

(**
	The name of the meta used to track our custom operator.
	Starts with a dash to decrease likelihood of conflicting with user-made meta.
*)
let pipe_forward_meta_name = "-customOperator_pipeForward" in

(**
	Wraps the provided expression with `@-customOperator_pipeForward` meta.
	Important for tracking when the pipe forward operator is used.
*)
let wrap_operator_meta e =
	EMeta((Meta.Custom(pipe_forward_meta_name), [], snd e), e), (snd e)
in

(**
	Given two expressions, provide the expression equivalent for our custom pipe forward operator.
	Normally infix operators would not be this complicated, but pipe forward has reverse priority rules.

	Given `a |> b |> c` we want `a |> c(b)` then `c(b(a))`.
	To distinguish whether `a |> c(b)` becomes `c(b, a)` or `c(b(a))`, custom metadata is used.
*)
let rec make_pipe_expr e1 e2 =
	match e2 with
	| (EMeta(entry, e2i)),p -> (
		let m,_,_ = entry in
		match m with
		| Meta.Custom meta_name when meta_name = pipe_forward_meta_name -> (
			match e2i with
			| ECall(ecall, eparams), pcall -> (
				wrap_operator_meta (
					match find_last_element [] eparams with
					| Some (acc, last) -> ECall(ecall, acc @ [make_pipe_expr e1 last]), pcall
					| _ -> ECall(ecall, [e1]), pcall
				)
			)
			| _ -> failwith (Printf.sprintf "`@%s` meta should only surrond ECall" pipe_forward_meta_name);
		)
		| _ -> make_pipe_expr e1 e2i
	)
	| ECall(e2call, e2param),p when (List.length e2param) > 0 -> wrap_operator_meta (ECall(e2call, e2param @ [e1]), p)
	| _ -> wrap_operator_meta (ECall(e2, [e1]), snd e2)
in

(**
	Initialization plugins work best for hooking the parser.
	Add this plugin using `--load-plugin` to enable this hook.

	This function is added to `on_expr_next_parse`, so it should be `(Parser.token_stream * Ast.expr) -> (Ast.expr option)`.
	When the parser begins looking for postfix/infix operators, this hook will be called first.

	Returning `None` does nothing and the compiler will continue to parse an expression like normal.
	Returning `Some(Ast.expr)` will provide a custom expression for the parser to use.
*)
let my_hook (s, e1) =
	(**
		Check if the first token is `|` without consuming it.
		`Stream.npeek` cannot be used without breaking the parser, so avoid it.
	*)
	match Stream.peek s with
	| Some (Binop OpOr, or_pos) -> (
		(**
			Consume the token to view the next one.
			We now must return an expression, otherwise there will be issues since a token is missing.
		*)
		Stream.junk s;

		(**
			Check if the second token is `>` without consuming it.
		*)
		match Stream.peek s with
		| Some (Binop OpGt, lt_pos) -> (
			(**
				Success! Our custom `|>` operator has been found.
			*)
			Stream.junk s;

			(**
				Let's first get the expression to the right of the operator.
			*)
			let e2 = secure_expr s in

			(**
				Use `make_pipe_expr` to generate the pipe forward expression from the left and right expressions.
			*)
			Some (make_pipe_expr e1 e2)
		)

		(**
			If the second token doesn't match, we should replicate the behavior that would normally occur.
		*)
		| _ -> (
			let e2 = secure_expr s in
			Some (Parser.make_binop OpOr e1 e2)
		)
	)
	| _ -> (
		None
	)
in

(**
	Add `my_hook` function to the end of the list of hooks.
	The list of available hooks are:

	`on_expr` : Parser.token_stream -> (Ast.expr option)
	Hooks at the beginning of the expression parsing function.

	`on_expr_next` : (Parser.token_stream * Ast.expr) -> (Ast.expr option)
	Hooks at the beginning of `expr_next`, which is called after parsing a "value" type expression.
	Used to parse postfix operators, infix operators, etc.

	`on_expr_expected` : Parser.token_stream -> (Ast.expr option)
	Hooks after an expression attempted to be parsed but failed.
	If an expression is returned, that expression is used instead of an "Expected expression" error occuring.

	`on_type_decl` : (Parser.token_stream * Parser.type_decl_completion_mode) -> (Ast.type_decl option)
	Hooks at the start of the type declaration parsing function.

	`on_class_field` : (Parser.token_stream * bool) -> (Ast.class_field option)
	Hooks at the start of the class field parsing function.
*)
Parser.hooks.on_expr_next <- (my_hook :: Parser.hooks.on_expr_next)
