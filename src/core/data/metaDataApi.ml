open Ast

let error s pos =
	Error.raise_typing_error s pos

(**
	Converts metadata arguments (expr list) into an EObjectDecl expression.
	Each argument must be either:
	  - `EConst (Ident key)` which is treated as `key = true`
	  - `EBinop (OpAssign, EConst (Ident key), value)` which is treated as `key = value`
*)
let of_metadata_entry ((_, exprs, pos) : metadata_entry) : expr =
	let fields = List.map (fun expr -> match fst expr with
		| EConst (Ident key) ->
			let kpos = snd expr in
			((key, kpos, NoQuotes), (EConst (Ident "true"), kpos))
		| EBinop (OpAssign, (EConst (Ident key), kpos), value) ->
			((key, kpos, NoQuotes), value)
		| _ ->
			error (Printf.sprintf "Expected identifier or identifier = value, got %s" (Printer.s_expr_inner "" expr)) (snd expr)
	) exprs in
	(EObjectDecl fields, pos)

module MetaReaderApi = struct
	type data = expr

	let read_optional data f = match fst data with
		| EObjectDecl [] ->
			()
		| _ ->
			f data

	let read_object data = match fst data with
		| EObjectDecl fl ->
			List.map (fun ((key, _, _), value) -> (key, value)) fl
		| _ ->
			error (Printf.sprintf "Expected object, got %s" (Printer.s_expr_inner "" data)) (snd data)

	let read_array data = match fst data with
		| EArrayDecl l ->
			l
		| _ ->
			error (Printf.sprintf "Expected array, got %s" (Printer.s_expr_inner "" data)) (snd data)

	let read_string data = match fst data with
		| EConst (String (s, _)) ->
			s
		| _ ->
			error (Printf.sprintf "Expected string, got %s" (Printer.s_expr_inner "" data)) (snd data)

	let read_bool data = match fst data with
		| EConst (Ident "true") ->
			true
		| EConst (Ident "false") ->
			false
		| _ ->
			error (Printf.sprintf "Expected bool, got %s" (Printer.s_expr_inner "" data)) (snd data)

	let read_int data = match fst data with
		| EConst (Int (s, None)) ->
			(try int_of_string s
			with Failure _ -> error (Printf.sprintf "Invalid int: %s" s) (snd data))
		| _ ->
			error (Printf.sprintf "Expected int, got %s" (Printer.s_expr_inner "" data)) (snd data)

	let data_to_string data =
		Printer.s_expr_inner "" data
end
