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

(* C# AST to source code printer.
   Converts the intermediate C# AST (cs_expr, cs_stmt, cs_type_def) into
   formatted C# source code strings.

   Main functions:
   - print_type_def: Converts a complete type definition to C# source
   - print_expr: Converts expressions to C# code
   - print_stmt: Converts statements to C# code
   - print_type: Converts type references to C# type syntax

   The printer handles indentation, operator precedence, and C# syntax rules. *)

open CsAst
open CsGlobals
open CsTypeMapping

(* Printer context *)
type printer_ctx = {
	mutable indent : int;
	mutable buf : Buffer.t;
}

let create_printer () = {
	indent = 0;
	buf = Buffer.create 16384;
}

let newline ctx =
	Buffer.add_char ctx.buf '\n';
	for _ = 1 to ctx.indent do
		Buffer.add_string ctx.buf "    "
	done

let print ctx s =
	Buffer.add_string ctx.buf s

let print_char ctx c =
	Buffer.add_char ctx.buf c

let indent ctx =
	ctx.indent <- ctx.indent + 1

let unindent ctx =
	ctx.indent <- ctx.indent - 1

let get_output ctx =
	Buffer.contents ctx.buf

(* Escape string for C# *)
let escape_string s =
	let b = Buffer.create (String.length s + 2) in
	Buffer.add_char b '"';
	String.iter (fun c ->
		match c with
		| '"' -> Buffer.add_string b "\\\""
		| '\\' -> Buffer.add_string b "\\\\"
		| '\n' -> Buffer.add_string b "\\n"
		| '\r' -> Buffer.add_string b "\\r"
		| '\t' -> Buffer.add_string b "\\t"
		| c when Char.code c < 32 ->
			(* Use \uXXXX format (exactly 4 hex digits) instead of \xXX because
			   C#'s \x escape is greedy and can consume up to 4 hex digits,
			   causing issues when followed by hex characters (e.g., \x05B -> 0x5B) *)
			Buffer.add_string b (Printf.sprintf "\\u%04x" (Char.code c))
		| c -> Buffer.add_char b c
	) s;
	Buffer.add_char b '"';
	Buffer.contents b

let escape_char c =
	match c with
	| '\'' -> "'\\''"
	| '\\' -> "'\\\\'"
	| '\n' -> "'\\n'"
	| '\r' -> "'\\r'"
	| '\t' -> "'\\t'"
	| c when Char.code c < 32 ->
		(* Use \uXXXX for char literals too for consistency *)
		Printf.sprintf "'\\u%04x'" (Char.code c)
	| c -> Printf.sprintf "'%c'" c

(* Print constant *)
let print_const ctx = function
	| CsConstNull -> print ctx "null"
	| CsConstBool true -> print ctx "true"
	| CsConstBool false -> print ctx "false"
	| CsConstInt i -> print ctx (Int32.to_string i)
	| CsConstLong i -> print ctx (Int64.to_string i ^ "L")
	| CsConstFloat f ->
		let s = Printf.sprintf "%.17g" f in
		print ctx s;
		if not (String.contains s '.') && not (String.contains s 'e') then
			print ctx ".0";
		print ctx "f"
	| CsConstDouble f ->
		let s = Printf.sprintf "%.17g" f in
		print ctx s;
		if not (String.contains s '.') && not (String.contains s 'e') then
			print ctx ".0"
	| CsConstString s -> print ctx (escape_string s)
	| CsConstChar c -> print ctx (escape_char c)

(* Type to string for generated C# code *)
let rec s_cs_type = function
	| CsTypeVoid -> "void"
	| CsTypeBool -> "bool"
	| CsTypeByte -> "byte"
	| CsTypeSByte -> "sbyte"
	| CsTypeChar -> "char"
	| CsTypeShort -> "short"
	| CsTypeUShort -> "ushort"
	| CsTypeInt -> "int"
	| CsTypeUInt -> "uint"
	| CsTypeLong -> "long"
	| CsTypeULong -> "ulong"
	| CsTypeFloat -> "float"
	| CsTypeDouble -> "double"
	| CsTypeDecimal -> "decimal"
	| CsTypeString -> "string"
	| CsTypeObject -> "object"
	| CsTypeDynamic -> "dynamic"
	| CsTypeNullable t -> s_cs_type t ^ "?"
	| CsTypeArray (t, None) -> s_cs_type t ^ "[]"
	| CsTypeArray (t, Some rank) ->
		s_cs_type t ^ "[" ^ String.make (rank - 1) ',' ^ "]"
	| CsTypeClass (([], name), []) -> name
	| CsTypeClass ((pack, name), []) ->
		(* Use global:: prefix to avoid namespace conflicts.
		   This ensures haxe.root.HaxeObject is always the global namespace path,
		   not relative to the current namespace (e.g., unit.spec.haxe.root) *)
		"global::" ^ String.concat "." pack ^ "." ^ name
	| CsTypeClass ((["haxe"; "root"], "Array"), _) ->
		(* Haxe Array is non-generic in C# - always output without type parameters *)
		"global::haxe.root.Array"
	| CsTypeClass (([], name), params) ->
		(* No package, just type with params *)
		name ^ "<" ^ String.concat ", " (List.map s_cs_type params) ^ ">"
	| CsTypeClass ((pack, name), params) ->
		(* Package with params - use global:: *)
		"global::" ^ String.concat "." pack ^ "." ^ name ^ "<" ^ String.concat ", " (List.map s_cs_type params) ^ ">"
	| CsTypeNested (parent, nested_name) ->
		(* Nested type: ParentType<T>.NestedClass *)
		s_cs_type parent ^ "." ^ nested_name
	| CsTypeNestedGeneric (parent, nested_name, params) ->
		(* Nested generic type: ParentType<T>.NestedClass<C> *)
		s_cs_type parent ^ "." ^ nested_name ^ "<" ^ String.concat ", " (List.map s_cs_type params) ^ ">"
	| CsTypeGenericParam name -> name
	| CsTypeFunc (args, ret) ->
		(* In C#, void cannot be used as a type argument, so Func<..., void> is invalid.
		   Instead, use Action<...> for void-returning delegates. *)
		begin match ret with
		| CsTypeVoid ->
			begin match args with
			| [] -> "Action"
			| _ -> "Action<" ^ String.concat ", " (List.map s_cs_type args) ^ ">"
			end
		| _ ->
			begin match args with
			| [] -> "Func<" ^ s_cs_type ret ^ ">"
			| _ -> "Func<" ^ String.concat ", " (List.map s_cs_type args @ [s_cs_type ret]) ^ ">"
			end
		end
	| CsTypeAction [] ->
		"Action"
	| CsTypeAction args ->
		"Action<" ^ String.concat ", " (List.map s_cs_type args) ^ ">"
	| CsTypeVar -> "var"

(* Print type *)
let print_type ctx t =
	print ctx (s_cs_type t)

(* Print type with generic parameters *)
let print_type_params ctx = function
	| [] -> ()
	| params ->
		print ctx "<";
		let first = ref true in
		List.iter (fun p ->
			if !first then first := false else print ctx ", ";
			print ctx p
		) params;
		print ctx ">"

(* Check if an expression is valid as a C# statement *)
let rec is_valid_statement_expr = function
	| CsCall _ | CsStaticCall _ | CsNew _ | CsCallGeneric _ | CsStaticCallGeneric _ -> true
	| CsBinop (CsOpAssign, _, _) | CsBinop (CsOpAssignOp _, _, _) -> true
	| CsUnop (CsOpIncrement, _, _) | CsUnop (CsOpDecrement, _, _) -> true
	| CsParens e -> is_valid_statement_expr e
	| CsAwait _ -> true
	| CsRaw _ | CsInlineCode _ -> true  (* Raw code and inline code pass through as-is *)
	| _ -> false

(* Print parameter *)
let rec print_param ctx p =
	begin match p.p_modifier with
	| None -> ()
	| Some CsParamRef -> print ctx "ref "
	| Some CsParamOut -> print ctx "out "
	| Some CsParamIn -> print ctx "in "
	| Some CsParamParams -> print ctx "params "
	end;
	begin match p.p_type with
	| Some t -> print_type ctx t; print ctx " "
	| None -> ()
	end;
	print ctx p.p_name;
	begin match p.p_default with
	| None -> ()
	| Some e -> print ctx " = "; print_expr ctx e
	end

and print_params ctx params =
	print ctx "(";
	let first = ref true in
	List.iter (fun p ->
		if !first then first := false else print ctx ", ";
		print_param ctx p
	) params;
	print ctx ")"

(* Print expression *)
and print_expr ctx = function
	| CsConst c -> print_const ctx c
	| CsLocal name -> print ctx (escape_identifier name)
	| CsThis -> print ctx "this"
	| CsBase -> print ctx "base"
	| CsNull -> print ctx "null"
	| CsDefault t -> print ctx "default("; print_type ctx t; print ctx ")"
	| CsTypeOf t -> print ctx "typeof("; print_type ctx t; print ctx ")"
	| CsNameOf s -> print ctx "nameof("; print ctx s; print ctx ")"
	| CsSizeOf t -> print ctx "sizeof("; print_type ctx t; print ctx ")"
	| CsBinop (op, e1, e2) ->
		print_expr ctx e1;
		print ctx " ";
		print ctx (binop_to_string op);
		print ctx " ";
		print_expr ctx e2
	| CsUnop (op, is_postfix, e) ->
		(* Prefix unary operators need parens around binary ops and ternary to preserve precedence.
		   E.g., !(a != b) should be "!(a != b)" not "!a != b" *)
		let needs_parens = match e with
			| CsBinop _ | CsTernary _ -> true
			| _ -> false
		in
		if is_postfix then begin
			print_expr ctx e;
			print ctx (unop_to_string op)
		end else begin
			print ctx (unop_to_string op);
			if needs_parens then print ctx "(";
			print_expr ctx e;
			if needs_parens then print ctx ")"
		end
	| CsTernary (cond, e1, e2) ->
		print_expr ctx cond;
		print ctx " ? ";
		print_expr ctx e1;
		print ctx " : ";
		print_expr ctx e2
	| CsField (e, name) ->
		(* Wrap low-precedence expressions in parentheses for field access.
		   Ternary (?:), binary ops, and lambdas need parens since they have lower
		   precedence than member access (.) in C#. *)
		let needs_parens = match e with
			| CsTernary _ | CsBinop _ | CsLambda _ -> true
			| _ -> false
		in
		if needs_parens then print ctx "(";
		print_expr ctx e;
		if needs_parens then print ctx ")";
		print ctx ".";
		print ctx (escape_identifier name)
	| CsStaticField (t, name) ->
		print_type ctx t;
		print ctx ".";
		print ctx (escape_identifier name)
	| CsArrayAccess (e, idx) ->
		print_expr ctx e;
		print ctx "[";
		print_expr ctx idx;
		print ctx "]"
	| CsCall (e, args) ->
		print_expr ctx e;
		print ctx "(";
		print_args ctx args;
		print ctx ")"
	| CsStaticCall (t, name, args) ->
		print_type ctx t;
		print ctx ".";
		print ctx (escape_identifier name);
		print ctx "(";
		print_args ctx args;
		print ctx ")"
	| CsStaticCallGeneric (t, name, type_args, args) ->
		print_type ctx t;
		print ctx ".";
		print ctx (escape_identifier name);
		print ctx "<";
		let first = ref true in
		List.iter (fun ta ->
			if !first then first := false else print ctx ", ";
			print_type ctx ta
		) type_args;
		print ctx ">(";
		print_args ctx args;
		print ctx ")"
	| CsCallGeneric (e, type_args, args) ->
		print_expr ctx e;
		print ctx "<";
		let first = ref true in
		List.iter (fun ta ->
			if !first then first := false else print ctx ", ";
			print_type ctx ta
		) type_args;
		print ctx ">(";
		print_args ctx args;
		print ctx ")"
	| CsNew (t, args) ->
		print ctx "new ";
		print_type ctx t;
		print ctx "(";
		print_args ctx args;
		print ctx ")"
	| CsNewArray (t, items) ->
		print ctx "new ";
		print_type ctx t;
		print ctx "[] { ";
		print_args ctx items;
		print ctx " }"
	| CsNewArraySize (t, size) ->
		(* For jagged arrays like int[][], we need: new int[size][]
		   not: new int[][size]
		   So we extract the inner array dimensions and append them after the size. *)
		let rec extract_array_dims dims t = match t with
			| CsTypeArray (inner, rank) ->
				let dim_str = "[" ^ (match rank with Some r -> String.make (r-1) ',' | None -> "") ^ "]" in
				extract_array_dims (dim_str :: dims) inner
			| _ -> (t, dims)
		in
		let (base_type, extra_dims) = extract_array_dims [] t in
		print ctx "new ";
		print_type ctx base_type;
		print ctx "[";
		print_expr ctx size;
		print ctx "]";
		List.iter (print ctx) extra_dims
	| CsCast (t, e) ->
		print ctx "((";
		print_type ctx t;
		print ctx ")(";
		print_expr ctx e;
		print ctx "))"
	| CsAs (e, t) ->
		print ctx "(";
		print_expr ctx e;
		print ctx " as ";
		print_type ctx t;
		print ctx ")"
	| CsIs (e, t) ->
		print ctx "(";
		print_expr ctx e;
		print ctx " is ";
		print_type ctx t;
		print ctx ")"
	| CsIsPattern (e, t, name) ->
		print ctx "(";
		print_expr ctx e;
		print ctx " is ";
		print_type ctx t;
		begin match name with
		| Some n -> print ctx " "; print ctx n
		| None -> ()
		end;
		print ctx ")"
	| CsLambda (params, body) ->
		if List.length params = 1 && (List.hd params).p_type = None then
			print ctx (List.hd params).p_name
		else begin
			print ctx "(";
			let first = ref true in
			List.iter (fun p ->
				if !first then first := false else print ctx ", ";
				begin match p.p_type with
				| Some t -> print_type ctx t; print ctx " "
				| None -> ()
				end;
				print ctx p.p_name
			) params;
			print ctx ")"
		end;
		print ctx " => ";
		begin match body with
		| CsLambdaExpr e -> print_expr ctx e
		| CsLambdaBlock stmts ->
			print ctx "{";
			indent ctx;
			List.iter (fun s -> newline ctx; print_stmt ctx s) stmts;
			unindent ctx;
			newline ctx;
			print ctx "}"
		end
	| CsParens e ->
		print ctx "(";
		print_expr ctx e;
		print ctx ")"
	| CsUnchecked e ->
		print ctx "unchecked(";
		print_expr ctx e;
		print ctx ")"
	| CsNullConditionalField (e, name) ->
		print_expr ctx e;
		print ctx "?.";
		print ctx (escape_identifier name)
	| CsNullConditionalCall (e, args) ->
		print_expr ctx e;
		print ctx "?(";
		print_args ctx args;
		print ctx ")"
	| CsNullConditionalIndex (e, idx) ->
		print_expr ctx e;
		print ctx "?[";
		print_expr ctx idx;
		print ctx "]"
	| CsAwait e ->
		print ctx "await ";
		print_expr ctx e
	| CsThrow e ->
		print ctx "throw ";
		print_expr ctx e
	| CsInterpolatedString parts ->
		print ctx "$\"";
		List.iter (function
			| CsInterpLiteral s ->
				(* Escape for interpolated string *)
				String.iter (fun c ->
					match c with
					| '"' -> print ctx "\\\""
					| '\\' -> print ctx "\\\\"
					| '{' -> print ctx "{{"
					| '}' -> print ctx "}}"
					| '\n' -> print ctx "\\n"
					| '\r' -> print ctx "\\r"
					| c -> print_char ctx c
				) s
			| CsInterpExpr (e, fmt) ->
				print ctx "{";
				print_expr ctx e;
				begin match fmt with
				| Some f -> print ctx ":"; print ctx f
				| None -> ()
				end;
				print ctx "}"
		) parts;
		print ctx "\""
	| CsRaw s -> print ctx s
	| CsInlineCode (template, args) ->
		(* Replace {0}, {1}, etc. with the corresponding argument *)
		let buf = Buffer.create (String.length template) in
		let i = ref 0 in
		let len = String.length template in
		while !i < len do
			if !i + 1 < len && template.[!i] = '{' then begin
				let start = !i + 1 in
				incr i;
				while !i < len && template.[!i] >= '0' && template.[!i] <= '9' do
					incr i
				done;
				if !i < len && template.[!i] = '}' then begin
					let idx_str = String.sub template start (!i - start) in
					let idx = int_of_string idx_str in
					if idx < List.length args then begin
						(* Print argument to a temp buffer *)
						let arg_ctx = { ctx with buf = Buffer.create 64 } in
						print_expr arg_ctx (List.nth args idx);
						Buffer.add_buffer buf arg_ctx.buf
					end else
						Buffer.add_string buf ("{" ^ idx_str ^ "}")
				end else begin
					Buffer.add_char buf '{';
					i := start
				end
			end else begin
				Buffer.add_char buf template.[!i]
			end;
			incr i
		done;
		print ctx (Buffer.contents buf)

and print_args ctx args =
	let first = ref true in
	List.iter (fun e ->
		if !first then first := false else print ctx ", ";
		print_expr ctx e
	) args

(* Print statement *)
and print_stmt ctx = function
	| CsExprStmt e ->
		(* Wrap invalid statement expressions with discard assignment *)
		if is_valid_statement_expr e then begin
			print_expr ctx e;
			print ctx ";"
		end else begin
			print ctx "_ = ";
			print_expr ctx e;
			print ctx ";"
		end
	| CsBlock stmts ->
		print ctx "{";
		indent ctx;
		List.iter (fun s -> newline ctx; print_stmt ctx s) stmts;
		unindent ctx;
		newline ctx;
		print ctx "}"
	| CsStmtList stmts ->
		(* Multiple statements without braces - emit sequentially *)
		List.iter (fun s -> print_stmt ctx s; newline ctx) stmts
	| CsVarDecl (name, typ, value) ->
		begin match typ with
		| Some t -> print_type ctx t
		| None -> print ctx "var"
		end;
		print ctx " ";
		print ctx (escape_identifier name);
		begin match value with
		| Some e -> print ctx " = "; print_expr ctx e
		| None -> ()
		end;
		print ctx ";"
	| CsMultiVarDecl (vars, typ) ->
		print_type ctx typ;
		print ctx " ";
		let first = ref true in
		List.iter (fun (name, value) ->
			if !first then first := false else print ctx ", ";
			print ctx (escape_identifier name);
			begin match value with
			| Some e -> print ctx " = "; print_expr ctx e
			| None -> ()
			end
		) vars;
		print ctx ";"
	| CsIf (cond, then_stmt, else_stmt) ->
		print ctx "if (";
		print_expr ctx cond;
		print ctx ")";
		print_block_or_stmt ctx then_stmt;
		begin match else_stmt with
		| None -> ()
		| Some s ->
			newline ctx;
			print ctx "else";
			print_block_or_stmt ctx s
		end
	| CsSwitch (e, sections) ->
		print ctx "switch (";
		print_expr ctx e;
		print ctx ")";
		newline ctx;
		print ctx "{";
		indent ctx;
		List.iter (fun section ->
			List.iter (fun label ->
				newline ctx;
				print_switch_label ctx label
			) section.sw_labels;
			indent ctx;
			List.iter (fun s -> newline ctx; print_stmt ctx s) section.sw_body;
			unindent ctx
		) sections;
		unindent ctx;
		newline ctx;
		print ctx "}"
	| CsWhile (cond, body) ->
		print ctx "while (";
		print_expr ctx cond;
		print ctx ")";
		print_block_or_stmt ctx body
	| CsDoWhile (body, cond) ->
		print ctx "do";
		print_block_or_stmt ctx body;
		print ctx " while (";
		print_expr ctx cond;
		print ctx ");"
	| CsFor (init, cond, incr, body) ->
		print ctx "for (";
		begin match init with
		| Some s -> print_stmt_no_semi ctx s
		| None -> ()
		end;
		print ctx "; ";
		begin match cond with
		| Some e -> print_expr ctx e
		| None -> ()
		end;
		print ctx "; ";
		begin match incr with
		| Some e -> print_expr ctx e
		| None -> ()
		end;
		print ctx ")";
		print_block_or_stmt ctx body
	| CsForeach (typ, name, expr, body) ->
		print ctx "foreach (";
		print_type ctx typ;
		print ctx " ";
		print ctx (escape_identifier name);
		print ctx " in ";
		print_expr ctx expr;
		print ctx ")";
		print_block_or_stmt ctx body
	| CsReturn None ->
		print ctx "return;"
	| CsReturn (Some e) ->
		print ctx "return ";
		print_expr ctx e;
		print ctx ";"
	| CsBreak ->
		print ctx "break;"
	| CsContinue ->
		print ctx "continue;"
	| CsThrowStmt e ->
		print ctx "throw ";
		print_expr ctx e;
		print ctx ";"
	| CsTry (body, catches, finally) ->
		print ctx "try";
		print_block_stmt ctx body;
		List.iter (fun c -> print_catch ctx c) catches;
		begin match finally with
		| None -> ()
		| Some s ->
			newline ctx;
			print ctx "finally";
			print_block_stmt ctx s
		end
	| CsUsing (e, body) ->
		print ctx "using (";
		print_expr ctx e;
		print ctx ")";
		print_block_or_stmt ctx body
	| CsUsingDecl (typ, name, e, body) ->
		print ctx "using (";
		print_type ctx typ;
		print ctx " ";
		print ctx (escape_identifier name);
		print ctx " = ";
		print_expr ctx e;
		print ctx ")";
		print_block_or_stmt ctx body
	| CsLock (e, body) ->
		print ctx "lock (";
		print_expr ctx e;
		print ctx ")";
		print_block_or_stmt ctx body
	| CsLabel name ->
		unindent ctx;
		newline ctx;
		print ctx name;
		print ctx ":";
		indent ctx
	| CsGoto name ->
		print ctx "goto ";
		print ctx name;
		print ctx ";"
	| CsEmpty ->
		print ctx ";"
	| CsComment s ->
		print ctx "// ";
		print ctx s
	| CsRawStmt s ->
		print ctx s
	| CsUncheckedStmt body ->
		print ctx "unchecked";
		print_block_stmt ctx body

and print_stmt_no_semi ctx = function
	| CsVarDecl (name, typ, value) ->
		begin match typ with
		| Some t -> print_type ctx t
		| None -> print ctx "var"
		end;
		print ctx " ";
		print ctx (escape_identifier name);
		begin match value with
		| Some e -> print ctx " = "; print_expr ctx e
		| None -> ()
		end
	| CsExprStmt e -> print_expr ctx e
	| s -> print_stmt ctx s

and print_block_or_stmt ctx s =
	match s with
	| CsBlock _ ->
		newline ctx;
		print_stmt ctx s
	| _ ->
		indent ctx;
		newline ctx;
		print_stmt ctx s;
		unindent ctx

and print_block_stmt ctx s =
	match s with
	| CsBlock stmts ->
		newline ctx;
		print ctx "{";
		indent ctx;
		List.iter (fun s -> newline ctx; print_stmt ctx s) stmts;
		unindent ctx;
		newline ctx;
		print ctx "}"
	| _ ->
		newline ctx;
		print ctx "{";
		indent ctx;
		newline ctx;
		print_stmt ctx s;
		unindent ctx;
		newline ctx;
		print ctx "}"

and print_switch_label ctx = function
	| CsCaseConst e ->
		print ctx "case ";
		print_expr ctx e;
		print ctx ":"
	| CsCasePattern (t, name) ->
		print ctx "case ";
		print_type ctx t;
		begin match name with
		| Some n -> print ctx " "; print ctx n
		| None -> ()
		end;
		print ctx ":"
	| CsCaseWhen (t, name, cond) ->
		print ctx "case ";
		print_type ctx t;
		begin match name with
		| Some n -> print ctx " "; print ctx n
		| None -> ()
		end;
		print ctx " when ";
		print_expr ctx cond;
		print ctx ":"
	| CsCaseDefault ->
		print ctx "default:"

and print_catch ctx c =
	newline ctx;
	print ctx "catch";
	begin match c.catch_type, c.catch_name with
	| None, None -> ()
	| Some t, None ->
		print ctx " (";
		print_type ctx t;
		print ctx ")"
	| Some t, Some n ->
		print ctx " (";
		print_type ctx t;
		print ctx " ";
		print ctx (escape_identifier n);
		print ctx ")"
	| None, Some _ -> ()
	end;
	begin match c.catch_when with
	| None -> ()
	| Some e ->
		print ctx " when (";
		print_expr ctx e;
		print ctx ")"
	end;
	print_block_stmt ctx c.catch_body

(* Print access modifier *)
let print_access ctx access =
	print ctx (AccessModifier.to_string access);
	print ctx " "

(* Print member modifiers *)
let print_modifiers ctx mods =
	List.iter (fun m ->
		print ctx (MemberModifier.to_string m);
		print ctx " "
	) mods

(* Print type modifiers *)
let print_type_modifiers ctx mods =
	List.iter (fun m ->
		print ctx (TypeModifier.to_string m);
		print ctx " "
	) mods

(* Print generic constraints *)
let print_constraints ctx constraints =
	List.iter (fun (name, types) ->
		newline ctx;
		print ctx "where ";
		print ctx name;
		print ctx " : ";
		let first = ref true in
		List.iter (fun t ->
			if !first then first := false else print ctx ", ";
			print_type ctx t
		) types
	) constraints

(* Print field *)
let print_field ctx f =
	print_access ctx f.f_access;
	print_modifiers ctx f.f_modifiers;
	print_type ctx f.f_type;
	print ctx " ";
	print ctx (escape_identifier f.f_name);
	begin match f.f_value with
	| None -> ()
	| Some e -> print ctx " = "; print_expr ctx e
	end;
	print ctx ";"

(* Print property *)
let print_property ctx p =
	(* Explicit interface implementations have no access modifier and use Interface.Property syntax *)
	begin match p.prop_explicit_interface with
	| Some iface_type ->
		(* Don't print access modifier for explicit interface implementation *)
		print_modifiers ctx p.prop_modifiers;
		print_type ctx p.prop_type;
		print ctx " ";
		print_type ctx iface_type;
		print ctx ".";
		print ctx p.prop_name  (* Don't escape - interface name is already part of the qualification *)
	| None ->
		print_access ctx p.prop_access;
		print_modifiers ctx p.prop_modifiers;
		print_type ctx p.prop_type;
		print ctx " ";
		print ctx (escape_identifier p.prop_name)
	end;
	print ctx " { ";
	begin match p.prop_getter with
	| None -> ()
	| Some acc ->
		begin match acc.acc_access with
		| Some a when a <> p.prop_access ->
			print ctx (AccessModifier.to_string a);
			print ctx " "
		| _ -> ()
		end;
		print ctx "get";
		begin match acc.acc_body with
		| None -> print ctx "; "
		| Some [] -> print ctx "; "  (* auto property *)
		| Some stmts ->
			print ctx " {";
			indent ctx;
			List.iter (fun s -> newline ctx; print_stmt ctx s) stmts;
			unindent ctx;
			newline ctx;
			print ctx "} "
		end
	end;
	begin match p.prop_setter with
	| None -> ()
	| Some acc ->
		begin match acc.acc_access with
		| Some a when a <> p.prop_access ->
			print ctx (AccessModifier.to_string a);
			print ctx " "
		| _ -> ()
		end;
		print ctx "set";
		begin match acc.acc_body with
		| None -> print ctx "; "
		| Some [] -> print ctx "; "
		| Some stmts ->
			print ctx " {";
			indent ctx;
			List.iter (fun s -> newline ctx; print_stmt ctx s) stmts;
			unindent ctx;
			newline ctx;
			print ctx "} "
		end
	end;
	print ctx "}";
	begin match p.prop_init with
	| None -> ()
	| Some e -> print ctx " = "; print_expr ctx e; print ctx ";"
	end

(* Print C# attribute *)
let print_attribute ctx attr =
	print ctx "[";
	print ctx attr.attr_name;
	if attr.attr_args <> [] then begin
		print ctx "(";
		print ctx (String.concat ", " attr.attr_args);
		print ctx ")"
	end;
	print ctx "]";
	newline ctx

(* Print method *)
let print_method ctx m =
	(* Print attributes first *)
	List.iter (print_attribute ctx) m.m_attributes;
	(* Explicit interface implementations have no access modifier and use Interface.Method syntax *)
	begin match m.m_explicit_interface with
	| None ->
		print_access ctx m.m_access;
		print_modifiers ctx m.m_modifiers;
		print_type ctx m.m_return_type;
		print ctx " ";
		print ctx (escape_identifier m.m_name)
	| Some iface_type ->
		(* No access modifier for explicit interface implementation *)
		print_type ctx m.m_return_type;
		print ctx " ";
		print_type ctx iface_type;
		print ctx ".";
		print ctx (escape_identifier m.m_name)
	end;
	print_type_params ctx m.m_type_params;
	print_params ctx m.m_params;
	print_constraints ctx m.m_constraints;
	begin match m.m_body with
	| None -> print ctx ";"
	| Some stmts ->
		newline ctx;
		print ctx "{";
		indent ctx;
		List.iter (fun s -> newline ctx; print_stmt ctx s) stmts;
		unindent ctx;
		newline ctx;
		print ctx "}"
	end

(* Print constructor *)
let rec print_constructor ctx class_name c =
	print_access ctx c.ctor_access;
	print_modifiers ctx c.ctor_modifiers;
	print ctx class_name;
	print_params ctx c.ctor_params;
	begin match c.ctor_base_call with
	| Some args ->
		print ctx " : base(";
		print_args ctx args;
		print ctx ")"
	| None ->
		begin match c.ctor_this_call with
		| Some args ->
			print ctx " : this(";
			print_args ctx args;
			print ctx ")"
		| None -> ()
		end
	end;
	newline ctx;
	print ctx "{";
	indent ctx;
	List.iter (fun s -> newline ctx; print_stmt ctx s) c.ctor_body;
	unindent ctx;
	newline ctx;
	print ctx "}"

(* Print member *)
and print_member ctx class_name = function
	| CsMemberField f -> print_field ctx f
	| CsMemberProperty p -> print_property ctx p
	| CsMemberMethod m -> print_method ctx m
	| CsMemberConstructor c -> print_constructor ctx class_name c
	| CsMemberStaticConstructor stmts ->
		print ctx "static ";
		print ctx class_name;
		print ctx "()";
		newline ctx;
		print ctx "{";
		indent ctx;
		List.iter (fun s -> newline ctx; print_stmt ctx s) stmts;
		unindent ctx;
		newline ctx;
		print ctx "}"
	| CsMemberEvent _ -> print ctx "// TODO: event"
	| CsMemberIndexer _ -> print ctx "// TODO: indexer"
	| CsMemberOperator _ -> print ctx "// TODO: operator"
	| CsMemberNestedType td -> print_type_def ctx td

(* Print class *)
and print_class ctx c =
	print_access ctx c.c_access;
	print_type_modifiers ctx c.c_modifiers;
	print ctx "class ";
	print ctx (snd c.c_path);
	print_type_params ctx c.c_type_params;
	let has_base = c.c_base <> None || c.c_interfaces <> [] in
	if has_base then begin
		print ctx " : ";
		let first = ref true in
		begin match c.c_base with
		| Some t ->
			print_type ctx t;
			first := false
		| None -> ()
		end;
		List.iter (fun t ->
			if !first then first := false else print ctx ", ";
			print_type ctx t
		) c.c_interfaces
	end;
	print_constraints ctx c.c_constraints;
	newline ctx;
	print ctx "{";
	indent ctx;
	List.iter (fun m ->
		newline ctx;
		print_member ctx (snd c.c_path) m;
		newline ctx
	) c.c_members;
	unindent ctx;
	newline ctx;
	print ctx "}"

(* Print interface *)
and print_interface ctx i =
	print_access ctx i.i_access;
	print_type_modifiers ctx i.i_modifiers;
	print ctx "interface ";
	print ctx (snd i.i_path);
	print_type_params ctx i.i_type_params;
	if i.i_base <> [] then begin
		print ctx " : ";
		let first = ref true in
		List.iter (fun t ->
			if !first then first := false else print ctx ", ";
			print_type ctx t
		) i.i_base
	end;
	print_constraints ctx i.i_constraints;
	newline ctx;
	print ctx "{";
	indent ctx;
	List.iter (fun m ->
		newline ctx;
		print_member ctx (snd i.i_path) m;
		newline ctx
	) i.i_members;
	unindent ctx;
	print ctx "}"

(* Print enum *)
and print_enum ctx e =
	print_access ctx e.e_access;
	print ctx "enum ";
	print ctx (snd e.e_path);
	begin match e.e_underlying with
	| Some t -> print ctx " : "; print_type ctx t
	| None -> ()
	end;
	newline ctx;
	print ctx "{";
	indent ctx;
	let first = ref true in
	List.iter (fun m ->
		if !first then first := false else print ctx ",";
		newline ctx;
		print ctx m.em_name;
		begin match m.em_value with
		| Some e -> print ctx " = "; print_expr ctx e
		| None -> ()
		end
	) e.e_members;
	unindent ctx;
	newline ctx;
	print ctx "}"

(* Print type definition *)
and print_type_def ctx = function
	| CsClassDef c -> print_class ctx c
	| CsStructDef _ -> print ctx "// TODO: struct"
	| CsInterfaceDef i -> print_interface ctx i
	| CsEnumDef e -> print_enum ctx e
	| CsDelegateDef _ -> print ctx "// TODO: delegate"

(* Print using directive *)
let print_using ctx = function
	| CsUsingNamespace ns ->
		print ctx "using ";
		print ctx (String.concat "." ns);
		print ctx ";"
	| CsUsingStatic path ->
		print ctx "using static ";
		print ctx (s_cs_path path);
		print ctx ";"
	| CsUsingAlias (alias, typ) ->
		print ctx "using ";
		print ctx alias;
		print ctx " = ";
		print_type ctx typ;
		print ctx ";"

(* Print file *)
let print_file ctx file =
	(* Suppress common warnings in generated code:
	   CA2200: Re-throwing caught exception changes stack information - Haxe exception handling intentionally re-throws
	   CS0168: Variable declared but never used - unavoidable in generated code
	   CS0219: Variable assigned but never used - unavoidable in generated code
	   CS1718: Comparison to same variable - intentional NaN checks (x != x) *)
	print ctx "#pragma warning disable CA2200, CS0168, CS0219, CS1718";
	newline ctx;
	newline ctx;
	List.iter (fun u ->
		print_using ctx u;
		newline ctx
	) file.file_usings;
	if file.file_usings <> [] then newline ctx;
	begin match file.file_namespace with
	| None ->
		List.iter (fun td ->
			print_type_def ctx td;
			newline ctx;
			newline ctx
		) file.file_types
	| Some ns ->
		print ctx "namespace ";
		print ctx (String.concat "." ns);
		newline ctx;
		print ctx "{";
		indent ctx;
		List.iter (fun td ->
			newline ctx;
			print_type_def ctx td;
			newline ctx
		) file.file_types;
		unindent ctx;
		newline ctx;
		print ctx "}"
	end;
	(* Print top-level types (like closures) outside the namespace *)
	if file.file_top_level_types <> [] then begin
		newline ctx;
		newline ctx;
		List.iter (fun td ->
			print_type_def ctx td;
			newline ctx;
			newline ctx
		) file.file_top_level_types
	end

(* Generate C# source from file *)
let generate_file file =
	let ctx = create_printer () in
	print_file ctx file;
	get_output ctx

(* Generate .csproj content *)
let generate_csproj proj =
	let b = Buffer.create 1024 in
	Buffer.add_string b "<Project Sdk=\"Microsoft.NET.Sdk\">\n";
	Buffer.add_string b "  <PropertyGroup>\n";
	Buffer.add_string b (Printf.sprintf "    <OutputType>%s</OutputType>\n" proj.proj_output_type);
	Buffer.add_string b (Printf.sprintf "    <TargetFramework>%s</TargetFramework>\n" proj.proj_target_framework);
	Buffer.add_string b "    <ImplicitUsings>disable</ImplicitUsings>\n";
	Buffer.add_string b "    <Nullable>disable</Nullable>\n";
	Buffer.add_string b "    <AllowUnsafeBlocks>true</AllowUnsafeBlocks>\n";
	(* Suppress CA2200: Haxe exception handling intentionally re-throws exceptions *)
	Buffer.add_string b "    <NoWarn>$(NoWarn);CA2200</NoWarn>\n";
	Buffer.add_string b "  </PropertyGroup>\n";
	Buffer.add_string b "</Project>\n";
	Buffer.contents b
