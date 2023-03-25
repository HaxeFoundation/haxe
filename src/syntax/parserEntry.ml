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
open Globals
open Ast
open Parser
open Semver
open Grammar
open DisplayPosition

(* eval *)
type small_type =
	| TNull
	| TBool of bool
	| TFloat of float
	| TString of string
	| TVersion of (version * version * version) * (version list option)

let is_true = function
	| TBool false | TNull | TFloat 0. -> false
	| _ -> true

let s_small_type v =
	match v with
	| TNull -> "null"
	| TBool _ -> "boolean"
	| TFloat _ -> "float"
	| TString _ -> "string"
	| TVersion _ -> "version"

let s_value v =
	match v with
	| TNull -> "null"
	| TBool b -> "boolean " ^ (string_of_bool b)
	| TFloat f -> "float " ^ (string_of_float f)
	| TString s -> "string \"" ^ s ^ "\""
	| TVersion (r,p) -> "version " ^ (Semver.to_string (r,p))

let parse_version s p =
	try match parse_version s with release,pre -> TVersion (release,pre)
	with Invalid_argument msg -> error (Custom msg) p

let cmp v1 v2 =
	match v1, v2 with
	| TNull, _ | _, TNull -> raise Exit
	| TFloat a, TFloat b -> compare a b
	| TString a, TString b -> compare a b
	| TBool a, TBool b -> compare a b
	| TVersion (release1,pre1), TVersion (release2,pre2) -> compare_version (release1,pre1) (release2,pre2)
	| TString _, TFloat _ | TFloat _, TString _
	| _, TVersion _ | TVersion _, _ ->
		raise (Invalid_argument ("Cannot compare " ^ (s_value v1) ^ " and " ^ (s_value v2)))
	| _ -> raise Exit (* always false *)

let rec eval ctx (e,p) =
	match e with
	| EConst (Ident i) ->
		(try TString (Define.raw_defined_value ctx i) with Not_found -> TNull)
	| EConst (String(s,_)) -> TString s
	| EConst (Int (i, _)) -> TFloat (float_of_string i)
	| EConst (Float (f, _)) -> TFloat (float_of_string f)
	| ECall ((EConst (Ident "version"),_),[(EConst (String(s,_)), p)]) -> parse_version s p
	| EBinop (OpBoolAnd, e1, e2) -> TBool (is_true (eval ctx e1) && is_true (eval ctx e2))
	| EBinop (OpBoolOr, e1, e2) -> TBool (is_true (eval ctx e1) || is_true(eval ctx e2))
	| EUnop (Not, _, e) -> TBool (not (is_true (eval ctx e)))
	| EParenthesis e -> eval ctx e
	| EBinop (op, e1, e2) ->
		let v1, v2 = eval_binop_exprs ctx e1 e2 in
		let compare op =
			try TBool (try op (cmp v1 v2) 0 with Exit -> false)
			with Invalid_argument msg -> error (Custom msg) p
		in
		(match op with
		| OpEq -> compare (=)
		| OpNotEq -> TBool (not (is_true (compare (=))))
		| OpGt -> compare (>)
		| OpGte -> compare (>=)
		| OpLt -> compare (<)
		| OpLte -> compare (<=)
		| _ -> error (Custom "Unsupported operation") p)
	| EField _ ->
		begin try
			let sl = string_list_of_expr_path_raise (e,p) in
			let i = String.concat "." (List.rev sl) in
			(try TString (Define.raw_defined_value ctx i) with Not_found -> TNull)
		with Exit ->
			error (Custom "Invalid condition expression") p
		end
	| _ ->
		error (Custom "Invalid condition expression") p

(**
	Attempt to auto-cast operands to a common type
*)
and eval_binop_exprs ctx e1 e2 =
	match eval ctx e1, eval ctx e2 with
	| (TString s1 as v1), (TFloat _ as v2) ->
		(try TFloat (float_of_string s1), v2
		with Failure _ -> v1, v2)
	| (TFloat _ as v1), (TString s2 as v2) ->
		(try v1, TFloat (float_of_string s2)
		with Failure _ -> v1, v2)
	| (TVersion _ as v1), TString s -> (v1, parse_version s (snd e2))
	| TString s, (TVersion _ as v2) -> (parse_version s (snd e1), v2)
	| v1, v2 -> (v1, v2)

class condition_handler = object(self)
	val mutable conditional_expressions = []
	val mutable conditional_stack = []
	val mutable depths = []

	method private maybe_parent allow_and e = match fst e with
		| EBinop(op,_,_) ->
			if op = OpBoolAnd && allow_and then e
			else (EParenthesis e,pos e)
		| _ -> e

	method private negate (e : expr) = match fst e with
		| EUnop(Not,_,e1) -> e1
		| EBinop(OpBoolAnd,e1,e2) -> (EBinop(OpBoolOr,self#negate e1,self#negate e2),(pos e))
		| EBinop(OpBoolOr,e1,e2) -> (EBinop(OpBoolAnd,self#negate e1,self#negate e2),(pos e))
		| _ -> (EUnop(Not,Prefix,e),(pos e))

	method private conjoin (lhs : expr) (rhs : expr) =
		let lhs = self#maybe_parent true lhs in
		let rhs = self#maybe_parent true rhs in
		(EBinop(OpBoolAnd,lhs,rhs),punion (pos lhs) (pos rhs))

	method private cond_if' (e : expr) =
		conditional_expressions <- e :: conditional_expressions;
		conditional_stack <- e :: conditional_stack

	method cond_if (e : expr) =
		self#cond_if' e;
		depths <- 1 :: depths

	method cond_else = match conditional_stack with
		| e :: el ->
			conditional_stack <- (self#negate e) :: el
		| [] ->
			die "" __LOC__

	method cond_elseif (e : expr) =
		self#cond_else;
		self#cond_if' e;
		match depths with
		| [] -> die "" __LOC__
		| depth :: depths' ->
			depths <- (depth + 1) :: depths'

	method cond_end =
		let rec loop d el =
			if d = 0 then el
			else loop (d - 1) (List.tl el)
		in
		match depths with
			| [] -> die "" __LOC__
			| depth :: depths' ->
				conditional_stack <- loop depth conditional_stack;
				depths <- depths'

	method get_current_condition = match conditional_stack with
		| e :: el ->
			List.fold_left self#conjoin e el
		| [] ->
			(EConst (Ident "true"),null_pos)

	method get_conditions =
		conditional_expressions
end

class dead_block_collector conds = object(self)
	val dead_blocks = DynArray.create ()
	val mutable current_block = []

	method open_dead_block (p : pos) =
		current_block <- ({p with pmin = p.pmax},conds#get_current_condition) :: current_block

	method close_dead_block (p : pos) = match current_block with
		| [] ->
			error (Custom "Internal error: Trying to close dead block that's not open") p;
		| (p0,cond) :: pl ->
			current_block <- pl;
			DynArray.add dead_blocks ({p0 with pmax = p.pmin},cond)

	method get_dead_blocks : (pos * expr) list =
		assert(current_block = []);
		DynArray.to_list dead_blocks
end

(* parse main *)
let parse entry ctx code file =
	let old = Lexer.save() in
	let restore_cache = TokenCache.clear () in
	let was_display = !in_display in
	let was_display_file = !in_display_file in
	let old_code = !code_ref in
	let old_macro = !in_macro in
	code_ref := code;
	in_display := display_position#get <> null_pos;
	in_display_file := !in_display && display_position#is_in_file (Path.UniqueKey.create file);
	syntax_errors := [];
	let restore =
		(fun () ->
			restore_cache ();
			in_display := was_display;
			in_macro := old_macro;
			in_display_file := was_display_file;
			code_ref := old_code;
		)
	in
	let mstack = ref [] in
	last_doc := None;
	in_macro := Define.defined ctx Define.Macro;
	Lexer.skip_header code;

	let sharp_error s p =
		let line = StringError.string_error ("#" ^ s) ["#if";"#elseif";"#else";"#end";"#error";"#line"] "Unknown token" in
		error (Custom line) p
	in

	let conds = new condition_handler in
	let dbc = new dead_block_collector conds in
	let sraw = Stream.from (fun _ -> Some (Lexer.sharp_token code)) in
	let rec next_token() = process_token (Lexer.token code)

	and process_token tk =
		match fst tk with
		| Comment s ->
			(* if encloses_resume (pos tk) then syntax_completion SCComment (pos tk); *)
			let tk = next_token() in
			let l = String.length s in
			if l > 0 && s.[0] = '*' then last_doc := Some (String.sub s 1 (l - (if l > 1 && s.[l-1] = '*' then 2 else 1)), (snd tk).pmin);
			tk
		| CommentLine s ->
			if !in_display_file then begin
				let p = pos tk in
				(* Completion at the / should not pick up the comment (issue #9133) *)
				let p = if is_completion() then {p with pmin = p.pmin + 1} else p in
				(* The > 0 check is to deal with the special case of line comments at the beginning of the file (issue #10322) *)
				if display_position#enclosed_in p && p.pmin > 0 then syntax_completion SCComment None (pos tk);
			end;
			next_token()
		| Sharp "end" ->
			(match !mstack with
			| [] -> tk
			| _ :: l ->
				conds#cond_end;
				mstack := l;
				next_token())
		| Sharp "elseif" ->
			(match !mstack with
			| [] -> tk
			| _ :: l ->
				let _,(e,pe) = parse_macro_cond sraw in
				conds#cond_elseif (e,pe);
				dbc#open_dead_block pe;
				mstack := l;
				let tk = skip_tokens (pos tk) false in
				process_token tk)
		| Sharp "else" ->
			(match !mstack with
			| [] -> tk
			| _ :: l ->
				conds#cond_else;
				dbc#open_dead_block (pos tk);
				mstack := l;
				let tk = skip_tokens (pos tk) false in
				process_token tk)
		| Sharp "if" ->
			process_token (enter_macro true (snd tk))
		| Sharp "error" ->
			(match Lexer.token code with
			| (Const (String(s,_)),p) -> error (Custom s) p
			| _ -> error Unimplemented (snd tk))
		| Sharp "line" ->
			let line = (match next_token() with
				| (Const (Int (s, _)),p) -> (try int_of_string s with _ -> error (Custom ("Could not parse ridiculous line number " ^ s)) p)
				| (t,p) -> error (Unexpected t) p
			) in
			!(Lexer.cur).Lexer.lline <- line - 1;
			next_token();
		| Sharp s ->
			sharp_error s (pos tk)
		| _ ->
			tk

	and enter_macro is_if p =
		let tk, e = parse_macro_cond sraw in
		(if is_if then conds#cond_if else conds#cond_elseif) e;
		let tk = (match tk with None -> Lexer.token code | Some tk -> tk) in
		if is_true (eval ctx e) then begin
			mstack := p :: !mstack;
			tk
		end else begin
			dbc#open_dead_block (pos e);
			skip_tokens_loop p true tk
		end

	and skip_tokens_loop p test tk =
		match fst tk with
		| Sharp "end" ->
			conds#cond_end;
			dbc#close_dead_block (pos tk);
			Lexer.token code
		| Sharp "elseif" when not test ->
			dbc#close_dead_block (pos tk);
			let _,(e,pe) = parse_macro_cond sraw in
			conds#cond_elseif (e,pe);
			dbc#open_dead_block pe;
			skip_tokens p test
		| Sharp "else" when not test ->
			conds#cond_else;
			dbc#close_dead_block (pos tk);
			dbc#open_dead_block (pos tk);
			skip_tokens p test
		| Sharp "else" ->
			conds#cond_else;
			dbc#close_dead_block (pos tk);
			mstack := snd tk :: !mstack;
			Lexer.token code
		| Sharp "elseif" ->
			dbc#close_dead_block (pos tk);
			enter_macro false (snd tk)
		| Sharp "if" ->
			let _,e = parse_macro_cond sraw in
			conds#cond_if e;
			dbc#open_dead_block (pos e);
			let tk = skip_tokens p false in
			skip_tokens_loop p test tk
		| Sharp ("error" | "line") ->
			skip_tokens p test
		| Sharp s ->
			sharp_error s (pos tk)
		| Eof ->
			syntax_error Unclosed_conditional ~pos:(Some p) sraw tk
		| _ ->
			skip_tokens p test

	and skip_tokens p test = skip_tokens_loop p test (Lexer.token code)

	in
	let s = Stream.from (fun _ ->
		let t = next_token() in
		TokenCache.add t;
		Some t
	) in
	try
		let l = entry s in
		(match !mstack with p :: _ -> syntax_error Unclosed_conditional ~pos:(Some p) sraw () | _ -> ());
		let was_display_file = !in_display_file in
		restore();
		Lexer.restore old;
		let pdi = {pd_errors = List.rev !syntax_errors;pd_dead_blocks = dbc#get_dead_blocks;pd_conditions = conds#get_conditions} in
		if was_display_file then
			ParseSuccess(l,true,pdi)
		else begin match List.rev !syntax_errors with
			| [] -> ParseSuccess(l,false,pdi)
			| error :: errors -> ParseError(l,error,errors)
		end
	with
		| Stream.Error _
		| Stream.Failure ->
			let last = (match Stream.peek s with None -> last_token s | Some t -> t) in
			Lexer.restore old;
			restore();
			error (Unexpected (fst last)) (pos last)
		| e ->
			Lexer.restore old;
			restore();
			raise e

let parse_string entry com s p error inlined =
	let old = Lexer.save() in
	let old_file = (try Some (Hashtbl.find Lexer.all_files p.pfile) with Not_found -> None) in
	let old_display = display_position#get in
	let old_in_display_file = !in_display_file in
	let old_syntax_errors = !syntax_errors in
	syntax_errors := [];
	let restore() =
		(match old_file with
		| None -> ()
		| Some f -> Hashtbl.replace Lexer.all_files p.pfile f);
		if not inlined then begin
			display_position#set old_display;
			in_display_file := old_in_display_file;
		end;
		syntax_errors := old_syntax_errors;
		Lexer.restore old
	in
	if inlined then
		Lexer.init p.pfile
	else begin
		display_position#reset;
		in_display_file := false;
	end;
	let result = try
		parse entry com (Sedlexing.Utf8.from_string s) p.pfile
	with Error (e,pe) ->
		restore();
		error (error_msg e) (if inlined then pe else p)
	| Lexer.Error (e,pe) ->
		restore();
		error (Lexer.error_msg e) (if inlined then pe else p)
	in
	restore();
	result

let parse_expr_string com s p error inl =
	let s = if p.pmin > 0 then (String.make p.pmin ' ') ^ s else s in
	let result = parse_string expr com s p error inl in
	if inl then
		result
	else begin
		let rec loop e =
			let e = map_expr loop e in
			(fst e,p)
		in
		match result with
		| ParseSuccess(data,is_display_file,pdi) -> ParseSuccess(loop data,is_display_file,pdi)
		| ParseError(data,error,errors) -> ParseError(loop data,error,errors)
	end
