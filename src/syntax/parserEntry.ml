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
open Grammar
open DisplayPosition

(* eval *)
type small_type =
	| TNull
	| TBool of bool
	| TFloat of float
	| TString of string

let is_true = function
	| TBool false | TNull | TFloat 0. | TString "" -> false
	| _ -> true

let cmp v1 v2 =
	match v1, v2 with
	| TNull, TNull -> 0
	| TFloat a, TFloat b -> compare a b
	| TString a, TString b -> compare a b
	| TBool a, TBool b -> compare a b
	| TString a, TFloat b -> compare (float_of_string a) b
	| TFloat a, TString b -> compare a (float_of_string b)
	| _ -> raise Exit (* always false *)

let rec eval ctx (e,p) =
	match e with
	| EConst (Ident i) ->
		(try TString (Define.raw_defined_value ctx i) with Not_found -> TNull)
	| EConst (String s) -> TString s
	| EConst (Int i) -> TFloat (float_of_string i)
	| EConst (Float f) -> TFloat (float_of_string f)
	| EBinop (OpBoolAnd, e1, e2) -> TBool (is_true (eval ctx e1) && is_true (eval ctx e2))
	| EBinop (OpBoolOr, e1, e2) -> TBool (is_true (eval ctx e1) || is_true(eval ctx e2))
	| EUnop (Not, _, e) -> TBool (not (is_true (eval ctx e)))
	| EParenthesis e -> eval ctx e
	| EBinop (op, e1, e2) ->
		let v1 = eval ctx e1 in
		let v2 = eval ctx e2 in
		let compare op =
			TBool (try op (cmp v1 v2) 0 with _ -> false)
		in
		(match op with
		| OpEq -> compare (=)
		| OpNotEq -> compare (<>)
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

(* parse main *)
let parse ctx code file =
	let old = Lexer.save() in
	let restore_cache = TokenCache.clear () in
	let was_display = !in_display in
	let was_display_file = !in_display_file in
	let old_code = !code_ref in
	let old_macro = !in_macro in
	code_ref := code;
	in_display := display_position#get <> null_pos;
	in_display_file := !in_display && Path.unique_full_path file = (display_position#get).pfile;
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

	let sraw = Stream.from (fun _ -> Some (Lexer.token code)) in
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
			if !in_display_file && display_position#enclosed_in (pos tk) then syntax_completion SCComment (pos tk);
			next_token()
		| Sharp "end" ->
			(match !mstack with
			| [] -> tk
			| _ :: l ->
				mstack := l;
				next_token())
		| Sharp "else" | Sharp "elseif" ->
			(match !mstack with
			| [] -> tk
			| _ :: l ->
				mstack := l;
				process_token (skip_tokens (snd tk) false))
		| Sharp "if" ->
			process_token (enter_macro (snd tk))
		| Sharp "error" ->
			(match Lexer.token code with
			| (Const (String s),p) -> error (Custom s) p
			| _ -> error Unimplemented (snd tk))
		| Sharp "line" ->
			let line = (match next_token() with
				| (Const (Int s),p) -> (try int_of_string s with _ -> error (Custom ("Could not parse ridiculous line number " ^ s)) p)
				| (t,p) -> error (Unexpected t) p
			) in
			!(Lexer.cur).Lexer.lline <- line - 1;
			next_token();
		| _ ->
			tk

	and enter_macro p =
		let tk, e = parse_macro_cond sraw in
		let tk = (match tk with None -> Lexer.token code | Some tk -> tk) in
		if is_true (eval ctx e) then begin
			mstack := p :: !mstack;
			tk
		end else
			skip_tokens_loop p true tk

	and skip_tokens_loop p test tk =
		match fst tk with
		| Sharp "end" ->
			Lexer.token code
		| Sharp "elseif" | Sharp "else" when not test ->
			skip_tokens p test
		| Sharp "else" ->
			mstack := snd tk :: !mstack;
			Lexer.token code
		| Sharp "elseif" ->
			enter_macro (snd tk)
		| Sharp "if" ->
			skip_tokens_loop p test (skip_tokens p false)
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
		let l = parse_file s in
		(match !mstack with p :: _ -> syntax_error Unclosed_conditional ~pos:(Some p) sraw () | _ -> ());
		let was_display_file = !in_display_file in
		restore();
		Lexer.restore old;
		if was_display_file then
			ParseDisplayFile(l,List.rev !syntax_errors)
		else begin match List.rev !syntax_errors with
			| [] -> ParseSuccess l
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

let parse_string com s p error inlined =
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
	Lexer.init p.pfile true;
	if not inlined then begin
		display_position#reset;
		in_display_file := false;
	end;
	let result = try
		parse com (Sedlexing.Utf8.from_string s) p.pfile
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
	let head = "class X{static function main() " in
	let head = (if p.pmin > String.length head then head ^ String.make (p.pmin - String.length head) ' ' else head) in
	let rec loop e = let e = Ast.map_expr loop e in (fst e,p) in
	let extract_expr (_,decls) = match decls with
		| [EClass { d_data = [{ cff_name = "main",null_pos; cff_kind = FFun { f_expr = Some e } }]},_] -> (if inl then e else loop e)
		| _ -> raise Exit
	in
	match parse_string com (head ^ s ^ ";}") p error inl with
	| ParseSuccess data -> ParseSuccess(extract_expr data)
	| ParseError(data,error,errors) -> ParseError(extract_expr data,error,errors)
	| ParseDisplayFile(data,errors) -> ParseDisplayFile(extract_expr data,errors)
