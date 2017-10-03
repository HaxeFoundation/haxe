(*
	The Haxe Compiler
	Copyright (C) 2005-2017  Haxe Foundation

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

open Sedlexing
open Sedlexing.Utf8
open Ast
open Globals

type error_msg =
	| Invalid_character of int
	| Unterminated_string
	| Unterminated_regexp
	| Unclosed_comment
	| Unclosed_code
	| Invalid_escape of char
	| Invalid_option

exception Error of error_msg * pos

let error_msg = function
	| Invalid_character c when c > 32 && c < 128 -> Printf.sprintf "Invalid character '%c'" (char_of_int c)
	| Invalid_character c -> Printf.sprintf "Invalid character 0x%.2X" c
	| Unterminated_string -> "Unterminated string"
	| Unterminated_regexp -> "Unterminated regular expression"
	| Unclosed_comment -> "Unclosed comment"
	| Unclosed_code -> "Unclosed code string"
	| Invalid_escape c -> Printf.sprintf "Invalid escape sequence \\%s" (Char.escaped c)
	| Invalid_option -> "Invalid regular expression option"

type lexer_file = {
	lfile : string;
	mutable lline : int;
	mutable lmaxline : int;
	mutable llines : (int * int) list;
	mutable lalines : (int * int) array;
	mutable lstrings : int list;
	mutable llast : int;
	mutable llastindex : int;
}

let make_file file =
	{
		lfile = file;
		lline = 1;
		lmaxline = 1;
		llines = [0,1];
		lalines = [|0,1|];
		lstrings = [];
		llast = max_int;
		llastindex = 0;
	}


let cur = ref (make_file "")

let all_files = Hashtbl.create 0

let buf = Buffer.create 100

let error e pos =
	raise (Error (e,{ pmin = pos; pmax = pos; pfile = !cur.lfile }))

let keywords =
	let h = Hashtbl.create 3 in
	List.iter (fun k -> Hashtbl.add h (s_keyword k) k)
		[Function;Class;Static;Var;If;Else;While;Do;For;
		Break;Return;Continue;Extends;Implements;Import;
		Switch;Case;Default;Public;Private;Try;Untyped;
		Catch;New;This;Throw;Extern;Enum;In;Interface;
		Cast;Override;Dynamic;Typedef;Package;
		Inline;Using;Null;True;False;Abstract;Macro;Final];
	h

let is_valid_identifier s = try
	for i = 0 to String.length s - 1 do
		match String.unsafe_get s i with
		| 'a'..'z' | 'A'..'Z' | '_' -> ()
		| '0'..'9' when i > 0 -> ()
		| _ -> raise Exit
	done;
	if Hashtbl.mem keywords s then raise Exit;
	true
with Exit ->
	false

let init file do_add =
	let f = make_file file in
	cur := f;
	if do_add then Hashtbl.replace all_files file f

let save() =
	!cur

let restore c =
	cur := c

let newline lexbuf =
	let cur = !cur in
	cur.lline <- cur.lline + 1;
	cur.llines <- (lexeme_end lexbuf,cur.lline) :: cur.llines

let fmt_pos p =
	p.pmin + (p.pmax - p.pmin) * 1000000

let add_fmt_string p =
	let file = (try
		Hashtbl.find all_files p.pfile
	with Not_found ->
		let f = make_file p.pfile in
		Hashtbl.replace all_files p.pfile f;
		f
	) in
	file.lstrings <- (fmt_pos p) :: file.lstrings

let fast_add_fmt_string p =
	let cur = !cur in
	cur.lstrings <- (fmt_pos p) :: cur.lstrings

let is_fmt_string p =
	try
		let file = Hashtbl.find all_files p.pfile in
		List.mem (fmt_pos p) file.lstrings
	with Not_found ->
		false

let remove_fmt_string p =
	try
		let file = Hashtbl.find all_files p.pfile in
		file.lstrings <- List.filter ((<>) (fmt_pos p)) file.lstrings
	with Not_found ->
		()

let find_line p f =
	(* rebuild cache if we have a new line *)
	if f.lmaxline <> f.lline then begin
		f.lmaxline <- f.lline;
		f.lalines <- Array.of_list (List.rev f.llines);
		f.llast <- max_int;
		f.llastindex <- 0;
	end;
	let rec loop min max =
		let med = (min + max) lsr 1 in
		let lp, line = Array.unsafe_get f.lalines med in
		if med = min then begin
			f.llast <- p;
			f.llastindex <- med;
			line, p - lp
		end else if lp > p then
			loop min med
		else
			loop med max
	in
	if p >= f.llast then begin
		let lp, line = Array.unsafe_get f.lalines f.llastindex in
		let lp2 = if f.llastindex = Array.length f.lalines - 1 then max_int else fst(Array.unsafe_get f.lalines (f.llastindex + 1)) in
		if p >= lp && p < lp2 then line, p - lp else loop 0 (Array.length f.lalines)
	end else
		loop 0 (Array.length f.lalines)

(* resolve a position within a non-haxe file by counting newlines *)
let resolve_pos file =
	let ch = open_in_bin file in
	let f = make_file file in
	let rec loop p =
		let inc i () =
			f.lline <- f.lline + 1;
			f.llines <- (p + i,f.lline) :: f.llines;
			i
		in
		let i = match input_char ch with
			| '\n' -> inc 1
			| '\r' ->
				ignore(input_char ch);
				inc 2
			| _ -> fun () -> 1
		in
		loop (p + i())
	in
	try
		loop 0
	with End_of_file ->
		close_in ch;
		f

let find_file file =
	try Hashtbl.find all_files file with Not_found -> try resolve_pos file with Sys_error _ -> make_file file

let find_pos p =
	find_line p.pmin (find_file p.pfile)

let get_error_line p =
	let l, _ = find_pos p in
	l

let old_format = ref false

let get_pos_coords p =
	let file = find_file p.pfile in
	let l1, p1 = find_line p.pmin file in
	let l2, p2 = find_line p.pmax file in
	if !old_format then
		l1, p1, l2, p2
	else
		l1, p1+1, l2, p2+1

let get_error_pos printer p =
	if p.pmin = -1 then
		"(unknown)"
	else
		let l1, p1, l2, p2 = get_pos_coords p in
		if l1 = l2 then begin
			let s = (if p1 = p2 then Printf.sprintf " %d" p1 else Printf.sprintf "s %d-%d" p1 p2) in
			Printf.sprintf "%s character%s" (printer p.pfile l1) s
		end else
			Printf.sprintf "%s lines %d-%d" (printer p.pfile l1) l1 l2

let reset() = Buffer.reset buf
let contents() = Buffer.contents buf
let store lexbuf = Buffer.add_string buf (lexeme lexbuf)
let add c = Buffer.add_string buf c

let mk_tok t pmin pmax =
	t , { pfile = !cur.lfile; pmin = pmin; pmax = pmax }

let mk lexbuf t =
	mk_tok t (lexeme_start lexbuf) (lexeme_end lexbuf)

let mk_ident lexbuf =
	let s = lexeme lexbuf in
	mk lexbuf (try Kwd (Hashtbl.find keywords s) with Not_found -> Const (Ident s))

let invalid_char lexbuf =
	error (Invalid_character (lexeme_char lexbuf 0)) (lexeme_start lexbuf)


let ident = [%sedlex.regexp?
	(
		Star '_',
		'a'..'z',
		Star ('_' | 'a'..'z' | 'A'..'Z' | '0'..'9')
	)
	|
	Plus '_'
	|
	(
		Plus '_',
		'0'..'9',
		Star ('_' | 'a'..'z' | 'A'..'Z' | '0'..'9')
	)
]

let idtype = [%sedlex.regexp? Star '_', 'A'..'Z', Star ('_' | 'a'..'z' | 'A'..'Z' | '0'..'9')]

let integer = [%sedlex.regexp? ('1'..'9', Star ('0'..'9')) | '0']

let rec skip_header lexbuf =
	match%sedlex lexbuf with
	| 0xfeff -> skip_header lexbuf
	| "#!", Star (Compl ('\n' | '\r')) -> skip_header lexbuf
	| "" | eof -> ()
	| _ -> assert false

let rec token lexbuf =
	match%sedlex lexbuf with
	| eof -> mk lexbuf Eof
	| Plus (Chars " \t") -> token lexbuf
	| "\r\n" -> newline lexbuf; token lexbuf
	| '\n' | '\r' -> newline lexbuf; token lexbuf
	| "0x", Plus ('0'..'9'|'a'..'f'|'A'..'F') -> mk lexbuf (Const (Int (lexeme lexbuf)))
	| integer -> mk lexbuf (Const (Int (lexeme lexbuf)))
	| integer, '.', Plus '0'..'9' -> mk lexbuf (Const (Float (lexeme lexbuf)))
	| '.', Plus '0'..'9' -> mk lexbuf (Const (Float (lexeme lexbuf)))
	| integer, ('e'|'E'), Opt ('+'|'-'), Plus '0'..'9' -> mk lexbuf (Const (Float (lexeme lexbuf)))
	| integer, '.', Star '0'..'9', ('e'|'E'), Opt ('+'|'-'), Plus '0'..'9' -> mk lexbuf (Const (Float (lexeme lexbuf)))
	| integer, "..." ->
		let s = lexeme lexbuf in
		mk lexbuf (IntInterval (String.sub s 0 (String.length s - 3)))
	| "//", Star (Compl ('\n' | '\r')) ->
		let s = lexeme lexbuf in
		mk lexbuf (CommentLine (String.sub s 2 ((String.length s)-2)))
	| "++" -> mk lexbuf (Unop Increment)
	| "--" -> mk lexbuf (Unop Decrement)
	| "~"  -> mk lexbuf (Unop NegBits)
	| "%=" -> mk lexbuf (Binop (OpAssignOp OpMod))
	| "&=" -> mk lexbuf (Binop (OpAssignOp OpAnd))
	| "|=" -> mk lexbuf (Binop (OpAssignOp OpOr))
	| "^=" -> mk lexbuf (Binop (OpAssignOp OpXor))
	| "+=" -> mk lexbuf (Binop (OpAssignOp OpAdd))
	| "-=" -> mk lexbuf (Binop (OpAssignOp OpSub))
	| "*=" -> mk lexbuf (Binop (OpAssignOp OpMult))
	| "/=" -> mk lexbuf (Binop (OpAssignOp OpDiv))
	| "<<=" -> mk lexbuf (Binop (OpAssignOp OpShl))
	| "||=" -> mk lexbuf (Binop (OpAssignOp OpBoolOr))
	| "&&=" -> mk lexbuf (Binop (OpAssignOp OpBoolAnd))
(*//| ">>=" -> mk lexbuf (Binop (OpAssignOp OpShr)) *)
(*//| ">>>=" -> mk lexbuf (Binop (OpAssignOp OpUShr)) *)
	| "==" -> mk lexbuf (Binop OpEq)
	| "!=" -> mk lexbuf (Binop OpNotEq)
	| "<=" -> mk lexbuf (Binop OpLte)
(*//| ">=" -> mk lexbuf (Binop OpGte) *)
	| "&&" -> mk lexbuf (Binop OpBoolAnd)
	| "||" -> mk lexbuf (Binop OpBoolOr)
	| "<<" -> mk lexbuf (Binop OpShl)
	| "->" -> mk lexbuf Arrow
	| "..." -> mk lexbuf (Binop OpInterval)
	| "=>" -> mk lexbuf (Binop OpArrow)
	| "!" -> mk lexbuf (Unop Not)
	| "<" -> mk lexbuf (Binop OpLt)
	| ">" -> mk lexbuf (Binop OpGt)
	| ";" -> mk lexbuf Semicolon
	| ":" -> mk lexbuf DblDot
	| "," -> mk lexbuf Comma
	| "." -> mk lexbuf Dot
	| "%" -> mk lexbuf (Binop OpMod)
	| "&" -> mk lexbuf (Binop OpAnd)
	| "|" -> mk lexbuf (Binop OpOr)
	| "^" -> mk lexbuf (Binop OpXor)
	| "+" -> mk lexbuf (Binop OpAdd)
	| "*" -> mk lexbuf (Binop OpMult)
	| "/" -> mk lexbuf (Binop OpDiv)
	| "-" -> mk lexbuf (Binop OpSub)
	| "=" -> mk lexbuf (Binop OpAssign)
	| "[" -> mk lexbuf BkOpen
	| "]" -> mk lexbuf BkClose
	| "{" -> mk lexbuf BrOpen
	| "}" -> mk lexbuf BrClose
	| "(" -> mk lexbuf POpen
	| ")" -> mk lexbuf PClose
	| "?" -> mk lexbuf Question
	| "@" -> mk lexbuf At

	| "/*" ->
		reset();
		let pmin = lexeme_start lexbuf in
		let pmax = (try comment lexbuf with Exit -> error Unclosed_comment pmin) in
		mk_tok (Comment (contents())) pmin pmax;
	| '"' ->
		reset();
		let pmin = lexeme_start lexbuf in
		let pmax = (try string lexbuf with Exit -> error Unterminated_string pmin) in
		let str = (try unescape (contents()) with Invalid_escape_sequence(c,i) -> error (Invalid_escape c) (pmin + i)) in
		mk_tok (Const (String (str,Double))) pmin pmax;
	| "'" ->
		reset();
		let pmin = lexeme_start lexbuf in
		let pmax = (try string2 lexbuf with Exit -> error Unterminated_string pmin) in
		let str = (try unescape (contents()) with Invalid_escape_sequence(c,i) -> error (Invalid_escape c) (pmin + i)) in
		let t = mk_tok (Const (String (str,Single))) pmin pmax in
		fast_add_fmt_string (snd t);
		t
	| "~/" ->
		reset();
		let pmin = lexeme_start lexbuf in
		let options, pmax = (try regexp lexbuf with Exit -> error Unterminated_regexp pmin) in
		let str = contents() in
		mk_tok (Const (Regexp (str,options))) pmin pmax;
	| '#', ident ->
		let v = lexeme lexbuf in
		let v = String.sub v 1 (String.length v - 1) in
		mk lexbuf (Sharp v)
	| '$', Star ('_' | 'a'..'z' | 'A'..'Z' | '0'..'9') ->
		let v = lexeme lexbuf in
		let v = String.sub v 1 (String.length v - 1) in
		mk lexbuf (Dollar v)
	| ident -> mk_ident lexbuf
	| idtype -> mk lexbuf (Const (Ident (lexeme lexbuf)))
	| _ -> invalid_char lexbuf

and comment lexbuf =
	match%sedlex lexbuf with
	| eof -> raise Exit
	| '\n' | '\r' | "\r\n" -> newline lexbuf; store lexbuf; comment lexbuf
	| "*/" -> lexeme_end lexbuf
	| '*' -> store lexbuf; comment lexbuf
	| Plus (Compl ('*' | '\n' | '\r')) -> store lexbuf; comment lexbuf
	| _ -> assert false

and string lexbuf =
	match%sedlex lexbuf with
	| eof -> raise Exit
	| '\n' | '\r' | "\r\n" -> newline lexbuf; store lexbuf; string lexbuf
	| "\\\"" -> store lexbuf; string lexbuf
	| "\\\\" -> store lexbuf; string lexbuf
	| '\\' -> store lexbuf; string lexbuf
	| '"' -> lexeme_end lexbuf
	| Plus (Compl ('"' | '\\' | '\r' | '\n')) -> store lexbuf; string lexbuf
	| _ -> assert false

and string2 lexbuf =
	match%sedlex lexbuf with
	| eof -> raise Exit
	| '\n' | '\r' | "\r\n" -> newline lexbuf; store lexbuf; string2 lexbuf
	| '\\' -> store lexbuf; string2 lexbuf
	| "\\\\" -> store lexbuf; string2 lexbuf
	| "\\'" -> store lexbuf; string2 lexbuf
	| "'" -> lexeme_end lexbuf
	| "$$" | "\\$" | '$' -> store lexbuf; string2 lexbuf
	| "${" ->
		let pmin = lexeme_start lexbuf in
		store lexbuf;
		(try code_string lexbuf 0 with Exit -> error Unclosed_code pmin);
		string2 lexbuf;
	| Plus (Compl ('\'' | '\\' | '\r' | '\n' | '$')) -> store lexbuf; string2 lexbuf
	| _ -> assert false

and code_string lexbuf open_braces =
	match%sedlex lexbuf with
	| eof -> raise Exit
	| '\n' | '\r' | "\r\n" -> newline lexbuf; store lexbuf; code_string lexbuf open_braces
	| '{' -> store lexbuf; code_string lexbuf (open_braces + 1)
	| '/' -> store lexbuf; code_string lexbuf open_braces
	| '}' ->
		store lexbuf;
		if open_braces > 0 then code_string lexbuf (open_braces - 1)
	| '"' ->
		add "\"";
		let pmin = lexeme_start lexbuf in
		(try ignore(string lexbuf) with Exit -> error Unterminated_string pmin);
		add "\"";
		code_string lexbuf open_braces
	| "'" ->
		add "'";
		let pmin = lexeme_start lexbuf in
		let pmax = (try string2 lexbuf with Exit -> error Unterminated_string pmin) in
		add "'";
		fast_add_fmt_string { pfile = !cur.lfile; pmin = pmin; pmax = pmax };
		code_string lexbuf open_braces
	| "/*" ->
		let pmin = lexeme_start lexbuf in
		(try ignore(comment lexbuf) with Exit -> error Unclosed_comment pmin);
		code_string lexbuf open_braces
	| "//", Star (Compl ('\n' | '\r')) -> store lexbuf; code_string lexbuf open_braces
	| Plus (Compl ('/' | '"' | '\'' | '{' | '}' | '\n' | '\r')) -> store lexbuf; code_string lexbuf open_braces
	| _ -> assert false

and regexp lexbuf =
	match%sedlex lexbuf with
	| eof | '\n' | '\r' -> raise Exit
	| '\\', '/' -> add "/"; regexp lexbuf
	| '\\', 'r' -> add "\r"; regexp lexbuf
	| '\\', 'n' -> add "\n"; regexp lexbuf
	| '\\', 't' -> add "\t"; regexp lexbuf
	| '\\', ('\\' | '$' | '.' | '*' | '+' | '^' | '|' | '{' | '}' | '[' | ']' | '(' | ')' | '?' | '-' | '0'..'9') -> add (lexeme lexbuf); regexp lexbuf
	| '\\', ('w' | 'W' | 'b' | 'B' | 's' | 'S' | 'd' | 'D' | 'x') -> add (lexeme lexbuf); regexp lexbuf
	| '\\', ('u' | 'U'), ('0'..'9' | 'a'..'f' | 'A'..'F'), ('0'..'9' | 'a'..'f' | 'A'..'F'), ('0'..'9' | 'a'..'f' | 'A'..'F'), ('0'..'9' | 'a'..'f' | 'A'..'F') -> add (lexeme lexbuf); regexp lexbuf
	| '\\', Compl '\\' -> error (Invalid_character (lexeme_char lexbuf 0)) (lexeme_end lexbuf - 1)
	| '/' -> regexp_options lexbuf, lexeme_end lexbuf
	| Plus (Compl ('\\' | '/' | '\r' | '\n')) -> store lexbuf; regexp lexbuf
	| _ -> assert false

and regexp_options lexbuf =
	match%sedlex lexbuf with
	| 'g' | 'i' | 'm' | 's' | 'u' ->
		let l = lexeme lexbuf in
		l ^ regexp_options lexbuf
	| 'a'..'z' -> error Invalid_option (lexeme_start lexbuf)
	| "" -> ""
	| _ -> assert false
