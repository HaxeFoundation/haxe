(*
 *  Haxe Compiler
 *  Copyright (c)2005 Nicolas Cannasse
 *
 *  This program is free software; you can redistribute it and/or modify
 *  it under the terms of the GNU General Public License as published by
 *  the Free Software Foundation; either version 2 of the License, or
 *  (at your option) any later version.
 *
 *  This program is distributed in the hope that it will be useful,
 *  but WITHOUT ANY WARRANTY; without even the implied warranty of
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 *  GNU General Public License for more details.
 *
 *  You should have received a copy of the GNU General Public License
 *  along with this program; if not, write to the Free Software
 *  Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
 *)

{
open Lexing
open Ast

type error_msg =
	| Invalid_character of char
	| Unterminated_string
	| Unterminated_regexp
	| Unclosed_comment
	| Invalid_escape
	| Invalid_option

exception Error of error_msg * pos

let error_msg = function
	| Invalid_character c when int_of_char c > 32 && int_of_char c < 128 -> Printf.sprintf "Invalid character '%c'" c
	| Invalid_character c -> Printf.sprintf "Invalid character 0x%.2X" (int_of_char c)
	| Unterminated_string -> "Unterminated string"
	| Unterminated_regexp -> "Unterminated regular expression"
	| Unclosed_comment -> "Unclosed comment"
	| Invalid_escape -> "Invalid escape sequence"
	| Invalid_option -> "Invalid regular expression option"

let cur_file = ref ""
let cur_line = ref 1
let all_lines = Hashtbl.create 0
let lines = ref []
let buf = Buffer.create 100

let error e pos =
	raise (Error (e,{ pmin = pos; pmax = pos; pfile = !cur_file }))

let keywords =
	let h = Hashtbl.create 3 in
	List.iter (fun k -> Hashtbl.add h (s_keyword k) k)
		[Function;Class;Static;Var;If;Else;While;Do;For;
		Break;Return;Continue;Extends;Implements;Import;
		Switch;Case;Default;Public;Private;Try;Untyped;
		Catch;New;This;Throw;Extern;Enum;In;Interface;
		Cast;Override;F9Dynamic;Typedef;Package;Callback;Inline];
	h

let init file =
	cur_file := file;
	cur_line := 1;
	lines := []

let save_lines() =
	Hashtbl.replace all_lines !cur_file !lines

let save() =
	save_lines();
	!cur_file, !cur_line

let restore (file,line) =
	save_lines();
	cur_file := file;
	cur_line := line;
	lines := Hashtbl.find all_lines file

let newline lexbuf =
	lines :=  (lexeme_end lexbuf,!cur_line) :: !lines;
	incr cur_line

let find_line p lines =
	let rec loop line delta = function
		| [] -> line + 1, p - delta
		| (lp,line) :: l when lp > p -> line, p - delta
		| (lp,line) :: l -> loop line lp l
	in
	loop 1 0 lines

let get_error_line p =
	let lines = List.rev (try Hashtbl.find all_lines p.pfile with Not_found -> []) in
	let l, _ = find_line p.pmin lines in
	l

let get_error_pos printer p =
	if p.pmin = -1 then
		"(unknown)"
	else
		let lines = List.rev (try Hashtbl.find all_lines p.pfile with Not_found -> []) in
		let l1, p1 = find_line p.pmin lines in
		let l2, p2 = find_line p.pmax lines in
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
	t , { pfile = !cur_file; pmin = pmin; pmax = pmax }

let mk lexbuf t =
	mk_tok t (lexeme_start lexbuf) (lexeme_end lexbuf)

let mk_ident lexbuf =
	match lexeme lexbuf with
	| s ->
		mk lexbuf (try Kwd (Hashtbl.find keywords s) with Not_found -> Const (Ident s))

let invalid_char lexbuf =
	error (Invalid_character (lexeme_char lexbuf 0)) (lexeme_start lexbuf)

}

let ident = ('_'* ['a'-'z'] ['_' 'a'-'z' 'A'-'Z' '0'-'9']* | '_')
let idtype = '_'* ['A'-'Z'] ['_' 'a'-'z' 'A'-'Z' '0'-'9']*

rule token = parse
	| eof { mk lexbuf Eof }
	| "\239\187\191" { token lexbuf }
	| [' ' '\t']+ { token lexbuf }
	| "\r\n" { newline lexbuf; token lexbuf }
	| '\n' | '\r' { newline lexbuf; token lexbuf }
	| "0x" ['0'-'9' 'a'-'f' 'A'-'F']+ { mk lexbuf (Const (Int (lexeme lexbuf))) }
	| ['0'-'9']+ { mk lexbuf (Const (Int (lexeme lexbuf))) }
	| ['0'-'9']+ '.' ['0'-'9']* { mk lexbuf (Const (Float (lexeme lexbuf))) }
	| '.' ['0'-'9']+ { mk lexbuf (Const (Float (lexeme lexbuf))) }
	| ['0'-'9']+ ['e' 'E'] ['+' '-']? ['0'-'9']+ { mk lexbuf (Const (Float (lexeme lexbuf))) }
	| ['0'-'9']+ '.' ['0'-'9']* ['e' 'E'] ['+' '-']? ['0'-'9']+ { mk lexbuf (Const (Float (lexeme lexbuf))) }
	| ['0'-'9']+ "..." {
			let s = lexeme lexbuf in
			mk lexbuf (IntInterval (String.sub s 0 (String.length s - 3)))
		}
	| "//" [^'\n' '\r']*  {
			let s = lexeme lexbuf in
			mk lexbuf (CommentLine (String.sub s 2 ((String.length s)-2)))
		}
	| "++" { mk lexbuf (Unop Increment) }
	| "--" { mk lexbuf (Unop Decrement) }
	| "~"  { mk lexbuf (Unop NegBits) }
	| "%=" { mk lexbuf (Binop (OpAssignOp OpMod)) }
	| "&=" { mk lexbuf (Binop (OpAssignOp OpAnd)) }
	| "|=" { mk lexbuf (Binop (OpAssignOp OpOr)) }
	| "^=" { mk lexbuf (Binop (OpAssignOp OpXor)) }
	| "+=" { mk lexbuf (Binop (OpAssignOp OpAdd)) }
	| "-=" { mk lexbuf (Binop (OpAssignOp OpSub)) }
	| "*=" { mk lexbuf (Binop (OpAssignOp OpMult)) }
	| "/=" { mk lexbuf (Binop (OpAssignOp OpDiv)) }
	| "<<=" { mk lexbuf (Binop (OpAssignOp OpShl)) }
(*//| ">>=" { mk lexbuf (Binop (OpAssignOp OpShr)) } *)
(*//| ">>>=" { mk lexbuf (Binop (OpAssignOp OpUShr)) } *)
	| "===" { mk lexbuf (Binop OpPhysEq) }
	| "!==" { mk lexbuf (Binop OpPhysNotEq) }
	| "==" { mk lexbuf (Binop OpEq) }
	| "!=" { mk lexbuf (Binop OpNotEq) }
	| "<=" { mk lexbuf (Binop OpLte) }
(*//| ">=" { mk lexbuf (Binop OpGte) }*)
	| "&&" { mk lexbuf (Binop OpBoolAnd) }
	| "||" { mk lexbuf (Binop OpBoolOr) }
	| "<<" { mk lexbuf (Binop OpShl) }
	| "->" { mk lexbuf Arrow }
	| "..." { mk lexbuf (Binop OpInterval) }
	| "!" { mk lexbuf (Unop Not) }
	| "<" { mk lexbuf (Binop OpLt) }
	| ">" { mk lexbuf (Binop OpGt) }
	| ";" { mk lexbuf Semicolon }
	| ":" { mk lexbuf DblDot }
	| "," { mk lexbuf Comma }
	| "." { mk lexbuf Dot }
	| "%" { mk lexbuf (Binop OpMod) }
	| "&" { mk lexbuf (Binop OpAnd) }
	| "|" { mk lexbuf (Binop OpOr) }
	| "^" { mk lexbuf (Binop OpXor) }
	| "+" { mk lexbuf (Binop OpAdd) }
	| "*" { mk lexbuf (Binop OpMult) }
	| "/" { mk lexbuf (Binop OpDiv) }
	| "-" { mk lexbuf (Binop OpSub) }
	| "=" { mk lexbuf (Binop OpAssign) }
	| "[" { mk lexbuf BkOpen }
	| "]" { mk lexbuf BkClose }
	| "{" { mk lexbuf BrOpen }
	| "}" { mk lexbuf BrClose }
	| "(" { mk lexbuf POpen }
	| ")" { mk lexbuf PClose }
	| "?" { mk lexbuf Question }
	| "/*" {
			reset();
			let pmin = lexeme_start lexbuf in
			let pmax = (try comment lexbuf with Exit -> error Unclosed_comment pmin) in
			mk_tok (Comment (contents())) pmin pmax;
		}
	| '"' {
			reset();
			let pmin = lexeme_start lexbuf in
			let pmax = (try string lexbuf with Exit -> error Unterminated_string pmin) in
			let str = (try unescape (contents()) with Exit -> error Invalid_escape pmin) in
			mk_tok (Const (String str)) pmin pmax;
		}
	| "'" {
			reset();
			let pmin = lexeme_start lexbuf in
			let pmax = (try string2 lexbuf with Exit -> error Unterminated_string pmin) in
			let str = (try unescape (contents()) with Exit -> error Invalid_escape pmin) in
			mk_tok (Const (String str)) pmin pmax;
		}
	| "~/" {
			reset();
			let pmin = lexeme_start lexbuf in
			let options, pmax = (try regexp lexbuf with Exit -> error Unterminated_regexp pmin) in
			let str = contents() in
			mk_tok (Const (Regexp (str,options))) pmin pmax;
		}
	| '#' ident {
			let v = lexeme lexbuf in
			let v = String.sub v 1 (String.length v - 1) in
			mk lexbuf (Macro v)
		}
	| ident { mk_ident lexbuf }
	| idtype { mk lexbuf (Const (Type (lexeme lexbuf))) }
	| _ { invalid_char lexbuf }

and comment = parse
	| eof { raise Exit }
	| '\n' | '\r' | "\r\n" { newline lexbuf; store lexbuf; comment lexbuf }
	| "*/" { lexeme_end lexbuf }
	| '*' { store lexbuf; comment lexbuf }
	| [^'*' '\n' '\r']+ { store lexbuf; comment lexbuf }

and string = parse
	| eof { raise Exit }
	| '\n' | '\r' | "\r\n" { newline lexbuf; store lexbuf; string lexbuf }
	| "\\\"" { store lexbuf; string lexbuf }
	| "\\\\" { store lexbuf; string lexbuf }
	| '\\' { store lexbuf; string lexbuf }
	| '"' { lexeme_end lexbuf }
	| [^'"' '\\' '\r' '\n']+ { store lexbuf; string lexbuf }

and string2 = parse
	| eof { raise Exit }
	| '\n' | '\r' | "\r\n" { newline lexbuf; store lexbuf; string2 lexbuf }
	| '\\' { store lexbuf; string2 lexbuf }
	| "\\\\" { store lexbuf; string2 lexbuf }
	| "\\'" { store lexbuf; string2 lexbuf }
	| "'" { lexeme_end lexbuf }
	| [^'\'' '\\' '\r' '\n']+ { store lexbuf; string2 lexbuf }

and regexp = parse
	| eof { raise Exit }
	| '\n' | '\r' | "\r\n" { newline lexbuf; store lexbuf; regexp lexbuf }
	| "\\/" { add "/"; regexp lexbuf }
	| "\\\\" | '\\' { store lexbuf; regexp lexbuf }
	| '/' { regexp_options lexbuf, lexeme_end lexbuf }
	| [^ '\\' '/' '\r' '\n']+ { store lexbuf; regexp lexbuf }

and regexp_options = parse
	| 'g' | 'i' | 'm' | 's' | 'u' {
			let l = lexeme lexbuf in
			l ^ regexp_options lexbuf
		}
	| ['a' - 'z'] { error Invalid_option (lexeme_start lexbuf) }
	| "" { "" }
