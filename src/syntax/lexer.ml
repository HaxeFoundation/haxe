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
	| Invalid_escape of char * (string option)
	| Invalid_option
	| Unterminated_markup

exception Error of error_msg * pos

type lexer_file = {
	lfile : string;
	mutable lline : int;
	mutable lmaxline : int;
	mutable llines : (int * int) list;
	mutable lalines : (int * int) array;
	mutable llast : int;
	mutable llastindex : int;
}

type lexer_ctx = {
	file : lexer_file;
	buf : Buffer.t;
}

type xml_lexing_context = {
	lexer_ctx : lexer_ctx;
	open_tag : string;
	close_tag : string;
	lexbuf : Sedlexing.lexbuf;
}

type format_context = {
	format_buffer : Buffer.t;
	mutable format_quote_open : bool;
	mutable format_quote_pmin : int;
}

let error_msg = function
	| Invalid_character c when c > 32 && c < 128 -> Printf.sprintf "Invalid character '%c'" (char_of_int c)
	| Invalid_character c -> Printf.sprintf "Invalid character 0x%.2X" c
	| Unterminated_string -> "Unterminated string"
	| Unterminated_regexp -> "Unterminated regular expression"
	| Unclosed_comment -> "Unclosed comment"
	| Unclosed_code -> "Unclosed code string"
	| Invalid_escape (c,None) -> Printf.sprintf "Invalid escape sequence \\%s" (Char.escaped c)
	| Invalid_escape (c,Some msg) -> Printf.sprintf "Invalid escape sequence \\%s. %s" (Char.escaped c) msg
	| Invalid_option -> "Invalid regular expression option"
	| Unterminated_markup -> "Unterminated markup literal"

let make_file file =
	{
		lfile = file;
		lline = 1;
		lmaxline = 1;
		llines = [0,1];
		lalines = [|0,1|];
		llast = max_int;
		llastindex = 0;
	}

let create_context file = {
	file = file;
	buf = Buffer.create 100;
}

let create_temp_ctx file =
	create_context (make_file file)

let all_files = ThreadSafeHashtbl.create 0

let create_file_ctx file =
	let f = make_file file in
	ThreadSafeHashtbl.replace all_files file f;
	create_context f

let newline ctx lexbuf =
	let cur = ctx.file in
	cur.lline <- cur.lline + 1;
	cur.llines <- (lexeme_end lexbuf,cur.lline) :: cur.llines

let copy_file source = {
	lfile = source.lfile;
	lline = source.lline;
	lmaxline = source.lmaxline;
	llines = source.llines;
	lalines = source.lalines;
	llast = source.llast;
	llastindex = source.llastindex;
}

let print_file file =
	let sllines = String.concat ";" (List.map (fun (i1,i2) -> Printf.sprintf "(%i,%i)" i1 i2) file.llines) in
	let slalines = String.concat ";" (Array.to_list (Array.map (fun (i1,i2) -> Printf.sprintf "(%i,%i)" i1 i2) file.lalines)) in
	Printf.sprintf "lfile: %s\nlline: %i\nlmaxline: %i\nllines: [%s]\nlalines: [%s]\nllast: %i\nllastindex: %i" file.lfile file.lline file.lmaxline sllines slalines file.llast file.llastindex

let error ctx e pos =
	raise (Error (e,{ pmin = pos; pmax = pos; pfile = ctx.file.lfile }))

let keywords =
	let h = Hashtbl.create 3 in
	List.iter (fun k -> Hashtbl.add h (s_keyword k) k)
		[Function;Class;Static;Var;If;Else;While;Do;For;
		Break;Return;Continue;Extends;Implements;Import;
		Switch;Case;Default;Public;Private;Try;Untyped;
		Catch;New;This;Throw;Extern;Enum;In;Interface;
		Cast;Override;Dynamic;Typedef;Package;
		Inline;Using;Null;True;False;Abstract;Macro;Final;
		Operator;Overload];
	h

let is_valid_identifier s =
	if String.length s = 0 then
		false
	else
		try
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

let split_suffix s is_int =
	let len = String.length s in
	let rec loop i pivot =
		if i = len then begin
			match pivot with
			| None ->
				(s,None)
			| Some pivot ->
				(* There might be a _ at the end of the literal because we allow _f64 and such *)
				let literal_length = if String.unsafe_get s (pivot - 1) = '_' then pivot - 1 else pivot in
				let literal = String.sub s 0 literal_length in
				let suffix  = String.sub s pivot (len - pivot) in
				(literal, Some suffix)
		end else begin
			let c = String.unsafe_get s i in
			match c with
			| 'i' | 'u' ->
				loop (i + 1) (Some i)
			| 'f' when not is_int ->
				loop (i + 1) (Some i)
			| _ ->
				loop (i + 1) pivot
		end
	in
	loop 0 None

let split_int_suffix s =
	let (literal,suffix) = split_suffix s true in
	Const (Int (literal,suffix))

let split_float_suffix s =
	let (literal,suffix) = split_suffix s false in
	Const (Float (literal,suffix))

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
let resolve_pos f next skip =
	let rec loop p =
		let inc i () =
			f.lline <- f.lline + 1;
			f.llines <- (p + i,f.lline) :: f.llines;
			i
		in
		let i = match next() with
			| '\n' -> inc 1
			| '\r' ->
				skip 1;
				inc 2
			| c -> (fun () ->
				let code = int_of_char c in
				if code < 0xC0 then ()
				else if code < 0xE0 then skip 1
				else if code < 0xF0 then skip 2
				else skip 3;
				1
			)
		in
		loop (p + i())
	in
	loop 0

let resolve_file_content_pos file content =
	let f = make_file file in
	let i = ref 0 in
	let next () =
		try
			let ret = String.get content !i in
			incr i;
			ret
		with Invalid_argument _ -> raise End_of_file
	in
	let skip n =
		i := !i + n
	in
	try resolve_pos f next skip with End_of_file -> f

let resolve_file_pos file =
	let ch = open_in_bin file in
	let f = make_file file in
	let next () = input_char ch in
	let rec skip n =
		if n > 0 then begin
			ignore(next ());
			skip (n - 1)
		end
	in
	try
		resolve_pos f next skip
	with End_of_file ->
		close_in ch;
		f

let find_file file =
	try
		ThreadSafeHashtbl.find all_files file
	with Not_found ->
		try
			let f = resolve_file_pos file in
			ThreadSafeHashtbl.add all_files file f;
			f
		with Sys_error _ ->
			make_file file

let find_pos p =
	find_line p.pmin (find_file p.pfile)

let get_error_line p =
	let l, _ = find_pos p in
	l

let get_error_line_if_exists p =
	try
		let file = ThreadSafeHashtbl.find all_files p.pfile in
		fst (find_line p.pmin file)
	with Not_found ->
		0

let get_pos_coords p =
	let file = find_file p.pfile in
	let l1, p1 = find_line p.pmin file in
	let l2, p2 = find_line p.pmax file in
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
;;

Globals.get_error_pos_ref := get_error_pos

let reset ctx = Buffer.reset ctx.buf
let contents ctx = Buffer.contents ctx.buf
let store ctx lexbuf = Buffer.add_string ctx.buf (lexeme lexbuf)
let add ctx c = Buffer.add_string ctx.buf c

let mk_tok ctx t pmin pmax =
	t , { pfile = ctx.file.lfile; pmin = pmin; pmax = pmax }

let mk ctx lexbuf t =
	mk_tok ctx t (lexeme_start lexbuf) (lexeme_end lexbuf)

let mk_ident ctx lexbuf =
	let s = lexeme lexbuf in
	mk ctx lexbuf (Const (Ident s))

let mk_keyword ctx lexbuf kwd =
	mk ctx lexbuf (Kwd kwd)

let invalid_char ctx lexbuf =
	error ctx (Invalid_character (Uchar.to_int (lexeme_char lexbuf 0))) (lexeme_start lexbuf)

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

let sharp_ident = [%sedlex.regexp?
	(
		('a'..'z' | 'A'..'Z' | '_'),
		Star ('a'..'z' | 'A'..'Z' | '0'..'9' | '_'),
		Star (
			'.',
			('a'..'z' | 'A'..'Z' | '_'),
			Star ('a'..'z' | 'A'..'Z' | '0'..'9' | '_')
		)
	)
]

let is_whitespace = function
	| ' ' | '\n' | '\r' | '\t' -> true
	| _ -> false

let string_is_whitespace s =
	try
		for i = 0 to String.length s - 1 do
			if not (is_whitespace (String.unsafe_get s i)) then
				raise Exit
		done;
		true
	with Exit ->
		false

let idtype = [%sedlex.regexp? Star '_', 'A'..'Z', Star ('_' | 'a'..'z' | 'A'..'Z' | '0'..'9')]

let digit = [%sedlex.regexp? '0'..'9']
let sep_digit = [%sedlex.regexp? Opt '_', digit]
let integer_digits = [%sedlex.regexp? (digit, Star sep_digit)]
let hex_digit = [%sedlex.regexp? '0'..'9'|'a'..'f'|'A'..'F']
let sep_hex_digit = [%sedlex.regexp? Opt '_', hex_digit]
let hex_digits = [%sedlex.regexp? (hex_digit, Star sep_hex_digit)]
let bin_digit = [%sedlex.regexp? '0'|'1']
let sep_bin_digit = [%sedlex.regexp? Opt '_', bin_digit]
let bin_digits = [%sedlex.regexp? (bin_digit, Star sep_bin_digit)]
let integer = [%sedlex.regexp? ('1'..'9', Star sep_digit) | '0']

let integer_suffix = [%sedlex.regexp? Opt '_', ('i'|'u'), Plus integer]

let float_suffix = [%sedlex.regexp? Opt '_', 'f', Plus integer]

(* https://www.w3.org/TR/xml/#sec-common-syn plus '$' for JSX *)
let xml_name_start_char = [%sedlex.regexp? '$' | ':' | 'A'..'Z' | '_' | 'a'..'z' | 0xC0 .. 0xD6 | 0xD8 .. 0xF6 | 0xF8 .. 0x2FF | 0x370 .. 0x37D | 0x37F .. 0x1FFF | 0x200C .. 0x200D | 0x2070 .. 0x218F | 0x2C00 .. 0x2FEF | 0x3001 .. 0xD7FF | 0xF900 .. 0xFDCF | 0xFDF0 .. 0xFFFD | 0x10000 .. 0xEFFFF]
let xml_name_char = [%sedlex.regexp? xml_name_start_char | '-' | '.' | '0'..'9' | 0xB7 | 0x0300 .. 0x036F | 0x203F .. 0x2040]
let xml_name = [%sedlex.regexp? Opt(xml_name_start_char, Star xml_name_char)]

let rec skip_header lexbuf =
	match%sedlex lexbuf with
	| 0xfeff -> skip_header lexbuf
	| "#!", Star (Compl ('\n' | '\r')) -> skip_header lexbuf
	| "" | eof -> ()
	| _ -> die "" __LOC__


let comment ctx lexbuf =
	let rec loop () = match%sedlex lexbuf with
		| eof ->
			raise Exit
		| '\n' | '\r' | "\r\n" ->
			newline ctx lexbuf;
			store ctx lexbuf;
			loop ()
		| "*/" ->
			lexeme_end lexbuf
		| '*' ->
			store ctx lexbuf;
			loop ()
		| Plus (Compl ('*' | '\n' | '\r')) ->
			store ctx lexbuf;
			loop ()
		| _ ->
			die "" __LOC__
	in
	loop ()

let string ctx lexbuf =
	let rec loop () = match%sedlex lexbuf with
		| eof -> raise Exit
		| '\n' | '\r' | "\r\n" ->
			newline ctx lexbuf;
			store ctx lexbuf;
			loop ()
		| "\\\"" ->
			store ctx lexbuf;
			loop ()
		| "\\\\" ->
			store ctx lexbuf;
			loop ()
		| '\\' ->
			store ctx lexbuf;
			loop ()
		| '"' ->
			lexeme_end lexbuf
		| Plus (Compl ('"' | '\\' | '\r' | '\n')) ->
			store ctx lexbuf;
			loop ()
		| _ ->
			die "" __LOC__
	in
	loop ()

let add_format_part fmt part =
	Buffer.add_string fmt.format_buffer part

let consume_buffer ctx fmt lexbuf f =
	let s = Buffer.contents ctx.buf in
	reset ctx;
	add_format_part fmt (f s)

let unescape_format ctx fmt s =
	try
		unescape s
	with Invalid_escape_sequence(c,i,msg) ->
		error ctx (Invalid_escape (c,msg)) (fmt.format_quote_pmin + i)

let rec string2 ctx fmt lexbuf =
	let rec loop () = match%sedlex lexbuf with
		| eof ->
			raise Exit
		| '\n' | '\r' | "\r\n" ->
			newline ctx lexbuf;
			store ctx lexbuf;
			loop ()
		| '\\' ->
			store ctx lexbuf;
			loop ()
		| "\\\\" ->
			store ctx lexbuf;
			loop ();
		| "\\'" ->
			store ctx lexbuf;
			loop ();
		| "'" ->
			consume_buffer ctx fmt lexbuf (fun s -> if fmt.format_quote_open then s else unescape_format ctx fmt s);
			if fmt.format_quote_open then add_format_part fmt "'";
			lexeme_end lexbuf
		| "$$" | "\\$" | '$' ->
			store ctx lexbuf;
			loop ();
		| "${" ->
			let pmin = lexeme_start lexbuf in
			consume_buffer ctx fmt lexbuf (fun s -> if fmt.format_quote_open then s else unescape_format ctx fmt s);
			add_format_part fmt "${";
			(try code_string ctx fmt lexbuf with Exit -> error ctx Unclosed_code pmin);
			loop ();
		| Plus (Compl ('\'' | '\\' | '\r' | '\n' | '$')) ->
			store ctx lexbuf;
			loop ();
		| _ ->
			die "" __LOC__
	in
	loop ()

and code_string ctx fmt lexbuf =
	let rec loop open_braces = match%sedlex lexbuf with
		| eof -> raise Exit
		| '\n' | '\r' | "\r\n" ->
			newline ctx lexbuf;
			store ctx lexbuf;
			loop open_braces
		| '{' ->
			store ctx lexbuf;
			loop (open_braces + 1)
		| '/' ->
			store ctx lexbuf;
			loop open_braces
		| '}' ->
			if open_braces > 0 then begin
				store ctx lexbuf;
				loop (open_braces - 1)
			end else begin
				consume_buffer ctx fmt lexbuf (fun s -> s);
				add_format_part fmt "}"
			end
		| '"' ->
			add ctx "\"";
			let pmin = lexeme_start lexbuf in
			(try ignore(string ctx lexbuf) with Exit -> error ctx Unterminated_string pmin);
			add ctx "\"";
			loop open_braces
		| "'" ->
			let pmin = lexeme_start lexbuf in
			consume_buffer ctx fmt lexbuf (fun s -> s);
			add_format_part fmt "'";
			let old_quote,old_pmin = fmt.format_quote_open,fmt.format_quote_pmin in
			fmt.format_quote_open <- true;
			fmt.format_quote_pmin <- pmin;
			(try ignore(string2 ctx fmt lexbuf) with Exit -> error ctx Unterminated_string pmin);
			fmt.format_quote_open <- old_quote;
			fmt.format_quote_pmin <- old_pmin;
			loop open_braces
		| "/*" ->
			let pmin = lexeme_start lexbuf in
			let save = contents ctx in
			reset ctx;
			(try ignore(comment ctx lexbuf) with Exit -> error ctx Unclosed_comment pmin);
			reset ctx;
			Buffer.add_string ctx.buf save;
			loop open_braces
		| "//", Star (Compl ('\n' | '\r')) ->
			store ctx lexbuf;
			loop open_braces
		| Plus (Compl ('/' | '"' | '\'' | '{' | '}' | '\n' | '\r')) ->
			store ctx lexbuf;
			loop open_braces
		| _ ->
			die "" __LOC__
	in
	loop 0

let rec regexp ctx lexbuf =
	let rec loop () = match%sedlex lexbuf with
		| eof | '\n' | '\r' ->
			raise Exit
		| '\\', '/' ->
			add  ctx"/";
			loop ()
		| '\\', 'r' ->
			add  ctx"\r";
			loop ()
		| '\\', 'n' ->
			add  ctx"\n";
			loop ()
		| '\\', 't' ->
			add  ctx"\t";
			loop ()
		| '\\', ('\\' | '$' | '.' | '*' | '+' | '^' | '|' | '{' | '}' | '[' | ']' | '(' | ')' | '?' | '-' | '0'..'9') ->
			add  ctx(lexeme lexbuf);
			loop ()
		| '\\', ('w' | 'W' | 'b' | 'B' | 's' | 'S' | 'd' | 'D' | 'x') ->
			add ctx (lexeme lexbuf);
			loop ()
		| '\\', ('u' | 'U'), ('0'..'9' | 'a'..'f' | 'A'..'F'), ('0'..'9' | 'a'..'f' | 'A'..'F'), ('0'..'9' | 'a'..'f' | 'A'..'F'), ('0'..'9' | 'a'..'f' | 'A'..'F') ->
			add ctx (lexeme lexbuf);
			loop ()
		| '\\', Compl '\\' ->
			error ctx (Invalid_character (Uchar.to_int (lexeme_char lexbuf 0))) (lexeme_end lexbuf - 1)
		| '/' ->
			regexp_options ctx lexbuf, lexeme_end lexbuf
		| Plus (Compl ('\\' | '/' | '\r' | '\n')) ->
			store ctx lexbuf;
			loop ()
	| _ ->
		die "" __LOC__
	in
	loop ()

and regexp_options ctx lexbuf =
	match%sedlex lexbuf with
	| 'g' | 'i' | 'm' | 's' | 'u' ->
		let l = lexeme lexbuf in
		l ^ regexp_options ctx lexbuf
	| 'a'..'z' -> error ctx Invalid_option (lexeme_start lexbuf)
	| "" -> ""
	| _ -> die "" __LOC__

let rec not_xml ctx depth in_open =
	let lexbuf = ctx.lexbuf in
	match%sedlex lexbuf with
	| eof ->
		raise Exit
	| '\n' | '\r' | "\r\n" ->
		newline ctx.lexer_ctx lexbuf;
		store ctx.lexer_ctx lexbuf;
		not_xml ctx depth in_open
	(* closing tag *)
	| '<','/',xml_name,'>' ->
		let s = lexeme lexbuf in
		Buffer.add_string ctx.lexer_ctx.buf s;
		(* If it matches our document close tag, finish or decrease depth. *)
		if s = ctx.close_tag then begin
			if depth = 0 then lexeme_end lexbuf
			else not_xml ctx (depth - 1) false
		end else
			not_xml ctx depth false
	(* opening tag *)
	| '<',xml_name ->
		let s = lexeme lexbuf in
		Buffer.add_string ctx.lexer_ctx.buf s;
		(* If it matches our document open tag, increase depth and set in_open to true. *)
		let depth,in_open = if s = ctx.open_tag then depth + 1,true else depth,false in
		not_xml ctx depth in_open
	(* /> *)
	| '/','>' ->
		let s = lexeme lexbuf in
		Buffer.add_string ctx.lexer_ctx.buf s;
		(* We only care about this if we are still in the opening tag, i.e. if it wasn't closed yet.
			In that case, decrease depth and finish if it's 0. *)
		let depth = if in_open then depth - 1 else depth in
		if depth < 0 then lexeme_end lexbuf
		else not_xml ctx depth false
	| '<' | '/' | '>' ->
		store ctx.lexer_ctx lexbuf;
		not_xml ctx depth in_open
	| Plus (Compl ('<' | '/' | '>' | '\n' | '\r')) ->
		store ctx.lexer_ctx lexbuf;
		not_xml ctx depth in_open
	| _ ->
		die "" __LOC__

let rec token ctx lexbuf =
	let mk = mk ctx in
	let token = token ctx in
	let newline = newline ctx in
	let mk_tok = mk_tok ctx in
	let mk_keyword = mk_keyword ctx in
	match%sedlex lexbuf with
	| eof -> mk lexbuf Eof
	| Plus (Chars " \t") -> token lexbuf
	| "\r\n" -> newline lexbuf; token lexbuf
	| '\n' | '\r' -> newline lexbuf; token lexbuf
	| "0x", Plus hex_digits, Opt integer_suffix ->
		mk lexbuf (split_int_suffix (lexeme lexbuf))
	| "0b", Plus bin_digits, Opt integer_suffix ->
		mk lexbuf (split_int_suffix (lexeme lexbuf))
	| integer, Opt integer_suffix ->
		mk lexbuf (split_int_suffix (lexeme lexbuf))
	| integer, float_suffix ->
		mk lexbuf (split_float_suffix (lexeme lexbuf))
	| integer, '.', Plus integer_digits, Opt float_suffix -> mk lexbuf (split_float_suffix (lexeme lexbuf))
	| '.', Plus integer_digits, Opt float_suffix -> mk lexbuf (split_float_suffix (lexeme lexbuf))
	| integer, ('e'|'E'), Opt ('+'|'-'), Plus integer_digits, Opt float_suffix -> mk lexbuf (split_float_suffix (lexeme lexbuf))
	| integer, '.', Star digit, ('e'|'E'), Opt ('+'|'-'), Plus integer_digits, Opt float_suffix -> mk lexbuf (split_float_suffix (lexeme lexbuf))
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
	| "??=" -> mk lexbuf (Binop (OpAssignOp OpNullCoal))
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
	| "..." -> mk lexbuf Spread
	| "=>" -> mk lexbuf (Binop OpArrow)
	| "!" -> mk lexbuf (Unop Not)
	| "<" -> mk lexbuf (Binop OpLt)
	| ">" -> mk lexbuf (Binop OpGt)
	| ";" -> mk lexbuf Semicolon
	| ":" -> mk lexbuf DblDot
	| "," -> mk lexbuf Comma
	| "." -> mk lexbuf Dot
	| "?." -> mk lexbuf QuestionDot
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
	| "??" -> mk lexbuf (Binop OpNullCoal)
	| "?" -> mk lexbuf Question
	| "@" -> mk lexbuf At

	| "/*" ->
		reset ctx;
		let pmin = lexeme_start lexbuf in
		let pmax = (try comment ctx lexbuf with Exit -> error ctx Unclosed_comment pmin) in
		mk_tok (Comment (contents ctx)) pmin pmax;
	| '"' ->
		reset ctx;
		let pmin = lexeme_start lexbuf in
		let pmax = (try string ctx lexbuf with Exit -> error ctx Unterminated_string pmin) in
		let str = (try unescape (contents ctx) with Invalid_escape_sequence(c,i,msg) -> error ctx (Invalid_escape (c,msg)) (pmin + i)) in
		mk_tok (Const (String(str,SDoubleQuotes))) pmin pmax;
	| "'" ->
		reset ctx;
		let pmin = lexeme_start lexbuf in
		let fmt = {
			format_buffer = Buffer.create 10;
			format_quote_open = false;
			format_quote_pmin = pmin;
		} in
		let pmax = (try string2 ctx fmt lexbuf with Exit -> error ctx Unterminated_string pmin) in
		mk_tok (Const (String(Buffer.contents fmt.format_buffer,SSingleQuotes))) pmin pmax;
	| "~/" ->
		reset ctx;
		let pmin = lexeme_start lexbuf in
		let options, pmax = (try regexp ctx lexbuf with Exit -> error ctx Unterminated_regexp pmin) in
		let str = contents ctx in
		mk_tok (Const (Regexp (str,options))) pmin pmax;
	| '#', ident ->
		let v = lexeme lexbuf in
		let v = String.sub v 1 (String.length v - 1) in
		mk lexbuf (Sharp v)
	| '$', Star ('_' | 'a'..'z' | 'A'..'Z' | '0'..'9') ->
		let v = lexeme lexbuf in
		let v = String.sub v 1 (String.length v - 1) in
		mk lexbuf (Dollar v)
	(* type decl *)
	| "package" -> mk_keyword lexbuf Package
	| "import" -> mk_keyword lexbuf Import
	| "using" -> mk_keyword lexbuf Using
	| "class" -> mk_keyword lexbuf Class
	| "interface" -> mk_keyword lexbuf Interface
	| "enum" -> mk_keyword lexbuf Enum
	| "abstract" -> mk_keyword lexbuf Abstract
	| "typedef" -> mk_keyword lexbuf Typedef
	(* relations *)
	| "extends" -> mk_keyword lexbuf Extends
	| "implements" -> mk_keyword lexbuf Implements
	(* modifier *)
	| "extern" -> mk_keyword lexbuf Extern
	| "static" -> mk_keyword lexbuf Static
	| "public" -> mk_keyword lexbuf Public
	| "private" -> mk_keyword lexbuf Private
	| "override" -> mk_keyword lexbuf Override
	| "dynamic" -> mk_keyword lexbuf Dynamic
	| "inline" -> mk_keyword lexbuf Inline
	| "macro" -> mk_keyword lexbuf Macro
	| "final" -> mk_keyword lexbuf Final
	| "operator" -> mk_keyword lexbuf Operator
	| "overload" -> mk_keyword lexbuf Overload
	(* fields *)
	| "function" -> mk_keyword lexbuf Function
	| "var" -> mk_keyword lexbuf Var
	(* values *)
	| "null" -> mk_keyword lexbuf Null
	| "true" -> mk_keyword lexbuf True
	| "false" -> mk_keyword lexbuf False
	| "this" -> mk_keyword lexbuf This
	(* expr *)
	| "if" -> mk_keyword lexbuf If
	| "else" -> mk_keyword lexbuf Else
	| "while" -> mk_keyword lexbuf While
	| "do" -> mk_keyword lexbuf Do
	| "for" -> mk_keyword lexbuf For
	| "break" -> mk_keyword lexbuf Break
	| "continue" -> mk_keyword lexbuf Continue
	| "return" -> mk_keyword lexbuf Return
	| "switch" -> mk_keyword lexbuf Switch
	| "case" -> mk_keyword lexbuf Case
	| "default" -> mk_keyword lexbuf Default
	| "throw" -> mk_keyword lexbuf Throw
	| "try" -> mk_keyword lexbuf Try
	| "catch" -> mk_keyword lexbuf Catch
	| "untyped" -> mk_keyword lexbuf Untyped
	| "new" -> mk_keyword lexbuf New
	| "in" -> mk_keyword lexbuf In
	| "cast" -> mk_keyword lexbuf Cast
	| ident -> mk_ident ctx lexbuf
	| idtype -> mk lexbuf (Const (Ident (lexeme lexbuf)))
	| _ -> invalid_char ctx lexbuf

let rec sharp_token ctx lexbuf =
	match%sedlex lexbuf with
	| sharp_ident -> mk_ident ctx lexbuf
	| Plus (Chars " \t") -> sharp_token ctx lexbuf
	| "\r\n" -> newline ctx lexbuf; sharp_token ctx lexbuf
	| '\n' | '\r' -> newline ctx lexbuf; sharp_token ctx lexbuf
	| "/*" ->
		reset ctx;
		let pmin = lexeme_start lexbuf in
		ignore(try comment ctx lexbuf with Exit -> error ctx Unclosed_comment pmin);
		sharp_token ctx lexbuf
	| _ -> token ctx lexbuf

let lex_xml ctx p lexbuf =
	let name,pmin = match%sedlex lexbuf with
	| xml_name -> lexeme lexbuf,lexeme_start lexbuf
	| _ -> invalid_char ctx lexbuf
	in
	if p + 1 <> pmin then invalid_char ctx lexbuf;
	Buffer.add_string ctx.buf ("<" ^ name);
	let open_tag = "<" ^ name in
	let close_tag = "</" ^ name ^ ">" in
	let xml_ctx = {
		lexer_ctx = ctx;
		open_tag = open_tag;
		close_tag = close_tag;
		lexbuf = lexbuf;
	} in
	try
		not_xml xml_ctx 0 (name <> "") (* don't allow self-closing fragments *)
	with Exit ->
		error ctx Unterminated_markup p
