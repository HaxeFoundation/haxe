open Globals
open Ast

let format_string defines s p process_expr =
	let e = ref None in
	let pmin = ref p.pmin in
	let min = ref (p.pmin + 1) in
	let add_expr (enext,p) len =
		min := !min + len;
		let enext = process_expr enext p in
		match !e with
		| None -> e := Some enext
		| Some prev ->
			e := Some (EBinop (OpAdd,prev,enext),punion (pos prev) p)
	in
	let add enext len =
		let p = { p with pmin = !min; pmax = !min + len } in
		add_expr (enext,p) len
	in
	let add_sub start pos =
		let len = pos - start in
		if len > 0 || !e = None then add (EConst (String (String.sub s start len,SDoubleQuotes))) len
	in
	let len = String.length s in
	let rec parse start pos =
		if pos = len then add_sub start pos else
		let c = String.unsafe_get s pos in
		let pos = pos + 1 in
		if c = '\'' then begin
			incr pmin;
			incr min;
		end;
		if c <> '$' || pos = len then parse start pos else
		match String.unsafe_get s pos with
		| '$' ->
			(* double $ *)
			add_sub start pos;
			parse (pos + 1) (pos + 1)
		| '{' ->
			parse_group start pos '{' '}' "brace"
		| 'a'..'z' | 'A'..'Z' | '_' ->
			add_sub start (pos - 1);
			incr min;
			let rec loop i =
				if i = len then i else
				let c = String.unsafe_get s i in
				match c with
				| 'a'..'z' | 'A'..'Z' | '0'..'9' | '_' -> loop (i+1)
				| _ -> i
			in
			let iend = loop (pos + 1) in
			let len = iend - pos in
			add (EConst (Ident (String.sub s pos len))) len;
			parse (pos + len) (pos + len)
		| _ ->
			(* keep as-it *)
			parse start pos
	and parse_group start pos gopen gclose gname =
		add_sub start (pos - 1);
		let rec loop groups i =
			if i = len then
				match groups with
				| [] -> die "" __LOC__
				| g :: _ -> Error.raise_typing_error ("Unclosed " ^ gname) { p with pmin = !pmin + g + 1; pmax = !pmin + g + 2 }
			else
				let c = String.unsafe_get s i in
				if c = gopen then
					loop (i :: groups) (i + 1)
				else if c = gclose then begin
					let groups = List.tl groups in
					if groups = [] then i else loop groups (i + 1)
				end else
					loop groups (i + 1)
		in
		let send = loop [pos] (pos + 1) in
		let slen = send - pos - 1 in
		let scode = String.sub s (pos + 1) slen in
		min := !min + 2;
		begin
			let e =
				let ep = { p with pmin = !pmin + pos + 2; pmax = !pmin + send + 1 } in
				let error msg pos =
					if Lexer.string_is_whitespace scode then Error.raise_typing_error "Expression cannot be empty" ep
					else Error.raise_typing_error msg pos
				in
				match ParserEntry.parse_expr_string defines scode ep error true with
					| ParseSuccess(data,_,_) -> data
					| ParseError(_,(msg,p),_) -> error (Parser.error_msg msg) p
			in
			add_expr e slen
		end;
		min := !min + 1;
		parse (send + 1) (send + 1)
	in
	parse 0 0;
	match !e with
	| None -> die "" __LOC__
	| Some e -> e
