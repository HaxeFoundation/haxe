open Globals

let byte_offset s chars =
	let len = String.length s in
	let rec loop i n =
		if n = chars || i >= len then i
		else
			let i = i + 1 in
			let rec skip i = if i < len && (Char.code s.[i]) land 0xC0 = 0x80 then skip (i + 1) else i in
			loop (skip i) (n + 1)
	in
	loop 0 0

let byte_range s p =
	let bmin = byte_offset s p.pmin in
	bmin,bmin + byte_offset (String.sub s bmin (String.length s - bmin)) (p.pmax - p.pmin)

let is_ident_char c = match c with
	| 'a'..'z' | 'A'..'Z' | '0'..'9' | '_' -> true
	| _ -> false

let find_word s word =
	let n = String.length word and len = String.length s in
	let rec loop i =
		if i + n > len then None
		else if String.sub s i n = word && (i = 0 || not (is_ident_char s.[i - 1])) && (i + n = len || not (is_ident_char s.[i + n])) then Some i
		else loop (i + 1)
	in
	loop 0

let read p =
	try
		if p.pmin < 0 || p.pmin >= p.pmax then None
		else begin
			let s = Std.input_file ~bin:true p.pfile in
			let bmin,bmax = byte_range s p in
			if bmax > String.length s then None else Some (String.sub s bmin (bmax - bmin))
		end
	with _ ->
		None

let meta_to_keyword p_meta p_name keyword replacement =
	let span = { p_meta with pmax = p_name.pmin } in
	match read span with
	| None ->
		None
	| Some text ->
		let meta_len = snd (byte_range text { p_meta with pmin = 0; pmax = p_meta.pmax - p_meta.pmin }) in
		let rec skip i = if i < String.length text && (match text.[i] with ' ' | '\t' | '\r' | '\n' -> true | _ -> false) then skip (i + 1) else i in
		let start = skip meta_len in
		let rest = String.sub text start (String.length text - start) in
		match find_word rest keyword with
		| None ->
			None
		| Some i ->
			let n = String.length keyword in
			Some (span,String.sub rest 0 i ^ replacement ^ String.sub rest (i + n) (String.length rest - i - n))
