open Globals
open Extlib_leftovers

let uppercase s =
	let bytes = Bytes.of_string s in
	Bytes.iteri
		(fun idx char ->
			let code = Char.code char in
			if 97 <= code && code <= 122 then
				Bytes.set bytes idx (Char.chr (code - 32))
		)
		bytes;
	Bytes.to_string bytes

let capitalize s =
	if String.length s = 0 then ""
	else
		let bytes = Bytes.of_string s in
		let code = Char.code (Bytes.get bytes 0) in
		if 97 <= code && code <= 122 then
			Bytes.set bytes 0 (Char.chr (code - 32));
		Bytes.to_string bytes

let starts_uppercase_identifier x =
	if String.length x = 0 then
		raise (Invalid_argument "Identifier name must not be empty")
	else
		let rec loop p =
			match String.unsafe_get x p with
			| 'A'..'Z' -> true
			| '_' -> p + 1 < String.length x && loop (p + 1)
			| _ -> false
		in
		loop 0

let check_uppercase x =
	if String.length x = 0 then
		failwith "empty part"
	else if not (starts_uppercase_identifier x) then
		failwith "Class name must start with an uppercase letter"

let s_escape ?(hex=true) s =
	let b = Buffer.create (String.length s) in
	for i = 0 to (String.length s) - 1 do
		match s.[i] with
		| '\n' -> Buffer.add_string b "\\n"
		| '\t' -> Buffer.add_string b "\\t"
		| '\r' -> Buffer.add_string b "\\r"
		| '"' -> Buffer.add_string b "\\\""
		| '\\' -> Buffer.add_string b "\\\\"
		| c when int_of_char c < 32 && hex -> Buffer.add_string b (Printf.sprintf "\\x%.2X" (int_of_char c))
		| c -> Buffer.add_char b c
	done;
	Buffer.contents b

let escape_res_name name allowed =
	ExtString.String.replace_chars (fun chr ->
		if (chr >= 'a' && chr <= 'z') || (chr >= 'A' && chr <= 'Z') || (chr >= '0' && chr <= '9') || chr = '_' || chr = '.' then
			Char.escaped chr
		else if List.mem chr allowed then
			Char.escaped chr
		else
			"-x" ^ (string_of_int (Char.code chr))) name


(* UTF8 *)

let to_utf8 str p =
	let u8 = try
		UTF8.validate str;
		str;
	with
		UTF8.Malformed_code ->
			(* ISO to utf8 *)
			let b = UTF8.Buf.create 0 in
			String.iter (fun c -> UTF8.Buf.add_char b (UCharExt.of_char c)) str;
			UTF8.Buf.contents b
	in
	let ccount = ref 0 in
	UTF8.iter (fun c ->
		let c = UCharExt.code c in
		if (c >= 0xD800 && c <= 0xDFFF) || c >= 0x110000 then failwith "Invalid unicode char";
		incr ccount;
		if c > 0x10000 then incr ccount;
	) u8;
	u8, !ccount

let utf16_add buf c =
	let add c =
		Buffer.add_char buf (char_of_int (c land 0xFF));
		Buffer.add_char buf (char_of_int (c lsr 8));
	in
	if c >= 0 && c < 0x10000 then begin
		if c >= 0xD800 && c <= 0xDFFF then failwith ("Invalid unicode char " ^ string_of_int c);
		add c;
	end else if c < 0x110000 then begin
		let c = c - 0x10000 in
		add ((c asr 10) + 0xD800);
		add ((c land 1023) + 0xDC00);
	end else
		failwith ("Invalid unicode char " ^ string_of_int c)

let utf8_to_utf16 str zt =
	let b = Buffer.create (String.length str * 2) in
	(try UTF8.iter (fun c -> utf16_add b (UCharExt.code c)) str with Invalid_argument _ | UCharExt.Out_of_range -> ()); (* if malformed *)
	if zt then utf16_add b 0;
	Buffer.contents b

let utf16_to_utf8 str =
	let b = Buffer.create 0 in
	let add c = Buffer.add_char b (char_of_int (c land 0xFF)) in
	let get i = int_of_char (String.unsafe_get str i) in
	let rec loop i =
		if i >= String.length str then ()
		else begin
			let c = get i in
			if c < 0x80 then begin
				add c;
				loop (i + 2);
			end else if c < 0x800 then begin
				let c = c lor ((get (i + 1)) lsl 8) in
				add c;
				add (c lsr 8);
				loop (i + 2);
			end else
				die "" __LOC__;
		end
	in
	loop 0;
	Buffer.contents b

let url_encode s add_char =
	let hex = "0123456789ABCDEF" in
	for i = 0 to String.length s - 1 do
		let c = String.unsafe_get s i in
		match c with
		| 'A'..'Z' | 'a'..'z' | '0'..'9' | '_' | '-' | '.' ->
			add_char c
		| _ ->
			add_char '%';
			add_char (String.unsafe_get hex (int_of_char c lsr 4));
			add_char (String.unsafe_get hex (int_of_char c land 0xF));
	done

let url_encode_s s =
	let b = Buffer.create 0 in
	url_encode s (Buffer.add_char b);
	Buffer.contents b