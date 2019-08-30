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
	if String.length x = 0 then false else x.[0] = '_' || (x.[0] >= 'A' && x.[0] <= 'Z')

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