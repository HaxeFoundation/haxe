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