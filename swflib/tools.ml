
exception Error of string

let error msg = raise (Error msg)

let read ch n =
	let s = String.create n in
	really_input ch s 0 n;
	s

let read_byte ch =
	input_byte ch

let read_f16 ch =
	let ch1 = read_byte ch in
	let ch2 = read_byte ch in
	ch1 lor (ch2 lsl 8)

let read_ui16 ch =
	let ch1 = read_byte ch in
	let ch2 = read_byte ch in
	ch1 lor (ch2 lsl 8)

let read_i16 ch =
	let ch1 = read_byte ch in
	let ch2 = read_byte ch in
	let n = ch1 lor (ch2 lsl 8) in
	if ch2 land 128 > 0 then
		n - 65536
	else
		n

let read_i32 ch =
	let ch1 = read_byte ch in
	let ch2 = read_byte ch in
	let ch3 = read_byte ch in
	let ch4 = read_byte ch in
	ch1 lor (ch2 lsl 8) lor (ch3 lsl 16) lor (ch4 lsl 24)

let read_string ch =
	let b = Buffer.create 8 in
	let rec loop() =
		let c = input_char ch in
		if c <> '\000' then begin
			Buffer.add_char b c;
			loop();
		end;
	in
	loop();
	Buffer.contents b

let rec read_strings ch n =
	if n = 0 then
		[]
	else
		let s = read_string ch in
		s :: read_strings ch (n-1)

let write_byte ch x =
	output_byte ch x

let write_f16 ch n =
	if n < 0 || n > 0xFFFF then error "Write F16 overflow";
	write_byte ch n;
	write_byte ch (n lsr 8)

let write_ui16 ch n =
	if n < 0 || n > 0xFFFF then error "Write UI16 overflow";
	write_byte ch n;
	write_byte ch (n lsr 8)

let write_i16 ch n =
	if n < -0x7FFF || n > 0x7FFF then error "Write I16 overflow";
	if n < 0 then 
		write_ui16 ch (65536 + n)
	else
		write_ui16 ch n

let write_string ch s =
	output_string ch s;
	output_char ch '\000'

let rec write_strings ch = function
	| [] -> ()
	| s :: l ->
		write_string ch s;
		write_strings ch l