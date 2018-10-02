open Unix

type t = {
	addr : Unix.inet_addr;
	port : int;
	mutable socket : Unix.file_descr option;
}

let create host port =
	let host = Unix.inet_addr_of_string host in
	let socket = Unix.socket Unix.PF_INET Unix.SOCK_STREAM 0 in
	Unix.connect socket (Unix.ADDR_INET (host,port));
	{
		addr = host;
		port = port;
		socket = Some socket;
	}

let read_byte this i = int_of_char (Bytes.get this i)

let read_ui16 this i =
	let ch1 = read_byte this i in
	let ch2 = read_byte this (i + 1) in
	ch1 lor (ch2 lsl 8)

let read_string socket =
	match socket.socket with
		| None ->
			failwith "no socket" (* TODO: reconnect? *)
		| Some socket ->
			let buf = Bytes.create 2 in
			let _ = recv socket buf 0 2 [] in
			let i = read_ui16 buf 0 in
			let buf = Bytes.create i in
			let _ = recv socket buf 0 i [] in
			Bytes.to_string buf

let send_string socket s =
	match socket.socket with
	| None ->
		failwith "no socket" (* TODO: reconnect? *)
	| Some socket ->
		let b = Bytes.unsafe_of_string s in
		let l = Bytes.length b in
		let buf = Bytes.make 4 ' ' in
		EvalBytes.write_i32 buf 0 (Int32.of_int l);
		ignore(send socket buf 0 4 []);
		let rec loop length offset =
			if length <= 0 then
				()
			else begin
				let k = min length 0xFFFF in
				ignore(send socket b offset k []);
				loop (length - k) (offset + k)
			end
		in
		loop l 0