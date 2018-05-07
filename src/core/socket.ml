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
		let l = String.length s in
		assert (l < 0xFFFF);
		let buf = Bytes.make 2 ' ' in
		Bytes.set buf 0 (Char.unsafe_chr l);
		Bytes.set buf 1 (Char.unsafe_chr (l lsr 8));
		ignore(send socket buf 0 2 []);
		ignore(send socket (Bytes.unsafe_of_string s) 0 (String.length s) [])