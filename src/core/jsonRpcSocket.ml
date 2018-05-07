open Json

let send_json socket json =
	let b = Buffer.create 0 in
	write_json (Buffer.add_string b) json;
	Socket.send_string socket (Buffer.contents b)

let send_event socket event data =
	send_json socket (JsonRpc.notification event data)