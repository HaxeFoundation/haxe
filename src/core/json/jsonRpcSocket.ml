open Json

let send_json socket json =
	Socket.send_string socket (string_of_json json)

let send_event socket event data =
	send_json socket (JsonRpc.notification event data)