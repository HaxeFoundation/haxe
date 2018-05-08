open Json

let jsonrpc_field = "jsonrpc", JString "2.0"

let notification method_name params =
	let fl = [
		jsonrpc_field;
		"method", JString method_name;
	] in
	let fl = Option.map_default (fun params -> ("params",params) :: fl) fl params in
	JObject fl

let result id data =
	JObject [
		jsonrpc_field;
		"id", id;
		"result", data;
	]

let error id code message =
	JObject [
		jsonrpc_field;
		"id", id;
		"error", JObject [
			"code", JInt code;
			"message", JString message;
		];
	]

type json_rpc_error =
	| Parse_error of string
	| Invalid_request of string
	| Method_not_found of Json.t * string (* id->methodname *)
	| Invalid_params of Json.t
	| Custom of Json.t * int * string (* id->code->message *)

exception JsonRpc_error of json_rpc_error

let raise_method_not_found id name = raise (JsonRpc_error (Method_not_found(id,name)))
let raise_custom id code message = raise (JsonRpc_error (Custom(id,code,message)))
let raise_invalid_params json = raise (JsonRpc_error (Invalid_params json))

let handle_jsonrpc_error f output =
	try f () with JsonRpc_error e ->
		match e with
		| Parse_error s -> output (error JNull (-32700) s)
		| Invalid_request s -> output (error JNull (-32600) s)
		| Method_not_found (id,meth) -> output (error id (-32601) (Printf.sprintf "Method `%s` not found" meth))
		| Invalid_params id -> output (error id (-32602) "Invalid params")
		| Custom (id,code,msg) -> output (error id code msg)

let parse_request input =
	let open Json.Reader in
	let lexbuf = Sedlexing.Utf8.from_string input in
	let json = try read_json lexbuf with Json_error s -> raise (JsonRpc_error (Parse_error s)) in
	let fields = match json with JObject fl -> fl | _ -> raise (JsonRpc_error (Invalid_request "not an object")) in
	let get_field name map =
		let field = try List.find (fun (n,_) -> n = name) fields with Not_found -> raise (JsonRpc_error (Invalid_request ("no `" ^ name ^ "` field"))) in
		let value = map (snd field) in
		match value with
		| None -> raise (JsonRpc_error (Invalid_request (Printf.sprintf "`%s` field has invalid data" name)))
		| Some v -> v
	in
	let id = get_field "id" (fun v -> Some v) in
	let meth = get_field "method" (function JString s -> Some s | _ -> None) in
	let params =
		try
			let f = List.find (fun (n,_) -> n = "params") fields in
			Some (snd f)
		with Not_found ->
			None
	in
	id,meth,params

let process_request input handle output =
	let id,meth,params = parse_request input in
	let res = handle id meth params in
	output id res