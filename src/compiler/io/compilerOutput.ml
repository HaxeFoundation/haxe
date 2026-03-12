open CompilerIo

(** Higher-level compiler output helpers ("what to send").

    Builds on {!CompilerIo} to provide convenience functions for common
    output operations like timer reports and result delivery.

    The {!result_handler} type abstracts over how compilation results
    are delivered to clients: in JSON-RPC mode it sends structured
    responses, while in CLI/pipe mode a default (no-op) handler is used.
    Call-sites should use the [send_*] functions below instead of
    interacting with the underlying JSON-RPC API directly. *)

(** Handler that encapsulates result delivery for a compilation request.
    Set up once when the request mode is determined; call-sites use
    {!send_result}, {!send_error}, etc. instead of touching this directly. *)
type result_handler = {
	send_result : Json.t -> unit;
	send_result_raise : 'a . Json.t -> 'a;
	send_error : Json.t list -> unit;
	send_error_raise : 'a . Json.t list -> 'a;
	jsonrpc : Jsonrpc_handler.jsonrpc_handler option;
}

(** Extract the ["message"] string from a JSON error object, falling back
    to the full JSON representation when the field is absent. *)
let extract_error_message je =
	match je with
	| Json.JObject fields ->
		(try match List.assoc "message" fields with
			| Json.JString s -> s
			| other -> Json.string_of_json other
		with Not_found -> Json.string_of_json je)
	| _ -> Json.string_of_json je

(** Create the default handler for non-JSON-RPC mode (CLI, plain server pipe).
    [send_error] writes error messages to stderr via [io].
    The [_raise] variants must never be called in this mode. *)
let create_default_result_handler io = {
	send_result = (fun _ -> ());
	send_result_raise = (fun _ -> assert false);
	send_error = (fun errors ->
		List.iter (fun je ->
			CompilerIo.write_err io (extract_error_message je ^ "\n")
		) errors
	);
	send_error_raise = (fun _ -> assert false);
	jsonrpc = None;
}

(** Send a JSON result to the client (non-raising). *)
let send_result rh json = rh.send_result json

(** Send a JSON result to the client and raise {!DisplayJson.JsonCompleted}. *)
let send_result_raise : 'a . result_handler -> Json.t -> 'a =
	fun rh json -> rh.send_result_raise json

(** Send a list of JSON error objects to the client (non-raising). *)
let send_error rh errors = rh.send_error errors

(** Send a list of JSON error objects to the client and raise. *)
let send_error_raise : 'a . result_handler -> Json.t list -> 'a =
	fun rh errors -> rh.send_error_raise errors

(** Whether this handler is backed by a JSON-RPC connection. *)
let has_json_rpc rh = rh.jsonrpc <> None

(** Return the JSON-RPC handler.
    @raise Invalid_argument if not in JSON-RPC mode. *)
let get_jsonrpc_exn rh = Option.get rh.jsonrpc

(** Collect timer report output and write it to stderr / the connection.
    Writes are wrapped in [try ... with] because in server mode the
    client connection may have been closed by the time we try to send. *)
let send_timer_report io timer_ctx =
	let buf = Buffer.create 4096 in
	Timer.report_times timer_ctx (fun s -> Buffer.add_string buf (s ^ "\n"));
	try (CompilerIo.write_err io) (Buffer.contents buf) with _ -> ()