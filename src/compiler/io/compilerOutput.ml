open CompilerIo
open Globals
open Message
open Common

(** Higher-level compiler output helpers ("what to send").

    Builds on {!CompilerIo} to provide convenience functions for common
    output operations like timer reports and result delivery.

    The {!result_handler} type abstracts over how compilation results
    are delivered to clients: in JSON-RPC mode it sends structured
    responses, while in CLI/pipe mode the protocol-specific handler
    routes messages to the appropriate output channels.
    Call-sites should use the [send_*] functions below instead of
    interacting with the underlying JSON-RPC API directly. *)

(** Handler that encapsulates result delivery for a compilation request.
    Set up once when the request mode is determined; call-sites use
    {!send_result}, {!send_error}, etc. instead of touching this directly. *)


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

(** Collect timer report output and write it to stderr / the connection.
    Writes are wrapped in [try ... with] because in server mode the
    client connection may have been closed by the time we try to send. *)
let send_timer_report io timer_ctx =
	let buf = Buffer.create 4096 in
	Timer.report_times timer_ctx (fun s -> Buffer.add_string buf (s ^ "\n"));
	try (CompilerIo.write_err io) (Buffer.contents buf) with _ -> ()

(** Create the handler for server-pipe mode (non-JSON-RPC server).
    Messages are written to stderr, errors are signaled via the pipe protocol,
    and timer reports are sent after successful compilation. *)
let create_server_result_handler io =
	let send_message _sev output =
		CompilerIo.write_err io (output ^ "\n");
		ServerMessage.message output
	in
	{
		send_result = (fun _ -> ());
		send_result_raise = (fun _ -> failwith "send_result_raise called in non-JSON-RPC mode");
		send_error = (fun errors ->
			List.iter (fun je ->
				CompilerIo.write_err io (extract_error_message je ^ "\n")
			) errors
		);
		send_error_raise = (fun _ -> failwith "send_error_raise called in non-JSON-RPC mode");
		flush_messages = (fun has_error com ->
			let has_error = ref has_error in
			MessageReporting.display_messages_from com.defines (List.rev com.part_scope.messages)
				~set_error:(fun () -> has_error := true; com.part_scope.has_error <- true)
				(fun sev output -> send_message sev output);
			com.sctx.was_compilation <- com.display.dms_full_typing;
			if !has_error then begin
				com.timer_ctx.measure_times <- No;
				CompilerIo.signal_error io
			end else
				if com.timer_ctx.measure_times = Yes then
					send_timer_report io com.timer_ctx
		);
		set_com = (fun _ -> NoCompletionPointFound);
	}

(** Create the handler for CLI mode (non-server).
    Messages are routed by severity: info to stdout, warnings/errors to stderr.
    Optionally prompts user to press enter before exiting on error. *)
let create_cli_result_handler io =
	let send_message sev output =
		match sev with
			| MessageSeverity.Information -> CompilerIo.write_out io (output ^ "\n")
			| Warning | Error | Hint -> CompilerIo.write_err io (output ^ "\n")
	in
	{
		send_result = (fun _ -> ());
		send_result_raise = (fun _ -> failwith "send_result_raise called in non-JSON-RPC mode");
		send_error = (fun errors ->
			List.iter (fun je ->
				CompilerIo.write_err io (extract_error_message je ^ "\n")
			) errors
		);
		send_error_raise = (fun _ -> failwith "send_error_raise called in non-JSON-RPC mode");
		flush_messages = (fun has_error com ->
			let has_error = ref has_error in
			MessageReporting.display_messages_from com.defines (List.rev com.part_scope.messages)
				~set_error:(fun () -> has_error := true; com.part_scope.has_error <- true)
				(fun sev output -> send_message sev output);
			if !has_error && !Helper.prompt then begin
				CompilerIo.write_out io "Press enter to exit...\n";
				ignore(read_line());
			end;
			CompilerIo.flush io
		);
		set_com = (fun _ -> NoCompletionPointFound);
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

(** Flush all compiler messages through the protocol. *)
let flush_messages rh has_error com = rh.flush_messages has_error com