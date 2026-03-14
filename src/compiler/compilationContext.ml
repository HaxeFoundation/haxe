open Globals
open ParsedArg

exception Abort

type compilation_context = {
	com : Common.context;
}

type server_connection = {
	read : unit -> string;
	write : string -> unit;
	close : unit -> unit;
	get_stdin : unit -> in_channel;
}

type server_accept = unit -> server_connection

let message ctx msg =
	ctx.com.part_scope.messages <- msg :: ctx.com.part_scope.messages

let error ctx ?(depth=0) ?(from_macro = false) msg p =
	message ctx (make_compiler_message ~from_macro msg p depth DKCompilerMessage Error)

let after_error ctx =
	ctx.com.has_error <- true;
	if Common.fail_fast ctx.com then raise Abort

let error_ext ctx (err : Error.error) =
	Error.recurse_error (fun depth err ->
		error ~depth ~from_macro:err.err_from_macro ctx (Error.error_msg err.err_message) err.err_pos
	) err;
	after_error ctx

let error ctx ?(depth=0) ?(from_macro = false) msg p =
	error ctx ~depth ~from_macro msg p;
	after_error ctx

let has_error ctx =
	ctx.com.has_error && (Common.is_compilation ctx.com || ctx.com.part_scope.messages <> [])