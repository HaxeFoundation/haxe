open Globals
open ParsedArg

exception Abort

type server_mode =
	| SMNone
	| SMListen of string
	| SMConnect of string

type communication = {
	write_out : string -> unit;
	write_err : string -> unit;
	flush     : compilation_context -> unit;
	exit      : Timer.timer_context -> int -> unit;
	is_server : bool;
	stdin     : in_channel option;
}

and compilation_context = {
	com : Common.context;
	mutable messages : compiler_message list;
	mutable has_next : bool;
	mutable has_error : bool;
	comm : communication;
	mutable runtime_args : string list;
	(** The pre-parsed arguments for this compilation batch. Used by
	    [Args.process_args_new] to apply arguments to [com]. *)
	mutable parsed_args : parsed_arg list;
}

type server_connection = {
	read : unit -> string;
	write : string -> unit;
	close : unit -> unit;
	get_stdin : unit -> in_channel option;
}

type server_accept = unit -> server_connection

let message ctx msg =
	ctx.messages <- msg :: ctx.messages

let error ctx ?(depth=0) ?(from_macro = false) msg p =
	message ctx (make_compiler_message ~from_macro msg p depth DKCompilerMessage Error)

let after_error ctx =
	ctx.has_error <- true;
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
	ctx.has_error || ctx.com.Common.has_error