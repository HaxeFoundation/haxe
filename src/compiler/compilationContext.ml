open Globals

exception Abort

type server_mode =
	| SMNone
	| SMListen of string
	| SMConnect of string

type arg_context = {
	mutable classes : Globals.path list;
	mutable xml_out : string option;
	mutable json_out : string option;
	mutable cmds : string list;
	mutable config_macros : string list;
	mutable no_output : bool;
	mutable did_something : bool;
	mutable force_typing : bool;
	mutable pre_compilation : (unit -> unit) list;
	mutable interp : bool;
	mutable jvm_flag : bool;
	mutable swf_version : bool;
	mutable native_libs : (string * bool) list;
	mutable raise_usage : unit -> unit;
	mutable display_arg : string option;
	mutable deprecations : string list;
}

type communication = {
	write_out : string -> unit;
	write_err : string -> unit;
	flush     : compilation_context -> unit;
	exit      : int -> unit;
	is_server : bool;
}

and compilation_context = {
	com : Common.context;
	mutable messages : compiler_message list;
	mutable has_next : bool;
	mutable has_error : bool;
	comm : communication;
}

type compilation_callbacks = {
	before_anything : compilation_context -> unit;
	after_arg_parsing : compilation_context -> unit;
	after_compilation : compilation_context -> unit;
}

type server_accept = unit -> (bool * (bool -> string option) * (string -> unit) * (unit -> unit))

type server_api = {
	cache : CompilationCache.t;
	callbacks : compilation_callbacks;
	on_context_create : unit -> int;
	init_wait_socket : string -> int -> server_accept;
	init_wait_connect : string -> int -> server_accept;
	init_wait_stdio : unit -> server_accept;
	wait_loop : bool -> server_accept -> int;
	do_connect : string -> int -> string list -> unit;
}

let message ctx msg =
	ctx.messages <- msg :: ctx.messages

let error ctx ?(depth=0) ?(from_macro = false) msg p =
	message ctx (make_compiler_message ~from_macro msg p depth DKCompilerMessage Error);
	ctx.has_error <- true

(* TODO rename *)
(* TODO: we might actually want the reverse (error calling "located_error") *)
let located_error ctx (err : Error.error) =
	Error.recurse_error (fun depth err ->
		error ~depth ~from_macro:err.err_from_macro ctx (Error.error_msg err.err_message) err.err_pos
	) err

