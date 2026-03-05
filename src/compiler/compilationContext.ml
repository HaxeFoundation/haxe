open Globals

exception Abort

type server_mode =
	| SMNone
	| SMListen of string
	| SMConnect of string

type native_lib_kind =
	| JavaLib
	| SwfLib
	| HxbLib

type native_lib_arg = {
	lib_file : string;
	lib_kind : native_lib_kind;
	lib_extern : bool;
}

type arg_context = {
	mutable classes : Globals.path list;
	mutable xml_out : string option;
	mutable hxb_out : string option;
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
	mutable hxb_libs : native_lib_arg list;
	mutable native_libs : native_lib_arg list;
	mutable raise_usage : unit -> unit;
	mutable display_arg : string option;
	mutable deprecations : string list;
	mutable measure_times : bool;
}

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
	timer_ctx : Timer.timer_context;
}

type compilation_callbacks = {
	before_anything : compilation_context -> unit;
	after_target_init : compilation_context -> unit;
	after_save : compilation_context -> unit;
	after_compilation : compilation_context -> unit;
}

type server_connection = {
	support_nonblock : bool;
	read : bool -> string option;
	write : string -> unit;
	close : unit -> unit;
	get_stdin : unit -> in_channel option;
}

type server_accept = unit -> server_connection

type server_api = {
	cache : CompilationCache.t;
	callbacks : compilation_callbacks;
	on_context_create : unit -> int;
}

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

let create_native_lib file extern kind = {
	lib_file = file;
	lib_extern = extern;
	lib_kind = kind;
}
