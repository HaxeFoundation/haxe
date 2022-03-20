open Globals

type server_mode =
	| SMNone
	| SMListen of string
	| SMConnect of string

type arg_context = {
	mutable classes : Globals.path list;
	mutable xml_out : string option;
	mutable json_out : string option;
	mutable swf_header : (int * int * float * int) option;
	mutable cmds : string list;
	mutable config_macros : string list;
	mutable cp_libs : string list;
	added_libs : (string,unit) Hashtbl.t;
	mutable no_output : bool;
	mutable did_something : bool;
	mutable force_typing : bool;
	mutable pre_compilation : (unit -> unit) list;
	mutable interp : bool;
	mutable jvm_flag : bool;
	mutable swf_version : bool;
	mutable native_libs : (string * bool) list;
	mutable raise_usage : unit -> unit;
	mutable server_mode : server_mode;
}

type communication = {
	write_out : string -> unit;
	write_err : string -> unit;
	flush     : compilation_context -> unit;
	is_server : bool;
}

and compilation_context = {
	com : Common.context;
	mutable on_exit : (unit -> unit) list;
	mutable messages : Common.compiler_message list;
	mutable has_next : bool;
	mutable has_error : bool;
	comm : communication;
}

type server_accept = unit -> (bool * (bool -> string option) * (string -> unit) * (unit -> unit))

type server_api = {
	cache : CompilationServer.t;
	before_anything : compilation_context -> unit;
	after_arg_parsing : compilation_context -> unit;
	after_compilation : compilation_context -> unit;
	init_wait_socket : string -> int -> server_accept;
	init_wait_connect : string -> int -> server_accept;
	init_wait_stdio : unit -> server_accept;
	wait_loop : bool -> server_accept -> unit;
	do_connect : string -> int -> string list -> unit;
}