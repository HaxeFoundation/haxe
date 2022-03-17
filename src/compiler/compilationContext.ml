open Globals

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
}

type server_mode =
	| SMNone
	| SMListen of string
	| SMConnect of string

type compilation_context = {
	com : Common.context;
	mutable finish : unit -> unit;
	mutable setup : unit -> unit;
	mutable messages : Common.compiler_message list;
	mutable has_next : bool;
	mutable has_error : bool;
	mutable server_mode : server_mode;
}