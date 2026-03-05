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

(** Pre-parsed representation of a single compiler argument.  Produced by
    [Args.parse_args_new] from raw string arguments and stored in the
    [RequestQueue] so the server can inspect requests without running a full
    compilation.  Applied to a [Common.context] by [Args.process_args_new]. *)
type parsed_arg =
	(* Targets *)
	| SetPlatform of platform * string
	| SetCustomTarget of string * string
	(* Compilation *)
	| AddClassPath of string
	| AddLibClassPath of string
	| AddHxbLib of string
	| SetMain of path
	| AddLib of string
	| HaxelibGlobal
	| Define of string * string option
	| Undefine of string
	| SetVerbose
	| SetDebug
	| SetInterp
	| SetJvmFlag
	| AddRuntimeArgs of string list
	| AddResource of string * string
	| RunCmd of string
	| SetSwfVersion of float
	| SetDce of string
	| AddNativeLib of native_lib_arg
	| AddNekoLibPath of string
	| Remap of string * string
	| SetCustomExtension of string
	| AddMacro of string
	| SetDisplayArg of string
	| SetXmlOut of string
	| SetJsonOut of string
	| SetHxbOut of string
	| SetNoOutput
	| SetMeasureTimes
	| AddWarning of string
	| AddDeprecation of string
	| AddClass of path
	| IncludeModule of string
	| SetPrompt
	(* Batch *)
	| Next
	| Each
	(* Server *)
	| ServerListen of string
	| ServerConnect of string
	| Connect of string
	(* Working directory - applied eagerly for hxml resolution *)
	| Cwd of string
	(* Hxml file reference - expanded lazily in process_params *)
	| HxmlFile of string
	(* Early-exit helpers (raise HelpMessage when processed) *)
	| ShowVersion
	| ShowHelp
	| ShowHelpDefines
	| ShowHelpMetas
	| ShowHelpUserDefines
	| ShowHelpUserMetas
	(* Raw CLI tokens preserved for com.args reconstruction *)
	| RawArgs of string list

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
	(** The pre-parsed arguments for this compilation part. Used by
	    [Args.process_args_new] to apply arguments to [com] without
	    re-parsing from scratch. *)
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

let create_native_lib file extern kind = {
	lib_file = file;
	lib_extern = extern;
	lib_kind = kind;
}
