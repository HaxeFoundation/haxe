open Globals

type native_lib_kind =
	| JavaLib
	| SwfLib
	| HxbLib

type native_lib_arg = {
	lib_file : string;
	lib_kind : native_lib_kind;
	lib_extern : bool;
}


type expanding_arg_function =
	| AddLibs of string list

type expanding_arg_state =
	| NotYetExpanded of expanding_arg_function
	| AlreadyExpanded of parsed_arg list

and expanding_arg = {
	original : parsed_arg list;
	mutable state : expanding_arg_state;
}

and parsed_arg =
	(* Targets *)
	| SetPlatform of platform * string
	| SetCustomTarget of string * string
	| SetCppiaTarget of string   (** --cppia: cpp target with cppia define *)
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
	| Interp                    (** --interp: eval target + interp flag *)
	| Run of string * string list (** --run: path + runtime args (terminal; rest becomes argv) *)
	| RunX of string            (** -x: shorthand run (non-terminal; subsequent args are still build args) *)
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

	| AddClass of path
	| IncludeModule of string
	| SetPrompt
	(* Batch *)
	| Next
	| Each
	| Expand of expanding_arg
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

	mutable measure_times : bool;
}

let create_native_lib file extern kind = {
	lib_file = file;
	lib_extern = extern;
	lib_kind = kind;
}