type pos = {
	pfile : string;
	pmin : int;
	pmax : int;
}

type path = string list * string
type located =
	| Message of string * pos
	| Stack of located list

module IntMap = Ptmap
module StringMap = Map.Make(struct type t = string let compare = String.compare end)
module Int32Map = Map.Make(struct type t = Int32.t let compare = Int32.compare end)

type platform =
	| Cross
	| Js
	| Lua
	| Neko
	| Flash
	| Php
	| Cpp
	| Cs
	| Java
	| Python
	| Hl
	| Eval

let version = 4300
let version_major = version / 1000
let version_minor = (version mod 1000) / 100
let version_revision = (version mod 100)
let version_pre = None

let null_pos = { pfile = "?"; pmin = -1; pmax = -1 }

let located msg p = Message (msg,p)
let located_stack stack = Stack stack

let rec extract_located = function
	| Message (msg,p) -> [(msg, p)]
	| Stack stack -> List.fold_left (fun acc s -> acc @ (extract_located s)) [] stack

let rec relocate msg p = match msg with
	| Message (msg,_) -> Message (msg,p)
	| Stack [] -> Stack []
	| Stack (hd :: tl) -> Stack ((relocate hd p) :: tl)

let rec extract_located_pos = function
	| Message (_,p) -> p
	| Stack [] -> null_pos
	| Stack (hd :: _) -> extract_located_pos hd

let macro_platform = ref Neko

let return_partial_type = ref false

let is_windows = Sys.os_type = "Win32" || Sys.os_type = "Cygwin"

let platforms = [
	Js;
	Lua;
	Neko;
	Flash;
	Php;
	Cpp;
	Cs;
	Java;
	Python;
	Hl;
	Eval;
]

(** Expected to match `haxe.display.Display.Platform`. *)
let platform_name = function
	| Cross -> "cross"
	| Js -> "js"
	| Lua -> "lua"
	| Neko -> "neko"
	| Flash -> "flash"
	| Php -> "php"
	| Cpp -> "cpp"
	| Cs -> "cs"
	| Java -> "java"
	| Python -> "python"
	| Hl -> "hl"
	| Eval -> "eval"

let parse_platform = function
	| "cross" -> Cross
	| "js" -> Js
	| "lua" -> Lua
	| "neko" -> Neko
	| "flash" -> Flash
	| "php" -> Php
	| "cpp" -> Cpp
	| "cs" -> Cs
	| "java" -> Java
	| "python" -> Python
	| "hl" -> Hl
	| "eval" -> Eval
	| p -> raise (failwith ("invalid platform " ^ p))

let platform_list_help = function
	| [] -> ""
	| [p] -> " (" ^ platform_name p ^ " only)"
	| pl -> " (for " ^ String.concat "," (List.map platform_name pl) ^ ")"

let mk_zero_range_pos p = { p with pmax = p.pmin }

let s_type_path (p,s) = match p with [] -> s | _ -> String.concat "." p ^ "." ^ s

let starts_with s c =
	String.length s > 0 && s.[0] = c

let get_error_pos_ref : ((string -> int -> string) -> pos -> string) ref = ref (fun printer p ->
	Printf.sprintf "%s: characters %d-%d" p.pfile p.pmin p.pmax
)

let s_version =
	let pre = Option.map_default (fun pre -> "-" ^ pre) "" version_pre in
	Printf.sprintf "%d.%d.%d%s" version_major version_minor version_revision pre

let s_version_full =
	match Version.version_extra with
		| Some (_,build) -> s_version ^ "+" ^ build
		| _ -> s_version


let patch_string_pos p s = { p with pmin = p.pmax - String.length s }

(**
	Terminates compiler process and prints user-friendly instructions about filing an issue.
	Usage: `die message __LOC__`, where `__LOC__` is a built-in ocaml constant
*)
let die ?p msg ml_loc =
	let msg =
		let str_pos, expr_msg =
			match p with
			| None -> "", ""
			| Some p -> ((!get_error_pos_ref (Printf.sprintf "%s:%d:") p) ^ " "), "the expression example and "
		in
		str_pos ^ "Compiler failure" ^ (if msg = "" then "" else ": " ^ msg) ^ "\n"
		^ str_pos ^ "Please submit an issue at https://github.com/HaxeFoundation/haxe/issues/new\n"
		^ str_pos ^ "Attach " ^ expr_msg ^ "the following information:"
	in
	let backtrace = Printexc.raw_backtrace_to_string (Printexc.get_callstack 21) in
	let backtrace =
		try snd (ExtString.String.split backtrace "\n")
		with ExtString.Invalid_string -> backtrace
	in
	let ver = s_version_full
	and os_type = if Sys.unix then "unix" else "windows" in
	Printf.eprintf "%s\nHaxe: %s; OS type: %s;\n%s\n%s" msg ver os_type ml_loc backtrace;
	assert false

module MessageSeverity = struct
	type t =
		| Error
		| Warning
		| Information
		| Hint

	let to_int = function
		| Error -> 1
		| Warning -> 2
		| Information -> 3
		| Hint -> 4
end

module MessageKind = struct
	type t =
		| DKUnusedImport
		| DKUnresolvedIdentifier
		| DKCompilerMessage
		| DKRemovableCode
		| DKParserError
		| DKDeprecationWarning
		| DKInactiveBlock
		| DKMissingFields

	let to_int = function
		| DKUnusedImport -> 0
		| DKUnresolvedIdentifier -> 1
		| DKCompilerMessage -> 2
		| DKRemovableCode -> 3
		| DKParserError -> 4
		| DKDeprecationWarning -> 5
		| DKInactiveBlock -> 6
		| DKMissingFields -> 7
end

type compiler_message = {
	cm_message : string;
	cm_pos : pos;
	cm_depth : int;
	cm_kind : MessageKind.t;
	cm_severity : MessageSeverity.t;
}

let make_compiler_message msg p depth kind sev = {
		cm_message = msg;
		cm_pos = p;
		cm_depth = depth;
		cm_kind = kind;
		cm_severity = sev;
}

let i32_31 = Int32.of_int 31
