type pos = {
	pfile : string;
	pmin : int;
	pmax : int;
}

type path = string list * string

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

let version = 4200
let version_major = version / 1000
let version_minor = (version mod 1000) / 100
let version_revision = (version mod 100)
let version_pre = None

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

let platform_list_help = function
	| [] -> ""
	| [p] -> " (" ^ platform_name p ^ " only)"
	| pl -> " (for " ^ String.concat "," (List.map platform_name pl) ^ ")"

let null_pos = { pfile = "?"; pmin = -1; pmax = -1 }

let mk_zero_range_pos p = { p with pmax = p.pmin }

let s_type_path (p,s) = match p with [] -> s | _ -> String.concat "." p ^ "." ^ s

let starts_with s c =
	String.length s > 0 && s.[0] = c

let get_error_pos_ref : ((string -> int -> string) -> pos -> string) ref = ref (fun printer p ->
	Printf.sprintf "%s: characters %d-%d" p.pfile p.pmin p.pmax
)

let s_version with_build =
	let pre = Option.map_default (fun pre -> "-" ^ pre) "" version_pre in
	let build =
		match with_build, Version.version_extra with
			| true, Some (_,build) -> "+" ^ build
			| _, _ -> ""
	in
	Printf.sprintf "%d.%d.%d%s%s" version_major version_minor version_revision pre build

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
	let ver = s_version true
	and os_type = if Sys.unix then "unix" else "windows" in
	Printf.eprintf "%s\nHaxe: %s; OS type: %s;\n%s\n%s" msg ver os_type ml_loc backtrace;
	assert false