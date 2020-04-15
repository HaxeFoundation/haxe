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

let version = 4100
let version_major = version / 1000
let version_minor = (version mod 1000) / 100
let version_revision = (version mod 100)
let version_pre = Some "rc.1"

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
	Printf.sprintf "%s characters %d-%d" (printer p.pfile (-1)) p.pmin p.pmax
)

(**
	Terminates compiler process and prints user-friendly instructions about filing an issue.
*)
let die ?p msg =
	let msg =
		let str_pos, expr_msg =
			match p with
			| None -> "", ""
			| Some p -> ((!get_error_pos_ref (Printf.sprintf "%s:%d:") p) ^ " "), "the expression example and "
		in
		str_pos ^ "Compiler failure" ^ (if msg = "" then "" else ": " ^ msg) ^ "\n"
		^ str_pos ^ "Please submit an issue with " ^ expr_msg ^ "the following information:"
	in
	let backtrace = Printexc.raw_backtrace_to_string (Printexc.get_callstack 50) in
	let backtrace =
		try snd (ExtString.String.split backtrace "\n")
		with ExtString.Invalid_string -> backtrace
	in
	Printf.eprintf "%s\n%s" msg backtrace;
	assert false