type pos = {
	pfile : string;
	pmin : int;
	pmax : int;
}

module IntMap = Ptmap
module StringMap = Map.Make(struct type t = string let compare = String.compare end)

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

let version = 4000
let version_major = version / 1000
let version_minor = (version mod 1000) / 100
let version_revision = (version mod 100)

let macro_platform = ref Neko

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

let s_type_path (p,s) = match p with [] -> s | _ -> String.concat "." p ^ "." ^ s