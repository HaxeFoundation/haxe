type pos = {
	pfile : string;
	pmin : int;
	pmax : int;
}

module IntMap = Map.Make(struct type t = int let compare a b = a - b end)
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

let version = 3300
let version_major = version / 1000
let version_minor = (version mod 1000) / 100
let version_revision = (version mod 100)
let version_is_stable = version_minor land 1 = 0

let is_windows = Sys.os_type = "Win32" || Sys.os_type = "Cygwin"

let s_version =
	Printf.sprintf "%d.%d.%d%s" version_major version_minor version_revision (match Version.version_extra with None -> "" | Some v -> " " ^ v)

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

let null_pos = { pfile = "?"; pmin = -1; pmax = -1 }

let s_type_path (p,s) = match p with [] -> s | _ -> String.concat "." p ^ "." ^ s