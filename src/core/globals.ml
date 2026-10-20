(*
	Positions (docs/incremental-compilation.md, phase 2, item 6).

	A position is relative to an anchor, a shared mutable offset (one per
	declaration once the parser allocates them): moving a declaration in its
	file only updates its anchor, and every position created in it (parsed,
	typed, in cached messages) follows. The type is abstract so that no code
	reads the offsets without the anchor: [Pos.min] and [Pos.max] return
	absolute offsets, and positions derived from another one with the
	[Pos.with_*] functions keep its anchor.
*)
module Pos : sig
	type anchor
	type t

	(* The anchor of positions that do not belong to a declaration. It is never shifted. *)
	val no_anchor : anchor
	val new_anchor : unit -> anchor
	(* Moves every position of [anchor] by [delta] characters. *)
	val shift : anchor -> int -> unit

	val make : ?anchor:anchor -> string -> int -> int -> t
	val file : t -> string
	val min : t -> int
	val max : t -> int
	val anchor : t -> anchor

	(* The [with_*] functions take absolute offsets and keep the anchor. *)
	val with_file : t -> string -> t
	val with_min : t -> int -> t
	val with_max : t -> int -> t
	val with_range : t -> int -> int -> t
end = struct
	type anchor = { mutable delta : int }

	type t = {
		pfile : string;
		pmin : int;
		pmax : int;
		panchor : anchor;
	}

	let no_anchor = { delta = 0 }
	let new_anchor () = { delta = 0 }
	let shift a d =
		assert (a != no_anchor);
		a.delta <- a.delta + d

	let make ?(anchor=no_anchor) file min max =
		{ pfile = file; pmin = min - anchor.delta; pmax = max - anchor.delta; panchor = anchor }

	let file p = p.pfile
	let min p = p.pmin + p.panchor.delta
	let max p = p.pmax + p.panchor.delta
	let anchor p = p.panchor

	let with_file p file = { p with pfile = file }
	let with_min p min = { p with pmin = min - p.panchor.delta }
	let with_max p max = { p with pmax = max - p.panchor.delta }
	let with_range p min max = { p with pmin = min - p.panchor.delta; pmax = max - p.panchor.delta }
end

type pos = Pos.t

type path = string list * string

module IntMap = Map.Make(struct type t = int let compare i1 i2 = i2 - i1 end)
module StringMap = Map.Make(struct type t = string let compare = String.compare end)
module Int32Map = Map.Make(struct type t = Int32.t let compare = Int32.compare end)

module IntHashtbl = Hashtbl.Make(struct
	type t = int

	let equal =
		Int.equal

	let hash = Int.hash
end)

module Int64Hashtbl = Hashtbl.Make(struct
	type t = Signed.Int64.t

	let equal =
		Signed.Int64.equal

	let hash = Hashtbl.hash
end)

module StringHashtbl = Hashtbl.Make(struct
	type t = string

	let equal =
		String.equal

	let hash s =
		Hashtbl.hash s
end)

type platform =
	| Cross
	| Js
	| Lua
	| Neko
	| Flash
	| Php
	| Cpp
	| Jvm
	| Python
	| Hl
	| Eval
	| CustomTarget of string

type compiler_version = {
	version: int;
	major: int;
	minor: int;
	revision: int;
	pre: string option;
	extra: (string * string) option;
}

let version = 5000
let version_major = version / 1000
let version_minor = (version mod 1000) / 100
let version_revision = (version mod 100)
let version_pre = Some "preview.1"

let file_pos file = Pos.make file 0 0
let fake_pos p = Pos.make p (-1) (-1)
let null_pos = fake_pos "?"

let no_color = false
let c_reset = if no_color then "" else "\x1b[0m"
let c_dim = if no_color then "" else "\x1b[2m"

let loc_short (loc:Printexc.location) =
	Printf.sprintf "%s:%d" loc.filename loc.line_number

let loc_to_string (loc:Printexc.location) =
	Printf.sprintf "%s, line %d, characters %d-%d" loc.filename loc.line_number loc.start_char loc.end_char

let trace s =
	let stack = Printexc.get_callstack 2 in
	match Printexc.backtrace_slots stack with
	| Some [|_; item |] ->
		(match Printexc.Slot.location item with
		| Some loc -> print_endline (Printf.sprintf "%s%s:%s %s" c_dim (loc_short loc) c_reset s)
		| _ -> ())
	| _ ->
		()

let trace_call_stack ?(n:int = 5) () =
	assert (n >= 0);
	let stack = Printexc.get_callstack (n+2) in
	let len = Printexc.raw_backtrace_length stack - 1 in

	let slot = Printexc.convert_raw_backtrace_slot (Printexc.get_raw_backtrace_slot stack 1) in
	let loc = Printexc.Slot.location slot in
	Option.may (fun loc -> print_endline (Printf.sprintf "%s%s:%s" c_dim (loc_short loc) c_reset)) loc;

	for i = 2 to len do
		let slot = Printexc.convert_raw_backtrace_slot (Printexc.get_raw_backtrace_slot stack i) in
		let loc = Printexc.Slot.location slot in
		Option.may (fun loc -> print_endline (Printf.sprintf "  called from %s" (loc_to_string loc))) loc;
	done

let is_windows = Sys.os_type = "Win32" || Sys.os_type = "Cygwin"

let max_custom_target_len = 16

let platforms = [
	Js;
	Lua;
	Neko;
	Flash;
	Php;
	Cpp;
	Jvm;
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
	| Jvm -> "jvm"
	| Python -> "python"
	| Hl -> "hl"
	| Eval -> "eval"
	| CustomTarget c -> c

let parse_platform = function
	| "cross" -> Cross
	| "js" -> Js
	| "lua" -> Lua
	| "neko" -> Neko
	| "flash" -> Flash
	| "php" -> Php
	| "cpp" -> Cpp
	| "jvm" -> Jvm
	| "python" -> Python
	| "hl" -> Hl
	| "eval" -> Eval
	| p -> CustomTarget p

let platform_list_help = function
	| [] -> ""
	| [p] -> " (" ^ platform_name p ^ " only)"
	| pl -> " (for " ^ String.concat "," (List.map platform_name pl) ^ ")"

let mk_zero_range_pos p = Pos.with_max p (Pos.min p)

let s_type_path (p,s) = match p with [] -> s | _ -> String.concat "." p ^ "." ^ s

let starts_with s c =
	String.length s > 0 && s.[0] = c

let get_error_pos_ref : ((string -> int -> string) -> pos -> string) ref = ref (fun printer p ->
	Printf.sprintf "%s: characters %d-%d" (Pos.file p) (Pos.min p) (Pos.max p)
)

let s_version =
	let pre = Option.map_default (fun pre -> "-" ^ pre) "" version_pre in
	Printf.sprintf "%d.%d.%d%s" version_major version_minor version_revision pre

let s_version_full v =
	match v.extra with
		| Some (_,build) -> s_version ^ "+" ^ build
		| _ -> s_version


let patch_string_pos p s = Pos.with_min p (Pos.max p - String.length s)

let gen_local_prefix = "`"

(* msg * backtrace *)
exception Ice of string * string

(* Raised to cooperatively cancel an ongoing compiler operation *)
exception Cancelled

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
	let backtrace = ml_loc ^ "\n" ^ backtrace in
	raise (Ice (msg,backtrace))

let dump_callstack () =
	print_endline (Printexc.raw_backtrace_to_string (Printexc.get_callstack 200))

let dump_backtrace () =
	print_endline (Printexc.raw_backtrace_to_string (Printexc.get_raw_backtrace ()))

let i32_31 = Int32.of_int 31
