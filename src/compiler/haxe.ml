(*
	The Haxe Compiler
	Copyright (C) 2005-2019  Haxe Foundation

	This program is free software; you can redistribute it and/or
	modify it under the terms of the GNU General Public License
	as published by the Free Software Foundation; either version 2
	of the License, or (at your option) any later version.

	This program is distributed in the hope that it will be useful,
	but WITHOUT ANY WARRANTY; without even the implied warranty of
	MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
	GNU General Public License for more details.

	You should have received a copy of the GNU General Public License
	along with this program; if not, write to the Free Software
	Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA  02110-1301, USA.
 *)

(*
	Conventions:
	- e: expression (typed or untyped)
	- c: class
	- en: enum
	- td: typedef (tdef)
	- a: abstract
	- an: anon
	- tf: tfunc
	- cf: class_field
	- ef: enum_field
	- t: type (t)
	- ct: complex_type
	- v: local variable (tvar)
	- m: module (module_def)
	- mt: module_type
	- p: pos

	"param" refers to type parameters
	"arg" refers to function arguments
	leading s_ means function returns string
	trailing l means list (but we also use natural plurals such as "metas")
	semantic suffixes may be used freely (e.g. e1, e_if, e')
*)
open Server
open ParsedArg
open Ipaddr

let bad_arg msg =
	prerr_endline msg;
	exit 1

let parse_host_port hp =
	match (Ipaddr.with_port_of_string ~default:(-1) hp) with
	(* Short ipv6 notation will be mixed up with port; extract port and rebuild ipv6 *)
	| Ok (V6 ip, -1) ->
		let octets = ExtLib.String.split_on_char ':' (V6.to_string ip) in
		(match (List.rev octets) with
			| port :: octets -> (try V6 (V6.of_string_exn (ExtLib.String.join ":" (List.rev octets))), int_of_string port with _ -> bad_arg "Invalid host/port")
			| _ -> bad_arg "Invalid host/port"
		)
	| Ok (_, -1) -> bad_arg "Invalid host/port: missing port"
	| Ok (ip, port) -> ip, port
	(* Default to 127.0.0.1 with given port if no host is provided *)
	| Error _ when Str.string_match (Str.regexp "[0-9]+$") hp 0 -> V4 (V4.of_string_exn "127.0.0.1"), int_of_string hp
	| Error _ -> bad_arg "Invalid host/port"

;;
Sys.catch_break true;

(* Ignore SIGPIPE to prevent process termination when stdin pipe is closed.
   Sys.sigpipe may not map to the real signal number, so use 13 directly. *)
(try Sys.set_signal 13 Sys.Signal_ignore with _ -> ());

(* Dynamic GC tuning: space_overhead interpolates from max (small heap) down to min
   (large heap). This runs before argument parsing, so it is overridden via an env
   var rather than a define: HAXE_SPACE_OVERHEAD="N" sets both bounds to N, "MIN:MAX"
   sets them independently. *)
let gc_config =
	let default = DynamicGc.{
		min_space_overhead = 80;
		max_space_overhead = 100;
		heap_start_worrying_mb = 4_096;
		heap_really_worry_mb = 8_192;
	} in
	match (try Some (Sys.getenv "HAXE_SPACE_OVERHEAD") with Not_found -> None) with
	| None | Some "" -> default
	| Some s ->
		(try
			match List.map (fun p -> int_of_string (String.trim p)) (String.split_on_char ':' s) with
			| [v] -> { default with min_space_overhead = v; max_space_overhead = v }
			| [mn; mx] -> { default with min_space_overhead = mn; max_space_overhead = mx }
			| _ -> default
		with _ -> default)
in
DynamicGc.setup_dynamic_tuning gc_config;

let args = List.tl (Array.to_list Sys.argv) in
set_binary_mode_out stdout true;
set_binary_mode_out stderr true;

let parsed_args = Args.parse_args args in
let request_args = try Args.expand_args parsed_args with Arg.Bad msg -> bad_arg msg in

(* Are we a client? *)

begin match request_args.connect_arg with
	| Some hp ->
		let host, port = parse_host_port hp in
		(* Pass the entire args again, the server can take that apart. *)
		let code = Server.Connect.do_connect host port parsed_args in
		exit code
	| None ->
		()
end;

let is_verbose () = match request_args.parts with
	| [] ->
		false
	| part :: _ ->
		List.mem SetVerbose part.args
in

(* Are we a server? *)

begin match request_args.server_mode with
	| SMListen hp ->
		let accept =
			let host, port = parse_host_port hp in
			Server.init_wait_socket host port
		in
		let code = Server.wait_loop (is_verbose()) accept in
		exit code
	| SMConnect hp ->
		let host, port = parse_host_port hp in
		let accept = Server.init_wait_connect host port in
		let code = Server.wait_loop (is_verbose()) accept in
		exit code
	| SMNone ->
		()
end;

(* We are a normal non-server compilation. *)

let sctx = Server.setup_server_context false false in
let io = CompilerIo.create_stdio_io () in
let request_scope = create_request_scope ~is_server:false io request_args.display_arg in
let code = try HighLevel.entry sctx request_scope request_args with Arg.Bad msg -> bad_arg msg in
if code = 0 then begin
	let timer_ctx = request_scope.timer_ctx in
	if timer_ctx.measure_times = Yes then
		CompilerOutput.send_timer_report io timer_ctx
end;
ServerCompilationContext.dispose sctx;
exit code;