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

;;
Sys.catch_break true;

(* Ignore SIGPIPE to prevent process termination when stdin pipe is closed.
   Sys.sigpipe may not map to the real signal number, so use 13 directly. *)
(try Sys.set_signal 13 Sys.Signal_ignore with _ -> ());

DynamicGc.(setup_dynamic_tuning
  {
    min_space_overhead = 100;
    max_space_overhead = 120;
    heap_start_worrying_mb = 4_096;
    heap_really_worry_mb = 8_192;
  });

let args = List.tl (Array.to_list Sys.argv) in
set_binary_mode_out stdout true;
set_binary_mode_out stderr true;

let parsed_args = Args.parse_args args in
let curdir = Unix.getcwd () in
let request_args = Args.expand_args parsed_args in
Unix.chdir curdir;

(* Are we a client? *)

begin match request_args.connect_arg with
	| Some hp ->
		let host, port = Helper.parse_host_port hp in
		(* Pass the entire args again, the server can take that apart. *)
		let code = Server.Connect.do_connect host port parsed_args in
		exit code
	| None ->
		()
end;

let entry = Compiler.HighLevel.entry in

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
			let host, port = Helper.parse_host_port hp in
			Server.init_wait_socket host port
		in
		let code = Server.wait_loop entry (is_verbose()) accept in
		exit code
	| SMConnect hp ->
		let host, port = Helper.parse_host_port hp in
		let accept = Server.init_wait_connect host port in
		let code = Server.wait_loop entry (is_verbose()) accept in
		exit code
	| SMNone ->
		()
end;

(* We are a normal non-server compilation. *)

let sctx = Server.setup_server_context false in
let comm = ServerCommunication.Communication.create_stdio () in
let request_scope = create_request_scope() in
Compiler.HighLevel.entry sctx request_scope comm parsed_args;
ServerCompilationContext.dispose sctx;