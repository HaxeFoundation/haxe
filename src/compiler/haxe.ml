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

let other = Timer.timer ["other"];;
Sys.catch_break true;
MacroContext.setup();

let args = List.tl (Array.to_list Sys.argv) in
(try
	let server = Sys.getenv "HAXE_COMPILATION_SERVER" in
	let host, port = (try ExtString.String.split server ":" with _ -> "127.0.0.1", server) in
	Server_old.do_connect host (try int_of_string port with _ -> failwith "Invalid HAXE_COMPILATION_SERVER port") args
with Not_found ->
	set_binary_mode_out stdout true;
	set_binary_mode_out stderr true;
	let cs = CompilationServer.create () in
	let sctx = ServerCompilationContext.create false cs in
	Server.process sctx (Communication.create_stdio ()) args;
);
other();
if !Timer.measure_times then Timer.report_times prerr_endline
