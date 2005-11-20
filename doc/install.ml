(*
 *  Haxe installer
 *  Copyright (c)2005 Nicolas Cannasse
 *
 *  This program is free software; you can redistribute it and/or modify
 *  it under the terms of the GNU General Public License as published by
 *  the Free Software Foundation; either version 2 of the License, or
 *  (at your option) any later version.
 *
 *  This program is distributed in the hope that it will be useful,
 *  but WITHOUT ANY WARRANTY; without even the implied warranty of
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 *  GNU General Public License for more details.
 *
 *  You should have received a copy of the GNU General Public License
 *  along with this program; if not, write to the Free Software
 *  Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
 *)

#load "unix.cma"


(* ----- BEGIN CONFIGURATION ---- *)

let zlib = "zlib.lib"
let bytecode = true
let native = true

(* ------ END CONFIGURATION ----- *)

let obj_ext = match Sys.os_type with "Win32" -> ".obj" | _ -> ".o"
let exe_ext = match Sys.os_type with "Win32" | "Cygwin" -> ".exe" | _ -> ""

let msg m =
	prerr_endline m;
	flush stdout

let command c =
	msg ("> " ^ c);
	if Sys.command c <> 0 then failwith ("Error while running " ^ c)

let cvs root cmd =
	command ("cvs -z3 -d" ^ root ^ " " ^ cmd)

let ocamlc file =
	if bytecode then command ("ocamlc -c " ^ file);
	if native then command ("ocamlopt -c " ^ file)

let modules l ext =
	String.concat " " (List.map (fun f -> f ^ ext) l)

;;

let sourceforge = ":pserver:anonymous@cvs.sourceforge.net:/cvsroot/ocaml-lib" in
let motiontwin = ":pserver:anonymous@cvs.motion-twin.com:/cvsroot" in

let download () =

	msg "*** Please hit enter on login (empty password) ***";
	cvs sourceforge "login";
	cvs sourceforge "co extlib-dev";

	msg "*** Please hit enter on login (empty password) ***";
	cvs motiontwin "login";
	cvs motiontwin "co haxe";
	cvs motiontwin "co ocaml/swflib";
	cvs motiontwin "co ocaml/extc";
	
in

let compile() =

	(try Unix.mkdir "bin" 0o740 with Unix.Unix_error(Unix.EEXIST,_,_) -> ());

	(* EXTLIB *)
	Sys.chdir "extlib-dev";
	command ("ocaml install.ml -nodoc -d ../ocaml " ^ (if bytecode then "-b " else "") ^ (if native then "-n" else ""));
	msg "";
	Sys.chdir "..";

	(* EXTC *)
	Sys.chdir "ocaml/extc";
	let c_opts = (if Sys.ocaml_version < "3.08" then " -ccopt -Dcaml_copy_string=copy_string " else " ") in
	command ("ocamlc" ^ c_opts ^ "extc_stubs.c");

	let options = "-cclib ../ocaml/extc/extc_stubs" ^ obj_ext ^ " -cclib " ^ zlib ^ " extc.mli extc.ml" in
	if bytecode then command ("ocamlc -a -o extc.cma " ^ options);
	if native then command ("ocamlopt -a -o extc.cmxa " ^ options);
	Sys.chdir "../..";

	(* SWFLIB *)
	Sys.chdir "ocaml/swflib";
	let files = "-I .. -I ../extc swf.ml swfZip.ml actionScript.ml swfParser.ml" in
	if bytecode then command ("ocamlc -a -o swflib.cma " ^ files);
	if native then command ("ocamlopt -a -o swflib.cmxa " ^ files);
	Sys.chdir "../..";

	(* HAXE *)
	Sys.chdir "haxe";
	command "ocamllex lexer.mll";
	ocamlc "ast.ml lexer.ml";
	ocamlc "-pp camlp4o parser.ml";
	ocamlc "-I ../ocaml -I ../ocaml/extc -I ../ocaml/swflib type.ml plugin.ml typer.ml genswf.ml main.ml";
	let mlist = ["ast";"lexer";"parser";"type";"plugin";"typer";"genswf";"main"] in
	if bytecode then command ("ocamlc -custom -o ../bin/haxe-byte" ^ exe_ext ^ " ../ocaml/extLib.cma ../ocaml/extc/extc.cma ../ocaml/swflib/swflib.cma " ^ modules mlist ".cmo");
	if native then command ("ocamlopt -o ../bin/haxe" ^ exe_ext ^ " ../ocaml/extLib.cmxa ../ocaml/extc/extc.cmxa ../ocaml/swflib/swflib.cmxa " ^ modules mlist ".cmx");

in
let startdir = Sys.getcwd() in
try
	download();
	compile();
	Sys.chdir startdir;
with
	Failure msg -> 
		Sys.chdir startdir;
		prerr_endline msg; exit 1