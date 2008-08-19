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

let bytecode = false
let native = true

(* ------ END CONFIGURATION ----- *)

let os_type = Sys.os_type

(* remove the comment to compile with windows using ocaml cygwin *)
(* let os_type = "Cygwin" *)

let obj_ext = match os_type with "Win32" -> ".obj" | _ -> ".o"
let exe_ext = match os_type with "Win32" | "Cygwin" -> ".exe" | _ -> ""
let ocamloptflags = match os_type with "Unix" -> "-cclib -fno-stack-protector " | _ -> ""

let zlib_path = match os_type with
	| "Win32" -> "../ocaml/extc/zlib/"
	| _ -> "./"

let zlib = match os_type with
	| "Win32" -> zlib_path ^ "zlib.lib"
	| _ ->
		try
			List.find Sys.file_exists ["/usr/lib/libz.dylib";"/usr/lib64/libz.so.1";"/usr/lib/libz.so.1";"/lib/libz.so.1";"/usr/lib/libz.so.4.1"]
		with
			Not_found ->
				failwith "LibZ was not found on your system, please install it or modify the search directories in the install script"

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
	if native then command ("ocamlopt -c " ^ ocamloptflags ^ file)

let modules l ext =
	String.concat " " (List.map (fun f -> f ^ ext) l)

;;

let motiontwin = ":pserver:anonymous@cvs.motion-twin.com:/cvsroot" in

let download_libs() =
	cvs motiontwin "co ocaml/swflib";
	cvs motiontwin "co ocaml/extc";
	cvs motiontwin "co ocaml/extlib-dev";
	cvs motiontwin "co ocaml/xml-light";
	cvs motiontwin "co neko/libs/include/ocaml"
in

let download() =
	msg "*** Please hit enter on login (empty password) ***";
	cvs motiontwin "login";
	cvs motiontwin "co haxe";
	download_libs();
in

let compile_libs() =
	(* EXTLIB *)
	Sys.chdir "ocaml/extlib-dev";
	command ("ocaml install.ml -nodoc -d .. " ^ (if bytecode then "-b " else "") ^ (if native then "-n" else ""));
	msg "";
	Sys.chdir "../..";

	(* EXTC *)
	Sys.chdir "ocaml/extc";
	let c_opts = (if Sys.ocaml_version < "3.08" then " -ccopt -Dcaml_copy_string=copy_string " else " ") in
	command ("ocamlc" ^ c_opts ^ " -I ../" ^ zlib_path ^ " extc_stubs.c");

	let options = "-cclib ../ocaml/extc/extc_stubs" ^ obj_ext ^ " -cclib " ^ zlib ^ " extc.mli extc.ml" in
	if bytecode then command ("ocamlc -a -o extc.cma " ^ options);
	if native then command ("ocamlopt -a -o extc.cmxa " ^ options);
	Sys.chdir "../..";

	(* SWFLIB *)
	Sys.chdir "ocaml/swflib";
	let files = "-I .. -I ../extc as3.mli as3hl.mli as3code.ml as3parse.ml as3hlparse.ml swf.ml swfZip.ml actionScript.ml swfParser.ml" in
	if bytecode then command ("ocamlc -a -o swflib.cma " ^ files);
	if native then command ("ocamlopt -a -o swflib.cmxa " ^ files);
	Sys.chdir "../..";

	(* XML-LIGHT *)
	Sys.chdir "ocaml/xml-light";
	command ("ocamlyacc	xml_parser.mly");
	command ("ocamlc xml.mli dtd.mli xml_parser.mli xml_lexer.mli");
	command ("ocamllex xml_lexer.mll");
	let files = "xml_parser.ml xml_lexer.ml dtd.ml xmlParser.mli xmlParser.ml xml.ml" in
	if bytecode then command ("ocamlc -a -o xml-light.cma " ^ files);
	if native then command ("ocamlopt -a -o xml-light.cmxa " ^ files);
	Sys.chdir "../..";

in

let compile() =

	(try Unix.mkdir "bin" 0o740 with Unix.Unix_error(Unix.EEXIST,_,_) -> ());

	compile_libs();

	(* HAXE *)
	Sys.chdir "haxe";
	command "ocamllex lexer.mll";
	let libs = [
		"../ocaml/extLib";
		"../ocaml/extc/extc";
		"../ocaml/swflib/swflib";
		"../ocaml/xml-light/xml-light";
		"unix"
	] in
	let neko = "../neko/libs/include/ocaml" in
	let paths = [
		"../ocaml";
		"../ocaml/swflib";
		"../ocaml/xml-light";
		"../ocaml/extc";
		neko
	] in
	let mlist = [
		"ast";"lexer";"type";"common";"parser";"typecore";
		"genxml";"typeload";"codegen";"typer";
		neko^"/nast";neko^"/binast";neko^"/nxml";
		"genneko";"genas3";"genjs";"genswf8";"genswf9";"genswf";"genphp";
		"main";
	] in
	let path_str = String.concat " " (List.map (fun s -> "-I " ^ s) paths) in
	let libs_str ext = " " ^ String.concat " " (List.map (fun l -> l ^ ext) libs) ^ " " in
	ocamlc (path_str ^ " -pp camlp4o " ^ modules mlist ".ml");
	if bytecode then command ("ocamlc -custom -o ../bin/haxe-byte" ^ exe_ext ^ libs_str ".cma" ^ modules mlist ".cmo");
	if native then command ("ocamlopt -o ../bin/haxe" ^ exe_ext ^ libs_str ".cmxa" ^ modules mlist ".cmx");

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