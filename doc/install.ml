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
	| "Win32" -> "libs/extc/zlib/"
	| _ -> "./"

let zlib = match os_type with
	| "Win32" -> zlib_path ^ "zlib.lib"
	| _ ->
		try
			List.find Sys.file_exists ["/usr/lib/libz.dylib";"/usr/lib64/libz.so.1";"/usr/lib/libz.so.1";"/lib/libz.so.1";"/usr/lib/libz.so.4.1";"/lib/x86_64-linux-gnu/libz.so.1"]
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


let download() =
	command "svn co http://haxe.googlecode.com/svn/trunk haxe";
in

let compile_libs() =
	Sys.chdir "haxe/libs";

	(* EXTLIB *)
	Sys.chdir "extlib";
	command ("ocaml install.ml -nodoc -d .. " ^ (if bytecode then "-b " else "") ^ (if native then "-n" else ""));
	msg "";
	Sys.chdir "..";

	(* EXTC *)
	Sys.chdir "extc";
	let c_opts = (if Sys.ocaml_version < "3.08" then " -ccopt -Dcaml_copy_string=copy_string " else " ") in
	command ("ocamlc" ^ c_opts ^ " -I .. -I ../../" ^ zlib_path ^ " extc_stubs.c");

	let options = "-cclib libs/extc/extc_stubs" ^ obj_ext ^ " -cclib " ^ zlib ^ " extc.ml" in
	let options = if Sys.os_type = "Win32" then options ^ " -cclib shell32.lib" else options in
	if bytecode then command ("ocamlc -a -I .. -o extc.cma " ^ options);
	if native then command ("ocamlopt -a -I .. -o extc.cmxa " ^ options);
	Sys.chdir "..";

	(* SWFLIB *)
	Sys.chdir "swflib";
	let files = "-I .. -I ../extc as3.mli as3hl.mli as3code.ml as3parse.ml as3hlparse.ml swf.ml actionScript.ml swfParser.ml png.mli png.ml" in
	if bytecode then command ("ocamlc -a -o swflib.cma " ^ files);
	if native then command ("ocamlopt -a -o swflib.cmxa " ^ files);
	Sys.chdir "..";

	(* NEKO *)
	Sys.chdir "neko";
	let files = "-I .. nast.ml nxml.ml binast.ml nbytecode.ml ncompile.ml" in
	if bytecode then command ("ocamlc -a -o neko.cma " ^ files);
	if native then command ("ocamlopt -a -o neko.cmxa " ^ files);
	Sys.chdir "..";

	(* ZIPLIB *)
	Sys.chdir "ziplib";
	let files = "-I .. -I ../extc zlib.mli zlib.ml zip.mli zip.ml" in
	if bytecode then command ("ocamlc -a -o zip.cma " ^ files);
	if native then command ("ocamlopt -a -o zip.cmxa " ^ files);
	Sys.chdir "..";

	(* JAVALIB *)
	Sys.chdir "javalib";
	let files = "-I .. jData.mli jReader.ml" in
	if bytecode then command ("ocamlc -a -o java.cma " ^ files);
	if native then command ("ocamlopt -a -o java.cmxa " ^ files);
	Sys.chdir "..";

	(* TTFLIB *)
	Sys.chdir "ttflib";
	let files = "-I ../extlib -I ../swflib tTFData.ml tTFParser.ml tTFTools.ml tTFSwfWriter.ml tTFCanvasWriter.ml tTFJsonWriter.ml main.ml" in
	if bytecode then command ("ocamlc -a -o ttf.cma " ^ files);
	if native then command ("ocamlopt -a -o ttf.cmxa " ^ files);
	Sys.chdir "..";

	(* XML-LIGHT *)
	Sys.chdir "xml-light";
	command ("ocamlyacc	xml_parser.mly");
	command ("ocamlc xml.mli dtd.mli xml_parser.mli xml_lexer.mli");
	command ("ocamllex xml_lexer.mll");
	let files = "xml_parser.ml xml_lexer.ml dtd.ml xmlParser.mli xmlParser.ml xml.ml" in
	if bytecode then command ("ocamlc -a -o xml-light.cma " ^ files);
	if native then command ("ocamlopt -a -o xml-light.cmxa " ^ files);
	Sys.chdir "..";

	Sys.chdir "../..";
in

let compile() =

	(try Unix.mkdir "bin" 0o740 with Unix.Unix_error(Unix.EEXIST,_,_) -> ());

	Sys.chdir "haxe";
	(* HAXE *)
	command "ocamllex lexer.mll";
	let libs = [
		"libs/extLib";
		"libs/extc/extc";
		"libs/swflib/swflib";
		"libs/xml-light/xml-light";
		"libs/neko/neko";
		"libs/javalib/java";
		"unix";
		"libs/ziplib/zip";
		"str"
	] in
	let paths = [
		"libs";
		"libs/swflib";
		"libs/xml-light";
		"libs/extc";
		"libs/neko";
		"libs/ziplib";
		"libs/javalib"
	] in
	let mlist = [
		"ast";"lexer";"type";"common";"parser";"typecore";
		"genxml";"optimizer";"typeload";"codegen";
    "gencommon"; "genneko";"genas3";"genjs";"genswf8";"genswf9";"genswf";"genphp";"gencpp"; "gencs";"genjava";
		"interp";"typer";"matcher";"dce";"main";
	] in
	let path_str = String.concat " " (List.map (fun s -> "-I " ^ s) paths) in
	let libs_str ext = " " ^ String.concat " " (List.map (fun l -> l ^ ext) libs) ^ " " in
	ocamlc (path_str ^ " -pp camlp4o " ^ modules mlist ".ml");
	if bytecode then command ("ocamlc -custom -o ../bin/haxe-byte" ^ exe_ext ^ libs_str ".cma" ^ modules mlist ".cmo");
	if native then command ("ocamlopt -o ../bin/haxe" ^ exe_ext ^ libs_str ".cmxa" ^ modules mlist ".cmx");
in

let make_std() =

	if Sys.file_exists "../bin/std" then command (if os_type = "Win32" then "rmdir /S /Q ..\\bin\\std" else "rm -rf ../bin/std");
	command "svn export -q std ../bin/std";

in
let startdir = Sys.getcwd() in
try
	download();
	compile_libs();
	compile();
	make_std();
	Sys.chdir startdir;
with
	Failure msg ->
		Sys.chdir startdir;
		prerr_endline msg; exit 1
