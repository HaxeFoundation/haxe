(*
 *  haXe/CPP Compiler
 *  Copyright (c)2008 Hugh Sanderson
 *  based on and including code by (c)2005-2008 Nicolas Cannasse
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


let join_path path separator =
   match fst path, snd path with
   | [], s -> s
   | el, s -> String.concat separator el ^ separator ^ s;;


class source_writer write_func close_func=
	object(this)
	val indent_str = "\t"
	val mutable indent = ""
	val mutable indents = []
	val mutable just_finished_block = false
	method close = close_func(); ()
	method write x = write_func x; just_finished_block <- false
	method indent_one = this#write indent_str

	method push_indent = indents <- indent_str::indents; indent <- String.concat "" indents
	method pop_indent = match indents with
							| h::tail -> indents <- tail; indent <- String.concat "" indents
							| [] -> indent <- "/*?*/";
	method write_i x = this#write (indent ^ x)
	method get_indent = indent
	method begin_block = this#write ("{\n"); this#push_indent
	method end_block = this#pop_indent; this#write_i "}\n"; just_finished_block <- true
	method end_block_line = this#pop_indent; this#write_i "}"; just_finished_block <- true
	method terminate_line = this#write (if just_finished_block then "" else ";\n")


	method add_include class_path =
		this#write ("#include <" ^ (join_path class_path "/") ^ ".h>\n")
end;;

let file_source_writer filename =
	let out_file = open_out filename in
	new source_writer (output_string out_file) (fun ()-> close_out out_file);;


let read_whole_file chan =
	let fileRead = ref "" in 
	(*we'll store results here before the EOF exception is thrown*)
	let bufSize = 4096 in
	let buf = String.create bufSize in (*rewritable buffer*)
	let rec reader accumString =
	let chunkLen = input chan buf 0 bufSize in (*input up to bufSize chars to buf*)
	if (chunkLen > 0) then (*more than 0 chars read*)
	let lastChunk = String.sub buf 0 chunkLen (*getting n chars read*) in
		reader (accumString^lastChunk) 
	else 
		accumString (*this case will occur before EOF*)
	in
	try
		let () = (fileRead := (reader "")) in (*reader stops and returns at 0 chars read*)
		!fileRead (*this will never be reached, EOF comes before*)
	with
		End_of_file -> !fileRead;; (*so we go and retrieve the string read*)



(* The cached_source_writer will not write to the file if it has not changed,
	thus allowing the makefile dependencies to work correctly *)
let cached_source_writer filename =
	try
		let in_file = open_in filename in
		let old_contents = read_whole_file in_file in
		close_in in_file;
		let buffer = Buffer.create 0 in
		let add_buf str = Buffer.add_string buffer str in
		let close = fun () ->
			let contents = Buffer.contents buffer in
			if (not (contents=old_contents) ) then begin
				let out_file = open_out filename in
				output_string out_file contents;
				close_out out_file;
			end;
		in
		new source_writer (add_buf) (close);
	with _ ->
		file_source_writer filename;;

let rec make_class_directories base dir_list =
	( match dir_list with
	| [] -> ()
	| dir :: remaining ->
		let path = base ^ "/" ^ dir  in
		if not (Sys.file_exists path) then
			Unix.mkdir path 0o755;
		make_class_directories path remaining
	);;


let new_source_file base_dir sub_dir extension class_path =
	make_class_directories base_dir ( sub_dir :: (fst class_path));
	cached_source_writer ( base_dir ^ "/" ^ sub_dir ^ "/" ^
							  ( String.concat "/" (fst class_path) ) ^ "/" ^
							  (snd class_path) ^ extension);;


let new_cpp_file base_dir = new_source_file base_dir "src" ".cpp";;

let new_header_file base_dir = new_source_file base_dir "include" ".h";;

let make_base_directory file =
	make_class_directories "." (file :: []);;

 
