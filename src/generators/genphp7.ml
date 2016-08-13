(*
	The Haxe Compiler
	Copyright (C) 2005-2016  Haxe Foundation

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

open Ast
open Type
open Common

(** 
	@param php7_path Output path specified by `-php7` compiler flag
	@param php_lib_path Path specified by `--php-lib` compiler flag
*)
let create_output_dir php7_path php_lib_path = 
	let lib_path = 
		match php_lib_path with
			| None -> ["lib"];
			| Some path -> (Str.split (Str.regexp "/")  path)
	in
	let rec create_recursively dir nested_dirs = 
		if not (Sys.file_exists dir) then (Unix.mkdir dir 0o755);
		match nested_dirs with
			| [] -> ();
			| next :: rest -> create_recursively (dir ^ "/" ^ next) rest
	in
	create_recursively php7_path lib_path

let generate com =
 	create_output_dir com.file com.php_lib