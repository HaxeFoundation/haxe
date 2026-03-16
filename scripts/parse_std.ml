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
	parse_std: walks the std/ directory, parses every .hx file using the
	Haxe parser, and reports the total elapsed time.

	Usage: parse_std [<std-dir>]

	The optional argument defaults to "std" relative to the current directory.
*)

(** Recursively collect all .hx file paths under [dir]. *)
let rec collect_hx_files dir acc =
	let entries = Sys.readdir dir in
	Array.sort String.compare entries;
	Array.fold_left (fun acc entry ->
		let path = Filename.concat dir entry in
		if Sys.is_directory path then
			collect_hx_files path acc
		else if Filename.check_suffix entry ".hx" then
			path :: acc
		else
			acc
	) acc entries

(** Defines set on every parse to avoid [#error] branches in std files that
    require a concrete target.  The set mirrors a typical eval/interpreter
    environment that supports sys APIs and threading. *)
let base_defines =
	let d = Define.empty_defines () in
	List.iter (Define.raw_define d)
		["eval"; "interp"; "sys"; "scriptable"; "target.threaded"; "target.atomics"];
	d

(** Parse a single .hx file and return the parse result.  Any parse errors
    inside the file are tolerated (the parser returns a [ParseError] variant
    rather than raising). *)
let parse_file file =
	let config = Parser.create_config base_defines false false DisplayTypes.DisplayMode.DMNone false None in
	let lctx = Lexer.create_file_ctx file in
	let ch = open_in_bin file in
	Fun.protect ~finally:(fun () -> close_in ch) (fun () ->
		let lexbuf = Sedlexing.Utf8.from_channel ch in
		ParserEntry.parse config Grammar.parse_file lctx lexbuf file
	)

let () =
	let std_dir =
		if Array.length Sys.argv > 1 then Sys.argv.(1)
		else "std"
	in
	if not (Sys.file_exists std_dir && Sys.is_directory std_dir) then begin
		Printf.eprintf "Directory not found: %s\n%!" std_dir;
		exit 1
	end;
	let files = List.rev (collect_hx_files std_dir []) in
	let total = List.length files in
	Printf.printf "Parsing %d .hx files in '%s' ...\n%!" total std_dir;
	let errors = ref 0 in
	let t0 = Unix.gettimeofday () in
	List.iter (fun file ->
		(try ignore (parse_file file) with
		| Parser.Error (msg, _) ->
			incr errors;
			Printf.eprintf "Parse error in %s: %s\n%!" file (Parser.error_msg msg)
		| Lexer.Error (msg, _) ->
			incr errors;
			Printf.eprintf "Lex error in %s: %s\n%!" file (Lexer.error_msg msg)
		| exn ->
			incr errors;
			Printf.eprintf "Unexpected error in %s: %s\n%!" file (Printexc.to_string exn))
	) files;
	let elapsed = Unix.gettimeofday () -. t0 in
	Printf.printf "Done. %d files parsed in %.3f seconds" total elapsed;
	if !errors > 0 then
		Printf.printf " (%d error(s))" !errors;
	print_newline ()
