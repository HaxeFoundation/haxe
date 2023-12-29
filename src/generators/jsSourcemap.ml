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
open Extlib_leftovers
open Globals
open Ast
open Type
open Common

type sourcemap = {
	sources : (string) DynArray.t;
	sources_hash : (string, int) Hashtbl.t;
	mappings : Rbuffer.t;

	mutable source_last_pos : sourcemap_pos;
	mutable print_comma : bool;
	mutable output_last_col : int;
	mutable output_current_col : int;
	mutable current_expr : sourcemap_pos option;
}

and sourcemap_pos = {
	file : int;
	line : int;
	col : int;
}

let encode_mapping smap pos =
	if smap.print_comma then
		Rbuffer.add_char smap.mappings ','
	else
		smap.print_comma <- true;

	let base64_vlq number =
		let encode_digit digit =
			let chars = [|
				'A';'B';'C';'D';'E';'F';'G';'H';'I';'J';'K';'L';'M';'N';'O';'P';
				'Q';'R';'S';'T';'U';'V';'W';'X';'Y';'Z';'a';'b';'c';'d';'e';'f';
				'g';'h';'i';'j';'k';'l';'m';'n';'o';'p';'q';'r';'s';'t';'u';'v';
				'w';'x';'y';'z';'0';'1';'2';'3';'4';'5';'6';'7';'8';'9';'+';'/'
			|] in
			Array.unsafe_get chars digit
		in
		let to_vlq number =
			if number < 0 then
				((-number) lsl 1) + 1
			else
				number lsl 1
		in
		let rec loop vlq =
			let shift = 5 in
			let base = 1 lsl shift in
			let mask = base - 1 in
			let continuation_bit = base in
			let digit = vlq land mask in
			let next = vlq asr shift in
			Rbuffer.add_char smap.mappings (encode_digit (
				if next > 0 then digit lor continuation_bit else digit));
			if next > 0 then loop next else ()
		in
		loop (to_vlq number)
	in

	base64_vlq (smap.output_current_col - smap.output_last_col);
	base64_vlq (pos.file - smap.source_last_pos.file);
	base64_vlq (pos.line - smap.source_last_pos.line);
	base64_vlq (pos.col - smap.source_last_pos.col);

	smap.source_last_pos <- pos;
	smap.output_last_col <- smap.output_current_col

let noop () = ()

let add_mapping smap pos =
	if pos.pmin < 0 then noop else

	let file = try
		Hashtbl.find smap.sources_hash pos.pfile
	with Not_found ->
		let length = DynArray.length smap.sources in
		Hashtbl.replace smap.sources_hash pos.pfile length;
		DynArray.add smap.sources pos.pfile;
		length
	in

	let pos =
		let line, col = Lexer.find_pos pos in
		let line = line - 1 in
		{ file = file; line = line; col = col }
	in

	if smap.source_last_pos <> pos then begin
		let old_current_expr = smap.current_expr in
		smap.current_expr <- Some pos;
		encode_mapping smap pos;
		(fun () -> smap.current_expr <- old_current_expr)
	end else
		noop

let add_mapping smap e =
	Option.map_default (fun smap -> add_mapping smap e.epos) noop smap

let handle_newlines smap str =
	Option.may (fun smap ->
		let rec loop from =
			try begin
				let next = String.index_from str from '\n' + 1 in
				Rbuffer.add_char smap.mappings ';';
				smap.output_last_col <- 0;
				smap.output_current_col <- 0;
				smap.print_comma <- false;
				Option.may (encode_mapping smap) smap.current_expr;
				loop next
			end with Not_found ->
				smap.output_current_col <- smap.output_current_col + (String.length str - from);
		in
		loop 0
	) smap

let write_mappings (com : Common.context) smap source_path_prefix =
	let basefile = Filename.basename com.file in
	let channel = open_out_bin (com.file ^ ".map") in
	let sources = DynArray.to_list smap.sources in
	let to_url file =
		ExtString.String.map (fun c -> if c == '\\' then '/' else c) (Path.get_full_path file)
	in
	output_string channel "{\n";
	output_string channel "\"version\":3,\n";
	output_string channel ("\"file\":\"" ^ (String.concat "\\\\" (ExtString.String.nsplit basefile "\\")) ^ "\",\n");
	output_string channel ("\"sourceRoot\":\"\",\n");
	output_string channel ("\"sources\":[" ^
		(String.concat "," (List.map (fun s -> "\"" ^ source_path_prefix ^ to_url s ^ "\"") sources)) ^
		"],\n");
	if Common.defined com Define.SourceMapContent then begin
		output_string channel ("\"sourcesContent\":[" ^
			(String.concat "," (List.map (fun s -> try "\"" ^ StringHelper.s_escape (Std.input_file ~bin:true s) ^ "\"" with _ -> "null") sources)) ^
			"],\n");
	end;
	output_string channel "\"names\":[],\n";
	output_string channel "\"mappings\":\"";
	Rbuffer.output_buffer channel smap.mappings;
	output_string channel "\"\n";
	output_string channel "}";
	close_out channel