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
open Type

type sourcemap = {
	sources : (string) DynArray.t;
	sources_hash : (string, int) Hashtbl.t;
	mappings : Buffer.t;

	mutable source_last_pos : sourcemap_pos;
	mutable print_comma : bool;
	mutable output_last_col : int;
	mutable output_current_col : int;
	mutable current_expr : sourcemap_pos option;
	(* the file of the last mapping, its index in [sources] and its lines *)
	mutable last_file : (string * int * Lexer.lexer_file) option;
}

and sourcemap_pos = {
	file : int;
	line : int;
	col : int;
}

let base64_chars = "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789+/"

(* Adds [number] to [b] as a base64 VLQ. *)
let rec add_vlq_digits b vlq =
	let digit = vlq land 31 and next = vlq asr 5 in
	Buffer.add_char b (String.unsafe_get base64_chars (if next > 0 then digit lor 32 else digit));
	if next > 0 then add_vlq_digits b next

let add_base64_vlq b number =
	add_vlq_digits b (if number < 0 then ((-number) lsl 1) + 1 else number lsl 1)

let encode_mapping smap pos =
	if smap.print_comma then
		Buffer.add_char smap.mappings ','
	else
		smap.print_comma <- true;
	let b = smap.mappings and last = smap.source_last_pos in
	add_base64_vlq b (smap.output_current_col - smap.output_last_col);
	add_base64_vlq b (pos.file - last.file);
	add_base64_vlq b (pos.line - last.line);
	add_base64_vlq b (pos.col - last.col);
	smap.source_last_pos <- pos;
	smap.output_last_col <- smap.output_current_col

let noop () = ()

(* The index of [file_name] in the sources (in their order of first use), and its lines. *)
let source_file smap file_name =
	match smap.last_file with
	| Some (name,file,lexer_file) when name == file_name -> file,lexer_file
	| _ ->
		let file = try
			Hashtbl.find smap.sources_hash file_name
		with Not_found ->
			let length = DynArray.length smap.sources in
			Hashtbl.replace smap.sources_hash file_name length;
			DynArray.add smap.sources file_name;
			length
		in
		let lexer_file = Lexer.find_file file_name in
		smap.last_file <- Some (file_name,file,lexer_file);
		file,lexer_file

(* What [add_mapping] gives when the mapping did not change: nothing to put
   back. *)
let unchanged = Some { file = -2; line = 0; col = 0 }

(* Maps the output that follows to [pos]: gives the mapping to put back with
   [clear_mapping] once the expression is output (no closure, no option is
   allocated for it). *)
let add_source_mapping smap pos =
	let last = smap.source_last_pos in
	if last.file <> pos.file || last.line <> pos.line || last.col <> pos.col then begin
		let old_current_expr = smap.current_expr in
		smap.current_expr <- Some pos;
		encode_mapping smap pos;
		old_current_expr
	end else
		unchanged

let add_mapping smap pos =
	if pos.pmin < 0 then unchanged else begin
		let file,lexer_file = source_file smap pos.pfile in
		let line, col = Lexer.find_line pos.pmin lexer_file in
		add_source_mapping smap { file = file; line = line - 1; col = col }
	end

let add_mapping smap e =
	match smap with
	| Some smap -> add_mapping smap e.epos
	| None -> unchanged

let clear_mapping smap old =
	if old != unchanged then match smap with
		| Some smap -> smap.current_expr <- old
		| None -> ()

(* The text [str] from [from] to [upto] is output. *)
let handle_newlines_sub smap str from upto =
	let rec loop from =
		match String.index_from_opt str from '\n' with
		| Some i when i < upto ->
			Buffer.add_char smap.mappings ';';
			smap.output_last_col <- 0;
			smap.output_current_col <- 0;
			smap.print_comma <- false;
			Option.may (encode_mapping smap) smap.current_expr;
			loop (i + 1)
		| _ ->
			smap.output_current_col <- smap.output_current_col + (upto - from);
	in
	loop from

let handle_newlines smap str =
	Option.may (fun smap -> handle_newlines_sub smap str 0 (String.length str)) smap

let create () = {
	source_last_pos = { file = 0; line = 0; col = 0};
	print_comma = false;
	output_last_col = 0;
	output_current_col = 0;
	sources = DynArray.create();
	sources_hash = Hashtbl.create 0;
	mappings = Buffer.create 16;
	current_expr = None;
	last_file = None;
}

(* The source map as JSON text. *)
let mappings_json (com : Gctx.t) smap source_path_prefix =
	let basefile = Filename.basename com.file in
	let b = Buffer.create 1024 in
	let sources = DynArray.to_list smap.sources in
	let to_url file =
		ExtString.String.map (fun c -> if c == '\\' then '/' else c) (Path.get_full_path file)
	in
	Buffer.add_string b "{\n";
	Buffer.add_string b "\"version\":3,\n";
	Buffer.add_string b ("\"file\":\"" ^ (String.concat "\\\\" (ExtString.String.nsplit basefile "\\")) ^ "\",\n");
	Buffer.add_string b ("\"sourceRoot\":\"\",\n");
	Buffer.add_string b ("\"sources\":[" ^
		(String.concat "," (List.map (fun s -> "\"" ^ source_path_prefix ^ to_url s ^ "\"") sources)) ^
		"],\n");
	if Gctx.defined com Define.SourceMapContent then begin
		Buffer.add_string b ("\"sourcesContent\":[" ^
			(String.concat "," (List.map (fun s -> try "\"" ^ StringHelper.s_escape (Std.input_file ~bin:true s) ^ "\"" with _ -> "null") sources)) ^
			"],\n");
	end;
	Buffer.add_string b "\"names\":[],\n";
	Buffer.add_string b "\"mappings\":\"";
	Buffer.add_string b (Buffer.sub smap.mappings 0 (Buffer.length smap.mappings));
	Buffer.add_string b "\"\n";
	Buffer.add_string b "}";
	Buffer.contents b

(* The source map, written next to the output as [<output>.map]. *)
let write_mappings (com : Gctx.t) smap source_path_prefix =
	let channel = open_out_bin (com.file ^ ".map") in
	output_string channel (mappings_json com smap source_path_prefix);
	close_out channel

(* The source map as a data: URL, for a [sourceMappingURL] comment: base64, padded. *)
let mappings_data_url (com : Gctx.t) smap source_path_prefix =
	let json = mappings_json com smap source_path_prefix in
	let chars = "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789+/" in
	let tbl = Array.init (String.length chars) (fun i -> String.get chars i) in
	let encoded = Bytes.unsafe_to_string (Base64.str_encode ~tbl json) in
	let padding = (4 - String.length encoded mod 4) mod 4 in
	"data:application/json;charset=utf-8;base64," ^ encoded ^ String.make padding '='
