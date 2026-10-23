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

let encode_mapping smap pos =
	if smap.print_comma then
		Buffer.add_char smap.mappings ','
	else
		smap.print_comma <- true;

	let base64_vlq number =
		let encode_digit digit = String.unsafe_get base64_chars digit in
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
			Buffer.add_char smap.mappings (encode_digit (
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

let add_source_mapping smap pos =
	let last = smap.source_last_pos in
	if last.file <> pos.file || last.line <> pos.line || last.col <> pos.col then begin
		let old_current_expr = smap.current_expr in
		smap.current_expr <- Some pos;
		encode_mapping smap pos;
		(fun () -> smap.current_expr <- old_current_expr)
	end else
		noop

let add_mapping smap pos =
	if (Pos.min pos) < 0 then noop else begin
		let file,lexer_file = source_file smap (Pos.file pos) in
		let line, col = Lexer.find_line (Pos.min pos) lexer_file in
		add_source_mapping smap { file = file; line = line - 1; col = col }
	end

let add_mapping smap e =
	Option.map_default (fun smap -> add_mapping smap e.epos) noop smap

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

(* The mappings of a fragment, encoded from an empty state with its own sources. *)
type encoded_fragment = {
	ef_mappings : string;
	ef_sources : string array;
	(* the final state *)
	ef_last_pos : sourcemap_pos option;
	ef_output_last_col : int;
	ef_output_current_col : int;
	ef_print_comma : bool;
	ef_has_newline : bool;
	(* the mappings after the first one, encoded for the global indexes of the
	   sources when they are not only shifted (the same in the next
	   compilations, as long as the sources keep their indexes) *)
	mutable ef_spliced : (int array * string) option;
}

let encoded_fragment smap = {
	ef_mappings = Buffer.contents smap.mappings;
	ef_sources = DynArray.to_array smap.sources;
	ef_last_pos = (if DynArray.length smap.sources = 0 then None else Some smap.source_last_pos);
	ef_output_last_col = smap.output_last_col;
	ef_output_current_col = smap.output_current_col;
	ef_print_comma = smap.print_comma;
	ef_has_newline = Buffer.length smap.mappings > 0 && String.contains (Buffer.contents smap.mappings) ';';
	ef_spliced = None;
}

let decode_vlq s i =
	let rec loop i shift acc =
		let c = String.unsafe_get s i in
		let digit = String.index base64_chars c in
		let acc = acc lor ((digit land 31) lsl shift) in
		if digit land 32 <> 0 then loop (i + 1) (shift + 5) acc
		else i + 1,(if acc land 1 = 1 then - (acc lsr 1) else acc lsr 1)
	in
	loop i 0 0

(* Puts the mappings of a fragment, whose text was just output, in [smap]
   (the state before the text is [col0], [last_col0], [comma0]): the
   encoded mappings are transcoded, the indices of their sources becoming
   those of [smap], and the first one encoded relatively to the previous
   mapping. Only when the state is the one of the fragment's start (no
   current expression, the first mapping not the same position as the
   previous one, which would not have been output): false otherwise, and
   nothing done. *)
(* Where the mappings after the first one start, and the source of the
   first one. *)
let first_mapping m =
	let len = String.length m in
	let i = ref 0 in
	while !i < len && m.[!i] = ';' do incr i done;
	let i1,_ = decode_vlq m !i in
	let i2,dfile = decode_vlq m i1 in
	let i3,_ = decode_vlq m i2 in
	let i4,_ = decode_vlq m i3 in
	i4,dfile

(* Whether the sources of a fragment keep their order once global: the
   deltas of the mappings are then the same. *)
let shifted globals =
	try Array.iteri (fun i g -> if g - globals.(0) <> i then raise Exit) globals; true with Exit -> false

(* The mappings after the first one of fragment [ef], its sources having
   the global indices [globals]: the same deltas, but the one of their
   source. Kept with the fragment for these indices. *)
let spliced_tail ef globals =
	match ef.ef_spliced with
	| Some (g,tail) when g = globals -> tail
	| _ ->
		let m = ef.ef_mappings in
		let len = String.length m in
		let i4,dfile = first_mapping m in
		let b = Buffer.create (len - i4) in
		let add_vlq number =
			let vlq = if number < 0 then ((-number) lsl 1) + 1 else number lsl 1 in
			let rec loop vlq =
				let digit = vlq land 31 and next = vlq asr 5 in
				Buffer.add_char b (String.unsafe_get base64_chars (if next > 0 then digit lor 32 else digit));
				if next > 0 then loop next
			in
			loop vlq
		in
		let local = ref dfile in
		let rec loop i =
			if i < len then match String.unsafe_get m i with
				| ';' | ',' as c -> Buffer.add_char b c; loop (i + 1)
				| _ ->
					(* a segment: column, source, line, column in the source *)
					let i1,dcol = decode_vlq m i in
					let i2,dfile = decode_vlq m i1 in
					let i3,dline = decode_vlq m i2 in
					let i4,dscol = decode_vlq m i3 in
					let previous = globals.(!local) in
					local := !local + dfile;
					add_vlq dcol;
					add_vlq (globals.(!local) - previous);
					add_vlq dline;
					add_vlq dscol;
					loop i4
		in
		loop i4;
		let tail = Buffer.contents b in
		ef.ef_spliced <- Some (globals,tail);
		tail

(* The global indices that the sources of the fragments [efs] get when they
   are spliced in this order into [smap] (without changing it): their
   mappings are transcoded for them in parallel, before being spliced in
   order. A fragment that cannot be spliced (generated again) may use its
   sources in another order: the tails computed for the fragments after it
   are then not used. *)
let predict_globals smap efs =
	let known = Hashtbl.copy smap.sources_hash in
	let n = ref (DynArray.length smap.sources) in
	Array.map (function
		| Some ef when ef.ef_last_pos <> None ->
			Some (Array.map (fun name -> match Hashtbl.find_opt known name with
				| Some i -> i
				| None -> let i = !n in Hashtbl.replace known name i; incr n; i
			) ef.ef_sources)
		| _ -> None
	) efs

let splice_fragment smap ef col0 last_col0 comma0 =
	if smap.current_expr <> None then false
	else if ef.ef_last_pos = None then begin
		(* no mapping: only newlines and columns *)
		if ef.ef_has_newline then begin
			Buffer.add_string smap.mappings ef.ef_mappings;
			smap.output_last_col <- ef.ef_output_last_col;
			smap.output_current_col <- ef.ef_output_current_col;
			smap.print_comma <- ef.ef_print_comma
		end else
			smap.output_current_col <- col0 + ef.ef_output_current_col;
		true
	end else begin
		let m = ef.ef_mappings in
		let len = String.length m in
		(* the leading newlines, then the first mapping *)
		let i = ref 0 in
		while !i < len && m.[!i] = ';' do incr i done;
		let newlines = !i in
		let i1,dcol = decode_vlq m !i in
		let i2,dfile = decode_vlq m i1 in
		let i3,dline = decode_vlq m i2 in
		let i4,dscol = decode_vlq m i3 in
		let index name = try Some (Hashtbl.find smap.sources_hash name) with Not_found -> None in
		let first_file = match index ef.ef_sources.(dfile) with Some i -> i | None -> DynArray.length smap.sources in
		let last = smap.source_last_pos in
		if last.file = first_file && last.line = dline && last.col = dscol then false
		else begin
			(* the sources, in their order of first use *)
			let globals = Array.map (fun name -> match index name with
				| Some i -> i
				| None ->
					let i = DynArray.length smap.sources in
					Hashtbl.replace smap.sources_hash name i;
					DynArray.add smap.sources name;
					i
			) ef.ef_sources in
			smap.last_file <- None;
			(* the state where the first mapping is encoded *)
			for _ = 1 to newlines do Buffer.add_char smap.mappings ';' done;
			let col = if newlines = 0 then col0 + dcol else dcol in
			smap.output_last_col <- (if newlines = 0 then last_col0 else 0);
			smap.print_comma <- (if newlines = 0 then comma0 else false);
			smap.output_current_col <- col;
			encode_mapping smap { file = globals.(dfile); line = dline; col = dscol };
			(* the others: the same deltas, but the one of their source *)
			if shifted globals then Buffer.add_substring smap.mappings m i4 (len - i4) else
				Buffer.add_string smap.mappings (spliced_tail ef globals);
			(* the final state *)
			let fl = match ef.ef_last_pos with Some p -> p | None -> assert false in
			smap.source_last_pos <- { fl with file = globals.(fl.file) };
			if ef.ef_has_newline then begin
				smap.output_last_col <- ef.ef_output_last_col;
				smap.output_current_col <- ef.ef_output_current_col;
				smap.print_comma <- ef.ef_print_comma
			end else begin
				smap.output_last_col <- col0 + ef.ef_output_last_col;
				smap.output_current_col <- col0 + ef.ef_output_current_col;
				smap.print_comma <- true
			end;
			true
		end
	end

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
