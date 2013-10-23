(*
 *  This file is part of ilLib
 *  Copyright (c)2004-2013 Haxe Foundation
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

open PeData;;
open PeReader;;
open IlMeta;;
open IO;;

(* TODO: convert from string to Bigstring if OCaml 4 is available *)
type meta_ctx = {
	compressed : bool;
		(* is a compressed stream *)
	strings_stream : string;
	mutable strings_offset : int;
		(* #Strings: a string heap containing the names of metadata items *)
	blob_stream : string;
	mutable blob_offset : int;
		(* #Blob: blob heap containing internal metadata binary object, such as default values, signatures, etc *)
	guid_stream : string;
	mutable guid_offset : int;
		(* #GUID: a GUID heap *)
	us_stream : string;
		(* #US: user-defined strings *)
	meta_stream : string;
		(* may be either: *)
			(* #~: compressed (optimized) metadata stream *)
			(* #-: uncompressed (unoptimized) metadata stream *)
	mutable meta_edit_continue : bool;
	mutable meta_has_deleted : bool;

	tables : (clr_meta DynArray.t) array;
	extra_streams : clr_stream_header list;
}

let empty = ""

(* ******* Reading from Strings ********* *)

let sget s pos = Char.code (String.get s pos)

let read_compressed_i32 s pos =
	let v = sget s pos in
	(* Printf.printf "compressed: %x (18 0x%x 19 0x%x)\n" v (sget s (pos+20)) (sget s (pos+21)); *)
	if v land 0x80 = 0x00 then
		pos+1, v
	else if v land 0xC0 = 0x80 then
		pos+2, ((v land 0x3F) lsl 8) lor (sget s (pos+1))
	else if v land 0xE0 = 0xC0 then
		pos+4, ((v land 0x1F) lsl 24) lor ((sget s (pos+1)) lsl 16) lor ((sget s (pos+2)) lsl 8) lor (sget s (pos+3))
	else
		error (Printf.sprintf "Error reading compressed data. Invalid first byte: %x" v)

let tbl_idx (idx : clr_meta_idx) : int = Obj.magic idx
let idx_tbl (idx : int) : clr_meta_idx = Obj.magic idx

let sread_i32 s pos =
	let n1 = sget s pos in
	let n2 = sget s (pos+1) in
	let n3 = sget s (pos+2) in
	let n4 = sget s (pos+3) in
	pos+4, (n4 lsl 24) lor (n3 lsl 16) lor (n2 lsl 8) lor n1

let sread_ui16 s pos =
	let n1 = sget s pos in
	let n2 = sget s (pos+1) in
	pos+2, (n2 lsl 8) lor n1

let read_cstring ctx pos =
	let s = ctx.strings_stream in
	let rec loop en =
		match String.get s en with
		| '\x00' -> en - pos
		| _ -> loop (en+1)
	in
	let len = loop pos in
	String.sub s pos len

let read_sstring_idx ctx pos =
	let s = ctx.meta_stream in
	let metapos,i = if ctx.strings_offset = 2 then
		sread_ui16 s pos
	else
		sread_i32 s pos
	in
	match i with
	| 0 ->
		metapos, empty
	| _ ->
		metapos, read_cstring ctx i

let read_sguid_idx ctx pos =
	let s = ctx.meta_stream in
	let metapos,i = if ctx.guid_offset = 2 then
		sread_ui16 s pos
	else
		sread_i32 s pos
	in
	match i with
	| 0 ->
		metapos, empty
	| _ ->
		let s = ctx.guid_stream in
		let i = i - 1 in
		let pos = i * 16 in
		metapos, String.sub s pos 16

(* ******* Metadata Tables ********* *)
let check_bounds ctx n nrows =
	let dynarr = ctx.tables.(n) in
	if DynArray.length dynarr > nrows then
		error (Printf.sprintf "Reading outside table array bounds detected for table %d: Index %d is greater than max, %d"
			n
			(DynArray.length dynarr)
			nrows)

let null_module () =
	{
		m_generation = 0;
		m_name = empty;
		m_vid = empty;
		m_encid = empty;
		m_encbase_id = empty;
	}

let mk_null = function
	| IModule -> Module (null_module())
	| _ -> assert false

let get_table ctx idx rid =
	let cur = ctx.tables.(tbl_idx idx) in
	let len = DynArray.length cur in
	if len <= rid then begin
		let rec loop n =
			if n <= rid then begin
				DynArray.add cur (mk_null idx);
				loop (n+1)
			end
		in
		loop len
	end;
	DynArray.get cur rid

(* special coded types  *)
let read_resolution_scope ctx pos =

let read_module ctx n pos = match get_table ctx IModule n with
	| Module m ->
		let s = ctx.meta_stream in
		let pos, gen = sread_ui16 s pos in
		let pos, name = read_sstring_idx ctx pos in
		let pos, vid = read_sguid_idx ctx pos in
		let pos, encid = read_sguid_idx ctx pos in
		let pos, encbase_id = read_sguid_idx ctx pos in
		m.m_generation <- gen;
		m.m_name <- name;
		m.m_vid <- vid;
		m.m_encid <- encid;
		m.m_encbase_id <- encbase_id;
		pos, Module m
	| _ -> assert false

let read_

let read_meta ctx =
	(* read header *)
	let s = ctx.meta_stream in
	let pos = 4 + 1 + 1 in
	let flags = sget s pos in
	List.iter (fun i -> if flags land i = i then match i with
		| 0x01 ->
			ctx.strings_offset <- 4
		| 0x02 ->
			ctx.guid_offset <- 4
		| 0x04 ->
			ctx.blob_offset <- 4
		| 0x20 ->
			assert (not ctx.compressed);
			ctx.meta_edit_continue <- true
		| 0x80 ->
			assert (not ctx.compressed);
			ctx.meta_has_deleted <- true
		| _ -> assert false
	) [0x01;0x02;0x04;0x20;0x80];
	let rid = sget s (pos+1) in
	ignore rid;
	let pos = pos + 2 in
	let mask = Array.init 8 ( fun n -> sget s (pos + n) ) in
	(* loop over masks and check which table is set *)
	let set_table = Array.init 64 (fun n ->
		let idx = n / 8 in
		let bit = n mod 8 in
		(mask.(idx) lsr bit) land 0x1 = 0x1
	) in
	let pos = ref (pos + 8 + 8) in (* there is an extra 'sorted' field, which we do not use *)
	let rows = Array.map (function
		| false -> false,0
		| true ->
			let nidx, nrows = sread_i32 s !pos in
			pos := nidx;
			true,nrows
	) set_table in
	Array.iteri (fun n r -> match r with
		| false,_ -> ()
		| true,nrows ->
			check_bounds ctx n nrows;
			let fn = match idx_tbl n with
				| IModule ->
					read_module
				| _ -> assert false
			in
			let rec loop_fn n =
				if n = nrows then
					()
				else begin
					let p, _ = fn ctx n !pos in
					pos := p;
					loop_fn (n+1)
				end
			in
			loop_fn 0
	) rows;
	()



let read_padded i npad =
	let buf = Buffer.create 10 in
	let rec loop n =
		let chr = read i in
		if chr = '\x00' then begin
			let npad = n land 0x3 in
			if npad <> 0 then ignore (nread i (4 - npad));
			Buffer.contents buf
		end else begin
			Buffer.add_char buf chr;
			if n = npad then
				Buffer.contents buf
			else
				loop (n+1)
		end
	in
	loop 1

let read_meta_tables pctx header =
	let i = pctx.r.i in
	seek_rva pctx (fst header.clr_meta);
	let magic = nread i 4 in
	if magic <> "BSJB" then error ("Error reading metadata table: Expected magic 'BSJB'. Got " ^ magic);
	let major = read_ui16 i in
	let minor = read_ui16 i in
	ignore major; ignore minor; (* no use for them *)
	ignore (read_i32 i); (* reserved *)
	let vlen = read_i32 i in
	let ver = nread i vlen in

	(* meta storage header *)
	ignore (read_ui16 i); (* reserved *)
	let nstreams = read_ui16 i in
	let rec streams n acc =
		let offset = read_i32 i in
		let size = read_real_i32 i in
		let name = read_padded i 32 in
		let acc = {
			str_offset = offset;
			str_size = size;
			str_name = name;
		} :: acc in
		if (n+1) = nstreams then
			acc
		else
			streams (n+1) acc
	in
	let streams = streams 0 [] in

	(* streams *)
	let compressed = ref None in
	let sstrings = ref "" in
	let sblob = ref "" in
	let sguid = ref "" in
	let sus = ref "" in
	let smeta = ref "" in
	let extra = ref [] in
	List.iter (fun s ->
		let rva = Int32.add (fst header.clr_meta) (Int32.of_int s.str_offset) in
		seek_rva pctx rva;
		match String.lowercase s.str_name with
		| "#guid" ->
			sguid := nread i (Int32.to_int s.str_size)
		| "#strings" ->
			sstrings := nread i (Int32.to_int s.str_size)
		| "#us" ->
			sus := nread i (Int32.to_int s.str_size)
		| "#blob" ->
			sblob := nread i (Int32.to_int s.str_size)
		| "#~" ->
			assert (Option.is_none !compressed);
			compressed := Some true;
			smeta := nread i (Int32.to_int s.str_size)
		| "#-" ->
			assert (Option.is_none !compressed);
			compressed := Some false;
			smeta := nread i (Int32.to_int s.str_size)
		| _ ->
			extra := s :: !extra
	) streams;
	let compressed = match !compressed with
		| None -> error "No compressed or uncompressed metadata streams was found!"
		| Some c -> c
	in
	let tables = Array.init 64 (fun _ -> DynArray.create ()) in
	let ctx = {
		compressed = compressed;
		strings_stream = !sstrings;
		strings_offset = 2;
		blob_stream = !sblob;
		blob_offset = 2;
		guid_stream = !sguid;
		guid_offset = 2;
		us_stream = !sus;
		meta_stream = !smeta;
		meta_edit_continue = false;
		meta_has_deleted = false;
		extra_streams = !extra;
		tables = tables;
	} in
	read_meta ctx;
	()

