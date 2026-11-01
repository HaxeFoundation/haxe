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
open EvalValue

let vstring s = VString s

let create_ascii s =
	{
		sstring = s;
		slength = String.length s;
		soffsets = [];
	}

let create_with_length s length = {
	sstring = s;
	slength = length;
	soffsets = [];
}

let empty_string = create_ascii ""

let v_empty_string = VString empty_string

let create_unknown_vstring s =
	create_with_length s (try UTF8.length s with _ -> String.length s)

let create_unknown s =
	vstring (create_unknown_vstring s)

let concat s1 s2 =
	create_with_length (s1.sstring ^ s2.sstring) (s1.slength + s2.slength)

let join sep sl =
	let l_sep = sep.slength in
	let buf = Buffer.create 0 in
	let _,length = List.fold_left (fun (first,length) s ->
		let length = if first then 0 else length + l_sep in
		let length = length + s.slength in
		if not first then Buffer.add_string buf sep.sstring;
		Buffer.add_string buf s.sstring;
		(false,length)
	) (true,0) sl in
	create_with_length (Buffer.contents buf) length

(* How to store a new offset pair, relative to the cached pairs that were read. *)
type offset_update =
	| Single
	| Before of (int * int)
	| After of (int * int)

(* Other threads may read or replace the cache at any time, so this builds a new list
   instead of modifying the one that was read. A concurrent update may be lost, which
   only means that the offset is computed again later. *)
let set_offsets s update pair =
	s.soffsets <- match update with
		| Single -> [pair]
		| Before pair' -> [pair;pair']
		| After pair' -> [pair';pair]

let get_offset' s c_index =
	let rec get_b_offset c_len b_offset =
		if c_len = 0 then b_offset else
		get_b_offset (c_len - 1) (UTF8.next s.sstring b_offset)
	in
	let rec rget_b_offset c_len b_offset =
		if c_len = 0 then b_offset else
		rget_b_offset (c_len + 1) (UTF8.prev s.sstring b_offset)
	in
	match s.soffsets with
	| [] ->
		get_b_offset c_index 0,Single
	| [(c_index1,b_offset1) as r1] ->
		let diff = c_index - c_index1 in
		let b_offset = match diff with
			| 0 -> b_offset1
			| 1 -> UTF8.next s.sstring b_offset1
			| -1 -> UTF8.prev s.sstring b_offset1
			| _ ->
				if diff > 0 then
					get_b_offset diff b_offset1
				else if c_index + diff < 0 then
					(* big step backwards, better to start over *)
					get_b_offset c_index 0
				else
					rget_b_offset diff b_offset1
		in
		(* If our jump is larger than the scientifically determined value 20, upgrade
		   to two offset pointers. *)
		let update = if diff > 20 || diff < -20 then Before r1 else Single in
		b_offset,update
	| [(c_index1,b_offset1) as r1;(c_index2,b_offset2) as r2] ->
		let diff1 = c_index - c_index1 in
		let diff2 = c_index - c_index2 in
		(* Updating the first pair keeps the second one and vice versa. *)
		let update1 = Before r2 in
		let update2 = After r1 in
		begin match diff1,diff2 with
			| 0,_ -> b_offset1,update1
			| _,0 -> b_offset2,update2
			| 1,_ -> UTF8.next s.sstring b_offset1,update1
			| _,1 -> UTF8.next s.sstring b_offset2,update2
			| -1,_ -> UTF8.prev s.sstring b_offset1,update1
			| _,-1 -> UTF8.prev s.sstring b_offset2,update2
			| _ ->
				let update,diff,b_offset' = if abs diff1 > abs diff2 then
					update2,diff2,b_offset2
				else
					update1,diff1,b_offset1
				in
				let b_offset = if diff > 0 then
					get_b_offset diff b_offset'
				else if c_index + diff < 0 then
					(* big step backwards, better to start over *)
					get_b_offset c_index 0
				else
					rget_b_offset diff b_offset'
				in
				b_offset,update
		end
	| _ ->
		(* The cache never has more than two pairs, but start over if it does. *)
		get_b_offset c_index 0,Single

let get_offset s c_index =
	let b_offset,update = get_offset' s c_index in
	set_offsets s update (c_index,b_offset);
	b_offset

let char_at s c_index =
	let b_offset,update = get_offset' s c_index in
	let vs = s in
	let i = b_offset in
	let s = s.sstring in
    let n = Char.code s.[i] in
    let char,diff =
		if n < 0x80 then n,1
		else if n <= 0xdf then
		(n - 0xc0) lsl 6 lor (0x7f land (Char.code s.[i + 1])),2
		else if n <= 0xef then
		let n' = n - 0xe0 in
		let m0 = Char.code s.[i + 2] in
		let m = Char.code (String.unsafe_get s (i + 1)) in
		let n' = n' lsl 6 lor (0x7f land m) in
		n' lsl 6 lor (0x7f land m0),3
		else if n <= 0xf7 then
		let n' = n - 0xf0 in
		let m0 = Char.code s.[i + 3] in
		let m = Char.code (String.unsafe_get s (i + 1)) in
		let n' = n' lsl 6 lor (0x7f land m) in
		let m = Char.code (String.unsafe_get s (i + 2)) in
		let n' = n' lsl 6 lor (0x7f land m) in
		n' lsl 6 lor (0x7f land m0),4
		else if n <= 0xfb then
		let n' = n - 0xf8 in
		let m0 = Char.code s.[i + 4] in
		let m = Char.code (String.unsafe_get s (i + 1)) in
		let n' = n' lsl 6 lor (0x7f land m) in
		let m = Char.code (String.unsafe_get s (i + 2)) in
		let n' = n' lsl 6 lor (0x7f land m) in
		let m = Char.code (String.unsafe_get s (i + 3)) in
		let n' = n' lsl 6 lor (0x7f land m) in
		n' lsl 6 lor (0x7f land m0),5
		else if n <= 0xfd then
		let n' = n - 0xfc in
		let m0 = Char.code s.[i + 5] in
		let m = Char.code (String.unsafe_get s (i + 1)) in
		let n' = n' lsl 6 lor (0x7f land m) in
		let m = Char.code (String.unsafe_get s (i + 2)) in
		let n' = n' lsl 6 lor (0x7f land m) in
		let m = Char.code (String.unsafe_get s (i + 3)) in
		let n' = n' lsl 6 lor (0x7f land m) in
		let m = Char.code (String.unsafe_get s (i + 4)) in
		let n' = n' lsl 6 lor (0x7f land m) in
		n' lsl 6 lor (0x7f land m0),6
		else invalid_arg "UTF8.look"
	in
	set_offsets vs update (c_index + 1,b_offset + diff);
	char

let string_of_char_code i =
	UTF8.init 1 (fun _ ->  UCharExt.uchar_of_int i)

let from_char_code i =
	create_with_length (string_of_char_code i) 1

type scan_result =
	| ScanHit
	| ScanAscii
	| ScanOther

let find_substring this sub reverse =
	let cl_this = this.slength in
	let cl_sub = sub.slength in
	let bl_this = String.length this.sstring in
	let bl_sub = String.length sub.sstring in
	let s_this = this.sstring in
	let s_sub = sub.sstring in
	let rec scan b_index b_len =
		let c = String.unsafe_get s_this (b_index + b_len) in
		if c = String.unsafe_get s_sub b_len then begin
			if b_len + 1 = bl_sub then ScanHit
			else scan b_index (b_len + 1)
		end else if c <= '\127' then
			ScanAscii
		else
			ScanOther
	in
	if not reverse then begin
		let rec loop c_index b_index =
			if c_index > cl_this - cl_sub || b_index >= bl_this then raise Not_found;
			match scan b_index 0 with
			| ScanHit ->
				c_index,b_index,b_index + bl_sub
			| ScanAscii ->
				loop (c_index + 1) (b_index + 1)
			| ScanOther ->
				loop (c_index + 1) (UTF8.next s_this b_index)
		in
		loop
	end else begin
		let rec loop c_index b_index =
			if c_index > cl_this - cl_sub || b_index < 0 then raise Not_found;
			match scan b_index 0 with
			| ScanHit ->
				c_index,b_index,b_index + bl_sub
			| _ ->
				loop (c_index - 1) (UTF8.prev s_this b_index)
		in
		loop
	end

let case_map this upper =
	let buf = UTF8.Buf.create 0 in
	let a,m = if upper then EvalBytes.Unicase._UPPER,1022 else EvalBytes.Unicase._LOWER,1021 in
	UTF8.iter (fun uc ->
		let c = UCharExt.int_of_uchar uc in
		let up = c lsr 6 in
		let uc = if up < m then begin
			let c = a.(up).(c land ((1 lsl 6) - 1)) in
			if c <> 0 then UCharExt.uchar_of_int c
			else uc
		end else
			uc
		in
		UTF8.Buf.add_char buf uc
	) this.sstring;
	create_with_length (UTF8.Buf.contents buf) this.slength

let substr this c_index c_length =
	if c_length < 0 then
		create_with_length "" 0
	else begin
		let b_offset1 = get_offset this c_index in
		let b_offset2 = UTF8.move this.sstring b_offset1 c_length in
		create_with_length (String.sub this.sstring b_offset1 (b_offset2 - b_offset1)) c_length
	end

module VStringBuffer = struct
	type t = vstring_buffer

	let create () = {
		bbuffer = Buffer.create 0;
		blength = 0;
	}

	let add_string this s =
		Buffer.add_string this.bbuffer s.sstring;
		this.blength <- this.blength + s.slength

	let add_substring this s b_pos b_len c_len =
		Buffer.add_substring this.bbuffer s.sstring b_pos b_len;
		this.blength <- this.blength + c_len

	let clear this =
		Buffer.clear this.bbuffer;
		this.blength <- 0

	let contents this =
		create_with_length (Buffer.contents this.bbuffer) this.blength
end