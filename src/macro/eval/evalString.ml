(*
	The Haxe Compiler
	Copyright (C) 2005-2018  Haxe Foundation

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

open EvalValue

let vstring s = VString s

let create_ascii s =
	{
		sstring = s;
		slength = String.length s;
		soffsets = [|(0,0);(0,0);(0,0)|];
	}

let create_with_length s length = {
	sstring = s;
	slength = length;
	soffsets = [|(0,0);(0,0);(0,0)|];
}

let create_unknown s =
	vstring (create_with_length s (try UTF8.length s with _ -> String.length s))

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

let get_offset s c_index =
	let c_offset,b_offset = ref 0,ref 0 in
	let diff = ref c_index in
	let i = ref 0 in
	Array.iteri (fun i' (c_offset',b_offset') ->
		if c_offset' <= c_index then begin
			let diff' = c_index - c_offset' in
			if diff' <= !diff then begin
				diff := diff';
				c_offset := c_offset';
				b_offset := b_offset';
				i := i';
			end
		end
	) s.soffsets;
	let rec nth_aux s i n =
		if n = 0 then i else
		nth_aux s (UTF8.next s i) (n - 1)
	in
	let b_offset = nth_aux s.sstring !b_offset (c_index - !c_offset) in
	s.soffsets.(!i) <- (c_index,b_offset);
	b_offset

let char_at s c_index =
	let b_offset = get_offset s c_index in
	let char = UTF8.look s.sstring b_offset in
	UChar.int_of_uchar char

let string_of_char_code i =
	UTF8.init 1 (fun _ ->  UChar.uchar_of_int i)

let from_char_code i =
	create_with_length (string_of_char_code i) 1

let find_substring this sub reverse =
	let cl_this = this.slength in
	let cl_sub = sub.slength in
	let bl_this = String.length this.sstring in
	let bl_sub = String.length sub.sstring in
	let s_this = this.sstring in
	let s_sub = sub.sstring in
	let rec scan b_index b_len =
		if b_len = bl_sub then true
		else if String.unsafe_get s_this (b_index + b_len) = String.unsafe_get s_sub b_len then scan b_index (b_len + 1)
		else false
	in
	if not reverse then begin
		let rec loop c_index b_index =
			if c_index > cl_this - cl_sub || b_index >= bl_this then raise Not_found;
			if scan b_index 0 then
				c_index,b_index,b_index + bl_sub
			else
				loop (c_index + 1) (UTF8.next s_this b_index)
		in
		loop
	end else begin
		let rec loop c_index b_index =
			if b_index < 0 then raise Not_found;
			if scan b_index 0 then
				c_index,b_index,b_index + bl_sub
			else
				loop (c_index - 1) (UTF8.prev s_this b_index)
		in
		loop
	end

let case_map this upper =
	let buf = UTF8.Buf.create 0 in
	let a,m = if upper then EvalBytes.Unicase._UPPER,1022 else EvalBytes.Unicase._LOWER,1021 in
	UTF8.iter (fun uc ->
		let c = UChar.int_of_uchar uc in
		let up = c lsr 6 in
		let uc = if up < m then begin
			let c = a.(up).(c land ((1 lsl 6) - 1)) in
			if c <> 0 then UChar.uchar_of_int c
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

	let contents this =
		create_with_length (Buffer.contents this.bbuffer) this.blength
end