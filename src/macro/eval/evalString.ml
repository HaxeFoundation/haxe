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
		slength = (try UTF8.length s with _ -> String.length s);
		snext = (0,0);
	}

let create_ucs2 s length = {
	sstring = s;
	slength = (try UTF8.length s with _ -> length);
	snext = (0,0);
}

let create_unknown s =
	vstring (create_ucs2 s (try UTF8.length s with _ -> String.length s))

let concat s1 s2 =
	create_ucs2 (s1.sstring ^ s2.sstring) (s1.slength + s2.slength)

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
	create_ucs2 (Buffer.contents buf) length

let get s =
	s.sstring

let rec nth_aux s i n =
  if n = 0 then i else
  nth_aux s (UTF8.next s i) (n - 1)

let get_offset s index =
	let offset = if fst s.snext = index then
		snd s.snext
	else if fst s.snext < index then
		nth_aux s.sstring (snd s.snext) (index - fst s.snext)
	else
		UTF8.nth s.sstring index
	in
	if index < s.slength then s.snext <- (index + 1,UTF8.next s.sstring offset);
	offset

let read_char s index =
	let offset = get_offset s index in
	let char = UTF8.look s.sstring offset in
	UChar.int_of_uchar char

let string_of_char_code i =
	UTF8.init 1 (fun _ ->  UChar.uchar_of_int i)

let from_char_code i =
	create_ucs2 (string_of_char_code i) 1

let find_substring this sub reverse =
	let l_this = this.slength in
	let l_sub = sub.slength in
	let lb_sub = (String.length sub.sstring) in
	let s_this = this.sstring in
	let s_sub = sub.sstring in
	let rec scan i k =
		if k = lb_sub then true
		else if String.unsafe_get s_this (i + k) = String.unsafe_get s_sub k then scan i (k + 1)
		else false
	in
	if not reverse then begin
		let rec loop i b =
			if i > l_this - l_sub || b >= String.length this.sstring then raise Not_found;
			if scan b 0 then
				i,b,b + lb_sub
			else
				loop (i + 1) (UTF8.next s_this b)
		in
		loop
	end else begin
		let rec loop i b =
			if b < 0 then raise Not_found;
			if scan b 0 then
				i,b,b + lb_sub
			else
				loop (i - 1) (UTF8.prev s_this b)
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
	create_ucs2 (UTF8.Buf.contents buf) this.slength

let substr this index length =
	if length < 0 then
		create_ucs2 "" 0
	else begin
		let offset1 = get_offset this index in
		let offset2 = UTF8.move this.sstring offset1 length in
		create_ucs2 (String.sub this.sstring offset1 (offset2 - offset1)) length
	end

module AwareBuffer = struct
	type t = vstring_buffer

	let create () = {
		bbuffer = Buffer.create 0;
		blength = 0;
	}

	let add_string this s =
		Buffer.add_string this.bbuffer s.sstring;
		this.blength <- this.blength + s.slength

	let add_substring this s pos len slen =
		Buffer.add_substring this.bbuffer s.sstring pos len;
		this.blength <- this.blength + slen

	let contents this =
		create_ucs2 (Buffer.contents this.bbuffer) this.blength
end