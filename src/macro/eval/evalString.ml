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

open Globals
open EvalValue
open EvalBytes

let create_ascii s = {
	srope = Rope.of_string s;
	sstring = lazy s;
	slength = String.length s;
	sascii = true;
}

let create_ascii_of_rope r = {
	srope = r;
	sstring = lazy (Rope.to_string r);
	slength = Rope.length r;
	sascii = true;
}

let create_ucs2 s length = {
	srope = Rope.of_string s;
	sstring = lazy s;
	slength = length;
	sascii = false;
}

let create_ucs2_of_rope r length = {
	srope = r;
	sstring = lazy (Rope.to_string r);
	slength = length;
	sascii = false;
}

let vstring s = VString s

module AwareBuffer = struct
	type t = vstring_buffer

	let create () = {
		bbuffer = Rope.Buffer.create 0;
		blength = 0;
		bascii = true;
	}

	let promote_to_ucs this =
		let current = Rope.to_string (Rope.Buffer.contents this.bbuffer) in
		let current = extend_ascii current in
		Rope.Buffer.clear this.bbuffer;
		this.bascii <- false;
		Rope.Buffer.add_string this.bbuffer current

	let add_string this s =
		begin match s.sascii,this.bascii with
		| true,true
		| false,false ->
			Rope.Buffer.add_rope this.bbuffer s.srope
		| true,false ->
			Rope.Buffer.add_string this.bbuffer (extend_ascii (Lazy.force s.sstring))
		| false,true ->
			promote_to_ucs this;
			Rope.Buffer.add_rope this.bbuffer s.srope
		end;
		this.blength <- this.blength + s.slength

	let contents this =
		if this.bascii then
			create_ascii_of_rope (Rope.Buffer.contents this.bbuffer)
		else
			create_ucs2_of_rope (Rope.Buffer.contents this.bbuffer) this.blength
end

let read_char s =
	read_ui16 (Bytes.unsafe_of_string (Lazy.force s.sstring))

let utf8_to_utf16 s =
	let only_ascii = ref true in
	let buf = Buffer.create 0 in
	let l = ref 0 in
	let add i =
		incr l;
		Buffer.add_char buf (Char.unsafe_chr i);
		Buffer.add_char buf (Char.unsafe_chr (i lsr 8));
	in
	let length = String.length s in
	let i = ref 0 in
	let get () =
		let i' = int_of_char (String.unsafe_get s !i) in
		incr i;
		i'
	in
	while !i < length do
		let c = get() in
		if c < 0x80 then
			add c
		else if c < 0xE0 then begin
			only_ascii := false;
			add (((c land 0x3F) lsl 6) lor ((get ()) land 0x7F))
		end else if c < 0xF0 then begin
			only_ascii := false;
			let c2 = get () in
			add (((c land 0x1F) lsl 12) lor ((c2 land 0x7F) lsl 6) lor ((get ()) land 0x7F));
		end else begin
			only_ascii := false;
			let c2 = get () in
			let c3 = get () in
			let c = (((c land 0x0F) lsl 18) lor ((c2 land 0x7F) lsl 12) lor ((c3 land 0x7F) lsl 6) lor ((get ()) land 0x7F)) in
			add ((c lsr 10) + 0xD7C0);
			add ((c land 0x3FF) lor 0xDC00);
		end
	done;
	Buffer.contents buf,!only_ascii,!l

let utf16_to_utf8 s =
	let buf = Buffer.create 0 in
	let i = ref 0 in
	let add i =
		Buffer.add_char buf (Char.unsafe_chr i)
	in
	let b = Bytes.unsafe_of_string s in
	let read_byte b i = try read_byte b i with _ -> 0 in
	let get () =
		let ch1 = read_byte b !i in
		let ch2 = read_byte b (!i + 1) in
		let c = ch1 lor (ch2 lsl 8) in
		i := !i + 2;
		c
	in
	let length = String.length s in
	while !i < length do
		let c = get() in
		let c = if 0xD800 <= c && c <= 0xDBFF then
			(((c - 0xD7C0) lsl 10) lor ((get()) land 0X3FF))
		else
			c
		in
		if c <= 0x7F then
			add c
		else if c <= 0x7FF then begin
			add (0xC0 lor (c lsr 6));
			add (0x80 lor (c land 63));
		end else if c <= 0xFFFF then begin
			add (0xE0 lor (c lsr 12));
			add (0x80 lor ((c lsr 6) land 63));
			add (0x80 lor (c land 63));
		end else begin
			add (0xF0 lor (c lsr 18));
			add (0x80 lor ((c lsr 12) land 63));
			add (0x80 lor ((c lsr 6) land 63));
			add (0x80 lor (c land 63));
		end
	done;
	Buffer.contents buf

let maybe_extend_ascii s =
	let s' = Lazy.force s.sstring in
	if s.sascii then begin
		extend_ascii s'
	end else
		s'

let concat s1 s2 =
	match s1.sascii,s2.sascii with
	| true,true ->
		create_ascii_of_rope (Rope.concat2 s1.srope s2.srope)
	| false,false ->
		create_ucs2_of_rope (Rope.concat2 s1.srope s2.srope) (s1.slength + s2.slength)
	| true,false ->
		create_ucs2 ((extend_ascii (Lazy.force s1.sstring)) ^ (Lazy.force s2.sstring)) (s1.slength + s2.slength)
	| false,true ->
		create_ucs2 ((Lazy.force s1.sstring) ^ (extend_ascii (Lazy.force s2.sstring))) (s1.slength + s2.slength)

let join sep sl =
	let buf = AwareBuffer.create () in
	let rec loop sl = match sl with
		| [s] ->
			AwareBuffer.add_string buf s;
		| s :: sl ->
			AwareBuffer.add_string buf s;
			AwareBuffer.add_string buf sep;
			loop sl;
		| [] ->
			()
	in
	loop sl;
	AwareBuffer.contents buf

let bytes_to_utf8 s =
	let s',is_ascii,length = utf8_to_utf16 (Bytes.unsafe_to_string s) in
	if is_ascii then
		vstring (create_ascii (Bytes.unsafe_to_string s))
	else
		vstring (create_ucs2 s' length)

exception InvalidUnicodeChar

let case_map this upper =
	let f = if upper then Char.uppercase else Char.lowercase in
	let dest = Bytes.of_string (Lazy.force this.sstring) in
	let l = Bytes.length dest in
	let rec loop i =
		if i = l then
			()
		else begin
			let c = EvalBytes.read_ui16 dest i in
			if c <= 0xFF then begin
				let c' = f (char_of_int c) in
				EvalBytes.write_ui16 dest i (int_of_char c');
			end;
			loop (i + 2)
		end
	in
	loop 0;
	(create_ucs2 (Bytes.unsafe_to_string dest) this.slength)

let from_char_code i =
	if i < 0 then
		raise Not_found
	else if i < 128 then
		create_ascii (String.make 1 (char_of_int i))
	else if i < 0x10000 then begin
		if i >= 0xD800 && i <= 0xDFFF then raise InvalidUnicodeChar;
		let b = Bytes.create 2 in
		write_ui16 b 0 i;
		create_ucs2 (Bytes.unsafe_to_string b) 1
	end else if i < 0x110000 then begin
		let i = i - 0x10000 in
		let b = Bytes.create 4 in
		write_ui16 b 0 ((i lsr 10 + 0xD800));
		write_ui16 b 2 ((i land 1023) + 0xDC00);
		create_ucs2 (Bytes.unsafe_to_string b) 2
	end else
		raise InvalidUnicodeChar

let get s =
	let s' = Lazy.force s.sstring in
	if s.sascii then s'
	else utf16_to_utf8 s'