(*
 * IO - Abstract input/output
 * Copyright (C) 2003 Nicolas Cannasse
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Lesser General Public
 * License as published by the Free Software Foundation; either
 * version 2.1 of the License, or (at your option) any later version,
 * with the special exception on linking described in file LICENSE.
 *
 * This library is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 * Lesser General Public License for more details.
 *
 * You should have received a copy of the GNU Lesser General Public
 * License along with this library; if not, write to the Free Software
 * Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
 *)

type input = {
	mutable in_read : unit -> char;
	mutable in_input : string -> int -> int -> int;
	mutable in_close : unit -> unit;
}

type 'a output = {
	mutable out_write : char -> unit;
	mutable out_output : string -> int -> int -> int;
	mutable out_close : unit -> 'a;
	mutable out_flush : unit -> unit;
}

exception No_more_input
exception Input_closed
exception Output_closed

(* -------------------------------------------------------------- *)
(* API *)

let default_close = (fun () -> ())

let create_in ~read ~input ~close =
	{
		in_read = read;
		in_input = input;
		in_close = close;
	}

let create_out ~write ~output ~flush ~close =
	{
		out_write = write;
		out_output = output;
		out_close = close;
		out_flush = flush;
	}

let read i = i.in_read()

let nread i n =
	if n < 0 then invalid_arg "IO.nread";
	if n = 0 then
		""
	else
	let s = String.create n in
	let l = ref n in
	let p = ref 0 in
	try
		while !l > 0 do
			let r = i.in_input s !p !l in
			if r = 0 then raise No_more_input;
			p := !p + r;
			l := !l - r;
		done;
		s
	with
		No_more_input as e ->
			if !p = 0 then raise e;
			String.sub s 0 !p

let really_output o s p l' =
	let sl = String.length s in
	if p + l' > sl || p < 0 || l' < 0 then invalid_arg "IO.really_output";
   	let l = ref l' in
	let p = ref p in
	while !l > 0 do
		let w = o.out_output s !p !l in
		if w = 0 then raise Sys_blocked_io;
		p := !p + w;
		l := !l - w;
	done;
	l'

let input i s p l =
	let sl = String.length s in
	if p + l > sl || p < 0 || l < 0 then invalid_arg "IO.input";
	if l = 0 then
		0
	else
		i.in_input s p l

let really_input i s p l' =
	let sl = String.length s in
	if p + l' > sl || p < 0 || l' < 0 then invalid_arg "IO.really_input";
	let l = ref l' in
	let p = ref p in
	while !l > 0 do
		let r = i.in_input s !p !l in
		if r = 0 then raise Sys_blocked_io;
		p := !p + r;
		l := !l - r;
	done;
	l'

let really_nread i n =
	if n < 0 then invalid_arg "IO.really_nread";
	if n = 0 then ""
	else
	let s = String.create n
	in
	ignore(really_input i s 0 n);
	s

let close_in i =
	let f _ = raise Input_closed in
	i.in_close();
	i.in_read <- f;
	i.in_input <- f;
	i.in_close <- f

let write o x = o.out_write x

let nwrite o s =
	let p = ref 0 in
	let l = ref (String.length s) in
	while !l > 0 do
		let w = o.out_output s !p !l in
		if w = 0 then raise Sys_blocked_io;
		p := !p + w;
		l := !l - w;
	done

let output o s p l =
	let sl = String.length s in
	if p + l > sl || p < 0 || l < 0 then invalid_arg "IO.output";
	o.out_output s p l

let printf o fmt =
	Printf.kprintf (fun s -> nwrite o s) fmt

let flush o = o.out_flush()

let close_out o =
	let f _ = raise Output_closed in
	let r = o.out_close() in
	o.out_write <- f;
	o.out_output <- f;
	o.out_close <- f;
	o.out_flush <- f;
	r

let read_all i =
	let maxlen = 1024 in
	let str = ref [] in
	let pos = ref 0 in
	let rec loop() =
		let s = nread i maxlen in
		str := (s,!pos) :: !str;
		pos := !pos + String.length s;
		loop()
	in
	try
		loop()
	with
		No_more_input ->
			let buf = String.create !pos in
			List.iter (fun (s,p) ->
				String.unsafe_blit s 0 buf p (String.length s)
			) !str;
			buf

let pos_in i =
	let p = ref 0 in
	{
		in_read = (fun () ->
			let c = i.in_read() in
			incr p;
			c
		);
		in_input = (fun s sp l ->
			let n = i.in_input s sp l in
			p := !p + n;
			n
		);
		in_close = i.in_close
	} , (fun () -> !p)

let pos_out o =
	let p = ref 0 in
	{
		out_write = (fun c ->
			o.out_write c;
			incr p
		);
		out_output = (fun s sp l ->
			let n = o.out_output s sp l in
			p := !p + n;
			n
		);
		out_close = o.out_close;
		out_flush = o.out_flush;
	} , (fun () -> !p)

(* -------------------------------------------------------------- *)
(* Standard IO *)

let input_string s =
	let pos = ref 0 in
	let len = String.length s in
	{
		in_read = (fun () ->
			if !pos >= len then raise No_more_input;
			let c = String.unsafe_get s !pos in
			incr pos;
			c
		);
		in_input = (fun sout p l ->
			if !pos >= len then raise No_more_input;
			let n = (if !pos + l > len then len - !pos else l) in
			String.unsafe_blit s !pos sout p n;
			pos := !pos + n;
			n
		);
		in_close = (fun () -> ());
	}

let output_string() =
	let b = Buffer.create 0 in
	{
		out_write = (fun c ->
			Buffer.add_char b c
		);
		out_output = (fun s p l ->
			Buffer.add_substring b s p l;
			l
		);
		out_close = (fun () -> Buffer.contents b);
		out_flush = (fun () -> ());
	}

let input_channel ch =
	{
		in_read = (fun () ->
			try
				input_char ch
			with
				End_of_file -> raise No_more_input
		);
		in_input = (fun s p l ->
			let n = Pervasives.input ch s p l in
			if n = 0 then raise No_more_input;
			n
		);
		in_close = (fun () -> Pervasives.close_in ch);
	}

let output_channel ch =
	{
		out_write = (fun c -> output_char ch c);
		out_output = (fun s p l -> Pervasives.output ch s p l; l);
		out_close = (fun () -> Pervasives.close_out ch);
		out_flush = (fun () -> Pervasives.flush ch);
	}

let input_enum e =
	let pos = ref 0 in
	{
		in_read = (fun () ->
			match Enum.get e with
			| None -> raise No_more_input
			| Some c ->
				incr pos;
				c
		);
		in_input = (fun s p l ->
			let rec loop p l =
				if l = 0 then
					0
				else
					match Enum.get e with
					| None -> l
					| Some c ->
						String.unsafe_set s p c;
						loop (p + 1) (l - 1)
			in
			let k = loop p l in
			if k = l then raise No_more_input;
			l - k
		);
		in_close = (fun () -> ());
	}

let output_enum() =
	let b = Buffer.create 0 in
	{
		out_write = (fun x ->
			Buffer.add_char b x
		);
		out_output = (fun s p l ->
			Buffer.add_substring b s p l;
			l
		);
		out_close = (fun () ->
			let s = Buffer.contents b in
			ExtString.String.enum s
		);
		out_flush = (fun () -> ());
	}

let pipe() =
	let input = ref "" in
	let inpos = ref 0 in
	let output = Buffer.create 0 in
	let flush() =
		input := Buffer.contents output;
		inpos := 0;
		Buffer.reset output;
		if String.length !input = 0 then raise No_more_input
	in
	let read() =
		if !inpos = String.length !input then flush();
		let c = String.unsafe_get !input !inpos in
		incr inpos;
		c
	in
	let input s p l =
		if !inpos = String.length !input then flush();
		let r = (if !inpos + l > String.length !input then String.length !input - !inpos else l) in
		String.unsafe_blit !input !inpos s p r;
		inpos := !inpos + r;
		r
	in
	let write c =
		Buffer.add_char output c
	in
	let output s p l =
		Buffer.add_substring output s p l;
		l
	in
	let input = {
		in_read = read;
		in_input = input;
		in_close = (fun () -> ());
	} in
	let output = {
		out_write = write;
		out_output = output;
		out_close = (fun () -> ());
		out_flush = (fun () -> ());
	} in
	input , output

external cast_output : 'a output -> unit output = "%identity"

(* -------------------------------------------------------------- *)
(* BINARY APIs *)

exception Overflow of string

let read_byte i = int_of_char (i.in_read())

let read_signed_byte i =
	let c = int_of_char (i.in_read()) in
	if c land 128 <> 0 then
		c - 256
	else
		c

let read_string i =
	let b = Buffer.create 8 in
	let rec loop() =
		let c = i.in_read() in
		if c <> '\000' then begin
			Buffer.add_char b c;
			loop();
		end;
	in
	loop();
	Buffer.contents b

let read_line i =
	let b = Buffer.create 8 in
	let cr = ref false in
	let rec loop() =
		let c = i.in_read() in
		match c with
		| '\n' ->
			()
		| '\r' ->
			cr := true;
			loop()
		| _ when !cr ->
			cr := false;
			Buffer.add_char b '\r';
			Buffer.add_char b c;
			loop();
		| _ ->
			Buffer.add_char b c;
			loop();
	in
	try
		loop();
		Buffer.contents b
	with
		No_more_input ->
			if !cr then Buffer.add_char b '\r';
			if Buffer.length b > 0 then
				Buffer.contents b
			else
				raise No_more_input

let read_ui16 i =
	let ch1 = read_byte i in
	let ch2 = read_byte i in
	ch1 lor (ch2 lsl 8)

let read_i16 i =
	let ch1 = read_byte i in
	let ch2 = read_byte i in
	let n = ch1 lor (ch2 lsl 8) in
	if ch2 land 128 <> 0 then
		n - 65536
	else
		n

let read_i32 ch =
	let ch1 = read_byte ch in
	let ch2 = read_byte ch in
	let ch3 = read_byte ch in
	let ch4 = read_byte ch in
	if ch4 land 128 <> 0 then begin
		if ch4 land 64 = 0 then raise (Overflow "read_i32");
		ch1 lor (ch2 lsl 8) lor (ch3 lsl 16) lor ((ch4 land 127) lsl 24)
	end else begin
		if ch4 land 64 <> 0 then raise (Overflow "read_i32");
		ch1 lor (ch2 lsl 8) lor (ch3 lsl 16) lor (ch4 lsl 24)
	end

let read_real_i32 ch =
	let ch1 = read_byte ch in
	let ch2 = read_byte ch in
	let ch3 = read_byte ch in
	let base = Int32.of_int (ch1 lor (ch2 lsl 8) lor (ch3 lsl 16)) in
	let big = Int32.shift_left (Int32.of_int (read_byte ch)) 24 in
	Int32.logor base big

let read_i64 ch =
	let ch1 = read_byte ch in
	let ch2 = read_byte ch in
	let ch3 = read_byte ch in
	let ch4 = read_byte ch in
	let base = Int64.of_int (ch1 lor (ch2 lsl 8) lor (ch3 lsl 16)) in
	let small = Int64.logor base (Int64.shift_left (Int64.of_int ch4) 24) in
	let big = Int64.of_int32 (read_real_i32 ch) in
	Int64.logor (Int64.shift_left big 32) small

let read_double ch =
	Int64.float_of_bits (read_i64 ch)

let write_byte o n =
	(* doesn't test bounds of n in order to keep semantics of Pervasives.output_byte *)
	write o (Char.unsafe_chr (n land 0xFF))

let write_string o s =
	nwrite o s;
	write o '\000'

let write_line o s =
	nwrite o s;
	write o '\n'

let write_ui16 ch n =
	if n < 0 || n > 0xFFFF then raise (Overflow "write_ui16");
	write_byte ch n;
	write_byte ch (n lsr 8)

let write_i16 ch n =
	if n < -0x8000 || n > 0x7FFF then raise (Overflow "write_i16");
	if n < 0 then
		write_ui16 ch (65536 + n)
	else
		write_ui16 ch n

let write_i32 ch n =
	write_byte ch n;
	write_byte ch (n lsr 8);
	write_byte ch (n lsr 16);
	write_byte ch (n asr 24)

let write_real_i32 ch n =
	let base = Int32.to_int n in
	let big = Int32.to_int (Int32.shift_right_logical n 24) in
	write_byte ch base;
	write_byte ch (base lsr 8);
	write_byte ch (base lsr 16);
	write_byte ch big

let write_i64 ch n =
	write_real_i32 ch (Int64.to_int32 n);
	write_real_i32 ch (Int64.to_int32 (Int64.shift_right_logical n 32))

let write_double ch f =
	write_i64 ch (Int64.bits_of_float f)

(* -------------------------------------------------------------- *)
(* Big Endians *)

module BigEndian = struct

let read_ui16 i =
	let ch2 = read_byte i in
	let ch1 = read_byte i in
	ch1 lor (ch2 lsl 8)

let read_i16 i =
	let ch2 = read_byte i in
	let ch1 = read_byte i in
	let n = ch1 lor (ch2 lsl 8) in
	if ch2 land 128 <> 0 then
		n - 65536
	else
		n

let read_i32 ch =
	let ch4 = read_byte ch in
	let ch3 = read_byte ch in
	let ch2 = read_byte ch in
	let ch1 = read_byte ch in
	if ch4 land 128 <> 0 then begin
		if ch4 land 64 = 0 then raise (Overflow "read_i32");
		ch1 lor (ch2 lsl 8) lor (ch3 lsl 16) lor ((ch4 land 127) lsl 24)
	end else begin
		if ch4 land 64 <> 0 then raise (Overflow "read_i32");
		ch1 lor (ch2 lsl 8) lor (ch3 lsl 16) lor (ch4 lsl 24)
	end

let read_real_i32 ch =
	let big = Int32.shift_left (Int32.of_int (read_byte ch)) 24 in
	let ch3 = read_byte ch in
	let ch2 = read_byte ch in
	let ch1 = read_byte ch in
	let base = Int32.of_int (ch1 lor (ch2 lsl 8) lor (ch3 lsl 16)) in
	Int32.logor base big

let read_i64 ch =
	let big = Int64.of_int32 (read_real_i32 ch) in
	let ch4 = read_byte ch in
	let ch3 = read_byte ch in
	let ch2 = read_byte ch in
	let ch1 = read_byte ch in
	let base = Int64.of_int (ch1 lor (ch2 lsl 8) lor (ch3 lsl 16)) in
	let small = Int64.logor base (Int64.shift_left (Int64.of_int ch4) 24) in
	Int64.logor (Int64.shift_left big 32) small

let read_double ch =
	Int64.float_of_bits (read_i64 ch)

let write_ui16 ch n =
	if n < 0 || n > 0xFFFF then raise (Overflow "write_ui16");
	write_byte ch (n lsr 8);
	write_byte ch n

let write_i16 ch n =
	if n < -0x8000 || n > 0x7FFF then raise (Overflow "write_i16");
	if n < 0 then
		write_ui16 ch (65536 + n)
	else
		write_ui16 ch n

let write_i32 ch n =
	write_byte ch (n asr 24);
	write_byte ch (n lsr 16);
	write_byte ch (n lsr 8);
	write_byte ch n

let write_real_i32 ch n =
	let base = Int32.to_int n in
	let big = Int32.to_int (Int32.shift_right_logical n 24) in
	write_byte ch big;
	write_byte ch (base lsr 16);
	write_byte ch (base lsr 8);
	write_byte ch base

let write_i64 ch n =
	write_real_i32 ch (Int64.to_int32 (Int64.shift_right_logical n 32));
	write_real_i32 ch (Int64.to_int32 n)

let write_double ch f =
	write_i64 ch (Int64.bits_of_float f)

end

(* -------------------------------------------------------------- *)
(* Bits API *)

type 'a bc = {
	ch : 'a;
	mutable nbits : int;
	mutable bits : int;
}

type in_bits = input bc
type out_bits = unit output bc

exception Bits_error

let input_bits ch =
	{
		ch = ch;
		nbits = 0;
		bits = 0;
	}

let output_bits ch =
	{
		ch = cast_output ch;
		nbits = 0;
		bits = 0;
	}

let rec read_bits b n =
	if b.nbits >= n then begin
		let c = b.nbits - n in
		let k = (b.bits asr c) land ((1 lsl n) - 1) in
		b.nbits <- c;
		k
	end else begin
		let k = read_byte b.ch in
		if b.nbits >= 24 then begin
			if n >= 31 then raise Bits_error;
			let c = 8 + b.nbits - n in
			let d = b.bits land ((1 lsl b.nbits) - 1) in
			let d = (d lsl (8 - c)) lor (k lsr c) in
			b.bits <- k;
			b.nbits <- c;
			d
		end else begin
			b.bits <- (b.bits lsl 8) lor k;
			b.nbits <- b.nbits + 8;
			read_bits b n;
		end
	end

let drop_bits b =
	b.nbits <- 0

let rec write_bits b ~nbits x =
	let n = nbits in
	if n + b.nbits >= 32 then begin
		if n > 31 then raise Bits_error;
		let n2 = 32 - b.nbits - 1 in
		let n3 = n - n2 in
		write_bits b ~nbits:n2 (x asr n3);
		write_bits b ~nbits:n3 (x land ((1 lsl n3) - 1));
	end else begin
		if n < 0 then raise Bits_error;
		if (x < 0 || x > (1 lsl n - 1)) && n <> 31 then raise Bits_error;
		b.bits <- (b.bits lsl n) lor x;
		b.nbits <- b.nbits + n;
		while b.nbits >= 8 do
			b.nbits <- b.nbits - 8;
			write_byte b.ch (b.bits asr b.nbits)
		done
	end

let flush_bits b =
	if b.nbits > 0 then write_bits b (8 - b.nbits) 0

(* -------------------------------------------------------------- *)
(* Generic IO *)

class in_channel ch =
  object
	method input s pos len = input ch s pos len
	method close_in() = close_in ch
  end

class out_channel ch =
  object
	method output s pos len = output ch s pos len
	method flush() = flush ch
	method close_out() = ignore(close_out ch)
  end

class in_chars ch =
  object
	method get() = try read ch with No_more_input -> raise End_of_file
	method close_in() = close_in ch
  end

class out_chars ch =
  object
	method put t = write ch t
	method flush() = flush ch
	method close_out() = ignore(close_out ch)
  end

let from_in_channel ch =
	let cbuf = String.create 1 in
	let read() =
		try
			if ch#input cbuf 0 1 = 0 then raise Sys_blocked_io;
			String.unsafe_get cbuf 0
		with
			End_of_file -> raise No_more_input
	in
	let input s p l =
		ch#input s p l
	in
	create_in
		~read
		~input
		~close:ch#close_in

let from_out_channel ch =
	let cbuf = String.create 1 in
	let write c =
		String.unsafe_set cbuf 0 c;
		if ch#output cbuf 0 1 = 0 then raise Sys_blocked_io;
	in
	let output s p l =
		ch#output s p l
	in
	create_out
		~write
		~output
		~flush:ch#flush
		~close:ch#close_out

let from_in_chars ch =
	let input s p l =
		let i = ref 0 in
		try
			while !i < l do
				String.unsafe_set s (p + !i) (ch#get());
				incr i
			done;
			l
		with
			End_of_file when !i > 0 ->
				!i
	in
	create_in
		~read:ch#get
		~input
		~close:ch#close_in

let from_out_chars ch =
	let output s p l =
		for i = p to p + l - 1 do
			ch#put (String.unsafe_get s i)
		done;
		l
	in
	create_out
		~write:ch#put
		~output
		~flush:ch#flush
		~close:ch#close_out
