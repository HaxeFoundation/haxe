(*
 * Unzip - inflate format decompression algorithm
 * Copyright (C) 2004 Nicolas Cannasse
 * Compliant with RFC 1950 and 1951
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

type huffman =
	| Found of int
	| NeedBit of huffman * huffman
	| NeedBits of int * huffman array


type adler32 = {
	mutable a1 : int;
	mutable a2 : int;
}

type window = {
	mutable wbuffer : string;
	mutable wpos : int;
	wcrc : adler32;
}

type state =
	| Head
	| Block
	| CData
	| Flat
	| Crc
	| Dist
	| DistOne
	| Done

type t = {
	mutable znbits : int;
	mutable zbits : int;
	mutable zstate : state;
	mutable zfinal : bool;
	mutable zhuffman : huffman;
	mutable zhuffdist : huffman option;
	mutable zlen : int;
	mutable zdist : int;
	mutable zneeded : int;
	mutable zoutput : string;
	mutable zoutpos : int;
	zinput : IO.input;
	zlengths : int array;
	zwindow : window;
}

type error_msg =
	| Invalid_huffman
	| Invalid_data
	| Invalid_crc
	| Truncated_data
	| Unsupported_dictionary

exception Error of error_msg

let error msg = raise (Error msg)

(* ************************************************************************ *)
(* HUFFMAN TREES *)

let rec tree_depth = function
	| Found _ -> 0
	| NeedBits _ -> assert false
	| NeedBit (a,b) ->
		1 + min (tree_depth a) (tree_depth b)

let rec tree_compress t =
	match tree_depth t with
	| 0 -> t
	| 1 ->
		(match t with
		| NeedBit (a,b) -> NeedBit (tree_compress a,tree_compress b)
		| _ -> assert false)
	| d ->
		let size = 1 lsl d in
		let tbl = Array.make size (Found (-1)) in
		tree_walk tbl 0 0 d t;
		NeedBits (d,tbl)

and tree_walk tbl p cd d = function
	| NeedBit (a,b) when d > 0 ->
		tree_walk tbl p (cd + 1) (d-1) a;
		tree_walk tbl (p lor (1 lsl cd)) (cd + 1) (d-1) b;
	| t ->
		Array.set tbl p (tree_compress t)

let make_huffman lengths pos nlengths maxbits =
	let counts = Array.make maxbits 0 in
	for i = 0 to nlengths - 1 do
		let p = Array.unsafe_get lengths (i + pos) in
		if p >= maxbits then error Invalid_huffman;
		Array.unsafe_set counts p (Array.unsafe_get counts p + 1);
	done;
	let code = ref 0 in
	let tmp = Array.make maxbits 0 in
	for i = 1 to maxbits - 2 do
		code := (!code + Array.unsafe_get counts i) lsl 1;
		Array.unsafe_set tmp i !code;
	done;
	let bits = Hashtbl.create 0 in
	for i = 0 to nlengths - 1 do
		let l = Array.unsafe_get lengths (i + pos) in
		if l <> 0 then begin
			let n = Array.unsafe_get tmp (l - 1) in
			Array.unsafe_set tmp (l - 1) (n + 1);
			Hashtbl.add bits (n,l) i;
		end;
	done;
	let rec tree_make v l =
		if l > maxbits then error Invalid_huffman;
		try
			Found (Hashtbl.find bits (v,l))
		with
			Not_found ->
				NeedBit (tree_make (v lsl 1) (l + 1) , tree_make (v lsl 1 lor 1) (l + 1))
	in
	tree_compress (NeedBit (tree_make 0 1 , tree_make 1 1))

(* ************************************************************************ *)
(* ADLER32 (CRC) *)

let adler32_create() = {
	a1 = 1;
	a2 = 0;
}

let adler32_update a s p l =
	let p = ref p in
	for i = 0 to l - 1 do
		let c = int_of_char (String.unsafe_get s !p) in
		a.a1 <- (a.a1 + c) mod 65521;
		a.a2 <- (a.a2 + a.a1) mod 65521;
		incr p;
	done

let adler32_read ch =
	let a2a = IO.read_byte ch in
	let a2b = IO.read_byte ch in
	let a1a = IO.read_byte ch in
	let a1b = IO.read_byte ch in
	{
		a1 = (a1a lsl 8) lor a1b;
		a2 = (a2a lsl 8) lor a2b;
	}

(* ************************************************************************ *)
(* WINDOW *)

let window_size = 1 lsl 15
let buffer_size = 1 lsl 16

let window_create size = {
		wbuffer = String.create buffer_size;
		wpos = 0;
		wcrc = adler32_create()
	}

let window_slide w =
	adler32_update w.wcrc w.wbuffer 0 window_size;
	let b = String.create buffer_size in
	w.wpos <- w.wpos - window_size;
	String.unsafe_blit w.wbuffer window_size b 0 w.wpos;
	w.wbuffer <- b

let window_add_string w s p len =
	if w.wpos + len > buffer_size then window_slide w;
	String.unsafe_blit s p w.wbuffer w.wpos len;
	w.wpos <- w.wpos + len

let window_add_char w c =
	if w.wpos = buffer_size then window_slide w;
	String.unsafe_set w.wbuffer w.wpos c;
	w.wpos <- w.wpos + 1

let window_get_last_char w =
	String.unsafe_get w.wbuffer (w.wpos - 1)

let window_available w =
	w.wpos

let window_checksum w =
	adler32_update w.wcrc w.wbuffer 0 w.wpos;
	w.wcrc

(* ************************************************************************ *)

let len_extra_bits_tbl = [|0;0;0;0;0;0;0;0;1;1;1;1;2;2;2;2;3;3;3;3;4;4;4;4;5;5;5;5;0;-1;-1|]
let len_base_val_tbl = [|3;4;5;6;7;8;9;10;11;13;15;17;19;23;27;31;35;43;51;59;67;83;99;115;131;163;195;227;258|]
let dist_extra_bits_tbl = [|0;0;0;0;1;1;2;2;3;3;4;4;5;5;6;6;7;7;8;8;9;9;10;10;11;11;12;12;13;13;-1;-1|]
let dist_base_val_tbl = [|1;2;3;4;5;7;9;13;17;25;33;49;65;97;129;193;257;385;513;769;1025;1537;2049;3073;4097;6145;8193;12289;16385;24577|]
let code_lengths_pos = [|16;17;18;0;8;7;9;6;10;5;11;4;12;3;13;2;14;1;15|]

let fixed_huffman = make_huffman (Array.init 288 (fun n ->
									if n <= 143 then 8
									else if n <= 255 then 9
									else if n <= 279 then 7
									else 8
								)) 0 288 10

let get_bits z n =
	while z.znbits < n do
		z.zbits <- z.zbits lor ((IO.read_byte z.zinput) lsl z.znbits);
		z.znbits <- z.znbits + 8;
	done;
	let b = z.zbits land (1 lsl n - 1) in
	z.znbits <- z.znbits - n;
	z.zbits <- z.zbits lsr n;
	b

let get_bit z =
	if z.znbits = 0 then begin
		z.znbits <- 8;
		z.zbits <- IO.read_byte z.zinput;
	end;
	let b = z.zbits land 1 = 1 in
	z.znbits <- z.znbits - 1;
	z.zbits <- z.zbits lsr 1;
	b

let rec get_rev_bits z n =
	if n = 0 then
		0
	else if get_bit z then
		(1 lsl (n - 1)) lor (get_rev_bits z (n-1))
	else
		get_rev_bits z (n-1)

let reset_bits z =
	z.zbits <- 0;
	z.znbits <- 0

let add_string z s p l =
	window_add_string z.zwindow s p l;
	String.unsafe_blit s p z.zoutput z.zoutpos l;
	z.zneeded <- z.zneeded - l;
	z.zoutpos <- z.zoutpos + l

let add_char z c =
	window_add_char z.zwindow c;
	String.unsafe_set z.zoutput z.zoutpos c;
	z.zneeded <- z.zneeded - 1;
	z.zoutpos <- z.zoutpos + 1

let add_dist_one z n =
	let c = window_get_last_char z.zwindow in
	let s = String.make n c in
	add_string z s 0 n

let add_dist z d l =
	add_string z z.zwindow.wbuffer (z.zwindow.wpos - d) l

let rec apply_huffman z = function
	| Found n -> n
	| NeedBit (a,b) -> apply_huffman z (if get_bit z then b else a)
	| NeedBits (n,t) -> apply_huffman z (Array.unsafe_get t (get_bits z n))

let inflate_lengths z a max =
	let i = ref 0 in
	let prev = ref 0 in
	while !i < max do
		match apply_huffman z z.zhuffman with
		| n when n <= 15 ->
			prev := n;
			Array.unsafe_set a !i n;
			incr i
		| 16 ->
			let n = 3 + get_bits z 2 in
			if !i + n > max then error Invalid_data;
			for k = 0 to n - 1 do
				Array.unsafe_set a !i !prev;
				incr i;
			done;
		| 17 ->
			let n = 3 + get_bits z 3 in
			i := !i + n;
			if !i > max then error Invalid_data;
		| 18 ->
			let n = 11 + get_bits z 7 in
			i := !i + n;
			if !i > max then error Invalid_data;
		| _ ->
			error Invalid_data
	done

let rec inflate_loop z =
	match z.zstate with
	| Head ->
		let cmf = IO.read_byte z.zinput in
		let cm = cmf land 15 in
		let cinfo = cmf lsr 4 in
		if cm <> 8 || cinfo <> 7 then error Invalid_data;
		let flg = IO.read_byte z.zinput in
		(*let fcheck = flg land 31 in*)
		let fdict = flg land 32 <> 0 in
		(*let flevel = flg lsr 6 in*)
		if (cmf lsl 8 + flg) mod 31 <> 0 then error Invalid_data;
		if fdict then error Unsupported_dictionary;
		z.zstate <- Block;
		inflate_loop z
	| Crc ->
		let calc = window_checksum z.zwindow in
		let crc = adler32_read z.zinput in
		if calc <> crc then error Invalid_crc;
		z.zstate <- Done;
		inflate_loop z
	| Done ->
		()
	| Block ->
		z.zfinal <- get_bit z;
		let btype = get_bits z 2 in
		(match btype with
		| 0 -> (* no compression *)
			z.zlen <- IO.read_ui16 z.zinput;
			let nlen = IO.read_ui16 z.zinput in
			if nlen <> 0xFFFF - z.zlen then error Invalid_data;
			z.zstate <- Flat;
			inflate_loop z;
			reset_bits z
		| 1 -> (* fixed Huffman *)
			z.zhuffman <- fixed_huffman;
			z.zhuffdist <- None;
			z.zstate <- CData;
			inflate_loop z
		| 2 -> (* dynamic Huffman *)
			let hlit = get_bits z 5 + 257 in
			let hdist = get_bits z 5 + 1 in
			let hclen = get_bits z 4 + 4 in
			for i = 0 to hclen - 1 do
				Array.unsafe_set z.zlengths (Array.unsafe_get code_lengths_pos i) (get_bits z 3);
			done;
			for i = hclen to 18 do
				Array.unsafe_set z.zlengths (Array.unsafe_get code_lengths_pos i) 0;
			done;
			z.zhuffman <- make_huffman z.zlengths 0 19 8;
			let lengths = Array.make (hlit + hdist) 0 in
			inflate_lengths z lengths (hlit + hdist);
			z.zhuffdist <- Some (make_huffman lengths hlit hdist 16);
			z.zhuffman <- make_huffman lengths 0 hlit 16;
			z.zstate <- CData;
			inflate_loop z
		| _ ->
			error Invalid_data)
	| Flat ->
		let rlen = min z.zlen z.zneeded in
		let str = IO.nread z.zinput rlen in
		let len = String.length str in
		z.zlen <- z.zlen - len;
		add_string z str 0 len;
		if z.zlen = 0 then z.zstate <- (if z.zfinal then Crc else Block);
		if z.zneeded > 0 then inflate_loop z
	| DistOne ->
		let len = min z.zlen z.zneeded in
		add_dist_one z len;
		z.zlen <- z.zlen - len;
		if z.zlen = 0 then z.zstate <- CData;
		if z.zneeded > 0 then inflate_loop z
	| Dist ->
		while z.zlen > 0 && z.zneeded > 0 do
			let len = min z.zneeded (min z.zlen z.zdist) in
			add_dist z z.zdist len;
			z.zlen <- z.zlen - len;
		done;
		if z.zlen = 0 then z.zstate <- CData;
		if z.zneeded > 0 then inflate_loop z
	| CData ->
		match apply_huffman z z.zhuffman with
		| n when n < 256 ->
			add_char z (Char.unsafe_chr n);
			if z.zneeded > 0 then inflate_loop z
		| 256 ->
			z.zstate <- if z.zfinal then Crc else Block;
			inflate_loop z
		| n ->
			let n = n - 257 in
			let extra_bits = Array.unsafe_get len_extra_bits_tbl n in
			if extra_bits = -1 then error Invalid_data;
			z.zlen <- (Array.unsafe_get len_base_val_tbl n) + (get_bits z extra_bits);
			let dist_code = (match z.zhuffdist with None -> get_rev_bits z 5 | Some h -> apply_huffman z h) in
			let extra_bits = Array.unsafe_get dist_extra_bits_tbl dist_code in
			if extra_bits = -1 then error Invalid_data;
			z.zdist <- (Array.unsafe_get dist_base_val_tbl dist_code) + (get_bits z extra_bits);
			if z.zdist > window_available z.zwindow then error Invalid_data;
			z.zstate <- (if z.zdist = 1 then DistOne else Dist);
			inflate_loop z

let inflate_data z s pos len =
	if pos < 0 || len < 0 || pos + len > String.length s then invalid_arg "inflate_data";
	z.zneeded <- len;
	z.zoutpos <- pos;
	z.zoutput <- s;
	try
		if len > 0 then inflate_loop z;
		len - z.zneeded
	with
		IO.No_more_input -> error Truncated_data

let inflate_init ?(header=true) ch =
	{
		zfinal = false;
		zhuffman = fixed_huffman;
		zhuffdist = None;
		zlen = 0;
		zdist = 0;
		zstate = (if header then Head else Block);
		zinput = ch;
		zbits = 0;
		znbits = 0;
		zneeded = 0;
		zoutput = "";
		zoutpos = 0;
		zlengths = Array.make 19 (-1);
		zwindow = window_create (1 lsl 15)
	}

let inflate ?(header=true) ch =
	let z = inflate_init ~header ch in
	let s = String.create 1 in
	IO.create_in
		~read:(fun() ->
			let l = inflate_data z s 0 1 in
			if l = 1 then String.unsafe_get s 0 else raise IO.No_more_input
		)
		~input:(fun s p l ->
			let n = inflate_data z s p l in
			if n = 0 then raise IO.No_more_input;
			n
		)
		~close:(fun () ->
			IO.close_in ch
		)
