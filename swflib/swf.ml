open Printf

(* ************************************************************************ *)
(* TYPES *)

type 'a bits = {
	mutable b_data : int;
	mutable b_count : int;
	b_ch : 'a;
}

type float16 = char * char

type rect = {
	left : int;
	right : int;
	top : int; 
	bottom : int;
}

type header = {
	h_version : char;
	h_size : rect;
	h_fps : float16;
	h_frame_count : int; 
}

type tag =
	| TEnd
	| TUnknown of int * string
	| TExtended of tag

type swf = header * tag list

(* ************************************************************************ *)
(* TOOLS *)

exception Error of string

let error msg = raise (Error msg)

let nbits x =
	if x < 0 then error "Negative nbits";
	if x = 0 then 
		1
	else
		let x = ref x in
		let nbits = ref 1 in
		while !x > 0 do
			x := !x lsr 1;
			incr nbits;
		done;
		!nbits

let rect_nbits r =
	let n = max (nbits r.left) (nbits r.right) in
	let n = max n (max (nbits r.bottom) (nbits r.top)) in
	if n >= 1 lsl 5 then error "Rect nbits overflow";
	n

let rect_length r =
	let n = rect_nbits r in
	((n * 4 + 5) + 7) / 8

let f16_value (a,b) =
	let k = int_of_char a lor (int_of_char b lsl 8) in
	float_of_int k /. float_of_int (1 lsl 8)

(* ************************************************************************ *)
(* READ PRIMS *)

let read ch n =
	let s = String.create n in
	really_input ch s 0 n;
	s

let read_char ch =
	input_char ch

let init_bits ch =
	{
		b_data = 0;
		b_count = 0;
		b_ch = ch;
	}

let skip ch n =
	seek_in ch ((pos_in ch) + n)

let rec read_bits b n =
	if b.b_count >= n then begin
		let c = b.b_count - n in
		let k = (b.b_data asr c) land ((1 lsl n) - 1) in
		b.b_count <- c;
		k
	end else begin
		if b.b_count >= 24 then error "Bits overflow";
		let k = input_byte b.b_ch in
		b.b_data <- (b.b_data lsl 8) lor k;
		b.b_count <- b.b_count + 8;
		read_bits b n
	end

let read_rect ch =
	let b = init_bits ch in
	let nbits = read_bits b 5 in
	let left = read_bits b nbits in
	let right = read_bits b nbits in
	let top = read_bits b nbits in
	let bottom = read_bits b nbits in
	{
		left = left;
		right = right;
		top = top;
		bottom = bottom;
	}

let read_f16 ch =
	let ch1 = input_char ch in
	let ch2 = input_char ch in
	ch1 , ch2

let read_ui16 ch =
	let ch1 = input_byte ch in
	let ch2 = input_byte ch in
	ch1 lor (ch2 lsl 8)

let read_i32 ch =
	let ch1 = input_byte ch in
	let ch2 = input_byte ch in
	let ch3 = input_byte ch in
	let ch4 = input_byte ch in
	ch1 lor (ch2 lsl 8) lor (ch3 lsl 16) lor (ch4 lsl 24)

(* ************************************************************************ *)
(* WRITE PRIMS *)

let rec write_bits b n x =
	if n + b.b_count >= 32 then error "Write bits overflow";
	b.b_data <- (b.b_data lsl n) lor x;
	b.b_count <- b.b_count + n;
	while b.b_count >= 8 do
		b.b_count <- b.b_count - 8;
		output_byte b.b_ch (b.b_data asr b.b_count)
	done

let flush_bits b =
	if b.b_count > 0 then write_bits b (8 - b.b_count) 0

let write_i32 ch n =
	output_byte ch n;
	output_byte ch (n lsr 8);
	output_byte ch (n lsr 16);
	output_byte ch (n asr 24)

let write_rect ch r =
	let b = init_bits ch in
	let nbits = rect_nbits r in
	write_bits b 5 nbits;
	write_bits b nbits r.left;
	write_bits b nbits r.right;
	write_bits b nbits r.top;
	write_bits b nbits r.bottom;
	flush_bits b

let write_f16 ch (a,b) =
	output_char ch a;
	output_char ch b

let write_ui16 ch n =
	if n < 0 || n > 0xFFFF then error "Write UI16 overflow";
	output_byte ch n;
	output_byte ch (n lsr 8)

(* ************************************************************************ *)
(* PARSING *)

let parse_tag ch =
	let h = read_ui16 ch in
	let id = h lsr 6 in
	let len = h land 63 in
	let len , extended = (
		if len = 63 then 
			let len = read_i32 ch in
			len , len < 63
		else 
			len , false
	) in
	let tag = (
		match id with
		| 0 ->
			assert (len = 0);
			TEnd
		| _ ->
			printf "Unknown tag 0x%.2X\n" id;
			TUnknown (id,read ch len)
	) in
	if extended then
		TExtended tag
	else
		tag

let parse ch =
	let sign = read ch 3 in
	(* TODO : compression *)
	if sign <> "FWS" then error "Invalid SWF signature";
	let ver = read_char ch in
	let file_len = read_i32 ch in
	let size = read_rect ch in
	let fps = read_f16 ch in
	let frame_count = read_ui16 ch in
	let h = {
		h_version = ver;
		h_size = size;
		h_fps = fps;
		h_frame_count = frame_count;
	} in
	let rec tags acc =
		match parse_tag ch with
		| TEnd -> List.rev acc
		| t -> tags (t :: acc)
	in
	h , (tags [])

(* ************************************************************************ *)
(* WRITING *)

let rec tag_data_len = function
	| TEnd -> 0
	| TExtended t -> tag_data_len t
	| TUnknown (_,data) -> String.length data

let rec tag_id = function
	| TEnd -> 0
	| TExtended t -> tag_id t
	| TUnknown (id,_) -> id

let tag_len t = 
	let dlen = tag_data_len t in
	let extended = (match t with TExtended _ -> true | _ -> dlen >= 63) in
	dlen + 2 + (if extended then 4 else 0)

let rec write_tag_data ch = function
	| TEnd -> ()
	| TExtended t -> write_tag_data ch t
	| TUnknown (_,data) -> output_string ch data

let write_tag ch t =
	let id = tag_id t in
	let dlen = tag_data_len t in
	let extended = (match t with TExtended _ -> true | _ -> dlen >= 63) in
	if extended then begin
		write_ui16 ch ((id lsl 6) lor 63);
		write_i32 ch dlen;
	end else begin
		write_ui16 ch ((id lsl 6) lor dlen);
	end;
	write_tag_data ch t

let write ch (h,tags) =
	output_string ch "FWS";
	output_char ch h.h_version;
	let rec calc_len = function
		| [] -> tag_len TEnd
		| t :: l -> 
			tag_len t + calc_len l
	in
	let len = 4 + 4 + rect_length h.h_size + 2 + 2 in
	let len = len + calc_len tags in
	write_i32 ch len;
	write_rect ch h.h_size;
	write_f16 ch h.h_fps;
	write_ui16 ch h.h_frame_count;
	List.iter (write_tag ch) tags;
	write_tag ch TEnd

;;
let swf = parse (open_in_bin "test.swf") in
let out = open_out_bin "test2.swf" in
write out swf;
close_out out;
