open Swf
open ActionScript
open IO
open Printf

(* ************************************************************************ *)
(* TOOLS *)

let swf_version = ref 0
let exact_match = ref true

let const n = fun _ -> n

let opt_len f = function
	| None -> 0
	| Some x -> f x

let opt_flag flags fid f fparam =
	if (flags land fid) = 0 then
		None
	else
		Some (f fparam)

let opt f = function
	| None -> ()
	| Some x -> f x

let flag = function
	| None -> false
	| Some _ -> true

let rec make_flags = function
	| [] -> 0
	| true :: l -> 1 lor ((make_flags l) lsl 1)
	| false :: l -> (make_flags l) lsl 1

let f16_value (a,b) =
	let k = int_of_char a lor (int_of_char b lsl 8) in
	float_of_int k /. float_of_int (1 lsl 8)

(* ************************************************************************ *)
(* LENGTH *)

let _nbits x =
	if x < 0 then error "Negative nbits";
	if x = 0 then 
		0
	else
		let x = ref x in
		let nbits = ref 0 in
		while !x > 0 do
			x := !x lsr 1;
			incr nbits;
		done;
		!nbits

let rect_nbits r = 
	if !exact_match then
		r.rect_nbits
	else
		max
			(max (_nbits r.left) (_nbits r.right))
			(max (_nbits r.top) (_nbits r.bottom))

let rgba_nbits c =
	max
		(max (_nbits c.r) (_nbits c.g))
		(max (_nbits c.b) (_nbits c.a))

let cxa_nbits c =
	if !exact_match then
		c.cxa_nbits
	else
		max
			(opt_len rgba_nbits c.cxa_add)
			(opt_len rgba_nbits c.cxa_mult)

let matrix_part_nbits m = 
	if !exact_match then
		m.m_nbits
	else
		max (_nbits m.mx) (_nbits m.my)

let rgb_length = 3

let rect_length r =
	let nbits = rect_nbits r in
	let nbits = nbits * 4 + 5 in
	(nbits + 7) / 8

let matrix_length m =
	let matrix_part_len m = 5 + matrix_part_nbits m * 2 in
	let nbits = 2 + opt_len matrix_part_len m.scale + opt_len matrix_part_len m.rotate + matrix_part_len m.trans in
	(nbits + 7) / 8

let cxa_length c =
	let nbits = cxa_nbits c in
	let nbits = 6 + opt_len (const (nbits * 4)) c.cxa_add + opt_len (const (nbits * 4)) c.cxa_mult in
	(nbits + 7) / 8

let clip_event_length (_,s) =
	if !swf_version >= 6 then 4 + 4 + String.length s else 2 + 4 + String.length s

let clip_events_length l =
	List.fold_left (fun acc evt -> acc + clip_event_length evt) (if !swf_version >= 6 then 10 else 6) l

let export_length e =
	2 + String.length e.exp_name + 1

let rec tag_data_length = function
	| TEnd ->
		0
	| TShowFrame ->
		0
	| TShape data ->
		String.length data
	| TRemoveObject _ ->
		4
	| TBitsJPEG data ->
		String.length data
	| TJPEGTables tab ->
		String.length tab
	| TSetBgColor _ ->
		rgb_length
	| TText s ->
		String.length s
	| TDoAction acts ->
		actions_length acts
	| TSound data ->
		String.length data
	| TBitsLossless data ->
		String.length data
	| TBitsJPEG2 data ->
		String.length data
	| TShape2 data ->
		String.length data
	| TProtect ->
		0
	| TPlaceObject2 p ->
		3
		+ 0 (* po_move *)
		+ opt_len (const 2) p.po_cid
		+ opt_len matrix_length p.po_matrix
		+ opt_len cxa_length p.po_color
		+ opt_len (const 2) p.po_ratio
		+ opt_len (fun s -> String.length s + 1) p.po_inst_name
		+ opt_len (const 2) p.po_clip_depth
		+ opt_len clip_events_length p.po_events
	| TRemoveObject2 _ ->
		2
	| TShape3 data ->
		String.length data
	| TButton2 data ->
		String.length data
	| TBitsJPEG3 data ->
		String.length data
	| TBitsLossless2 data ->
		String.length data
	| TEditText data ->
		String.length data
	| TClip c ->
		List.fold_left (fun acc t -> acc + tag_length t) 4 (TEnd :: c.c_tags)
	| TFrameLabel label ->
		String.length label + 1
	| TSoundStreamHead2 data ->
		String.length data
	| TMorphShape data ->
		String.length data
	| TFont2 data ->
		String.length data
	| TExport el ->
		List.fold_left (fun acc e -> acc + export_length e) 2 el
	| TDoInitAction i ->
		2 + actions_length i.dia_actions
	| TUnknown (_,data) ->
		String.length data
	| TExtended t ->
		tag_data_length t

and tag_length t = 
	let dlen = tag_data_length t in
	let extended = (match t with TExtended _ -> true | _ -> dlen >= 63) in
	dlen + 2 + (if extended then 4 else 0)

(* ************************************************************************ *)
(* READ PRIMS *)

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
		let k = read_byte b.b_ch in
		b.b_data <- (b.b_data lsl 8) lor k;
		b.b_count <- b.b_count + 8;
		read_bits b n
	end

let read_rgb ch =
	let r = read_byte ch in
	let g = read_byte ch in
	let b = read_byte ch in
	{
		cr = r;
		cg = g;
		cb = b;
	}

let read_rect ch =
	let b = init_bits ch in
	let nbits = read_bits b 5 in
	let left = read_bits b nbits in
	let right = read_bits b nbits in
	let top = read_bits b nbits in
	let bottom = read_bits b nbits in
	{
		rect_nbits = nbits;
		left = left;
		right = right;
		top = top;
		bottom = bottom;
	}

let read_matrix ch =
	let b = init_bits ch in
	let read_matrix_part() =
		let nbits = read_bits b 5 in
		let x = read_bits b nbits in
		let y = read_bits b nbits in
		{
			m_nbits = nbits;
			mx = x;
			my = y;
		}
	in
	let has_scale = (read_bits b 1 = 1) in
	let scale = (if has_scale then Some (read_matrix_part()) else None) in
	let has_rotate = (read_bits b 1 = 1) in
	let rotate = (if has_rotate then Some (read_matrix_part()) else None) in
	let trans = read_matrix_part() in
	{
		scale = scale;
		rotate = rotate;
		trans = trans;
	}	

let read_cxa ch =
	let b = init_bits ch in
	let has_add = (read_bits b 1 = 1) in
	let has_mult = (read_bits b 1 = 1) in
	let nbits = read_bits b 4 in
	let read_cxa_color() =
		let r = read_bits b nbits in
		let g = read_bits b nbits in
		let bl = read_bits b nbits in
		let a = read_bits b nbits in
		{
			r = r;
			g = g;
			b = bl;
			a = a;
		}
	in
	let mult = (if has_mult then Some (read_cxa_color()) else None) in
	let add = (if has_add then Some (read_cxa_color()) else None) in
	{
		cxa_nbits = nbits;
		cxa_add = add;
		cxa_mult = mult;
	}

let read_event ch =
	(if !swf_version >= 6 then read_ui32 else read_ui16) ch

(* ************************************************************************ *)
(* WRITE PRIMS *)

let rec write_bits b n x =
	if n + b.b_count >= 32 then error "Write bits count overflow";
	if x >= 1 lsl n then error "Write bits value overflow";
	b.b_data <- (b.b_data lsl n) lor x;
	b.b_count <- b.b_count + n;
	while b.b_count >= 8 do
		b.b_count <- b.b_count - 8;
		write_byte b.b_ch (b.b_data asr b.b_count)
	done

let flush_bits b =
	if b.b_count > 0 then write_bits b (8 - b.b_count) 0

let write_rgb ch c =
	write_byte ch c.cr;
	write_byte ch c.cg;
	write_byte ch c.cb

let write_rect ch r =
	let b = init_bits ch in
	let nbits = rect_nbits r in
	write_bits b 5 nbits;
	write_bits b nbits r.left;
	write_bits b nbits r.right;
	write_bits b nbits r.top;
	write_bits b nbits r.bottom;
	flush_bits b

let write_matrix ch m =
	let b = init_bits ch in
	let write_matrix_part m =
		let nbits = matrix_part_nbits m in
		write_bits b 5 nbits;
		write_bits b nbits m.mx;
		write_bits b nbits m.my;
	in
	(match m.scale with
	| None ->
		write_bits b 1 0
	| Some s ->
		write_bits b 1 1;
		write_matrix_part s
	);
	(match m.rotate with
	| None ->
		write_bits b 1 0
	| Some r ->
		write_bits b 1 1;
		write_matrix_part r);
	write_matrix_part m.trans;
	flush_bits b

let write_cxa ch c =
	let b = init_bits ch in
	let nbits = cxa_nbits c in
	(match c.cxa_add , c.cxa_mult with
	| None , None ->
		write_bits b 2 0;
		write_bits b 4 1; (* some strange MM thing... *)
	| Some c , None -> 
		write_bits b 2 2;
		write_bits b 4 nbits;
		List.iter (write_bits b nbits) [c.r;c.g;c.b;c.a];
	| None , Some c -> 
		write_bits b 2 1;
		write_bits b 4 nbits;
		List.iter (write_bits b nbits) [c.r;c.g;c.b;c.a];
	| Some c1 , Some c2 -> 
		write_bits b 2 3;
		write_bits b 4 nbits;
		List.iter (write_bits b nbits) [c2.r;c2.g;c2.b;c2.a;c1.r;c1.g;c1.b;c1.a]
	);
	flush_bits b

let write_event ch evt =
	(if !swf_version >= 6 then write_ui32 else write_ui16) ch evt

(* ************************************************************************ *)
(* PARSING *)

let parse_clip_events ch =
	let reserved = read_ui16 ch in
	let all_events = read_event ch in
	let rec loop() =
		let events = read_event ch in
		if events = 0 then
			[]
		else
			let len = read_ui32 ch in
			let s = nread ch len in
			(events , s) :: (loop())
	in
	loop()

let rec parse_tag ch =
	let h = read_ui16 ch in
	let id = h lsr 6 in
	let len = h land 63 in
	let len , extended = (
		if len = 63 then 
			let len = read_ui32 ch in
			len , len < 63
		else 
			len , false
	) in	
	let tag = (
		match id with
		| 0x00 ->
			TEnd
		| 0x01 ->
			TShowFrame
		| 0x02 ->
			TShape (nread ch len)
		(*//0x04 TPlaceObject *)
		| 0x05 ->
			let cid = read_ui16 ch in
			let depth = read_ui16 ch in
			TRemoveObject (cid,depth)
		| 0x06 ->
			TBitsJPEG (nread ch len)
		(*//0x07 TButton *)
		| 0x08 ->
			TJPEGTables (nread ch len)
		| 0x09 ->
			TSetBgColor (read_rgb ch)
		(*//0x0A TFont *)
		| 0x0B ->
			TText (nread ch len)
		| 0x0C ->
			TDoAction (parse_actions ch)
		(*//0x0D TFontInfo *)
		| 0x0E ->
			TSound (nread ch len)
		(*//0x0F TStartSound *)
		(*//0x11 TButtonSound *)
		(*//0x12 TSoundStreamHead *)
		(*//0x13 TSoundStreamBlock *)
		| 0x14 ->
			TBitsLossless (nread ch len)
		| 0x15 ->
			TBitsJPEG2 (nread ch len)
		| 0x16 ->
			TShape2 (nread ch len)
		(*//0x17 TButtonCXForm *)
		| 0x18 ->
			TProtect
		| 0x1A ->
			let f = read_byte ch in
			let depth = read_ui16 ch in
			let move = (f land 1) <> 0 in
			let cid = opt_flag f 2 read_ui16 ch in
			let matrix = opt_flag f 4 read_matrix ch in
			let color = opt_flag f 8 read_cxa ch in
			let ratio = opt_flag f 16 read_ui16 ch in
			let name = opt_flag f 32 read_string ch in
			let clip_depth = opt_flag f 64 read_ui16 ch in
			let clip_events = opt_flag f 128 parse_clip_events ch in
			TPlaceObject2 {
				po_depth = depth;
				po_move = move;
				po_cid = cid;
				po_matrix = matrix;
				po_color = color;
				po_ratio = ratio;
				po_inst_name = name;
				po_clip_depth = clip_depth;
				po_events = clip_events;
			}
		| 0x1C ->
			let depth = read_ui16 ch in
			TRemoveObject2 depth
		| 0x20 ->
			TShape3 (nread ch len)
		(*//0x21 TText2 *)
		| 0x22 ->
			TButton2 (nread ch len)
		| 0x23 ->
			TBitsJPEG3 (nread ch len)
		| 0x24 ->
			TBitsLossless2 (nread ch len)
		| 0x25 ->
			TEditText (nread ch len)
		| 0x27 ->
			let cid = read_ui16 ch in
			let fcount = read_ui16 ch in
			let tags = parse_tag_list ch in
			TClip {
				c_id = cid;
				c_frame_count = fcount;
				c_tags = tags;
			}
		| 0x2B ->
			let label = read_string ch in
			TFrameLabel label
		| 0x2D ->
			TSoundStreamHead2 (nread ch len)		
		| 0x2E ->
			TMorphShape (nread ch len)
		| 0x30 ->
			TFont2 (nread ch len)
		| 0x38 ->
			let rec loop n =
				if n = 0 then
					[]
				else
					let cid = read_ui16 ch in
					let name = read_string ch in
					{
						exp_id = cid;
						exp_name = name
					} :: loop (n-1)
			in
			TExport (loop (read_ui16 ch))
		(*// 0x39 TImport *)
		(*// 0x3A TEnableDebugger *)
		| 0x3B ->
			let cid = read_ui16 ch in
			let actions = parse_actions ch in
			TDoInitAction {
				dia_id = cid;
				dia_actions = actions;
			}
		(*// 0x3C TVideoStream *)
		(*// 0x3D TVideoFrame *)
		(*// 0x3E TFontInfo2 *)
		(*// 0x40 TEnableDebugger2 *)
		(*// 0x41 TScriptLimits *)
		(*// 0x42 TSetTabIndex *)
		| _ ->
			printf "Unknown tag 0x%.2X\n" id;
			TUnknown (id,nread ch len)
	) in
	let t = (if extended then
			TExtended tag
		else
			tag)
	in
	let len2 = tag_data_length t in
	if len <> len2 then error (sprintf "Datalen mismatch for tag 0x%.2X (%d != %d)" id len len2);
	t

and parse_tag_list ch =
	let rec loop acc =
		match parse_tag ch with
		| TEnd -> List.rev acc
		| t -> loop (t :: acc)
	in
	loop []

let parse ch =
	let sign = nread ch 3 in
	(* TODO : compression *)
	if sign <> "FWS" && sign <> "CWS" then error "Invalid SWF signature";
	let ver = read_byte ch in
	swf_version := ver;
	let file_len = read_ui32 ch in
	let compressed, ch = (if sign = "CWS" then true , inflate ch else false, ch) in
	let size = read_rect ch in
	let fps = read_ui16 ch in
	let frame_count = read_ui16 ch in
	let h = {
		h_version = ver;
		h_size = size;
		h_fps = fps;
		h_frame_count = frame_count;
		h_compressed = compressed;
	} in
	h , parse_tag_list ch

(* ************************************************************************ *)
(* WRITING *)

let rec tag_id = function
	| TEnd -> 0x00
	| TShowFrame -> 0x01
	| TShape _ -> 0x02
	| TRemoveObject _ -> 0x05
	| TBitsJPEG _ -> 0x06
	| TJPEGTables _ -> 0x08
	| TSetBgColor _ -> 0x09
	| TText _ -> 0x0B
	| TDoAction _ -> 0x0C
	| TSound _ -> 0x0E
	| TBitsLossless _ -> 0x14
	| TBitsJPEG2 _ -> 0x15
	| TShape2 _ -> 0x16
	| TProtect -> 0x18
	| TPlaceObject2 _ -> 0x1A
	| TRemoveObject2 _ -> 0x1C
	| TShape3 _ -> 0x20
	| TButton2 _ -> 0x22
	| TBitsJPEG3 _ -> 0x23
	| TBitsLossless2 _ -> 0x24
	| TEditText _ -> 0x25
	| TClip _ -> 0x27
	| TFrameLabel _ -> 0x2B
	| TSoundStreamHead2 _ -> 0x2D
	| TMorphShape _ -> 0x2E
	| TFont2 _ -> 0x30
	| TExport _ -> 0x38
	| TDoInitAction _ -> 0x3B
	| TUnknown (id,_) -> id
	| TExtended t -> tag_id t

let write_clip_event ch (id,data) =
	write_event ch id;
	write_ui32 ch (String.length data);
	nwrite ch data

let write_clip_events ch event_list =
 	write_ui16 ch 0;
	let all_events = List.fold_left (fun acc (id,_) -> acc lor id) 0 event_list in
	write_event ch all_events;
	List.iter (write_clip_event ch) event_list;
	write_event ch 0

let rec write_tag_data ch = function
	| TEnd ->
		()		
	| TShowFrame ->
		()
	| TShape data ->
		nwrite ch data
	| TRemoveObject (cid,depth) ->
		write_ui16 ch cid;
		write_ui16 ch depth;
	| TBitsJPEG data ->
		nwrite ch data
	| TJPEGTables tab ->
		nwrite ch tab
	| TSetBgColor c ->
		write_rgb ch c
	| TText data ->
		nwrite ch data
	| TDoAction acts ->
		write_actions ch acts
	| TSound data ->
		nwrite ch data
	| TBitsLossless data ->
		nwrite ch data
	| TBitsJPEG2 data ->
		nwrite ch data
	| TShape2 data ->
		nwrite ch data
	| TProtect -> 
		()
	| TPlaceObject2 p ->
		write_byte ch (make_flags [
			p.po_move;
			flag p.po_cid;
			flag p.po_matrix;
			flag p.po_color;
			flag p.po_ratio;
			flag p.po_inst_name;
			flag p.po_clip_depth;
			flag p.po_events
		]);
		write_ui16 ch p.po_depth;
		opt (write_ui16 ch) p.po_cid;
		opt (write_matrix ch) p.po_matrix;
		opt (write_cxa ch) p.po_color;
		opt (write_ui16 ch) p.po_ratio;
		opt (write_string ch) p.po_inst_name;
		opt (write_ui16 ch) p.po_clip_depth;
		opt (write_clip_events ch) p.po_events;
	| TRemoveObject2 depth ->
		write_ui16 ch depth;
	| TShape3 data -> 
		nwrite ch data
	| TButton2 data ->
		nwrite ch data
	| TBitsJPEG3 data ->
		nwrite ch data
	| TBitsLossless2 data ->
		nwrite ch data
	| TEditText data ->
		nwrite ch data
	| TClip c ->
		write_ui16 ch c.c_id;
		write_ui16 ch c.c_frame_count;
		List.iter (write_tag ch) c.c_tags;
		write_tag ch TEnd
	| TFrameLabel label ->
		write_string ch label
	| TSoundStreamHead2 data ->
		nwrite ch data
	| TMorphShape data ->
		nwrite ch data
	| TFont2 data ->
		nwrite ch data
	| TExport el ->
		write_ui16 ch (List.length el);
		List.iter (fun e ->
			write_ui16 ch e.exp_id;
			write_string ch e.exp_name
		) el
	| TDoInitAction i ->
		write_ui16 ch i.dia_id;
		write_actions ch i.dia_actions;
	| TUnknown (_,data) ->
		nwrite ch data
	| TExtended t ->
		write_tag_data ch t

and write_tag ch t =
	let id = tag_id t in
	let dlen = tag_data_length t in
	let extended = (match t with TExtended _ -> true | _ -> dlen >= 63) in
	if extended then begin
		write_ui16 ch ((id lsl 6) lor 63);
		write_ui32 ch dlen;
	end else begin
		write_ui16 ch ((id lsl 6) lor dlen);
	end;
	write_tag_data ch t

let write ch (h,tags) =
	swf_version := h.h_version;
	nwrite ch (if h.h_compressed then "CWS" else "FWS");
	write ch (char_of_int h.h_version);
	let rec calc_len = function
		| [] -> tag_length TEnd
		| t :: l -> 
			tag_length t + calc_len l
	in
	let len = calc_len tags in
	let old_exact_match = !exact_match in
	exact_match := true;
	let len = len + 4 + 4 + rect_length h.h_size + 2 + 2 in
	write_ui32 ch len;
	let ch = (if h.h_compressed then deflate ch else ch) in
	write_rect ch h.h_size;
	write_ui16 ch h.h_fps;
	write_ui16 ch h.h_frame_count;
	exact_match := old_exact_match;
	List.iter (write_tag ch) tags;
	write_tag ch TEnd;
	flush ch

;;
Swf.__parser := parse;
Swf.__printer := write