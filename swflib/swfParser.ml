open Swf
open ActionScript
open IO

(* ************************************************************************ *)
(* TOOLS *)

type 'a bits = {
	mutable b_data : int;
	mutable b_count : int;
	b_ch : 'a;
}

let swf_version = ref 0
let id_count = ref 0
let exact_match = ref true
let tag_end = { tid = 0; textended = false; tdata = TEnd }

let sum f l =
	List.fold_left (fun acc x -> acc + f x) 0 l

let gen_id() =
	incr id_count;
	!id_count

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

let rgba_length = 4

let color_length = function
	| ColorRGB _ -> rgb_length
	| ColorRGBA _ -> rgba_length

let rect_length r =
	let nbits = rect_nbits r in
	let nbits = nbits * 4 + 5 in
	(nbits + 7) / 8

let gradient_length = function
	| GradientRGB l -> 1 + (1 + rgb_length) * List.length l
	| GradientRGBA l -> 1 + (1 + rgba_length) * List.length l

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
	(if !swf_version >= 6 then 10 else 6) + sum clip_event_length l

let export_length e =
	2 + String.length e.exp_name + 1

let sound_length s =
	2 + 1 + 4 + String.length s.so_data

let shape_fill_style_length s =
	1 + match s with
	| SFSSolid _ -> rgb_length
	| SFSSolid3 _ -> rgba_length
	| SFSLinearGradient (m,g)
	| SFSRadialGradient (m,g) -> matrix_length m + gradient_length g
	| SFSBitmap b -> 2 + matrix_length b.sfb_mpos

let shape_fill_styles_length s =
	let n = List.length s in
	(if n < 0xFF then 1 else 3) + sum shape_fill_style_length s

let shape_with_style_length s =
	shape_fill_styles_length s.sws_fill_styles + String.length s.sws_data

let shape_length s =
	2 + rect_length s.sh_bounds + shape_with_style_length s.sh_style

let bitmap_length b =
	2 + String.length b.bmp_data

let bitmap_lossless_length b =
	2 + 1 + 2 + 2 + String.length b.bll_data

let morph_shape_length s =
	2 + rect_length s.msh_start_bounds + rect_length s.msh_end_bounds + String.length s.msh_data
	
let text_record_length t r =
	1 + opt_len (const 4) r.txr_font + 
		opt_len color_length r.txr_color + 
		opt_len (const 2) r.txr_dx + 
		opt_len (const 2) r.txr_dy + 
		1 + ((((t.txt_ngbits + t.txt_nabits) * List.length r.txr_glyphs) + 7) / 8)

let text_length t =
	2 + rect_length t.txt_bounds + matrix_length t.txt_matrix + 2 + sum (text_record_length t) t.txt_records + 1

let button_record_length r =
	1 + 2 + 2 + matrix_length r.btr_mpos + (match r.btr_color with None -> 0 | Some c -> cxa_length c)

let button_action_length r =
	2 + 2 + actions_length r.bta_actions
	
let button2_length b =
	2 + 1 + 2 + 
		1 + sum button_record_length b.bt2_records + 
		sum button_action_length b.bt2_actions

let font2_length f =
	2 + String.length f.ft2_data

let edit_text_layout_length = 9

let edit_text_length t =
	2 + rect_length t.edt_bounds + 2 + 
		opt_len (const 4) t.edt_font + 
		opt_len (const rgba_length) t.edt_color +
		opt_len (const 2) t.edt_maxlen +
		opt_len (const edit_text_layout_length) t.edt_layout +
		String.length t.edt_variable + 1 +
		opt_len (fun s -> String.length s + 1) t.edt_text

let rec tag_data_length = function
	| TEnd ->
		0
	| TShowFrame ->
		0
	| TShape s ->
		shape_length s
	| TRemoveObject _ ->
		4
	| TBitsJPEG b ->
		bitmap_length b
	| TJPEGTables tab ->
		String.length tab
	| TSetBgColor _ ->
		rgb_length
	| TText t ->
		text_length t
	| TDoAction acts ->
		actions_length acts
	| TSound s ->
		sound_length s
	| TStartSound s ->
		2 + String.length s.sts_data
	| TBitsLossless b ->
		bitmap_lossless_length b
	| TBitsJPEG2 b ->
		bitmap_length b
	| TShape2 s ->
		shape_length s
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
	| TShape3 s ->
		shape_length s
	| TButton2 b ->
		button2_length b
	| TBitsJPEG3 b ->
		2 + 4 + String.length b.jp3_alpha_data + String.length b.jp3_data
	| TBitsLossless2 b ->
		bitmap_lossless_length b
	| TEditText t ->
		edit_text_length t
	| TClip c ->
		4 + sum tag_length (tag_end :: c.c_tags)
	| TFrameLabel label ->
		String.length label + 1
	| TSoundStreamHead2 data ->
		String.length data
	| TMorphShape s ->
		morph_shape_length s
	| TFont2 f ->
		font2_length f
	| TExport el ->
		2 + sum export_length el
	| TDoInitAction i ->
		2 + actions_length i.dia_actions
	| TUnknown (_,data) ->
		String.length data

and tag_length t = 
	let dlen = tag_data_length t.tdata in
	dlen + 2 + (if t.textended || dlen >= 63 then 4 else 0)

(* ************************************************************************ *)
(* READ PRIMS *)

let init_bits ch =
	{
		b_data = 0;
		b_count = 0;
		b_ch = ch;
	}

let skip ch n =
	seek_in ch ((Pervasives.pos_in ch) + n)

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

let read_rgba ch =
	let r = read_byte ch in
	let g = read_byte ch in
	let b = read_byte ch in
	let a = read_byte ch in
	{
		r = r;
		g = g;
		b = b;
		a = a;
	}

let read_rgb ch =
	let r = read_byte ch in
	let g = read_byte ch in
	let b = read_byte ch in
	{
		cr = r;
		cg = g;
		cb = b;
	}

let read_gradient ch is_rgba =
	let rec loop_rgb n =
		if n = 0 then
			[]
		else
			let r = read_byte ch in
			let c = read_rgb ch in
			(r, c) :: loop_rgb (n-1)
	in
	let rec loop_rgba n =
		if n = 0 then
			[]
		else
			let r = read_byte ch in
			let c = read_rgba ch in
			(r, c) :: loop_rgba (n-1)
	in
	let n = read_byte ch in
	if is_rgba then
		GradientRGBA (loop_rgba n)
	else
		GradientRGB (loop_rgb n)

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
	(if !swf_version >= 6 then read_i32 else read_ui16) ch

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

let write_rgba ch c =
	write_byte ch c.r;
	write_byte ch c.g;
	write_byte ch c.b;
	write_byte ch c.a

let write_color ch = function
	| ColorRGB c -> write_rgb ch c
	| ColorRGBA c -> write_rgba ch c

let write_gradient ch = function
	| GradientRGB l ->
		let n = List.length l in
		write_byte ch n;
		List.iter (fun (ratio,c) -> write_byte ch ratio; write_rgb ch c) l
	| GradientRGBA l ->
		let n = List.length l in
		write_byte ch n;
		List.iter (fun (ratio,c) -> write_byte ch ratio; write_rgba ch c) l

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
	(if !swf_version >= 6 then write_i32 else write_ui16) ch evt

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
			let len = read_i32 ch in
			let s = nread ch len in
			(events , s) :: (loop())
	in
	loop()

let parse_shape_fill_style ch is_shape3 =
	let t = read_byte ch in
	match t with
	| 0x00 when is_shape3 -> SFSSolid3 (read_rgba ch)
	| 0x00 -> SFSSolid (read_rgb ch)
	| 0x10 -> 
		let m = read_matrix ch in
		let g = read_gradient ch is_shape3 in
		SFSLinearGradient (m,g)
	| 0x12 ->
		let m = read_matrix ch in
		let g = read_gradient ch is_shape3 in
		SFSRadialGradient (m,g)
	| 0x40
	| 0x41
	| 0x42
	| 0x43 ->
		let id = read_ui16 ch in
		let m = read_matrix ch in
		SFSBitmap {
			sfb_repeat = (t = 0x40 || t = 0x42);
			sfb_smooth = (t = 0x42 || t = 0x43);
			sfb_cid = id;
			sfb_mpos = m;
		}
	| _ ->
		assert false

let parse_shape_fill_styles ch is_shape3 =
	let rec loop n =
		if n = 0 then
			[]
		else
			let s = parse_shape_fill_style ch is_shape3 in
			s :: loop (n-1)
	in
	let n = (match read_byte ch with 0xFF -> read_ui16 ch | n -> n) in	
	loop n

let parse_shape_with_style ch len is_shape3 =
	let fstyles = parse_shape_fill_styles ch is_shape3 in
	let data = nread ch (len - shape_fill_styles_length fstyles) in
	{
		sws_fill_styles = fstyles;
		sws_data = data;
	}
		

let parse_shape ch len is_shape3 =
	let id = read_ui16 ch in
	let bounds = read_rect ch in
	let style = parse_shape_with_style ch (len - 2 - rect_length bounds) is_shape3 in
	{
		sh_id = id;
		sh_bounds = bounds;
		sh_style = style;
	}

let parse_bitmap ch len =
	let id = read_ui16 ch in
	let data = nread ch (len - 2) in
	{
		bmp_id = id;
		bmp_data = data;
	}

let parse_bitmap_lossless ch len =
	let id = read_ui16 ch in
	let format = read_byte ch in
	let width = read_ui16 ch in
	let height = read_ui16 ch in
	let data = nread ch (len - 7) in
	{
		bll_id = id;
		bll_format = format;
		bll_width = width;
		bll_height = height;
		bll_data = data;
	}

let parse_text ch is_txt2 =
	let id = read_ui16 ch in
	let bounds = read_rect ch in
	let matrix = read_matrix ch in
	let ngbits = read_byte ch in
	let nabits = read_byte ch in
	let rec loop_glyphs bits n =
		if n = 0 then
			[]
		else
			let indx = read_bits bits ngbits in
			let adv = read_bits bits nabits in
			let g = {
				txg_index = indx;
				txg_advanced = adv;
			} in
			g :: loop_glyphs bits (n-1)
	in		
	let rec loop() =
		let flags = read_byte ch in
		if flags = 0 then
			[]
		else
			let font_id = (if flags land 8 <> 0 then read_ui16 ch else 0) in
			let color = (if flags land 4 <> 0 then Some (if is_txt2 then ColorRGBA (read_rgba ch) else ColorRGB (read_rgb ch)) else None) in
			let dx = (if flags land 1 <> 0 then Some (read_i16 ch) else None) in
			let dy = (if flags land 2 <> 0 then Some (read_i16 ch) else None) in
			let font = (if flags land 8 <> 0 then Some (font_id,read_ui16 ch) else None) in
			let nglyphs = read_byte ch in
			let r = {
				txr_font = font;
				txr_color = color;
				txr_dx = dx;
				txr_dy = dy;
				txr_glyphs = loop_glyphs (init_bits ch) nglyphs;
			} in
			r :: loop()
	in
	{
		txt_id = id;
		txt_bounds = bounds;
		txt_matrix = matrix;
		txt_ngbits = ngbits;
		txt_nabits = nabits;
		txt_records = loop();
	}

let parse_edit_text_layout ch =
	let align = read_byte ch in
	let ml = read_ui16 ch in
	let rl = read_ui16 ch in
	let ident = read_ui16 ch in
	let lead = read_ui16 ch in
	{
		edtl_align = align;
		edtl_left_margin = ml;
		edtl_right_margin = rl;
		edtl_indent = ident;
		edtl_leading = lead;
	}

let parse_edit_text ch =
	let id = read_ui16 ch in
	let bounds = read_rect ch in
	let flags = read_ui16 ch in
	let font = (if flags land 1 <> 0 then 
			let fid = read_ui16 ch in
			let height = read_ui16 ch in
			Some (fid, height)
		else
			None) in
	let color = (if flags land 4 <> 0 then Some (read_rgba ch) else None) in
	let maxlen = (if flags land 2 <> 0 then Some (read_ui16 ch) else None) in
	let layout = (if flags land (1 lsl 13) <> 0 then Some (parse_edit_text_layout ch) else None) in
	let variable = read_string ch in
	let text = (if flags land 128 <> 0 then Some (read_string ch) else None) in
	{
		edt_id = id;
		edt_bounds = bounds;
		edt_font = font;
		edt_color = color;
		edt_maxlen = maxlen;
		edt_layout = layout;
		edt_variable = variable;
		edt_text = text;
		edt_wordwrap = (flags land 64) <> 0;
		edt_multiline = (flags land 32) <> 0;
		edt_password = (flags land 16) <> 0;
		edt_readonly = (flags land 8) <> 0;
		edt_autosize = (flags land (1 lsl 14)) <> 0;
		edt_noselect = (flags land 4096) <> 0;
		edt_border = (flags land 2048) <> 0;
		edt_html = (flags land 512) <> 0;
		edt_outlines = (flags land 256) <> 0;
	}

let parse_font2 ch len =
	let id = read_ui16 ch in
	let data = nread ch (len - 2) in
	{
		ft2_id = id;
		ft2_data = data;
	}

let parse_morph_shape ch len =
	let id = read_ui16 ch in
	let sbounds = read_rect ch in
	let ebounds = read_rect ch in
	let data = nread ch (len - 2 - rect_length sbounds - rect_length ebounds) in
	{
		msh_id = id;
		msh_start_bounds = sbounds;
		msh_end_bounds = ebounds;
		msh_data = data;
	}

let rec parse_button_records ch color =
	let flags = read_byte ch in
	if flags = 0 then
		[]
	else
		let cid = read_ui16 ch in
		let depth = read_ui16 ch in
		let mpos = read_matrix ch in
		let cxa = (if color then Some (read_cxa ch) else None) in
		let r = {
			btr_flags = flags;
			btr_cid = cid;
			btr_depth = depth;
			btr_mpos = mpos;
			btr_color = cxa;
		} in
		r :: parse_button_records ch color

let rec parse_button_actions ch =
	let size = read_ui16 ch in	
	let flags = read_ui16 ch in
	let actions = parse_actions ch in
	let bta = {
		bta_flags = flags;
		bta_actions = actions;
	} in
	if size = 0 then
		[bta]
	else
		bta :: parse_button_actions ch

let parse_button2 ch len =
	let id = read_ui16 ch in
	let flags = read_byte ch in
	let track = (match flags with 0 -> false | 1 -> true | _ -> assert false) in
	let offset = read_ui16 ch in	
	let records = parse_button_records ch true in
	let actions = (if offset = 0 then [] else parse_button_actions ch) in
	{
		bt2_id = id;
		bt2_track_as_menu = track;
		bt2_records = records;
		bt2_actions = actions;
	}

let rec parse_tag ch =
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
	let t = (
		match id with
		| 0x00 ->
			TEnd
		| 0x01 ->
			TShowFrame
		| 0x02 ->
			TShape (parse_shape ch len false)
		(*//0x04 TPlaceObject *)
		| 0x05 ->
			let cid = read_ui16 ch in
			let depth = read_ui16 ch in
			TRemoveObject {
				rmo_id = cid;
				rmo_depth = depth;
			}
		| 0x06 ->
			TBitsJPEG (parse_bitmap ch len)
		(*//0x07 TButton *)
		| 0x08 ->
			TJPEGTables (nread ch len)
		| 0x09 ->
			TSetBgColor (read_rgb ch)
		(*//0x0A TFont *)
		| 0x0B ->
			TText (parse_text ch false)
		| 0x0C ->
			TDoAction (parse_actions ch)
		(*//0x0D TFontInfo *)
		| 0x0E ->
			let sid = read_ui16 ch in
			let flags = read_byte ch in
			let samples = read_i32 ch in
			let data = nread ch (len - 7) in
			TSound {
				so_id = sid;
				so_flags = flags;
				so_samples = samples;
				so_data = data;
			}
		| 0x0F ->
			let sid = read_ui16 ch in
			let data = nread ch (len - 2) in
			TStartSound {
				sts_id = sid;
				sts_data = data;
			}
		(*//0x11 TButtonSound *)
		(*//0x12 TSoundStreamHead *)
		(*//0x13 TSoundStreamBlock *)
		| 0x14 ->
			TBitsLossless (parse_bitmap_lossless ch len)
		| 0x15 ->
			TBitsJPEG2 (parse_bitmap ch len)
		| 0x16 ->
			TShape2 (parse_shape ch len false)
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
			TShape3 (parse_shape ch len true)
		(*//0x21 TText2 *)
		| 0x22 ->
			TButton2 (parse_button2 ch len)
		| 0x23 ->
			let id = read_ui16 ch in
			let size = read_i32 ch in
			let data = nread ch size in
			let alpha_data = nread ch (len - 6 - size) in
			TBitsJPEG3 {
				jp3_id = id;
				jp3_data = data;
				jp3_alpha_data = alpha_data;
			}
		| 0x24 ->
			TBitsLossless2 (parse_bitmap_lossless ch len)
		| 0x25 ->
			TEditText (parse_edit_text ch)
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
			TMorphShape (parse_morph_shape ch len)
		| 0x30 ->
			TFont2 (parse_font2 ch len)
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
			Printf.printf "Unknown tag 0x%.2X\n" id;
			TUnknown (id,nread ch len)
	) in
	let len2 = tag_data_length t in
(*//	if len <> len2 then error (sprintf "Datalen mismatch for tag 0x%.2X (%d != %d)" id len len2); *)
	{
		tid = gen_id();
		tdata = t;
		textended = extended;
	}

and parse_tag_list ch =
	let rec loop acc =
		match parse_tag ch with
		| { tdata = TEnd } -> List.rev acc
		| t -> loop (t :: acc)
	in
	loop []

let parse ch =
	let sign = nread ch 3 in
	if sign <> "FWS" && sign <> "CWS" then error "Invalid SWF signature";
	let ver = read_byte ch in
	swf_version := ver;
	let file_len = read_i32 ch in
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
	| TStartSound _ -> 0x0F
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

let write_clip_event ch (id,data) =
	write_event ch id;
	write_i32 ch (String.length data);
	nwrite ch data

let write_clip_events ch event_list =
 	write_ui16 ch 0;
	let all_events = List.fold_left (fun acc (id,_) -> acc lor id) 0 event_list in
	write_event ch all_events;
	List.iter (write_clip_event ch) event_list;
	write_event ch 0

let write_shape_fill_style ch s =
	match s with
	| SFSSolid c -> 
		write_byte ch 0x00;
		write_rgb ch c
	| SFSSolid3 c ->
		write_byte ch 0x00;
		write_rgba ch c
	| SFSLinearGradient (m,g) ->
		write_byte ch 0x10;
		write_matrix ch m;
		write_gradient ch g
	| SFSRadialGradient (m,g) ->
		write_byte ch 0x12;
		write_matrix ch m;
		write_gradient ch g
	| SFSBitmap b ->
		write_byte ch (match b.sfb_repeat , b.sfb_smooth with
			| true, false -> 0x40
			| false , false -> 0x41
			| true , true -> 0x42
			| false, true -> 0x43);
		write_ui16 ch b.sfb_cid;
		write_matrix ch b.sfb_mpos

let write_shape_fill_styles ch sl =
	let n = List.length sl in
	if n >= 0xFF then begin
		write_byte ch 0xFF;
		write_ui16 ch n;
	end else
		write_byte ch n;
	List.iter (write_shape_fill_style ch) sl

let write_shape_with_style ch s =
	write_shape_fill_styles ch s.sws_fill_styles;
	nwrite ch s.sws_data

let write_shape ch s =
	write_ui16 ch s.sh_id;
	write_rect ch s.sh_bounds;
	write_shape_with_style ch s.sh_style

let write_bitmap ch b =
	write_ui16 ch b.bmp_id;
	nwrite ch b.bmp_data

let write_bitmap_lossless ch b =
	write_ui16 ch b.bll_id;
	write_byte ch b.bll_format;
	write_ui16 ch b.bll_width;
	write_ui16 ch b.bll_height;
	nwrite ch b.bll_data

let write_morph_shape ch s =
	write_ui16 ch s.msh_id;
	write_rect ch s.msh_start_bounds;
	write_rect ch s.msh_end_bounds;
	nwrite ch s.msh_data

let write_text_record ch t r =
	write_byte ch (make_flags [flag r.txr_dx; flag r.txr_dy; flag r.txr_color; flag r.txr_font; false; false; false; true]);
	opt (fun (id,_) -> write_ui16 ch id) r.txr_font;
	opt (write_color ch) r.txr_color;
	opt (write_i16 ch) r.txr_dx;
	opt (write_i16 ch) r.txr_dy;
	opt (fun (_,id) -> write_ui16 ch id) r.txr_font;
	write_byte ch (List.length r.txr_glyphs);
	let bits = init_bits ch in
	List.iter (fun g ->
		write_bits bits t.txt_ngbits g.txg_index;
		write_bits bits t.txt_nabits g.txg_advanced;
	) r.txr_glyphs;
	flush_bits bits

let write_text ch t =
	write_ui16 ch t.txt_id;
	write_rect ch t.txt_bounds;
	write_matrix ch t.txt_matrix;
	write_byte ch t.txt_ngbits;
	write_byte ch t.txt_nabits;
	List.iter (write_text_record ch t) t.txt_records;
	write_byte ch 0

let write_edit_text_layout ch l =
	write_byte ch l.edtl_align;
	write_ui16 ch l.edtl_left_margin;
	write_ui16 ch l.edtl_right_margin;
	write_ui16 ch l.edtl_indent;
	write_ui16 ch l.edtl_leading

let write_edit_text ch t =
	write_ui16 ch t.edt_id;
	write_rect ch t.edt_bounds;
	write_ui16 ch (make_flags [
		flag t.edt_font; flag t.edt_maxlen; flag t.edt_color; t.edt_readonly;
		t.edt_password; t.edt_multiline; t.edt_wordwrap; flag t.edt_text;
		t.edt_outlines; t.edt_html; false; t.edt_border;
		t.edt_noselect; flag t.edt_layout; t.edt_autosize; false
	]);
	opt (fun (id,h) -> write_ui16 ch id; write_ui16 ch h) t.edt_font;
	opt (write_rgba ch) t.edt_color;
	opt (write_ui16 ch) t.edt_maxlen;
	opt (write_edit_text_layout ch) t.edt_layout;
	write_string ch t.edt_variable;
	opt (write_string ch) t.edt_text

let write_font2 ch t =
	write_ui16 ch t.ft2_id;
	nwrite ch t.ft2_data

let write_button_record ch r =
	write_byte ch r.btr_flags;
	write_ui16 ch r.btr_cid;
	write_ui16 ch r.btr_depth;
	write_matrix ch r.btr_mpos;
	match r.btr_color with
	| None -> ()
	| Some c -> 
		write_cxa ch c

let write_button_actions ch = function
	| [] -> assert false
	| [a] ->
		write_ui16 ch 0;
		write_ui16 ch a.bta_flags;
		write_actions ch a.bta_actions
	| a :: l ->
		let size = button_action_length a in
		write_ui16 ch size;
		write_ui16 ch a.bta_flags;
		write_actions ch a.bta_actions

let write_button2 ch b =
	write_ui16 ch b.bt2_id;
	write_byte ch (if b.bt2_track_as_menu then 1 else 0);
	if b.bt2_actions <> [] then write_ui16 ch (3 + sum button_record_length b.bt2_records) else write_ui16 ch 0;
	List.iter (write_button_record ch) b.bt2_records;
	write_byte ch 0;
	if b.bt2_actions <> [] then write_button_actions ch b.bt2_actions	

let rec write_tag_data ch = function
	| TEnd ->
		()		
	| TShowFrame ->
		()
	| TShape s ->
		write_shape ch s
	| TRemoveObject r ->
		write_ui16 ch r.rmo_id;
		write_ui16 ch r.rmo_depth;
	| TBitsJPEG b ->
		write_bitmap ch b
	| TJPEGTables tab ->
		nwrite ch tab
	| TSetBgColor c ->
		write_rgb ch c
	| TText t ->
		write_text ch t
	| TDoAction acts ->
		write_actions ch acts
	| TSound s ->
		write_ui16 ch s.so_id;
		write_byte ch s.so_flags;
		write_i32 ch s.so_samples;
		nwrite ch s.so_data
	| TStartSound s ->
		write_ui16 ch s.sts_id;
		nwrite ch s.sts_data
	| TBitsLossless b ->
		write_bitmap_lossless ch b
	| TBitsJPEG2 b ->
		write_bitmap ch b
	| TShape2 s ->
		write_shape ch s
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
	| TShape3 s -> 
		write_shape ch s
	| TButton2 b ->
		write_button2 ch b
	| TBitsJPEG3 b ->
		write_ui16 ch b.jp3_id;
		write_i32 ch (String.length b.jp3_data);
		nwrite ch b.jp3_data;
		nwrite ch b.jp3_alpha_data;
	| TBitsLossless2 b ->
		write_bitmap_lossless ch b
	| TEditText t ->
		write_edit_text ch t
	| TClip c ->
		write_ui16 ch c.c_id;
		write_ui16 ch c.c_frame_count;
		List.iter (write_tag ch) c.c_tags;
		write_tag ch tag_end;
	| TFrameLabel label ->
		write_string ch label
	| TSoundStreamHead2 data ->
		nwrite ch data
	| TMorphShape s ->
		write_morph_shape ch s
	| TFont2 f ->
		write_font2 ch f
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

and write_tag ch t =
	let id = tag_id t.tdata in
	let dlen = tag_data_length t.tdata in
	if t.textended || dlen >= 63 then begin
		write_ui16 ch ((id lsl 6) lor 63);
		write_i32 ch dlen;
	end else begin
		write_ui16 ch ((id lsl 6) lor dlen);
	end;
	write_tag_data ch t.tdata

let write ch (h,tags) =
	swf_version := h.h_version;
	nwrite ch (if h.h_compressed then "CWS" else "FWS");
	write ch (char_of_int h.h_version);
	let rec calc_len = function
		| [] -> tag_length tag_end
		| t :: l -> 
			tag_length t + calc_len l
	in
	let len = calc_len tags in
	let old_exact_match = !exact_match in
	exact_match := true;
	let len = len + 4 + 4 + rect_length h.h_size + 2 + 2 in
	write_i32 ch len;
	let ch = (if h.h_compressed then deflate ch else ch) in
	write_rect ch h.h_size;
	write_ui16 ch h.h_fps;
	write_ui16 ch h.h_frame_count;
	exact_match := old_exact_match;
	List.iter (write_tag ch) tags;
	write_tag ch tag_end;
	flush ch

;;
Swf.__parser := parse;
Swf.__printer := write