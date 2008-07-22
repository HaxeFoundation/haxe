(*
 *  This file is part of SwfLib
 *  Copyright (c)2004 Nicolas Cannasse
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
open Swf
open ActionScript
open IO

(* ************************************************************************ *)
(* TOOLS *)

let full_parsing = ref true
let force_as3_parsing = ref false
let swf_version = ref 0
let id_count = ref 0
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

let rec read_count n f arg =
	if n = 0 then
		[]
	else
		let v = f arg in
		v :: read_count (n - 1) f arg

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
	r.rect_nbits

let bigrect_nbits r =
	r.brect_nbits

let rgba_nbits c =
	max
		(max (_nbits c.r) (_nbits c.g))
		(max (_nbits c.b) (_nbits c.a))

let cxa_nbits c =
	c.cxa_nbits

let matrix_part_nbits m =
	m.m_nbits

let rgb_length = 3

let rgba_length = 4

let color_length = function
	| ColorRGB _ -> rgb_length
	| ColorRGBA _ -> rgba_length

let rect_length r =
	let nbits = rect_nbits r in
	let nbits = nbits * 4 + 5 in
	(nbits + 7) / 8

let big_rect_length r =
	let nbits = bigrect_nbits r in
	let nbits = nbits * 4 + 5 in
	(nbits + 7) / 8

let gradient_length = function
	| GradientRGB (l,_) -> 1 + (1 + rgb_length) * List.length l
	| GradientRGBA (l,_) -> 1 + (1 + rgba_length) * List.length l

let matrix_length m =
	let matrix_part_len m = 5 + matrix_part_nbits m * 2 in
	let nbits = 2 + opt_len matrix_part_len m.scale + opt_len matrix_part_len m.rotate + matrix_part_len m.trans in
	(nbits + 7) / 8

let cxa_length c =
	let nbits = cxa_nbits c in
	let nbits = 6 + opt_len (const (nbits * 4)) c.cxa_add + opt_len (const (nbits * 4)) c.cxa_mult in
	(nbits + 7) / 8

let clip_event_length c =
	(if !swf_version >= 6 then 4 else 2) + 4 + (opt_len (const 1) c.cle_key) + actions_length c.cle_actions

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
	| SFSRadialGradient (m,g,None) -> matrix_length m + gradient_length g
	| SFSRadialGradient (m,g,Some _) -> matrix_length m + gradient_length g + 2
	| SFSBitmap b -> 2 + matrix_length b.sfb_mpos

let shape_line_style_length s =
	2 + match s.sls_flags with
		| None -> color_length s.sls_color
		| Some _ ->
			2 + (match s.sls_fill with None -> color_length s.sls_color | Some f -> shape_fill_style_length f)
			  + opt_len (const 2) s.sls_miter

let shape_array_length f s =
	let n = List.length s in
	(if n < 0xFF then 1 else 3) + sum f s

let shape_new_styles_length s =
	shape_array_length shape_fill_style_length s.sns_fill_styles +
	shape_array_length shape_line_style_length s.sns_line_styles +
	1

let shape_records_length records =
	let nbits = ref 8 in
	let nfbits = ref records.srs_nfbits in
	let nlbits = ref records.srs_nlbits in
	List.iter (fun r ->
		nbits := !nbits + 6;
		match r with
		| SRStyleChange s ->
			nbits := !nbits +
				opt_len (fun (n,_,_) -> 5 + n * 2) s.scsr_move +
				opt_len (const !nfbits) s.scsr_fs0 +
				opt_len (const !nfbits) s.scsr_fs1 +
				opt_len (const !nlbits) s.scsr_ls;
			(match s.scsr_new_styles with
			| None -> ()
			| Some s ->
				nbits := (((!nbits + 7) / 8) + shape_new_styles_length s) * 8;
				nfbits := s.sns_nfbits;
				nlbits := s.sns_nlbits)
		| SRCurvedEdge s ->
			nbits := !nbits + s.scer_nbits * 4
		| SRStraightEdge s ->
			nbits := !nbits + 1 + (match s.sser_line with
								| None , None -> assert false
								| Some _ , None
								| None, Some _ -> 1 + s.sser_nbits
								| Some _ , Some _ -> 2 * s.sser_nbits)
	) records.srs_records;
	nbits := !nbits + 6;
	(!nbits + 7) / 8

let shape_with_style_length s =
	shape_array_length shape_fill_style_length s.sws_fill_styles +
	shape_array_length shape_line_style_length s.sws_line_styles +
	shape_records_length s.sws_records

let shape_length s =
	2 + rect_length s.sh_bounds + opt_len (fun (r,_) -> rect_length r + 1) s.sh_bounds2 + shape_with_style_length s.sh_style

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
	2 + big_rect_length t.txt_bounds + matrix_length t.txt_matrix + 2 + sum (text_record_length t) t.txt_records + 1

let filters_length l =
	1 + sum (fun f ->
		1 + match f with
		| FDropShadow s
		| FBlur s
		| FGlow s
		| FBevel s
		| FAdjustColor s ->
			String.length s
		| FGradientGlow fg
		| FGradientBevel fg ->
			1 + ((rgba_length + 1) * List.length fg.fgr_colors) + String.length fg.fgr_data
	) l

let button_record_length r =
	1 + 2 + 2 + matrix_length r.btr_mpos + (match r.btr_color with None -> 0 | Some c -> cxa_length c)
	+ opt_len filters_length r.btr_filters

let button_action_length r =
	2 + 2 + actions_length r.bta_actions

let button2_length b =
	2 + 1 + 2 +
		1 + sum button_record_length b.bt2_records +
		sum button_action_length b.bt2_actions

let font2_length f =
	2 + String.length f.ft2_data

let font3_length f =
	2 + String.length f.ft3_data

let font_glyphs_length f =
	2 + String.length f.fgl_data

let edit_text_layout_length = 9

let header_length h =
	3 + 1 + rect_length h.h_size + 2 + 4

let edit_text_length t =
	2 + rect_length t.edt_bounds + 2 +
		opt_len (const 4) t.edt_font +
		opt_len (const rgba_length) t.edt_color +
		opt_len (const 2) t.edt_maxlen +
		opt_len (const edit_text_layout_length) t.edt_layout +
		String.length t.edt_variable + 1 +
		opt_len (fun s -> String.length s + 1) t.edt_text

let place_object_length p v3 =
	3
	+ (if v3 then 1 else 0)
	+ 0 (* po_move *)
	+ opt_len (const 2) p.po_cid
	+ opt_len matrix_length p.po_matrix
	+ opt_len cxa_length p.po_color
	+ opt_len (const 2) p.po_ratio
	+ opt_len (fun s -> String.length s + 1) p.po_inst_name
	+ opt_len (const 2) p.po_clip_depth
	+ opt_len clip_events_length p.po_events
	+ (if v3 then
		opt_len filters_length p.po_filters
		+ opt_len (const 1) p.po_blend
		+ opt_len (const 1) p.po_bcache
	else
		0)

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
		2 + String.length b.jpg_data
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
		2 + String.length b.jp2_table + String.length b.jp2_data
	| TShape2 s ->
		shape_length s
	| TProtect ->
		0
	| TPlaceObject2 p ->
		place_object_length p false
	| TRemoveObject2 _ ->
		2
	| TShape3 s ->
		shape_length s
	| TText2 t ->
		text_length t
	| TButton2 b ->
		button2_length b
	| TBitsJPEG3 b ->
		2 + 4 + String.length b.jp3_alpha_data + String.length b.jp3_data + String.length b.jp3_table
	| TBitsLossless2 b ->
		bitmap_lossless_length b
	| TEditText t ->
		edit_text_length t
	| TClip c ->
		4 + sum tag_length (tag_end :: c.c_tags)
	| TProductInfo s ->
		String.length s
	| TFrameLabel (label,id) ->
		String.length label + 1 + (match id with None -> 0 | Some _ -> 1)
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
	| TVideoStream s ->
		String.length s
	| TVideoFrame s ->
		String.length s
	| TDebugID s ->
		String.length s
	| TEnableDebugger2 (_,data) ->
		2 + String.length data + 1
	| TScriptLimits _ ->
		4
	| TSandbox _ ->
		4
	| TPlaceObject3 p ->
		place_object_length p true
	| TFontGlyphs f ->
		font_glyphs_length f
	| TTextInfo s ->
		String.length s
	| TFont3 f ->
		font3_length f
	| TF9Classes l ->
		2 + sum (fun c -> String.length c.f9_classname + 1 + 2) l
	| TMetaData meta ->
		String.length meta
	| TActionScript3 (id,a) ->
		(match id with None -> 0 | Some (id,f) -> 4 + String.length f + 1) + As3parse.as3_length a
	| TShape4 s ->
		shape_length s
	| TShape5 (_,s) ->
		2 + String.length s
	| TF9Scene name ->
		2 + String.length name + 1 + 1
	| TUnknown (_,data) ->
		String.length data

and tag_length t =
	let dlen = tag_data_length t.tdata in
	dlen + 2 + (if t.textended || dlen >= 63 then 4 else 0)

(* ************************************************************************ *)
(* READ PRIMS *)

let skip ch n =
	seek_in ch ((Pervasives.pos_in ch) + n)

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
	let grad_rgb() =
		let r = read_byte ch in
		let c = read_rgb ch in
		(r, c)
	in
	let grad_rgba() =
		let r = read_byte ch in
		let c = read_rgba ch in
		(r, c)
	in
	let n = read_byte ch in
	let n , flags = n land 0xF , n lsr 4 in
	if is_rgba then
		GradientRGBA (read_count n grad_rgba (),flags)
	else
		GradientRGB (read_count n grad_rgb (),flags)

let read_rect ch =
	let b = input_bits ch in
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

let rec read_multi_bits b n =
	if n <= 30 then
		[read_bits b n]
	else
		let d = read_bits b 30 in
		d :: read_multi_bits b (n - 30)

let read_big_rect ch =
	let b = input_bits ch in
	let nbits = read_bits b 5 in
	let left = read_multi_bits b nbits in
	let right = read_multi_bits b nbits in
	let top = read_multi_bits b nbits in
	let bottom = read_multi_bits b nbits in
	{
		brect_nbits = nbits;
		bleft = left;
		bright = right;
		btop = top;
		bbottom = bottom;
	}

let read_matrix ch =
	let b = input_bits ch in
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
	let b = input_bits ch in
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
	| GradientRGB (l,flags) ->
		let n = List.length l in
		write_byte ch (n lor (flags lsl 4));
		List.iter (fun (ratio,c) -> write_byte ch ratio; write_rgb ch c) l
	| GradientRGBA (l,flags) ->
		let n = List.length l in
		write_byte ch (n lor (flags lsl 4));
		List.iter (fun (ratio,c) -> write_byte ch ratio; write_rgba ch c) l

let write_rect ch r =
	let b = output_bits ch in
	let nbits = rect_nbits r in
	write_bits b 5 nbits;
	write_bits b nbits r.left;
	write_bits b nbits r.right;
	write_bits b nbits r.top;
	write_bits b nbits r.bottom;
	flush_bits b

let rec write_multi_bits b n l =
	if n <= 30 then
		match l with
		| [] -> write_bits b n 0
		| [x] -> write_bits b n x
		| _ -> assert false
	else
		match l with
		| [] -> write_bits b 30 0; write_multi_bits b (n - 30) []
		| x :: l -> write_bits b 30 x; write_multi_bits b (n - 30) l

let write_big_rect ch r =
	let b = output_bits ch in
	let nbits = bigrect_nbits r in
	write_bits b 5 nbits;
	write_multi_bits b nbits r.bleft;
	write_multi_bits b nbits r.bright;
	write_multi_bits b nbits r.btop;
	write_multi_bits b nbits r.bbottom;
	flush_bits b

let write_matrix ch m =
	let b = output_bits ch in
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
	let b = output_bits ch in
	let nbits = cxa_nbits c in
	(match c.cxa_add , c.cxa_mult with
	| None , None ->
		write_bits b 2 0;
		write_bits b 4 1; (* some strange MM thing... *)
	| Some c , None ->
		write_bits b 2 2;
		write_bits b 4 nbits;
		List.iter (write_bits b ~nbits) [c.r;c.g;c.b;c.a];
	| None , Some c ->
		write_bits b 2 1;
		write_bits b 4 nbits;
		List.iter (write_bits b ~nbits) [c.r;c.g;c.b;c.a];
	| Some c1 , Some c2 ->
		write_bits b 2 3;
		write_bits b 4 nbits;
		List.iter (write_bits b ~nbits) [c2.r;c2.g;c2.b;c2.a;c1.r;c1.g;c1.b;c1.a]
	);
	flush_bits b

let write_event ch evt =
	(if !swf_version >= 6 then write_i32 else write_ui16) ch evt

(* ************************************************************************ *)
(* PARSING *)

let parse_clip_events ch =
	ignore(read_ui16 ch); (* reserved *)
	ignore(read_event ch); (* all_events *)
	let rec loop() =
		let events = read_event ch in
		if events = 0 then
			[]
		else begin
			ignore(read_i32 ch); (* len *)
			let key = (if events land (1 lsl 17) <> 0 then Some (read ch) else None) in
			let e = {
				cle_events = events;
				cle_key = key;
				cle_actions = parse_actions ch
			} in
			e :: (loop())
		end;
	in
	loop()

let parse_shape_fill_style ch vshape =
	let t = read_byte ch in
	match t with
	| 0x00 when vshape >= 3 -> SFSSolid3 (read_rgba ch)
	| 0x00 -> SFSSolid (read_rgb ch)
	| 0x10 ->
		let m = read_matrix ch in
		let g = read_gradient ch (vshape >= 3) in
		SFSLinearGradient (m,g)
	| 0x12 ->
		let m = read_matrix ch in
		let g = read_gradient ch (vshape >= 3) in
		SFSRadialGradient (m,g,None)
	| 0x13 ->
		let m = read_matrix ch in
		let g = read_gradient ch (vshape >= 3) in
		let i = read_i16 ch in
		SFSRadialGradient (m,g,Some i)
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

let parse_shape_line_style ch vshape =
	let width = read_ui16 ch in
	if vshape >= 4 then begin
		let flags = read_ui16 ch in
		let fill = (flags land 8 <> 0) in
		let miterjoin = (flags land 0x20 <> 0) in
		let miter = (if miterjoin then Some (IO.read_ui16 ch) else None) in
		let color = (if fill then { r = 0; g = 0; b = 0; a = 0 } else read_rgba ch) in
		(*
			let noVscale = (flags land 0x02 <> 0) in
			let noHscale = (flags land 0x04 <> 0) in
			let beveljoin = (flags land 0x10 <> 0) in
			let nocap = (flags land 0x40 <> 0) in
			let squarecap = (flags land 0x80 <> 0) in
		*)
		{
			sls_width = width;
			sls_color = ColorRGBA color;
			sls_fill = if fill then Some (parse_shape_fill_style ch vshape) else None;
			sls_flags = Some flags;
			sls_miter = miter;
		}
	end else
		{
			sls_width = width;
			sls_color = if vshape = 3 then ColorRGBA (read_rgba ch) else ColorRGB (read_rgb ch);
			sls_fill = None;
			sls_flags = None;
			sls_miter = None;
		}

let parse_shape_array f ch vshape =
	let n = (match read_byte ch with 0xFF -> read_ui16 ch | n -> n) in
	read_count n (f ch) vshape

let parse_shape_style_change_record ch b flags nlbits nfbits vshape =
	let move = (if flags land 1 <> 0 then begin
		let mbits = read_bits b 5 in
		let dx = read_bits b mbits in
		let dy = read_bits b mbits in
		Some (mbits,dx,dy)
	end else
		None)
	in
	let fs0 = (if flags land 2 <> 0 then Some (read_bits b !nfbits) else None) in
	let fs1 = (if flags land 4 <> 0 then Some (read_bits b !nfbits) else None) in
	let ls = (if flags land 8 <> 0 then Some (read_bits b !nlbits) else None) in
	let styles = (if flags land 16 <> 0 then begin
		IO.drop_bits b;
		let fstyles = parse_shape_array parse_shape_fill_style ch vshape in
		let lstyles = parse_shape_array parse_shape_line_style ch vshape in
		let bits = read_byte ch in
		nlbits := bits land 15;
		nfbits := bits lsr 4;
		Some {
			sns_fill_styles = fstyles;
			sns_line_styles = lstyles;
			sns_nlbits = !nlbits;
			sns_nfbits = !nfbits;
		}
	end else
		None
	) in
	{
		scsr_move = move;
		scsr_fs0 = fs0;
		scsr_fs1 = fs1;
		scsr_ls = ls;
		scsr_new_styles = styles;
	}

let parse_shape_curved_edge_record b flags =
	let nbits = (flags land 15) + 2 in
	let cx = read_bits b nbits in
	let cy = read_bits b nbits in
	let ax = read_bits b nbits in
	let ay = read_bits b nbits in
	{
		scer_nbits = nbits;
		scer_cx = cx;
		scer_cy = cy;
		scer_ax = ax;
		scer_ay = ay;
	}

let parse_shape_straight_edge_record b flags =
	let nbits = (flags land 15) + 2 in
	let is_general = (read_bits b 1 = 1) in
	let l = (if is_general then
		let dx = read_bits b nbits in
		let dy = read_bits b nbits in
		Some dx, Some dy
	else
		let is_vertical = (read_bits b 1 = 1) in
		let p = read_bits b nbits in
		if is_vertical then
			None, Some p
		else
			Some p, None)
	in
	{
		sser_nbits = nbits;
		sser_line = l;
	}

let parse_shape_records ch nlbits nfbits vshape =
	let b = input_bits ch in
	let nlbits = ref nlbits in
	let nfbits = ref nfbits in
	let rec loop() =
		let flags = read_bits b 6 in
		if flags = 0 then
			[]
		else
			let r =
				(if (flags land 32) = 0 then
					SRStyleChange (parse_shape_style_change_record ch b flags nlbits nfbits vshape)
				else if (flags land 48) = 32 then
					SRCurvedEdge (parse_shape_curved_edge_record b flags)
				else
					SRStraightEdge (parse_shape_straight_edge_record b flags))
			in
			r :: loop()
	in
	loop()

let parse_shape_with_style ch vshape =
	let fstyles = parse_shape_array parse_shape_fill_style ch vshape in
	let lstyles = parse_shape_array parse_shape_line_style ch vshape in
	let bits = read_byte ch in
	let nlbits = bits land 15 in
	let nfbits = bits lsr 4 in
	let records = parse_shape_records ch nlbits nfbits vshape in
	{
		sws_fill_styles = fstyles;
		sws_line_styles = lstyles;
		sws_records = {
			srs_nlbits = nlbits;
			srs_nfbits = nfbits;
			srs_records = records;
		}
	}


let parse_shape ch len vshape =
	let id = read_ui16 ch in
	let bounds = read_rect ch in
	let bounds2 = (if vshape = 4 then
		let r = read_rect ch in
		let b = read_byte ch in
		Some (r, b)
	else
		None
	) in
	let style = parse_shape_with_style ch vshape in
	{
		sh_id = id;
		sh_bounds = bounds;
		sh_bounds2 = bounds2;
		sh_style = style;
	}

let parse_jpg_table ch =
	let b = Buffer.create 0 in
	let rec loop flag =
		let c = IO.read ch in
		Buffer.add_char b c;
		match int_of_char c with
		| 0xFF -> loop true
		| 0xD9 when flag -> ()
		| _ -> loop false
	in
	loop false;
	Buffer.contents b

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
	let bounds = read_big_rect ch in
	let matrix = read_matrix ch in
	let ngbits = read_byte ch in
	let nabits = read_byte ch in
	let read_glyph bits =
		let indx = read_bits bits ngbits in
		let adv = read_bits bits nabits in
		{
			txg_index = indx;
			txg_advanced = adv;
		}
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
				txr_glyphs = read_count nglyphs read_glyph (input_bits ch);
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

let parse_font3 ch len =
	let id = read_ui16 ch in
	let data = nread ch (len - 2) in
	{
		ft3_id = id;
		ft3_data = data;
	}

let parse_font_glyphs ch len =
	let id = read_ui16 ch in
	let data = nread ch (len - 2) in
	{
		fgl_id = id;
		fgl_data = data;
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

let parse_filter_gradient ch =
	let ncolors = read_byte ch in
	let colors = read_count ncolors read_rgba ch in
	let cvals = read_count ncolors read_byte ch in
	let data = nread ch 19 in
	{
		fgr_colors = List.combine colors cvals;
		fgr_data = data;
	}

let parse_filter ch =
	match read_byte ch with
	| 0 -> FDropShadow (nread ch 23)
	| 1 -> FBlur (nread ch 9)
	| 2 -> FGlow (nread ch 15)
	| 3 -> FBevel (nread ch 27)
	| 4 -> FGradientGlow (parse_filter_gradient ch)
	| 6 -> FAdjustColor (nread ch 80)
	| 7 -> FGradientBevel (parse_filter_gradient ch)
	| _ -> assert false

let parse_filters ch =
	let nf = read_byte ch in
	read_count nf parse_filter ch

let rec parse_button_records ch color =
	let flags = read_byte ch in
	if flags = 0 then
		[]
	else
		let cid = read_ui16 ch in
		let depth = read_ui16 ch in
		let mpos = read_matrix ch in
		let cxa = (if color then Some (read_cxa ch) else None) in
		let filters = (if flags land 16 = 0 then None else Some (parse_filters ch)) in
		let r = {
			btr_flags = flags;
			btr_cid = cid;
			btr_depth = depth;
			btr_mpos = mpos;
			btr_color = cxa;
			btr_filters = filters;
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

let parse_place_object ch v3 =
	let f = read_byte ch in
	let fext = (if v3 then read_byte ch else 0) in
	let depth = read_ui16 ch in
	let move = (f land 1) <> 0 in
	let cid = opt_flag f 2 read_ui16 ch in
	let matrix = opt_flag f 4 read_matrix ch in
	let color = opt_flag f 8 read_cxa ch in
	let ratio = opt_flag f 16 read_ui16 ch in
	let name = opt_flag f 32 read_string ch in
	let clip_depth = opt_flag f 64 read_ui16 ch in
	let clip_events = opt_flag f 128 parse_clip_events ch in
	let filters = opt_flag fext 1 parse_filters ch in
	let blend = opt_flag fext 2 read_byte ch in
	let bcache = opt_flag fext 4 read_byte ch in
	{
		po_depth = depth;
		po_move = move;
		po_cid = cid;
		po_matrix = matrix;
		po_color = color;
		po_ratio = ratio;
		po_inst_name = name;
		po_clip_depth = clip_depth;
		po_events = clip_events;
		po_filters = filters;
		po_blend = blend;
		po_bcache = bcache;
	}

let rec parse_tag ch h =
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
		| 0x02 when !full_parsing ->
			TShape (parse_shape ch len 1)
		(*//0x04 TPlaceObject *)
		| 0x05 ->
			let cid = read_ui16 ch in
			let depth = read_ui16 ch in
			TRemoveObject {
				rmo_id = cid;
				rmo_depth = depth;
			}
		| 0x06 ->
			let id = read_ui16 ch in
			let data = nread ch (len - 2) in
			TBitsJPEG {
				jpg_id = id;
				jpg_data = data;
			}
		(*//0x07 TButton *)
		| 0x08 ->
			TJPEGTables (nread ch len)
		| 0x09 ->
			TSetBgColor (read_rgb ch)
		(*//0x0A TFont *)
		| 0x0B when !full_parsing ->
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
			let id = read_ui16 ch in
			let table = parse_jpg_table ch in
			let data = nread ch (len - 2 - String.length table) in
			TBitsJPEG2 {
				jp2_id = id;
				jp2_table = table;
				jp2_data = data;
			}
		| 0x16 when !full_parsing ->
			TShape2 (parse_shape ch len 2)
		(*//0x17 TButtonCXForm *)
		| 0x18 ->
			TProtect
		| 0x1A when !full_parsing ->
			TPlaceObject2 (parse_place_object ch false)
		| 0x1C ->
			let depth = read_ui16 ch in
			TRemoveObject2 depth
		| 0x20 when !full_parsing ->
			TShape3 (parse_shape ch len 3)
		| 0x21 when !full_parsing ->
			TText2 (parse_text ch true)
		| 0x22 when !full_parsing ->
			TButton2 (parse_button2 ch len)
		| 0x23 ->
			let id = read_ui16 ch in
			let size = read_i32 ch in
			let table = parse_jpg_table ch in
			let data = nread ch (size - String.length table) in
			let alpha_data = nread ch (len - 6 - size) in
			TBitsJPEG3 {
				jp3_id = id;
				jp3_table = table;
				jp3_data = data;
				jp3_alpha_data = alpha_data;
			}
		| 0x24 ->
			TBitsLossless2 (parse_bitmap_lossless ch len)
		| 0x25 when !full_parsing ->
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
		| 0x29 ->
			TProductInfo (nread ch len)
		| 0x2B ->
			let label = read_string ch in
			let id = (if len = String.length label + 2 then Some (read ch) else None) in
			TFrameLabel (label,id)
		| 0x2D ->
			TSoundStreamHead2 (nread ch len)
		| 0x2E when !full_parsing ->
			TMorphShape (parse_morph_shape ch len)
		| 0x30 when !full_parsing ->
			TFont2 (parse_font2 ch len)
		| 0x38 ->
			let read_export() =
				let cid = read_ui16 ch in
				let name = read_string ch in
				{
					exp_id = cid;
					exp_name = name
				}
			in
			TExport (read_count (read_ui16 ch) read_export ())
		(*// 0x39 TImport *)
		(*// 0x3A TEnableDebugger *)
		| 0x3B ->
			let cid = read_ui16 ch in
			let actions = parse_actions ch in
			TDoInitAction {
				dia_id = cid;
				dia_actions = actions;
			}
		| 0x3C ->
			TVideoStream (nread ch len)
		| 0x3D ->
			TVideoFrame (nread ch len)
		(*// 0x3E TFontInfo2 *)
		| 0x3F ->
			TDebugID (nread ch len)
		| 0x40 ->
			let tag = read_ui16 ch in
			(* 0 in general, 6517 for some swfs *)
			let pass_md5 = read_string ch in
			TEnableDebugger2 (tag,pass_md5)
		| 0x41 ->
			let recursion_depth = read_ui16 ch in
			let script_timeout = read_ui16 ch in
			TScriptLimits (recursion_depth, script_timeout)
		(*// 0x42 TSetTabIndex *)
		| 0x45 ->
			TSandbox (match IO.read_i32 ch with
				| 0 -> SBLocal
				| 1 -> SBNetwork
				| n -> SBUnknown n
			)
		| 0x46 when !full_parsing ->
			TPlaceObject3 (parse_place_object ch true)
		| 0x48 when !full_parsing || !force_as3_parsing ->
			TActionScript3 (None , As3parse.parse ch len)
		| 0x49 when !full_parsing ->
			TFontGlyphs (parse_font_glyphs ch len)
		| 0x4A ->
			TTextInfo (nread ch len)
		| 0x4B when !full_parsing ->
			TFont3 (parse_font3 ch len)
		| 0x4C ->
			let i = read_ui16 ch in
			let rec loop i =
				if i = 0 then
					[]
				else
					let a = read_ui16 ch in
					let s = read_string ch in
					{
						f9_cid = if a = 0 then None else Some a;
						f9_classname = s;
					} :: loop (i - 1)
			in
			TF9Classes (loop i)
		| 0x4D ->
			TMetaData (nread ch len)
		| 0x52 when !full_parsing || !force_as3_parsing ->
			let id = read_i32 ch in
			let frame = read_string ch in
			let len = len - (4 + String.length frame + 1) in
			TActionScript3 (Some (id,frame), As3parse.parse ch len)
		| 0x53 when !full_parsing ->
			TShape4 (parse_shape ch len 4)
		| 0x54 when !full_parsing ->
			let id = read_ui16 ch in
			TShape5 (id,nread ch (len - 2))
		| 0x56 ->
			let n = read_ui16 ch in
			if n <> 1 then assert false;
			let name = read_string ch in
			let k = read_byte ch in
			if k <> 0 then assert false;
			TF9Scene name
		| _ ->
			(*if !Swf.warnings then Printf.printf "Unknown tag 0x%.2X\n" id;*)
			TUnknown (id,nread ch len)
	) in
(*	let len2 = tag_data_length t in
	if len <> len2 then error (Printf.sprintf "Datalen mismatch for tag 0x%.2X (%d != %d)" id len len2);
*)	{
		tid = gen_id();
		tdata = t;
		textended = extended;
	}

and parse_tag_list ch =
	let rec loop acc =
		let h = (try read_ui16 ch with IO.No_more_input -> 0) in
		match parse_tag ch h with
		| { tdata = TEnd } -> List.rev acc
		| t -> loop (t :: acc)
	in
	loop []

let parse ch =
	let sign = nread ch 3 in
	if sign <> "FWS" && sign <> "CWS" then error "Invalid SWF signature";
	let ver = read_byte ch in
	swf_version := ver;
	ignore(read_i32 ch); (* file length *)
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
	| TText2 _ -> 0x21
	| TButton2 _ -> 0x22
	| TBitsJPEG3 _ -> 0x23
	| TBitsLossless2 _ -> 0x24
	| TEditText _ -> 0x25
	| TClip _ -> 0x27
	| TProductInfo _ -> 0x29
	| TFrameLabel _ -> 0x2B
	| TSoundStreamHead2 _ -> 0x2D
	| TMorphShape _ -> 0x2E
	| TFont2 _ -> 0x30
	| TExport _ -> 0x38
	| TDoInitAction _ -> 0x3B
	| TVideoStream _ -> 0x3C
	| TVideoFrame _ -> 0x3D
	| TDebugID _ -> 0x3F
	| TEnableDebugger2 _ -> 0x40
	| TScriptLimits _ -> 0x41
	| TSandbox _ -> 0x45
	| TPlaceObject3 _ -> 0x46
	| TFontGlyphs _ -> 0x49
	| TTextInfo _ -> 0x4A
	| TFont3 _ -> 0x4B
	| TF9Classes _ -> 0x4C
	| TMetaData _ -> 0x4D
	| TActionScript3 (None,_) -> 0x48
	| TActionScript3 _ -> 0x52
	| TShape4 _ -> 0x53
	| TShape5 _ -> 0x54
	| TF9Scene _ -> 0x56
	| TUnknown (id,_) -> id

let write_clip_event ch c =
	write_event ch c.cle_events;
	write_i32 ch (actions_length c.cle_actions + opt_len (const 1) c.cle_key);
	opt (write ch) c.cle_key;
	write_actions ch c.cle_actions

let write_clip_events ch event_list =
 	write_ui16 ch 0;
	let all_events = List.fold_left (fun acc c -> acc lor c.cle_events) 0 event_list in
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
	| SFSRadialGradient (m,g,None) ->
		write_byte ch 0x12;
		write_matrix ch m;
		write_gradient ch g
	| SFSRadialGradient (m,g,Some i) ->
		write_byte ch 0x13;
		write_matrix ch m;
		write_gradient ch g;
		write_i16 ch i;
	| SFSBitmap b ->
		write_byte ch (match b.sfb_repeat , b.sfb_smooth with
			| true, false -> 0x40
			| false , false -> 0x41
			| true , true -> 0x42
			| false, true -> 0x43);
		write_ui16 ch b.sfb_cid;
		write_matrix ch b.sfb_mpos

let write_shape_line_style ch l =
	write_ui16 ch l.sls_width;
	opt (write_ui16 ch) l.sls_flags;
	opt (write_ui16 ch) l.sls_miter;
	match l.sls_fill with
	| None ->
		write_color ch l.sls_color;
	| Some fill ->
		write_shape_fill_style ch fill

let write_shape_array ch f sl =
	let n = List.length sl in
	if n >= 0xFF then begin
		write_byte ch 0xFF;
		write_ui16 ch n;
	end else
		write_byte ch n;
	List.iter (f ch) sl

let write_shape_style_change_record ch b nlbits nfbits s =
	let flags = make_flags [flag s.scsr_move; flag s.scsr_fs0; flag s.scsr_fs1; flag s.scsr_ls; flag s.scsr_new_styles] in
	write_bits b 6 flags;
	opt (fun (n,dx,dy) ->
		write_bits b 5 n;
		write_bits b n dx;
		write_bits b n dy;
	) s.scsr_move;
	opt (write_bits b ~nbits:!nfbits) s.scsr_fs0;
	opt (write_bits b ~nbits:!nfbits) s.scsr_fs1;
	opt (write_bits b ~nbits:!nlbits) s.scsr_ls;
	match s.scsr_new_styles with
	| None -> ()
	| Some s ->
		flush_bits b;
		write_shape_array ch write_shape_fill_style s.sns_fill_styles;
		write_shape_array ch write_shape_line_style s.sns_line_styles;
		nfbits := s.sns_nfbits;
		nlbits := s.sns_nlbits;
		write_bits b 4 !nfbits;
		write_bits b 4 !nlbits

let write_shape_record ch b nlbits nfbits = function
	| SRStyleChange s ->
		write_shape_style_change_record ch b nlbits nfbits s
	| SRCurvedEdge s ->
		write_bits b 2 2;
		write_bits b 4 (s.scer_nbits - 2);
		write_bits b s.scer_nbits s.scer_cx;
		write_bits b s.scer_nbits s.scer_cy;
		write_bits b s.scer_nbits s.scer_ax;
		write_bits b s.scer_nbits s.scer_ay;
	| SRStraightEdge s ->
		write_bits b 2 3;
		write_bits b 4 (s.sser_nbits - 2);
		match s.sser_line with
		| None , None -> assert false
		| None , Some p
		| Some p , None ->
			write_bits b 1 0;
			write_bits b 1 (if (fst s.sser_line) = None then 1 else 0);
			write_bits b s.sser_nbits p;
		| Some dx, Some dy ->
			write_bits b 1 1;
			write_bits b s.sser_nbits dx;
			write_bits b s.sser_nbits dy

let write_shape_with_style ch s =
	write_shape_array ch write_shape_fill_style s.sws_fill_styles;
	write_shape_array ch write_shape_line_style s.sws_line_styles;
	let r = s.sws_records in
	let b = output_bits ch in
	write_bits b 4 r.srs_nfbits;
	write_bits b 4 r.srs_nlbits;
	let nlbits = ref r.srs_nlbits in
	let nfbits = ref r.srs_nfbits in
	List.iter (write_shape_record ch b nlbits nfbits) r.srs_records;
	write_bits b 6 0;
	flush_bits b

let write_shape ch s =
	write_ui16 ch s.sh_id;
	write_rect ch s.sh_bounds;
	(match s.sh_bounds2 with
	| None -> ()
	| Some (r,b) ->
		write_rect ch r;
		write_byte ch b);
	write_shape_with_style ch s.sh_style

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
	let bits = output_bits ch in
	List.iter (fun g ->
		write_bits bits t.txt_ngbits g.txg_index;
		write_bits bits t.txt_nabits g.txg_advanced;
	) r.txr_glyphs;
	flush_bits bits

let write_text ch t =
	write_ui16 ch t.txt_id;
	write_big_rect ch t.txt_bounds;
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

let write_font3 ch t =
	write_ui16 ch t.ft3_id;
	nwrite ch t.ft3_data

let write_font_glyphs ch t =
	write_ui16 ch t.fgl_id;
	nwrite ch t.fgl_data

let write_filter_gradient ch fg =
	write_byte ch (List.length fg.fgr_colors);
	List.iter (fun (c,_) -> write_rgba ch c) fg.fgr_colors;
	List.iter (fun (_,n) -> write_byte ch n) fg.fgr_colors;
	nwrite ch fg.fgr_data

let write_filter ch = function
	| FDropShadow s ->
		write_byte ch 0;
		nwrite ch s
	| FBlur s ->
		write_byte ch 1;
		nwrite ch s
	| FGlow s ->
		write_byte ch 2;
		nwrite ch s
	| FBevel s ->
		write_byte ch 3;
		nwrite ch s
	| FGradientGlow fg ->
		write_byte ch 4;
		write_filter_gradient ch fg
	| FAdjustColor s ->
		write_byte ch 6;
		nwrite ch s
	| FGradientBevel fg ->
		write_byte ch 7;
		write_filter_gradient ch fg

let write_button_record ch r =
	write_byte ch r.btr_flags;
	write_ui16 ch r.btr_cid;
	write_ui16 ch r.btr_depth;
	write_matrix ch r.btr_mpos;
	(match r.btr_color with
	| None -> ()
	| Some c ->
		write_cxa ch c);
	opt (fun l ->
		write_byte ch (List.length l);
		List.iter (write_filter ch) l
	) r.btr_filters

let rec write_button_actions ch = function
	| [] -> assert false
	| [a] ->
		write_ui16 ch 0;
		write_ui16 ch a.bta_flags;
		write_actions ch a.bta_actions
	| a :: l ->
		let size = button_action_length a in
		write_ui16 ch size;
		write_ui16 ch a.bta_flags;
		write_actions ch a.bta_actions;
		write_button_actions ch l

let write_button2 ch b =
	write_ui16 ch b.bt2_id;
	write_byte ch (if b.bt2_track_as_menu then 1 else 0);
	if b.bt2_actions <> [] then write_ui16 ch (3 + sum button_record_length b.bt2_records) else write_ui16 ch 0;
	List.iter (write_button_record ch) b.bt2_records;
	write_byte ch 0;
	if b.bt2_actions <> [] then write_button_actions ch b.bt2_actions

let write_place_object ch p v3 =
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
	if v3 then write_byte ch (make_flags [flag p.po_filters; flag p.po_blend; flag p.po_bcache]);
	write_ui16 ch p.po_depth;
	opt (write_ui16 ch) p.po_cid;
	opt (write_matrix ch) p.po_matrix;
	opt (write_cxa ch) p.po_color;
	opt (write_ui16 ch) p.po_ratio;
	opt (write_string ch) p.po_inst_name;
	opt (write_ui16 ch) p.po_clip_depth;
	opt (write_clip_events ch) p.po_events;
	if v3 then begin
		opt (fun l ->
			write_byte ch (List.length l);
			List.iter (write_filter ch) l
		) p.po_filters;
		opt (write_byte ch) p.po_blend;
		opt (write_byte ch) p.po_bcache;
	end

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
		write_ui16 ch b.jpg_id;
		nwrite ch b.jpg_data
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
		write_ui16 ch b.jp2_id;
		nwrite ch b.jp2_table;
		nwrite ch b.jp2_data;
	| TShape2 s ->
		write_shape ch s
	| TProtect ->
		()
	| TPlaceObject2 p ->
		write_place_object ch p false;
	| TRemoveObject2 depth ->
		write_ui16 ch depth;
	| TShape3 s ->
		write_shape ch s
	| TText2 t ->
		write_text ch t
	| TButton2 b ->
		write_button2 ch b
	| TBitsJPEG3 b ->
		write_ui16 ch b.jp3_id;
		write_i32 ch (String.length b.jp3_data + String.length b.jp3_table);
		nwrite ch b.jp3_table;
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
	| TProductInfo s ->
		nwrite ch s
	| TFrameLabel (label,id) ->
		write_string ch label;
		opt (write ch) id;
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
	| TVideoStream s ->
		nwrite ch s
	| TVideoFrame s ->
		nwrite ch s
	| TDebugID s ->
		nwrite ch s
	| TEnableDebugger2 (tag,pass) ->
		write_ui16 ch tag;
		write_string ch pass
	| TScriptLimits (recursion_depth, script_timeout) ->
		write_ui16 ch recursion_depth;
		write_ui16 ch script_timeout;
	| TSandbox s ->
		write_i32 ch (match s with
		| SBLocal -> 0
		| SBNetwork -> 1
		| SBUnknown n -> n)
	| TPlaceObject3 p ->
		write_place_object ch p true;
	| TFontGlyphs f ->
		write_font_glyphs ch f
	| TTextInfo s ->
		nwrite ch s
	| TFont3 f ->
		write_font3 ch f
	| TF9Classes l ->
		write_ui16 ch (List.length l);
		List.iter (fun c ->
			write_ui16 ch (match c.f9_cid with None -> 0 | Some id -> id);
			write_string ch c.f9_classname
		) l
	| TMetaData meta ->
		nwrite ch meta
	| TActionScript3 (id,a) ->
		(match id with
		| None -> ()
		| Some (id,frame) ->
			write_i32 ch id;
			write_string ch frame;
		);
		As3parse.write ch a
	| TShape4 s ->
		write_shape ch s
	| TShape5 (id,s) ->
		write_ui16 ch id;
		nwrite ch s
	| TF9Scene s ->
		write_ui16 ch 1;
		write_string ch s;
		write_byte ch 0;
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
	let len = len + 4 + 4 + rect_length h.h_size + 2 + 2 in
	write_i32 ch len;
	let ch = (if h.h_compressed then deflate ch else ch) in
	write_rect ch h.h_size;
	write_ui16 ch h.h_fps;
	write_ui16 ch h.h_frame_count;
	List.iter (write_tag ch) tags;
	write_tag ch tag_end;
	flush ch

let init inflate deflate =
	Swf.__parser := parse;
	Swf.__printer := write;
	Swf.__inflate := inflate;
	Swf.__deflate := deflate;

;;
Swf.__parser := parse;
Swf.__printer := write