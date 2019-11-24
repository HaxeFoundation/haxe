(*
 * Copyright (C)2005-2014 Haxe Foundation
 *
 * Permission is hereby granted, free of charge, to any person obtaining a
 * copy of this software and associated documentation files (the "Software"),
 * to deal in the Software without restriction, including without limitation
 * the rights to use, copy, modify, merge, publish, distribute, sublicense,
 * and/or sell copies of the Software, and to permit persons to whom the
 * Software is furnished to do so, subject to the following conditions:
 *
 * The above copyright notice and this permission notice shall be included in
 * all copies or substantial portions of the Software.
 *
 * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
 * IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
 * FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
 * AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
 * LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING
 * FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER
 * DEALINGS IN THE SOFTWARE.
 *)

open TTFData
open Swf

let num_bits x =
	if x = 0 then
		0
	else
		let rec loop n v =
			if v = 0 then n else loop (n + 1) (v lsr 1)
		in
		loop 1 (abs x)

let round x = int_of_float (floor (x +. 0.5))

let to_twips v = round (v *. 20.)

type ctx = {
	ttf : ttf;
}

let begin_fill =
	SRStyleChange {
		scsr_move = None;
		scsr_fs0 = Some(1);
		scsr_fs1 = None;
		scsr_ls = None;
		scsr_new_styles = None;
	}

let end_fill =
	SRStyleChange {
		scsr_move = None;
		scsr_fs0 = None;
		scsr_fs1 = None;
		scsr_ls = None;
		scsr_new_styles = None;
	}

let align_bits x nbits = x land ((1 lsl nbits ) - 1)

let move_to ctx x y =
	let x = to_twips x in
	let y = to_twips y in
	let nbits = max (num_bits x) (num_bits y) in
	SRStyleChange {
		scsr_move = Some (nbits, align_bits x nbits, align_bits y nbits);
		scsr_fs0 = Some(1);
		scsr_fs1 = None;
		scsr_ls = None;
		scsr_new_styles = None;
	}

let line_to ctx x y =
	let x = to_twips x in
	let y = to_twips y in
	if x = 0 && y = 0 then raise Exit;
	let nbits = max (num_bits x) (num_bits y) in
	SRStraightEdge {
		sser_nbits = nbits;
		sser_line = (if x = 0 then None else Some(align_bits x nbits)), (if y = 0 then None else Some(align_bits y nbits));
	}

let curve_to ctx cx cy ax ay =
	let cx = to_twips cx in
	let cy = to_twips cy in
	let ax = to_twips ax in
	let ay = to_twips ay in
	let nbits = max (max (num_bits cx) (num_bits cy)) (max (num_bits ax) (num_bits ay)) in
	SRCurvedEdge {
		scer_nbits = nbits;
		scer_cx = align_bits cx nbits;
		scer_cy = align_bits cy nbits;
		scer_ax = align_bits ax nbits;
		scer_ay = align_bits ay nbits;
	}

open TTFTools

let write_paths ctx paths =
	let scale = 1024. /. (float_of_int ctx.ttf.ttf_head.hd_units_per_em) in
	let srl = DynArray.create () in
	List.iter (fun path ->
		try
			DynArray.add srl (match path.gp_type with
			| 0 -> move_to ctx (path.gp_x *. scale) ((-1.) *. path.gp_y *. scale);
			| 1 -> line_to ctx (path.gp_x *. scale) ((-1.) *. path.gp_y *. scale);
			| 2 -> curve_to ctx (path.gp_cx *. scale) ((-1.) *. path.gp_cy *. scale) (path.gp_x *. scale) ((-1.) *. path.gp_y *. scale);
			| _ -> assert false)
		with Exit ->
			()
	) paths;
	DynArray.add srl (end_fill);
	{
		srs_nfbits = 1;
		srs_nlbits = 0;
		srs_records = DynArray.to_list srl;
	}

let rec write_glyph ctx key glyf =
	{
		font_char_code = key;
		font_shape = write_paths ctx (TTFTools.build_glyph_paths ctx.ttf true glyf);
	}

let write_font_layout ctx lut =
	let scale = 1024. /. (float_of_int ctx.ttf.ttf_head.hd_units_per_em) in
	let hmtx = Hashtbl.fold (fun k v acc -> (k,ctx.ttf.ttf_hmtx.(v)) :: acc) lut [] in
	let hmtx = List.stable_sort (fun a b -> compare (fst a) (fst b)) hmtx in
	let hmtx = List.map (fun (k,g) -> g) hmtx in
	{
			font_ascent = round((float_of_int ctx.ttf.ttf_os2.os2_us_win_ascent) *. scale *. 20.);
			font_descent = round((float_of_int ctx.ttf.ttf_os2.os2_us_win_descent) *. scale *. 20.);
			font_leading = round(((float_of_int(ctx.ttf.ttf_os2.os2_us_win_ascent + ctx.ttf.ttf_os2.os2_us_win_descent - ctx.ttf.ttf_head.hd_units_per_em)) *. scale) *. 20.);
			font_glyphs_layout = Array.of_list( ExtList.List.mapi (fun i h ->
			{
				font_advance = round((float_of_int h.advance_width) *. scale *. 20.);
				font_bounds = {rect_nbits=0; left=0; right=0; top=0; bottom=0};
			}) hmtx );
			font_kerning = [];
	}

let bi v = if v then 1 else 0

let int_from_langcode lc =
	match lc with
	| LCNone -> 0
	| LCLatin -> 1
	| LCJapanese -> 2
	| LCKorean -> 3
	| LCSimplifiedChinese -> 4
	| LCTraditionalChinese -> 5

let write_font2 ch b f2 =
	IO.write_bits b 1 (bi true);
	IO.write_bits b 1 (bi f2.font_shift_jis);
	IO.write_bits b 1 (bi f2.font_is_small);
	IO.write_bits b 1 (bi f2.font_is_ansi);
	IO.write_bits b 1 (bi f2.font_wide_offsets);
	IO.write_bits b 1 (bi f2.font_wide_codes);
	IO.write_bits b 1 (bi f2.font_is_italic);
	IO.write_bits b 1 (bi f2.font_is_bold);
	IO.write_byte ch (int_from_langcode f2.font_language);
	IO.write_byte ch (String.length f2.font_name);
	IO.nwrite_string ch f2.font_name;
	IO.write_ui16 ch (Array.length f2.font_glyphs);
	let glyph_offset = ref (((Array.length f2.font_glyphs) * 4)+4) in
	Array.iter (fun g ->
		IO.write_i32 ch !glyph_offset;
		glyph_offset := !glyph_offset + SwfParser.font_shape_records_length g.font_shape;
	)f2.font_glyphs;
	IO.write_i32 ch !glyph_offset;
	Array.iter (fun g -> SwfParser.write_shape_without_style ch g.font_shape;) f2.font_glyphs;
	Array.iter (fun g -> IO.write_ui16 ch g.font_char_code; )f2.font_glyphs;
	IO.write_i16 ch f2.font_layout.font_ascent;
	IO.write_i16 ch f2.font_layout.font_descent;
	IO.write_i16 ch f2.font_layout.font_leading;
	Array.iter (fun g ->
		let fa = ref g.font_advance in
		if (!fa) <  -32767 then fa := -32768;(* fix or check *)
		if (!fa) > 32766 then fa := 32767;
		IO.write_i16 ch !fa;) f2.font_layout.font_glyphs_layout;
	Array.iter (fun g -> SwfParser.write_rect ch g.font_bounds;) f2.font_layout.font_glyphs_layout;
	IO.write_ui16 ch 0 (* TODO: optional FontKerningTable *)

let to_swf ttf config =
	let ctx = {
		ttf = ttf;
	} in
	let lut = TTFTools.build_lut ttf config.ttfc_range_str in
	let glyfs = Hashtbl.fold (fun k v acc -> (k,ctx.ttf.ttf_glyfs.(v)) :: acc) lut [] in
	let glyfs = List.stable_sort (fun a b -> compare (fst a) (fst b)) glyfs in
	let glyfs = List.map (fun (k,g) -> write_glyph ctx k g) glyfs in
	let glyfs_font_layout = write_font_layout ctx lut in
	let glyfs = Array.of_list glyfs in
	{
		font_shift_jis = false;
		font_is_small = false;
		font_is_ansi = false;
		font_wide_offsets = true;
		font_wide_codes = true;
		font_is_italic = config.ttfc_font_posture = TFPItalic;
		font_is_bold = config.ttfc_font_weight = TFWBold;
		font_language = LCNone;
		font_name = (match config.ttfc_font_name with Some s -> s | None -> ttf.ttf_font_name);
		font_glyphs = glyfs;
		font_layout = glyfs_font_layout;
	}
;;
