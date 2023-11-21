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
open IO

type ctx = {
	file : Stdlib.in_channel;
	ch : input;
	mutable entry : entry;
}

let rd16 = BigEndian.read_i16
let rdu16 = BigEndian.read_ui16
let rd32 = BigEndian.read_i32
let rd32r = BigEndian.read_real_i32

let parse_header ctx =
	let ch = ctx.ch in
	let major_version = rdu16 ch in
	let minor_version = rdu16 ch in
	let num_tables = rdu16 ch in
	let search_range = rdu16 ch in
	let entry_selector = rdu16 ch in
	let range_shift = rdu16 ch in
	{
		hd_major_version = major_version;
		hd_minor_version = minor_version;
		hd_num_tables = num_tables;
		hd_search_range = search_range;
		hd_entry_selector = entry_selector;
		hd_range_shift = range_shift;
	}

let parse_directory ctx header =
	let ch = ctx.ch in
	let directory = Hashtbl.create 0 in
	for i = 0 to header.hd_num_tables - 1 do
		let name = nread_string ch 4 in
		let cs = rd32r ch in
		let off = rd32r ch in
		let length = rd32r ch in
		Hashtbl.add directory name {
			entry_table_name = name;
			entry_checksum = cs;
			entry_offset = off;
			entry_length = length;
		}
	done;
	directory

let parse_head_table ctx =
	let ch = ctx.ch in
	let version = rd32r ch in
	let font_revision = rd32r ch in
	let checksum_adjustment = rd32r ch in
	let magic_number = rd32r ch in
	let flags = rdu16 ch in
	let units_per_em = rdu16 ch in
	let created = BigEndian.read_double ch in
	let modified = BigEndian.read_double ch in
	let xmin = rd16 ch in
	let ymin = rd16 ch in
	let xmax = rd16 ch in
	let ymax = rd16 ch in
	let mac_style = rdu16 ch in
	let lowest_rec_ppem = rdu16 ch in
	let font_direction_hint = rd16 ch in
	let index_to_loc_format = rd16 ch in
	let glyph_data_format = rd16 ch in
	{
		hd_version = version;
		hd_font_revision = font_revision;
		hd_checksum_adjustment = checksum_adjustment;
		hd_magic_number = magic_number;
		hd_flags = flags;
		hd_units_per_em = units_per_em;
		hd_created = created;
		hd_modified = modified;
		hd_xmin = xmin;
		hd_ymin = ymin;
		hd_xmax = xmax;
		hd_ymax = ymax;
		hd_mac_style = mac_style;
		hd_lowest_rec_ppem = lowest_rec_ppem;
		hd_font_direction_hint = font_direction_hint;
		hd_index_to_loc_format = index_to_loc_format;
		hd_glyph_data_format = glyph_data_format;
	}

let parse_hhea_table ctx =
	let ch = ctx.ch in
	let version = rd32r ch in
	let ascender = rd16 ch in
	let descender = rd16 ch in
	let line_gap = rd16 ch in
	let advance_width_max = rdu16 ch in
	let min_left_side_bearing = rd16 ch in
	let min_right_side_bearing = rd16 ch in
	let x_max_extent = rd16 ch in
	let caret_slope_rise = rd16 ch in
	let caret_slope_run = rd16 ch in
	let caret_offset = rd16 ch in
	let reserved = nread_string ch 8 in
	let metric_data_format = rd16 ch in
	let number_of_hmetrics = rdu16 ch in
	{
		hhea_version = version;
		hhea_ascent = ascender;
		hhea_descent = descender;
		hhea_line_gap = line_gap;
		hhea_advance_width_max = advance_width_max;
		hhea_min_left_side_bearing = min_left_side_bearing;
		hhea_min_right_side_bearing = min_right_side_bearing;
		hhea_x_max_extent = x_max_extent;
		hhea_caret_slope_rise = caret_slope_rise;
		hhea_caret_slope_run = caret_slope_run;
		hhea_caret_offset = caret_offset;
		hhea_reserved = reserved;
		hhea_metric_data_format = metric_data_format;
		hhea_number_of_hmetrics = number_of_hmetrics;
	}

let parse_maxp_table ctx =
	let ch = ctx.ch in
	let version_number = rd32r ch in
	let num_glyphs = rdu16 ch in
	let max_points = rdu16 ch in
	let max_contours = rdu16 ch in
	let max_component_points = rdu16 ch in
	let max_component_contours = rdu16 ch in
	let max_zones = rdu16 ch in
	let max_twilight_points = rdu16 ch in
	let max_storage = rdu16 ch in
	let max_function_defs = rdu16 ch in
	let max_instruction_defs = rdu16 ch in
	let max_stack_elements = rdu16 ch in
	let max_size_of_instructions = rdu16 ch in
	let max_component_elements = rdu16 ch in
	let max_component_depth = rdu16 ch in
	{
		maxp_version_number = version_number;
		maxp_num_glyphs = num_glyphs;
		maxp_max_points = max_points;
		maxp_max_contours = max_contours;
		maxp_max_component_points = max_component_points;
		maxp_max_component_contours = max_component_contours;
		maxp_max_zones = max_zones;
		maxp_max_twilight_points = max_twilight_points;
		maxp_max_storage = max_storage;
		maxp_max_function_defs = max_function_defs;
		maxp_max_instruction_defs = max_instruction_defs;
		maxp_max_stack_elements = max_stack_elements;
		maxp_max_size_of_instructions = max_size_of_instructions;
		maxp_max_component_elements = max_component_elements;
		maxp_max_component_depth = max_component_depth;
	}

let parse_loca_table head maxp ctx =
	let ch = ctx.ch in
	if head.hd_index_to_loc_format = 0 then
		Array.init (maxp.maxp_num_glyphs + 1) (fun _ -> Int32.of_int ((rdu16 ch) * 2))
	else
		Array.init (maxp.maxp_num_glyphs + 1) (fun _ -> rd32r ch)

let parse_hmtx_table maxp hhea ctx =
	let ch = ctx.ch in
	let last_advance_width = ref 0 in (* check me 1/2*)
	Array.init maxp.maxp_num_glyphs (fun i ->
		let advance_width = if i > hhea.hhea_number_of_hmetrics-1 then (* check me 2/2*)
			!last_advance_width
		else
			rdu16 ch
		in
		last_advance_width := advance_width;
		let left_side_bearing = rd16 ch in
		{
			advance_width = advance_width;
			left_side_bearing = left_side_bearing;
		}
	)

let parse_cmap_table ctx =
	let ch = ctx.ch in
	let version = rdu16 ch in
	let num_subtables = rdu16 ch in
	let dir = ExtList.List.init num_subtables (fun _ ->
		let platform_id = rdu16 ch in
		let platform_specific_id = rdu16 ch in
		let offset = rd32r ch in
		{
			csh_platform_id = platform_id;
			csh_platform_specific_id = platform_specific_id;
			csh_offset = offset;
		}
	) in
	let dir = List.stable_sort (fun csh1 csh2 ->
		if csh1.csh_platform_id < csh2.csh_platform_id then -1
		else if csh1.csh_platform_id > csh2.csh_platform_id then 1
		else compare csh1.csh_platform_specific_id csh2.csh_platform_specific_id
	) dir in
	let parse_sub entry =
		seek_in ctx.file ((Int32.to_int ctx.entry.entry_offset) + (Int32.to_int entry.csh_offset));
		let format = rdu16 ch in
		let def = match format with
			| 0 ->
				let length = rdu16 ch in
				let language = rdu16 ch in
				let glyph_index = Array.init 256 (fun _ -> read ch) in
				Cmap0 {
					c0_format = 0;
					c0_length = length;
					c0_language = language;
					c0_glyph_index_array = glyph_index;
				}
			| 4 ->
				let length = rdu16 ch in
				let language = rdu16 ch in
				let seg_count_x2 = rdu16 ch in
				let seg_count = seg_count_x2 / 2 in
				let search_range = rdu16 ch in
				let entry_selector = rdu16 ch in
				let range_shift = rdu16 ch in
				let end_code = Array.init seg_count (fun _ -> rdu16 ch) in
				let reserved = rdu16 ch in
				assert (reserved = 0);
				let start_code = Array.init seg_count (fun _ -> rdu16 ch) in
				let id_delta = Array.init seg_count (fun _ -> rdu16 ch) in
				let id_range_offset = Array.init seg_count (fun _ -> rdu16 ch) in
				let count = (length - (8 * seg_count + 16)) / 2 in
				let glyph_index = Array.init count (fun _ -> rdu16 ch) in
				Cmap4 {
					c4_format = format;
					c4_length = length;
					c4_language = language;
					c4_seg_count_x2 = seg_count_x2;
					c4_search_range = search_range;
					c4_entry_selector = entry_selector;
					c4_range_shift = range_shift;
					c4_end_code = end_code;
					c4_reserved_pad = reserved;
					c4_start_code = start_code;
					c4_id_delta = id_delta;
					c4_id_range_offset = id_range_offset;
					c4_glyph_index_array = glyph_index;
				}
			| 6 ->
				let length = rdu16 ch in
				let language = rdu16 ch in
				let first_code = rdu16 ch in
				let entry_count = rdu16 ch in
				let glyph_index = Array.init entry_count (fun _ -> rdu16 ch) in
				Cmap6 {
					c6_format = format;
					c6_length = length;
					c6_language = language;
					c6_first_code = first_code;
					c6_entry_count = entry_count;
					c6_glyph_index_array = glyph_index;
				}
  			| 12 ->
				ignore (rd16 ch);
				let length = rd32r ch in
				let language = rd32r ch in
				let num_groups = rd32r ch in
				let groups = ExtList.List.init (Int32.to_int num_groups) (fun _ ->
					let start = rd32r ch in
					let stop = rd32r ch in
					let start_glyph = rd32r ch in
					{
						c12g_start_char_code = start;
						c12g_end_char_code = stop;
						c12g_start_glyph_code = start_glyph;
					}
				) in
				Cmap12 {
					c12_format = Int32.of_int 12;
					c12_length = length;
					c12_language = language;
					c12_num_groups = num_groups;
					c12_groups = groups;
				}
			| x ->
				failwith ("Not implemented format: " ^ (string_of_int x));
		in
		{
			cs_def = def;
			cs_header = entry;
		}

	in
	{
		cmap_version = version;
		cmap_num_subtables = num_subtables;
		cmap_subtables = List.map parse_sub dir;
	}

let parse_glyf_table maxp loca cmap hmtx ctx =
	let ch = ctx.ch in
	let parse_glyf i =
		seek_in ctx.file ((Int32.to_int ctx.entry.entry_offset) + (Int32.to_int loca.(i)));
		let num_contours = rd16 ch in
		let xmin = rd16 ch in
		let ymin = rd16 ch in
		let xmax = rd16 ch in
		let ymax = rd16 ch in
		let header = {
			gh_num_contours = num_contours;
			gh_xmin = xmin;
			gh_ymin = ymin;
			gh_xmax = xmax;
			gh_ymax = ymax;
		} in
		if num_contours >= 0 then begin
			let num_points = ref 0 in
			let end_pts_of_contours = Array.init num_contours (fun i ->
				let v = rdu16 ch in
				if i = num_contours - 1 then num_points := v + 1;
				v
			) in
			let instruction_length = rdu16 ch in
			let instructions = Array.init instruction_length (fun _ ->
				read ch
			) in
			let flags = DynArray.create () in
			let rec loop index =
				if index >= !num_points then () else begin
					let v = read_byte ch in
					let incr = if (v land 8) == 0 then begin
						DynArray.add flags v;
						1
					end else begin
						let r = (int_of_char (read ch)) in
						for i = 0 to r do DynArray.add flags v done;
						r + 1
					end in
					loop (index + incr)
				end
			in
			loop 0;
			assert (DynArray.length flags = !num_points);
			let x_coordinates = Array.init !num_points (fun i ->
				let flag = DynArray.get flags i in
				if flag land 0x10 <> 0 then begin
					if flag land 0x02 <> 0 then read_byte ch
					else 0
				end else begin
					if flag land 0x02 <> 0 then -read_byte ch
					else rd16 ch
				end
			) in
			let y_coordinates = Array.init !num_points (fun i ->
				let flag = DynArray.get flags i in
				if flag land 0x20 <> 0 then begin
					if flag land 0x04 <> 0 then read_byte ch
					else 0
				end else begin
					if flag land 0x04 <> 0 then -read_byte ch
					else rd16 ch
				end;
			) in
			TGlyfSimple (header, {
				gs_end_pts_of_contours = end_pts_of_contours;
				gs_instruction_length = instruction_length;
				gs_instructions = instructions;
				gs_flags = DynArray.to_array flags;
				gs_x_coordinates = x_coordinates;
				gs_y_coordinates = y_coordinates;
			})
		end else if num_contours = -1 then begin
			let acc = DynArray.create () in
			let rec loop () =
				let flags = rdu16 ch in
				let glyph_index = rdu16 ch in
				let arg1,arg2 = if flags land 1 <> 0 then begin
					let arg1 = rd16 ch in
					let arg2 = rd16 ch in
					arg1,arg2
				end else begin
					let arg1 = read_byte ch in
					let arg2 = read_byte ch in
					arg1,arg2
				end in
				let fmt214 i = (float_of_int i) /. (float_of_int 0x4000) in
				let fmode =	if flags land 8 <> 0 then
					Scale (fmt214 (rd16 ch))
				else if flags land 64 <> 0 then begin
					let s1 = fmt214 (rd16 ch) in
					let s2 = fmt214 (rd16 ch) in
					ScaleXY (s1,s2)
				end else if flags land 128 <> 0 then begin
					let a = fmt214 (rd16 ch) in
					let b = fmt214 (rd16 ch) in
					let c = fmt214 (rd16 ch) in
					let d = fmt214 (rd16 ch) in
					ScaleMatrix (a,b,c,d)
				end else
					NoScale
				in
				DynArray.add acc {
					gc_flags = flags;
					gc_glyf_index = glyph_index;
					gc_arg1 = if flags land 2 <> 0 then arg1 else 0;
					gc_arg2 = if flags land 2 <> 0 then arg2 else 0;
					gc_transformation = fmode;
				};
				if flags land 0x20 <> 0 then loop ();
			in
			loop ();
			TGlyfComposite (header,(DynArray.to_list acc))
		end else
			failwith "Unknown Glyf"
	in
	Array.init maxp.maxp_num_glyphs (fun i ->
		let len = (Int32.to_int loca.(i + 1)) - (Int32.to_int loca.(i)) in
		if len > 0 then parse_glyf i else TGlyfNull
	)

let parse_kern_table ctx =
	let ch = ctx.ch in
	let version = Int32.of_int (rd16 ch) in
	let num_tables = Int32.of_int (rd16 ch) in
	let tables = ExtList.List.init (Int32.to_int num_tables) (fun _ ->
		let length = Int32.of_int (rdu16 ch) in
		let tuple_index = rdu16 ch in
		let coverage = rdu16 ch in
		let def = match coverage lsr 8 with
		| 0 ->
			let num_pairs = rdu16 ch in
			let search_range = rdu16 ch in
			let entry_selector = rdu16 ch in
			let range_shift = rdu16 ch in
			let kerning_pairs = ExtList.List.init num_pairs (fun _ ->
				let left = rdu16 ch in
				let right = rdu16 ch in
				let value = rd16 ch in
				{
					kern_left = left;
					kern_right = right;
					kern_value = value;
				}
			) in
			Kern0 {
				k0_num_pairs = num_pairs;
				k0_search_range = search_range;
				k0_entry_selector = entry_selector;
				k0_range_shift = range_shift;
				k0_pairs = kerning_pairs;
			}
		| 2 ->
			let row_width = rdu16 ch in
			let left_offset_table = rdu16 ch in
			let right_offset_table = rdu16 ch in
			let array_offset = rdu16 ch in
			let first_glyph = rdu16 ch in
			let num_glyphs = rdu16 ch in
			let offsets = ExtList.List.init num_glyphs (fun _ ->
				rdu16 ch
			) in
			Kern2 {
				k2_row_width = row_width;
				k2_left_offset_table = left_offset_table;
				k2_right_offset_table = right_offset_table;
				k2_array = array_offset;
				k2_first_glyph = first_glyph;
				k2_num_glyphs = num_glyphs;
				k2_offsets = offsets;
			}
		| i ->
			failwith ("Unknown kerning: " ^ (string_of_int i));
		in
		{
			ks_def = def;
			ks_header = {
				ksh_length = length;
				ksh_coverage = coverage;
				ksh_tuple_index = tuple_index;
			}
		}
	) in
	{
		kern_version = version;
		kern_num_tables = num_tables;
		kern_subtables = tables;
	}

let parse_name_table ctx =
	let ch = ctx.ch in
	let format = rdu16 ch in
	let num_records = rdu16 ch in
	let offset = rdu16 ch in
	let records = Array.init num_records (fun _ ->
		let platform_id = rdu16 ch in
		let platform_specific_id = rdu16 ch in
		let language_id = rdu16 ch in
		let name_id = rdu16 ch in
		let length = rdu16 ch in
		let offset = rdu16 ch in
		{
			nr_platform_id = platform_id;
			nr_platform_specific_id = platform_specific_id;
			nr_language_id = language_id;
			nr_name_id = name_id;
			nr_length = length;
			nr_offset = offset;
			nr_value = "";
		}
	) in
	let ttf_name = ref "" in
	(* TODO: use real utf16 conversion *)
	let set_name n =
		let l = ExtList.List.init (String.length n / 2) (fun i -> String.make 1 n.[i * 2 + 1]) in
		ttf_name := String.concat "" l
	in
	let records = Array.map (fun r ->
		seek_in ctx.file ((Int32.to_int ctx.entry.entry_offset) + offset + r.nr_offset);
		r.nr_value <- nread_string ch r.nr_length;
		if r.nr_name_id = 4 && r.nr_platform_id = 3 || r.nr_platform_id = 0 then set_name r.nr_value;
		r
	) records in
	{
		name_format = format;
		name_num_records = num_records;
		name_offset = offset;
		name_records = records;
	},!ttf_name

let parse_os2_table ctx =
	let ch = ctx.ch in
	let version = rdu16 ch in
	let x_avg_char_width = rd16 ch in
	let us_weight_class = rdu16 ch in
	let us_width_class = rdu16 ch in
	let fs_type = rd16 ch in
	let y_subscript_x_size = rd16 ch in
	let y_subscript_y_size = rd16 ch in
	let y_subscript_x_offset = rd16 ch in
	let y_subscript_y_offset = rd16 ch in
	let y_superscript_x_size = rd16 ch in
	let y_superscript_y_size = rd16 ch in
	let y_superscript_x_offset = rd16 ch in
	let y_superscript_y_offset = rd16 ch in
	let y_strikeout_size = rd16 ch in
	let y_strikeout_position = rd16 ch in
	let s_family_class = rd16 ch in

	let b_family_type = read_byte ch in
	let b_serif_style = read_byte ch in
	let b_weight = read_byte ch in
	let b_proportion = read_byte ch in
	let b_contrast = read_byte ch in
	let b_stroke_variation = read_byte ch in
	let b_arm_style = read_byte ch in
	let b_letterform = read_byte ch in
	let b_midline = read_byte ch in
	let b_x_height = read_byte ch in

	let ul_unicode_range_1 = rd32r ch in
	let ul_unicode_range_2 = rd32r ch in
	let ul_unicode_range_3 = rd32r ch in
	let ul_unicode_range_4 = rd32r ch in
	let ach_vendor_id = rd32r ch in
	let fs_selection = rd16 ch in
	let us_first_char_index = rdu16 ch in
	let us_last_char_index = rdu16 ch in
	let s_typo_ascender = rd16 ch in
	let s_typo_descender = rd16 ch in
	let s_typo_line_gap = rd16 ch in
	let us_win_ascent = rdu16 ch in
	let us_win_descent = rdu16 ch in
	{
		os2_version = version;
		os2_x_avg_char_width = x_avg_char_width;
		os2_us_weight_class = us_weight_class;
		os2_us_width_class = us_width_class;
		os2_fs_type = fs_type;
		os2_y_subscript_x_size = y_subscript_x_size;
		os2_y_subscript_y_size = y_subscript_y_size;
		os2_y_subscript_x_offset = y_subscript_x_offset;
		os2_y_subscript_y_offset = y_subscript_y_offset;
		os2_y_superscript_x_size = y_superscript_x_size;
		os2_y_superscript_y_size = y_superscript_y_size;
		os2_y_superscript_x_offset = y_superscript_x_offset;
		os2_y_superscript_y_offset = y_superscript_y_offset;
		os2_y_strikeout_size = y_strikeout_size;
		os2_y_strikeout_position = y_strikeout_position;
		os2_s_family_class = s_family_class;
		os2_b_family_type = b_family_type;
		os2_b_serif_style = b_serif_style;
		os2_b_weight = b_weight;
		os2_b_proportion = b_proportion;
		os2_b_contrast = b_contrast;
		os2_b_stroke_variation = b_stroke_variation;
		os2_b_arm_style = b_arm_style;
		os2_b_letterform = b_letterform;
		os2_b_midline = b_midline;
		os2_b_x_height = b_x_height;
		os2_ul_unicode_range_1 = ul_unicode_range_1;
		os2_ul_unicode_range_2 = ul_unicode_range_2;
		os2_ul_unicode_range_3 = ul_unicode_range_3;
		os2_ul_unicode_range_4 = ul_unicode_range_4;
		os2_ach_vendor_id = ach_vendor_id;
		os2_fs_selection = fs_selection;
		os2_us_first_char_index = us_first_char_index;
		os2_us_last_char_index = us_last_char_index;
		os2_s_typo_ascender = s_typo_ascender;
		os2_s_typo_descender = s_typo_descender;
		os2_s_typo_line_gap = s_typo_line_gap;
		os2_us_win_ascent = us_win_ascent;
		os2_us_win_descent = us_win_descent;
	}

let parse file : ttf =
	let ctx = {
		file = file;
		ch = input_channel file;
		entry = {
			entry_table_name = "";
			entry_offset = Int32.of_int 0;
			entry_length = Int32.of_int 0;
			entry_checksum = Int32.of_int 0;
		}
	} in
	let header = parse_header ctx in
	let directory = parse_directory ctx header in
	let parse_table entry f =
		seek_in file (Int32.to_int entry.entry_offset);
		ctx.entry <- entry;
		f ctx
	in
	let parse_req_table name f =
		try
			let entry = Hashtbl.find directory name in
			parse_table entry f
		with Not_found ->
			failwith (Printf.sprintf "Required table %s could not be found" name)
	in
	let parse_opt_table name f =
		try
			let entry = Hashtbl.find directory name in
			Some (parse_table entry f)
		with Not_found ->
			None
	in
	let head = parse_req_table "head" parse_head_table in
	let hhea = parse_req_table "hhea" parse_hhea_table in
	let maxp = parse_req_table "maxp" parse_maxp_table in
	let loca = parse_req_table "loca" (parse_loca_table head maxp) in
	let hmtx = parse_req_table "hmtx" (parse_hmtx_table maxp hhea) in
	let cmap = parse_req_table "cmap" (parse_cmap_table) in
	let glyfs = parse_req_table "glyf" (parse_glyf_table maxp loca cmap hmtx) in
	let kern = parse_opt_table "kern" (parse_kern_table) in
	let name,ttf_name = parse_req_table "name" (parse_name_table) in
	let os2 = parse_req_table "OS/2" (parse_os2_table) in
	{
		ttf_header = header;
		ttf_font_name = ttf_name;
		ttf_directory = directory;
		ttf_head = head;
		ttf_hhea = hhea;
		ttf_maxp = maxp;
		ttf_loca = loca;
		ttf_hmtx = hmtx;
		ttf_cmap = cmap;
		ttf_glyfs = glyfs;
		ttf_name = name;
		ttf_os2 = os2;
		ttf_kern = kern;
	}
