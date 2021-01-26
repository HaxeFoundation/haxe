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

type header = {
	hd_major_version : int;
	hd_minor_version : int;
	hd_num_tables : int;
	hd_search_range : int;
	hd_entry_selector : int;
	hd_range_shift : int;
}

type entry = {
	entry_table_name : string;
	entry_checksum : int32;
	entry_offset : int32;
	entry_length: int32;
}

(* GLYF *)

type glyf_header = {
	gh_num_contours : int;
	gh_xmin : int;
	gh_ymin : int;
	gh_xmax : int;
	gh_ymax : int;
}

type glyf_simple = {
	gs_end_pts_of_contours : int array;
	gs_instruction_length : int;
	gs_instructions : char array;
	gs_flags : int array;
	gs_x_coordinates : int array;
	gs_y_coordinates : int array;
}

type transformation_option =
	| NoScale
	| Scale of float
	| ScaleXY of float * float
	| ScaleMatrix of float * float * float * float

type glyf_component = {
	gc_flags : int;
	gc_glyf_index : int;
	gc_arg1 : int;
	gc_arg2 : int;
	gc_transformation : transformation_option;
}

type glyf =
	| TGlyfSimple of glyf_header * glyf_simple
	| TGlyfComposite of glyf_header * glyf_component list
	| TGlyfNull

(* HMTX *)

type hmtx = {
	advance_width : int;
	left_side_bearing : int;
}

(* CMAP *)

type cmap_subtable_header = {
	csh_platform_id : int;
	csh_platform_specific_id : int;
	csh_offset : int32;
}

type cmap_format_0 = {
	c0_format : int;
	c0_length : int;
	c0_language : int;
	c0_glyph_index_array : char array;
}

type cmap_format_4 = {
	c4_format : int;
	c4_length : int;
	c4_language : int;
	c4_seg_count_x2 : int;
	c4_search_range : int;
	c4_entry_selector : int;
	c4_range_shift : int;
	c4_end_code : int array;
	c4_reserved_pad : int;
	c4_start_code : int array;
	c4_id_delta : int array;
	c4_id_range_offset : int array;
	c4_glyph_index_array : int array;
}

type cmap_format_6 = {
	c6_format : int;
	c6_length : int;
	c6_language : int;
	c6_first_code : int;
	c6_entry_count : int;
	c6_glyph_index_array : int array;
}

type cmap_format_12_group = {
	c12g_start_char_code : int32;
	c12g_end_char_code : int32;
	c12g_start_glyph_code : int32;
}

type cmap_format_12 = {
	c12_format : int32;
	c12_length : int32;
	c12_language : int32;
	c12_num_groups : int32;
	c12_groups : cmap_format_12_group list;
}

type cmap_subtable_def =
	| Cmap0 of cmap_format_0
	| Cmap4 of cmap_format_4
	| Cmap6 of cmap_format_6
	| Cmap12 of cmap_format_12
	| CmapUnk of string

type cmap_subtable = {
	cs_header : cmap_subtable_header;
	cs_def : cmap_subtable_def;
}

type cmap = {
	cmap_version : int;
	cmap_num_subtables : int;
	cmap_subtables : cmap_subtable list;
}

(* KERN *)

type kern_subtable_header = {
	ksh_length : int32;
	ksh_coverage : int;
	ksh_tuple_index : int;
}

type kern_pair = {
	kern_left : int;
	kern_right : int;
	kern_value : int;
}

type kern_format_0 = {
	k0_num_pairs : int;
	k0_search_range : int;
	k0_entry_selector : int;
	k0_range_shift : int;
	k0_pairs : kern_pair list;
}

type kern_format_2 = {
	k2_row_width : int;
	k2_left_offset_table : int;
	k2_right_offset_table : int;
	k2_array : int;
	k2_first_glyph : int;
	k2_num_glyphs : int;
	k2_offsets : int list;
}

type kern_subtable_def =
	| Kern0 of kern_format_0
	| Kern2 of kern_format_2

type kern_subtable = {
	ks_header : kern_subtable_header;
	ks_def : kern_subtable_def;
}

type kern = {
	kern_version : int32;
	kern_num_tables : int32;
	kern_subtables : kern_subtable list;
}

(* NAME *)

type name_record = {
	nr_platform_id : int;
	nr_platform_specific_id : int;
	nr_language_id : int;
	nr_name_id : int;
	nr_length : int;
	nr_offset : int;
	mutable nr_value : string;
}

type name = {
	name_format : int;
	name_num_records : int;
	name_offset : int;
	name_records : name_record array;
}

(* HEAD *)

type head = {
	hd_version : int32;
	hd_font_revision : int32;
	hd_checksum_adjustment : int32;
	hd_magic_number : int32;
	hd_flags : int;
	hd_units_per_em : int;
	hd_created : float;
	hd_modified : float;
	hd_xmin : int;
	hd_ymin : int;
	hd_xmax : int;
	hd_ymax : int;
	hd_mac_style : int;
	hd_lowest_rec_ppem : int;
	hd_font_direction_hint : int;
	hd_index_to_loc_format : int;
	hd_glyph_data_format : int;
}

(* HHEA *)

type hhea = {
	hhea_version : int32;
	hhea_ascent : int;
	hhea_descent : int;
	hhea_line_gap : int;
	hhea_advance_width_max : int;
	hhea_min_left_side_bearing : int;
	hhea_min_right_side_bearing : int;
	hhea_x_max_extent : int;
	hhea_caret_slope_rise : int;
	hhea_caret_slope_run : int;
	hhea_caret_offset : int;
	hhea_reserved : string;
	hhea_metric_data_format : int;
	hhea_number_of_hmetrics :int;
}

(* LOCA *)

type loca = int32 array

(* MAXP *)

type maxp = {
	maxp_version_number : int32;
	maxp_num_glyphs : int;
	maxp_max_points : int;
	maxp_max_contours : int;
	maxp_max_component_points : int;
	maxp_max_component_contours : int;
	maxp_max_zones : int;
	maxp_max_twilight_points : int;
	maxp_max_storage : int;
	maxp_max_function_defs : int;
	maxp_max_instruction_defs :int;
	maxp_max_stack_elements : int;
	maxp_max_size_of_instructions :int;
	maxp_max_component_elements :int;
	maxp_max_component_depth :int;
}

(* OS2 *)

type os2 = {
	os2_version : int;
	os2_x_avg_char_width : int;
	os2_us_weight_class : int;
	os2_us_width_class : int;
	os2_fs_type : int;
	os2_y_subscript_x_size : int;
	os2_y_subscript_y_size : int;
	os2_y_subscript_x_offset : int;
	os2_y_subscript_y_offset : int;
	os2_y_superscript_x_size : int;
	os2_y_superscript_y_size : int;
	os2_y_superscript_x_offset : int;
	os2_y_superscript_y_offset : int;
	os2_y_strikeout_size : int;
	os2_y_strikeout_position : int;
	os2_s_family_class : int;
	os2_b_family_type : int;
	os2_b_serif_style : int;
	os2_b_weight : int;
	os2_b_proportion : int;
	os2_b_contrast : int;
	os2_b_stroke_variation : int;
	os2_b_arm_style : int;
	os2_b_letterform : int;
	os2_b_midline : int;
	os2_b_x_height : int;
	os2_ul_unicode_range_1 : int32;
	os2_ul_unicode_range_2 : int32;
	os2_ul_unicode_range_3 : int32;
	os2_ul_unicode_range_4 : int32;
	os2_ach_vendor_id : int32;
	os2_fs_selection : int;
	os2_us_first_char_index : int;
	os2_us_last_char_index : int;
	os2_s_typo_ascender : int;
	os2_s_typo_descender : int;
	os2_s_typo_line_gap : int;
	os2_us_win_ascent : int;
	os2_us_win_descent : int;
}

type ttf = {
	ttf_header : header;
	ttf_font_name : string;
	ttf_directory: (string,entry) Hashtbl.t;
	ttf_glyfs : glyf array;
	ttf_hmtx : hmtx array;
	ttf_cmap : cmap;
	ttf_head : head;
	ttf_loca : loca;
	ttf_hhea : hhea;
	ttf_maxp : maxp;
	ttf_name : name;
	ttf_os2 : os2;
	ttf_kern : kern option;
}

type ttf_font_weight =
	| TFWRegular
	| TFWBold

type ttf_font_posture =
	| TFPNormal
	| TFPItalic

type ttf_config = {
	mutable ttfc_range_str : string;
	mutable ttfc_font_name : string option;
	mutable ttfc_font_weight : ttf_font_weight;
	mutable ttfc_font_posture : ttf_font_posture;
}
