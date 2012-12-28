open IO
open Swf

(* DATA *)

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
	| Scale of float
	| ScaleXY of float * float
	| ScaleMatrix of float * float * float * float

type glyf_component = {
	gc_flags : int;
	gc_glyf_index : int;
	gc_arg1 : int;
	gc_arg2 : int;
	gc_transformations : transformation_option list;
}

type glyf =
	| TglyfSimple of glyf_header * glyf_simple
	| TglyfComposite of glyf_header * glyf_component list
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
	ttf_hmtx : hmtx list;
	ttf_cmap : cmap;
	ttf_head : head;
	ttf_loca : loca;
	ttf_hhea : hhea;
	ttf_maxp : maxp;
	ttf_name : name;
	ttf_os2 : os2;
	ttf_kern : kern option;
}

type ctx = {
	file : Pervasives.in_channel;
	ch : input;
	mutable entry : entry;
}

let rd16 = BigEndian.read_i16
let rdu16 = BigEndian.read_ui16
let rd32 = BigEndian.read_i32
let rd32r = BigEndian.read_real_i32
let rdd = BigEndian.read_double
let ti32 = Int32.of_int
let ti = Int32.to_int

(* PARSING *)

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
		let name = nread ch 4 in
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
	let created = rdd ch in
	let modified = rdd ch in
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
	let reserved = nread ch 8 in
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
		Array.init (maxp.maxp_num_glyphs + 1) (fun _ -> ti32 ((rdu16 ch) * 2))
	else
		Array.init (maxp.maxp_num_glyphs + 1) (fun _ -> rd32r ch)

let parse_hmtx_table maxp hhea ctx =
	let ch = ctx.ch in
	let last_advance_width = ref 0 in (* check me 1/2*)
	ExtList.List.init maxp.maxp_num_glyphs (fun i ->
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
	let parse_sub entry =
		seek_in ctx.file ((ti ctx.entry.entry_offset) + (ti entry.csh_offset));
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
				let count = length - (8 * seg_count + 16) / 2 in
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
(*  			| 12 ->
				let format = rd32r ch in
				let length = rd32r ch in
				let language = rd32r ch in
				let num_groups = rd32r ch in
				let groups = ExtList.List.init num_groups (fun _ ->
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
					c12_format = format;
					c12_length = length;
					c12_language = language;
					c12_num_groups = num_groups;
					c12_groups = groups;
				} *)
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
		cmap_subtables = List.rev_map parse_sub dir;
	}

let parse_glyf_table maxp loca cmap hmtx ctx =
	let ch = ctx.ch in
	let parse_glyf i =
		seek_in ctx.file ((ti ctx.entry.entry_offset) + (ti loca.(i)));
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
			let x = ref 0 in
			let y = ref 0 in
			let x_coordinates = Array.init !num_points (fun i ->
				let flag = DynArray.get flags i in
				if flag land 0x10 <> 0 then begin
					if flag land 0x02 <> 0 then begin x:= !x + read_byte ch; !x end
					else !x
				end else begin
					if flag land 0x02 <> 0 then begin x:= !x -read_byte ch; !x end
					else begin x := !x + rd16 ch; !x end
				end
			) in
			let y_coordinates = Array.init !num_points (fun i ->
				let flag = DynArray.get flags i in
				if flag land 0x20 <> 0 then begin
					if flag land 0x04 <> 0 then begin y:= !y + read_byte ch; !y end
					else !y
				end else begin
					if flag land 0x04 <> 0 then begin y:= !y -read_byte ch; !y end
					else begin y := !y + rd16 ch; !y end
				end;
			) in
			TglyfSimple (header, {
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
				let tmodes = DynArray.create () in
				if flags land 8 <> 0 then DynArray.add tmodes (Scale (fmt214 (rd16 ch)));
				if flags land 64 <> 0 then begin
					let s1 = fmt214 (rd16 ch) in
					let s2 = fmt214 (rd16 ch) in
					DynArray.add tmodes (ScaleXY (s1,s2))
				end;
				if flags land 128 <> 0 then begin
					let a = fmt214 (rd16 ch) in
					let b = fmt214 (rd16 ch) in
					let c = fmt214 (rd16 ch) in
					let d = fmt214 (rd16 ch) in
					DynArray.add tmodes (ScaleMatrix (a,b,c,d))
				end;
				DynArray.add acc {
					gc_flags = flags;
					gc_glyf_index = glyph_index;
					gc_arg1 = arg1;
					gc_arg2 = arg2;
					gc_transformations = DynArray.to_list tmodes;
				}
			in
			loop ();
			TglyfComposite (header,(DynArray.to_list acc))
		end else
			failwith "Unknown Glyf"
	in
	Array.init maxp.maxp_num_glyphs (fun i ->
		let len = (ti loca.(i + 1)) - (ti loca.(i)) in
		if len > 0 then parse_glyf i else TGlyfNull
	)

let parse_kern_table ctx =
	let ch = ctx.ch in
	let version = ti32 (rd16 ch) in
	let num_tables = ti32 (rd16 ch) in
	let tables = ExtList.List.init (ti num_tables) (fun _ ->
		let length = ti32 (rdu16 ch) in
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
	let records = Array.map (fun r ->
		seek_in ctx.file ((ti ctx.entry.entry_offset) + offset + r.nr_offset);
		r.nr_value <- nread ch r.nr_length;
		if r.nr_name_id = 4 && r.nr_platform_id = 3 || r.nr_platform_id = 0 then ttf_name := r.nr_value;
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
			entry_offset = ti32 0;
			entry_length = ti32 0;
			entry_checksum = ti32 0;
		}
	} in
	let header = parse_header ctx in
	let directory = parse_directory ctx header in
	let parse_table entry f =
		seek_in file (ti entry.entry_offset);
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

(* WRITING *)


(* This can stay *)

let _nbits x =
	if x = 0 then
		0
	else
		let x = ref x in
		if !x < 0 then x := -1 * !x;
		let nbits = ref 0 in
		while !x > 0 do
			x := !x lsr 1;
			incr nbits;
		done;
		!nbits + 1

let round x = int_of_float (floor (x +. 0.5))

type write_ctx = {
	ttf : ttf;
}

type glyf_path = {
	gp_type : int;
	gp_x : float;
	gp_y : float;
	gp_cx : float;
	gp_cy : float;
}

let mk_path t x y cx cy = {
	gp_type = t;
	gp_x = x;
	gp_y = y;
	gp_cx = cx;
	gp_cy = cy;
}

type simple_point = {
	x : float;
	y : float;
	on_curve : bool;
	end_point : bool;
}

let build_paths ctx g =
	let len = Array.length g.gs_x_coordinates in
	let init_points () =
		let current_end = ref 0 in
		Array.init len (fun i -> {
			x = float_of_int g.gs_x_coordinates.(i);
			y = float_of_int g.gs_y_coordinates.(i);
			on_curve = g.gs_flags.(i) land 0x01 <> 0;
			end_point =
				if g.gs_end_pts_of_contours.(!current_end) = i then begin
					incr current_end;
					true
				end else
					false;
		})
	in
	let points = init_points () in
	let arr = DynArray.create () in
	let add t x y cx cy = DynArray.add arr (mk_path t x y cx cy) in
	let flush pl =
		let rec flush pl = match pl with
		| c :: a :: [] ->
			add 2 a.x a.y c.x c.y;
		| a :: [] ->
			add 1 a.x a.y 0.0 0.0;
		| c1 :: c2 :: pl ->
			let mid = {
				x = c1.x +. (c2.x -. c1.x) /. 2.0;
				y = c1.y +. (c2.y -. c1.y) /. 2.0;
				on_curve = true;
				end_point = false;
			} in
			add 2 mid.x mid.y c1.x c1.y;
			flush (c2 :: pl)
		| _ ->
			Printf.printf "Fail, len: %i\n" (List.length pl);
		in
		flush (List.rev pl)
	in
	let last = ref None in
	let rec loop new_contour pl index =
		let p = points.(index) in
		let loop pl =
			if p.end_point || index + 1 = len then begin
				match !last with
					| None -> assert false
					| Some p -> flush (p :: pl);
			end;
			if index + 1 = len then
				()
			else
				loop p.end_point pl (index + 1);
		in
		if new_contour then begin
			last := Some p;
			add 0 p.x p.y 0.0 0.0;
			if p.on_curve then begin
				loop []
			end else begin
				Printf.printf "Found off-curve starting point, not sure what to do\n";
				loop [p]
			end
		end else begin
			if not p.on_curve then
				loop (p :: pl)
			else begin
				flush (p :: pl);
				loop []
			end
		end
	in
	loop true [] 0;
	DynArray.to_list arr

let to_float5 v =
	let temp1 = round(v *. 1000.) in
	let diff = temp1 mod 50 in
	let temp2 = if diff < 25 then temp1 - diff else temp1 + (50 - diff) in
	(float_of_int temp2) /. 1000.

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

let move_to ctx x y last_x last_y =
	let x = to_float5 x in
	let y = to_float5 y in
	last_x := x;
	last_y := y;
	let x = round(x *. 20.) in
	let y = round(y *. 20.) in
	let nbits = max (_nbits x) (_nbits y) in
	SRStyleChange {
		scsr_move = Some (nbits, align_bits x nbits, align_bits y nbits);
		scsr_fs0 = Some(1);
		scsr_fs1 = None;
		scsr_ls = None;
		scsr_new_styles = None;
	}

let line_to ctx x y last_x last_y =
	let x = to_float5 x in
	let y = to_float5 y in
	let dx = round((x -. !last_x) *. 20.) in
	let dy = round((y -. !last_y) *. 20.) in
	if dx = 0 && dy = 0 then raise Exit;
	last_x := x;
	last_y := y;
	let nbits = max (_nbits dx) (_nbits dy) in
	SRStraightEdge {
		sser_nbits = nbits;
		sser_line = (if dx = 0 then None else Some(align_bits dx nbits)), (if dy = 0 then None else Some(align_bits dy nbits));
	}

let curve_to ctx cx cy ax ay last_x last_y =
	let cx = to_float5 cx in
	let cy = to_float5 cy in
	let ax = to_float5 ax in
	let ay = to_float5 ay in
	let dcx = round ((cx -. !last_x) *. 20.) in
	let dcy = round ((cy -. !last_y) *. 20.) in
	let dax = round ((ax -. cx) *. 20.) in
	let day = round ((ay -. cy) *. 20.) in
	last_x := ax;
	last_y := ay;
	let nbits = max (max (_nbits dcx) (_nbits dcy)) (max (_nbits dax) (_nbits day)) in
	SRCurvedEdge {
		scer_nbits = nbits;
		scer_cx = align_bits dcx nbits;
		scer_cy = align_bits dcy nbits;
		scer_ax = align_bits dax nbits;
		scer_ay = align_bits day nbits;
	}

let write_paths ctx paths =
	let scale = 1024. /. (float_of_int ctx.ttf.ttf_head.hd_units_per_em) in
	let last_x = ref 0.0 in
	let last_y = ref 0.0 in
	let srl = DynArray.create () in
	List.iter (fun path ->
		try
			DynArray.add srl (match path.gp_type with
			| 0 -> move_to ctx (path.gp_x *. scale) ((-1.) *. path.gp_y *. scale) last_x last_y;
			| 1 -> line_to ctx (path.gp_x *. scale) ((-1.) *. path.gp_y *. scale) last_x last_y;
			| 2 -> curve_to ctx (path.gp_cx *. scale) ((-1.) *. path.gp_cy *. scale) (path.gp_x *. scale) ((-1.) *. path.gp_y *. scale) last_x last_y;
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

let write_glyph ctx key glyf =
	match glyf with
	| TglyfSimple (h,g) ->
		let path = build_paths ctx g in
		{
			font_char_code = key;
			font_shape = write_paths ctx path;
		}
	| TglyfComposite (h,g) ->
		{
			font_char_code = key;
			font_shape = write_paths ctx [];
		}
	| TGlyfNull ->
		{
			font_char_code = key;
			font_shape = write_paths ctx [];
		}

let write_font_layout ctx lut =
	let scale = 1024. /. (float_of_int ctx.ttf.ttf_head.hd_units_per_em) in
	(* check for shorter ocaml *)
	let hmtx = Array.of_list ctx.ttf.ttf_hmtx in
	let hmtx = Hashtbl.fold (fun k v acc -> (k,hmtx.(v)) :: acc) lut [] in
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

let map_char_code cc c4 =
	let index = ref 0 in
	let seg_count = c4.c4_seg_count_x2 / 2 in
	if cc >= 0xFFFF then 0 else begin
		for i = 0 to seg_count - 1 do
			if c4.c4_end_code.(i) >= cc && c4.c4_start_code.(i) <= cc then begin
				if c4.c4_id_range_offset.(i) > 0 then
					let v = c4.c4_id_range_offset.(i)/2 + cc - c4.c4_start_code.(i) - seg_count + i in
					index := c4.c4_glyph_index_array.(v)
				else
					index := (c4.c4_id_delta.(i) + cc) mod 65536
			end
		done;
		!index
	end

let make_cmap4_map ctx acc c4 =
	let seg_count = c4.c4_seg_count_x2 / 2 in
	for i = 0 to seg_count - 1 do
		for j = c4.c4_start_code.(i) to c4.c4_end_code.(i) do
			let index = map_char_code j c4 in
			Hashtbl.replace acc j index
		done;
	done

let bi v = if v then 1 else 0

let int_from_langcode lc =
	match lc with
	| LCNone -> 0
	| LCLatin -> 1
	| LCJapanese -> 2
	| LCKorean -> 3
	| LCSimplifiedChinese -> 4
	| LCTraditionalChinese -> 5

let calculate_tag_size f2 =
	let size = ref(
	2 + (* char id *)
	1 + (* 8 bit flags *)
	1 + (* langcode *)
	1 + (* font name length *)
	(String.length f2.font_name) + (* font name string*)
	2 + (* num glyphs *)
	((Array.length f2.font_glyphs) * 4) + (* offsets table *)
	4 + (* codetable offset *)
	(* glyps shape records : calculated below *)
	((Array.length f2.font_glyphs) * 2) + (* codes table *)
	2 + (* font_ascent *)
	2 + (* font_descent *)
	2 + (* font_leading *)
	((Array.length f2.font_glyphs) * 2) + (* FontAdvanceTable *)
	2 (* kerning count *)
	) in
	Array.iter (fun g -> size := !size + SwfParser.font_shape_records_length g.font_shape;)f2.font_glyphs;(* glyphs shape records *)
	Array.iter (fun g -> size := !size + SwfParser.rect_length {rect_nbits=0; left=0; right=0; top=0; bottom=0};)f2.font_glyphs;(* FontBoundsTable *)
	!size

let print_records f2 =
	let glyph_offset = ref (((Array.length f2.font_glyphs) * 4)+4) in
	print_string ("numglyps: " ^ string_of_int (Array.length f2.font_glyphs) ^ "\n");
	Array.iter (fun g ->
		let character =  if g.font_char_code > 255 then Char.chr(255) else Char.chr(g.font_char_code) in
		let s = String.make 1 character in
		print_string "==================== Glyph start =====================\n";
		print_string ("char code: " ^ string_of_int g.font_char_code ^ ", char: " ^ s ^ "\n");
		print_string ("glyph_offset: " ^ string_of_int !glyph_offset ^ "\n");
		glyph_offset := !glyph_offset + SwfParser.font_shape_records_length g.font_shape;
		print_string ("records: " ^ string_of_int (List.length g.font_shape.srs_records) ^ "\n");
		print_string ("bytes: " ^ string_of_int (SwfParser.font_shape_records_length g.font_shape) ^ "\n\n");
		List.iter (fun r ->
			match r with
			| SRStyleChange s ->
				print_string "SRStyleChange\n";
				(match s.scsr_move, s.scsr_fs0 with
				|None, None -> ();
					print_string "end_fill\n\n";
				|None, Some(v) -> ();
					print_string "begin_fill\n\n";
				|Some(m , x, y), Some(v)  ->
					print_string ("s.mbits: " ^ string_of_int m ^ "\n");
					print_string ("s.scsr_move x: " ^ string_of_int x ^ "\n");
					print_string ("s.scsr_move y: " ^ string_of_int y ^ "\n\n");
				| _,_-> ();
				);
			| SRStraightEdge s->
				print_string "SRStraightEdge\n";
				print_string ("s.sser_nbits: " ^ string_of_int s.sser_nbits ^ "\n");
				(match s.sser_line with
				|None, None ->
					print_string ("s.sser_line x,y: None\n\n");
				|Some (x), None ->
					print_string ("s.sser_line x: " ^ string_of_int x ^"\n\n");
				|None, Some (y) ->
					print_string ("s.sser_line y: " ^ string_of_int y ^"\n\n");
				|Some (x), Some (y) ->
					print_string ("s.sser_line x: " ^ string_of_int x ^ "\n");
					print_string ("s.sser_line y: " ^ string_of_int y ^ "\n\n");
				);
			| SRCurvedEdge s ->
				print_string "SRCurvedEdge\n";
				print_string ("s.scer_nbits: " ^ string_of_int s.scer_nbits ^ "\n");
				print_string ("s.scer_cx: " ^ string_of_int s.scer_cx ^ "\n");
				print_string ("s.scer_cy: " ^ string_of_int s.scer_cy ^ "\n");
				print_string ("s.scer_ax: " ^ string_of_int s.scer_ax ^ "\n");
				print_string ("s.scer_ay: " ^ string_of_int s.scer_ay ^ "\n\n");

		) g.font_shape.srs_records;
	)f2.font_glyphs

let write_font2 ch b f2 =
	(* print_records f2; *)
	(* write_byte ch 255; *) (* 48 DefineFont 1/2 *)
	(* write_byte ch 18; *)  (* 48 DefineFont 2/2 *)
	(* write_i32 ch calculate_tag_size f2; *)
	write_ui16 ch 1; (* TODO: Char id *)
	write_bits b 1 (bi true);
	write_bits b 1 (bi f2.font_shift_jis);
	write_bits b 1 (bi f2.font_is_small);
	write_bits b 1 (bi f2.font_is_ansi);
	write_bits b 1 (bi f2.font_wide_offsets);
	write_bits b 1 (bi f2.font_wide_codes);
	write_bits b 1 (bi f2.font_is_italic);
	write_bits b 1 (bi f2.font_is_bold);
	write_byte ch (int_from_langcode f2.font_language);
	write_byte ch (String.length f2.font_name);
	nwrite ch f2.font_name;
	write_ui16 ch (Array.length f2.font_glyphs);
	let glyph_offset = ref (((Array.length f2.font_glyphs) * 4)+4) in
	Array.iter (fun g ->
		write_i32 ch !glyph_offset;
		glyph_offset := !glyph_offset + SwfParser.font_shape_records_length g.font_shape;
	)f2.font_glyphs;
	write_i32 ch !glyph_offset;
	Array.iter (fun g -> SwfParser.write_shape_without_style ch g.font_shape;) f2.font_glyphs;
	Array.iter (fun g -> write_ui16 ch g.font_char_code; )f2.font_glyphs;
	write_i16 ch f2.font_layout.font_ascent;
	write_i16 ch f2.font_layout.font_descent;
	write_i16 ch f2.font_layout.font_leading;
	Array.iter (fun g ->
		let fa = ref g.font_advance in
		if (!fa) <  -32767 then fa := -32768;(* fix or check *)
		if (!fa) > 32766 then fa := 32767;
		write_i16 ch !fa;) f2.font_layout.font_glyphs_layout;
	Array.iter (fun g -> SwfParser.write_rect ch g.font_bounds;) f2.font_layout.font_glyphs_layout;
	write_ui16 ch 0 (* TODO: optional FontKerningTable *)

let write_swf ttf range_str =
	let ctx = {
		ttf = ttf;
	} in

	let lut = Hashtbl.create 0 in
	Hashtbl.add lut 0 0;
	Hashtbl.add lut 1 1;
	Hashtbl.add lut 2 2;
	let rec loop cl = match cl with
		| [] ->
			()
		| {cs_def = Cmap0 c0} :: cl ->
			Array.iteri (fun i c -> Hashtbl.add lut i (int_of_char c)) c0.c0_glyph_index_array;
			loop cl
		| {cs_def = Cmap4 c4} :: cl ->
			make_cmap4_map ctx lut c4;
			loop cl
		| _ :: cl ->
			loop cl
	in
	loop ctx.ttf.ttf_cmap.cmap_subtables;

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
		font_is_italic = false;
		font_is_bold = false;
		font_language = LCNone;
		font_name = "chopin"; (* ttf.ttf_name; *)
		font_glyphs = glyfs;
		font_layout = glyfs_font_layout;
	}
;;
if Array.length Sys.argv < 2 then failwith "Usage: ttf [font name]";
let fontname = Sys.argv.(1) in
let f2 = write_swf (parse (open_in_bin (fontname ^ ".ttf"))) "" in
let ch = (output_channel (open_out_bin (fontname ^ ".dat"))) in
let b = output_bits ch in
write_font2 ch b f2;
close_out ch;
let xml = "<?xml version=\"1.0\" ?>
<swf>
	<FileAttributes/>
	<Custom tagId=\"75\" file=\"" ^ fontname ^ ".dat\" comment=\"DefineFont3\"/>
	<SymbolClass id=\"1\" class=\"TestFont\" base=\"flash.text.Font\"/>
	<DefineABC file=\"Main.swf\" isBoot=\"true\"/>
	<ShowFrame/>
</swf>"
in
let ch = open_out_bin (fontname ^ ".xml") in
Pervasives.output_string ch xml;
Pervasives.close_out ch;
if Sys.command "haxe -main Main -swf main.swf" <> 0 then failwith "Could not execute haxe";
if Sys.command ("hxswfml xml2swf " ^ fontname ^ ".xml " ^ fontname ^ ".swf -no-strict") <> 0 then failwith "Could not execute hxswfml";;