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

type glyf_def =
	| TglyfSimple of glyf_simple
	| TglyfComposite of glyf_component list

type glyf = {
	glyf_header : glyf_header;
	glyf_def : glyf_def;
}

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

type cmap_subtable_def =
	| Cmap0 of cmap_format_0
	| Cmap4 of cmap_format_4
	| Cmap6 of cmap_format_6
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
	left : int;
	right : int;
	value : int;
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
	os2_us_win_ascent : int;
	os2_us_win_descent : int;
}

type table =
	| TGlyf of glyf array
	| THmtx of hmtx list
	| TCmap of cmap
	| TKern of kern
	| TName of name
	| THead of head
	| THhea of hhea
	| TLoca of loca
	| TMaxp of maxp
	| TOS2 of os2
	| TUnk of string

type ttf = {
	ttf_header : header;
	ttf_name : string;
	ttf_directory: (string,entry) Hashtbl.t;
	ttf_tables : (string * table) list;
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
	let last_advance_width = ref 0 in
	ExtList.List.init maxp.maxp_num_glyphs (fun i ->
		let advance_width = if i > hhea.hhea_number_of_hmetrics then
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
		let length = rdu16 ch in
		let language = rdu16 ch in
		let def = match format with
			| 0 ->
				let glyph_index = Array.init 256 (fun _ -> read ch) in
				Cmap0 {
					c0_format = 0;
					c0_length = length;
					c0_language = language;
					c0_glyph_index_array = glyph_index;
				}
			| 4 ->
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
				(* TODO: whatever Reader.hx does here *)
				Cmap4 {
					c4_format = format;
					c4_length = length;
					c4_language = language;
					c4_seg_count_x2 = seg_count;
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
		let def = if num_contours >= 0 then begin
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
						for i = 0 to r - 1 do DynArray.add flags v done;
						r
					end in
					loop (index + incr)
				end
			in
			loop 0;
			let last = ref 0 in
			let x_coordinates = Array.init !num_points (fun i ->
				let flag = DynArray.get flags i in
				let v = if flag land 0x10 <> 0 then begin
						if flag land 0x02 <> 0 then read_byte ch
						else !last
					end else begin
						if flag land 0x02 <> 0 then -read_byte ch
						else rd16 ch
					end
				in
				last := v;
				v
			) in
			last := 0;
			let y_coordinates = Array.init !num_points (fun i ->
				let flag = DynArray.get flags i in
				let v = if flag land 0x20 <> 0 then begin
						if flag land 0x04 <> 0 then read_byte ch
						else !last
					end else begin
						if flag land 0x04 <> 0 then -read_byte ch
						else rd16 ch
					end;
				in
				last := v;
				v
			) in
			TglyfSimple {
				gs_end_pts_of_contours = end_pts_of_contours;
				gs_instruction_length = instruction_length;
				gs_instructions = instructions;
				gs_flags = DynArray.to_array flags;
				gs_x_coordinates = x_coordinates;
				gs_y_coordinates = y_coordinates;
			}
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
			TglyfComposite (DynArray.to_list acc)
		end else
			failwith "Unknown Glyf"
		in
		{
			glyf_header = header;
			glyf_def = def;
		}
	in
	Array.init maxp.maxp_num_glyphs (fun i -> parse_glyf i)

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
					left = left;
					right = right;
					value = value;
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
	let head = parse_table (Hashtbl.find directory "head") parse_head_table in
	let hhea = parse_table (Hashtbl.find directory "hhea") parse_hhea_table in
	let maxp = parse_table (Hashtbl.find directory "maxp") parse_maxp_table in
	let loca = parse_table (Hashtbl.find directory "loca") (parse_loca_table head maxp) in
	let hmtx = parse_table (Hashtbl.find directory "hmtx") (parse_hmtx_table maxp hhea) in
	let cmap = parse_table (Hashtbl.find directory "cmap") (parse_cmap_table) in
	let glyf = parse_table (Hashtbl.find directory "glyf") (parse_glyf_table maxp loca cmap hmtx) in
	(* let kern = parse_table (Hashtbl.find directory "kern") (parse_kern_table) in *)
	let name,ttf_name = parse_table (Hashtbl.find directory "name") (parse_name_table) in
	let os2 = parse_table (Hashtbl.find directory "OS/2") (parse_os2_table) in
	let tables = ("head", (THead head)) :: ("hhea", (THhea hhea)) :: ("maxp", (TMaxp maxp))
			  :: ("loca", (TLoca loca)) :: ("hmtx", (THmtx hmtx)) :: ("cmap", (TCmap cmap))
			  :: ("glyf", (TGlyf glyf)) :: ("name", (TName name))
			  :: ("OS/2", (TOS2 os2)) :: []
	in
	{
		ttf_header = header;
		ttf_name = ttf_name;
		ttf_directory = directory;
		ttf_tables = tables;
	}

(* WRITING *)

(* TODO: move this to swf.ml *)

type font_language_code =
	| LCNone (*0*)
	| LCLatin (*1*)
	| LCJapanese (*2*)
	| LCKorean (*3*)
	| LCSimplifiedChinese (*4*)
	| LCTraditionalChinese (*5*)

type font_glyph_data = {
	font_char_code: int;
	font_shape: shape_records;
}

type font_layout_glyph_data = {
	font_advance: int;
	font_bounds: rect;
}

type font_kerning_data = {
	font_char_code1: int;
	font_char_code2: int;
	font_adjust: int;
}

type font_layout_data = {
	font_ascent: int;
	font_descent: int;
	font_leading: int;
	font_glyphs: font_layout_glyph_data list;
	font_kerning: font_kerning_data list;
}

type font2_data = {
	font_shift_jis: bool;
	font_is_small: bool;
	font_is_ansi: bool;
	font_is_italic: bool;
	font_is_bold: bool;
	font_language: font_language_code;
	font_name: string;
	font_glyphs: font_glyph_data array;
	font_layout: font_layout_data option;
}

(* This can stay *)

type write_ctx = {
	head : head;
	cmap : cmap;
	glyf : glyf array;
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

let make_path ctx pq p1 p2 arr g =
	let p1_on_curve = g.gs_flags.(p1) land 0x01 <> 0 in
	let p2_on_curve = g.gs_flags.(p2) land 0x01 <> 0 in
	match p1_on_curve, p2_on_curve with
	| true, true ->
		let path = mk_path 1 (float_of_int g.gs_x_coordinates.(p2)) (float_of_int g.gs_y_coordinates.(p2)) 0.0 0.0 in
		DynArray.add arr path;
	| false, false ->
		let path = mk_path 2 !pq.gp_x !pq.gp_y (float_of_int g.gs_x_coordinates.(p1)) (float_of_int g.gs_y_coordinates.(p1)) in
		DynArray.add arr path;
		pq := (mk_path (-1) (float_of_int g.gs_x_coordinates.(p2)) (float_of_int g.gs_y_coordinates.(p2)) 0.0 0.0)
	| true, false ->
		pq := (mk_path (-1) (float_of_int g.gs_x_coordinates.(p2)) (float_of_int g.gs_y_coordinates.(p2)) 0.0 0.0)
	| false, true ->
		let path = mk_path 2 (float_of_int g.gs_x_coordinates.(p2)) (float_of_int g.gs_y_coordinates.(p2)) !pq.gp_x !pq.gp_y in
		DynArray.add arr path

let build_paths ctx g =
	let len = Array.length g.gs_end_pts_of_contours in
	let arr = DynArray.create () in
	let cp = ref 0 in
	let start = ref 0 in
	let stop = ref 0 in
	let pq = ref (mk_path (-1) 0.0 0.0 0.0 0.0) in
	for i = 0 to len - 1 do
		start := !cp;
		stop := g.gs_end_pts_of_contours.(i);
		let path = mk_path 0 (float_of_int g.gs_x_coordinates.(!start)) (float_of_int g.gs_y_coordinates.(!start)) 0.0 0.0 in
		DynArray.add arr path;
		for j = 0 to !stop - !start - 1 do
			make_path ctx pq !cp (!cp + 1) arr g;
			incr cp;
		done;
		make_path ctx pq !stop !start arr g;
		incr cp;
	done;
	DynArray.to_list arr

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
		!nbits

let round x = int_of_float (floor (x +. 0.5))

let twips_of_float v =
	let temp1 = round(v *. 1000.) in
	let diff = temp1 mod 50 in
	let temp2 = if diff < 25 then temp1 - diff else temp1 + (50 - diff) in
	let temp3 = (float_of_int temp2) /. 1000. in
	round (temp3 *. 20.)

let begin_fill ctx c a =
	SRStyleChange {
		scsr_move = None;
		scsr_fs0 = None;
		scsr_fs1 = Some(1); (* Some (c lor (a lsl 24));*) (* CHECK *)
		scsr_ls = None; (* CHECK *)
		scsr_new_styles = None;
	}

let move_to ctx x y =
	SRStyleChange {
		scsr_move = Some (0,twips_of_float x, twips_of_float y); (* TODO *)
		scsr_fs0 = None;
		scsr_fs1 = None; (* TODO *)
		scsr_ls = None;
		scsr_new_styles = None;
	}

let line_to ctx x y =
	SRStraightEdge {
		sser_nbits = max (_nbits (twips_of_float x)) (_nbits (twips_of_float y)); (* TODO ? *)
		sser_line = Some (int_of_float x), Some (twips_of_float y);
	}

let curve_to ctx cx cy x y =
	let m1 = max (_nbits (twips_of_float x)) (_nbits (twips_of_float y)) in
	let m2 = max (_nbits (twips_of_float cx)) (_nbits (twips_of_float cy)) in
	let m = max m1 m2 in
	SRCurvedEdge {
		scer_nbits = 2 + m; (* TODO ? *)
		scer_cx = twips_of_float cx;
		scer_cy = twips_of_float cy;
		scer_ax = twips_of_float x;
		scer_ay = twips_of_float y;
	}

let write_paths ctx paths =
	let scale = 1024. /. (float_of_int ctx.head.hd_units_per_em) in
	let srl = DynArray.create () in
	DynArray.add srl (begin_fill ctx 0 1);
	List.iter (fun path ->
		DynArray.add srl (match path.gp_type with
		| 0 -> move_to ctx (path.gp_x *. scale) ((-1.) *. path.gp_y *. scale);
		| 1 -> line_to ctx (path.gp_x *. scale) ((-1.) *. path.gp_y *. scale);
		| 2 -> curve_to ctx (path.gp_cx *. scale) ((-1.) *. path.gp_cy *. scale) (path.gp_x *. scale) ((-1.) *. path.gp_y *. scale);
		| _ -> assert false)
	) paths;
	{
		srs_nfbits = 2; (* TODO *)
		srs_nlbits = 0; (* TODO *)
		srs_records = DynArray.to_list srl;
	}

let write_glyph ctx key glyf =
	match glyf.glyf_def with
	| TglyfSimple g ->
		let path = build_paths ctx g in
		{
			font_char_code = key;
			font_shape = write_paths ctx path;
		}
	| TglyfComposite g ->
		{
			font_char_code = 0; (* TODO *)
			font_shape = write_paths ctx []; (* TODO *)
		}

let map_char_code cc c4 =
	(* TODO: well yeah... *)
	0

let make_cmap4_map ctx c4 =
	let lut = Hashtbl.create 0 in
	let keys = DynArray.create () in
	let seg_count = c4.c4_seg_count_x2 / 2 in
	for i = 0 to seg_count - 1 do
		for j = c4.c4_start_code.(i) to c4.c4_end_code.(i) do
			let index = i (* map_char_code j c4 *) in
			Hashtbl.add lut j index;
			DynArray.add keys j
		done;
	done;
	lut,DynArray.to_list keys

let bi v = if v then 1 else 0

let write_font2 ch b f2 =
	write_byte ch 255; (* 48 DefineFont 1/2 *)
	write_byte ch 18; (* 48 DefineFont 2/2 *)
	write_i32 ch 16777215; (* TODO: tag body size/length *)
	write_ui16 ch 1; (*TODO: Char id *)

	write_bits b 1 (bi (f2.font_layout <> None));
	write_bits b 1 (bi f2.font_shift_jis);
	write_bits b 1 (bi f2.font_is_small);
	write_bits b 1 (bi f2.font_is_ansi);
	write_bits b 1 (bi false); (* TODO: wide offsets *)
	write_bits b 1 (bi true); (* TODO: wide codes *)
	write_bits b 1 (bi f2.font_is_italic);
	write_bits b 1 (bi f2.font_is_bold);

	write_byte ch 0; (* TODO: f2.font_language; LCNone *)

	write_byte ch (String.length f2.font_name);
	nwrite ch f2.font_name;
	write_ui16 ch (Array.length f2.font_glyphs); (* numglyps *)
	Array.iter (fun _ -> write_ui16 ch 0) f2.font_glyphs; (* TODO: offsetTable *)
	write_ui16 ch (2 * Array.length f2.font_glyphs); (* TODO: offset to codeTable depends on 16 or 32 bit *)

	Array.iter (fun g ->
		(* let b = output_bits ch in *)
		let records_length = SwfParser.shape_records_length g.font_shape in
		let nlbits = ref g.font_shape.srs_nlbits in
		let nfbits = ref g.font_shape.srs_nfbits in
		let character = Char.chr(g.font_char_code) in
		let s = String.make 1 character in
		print_string "==================== Glyph start =====================\n";
		print_string ("char code: " ^ string_of_int g.font_char_code ^ ", char: "^s^"\n");
		print_string ("records_length bytes: " ^ string_of_int records_length ^ "\n");
		List.iter (fun r ->
			match r with
			| SRStyleChange s ->
				print_string "SRStyleChange\n";
				(match s.scsr_move with
				|None -> ();
					print_string "s.scsr_move 0,x,y: None\n\n";
				|Some (w , x, y) ->
					print_string ("s.scsr_move x: " ^ string_of_int x ^ "\n");
					print_string ("s.scsr_move y: " ^ string_of_int y ^ "\n\n");
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

			try
				SwfParser.write_shape_record ch b nlbits nfbits r;
			with _ -> print_string "----------^^ FAILURE!"; print_string "\n\n";

		) g.font_shape.srs_records
	) f2.font_glyphs
	(* TODO: rest *)

let print_glyph g =
	let hd = g.glyf_header in
	print_endline "===== HEADER =====";
	print_endline (Printf.sprintf "gh_num_contours = %i, gh_xmin = %i, gh_ymin = %i, gh_xmax = %i, gh_ymax = %i" hd.gh_num_contours hd.gh_xmin hd.gh_ymin hd.gh_xmax hd.gh_ymax);
	match g.glyf_def with
	| TglyfComposite _ ->
		print_endline "===== COMPOSITE =====";
	| TglyfSimple gs ->
		print_endline "===== SIMPLE =====";
		print_endline (Printf.sprintf "gs_end_pts_of_contours[%i]: %s" (Array.length gs.gs_end_pts_of_contours) (String.concat "," (Array.to_list (Array.map string_of_int gs.gs_end_pts_of_contours))));
		print_endline (Printf.sprintf "gs_flags[%i]: %s" (Array.length gs.gs_flags) (String.concat "," (Array.to_list (Array.map string_of_int gs.gs_flags))));
		print_endline (Printf.sprintf "gs_x_coordinates[%i]: %s" (Array.length gs.gs_x_coordinates) (String.concat "," (Array.to_list (Array.map string_of_int gs.gs_x_coordinates))));
		print_endline (Printf.sprintf "gs_y_coordinates[%i]: %s" (Array.length gs.gs_y_coordinates) (String.concat "," (Array.to_list (Array.map string_of_int gs.gs_y_coordinates))))

let write_swf ttf range_str =
	let ctx = {
		head = (match List.assoc "head" ttf.ttf_tables with THead head -> head | _ -> assert false);
		cmap = (match List.assoc "cmap" ttf.ttf_tables with TCmap cmap -> cmap | _ -> assert false);
		glyf = (match List.assoc "glyf" ttf.ttf_tables with TGlyf glyf -> glyf | _ -> assert false);
	} in
	let rec loop cl = match cl with
		| [] -> failwith "Cmap4 table not found"
		| {cs_def = Cmap4 c4} :: _ ->
			make_cmap4_map ctx c4
		| _ :: cl ->
			loop cl
	in
	let glyph_lut,keys = loop ctx.cmap.cmap_subtables in
	ignore(glyph_lut);
	(* TODO: check range, perform lookup *)
	{
		font_shift_jis = false;
		font_is_small = false;
		font_is_ansi = false;
		font_is_italic = false;
		font_is_bold = false;
		font_language = LCNone;
		font_name = "chopin"; (* ttf.ttf_name; *)
		font_glyphs = Array.of_list (ExtList.List.mapi (fun i key -> write_glyph ctx key (ctx.glyf.(i))) keys);
		font_layout = None;
	}
;;
let ttf = parse (open_in_bin "chopin.ttf") in
(match List.assoc "glyf" ttf.ttf_tables with TGlyf glyf -> Array.iter print_glyph glyf | _ -> assert false);

(* let f2 = write_swf ( "" in
let ch = (output_channel (open_out_bin "chopin_test.dat")) in
let b = output_bits ch in
write_font2 ch b f2
 *)
(* ocamlopt -I ../extlib -I ../extc enum.cmx extlist.cmx extstring.cmx dynarray.cmx multiarray.cmx swf.cmx io.cmx as3code.cmx as3parse.cmx actionscript.cmx swfparser.cmx ttf.ml -o run.exe *)