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
type float16 = int

type unknown = string

type action_count = int

type rgb = {
	cr : int;
	cg : int;
	cb : int;
}

type rgba = {
	r : int;
	g : int;
	b : int;
	a : int;
}

type color =
	| ColorRGB of rgb
	| ColorRGBA of rgba

type gradient =
	| GradientRGB of ((int * rgb) list * int)
	| GradientRGBA of ((int * rgba) list * int) 

type rect = {
	rect_nbits : int;
	left : int;
	right : int;
	top : int; 
	bottom : int;
}

type big_rect = {
	brect_nbits : int;
	bleft : int list;
	bright : int list;
	btop : int list; 
	bbottom : int list;
}

type matrix_part = {
	m_nbits : int;
	mx : int;
	my : int;
}

type matrix = {
	scale : matrix_part option;
	rotate : matrix_part option;
	trans : matrix_part;
}

type color_transform_alpha = {
	cxa_nbits : int;
	cxa_add : rgba option;
	cxa_mult : rgba option;
}

type function_decl = {
	f_name : string;
	f_args : string list;
	mutable f_codelen : action_count;
}

type func2_flags =
	| ThisRegister
	| ThisNoVar
	| ArgumentsRegister
	| ArgumentsNoVar
	| SuperRegister
	| SuperNoVar
	| RootRegister
	| ParentRegister
	| GlobalRegister

type function_decl2 = {
	f2_name : string;
	f2_flags : func2_flags list;
	f2_args : (int * string) list;
	mutable f2_nregs : int;
	mutable f2_codelen : action_count;
}

type try_style =
	| TryRegister of int
	| TryVariable of string

type try_block = {
	tr_style : try_style;
	mutable tr_trylen : action_count;
	mutable tr_catchlen : action_count option;
	mutable tr_finallylen : action_count option
}

type push_item =
	| PString of string
	| PFloat of int32
	| PNull
	| PUndefined
	| PReg of int
	| PBool of bool
	| PDouble of float
	| PInt of int32
	| PStack of int
	| PStack2 of int

type property =
	| PX
	| PY
	| PXScale
	| PYScale
	| PCurrentFrame
	| PTotalFrames
	| PAlpha
	| PVisible
	| PWidth
	| PHeight
	| PRotation
	| PTarget
	| PFramesLoaded
	| PName
	| PDropTarget
	| PUrl
	| PHighQuality
	| PFocusRect
	| PSoundBufTime
	| PQuality
	| PXMouse
	| PYMouse

type action =
	| AEnd

	| ANextFrame
	| APrevFrame
	| APlay
	| AStop
	| AToggleHighQuality
	| AStopSounds
	| AAddNum
	| ASubtract
	| AMultiply
	| ADivide
	| ACompareNum
	| AEqualNum
	| ALogicalAnd
	| ALogicalOr
	| ANot
	| AStringEqual
	| AStringLength
	| ASubString
	| APop
	| AToInt
	| AEval
	| ASet
	| ATellTarget
	| AStringAdd
	| AGetProperty
	| ASetProperty
	| ADuplicateMC
	| ARemoveMC
	| ATrace
	| AStartDrag
	| AStopDrag
	| AThrow
	| ACast
	| AImplements
	| AFSCommand2
	| ARandom
	| AMBStringLength
	| AOrd
	| AChr
	| AGetTimer
	| AMBStringSub
	| AMBOrd
	| AMBChr
	| ADeleteObj
	| ADelete
	| ALocalAssign
	| ACall
	| AReturn
	| AMod
	| ANew
	| ALocalVar
	| AInitArray
	| AObject
	| ATypeOf
	| ATargetPath
	| AEnum
	| AAdd
	| ACompare
	| AEqual
	| AToNumber
	| AToString
	| ADup
	| ASwap
	| AObjGet
	| AObjSet
	| AIncrement
	| ADecrement
	| AObjCall
	| ANewMethod
	| AInstanceOf
	| AEnum2
	| AAnd
	| AOr
	| AXor
	| AShl
	| AShr
	| AAsr
	| APhysEqual
	| AGreater
	| AStringGreater
	| AExtends

	| AGotoFrame of int
	| AGetURL of string * string
	| ASetReg of int
	| AStringPool of string list
	| AWaitForFrame of int * int
	| ASetTarget of string
	| AGotoLabel of string
	| AWaitForFrame2 of int
	| AFunction2 of function_decl2
	| ATry of try_block
	| AWith of int
	| APush of push_item list
	| AJump of action_count
	| AGetURL2 of int
	| AFunction of function_decl
	| ACondJump of action_count
	| ACallFrame (* no data *)
	| AGotoFrame2 of bool * int option

	| AUnknown of int * unknown

type actions = action DynArray.t

type header = {
	mutable h_version : int;
	mutable h_size : rect;
	mutable h_fps : float16;
	mutable h_frame_count : int; 
	mutable h_compressed : bool;
}

type export = {
	mutable exp_id : int;
	exp_name : string;
}

type do_init_action = {
	mutable dia_id : int;
	dia_actions : actions;
}

type sound = {
	mutable so_id : int;
	so_flags : int;
	so_samples : int;
	so_data : unknown;
}

type start_sound = {
	mutable sts_id : int;
	sts_data : unknown;
}

type sfs_bitmap = {
	sfb_repeat : bool;
	sfb_smooth : bool;
	mutable sfb_cid : int;
	sfb_mpos : matrix;
}

type shape_fill_style = 
	| SFSSolid of rgb
	| SFSSolid3 of rgba
	| SFSLinearGradient of matrix * gradient
	| SFSRadialGradient of matrix * gradient * int option
	| SFSBitmap of sfs_bitmap

type shape_line_style = {
	sls_width : int;
	sls_color : color;
	sls_flags : int option;
	sls_fill : shape_fill_style option;
	sls_miter : int option;
}

type shape_new_styles = {
	sns_fill_styles : shape_fill_style list;
	sns_line_styles : shape_line_style list;
	sns_nlbits : int;
	sns_nfbits : int;
}

type shape_change_style_record = {
	scsr_move : (int * int * int) option;
	scsr_fs0 : int option;
	scsr_fs1 : int option;
	scsr_ls : int option;
	scsr_new_styles : shape_new_styles option;
}

type shape_curved_edge_record = {
	scer_nbits : int;
	scer_cx : int;
	scer_cy : int;
	scer_ax : int;
	scer_ay : int;
}

type shape_straight_edge_record = {
	sser_nbits : int;
	sser_line : int option * int option;
}

type shape_record = 
	| SRStyleChange of shape_change_style_record
	| SRCurvedEdge of shape_curved_edge_record
	| SRStraightEdge of shape_straight_edge_record

type shape_records = {
	srs_nlbits : int;
	srs_nfbits : int;
	srs_records : shape_record list;
}

type shape_with_style = {
	sws_fill_styles : shape_fill_style list;
	sws_line_styles : shape_line_style list;
	sws_records : shape_records;
}

type shape = {
	mutable sh_id : int;
	sh_bounds : rect;
	sh_bounds2 : (rect * int) option;
	sh_style : shape_with_style;
}

type filter_gradient = {
	fgr_colors : (rgba * int) list;
	fgr_data : unknown;
}

type filter =
	| FDropShadow of unknown
	| FBlur of unknown
	| FGlow of unknown
	| FBevel of unknown
	| FGradientGlow of filter_gradient
	| FAdjustColor of unknown
	| FGradientBevel of filter_gradient

type bitmap_jpg = {
	mutable jpg_id : int;
	jpg_data : string;
}

type bitmap_jpg2 = {
	mutable jp2_id : int;
	jp2_table : string;
	jp2_data : string;
}

type bitmap_jpg3 = {
	mutable jp3_id : int;
	jp3_table : string;
	jp3_data : string;
	jp3_alpha_data : string;
}

type bitmap_lossless = {
	mutable bll_id : int;
	bll_format : int;
	bll_width : int;
	bll_height : int;
	bll_data : unknown;
}

type morph_shape = {
	mutable msh_id : int;
	msh_start_bounds : rect;
	msh_end_bounds : rect;
	msh_data : unknown;
}

type font2 = {
	mutable ft2_id : int;
	ft2_data : unknown;
}

type font3 = {
	mutable ft3_id : int;
	ft3_data : unknown;
}

type font_glyphs = {
	mutable fgl_id : int;
	fgl_data : unknown;
}

type text_glyph = {
	txg_index : int;
	txg_advanced : int;
}

type text_record = {
	mutable txr_font : (int * int) option;
	txr_color : color option;
	txr_dx : int option;
	txr_dy : int option;
	txr_glyphs : text_glyph list;
}

type text = {
	mutable txt_id : int;
	txt_bounds : big_rect;
	txt_matrix : matrix;
	txt_ngbits : int;
	txt_nabits : int;
	txt_records : text_record list;
}

type button_record = {
	btr_flags : int;
	mutable btr_cid : int;
	btr_depth : int;
	btr_mpos : matrix;
	btr_color : color_transform_alpha option;
	btr_filters : filter list option;
}

type button_action = {
	bta_flags : int;
	bta_actions : actions;
}

type button2 = {
	mutable bt2_id : int;
	bt2_track_as_menu : bool;
	bt2_records : button_record list;
	bt2_actions : button_action list;
}

type remove_object = {
	mutable rmo_id : int;
	rmo_depth : int;
}

type edit_text_layout = {
	edtl_align : int;
	edtl_left_margin : int;
	edtl_right_margin : int;
	edtl_indent : int;
	edtl_leading : int;
}

type edit_text = {
	mutable edt_id : int;
	edt_bounds : rect;
	mutable edt_font : (int * int) option;
	edt_color : rgba option;
	edt_maxlen : int option;
	edt_layout : edit_text_layout option;
	edt_variable : string;
	edt_text : string option;
	edt_wordwrap : bool;
	edt_multiline : bool;
	edt_password : bool;
	edt_readonly : bool;
	edt_autosize : bool;
	edt_noselect : bool;
	edt_border : bool;
	edt_html : bool;
	edt_outlines : bool;
}

type f9class = {
	mutable f9_cid : int option;
	f9_classname : string;
}

type sandbox =
	| SBLocal
	| SBNetwork
	| SBUnknown of int

type tag_data =
	| TEnd
	| TShowFrame
	| TShape of shape
	| TRemoveObject of remove_object
	| TBitsJPEG of bitmap_jpg
	| TJPEGTables of string
	| TSetBgColor of rgb
	| TText of text
	| TDoAction of actions
	| TSound of sound
	| TStartSound of start_sound
	| TBitsLossless of bitmap_lossless
	| TBitsJPEG2 of bitmap_jpg2
	| TShape2 of shape
	| TProtect
	| TPlaceObject2 of place_object
	| TRemoveObject2 of int
	| TShape3 of shape
	| TText2 of text
	| TButton2 of button2
	| TBitsJPEG3 of bitmap_jpg3
	| TBitsLossless2 of bitmap_lossless
	| TEditText of edit_text
	| TClip of clip
	| TProductInfo of unknown
	| TFrameLabel of string * char option
	| TSoundStreamHead2 of unknown
	| TMorphShape of morph_shape
	| TFont2 of font2
	| TExport of export list
	| TDoInitAction of do_init_action
	| TVideoStream of unknown
	| TVideoFrame of unknown
	| TDebugID of unknown
	| TEnableDebugger2 of int * string
	| TScriptLimits of int * int
	| TSandbox of sandbox
	| TPlaceObject3 of place_object
	| TFontGlyphs of font_glyphs
	| TTextInfo of unknown
	| TFont3 of font3
	| TF9Classes of f9class list
	| TMetaData of string
	| TActionScript3 of (int * string) option * As3.as3_tag
	| TShape4 of shape
	| TShape5 of int * string
	| TF9Scene of string
	| TUnknown of int * unknown

and tag = {
	mutable tid : int;
	mutable textended : bool;
	mutable tdata : tag_data;
}

and clip_event = {
	cle_events : int;
	cle_key : char option;
	cle_actions : actions;
}

and place_object = {
	po_depth : int;
	po_move : bool;
	mutable po_cid : int option;
	po_matrix : matrix option;
	po_color : color_transform_alpha option;
	po_ratio : float16 option;
	po_inst_name : string option;
	po_clip_depth : int option;
	po_events : clip_event list option;
	po_filters : filter list option;
	po_blend : int option;
	po_bcache : int option;
}

and clip = {
	mutable c_id : int;
	c_frame_count : int;
	c_tags : tag list;
}

type swf = header * tag list

let __deflate = ref (fun (_:unit IO.output) -> assert false)
let __inflate = ref (fun _ -> assert false)
let __parser = ref (fun _ -> assert false)
let __printer = ref (fun (_:unit IO.output) _ -> ())

exception Error of string

let error msg = raise (Error msg)

let warnings = ref true

let to_float16 f =
	let sign , f = (if f < 0. then true , 0. -. f else false , f) in
	let high = int_of_float f in
	let low = int_of_float ((f -. (float high)) *. 256.) in
	if high > 127 then failwith "to_float16";
	(high lsl 8) lor (if sign then low lor (1 lsl 15) else low)

let parse (ch : IO.input) =
	(!__parser ch : swf)

let write (ch : 'a IO.output) (data : swf) =
	!__printer (Obj.magic ch) data

let deflate (ch : 'a IO.output) =
	(Obj.magic (!__deflate (Obj.magic ch) : unit IO.output) : 'a IO.output)

let inflate (ch : IO.input) =
	(!__inflate ch : IO.input)
