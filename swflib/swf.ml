type float16 = int

type unknown = string

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
	| GradientRGB of (int * rgb) list
	| GradientRGBA of (int * rgba) list

type rect = {
	rect_nbits : int;
	left : int;
	right : int;
	top : int; 
	bottom : int;
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
	f_codelen : int;
}

type function_decl2 = {
	f2_name : string;
	f2_nregs : int;
	f2_flags : int;
	f2_args : (int * string) list;
	f2_codelen : int;
}

type push_item =
	| PString of string
	| PFloat of unknown
	| PNull
	| PUndefined
	| PReg of int
	| PBool of bool
	| PDouble of unknown
	| PInt of unknown
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
	| APrefFrame
	| APlay
	| AStop
	| AToggleHighQuality
	| AStopSounds
	| AAdd
	| ASubtract
	| AMultiply
	| ADivide
	| ACompare
	| ANumberEqual
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
	| AImplement
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
	| ALocalVar
	| ACall
	| AReturn
	| AMod
	| ANew
	| ALocalAssign
	| AInitArray
	| AObject
	| ATypeOf
	| ATargetPath
	| AEnum
	| AAdd2
	| ACompare2
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
	| APushReg of int
	| AStringPool of string list
	| AWaitForFrame of int * int
	| ASetTarget of string
	| AGotoLabel of string
	| AWaitForFrame2 of int
	| AFunction2 of function_decl2
	| APush of push_item list
	| AWith of string * int
	| AJump of int
	| AGetURL2 of int
	| AFunction of function_decl
	| ACondJump of int
	| ACallFrame (* no data *)
	| AGotoFrame2 of bool * int option

	| AUnknown of int * unknown

type actions = action DynArray.t

type header = {
	h_version : int;
	h_size : rect;
	h_fps : float16;
	h_frame_count : int; 
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
	| SFSRadialGradient of matrix * gradient
	| SFSBitmap of sfs_bitmap

type shape_line_style = {
	sls_width : int;
	sls_color : color;
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
	sh_style : shape_with_style;
}

type bitmap = {
	mutable bmp_id : int;
	bmp_data : string;
}

type bitmap_jpg3 = {
	mutable jp3_id : int;
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
	txt_bounds : rect;
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

type tag_data =
	| TEnd
	| TShowFrame
	| TShape of shape
	| TRemoveObject of remove_object
	| TBitsJPEG of bitmap
	| TJPEGTables of string
	| TSetBgColor of rgb
	| TText of text
	| TDoAction of actions
	| TSound of sound
	| TStartSound of start_sound
	| TBitsLossless of bitmap_lossless
	| TBitsJPEG2 of bitmap
	| TShape2 of shape
	| TProtect
	| TPlaceObject2 of place_object2
	| TRemoveObject2 of int
	| TShape3 of shape
	| TText2 of text
	| TButton2 of button2
	| TBitsJPEG3 of bitmap_jpg3
	| TBitsLossless2 of bitmap_lossless
	| TEditText of edit_text
	| TClip of clip
	| TFrameLabel of string
	| TSoundStreamHead2 of unknown
	| TMorphShape of morph_shape
	| TFont2 of font2
	| TExport of export list
	| TDoInitAction of do_init_action
	| TUnknown of int * unknown

and tag = {
	mutable tid : int;
	textended : bool;
	tdata : tag_data;
}

and clip_event = int * unknown

and place_object2 = {
	po_depth : int;
	po_move : bool;
	mutable po_cid : int option;
	po_matrix : matrix option;
	po_color : color_transform_alpha option;
	po_ratio : float16 option;
	po_inst_name : string option;
	po_clip_depth : int option;
	po_events : clip_event list option;
}

and clip = {
	mutable c_id : int;
	c_frame_count : int;
	c_tags : tag list;
}

type swf = header * tag list

let __deflate = ref (fun (_:unit IO.stdout) -> assert false)
let __inflate = ref (fun _ -> assert false)
let __parser = ref (fun _ -> assert false)
let __printer = ref (fun (_:unit IO.stdout) _ -> ())

exception Error of string

let error msg = raise (Error msg)

let parse (ch : IO.stdin) =
	(!__parser ch : swf)

let write (ch : 'a IO.stdout) (data : swf) =
	!__printer (Obj.magic ch) data

let deflate (ch : 'a IO.stdout) =
	(Obj.magic (!__deflate (Obj.magic ch) : unit IO.stdout) : 'a IO.stdout)

let inflate (ch : IO.stdin) =
	(!__inflate ch : IO.stdin)
