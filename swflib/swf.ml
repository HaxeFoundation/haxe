type 'a bits = {
	mutable b_data : int;
	mutable b_count : int;
	b_ch : 'a;
}

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

type header = {
	h_version : int;
	h_size : rect;
	h_fps : float16;
	h_frame_count : int; 
	mutable h_compressed : bool;
}

type export = {
	exp_id : int;
	exp_name : string;
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

type do_init_action = {
	dia_id : int;
	dia_actions : actions;
}

type tag =
	| TEnd
	| TShowFrame
	| TShape of unknown
	| TRemoveObject of int * int
	| TBitsJPEG of unknown
	| TJPEGTables of string
	| TSetBgColor of rgb
	| TText of unknown
	| TDoAction of actions
	| TSound of unknown
	| TBitsLossless of unknown
	| TBitsJPEG2 of unknown
	| TShape2 of unknown
	| TProtect
	| TPlaceObject2 of place_object2
	| TRemoveObject2 of int
	| TShape3 of unknown
	| TButton2 of unknown
	| TBitsJPEG3 of unknown
	| TBitsLossless2 of unknown
	| TEditText of unknown
	| TClip of clip
	| TFrameLabel of string
	| TSoundStreamHead2 of unknown
	| TMorphShape of unknown
	| TFont2 of unknown
	| TExport of export list
	| TDoInitAction of do_init_action
	| TUnknown of int * unknown
	| TExtended of tag

and clip_event = int * unknown

and place_object2 = {
	po_depth : int;
	po_move : bool;
	po_cid : int option;
	po_matrix : matrix option;
	po_color : color_transform_alpha option;
	po_ratio : float16 option;
	po_inst_name : string option;
	po_clip_depth : int option;
	po_events : clip_event list option;
}

and clip = {
	c_id : int;
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
