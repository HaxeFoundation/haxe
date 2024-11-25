open Globals

type jwildcard =
	| WExtends (* + *)
	| WSuper (* -  *)
	| WNone

type jtype_argument =
	| TType of jwildcard * jsignature
	| TAny (* * *)

and jsignature =
	| TByte (* B *)
	| TChar (* C *)
	| TDouble (* D *)
	| TFloat (* F *)
	| TInt (* I *)
	| TLong (* J *)
	| TShort (* S *)
	| TBool (* Z *)
	| TObject of path * jtype_argument list (* L Classname *)
	| TObjectInner of (string list) * (string * jtype_argument list) list (* L Classname ClassTypeSignatureSuffix *)
	| TArray of jsignature * int option (* [ *)
	| TMethod of jmethod_signature (* ( *)
	| TTypeParameter of string (* T *)

(* ( jsignature list ) ReturnDescriptor (| V | jsignature) *)
and jmethod_signature = jsignature list * jsignature option

type jtypes = (string * jsignature option * jsignature list) list

type jannotation = {
	ann_type : jsignature;
	ann_elements : (string * jannotation_value) list;
}

and jannotation_value =
	| ValConst of jsignature * int
	| ValEnum of jsignature * string (* e *)
	| ValClass of jsignature (* c *) (* V -> Void *)
	| ValAnnotation of jannotation (* @ *)
	| ValArray of jannotation_value list (* [ *)

type jlocal = {
	ld_start_pc : int;
	ld_length : int;
	ld_name : string;
	ld_descriptor : string;
	ld_index : int;
}

type jattribute =
	| AttrCode of jattribute list
	| AttrDeprecated
	| AttrLocalVariableTable of jlocal list
	| AttrMethodParameters of (string * int) list
	| AttrSignature of string
	| AttrVisibleAnnotations of jannotation list
	| AttrOther

type jfield = {
	jf_name : string;
	jf_flags : int;
	jf_types : jtypes;
	jf_descriptor : jsignature;
	jf_attributes : jattribute list;
	jf_code : jattribute list option;
}

type jclass = {
	jc_path : path;
	jc_flags : int;
	jc_super : jsignature;
	jc_interfaces : jsignature list;
	jc_types : jtypes;
	jc_fields : jfield list;
	jc_methods : jfield list;
	jc_attributes : jattribute list;
}