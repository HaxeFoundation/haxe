
type 'a index
type 'a index_nz

type as3_ident = string
type as3_int = int32
type as3_float = float

type as3_base_right =
	| A3RPrivate
	| A3RPublic of as3_ident index option
	| A3RInternal of as3_ident index option
	| A3RProtected of as3_ident index

type as3_rights = as3_base_right index_nz list

type as3_type =
	| A3TClassInterface of as3_ident index * as3_base_right index
	| A3TMethodVar of as3_ident index * as3_base_right index

type as3_method_type = {
	mt3_ret : as3_type index option;
	mt3_args : as3_type index option list;
	mt3_unk : int;	
}

type as3_field_value =
	| A3VNull
	| A3VBool of bool
	| A3VString of as3_ident index
	| A3VInt of as3_int index
	| A3VFloat of as3_float index
	| A3VNamespace of as3_base_right index

type as3_method = {
	m3_type : as3_method_type index_nz;
	m3_final : bool;
	m3_override : bool;
}

type as3_var = {
	v3_type : as3_type index option;
	v3_value : as3_field_value option;
	v3_const : bool;
}

type as3_field_kind =
	| A3FMethod of as3_method
	| A3FVar of as3_var

type as3_field = {
	f3_name : as3_type index;
	f3_slot : int;
	f3_kind : as3_field_kind;
}

type as3_class = {
	cl3_name : as3_type index;
	cl3_super : as3_type index option;
	cl3_sealed : bool;
	cl3_final : bool;
	cl3_interface : bool;
	cl3_rights : as3_base_right index option;
	cl3_implements : as3_type index array;
	cl3_slot : int;
	cl3_fields : as3_field array;
}

type as3_static = {
	st3_slot : int;
	st3_fields : as3_field array;
}

type as3_init = {
	in3_slot : int;
	in3_type : as3_type index;
	in3_class : as3_class index_nz;
	in3_unk : int;
}

type as3_tag = {
	as3_frame : string;
	as3_ints : as3_int array;
	(* ??? *)
	as3_floats : as3_float array;
	as3_idents : as3_ident array;
	as3_base_rights : as3_base_right array;
	as3_rights : as3_rights array;
	mutable as3_types : as3_type array;
	mutable as3_method_types : as3_method_type array;
	(* ??? *)
	mutable as3_classes : as3_class array;
	mutable as3_statics : as3_static array;
	mutable as3_inits : as3_init array;
	mutable as3_unknown : string;

	as3_original_data : string;
}
