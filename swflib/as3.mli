
type 'a index
type 'a index_nz

type as3_ident = string
type as3_int = int32
type as3_float = float

type as3_base_right =
	| A3RPrivate of as3_ident index option
	| A3RPublic of as3_ident index option
	| A3RInternal of as3_ident index option
	| A3RProtected of as3_ident index
	| A3RUnknown1 of as3_ident index
	| A3RUnknown2 of as3_ident index option

type as3_rights = as3_base_right index list

type as3_type =
	| A3TClassInterface of as3_ident index * as3_base_right index
	| A3TMethodVar of as3_ident index * as3_base_right index
	| A3TUnknown1 of int * int
	| A3TUnknown2 of int * int * int

type as3_value =
	| A3VNone
	| A3VNull
	| A3VBool of bool
	| A3VString of as3_ident index
	| A3VInt of as3_int index
	| A3VFloat of as3_float index
	| A3VNamespace of as3_base_right index

type as3_method_type = {
	mt3_ret : as3_type index option;
	mt3_args : as3_type index option list;
	mt3_native : bool;
	mt3_var_args : bool;
	mt3_unk : int;
	mt3_dparams : as3_value list option;
	mt3_pnames : as3_ident index list option;
	mt3_unk_flags : bool * bool * bool * bool;
}

type as3_method_kind =
	| MK3Normal
	| MK3Getter
	| MK3Setter

type as3_method = {
	m3_type : as3_method_type index_nz;
	m3_final : bool;
	m3_override : bool;
	m3_kind : as3_method_kind;
}

type as3_var = {
	v3_type : as3_type index option;
	v3_value : as3_value;
	v3_const : bool;
}

type as3_metadata = {
	meta3_name : as3_ident index;
	meta3_data : (as3_ident index * as3_ident index) array;
}

type as3_field_kind =
	| A3FMethod of as3_method
	| A3FVar of as3_var
	| A3FClass of as3_class index_nz

and as3_field = {
	f3_name : as3_type index;
	f3_slot : int;
	f3_kind : as3_field_kind;
	f3_metas : as3_metadata index_nz array option;
}

and as3_class = {
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

type as3_inits = {
	in3_slot : int;
	in3_fields : as3_field array;
}

type as3_function = {
	fun3_id : as3_method_type index_nz;
	fun3_unk1 : int;
	fun3_unk2 : int;
	fun3_unk3 : int;
	fun3_unk4 : int;
	fun3_code : string;		
}

type as3_tag = {
	as3_id : (int * string) option;
	as3_ints : as3_int array;
	(* ??? *)
	as3_floats : as3_float array;
	as3_idents : as3_ident array;
	as3_base_rights : as3_base_right array;
	as3_rights : as3_rights array;
	mutable as3_types : as3_type array;
	mutable as3_method_types : as3_method_type array;
	mutable as3_metadatas : as3_metadata array;
	mutable as3_classes : as3_class array;
	mutable as3_statics : as3_static array;
	mutable as3_inits : as3_inits array;
	mutable as3_functions : as3_function array;

	mutable as3_unknown : string;
	as3_original_data : string;
}
