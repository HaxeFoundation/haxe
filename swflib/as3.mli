(*
 *  This file is part of SwfLib
 *  Copyright (c)2004-2006 Nicolas Cannasse
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

type 'a index
type 'a index_nz

type as3_ident = string
type as3_int = int32
type as3_uint = int32
type as3_float = float
type as3_slot = int

type reg = int
type nargs = int

type as3_jump =
	| J3NotLt
	| J3NotLte
	| J3NotGt
	| J3NotGte
	| J3Always
	| J3True
	| J3False
	| J3Eq
	| J3Neq
	| J3Lt
	| J3Lte
	| J3Gt
	| J3Gte
	| J3PhysEq
	| J3PhysNeq

type as3_op =
	| A3OAs
	| A3ONeg
	| A3OIncr
	| A3ODecr
	| A3ONot
	| A3OBitNot
	| A3OAdd
	| A3OSub
	| A3OMul
	| A3ODiv
	| A3OMod
	| A3OShl
	| A3OShr
	| A3OUShr
	| A3OAnd
	| A3OOr
	| A3OXor
	| A3OEq
	| A3OPhysEq
	| A3OLt
	| A3OLte
	| A3OGt
	| A3OGte
	| A3OIs
	| A3OIn
	| A3OIIncr
	| A3OIDecr
	| A3OINeg
	| A3OIAdd
	| A3OISub
	| A3OIMul
	| A3OMemSet8
	| A3OMemSet16
	| A3OMemSet32
	| A3OMemSetFloat
	| A3OMemSetDouble
	| A3OMemGet8
	| A3OMemGet16
	| A3OMemGet32
	| A3OMemGetFloat
	| A3OMemGetDouble
	| A3OSign1
	| A3OSign8
	| A3OSign16

type as3_name = as3_multi_name index

and as3_opcode =
	| A3BreakPoint
	| A3Nop
	| A3Throw
	| A3GetSuper of as3_name
	| A3SetSuper of as3_name
	| A3DxNs of as3_ident index
	| A3DxNsLate
	| A3RegKill of reg
	| A3Label
	| A3Jump of as3_jump * int
	| A3Switch of int * int list
	| A3PushWith
	| A3PopScope
	| A3ForIn
	| A3HasNext
	| A3Null
	| A3Undefined
	| A3ForEach
	| A3SmallInt of int
	| A3Int of int
	| A3True
	| A3False
	| A3NaN
	| A3Pop
	| A3Dup
	| A3Swap
	| A3String of as3_ident index
	| A3IntRef of as3_int index
	| A3UIntRef of as3_uint index
	| A3Float of as3_float index
	| A3Scope
	| A3Namespace of as3_namespace index
	| A3Next of reg * reg
	| A3Function of as3_method_type index_nz
	| A3CallStack of nargs
	| A3Construct of nargs
	| A3CallMethod of as3_slot * nargs
	| A3CallStatic of as3_method_type index * nargs
	| A3CallSuper of as3_name * nargs
	| A3CallProperty of as3_name * nargs
	| A3RetVoid
	| A3Ret
	| A3ConstructSuper of nargs
	| A3ConstructProperty of as3_name * nargs
	| A3CallPropLex of as3_name * nargs
	| A3CallSuperVoid of as3_name * nargs
	| A3CallPropVoid of as3_name * nargs
	| A3ApplyType of nargs
	| A3Object of nargs
	| A3Array of nargs
	| A3NewBlock
	| A3ClassDef of unit index_nz
	| A3GetDescendants of as3_name
	| A3Catch of int
	| A3FindPropStrict of as3_name
	| A3FindProp of as3_name
	| A3FindDefinition of as3_name
	| A3GetLex of as3_name
	| A3SetProp of as3_name
	| A3Reg of reg
	| A3SetReg of reg
	| A3GetGlobalScope
	| A3GetScope of int
	| A3GetProp of as3_name
	| A3InitProp of as3_name
	| A3DeleteProp of as3_name
	| A3GetSlot of as3_slot
	| A3SetSlot of as3_slot
	| A3ToString
	| A3ToXml
	| A3ToXmlAttr
	| A3ToInt
	| A3ToUInt
	| A3ToNumber
	| A3ToBool
	| A3ToObject
	| A3CheckIsXml
	| A3Cast of as3_name
	| A3AsAny
	| A3AsString
	| A3AsType of as3_name
	| A3AsObject
	| A3IncrReg of reg
	| A3DecrReg of reg
	| A3Typeof
	| A3InstanceOf
	| A3IsType of as3_name
	| A3IncrIReg of reg
	| A3DecrIReg of reg
	| A3This
	| A3SetThis
	| A3DebugReg of as3_ident index * reg * int
	| A3DebugLine of int
	| A3DebugFile of as3_ident index
	| A3BreakPointLine of int
	| A3Timestamp
	| A3Op of as3_op
	| A3Unk of char

and as3_namespace =
	| A3NPrivate of as3_ident index option
	| A3NPublic of as3_ident index option
	| A3NInternal of as3_ident index option
	| A3NProtected of as3_ident index
	| A3NNamespace of as3_ident index
	| A3NExplicit of as3_ident index
	| A3NStaticProtected of as3_ident index option

and as3_ns_set = as3_namespace index list

and as3_multi_name =
	| A3MName of as3_ident index * as3_namespace index
	| A3MMultiName of as3_ident index option * as3_ns_set index
	| A3MRuntimeName of as3_ident index
	| A3MRuntimeNameLate
	| A3MMultiNameLate of as3_ns_set index
	| A3MAttrib of as3_multi_name
	| A3MParams of as3_multi_name index * as3_multi_name index list

and as3_value =
	| A3VNone
	| A3VNull
	| A3VBool of bool
	| A3VString of as3_ident index
	| A3VInt of as3_int index
	| A3VUInt of as3_uint index
	| A3VFloat of as3_float index
	| A3VNamespace of int * as3_namespace index (* int : kind of namespace *)

and as3_method_type = {
	mt3_ret : as3_name option;
	mt3_args : as3_name option list;
	mt3_native : bool;
	mt3_var_args : bool;
	mt3_arguments_defined : bool;
	mt3_uses_dxns : bool;
	mt3_new_block : bool;
	mt3_unused_flag : bool;
	mt3_debug_name : as3_ident index option;
	mt3_dparams : as3_value list option;
	mt3_pnames : as3_ident index option list option;
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
	v3_type : as3_name option;
	v3_value : as3_value;
	v3_const : bool;
}

type as3_metadata = {
	meta3_name : as3_ident index;
	meta3_data : (as3_ident index option * as3_ident index) array;
}

type as3_field_kind =
	| A3FMethod of as3_method
	| A3FVar of as3_var
	| A3FClass of as3_class index_nz
	| A3FFunction of as3_method_type index_nz

and as3_field = {
	f3_name : as3_name;
	f3_slot : as3_slot;
	f3_kind : as3_field_kind;
	f3_metas : as3_metadata index_nz array option;
}

and as3_class = {
	cl3_name : as3_name;
	cl3_super : as3_name option;
	cl3_sealed : bool;
	cl3_final : bool;
	cl3_interface : bool;
	cl3_namespace : as3_namespace index option;
	cl3_implements : as3_name array;
	cl3_construct : as3_method_type index_nz;
	cl3_fields : as3_field array;
}

type as3_static = {
	st3_method : as3_method_type index_nz;
	st3_fields : as3_field array;
}

type as3_try_catch = {
	tc3_start : int;
	tc3_end : int;
	tc3_handle : int;
	tc3_type : as3_name option;
	tc3_name : as3_name option;
}

type as3_function = {
	fun3_id : as3_method_type index_nz;
	fun3_stack_size : int;
	fun3_nregs : int;
	fun3_init_scope : int;
	fun3_max_scope : int;
	fun3_code : as3_opcode array;
	fun3_trys : as3_try_catch array;
	fun3_locals : as3_field array;
}

type as3_tag = {
	as3_ints : as3_int array;
	as3_uints : as3_uint array;
	as3_floats : as3_float array;
	as3_idents : as3_ident array;
	as3_namespaces : as3_namespace array;
	as3_nsets : as3_ns_set array;
	mutable as3_names : as3_multi_name array;
	mutable as3_method_types : as3_method_type array;
	mutable as3_metadatas : as3_metadata array;
	mutable as3_classes : as3_class array;
	mutable as3_statics : as3_static array;
	mutable as3_inits : as3_static array;
	mutable as3_functions : as3_function array;
	mutable as3_unknown : string; (* only for partial parsing *)
}
