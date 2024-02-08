(*
	The Haxe Compiler
	Copyright (C) 2005-2019  Haxe Foundation

	This program is free software; you can redistribute it and/or
	modify it under the terms of the GNU General Public License
	as published by the Free Software Foundation; either version 2
	of the License, or (at your option) any later version.

	This program is distributed in the hope that it will be useful,
	but WITHOUT ANY WARRANTY; without even the implied warranty of
	MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
	GNU General Public License for more details.

	You should have received a copy of the GNU General Public License
	along with this program; if not, write to the Free Software
	Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA  02110-1301, USA.
 *)

open JvmGlobals

(* Low-level data that is written out. *)

type jvm_attribute = {
	attr_index : jvm_constant_pool_index;
	attr_data  : bytes;
}

type jvm_field = {
	field_access_flags     : int;
	field_name_index       : int;
	field_descriptor_index : int;
	field_attributes       : jvm_attribute array;
}

type jvm_class = {
	class_minor_version : int;
	class_major_version : int;
	class_constant_pool : bytes;
	class_access_flags  : int;
	class_this_class    : int;
	class_super_class   : int;
	class_interfaces    : int array;
	class_fields        : jvm_field array;
	class_methods       : jvm_field array;
	class_attributes    : jvm_attribute array;
}

type jvm_exception = {
	exc_start_pc   : int;
	exc_end_pc     : int;
	exc_handler_pc : int;
	exc_catch_type : jvm_constant_pool_index option;
}

type jvm_code = {
	code_max_stack  : int;
	code_max_locals : int;
	code_code       : bytes;
	code_exceptions : jvm_exception array;
	code_attributes : jvm_attribute array;
}

(* Opcodes *)

type jbyte = int
type jshort = int
type jbranchoffset = int

type jcmp =
	| CmpEq
	| CmpNe
	| CmpLt
	| CmpGe
	| CmpGt
	| CmpLe

type opwide =
	| OpWIinc of jshort * jshort
	| OpWIload of jshort
	| OpWFload of jshort
	| OpWAload of jshort
	| OpWLload of jshort
	| OpWDload of jshort
	| OpWIstore of jshort
	| OpWFstore of jshort
	| OpWAstore of jshort
	| OpWLstore of jshort
	| OpWDstore of jshort

and jopcode =
	(* double *)
	| OpD2f
	| OpD2i
	| OpD2l
	| OpDadd
	| OpDaload
	| OpDastore
	| OpDcmpg
	| OpDcmpl
	| OpDdiv
	| OpDconst_0
	| OpDconst_1
	| OpDload_0
	| OpDload_1
	| OpDload_2
	| OpDload_3
	| OpDload of jbyte
	| OpDmul
	| OpDneg
	| OpDrem
	| OpDreturn
	| OpDstore_0
	| OpDstore_1
	| OpDstore_2
	| OpDstore_3
	| OpDstore of jbyte
	| OpDsub
	(* float *)
	| OpF2d
	| OpF2i
	| OpF2l
	| OpFadd
	| OpFaload
	| OpFastore
	| OpFcmpg
	| OpFcmpl
	| OpFdiv
	| OpFconst_0
	| OpFconst_1
	| OpFconst_2
	| OpFload_0
	| OpFload_1
	| OpFload_2
	| OpFload_3
	| OpFload of jbyte
	| OpFmul
	| OpFneg
	| OpFrem
	| OpFreturn
	| OpFstore_0
	| OpFstore_1
	| OpFstore_2
	| OpFstore_3
	| OpFstore of jbyte
	| OpFsub
	(* int *)
	| OpI2b
	| OpI2c
	| OpI2d
	| OpI2f
	| OpI2l
	| OpI2s
	| OpIadd
	| OpIaload
	| OpIand
	| OpIastore
	| OpIconst_m1
	| OpIconst_0
	| OpIconst_1
	| OpIconst_2
	| OpIconst_3
	| OpIconst_4
	| OpIconst_5
	| OpIdiv
	| OpIload_0
	| OpIload_1
	| OpIload_2
	| OpIload_3
	| OpIload of jbyte
	| OpImul
	| OpIneg
	| OpIor
	| OpIrem
	| OpIreturn
	| OpIshl
	| OpIshr
	| OpIstore_0
	| OpIstore_1
	| OpIstore_2
	| OpIstore_3
	| OpIstore of jbyte
	| OpIsub
	| OpIushr
	| OpIxor
	(* long *)
	| OpL2d
	| OpL2f
	| OpL2i
	| OpLadd
	| OpLaload
	| OpLand
	| OpLastore
	| OpLconst_0
	| OpLconst_1
	| OpLcmp
	| OpLdiv
	| OpLload_0
	| OpLload_1
	| OpLload_2
	| OpLload_3
	| OpLload of jbyte
	| OpLmul
	| OpLneg
	| OpLor
	| OpLrem
	| OpLreturn
	| OpLshl
	| OpLshr
	| OpLstore_0
	| OpLstore_1
	| OpLstore_2
	| OpLstore_3
	| OpLstore of jbyte
	| OpLsub
	| OpLushr
	| OpLxor
	(* short *)
	| OpSaload
	| OpSastore
	| OpSipush of jshort
	(* array *)
	| OpAaload
	| OpAastore
	| OpAnewarray of jvm_constant_pool_index
	| OpArraylength
	| OpBaload
	| OpBastore
	| OpBipush of jbyte
	| OpCaload
	| OpCastore
	| OpMultianewarray of jvm_constant_pool_index * jbyte
	| OpNewarray of jbyte
	(* reference *)
	| OpAload_0
	| OpAload_1
	| OpAload_2
	| OpAload_3
	| OpAload of jbyte
	| OpAreturn
	| OpAstore_0
	| OpAstore_1
	| OpAstore_2
	| OpAstore_3
	| OpAstore of jbyte
	(* object *)
	| OpNew of jvm_constant_pool_index
	| OpInstanceof of jvm_constant_pool_index
	| OpCheckcast of jvm_constant_pool_index
	| OpInvokedynamic of jvm_constant_pool_index
	| OpInvokeinterface of jvm_constant_pool_index * jbyte
	| OpInvokespecial of jvm_constant_pool_index
	| OpInvokestatic of jvm_constant_pool_index
	| OpInvokevirtual of jvm_constant_pool_index
	| OpGetfield of jvm_constant_pool_index
	| OpGetstatic of jvm_constant_pool_index
	| OpPutfield of jvm_constant_pool_index
	| OpPutstatic of jvm_constant_pool_index
	(* branching *)
	| OpIf_acmpeq of jbranchoffset ref
	| OpIf_acmpne of jbranchoffset ref
	| OpIf_icmp of jcmp * jbranchoffset ref
	| OpIf of jcmp * jbranchoffset ref
	| OpIfnonnull of jbranchoffset ref
	| OpIfnull of jbranchoffset ref
	| OpGoto of jbranchoffset ref
	| OpGoto_w of jbranchoffset ref
	| OpJsr of jbranchoffset ref
	| OpJsr_w of jbranchoffset ref
	(* stack *)
	| OpAconst_null
	| OpDup
	| OpDup_x1
	| OpDup_x2
	| OpDup2
	| OpDup2_x1
	| OpDup2_x2
	| OpLdc of jbyte
	| OpLdc_w of jvm_constant_pool_index
	| OpLdc2_w of jvm_constant_pool_index
	| OpNop
	| OpPop
	| OpPop2
	| OpSwap
	(* other *)
	| OpAthrow
	| OpIinc of jbyte * jbyte
	| OpLookupswitch of int (* num pad bytes *) * jbranchoffset ref (* default *) * (Int32.t * jbranchoffset ref) array
	| OpTableswitch of int (* num pad bytes *) * jbranchoffset ref (* default *) * Int32.t (* low *) * Int32.t (* high *) * jbranchoffset ref array
	| OpMonitorenter
	| OpMonitorexit
	| OpRet of jbyte
	| OpReturn
	| OpWide of opwide