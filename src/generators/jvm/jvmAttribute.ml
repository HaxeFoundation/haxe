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
open JvmData
open JvmVerificationTypeInfo
open JvmWriter

type j_annotation = {
	ann_type : int;
	ann_elements : (jvm_constant_pool_index * j_annotation_element_value) array;
}

and j_annotation_element_value = char * j_annotation_value

and j_annotation_value =
	| ValConst of int (* B, C, D, E, F, I, J, S, Z, s *)
	| ValEnum of int * int (* e *)
	| ValClass of int (* c *) (* V -> Void *)
	| ValAnnotation of j_annotation (* @ *)
	| ValArray of j_annotation_element_value array (* [ *)

(* https://docs.oracle.com/javase/specs/jvms/se7/html/jvms-4.html#jvms-4.7.4 *)
type j_stack_map_frame =
	| StackSame of int
	| Stack1StackItem of int * JvmVerificationTypeInfo.t
	| Stack1StackItemExtended of int * JvmVerificationTypeInfo.t
	| StackChop1 of int
	| StackChop2 of int
	| StackChop3 of int
	| StackSameExtended of int
	| StackAppend1 of int * JvmVerificationTypeInfo.t
	| StackAppend2 of int * JvmVerificationTypeInfo.t * JvmVerificationTypeInfo.t
	| StackAppend3 of int * JvmVerificationTypeInfo.t * JvmVerificationTypeInfo.t * JvmVerificationTypeInfo.t
	| StackFull of int * JvmVerificationTypeInfo.t array * JvmVerificationTypeInfo.t array

(* https://docs.oracle.com/javase/specs/jvms/se7/html/jvms-4.html#jvms-4.7.13 *)
type jvm_local_debug = {
	ld_start_pc : int;
	ld_length : int;
	ld_name_index : jvm_constant_pool_index;
	ld_descriptor_index : jvm_constant_pool_index;
	ld_index : int;
}

type jvm_inner_class = {
	ic_inner_class_info_index : int;
	ic_outer_class_info_index : int;
	ic_inner_name_index : int;
	ic_inner_class_access_flags : int;
}

type j_bootstrap_method = {
	bm_method_ref : jvm_constant_pool_index;
	bm_arguments : jvm_constant_pool_index array;
}

(* https://docs.oracle.com/javase/specs/jvms/se7/html/jvms-4.html#jvms-4.7 *)
type j_attribute =
	| AttributeConstantValue of jvm_constant_pool_index
	| AttributeCode of jvm_code
	| AttributeStackMapTable of j_stack_map_frame array
	| AttributeExceptions of jvm_constant_pool_index array
	| AttributeSourceFile of jvm_constant_pool_index
	| AttributeLineNumberTable of (int * int) array
	| AttributeSignature of jvm_constant_pool_index
	| AttributeLocalVariableTable of jvm_local_debug array
	| AttributeLocalVariableTypeTable of jvm_local_debug array
	| AttributeInnerClasses of jvm_inner_class array
	| AttributeEnclosingMethod of jvm_constant_pool_index * jvm_constant_pool_index
	| AttributeRuntimeVisibleAnnotations of j_annotation array
	| AttributeRuntimeVisibleParameterAnnotations of j_annotation array array
	| AttributeBootstrapMethods of j_bootstrap_method array

let write_verification_type ch = function
	| VTop -> write_byte ch 0
	| VInteger -> write_byte ch 1
	| VFloat -> write_byte ch 2
	| VDouble -> write_byte ch 3
	| VLong -> write_byte ch 4
	| VNull -> write_byte ch 5
	| VUninitializedThis -> write_byte ch 6
	| VObject i ->
		write_byte ch 7;
		write_ui16 ch i;
	| VUninitialized i ->
		write_byte ch 8;
		write_ui16 ch i

let write_stack_map_frame ch = function
	| StackSame i ->
		assert (i < 64);
		write_byte ch i
	| Stack1StackItem(i,t) ->
		assert (i < 64);
		write_byte ch (i + 64);
		write_verification_type ch t;
	| Stack1StackItemExtended(i,t) ->
		write_byte ch 247;
		write_ui16 ch i;
		write_verification_type ch t;
	| StackChop1 i1 ->
		write_byte ch 250;
		write_ui16 ch i1;
	| StackChop2 i1 ->
		write_byte ch 249;
		write_ui16 ch i1;
	| StackChop3 i1 ->
		write_byte ch 248;
		write_ui16 ch i1;
	| StackSameExtended i ->
		write_byte ch 251;
		write_ui16 ch i
	| StackAppend1(i1,t1) ->
		write_byte ch 252;
		write_ui16 ch i1;
		write_verification_type ch t1;
	| StackAppend2(i1,t1,t2) ->
		write_byte ch 253;
		write_ui16 ch i1;
		write_verification_type ch t1;
		write_verification_type ch t2;
	| StackAppend3(i1,t1,t2,t3) ->
		write_byte ch 254;
		write_ui16 ch i1;
		write_verification_type ch t1;
		write_verification_type ch t2;
		write_verification_type ch t3;
	| StackFull(i1,tl1,tl2) ->
		write_byte ch 255;
		write_ui16 ch i1;
		write_array16 ch write_verification_type tl1;
		write_array16 ch write_verification_type tl2

let write_constant pool ch const =
	let offset = pool#add const in
	write_ui16 ch offset

let rec write_annotation ch ann =
	write_ui16 ch ann.ann_type;
	write_array16 ch (fun _ (i,v) ->
		write_ui16 ch i;
		let rec loop _ (c,v) =
			write_byte ch (Char.code c);
			match v with
			| ValConst i ->
				write_ui16 ch i
			| ValEnum(i1,i2) ->
				write_ui16 ch i1;
				write_ui16 ch i2;
			| ValClass i ->
				write_ui16 ch i
			| ValAnnotation a ->
				write_annotation ch a
			| ValArray annl ->
				write_array16 ch loop annl
		in
		loop ch v
	) ann.ann_elements

let write_attribute pool jvma =
	let ch = IO.output_bytes () in
	let write_local_table table =
		write_array16 ch (fun _ d ->
			write_ui16 ch d.ld_start_pc;
			write_ui16 ch d.ld_length;
			write_ui16 ch d.ld_name_index;
			write_ui16 ch d.ld_descriptor_index;
			write_ui16 ch d.ld_index;
		) table
	in
	let name = match jvma with
	| AttributeConstantValue const ->
		write_ui16 ch const;
		"ConstantValue";
	| AttributeCode code ->
		write_ui16 ch code.code_max_stack;
		write_ui16 ch code.code_max_locals;
		write_ui32 ch (Bytes.length code.code_code);
		write_bytes ch code.code_code;
		write_array16 ch write_exception code.code_exceptions;
		write_jvm_attributes ch code.code_attributes;
		"Code";
	| AttributeStackMapTable stack_map ->
		write_array16 ch write_stack_map_frame stack_map;
		"StackMapTable"
	| AttributeExceptions a ->
		write_array16 ch (fun _ offset -> write_ui16 ch offset) a;
		"Exceptions"
	| AttributeInnerClasses icl ->
		write_array16 ch (fun ch ic ->
			write_ui16 ch ic.ic_inner_class_info_index;
			write_ui16 ch ic.ic_outer_class_info_index;
			write_ui16 ch ic.ic_inner_name_index;
			write_ui16 ch ic.ic_inner_class_access_flags;
		) icl;
		"InnerClasses"
	| AttributeEnclosingMethod(i1,i2) ->
		write_ui16 ch i1;
		write_ui16 ch i2;
		"EnclosingMethod"
	| AttributeSourceFile offset ->
		write_ui16 ch offset;
		"SourceFile";
	| AttributeLineNumberTable a ->
		write_array16 ch (fun _ (i1,i2) ->
			write_ui16 ch i1;
			write_ui16 ch i2
		) a;
		"LineNumberTable"
	| AttributeSignature offset ->
		write_ui16 ch offset;
		"Signature"
	| AttributeLocalVariableTable table ->
		write_local_table table;
		"LocalVariableTable"
	| AttributeLocalVariableTypeTable table ->
		write_local_table table;
		"LocalVariableTypeTable"
	| AttributeRuntimeVisibleAnnotations al ->
		write_array16 ch write_annotation al;
		"RuntimeVisibleAnnotations"
	| AttributeRuntimeVisibleParameterAnnotations al ->
		write_byte ch (Array.length al);
		Array.iter (write_array16 ch write_annotation) al;
		"RuntimeVisibleParameterAnnotations"
	| AttributeBootstrapMethods a ->
		write_array16 ch (fun _ bm ->
			write_ui16 ch bm.bm_method_ref;
			write_array16 ch (fun _ i -> write_ui16 ch i) bm.bm_arguments;
		) a;
		"BootstrapMethods"
	in
	{
		attr_index = pool#add (ConstUtf8 name);
		attr_data = IO.close_out ch
	}
