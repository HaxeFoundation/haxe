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

(* Low-level writing corresponding to jvmData. *)

let write_jvm_attribute ch jvma =
	write_ui16 ch jvma.attr_index;
	write_ui32 ch (Bytes.length jvma.attr_data);
	write_bytes ch jvma.attr_data

let write_jvm_attributes ch jvmal =
	write_array16 ch write_jvm_attribute jvmal

let write_jvm_field ch jvmf =
	write_ui16 ch jvmf.field_access_flags;
	write_ui16 ch jvmf.field_name_index;
	write_ui16 ch jvmf.field_descriptor_index;
	write_jvm_attributes ch jvmf.field_attributes

let write_jvm_class ch jvmc =
	write_byte ch 0xCA;
	write_byte ch 0xFE;
	write_byte ch 0xBA;
	write_byte ch 0xBE;
	write_ui16 ch jvmc.class_minor_version;
	write_ui16 ch jvmc.class_major_version;
	write_bytes ch jvmc.class_constant_pool;
	write_ui16 ch jvmc.class_access_flags;
	write_ui16 ch jvmc.class_this_class;
	write_ui16 ch jvmc.class_super_class;
	write_ui16 ch (Array.length jvmc.class_interfaces);
	Array.iter (write_ui16 ch) jvmc.class_interfaces;
	write_ui16 ch (Array.length jvmc.class_fields);
	Array.iter (write_jvm_field ch) jvmc.class_fields;
	write_ui16 ch (Array.length jvmc.class_methods);
	Array.iter (write_jvm_field ch) jvmc.class_methods;
	write_jvm_attributes ch jvmc.class_attributes

(* Level 2: Targeting JVM structures *)

let write_exception ch jvme =
	write_ui16 ch jvme.exc_start_pc;
	write_ui16 ch jvme.exc_end_pc;
	write_ui16 ch jvme.exc_handler_pc;
	match jvme.exc_catch_type with
	| None -> write_ui16 ch 0
	| Some t -> write_ui16 ch t

let write_opcode ch code =
  let w = write_byte ch in
  let wr i32 = write_byte ch (Int32.to_int i32) in
  (* TODO: probably don't need these *)
  let bp i =
    w ((i lsr 8) land 0xFF);
    w (i land 0xFF);
  in
  let b4 i =
    w ((i lsr 24) land 0xFF);
    w ((i lsr 16) land 0xFF);
    w ((i lsr 8) land 0xFF);
    w (i land 0xFF);
  in
  let b4r i32 =
    wr (Int32.logand (Int32.shift_right_logical i32 24) i320xFF);
    wr (Int32.logand (Int32.shift_right_logical i32 16) i320xFF);
    wr (Int32.logand (Int32.shift_right_logical i32 8) i320xFF);
    wr (Int32.logand i32 i320xFF);
  in
  match code with
    (* double *)
    | OpD2f -> w 0x90
    | OpD2i -> w 0x8e
    | OpD2l -> w 0x8f
    | OpDadd -> w 0x63
    | OpDaload -> w 0x31
    | OpDastore -> w 0x52
    | OpDcmpg -> w 0x98
    | OpDcmpl -> w 0x97
    | OpDdiv -> w 0x6f
    | OpDconst_0 -> w 0xe
    | OpDconst_1 -> w 0xf
    | OpDload_0 -> w 0x26
    | OpDload_1 -> w 0x27
    | OpDload_2 -> w 0x28
    | OpDload_3 -> w 0x29
    | OpDload i -> w 0x18; w i
    | OpDmul -> w 0x6b
    | OpDneg -> w 0x77
    | OpDrem -> w 0x73
    | OpDreturn -> w 0xaf
    | OpDstore_0 -> w 0x47
    | OpDstore_1 -> w 0x48
    | OpDstore_2 -> w 0x49
    | OpDstore_3 -> w 0x4a
    | OpDstore i -> w 0x39; w i
    | OpDsub -> w 0x67
    (* float *)
    | OpF2d -> w 0x8d
    | OpF2i -> w 0x8b
    | OpF2l -> w 0x8c
    | OpFadd -> w 0x62
    | OpFaload -> w 0x30
    | OpFastore -> w 0x51
    | OpFcmpg -> w 0x96
    | OpFcmpl -> w 0x95
    | OpFdiv -> w 0x6e
    | OpFconst_0 -> w 0xb
    | OpFconst_1 -> w 0xc
    | OpFconst_2 -> w 0xd
    | OpFload_0 -> w 0x22
    | OpFload_1 -> w 0x23
    | OpFload_2 -> w 0x24
    | OpFload_3 -> w 0x25
    | OpFload i -> w 0x17; w i
    | OpFmul -> w 0x6a
    | OpFneg -> w 0x76
    | OpFrem -> w 0x72
    | OpFreturn -> w 0xae
    | OpFstore_0 -> w 0x43
    | OpFstore_1 -> w 0x44
    | OpFstore_2 -> w 0x45
    | OpFstore_3 -> w 0x46
    | OpFstore i -> w 0x38; w i
    | OpFsub -> w 0x66
    (* int *)
    | OpI2b -> w 0x91
    | OpI2c -> w 0x92
    | OpI2d -> w 0x87
    | OpI2f -> w 0x86
    | OpI2l -> w 0x85
    | OpI2s -> w 0x93
    | OpIadd -> w 0x60
    | OpIaload -> w 0x2e
    | OpIand -> w 0x7e
    | OpIastore -> w 0x4f
    | OpIconst_m1 -> w 0x2
    | OpIconst_0 -> w 0x3
    | OpIconst_1 -> w 0x4
    | OpIconst_2 -> w 0x5
    | OpIconst_3 -> w 0x6
    | OpIconst_4 -> w 0x7
    | OpIconst_5 -> w 0x8
    | OpIdiv -> w 0x6c
    | OpIload_0 -> w 0x1a
    | OpIload_1 -> w 0x1b
    | OpIload_2 -> w 0x1c
    | OpIload_3 -> w 0x1d
    | OpIload i -> w 0x15; w i
    | OpImul -> w 0x68
    | OpIneg -> w 0x74
    | OpIor -> w 0x80
    | OpIrem -> w 0x70
    | OpIreturn -> w 0xac
    | OpIshl -> w 0x78
    | OpIshr -> w 0x7a
    | OpIstore_0 -> w 0x3b
    | OpIstore_1 -> w 0x3c
    | OpIstore_2 -> w 0x3d
    | OpIstore_3 -> w 0x3e
    | OpIstore i -> w 0x36; w i
    | OpIsub -> w 0x64
    | OpIushr -> w 0x7c
    | OpIxor -> w 0x82
    (* long *)
    | OpL2d -> w 0x8a
    | OpL2f -> w 0x89
    | OpL2i -> w 0x88
    | OpLadd -> w 0x61
    | OpLaload -> w 0x2f
    | OpLand -> w 0x7f
    | OpLastore -> w 0x50
	| OpLconst_0 -> w 0x9
	| OpLconst_1 -> w 0xa
    | OpLcmp -> w 0x94
    | OpLdiv -> w 0x6d
    | OpLload_0 -> w 0x1e
    | OpLload_1 -> w 0x1f
    | OpLload_2 -> w 0x20
    | OpLload_3 -> w 0x21
    | OpLload i ->  w 0x16; w i
    | OpLmul -> w 0x69
    | OpLneg -> w 0x75
    | OpLor -> w 0x81
    | OpLrem -> w 0x71
    | OpLreturn -> w 0xad
    | OpLshl -> w 0x79
    | OpLshr -> w 0x7b
    | OpLstore_0 -> w 0x3f
    | OpLstore_1 -> w 0x40
    | OpLstore_2 -> w 0x41
    | OpLstore_3 -> w 0x42
    | OpLstore i -> w 0x37; w i
    | OpLsub -> w 0x65
    | OpLushr -> w 0x7d
    | OpLxor -> w 0x83
    (* short *)
    | OpSaload -> w 0x35
    | OpSastore -> w 0x56
    | OpSipush i -> w 0x11; bp i
    (* array *)
    | OpAaload -> w 0x32
    | OpAastore -> w 0x53
    | OpAnewarray offset -> w 0xbd; bp offset
    | OpArraylength -> w 0xbe
    | OpBaload -> w 0x33
    | OpBastore -> w 0x54
    | OpBipush i -> w 0x10; w i
    | OpCaload -> w 0x34
    | OpCastore -> w 0x55
    | OpMultianewarray(offset,d) -> w 0xc5; bp offset; w d
    | OpNewarray t -> w 0xbc; w t
    (* reference *)
    | OpAload_0 -> w 0x2a
    | OpAload_1 -> w 0x2b
    | OpAload_2 -> w 0x2c
    | OpAload_3 -> w 0x2d
    | OpAload i -> w 0x19; w i
    | OpAreturn -> w 0xb0
    | OpAstore_0 -> w 0x4b
    | OpAstore_1 -> w 0x4c
    | OpAstore_2 -> w 0x4d
    | OpAstore_3 -> w 0x4e
    | OpAstore i -> w 0x3a; w i
    (* object *)
    | OpNew offset -> w 0xbb; bp offset
    | OpInstanceof offset -> w 0xc1; bp offset
    | OpCheckcast offset -> w 0xc0; bp offset
    | OpInvokedynamic offset -> w 0xba; bp offset; w 0; w 0 (* ??? *)
    | OpInvokeinterface(offset,c) -> w 0xb9; bp offset; w c; w 0
    | OpInvokespecial offset -> w 0xb7; bp offset
    | OpInvokestatic offset -> w 0xb8; bp offset
    | OpInvokevirtual offset -> w 0xb6; bp offset
    | OpGetfield offset -> w 0xb4; bp offset
    | OpGetstatic offset -> w 0xb2; bp offset
    | OpPutfield offset -> w 0xb5; bp offset
    | OpPutstatic offset -> w 0xb3; bp offset
    (* branching *)
    | OpIf_acmpeq i -> w 0xa5; bp !i
    | OpIf_acmpne i -> w 0xa6; bp !i
    | OpIf_icmp(cmp,i) ->
      begin match cmp with
        | CmpEq -> w 0x9f
        | CmpNe -> w 0xa0
        | CmpLt -> w 0xa1
        | CmpGe -> w 0xa2
        | CmpGt -> w 0xa3
        | CmpLe -> w 0xa4
      end;
      bp !i
    | OpIf(cmp,i) ->
      begin match cmp with
        | CmpEq -> w 0x99
        | CmpNe -> w 0x9a
        | CmpLt -> w 0x9b
        | CmpGe -> w 0x9c
        | CmpGt -> w 0x9d
        | CmpLe -> w 0x9e
      end;
      bp !i
    | OpIfnonnull i -> w 0xc7; bp !i
    | OpIfnull i -> w 0xc6; bp !i
    | OpGoto i -> w 0xa7; bp !i
    | OpGoto_w i -> w 0xc8; b4 !i
    | OpJsr i -> w 0xa8; bp !i
    | OpJsr_w i -> w 0xc9; b4 !i
    (* stack *)
    | OpAconst_null -> w 0x1
    | OpDup -> w 0x59
    | OpDup_x1 -> w 0x5a
    | OpDup_x2 -> w 0x5b
    | OpDup2 -> w 0x5c
    | OpDup2_x1 -> w 0x5d
    | OpDup2_x2 -> w 0x5e
    | OpLdc i -> w 0x12; w i
    | OpLdc_w i -> w 0x13; bp i
    | OpLdc2_w i -> w 0x14; bp i
    | OpNop -> w 0x0
    | OpPop -> w 0x57
    | OpPop2 -> w 0x58
    | OpSwap -> w 0x5f
    (* other *)
    | OpAthrow -> w 0xbf
    | OpIinc(i,c) -> w 0x84; w i; w c (* TODO: signed? *)
    | OpLookupswitch(pad,def,pairs) ->
		w 0xab;
		if pad > 0 then for _ = 0 to pad -1 do w 0 done;
		b4 !def;
		b4 (Array.length pairs);
		Array.iter (fun (i,offset) ->
			b4r i;
			b4 !offset
		) pairs;
    | OpTableswitch(pad,def,low,high,offsets) ->
		w 0xaa;
		if pad > 0 then for _ = 0 to pad -1 do w 0 done;
		b4 !def;
		b4r low;
		b4r high;
		Array.iter (fun offset ->
			b4 !offset
		) offsets;
    | OpMonitorenter -> w 0xc2
    | OpMonitorexit -> w 0xc3
    | OpRet i -> w 0xa9; w i
    | OpReturn -> w 0xb1
	| OpWide op ->
		w 0xc4;
		begin match op with
			| OpWIinc(i1,i2) -> w 0x84; bp i1; bp i2;
			| OpWIload i -> w 0x15; bp i
			| OpWFload i -> w 0x17; bp i
			| OpWAload i -> w 0x19; bp i
			| OpWLload i -> w 0x16; bp i
			| OpWDload i -> w 0x18; bp i
			| OpWIstore i -> w 0x36; bp i
			| OpWFstore i -> w 0x38; bp i
			| OpWAstore i -> w 0x3a; bp i
			| OpWLstore i -> w 0x37; bp i
			| OpWDstore i -> w 0x39; bp i
		end
