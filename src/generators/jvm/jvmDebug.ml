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

(* Printing debug functions *)

let s_const pool const =
	let rec loop depth const =
		if depth = 3 then
			"..."
		else begin
			let rec_find i = loop (depth + 1) (pool#get i) in
			match const with
			| ConstUtf8 s -> Printf.sprintf "Utf8 \"%s\"" s
			| ConstInt i32 -> Printf.sprintf "Int %i" (Int32.to_int i32)
			| ConstFloat f -> Printf.sprintf "Float %f" f
			| ConstLong i64 -> Printf.sprintf "Int %i" (Int64.to_int i64)
			| ConstDouble f -> Printf.sprintf "Double %f" f
			| ConstClass i -> Printf.sprintf "Class(%s)" (rec_find i)
			| ConstString i -> Printf.sprintf "String(%s)" (rec_find i)
			| ConstFieldref(i1,i2) -> Printf.sprintf "Fieldref(%s, %s)" (rec_find i1) (rec_find i2)
			| ConstMethodref(i1,i2) -> Printf.sprintf "Methodref(%s, %s)" (rec_find i1) (rec_find i2)
			| ConstInterfaceMethodref(i1,i2) -> Printf.sprintf "InterfaceMethodref(%s, %s)" (rec_find i1) (rec_find i2)
			| ConstNameAndType(i1,i2) -> Printf.sprintf "NameAndType(%s, %s)" (rec_find i1) (rec_find i2)
			| ConstMethodHandle(i1,i2) -> Printf.sprintf "MethodHandle(%i, %s)" i1 (rec_find i2)
			| ConstMethodType i -> Printf.sprintf "MethodType(%s)" (rec_find i)
			| ConstInvokeDynamic(i1,i2) -> Printf.sprintf "InvokeDynamic(%i, %s)" i1 (rec_find i2)
		end
	in
	loop 0 const

let s_const_nice pool const =
	let rec loop depth const =
		if depth = 3 then
			"..."
		else begin
			let rec_find i = loop (depth + 1) (pool#get i) in
			match const with
			| ConstUtf8 s -> Printf.sprintf "%s" s
			| ConstInt i32 -> Printf.sprintf "%i" (Int32.to_int i32)
			| ConstFloat f -> Printf.sprintf "%f" f
			| ConstLong i64 -> Printf.sprintf "%i" (Int64.to_int i64)
			| ConstDouble f -> Printf.sprintf "%f" f
			| ConstClass i -> Printf.sprintf "%s" (rec_find i)
			| ConstString i -> Printf.sprintf "%s" (rec_find i)
			| ConstFieldref(i1,i2) -> Printf.sprintf "%s.%s" (rec_find i1) (rec_find i2)
			| ConstMethodref(i1,i2) -> Printf.sprintf "%s.%s" (rec_find i1) (rec_find i2)
			| ConstInterfaceMethodref(i1,i2) -> Printf.sprintf "%s.%s" (rec_find i1) (rec_find i2)
			| ConstNameAndType(i1,i2) -> Printf.sprintf "%s:%s" (rec_find i1) (rec_find i2)
			| ConstMethodHandle(i1,i2) -> Printf.sprintf "MethodHandle(%i, %s)" i1 (rec_find i2)
			| ConstMethodType i -> Printf.sprintf "MethodType(%s)" (rec_find i)
			| ConstInvokeDynamic(i1,i2) -> Printf.sprintf "InvokeDynamic(%i, %s)" i1 (rec_find i2)
		end
	in
	loop 0 const

let s_jcode pool code =
  let wi s i = Printf.sprintf "%s %i" s i in
  let sc i = s_const_nice pool (pool#get i) in
  match code with
  (* double *)
  | OpD2f -> "d2f"
  | OpD2i -> "d2i"
  | OpD2l -> "d2l"
  | OpDadd -> "dadd"
  | OpDaload -> "daload"
  | OpDastore -> "dastore"
  | OpDcmpg -> "dcmpg"
  | OpDcmpl -> "dcmpl"
  | OpDdiv -> "ddiv"
  | OpDconst_0 -> "dconst_0"
  | OpDconst_1 -> "dconst_1"
  | OpDload_0 -> "dload_0"
  | OpDload_1 -> "dload_1"
  | OpDload_2 -> "dload_2"
  | OpDload_3 -> "dload_3"
  | OpDload i -> wi "dload" i
  | OpDmul -> "dmul"
  | OpDneg -> "dneg"
  | OpDrem -> "drem"
  | OpDreturn -> "dreturn"
  | OpDstore_0 -> "dstore_0"
  | OpDstore_1 -> "dstore_1"
  | OpDstore_2 -> "dstore_2"
  | OpDstore_3 -> "dstore_3"
  | OpDstore i -> wi "dstore" i
  | OpDsub -> "dsub"
  (* float *)
  | OpF2d -> "f2d"
  | OpF2i -> "f2i"
  | OpF2l -> "f2l"
  | OpFadd -> "fadd"
  | OpFaload -> "faload"
  | OpFastore -> "fastore"
  | OpFcmpg -> "fcmpg"
  | OpFcmpl -> "fcmpl"
  | OpFdiv -> "fdiv"
  | OpFconst_0 -> "fconst_0"
  | OpFconst_1 -> "fconst_1"
  | OpFconst_2 -> "fconst_2"
  | OpFload_0 -> "fload_0"
  | OpFload_1 -> "fload_1"
  | OpFload_2 -> "fload_2"
  | OpFload_3 -> "fload_3"
  | OpFload i -> wi "fload" i
  | OpFmul -> "fmul"
  | OpFneg -> "fneg"
  | OpFrem -> "frem"
  | OpFreturn -> "freturn"
  | OpFstore_0 -> "fstore_0"
  | OpFstore_1 -> "fstore_1"
  | OpFstore_2 -> "fstore_2"
  | OpFstore_3 -> "fstore_3"
  | OpFstore i -> wi "fstore" i
  | OpFsub -> "fsub"
  (* int *)
  | OpI2b -> "i2b"
  | OpI2c -> "i2c"
  | OpI2d -> "i2d"
  | OpI2f -> "i2f"
  | OpI2l -> "i2l"
  | OpI2s -> "i2s"
  | OpIadd -> "iadd"
  | OpIaload -> "iaload"
  | OpIand -> "iand"
  | OpIastore -> "iastore"
  | OpIconst_m1 -> "iconst_m1"
  | OpIconst_0 -> "iconst_0"
  | OpIconst_1 -> "iconst_1"
  | OpIconst_2 -> "iconst_2"
  | OpIconst_3 -> "iconst_3"
  | OpIconst_4 -> "iconst_4"
  | OpIconst_5 -> "iconst_5"
  | OpIdiv -> "idiv"
  | OpIload_0 -> "iload_0"
  | OpIload_1 -> "iload_1"
  | OpIload_2 -> "iload_2"
  | OpIload_3 -> "iload_3"
  | OpIload i -> wi "iload" i
  | OpImul -> "imul"
  | OpIneg -> "ineg"
  | OpIor -> "ior"
  | OpIrem -> "irem"
  | OpIreturn -> "ireturn"
  | OpIshl -> "ishl"
  | OpIshr -> "ishr"
  | OpIstore_0 -> "istore_0"
  | OpIstore_1 -> "istore_1"
  | OpIstore_2 -> "istore_2"
  | OpIstore_3 -> "istore_3"
  | OpIstore i -> wi "istore" i
  | OpIsub -> "isub"
  | OpIushr -> "iushr"
  | OpIxor -> "ixor"
  (* long *)
  | OpL2d -> "l2d"
  | OpL2f -> "l2f"
  | OpL2i -> "l2i"
  | OpLadd -> "ladd"
  | OpLaload -> "laload"
  | OpLand -> "land"
  | OpLastore -> "lastore"
  | OpLconst_0 -> "lconst_0"
  | OpLconst_1 -> "lconst_1"
  | OpLcmp -> "lcmp"
  | OpLdiv -> "ldiv"
  | OpLload_0 -> "lload_0"
  | OpLload_1 -> "lload_1"
  | OpLload_2 -> "lload_2"
  | OpLload_3 -> "lload_3"
  | OpLload i -> wi "lload" i
  | OpLmul -> "lmul"
  | OpLneg -> "lneg"
  | OpLor -> "lor"
  | OpLrem -> "lrem"
  | OpLreturn -> "lreturn"
  | OpLshl -> "lshl"
  | OpLshr -> "lshr"
  | OpLstore_0 -> "lstore_0"
  | OpLstore_1 -> "lstore_1"
  | OpLstore_2 -> "lstore_2"
  | OpLstore_3 -> "lstore_3"
  | OpLstore i -> wi "lstore" i
  | OpLsub -> "lsub"
  | OpLushr -> "lushr"
  | OpLxor -> "lxor"
  (* short *)
  | OpSaload -> "saload"
  | OpSastore -> "sastore"
  | OpSipush i -> wi "sipush" i
  (* array *)
  | OpAaload -> "aaload"
  | OpAastore -> "aastore"
  | OpAnewarray offset -> wi "anewarray" offset
  | OpArraylength -> "arraylength"
  | OpBaload -> "baload"
  | OpBastore -> "bastore"
  | OpBipush i -> wi "bipush" i
  | OpCaload -> "caload"
  | OpCastore -> "castore"
  | OpMultianewarray(path,i) -> "multinewarray" (* TODO *)
  | OpNewarray(jsig) -> "newarray" (* TODO *)
  (* reference *)
  | OpAload_0 -> "aload_0"
  | OpAload_1 -> "aload_1"
  | OpAload_2 -> "aload_2"
  | OpAload_3 -> "aload_3"
  | OpAload i -> wi "aload" i
  | OpAreturn -> "areturn"
  | OpAstore_0 -> "astore_0"
  | OpAstore_1 -> "astore_1"
  | OpAstore_2 -> "astore_2"
  | OpAstore_3 -> "astore_3"
  | OpAstore i -> wi "astore" i
  (* object *)
  | OpNew offset -> wi "new" offset
  | OpInstanceof offset -> wi "instanceof" offset
  | OpCheckcast offset -> wi "checkcast" offset
  | OpInvokedynamic arg -> "invokedynamic"
  | OpInvokeinterface(arg1,arg2) -> "invokeinterface"
  | OpInvokespecial arg1 -> Printf.sprintf "invokespecial %s" (sc arg1)
  | OpInvokestatic arg1 -> Printf.sprintf "invokestatic %s" (sc arg1)
  | OpInvokevirtual arg1 -> Printf.sprintf "invokevirtual %s" (sc arg1)
  | OpGetfield arg1 -> Printf.sprintf "getfield %s" (sc arg1)
  | OpGetstatic arg1 -> Printf.sprintf "getstatic %s" (sc arg1)
  | OpPutfield arg1 -> Printf.sprintf "putfield %s" (sc arg1)
  | OpPutstatic arg1 -> Printf.sprintf "putstatic %s" (sc arg1)
  (* branching *)
  | OpIf_acmpeq i -> wi "acmpeq" !i
  | OpIf_acmpne i -> wi "acmpne" !i
  | OpIf_icmp(cmp,i) -> wi "if_icmp" !i (* TODO *)
  | OpIf(cmp,i) -> wi "if" !i (* TODO *)
  | OpIfnonnull i -> wi "ifnotnull" !i
  | OpIfnull i -> wi "ifnull" !i
  | OpGoto i -> wi "goto" !i
  | OpGoto_w i -> wi "goto_w" !i
  | OpJsr i -> wi "jsr" !i
  | OpJsr_w i -> wi "jsr_w" !i
  (* stack *)
  | OpAconst_null -> "aconst_null"
  | OpDup -> "dup"
  | OpDup_x1 -> "dup_x1"
  | OpDup_x2 -> "dup_x2"
  | OpDup2 -> "dup2"
  | OpDup2_x1 -> "dup2_x1"
  | OpDup2_x2 -> "dup2_x2"
  | OpLdc i -> wi "ldc" i
  | OpLdc_w i -> wi "ldc_w" i
  | OpLdc2_w i -> wi "ldc2_w" i
  | OpNop -> "nop"
  | OpPop -> "pop"
  | OpPop2 -> "pop2"
  | OpSwap -> "swap"
  (* other *)
  | OpAthrow -> "athrow"
  | OpIinc(i1,i2) -> wi "iinc" i1 (* TODO *)
  | OpLookupswitch _ -> "lookupswitch"
  | OpMonitorenter -> "monitorenter"
  | OpMonitorexit -> "monitorexit"
  | OpRet i -> wi "ret" i
  | OpReturn -> "return"
  | OpTableswitch _ -> "tableswitch"
  | OpWide _ -> "wide"