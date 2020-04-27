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
open JvmSignature

(* Opcode builder with stack management *)

exception EmptyStack

let terminates = function
	| OpDreturn
	| OpFreturn
	| OpIreturn
	| OpLreturn
	| OpAreturn
	| OpGoto _
	| OpGoto_w _
	| OpJsr _
	| OpJsr_w _
	| OpAthrow
	| OpReturn ->
		true
	| _ ->
		false

class jvm_stack = object(self)
	val mutable stack = [];
	val mutable stack_size = 0;
	val mutable max_stack_size = 0;

	method push js =
		stack <- js :: stack;
		stack_size <- stack_size + (signature_size js);
		if stack_size > max_stack_size then max_stack_size <- stack_size

	method pop = match stack with
		| js :: l ->
			stack <- l;
			stack_size <- stack_size - (signature_size js);
			js
		| [] ->
			raise EmptyStack

	method to_string =
		Printf.sprintf "[%s]" (String.concat ", " (List.map (generate_signature false) stack))

	method save =
		(stack,stack_size)

	method top = match stack with
		| hd :: _ -> hd
		| [] -> raise EmptyStack

	method restore ((stack',stack_size') : (jsignature list * int)) =
		stack <- stack';
		stack_size <- stack_size'

	method get_max_stack_size = max_stack_size
	method set_max_stack_size value = max_stack_size <- value

	method get_stack = stack

	method get_stack_items i =
		let rec loop acc i l =
			if i = 0 then List.rev acc
			else match l with
			| x :: l ->
				loop (x :: acc) (i - 1) l
			| [] ->
				raise EmptyStack
		in
		loop [] i stack
end

let s_vt = generate_method_signature true

let s_vtl l = Printf.sprintf "[%s]" (String.concat ", " (List.map s_vt l))

class builder pool = object(self)
	val stack = new jvm_stack;
	val lines = DynArray.create()
	val mutable last_line = -1
	val mutable current_line = -1

	(* ops *)
	val ops = DynArray.create();
	val stack_debug = DynArray.create()
	val mutable fp = 0
	val mutable terminated = false

	method is_terminated = terminated
	method set_terminated b = terminated <- b

	method debug_stack =
		let l = DynArray.to_list stack_debug in
		let opmax = ref 0 in
		let l = List.map (fun (op,_,after,line) ->
			let sop = JvmDebug.s_jcode pool op in
			if String.length sop > !opmax then opmax := String.length sop;
			let safter = s_vtl after in
			(line,sop,safter)
		) l in
		let s_ops = String.concat "\n\t\t" (List.map (fun (line,sop,safter) ->
			Printf.sprintf "%4i %*s %s" line !opmax sop safter
		) l) in
		s_ops

	method stack_error opcode expected actual =
		let s_ops = self#debug_stack in
		jerror
			(Printf.sprintf "Stack error\n\tops      :\n\t\t%s\n\t     line: %i\n\toperation: %s\n\texpected : %s\n\tactual   : %s"
				s_ops
				current_line
				(JvmDebug.s_jcode pool opcode)
				(s_vtl expected)
				(s_vtl actual)
			)

	method op opcode length expect return =
		if last_line <> current_line then begin
			last_line <- current_line;
			DynArray.add lines (fp,current_line)
		end;
		DynArray.add ops opcode;
		fp <- fp + length;
		let cur = stack#get_stack in
		List.iter (fun js ->
			let js' = try
				stack#pop
			with EmptyStack ->
				self#stack_error opcode expect cur;
				Globals.die ""
			in
			(* TODO: some unification or something? *)
			match js,js' with
			| (TObject _ | TTypeParameter _),(TObject _ | TTypeParameter _ | TArray _) -> () (* TODO ??? *)
			| TMethod _,TMethod _ -> ()
			| TMethod _,TObject(path,[]) when path = NativeSignatures.haxe_function_path -> ()
			| TTypeParameter _,TMethod _ -> ()
			| TObject _,TMethod _ -> ()
			| TMethod _,TObject _ -> ()
			| TArray _,TArray _ -> ()
			| TBool,TInt -> ()
			| TInt,TBool -> ()
			| TDouble,TInt -> ()
			| TInt,(TChar | TShort | TByte) -> ()
			| (TObject _ | TTypeParameter _),TUninitialized _ -> ()
			| _ ->
				if js <> js' then self#stack_error opcode expect cur
		) expect;
		List.iter stack#push (List.rev return);
		DynArray.add stack_debug (opcode,cur,stack#get_stack,current_line);
		if terminates opcode then terminated <- true

	method op_maybe_wide op opw i tl tr = match get_numeric_range_unsigned i with
		| Int8Range -> self#op op 2 tl tr
		| Int16Range -> self#op (OpWide opw) 4 tl tr
		| Int32Range -> Globals.die ""

	(* variables *)

	method iload ?(jsig=TInt) i = match i with
		| 0 -> self#op OpIload_0 1 [] [jsig]
		| 1 -> self#op OpIload_1 1 [] [jsig]
		| 2 -> self#op OpIload_2 1 [] [jsig]
		| 3 -> self#op OpIload_3 1 [] [jsig]
		| i -> self#op_maybe_wide (OpIload i) (OpWIload i) i [] [jsig]

	method lload i = match i with
		| 0 -> self#op OpLload_0 1 [] [TLong]
		| 1 -> self#op OpLload_1 1 [] [TLong]
		| 2 -> self#op OpLload_2 1 [] [TLong]
		| 3 -> self#op OpLload_3 1 [] [TLong]
		| i -> self#op_maybe_wide (OpLload i) (OpWLload i) i [] [TLong]

	method fload i = match i with
		| 0 -> self#op OpFload_0 1 [] [TFloat]
		| 1 -> self#op OpFload_1 1 [] [TFloat]
		| 2 -> self#op OpFload_2 1 [] [TFloat]
		| 3 -> self#op OpFload_3 1 [] [TFloat]
		| i -> self#op_maybe_wide (OpFload i) (OpWFload i) i [] [TFloat]

	method dload i = match i with
		| 0 -> self#op OpDload_0 1 [] [TDouble]
		| 1 -> self#op OpDload_1 1 [] [TDouble]
		| 2 -> self#op OpDload_2 1 [] [TDouble]
		| 3 -> self#op OpDload_3 1 [] [TDouble]
		| i -> self#op_maybe_wide (OpDload i) (OpWDload i) i [] [TDouble]

	method aload vtt i = match i with
		| 0 -> self#op OpAload_0 1 [] [vtt]
		| 1 -> self#op OpAload_1 1 [] [vtt]
		| 2 -> self#op OpAload_2 1 [] [vtt]
		| 3 -> self#op OpAload_3 1 [] [vtt]
		| i -> self#op_maybe_wide (OpAload i) (OpWAload i) i [] [vtt]

	method istore i = match i with
		| 0 -> self#op OpIstore_0 1 [TInt] []
		| 1 -> self#op OpIstore_1 1 [TInt] []
		| 2 -> self#op OpIstore_2 1 [TInt] []
		| 3 -> self#op OpIstore_3 1 [TInt] []
		| i -> self#op_maybe_wide (OpIstore i) (OpWIstore i) i [TInt] []

	method lstore i = match i with
		| 0 -> self#op OpLstore_0 1 [TLong] []
		| 1 -> self#op OpLstore_1 1 [TLong] []
		| 2 -> self#op OpLstore_2 1 [TLong] []
		| 3 -> self#op OpLstore_3 1 [TLong] []
		| i -> self#op_maybe_wide (OpLstore i) (OpWLstore i) i [TLong] []

	method fstore i = match i with
		| 0 -> self#op OpFstore_0 1 [TFloat] []
		| 1 -> self#op OpFstore_1 1 [TFloat] []
		| 2 -> self#op OpFstore_2 1 [TFloat] []
		| 3 -> self#op OpFstore_3 1 [TFloat] []
		| i -> self#op_maybe_wide (OpFstore i) (OpWFstore i) i [TFloat] []

	method dstore i = match i with
		| 0 -> self#op OpDstore_0 1 [TDouble] []
		| 1 -> self#op OpDstore_1 1 [TDouble] []
		| 2 -> self#op OpDstore_2 1 [TDouble] []
		| 3 -> self#op OpDstore_3 1 [TDouble] []
		| i -> self#op_maybe_wide (OpDstore i) (OpWDstore i) i [TDouble] []

	method astore vtt i = match i with
		| 0 -> self#op OpAstore_0 1 [vtt] []
		| 1 -> self#op OpAstore_1 1 [vtt] []
		| 2 -> self#op OpAstore_2 1 [vtt] []
		| 3 -> self#op OpAstore_3 1 [vtt] []
		| i -> self#op_maybe_wide (OpAstore i) (OpWAstore i) i [vtt] []

	method iinc index i = match get_numeric_range_unsigned index,get_numeric_range i with
		| Int8Range,Int8Range ->
			self#op (OpIinc(index,i)) 3 [] []
		| (Int8Range | Int16Range),(Int8Range | Int16Range) ->
			self#op (OpWide (OpWIinc(index,i))) 6 [] []
		| _ ->
			Globals.die ""

	(* conversions *)

	method d2f = self#op OpD2f 1 [TDouble] [TFloat]
	method d2i = self#op OpD2i 1 [TDouble] [TInt]
	method d2l = self#op OpD2l 1 [TDouble] [TLong]

	method f2d = self#op OpF2d 1 [TFloat] [TDouble]
	method f2i = self#op OpF2i 1 [TFloat] [TInt]
	method f2l = self#op OpF2l 1 [TFloat] [TLong]

	method i2b jsig = self#op OpI2b 1 [TInt] [jsig]
	method i2c = self#op OpI2c 1 [TInt] [TChar]
	method i2d = self#op OpI2d 1 [TInt] [TDouble]
	method i2f = self#op OpI2f 1 [TInt] [TFloat]
	method i2l = self#op OpI2l 1 [TInt] [TLong]
	method i2s = self#op OpI2s 1 [TInt] [TShort]

	method l2d = self#op OpL2d 1 [TLong] [TDouble]
	method l2f = self#op OpL2f 1 [TLong] [TFloat]
	method l2i = self#op OpL2i 1 [TLong] [TInt]

	(* arrays *)

	method newarray ta t = self#op (OpNewarray t) 2 [TInt] [ta]
	method anewarray ta offset = self#op (OpAnewarray offset) 3 [TInt] [ta]

	method arraylength ta = self#op OpArraylength 1 [ta] [TInt]

	method aastore ta te = self#op OpAastore 1 [te;TInt;ta] []
	method aaload ta te = self#op OpAaload 1 [TInt;ta] [te]

	method castore ta = self#op OpCastore 1 [TChar;TInt;ta] []
	method caload ta = self#op OpCaload 1 [TInt;ta] [TChar]

	method fastore ta = self#op OpFastore 1 [TFloat;TInt;ta] []
	method faload ta = self#op OpFaload 1 [TInt;ta] [TFloat]

	method lastore ta = self#op OpLastore 1 [TLong;TInt;ta] []
	method laload ta = self#op OpLaload 1 [TInt;ta] [TLong]

	method sastore ta = self#op OpSastore 1 [TShort;TInt;ta] []
	method saload ta = self#op OpSaload 1 [TInt;ta] [TShort]

	method iastore ta = self#op OpIastore 1 [TInt;TInt;ta] []
	method iaload ta = self#op OpIaload 1 [TInt;ta] [TInt]

	method dastore ta = self#op OpDastore 1 [TDouble;TInt;ta] []
	method daload ta = self#op OpDaload 1 [TInt;ta] [TDouble]

	method bastore jsig ta = self#op OpBastore 1 [jsig;TInt;ta] []
	method baload jsig ta = self#op OpBaload 1 [TInt;ta] [jsig]

	(* fields *)

	method getstatic offset t =
		self#op (OpGetstatic offset) 3 [] [t]

	method putstatic offset t =
		self#op (OpPutstatic offset) 3 [t] []

	method getfield offset tobj t =
		self#op (OpGetfield offset) 3 [tobj] [t]

	method putfield offset tobj t =
		self#op (OpPutfield offset) 3 [t;tobj] []

	(* calls *)

	method invokestatic offset tl tr =
		self#op (OpInvokestatic offset) 3 (List.rev tl) tr

	method invokevirtual offset t1 tl tr =
		self#op (OpInvokevirtual offset) 3 (List.rev (t1 :: tl)) tr

	method invokeinterface offset t1 tl tr =
		let count = List.fold_left (fun count jsig -> count + signature_size jsig) 1 tl in
		self#op (OpInvokeinterface(offset,count)) 5 (List.rev (t1 :: tl)) tr

	method invokespecial offset t1 tl tr =
		self#op (OpInvokespecial offset) 3 (List.rev (t1 :: tl)) tr

	method invokedynamic offset tl tr =
		self#op (OpInvokedynamic(offset)) 5 (List.rev tl) tr

	method new_ offset =
		self#op (OpNew offset) 3 [] [TUninitialized (Some fp)]

	(* return *)

	method return_void =
		self#op OpReturn 1 [] []

	method return_value t =
		let op = match t with
		| TInt | TBool | TByte | TChar | TShort -> OpIreturn
		| TLong -> OpLreturn
		| TFloat -> OpFreturn
		| TDouble -> OpDreturn
		| _ -> OpAreturn
		in
		self#op op 1 [t] []

	method athrow =
		self#op OpAthrow 1 [stack#top] []

	(* control flow *)

	method if_ cmp r = self#op (OpIf(cmp,r)) 3 [TBool] []

	method if_icmp cmp r = self#op (OpIf_icmp(cmp,r)) 3 [TInt;TInt] []

	method if_acmp_eq t1 t2 r = self#op (OpIf_acmpeq r) 3 [t1;t2] []

	method if_acmp_ne t1 t2 r = self#op (OpIf_acmpne r) 3 [t1;t2] []

	method if_null t r = self#op (OpIfnull r) 3 [t] []

	method if_nonnull t r = self#op (OpIfnonnull r) 3 [t] []

	method goto r = self#op (OpGoto r) 3 [] []

	method lookupswitch offset_def offsets =
		let pad = (fp + 1) mod 4 in
		let pad = if pad = 0 then pad else 4 - pad in
		self#op (OpLookupswitch(pad,offset_def,offsets)) (9 + pad + (Array.length offsets * 8)) [TInt] []

	method tableswitch offset_def low high offsets =
		let pad = (fp + 1) mod 4 in
		let pad = if pad = 0 then pad else 4 - pad in
		self#op (OpTableswitch(pad,offset_def,low,high,offsets)) (13 + pad + (Array.length offsets * 4)) [TInt] []

	(* compare *)

	method dcmpg = self#op OpDcmpg 1 [TDouble;TDouble] [TInt]
	method dcmpl = self#op OpDcmpl 1 [TDouble;TDouble] [TInt]

	method fcmpg = self#op OpFcmpg 1 [TFloat;TFloat] [TInt]
	method fcmpl = self#op OpFcmpl 1 [TFloat;TFloat] [TInt]

	method lcmpl = self#op OpLcmp 1 [TLong;TLong] [TInt]

	(* ops *)

	method iadd = self#op OpIadd 1 [TInt;TInt] [TInt]
	method isub = self#op OpIsub 1 [TInt;TInt] [TInt]
	method imul = self#op OpImul 1 [TInt;TInt] [TInt]
	method iand = self#op OpIand 1 [TInt;TInt] [TInt]
	method ior = self#op OpIor 1 [TInt;TInt] [TInt]
	method ixor = self#op OpIxor 1 [TInt;TInt] [TInt]
	method ishl = self#op OpIshl 1 [TInt;TInt] [TInt]
	method ishr = self#op OpIshr 1 [TInt;TInt] [TInt]
	method iushr = self#op OpIushr 1 [TInt;TInt] [TInt]
	method irem = self#op OpIrem 1 [TInt;TInt] [TInt]
	method ineg = self#op OpIneg 1 [TInt] [TInt]

	method ladd = self#op OpLadd 1 [TLong;TLong] [TLong]
	method lsub = self#op OpLsub 1 [TLong;TLong] [TLong]
	method lmul = self#op OpLmul 1 [TLong;TLong] [TLong]
	method ldiv = self#op OpLdiv 1 [TLong;TLong] [TLong]
	method land_ = self#op OpLand 1 [TLong;TLong] [TLong]
	method lor_ = self#op OpLor 1 [TLong;TLong] [TLong]
	method lxor_ = self#op OpLxor 1 [TLong;TLong] [TLong]
	method lshl = self#op OpLshl 1 [TInt;TLong] [TLong]
	method lshr = self#op OpLshr 1 [TInt;TLong] [TLong]
	method lushr = self#op OpLushr 1 [TInt;TLong] [TLong]
	method lrem = self#op OpLrem 1 [TLong;TLong] [TLong]
	method lneg = self#op OpLneg 1 [TLong] [TLong]

	method fadd = self#op OpFadd 1 [TFloat;TFloat] [TFloat]
	method fsub = self#op OpFsub 1 [TFloat;TFloat] [TFloat]
	method fmul = self#op OpFmul 1 [TFloat;TFloat] [TFloat]
	method fdiv = self#op OpFdiv 1 [TFloat;TFloat] [TFloat]
	method frem = self#op OpFrem 1 [TFloat;TFloat] [TFloat]
	method fneg = self#op OpFneg 1 [TFloat] [TFloat]

	method dadd = self#op OpDadd 1 [TDouble;TDouble] [TDouble]
	method dsub = self#op OpDsub 1 [TDouble;TDouble] [TDouble]
	method dmul = self#op OpDmul 1 [TDouble;TDouble] [TDouble]
	method ddiv = self#op OpDdiv 1 [TDouble;TDouble] [TDouble]
	method drem = self#op OpDrem 1 [TDouble;TDouble] [TDouble]
	method dneg = self#op OpDneg 1 [TDouble] [TDouble]

	(* stack *)

	method pop =
		let top = stack#top in
		self#op (if signature_size top = 1 then OpPop else OpPop2) 1 [top] []

	method dup = match stack#top with
		| TLong | TDouble -> self#op OpDup2 1 [] [stack#top]
		| _ -> self#op OpDup 1 [] [stack#top]

	method dup_x1 =
		let tl = stack#get_stack_items 2 in
		let tl2 = (List.hd tl :: (List.rev tl)) in
		match tl with
		| (TLong | TDouble) :: _ -> self#op OpDup2_x1 1 tl tl2
		| _ ->  self#op OpDup_x1 1 tl tl2

	method dup_x2 =
		let tl = stack#get_stack_items 3 in
		let sizes = List.map signature_size tl in
		match sizes,tl with
		| 2 :: 2 :: _,vt1 :: vt2 :: _ -> self#op OpDup2_x2 1 [vt1;vt2] [vt1;vt2;vt1]
		| 1 :: 2 :: _,vt1 :: vt2 :: _ -> self#op OpDup_x2 1 [vt1;vt2] [vt1;vt2;vt1]
		| 1 :: 1 :: 1 :: _,vt1 :: vt2 :: vt3 :: _ -> self#op OpDup_x2 1 tl [vt1;vt2;vt3;vt1]
		| 2 :: 1 :: 1 :: _,vt1 :: vt2 :: vt3 :: _ -> self#op OpDup2_x2 1 tl [vt1;vt2;vt3;vt1]
		| _ -> jerror "???"

	method swap =
		let tl = stack#get_stack_items 2 in
		self#op OpSwap 1 tl (List.rev tl)

	method checkcast_sig jsig =
		let offset = pool#add_type (generate_signature false jsig) in
		self#op (OpCheckcast offset) 3 [stack#top] [jsig]

	method checkcast path =
		let offset = pool#add_path path in
		let jsig = match path with
			| [],"Int" -> TInt
			| ["haxe" | "java"],"Int64" -> TLong
			| _ -> TObject(path,[])
		in
		self#op (OpCheckcast offset) 3 [stack#top] [jsig]

	method instanceof path =
		let offset = pool#add_path path in
		self#op (OpInstanceof offset) 3 [stack#top] [TInt]

	(* monitor *)

	method monitorenter =
		self#op OpMonitorenter 1 [stack#top] []

	method monitorexit =
		self#op OpMonitorexit 1 [stack#top] []

	(* constants *)

	method aconst_null t = self#op OpAconst_null 1 [] [t]

	method ldc offset vt =
		if offset < 0xFF then
			self#op (OpLdc offset) 2 [] [vt]
		else
			self#op (OpLdc_w offset) 3 [] [vt]

	method bconst b =
		self#op (if b then OpIconst_1 else OpIconst_0) 1 [] [TBool]

	method iconst i32 =
		let instr,count = match Int32.to_int i32 with
			| -1 -> OpIconst_m1,1
			| 0 -> OpIconst_0,1
			| 1 -> OpIconst_1,1
			| 2 -> OpIconst_2,1
			| 3 -> OpIconst_3,1
			| 4 -> OpIconst_4,1
			| 5 -> OpIconst_5,1
			| i ->
				match get_numeric_range i with
				| Int8Range ->
					OpBipush i,2
				| Int16Range ->
					OpSipush i,3
				| Int32Range ->
					let offset = pool#add (ConstInt i32) in
					if offset < 0xFF then
						OpLdc offset,2
					else
						OpLdc_w offset,3
		in
		self#op instr count [] [TInt]

	method dconst f =
		let instr,count = match f with
			| 0.0 -> OpDconst_0,1
			| 1.0 -> OpDconst_1,1
			| _ ->
				let offset = pool#add (ConstDouble f) in
				OpLdc2_w offset,3
		in
		self#op instr count [] [TDouble]

	method lconst d =
		let instr,count = if d = Int64.zero then
			OpLconst_0,1
		else if d = Int64.one then
			OpLconst_1,1
		else begin
			let offset = pool#add (ConstLong d) in
			OpLdc2_w offset,3
		end in
		self#op instr count [] [TLong]

	method fconst f =
		let instr,count = match f with
			| 0.0 -> OpFconst_0,1
			| 1.0 -> OpFconst_1,1
			| 2.0 -> OpFconst_2,1
			| _ ->
				let offset = pool#add (ConstFloat f) in
				OpLdc_w offset,3
		in
		self#op instr count [] [TFloat]

	method sconst t offset =
		self#ldc offset t

	method get_pool : JvmConstantPool.constant_pool = pool
	method get_fp = fp
	method get_ops = ops
	method get_stack = stack
	method get_max_stack_size = stack#get_max_stack_size

	method get_lines = lines
	method set_line line = current_line <- line

	method export_code =
		let ch = IO.output_bytes () in
		DynArray.iter (JvmWriter.write_opcode ch) ops;
		IO.close_out ch
end