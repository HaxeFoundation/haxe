package cs.system.reflection.emit;

/** Provides field representations of the Microsoft Intermediate Language (MSIL) instructions for emission by the  class members (such as ). */
@:native("System.Reflection.Emit.OpCodes")
extern class OpCodes {
	/** Adds two values and pushes the result onto the evaluation stack. */
	static var Add(default, never):cs.system.reflection.emit.OpCode;
	/** Adds two integers, performs an overflow check, and pushes the result onto the evaluation stack. */
	static var Add_Ovf(default, never):cs.system.reflection.emit.OpCode;
	/** Adds two unsigned integer values, performs an overflow check, and pushes the result onto the evaluation stack. */
	static var Add_Ovf_Un(default, never):cs.system.reflection.emit.OpCode;
	/** Computes the bitwise AND of two values and pushes the result onto the evaluation stack. */
	static var And(default, never):cs.system.reflection.emit.OpCode;
	/** Returns an unmanaged pointer to the argument list of the current method. */
	static var Arglist(default, never):cs.system.reflection.emit.OpCode;
	/** Transfers control to a target instruction if two values are equal. */
	static var Beq(default, never):cs.system.reflection.emit.OpCode;
	/** Transfers control to a target instruction (short form) if two values are equal. */
	static var Beq_S(default, never):cs.system.reflection.emit.OpCode;
	/** Transfers control to a target instruction if the first value is greater than or equal to the second value. */
	static var Bge(default, never):cs.system.reflection.emit.OpCode;
	/** Transfers control to a target instruction (short form) if the first value is greater than or equal to the second value. */
	static var Bge_S(default, never):cs.system.reflection.emit.OpCode;
	/** Transfers control to a target instruction if the first value is greater than the second value, when comparing unsigned integer values or unordered float values. */
	static var Bge_Un(default, never):cs.system.reflection.emit.OpCode;
	/** Transfers control to a target instruction (short form) if the first value is greater than the second value, when comparing unsigned integer values or unordered float values. */
	static var Bge_Un_S(default, never):cs.system.reflection.emit.OpCode;
	/** Transfers control to a target instruction if the first value is greater than the second value. */
	static var Bgt(default, never):cs.system.reflection.emit.OpCode;
	/** Transfers control to a target instruction (short form) if the first value is greater than the second value. */
	static var Bgt_S(default, never):cs.system.reflection.emit.OpCode;
	/** Transfers control to a target instruction if the first value is greater than the second value, when comparing unsigned integer values or unordered float values. */
	static var Bgt_Un(default, never):cs.system.reflection.emit.OpCode;
	/** Transfers control to a target instruction (short form) if the first value is greater than the second value, when comparing unsigned integer values or unordered float values. */
	static var Bgt_Un_S(default, never):cs.system.reflection.emit.OpCode;
	/** Transfers control to a target instruction if the first value is less than or equal to the second value. */
	static var Ble(default, never):cs.system.reflection.emit.OpCode;
	/** Transfers control to a target instruction (short form) if the first value is less than or equal to the second value. */
	static var Ble_S(default, never):cs.system.reflection.emit.OpCode;
	/** Transfers control to a target instruction if the first value is less than or equal to the second value, when comparing unsigned integer values or unordered float values. */
	static var Ble_Un(default, never):cs.system.reflection.emit.OpCode;
	/** Transfers control to a target instruction (short form) if the first value is less than or equal to the second value, when comparing unsigned integer values or unordered float values. */
	static var Ble_Un_S(default, never):cs.system.reflection.emit.OpCode;
	/** Transfers control to a target instruction if the first value is less than the second value. */
	static var Blt(default, never):cs.system.reflection.emit.OpCode;
	/** Transfers control to a target instruction (short form) if the first value is less than the second value. */
	static var Blt_S(default, never):cs.system.reflection.emit.OpCode;
	/** Transfers control to a target instruction if the first value is less than the second value, when comparing unsigned integer values or unordered float values. */
	static var Blt_Un(default, never):cs.system.reflection.emit.OpCode;
	/** Transfers control to a target instruction (short form) if the first value is less than the second value, when comparing unsigned integer values or unordered float values. */
	static var Blt_Un_S(default, never):cs.system.reflection.emit.OpCode;
	/** Transfers control to a target instruction when two unsigned integer values or unordered float values are not equal. */
	static var Bne_Un(default, never):cs.system.reflection.emit.OpCode;
	/** Transfers control to a target instruction (short form) when two unsigned integer values or unordered float values are not equal. */
	static var Bne_Un_S(default, never):cs.system.reflection.emit.OpCode;
	/** Converts a value type to an object reference (type ). */
	static var Box(default, never):cs.system.reflection.emit.OpCode;
	/** Unconditionally transfers control to a target instruction. */
	static var Br(default, never):cs.system.reflection.emit.OpCode;
	/** Unconditionally transfers control to a target instruction (short form). */
	static var Br_S(default, never):cs.system.reflection.emit.OpCode;
	/** Signals the Common Language Infrastructure (CLI) to inform the debugger that a break point has been tripped. */
	static var Break(default, never):cs.system.reflection.emit.OpCode;
	/** Transfers control to a target instruction if  is , a null reference ( in Visual Basic), or zero. */
	static var Brfalse(default, never):cs.system.reflection.emit.OpCode;
	/** Transfers control to a target instruction if  is , a null reference, or zero. */
	static var Brfalse_S(default, never):cs.system.reflection.emit.OpCode;
	/** Transfers control to a target instruction if  is , not null, or non-zero. */
	static var Brtrue(default, never):cs.system.reflection.emit.OpCode;
	/** Transfers control to a target instruction (short form) if  is , not null, or non-zero. */
	static var Brtrue_S(default, never):cs.system.reflection.emit.OpCode;
	/** Calls the method indicated by the passed method descriptor. */
	static var Call(default, never):cs.system.reflection.emit.OpCode;
	/** Calls the method indicated on the evaluation stack (as a pointer to an entry point) with arguments described by a calling convention. */
	static var Calli(default, never):cs.system.reflection.emit.OpCode;
	/** Calls a late-bound method on an object, pushing the return value onto the evaluation stack. */
	static var Callvirt(default, never):cs.system.reflection.emit.OpCode;
	/** Attempts to cast an object passed by reference to the specified class. */
	static var Castclass(default, never):cs.system.reflection.emit.OpCode;
	/** Compares two values. If they are equal, the integer value 1 ) is pushed onto the evaluation stack; otherwise 0 () is pushed onto the evaluation stack. */
	static var Ceq(default, never):cs.system.reflection.emit.OpCode;
	/** Compares two values. If the first value is greater than the second, the integer value 1 ) is pushed onto the evaluation stack; otherwise 0 () is pushed onto the evaluation stack. */
	static var Cgt(default, never):cs.system.reflection.emit.OpCode;
	/** Compares two unsigned or unordered values. If the first value is greater than the second, the integer value 1 ) is pushed onto the evaluation stack; otherwise 0 () is pushed onto the evaluation stack. */
	static var Cgt_Un(default, never):cs.system.reflection.emit.OpCode;
	/** Throws  if value is not a finite number. */
	static var Ckfinite(default, never):cs.system.reflection.emit.OpCode;
	/** Compares two values. If the first value is less than the second, the integer value 1 ) is pushed onto the evaluation stack; otherwise 0 () is pushed onto the evaluation stack. */
	static var Clt(default, never):cs.system.reflection.emit.OpCode;
	/** Compares the unsigned or unordered values  and . If  is less than , then the integer value 1 ) is pushed onto the evaluation stack; otherwise 0 () is pushed onto the evaluation stack. */
	static var Clt_Un(default, never):cs.system.reflection.emit.OpCode;
	/** Constrains the type on which a virtual method call is made. */
	static var Constrained(default, never):cs.system.reflection.emit.OpCode;
	/** Converts the value on top of the evaluation stack to . */
	static var Conv_I(default, never):cs.system.reflection.emit.OpCode;
	/** Converts the value on top of the evaluation stack to , then extends (pads) it to . */
	static var Conv_I1(default, never):cs.system.reflection.emit.OpCode;
	/** Converts the value on top of the evaluation stack to , then extends (pads) it to . */
	static var Conv_I2(default, never):cs.system.reflection.emit.OpCode;
	/** Converts the value on top of the evaluation stack to . */
	static var Conv_I4(default, never):cs.system.reflection.emit.OpCode;
	/** Converts the value on top of the evaluation stack to . */
	static var Conv_I8(default, never):cs.system.reflection.emit.OpCode;
	/** Converts the signed value on top of the evaluation stack to signed , throwing  on overflow. */
	static var Conv_Ovf_I(default, never):cs.system.reflection.emit.OpCode;
	/** Converts the unsigned value on top of the evaluation stack to signed , throwing  on overflow. */
	static var Conv_Ovf_I_Un(default, never):cs.system.reflection.emit.OpCode;
	/** Converts the signed value on top of the evaluation stack to signed  and extends it to , throwing  on overflow. */
	static var Conv_Ovf_I1(default, never):cs.system.reflection.emit.OpCode;
	/** Converts the unsigned value on top of the evaluation stack to signed  and extends it to , throwing  on overflow. */
	static var Conv_Ovf_I1_Un(default, never):cs.system.reflection.emit.OpCode;
	/** Converts the signed value on top of the evaluation stack to signed  and extending it to , throwing  on overflow. */
	static var Conv_Ovf_I2(default, never):cs.system.reflection.emit.OpCode;
	/** Converts the unsigned value on top of the evaluation stack to signed  and extends it to , throwing  on overflow. */
	static var Conv_Ovf_I2_Un(default, never):cs.system.reflection.emit.OpCode;
	/** Converts the signed value on top of the evaluation stack to signed , throwing  on overflow. */
	static var Conv_Ovf_I4(default, never):cs.system.reflection.emit.OpCode;
	/** Converts the unsigned value on top of the evaluation stack to signed , throwing  on overflow. */
	static var Conv_Ovf_I4_Un(default, never):cs.system.reflection.emit.OpCode;
	/** Converts the signed value on top of the evaluation stack to signed , throwing  on overflow. */
	static var Conv_Ovf_I8(default, never):cs.system.reflection.emit.OpCode;
	/** Converts the unsigned value on top of the evaluation stack to signed , throwing  on overflow. */
	static var Conv_Ovf_I8_Un(default, never):cs.system.reflection.emit.OpCode;
	/** Converts the signed value on top of the evaluation stack to , throwing  on overflow. */
	static var Conv_Ovf_U(default, never):cs.system.reflection.emit.OpCode;
	/** Converts the unsigned value on top of the evaluation stack to , throwing  on overflow. */
	static var Conv_Ovf_U_Un(default, never):cs.system.reflection.emit.OpCode;
	/** Converts the signed value on top of the evaluation stack to  and extends it to , throwing  on overflow. */
	static var Conv_Ovf_U1(default, never):cs.system.reflection.emit.OpCode;
	/** Converts the unsigned value on top of the evaluation stack to  and extends it to , throwing  on overflow. */
	static var Conv_Ovf_U1_Un(default, never):cs.system.reflection.emit.OpCode;
	/** Converts the signed value on top of the evaluation stack to  and extends it to , throwing  on overflow. */
	static var Conv_Ovf_U2(default, never):cs.system.reflection.emit.OpCode;
	/** Converts the unsigned value on top of the evaluation stack to  and extends it to , throwing  on overflow. */
	static var Conv_Ovf_U2_Un(default, never):cs.system.reflection.emit.OpCode;
	/** Converts the signed value on top of the evaluation stack to , throwing  on overflow. */
	static var Conv_Ovf_U4(default, never):cs.system.reflection.emit.OpCode;
	/** Converts the unsigned value on top of the evaluation stack to , throwing  on overflow. */
	static var Conv_Ovf_U4_Un(default, never):cs.system.reflection.emit.OpCode;
	/** Converts the signed value on top of the evaluation stack to , throwing  on overflow. */
	static var Conv_Ovf_U8(default, never):cs.system.reflection.emit.OpCode;
	/** Converts the unsigned value on top of the evaluation stack to , throwing  on overflow. */
	static var Conv_Ovf_U8_Un(default, never):cs.system.reflection.emit.OpCode;
	/** Converts the unsigned integer value on top of the evaluation stack to . */
	static var Conv_R_Un(default, never):cs.system.reflection.emit.OpCode;
	/** Converts the value on top of the evaluation stack to . */
	static var Conv_R4(default, never):cs.system.reflection.emit.OpCode;
	/** Converts the value on top of the evaluation stack to . */
	static var Conv_R8(default, never):cs.system.reflection.emit.OpCode;
	/** Converts the value on top of the evaluation stack to , and extends it to . */
	static var Conv_U(default, never):cs.system.reflection.emit.OpCode;
	/** Converts the value on top of the evaluation stack to , and extends it to . */
	static var Conv_U1(default, never):cs.system.reflection.emit.OpCode;
	/** Converts the value on top of the evaluation stack to , and extends it to . */
	static var Conv_U2(default, never):cs.system.reflection.emit.OpCode;
	/** Converts the value on top of the evaluation stack to , and extends it to . */
	static var Conv_U4(default, never):cs.system.reflection.emit.OpCode;
	/** Converts the value on top of the evaluation stack to , and extends it to . */
	static var Conv_U8(default, never):cs.system.reflection.emit.OpCode;
	/** Copies a specified number bytes from a source address to a destination address. */
	static var Cpblk(default, never):cs.system.reflection.emit.OpCode;
	/** Copies the value type located at the address of an object (type ,  or ) to the address of the destination object (type ,  or ). */
	static var Cpobj(default, never):cs.system.reflection.emit.OpCode;
	/** Divides two values and pushes the result as a floating-point (type ) or quotient (type ) onto the evaluation stack. */
	static var Div(default, never):cs.system.reflection.emit.OpCode;
	/** Divides two unsigned integer values and pushes the result () onto the evaluation stack. */
	static var Div_Un(default, never):cs.system.reflection.emit.OpCode;
	/** Copies the current topmost value on the evaluation stack, and then pushes the copy onto the evaluation stack. */
	static var Dup(default, never):cs.system.reflection.emit.OpCode;
	/** Transfers control from the  clause of an exception back to the Common Language Infrastructure (CLI) exception handler. */
	static var Endfilter(default, never):cs.system.reflection.emit.OpCode;
	/** Transfers control from the  or  clause of an exception block back to the Common Language Infrastructure (CLI) exception handler. */
	static var Endfinally(default, never):cs.system.reflection.emit.OpCode;
	/** Initializes a specified block of memory at a specific address to a given size and initial value. */
	static var Initblk(default, never):cs.system.reflection.emit.OpCode;
	/** Initializes each field of the value type at a specified address to a null reference or a 0 of the appropriate primitive type. */
	static var Initobj(default, never):cs.system.reflection.emit.OpCode;
	/** Tests whether an object reference (type ) is an instance of a particular class. */
	static var Isinst(default, never):cs.system.reflection.emit.OpCode;
	/** Exits current method and jumps to specified method. */
	static var Jmp(default, never):cs.system.reflection.emit.OpCode;
	/** Loads an argument (referenced by a specified index value) onto the stack. */
	static var Ldarg(default, never):cs.system.reflection.emit.OpCode;
	/** Loads the argument at index 0 onto the evaluation stack. */
	static var Ldarg_0(default, never):cs.system.reflection.emit.OpCode;
	/** Loads the argument at index 1 onto the evaluation stack. */
	static var Ldarg_1(default, never):cs.system.reflection.emit.OpCode;
	/** Loads the argument at index 2 onto the evaluation stack. */
	static var Ldarg_2(default, never):cs.system.reflection.emit.OpCode;
	/** Loads the argument at index 3 onto the evaluation stack. */
	static var Ldarg_3(default, never):cs.system.reflection.emit.OpCode;
	/** Loads the argument (referenced by a specified short form index) onto the evaluation stack. */
	static var Ldarg_S(default, never):cs.system.reflection.emit.OpCode;
	/** Load an argument address onto the evaluation stack. */
	static var Ldarga(default, never):cs.system.reflection.emit.OpCode;
	/** Load an argument address, in short form, onto the evaluation stack. */
	static var Ldarga_S(default, never):cs.system.reflection.emit.OpCode;
	/** Pushes a supplied value of type  onto the evaluation stack as an . */
	static var Ldc_I4(default, never):cs.system.reflection.emit.OpCode;
	/** Pushes the integer value of 0 onto the evaluation stack as an . */
	static var Ldc_I4_0(default, never):cs.system.reflection.emit.OpCode;
	/** Pushes the integer value of 1 onto the evaluation stack as an . */
	static var Ldc_I4_1(default, never):cs.system.reflection.emit.OpCode;
	/** Pushes the integer value of 2 onto the evaluation stack as an . */
	static var Ldc_I4_2(default, never):cs.system.reflection.emit.OpCode;
	/** Pushes the integer value of 3 onto the evaluation stack as an . */
	static var Ldc_I4_3(default, never):cs.system.reflection.emit.OpCode;
	/** Pushes the integer value of 4 onto the evaluation stack as an . */
	static var Ldc_I4_4(default, never):cs.system.reflection.emit.OpCode;
	/** Pushes the integer value of 5 onto the evaluation stack as an . */
	static var Ldc_I4_5(default, never):cs.system.reflection.emit.OpCode;
	/** Pushes the integer value of 6 onto the evaluation stack as an . */
	static var Ldc_I4_6(default, never):cs.system.reflection.emit.OpCode;
	/** Pushes the integer value of 7 onto the evaluation stack as an . */
	static var Ldc_I4_7(default, never):cs.system.reflection.emit.OpCode;
	/** Pushes the integer value of 8 onto the evaluation stack as an . */
	static var Ldc_I4_8(default, never):cs.system.reflection.emit.OpCode;
	/** Pushes the integer value of -1 onto the evaluation stack as an . */
	static var Ldc_I4_M1(default, never):cs.system.reflection.emit.OpCode;
	/** Pushes the supplied  value onto the evaluation stack as an , short form. */
	static var Ldc_I4_S(default, never):cs.system.reflection.emit.OpCode;
	/** Pushes a supplied value of type  onto the evaluation stack as an . */
	static var Ldc_I8(default, never):cs.system.reflection.emit.OpCode;
	/** Pushes a supplied value of type  onto the evaluation stack as type  (float). */
	static var Ldc_R4(default, never):cs.system.reflection.emit.OpCode;
	/** Pushes a supplied value of type  onto the evaluation stack as type  (float). */
	static var Ldc_R8(default, never):cs.system.reflection.emit.OpCode;
	/** Loads the element at a specified array index onto the top of the evaluation stack as the type specified in the instruction. */
	static var Ldelem(default, never):cs.system.reflection.emit.OpCode;
	/** Loads the element with type  at a specified array index onto the top of the evaluation stack as a . */
	static var Ldelem_I(default, never):cs.system.reflection.emit.OpCode;
	/** Loads the element with type  at a specified array index onto the top of the evaluation stack as an . */
	static var Ldelem_I1(default, never):cs.system.reflection.emit.OpCode;
	/** Loads the element with type  at a specified array index onto the top of the evaluation stack as an . */
	static var Ldelem_I2(default, never):cs.system.reflection.emit.OpCode;
	/** Loads the element with type  at a specified array index onto the top of the evaluation stack as an . */
	static var Ldelem_I4(default, never):cs.system.reflection.emit.OpCode;
	/** Loads the element with type  at a specified array index onto the top of the evaluation stack as an . */
	static var Ldelem_I8(default, never):cs.system.reflection.emit.OpCode;
	/** Loads the element with type  at a specified array index onto the top of the evaluation stack as type  (float). */
	static var Ldelem_R4(default, never):cs.system.reflection.emit.OpCode;
	/** Loads the element with type  at a specified array index onto the top of the evaluation stack as type  (float). */
	static var Ldelem_R8(default, never):cs.system.reflection.emit.OpCode;
	/** Loads the element containing an object reference at a specified array index onto the top of the evaluation stack as type  (object reference). */
	static var Ldelem_Ref(default, never):cs.system.reflection.emit.OpCode;
	/** Loads the element with type  at a specified array index onto the top of the evaluation stack as an . */
	static var Ldelem_U1(default, never):cs.system.reflection.emit.OpCode;
	/** Loads the element with type  at a specified array index onto the top of the evaluation stack as an . */
	static var Ldelem_U2(default, never):cs.system.reflection.emit.OpCode;
	/** Loads the element with type  at a specified array index onto the top of the evaluation stack as an . */
	static var Ldelem_U4(default, never):cs.system.reflection.emit.OpCode;
	/** Loads the address of the array element at a specified array index onto the top of the evaluation stack as type  (managed pointer). */
	static var Ldelema(default, never):cs.system.reflection.emit.OpCode;
	/** Finds the value of a field in the object whose reference is currently on the evaluation stack. */
	static var Ldfld(default, never):cs.system.reflection.emit.OpCode;
	/** Finds the address of a field in the object whose reference is currently on the evaluation stack. */
	static var Ldflda(default, never):cs.system.reflection.emit.OpCode;
	/** Pushes an unmanaged pointer (type ) to the native code implementing a specific method onto the evaluation stack. */
	static var Ldftn(default, never):cs.system.reflection.emit.OpCode;
	/** Loads a value of type  as a  onto the evaluation stack indirectly. */
	static var Ldind_I(default, never):cs.system.reflection.emit.OpCode;
	/** Loads a value of type  as an  onto the evaluation stack indirectly. */
	static var Ldind_I1(default, never):cs.system.reflection.emit.OpCode;
	/** Loads a value of type  as an  onto the evaluation stack indirectly. */
	static var Ldind_I2(default, never):cs.system.reflection.emit.OpCode;
	/** Loads a value of type  as an  onto the evaluation stack indirectly. */
	static var Ldind_I4(default, never):cs.system.reflection.emit.OpCode;
	/** Loads a value of type  as an  onto the evaluation stack indirectly. */
	static var Ldind_I8(default, never):cs.system.reflection.emit.OpCode;
	/** Loads a value of type  as a type  (float) onto the evaluation stack indirectly. */
	static var Ldind_R4(default, never):cs.system.reflection.emit.OpCode;
	/** Loads a value of type  as a type  (float) onto the evaluation stack indirectly. */
	static var Ldind_R8(default, never):cs.system.reflection.emit.OpCode;
	/** Loads an object reference as a type  (object reference) onto the evaluation stack indirectly. */
	static var Ldind_Ref(default, never):cs.system.reflection.emit.OpCode;
	/** Loads a value of type  as an  onto the evaluation stack indirectly. */
	static var Ldind_U1(default, never):cs.system.reflection.emit.OpCode;
	/** Loads a value of type  as an  onto the evaluation stack indirectly. */
	static var Ldind_U2(default, never):cs.system.reflection.emit.OpCode;
	/** Loads a value of type  as an  onto the evaluation stack indirectly. */
	static var Ldind_U4(default, never):cs.system.reflection.emit.OpCode;
	/** Pushes the number of elements of a zero-based, one-dimensional array onto the evaluation stack. */
	static var Ldlen(default, never):cs.system.reflection.emit.OpCode;
	/** Loads the local variable at a specific index onto the evaluation stack. */
	static var Ldloc(default, never):cs.system.reflection.emit.OpCode;
	/** Loads the local variable at index 0 onto the evaluation stack. */
	static var Ldloc_0(default, never):cs.system.reflection.emit.OpCode;
	/** Loads the local variable at index 1 onto the evaluation stack. */
	static var Ldloc_1(default, never):cs.system.reflection.emit.OpCode;
	/** Loads the local variable at index 2 onto the evaluation stack. */
	static var Ldloc_2(default, never):cs.system.reflection.emit.OpCode;
	/** Loads the local variable at index 3 onto the evaluation stack. */
	static var Ldloc_3(default, never):cs.system.reflection.emit.OpCode;
	/** Loads the local variable at a specific index onto the evaluation stack, short form. */
	static var Ldloc_S(default, never):cs.system.reflection.emit.OpCode;
	/** Loads the address of the local variable at a specific index onto the evaluation stack. */
	static var Ldloca(default, never):cs.system.reflection.emit.OpCode;
	/** Loads the address of the local variable at a specific index onto the evaluation stack, short form. */
	static var Ldloca_S(default, never):cs.system.reflection.emit.OpCode;
	/** Pushes a null reference (type ) onto the evaluation stack. */
	static var Ldnull(default, never):cs.system.reflection.emit.OpCode;
	/** Copies the value type object pointed to by an address to the top of the evaluation stack. */
	static var Ldobj(default, never):cs.system.reflection.emit.OpCode;
	/** Pushes the value of a static field onto the evaluation stack. */
	static var Ldsfld(default, never):cs.system.reflection.emit.OpCode;
	/** Pushes the address of a static field onto the evaluation stack. */
	static var Ldsflda(default, never):cs.system.reflection.emit.OpCode;
	/** Pushes a new object reference to a string literal stored in the metadata. */
	static var Ldstr(default, never):cs.system.reflection.emit.OpCode;
	/** Converts a metadata token to its runtime representation, pushing it onto the evaluation stack. */
	static var Ldtoken(default, never):cs.system.reflection.emit.OpCode;
	/** Pushes an unmanaged pointer (type ) to the native code implementing a particular virtual method associated with a specified object onto the evaluation stack. */
	static var Ldvirtftn(default, never):cs.system.reflection.emit.OpCode;
	/** Exits a protected region of code, unconditionally transferring control to a specific target instruction. */
	static var Leave(default, never):cs.system.reflection.emit.OpCode;
	/** Exits a protected region of code, unconditionally transferring control to a target instruction (short form). */
	static var Leave_S(default, never):cs.system.reflection.emit.OpCode;
	/** Allocates a certain number of bytes from the local dynamic memory pool and pushes the address (a transient pointer, type ) of the first allocated byte onto the evaluation stack. */
	static var Localloc(default, never):cs.system.reflection.emit.OpCode;
	/** Pushes a typed reference to an instance of a specific type onto the evaluation stack. */
	static var Mkrefany(default, never):cs.system.reflection.emit.OpCode;
	/** Multiplies two values and pushes the result on the evaluation stack. */
	static var Mul(default, never):cs.system.reflection.emit.OpCode;
	/** Multiplies two integer values, performs an overflow check, and pushes the result onto the evaluation stack. */
	static var Mul_Ovf(default, never):cs.system.reflection.emit.OpCode;
	/** Multiplies two unsigned integer values, performs an overflow check, and pushes the result onto the evaluation stack. */
	static var Mul_Ovf_Un(default, never):cs.system.reflection.emit.OpCode;
	/** Negates a value and pushes the result onto the evaluation stack. */
	static var Neg(default, never):cs.system.reflection.emit.OpCode;
	/** Pushes an object reference to a new zero-based, one-dimensional array whose elements are of a specific type onto the evaluation stack. */
	static var Newarr(default, never):cs.system.reflection.emit.OpCode;
	/** Creates a new object or a new instance of a value type, pushing an object reference (type ) onto the evaluation stack. */
	static var Newobj(default, never):cs.system.reflection.emit.OpCode;
	/** Fills space if opcodes are patched. No meaningful operation is performed although a processing cycle can be consumed. */
	static var Nop(default, never):cs.system.reflection.emit.OpCode;
	/** Computes the bitwise complement of the integer value on top of the stack and pushes the result onto the evaluation stack as the same type. */
	static var Not(default, never):cs.system.reflection.emit.OpCode;
	/** Compute the bitwise complement of the two integer values on top of the stack and pushes the result onto the evaluation stack. */
	static var Or(default, never):cs.system.reflection.emit.OpCode;
	/** Removes the value currently on top of the evaluation stack. */
	static var Pop(default, never):cs.system.reflection.emit.OpCode;
	/** This is a reserved instruction. */
	static var Prefix1(default, never):cs.system.reflection.emit.OpCode;
	/** This is a reserved instruction. */
	static var Prefix2(default, never):cs.system.reflection.emit.OpCode;
	/** This is a reserved instruction. */
	static var Prefix3(default, never):cs.system.reflection.emit.OpCode;
	/** This is a reserved instruction. */
	static var Prefix4(default, never):cs.system.reflection.emit.OpCode;
	/** This is a reserved instruction. */
	static var Prefix5(default, never):cs.system.reflection.emit.OpCode;
	/** This is a reserved instruction. */
	static var Prefix6(default, never):cs.system.reflection.emit.OpCode;
	/** This is a reserved instruction. */
	static var Prefix7(default, never):cs.system.reflection.emit.OpCode;
	/** This is a reserved instruction. */
	static var Prefixref(default, never):cs.system.reflection.emit.OpCode;
	/** Specifies that the subsequent array address operation performs no type check at run time, and that it returns a managed pointer whose mutability is restricted. */
	static var Readonly(default, never):cs.system.reflection.emit.OpCode;
	/** Retrieves the type token embedded in a typed reference. */
	static var Refanytype(default, never):cs.system.reflection.emit.OpCode;
	/** Retrieves the address (type ) embedded in a typed reference. */
	static var Refanyval(default, never):cs.system.reflection.emit.OpCode;
	/** Divides two values and pushes the remainder onto the evaluation stack. */
	static var Rem(default, never):cs.system.reflection.emit.OpCode;
	/** Divides two unsigned values and pushes the remainder onto the evaluation stack. */
	static var Rem_Un(default, never):cs.system.reflection.emit.OpCode;
	/** Returns from the current method, pushing a return value (if present) from the callee's evaluation stack onto the caller's evaluation stack. */
	static var Ret(default, never):cs.system.reflection.emit.OpCode;
	/** Rethrows the current exception. */
	static var Rethrow(default, never):cs.system.reflection.emit.OpCode;
	/** Shifts an integer value to the left (in zeroes) by a specified number of bits, pushing the result onto the evaluation stack. */
	static var Shl(default, never):cs.system.reflection.emit.OpCode;
	/** Shifts an integer value (in sign) to the right by a specified number of bits, pushing the result onto the evaluation stack. */
	static var Shr(default, never):cs.system.reflection.emit.OpCode;
	/** Shifts an unsigned integer value (in zeroes) to the right by a specified number of bits, pushing the result onto the evaluation stack. */
	static var Shr_Un(default, never):cs.system.reflection.emit.OpCode;
	/** Pushes the size, in bytes, of a supplied value type onto the evaluation stack. */
	static var Sizeof(default, never):cs.system.reflection.emit.OpCode;
	/** Stores the value on top of the evaluation stack in the argument slot at a specified index. */
	static var Starg(default, never):cs.system.reflection.emit.OpCode;
	/** Stores the value on top of the evaluation stack in the argument slot at a specified index, short form. */
	static var Starg_S(default, never):cs.system.reflection.emit.OpCode;
	/** Replaces the array element at a given index with the value on the evaluation stack, whose type is specified in the instruction. */
	static var Stelem(default, never):cs.system.reflection.emit.OpCode;
	/** Replaces the array element at a given index with the  value on the evaluation stack. */
	static var Stelem_I(default, never):cs.system.reflection.emit.OpCode;
	/** Replaces the array element at a given index with the  value on the evaluation stack. */
	static var Stelem_I1(default, never):cs.system.reflection.emit.OpCode;
	/** Replaces the array element at a given index with the  value on the evaluation stack. */
	static var Stelem_I2(default, never):cs.system.reflection.emit.OpCode;
	/** Replaces the array element at a given index with the  value on the evaluation stack. */
	static var Stelem_I4(default, never):cs.system.reflection.emit.OpCode;
	/** Replaces the array element at a given index with the  value on the evaluation stack. */
	static var Stelem_I8(default, never):cs.system.reflection.emit.OpCode;
	/** Replaces the array element at a given index with the  value on the evaluation stack. */
	static var Stelem_R4(default, never):cs.system.reflection.emit.OpCode;
	/** Replaces the array element at a given index with the  value on the evaluation stack. */
	static var Stelem_R8(default, never):cs.system.reflection.emit.OpCode;
	/** Replaces the array element at a given index with the object ref value (type ) on the evaluation stack. */
	static var Stelem_Ref(default, never):cs.system.reflection.emit.OpCode;
	/** Replaces the value stored in the field of an object reference or pointer with a new value. */
	static var Stfld(default, never):cs.system.reflection.emit.OpCode;
	/** Stores a value of type  at a supplied address. */
	static var Stind_I(default, never):cs.system.reflection.emit.OpCode;
	/** Stores a value of type  at a supplied address. */
	static var Stind_I1(default, never):cs.system.reflection.emit.OpCode;
	/** Stores a value of type  at a supplied address. */
	static var Stind_I2(default, never):cs.system.reflection.emit.OpCode;
	/** Stores a value of type  at a supplied address. */
	static var Stind_I4(default, never):cs.system.reflection.emit.OpCode;
	/** Stores a value of type  at a supplied address. */
	static var Stind_I8(default, never):cs.system.reflection.emit.OpCode;
	/** Stores a value of type  at a supplied address. */
	static var Stind_R4(default, never):cs.system.reflection.emit.OpCode;
	/** Stores a value of type  at a supplied address. */
	static var Stind_R8(default, never):cs.system.reflection.emit.OpCode;
	/** Stores a object reference value at a supplied address. */
	static var Stind_Ref(default, never):cs.system.reflection.emit.OpCode;
	/** Pops the current value from the top of the evaluation stack and stores it in a the local variable list at a specified index. */
	static var Stloc(default, never):cs.system.reflection.emit.OpCode;
	/** Pops the current value from the top of the evaluation stack and stores it in a the local variable list at index 0. */
	static var Stloc_0(default, never):cs.system.reflection.emit.OpCode;
	/** Pops the current value from the top of the evaluation stack and stores it in a the local variable list at index 1. */
	static var Stloc_1(default, never):cs.system.reflection.emit.OpCode;
	/** Pops the current value from the top of the evaluation stack and stores it in a the local variable list at index 2. */
	static var Stloc_2(default, never):cs.system.reflection.emit.OpCode;
	/** Pops the current value from the top of the evaluation stack and stores it in a the local variable list at index 3. */
	static var Stloc_3(default, never):cs.system.reflection.emit.OpCode;
	/** Pops the current value from the top of the evaluation stack and stores it in a the local variable list at  (short form). */
	static var Stloc_S(default, never):cs.system.reflection.emit.OpCode;
	/** Copies a value of a specified type from the evaluation stack into a supplied memory address. */
	static var Stobj(default, never):cs.system.reflection.emit.OpCode;
	/** Replaces the value of a static field with a value from the evaluation stack. */
	static var Stsfld(default, never):cs.system.reflection.emit.OpCode;
	/** Subtracts one value from another and pushes the result onto the evaluation stack. */
	static var Sub(default, never):cs.system.reflection.emit.OpCode;
	/** Subtracts one integer value from another, performs an overflow check, and pushes the result onto the evaluation stack. */
	static var Sub_Ovf(default, never):cs.system.reflection.emit.OpCode;
	/** Subtracts one unsigned integer value from another, performs an overflow check, and pushes the result onto the evaluation stack. */
	static var Sub_Ovf_Un(default, never):cs.system.reflection.emit.OpCode;
	/** Implements a jump table. */
	static var Switch(default, never):cs.system.reflection.emit.OpCode;
	/** Performs a postfixed method call instruction such that the current method's stack frame is removed before the actual call instruction is executed. */
	static var Tailcall(default, never):cs.system.reflection.emit.OpCode;
	/** Throws the exception object currently on the evaluation stack. */
	static var Throw(default, never):cs.system.reflection.emit.OpCode;
	/** Indicates that an address currently atop the evaluation stack might not be aligned to the natural size of the immediately following , , , , , , , or  instruction. */
	static var Unaligned(default, never):cs.system.reflection.emit.OpCode;
	/** Converts the boxed representation of a value type to its unboxed form. */
	static var Unbox(default, never):cs.system.reflection.emit.OpCode;
	/** Converts the boxed representation of a type specified in the instruction to its unboxed form. */
	static var Unbox_Any(default, never):cs.system.reflection.emit.OpCode;
	/** Specifies that an address currently atop the evaluation stack might be volatile, and the results of reading that location cannot be cached or that multiple stores to that location cannot be suppressed. */
	static var Volatile(default, never):cs.system.reflection.emit.OpCode;
	/** Computes the bitwise XOR of the top two values on the evaluation stack, pushing the result onto the evaluation stack. */
	static var Xor(default, never):cs.system.reflection.emit.OpCode;
	/**
	 * Returns true or false if the supplied opcode takes a single byte argument.
	 * @param inst An instance of an Opcode object.
	 * @return or .
	 */
	static function TakesSingleByteArgument(inst:cs.system.reflection.emit.OpCode):Bool;
}
