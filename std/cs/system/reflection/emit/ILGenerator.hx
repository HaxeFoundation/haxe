package cs.system.reflection.emit;

/** Generates Microsoft intermediate language (MSIL) instructions. */
@:native("System.Reflection.Emit.ILGenerator")
extern class ILGenerator {
	/**
	 * Gets the current offset, in bytes, in the Microsoft intermediate language (MSIL)
	 * stream that is being emitted by the .
	 * @return The offset in the MSIL stream at which the next instruction will be
	 * emitted.
	 */
	var ILOffset(default, never):Int;
	/**
	 * Begins a catch block.
	 * @param exceptionType The  object that represents the exception.
	 */
	function BeginCatchBlock(exceptionType:cs.system.Type):Void;
	/** Begins an exception block for a filtered exception. */
	function BeginExceptFilterBlock():Void;
	/**
	 * Begins an exception block for a non-filtered exception.
	 * @return The label for the end of the block. This will leave you in the correct
	 * place to execute finally blocks or to finish the try.
	 */
	function BeginExceptionBlock():cs.system.reflection.emit.Label;
	/** Begins an exception fault block in the Microsoft intermediate language (MSIL) stream. */
	function BeginFaultBlock():Void;
	/** Begins a finally block in the Microsoft intermediate language (MSIL) instruction stream. */
	function BeginFinallyBlock():Void;
	/** Begins a lexical scope. */
	function BeginScope():Void;
	@:overload(function(localType:cs.system.Type):cs.system.reflection.emit.LocalBuilder {})
	/**
	 * Declares a local variable of the specified type.
	 * @param localType A  object that represents the type of the local variable.
	 * @return The declared local variable.
	 */
	function DeclareLocal(localType:cs.system.Type, pinned:Bool):cs.system.reflection.emit.LocalBuilder;
	/**
	 * Declares a new label.
	 * @return A new label that can be used as a token for branching.
	 */
	function DefineLabel():cs.system.reflection.emit.Label;
	@:overload(function(opcode:cs.system.reflection.emit.OpCode):Void {})
	@:overload(function(opcode:cs.system.reflection.emit.OpCode, arg:cs.UInt8):Void {})
	@:overload(function(opcode:cs.system.reflection.emit.OpCode, arg:Float):Void {})
	@:overload(function(opcode:cs.system.reflection.emit.OpCode, arg:cs.Int16):Void {})
	@:overload(function(opcode:cs.system.reflection.emit.OpCode, arg:Int):Void {})
	@:overload(function(opcode:cs.system.reflection.emit.OpCode, arg:haxe.Int64):Void {})
	@:overload(function(opcode:cs.system.reflection.emit.OpCode, con:cs.system.reflection.ConstructorInfo):Void {})
	@:overload(function(opcode:cs.system.reflection.emit.OpCode, label:cs.system.reflection.emit.Label):Void {})
	@:overload(function(opcode:cs.system.reflection.emit.OpCode, labels:cs.NativeArray<cs.system.reflection.emit.Label>):Void {})
	@:overload(function(opcode:cs.system.reflection.emit.OpCode, local:cs.system.reflection.emit.LocalBuilder):Void {})
	@:overload(function(opcode:cs.system.reflection.emit.OpCode, signature:cs.system.reflection.emit.SignatureHelper):Void {})
	@:overload(function(opcode:cs.system.reflection.emit.OpCode, field:cs.system.reflection.FieldInfo):Void {})
	@:overload(function(opcode:cs.system.reflection.emit.OpCode, meth:cs.system.reflection.MethodInfo):Void {})
	@:overload(function(opcode:cs.system.reflection.emit.OpCode, arg:cs.Int8):Void {})
	@:overload(function(opcode:cs.system.reflection.emit.OpCode, arg:Single):Void {})
	@:overload(function(opcode:cs.system.reflection.emit.OpCode, str:String):Void {})
	/**
	 * Puts the specified instruction onto the stream of instructions.
	 * @param opcode The Microsoft Intermediate Language (MSIL) instruction to be put
	 * onto the stream.
	 */
	function Emit(opcode:cs.system.reflection.emit.OpCode, cls:cs.system.Type):Void;
	/**
	 * Puts a  or  instruction onto the Microsoft intermediate language (MSIL) stream
	 * to call a  method.
	 * @param opcode The MSIL instruction to be emitted onto the stream. Must be , , or
	 * .
	 * @param methodInfo The  method to be called.
	 * @param optionalParameterTypes The types of the optional arguments if the method
	 * is a  method; otherwise, .
	 */
	function EmitCall(opcode:cs.system.reflection.emit.OpCode, methodInfo:cs.system.reflection.MethodInfo, optionalParameterTypes:cs.NativeArray<cs.system.Type>):Void;
	@:overload(function(opcode:cs.system.reflection.emit.OpCode, unmanagedCallConv:cs.system.runtime.interopservices.CallingConvention, returnType:cs.system.Type, parameterTypes:cs.NativeArray<cs.system.Type>):Void {})
	/**
	 * Puts a  instruction onto the Microsoft intermediate language (MSIL) stream,
	 * specifying a managed calling convention for the indirect call.
	 * @param opcode The MSIL instruction to be emitted onto the stream. Must be .
	 * @param callingConvention The managed calling convention to be used.
	 * @param returnType The  of the result.
	 * @param parameterTypes The types of the required arguments to the instruction.
	 * @param optionalParameterTypes The types of the optional arguments for  calls.
	 */
	function EmitCalli(opcode:cs.system.reflection.emit.OpCode, callingConvention:cs.system.reflection.CallingConventions, returnType:cs.system.Type, parameterTypes:cs.NativeArray<cs.system.Type>, optionalParameterTypes:cs.NativeArray<cs.system.Type>):Void;
	@:overload(function(localBuilder:cs.system.reflection.emit.LocalBuilder):Void {})
	@:overload(function(fld:cs.system.reflection.FieldInfo):Void {})
	/**
	 * Emits the Microsoft intermediate language (MSIL) necessary to call  with the
	 * given local variable.
	 * @param localBuilder The local variable whose value is to be written to the
	 * console.
	 */
	function EmitWriteLine(value:String):Void;
	/** Ends an exception block. */
	function EndExceptionBlock():Void;
	/** Ends a lexical scope. */
	function EndScope():Void;
	/**
	 * Marks the Microsoft intermediate language (MSIL) stream's current position with
	 * the given label.
	 * @param loc The label for which to set an index.
	 */
	function MarkLabel(loc:cs.system.reflection.emit.Label):Void;
	/**
	 * Emits an instruction to throw an exception.
	 * @param excType The class of the type of exception to throw.
	 */
	function ThrowException(excType:cs.system.Type):Void;
	/**
	 * Specifies the namespace to be used in evaluating locals and watches for the
	 * current active lexical scope.
	 * @param usingNamespace The namespace to be used in evaluating locals and watches
	 * for the current active lexical scope
	 */
	function UsingNamespace(usingNamespace:String):Void;
}
