package cs.system.reflection.emit;

/** Describes an intermediate language (IL) instruction. */
@:native("System.Reflection.Emit.OpCode")
extern class OpCode extends cs.system.ValueType {
	/**
	 * The flow control characteristics of the intermediate language (IL) instruction.
	 * @return Read-only. The type of flow control.
	 */
	var FlowControl(default, never):cs.system.reflection.emit.FlowControl;
	/**
	 * The name of the intermediate language (IL) instruction.
	 * @return Read-only. The name of the IL instruction.
	 */
	var Name(default, never):String;
	/**
	 * The type of intermediate language (IL) instruction.
	 * @return Read-only. The type of intermediate language (IL) instruction.
	 */
	var OpCodeType(default, never):cs.system.reflection.emit.OpCodeType;
	/**
	 * The operand type of an intermediate language (IL) instruction.
	 * @return Read-only. The operand type of an IL instruction.
	 */
	var OperandType(default, never):cs.system.reflection.emit.OperandType;
	/**
	 * The size of the intermediate language (IL) instruction.
	 * @return Read-only. The size of the IL instruction.
	 */
	var Size(default, never):Int;
	/**
	 * How the intermediate language (IL) instruction pops the stack.
	 * @return Read-only. The way the IL instruction pops the stack.
	 */
	var StackBehaviourPop(default, never):cs.system.reflection.emit.StackBehaviour;
	/**
	 * How the intermediate language (IL) instruction pushes operand onto the stack.
	 * @return Read-only. The way the IL instruction pushes operand onto the stack.
	 */
	var StackBehaviourPush(default, never):cs.system.reflection.emit.StackBehaviour;
	/**
	 * Gets the numeric value of the intermediate language (IL) instruction.
	 * @return Read-only. The numeric value of the IL instruction.
	 */
	var Value(default, never):cs.Int16;
	/**
	 * Indicates whether two  structures are equal.
	 * @param a The  to compare to .
	 * @param b The  to compare to .
	 * @return if  is equal to ; otherwise, .
	 */
	static function op_Equality(a:cs.system.reflection.emit.OpCode, b:cs.system.reflection.emit.OpCode):Bool;
	/**
	 * Indicates whether two  structures are not equal.
	 * @param a The  to compare to .
	 * @param b The  to compare to .
	 * @return if  is not equal to ; otherwise, .
	 */
	static function op_Inequality(a:cs.system.reflection.emit.OpCode, b:cs.system.reflection.emit.OpCode):Bool;
	@:overload(function(obj:Dynamic):Bool {})
	/**
	 * Tests whether the given object is equal to this .
	 * @param obj The object to compare to this object.
	 * @return if  is an instance of  and is equal to this object; otherwise, .
	 */
	function Equals(obj:cs.system.reflection.emit.OpCode):Bool;
	/**
	 * Returns the generated hash code for this .
	 * @return The hash code for this instance.
	 */
	function GetHashCode():Int;
	/**
	 * Returns this  as a .
	 * @return A string containing the name of this .
	 */
	function ToString():String;
}
