package cs.system.reflection;

/** Represents an argument of a custom attribute in the reflection-only context, or an element of an array argument. */
@:native("System.Reflection.CustomAttributeTypedArgument")
extern class CustomAttributeTypedArgument extends cs.system.ValueType {
	/**
	 * Gets the type of the argument or of the array argument element.
	 * @return A  object representing the type of the argument or of the array element.
	 */
	var ArgumentType(default, never):cs.system.Type;
	/**
	 * Gets the value of the argument for a simple argument or for an element of an
	 * array argument; gets a collection of values for an array argument.
	 * @return An object that represents the value of the argument or element, or a
	 * generic  of  objects that represent the values of an array-type argument.
	 */
	var Value(default, never):Dynamic;
	@:overload(function(value:Dynamic):Void {})
	function new(argumentType:cs.system.Type, value:Dynamic):Void;
	/**
	 * Tests whether two  structures are equivalent.
	 * @param left The  structure to the left of the equality operator.
	 * @param right The  structure to the right of the equality operator.
	 * @return if the two  structures are equal; otherwise, .
	 */
	static function op_Equality(left:cs.system.reflection.CustomAttributeTypedArgument, right:cs.system.reflection.CustomAttributeTypedArgument):Bool;
	/**
	 * Tests whether two  structures are different.
	 * @param left The  structure to the left of the inequality operator.
	 * @param right The  structure to the right of the inequality operator.
	 * @return if the two  structures are different; otherwise, .
	 */
	static function op_Inequality(left:cs.system.reflection.CustomAttributeTypedArgument, right:cs.system.reflection.CustomAttributeTypedArgument):Bool;
	/**
	 * Indicates whether this instance and a specified object are equal.
	 * @param obj Another object to compare to.
	 * @return if  and this instance are the same type and represent the same value;
	 * otherwise, .
	 */
	function Equals(obj:Dynamic):Bool;
	/**
	 * Returns the hash code for this instance.
	 * @return A 32-bit signed integer that is the hash code for this instance.
	 */
	function GetHashCode():Int;
	/**
	 * Returns a string consisting of the argument name, the equal sign, and a string
	 * representation of the argument value.
	 * @return A string consisting of the argument name, the equal sign, and a string
	 * representation of the argument value.
	 */
	function ToString():String;
}
