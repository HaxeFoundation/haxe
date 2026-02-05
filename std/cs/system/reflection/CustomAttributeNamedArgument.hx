package cs.system.reflection;

/** Represents a named argument of a custom attribute in the reflection-only context. */
@:native("System.Reflection.CustomAttributeNamedArgument")
extern class CustomAttributeNamedArgument extends cs.system.ValueType {
	/**
	 * Gets a value that indicates whether the named argument is a field.
	 * @return if the named argument is a field; otherwise, .
	 */
	var IsField(default, never):Bool;
	/**
	 * Gets the attribute member that would be used to set the named argument.
	 * @return The attribute member that would be used to set the named argument.
	 */
	var MemberInfo(default, never):cs.system.reflection.MemberInfo;
	/**
	 * Gets the name of the attribute member that would be used to set the named
	 * argument.
	 * @return The name of the attribute member that would be used to set the named
	 * argument.
	 */
	var MemberName(default, never):String;
	/**
	 * Gets a  structure that can be used to obtain the type and value of the current
	 * named argument.
	 * @return A structure that can be used to obtain the type and value of the current
	 * named argument.
	 */
	var TypedValue(default, never):cs.system.reflection.CustomAttributeTypedArgument;
	@:overload(function(memberInfo:cs.system.reflection.MemberInfo, value:Dynamic):Void {})
	function new(memberInfo:cs.system.reflection.MemberInfo, typedArgument:cs.system.reflection.CustomAttributeTypedArgument):Void;
	/**
	 * Tests whether two  structures are equivalent.
	 * @param left The structure to the left of the equality operator.
	 * @param right The structure to the right of the equality operator.
	 * @return if the two  structures are equal; otherwise, .
	 */
	static function op_Equality(left:cs.system.reflection.CustomAttributeNamedArgument, right:cs.system.reflection.CustomAttributeNamedArgument):Bool;
	/**
	 * Tests whether two  structures are different.
	 * @param left The structure to the left of the inequality operator.
	 * @param right The structure to the right of the inequality operator.
	 * @return if the two  structures are different; otherwise, .
	 */
	static function op_Inequality(left:cs.system.reflection.CustomAttributeNamedArgument, right:cs.system.reflection.CustomAttributeNamedArgument):Bool;
	/**
	 * Returns a value that indicates whether this instance is equal to a specified
	 * object.
	 * @param obj An object to compare with this instance, or .
	 * @return if  equals the type and value of this instance; otherwise, .
	 */
	function Equals(obj:Dynamic):Bool;
	/**
	 * Returns the hash code for this instance.
	 * @return A 32-bit signed integer hash code.
	 */
	function GetHashCode():Int;
	/**
	 * Returns a string that consists of the argument name, the equal sign, and a
	 * string representation of the argument value.
	 * @return A string that consists of the argument name, the equal sign, and a
	 * string representation of the argument value.
	 */
	function ToString():String;
}
