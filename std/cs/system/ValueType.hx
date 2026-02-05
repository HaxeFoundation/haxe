package cs.system;

/** Provides the base class for value types. */
@:native("System.ValueType")
extern class ValueType {
	/**
	 * Indicates whether this instance and a specified object are equal.
	 * @param obj The object to compare with the current instance.
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
	 * Returns the fully qualified type name of this instance.
	 * @return The fully qualified type name.
	 */
	function ToString():String;
}
