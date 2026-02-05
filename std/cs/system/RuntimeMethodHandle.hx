package cs.system;

/** is a handle to the internal metadata representation of a method. */
@:native("System.RuntimeMethodHandle")
extern class RuntimeMethodHandle extends cs.system.ValueType {
	/**
	 * Gets the value of this instance.
	 * @return A  that is the internal metadata representation of a method.
	 */
	var Value(default, never):cs.system.IntPtr;
	/**
	 * Indicates whether two instances of  are equal.
	 * @param left A  to compare to .
	 * @param right A  to compare to .
	 * @return if the value of  is equal to the value of ; otherwise, .
	 */
	static function op_Equality(left:cs.system.RuntimeMethodHandle, right:cs.system.RuntimeMethodHandle):Bool;
	/**
	 * Indicates whether two instances of  are not equal.
	 * @param left A  to compare to .
	 * @param right A  to compare to .
	 * @return if the value of  is unequal to the value of ; otherwise, .
	 */
	static function op_Inequality(left:cs.system.RuntimeMethodHandle, right:cs.system.RuntimeMethodHandle):Bool;
	@:overload(function(obj:Dynamic):Bool {})
	/**
	 * Indicates whether this instance is equal to a specified object.
	 * @param obj A  to compare to this instance.
	 * @return if  is a  and equal to the value of this instance; otherwise, .
	 */
	function Equals(handle:cs.system.RuntimeMethodHandle):Bool;
	/**
	 * Obtains a pointer to the method represented by this instance.
	 * @return A pointer to the method represented by this instance.
	 */
	function GetFunctionPointer():cs.system.IntPtr;
	/**
	 * Returns the hash code for this instance.
	 * @return A 32-bit signed integer hash code.
	 */
	function GetHashCode():Int;
	/**
	 * Populates a  with the data necessary to deserialize the field represented by
	 * this instance.
	 * @param info The object to populate with serialization information.
	 * @param context (Reserved) The place to store and retrieve serialized data.
	 */
	function GetObjectData(info:cs.system.runtime.serialization.SerializationInfo, context:cs.system.runtime.serialization.StreamingContext):Void;
}
