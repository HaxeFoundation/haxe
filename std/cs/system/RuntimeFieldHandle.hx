package cs.system;

/** Represents a field using an internal metadata token. */
@:native("System.RuntimeFieldHandle")
extern class RuntimeFieldHandle extends cs.system.ValueType {
	/**
	 * Gets a handle to the field represented by the current instance.
	 * @return An  that contains the handle to the field represented by the current
	 * instance.
	 */
	var Value(default, never):cs.system.IntPtr;
	/**
	 * Indicates whether two  structures are equal.
	 * @param left The  to compare to .
	 * @param right The  to compare to .
	 * @return if  is equal to ; otherwise, .
	 */
	static function op_Equality(left:cs.system.RuntimeFieldHandle, right:cs.system.RuntimeFieldHandle):Bool;
	/**
	 * Indicates whether two  structures are not equal.
	 * @param left The  to compare to .
	 * @param right The  to compare to .
	 * @return if  is not equal to ; otherwise, .
	 */
	static function op_Inequality(left:cs.system.RuntimeFieldHandle, right:cs.system.RuntimeFieldHandle):Bool;
	@:overload(function(obj:Dynamic):Bool {})
	/**
	 * Indicates whether the current instance is equal to the specified object.
	 * @param obj The object to compare to the current instance.
	 * @return if  is a  and equal to the value of the current instance; otherwise, .
	 */
	function Equals(handle:cs.system.RuntimeFieldHandle):Bool;
	/**
	 * Returns the hash code for this instance.
	 * @return A 32-bit signed integer that is the hash code for this instance.
	 */
	function GetHashCode():Int;
	/**
	 * Populates a  with the data necessary to deserialize the field represented by the
	 * current instance.
	 * @param info The  object to populate with serialization information.
	 * @param context (Reserved) The place to store and retrieve serialized data.
	 */
	function GetObjectData(info:cs.system.runtime.serialization.SerializationInfo, context:cs.system.runtime.serialization.StreamingContext):Void;
}
