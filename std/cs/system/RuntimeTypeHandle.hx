package cs.system;

/** Represents a type using an internal metadata token. */
@:native("System.RuntimeTypeHandle")
extern class RuntimeTypeHandle extends cs.system.ValueType {
	/**
	 * Gets a handle to the type represented by this instance.
	 * @return A handle to the type represented by this instance.
	 */
	var Value(default, never):cs.system.IntPtr;
	@:overload(function(left:Dynamic, right:cs.system.RuntimeTypeHandle):Bool {})
	/**
	 * Indicates whether an object and a  structure are equal.
	 * @param left An object to compare to .
	 * @param right A  structure to compare to .
	 * @return if  is a  structure and is equal to ; otherwise, .
	 */
	static function op_Equality(left:cs.system.RuntimeTypeHandle, right:Dynamic):Bool;
	@:overload(function(left:Dynamic, right:cs.system.RuntimeTypeHandle):Bool {})
	/**
	 * Indicates whether an object and a  structure are not equal.
	 * @param left An object to compare to .
	 * @param right A  structure to compare to .
	 * @return if  is a  and is not equal to ; otherwise, .
	 */
	static function op_Inequality(left:cs.system.RuntimeTypeHandle, right:Dynamic):Bool;
	@:overload(function(obj:Dynamic):Bool {})
	/**
	 * Indicates whether the specified object is equal to the current  structure.
	 * @param obj An object to compare to the current instance.
	 * @return if  is a  structure and is equal to the value of this instance;
	 * otherwise, .
	 */
	function Equals(handle:cs.system.RuntimeTypeHandle):Bool;
	/**
	 * Returns the hash code for the current instance.
	 * @return A 32-bit signed integer hash code.
	 */
	function GetHashCode():Int;
	/**
	 * Gets a handle to the module that contains the type represented by the current
	 * instance.
	 * @return A  structure representing a handle to the module that contains the type
	 * represented by the current instance.
	 */
	function GetModuleHandle():cs.system.ModuleHandle;
	/**
	 * Populates a  with the data necessary to deserialize the type represented by the
	 * current instance.
	 * @param info The object to be populated with serialization information.
	 * @param context (Reserved) The location where serialized data will be stored and
	 * retrieved.
	 */
	function GetObjectData(info:cs.system.runtime.serialization.SerializationInfo, context:cs.system.runtime.serialization.StreamingContext):Void;
}
