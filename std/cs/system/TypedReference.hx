package cs.system;

/** Describes objects that contain both a managed pointer to a location and a runtime representation of the type that may be stored at that location. */
@:native("System.TypedReference")
extern class TypedReference extends cs.system.ValueType {
	/**
	 * Returns the type of the target of the specified .
	 * @param value The value whose target's type is to be returned.
	 * @return The type of the target of the specified .
	 */
	static function GetTargetType(value:cs.system.TypedReference):cs.system.Type;
	/**
	 * Makes a  for a field identified by a specified object and list of field
	 * descriptions.
	 * @param target An object that contains the field described by the first element
	 * of .
	 * @param flds A list of field descriptions where each element describes a field
	 * that contains the field described by the succeeding element. Each described
	 * field must be a value type. The field descriptions must be  objects supplied by
	 * the type system.
	 * @return A  for the field described by the last element of .
	 */
	static function MakeTypedReference(target:Dynamic, flds:cs.NativeArray<cs.system.reflection.FieldInfo>):cs.system.TypedReference;
	/**
	 * Converts the specified value to a . This method is not supported.
	 * @param target The target of the conversion.
	 * @param value The value to be converted.
	 */
	static function SetTypedReference(target:cs.system.TypedReference, value:Dynamic):Void;
	/**
	 * Returns the internal metadata type handle for the specified .
	 * @param value The  for which the type handle is requested.
	 * @return The internal metadata type handle for the specified .
	 */
	static function TargetTypeToken(value:cs.system.TypedReference):cs.system.RuntimeTypeHandle;
	/**
	 * Converts the specified  to an .
	 * @param value The  to be converted.
	 * @return An  converted from a .
	 */
	static function ToObject(value:cs.system.TypedReference):Dynamic;
	/**
	 * Checks if this object is equal to the specified object.
	 * @param o The object with which to compare the current object.
	 * @return if this object is equal to the specified object; otherwise, .
	 */
	function Equals(o:Dynamic):Bool;
	/**
	 * Returns the hash code of this object.
	 * @return The hash code of this object.
	 */
	function GetHashCode():Int;
}
