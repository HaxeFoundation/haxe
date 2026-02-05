package cs.system.runtime.interopservices;

/** Encapsulates an array and an offset within the specified array. */
@:native("System.Runtime.InteropServices.ArrayWithOffset")
extern class ArrayWithOffset extends cs.system.ValueType {
	function new(array:Dynamic, offset:Int):Void;
	/**
	 * Determines whether two specified  objects have the same value.
	 * @param a An  object to compare with the  parameter.
	 * @param b An  object to compare with the  parameter.
	 * @return if the value of  is the same as the value of ; otherwise, .
	 */
	static function op_Equality(a:cs.system.runtime.interopservices.ArrayWithOffset, b:cs.system.runtime.interopservices.ArrayWithOffset):Bool;
	/**
	 * Determines whether two specified  objects no not have the same value.
	 * @param a An  object to compare with the  parameter.
	 * @param b An  object to compare with the  parameter.
	 * @return if the value of  is not the same as the value of ; otherwise, .
	 */
	static function op_Inequality(a:cs.system.runtime.interopservices.ArrayWithOffset, b:cs.system.runtime.interopservices.ArrayWithOffset):Bool;
	@:overload(function(obj:Dynamic):Bool {})
	/**
	 * Indicates whether the specified object matches the current  object.
	 * @param obj Object to compare with this instance.
	 * @return if the object matches this ; otherwise, .
	 */
	function Equals(obj:cs.system.runtime.interopservices.ArrayWithOffset):Bool;
	/**
	 * Returns the managed array referenced by this .
	 * @return The managed array this instance references.
	 */
	function GetArray():Dynamic;
	/**
	 * Returns a hash code for this value type.
	 * @return The hash code for this instance.
	 */
	function GetHashCode():Int;
	/**
	 * Returns the offset provided when this  was constructed.
	 * @return The offset for this instance.
	 */
	function GetOffset():Int;
}
