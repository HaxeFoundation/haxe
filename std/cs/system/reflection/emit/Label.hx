package cs.system.reflection.emit;

/** Represents a label in the instruction stream.  is used in conjunction with the  class. */
@:native("System.Reflection.Emit.Label")
extern class Label extends cs.system.ValueType {
	/**
	 * Indicates whether two  structures are equal.
	 * @param a The  to compare to .
	 * @param b The  to compare to .
	 * @return if  is equal to ; otherwise, .
	 */
	static function op_Equality(a:cs.system.reflection.emit.Label, b:cs.system.reflection.emit.Label):Bool;
	/**
	 * Indicates whether two  structures are not equal.
	 * @param a The  to compare to .
	 * @param b The  to compare to .
	 * @return if  is not equal to ; otherwise, .
	 */
	static function op_Inequality(a:cs.system.reflection.emit.Label, b:cs.system.reflection.emit.Label):Bool;
	@:overload(function(obj:Dynamic):Bool {})
	/**
	 * Checks if the given object is an instance of  and is equal to this instance.
	 * @param obj The object to compare with this  instance.
	 * @return if  is an instance of  and is equal to this object; otherwise, .
	 */
	function Equals(obj:cs.system.reflection.emit.Label):Bool;
	/**
	 * Generates a hash code for this instance.
	 * @return A hash code for this instance.
	 */
	function GetHashCode():Int;
}
