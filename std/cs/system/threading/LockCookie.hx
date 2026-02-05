package cs.system.threading;

/** Defines the lock that implements single-writer/multiple-reader semantics. This is a value type. */
@:native("System.Threading.LockCookie")
extern class LockCookie extends cs.system.ValueType {
	/**
	 * Indicates whether two  structures are equal.
	 * @param a The  to compare to .
	 * @param b The  to compare to .
	 * @return if  is equal to ; otherwise, .
	 */
	static function op_Equality(a:cs.system.threading.LockCookie, b:cs.system.threading.LockCookie):Bool;
	/**
	 * Indicates whether two  structures are not equal.
	 * @param a The  to compare to .
	 * @param b The  to compare to .
	 * @return if  is not equal to ; otherwise, .
	 */
	static function op_Inequality(a:cs.system.threading.LockCookie, b:cs.system.threading.LockCookie):Bool;
	@:overload(function(obj:Dynamic):Bool {})
	/**
	 * Indicates whether a specified object is a  and is equal to the current instance.
	 * @param obj The object to compare to the current instance.
	 * @return if the value of  is equal to the value of the current instance;
	 * otherwise, .
	 */
	function Equals(obj:cs.system.threading.LockCookie):Bool;
	/**
	 * Returns the hash code for this instance.
	 * @return A 32-bit signed integer hash code.
	 */
	function GetHashCode():Int;
}
