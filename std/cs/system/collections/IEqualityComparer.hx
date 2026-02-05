package cs.system.collections;

/** Defines methods to support the comparison of objects for equality. */
@:native("System.Collections.IEqualityComparer")
extern interface IEqualityComparer {
	/**
	 * Determines whether the specified objects are equal.
	 * @param x The first object to compare.
	 * @param y The second object to compare.
	 * @return if the specified objects are equal; otherwise, .
	 */
	function Equals(x:Dynamic, y:Dynamic):Bool;
	/**
	 * Returns a hash code for the specified object.
	 * @param obj The  for which a hash code is to be returned.
	 * @return A hash code for the specified object.
	 */
	function GetHashCode(obj:Dynamic):Int;
}
