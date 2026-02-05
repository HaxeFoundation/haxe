package cs.system.collections;

/** Exposes a method that compares two objects. */
@:native("System.Collections.IComparer")
extern interface IComparer {
	/**
	 * Compares two objects and returns a value indicating whether one is less than,
	 * equal to, or greater than the other.
	 * @param x The first object to compare.
	 * @param y The second object to compare.
	 * @return A signed integer that indicates the relative values of  and :   - If
	 * less than 0,  is less than .   - If 0,  equals .   - If greater than 0,  is
	 * greater than .
	 */
	function Compare(x:Dynamic, y:Dynamic):Int;
}
