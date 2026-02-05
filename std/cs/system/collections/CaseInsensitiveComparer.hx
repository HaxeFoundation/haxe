package cs.system.collections;

/** Compares two objects for equivalence, ignoring the case of strings. */
@:native("System.Collections.CaseInsensitiveComparer")
extern class CaseInsensitiveComparer {
	/**
	 * Gets an instance of  that is associated with the  of the current thread and that
	 * is always available.
	 * @return An instance of  that is associated with the  of the current thread.
	 */
	static var Default(default, never):cs.system.collections.CaseInsensitiveComparer;
	/**
	 * Gets an instance of  that is associated with  and that is always available.
	 * @return An instance of  that is associated with .
	 */
	static var DefaultInvariant(default, never):cs.system.collections.CaseInsensitiveComparer;
	@:overload(function():Void {})
	function new(culture:cs.system.globalization.CultureInfo):Void;
	/**
	 * Performs a case-insensitive comparison of two objects of the same type and
	 * returns a value indicating whether one is less than, equal to, or greater than
	 * the other.
	 * @param a The first object to compare.
	 * @param b The second object to compare.
	 * @return A signed integer that indicates the relative values of  and , as shown
	 * in the following table. Value Meaning Less than zero is less than , with casing
	 * ignored. Zero equals , with casing ignored. Greater than zero is greater than ,
	 * with casing ignored.
	 */
	function Compare(a:Dynamic, b:Dynamic):Int;
}
