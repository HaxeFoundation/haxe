package cs.system.collections;

/** Compares two objects for equivalence, where string comparisons are case-sensitive. */
@:native("System.Collections.Comparer")
extern class Comparer {
	/** Represents an instance of  that is associated with the  of the current thread. This field is read-only. */
	static var Default(default, never):cs.system.collections.Comparer;
	/** Represents an instance of  that is associated with . This field is read-only. */
	static var DefaultInvariant(default, never):cs.system.collections.Comparer;
	function new(culture:cs.system.globalization.CultureInfo):Void;
	/**
	 * Performs a case-sensitive comparison of two objects of the same type and returns
	 * a value indicating whether one is less than, equal to, or greater than the
	 * other.
	 * @param a The first object to compare.
	 * @param b The second object to compare.
	 * @return A signed integer that indicates the relative values of  and , as shown
	 * in the following table. Value Meaning Less than zero is less than . Zero equals
	 * . Greater than zero is greater than .
	 */
	function Compare(a:Dynamic, b:Dynamic):Int;
	/**
	 * Populates a  object with the data required for serialization.
	 * @param info The object to populate with data.
	 * @param context The context information about the source or destination of the
	 * serialization.
	 */
	function GetObjectData(info:cs.system.runtime.serialization.SerializationInfo, context:cs.system.runtime.serialization.StreamingContext):Void;
}
