package cs.system;

/** Defines a generalized type-specific comparison method that a value type or class implements to order or sort its instances. */
@:native("System.IComparable`1")
extern interface IComparable_1<T> {
	/**
	 * Compares the current instance with another object of the same type and returns
	 * an integer that indicates whether the current instance precedes, follows, or
	 * occurs in the same position in the sort order as the other object.
	 * @param obj An object to compare with this instance.
	 * @return A value that indicates the relative order of the objects being compared.
	 * The return value has these meanings: Value Meaning Less than zero This instance
	 * precedes  in the sort order. Zero This instance occurs in the same position in
	 * the sort order as . Greater than zero This instance follows  in the sort order.
	 */
	function CompareTo(other:T):Int;
}
