package cs.system.collections;

/** Supports the structural comparison of collection objects. */
@:native("System.Collections.IStructuralComparable")
extern interface IStructuralComparable {
	/**
	 * Determines whether the current collection object precedes, occurs in the same
	 * position as, or follows another object in the sort order.
	 * @param other The object to compare with the current instance.
	 * @param comparer An object that compares members of the current collection object
	 * with the corresponding members of .
	 * @return A signed integer that indicates the relationship of the current
	 * collection object to  in the sort order: - If less than 0, the current instance
	 * precedes . - If 0, the current instance and  are equal. - If greater than 0, the
	 * current instance follows . Return value Description -1 The current instance
	 * precedes . 0 The current instance and  are equal. 1 The current instance follows
	 * .
	 */
	function CompareTo(other:Dynamic, comparer:cs.system.collections.IComparer):Int;
}
