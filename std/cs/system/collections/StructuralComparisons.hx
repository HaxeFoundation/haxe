package cs.system.collections;

/** Provides objects for performing a structural comparison of two collection objects. */
@:native("System.Collections.StructuralComparisons")
extern class StructuralComparisons {
	/**
	 * Gets a predefined object that performs a structural comparison of two objects.
	 * @return A predefined object that is used to perform a structural comparison of
	 * two collection objects.
	 */
	static var StructuralComparer(default, never):cs.system.collections.IComparer;
	/**
	 * Gets a predefined object that compares two objects for structural equality.
	 * @return A predefined object that is used to compare two collection objects for
	 * structural equality.
	 */
	static var StructuralEqualityComparer(default, never):cs.system.collections.IEqualityComparer;
}
