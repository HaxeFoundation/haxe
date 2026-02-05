package cs.system.collections;

/** Defines methods to support the comparison of objects for structural equality. */
@:native("System.Collections.IStructuralEquatable")
extern interface IStructuralEquatable {
	/**
	 * Determines whether an object is structurally equal to the current instance.
	 * @param other The object to compare with the current instance.
	 * @param comparer An object that determines whether the current instance and  are
	 * equal.
	 * @return if the two objects are equal; otherwise, .
	 */
	function Equals(other:Dynamic, comparer:cs.system.collections.IEqualityComparer):Bool;
	/**
	 * Returns a hash code for the current instance.
	 * @param comparer An object that computes the hash code of the current object.
	 * @return The hash code for the current instance.
	 */
	function GetHashCode(comparer:cs.system.collections.IEqualityComparer):Int;
}
