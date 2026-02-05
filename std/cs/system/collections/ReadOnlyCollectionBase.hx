package cs.system.collections;

/** Provides the  base class for a strongly typed non-generic read-only collection. */
@:native("System.Collections.ReadOnlyCollectionBase")
extern class ReadOnlyCollectionBase {
	/**
	 * Gets the number of elements contained in the  instance.
	 * @return The number of elements contained in the  instance. Retrieving the value
	 * of this property is an O(1) operation.
	 */
	var Count(default, never):Int;
	/**
	 * Gets the list of elements contained in the  instance.
	 * @return An  representing the  instance itself.
	 */
	var InnerList(default, never):cs.system.collections.ArrayList;
	/**
	 * Returns an enumerator that iterates through the  instance.
	 * @return An  for the  instance.
	 */
	function GetEnumerator():cs.system.collections.IEnumerator;
}
