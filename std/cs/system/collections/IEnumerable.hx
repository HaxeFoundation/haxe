package cs.system.collections;

/** Exposes an enumerator, which supports a simple iteration over a non-generic collection. */
@:native("System.Collections.IEnumerable")
extern interface IEnumerable {
	/**
	 * Returns an enumerator that iterates through a collection.
	 * @return An  object that can be used to iterate through the collection.
	 */
	function GetEnumerator():cs.system.collections.IEnumerator;
}
