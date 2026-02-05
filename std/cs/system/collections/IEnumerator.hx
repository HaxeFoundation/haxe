package cs.system.collections;

/** Supports a simple iteration over a non-generic collection. */
@:native("System.Collections.IEnumerator")
extern interface IEnumerator {
	/**
	 * Gets the element in the collection at the current position of the enumerator.
	 * @return The element in the collection at the current position of the enumerator.
	 */
	var Current(default, never):Dynamic;
	/**
	 * Advances the enumerator to the next element of the collection.
	 * @return if the enumerator was successfully advanced to the next element;  if the
	 * enumerator has passed the end of the collection.
	 */
	function MoveNext():Bool;
	/** Sets the enumerator to its initial position, which is before the first element in the collection. */
	function Reset():Void;
}
