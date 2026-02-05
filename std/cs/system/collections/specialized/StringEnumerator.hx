package cs.system.collections.specialized;

/** Supports a simple iteration over a . */
@:native("System.Collections.Specialized.StringEnumerator")
extern class StringEnumerator {
	/**
	 * Gets the current element in the collection.
	 * @return The current element in the collection.
	 */
	var Current(default, never):String;
	/**
	 * Advances the enumerator to the next element of the collection.
	 * @return if the enumerator was successfully advanced to the next element;  if the
	 * enumerator has passed the end of the collection.
	 */
	function MoveNext():Bool;
	/** Sets the enumerator to its initial position, which is before the first element in the collection. */
	function Reset():Void;
}
