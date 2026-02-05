package cs.system.text.regularexpressions;

/** Represents the set of successful matches found by iteratively applying a regular expression pattern to the input string. */
@:native("System.Text.RegularExpressions.MatchCollection")
extern class MatchCollection {
	/**
	 * Gets the number of matches.
	 * @return The number of matches.
	 */
	var Count(default, never):Int;
	/**
	 * Gets a value that indicates whether the collection is read only.
	 * @return in all cases.
	 */
	var IsReadOnly(default, never):Bool;
	/**
	 * Gets a value indicating whether access to the collection is synchronized
	 * (thread-safe).
	 * @return in all cases.
	 */
	var IsSynchronized(default, never):Bool;
	/**
	 * Gets an object that can be used to synchronize access to the collection.
	 * @return An object that can be used to synchronize access to the collection. This
	 * property always returns the object itself.
	 */
	var SyncRoot(default, never):Dynamic;
	@:native("get_Item")
	function get_Item(index0:Int):cs.system.text.regularexpressions.Match;
	@:overload(function(array:cs.system.Array, arrayIndex:Int):Void {})
	/**
	 * Copies all the elements of the collection to the given array starting at the
	 * given index.
	 * @param array The array the collection is to be copied into.
	 * @param arrayIndex The position in the array where copying is to begin.
	 */
	function CopyTo(array:cs.NativeArray<cs.system.text.regularexpressions.Match>, arrayIndex:Int):Void;
	/**
	 * Provides an enumerator that iterates through the collection.
	 * @return An object that contains all  objects within the .
	 */
	function GetEnumerator():cs.system.collections.IEnumerator;
}
