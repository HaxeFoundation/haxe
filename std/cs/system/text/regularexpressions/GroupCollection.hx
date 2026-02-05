package cs.system.text.regularexpressions;

/** Returns the set of captured groups in a single match. */
@:native("System.Text.RegularExpressions.GroupCollection")
extern class GroupCollection {
	/**
	 * Returns the number of groups in the collection.
	 * @return The number of groups in the collection.
	 */
	var Count(default, never):Int;
	/**
	 * Gets a value that indicates whether the collection is read-only.
	 * @return in all cases.
	 */
	var IsReadOnly(default, never):Bool;
	/**
	 * Gets a value that indicates whether access to the  is synchronized
	 * (thread-safe).
	 * @return in all cases.
	 */
	var IsSynchronized(default, never):Bool;
	/**
	 * Gets an object that can be used to synchronize access to the .
	 * @return A copy of the  object to synchronize.
	 */
	var SyncRoot(default, never):Dynamic;
	@:overload(function(index0:Int):cs.system.text.regularexpressions.Group {})
	@:native("get_Item")
	function get_Item(index0:String):cs.system.text.regularexpressions.Group;
	@:overload(function(array:cs.system.Array, arrayIndex:Int):Void {})
	/**
	 * Copies all the elements of the collection to the given array beginning at the
	 * given index.
	 * @param array The array the collection is to be copied into.
	 * @param arrayIndex The position in the destination array where the copying is to
	 * begin.
	 */
	function CopyTo(array:cs.NativeArray<cs.system.text.regularexpressions.Group>, arrayIndex:Int):Void;
	/**
	 * Provides an enumerator that iterates through the collection.
	 * @return An enumerator that contains all  objects in the .
	 */
	function GetEnumerator():cs.system.collections.IEnumerator;
}
