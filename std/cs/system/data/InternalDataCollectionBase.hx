package cs.system.data;

/** Provides the base functionality for creating collections. */
@:native("System.Data.InternalDataCollectionBase")
extern class InternalDataCollectionBase {
	/**
	 * Gets the total number of elements in a collection.
	 * @return The total number of elements in a collection.
	 */
	var Count(default, never):Int;
	/**
	 * Gets a value that indicates whether the  is read-only.
	 * @return if the collection is read-only; otherwise, . The default is .
	 */
	var IsReadOnly(default, never):Bool;
	/**
	 * Gets a value that indicates whether the  is synchronized.
	 * @return if the collection is synchronized; otherwise, . The default is .
	 */
	var IsSynchronized(default, never):Bool;
	/**
	 * Gets the items of the collection as a list.
	 * @return An  that contains the collection.
	 */
	var List(default, never):cs.system.collections.ArrayList;
	/**
	 * Gets an object that can be used to synchronize the collection.
	 * @return The  used to synchronize the collection.
	 */
	var SyncRoot(default, never):Dynamic;
	function new():Void;
	/**
	 * Copies all the elements of the current  to a one-dimensional , starting at the
	 * specified  index.
	 * @param ar The one-dimensional  to copy the current  object's elements into.
	 * @param index The destination  index to start copying into.
	 */
	function CopyTo(ar:cs.system.Array, index:Int):Void;
	/**
	 * Gets an  for the collection.
	 * @return An  for the collection.
	 */
	function GetEnumerator():cs.system.collections.IEnumerator;
}
