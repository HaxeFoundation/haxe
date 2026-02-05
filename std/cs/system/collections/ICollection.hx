package cs.system.collections;

/** Defines size, enumerators, and synchronization methods for all nongeneric collections. */
@:native("System.Collections.ICollection")
extern interface ICollection extends cs.system.collections.IEnumerable {
	/**
	 * Gets the number of elements contained in the .
	 * @return The number of elements contained in the .
	 */
	var Count(default, never):Int;
	/**
	 * Gets a value indicating whether access to the  is synchronized (thread safe).
	 * @return if access to the  is synchronized (thread safe); otherwise, .
	 */
	var IsSynchronized(default, never):Bool;
	/**
	 * Gets an object that can be used to synchronize access to the .
	 * @return An object that can be used to synchronize access to the .
	 */
	var SyncRoot(default, never):Dynamic;
	/**
	 * Copies the elements of the  to an , starting at a particular  index.
	 * @param array The one-dimensional  that is the destination of the elements copied
	 * from . The  must have zero-based indexing.
	 * @param index The zero-based index in  at which copying begins.
	 */
	function CopyTo(array:cs.system.Array, index:Int):Void;
}
