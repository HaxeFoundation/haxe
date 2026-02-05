package cs.system.collections;

/** Provides the  base class for a strongly typed collection of key/value pairs. */
@:native("System.Collections.DictionaryBase")
extern class DictionaryBase {
	/**
	 * Gets the number of elements contained in the  instance.
	 * @return The number of elements contained in the  instance.
	 */
	var Count(default, never):Int;
	/**
	 * Gets the list of elements contained in the  instance.
	 * @return An  representing the  instance itself.
	 */
	var Dictionary(default, never):cs.system.collections.IDictionary;
	/**
	 * Gets the list of elements contained in the  instance.
	 * @return A  representing the  instance itself.
	 */
	var InnerHashtable(default, never):cs.system.collections.Hashtable;
	/** Clears the contents of the  instance. */
	function Clear():Void;
	/**
	 * Copies the  elements to a one-dimensional  at the specified index.
	 * @param array The one-dimensional  that is the destination of the  objects copied
	 * from the  instance. The  must have zero-based indexing.
	 * @param index The zero-based index in  at which copying begins.
	 */
	function CopyTo(array:cs.system.Array, index:Int):Void;
	/**
	 * Returns an  that iterates through the  instance.
	 * @return An  for the  instance.
	 */
	function GetEnumerator():cs.system.collections.IDictionaryEnumerator;
}
