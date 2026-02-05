package cs.system.collections;

/** Provides the  base class for a strongly typed collection. */
@:native("System.Collections.CollectionBase")
extern class CollectionBase {
	/**
	 * Gets or sets the number of elements that the  can contain.
	 * @return The number of elements that the  can contain.
	 */
	var Capacity(default, default):Int;
	/**
	 * Gets the number of elements contained in the  instance. This property cannot be
	 * overridden.
	 * @return The number of elements contained in the  instance. Retrieving the value
	 * of this property is an O(1) operation.
	 */
	var Count(default, never):Int;
	/**
	 * Gets an  containing the list of elements in the  instance.
	 * @return An  representing the  instance itself. Retrieving the value of this
	 * property is an O(1) operation.
	 */
	var InnerList(default, never):cs.system.collections.ArrayList;
	/**
	 * Gets an  containing the list of elements in the  instance.
	 * @return An  representing the  instance itself.
	 */
	var List(default, never):cs.system.collections.IList;
	/** Removes all objects from the  instance. This method cannot be overridden. */
	function Clear():Void;
	/**
	 * Returns an enumerator that iterates through the  instance.
	 * @return An  for the  instance.
	 */
	function GetEnumerator():cs.system.collections.IEnumerator;
	/**
	 * Removes the element at the specified index of the  instance. This method is not
	 * overridable.
	 * @param index The zero-based index of the element to remove.
	 */
	function RemoveAt(index:Int):Void;
}
