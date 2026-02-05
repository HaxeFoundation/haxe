package cs.system.collections.specialized;

/** Represents a collection of strings. */
@:native("System.Collections.Specialized.StringCollection")
extern class StringCollection {
	/**
	 * Gets the number of strings contained in the .
	 * @return The number of strings contained in the .
	 */
	var Count(default, never):Int;
	/**
	 * Gets a value indicating whether the  is read-only.
	 * @return This property always returns .
	 */
	var IsReadOnly(default, never):Bool;
	/**
	 * Gets a value indicating whether access to the  is synchronized (thread safe).
	 * @return This property always returns .
	 */
	var IsSynchronized(default, never):Bool;
	/**
	 * Gets an object that can be used to synchronize access to the .
	 * @return An object that can be used to synchronize access to the .
	 */
	var SyncRoot(default, never):Dynamic;
	@:native("get_Item")
	function get_Item(index0:Int):String;
	@:native("set_Item")
	function set_Item(index0:Int, value:String):Void;
	function new():Void;
	/**
	 * Adds a string to the end of the .
	 * @param value The string to add to the end of the . The value can be .
	 * @return The zero-based index at which the new element is inserted.
	 */
	function Add(value:String):Int;
	/**
	 * Copies the elements of a string array to the end of the .
	 * @param value An array of strings to add to the end of the . The array itself can
	 * not be  but it can contain elements that are .
	 */
	function AddRange(value:cs.NativeArray<String>):Void;
	/** Removes all the strings from the . */
	function Clear():Void;
	/**
	 * Determines whether the specified string is in the .
	 * @param value The string to locate in the . The value can be .
	 * @return if  is found in the ; otherwise, .
	 */
	function Contains(value:String):Bool;
	/**
	 * Copies the entire  values to a one-dimensional array of strings, starting at the
	 * specified index of the target array.
	 * @param array The one-dimensional array of strings that is the destination of the
	 * elements copied from . The  must have zero-based indexing.
	 * @param index The zero-based index in  at which copying begins.
	 */
	function CopyTo(array:cs.NativeArray<String>, index:Int):Void;
	/**
	 * Returns a  that iterates through the .
	 * @return A  for the .
	 */
	function GetEnumerator():cs.system.collections.specialized.StringEnumerator;
	/**
	 * Searches for the specified string and returns the zero-based index of the first
	 * occurrence within the .
	 * @param value The string to locate. The value can be .
	 * @return The zero-based index of the first occurrence of  in the , if found;
	 * otherwise, -1.
	 */
	function IndexOf(value:String):Int;
	/**
	 * Inserts a string into the  at the specified index.
	 * @param index The zero-based index at which  is inserted.
	 * @param value The string to insert. The value can be .
	 */
	function Insert(index:Int, value:String):Void;
	/**
	 * Removes the first occurrence of a specific string from the .
	 * @param value The string to remove from the . The value can be .
	 */
	function Remove(value:String):Void;
	/**
	 * Removes the string at the specified index of the .
	 * @param index The zero-based index of the string to remove.
	 */
	function RemoveAt(index:Int):Void;
}
