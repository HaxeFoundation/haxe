package cs.system.collections;

/** Implements the  interface using an array whose size is dynamically increased as required. */
@:native("System.Collections.ArrayList")
extern class ArrayList {
	/**
	 * Gets or sets the number of elements that the  can contain.
	 * @return The number of elements that the  can contain.
	 */
	var Capacity(default, default):Int;
	/**
	 * Gets the number of elements actually contained in the .
	 * @return The number of elements actually contained in the .
	 */
	var Count(default, never):Int;
	/**
	 * Gets a value indicating whether the  has a fixed size.
	 * @return if the  has a fixed size; otherwise, . The default is .
	 */
	var IsFixedSize(default, never):Bool;
	/**
	 * Gets a value indicating whether the  is read-only.
	 * @return if the  is read-only; otherwise, . The default is .
	 */
	var IsReadOnly(default, never):Bool;
	/**
	 * Gets a value indicating whether access to the  is synchronized (thread safe).
	 * @return if access to the  is synchronized (thread safe); otherwise, . The
	 * default is .
	 */
	var IsSynchronized(default, never):Bool;
	/**
	 * Gets an object that can be used to synchronize access to the .
	 * @return An object that can be used to synchronize access to the .
	 */
	var SyncRoot(default, never):Dynamic;
	@:native("get_Item")
	function get_Item(index0:Int):Dynamic;
	@:native("set_Item")
	function set_Item(index0:Int, value:Dynamic):Void;
	@:overload(function():Void {})
	@:overload(function(c:cs.system.collections.ICollection):Void {})
	function new(capacity:Int):Void;
	/**
	 * Creates an  wrapper for a specific .
	 * @param list The  to wrap.
	 * @return The  wrapper around the .
	 */
	static function Adapter(list:cs.system.collections.IList):cs.system.collections.ArrayList;
	@:overload(function(list:cs.system.collections.ArrayList):cs.system.collections.ArrayList {})
	/**
	 * Returns an  wrapper with a fixed size.
	 * @param list The  to wrap.
	 * @return An  wrapper with a fixed size.
	 */
	static function FixedSize(list:cs.system.collections.IList):cs.system.collections.IList;
	@:overload(function(list:cs.system.collections.ArrayList):cs.system.collections.ArrayList {})
	/**
	 * Returns a read-only  wrapper.
	 * @param list The  to wrap.
	 * @return A read-only  wrapper around .
	 */
	static function ReadOnly(list:cs.system.collections.IList):cs.system.collections.IList;
	/**
	 * Returns an  whose elements are copies of the specified value.
	 * @param value The  to copy multiple times in the new . The value can be .
	 * @param count The number of times  should be copied.
	 * @return An  with  number of elements, all of which are copies of .
	 */
	static function Repeat(value:Dynamic, count:Int):cs.system.collections.ArrayList;
	@:overload(function(list:cs.system.collections.ArrayList):cs.system.collections.ArrayList {})
	/**
	 * Returns an  wrapper that is synchronized (thread safe).
	 * @param list The  to synchronize.
	 * @return An  wrapper that is synchronized (thread safe).
	 */
	static function Synchronized(list:cs.system.collections.IList):cs.system.collections.IList;
	/**
	 * Adds an object to the end of the .
	 * @param value The  to be added to the end of the . The value can be .
	 * @return The  index at which the  has been added.
	 */
	function Add(value:Dynamic):Int;
	/**
	 * Adds the elements of an  to the end of the .
	 * @param c The  whose elements should be added to the end of the . The collection
	 * itself cannot be , but it can contain elements that are .
	 */
	function AddRange(c:cs.system.collections.ICollection):Void;
	@:overload(function(value:Dynamic):Int {})
	@:overload(function(value:Dynamic, comparer:cs.system.collections.IComparer):Int {})
	/**
	 * Searches a range of elements in the sorted  for an element using the specified
	 * comparer and returns the zero-based index of the element.
	 * @param index The zero-based starting index of the range to search.
	 * @param count The length of the range to search.
	 * @param value The  to locate. The value can be .
	 * @param comparer The  implementation to use when comparing elements. -or- to use
	 * the default comparer that is the  implementation of each element.
	 * @return The zero-based index of  in the sorted , if  is found; otherwise, a
	 * negative number, which is the bitwise complement of the index of the next
	 * element that is larger than  or, if there is no larger element, the bitwise
	 * complement of .
	 */
	function BinarySearch(index:Int, count:Int, value:Dynamic, comparer:cs.system.collections.IComparer):Int;
	/** Removes all elements from the . */
	function Clear():Void;
	/**
	 * Creates a shallow copy of the .
	 * @return A shallow copy of the .
	 */
	function Clone():Dynamic;
	/**
	 * Determines whether an element is in the .
	 * @param item The  to locate in the . The value can be .
	 * @return if  is found in the ; otherwise, .
	 */
	function Contains(item:Dynamic):Bool;
	@:overload(function(array:cs.system.Array):Void {})
	@:overload(function(array:cs.system.Array, arrayIndex:Int):Void {})
	/**
	 * Copies the entire  to a compatible one-dimensional , starting at the beginning
	 * of the target array.
	 * @param array The one-dimensional  that is the destination of the elements copied
	 * from . The  must have zero-based indexing.
	 */
	function CopyTo(index:Int, array:cs.system.Array, arrayIndex:Int, count:Int):Void;
	@:overload(function():cs.system.collections.IEnumerator {})
	/**
	 * Returns an enumerator for the entire .
	 * @return An  for the entire .
	 */
	function GetEnumerator(index:Int, count:Int):cs.system.collections.IEnumerator;
	/**
	 * Returns an  which represents a subset of the elements in the source .
	 * @param index The zero-based  index at which the range starts.
	 * @param count The number of elements in the range.
	 * @return An  which represents a subset of the elements in the source .
	 */
	function GetRange(index:Int, count:Int):cs.system.collections.ArrayList;
	@:overload(function(value:Dynamic):Int {})
	@:overload(function(value:Dynamic, startIndex:Int):Int {})
	/**
	 * Searches for the specified  and returns the zero-based index of the first
	 * occurrence within the entire .
	 * @param value The  to locate in the . The value can be .
	 * @return The zero-based index of the first occurrence of  within the entire , if
	 * found; otherwise, -1.
	 */
	function IndexOf(value:Dynamic, startIndex:Int, count:Int):Int;
	/**
	 * Inserts an element into the  at the specified index.
	 * @param index The zero-based index at which  should be inserted.
	 * @param value The  to insert. The value can be .
	 */
	function Insert(index:Int, value:Dynamic):Void;
	/**
	 * Inserts the elements of a collection into the  at the specified index.
	 * @param index The zero-based index at which the new elements should be inserted.
	 * @param c The  whose elements should be inserted into the . The collection itself
	 * cannot be , but it can contain elements that are .
	 */
	function InsertRange(index:Int, c:cs.system.collections.ICollection):Void;
	@:overload(function(value:Dynamic):Int {})
	@:overload(function(value:Dynamic, startIndex:Int):Int {})
	/**
	 * Searches for the specified  and returns the zero-based index of the last
	 * occurrence within the entire .
	 * @param value The  to locate in the . The value can be .
	 * @return The zero-based index of the last occurrence of  within the entire the ,
	 * if found; otherwise, -1.
	 */
	function LastIndexOf(value:Dynamic, startIndex:Int, count:Int):Int;
	/**
	 * Removes the first occurrence of a specific object from the .
	 * @param obj The  to remove from the . The value can be .
	 */
	function Remove(obj:Dynamic):Void;
	/**
	 * Removes the element at the specified index of the .
	 * @param index The zero-based index of the element to remove.
	 */
	function RemoveAt(index:Int):Void;
	/**
	 * Removes a range of elements from the .
	 * @param index The zero-based starting index of the range of elements to remove.
	 * @param count The number of elements to remove.
	 */
	function RemoveRange(index:Int, count:Int):Void;
	@:overload(function():Void {})
	/** Reverses the order of the elements in the entire . */
	function Reverse(index:Int, count:Int):Void;
	/**
	 * Copies the elements of a collection over a range of elements in the .
	 * @param index The zero-based  index at which to start copying the elements of .
	 * @param c The  whose elements to copy to the . The collection itself cannot be ,
	 * but it can contain elements that are .
	 */
	function SetRange(index:Int, c:cs.system.collections.ICollection):Void;
	@:overload(function():Void {})
	@:overload(function(comparer:cs.system.collections.IComparer):Void {})
	/** Sorts the elements in the entire . */
	function Sort(index:Int, count:Int, comparer:cs.system.collections.IComparer):Void;
	@:overload(function():cs.NativeArray<Dynamic> {})
	/**
	 * Copies the elements of the  to a new  array.
	 * @return An  array containing copies of the elements of the .
	 */
	function ToArray(type:cs.system.Type):cs.system.Array;
	/** Sets the capacity to the actual number of elements in the . */
	function TrimToSize():Void;
}
