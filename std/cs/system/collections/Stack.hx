package cs.system.collections;

/** Represents a simple last-in-first-out (LIFO) non-generic collection of objects. */
@:native("System.Collections.Stack")
extern class Stack {
	/**
	 * Gets the number of elements contained in the .
	 * @return The number of elements contained in the .
	 */
	var Count(default, never):Int;
	/**
	 * Gets a value indicating whether access to the  is synchronized (thread safe).
	 * @return , if access to the  is synchronized (thread safe); otherwise, . The
	 * default is .
	 */
	var IsSynchronized(default, never):Bool;
	/**
	 * Gets an object that can be used to synchronize access to the .
	 * @return An  that can be used to synchronize access to the .
	 */
	var SyncRoot(default, never):Dynamic;
	@:overload(function():Void {})
	@:overload(function(col:cs.system.collections.ICollection):Void {})
	function new(initialCapacity:Int):Void;
	/**
	 * Returns a synchronized (thread safe) wrapper for the .
	 * @param stack The  to synchronize.
	 * @return A synchronized wrapper around the .
	 */
	static function Synchronized(stack:cs.system.collections.Stack):cs.system.collections.Stack;
	/** Removes all objects from the . */
	function Clear():Void;
	/**
	 * Creates a shallow copy of the .
	 * @return A shallow copy of the .
	 */
	function Clone():Dynamic;
	/**
	 * Determines whether an element is in the .
	 * @param obj The object to locate in the . The value can be .
	 * @return , if  is found in the ; otherwise, .
	 */
	function Contains(obj:Dynamic):Bool;
	/**
	 * Copies the  to an existing one-dimensional , starting at the specified array
	 * index.
	 * @param array The one-dimensional  that is the destination of the elements copied
	 * from . The  must have zero-based indexing.
	 * @param index The zero-based index in  at which copying begins.
	 */
	function CopyTo(array:cs.system.Array, index:Int):Void;
	/**
	 * Returns an  for the .
	 * @return An  for the .
	 */
	function GetEnumerator():cs.system.collections.IEnumerator;
	/**
	 * Returns the object at the top of the  without removing it.
	 * @return The  at the top of the .
	 */
	function Peek():Dynamic;
	/**
	 * Removes and returns the object at the top of the .
	 * @return The  removed from the top of the .
	 */
	function Pop():Dynamic;
	/**
	 * Inserts an object at the top of the .
	 * @param obj The  to push onto the . The value can be .
	 */
	function Push(obj:Dynamic):Void;
	/**
	 * Copies the  to a new array.
	 * @return A new array containing copies of the elements of the .
	 */
	function ToArray():cs.NativeArray<Dynamic>;
}
