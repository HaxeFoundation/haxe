package cs.system.collections;

/** Represents a first-in, first-out collection of objects. */
@:native("System.Collections.Queue")
extern class Queue {
	/**
	 * Gets the number of elements contained in the .
	 * @return The number of elements contained in the .
	 */
	var Count(default, never):Int;
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
	@:overload(function():Void {})
	@:overload(function(col:cs.system.collections.ICollection):Void {})
	@:overload(function(capacity:Int):Void {})
	function new(capacity:Int, growFactor:Single):Void;
	/**
	 * Returns a new  that wraps the original queue, and is thread safe.
	 * @param queue The  to synchronize.
	 * @return A  wrapper that is synchronized (thread safe).
	 */
	static function Synchronized(queue:cs.system.collections.Queue):cs.system.collections.Queue;
	/** Removes all objects from the . */
	function Clear():Void;
	/**
	 * Creates a shallow copy of the .
	 * @return A shallow copy of the .
	 */
	function Clone():Dynamic;
	/**
	 * Determines whether an element is in the .
	 * @param obj The  to locate in the . The value can be .
	 * @return if  is found in the ; otherwise, .
	 */
	function Contains(obj:Dynamic):Bool;
	/**
	 * Copies the  elements to an existing one-dimensional , starting at the specified
	 * array index.
	 * @param array The one-dimensional  that is the destination of the elements copied
	 * from . The  must have zero-based indexing.
	 * @param index The zero-based index in  at which copying begins.
	 */
	function CopyTo(array:cs.system.Array, index:Int):Void;
	/**
	 * Removes and returns the object at the beginning of the .
	 * @return The object that is removed from the beginning of the .
	 */
	function Dequeue():Dynamic;
	/**
	 * Adds an object to the end of the .
	 * @param obj The object to add to the . The value can be .
	 */
	function Enqueue(obj:Dynamic):Void;
	/**
	 * Returns an enumerator that iterates through the .
	 * @return An  for the .
	 */
	function GetEnumerator():cs.system.collections.IEnumerator;
	/**
	 * Returns the object at the beginning of the  without removing it.
	 * @return The object at the beginning of the .
	 */
	function Peek():Dynamic;
	/**
	 * Copies the  elements to a new array.
	 * @return A new array containing elements copied from the .
	 */
	function ToArray():cs.NativeArray<Dynamic>;
	/** Sets the capacity to the actual number of elements in the . */
	function TrimToSize():Void;
}
