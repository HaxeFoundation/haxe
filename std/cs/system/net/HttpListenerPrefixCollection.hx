package cs.system.net;

/** Represents the collection used to store Uniform Resource Identifier (URI) prefixes for  objects. */
@:native("System.Net.HttpListenerPrefixCollection")
extern class HttpListenerPrefixCollection {
	/**
	 * Gets the number of prefixes contained in the collection.
	 * @return An  that contains the number of prefixes in this collection.
	 */
	var Count(default, never):Int;
	/**
	 * Gets a value that indicates whether access to the collection is read-only.
	 * @return Always returns .
	 */
	var IsReadOnly(default, never):Bool;
	/**
	 * Gets a value that indicates whether access to the collection is synchronized
	 * (thread-safe).
	 * @return This property always returns .
	 */
	var IsSynchronized(default, never):Bool;
	/**
	 * Adds a Uniform Resource Identifier (URI) prefix to the collection.
	 * @param uriPrefix A  that identifies the URI information that is compared in
	 * incoming requests. The prefix must be terminated with a forward slash ("/").
	 */
	function Add(uriPrefix:String):Void;
	/** Removes all the Uniform Resource Identifier (URI) prefixes from the collection. */
	function Clear():Void;
	/**
	 * Returns a  value that indicates whether the specified prefix is contained in the
	 * collection.
	 * @param uriPrefix A  that contains the Uniform Resource Identifier (URI) prefix
	 * to test.
	 * @return if this collection contains the prefix specified by ; otherwise, .
	 */
	function Contains(uriPrefix:String):Bool;
	@:overload(function(array:cs.system.Array, offset:Int):Void {})
	/**
	 * Copies the contents of an  to the specified array.
	 * @param array The one dimensional  that receives the Uniform Resource Identifier
	 * (URI) prefix strings in this collection.
	 * @param offset The zero-based index in  at which copying begins.
	 */
	function CopyTo(array:cs.NativeArray<String>, offset:Int):Void;
	/**
	 * Returns an object that can be used to iterate through the collection.
	 * @return An object that implements the  interface and provides access to the
	 * strings in this collection.
	 */
	function GetEnumerator():cs.system.collections.generic.IEnumerator<String>;
	/**
	 * Removes the specified Uniform Resource Identifier (URI) from the list of
	 * prefixes handled by the  object.
	 * @param uriPrefix A  that contains the URI prefix to remove.
	 * @return if the  was found in the  and removed; otherwise .
	 */
	function Remove(uriPrefix:String):Bool;
}
