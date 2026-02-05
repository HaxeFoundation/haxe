package cs.system.net;

/** Provides a collection container for instances of the  class. */
@:native("System.Net.CookieCollection")
extern class CookieCollection {
	/**
	 * Gets the number of cookies contained in a .
	 * @return The number of cookies contained in a .
	 */
	var Count(default, never):Int;
	/**
	 * Gets a value that indicates whether a  is read-only.
	 * @return if this is a read-only ; otherwise, . The default is .
	 */
	var IsReadOnly(default, never):Bool;
	/**
	 * Gets a value that indicates whether access to a  is thread safe.
	 * @return if access to the  is thread safe; otherwise, . The default is .
	 */
	var IsSynchronized(default, never):Bool;
	/**
	 * Gets an object to synchronize access to the .
	 * @return An object to synchronize access to the .
	 */
	var SyncRoot(default, never):Dynamic;
	@:overload(function(index0:Int):cs.system.net.Cookie {})
	@:native("get_Item")
	function get_Item(index0:String):cs.system.net.Cookie;
	function new():Void;
	@:overload(function(cookie:cs.system.net.Cookie):Void {})
	/**
	 * Adds a  to a .
	 * @param cookie The  to be added to a .
	 */
	function Add(cookies:cs.system.net.CookieCollection):Void;
	@:overload(function(array:cs.system.Array, index:Int):Void {})
	/**
	 * Copies the elements of a  to the specified array, starting at a particular
	 * index.
	 * @param array The target array to which the  will be copied.
	 * @param index The zero-based index in the target array where copying begins.
	 */
	function CopyTo(array:cs.NativeArray<cs.system.net.Cookie>, index:Int):Void;
	/**
	 * Gets an enumerator that can iterate through a .
	 * @return An  for this collection.
	 */
	function GetEnumerator():cs.system.collections.IEnumerator;
}
