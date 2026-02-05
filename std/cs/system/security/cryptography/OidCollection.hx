package cs.system.security.cryptography;

/** Represents a collection of  objects. This class cannot be inherited. */
@:native("System.Security.Cryptography.OidCollection")
extern class OidCollection {
	/**
	 * Gets the number of  objects in a collection.
	 * @return The number of  objects in a collection.
	 */
	var Count(default, never):Int;
	/**
	 * Gets a value that indicates whether access to the  object is thread safe.
	 * @return in all cases.
	 */
	var IsSynchronized(default, never):Bool;
	/**
	 * Gets an object that can be used to synchronize access to the  object.
	 * @return An object that can be used to synchronize access to the  object.
	 */
	var SyncRoot(default, never):Dynamic;
	@:overload(function(index0:Int):cs.system.security.cryptography.Oid {})
	@:native("get_Item")
	function get_Item(index0:String):cs.system.security.cryptography.Oid;
	function new():Void;
	/**
	 * Adds an  object to the  object.
	 * @param oid The  object to add to the collection.
	 * @return The index of the added  object.
	 */
	function Add(oid:cs.system.security.cryptography.Oid):Int;
	/**
	 * Copies the  object into an array.
	 * @param array The array to copy the  object into.
	 * @param index The location where the copy operation starts.
	 */
	function CopyTo(array:cs.NativeArray<cs.system.security.cryptography.Oid>, index:Int):Void;
	/**
	 * Returns an  object that can be used to navigate the  object.
	 * @return An  object.
	 */
	function GetEnumerator():cs.system.security.cryptography.OidEnumerator;
}
