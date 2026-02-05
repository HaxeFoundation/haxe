package cs.system.security.cryptography;

/** Represents a collection of  objects. This class cannot be inherited. */
@:native("System.Security.Cryptography.AsnEncodedDataCollection")
extern class AsnEncodedDataCollection {
	/**
	 * Gets the number of  objects in a collection.
	 * @return The number of  objects.
	 */
	var Count(default, never):Int;
	/**
	 * Gets a value that indicates whether access to the  object is thread safe.
	 * @return in all cases.
	 */
	var IsSynchronized(default, never):Bool;
	/**
	 * Gets an object that can be used to synchronize access to the  object.
	 * @return An object used to synchronize access to the  object.
	 */
	var SyncRoot(default, never):Dynamic;
	@:native("get_Item")
	function get_Item(index0:Int):cs.system.security.cryptography.AsnEncodedData;
	@:overload(function():Void {})
	function new(asnEncodedData:cs.system.security.cryptography.AsnEncodedData):Void;
	/**
	 * Adds an  object to the  object.
	 * @param asnEncodedData The  object to add to the collection.
	 * @return The index of the added  object.
	 */
	function Add(asnEncodedData:cs.system.security.cryptography.AsnEncodedData):Int;
	/**
	 * Copies the  object into an array.
	 * @param array The array that the  object is to be copied into.
	 * @param index The location where the copy operation starts.
	 */
	function CopyTo(array:cs.NativeArray<cs.system.security.cryptography.AsnEncodedData>, index:Int):Void;
	/**
	 * Returns an  object that can be used to navigate the  object.
	 * @return An  object.
	 */
	function GetEnumerator():cs.system.security.cryptography.AsnEncodedDataEnumerator;
	/**
	 * Removes an  object from the  object.
	 * @param asnEncodedData The  object to remove.
	 */
	function Remove(asnEncodedData:cs.system.security.cryptography.AsnEncodedData):Void;
}
