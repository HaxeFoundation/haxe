package cs.system.security.cryptography.x509certificates;

/** Represents a collection of  objects. This class cannot be inherited. */
@:native("System.Security.Cryptography.X509Certificates.X509ExtensionCollection")
extern class X509ExtensionCollection {
	/**
	 * Gets the number of  objects in a  object.
	 * @return An integer representing the number of  objects in the  object.
	 */
	var Count(default, never):Int;
	/**
	 * Gets a value indicating whether the collection is guaranteed to be thread safe.
	 * @return if the collection is thread safe; otherwise, .
	 */
	var IsSynchronized(default, never):Bool;
	/**
	 * Gets an object that you can use to synchronize access to the  object.
	 * @return An object that you can use to synchronize access to the  object.
	 */
	var SyncRoot(default, never):Dynamic;
	@:overload(function(index0:Int):cs.system.security.cryptography.x509certificates.X509Extension {})
	@:native("get_Item")
	function get_Item(index0:String):cs.system.security.cryptography.x509certificates.X509Extension;
	function new():Void;
	/**
	 * Adds an  object to an  object.
	 * @param extension An  object to add to the  object.
	 * @return The index at which the  parameter was added.
	 */
	function Add(extension:cs.system.security.cryptography.x509certificates.X509Extension):Int;
	/**
	 * Copies a collection into an array starting at the specified index.
	 * @param array An array of  objects.
	 * @param index The location in the array at which copying starts.
	 */
	function CopyTo(array:cs.NativeArray<cs.system.security.cryptography.x509certificates.X509Extension>, index:Int):Void;
	/**
	 * Returns an enumerator that can iterate through an  object.
	 * @return An  object to use to iterate through the  object.
	 */
	function GetEnumerator():cs.system.security.cryptography.x509certificates.X509ExtensionEnumerator;
}
