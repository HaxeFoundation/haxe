package cs.system.security.cryptography.x509certificates;

/** Represents a collection of  objects. This class cannot be inherited. */
@:native("System.Security.Cryptography.X509Certificates.X509ChainElementCollection")
extern class X509ChainElementCollection {
	/**
	 * Gets the number of elements in the collection.
	 * @return An integer representing the number of elements in the collection.
	 */
	var Count(default, never):Int;
	/**
	 * Gets a value indicating whether the collection of chain elements is
	 * synchronized.
	 * @return Always returns .
	 */
	var IsSynchronized(default, never):Bool;
	/**
	 * Gets an object that can be used to synchronize access to an  object.
	 * @return A pointer reference to the current object.
	 */
	var SyncRoot(default, never):Dynamic;
	@:native("get_Item")
	function get_Item(index0:Int):cs.system.security.cryptography.x509certificates.X509ChainElement;
	/**
	 * Copies an  object into an array, starting at the specified index.
	 * @param array An array of  objects.
	 * @param index An integer representing the index value.
	 */
	function CopyTo(array:cs.NativeArray<cs.system.security.cryptography.x509certificates.X509ChainElement>, index:Int):Void;
	/**
	 * Gets an  object that can be used to navigate through a collection of chain
	 * elements.
	 * @return An  object.
	 */
	function GetEnumerator():cs.system.security.cryptography.x509certificates.X509ChainElementEnumerator;
}
