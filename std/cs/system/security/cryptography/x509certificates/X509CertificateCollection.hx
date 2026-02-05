package cs.system.security.cryptography.x509certificates;

/** Defines a collection that stores  objects. */
@:native("System.Security.Cryptography.X509Certificates.X509CertificateCollection")
extern class X509CertificateCollection extends cs.system.collections.CollectionBase {
	@:native("get_Item")
	function get_Item(index0:Int):cs.system.security.cryptography.x509certificates.X509Certificate;
	@:native("set_Item")
	function set_Item(index0:Int, value:cs.system.security.cryptography.x509certificates.X509Certificate):Void;
	@:overload(function():Void {})
	@:overload(function(value:cs.system.security.cryptography.x509certificates.X509CertificateCollection):Void {})
	function new(value:cs.NativeArray<cs.system.security.cryptography.x509certificates.X509Certificate>):Void;
	/**
	 * Adds an  with the specified value to the current .
	 * @param value The  to add to the current .
	 * @return The index into the current  at which the new  was inserted.
	 */
	function Add(value:cs.system.security.cryptography.x509certificates.X509Certificate):Int;
	@:overload(function(value:cs.system.security.cryptography.x509certificates.X509CertificateCollection):Void {})
	/**
	 * Copies the elements of an array of type  to the end of the current .
	 * @param value The array of type  containing the objects to add to the current .
	 */
	function AddRange(value:cs.NativeArray<cs.system.security.cryptography.x509certificates.X509Certificate>):Void;
	/**
	 * Gets a value indicating whether the current  contains the specified .
	 * @param value The  to locate.
	 * @return if the  is contained in this collection; otherwise, .
	 */
	function Contains(value:cs.system.security.cryptography.x509certificates.X509Certificate):Bool;
	/**
	 * Copies the  values in the current  to a one-dimensional  instance at the
	 * specified index.
	 * @param array The one-dimensional  that is the destination of the values copied
	 * from .
	 * @param index The index into  to begin copying.
	 */
	function CopyTo(array:cs.NativeArray<cs.system.security.cryptography.x509certificates.X509Certificate>, index:Int):Void;
	/**
	 * Returns an enumerator that can iterate through the .
	 * @return An enumerator of the subelements of  you can use to iterate through the
	 * collection.
	 */
	function GetEnumerator():cs.system.security.cryptography.x509certificates.X509CertificateCollection_X509CertificateEnumerator;
	/**
	 * Builds a hash value based on all values contained in the current .
	 * @return A hash value based on all values contained in the current .
	 */
	function GetHashCode():Int;
	/**
	 * Returns the index of the specified  in the current .
	 * @param value The  to locate.
	 * @return The index of the  specified by the  parameter in the , if found;
	 * otherwise, -1.
	 */
	function IndexOf(value:cs.system.security.cryptography.x509certificates.X509Certificate):Int;
	/**
	 * Inserts a  into the current  at the specified index.
	 * @param index The zero-based index where  should be inserted.
	 * @param value The  to insert.
	 */
	function Insert(index:Int, value:cs.system.security.cryptography.x509certificates.X509Certificate):Void;
	/**
	 * Removes a specific  from the current .
	 * @param value The  to remove from the current .
	 */
	function Remove(value:cs.system.security.cryptography.x509certificates.X509Certificate):Void;
}
