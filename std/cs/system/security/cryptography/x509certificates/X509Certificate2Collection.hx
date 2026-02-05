package cs.system.security.cryptography.x509certificates;

/** Represents a collection of  objects. This class cannot be inherited. */
@:native("System.Security.Cryptography.X509Certificates.X509Certificate2Collection")
extern class X509Certificate2Collection extends cs.system.security.cryptography.x509certificates.X509CertificateCollection {
	@:overload(function():Void {})
	@:overload(function(certificate:cs.system.security.cryptography.x509certificates.X509Certificate2):Void {})
	@:overload(function(certificates:cs.system.security.cryptography.x509certificates.X509Certificate2Collection):Void {})
	function new(certificates:cs.NativeArray<cs.system.security.cryptography.x509certificates.X509Certificate2>):Void;
	/**
	 * Adds an object to the end of the .
	 * @param certificate An X.509 certificate represented as an  object.
	 * @return The  index at which the  has been added.
	 */
	function Add(certificate:cs.system.security.cryptography.x509certificates.X509Certificate2):Int;
	@:overload(function(certificates:cs.system.security.cryptography.x509certificates.X509Certificate2Collection):Void {})
	/**
	 * Adds multiple  objects in an array to the  object.
	 * @param certificates An array of  objects.
	 */
	function AddRange(certificates:cs.NativeArray<cs.system.security.cryptography.x509certificates.X509Certificate2>):Void;
	/**
	 * Determines whether the  object contains a specific certificate.
	 * @param certificate The  object to locate in the collection.
	 * @return if the  contains the specified ; otherwise, .
	 */
	function Contains(certificate:cs.system.security.cryptography.x509certificates.X509Certificate2):Bool;
	@:overload(function(contentType:cs.system.security.cryptography.x509certificates.X509ContentType):cs.NativeArray<cs.UInt8> {})
	/**
	 * Exports X.509 certificate information into a byte array.
	 * @param contentType A supported  object.
	 * @return X.509 certificate information in a byte array.
	 */
	function Export(contentType:cs.system.security.cryptography.x509certificates.X509ContentType, password:String):cs.NativeArray<cs.UInt8>;
	/**
	 * Searches an  object using the search criteria specified by the  enumeration and
	 * the  object.
	 * @param findType One of the  values.
	 * @param findValue The search criteria as an object.
	 * @param validOnly to allow only valid certificates to be returned from the
	 * search; otherwise, .
	 * @return An  object.
	 */
	function Find(findType:cs.system.security.cryptography.x509certificates.X509FindType, findValue:Dynamic, validOnly:Bool):cs.system.security.cryptography.x509certificates.X509Certificate2Collection;
	/**
	 * Returns an enumerator that can iterate through a  object.
	 * @return An  object that can iterate through the  object.
	 */
	function GetEnumerator():cs.system.security.cryptography.x509certificates.X509Certificate2Enumerator;
	@:overload(function(rawData:cs.NativeArray<cs.UInt8>):Void {})
	@:overload(function(fileName:String):Void {})
	@:overload(function(rawData:cs.NativeArray<cs.UInt8>, password:String, keyStorageFlags:cs.system.security.cryptography.x509certificates.X509KeyStorageFlags):Void {})
	/**
	 * Imports a certificate in the form of a byte array into a  object.
	 * @param rawData A byte array containing data from an X.509 certificate.
	 */
	function Import(fileName:String, password:String, keyStorageFlags:cs.system.security.cryptography.x509certificates.X509KeyStorageFlags):Void;
	/**
	 * Inserts an object into the  object at the specified index.
	 * @param index The zero-based index at which to insert .
	 * @param certificate The  object to insert.
	 */
	function Insert(index:Int, certificate:cs.system.security.cryptography.x509certificates.X509Certificate2):Void;
	/**
	 * Removes the first occurrence of a certificate from the  object.
	 * @param certificate The  object to be removed from the  object.
	 */
	function Remove(certificate:cs.system.security.cryptography.x509certificates.X509Certificate2):Void;
	@:overload(function(certificates:cs.system.security.cryptography.x509certificates.X509Certificate2Collection):Void {})
	/**
	 * Removes multiple  objects in an array from an  object.
	 * @param certificates An array of  objects.
	 */
	function RemoveRange(certificates:cs.NativeArray<cs.system.security.cryptography.x509certificates.X509Certificate2>):Void;
}
