package cs.system.security.cryptography.x509certificates;

/** Represents an X.509 store, which is a physical store where certificates are persisted and managed. This class cannot be inherited. */
@:native("System.Security.Cryptography.X509Certificates.X509Store")
extern class X509Store {
	/**
	 * Returns a collection of certificates located in an X.509 certificate store.
	 * @return A collection of certificates.
	 */
	var Certificates(default, never):cs.system.security.cryptography.x509certificates.X509Certificate2Collection;
	var IsOpen(default, never):Bool;
	/**
	 * Gets the location of the X.509 certificate store.
	 * @return The location of the certificate store.
	 */
	var Location(default, never):cs.system.security.cryptography.x509certificates.StoreLocation;
	/**
	 * Gets the name of the X.509 certificate store.
	 * @return The name of the certificate store.
	 */
	var Name(default, never):String;
	/**
	 * Gets an  handle to an  store.
	 * @return A handle to an  store.
	 */
	var StoreHandle(default, never):cs.system.IntPtr;
	@:overload(function():Void {})
	@:overload(function(storeHandle:cs.system.IntPtr):Void {})
	@:overload(function(storeLocation:cs.system.security.cryptography.x509certificates.StoreLocation):Void {})
	@:overload(function(storeName:cs.system.security.cryptography.x509certificates.StoreName):Void {})
	@:overload(function(storeName:String):Void {})
	@:overload(function(storeName:cs.system.security.cryptography.x509certificates.StoreName, storeLocation:cs.system.security.cryptography.x509certificates.StoreLocation):Void {})
	@:overload(function(storeName:String, storeLocation:cs.system.security.cryptography.x509certificates.StoreLocation):Void {})
	@:overload(function(storeName:cs.system.security.cryptography.x509certificates.StoreName, storeLocation:cs.system.security.cryptography.x509certificates.StoreLocation, flags:cs.system.security.cryptography.x509certificates.OpenFlags):Void {})
	function new(storeName:String, storeLocation:cs.system.security.cryptography.x509certificates.StoreLocation, flags:cs.system.security.cryptography.x509certificates.OpenFlags):Void;
	/**
	 * Adds a certificate to an X.509 certificate store.
	 * @param certificate The certificate to add.
	 */
	function Add(certificate:cs.system.security.cryptography.x509certificates.X509Certificate2):Void;
	/**
	 * Adds a collection of certificates to an X.509 certificate store.
	 * @param certificates The collection of certificates to add.
	 */
	function AddRange(certificates:cs.system.security.cryptography.x509certificates.X509Certificate2Collection):Void;
	/** Closes an X.509 certificate store. */
	function Close():Void;
	/** Releases the resources used by this . */
	function Dispose():Void;
	/**
	 * Opens an X.509 certificate store or creates a new store, depending on  flag
	 * settings.
	 * @param flags A bitwise combination of enumeration values that specifies the way
	 * to open the X.509 certificate store.
	 */
	function Open(flags:cs.system.security.cryptography.x509certificates.OpenFlags):Void;
	/**
	 * Removes a certificate from an X.509 certificate store.
	 * @param certificate The certificate to remove.
	 */
	function Remove(certificate:cs.system.security.cryptography.x509certificates.X509Certificate2):Void;
	/**
	 * Removes a range of certificates from an X.509 certificate store.
	 * @param certificates A range of certificates to remove.
	 */
	function RemoveRange(certificates:cs.system.security.cryptography.x509certificates.X509Certificate2Collection):Void;
}
