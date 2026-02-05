package cs.system.security.cryptography.x509certificates;

/** Defines the usage of a key contained within an X.509 certificate.  This class cannot be inherited. */
@:native("System.Security.Cryptography.X509Certificates.X509KeyUsageExtension")
extern class X509KeyUsageExtension extends cs.system.security.cryptography.x509certificates.X509Extension {
	/**
	 * Gets the key usage flag associated with the certificate.
	 * @return One of the  values.
	 */
	var KeyUsages(default, never):cs.system.security.cryptography.x509certificates.X509KeyUsageFlags;
	@:overload(function():Void {})
	@:overload(function(encodedKeyUsage:cs.system.security.cryptography.AsnEncodedData, critical:Bool):Void {})
	function new(keyUsages:cs.system.security.cryptography.x509certificates.X509KeyUsageFlags, critical:Bool):Void;
	/**
	 * Initializes a new instance of the  class using an  object.
	 * @param asnEncodedData The encoded data to use to create the extension.
	 */
	function CopyFrom(asnEncodedData:cs.system.security.cryptography.AsnEncodedData):Void;
}
