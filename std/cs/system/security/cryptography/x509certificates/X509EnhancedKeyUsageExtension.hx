package cs.system.security.cryptography.x509certificates;

/** Defines the collection of object identifiers (OIDs) that indicates the applications that use the key. This class cannot be inherited. */
@:native("System.Security.Cryptography.X509Certificates.X509EnhancedKeyUsageExtension")
extern class X509EnhancedKeyUsageExtension extends cs.system.security.cryptography.x509certificates.X509Extension {
	/**
	 * Gets the collection of object identifiers (OIDs) that indicate the applications
	 * that use the key.
	 * @return An  object indicating the applications that use the key.
	 */
	var EnhancedKeyUsages(default, never):cs.system.security.cryptography.OidCollection;
	@:overload(function():Void {})
	@:overload(function(encodedEnhancedKeyUsages:cs.system.security.cryptography.AsnEncodedData, critical:Bool):Void {})
	function new(enhancedKeyUsages:cs.system.security.cryptography.OidCollection, critical:Bool):Void;
	/**
	 * Initializes a new instance of the  class using an  object.
	 * @param asnEncodedData The encoded data to use to create the extension.
	 */
	function CopyFrom(asnEncodedData:cs.system.security.cryptography.AsnEncodedData):Void;
}
