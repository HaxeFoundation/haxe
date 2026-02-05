package cs.system.security.cryptography.x509certificates;

/** Defines a string that identifies a certificate's subject key identifier (SKI). This class cannot be inherited. */
@:native("System.Security.Cryptography.X509Certificates.X509SubjectKeyIdentifierExtension")
extern class X509SubjectKeyIdentifierExtension extends cs.system.security.cryptography.x509certificates.X509Extension {
	/**
	 * Gets a string that represents the subject key identifier (SKI) for a
	 * certificate.
	 * @return A string, encoded in hexadecimal format, that represents the subject key
	 * identifier (SKI).
	 */
	var SubjectKeyIdentifier(default, never):String;
	@:overload(function():Void {})
	@:overload(function(subjectKeyIdentifier:cs.NativeArray<cs.UInt8>, critical:Bool):Void {})
	@:overload(function(encodedSubjectKeyIdentifier:cs.system.security.cryptography.AsnEncodedData, critical:Bool):Void {})
	@:overload(function(key:cs.system.security.cryptography.x509certificates.PublicKey, critical:Bool):Void {})
	@:overload(function(subjectKeyIdentifier:String, critical:Bool):Void {})
	function new(key:cs.system.security.cryptography.x509certificates.PublicKey, algorithm:cs.system.security.cryptography.x509certificates.X509SubjectKeyIdentifierHashAlgorithm, critical:Bool):Void;
	/**
	 * Creates a new instance of the  class by copying information from encoded data.
	 * @param asnEncodedData The  object to use to create the extension.
	 */
	function CopyFrom(asnEncodedData:cs.system.security.cryptography.AsnEncodedData):Void;
}
