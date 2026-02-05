package cs.system.security.cryptography.x509certificates;

/** Defines the constraints set on a certificate. This class cannot be inherited. */
@:native("System.Security.Cryptography.X509Certificates.X509BasicConstraintsExtension")
extern class X509BasicConstraintsExtension extends cs.system.security.cryptography.x509certificates.X509Extension {
	/**
	 * Gets a value indicating whether a certificate is a certificate authority (CA)
	 * certificate.
	 * @return if the certificate is a certificate authority (CA) certificate,
	 * otherwise, .
	 */
	var CertificateAuthority(default, never):Bool;
	/**
	 * Gets a value indicating whether a certificate has a restriction on the number of
	 * path levels it allows.
	 * @return if the certificate has a restriction on the number of path levels it
	 * allows, otherwise, .
	 */
	var HasPathLengthConstraint(default, never):Bool;
	/**
	 * Gets the number of levels allowed in a certificate's path.
	 * @return An integer indicating the number of levels allowed in a certificate's
	 * path.
	 */
	var PathLengthConstraint(default, never):Int;
	@:overload(function():Void {})
	@:overload(function(encodedBasicConstraints:cs.system.security.cryptography.AsnEncodedData, critical:Bool):Void {})
	function new(certificateAuthority:Bool, hasPathLengthConstraint:Bool, pathLengthConstraint:Int, critical:Bool):Void;
	/**
	 * Initializes a new instance of the  class using an  object.
	 * @param asnEncodedData The encoded data to use to create the extension.
	 */
	function CopyFrom(asnEncodedData:cs.system.security.cryptography.AsnEncodedData):Void;
}
