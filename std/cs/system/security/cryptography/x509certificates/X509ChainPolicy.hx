package cs.system.security.cryptography.x509certificates;

/** Represents the chain policy to be applied when building an X509 certificate chain. This class cannot be inherited. */
@:native("System.Security.Cryptography.X509Certificates.X509ChainPolicy")
extern class X509ChainPolicy {
	/**
	 * Gets a collection of object identifiers (OIDs) specifying which application
	 * policies or enhanced key usages (EKUs) the certificate must support.
	 * @return An  object.
	 */
	var ApplicationPolicy(default, never):cs.system.security.cryptography.OidCollection;
	/**
	 * Gets a collection of object identifiers (OIDs) specifying which certificate
	 * policies the certificate must support.
	 * @return An  object.
	 */
	var CertificatePolicy(default, never):cs.system.security.cryptography.OidCollection;
	/**
	 * Gets an object that represents an additional collection of certificates that can
	 * be searched by the chaining engine when validating a certificate chain.
	 * @return An  object.
	 */
	var ExtraStore(default, never):cs.system.security.cryptography.x509certificates.X509Certificate2Collection;
	/**
	 * Gets or sets values for X509 revocation flags.
	 * @return An  object.
	 */
	var RevocationFlag(default, default):cs.system.security.cryptography.x509certificates.X509RevocationFlag;
	/**
	 * Gets or sets values for X509 certificate revocation mode.
	 * @return An  object.
	 */
	var RevocationMode(default, default):cs.system.security.cryptography.x509certificates.X509RevocationMode;
	/**
	 * Gets or sets the maximum amount of time to be spent during online revocation
	 * verification or downloading the certificate revocation list (CRL). A value of 
	 * means there are no limits.
	 * @return A  object.
	 */
	var UrlRetrievalTimeout(default, default):cs.system.TimeSpan;
	/**
	 * Gets verification flags for the certificate.
	 * @return A value from the  enumeration.
	 */
	var VerificationFlags(default, default):cs.system.security.cryptography.x509certificates.X509VerificationFlags;
	/**
	 * Gets or sets the time for which the chain is to be validated.
	 * @return A  object.
	 */
	var VerificationTime(default, default):cs.system.DateTime;
	function new():Void;
	/** Resets the  members to their default values. */
	function Reset():Void;
}
