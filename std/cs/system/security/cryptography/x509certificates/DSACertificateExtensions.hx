package cs.system.security.cryptography.x509certificates;

/** Provides extension methods for retrieving  implementations for the public and private keys of an . */
@:native("System.Security.Cryptography.X509Certificates.DSACertificateExtensions")
extern class DSACertificateExtensions {
	/**
	 * Combines a private key with the public key of a  certificate to generate a new
	 * DSA certificate.
	 * @param certificate The DSA certificate.
	 * @param privateKey The private DSA key.
	 * @return A new DSA certificate with the  property set to . The input DSA
	 * certificate object isn't modified.
	 */
	static function CopyWithPrivateKey(certificate:cs.system.security.cryptography.x509certificates.X509Certificate2, privateKey:cs.system.security.cryptography.DSA):cs.system.security.cryptography.x509certificates.X509Certificate2;
	/**
	 * Gets the  private key from the .
	 * @param certificate The certificate.
	 * @return The private key, or  if the certificate does not have a DSA private key.
	 */
	static function GetDSAPrivateKey(certificate:cs.system.security.cryptography.x509certificates.X509Certificate2):cs.system.security.cryptography.DSA;
	/**
	 * Gets the  public key from the .
	 * @param certificate The certificate.
	 * @return The public key, or  if the certificate does not have a DSA public key.
	 */
	static function GetDSAPublicKey(certificate:cs.system.security.cryptography.x509certificates.X509Certificate2):cs.system.security.cryptography.DSA;
}
