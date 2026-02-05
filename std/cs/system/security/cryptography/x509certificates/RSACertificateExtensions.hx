package cs.system.security.cryptography.x509certificates;

/** Provides extension methods for retrieving  implementations for the public and private keys of an . */
@:native("System.Security.Cryptography.X509Certificates.RSACertificateExtensions")
extern class RSACertificateExtensions {
	/**
	 * Combines a private key with the public key of an  certificate to generate a new
	 * RSA certificate.
	 * @param certificate The RSA certificate.
	 * @param privateKey The private RSA key.
	 * @return A new RSA certificate with the  property set to . The input RSA
	 * certificate object isn't modified.
	 */
	static function CopyWithPrivateKey(certificate:cs.system.security.cryptography.x509certificates.X509Certificate2, privateKey:cs.system.security.cryptography.RSA):cs.system.security.cryptography.x509certificates.X509Certificate2;
	/**
	 * Gets the  private key from the .
	 * @param certificate The certificate.
	 * @return The private key, or  if the certificate does not have an RSA private
	 * key.
	 */
	static function GetRSAPrivateKey(certificate:cs.system.security.cryptography.x509certificates.X509Certificate2):cs.system.security.cryptography.RSA;
	/**
	 * Gets the  public key from the .
	 * @param certificate The certificate.
	 * @return The public key, or  if the certificate does not have an RSA public key.
	 */
	static function GetRSAPublicKey(certificate:cs.system.security.cryptography.x509certificates.X509Certificate2):cs.system.security.cryptography.RSA;
}
