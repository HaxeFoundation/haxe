package cs.system.security.cryptography.x509certificates;

/** Provides extension methods for retrieving  implementations for the     public and private keys of a  certificate. */
@:native("System.Security.Cryptography.X509Certificates.ECDsaCertificateExtensions")
extern class ECDsaCertificateExtensions {
	/**
	 * Combines a private key with the public key of an  certificate to generate a new
	 * ECDSA certificate.
	 * @param certificate The ECDSA certificate.
	 * @param privateKey The private ECDSA key.
	 * @return A new ECDSA certificate with the  property set to . The input ECDSA
	 * certificate object isn't modified.
	 */
	static function CopyWithPrivateKey(certificate:cs.system.security.cryptography.x509certificates.X509Certificate2, privateKey:cs.system.security.cryptography.ECDsa):cs.system.security.cryptography.x509certificates.X509Certificate2;
	/**
	 * Gets the  private key from the  certificate.
	 * @param certificate The certificate.
	 * @return The private key, or  if the certificate does not have an ECDsa private
	 * key.
	 */
	static function GetECDsaPrivateKey(certificate:cs.system.security.cryptography.x509certificates.X509Certificate2):cs.system.security.cryptography.ECDsa;
	/**
	 * Gets the  public key from the  certificate.
	 * @param certificate The certificate.
	 * @return The public key, or  if the certificate does not have an ECDsa public
	 * key.
	 */
	static function GetECDsaPublicKey(certificate:cs.system.security.cryptography.x509certificates.X509Certificate2):cs.system.security.cryptography.ECDsa;
}
