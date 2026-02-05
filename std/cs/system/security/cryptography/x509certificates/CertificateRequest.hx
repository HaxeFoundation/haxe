package cs.system.security.cryptography.x509certificates;

/** Represents an abstraction over the PKCS#10 CertificationRequestInfo and the X.509 TbsCertificate. */
@:native("System.Security.Cryptography.X509Certificates.CertificateRequest")
extern class CertificateRequest {
	/**
	 * Gets the X.509 Certificate Extensions collection, which is a mutable collection,
	 * to include in the certificate or certificate request.
	 * @return The X.509 Certificate Extensions collection to include in the
	 * certificate or certificate request.
	 */
	var CertificateExtensions(default, never):cs.system.collections.objectmodel.Collection<cs.system.security.cryptography.x509certificates.X509Extension>;
	/**
	 * Gets the hash algorithm to use when signing the certificate or certificate
	 * request.
	 * @return The hash algorithm to use when signing the certificate or certificate
	 * request.
	 */
	var HashAlgorithm(default, never):cs.system.security.cryptography.HashAlgorithmName;
	/**
	 * Gets a representation of the public key for the certificate or certificate
	 * request.
	 * @return The representation of the public key for the certificate or certificate
	 * request.
	 */
	var PublicKey(default, never):cs.system.security.cryptography.x509certificates.PublicKey;
	/**
	 * Gets the X.500 Distinguished Name to use as the Subject in a created certificate
	 * or certificate request.
	 * @return The X.500 Distinguished Name to use as the Subject in a created
	 * certificate or certificate request.
	 */
	var SubjectName(default, never):cs.system.security.cryptography.x509certificates.X500DistinguishedName;
	@:overload(function(subjectName:cs.system.security.cryptography.x509certificates.X500DistinguishedName, key:cs.system.security.cryptography.ECDsa, hashAlgorithm:cs.system.security.cryptography.HashAlgorithmName):Void {})
	@:overload(function(subjectName:cs.system.security.cryptography.x509certificates.X500DistinguishedName, publicKey:cs.system.security.cryptography.x509certificates.PublicKey, hashAlgorithm:cs.system.security.cryptography.HashAlgorithmName):Void {})
	@:overload(function(subjectName:String, key:cs.system.security.cryptography.ECDsa, hashAlgorithm:cs.system.security.cryptography.HashAlgorithmName):Void {})
	@:overload(function(subjectName:cs.system.security.cryptography.x509certificates.X500DistinguishedName, key:cs.system.security.cryptography.RSA, hashAlgorithm:cs.system.security.cryptography.HashAlgorithmName, padding:cs.system.security.cryptography.RSASignaturePadding):Void {})
	function new(subjectName:String, key:cs.system.security.cryptography.RSA, hashAlgorithm:cs.system.security.cryptography.HashAlgorithmName, padding:cs.system.security.cryptography.RSASignaturePadding):Void;
	@:overload(function(issuerCertificate:cs.system.security.cryptography.x509certificates.X509Certificate2, notBefore:cs.system.DateTimeOffset, notAfter:cs.system.DateTimeOffset, serialNumber:cs.NativeArray<cs.UInt8>):cs.system.security.cryptography.x509certificates.X509Certificate2 {})
	/**
	 * Signs the current certificate request to create a chain-signed or self-signed
	 * certificate.
	 * @param issuerName The  for the issuer.
	 * @param generator An  object representing the issuing certificate authority.
	 * @param notBefore The oldest date and time when this certificate is considered
	 * valid. Typically , plus or minus a few seconds.
	 * @param notAfter The date and time when this certificate is no longer considered
	 * valid.
	 * @param serialNumber The serial number to use for the new certificate. This value
	 * should be unique per issuer. The value is interpreted as an unsigned integer of
	 * arbitrary size in big-endian byte ordering. RFC 3280 recommends confining it to
	 * 20 bytes or less.
	 * @return An  object with the specified values. The returned object won't assert .
	 */
	function Create(issuerName:cs.system.security.cryptography.x509certificates.X500DistinguishedName, generator:cs.system.security.cryptography.x509certificates.X509SignatureGenerator, notBefore:cs.system.DateTimeOffset, notAfter:cs.system.DateTimeOffset, serialNumber:cs.NativeArray<cs.UInt8>):cs.system.security.cryptography.x509certificates.X509Certificate2;
	/**
	 * Creates a self-signed certificate using the established subject, key, and
	 * optional extensions.
	 * @param notBefore The oldest date and time when this certificate is considered
	 * valid. Typically , plus or minus a few seconds.
	 * @param notAfter The date and time when this certificate is no longer considered
	 * valid.
	 * @return An  object with the specified values. The returned object will assert .
	 */
	function CreateSelfSigned(notBefore:cs.system.DateTimeOffset, notAfter:cs.system.DateTimeOffset):cs.system.security.cryptography.x509certificates.X509Certificate2;
	@:overload(function():cs.NativeArray<cs.UInt8> {})
	/**
	 * Creates an ASN.1 DER-encoded PKCS#10 CertificationRequest value representing the
	 * state of the current object.
	 * @return A DER-encoded certificate signing request.
	 */
	function CreateSigningRequest(signatureGenerator:cs.system.security.cryptography.x509certificates.X509SignatureGenerator):cs.NativeArray<cs.UInt8>;
}
