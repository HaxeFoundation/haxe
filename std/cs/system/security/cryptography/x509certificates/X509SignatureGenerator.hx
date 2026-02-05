package cs.system.security.cryptography.x509certificates;

/** Base class for building encoded signatures as needed for X.509 certificates. */
@:native("System.Security.Cryptography.X509Certificates.X509SignatureGenerator")
extern class X509SignatureGenerator {
	/**
	 * Gets the public key associated with the private key with which signatures are
	 * being generated.
	 * @return The public key associated with the private key with which signatures are
	 * being generated.
	 */
	var PublicKey(default, never):cs.system.security.cryptography.x509certificates.PublicKey;
	/**
	 * Creates an  object for ECDSA signatures using the specified private key.
	 * @param key The private key.
	 * @return An  object for ECDSA signatures.
	 */
	static function CreateForECDsa(key:cs.system.security.cryptography.ECDsa):cs.system.security.cryptography.x509certificates.X509SignatureGenerator;
	/**
	 * Creates an  object for RSA signatures using the specified private key and
	 * padding mode.
	 * @param key The private key.
	 * @param signaturePadding The padding mode.
	 * @return An  object for RSA signatures.
	 */
	static function CreateForRSA(key:cs.system.security.cryptography.RSA, signaturePadding:cs.system.security.cryptography.RSASignaturePadding):cs.system.security.cryptography.x509certificates.X509SignatureGenerator;
	/**
	 * When overridden in a derived class, encodes the X.509 algorithm identifier for
	 * this signature.
	 * @param hashAlgorithm The hash algorithm to use for encoding.
	 * @return The encoded value for the X.509 algorithm identifier.
	 */
	function GetSignatureAlgorithmIdentifier(hashAlgorithm:cs.system.security.cryptography.HashAlgorithmName):cs.NativeArray<cs.UInt8>;
	/**
	 * When overridden in a derived class, produces a signature for the specified data
	 * using the specified hash algorithm and encodes the results appropriately for
	 * X.509 signature values.
	 * @param data The input data for which to produce the signature.
	 * @param hashAlgorithm The hash algorithm to use to produce the signature.
	 * @return The X.509 signature for the specified data.
	 */
	function SignData(data:cs.NativeArray<cs.UInt8>, hashAlgorithm:cs.system.security.cryptography.HashAlgorithmName):cs.NativeArray<cs.UInt8>;
}
