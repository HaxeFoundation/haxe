package cs.system.security.cryptography;

/** Provides an abstract base class that Elliptic Curve Diffie-Hellman (ECDH) algorithm implementations can derive from. This class provides the basic set of operations that all ECDH implementations must support. */
@:native("System.Security.Cryptography.ECDiffieHellman")
extern class ECDiffieHellman extends cs.system.security.cryptography.AsymmetricAlgorithm {
	/**
	 * Gets the public key that is being used by the current Elliptic Curve
	 * Diffie-Hellman (ECDH) instance.
	 * @return The public part of the ECDH key pair that is being used by this 
	 * instance.
	 */
	var PublicKey(default, never):cs.system.security.cryptography.ECDiffieHellmanPublicKey;
	@:overload(function():cs.system.security.cryptography.ECDiffieHellman {})
	@:overload(function(curve:cs.system.security.cryptography.ECCurve):cs.system.security.cryptography.ECDiffieHellman {})
	@:overload(function(parameters:cs.system.security.cryptography.ECParameters):cs.system.security.cryptography.ECDiffieHellman {})
	/**
	 * Creates a new instance of the default implementation of the Elliptic Curve
	 * Diffie-Hellman (ECDH) algorithm.
	 * @return A new instance of the default implementation of this class.
	 */
	static function Create(algorithm:String):cs.system.security.cryptography.ECDiffieHellman;
	@:overload(function(otherPartyPublicKey:cs.system.security.cryptography.ECDiffieHellmanPublicKey, hashAlgorithm:cs.system.security.cryptography.HashAlgorithmName):cs.NativeArray<cs.UInt8> {})
	/**
	 * Derives bytes that can be used as a key using a hash function, given another
	 * party's public key and hash algorithm's name.
	 * @param otherPartyPublicKey The other party's public key.
	 * @param hashAlgorithm The hash algorithm  to use to derive the key material.
	 * @return The key material from the key exchange with the other party's public
	 * key.
	 */
	function DeriveKeyFromHash(otherPartyPublicKey:cs.system.security.cryptography.ECDiffieHellmanPublicKey, hashAlgorithm:cs.system.security.cryptography.HashAlgorithmName, secretPrepend:cs.NativeArray<cs.UInt8>, secretAppend:cs.NativeArray<cs.UInt8>):cs.NativeArray<cs.UInt8>;
	@:overload(function(otherPartyPublicKey:cs.system.security.cryptography.ECDiffieHellmanPublicKey, hashAlgorithm:cs.system.security.cryptography.HashAlgorithmName, hmacKey:cs.NativeArray<cs.UInt8>):cs.NativeArray<cs.UInt8> {})
	/**
	 * Derives bytes that can be used as a key using a Hash-based Message
	 * Authentication Code (HMAC).
	 * @param otherPartyPublicKey The other party's public key.
	 * @param hashAlgorithm The hash algorithm to use to derive the key material.
	 * @param hmacKey The key for the HMAC.
	 * @return The key material from the key exchange with the other party's public
	 * key.
	 */
	function DeriveKeyFromHmac(otherPartyPublicKey:cs.system.security.cryptography.ECDiffieHellmanPublicKey, hashAlgorithm:cs.system.security.cryptography.HashAlgorithmName, hmacKey:cs.NativeArray<cs.UInt8>, secretPrepend:cs.NativeArray<cs.UInt8>, secretAppend:cs.NativeArray<cs.UInt8>):cs.NativeArray<cs.UInt8>;
	/**
	 * Derives bytes that can be used as a key, given another party's public key.
	 * @param otherPartyPublicKey The other party's public key.
	 * @return The key material from the key exchange with the other party's public
	 * key.
	 */
	function DeriveKeyMaterial(otherPartyPublicKey:cs.system.security.cryptography.ECDiffieHellmanPublicKey):cs.NativeArray<cs.UInt8>;
	/**
	 * When implemented in a derived class, derives bytes that can be used as a key
	 * using a Transport Layer Security (TLS) Pseudo-Random Function (PRF) derivation
	 * algorithm.
	 * @param otherPartyPublicKey The other party's public key.
	 * @param prfLabel The ASCII-encoded PRF label.
	 * @param prfSeed The 64-byte PRF seed.
	 * @return The key material from the key exchange with the other party's public
	 * key.
	 */
	function DeriveKeyTls(otherPartyPublicKey:cs.system.security.cryptography.ECDiffieHellmanPublicKey, prfLabel:cs.NativeArray<cs.UInt8>, prfSeed:cs.NativeArray<cs.UInt8>):cs.NativeArray<cs.UInt8>;
	function ExportECPrivateKey():cs.NativeArray<cs.UInt8>;
	/**
	 * When overridden in a derived class, exports either the public or the public and
	 * private key information using the explicit curve form from a working  key to an 
	 * structure so that it can be passed to the  method.
	 * @param includePrivateParameters to include private parameters; otherwise, .
	 * @return An object that represents the point on the curve for this key, using the
	 * explicit curve format.
	 */
	function ExportExplicitParameters(includePrivateParameters:Bool):cs.system.security.cryptography.ECParameters;
	/**
	 * When overridden in a derived class, exports either the public or the public and
	 * private key information from a working  key to an  structure so that it can be
	 * passed to the  method.
	 * @param includePrivateParameters to include private parameters; otherwise,  to
	 * include public parameters only.
	 * @return An object that represents the point on the curve for this key. It can be
	 * passed to the  method.
	 */
	function ExportParameters(includePrivateParameters:Bool):cs.system.security.cryptography.ECParameters;
	/**
	 * This method throws in all cases.
	 * @param xmlString The XML string to use to reconstruct the  object.
	 */
	function FromXmlString(xmlString:String):Void;
	/**
	 * When overridden in a derived class, generates a new ephemeral public/private key
	 * pair for the specified curve.
	 * @param curve The curve used to generate an ephemeral public/private key pair.
	 */
	function GenerateKey(curve:cs.system.security.cryptography.ECCurve):Void;
	function ImportECPrivateKey(source:cs.system.ReadOnlySpan<cs.UInt8>, bytesRead:cs.Ref<Int>):Void;
	/**
	 * When overridden in a derived class, imports the specified parameters for an  as
	 * an ephemeral key into the current  object.
	 * @param parameters The curve's parameters to import.
	 */
	function ImportParameters(parameters:cs.system.security.cryptography.ECParameters):Void;
	/**
	 * This method throws in all cases.
	 * @param includePrivateParameters to include private parameters; otherwise, .
	 * @return This method does not return a value.
	 */
	function ToXmlString(includePrivateParameters:Bool):String;
	function TryExportECPrivateKey(destination:cs.system.Span<cs.UInt8>, bytesWritten:cs.Ref<Int>):Bool;
}
