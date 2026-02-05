package cs.system.security.cryptography;

/** Provides an abstract base class that encapsulates the Elliptic Curve Digital Signature Algorithm (ECDSA). */
@:native("System.Security.Cryptography.ECDsa")
extern class ECDsa extends cs.system.security.cryptography.AsymmetricAlgorithm {
	@:overload(function():cs.system.security.cryptography.ECDsa {})
	@:overload(function(curve:cs.system.security.cryptography.ECCurve):cs.system.security.cryptography.ECDsa {})
	@:overload(function(parameters:cs.system.security.cryptography.ECParameters):cs.system.security.cryptography.ECDsa {})
	/**
	 * Creates a new instance of the default implementation of the Elliptic Curve
	 * Digital Signature Algorithm (ECDSA).
	 * @return A new instance of the default implementation () of this class.
	 */
	static function Create(algorithm:String):cs.system.security.cryptography.ECDsa;
	function ExportECPrivateKey():cs.NativeArray<cs.UInt8>;
	/**
	 * When overridden in a derived class, exports the explicit parameters for an
	 * elliptic curve.
	 * @param includePrivateParameters to include private parameters; otherwise, .
	 * @return The parameters representing the point on the curve for this key, using
	 * the explicit curve format.
	 */
	function ExportExplicitParameters(includePrivateParameters:Bool):cs.system.security.cryptography.ECParameters;
	/**
	 * When overridden in a derived class, exports the named or explicit parameters for
	 * an elliptic curve. If the curve has a name, the  field contains named curve
	 * parameters, otherwise it         contains explicit parameters.
	 * @param includePrivateParameters to include private parameters; otherwise, .
	 * @return The parameters representing the point on the curve for this key.
	 */
	function ExportParameters(includePrivateParameters:Bool):cs.system.security.cryptography.ECParameters;
	/**
	 * This method throws in all cases.
	 * @param xmlString The XML string to use to reconstruct the  object.
	 */
	function FromXmlString(xmlString:String):Void;
	/**
	 * When overridden in a derived class, generates a new public/private key pair for
	 * the specified curve.
	 * @param curve The curve to use.
	 */
	function GenerateKey(curve:cs.system.security.cryptography.ECCurve):Void;
	function ImportECPrivateKey(source:cs.system.ReadOnlySpan<cs.UInt8>, bytesRead:cs.Ref<Int>):Void;
	/**
	 * When overridden in a derived class, imports the specified parameters.
	 * @param parameters The curve parameters.
	 */
	function ImportParameters(parameters:cs.system.security.cryptography.ECParameters):Void;
	@:overload(function(data:cs.NativeArray<cs.UInt8>, hashAlgorithm:cs.system.security.cryptography.HashAlgorithmName):cs.NativeArray<cs.UInt8> {})
	@:overload(function(data:cs.system.io.Stream, hashAlgorithm:cs.system.security.cryptography.HashAlgorithmName):cs.NativeArray<cs.UInt8> {})
	/**
	 * Computes the hash value of a portion of the specified byte array using the
	 * specified hash algorithm and signs the resulting hash value.
	 * @param data The input data for which to compute the hash.
	 * @param offset The offset into the array at which to begin using data.
	 * @param count The number of bytes in the array to use as data.
	 * @param hashAlgorithm The hash algorithm to use to create the hash value.
	 * @return The ECDSA signature for the specified data.
	 */
	function SignData(data:cs.NativeArray<cs.UInt8>, offset:Int, count:Int, hashAlgorithm:cs.system.security.cryptography.HashAlgorithmName):cs.NativeArray<cs.UInt8>;
	/**
	 * Generates a digital signature for the specified hash value.
	 * @param hash The hash value of the data that is being signed.
	 * @return A digital signature that consists of the given hash value encrypted with
	 * the private key.
	 */
	function SignHash(hash:cs.NativeArray<cs.UInt8>):cs.NativeArray<cs.UInt8>;
	/**
	 * This method throws in all cases.
	 * @param includePrivateParameters to include private parameters; otherwise, .
	 * @return This method does not return a value.
	 */
	function ToXmlString(includePrivateParameters:Bool):String;
	function TryExportECPrivateKey(destination:cs.system.Span<cs.UInt8>, bytesWritten:cs.Ref<Int>):Bool;
	/**
	 * @param data 
	 * @param destination 
	 * @param hashAlgorithm 
	 * @param bytesWritten 
	 */
	function TrySignData(data:cs.system.ReadOnlySpan<cs.UInt8>, destination:cs.system.Span<cs.UInt8>, hashAlgorithm:cs.system.security.cryptography.HashAlgorithmName, bytesWritten:cs.Ref<Int>):Bool;
	/**
	 * @param hash 
	 * @param destination 
	 * @param bytesWritten 
	 */
	function TrySignHash(hash:cs.system.ReadOnlySpan<cs.UInt8>, destination:cs.system.Span<cs.UInt8>, bytesWritten:cs.Ref<Int>):Bool;
	@:overload(function(data:cs.NativeArray<cs.UInt8>, signature:cs.NativeArray<cs.UInt8>, hashAlgorithm:cs.system.security.cryptography.HashAlgorithmName):Bool {})
	@:overload(function(data:cs.system.io.Stream, signature:cs.NativeArray<cs.UInt8>, hashAlgorithm:cs.system.security.cryptography.HashAlgorithmName):Bool {})
	@:overload(function(data:cs.system.ReadOnlySpan<cs.UInt8>, signature:cs.system.ReadOnlySpan<cs.UInt8>, hashAlgorithm:cs.system.security.cryptography.HashAlgorithmName):Bool {})
	/**
	 * Verifies that a digital signature is valid by calculating the hash value of the
	 * specified data using the specified hash algorithm and comparing it to the
	 * provided signature.
	 * @param data The signed data.
	 * @param signature The signature data to be verified.
	 * @param hashAlgorithm The hash algorithm used to create the hash value of the
	 * data.
	 * @return if the signature is valid; otherwise, .
	 */
	function VerifyData(data:cs.NativeArray<cs.UInt8>, offset:Int, count:Int, signature:cs.NativeArray<cs.UInt8>, hashAlgorithm:cs.system.security.cryptography.HashAlgorithmName):Bool;
	@:overload(function(hash:cs.NativeArray<cs.UInt8>, signature:cs.NativeArray<cs.UInt8>):Bool {})
	/**
	 * Verifies a digital signature against the specified hash value.
	 * @param hash The hash value of a block of data.
	 * @param signature The digital signature to be verified.
	 * @return if the hash value equals the decrypted signature; otherwise, .
	 */
	function VerifyHash(hash:cs.system.ReadOnlySpan<cs.UInt8>, signature:cs.system.ReadOnlySpan<cs.UInt8>):Bool;
}
