package cs.system.security.cryptography;

/** Represents the abstract base class from which all implementations of the Digital Signature Algorithm () must inherit. */
@:native("System.Security.Cryptography.DSA")
extern class DSA extends cs.system.security.cryptography.AsymmetricAlgorithm {
	@:overload(function():cs.system.security.cryptography.DSA {})
	@:overload(function(keySizeInBits:Int):cs.system.security.cryptography.DSA {})
	@:overload(function(parameters:cs.system.security.cryptography.DSAParameters):cs.system.security.cryptography.DSA {})
	/**
	 * Creates the default cryptographic object used to perform the asymmetric
	 * algorithm.
	 * @return A cryptographic object used to perform the asymmetric algorithm.
	 */
	static function Create(algName:String):cs.system.security.cryptography.DSA;
	/**
	 * When overridden in a derived class, creates the  signature for the specified
	 * data.
	 * @param rgbHash The data to be signed.
	 * @return The digital signature for the specified data.
	 */
	function CreateSignature(rgbHash:cs.NativeArray<cs.UInt8>):cs.NativeArray<cs.UInt8>;
	/**
	 * When overridden in a derived class, exports the .
	 * @param includePrivateParameters to include private parameters; otherwise, .
	 * @return The parameters for .
	 */
	function ExportParameters(includePrivateParameters:Bool):cs.system.security.cryptography.DSAParameters;
	/**
	 * Reconstructs a  object from an XML string.
	 * @param xmlString The XML string to use to reconstruct the  object.
	 */
	function FromXmlString(xmlString:String):Void;
	/**
	 * When overridden in a derived class, imports the specified .
	 * @param parameters The parameters for .
	 */
	function ImportParameters(parameters:cs.system.security.cryptography.DSAParameters):Void;
	@:overload(function(data:cs.NativeArray<cs.UInt8>, hashAlgorithm:cs.system.security.cryptography.HashAlgorithmName):cs.NativeArray<cs.UInt8> {})
	@:overload(function(data:cs.system.io.Stream, hashAlgorithm:cs.system.security.cryptography.HashAlgorithmName):cs.NativeArray<cs.UInt8> {})
	/**
	 * Computes the hash value of a portion of the specified byte array using the
	 * specified hash algorithm and signs the resulting hash value.
	 * @param data The input data for which to compute the hash.
	 * @param offset The offset into the array at which to begin using data.
	 * @param count The number of bytes in the array to use as data.
	 * @param hashAlgorithm The hash algorithm to use to create the hash value.
	 * @return The DSA signature for the specified data.
	 */
	function SignData(data:cs.NativeArray<cs.UInt8>, offset:Int, count:Int, hashAlgorithm:cs.system.security.cryptography.HashAlgorithmName):cs.NativeArray<cs.UInt8>;
	/**
	 * Creates and returns an XML string representation of the current  object.
	 * @param includePrivateParameters to include private parameters; otherwise, .
	 * @return An XML string encoding of the current  object.
	 */
	function ToXmlString(includePrivateParameters:Bool):String;
	/**
	 * Attempts to create the DSA signature for the specified hash into the provided
	 * buffer.
	 * @param hash The hash to sign.
	 * @param destination The byte span to receive the signature.
	 * @param bytesWritten When this method returns, contains a value that indicates
	 * the number of bytes written to .
	 * @return if  is large enough to receive the result; otherwise, .
	 */
	function TryCreateSignature(hash:cs.system.ReadOnlySpan<cs.UInt8>, destination:cs.system.Span<cs.UInt8>, bytesWritten:cs.Ref<Int>):Bool;
	/**
	 * Attempts to create the DSA signature for the specified data into the provided
	 * buffer.
	 * @param data The data to hash and sign.
	 * @param destination The byte span to receive the signature.
	 * @param hashAlgorithm The name of the hash algorithm to use.
	 * @param bytesWritten When this method returns, contains a value that indicates
	 * the number of bytes written to .
	 * @return if  is large enough to receive the result; otherwise, .
	 */
	function TrySignData(data:cs.system.ReadOnlySpan<cs.UInt8>, destination:cs.system.Span<cs.UInt8>, hashAlgorithm:cs.system.security.cryptography.HashAlgorithmName, bytesWritten:cs.Ref<Int>):Bool;
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
	 * @return if the digital signature is valid; otherwise, .
	 */
	function VerifyData(data:cs.NativeArray<cs.UInt8>, offset:Int, count:Int, signature:cs.NativeArray<cs.UInt8>, hashAlgorithm:cs.system.security.cryptography.HashAlgorithmName):Bool;
	@:overload(function(rgbHash:cs.NativeArray<cs.UInt8>, rgbSignature:cs.NativeArray<cs.UInt8>):Bool {})
	/**
	 * When overridden in a derived class, verifies the  signature for the specified
	 * data.
	 * @param rgbHash The hash of the data signed with .
	 * @param rgbSignature The signature to be verified for rgbData.
	 * @return if  matches the signature computed using the specified hash algorithm
	 * and key on ; otherwise, .
	 */
	function VerifySignature(hash:cs.system.ReadOnlySpan<cs.UInt8>, signature:cs.system.ReadOnlySpan<cs.UInt8>):Bool;
}
