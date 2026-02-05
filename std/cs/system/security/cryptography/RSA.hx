package cs.system.security.cryptography;

/** Represents the base class from which all implementations of the  algorithm inherit. */
@:native("System.Security.Cryptography.RSA")
extern class RSA extends cs.system.security.cryptography.AsymmetricAlgorithm {
	@:overload(function():cs.system.security.cryptography.RSA {})
	@:overload(function(keySizeInBits:Int):cs.system.security.cryptography.RSA {})
	@:overload(function(parameters:cs.system.security.cryptography.RSAParameters):cs.system.security.cryptography.RSA {})
	/**
	 * Creates an instance of the default implementation of the  algorithm.
	 * @return A new instance of the default implementation of .
	 */
	static function Create(algName:String):cs.system.security.cryptography.RSA;
	/**
	 * When overridden in a derived class, decrypts the input data using the specified
	 * padding mode.
	 * @param data The data to decrypt.
	 * @param padding The padding mode.
	 * @return The decrypted data.
	 */
	function Decrypt(data:cs.NativeArray<cs.UInt8>, padding:cs.system.security.cryptography.RSAEncryptionPadding):cs.NativeArray<cs.UInt8>;
	/**
	 * When overridden in a derived class, decrypts the input data using the private
	 * key.
	 * @param rgb The cipher text to be decrypted.
	 * @return The resulting decryption of the  parameter in plain text.
	 */
	function DecryptValue(rgb:cs.NativeArray<cs.UInt8>):cs.NativeArray<cs.UInt8>;
	/**
	 * When overridden in a derived class, encrypts the input data using the specified
	 * padding mode.
	 * @param data The data to encrypt.
	 * @param padding The padding mode.
	 * @return The encrypted data.
	 */
	function Encrypt(data:cs.NativeArray<cs.UInt8>, padding:cs.system.security.cryptography.RSAEncryptionPadding):cs.NativeArray<cs.UInt8>;
	/**
	 * When overridden in a derived class, encrypts the input data using the public
	 * key.
	 * @param rgb The plain text to be encrypted.
	 * @return The resulting encryption of the  parameter as cipher text.
	 */
	function EncryptValue(rgb:cs.NativeArray<cs.UInt8>):cs.NativeArray<cs.UInt8>;
	/**
	 * When overridden in a derived class, exports the .
	 * @param includePrivateParameters to include private parameters; otherwise, .
	 * @return The parameters for .
	 */
	function ExportParameters(includePrivateParameters:Bool):cs.system.security.cryptography.RSAParameters;
	function ExportRSAPrivateKey():cs.NativeArray<cs.UInt8>;
	function ExportRSAPublicKey():cs.NativeArray<cs.UInt8>;
	/**
	 * Initializes an  object from the key information from an XML string.
	 * @param xmlString The XML string containing  key information.
	 */
	function FromXmlString(xmlString:String):Void;
	/**
	 * When overridden in a derived class, imports the specified .
	 * @param parameters The parameters for .
	 */
	function ImportParameters(parameters:cs.system.security.cryptography.RSAParameters):Void;
	function ImportRSAPrivateKey(source:cs.system.ReadOnlySpan<cs.UInt8>, bytesRead:cs.Ref<Int>):Void;
	function ImportRSAPublicKey(source:cs.system.ReadOnlySpan<cs.UInt8>, bytesRead:cs.Ref<Int>):Void;
	@:overload(function(data:cs.NativeArray<cs.UInt8>, hashAlgorithm:cs.system.security.cryptography.HashAlgorithmName, padding:cs.system.security.cryptography.RSASignaturePadding):cs.NativeArray<cs.UInt8> {})
	@:overload(function(data:cs.system.io.Stream, hashAlgorithm:cs.system.security.cryptography.HashAlgorithmName, padding:cs.system.security.cryptography.RSASignaturePadding):cs.NativeArray<cs.UInt8> {})
	/**
	 * Computes the hash value of a portion of the specified byte array using the
	 * specified hash algorithm and padding mode, and signs the resulting hash value.
	 * @param data The input data for which to compute the hash.
	 * @param offset The offset into the array at which to begin using data.
	 * @param count The number of bytes in the array to use as data.
	 * @param hashAlgorithm The hash algorithm to use to create the hash value.
	 * @param padding The padding mode.
	 * @return The RSA signature for the specified data.
	 */
	function SignData(data:cs.NativeArray<cs.UInt8>, offset:Int, count:Int, hashAlgorithm:cs.system.security.cryptography.HashAlgorithmName, padding:cs.system.security.cryptography.RSASignaturePadding):cs.NativeArray<cs.UInt8>;
	/**
	 * When overridden in a derived class, computes the signature for the specified
	 * hash value by encrypting it with the private key using the specified padding.
	 * @param hash The hash value of the data to be signed.
	 * @param hashAlgorithm The hash algorithm used to create the hash value of the
	 * data.
	 * @param padding The padding.
	 * @return The RSA signature for the specified hash value.
	 */
	function SignHash(hash:cs.NativeArray<cs.UInt8>, hashAlgorithm:cs.system.security.cryptography.HashAlgorithmName, padding:cs.system.security.cryptography.RSASignaturePadding):cs.NativeArray<cs.UInt8>;
	/**
	 * Creates and returns an XML string containing the key of the current  object.
	 * @param includePrivateParameters to include a public and private RSA key;  to
	 * include only the public key.
	 * @return An XML string containing the key of the current  object.
	 */
	function ToXmlString(includePrivateParameters:Bool):String;
	/**
	 * @param data 
	 * @param destination 
	 * @param padding 
	 * @param bytesWritten 
	 */
	function TryDecrypt(data:cs.system.ReadOnlySpan<cs.UInt8>, destination:cs.system.Span<cs.UInt8>, padding:cs.system.security.cryptography.RSAEncryptionPadding, bytesWritten:cs.Ref<Int>):Bool;
	/**
	 * @param data 
	 * @param destination 
	 * @param padding 
	 * @param bytesWritten 
	 */
	function TryEncrypt(data:cs.system.ReadOnlySpan<cs.UInt8>, destination:cs.system.Span<cs.UInt8>, padding:cs.system.security.cryptography.RSAEncryptionPadding, bytesWritten:cs.Ref<Int>):Bool;
	function TryExportRSAPrivateKey(destination:cs.system.Span<cs.UInt8>, bytesWritten:cs.Ref<Int>):Bool;
	function TryExportRSAPublicKey(destination:cs.system.Span<cs.UInt8>, bytesWritten:cs.Ref<Int>):Bool;
	/**
	 * @param data 
	 * @param destination 
	 * @param hashAlgorithm 
	 * @param padding 
	 * @param bytesWritten 
	 */
	function TrySignData(data:cs.system.ReadOnlySpan<cs.UInt8>, destination:cs.system.Span<cs.UInt8>, hashAlgorithm:cs.system.security.cryptography.HashAlgorithmName, padding:cs.system.security.cryptography.RSASignaturePadding, bytesWritten:cs.Ref<Int>):Bool;
	/**
	 * @param hash 
	 * @param destination 
	 * @param hashAlgorithm 
	 * @param padding 
	 * @param bytesWritten 
	 */
	function TrySignHash(hash:cs.system.ReadOnlySpan<cs.UInt8>, destination:cs.system.Span<cs.UInt8>, hashAlgorithm:cs.system.security.cryptography.HashAlgorithmName, padding:cs.system.security.cryptography.RSASignaturePadding, bytesWritten:cs.Ref<Int>):Bool;
	@:overload(function(data:cs.NativeArray<cs.UInt8>, signature:cs.NativeArray<cs.UInt8>, hashAlgorithm:cs.system.security.cryptography.HashAlgorithmName, padding:cs.system.security.cryptography.RSASignaturePadding):Bool {})
	@:overload(function(data:cs.system.io.Stream, signature:cs.NativeArray<cs.UInt8>, hashAlgorithm:cs.system.security.cryptography.HashAlgorithmName, padding:cs.system.security.cryptography.RSASignaturePadding):Bool {})
	@:overload(function(data:cs.system.ReadOnlySpan<cs.UInt8>, signature:cs.system.ReadOnlySpan<cs.UInt8>, hashAlgorithm:cs.system.security.cryptography.HashAlgorithmName, padding:cs.system.security.cryptography.RSASignaturePadding):Bool {})
	/**
	 * Verifies that a digital signature is valid by calculating the hash value of the
	 * specified data using the specified hash algorithm and padding, and comparing it
	 * to the provided signature.
	 * @param data The signed data.
	 * @param signature The signature data to be verified.
	 * @param hashAlgorithm The hash algorithm used to create the hash value of the
	 * data.
	 * @param padding The padding mode.
	 * @return if the signature is valid; otherwise, .
	 */
	function VerifyData(data:cs.NativeArray<cs.UInt8>, offset:Int, count:Int, signature:cs.NativeArray<cs.UInt8>, hashAlgorithm:cs.system.security.cryptography.HashAlgorithmName, padding:cs.system.security.cryptography.RSASignaturePadding):Bool;
	@:overload(function(hash:cs.NativeArray<cs.UInt8>, signature:cs.NativeArray<cs.UInt8>, hashAlgorithm:cs.system.security.cryptography.HashAlgorithmName, padding:cs.system.security.cryptography.RSASignaturePadding):Bool {})
	/**
	 * Verifies that a digital signature is valid by determining the hash value in the
	 * signature using the specified hash algorithm and padding, and comparing it to
	 * the provided hash value.
	 * @param hash The hash value of the signed data.
	 * @param signature The signature data to be verified.
	 * @param hashAlgorithm The hash algorithm used to create the hash value.
	 * @param padding The padding mode.
	 * @return if the signature is valid; otherwise, .
	 */
	function VerifyHash(hash:cs.system.ReadOnlySpan<cs.UInt8>, signature:cs.system.ReadOnlySpan<cs.UInt8>, hashAlgorithm:cs.system.security.cryptography.HashAlgorithmName, padding:cs.system.security.cryptography.RSASignaturePadding):Bool;
}
