package cs.system.security.cryptography;

/** Performs asymmetric encryption and decryption using the implementation of the  algorithm provided by the cryptographic service provider (CSP). This class cannot be inherited. */
@:native("System.Security.Cryptography.RSACryptoServiceProvider")
extern class RSACryptoServiceProvider extends cs.system.security.cryptography.RSA {
	/**
	 * Gets or sets a value indicating whether the key should be persisted in the
	 * computer's key store instead of the user profile store.
	 * @return if the key should be persisted in the computer key store; otherwise, .
	 */
	static var UseMachineKeyStore(default, default):Bool;
	/**
	 * Gets a  object that describes additional information about a cryptographic key
	 * pair.
	 * @return A  object that describes additional information about a cryptographic
	 * key pair.
	 */
	var CspKeyContainerInfo(default, never):cs.system.security.cryptography.CspKeyContainerInfo;
	/**
	 * Gets or sets a value indicating whether the key should be persisted in the
	 * cryptographic service provider (CSP).
	 * @return if the key should be persisted in the CSP; otherwise, .
	 */
	var PersistKeyInCsp(default, default):Bool;
	/**
	 * Gets a value that indicates whether the  object contains only a public key.
	 * @return if the  object contains only a public key; otherwise, .
	 */
	var PublicOnly(default, never):Bool;
	@:overload(function():Void {})
	@:overload(function(dwKeySize:Int):Void {})
	@:overload(function(parameters:cs.system.security.cryptography.CspParameters):Void {})
	function new(dwKeySize:Int, parameters:cs.system.security.cryptography.CspParameters):Void;
	@:overload(function(rgb:cs.NativeArray<cs.UInt8>, fOAEP:Bool):cs.NativeArray<cs.UInt8> {})
	/**
	 * Decrypts data with the  algorithm.
	 * @param rgb The data to be decrypted.
	 * @param fOAEP to perform direct  decryption using OAEP padding (only available on
	 * a computer running Microsoft Windows XP or later); otherwise,  to use PKCS#1
	 * v1.5 padding.
	 * @return The decrypted data, which is the original plain text before encryption.
	 */
	function Decrypt(data:cs.NativeArray<cs.UInt8>, padding:cs.system.security.cryptography.RSAEncryptionPadding):cs.NativeArray<cs.UInt8>;
	/**
	 * This method is not supported in the current version.
	 * @param rgb The data to be decrypted.
	 * @return The decrypted data, which is the original plain text before encryption.
	 */
	function DecryptValue(rgb:cs.NativeArray<cs.UInt8>):cs.NativeArray<cs.UInt8>;
	@:overload(function(rgb:cs.NativeArray<cs.UInt8>, fOAEP:Bool):cs.NativeArray<cs.UInt8> {})
	/**
	 * Encrypts data with the  algorithm.
	 * @param rgb The data to be encrypted.
	 * @param fOAEP to perform direct  encryption using OAEP padding (only available on
	 * a computer running Windows XP or later); otherwise,  to use PKCS#1 v1.5 padding.
	 * @return The encrypted data.
	 */
	function Encrypt(data:cs.NativeArray<cs.UInt8>, padding:cs.system.security.cryptography.RSAEncryptionPadding):cs.NativeArray<cs.UInt8>;
	/**
	 * This method is not supported in the current version.
	 * @param rgb The data to be encrypted.
	 * @return The encrypted data.
	 */
	function EncryptValue(rgb:cs.NativeArray<cs.UInt8>):cs.NativeArray<cs.UInt8>;
	/**
	 * Exports a blob containing the key information associated with an  object.
	 * @param includePrivateParameters to include the private key; otherwise, .
	 * @return A byte array containing the key information associated with an  object.
	 */
	function ExportCspBlob(includePrivateParameters:Bool):cs.NativeArray<cs.UInt8>;
	/**
	 * Exports the .
	 * @param includePrivateParameters to include private parameters; otherwise, .
	 * @return The parameters for .
	 */
	function ExportParameters(includePrivateParameters:Bool):cs.system.security.cryptography.RSAParameters;
	/**
	 * Imports a blob that represents RSA key information.
	 * @param keyBlob A byte array that represents an RSA key blob.
	 */
	function ImportCspBlob(keyBlob:cs.NativeArray<cs.UInt8>):Void;
	/**
	 * Imports the specified .
	 * @param parameters The parameters for .
	 */
	function ImportParameters(parameters:cs.system.security.cryptography.RSAParameters):Void;
	@:overload(function(buffer:cs.NativeArray<cs.UInt8>, halg:Dynamic):cs.NativeArray<cs.UInt8> {})
	@:overload(function(inputStream:cs.system.io.Stream, halg:Dynamic):cs.NativeArray<cs.UInt8> {})
	/**
	 * Computes the hash value of a subset of the specified byte array using the
	 * specified hash algorithm, and signs the resulting hash value.
	 * @param buffer The input data for which to compute the hash.
	 * @param offset The offset into the array from which to begin using data.
	 * @param count The number of bytes in the array to use as data.
	 * @param halg The hash algorithm to use to create the hash value.
	 * @return The  signature for the specified data.
	 */
	function SignData(buffer:cs.NativeArray<cs.UInt8>, offset:Int, count:Int, halg:Dynamic):cs.NativeArray<cs.UInt8>;
	@:overload(function(rgbHash:cs.NativeArray<cs.UInt8>, str:String):cs.NativeArray<cs.UInt8> {})
	/**
	 * Computes the signature for the specified hash value by encrypting it with the
	 * private key using the specified padding.
	 * @param hash The hash value of the data to be signed.
	 * @param hashAlgorithm The hash algorithm name used to create the hash value of
	 * the data.
	 * @param padding The padding.
	 * @return The  signature for the specified hash value.
	 */
	function SignHash(hash:cs.NativeArray<cs.UInt8>, hashAlgorithm:cs.system.security.cryptography.HashAlgorithmName, padding:cs.system.security.cryptography.RSASignaturePadding):cs.NativeArray<cs.UInt8>;
	/**
	 * Verifies that a digital signature is valid by determining the hash value in the
	 * signature using the provided public key and comparing it to the hash value of
	 * the provided data.
	 * @param buffer The data that was signed.
	 * @param halg The name of the hash algorithm used to create the hash value of the
	 * data.
	 * @param signature The signature data to be verified.
	 * @return if the signature is valid; otherwise, .
	 */
	function VerifyData(buffer:cs.NativeArray<cs.UInt8>, halg:Dynamic, signature:cs.NativeArray<cs.UInt8>):Bool;
	@:overload(function(rgbHash:cs.NativeArray<cs.UInt8>, str:String, rgbSignature:cs.NativeArray<cs.UInt8>):Bool {})
	/**
	 * Verifies that a digital signature is valid by determining the hash value in the
	 * signature using the specified hashing algorithm and padding, and comparing it to
	 * the provided hash value.
	 * @param hash The hash value of the signed data.
	 * @param signature The signature data to be verified.
	 * @param hashAlgorithm The hash algorithm name used to create the hash value.
	 * @param padding The padding.
	 * @return if the signature is valid; otherwise, .
	 */
	function VerifyHash(hash:cs.NativeArray<cs.UInt8>, signature:cs.NativeArray<cs.UInt8>, hashAlgorithm:cs.system.security.cryptography.HashAlgorithmName, padding:cs.system.security.cryptography.RSASignaturePadding):Bool;
}
