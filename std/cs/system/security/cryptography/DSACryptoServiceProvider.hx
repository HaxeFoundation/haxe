package cs.system.security.cryptography;

/** Defines a wrapper object to access the cryptographic service provider (CSP) implementation of the  algorithm. This class cannot be inherited. */
@:native("System.Security.Cryptography.DSACryptoServiceProvider")
extern class DSACryptoServiceProvider extends cs.system.security.cryptography.DSA {
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
	/**
	 * Creates the  signature for the specified data.
	 * @param rgbHash The data to be signed.
	 * @return The digital signature for the specified data.
	 */
	function CreateSignature(rgbHash:cs.NativeArray<cs.UInt8>):cs.NativeArray<cs.UInt8>;
	/**
	 * Exports a blob containing the key information associated with a  object.
	 * @param includePrivateParameters to include the private key; otherwise, .
	 * @return A byte array containing the key information associated with a  object.
	 */
	function ExportCspBlob(includePrivateParameters:Bool):cs.NativeArray<cs.UInt8>;
	/**
	 * Exports the .
	 * @param includePrivateParameters to include private parameters; otherwise, .
	 * @return The parameters for .
	 */
	function ExportParameters(includePrivateParameters:Bool):cs.system.security.cryptography.DSAParameters;
	/**
	 * Imports a blob that represents DSA key information.
	 * @param keyBlob A byte array that represents a DSA key blob.
	 */
	function ImportCspBlob(keyBlob:cs.NativeArray<cs.UInt8>):Void;
	/**
	 * Imports the specified .
	 * @param parameters The parameters for .
	 */
	function ImportParameters(parameters:cs.system.security.cryptography.DSAParameters):Void;
	@:overload(function(buffer:cs.NativeArray<cs.UInt8>):cs.NativeArray<cs.UInt8> {})
	@:overload(function(inputStream:cs.system.io.Stream):cs.NativeArray<cs.UInt8> {})
	/**
	 * Computes the hash value of the specified byte array and signs the resulting hash
	 * value.
	 * @param buffer The input data for which to compute the hash.
	 * @return The  signature for the specified data.
	 */
	function SignData(buffer:cs.NativeArray<cs.UInt8>, offset:Int, count:Int):cs.NativeArray<cs.UInt8>;
	/**
	 * Computes the signature for the specified hash value by encrypting it with the
	 * private key.
	 * @param rgbHash The hash value of the data to be signed.
	 * @param str The name of the hash algorithm used to create the hash value of the
	 * data.
	 * @return The  signature for the specified hash value.
	 */
	function SignHash(rgbHash:cs.NativeArray<cs.UInt8>, str:String):cs.NativeArray<cs.UInt8>;
	/**
	 * Verifies the specified signature data by comparing it to the signature computed
	 * for the specified data.
	 * @param rgbData The data that was signed.
	 * @param rgbSignature The signature data to be verified.
	 * @return if the signature verifies as valid; otherwise, .
	 */
	function VerifyData(rgbData:cs.NativeArray<cs.UInt8>, rgbSignature:cs.NativeArray<cs.UInt8>):Bool;
	/**
	 * Verifies the specified signature data by comparing it to the signature computed
	 * for the specified hash value.
	 * @param rgbHash The hash value of the data to be signed.
	 * @param str The name of the hash algorithm used to create the hash value of the
	 * data.
	 * @param rgbSignature The signature data to be verified.
	 * @return if the signature verifies as valid; otherwise, .
	 */
	function VerifyHash(rgbHash:cs.NativeArray<cs.UInt8>, str:String, rgbSignature:cs.NativeArray<cs.UInt8>):Bool;
	/**
	 * Verifies the  signature for the specified data.
	 * @param rgbHash The data signed with .
	 * @param rgbSignature The signature to be verified for .
	 * @return if  matches the signature computed using the specified hash algorithm
	 * and key on ; otherwise, .
	 */
	function VerifySignature(rgbHash:cs.NativeArray<cs.UInt8>, rgbSignature:cs.NativeArray<cs.UInt8>):Bool;
}
