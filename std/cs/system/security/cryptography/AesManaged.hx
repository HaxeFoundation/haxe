package cs.system.security.cryptography;

/** Provides a managed implementation of the Advanced Encryption Standard (AES) symmetric algorithm. */
@:native("System.Security.Cryptography.AesManaged")
extern class AesManaged extends cs.system.security.cryptography.Aes {
	function new():Void;
	@:overload(function():cs.system.security.cryptography.ICryptoTransform {})
	/**
	 * Creates a symmetric decryptor object using the current key and initialization
	 * vector (IV).
	 * @return A symmetric decryptor object.
	 */
	function CreateDecryptor(key:cs.NativeArray<cs.UInt8>, iv:cs.NativeArray<cs.UInt8>):cs.system.security.cryptography.ICryptoTransform;
	@:overload(function():cs.system.security.cryptography.ICryptoTransform {})
	/**
	 * Creates a symmetric encryptor object using the current key and initialization
	 * vector (IV).
	 * @return A symmetric encryptor object.
	 */
	function CreateEncryptor(key:cs.NativeArray<cs.UInt8>, iv:cs.NativeArray<cs.UInt8>):cs.system.security.cryptography.ICryptoTransform;
	/** Generates a random initialization vector (IV) to use for the symmetric algorithm. */
	function GenerateIV():Void;
	/** Generates a random key to use for the symmetric algorithm. */
	function GenerateKey():Void;
}
