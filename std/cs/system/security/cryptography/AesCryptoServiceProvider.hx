package cs.system.security.cryptography;

/** Performs symmetric encryption and decryption using the Cryptographic Application Programming Interfaces (CAPI) implementation of the Advanced Encryption Standard (AES) algorithm. */
@:native("System.Security.Cryptography.AesCryptoServiceProvider")
extern class AesCryptoServiceProvider extends cs.system.security.cryptography.Aes {
	function new():Void;
	@:overload(function():cs.system.security.cryptography.ICryptoTransform {})
	/**
	 * Creates a symmetric AES decryptor object using the current key and
	 * initialization vector (IV).
	 * @return A symmetric AES decryptor object.
	 */
	function CreateDecryptor(rgbKey:cs.NativeArray<cs.UInt8>, rgbIV:cs.NativeArray<cs.UInt8>):cs.system.security.cryptography.ICryptoTransform;
	@:overload(function():cs.system.security.cryptography.ICryptoTransform {})
	/**
	 * Creates a symmetric AES encryptor object using the current key and
	 * initialization vector (IV).
	 * @return A symmetric AES encryptor object.
	 */
	function CreateEncryptor(rgbKey:cs.NativeArray<cs.UInt8>, rgbIV:cs.NativeArray<cs.UInt8>):cs.system.security.cryptography.ICryptoTransform;
	/** Generates a random initialization vector (IV) to use for the algorithm. */
	function GenerateIV():Void;
	/** Generates a random key to use for the algorithm. */
	function GenerateKey():Void;
}
