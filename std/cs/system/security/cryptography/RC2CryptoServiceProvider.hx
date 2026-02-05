package cs.system.security.cryptography;

/** Defines a wrapper object to access the cryptographic service provider (CSP) implementation of the  algorithm. This class cannot be inherited. */
@:native("System.Security.Cryptography.RC2CryptoServiceProvider")
extern class RC2CryptoServiceProvider extends cs.system.security.cryptography.RC2 {
	/**
	 * Gets or sets a value that determines whether to create a key with an
	 * 11-byte-long, zero-value salt.
	 * @return if the key should be created with an 11-byte-long, zero-value salt;
	 * otherwise, . The default is .
	 */
	var UseSalt(default, default):Bool;
	function new():Void;
	/**
	 * Creates a symmetric  decryptor object with the specified key () and
	 * initialization vector ().
	 * @param rgbKey The secret key to use for the symmetric algorithm.
	 * @param rgbIV The initialization vector to use for the symmetric algorithm.
	 * @return A symmetric  decryptor object.
	 */
	function CreateDecryptor(rgbKey:cs.NativeArray<cs.UInt8>, rgbIV:cs.NativeArray<cs.UInt8>):cs.system.security.cryptography.ICryptoTransform;
	/**
	 * Creates a symmetric  encryptor object with the specified key () and
	 * initialization vector ().
	 * @param rgbKey The secret key to use for the symmetric algorithm.
	 * @param rgbIV The initialization vector to use for the symmetric algorithm.
	 * @return A symmetric  encryptor object.
	 */
	function CreateEncryptor(rgbKey:cs.NativeArray<cs.UInt8>, rgbIV:cs.NativeArray<cs.UInt8>):cs.system.security.cryptography.ICryptoTransform;
	/** Generates a random initialization vector () to use for the algorithm. */
	function GenerateIV():Void;
	/** Generates a random key () to be used for the algorithm. */
	function GenerateKey():Void;
}
