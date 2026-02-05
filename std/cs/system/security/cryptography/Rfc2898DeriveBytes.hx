package cs.system.security.cryptography;

/** Implements password-based key derivation functionality, PBKDF2, by using a pseudo-random number generator based on . */
@:native("System.Security.Cryptography.Rfc2898DeriveBytes")
extern class Rfc2898DeriveBytes extends cs.system.security.cryptography.DeriveBytes {
	var HashAlgorithm(default, never):cs.system.security.cryptography.HashAlgorithmName;
	/**
	 * Gets or sets the number of iterations for the operation.
	 * @return The number of iterations for the operation.
	 */
	var IterationCount(default, default):Int;
	/**
	 * Gets or sets the key salt value for the operation.
	 * @return The key salt value for the operation.
	 */
	var Salt(default, default):cs.NativeArray<cs.UInt8>;
	@:overload(function(password:String, salt:cs.NativeArray<cs.UInt8>):Void {})
	@:overload(function(password:String, saltSize:Int):Void {})
	@:overload(function(password:cs.NativeArray<cs.UInt8>, salt:cs.NativeArray<cs.UInt8>, iterations:Int):Void {})
	@:overload(function(password:String, salt:cs.NativeArray<cs.UInt8>, iterations:Int):Void {})
	@:overload(function(password:String, saltSize:Int, iterations:Int):Void {})
	@:overload(function(password:cs.NativeArray<cs.UInt8>, salt:cs.NativeArray<cs.UInt8>, iterations:Int, hashAlgorithm:cs.system.security.cryptography.HashAlgorithmName):Void {})
	@:overload(function(password:String, salt:cs.NativeArray<cs.UInt8>, iterations:Int, hashAlgorithm:cs.system.security.cryptography.HashAlgorithmName):Void {})
	function new(password:String, saltSize:Int, iterations:Int, hashAlgorithm:cs.system.security.cryptography.HashAlgorithmName):Void;
	/**
	 * Derives a cryptographic key from the  object.
	 * @param algname The algorithm name for which to derive the key.
	 * @param alghashname The hash algorithm name to use to derive the key.
	 * @param keySize The size of the key, in bits, to derive.
	 * @param rgbIV The initialization vector (IV) to use to derive the key.
	 * @return The derived key.
	 */
	function CryptDeriveKey(algname:String, alghashname:String, keySize:Int, rgbIV:cs.NativeArray<cs.UInt8>):cs.NativeArray<cs.UInt8>;
	/**
	 * Returns the pseudo-random key for this object.
	 * @param cb The number of pseudo-random key bytes to generate.
	 * @return A byte array filled with pseudo-random key bytes.
	 */
	function GetBytes(cb:Int):cs.NativeArray<cs.UInt8>;
	/** Resets the state of the operation. */
	function Reset():Void;
}
