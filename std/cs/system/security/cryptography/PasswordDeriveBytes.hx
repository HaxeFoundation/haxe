package cs.system.security.cryptography;

/** Derives a key from a password using an extension of the PBKDF1 algorithm. */
@:native("System.Security.Cryptography.PasswordDeriveBytes")
extern class PasswordDeriveBytes extends cs.system.security.cryptography.DeriveBytes {
	/**
	 * Gets or sets the name of the hash algorithm for the operation.
	 * @return The name of the hash algorithm for the operation.
	 */
	var HashName(default, default):String;
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
	@:overload(function(password:cs.NativeArray<cs.UInt8>, salt:cs.NativeArray<cs.UInt8>):Void {})
	@:overload(function(strPassword:String, rgbSalt:cs.NativeArray<cs.UInt8>):Void {})
	@:overload(function(password:cs.NativeArray<cs.UInt8>, salt:cs.NativeArray<cs.UInt8>, cspParams:cs.system.security.cryptography.CspParameters):Void {})
	@:overload(function(strPassword:String, rgbSalt:cs.NativeArray<cs.UInt8>, cspParams:cs.system.security.cryptography.CspParameters):Void {})
	@:overload(function(password:cs.NativeArray<cs.UInt8>, salt:cs.NativeArray<cs.UInt8>, hashName:String, iterations:Int):Void {})
	@:overload(function(strPassword:String, rgbSalt:cs.NativeArray<cs.UInt8>, strHashName:String, iterations:Int):Void {})
	@:overload(function(password:cs.NativeArray<cs.UInt8>, salt:cs.NativeArray<cs.UInt8>, hashName:String, iterations:Int, cspParams:cs.system.security.cryptography.CspParameters):Void {})
	function new(strPassword:String, rgbSalt:cs.NativeArray<cs.UInt8>, strHashName:String, iterations:Int, cspParams:cs.system.security.cryptography.CspParameters):Void;
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
	 * Returns pseudo-random key bytes.
	 * @param cb The number of pseudo-random key bytes to generate.
	 * @return A byte array filled with pseudo-random key bytes.
	 */
	function GetBytes(cb:Int):cs.NativeArray<cs.UInt8>;
	/** Resets the state of the operation. */
	function Reset():Void;
}
