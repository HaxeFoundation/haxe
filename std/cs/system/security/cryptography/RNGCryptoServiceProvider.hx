package cs.system.security.cryptography;

/** Implements a cryptographic Random Number Generator (RNG) using the implementation provided by the cryptographic service provider (CSP). This class cannot be inherited. */
@:native("System.Security.Cryptography.RNGCryptoServiceProvider")
extern class RNGCryptoServiceProvider extends cs.system.security.cryptography.RandomNumberGenerator {
	@:overload(function():Void {})
	@:overload(function(rgb:cs.NativeArray<cs.UInt8>):Void {})
	@:overload(function(cspParams:cs.system.security.cryptography.CspParameters):Void {})
	function new(str:String):Void;
	@:overload(function(data:cs.NativeArray<cs.UInt8>):Void {})
	@:overload(function(data:cs.system.Span<cs.UInt8>):Void {})
	/**
	 * Fills an array of bytes with a cryptographically strong sequence of random
	 * values.
	 * @param data The array to fill with a cryptographically strong sequence of random
	 * values.
	 */
	function GetBytes(data:cs.NativeArray<cs.UInt8>, offset:Int, count:Int):Void;
	@:overload(function(data:cs.NativeArray<cs.UInt8>):Void {})
	/**
	 * Fills an array of bytes with a cryptographically strong sequence of random
	 * nonzero values.
	 * @param data The array to fill with a cryptographically strong sequence of random
	 * nonzero values.
	 */
	function GetNonZeroBytes(data:cs.system.Span<cs.UInt8>):Void;
}
