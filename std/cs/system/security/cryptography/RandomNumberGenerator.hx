package cs.system.security.cryptography;

/** Represents the abstract class from which all implementations of cryptographic random number generators derive. */
@:native("System.Security.Cryptography.RandomNumberGenerator")
extern class RandomNumberGenerator {
	@:overload(function():cs.system.security.cryptography.RandomNumberGenerator {})
	/**
	 * Creates an instance of the default implementation of a cryptographic random
	 * number generator that can be used to generate random data.
	 * @return A new instance of a cryptographic random number generator.
	 */
	static function Create(rngName:String):cs.system.security.cryptography.RandomNumberGenerator;
	/**
	 * Fills a span with cryptographically strong random bytes.
	 * @param data The span to fill with cryptographically strong random bytes.
	 */
	static function Fill(data:cs.system.Span<cs.UInt8>):Void;
	@:overload(function(toExclusive:Int):Int {})
	/**
	 * Generates a random integer between 0 (inclusive) and a specified exclusive upper
	 * bound using a cryptographically strong random number generator.
	 * @param toExclusive The exclusive upper bound of the random range.
	 * @return A random integer between 0 (inclusive) and  (exclusive).
	 */
	static function GetInt32(fromInclusive:Int, toExclusive:Int):Int;
	/** When overridden in a derived class, releases all resources used by the current instance of the  class. */
	function Dispose():Void;
	@:overload(function(data:cs.NativeArray<cs.UInt8>):Void {})
	@:overload(function(data:cs.system.Span<cs.UInt8>):Void {})
	/**
	 * When overridden in a derived class, fills an array of bytes with a
	 * cryptographically strong random sequence of values.
	 * @param data The array to fill with cryptographically strong random bytes.
	 */
	function GetBytes(data:cs.NativeArray<cs.UInt8>, offset:Int, count:Int):Void;
	@:overload(function(data:cs.NativeArray<cs.UInt8>):Void {})
	/**
	 * When overridden in a derived class, fills an array of bytes with a
	 * cryptographically strong random sequence of nonzero values.
	 * @param data The array to fill with cryptographically strong random nonzero
	 * bytes.
	 */
	function GetNonZeroBytes(data:cs.system.Span<cs.UInt8>):Void;
}
