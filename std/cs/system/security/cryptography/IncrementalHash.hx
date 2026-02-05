package cs.system.security.cryptography;

/** Provides support for computing a hash or Hash-based Message Authentication Code (HMAC) value incrementally across several segments. */
@:native("System.Security.Cryptography.IncrementalHash")
extern class IncrementalHash {
	/**
	 * Gets the name of the algorithm being performed. HMAC algorithms are prepended
	 * with "HMAC" to distinguish them from an unkeyed digest.
	 * @return The name of the algorithm being performed.
	 */
	var AlgorithmName(default, never):cs.system.security.cryptography.HashAlgorithmName;
	/**
	 * Creates an  for the specified algorithm.
	 * @param hashAlgorithm The name of the hash algorithm to perform.
	 * @return An  instance ready to compute the hash algorithm specified by .
	 */
	static function CreateHash(hashAlgorithm:cs.system.security.cryptography.HashAlgorithmName):cs.system.security.cryptography.IncrementalHash;
	/**
	 * Creates an  for the Hash-based Message Authentication Code (HMAC) algorithm
	 * using the specified hash algorithm and key.
	 * @param hashAlgorithm The name of the hash algorithm to perform within the HMAC.
	 * @param key The secret key for the HMAC. The key can be of any length, but a key
	 * longer than the output size of the specified hash algorithm will be hashed to
	 * derive a correctly-sized key. Therefore, the recommended size of the secret key
	 * is the output size of the specified hash algorithm.
	 * @return An instance of the  class ready to compute the specified hash algorithm.
	 */
	static function CreateHMAC(hashAlgorithm:cs.system.security.cryptography.HashAlgorithmName, key:cs.NativeArray<cs.UInt8>):cs.system.security.cryptography.IncrementalHash;
	@:overload(function(data:cs.NativeArray<cs.UInt8>):Void {})
	@:overload(function(data:cs.system.ReadOnlySpan<cs.UInt8>):Void {})
	/**
	 * Appends the specified data to the data already processed in the hash or HMAC.
	 * @param data The data to process.
	 */
	function AppendData(data:cs.NativeArray<cs.UInt8>, offset:Int, count:Int):Void;
	/** Releases the resources used by the current instance of the  class. */
	function Dispose():Void;
	/**
	 * Retrieves the hash or Hash-based Message Authentication Code (HMAC) for the data
	 * accumulated from prior calls to the  method,  and resets the object to its
	 * initial state.
	 * @return The computed hash or HMAC.
	 */
	function GetHashAndReset():cs.NativeArray<cs.UInt8>;
	/**
	 * @param destination 
	 * @param bytesWritten 
	 */
	function TryGetHashAndReset(destination:cs.system.Span<cs.UInt8>, bytesWritten:cs.Ref<Int>):Bool;
}
