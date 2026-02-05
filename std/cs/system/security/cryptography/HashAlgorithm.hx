package cs.system.security.cryptography;

/** Represents the base class from which all implementations of cryptographic hash algorithms must derive. */
@:native("System.Security.Cryptography.HashAlgorithm")
extern class HashAlgorithm {
	/**
	 * Gets a value indicating whether the current transform can be reused.
	 * @return Always .
	 */
	var CanReuseTransform(default, never):Bool;
	/**
	 * When overridden in a derived class, gets a value indicating whether multiple
	 * blocks can be transformed.
	 * @return if multiple blocks can be transformed; otherwise, .
	 */
	var CanTransformMultipleBlocks(default, never):Bool;
	/**
	 * Gets the value of the computed hash code.
	 * @return The current value of the computed hash code.
	 */
	var Hash(default, never):cs.NativeArray<cs.UInt8>;
	/**
	 * Gets the size, in bits, of the computed hash code.
	 * @return The size, in bits, of the computed hash code.
	 */
	var HashSize(default, never):Int;
	/**
	 * When overridden in a derived class, gets the input block size.
	 * @return The input block size.
	 */
	var InputBlockSize(default, never):Int;
	/**
	 * When overridden in a derived class, gets the output block size.
	 * @return The output block size.
	 */
	var OutputBlockSize(default, never):Int;
	@:overload(function():cs.system.security.cryptography.HashAlgorithm {})
	/**
	 * Creates an instance of the default implementation of a hash algorithm.
	 * @return A new  instance, unless the default settings have been changed using the
	 * .
	 */
	static function Create(hashName:String):cs.system.security.cryptography.HashAlgorithm;
	/** Releases all resources used by the  class. */
	function Clear():Void;
	@:overload(function(buffer:cs.NativeArray<cs.UInt8>):cs.NativeArray<cs.UInt8> {})
	@:overload(function(inputStream:cs.system.io.Stream):cs.NativeArray<cs.UInt8> {})
	/**
	 * Computes the hash value for the specified byte array.
	 * @param buffer The input to compute the hash code for.
	 * @return The computed hash code.
	 */
	function ComputeHash(buffer:cs.NativeArray<cs.UInt8>, offset:Int, count:Int):cs.NativeArray<cs.UInt8>;
	/** Releases all resources used by the current instance of the  class. */
	function Dispose():Void;
	/** Initializes an implementation of the  class. */
	function Initialize():Void;
	/**
	 * Computes the hash value for the specified region of the input byte array and
	 * copies the specified region of the input byte array to the specified region of
	 * the output byte array.
	 * @param inputBuffer The input to compute the hash code for.
	 * @param inputOffset The offset into the input byte array from which to begin
	 * using data.
	 * @param inputCount The number of bytes in the input byte array to use as data.
	 * @param outputBuffer A copy of the part of the input array used to compute the
	 * hash code.
	 * @param outputOffset The offset into the output byte array from which to begin
	 * writing data.
	 * @return The number of bytes written.
	 */
	function TransformBlock(inputBuffer:cs.NativeArray<cs.UInt8>, inputOffset:Int, inputCount:Int, outputBuffer:cs.NativeArray<cs.UInt8>, outputOffset:Int):Int;
	/**
	 * Computes the hash value for the specified region of the specified byte array.
	 * @param inputBuffer The input to compute the hash code for.
	 * @param inputOffset The offset into the byte array from which to begin using
	 * data.
	 * @param inputCount The number of bytes in the byte array to use as data.
	 * @return An array that is a copy of the part of the input that is hashed.
	 */
	function TransformFinalBlock(inputBuffer:cs.NativeArray<cs.UInt8>, inputOffset:Int, inputCount:Int):cs.NativeArray<cs.UInt8>;
	/**
	 * @param source 
	 * @param destination 
	 * @param bytesWritten 
	 */
	function TryComputeHash(source:cs.system.ReadOnlySpan<cs.UInt8>, destination:cs.system.Span<cs.UInt8>, bytesWritten:cs.Ref<Int>):Bool;
}
