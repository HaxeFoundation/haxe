package cs.system.security.cryptography;

/** Defines the basic operations of cryptographic transformations. */
@:native("System.Security.Cryptography.ICryptoTransform")
extern interface ICryptoTransform extends cs.system.IDisposable {
	/**
	 * Gets a value indicating whether the current transform can be reused.
	 * @return if the current transform can be reused; otherwise, .
	 */
	var CanReuseTransform(default, never):Bool;
	/**
	 * Gets a value indicating whether multiple blocks can be transformed.
	 * @return if multiple blocks can be transformed; otherwise, .
	 */
	var CanTransformMultipleBlocks(default, never):Bool;
	/**
	 * Gets the input block size.
	 * @return The size of the input data blocks in bytes.
	 */
	var InputBlockSize(default, never):Int;
	/**
	 * Gets the output block size.
	 * @return The size of the output data blocks in bytes.
	 */
	var OutputBlockSize(default, never):Int;
	/**
	 * Transforms the specified region of the input byte array and copies the resulting
	 * transform to the specified region of the output byte array.
	 * @param inputBuffer The input for which to compute the transform.
	 * @param inputOffset The offset into the input byte array from which to begin
	 * using data.
	 * @param inputCount The number of bytes in the input byte array to use as data.
	 * @param outputBuffer The output to which to write the transform.
	 * @param outputOffset The offset into the output byte array from which to begin
	 * writing data.
	 * @return The number of bytes written.
	 */
	function TransformBlock(inputBuffer:cs.NativeArray<cs.UInt8>, inputOffset:Int, inputCount:Int, outputBuffer:cs.NativeArray<cs.UInt8>, outputOffset:Int):Int;
	/**
	 * Transforms the specified region of the specified byte array.
	 * @param inputBuffer The input for which to compute the transform.
	 * @param inputOffset The offset into the byte array from which to begin using
	 * data.
	 * @param inputCount The number of bytes in the byte array to use as data.
	 * @return The computed transform.
	 */
	function TransformFinalBlock(inputBuffer:cs.NativeArray<cs.UInt8>, inputOffset:Int, inputCount:Int):cs.NativeArray<cs.UInt8>;
}
