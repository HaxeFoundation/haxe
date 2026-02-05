package cs.system.security.cryptography;

/** Converts a  from base 64. */
@:native("System.Security.Cryptography.FromBase64Transform")
extern class FromBase64Transform {
	/**
	 * Gets a value indicating whether the current transform can be reused.
	 * @return Always .
	 */
	var CanReuseTransform(default, never):Bool;
	/**
	 * Gets a value that indicates whether multiple blocks can be transformed.
	 * @return Always .
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
	@:overload(function():Void {})
	function new(whitespaces:cs.system.security.cryptography.FromBase64TransformMode):Void;
	/** Releases all resources used by the . */
	function Clear():Void;
	/** Releases all resources used by the current instance of the  class. */
	function Dispose():Void;
	/**
	 * Converts the specified region of the input byte array from base 64 and copies
	 * the result to the specified region of the output byte array.
	 * @param inputBuffer The input to compute from base 64.
	 * @param inputOffset The offset into the input byte array from which to begin
	 * using data.
	 * @param inputCount The number of bytes in the input byte array to use as data.
	 * @param outputBuffer The output to which to write the result.
	 * @param outputOffset The offset into the output byte array from which to begin
	 * writing data.
	 * @return The number of bytes written.
	 */
	function TransformBlock(inputBuffer:cs.NativeArray<cs.UInt8>, inputOffset:Int, inputCount:Int, outputBuffer:cs.NativeArray<cs.UInt8>, outputOffset:Int):Int;
	/**
	 * Converts the specified region of the specified byte array from base 64.
	 * @param inputBuffer The input to convert from base 64.
	 * @param inputOffset The offset into the byte array from which to begin using
	 * data.
	 * @param inputCount The number of bytes in the byte array to use as data.
	 * @return The computed conversion.
	 */
	function TransformFinalBlock(inputBuffer:cs.NativeArray<cs.UInt8>, inputOffset:Int, inputCount:Int):cs.NativeArray<cs.UInt8>;
}
