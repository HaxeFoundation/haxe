package cs.system.text;

/** Converts a set of characters into a sequence of bytes. */
@:native("System.Text.Encoder")
extern class Encoder {
	/**
	 * Gets or sets a  object for the current  object.
	 * @return A  object.
	 */
	var Fallback(default, default):cs.system.text.EncoderFallback;
	/**
	 * Gets the  object associated with the current  object.
	 * @return A  object.
	 */
	var FallbackBuffer(default, never):cs.system.text.EncoderFallbackBuffer;
	@:overload(function(chars:cs.system.ReadOnlySpan<cs.Char16>, bytes:cs.system.Span<cs.UInt8>, flush:Bool, charsUsed:cs.Ref<Int>, bytesUsed:cs.Ref<Int>, completed:cs.Ref<Bool>):Void {})
	@:overload(function(chars:cs.Pointer<cs.Char16>, charCount:Int, bytes:cs.Pointer<cs.UInt8>, byteCount:Int, flush:Bool, charsUsed:cs.Ref<Int>, bytesUsed:cs.Ref<Int>, completed:cs.Ref<Bool>):Void {})
	/**
	 * Converts a buffer of Unicode characters to an encoded byte sequence and stores
	 * the result in another buffer.
	 * @param chars The address of a string of UTF-16 encoded characters to convert.
	 * @param charCount The number of characters in  to convert.
	 * @param bytes The address of a buffer to store the converted bytes.
	 * @param byteCount The maximum number of bytes in  to use in the conversion.
	 * @param flush to indicate no further data is to be converted; otherwise, .
	 * @param charsUsed When this method returns, contains the number of characters
	 * from  that were used in the conversion. This parameter is passed uninitialized.
	 * @param bytesUsed When this method returns, contains the number of bytes that
	 * were used in the conversion. This parameter is passed uninitialized.
	 * @param completed When this method returns, contains  if all the characters
	 * specified by  were converted; otherwise, . This parameter is passed
	 * uninitialized.
	 */
	function Convert(chars:cs.NativeArray<cs.Char16>, charIndex:Int, charCount:Int, bytes:cs.NativeArray<cs.UInt8>, byteIndex:Int, byteCount:Int, flush:Bool, charsUsed:cs.Ref<Int>, bytesUsed:cs.Ref<Int>, completed:cs.Ref<Bool>):Void;
	@:overload(function(chars:cs.system.ReadOnlySpan<cs.Char16>, flush:Bool):Int {})
	@:overload(function(chars:cs.Pointer<cs.Char16>, count:Int, flush:Bool):Int {})
	/**
	 * When overridden in a derived class, calculates the number of bytes produced by
	 * encoding a set of characters starting at the specified character pointer. A
	 * parameter indicates whether to clear the internal state of the encoder after the
	 * calculation.
	 * @param chars A pointer to the first character to encode.
	 * @param count The number of characters to encode.
	 * @param flush to simulate clearing the internal state of the encoder after the
	 * calculation; otherwise, .
	 * @return The number of bytes produced by encoding the specified characters and
	 * any characters in the internal buffer.
	 */
	function GetByteCount(chars:cs.NativeArray<cs.Char16>, index:Int, count:Int, flush:Bool):Int;
	@:overload(function(chars:cs.system.ReadOnlySpan<cs.Char16>, bytes:cs.system.Span<cs.UInt8>, flush:Bool):Int {})
	@:overload(function(chars:cs.Pointer<cs.Char16>, charCount:Int, bytes:cs.Pointer<cs.UInt8>, byteCount:Int, flush:Bool):Int {})
	/**
	 * When overridden in a derived class, encodes a set of characters starting at the
	 * specified character pointer and any characters in the internal buffer into a
	 * sequence of bytes that are stored starting at the specified byte pointer. A
	 * parameter indicates whether to clear the internal state of the encoder after the
	 * conversion.
	 * @param chars A pointer to the first character to encode.
	 * @param charCount The number of characters to encode.
	 * @param bytes A pointer to the location at which to start writing the resulting
	 * sequence of bytes.
	 * @param byteCount The maximum number of bytes to write.
	 * @param flush to clear the internal state of the encoder after the conversion;
	 * otherwise, .
	 * @return The actual number of bytes written at the location indicated by the 
	 * parameter.
	 */
	function GetBytes(chars:cs.NativeArray<cs.Char16>, charIndex:Int, charCount:Int, bytes:cs.NativeArray<cs.UInt8>, byteIndex:Int, flush:Bool):Int;
	/** When overridden in a derived class, sets the encoder back to its initial state. */
	function Reset():Void;
}
