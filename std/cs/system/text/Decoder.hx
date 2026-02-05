package cs.system.text;

/** Converts a sequence of encoded bytes into a set of characters. */
@:native("System.Text.Decoder")
extern class Decoder {
	/**
	 * Gets or sets a  object for the current  object.
	 * @return A  object.
	 */
	var Fallback(default, default):cs.system.text.DecoderFallback;
	/**
	 * Gets the  object associated with the current  object.
	 * @return A  object.
	 */
	var FallbackBuffer(default, never):cs.system.text.DecoderFallbackBuffer;
	@:overload(function(bytes:cs.system.ReadOnlySpan<cs.UInt8>, chars:cs.system.Span<cs.Char16>, flush:Bool, bytesUsed:cs.Ref<Int>, charsUsed:cs.Ref<Int>, completed:cs.Ref<Bool>):Void {})
	@:overload(function(bytes:cs.Pointer<cs.UInt8>, byteCount:Int, chars:cs.Pointer<cs.Char16>, charCount:Int, flush:Bool, bytesUsed:cs.Ref<Int>, charsUsed:cs.Ref<Int>, completed:cs.Ref<Bool>):Void {})
	/**
	 * Converts a buffer of encoded bytes to UTF-16 encoded characters and stores the
	 * result in another buffer.
	 * @param bytes The address of a buffer that contains the byte sequences to
	 * convert.
	 * @param byteCount The number of bytes in  to convert.
	 * @param chars The address of a buffer to store the converted characters.
	 * @param charCount The maximum number of characters in  to use in the conversion.
	 * @param flush to indicate no further data is to be converted; otherwise, .
	 * @param bytesUsed When this method returns, contains the number of bytes that
	 * were produced by the conversion. This parameter is passed uninitialized.
	 * @param charsUsed When this method returns, contains the number of characters
	 * from  that were used in the conversion. This parameter is passed uninitialized.
	 * @param completed When this method returns, contains  if all the characters
	 * specified by  were converted; otherwise, . This parameter is passed
	 * uninitialized.
	 */
	function Convert(bytes:cs.NativeArray<cs.UInt8>, byteIndex:Int, byteCount:Int, chars:cs.NativeArray<cs.Char16>, charIndex:Int, charCount:Int, flush:Bool, bytesUsed:cs.Ref<Int>, charsUsed:cs.Ref<Int>, completed:cs.Ref<Bool>):Void;
	@:overload(function(bytes:cs.system.ReadOnlySpan<cs.UInt8>, flush:Bool):Int {})
	@:overload(function(bytes:cs.Pointer<cs.UInt8>, count:Int, flush:Bool):Int {})
	@:overload(function(bytes:cs.NativeArray<cs.UInt8>, index:Int, count:Int):Int {})
	/**
	 * When overridden in a derived class, calculates the number of characters produced
	 * by decoding a sequence of bytes starting at the specified byte pointer. A
	 * parameter indicates whether to clear the internal state of the decoder after the
	 * calculation.
	 * @param bytes A pointer to the first byte to decode.
	 * @param count The number of bytes to decode.
	 * @param flush to simulate clearing the internal state of the encoder after the
	 * calculation; otherwise, .
	 * @return The number of characters produced by decoding the specified sequence of
	 * bytes and any bytes in the internal buffer.
	 */
	function GetCharCount(bytes:cs.NativeArray<cs.UInt8>, index:Int, count:Int, flush:Bool):Int;
	@:overload(function(bytes:cs.system.ReadOnlySpan<cs.UInt8>, chars:cs.system.Span<cs.Char16>, flush:Bool):Int {})
	@:overload(function(bytes:cs.Pointer<cs.UInt8>, byteCount:Int, chars:cs.Pointer<cs.Char16>, charCount:Int, flush:Bool):Int {})
	@:overload(function(bytes:cs.NativeArray<cs.UInt8>, byteIndex:Int, byteCount:Int, chars:cs.NativeArray<cs.Char16>, charIndex:Int):Int {})
	/**
	 * When overridden in a derived class, decodes a sequence of bytes starting at the
	 * specified byte pointer and any bytes in the internal buffer into a set of
	 * characters that are stored starting at the specified character pointer. A
	 * parameter indicates whether to clear the internal state of the decoder after the
	 * conversion.
	 * @param bytes A pointer to the first byte to decode.
	 * @param byteCount The number of bytes to decode.
	 * @param chars A pointer to the location at which to start writing the resulting
	 * set of characters.
	 * @param charCount The maximum number of characters to write.
	 * @param flush to clear the internal state of the decoder after the conversion;
	 * otherwise, .
	 * @return The actual number of characters written at the location indicated by the
	 * parameter.
	 */
	function GetChars(bytes:cs.NativeArray<cs.UInt8>, byteIndex:Int, byteCount:Int, chars:cs.NativeArray<cs.Char16>, charIndex:Int, flush:Bool):Int;
	/** When overridden in a derived class, sets the decoder back to its initial state. */
	function Reset():Void;
}
