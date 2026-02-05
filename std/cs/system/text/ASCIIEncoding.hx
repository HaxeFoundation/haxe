package cs.system.text;

/** Represents an ASCII character encoding of Unicode characters. */
@:native("System.Text.ASCIIEncoding")
extern class ASCIIEncoding extends cs.system.text.Encoding {
	function new():Void;
	@:overload(function(chars:String):Int {})
	@:overload(function(chars:cs.Pointer<cs.Char16>, count:Int):Int {})
	/**
	 * Calculates the number of bytes produced by encoding a set of characters starting
	 * at the specified character pointer.
	 * @param chars A pointer to the first character to encode.
	 * @param count The number of characters to encode.
	 * @return The number of bytes produced by encoding the specified characters.
	 */
	function GetByteCount(chars:cs.NativeArray<cs.Char16>, index:Int, count:Int):Int;
	@:overload(function(chars:cs.Pointer<cs.Char16>, charCount:Int, bytes:cs.Pointer<cs.UInt8>, byteCount:Int):Int {})
	@:overload(function(chars:cs.NativeArray<cs.Char16>, charIndex:Int, charCount:Int, bytes:cs.NativeArray<cs.UInt8>, byteIndex:Int):Int {})
	/**
	 * Encodes a set of characters starting at the specified character pointer into a
	 * sequence of bytes that are stored starting at the specified byte pointer.
	 * @param chars A pointer to the first character to encode.
	 * @param charCount The number of characters to encode.
	 * @param bytes A pointer to the location at which to start writing the resulting
	 * sequence of bytes.
	 * @param byteCount The maximum number of bytes to write.
	 * @return The actual number of bytes written at the location indicated by .
	 */
	function GetBytes(chars:String, charIndex:Int, charCount:Int, bytes:cs.NativeArray<cs.UInt8>, byteIndex:Int):Int;
	@:overload(function(bytes:cs.Pointer<cs.UInt8>, count:Int):Int {})
	/**
	 * Calculates the number of characters produced by decoding a sequence of bytes
	 * starting at the specified byte pointer.
	 * @param bytes A pointer to the first byte to decode.
	 * @param count The number of bytes to decode.
	 * @return The number of characters produced by decoding the specified sequence of
	 * bytes.
	 */
	function GetCharCount(bytes:cs.NativeArray<cs.UInt8>, index:Int, count:Int):Int;
	@:overload(function(bytes:cs.Pointer<cs.UInt8>, byteCount:Int, chars:cs.Pointer<cs.Char16>, charCount:Int):Int {})
	/**
	 * Decodes a sequence of bytes starting at the specified byte pointer into a set of
	 * characters that are stored starting at the specified character pointer.
	 * @param bytes A pointer to the first byte to decode.
	 * @param byteCount The number of bytes to decode.
	 * @param chars A pointer to the location at which to start writing the resulting
	 * set of characters.
	 * @param charCount The maximum number of characters to write.
	 * @return The actual number of characters written at the location indicated by .
	 */
	function GetChars(bytes:cs.NativeArray<cs.UInt8>, byteIndex:Int, byteCount:Int, chars:cs.NativeArray<cs.Char16>, charIndex:Int):Int;
	/**
	 * Obtains a decoder that converts an ASCII encoded sequence of bytes into a
	 * sequence of Unicode characters.
	 * @return A  that converts an ASCII encoded sequence of bytes into a sequence of
	 * Unicode characters.
	 */
	function GetDecoder():cs.system.text.Decoder;
	/**
	 * Obtains an encoder that converts a sequence of Unicode characters into an ASCII
	 * encoded sequence of bytes.
	 * @return An  that converts a sequence of Unicode characters into an ASCII encoded
	 * sequence of bytes.
	 */
	function GetEncoder():cs.system.text.Encoder;
	/**
	 * Calculates the maximum number of bytes produced by encoding the specified number
	 * of characters.
	 * @param charCount The number of characters to encode.
	 * @return The maximum number of bytes produced by encoding the specified number of
	 * characters.
	 */
	function GetMaxByteCount(charCount:Int):Int;
	/**
	 * Calculates the maximum number of characters produced by decoding the specified
	 * number of bytes.
	 * @param byteCount The number of bytes to decode.
	 * @return The maximum number of characters produced by decoding the specified
	 * number of bytes.
	 */
	function GetMaxCharCount(byteCount:Int):Int;
	/**
	 * Decodes a range of bytes from a byte array into a string.
	 * @param bytes The byte array containing the sequence of bytes to decode.
	 * @param byteIndex The index of the first byte to decode.
	 * @param byteCount The number of bytes to decode.
	 * @return A  containing the results of decoding the specified sequence of bytes.
	 */
	function GetString(bytes:cs.NativeArray<cs.UInt8>, byteIndex:Int, byteCount:Int):String;
}
