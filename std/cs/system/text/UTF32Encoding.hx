package cs.system.text;

/** Represents a UTF-32 encoding of Unicode characters. */
@:native("System.Text.UTF32Encoding")
extern class UTF32Encoding extends cs.system.text.Encoding {
	@:overload(function():Void {})
	@:overload(function(bigEndian:Bool, byteOrderMark:Bool):Void {})
	function new(bigEndian:Bool, byteOrderMark:Bool, throwOnInvalidCharacters:Bool):Void;
	/**
	 * Determines whether the specified  is equal to the current  object.
	 * @param value The  to compare with the current object.
	 * @return if  is an instance of  and is equal to the current object; otherwise, .
	 */
	function Equals(value:Dynamic):Bool;
	@:overload(function(s:String):Int {})
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
	 * @return The actual number of bytes written at the location indicated by the 
	 * parameter.
	 */
	function GetBytes(s:String, charIndex:Int, charCount:Int, bytes:cs.NativeArray<cs.UInt8>, byteIndex:Int):Int;
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
	 * Obtains a decoder that converts a UTF-32 encoded sequence of bytes into a
	 * sequence of Unicode characters.
	 * @return A  that converts a UTF-32 encoded sequence of bytes into a sequence of
	 * Unicode characters.
	 */
	function GetDecoder():cs.system.text.Decoder;
	/**
	 * Obtains an encoder that converts a sequence of Unicode characters into a UTF-32
	 * encoded sequence of bytes.
	 * @return A  that converts a sequence of Unicode characters into a UTF-32 encoded
	 * sequence of bytes.
	 */
	function GetEncoder():cs.system.text.Encoder;
	/**
	 * Returns the hash code for the current instance.
	 * @return The hash code for the current  object.
	 */
	function GetHashCode():Int;
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
	 * Returns a Unicode byte order mark encoded in UTF-32 format, if the  object is
	 * configured to supply one.
	 * @return A byte array containing the Unicode byte order mark, if the  object is
	 * configured to supply one. Otherwise, this method returns a zero-length byte
	 * array.
	 */
	function GetPreamble():cs.NativeArray<cs.UInt8>;
	/**
	 * Decodes a range of bytes from a byte array into a string.
	 * @param bytes The byte array containing the sequence of bytes to decode.
	 * @param index The index of the first byte to decode.
	 * @param count The number of bytes to decode.
	 * @return A string that contains the results of decoding the specified sequence of
	 * bytes.
	 */
	function GetString(bytes:cs.NativeArray<cs.UInt8>, index:Int, count:Int):String;
}
