package cs.system.text;

/** Represents a character encoding. */
@:native("System.Text.Encoding")
extern class Encoding {
	/**
	 * Gets an encoding for the ASCII (7-bit) character set.
	 * @return An  encoding for the ASCII (7-bit) character set.
	 */
	static var ASCII(default, never):cs.system.text.Encoding;
	/**
	 * Gets an encoding for the UTF-16 format that uses the big endian byte order.
	 * @return An encoding object for the UTF-16 format that uses the big endian byte
	 * order.
	 */
	static var BigEndianUnicode(default, never):cs.system.text.Encoding;
	/**
	 * Gets the default encoding for this .NET implementation.
	 * @return The default encoding for this .NET implementation.
	 */
	static var Default(default, never):cs.system.text.Encoding;
	/**
	 * Gets an encoding for the UTF-16 format using the little endian byte order.
	 * @return An encoding for the UTF-16 format using the little endian byte order.
	 */
	static var Unicode(default, never):cs.system.text.Encoding;
	/**
	 * Gets an encoding for the UTF-32 format using the little endian byte order.
	 * @return An  encoding object for the UTF-32 format using the little endian byte
	 * order.
	 */
	static var UTF32(default, never):cs.system.text.Encoding;
	/**
	 * Gets an encoding for the UTF-7 format.
	 * @return An encoding for the UTF-7 format.
	 */
	static var UTF7(default, never):cs.system.text.Encoding;
	/**
	 * Gets an encoding for the UTF-8 format.
	 * @return An encoding for the UTF-8 format.
	 */
	static var UTF8(default, never):cs.system.text.Encoding;
	/**
	 * When overridden in a derived class, gets a name for the current encoding that
	 * can be used with mail agent body tags.
	 * @return A name for the current  that can be used with mail agent body tags. -or-
	 * An empty string (""), if the current  cannot be used.
	 */
	var BodyName(default, never):String;
	/**
	 * When overridden in a derived class, gets the code page identifier of the current
	 * .
	 * @return The code page identifier of the current .
	 */
	var CodePage(default, never):Int;
	/**
	 * Gets or sets the  object for the current  object.
	 * @return The decoder fallback object for the current  object.
	 */
	var DecoderFallback(default, default):cs.system.text.DecoderFallback;
	/**
	 * Gets or sets the  object for the current  object.
	 * @return The encoder fallback object for the current  object.
	 */
	var EncoderFallback(default, default):cs.system.text.EncoderFallback;
	/**
	 * When overridden in a derived class, gets the human-readable description of the
	 * current encoding.
	 * @return The human-readable description of the current .
	 */
	var EncodingName(default, never):String;
	/**
	 * When overridden in a derived class, gets a name for the current encoding that
	 * can be used with mail agent header tags.
	 * @return A name for the current  to use with mail agent header tags. -or- An
	 * empty string (""), if the current  cannot be used.
	 */
	var HeaderName(default, never):String;
	/**
	 * When overridden in a derived class, gets a value indicating whether the current
	 * encoding can be used by browser clients for displaying content.
	 * @return if the current  can be used by browser clients for displaying content;
	 * otherwise, .
	 */
	var IsBrowserDisplay(default, never):Bool;
	/**
	 * When overridden in a derived class, gets a value indicating whether the current
	 * encoding can be used by browser clients for saving content.
	 * @return if the current  can be used by browser clients for saving content;
	 * otherwise, .
	 */
	var IsBrowserSave(default, never):Bool;
	/**
	 * When overridden in a derived class, gets a value indicating whether the current
	 * encoding can be used by mail and news clients for displaying content.
	 * @return if the current  can be used by mail and news clients for displaying
	 * content; otherwise, .
	 */
	var IsMailNewsDisplay(default, never):Bool;
	/**
	 * When overridden in a derived class, gets a value indicating whether the current
	 * encoding can be used by mail and news clients for saving content.
	 * @return if the current  can be used by mail and news clients for saving content;
	 * otherwise, .
	 */
	var IsMailNewsSave(default, never):Bool;
	/**
	 * When overridden in a derived class, gets a value indicating whether the current
	 * encoding is read-only.
	 * @return if the current  is read-only; otherwise, . The default is .
	 */
	var IsReadOnly(default, never):Bool;
	/**
	 * When overridden in a derived class, gets a value indicating whether the current
	 * encoding uses single-byte code points.
	 * @return if the current  uses single-byte code points; otherwise, .
	 */
	var IsSingleByte(default, never):Bool;
	var Preamble(default, never):cs.system.ReadOnlySpan<cs.UInt8>;
	/**
	 * When overridden in a derived class, gets the name registered with the Internet
	 * Assigned Numbers Authority (IANA) for the current encoding.
	 * @return The IANA name for the current .
	 */
	var WebName(default, never):String;
	/**
	 * When overridden in a derived class, gets the Windows operating system code page
	 * that most closely corresponds to the current encoding.
	 * @return The Windows operating system code page that most closely corresponds to
	 * the current .
	 */
	var WindowsCodePage(default, never):Int;
	@:overload(function(srcEncoding:cs.system.text.Encoding, dstEncoding:cs.system.text.Encoding, bytes:cs.NativeArray<cs.UInt8>):cs.NativeArray<cs.UInt8> {})
	/**
	 * Converts an entire byte array from one encoding to another.
	 * @param srcEncoding The encoding format of .
	 * @param dstEncoding The target encoding format.
	 * @param bytes The bytes to convert.
	 * @return An array of type  containing the results of converting  from  to .
	 */
	static function Convert(srcEncoding:cs.system.text.Encoding, dstEncoding:cs.system.text.Encoding, bytes:cs.NativeArray<cs.UInt8>, index:Int, count:Int):cs.NativeArray<cs.UInt8>;
	@:overload(function(codepage:Int):cs.system.text.Encoding {})
	@:overload(function(name:String):cs.system.text.Encoding {})
	@:overload(function(codepage:Int, encoderFallback:cs.system.text.EncoderFallback, decoderFallback:cs.system.text.DecoderFallback):cs.system.text.Encoding {})
	/**
	 * Returns the encoding associated with the specified code page identifier.
	 * @param codepage The code page identifier of the preferred encoding. Possible
	 * values are listed in the Code Page column of the table that appears in the 
	 * class topic. -or- 0 (zero), to use the default encoding.
	 * @return The encoding that is associated with the specified code page.
	 */
	static function GetEncoding(name:String, encoderFallback:cs.system.text.EncoderFallback, decoderFallback:cs.system.text.DecoderFallback):cs.system.text.Encoding;
	/**
	 * Returns an array that contains all encodings.
	 * @return An array that contains all encodings.
	 */
	static function GetEncodings():cs.NativeArray<cs.system.text.EncodingInfo>;
	/**
	 * Registers an encoding provider.
	 * @param provider A subclass of  that provides access to additional character
	 * encodings.
	 */
	static function RegisterProvider(provider:cs.system.text.EncodingProvider):Void;
	/**
	 * When overridden in a derived class, creates a shallow copy of the current 
	 * object.
	 * @return A copy of the current  object.
	 */
	function Clone():Dynamic;
	/**
	 * Determines whether the specified  is equal to the current instance.
	 * @param value The  to compare with the current instance.
	 * @return if  is an instance of  and is equal to the current instance; otherwise,
	 * .
	 */
	function Equals(value:Dynamic):Bool;
	@:overload(function(chars:cs.NativeArray<cs.Char16>):Int {})
	@:overload(function(chars:cs.system.ReadOnlySpan<cs.Char16>):Int {})
	@:overload(function(s:String):Int {})
	@:overload(function(chars:cs.Pointer<cs.Char16>, count:Int):Int {})
	@:overload(function(chars:cs.NativeArray<cs.Char16>, index:Int, count:Int):Int {})
	/**
	 * When overridden in a derived class, calculates the number of bytes produced by
	 * encoding a set of characters starting at the specified character pointer.
	 * @param chars A pointer to the first character to encode.
	 * @param count The number of characters to encode.
	 * @return The number of bytes produced by encoding the specified characters.
	 */
	function GetByteCount(s:String, index:Int, count:Int):Int;
	@:overload(function(chars:cs.NativeArray<cs.Char16>):cs.NativeArray<cs.UInt8> {})
	@:overload(function(s:String):cs.NativeArray<cs.UInt8> {})
	@:overload(function(chars:cs.system.ReadOnlySpan<cs.Char16>, bytes:cs.system.Span<cs.UInt8>):Int {})
	@:overload(function(chars:cs.NativeArray<cs.Char16>, index:Int, count:Int):cs.NativeArray<cs.UInt8> {})
	@:overload(function(s:String, index:Int, count:Int):cs.NativeArray<cs.UInt8> {})
	@:overload(function(chars:cs.Pointer<cs.Char16>, charCount:Int, bytes:cs.Pointer<cs.UInt8>, byteCount:Int):Int {})
	@:overload(function(chars:cs.NativeArray<cs.Char16>, charIndex:Int, charCount:Int, bytes:cs.NativeArray<cs.UInt8>, byteIndex:Int):Int {})
	/**
	 * When overridden in a derived class, encodes a set of characters starting at the
	 * specified character pointer into a sequence of bytes that are stored starting at
	 * the specified byte pointer.
	 * @param chars A pointer to the first character to encode.
	 * @param charCount The number of characters to encode.
	 * @param bytes A pointer to the location at which to start writing the resulting
	 * sequence of bytes.
	 * @param byteCount The maximum number of bytes to write.
	 * @return The actual number of bytes written at the location indicated by the 
	 * parameter.
	 */
	function GetBytes(s:String, charIndex:Int, charCount:Int, bytes:cs.NativeArray<cs.UInt8>, byteIndex:Int):Int;
	@:overload(function(bytes:cs.NativeArray<cs.UInt8>):Int {})
	@:overload(function(bytes:cs.system.ReadOnlySpan<cs.UInt8>):Int {})
	@:overload(function(bytes:cs.Pointer<cs.UInt8>, count:Int):Int {})
	/**
	 * When overridden in a derived class, calculates the number of characters produced
	 * by decoding a sequence of bytes starting at the specified byte pointer.
	 * @param bytes A pointer to the first byte to decode.
	 * @param count The number of bytes to decode.
	 * @return The number of characters produced by decoding the specified sequence of
	 * bytes.
	 */
	function GetCharCount(bytes:cs.NativeArray<cs.UInt8>, index:Int, count:Int):Int;
	@:overload(function(bytes:cs.NativeArray<cs.UInt8>):cs.NativeArray<cs.Char16> {})
	@:overload(function(bytes:cs.system.ReadOnlySpan<cs.UInt8>, chars:cs.system.Span<cs.Char16>):Int {})
	@:overload(function(bytes:cs.NativeArray<cs.UInt8>, index:Int, count:Int):cs.NativeArray<cs.Char16> {})
	@:overload(function(bytes:cs.Pointer<cs.UInt8>, byteCount:Int, chars:cs.Pointer<cs.Char16>, charCount:Int):Int {})
	/**
	 * When overridden in a derived class, decodes a sequence of bytes starting at the
	 * specified byte pointer into a set of characters that are stored starting at the
	 * specified character pointer.
	 * @param bytes A pointer to the first byte to decode.
	 * @param byteCount The number of bytes to decode.
	 * @param chars A pointer to the location at which to start writing the resulting
	 * set of characters.
	 * @param charCount The maximum number of characters to write.
	 * @return The actual number of characters written at the location indicated by the
	 * parameter.
	 */
	function GetChars(bytes:cs.NativeArray<cs.UInt8>, byteIndex:Int, byteCount:Int, chars:cs.NativeArray<cs.Char16>, charIndex:Int):Int;
	/**
	 * When overridden in a derived class, obtains a decoder that converts an encoded
	 * sequence of bytes into a sequence of characters.
	 * @return A  that converts an encoded sequence of bytes into a sequence of
	 * characters.
	 */
	function GetDecoder():cs.system.text.Decoder;
	/**
	 * When overridden in a derived class, obtains an encoder that converts a sequence
	 * of Unicode characters into an encoded sequence of bytes.
	 * @return A  that converts a sequence of Unicode characters into an encoded
	 * sequence of bytes.
	 */
	function GetEncoder():cs.system.text.Encoder;
	/**
	 * Returns the hash code for the current instance.
	 * @return The hash code for the current instance.
	 */
	function GetHashCode():Int;
	/**
	 * When overridden in a derived class, calculates the maximum number of bytes
	 * produced by encoding the specified number of characters.
	 * @param charCount The number of characters to encode.
	 * @return The maximum number of bytes produced by encoding the specified number of
	 * characters.
	 */
	function GetMaxByteCount(charCount:Int):Int;
	/**
	 * When overridden in a derived class, calculates the maximum number of characters
	 * produced by decoding the specified number of bytes.
	 * @param byteCount The number of bytes to decode.
	 * @return The maximum number of characters produced by decoding the specified
	 * number of bytes.
	 */
	function GetMaxCharCount(byteCount:Int):Int;
	/**
	 * When overridden in a derived class, returns a sequence of bytes that specifies
	 * the encoding used.
	 * @return A byte array containing a sequence of bytes that specifies the encoding
	 * used. -or- A byte array of length zero, if a preamble is not required.
	 */
	function GetPreamble():cs.NativeArray<cs.UInt8>;
	@:overload(function(bytes:cs.NativeArray<cs.UInt8>):String {})
	@:overload(function(bytes:cs.system.ReadOnlySpan<cs.UInt8>):String {})
	@:overload(function(bytes:cs.Pointer<cs.UInt8>, byteCount:Int):String {})
	/**
	 * When overridden in a derived class, decodes a specified number of bytes starting
	 * at a specified address into a string.
	 * @param bytes A pointer to a byte array.
	 * @param byteCount The number of bytes to decode.
	 * @return A string that contains the results of decoding the specified sequence of
	 * bytes.
	 */
	function GetString(bytes:cs.NativeArray<cs.UInt8>, index:Int, count:Int):String;
	@:overload(function():Bool {})
	/**
	 * Gets a value indicating whether the current encoding is always normalized, using
	 * the default normalization form.
	 * @return if the current  is always normalized; otherwise, . The default is .
	 */
	function IsAlwaysNormalized(form:cs.system.text.NormalizationForm):Bool;
}
