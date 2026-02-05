package cs.system.text;

/** Throws  when an encoded input byte sequence cannot be converted to a decoded output character. This class cannot be inherited. */
@:native("System.Text.DecoderExceptionFallbackBuffer")
extern class DecoderExceptionFallbackBuffer extends cs.system.text.DecoderFallbackBuffer {
	function new():Void;
	/**
	 * Throws  when the input byte sequence cannot be decoded. The nominal return value
	 * is not used.
	 * @param bytesUnknown An input array of bytes.
	 * @param index The index position of a byte in the input.
	 * @return None. No value is returned because the  method always throws an
	 * exception. The nominal return value is . A return value is defined, although it
	 * is unchanging, because this method implements an abstract method.
	 */
	function Fallback(bytesUnknown:cs.NativeArray<cs.UInt8>, index:Int):Bool;
	/**
	 * Retrieves the next character in the exception data buffer.
	 * @return The return value is always the Unicode character NULL (U+0000). A return
	 * value is defined, although it is unchanging, because this method implements an
	 * abstract method.
	 */
	function GetNextChar():cs.Char16;
	/**
	 * Causes the next call to  to access the exception data buffer character position
	 * that is prior to the current position.
	 * @return The return value is always . A return value is defined, although it is
	 * unchanging, because this method implements an abstract method.
	 */
	function MovePrevious():Bool;
}
