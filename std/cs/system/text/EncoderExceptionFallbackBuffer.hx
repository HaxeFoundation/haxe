package cs.system.text;

/** Throws  when an input character cannot be converted to an encoded output byte sequence. This class cannot be inherited. */
@:native("System.Text.EncoderExceptionFallbackBuffer")
extern class EncoderExceptionFallbackBuffer extends cs.system.text.EncoderFallbackBuffer {
	function new():Void;
	@:overload(function(charUnknown:cs.Char16, index:Int):Bool {})
	/**
	 * Throws an exception because the input character cannot be encoded. Parameters
	 * specify the value and index position of the surrogate pair in the input, and the
	 * nominal return value is not used.
	 * @param charUnknownHigh The high surrogate of the input pair.
	 * @param charUnknownLow The low surrogate of the input pair.
	 * @param index The index position of the surrogate pair in the input buffer.
	 * @return None. No value is returned because the  method always throws an
	 * exception.
	 */
	function Fallback(charUnknownHigh:cs.Char16, charUnknownLow:cs.Char16, index:Int):Bool;
	/**
	 * Retrieves the next character in the exception fallback buffer.
	 * @return The return value is always the Unicode character, NULL (U+0000). A
	 * return value is defined, although it is unchanging, because this method
	 * implements an abstract method.
	 */
	function GetNextChar():cs.Char16;
	/**
	 * Causes the next call to the  method to access the exception data buffer
	 * character position that is prior to the current position.
	 * @return The return value is always . A return value is defined, although it is
	 * unchanging, because this method implements an abstract method.
	 */
	function MovePrevious():Bool;
}
