package cs.system.text;

/** Represents a substitute output string that is emitted when the original input byte sequence cannot be decoded. This class cannot be inherited. */
@:native("System.Text.DecoderReplacementFallbackBuffer")
extern class DecoderReplacementFallbackBuffer extends cs.system.text.DecoderFallbackBuffer {
	function new(fallback:cs.system.text.DecoderReplacementFallback):Void;
	/**
	 * Prepares the replacement fallback buffer to use the current replacement string.
	 * @param bytesUnknown An input byte sequence. This parameter is ignored unless an
	 * exception is thrown.
	 * @param index The index position of the byte in . This parameter is ignored in
	 * this operation.
	 * @return if the replacement string is not empty;  if the replacement string is
	 * empty.
	 */
	function Fallback(bytesUnknown:cs.NativeArray<cs.UInt8>, index:Int):Bool;
	/**
	 * Retrieves the next character in the replacement fallback buffer.
	 * @return The next character in the replacement fallback buffer.
	 */
	function GetNextChar():cs.Char16;
	/**
	 * Causes the next call to  to access the character position in the replacement
	 * fallback buffer prior to the current character position.
	 * @return if the  operation was successful; otherwise, .
	 */
	function MovePrevious():Bool;
	/** Initializes all internal state information and data in the  object. */
	function Reset():Void;
}
