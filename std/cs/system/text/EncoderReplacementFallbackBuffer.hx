package cs.system.text;

/** Represents a substitute input string that is used when the original input character cannot be encoded. This class cannot be inherited. */
@:native("System.Text.EncoderReplacementFallbackBuffer")
extern class EncoderReplacementFallbackBuffer extends cs.system.text.EncoderFallbackBuffer {
	function new(fallback:cs.system.text.EncoderReplacementFallback):Void;
	@:overload(function(charUnknown:cs.Char16, index:Int):Bool {})
	/**
	 * Indicates whether a replacement string can be used when an input surrogate pair
	 * cannot be encoded, or whether the surrogate pair can be ignored. Parameters
	 * specify the surrogate pair and the index position of the pair in the input.
	 * @param charUnknownHigh The high surrogate of the input pair.
	 * @param charUnknownLow The low surrogate of the input pair.
	 * @param index The index position of the surrogate pair in the input buffer.
	 * @return if the replacement string is not empty;  if the replacement string is
	 * empty.
	 */
	function Fallback(charUnknownHigh:cs.Char16, charUnknownLow:cs.Char16, index:Int):Bool;
	/**
	 * Retrieves the next character in the replacement fallback buffer.
	 * @return The next Unicode character in the replacement fallback buffer that the
	 * application can encode.
	 */
	function GetNextChar():cs.Char16;
	/**
	 * Causes the next call to the  method to access the character position in the
	 * replacement fallback buffer prior to the current character position.
	 * @return if the  operation was successful; otherwise, .
	 */
	function MovePrevious():Bool;
	/** Initializes all internal state information and data in this instance of . */
	function Reset():Void;
}
