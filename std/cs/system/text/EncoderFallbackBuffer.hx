package cs.system.text;

/** Provides a buffer that allows a fallback handler to return an alternate string to an encoder when it cannot encode an input character. */
@:native("System.Text.EncoderFallbackBuffer")
extern class EncoderFallbackBuffer {
	/**
	 * When overridden in a derived class, gets the number of characters in the current
	 * object that remain to be processed.
	 * @return The number of characters in the current fallback buffer that have not
	 * yet been processed.
	 */
	var Remaining(default, never):Int;
	@:overload(function(charUnknown:cs.Char16, index:Int):Bool {})
	/**
	 * When overridden in a derived class, prepares the fallback buffer to handle the
	 * specified surrogate pair.
	 * @param charUnknownHigh The high surrogate of the input pair.
	 * @param charUnknownLow The low surrogate of the input pair.
	 * @param index The index position of the surrogate pair in the input buffer.
	 * @return if the fallback buffer can process  and ;  if the fallback buffer
	 * ignores the surrogate pair.
	 */
	function Fallback(charUnknownHigh:cs.Char16, charUnknownLow:cs.Char16, index:Int):Bool;
	/**
	 * When overridden in a derived class, retrieves the next character in the fallback
	 * buffer.
	 * @return The next character in the fallback buffer.
	 */
	function GetNextChar():cs.Char16;
	/**
	 * When overridden in a derived class, causes the next call to the  method to
	 * access the data buffer character position that is prior to the current character
	 * position.
	 * @return if the  operation was successful; otherwise, .
	 */
	function MovePrevious():Bool;
	/** Initializes all data and state information pertaining to this fallback buffer. */
	function Reset():Void;
}
