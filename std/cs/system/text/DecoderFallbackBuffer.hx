package cs.system.text;

/** Provides a buffer that allows a fallback handler to return an alternate string to a decoder when it cannot decode an input byte sequence. */
@:native("System.Text.DecoderFallbackBuffer")
extern class DecoderFallbackBuffer {
	/**
	 * When overridden in a derived class, gets the number of characters in the current
	 * object that remain to be processed.
	 * @return The number of characters in the current fallback buffer that have not
	 * yet been processed.
	 */
	var Remaining(default, never):Int;
	/**
	 * When overridden in a derived class, prepares the fallback buffer to handle the
	 * specified input byte sequence.
	 * @param bytesUnknown An input array of bytes.
	 * @param index The index position of a byte in .
	 * @return if the fallback buffer can process ;  if the fallback buffer ignores .
	 */
	function Fallback(bytesUnknown:cs.NativeArray<cs.UInt8>, index:Int):Bool;
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
