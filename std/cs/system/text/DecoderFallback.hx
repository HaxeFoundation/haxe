package cs.system.text;

/** Provides a failure-handling mechanism, called a fallback, for an encoded input byte sequence that cannot be converted to an output character. */
@:native("System.Text.DecoderFallback")
extern class DecoderFallback {
	/**
	 * Gets an object that throws an exception when an input byte sequence cannot be
	 * decoded.
	 * @return A type derived from the  class. The default value is a  object.
	 */
	static var ExceptionFallback(default, never):cs.system.text.DecoderFallback;
	/**
	 * Gets an object that outputs a substitute string in place of an input byte
	 * sequence that cannot be decoded.
	 * @return A type derived from the  class. The default value is a  object that
	 * emits the QUESTION MARK character ("?", U+003F) in place of unknown byte
	 * sequences.
	 */
	static var ReplacementFallback(default, never):cs.system.text.DecoderFallback;
	/**
	 * When overridden in a derived class, gets the maximum number of characters the
	 * current  object can return.
	 * @return The maximum number of characters the current  object can return.
	 */
	var MaxCharCount(default, never):Int;
	/**
	 * When overridden in a derived class, initializes a new instance of the  class.
	 * @return An object that provides a fallback buffer for a decoder.
	 */
	function CreateFallbackBuffer():cs.system.text.DecoderFallbackBuffer;
}
