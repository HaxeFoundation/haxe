package cs.system.text;

/** Provides a failure-handling mechanism, called a fallback, for an input character that cannot be converted to an encoded output byte sequence. */
@:native("System.Text.EncoderFallback")
extern class EncoderFallback {
	/**
	 * Gets an object that throws an exception when an input character cannot be
	 * encoded.
	 * @return A type derived from the  class. The default value is a  object.
	 */
	static var ExceptionFallback(default, never):cs.system.text.EncoderFallback;
	/**
	 * Gets an object that outputs a substitute string in place of an input character
	 * that cannot be encoded.
	 * @return A type derived from the  class. The default value is a  object that
	 * replaces unknown input characters with the QUESTION MARK character ("?",
	 * U+003F).
	 */
	static var ReplacementFallback(default, never):cs.system.text.EncoderFallback;
	/**
	 * When overridden in a derived class, gets the maximum number of characters the
	 * current  object can return.
	 * @return The maximum number of characters the current  object can return.
	 */
	var MaxCharCount(default, never):Int;
	/**
	 * When overridden in a derived class, initializes a new instance of the  class.
	 * @return An object that provides a fallback buffer for an encoder.
	 */
	function CreateFallbackBuffer():cs.system.text.EncoderFallbackBuffer;
}
