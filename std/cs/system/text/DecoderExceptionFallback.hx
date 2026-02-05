package cs.system.text;

/** Provides a failure-handling mechanism, called a fallback, for an encoded input byte sequence that cannot be converted to an input character. The fallback throws an exception instead of decoding the input byte sequence. This class cannot be inherited. */
@:native("System.Text.DecoderExceptionFallback")
extern class DecoderExceptionFallback extends cs.system.text.DecoderFallback {
	function new():Void;
	/**
	 * Returns a decoder fallback buffer that throws an exception if it cannot convert
	 * a sequence of bytes to a character.
	 * @return A decoder fallback buffer that throws an exception when it cannot decode
	 * a byte sequence.
	 */
	function CreateFallbackBuffer():cs.system.text.DecoderFallbackBuffer;
	/**
	 * Indicates whether the current  object and a specified object are equal.
	 * @param value An object that derives from the  class.
	 * @return if  is not  and is a  object; otherwise, .
	 */
	function Equals(value:Dynamic):Bool;
	/**
	 * Retrieves the hash code for this instance.
	 * @return The return value is always the same arbitrary value, and has no special
	 * significance.
	 */
	function GetHashCode():Int;
}
