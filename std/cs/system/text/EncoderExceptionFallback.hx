package cs.system.text;

/** Provides a failure-handling mechanism, called a fallback, for an input character that cannot be converted to an output byte sequence. The fallback throws an exception if an input character cannot be converted to an output byte sequence. This class cannot be inherited. */
@:native("System.Text.EncoderExceptionFallback")
extern class EncoderExceptionFallback extends cs.system.text.EncoderFallback {
	function new():Void;
	/**
	 * Returns an encoder fallback buffer that throws an exception if it cannot convert
	 * a character sequence to a byte sequence.
	 * @return An encoder fallback buffer that throws an exception when it cannot
	 * encode a character sequence.
	 */
	function CreateFallbackBuffer():cs.system.text.EncoderFallbackBuffer;
	/**
	 * Indicates whether the current  object and a specified object are equal.
	 * @param value An object that derives from the  class.
	 * @return if  is not  ( in Visual Basic .NET) and is a  object; otherwise, .
	 */
	function Equals(value:Dynamic):Bool;
	/**
	 * Retrieves the hash code for this instance.
	 * @return The return value is always the same arbitrary value, and has no special
	 * significance.
	 */
	function GetHashCode():Int;
}
