package cs.system.text;

/** The exception that is thrown when an encoder fallback operation fails. This class cannot be inherited. */
@:native("System.Text.EncoderFallbackException")
extern class EncoderFallbackException extends cs.system.ArgumentException {
	/**
	 * Gets the input character that caused the exception.
	 * @return The character that cannot be encoded.
	 */
	var CharUnknown(default, never):cs.Char16;
	/**
	 * Gets the high component character of the surrogate pair that caused the
	 * exception.
	 * @return The high component character of the surrogate pair that cannot be
	 * encoded.
	 */
	var CharUnknownHigh(default, never):cs.Char16;
	/**
	 * Gets the low component character of the surrogate pair that caused the
	 * exception.
	 * @return The low component character of the surrogate pair that cannot be
	 * encoded.
	 */
	var CharUnknownLow(default, never):cs.Char16;
	/**
	 * Gets the index position in the input buffer of the character that caused the
	 * exception.
	 * @return The index position in the input buffer of the character that cannot be
	 * encoded.
	 */
	var Index(default, never):Int;
	@:overload(function():Void {})
	@:overload(function(message:String):Void {})
	function new(message:String, innerException:cs.system.Exception):Void;
	/**
	 * Indicates whether the input that caused the exception is a surrogate pair.
	 * @return if the input was a surrogate pair; otherwise, .
	 */
	function IsUnknownSurrogate():Bool;
}
