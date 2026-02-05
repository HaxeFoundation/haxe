package cs.system.text;

/** Provides a failure-handling mechanism, called a fallback, for an encoded input byte sequence that cannot be converted to an output character. The fallback emits a user-specified replacement string instead of a decoded input byte sequence. This class cannot be inherited. */
@:native("System.Text.DecoderReplacementFallback")
extern class DecoderReplacementFallback extends cs.system.text.DecoderFallback {
	/**
	 * Gets the replacement string that is the value of the  object.
	 * @return A substitute string that is emitted in place of an input byte sequence
	 * that cannot be decoded.
	 */
	var DefaultString(default, never):String;
	@:overload(function():Void {})
	function new(replacement:String):Void;
	/**
	 * Creates a  object that is initialized with the replacement string of this 
	 * object.
	 * @return A  object that specifies a string to use instead of the original
	 * decoding operation input.
	 */
	function CreateFallbackBuffer():cs.system.text.DecoderFallbackBuffer;
	/**
	 * Indicates whether the value of a specified object is equal to the  object.
	 * @param value A  object.
	 * @return if  is a  object having a  property that is equal to the  property of
	 * the current  object; otherwise, .
	 */
	function Equals(value:Dynamic):Bool;
	/**
	 * Retrieves the hash code for the value of the  object.
	 * @return The hash code of the value of the object.
	 */
	function GetHashCode():Int;
}
