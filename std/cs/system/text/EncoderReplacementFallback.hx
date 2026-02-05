package cs.system.text;

/** Provides a failure handling mechanism, called a fallback, for an input character that cannot be converted to an output byte sequence. The fallback uses a user-specified replacement string instead of the original input character. This class cannot be inherited. */
@:native("System.Text.EncoderReplacementFallback")
extern class EncoderReplacementFallback extends cs.system.text.EncoderFallback {
	/**
	 * Gets the replacement string that is the value of the  object.
	 * @return A substitute string that is used in place of an input character that
	 * cannot be encoded.
	 */
	var DefaultString(default, never):String;
	@:overload(function():Void {})
	function new(replacement:String):Void;
	/**
	 * Creates a  object that is initialized with the replacement string of this 
	 * object.
	 * @return A  object equal to this  object.
	 */
	function CreateFallbackBuffer():cs.system.text.EncoderFallbackBuffer;
	/**
	 * Indicates whether the value of a specified object is equal to the  object.
	 * @param value A  object.
	 * @return if the  parameter specifies an  object and the replacement string of
	 * that object is equal to the replacement string of this  object; otherwise, .
	 */
	function Equals(value:Dynamic):Bool;
	/**
	 * Retrieves the hash code for the value of the  object.
	 * @return The hash code of the value of the object.
	 */
	function GetHashCode():Int;
}
