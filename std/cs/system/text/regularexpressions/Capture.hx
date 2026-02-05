package cs.system.text.regularexpressions;

/** Represents the results from a single successful subexpression capture. */
@:native("System.Text.RegularExpressions.Capture")
extern class Capture {
	/**
	 * The position in the original string where the first character of the captured
	 * substring is found.
	 * @return The zero-based starting position in the original string where the
	 * captured substring is found.
	 */
	var Index(default, never):Int;
	/**
	 * Gets the length of the captured substring.
	 * @return The length of the captured substring.
	 */
	var Length(default, never):Int;
	/**
	 * Gets the captured substring from the input string.
	 * @return The substring that is captured by the match.
	 */
	var Value(default, never):String;
	/**
	 * Retrieves the captured substring from the input string by calling the  property.
	 * @return The substring that was captured by the match.
	 */
	function ToString():String;
}
