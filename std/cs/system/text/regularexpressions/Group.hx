package cs.system.text.regularexpressions;

/** Represents the results from a single capturing group. */
@:native("System.Text.RegularExpressions.Group")
extern class Group extends cs.system.text.regularexpressions.Capture {
	/**
	 * Gets a collection of all the captures matched by the capturing group, in
	 * innermost-leftmost-first order (or innermost-rightmost-first order if the
	 * regular expression is modified with the  option). The collection may have zero
	 * or more items.
	 * @return The collection of substrings matched by the group.
	 */
	var Captures(default, never):cs.system.text.regularexpressions.CaptureCollection;
	/**
	 * Returns the name of the capturing group represented by the current instance.
	 * @return The name of the capturing group represented by the current instance.
	 */
	var Name(default, never):String;
	/**
	 * Gets a value indicating whether the match is successful.
	 * @return if the match is successful; otherwise, .
	 */
	var Success(default, never):Bool;
	/**
	 * Returns a  object equivalent to the one supplied that is safe to share between
	 * multiple threads.
	 * @param inner The input  object.
	 * @return A regular expression  object.
	 */
	static function Synchronized(inner:cs.system.text.regularexpressions.Group):cs.system.text.regularexpressions.Group;
}
