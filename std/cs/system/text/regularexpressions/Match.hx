package cs.system.text.regularexpressions;

/** Represents the results from a single regular expression match. */
@:native("System.Text.RegularExpressions.Match")
extern class Match extends cs.system.text.regularexpressions.Group {
	/**
	 * Gets the empty group. All failed matches return this empty match.
	 * @return An empty match.
	 */
	static var Empty(default, never):cs.system.text.regularexpressions.Match;
	/**
	 * Gets a collection of groups matched by the regular expression.
	 * @return The character groups matched by the pattern.
	 */
	var Groups(default, never):cs.system.text.regularexpressions.GroupCollection;
	/**
	 * Returns a  instance equivalent to the one supplied that is suitable to share
	 * between multiple threads.
	 * @param inner A regular expression match equivalent to the one expected.
	 * @return A regular expression match that is suitable to share between multiple
	 * threads.
	 */
	static function Synchronized(inner:cs.system.text.regularexpressions.Match):cs.system.text.regularexpressions.Match;
	/**
	 * Returns a new  object with the results for the next match, starting at the
	 * position at which the last match ended (at the character after the last matched
	 * character).
	 * @return The next regular expression match.
	 */
	function NextMatch():cs.system.text.regularexpressions.Match;
	/**
	 * Returns the expansion of the specified replacement pattern.
	 * @param replacement The replacement pattern to use.
	 * @return The expanded version of the  parameter.
	 */
	function Result(replacement:String):String;
}
