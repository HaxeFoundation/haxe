package cs.system.text.regularexpressions;

/** The exception that is thrown when the execution time of a regular expression pattern-matching method exceeds its time-out interval. */
@:native("System.Text.RegularExpressions.RegexMatchTimeoutException")
extern class RegexMatchTimeoutException extends cs.system.TimeoutException {
	/**
	 * Gets the input text that the regular expression engine was processing when the
	 * time-out occurred.
	 * @return The regular expression input text.
	 */
	var Input(default, never):String;
	/**
	 * Gets the time-out interval for a regular expression match.
	 * @return The time-out interval.
	 */
	var MatchTimeout(default, never):cs.system.TimeSpan;
	/**
	 * Gets the regular expression pattern that was used in the matching operation when
	 * the time-out occurred.
	 * @return The regular expression pattern.
	 */
	var Pattern(default, never):String;
	@:overload(function():Void {})
	@:overload(function(message:String):Void {})
	@:overload(function(message:String, inner:cs.system.Exception):Void {})
	function new(regexInput:String, regexPattern:String, matchTimeout:cs.system.TimeSpan):Void;
}
