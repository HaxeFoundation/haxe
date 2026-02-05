package cs.system.text.regularexpressions;

/** Represents an immutable regular expression. */
@:native("System.Text.RegularExpressions.Regex")
extern class Regex {
	/** Specifies that a pattern-matching operation should not time out. */
	static var InfiniteMatchTimeout(default, never):cs.system.TimeSpan;
	/**
	 * Gets or sets the maximum number of entries in the current static cache of
	 * compiled regular expressions.
	 * @return The maximum number of entries in the static cache.
	 */
	static var CacheSize(default, default):Int;
	/**
	 * Gets or sets a dictionary that maps named capturing groups to their index
	 * values.
	 * @return A dictionary that maps named capturing groups to their index values.
	 */
	var CapNames(default, default):cs.system.collections.IDictionary;
	/**
	 * Gets or sets a dictionary that maps numbered capturing groups to their index
	 * values.
	 * @return A dictionary that maps numbered capturing groups to their index values.
	 */
	var Caps(default, default):cs.system.collections.IDictionary;
	/**
	 * Gets the time-out interval of the current instance.
	 * @return The maximum time interval that can elapse in a pattern-matching
	 * operation before a  is thrown, or  if time-outs are disabled.
	 */
	var MatchTimeout(default, never):cs.system.TimeSpan;
	/**
	 * Gets the options that were passed into the  constructor.
	 * @return One or more members of the  enumeration that represent options that were
	 * passed to the  constructor
	 */
	var Options(default, never):cs.system.text.regularexpressions.RegexOptions;
	/**
	 * Gets a value that indicates whether the regular expression searches from right
	 * to left.
	 * @return if the regular expression searches from right to left; otherwise, .
	 */
	var RightToLeft(default, never):Bool;
	@:overload(function(pattern:String):Void {})
	@:overload(function(pattern:String, options:cs.system.text.regularexpressions.RegexOptions):Void {})
	function new(pattern:String, options:cs.system.text.regularexpressions.RegexOptions, matchTimeout:cs.system.TimeSpan):Void;
	/**
	 * Escapes a minimal set of characters (\, *, +, ?, |, {, [, (,), ^, $, ., #, and
	 * white space) by replacing them with their escape codes. This instructs the
	 * regular expression engine to interpret these characters literally rather than as
	 * metacharacters.
	 * @param str The input string that contains the text to convert.
	 * @return A string of characters with metacharacters converted to their escaped
	 * form.
	 */
	static function Escape(str:String):String;
	@:overload(function(input:String, pattern:String):Bool {})
	@:overload(function(input:String, pattern:String, options:cs.system.text.regularexpressions.RegexOptions):Bool {})
	/**
	 * Indicates whether the regular expression specified in the  constructor finds a
	 * match in a specified input string.
	 * @param input The string to search for a match.
	 * @return if the regular expression finds a match; otherwise, .
	 */
	static function IsMatch(input:String, pattern:String, options:cs.system.text.regularexpressions.RegexOptions, matchTimeout:cs.system.TimeSpan):Bool;
	@:overload(function(input:String, pattern:String):cs.system.text.regularexpressions.Match {})
	@:overload(function(input:String, pattern:String, options:cs.system.text.regularexpressions.RegexOptions):cs.system.text.regularexpressions.Match {})
	/**
	 * Searches the specified input string for the first occurrence of the regular
	 * expression specified in the  constructor.
	 * @param input The string to search for a match.
	 * @return An object that contains information about the match.
	 */
	static function Match(input:String, pattern:String, options:cs.system.text.regularexpressions.RegexOptions, matchTimeout:cs.system.TimeSpan):cs.system.text.regularexpressions.Match;
	@:overload(function(input:String, pattern:String):cs.system.text.regularexpressions.MatchCollection {})
	@:overload(function(input:String, pattern:String, options:cs.system.text.regularexpressions.RegexOptions):cs.system.text.regularexpressions.MatchCollection {})
	/**
	 * Searches the specified input string for all occurrences of a regular expression.
	 * @param input The string to search for a match.
	 * @return A collection of the  objects found by the search. If no matches are
	 * found, the method returns an empty collection object.
	 */
	static function Matches(input:String, pattern:String, options:cs.system.text.regularexpressions.RegexOptions, matchTimeout:cs.system.TimeSpan):cs.system.text.regularexpressions.MatchCollection;
	@:overload(function(input:String, pattern:String, replacement:String):String {})
	@:overload(function(input:String, pattern:String, evaluator:cs.system.text.regularexpressions.MatchEvaluator):String {})
	@:overload(function(input:String, pattern:String, replacement:String, options:cs.system.text.regularexpressions.RegexOptions):String {})
	@:overload(function(input:String, pattern:String, evaluator:cs.system.text.regularexpressions.MatchEvaluator, options:cs.system.text.regularexpressions.RegexOptions):String {})
	@:overload(function(input:String, pattern:String, replacement:String, options:cs.system.text.regularexpressions.RegexOptions, matchTimeout:cs.system.TimeSpan):String {})
	/**
	 * In a specified input string, replaces all strings that match a regular
	 * expression pattern with a specified replacement string.
	 * @param input The string to search for a match.
	 * @param replacement The replacement string.
	 * @return A new string that is identical to the input string, except that the
	 * replacement string takes the place of each matched string. If the regular
	 * expression pattern is not matched in the current instance, the method returns
	 * the current instance unchanged.
	 */
	static function Replace(input:String, pattern:String, evaluator:cs.system.text.regularexpressions.MatchEvaluator, options:cs.system.text.regularexpressions.RegexOptions, matchTimeout:cs.system.TimeSpan):String;
	@:overload(function(input:String, pattern:String):cs.NativeArray<String> {})
	@:overload(function(input:String, pattern:String, options:cs.system.text.regularexpressions.RegexOptions):cs.NativeArray<String> {})
	/**
	 * Splits an input string into an array of substrings at the positions defined by a
	 * regular expression pattern specified in the  constructor.
	 * @param input The string to split.
	 * @return An array of strings.
	 */
	static function Split(input:String, pattern:String, options:cs.system.text.regularexpressions.RegexOptions, matchTimeout:cs.system.TimeSpan):cs.NativeArray<String>;
	/**
	 * Converts any escaped characters in the input string.
	 * @param str The input string containing the text to convert.
	 * @return A string of characters with any escaped characters converted to their
	 * unescaped form.
	 */
	static function Unescape(str:String):String;
	/**
	 * Returns an array of capturing group names for the regular expression.
	 * @return A string array of group names.
	 */
	function GetGroupNames():cs.NativeArray<String>;
	/**
	 * Returns an array of capturing group numbers that correspond to group names in an
	 * array.
	 * @return An integer array of group numbers.
	 */
	function GetGroupNumbers():cs.NativeArray<Int>;
	/**
	 * Gets the group name that corresponds to the specified group number.
	 * @param i The group number to convert to the corresponding group name.
	 * @return A string that contains the group name associated with the specified
	 * group number. If there is no group name that corresponds to , the method returns
	 * .
	 */
	function GroupNameFromNumber(i:Int):String;
	/**
	 * Returns the group number that corresponds to the specified group name.
	 * @param name The group name to convert to the corresponding group number.
	 * @return The group number that corresponds to the specified group name, or -1 if 
	 * is not a valid group name.
	 */
	function GroupNumberFromName(name:String):Int;
	@:overload(function(input:String):Bool {})
	/**
	 * Indicates whether the regular expression specified in the  constructor finds a
	 * match in a specified input string.
	 * @param input The string to search for a match.
	 * @return if the regular expression finds a match; otherwise, .
	 */
	function IsMatch(input:String, startat:Int):Bool;
	@:overload(function(input:String):cs.system.text.regularexpressions.Match {})
	@:overload(function(input:String, startat:Int):cs.system.text.regularexpressions.Match {})
	/**
	 * Searches the specified input string for the first occurrence of the regular
	 * expression specified in the  constructor.
	 * @param input The string to search for a match.
	 * @return An object that contains information about the match.
	 */
	function Match(input:String, beginning:Int, length:Int):cs.system.text.regularexpressions.Match;
	@:overload(function(input:String):cs.system.text.regularexpressions.MatchCollection {})
	/**
	 * Searches the specified input string for all occurrences of a regular expression.
	 * @param input The string to search for a match.
	 * @return A collection of the  objects found by the search. If no matches are
	 * found, the method returns an empty collection object.
	 */
	function Matches(input:String, startat:Int):cs.system.text.regularexpressions.MatchCollection;
	@:overload(function(input:String, replacement:String):String {})
	@:overload(function(input:String, evaluator:cs.system.text.regularexpressions.MatchEvaluator):String {})
	@:overload(function(input:String, replacement:String, count:Int):String {})
	@:overload(function(input:String, evaluator:cs.system.text.regularexpressions.MatchEvaluator, count:Int):String {})
	@:overload(function(input:String, replacement:String, count:Int, startat:Int):String {})
	/**
	 * In a specified input string, replaces all strings that match a regular
	 * expression pattern with a specified replacement string.
	 * @param input The string to search for a match.
	 * @param replacement The replacement string.
	 * @return A new string that is identical to the input string, except that the
	 * replacement string takes the place of each matched string. If the regular
	 * expression pattern is not matched in the current instance, the method returns
	 * the current instance unchanged.
	 */
	function Replace(input:String, evaluator:cs.system.text.regularexpressions.MatchEvaluator, count:Int, startat:Int):String;
	@:overload(function(input:String):cs.NativeArray<String> {})
	@:overload(function(input:String, count:Int):cs.NativeArray<String> {})
	/**
	 * Splits an input string into an array of substrings at the positions defined by a
	 * regular expression pattern specified in the  constructor.
	 * @param input The string to split.
	 * @return An array of strings.
	 */
	function Split(input:String, count:Int, startat:Int):cs.NativeArray<String>;
	/**
	 * Returns the regular expression pattern that was passed into the  constructor.
	 * @return The  parameter that was passed into the  constructor.
	 */
	function ToString():String;
}
