package cs.system.io.enumeration;

/** Provides methods for matching file system names. */
@:native("System.IO.Enumeration.FileSystemName")
extern class FileSystemName {
	/**
	 * Verifies if the given expression matches the given name. Supports the following
	 * wildcards: '*' and '?'. The backslash character '' escapes.
	 * @param expression The expression to match with.
	 * @param name The name to check against the expression.
	 * @param ignoreCase to ignore case (default);  if the match should be
	 * case-sensitive.
	 * @return if the given expression matches the given name; otherwise, .
	 */
	static function MatchesSimpleExpression(expression:cs.system.ReadOnlySpan<cs.Char16>, name:cs.system.ReadOnlySpan<cs.Char16>, ?ignoreCase:Bool):Bool;
	/**
	 * Verifies if the given Win32 expression matches the given name. Supports the
	 * following wildcards: '*', '?', '<', '>', '"'. The backslash character ''
	 * escapes.
	 * @param expression The expression to match with, such as "*.foo".
	 * @param name The name to check against the expression.
	 * @param ignoreCase to ignore case (default),  if the match should be
	 * case-sensitive.
	 * @return if the given expression matches the given name; otherwise, .
	 */
	static function MatchesWin32Expression(expression:cs.system.ReadOnlySpan<cs.Char16>, name:cs.system.ReadOnlySpan<cs.Char16>, ?ignoreCase:Bool):Bool;
	/**
	 * Translates the given Win32 expression. Change '*' and '?' to '<', '>' and '"' to
	 * match Win32 behavior.
	 * @param expression The expression to translate.
	 * @return A string with the translated Win32 expression.
	 */
	static function TranslateWin32Expression(expression:String):String;
}
