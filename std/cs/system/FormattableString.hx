package cs.system;

/** Represents a composite format string, along with the arguments to be formatted. */
@:native("System.FormattableString")
extern class FormattableString {
	/**
	 * Gets the number of arguments to be formatted.
	 * @return The number of arguments to be formatted.
	 */
	var ArgumentCount(default, never):Int;
	/**
	 * Returns the composite format string.
	 * @return The composite format string.
	 */
	var Format(default, never):String;
	/**
	 * Returns a result string in which arguments are formatted by using the
	 * conventions of the invariant culture.
	 * @param formattable The object to convert to a result string.
	 * @return The string that results from formatting the current instance by using
	 * the conventions of the invariant culture.
	 */
	static function Invariant(formattable:cs.system.FormattableString):String;
	/**
	 * Returns the argument at the specified index position.
	 * @param index The index of the argument. Its value can range from zero to one
	 * less than the value of .
	 * @return The argument.
	 */
	function GetArgument(index:Int):Dynamic;
	/**
	 * Returns an object array that contains one or more objects to format.
	 * @return An object array that contains one or more objects to format.
	 */
	function GetArguments():cs.NativeArray<Dynamic>;
	@:overload(function():String {})
	/**
	 * Returns the string that results from formatting the composite format string
	 * along with its arguments by using the formatting conventions of the current
	 * culture.
	 * @return A result string formatted by using the conventions of the current
	 * culture.
	 */
	function ToString(formatProvider:cs.system.IFormatProvider):String;
}
