package cs.system.runtime.serialization;

/** Specifies date-time format options. */
@:native("System.Runtime.Serialization.DateTimeFormat")
extern class DateTimeFormat {
	/**
	 * Gets or sets the formatting options that customize string parsing for some date
	 * and time parsing methods.
	 * @return The formatting options that customize string parsing for some date and
	 * time parsing methods.
	 */
	var DateTimeStyles(default, default):cs.system.globalization.DateTimeStyles;
	/** Gets an object that controls formatting. */
	var FormatProvider(default, never):cs.system.IFormatProvider;
	/**
	 * Gets the format strings to control the formatting produced when a date or time
	 * is represented as a string.
	 * @return The format strings to control the formatting produced when a date or
	 * time is represented as a string.
	 */
	var FormatString(default, never):String;
	@:overload(function(formatString:String):Void {})
	function new(formatString:String, formatProvider:cs.system.IFormatProvider):Void;
}
