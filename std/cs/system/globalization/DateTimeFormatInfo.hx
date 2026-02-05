package cs.system.globalization;

/** Provides culture-specific information about the format of date and time values. */
@:native("System.Globalization.DateTimeFormatInfo")
extern class DateTimeFormatInfo {
	/**
	 * Gets a read-only  object that formats values based on the current culture.
	 * @return A read-only  object based on the  object for the current thread.
	 */
	static var CurrentInfo(default, never):cs.system.globalization.DateTimeFormatInfo;
	/**
	 * Gets the default read-only  object that is culture-independent (invariant).
	 * @return A read-only object that is culture-independent (invariant).
	 */
	static var InvariantInfo(default, never):cs.system.globalization.DateTimeFormatInfo;
	/**
	 * Gets or sets a one-dimensional array of type  containing the culture-specific
	 * abbreviated names of the days of the week.
	 * @return A one-dimensional array of type  containing the culture-specific
	 * abbreviated names of the days of the week. The array for  contains "Sun", "Mon",
	 * "Tue", "Wed", "Thu", "Fri", and "Sat".
	 */
	var AbbreviatedDayNames(default, default):cs.NativeArray<String>;
	/**
	 * Gets or sets a string array of abbreviated month names associated with the
	 * current  object.
	 * @return An array of abbreviated month names.
	 */
	var AbbreviatedMonthGenitiveNames(default, default):cs.NativeArray<String>;
	/**
	 * Gets or sets a one-dimensional string array that contains the culture-specific
	 * abbreviated names of the months.
	 * @return A one-dimensional string array with 13 elements that contains the
	 * culture-specific abbreviated names of the months. For 12-month calendars, the
	 * 13th element of the array is an empty string. The array for  contains "Jan",
	 * "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec", and
	 * "".
	 */
	var AbbreviatedMonthNames(default, default):cs.NativeArray<String>;
	/**
	 * Gets or sets the string designator for hours that are "ante meridiem" (before
	 * noon).
	 * @return The string designator for hours that are ante meridiem. The default for 
	 * is "AM".
	 */
	var AMDesignator(default, default):String;
	/**
	 * Gets or sets the calendar to use for the current culture.
	 * @return The calendar to use for the current culture. The default for  is a 
	 * object.
	 */
	var Calendar(default, default):cs.system.globalization.Calendar;
	/**
	 * Gets or sets a value that specifies which rule is used to determine the first
	 * calendar week of the year.
	 * @return A value that determines the first calendar week of the year. The default
	 * for  is .
	 */
	var CalendarWeekRule(default, default):cs.system.globalization.CalendarWeekRule;
	/**
	 * Gets or sets the string that separates the components of a date, that is, the
	 * year, month, and day.
	 * @return The string that separates the components of a date, that is, the year,
	 * month, and day. The default for  is "/".
	 */
	var DateSeparator(default, default):String;
	/**
	 * Gets or sets a one-dimensional string array that contains the culture-specific
	 * full names of the days of the week.
	 * @return A one-dimensional string array that contains the culture-specific full
	 * names of the days of the week. The array for  contains "Sunday", "Monday",
	 * "Tuesday", "Wednesday", "Thursday", "Friday", and "Saturday".
	 */
	var DayNames(default, default):cs.NativeArray<String>;
	/**
	 * Gets or sets the first day of the week.
	 * @return An enumeration value that represents the first day of the week. The
	 * default for  is .
	 */
	var FirstDayOfWeek(default, default):cs.system.DayOfWeek;
	/**
	 * Gets or sets the custom format string for a long date and long time value.
	 * @return The custom format string for a long date and long time value.
	 */
	var FullDateTimePattern(default, default):String;
	/**
	 * Gets a value indicating whether the  object is read-only.
	 * @return if the  object is read-only; otherwise, .
	 */
	var IsReadOnly(default, never):Bool;
	/**
	 * Gets or sets the custom format string for a long date value.
	 * @return The custom format string for a long date value.
	 */
	var LongDatePattern(default, default):String;
	/**
	 * Gets or sets the custom format string for a long time value.
	 * @return The format pattern for a long time value.
	 */
	var LongTimePattern(default, default):String;
	/**
	 * Gets or sets the custom format string for a month and day value.
	 * @return The custom format string for a month and day value.
	 */
	var MonthDayPattern(default, default):String;
	/**
	 * Gets or sets a string array of month names associated with the current  object.
	 * @return A string array of month names.
	 */
	var MonthGenitiveNames(default, default):cs.NativeArray<String>;
	/**
	 * Gets or sets a one-dimensional array of type  containing the culture-specific
	 * full names of the months.
	 * @return A one-dimensional array of type  containing the culture-specific full
	 * names of the months. In a 12-month calendar, the 13th element of the array is an
	 * empty string. The array for  contains "January", "February", "March", "April",
	 * "May", "June", "July", "August", "September", "October", "November", "December",
	 * and "".
	 */
	var MonthNames(default, default):cs.NativeArray<String>;
	/**
	 * Gets the native name of the calendar associated with the current  object.
	 * @return The native name of the calendar used in the culture associated with the
	 * current  object if that name is available, or the empty string ("") if the
	 * native calendar name is not available.
	 */
	var NativeCalendarName(default, never):String;
	/**
	 * Gets or sets the string designator for hours that are "post meridiem" (after
	 * noon).
	 * @return The string designator for hours that are "post meridiem" (after noon).
	 * The default for  is "PM".
	 */
	var PMDesignator(default, default):String;
	/**
	 * Gets the custom format string for a time value that is based on the Internet
	 * Engineering Task Force (IETF) Request for Comments (RFC) 1123 specification.
	 * @return The custom format string for a time value that is based on the IETF RFC
	 * 1123 specification.
	 */
	var RFC1123Pattern(default, never):String;
	/**
	 * Gets or sets the custom format string for a short date value.
	 * @return The custom format string for a short date value.
	 */
	var ShortDatePattern(default, default):String;
	/**
	 * Gets or sets a string array of the shortest unique abbreviated day names
	 * associated with the current  object.
	 * @return A string array of day names.
	 */
	var ShortestDayNames(default, default):cs.NativeArray<String>;
	/**
	 * Gets or sets the custom format string for a short time value.
	 * @return The custom format string for a short time value.
	 */
	var ShortTimePattern(default, default):String;
	/**
	 * Gets the custom format string for a sortable date and time value.
	 * @return The custom format string for a sortable date and time value.
	 */
	var SortableDateTimePattern(default, never):String;
	/**
	 * Gets or sets the string that separates the components of time, that is, the
	 * hour, minutes, and seconds.
	 * @return The string that separates the components of time. The default for  is
	 * ":".
	 */
	var TimeSeparator(default, default):String;
	/**
	 * Gets the custom format string for a universal, sortable date and time string, as
	 * defined by ISO 8601.
	 * @return The custom format string for a universal, sortable date and time string.
	 */
	var UniversalSortableDateTimePattern(default, never):String;
	/**
	 * Gets or sets the custom format string for a year and month value.
	 * @return The custom format string for a year and month value.
	 */
	var YearMonthPattern(default, default):String;
	function new():Void;
	/**
	 * Returns the  object associated with the specified .
	 * @param provider The  that gets the  object. -or- to get .
	 * @return A  object associated with .
	 */
	static function GetInstance(provider:cs.system.IFormatProvider):cs.system.globalization.DateTimeFormatInfo;
	/**
	 * Returns a read-only  wrapper.
	 * @param dtfi The  object to wrap.
	 * @return A read-only  wrapper.
	 */
	static function ReadOnly(dtfi:cs.system.globalization.DateTimeFormatInfo):cs.system.globalization.DateTimeFormatInfo;
	/**
	 * Creates a shallow copy of the .
	 * @return A new  object copied from the original .
	 */
	function Clone():Dynamic;
	/**
	 * Returns the culture-specific abbreviated name of the specified day of the week
	 * based on the culture associated with the current  object.
	 * @param dayofweek A  value.
	 * @return The culture-specific abbreviated name of the day of the week represented
	 * by .
	 */
	function GetAbbreviatedDayName(dayofweek:cs.system.DayOfWeek):String;
	/**
	 * Returns the string containing the abbreviated name of the specified era, if an
	 * abbreviation exists.
	 * @param era The integer representing the era.
	 * @return A string containing the abbreviated name of the specified era, if an
	 * abbreviation exists. -or- A string containing the full name of the era, if an
	 * abbreviation does not exist.
	 */
	function GetAbbreviatedEraName(era:Int):String;
	/**
	 * Returns the culture-specific abbreviated name of the specified month based on
	 * the culture associated with the current  object.
	 * @param month An integer from 1 through 13 representing the name of the month to
	 * retrieve.
	 * @return The culture-specific abbreviated name of the month represented by .
	 */
	function GetAbbreviatedMonthName(month:Int):String;
	@:overload(function():cs.NativeArray<String> {})
	/**
	 * Returns all the standard patterns in which date and time values can be
	 * formatted.
	 * @return An array that contains the standard patterns in which date and time
	 * values can be formatted.
	 */
	function GetAllDateTimePatterns(format:cs.Char16):cs.NativeArray<String>;
	/**
	 * Returns the culture-specific full name of the specified day of the week based on
	 * the culture associated with the current  object.
	 * @param dayofweek A  value.
	 * @return The culture-specific full name of the day of the week represented by .
	 */
	function GetDayName(dayofweek:cs.system.DayOfWeek):String;
	/**
	 * Returns the integer representing the specified era.
	 * @param eraName The string containing the name of the era.
	 * @return The integer representing the era, if  is valid; otherwise, -1.
	 */
	function GetEra(eraName:String):Int;
	/**
	 * Returns the string containing the name of the specified era.
	 * @param era The integer representing the era.
	 * @return A string containing the name of the era.
	 */
	function GetEraName(era:Int):String;
	/**
	 * Returns an object of the specified type that provides a date and time 
	 * formatting service.
	 * @param formatType The type of the required formatting service.
	 * @return The current  object, if  is the same as the type of the current ;
	 * otherwise, .
	 */
	function GetFormat(formatType:cs.system.Type):Dynamic;
	/**
	 * Returns the culture-specific full name of the specified month based on the
	 * culture associated with the current  object.
	 * @param month An integer from 1 through 13 representing the name of the month to
	 * retrieve.
	 * @return The culture-specific full name of the month represented by .
	 */
	function GetMonthName(month:Int):String;
	/**
	 * Obtains the shortest abbreviated day name for a specified day of the week
	 * associated with the current  object.
	 * @param dayOfWeek One of the  values.
	 * @return The abbreviated name of the week that corresponds to the  parameter.
	 */
	function GetShortestDayName(dayOfWeek:cs.system.DayOfWeek):String;
	/**
	 * Sets the custom date and time format strings that correspond to a specified
	 * standard format string.
	 * @param patterns An array of custom format strings.
	 * @param format The standard format string associated with the custom format
	 * strings specified in the  parameter.
	 */
	function SetAllDateTimePatterns(patterns:cs.NativeArray<String>, format:cs.Char16):Void;
}
