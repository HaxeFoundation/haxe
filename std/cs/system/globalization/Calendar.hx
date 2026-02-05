package cs.system.globalization;

/** Represents time in divisions, such as weeks, months, and years. */
@:native("System.Globalization.Calendar")
extern class Calendar {
	/** Represents the current era of the current calendar. The value of this field is 0. */
	static var CurrentEra(default, never):Int;
	/**
	 * Gets a value indicating whether the current calendar is solar-based,
	 * lunar-based, or a combination of both.
	 * @return One of the  values.
	 */
	var AlgorithmType(default, never):cs.system.globalization.CalendarAlgorithmType;
	/**
	 * Gets the number of days in the year that precedes the year that is specified by
	 * the  property.
	 * @return The number of days in the year that precedes the year specified by .
	 */
	var DaysInYearBeforeMinSupportedYear(default, never):Int;
	/**
	 * When overridden in a derived class, gets the list of eras in the current
	 * calendar.
	 * @return An array of integers that represents the eras in the current calendar.
	 */
	var Eras(default, never):cs.NativeArray<Int>;
	/**
	 * Gets a value indicating whether this  object is read-only.
	 * @return if this  object is read-only; otherwise, .
	 */
	var IsReadOnly(default, never):Bool;
	/**
	 * Gets the latest date and time supported by this  object.
	 * @return The latest date and time supported by this calendar. The default is .
	 */
	var MaxSupportedDateTime(default, never):cs.system.DateTime;
	/**
	 * Gets the earliest date and time supported by this  object.
	 * @return The earliest date and time supported by this calendar. The default is .
	 */
	var MinSupportedDateTime(default, never):cs.system.DateTime;
	/**
	 * Gets or sets the last year of a 100-year range that can be represented by a
	 * 2-digit year.
	 * @return The last year of a 100-year range that can be represented by a 2-digit
	 * year.
	 */
	var TwoDigitYearMax(default, default):Int;
	/**
	 * Returns a read-only version of the specified  object.
	 * @param calendar A  object.
	 * @return The  object specified by the  parameter, if  is read-only. -or- A
	 * read-only memberwise clone of the  object specified by , if  is not read-only.
	 */
	static function ReadOnly(calendar:cs.system.globalization.Calendar):cs.system.globalization.Calendar;
	/**
	 * Returns a  that is the specified number of days away from the specified .
	 * @param time The  to which to add days.
	 * @param days The number of days to add.
	 * @return The  that results from adding the specified number of days to the
	 * specified .
	 */
	function AddDays(time:cs.system.DateTime, days:Int):cs.system.DateTime;
	/**
	 * Returns a  that is the specified number of hours away from the specified .
	 * @param time The  to which to add hours.
	 * @param hours The number of hours to add.
	 * @return The  that results from adding the specified number of hours to the
	 * specified .
	 */
	function AddHours(time:cs.system.DateTime, hours:Int):cs.system.DateTime;
	/**
	 * Returns a  that is the specified number of milliseconds away from the specified
	 * .
	 * @param time The  to add milliseconds to.
	 * @param milliseconds The number of milliseconds to add.
	 * @return The  that results from adding the specified number of milliseconds to
	 * the specified .
	 */
	function AddMilliseconds(time:cs.system.DateTime, milliseconds:Float):cs.system.DateTime;
	/**
	 * Returns a  that is the specified number of minutes away from the specified .
	 * @param time The  to which to add minutes.
	 * @param minutes The number of minutes to add.
	 * @return The  that results from adding the specified number of minutes to the
	 * specified .
	 */
	function AddMinutes(time:cs.system.DateTime, minutes:Int):cs.system.DateTime;
	/**
	 * When overridden in a derived class, returns a  that is the specified number of
	 * months away from the specified .
	 * @param time The  to which to add months.
	 * @param months The number of months to add.
	 * @return The  that results from adding the specified number of months to the
	 * specified .
	 */
	function AddMonths(time:cs.system.DateTime, months:Int):cs.system.DateTime;
	/**
	 * Returns a  that is the specified number of seconds away from the specified .
	 * @param time The  to which to add seconds.
	 * @param seconds The number of seconds to add.
	 * @return The  that results from adding the specified number of seconds to the
	 * specified .
	 */
	function AddSeconds(time:cs.system.DateTime, seconds:Int):cs.system.DateTime;
	/**
	 * Returns a  that is the specified number of weeks away from the specified .
	 * @param time The  to which to add weeks.
	 * @param weeks The number of weeks to add.
	 * @return The  that results from adding the specified number of weeks to the
	 * specified .
	 */
	function AddWeeks(time:cs.system.DateTime, weeks:Int):cs.system.DateTime;
	/**
	 * When overridden in a derived class, returns a  that is the specified number of
	 * years away from the specified .
	 * @param time The  to which to add years.
	 * @param years The number of years to add.
	 * @return The  that results from adding the specified number of years to the
	 * specified .
	 */
	function AddYears(time:cs.system.DateTime, years:Int):cs.system.DateTime;
	/**
	 * Creates a new object that is a copy of the current  object.
	 * @return A new instance of  that is the memberwise clone of the current  object.
	 */
	function Clone():Dynamic;
	/**
	 * When overridden in a derived class, returns the day of the month in the
	 * specified .
	 * @param time The  to read.
	 * @return A positive integer that represents the day of the month in the 
	 * parameter.
	 */
	function GetDayOfMonth(time:cs.system.DateTime):Int;
	/**
	 * When overridden in a derived class, returns the day of the week in the specified
	 * .
	 * @param time The  to read.
	 * @return A  value that represents the day of the week in the  parameter.
	 */
	function GetDayOfWeek(time:cs.system.DateTime):cs.system.DayOfWeek;
	/**
	 * When overridden in a derived class, returns the day of the year in the specified
	 * .
	 * @param time The  to read.
	 * @return A positive integer that represents the day of the year in the 
	 * parameter.
	 */
	function GetDayOfYear(time:cs.system.DateTime):Int;
	@:overload(function(year:Int, month:Int):Int {})
	/**
	 * Returns the number of days in the specified month and year of the current era.
	 * @param year An integer that represents the year.
	 * @param month A positive integer that represents the month.
	 * @return The number of days in the specified month in the specified year in the
	 * current era.
	 */
	function GetDaysInMonth(year:Int, month:Int, era:Int):Int;
	@:overload(function(year:Int):Int {})
	/**
	 * Returns the number of days in the specified year of the current era.
	 * @param year An integer that represents the year.
	 * @return The number of days in the specified year in the current era.
	 */
	function GetDaysInYear(year:Int, era:Int):Int;
	/**
	 * When overridden in a derived class, returns the era of the specified .
	 * @param time The  to read.
	 * @return An integer that represents the era of .
	 */
	function GetEra(time:cs.system.DateTime):Int;
	/**
	 * Returns the hours value in the specified .
	 * @param time The  to read.
	 * @return An integer from 0 to 23 that represents the hour in .
	 */
	function GetHour(time:cs.system.DateTime):Int;
	@:overload(function(year:Int):Int {})
	/**
	 * Calculates the leap month for a specified year.
	 * @param year A year.
	 * @return A positive integer that indicates the leap month in the specified year.
	 * -or- Zero if this calendar does not support a leap month or if the  parameter
	 * does not represent a leap year.
	 */
	function GetLeapMonth(year:Int, era:Int):Int;
	/**
	 * Returns the milliseconds value in the specified .
	 * @param time The  to read.
	 * @return A double-precision floating-point number from 0 to 999 that represents
	 * the milliseconds in the  parameter.
	 */
	function GetMilliseconds(time:cs.system.DateTime):Float;
	/**
	 * Returns the minutes value in the specified .
	 * @param time The  to read.
	 * @return An integer from 0 to 59 that represents the minutes in .
	 */
	function GetMinute(time:cs.system.DateTime):Int;
	/**
	 * When overridden in a derived class, returns the month in the specified .
	 * @param time The  to read.
	 * @return A positive integer that represents the month in .
	 */
	function GetMonth(time:cs.system.DateTime):Int;
	@:overload(function(year:Int):Int {})
	/**
	 * Returns the number of months in the specified year in the current era.
	 * @param year An integer that represents the year.
	 * @return The number of months in the specified year in the current era.
	 */
	function GetMonthsInYear(year:Int, era:Int):Int;
	/**
	 * Returns the seconds value in the specified .
	 * @param time The  to read.
	 * @return An integer from 0 to 59 that represents the seconds in .
	 */
	function GetSecond(time:cs.system.DateTime):Int;
	/**
	 * Returns the week of the year that includes the date in the specified  value.
	 * @param time A date and time value.
	 * @param rule An enumeration value that defines a calendar week.
	 * @param firstDayOfWeek An enumeration value that represents the first day of the
	 * week.
	 * @return A positive integer that represents the week of the year that includes
	 * the date in the  parameter.
	 */
	function GetWeekOfYear(time:cs.system.DateTime, rule:cs.system.globalization.CalendarWeekRule, firstDayOfWeek:cs.system.DayOfWeek):Int;
	/**
	 * When overridden in a derived class, returns the year in the specified .
	 * @param time The  to read.
	 * @return An integer that represents the year in .
	 */
	function GetYear(time:cs.system.DateTime):Int;
	@:overload(function(year:Int, month:Int, day:Int):Bool {})
	/**
	 * Determines whether the specified date in the current era is a leap day.
	 * @param year An integer that represents the year.
	 * @param month A positive integer that represents the month.
	 * @param day A positive integer that represents the day.
	 * @return if the specified day is a leap day; otherwise, .
	 */
	function IsLeapDay(year:Int, month:Int, day:Int, era:Int):Bool;
	@:overload(function(year:Int, month:Int):Bool {})
	/**
	 * Determines whether the specified month in the specified year in the current era
	 * is a leap month.
	 * @param year An integer that represents the year.
	 * @param month A positive integer that represents the month.
	 * @return if the specified month is a leap month; otherwise, .
	 */
	function IsLeapMonth(year:Int, month:Int, era:Int):Bool;
	@:overload(function(year:Int):Bool {})
	/**
	 * Determines whether the specified year in the current era is a leap year.
	 * @param year An integer that represents the year.
	 * @return if the specified year is a leap year; otherwise, .
	 */
	function IsLeapYear(year:Int, era:Int):Bool;
	@:overload(function(year:Int, month:Int, day:Int, hour:Int, minute:Int, second:Int, millisecond:Int):cs.system.DateTime {})
	/**
	 * Returns a  that is set to the specified date and time in the current era.
	 * @param year An integer that represents the year.
	 * @param month A positive integer that represents the month.
	 * @param day A positive integer that represents the day.
	 * @param hour An integer from 0 to 23 that represents the hour.
	 * @param minute An integer from 0 to 59 that represents the minute.
	 * @param second An integer from 0 to 59 that represents the second.
	 * @param millisecond An integer from 0 to 999 that represents the millisecond.
	 * @return The  that is set to the specified date and time in the current era.
	 */
	function ToDateTime(year:Int, month:Int, day:Int, hour:Int, minute:Int, second:Int, millisecond:Int, era:Int):cs.system.DateTime;
	/**
	 * Converts the specified year to a four-digit year by using the  property to
	 * determine the appropriate century.
	 * @param year A two-digit or four-digit integer that represents the year to
	 * convert.
	 * @return An integer that contains the four-digit representation of .
	 */
	function ToFourDigitYear(year:Int):Int;
}
