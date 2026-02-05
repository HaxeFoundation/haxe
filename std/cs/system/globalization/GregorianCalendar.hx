package cs.system.globalization;

/** Represents the Gregorian calendar. */
@:native("System.Globalization.GregorianCalendar")
extern class GregorianCalendar extends cs.system.globalization.Calendar {
	/** Represents the current era. This field is constant. */
	static var ADEra(default, never):Int;
	/**
	 * Gets or sets the  value that denotes the language version of the current .
	 * @return A  value that denotes the language version of the current .
	 */
	var CalendarType(default, default):cs.system.globalization.GregorianCalendarTypes;
	@:overload(function():Void {})
	function new(type:cs.system.globalization.GregorianCalendarTypes):Void;
	/**
	 * Returns a  that is the specified number of months away from the specified .
	 * @param time The  to which to add months.
	 * @param months The number of months to add.
	 * @return The  that results from adding the specified number of months to the
	 * specified .
	 */
	function AddMonths(time:cs.system.DateTime, months:Int):cs.system.DateTime;
	/**
	 * Returns a  that is the specified number of years away from the specified .
	 * @param time The  to which to add years.
	 * @param years The number of years to add.
	 * @return The  that results from adding the specified number of years to the
	 * specified .
	 */
	function AddYears(time:cs.system.DateTime, years:Int):cs.system.DateTime;
	/**
	 * Returns the day of the month in the specified .
	 * @param time The  to read.
	 * @return An integer from 1 to 31 that represents the day of the month in .
	 */
	function GetDayOfMonth(time:cs.system.DateTime):Int;
	/**
	 * Returns the day of the week in the specified .
	 * @param time The  to read.
	 * @return A  value that represents the day of the week in .
	 */
	function GetDayOfWeek(time:cs.system.DateTime):cs.system.DayOfWeek;
	/**
	 * Returns the day of the year in the specified .
	 * @param time The  to read.
	 * @return An integer from 1 to 366 that represents the day of the year in .
	 */
	function GetDayOfYear(time:cs.system.DateTime):Int;
	/**
	 * Returns the number of days in the specified month in the specified year in the
	 * specified era.
	 * @param year An integer that represents the year.
	 * @param month An integer from 1 to 12 that represents the month.
	 * @param era An integer that represents the era.
	 * @return The number of days in the specified month in the specified year in the
	 * specified era.
	 */
	function GetDaysInMonth(year:Int, month:Int, era:Int):Int;
	/**
	 * Returns the number of days in the specified year in the specified era.
	 * @param year An integer that represents the year.
	 * @param era An integer that represents the era.
	 * @return The number of days in the specified year in the specified era.
	 */
	function GetDaysInYear(year:Int, era:Int):Int;
	/**
	 * Returns the era in the specified .
	 * @param time The  to read.
	 * @return An integer that represents the era in .
	 */
	function GetEra(time:cs.system.DateTime):Int;
	/**
	 * Calculates the leap month for a specified year and era.
	 * @param year A year.
	 * @param era An era. Specify either  or .
	 * @return Always 0 because the Gregorian calendar does not recognize leap months.
	 */
	function GetLeapMonth(year:Int, era:Int):Int;
	/**
	 * Returns the month in the specified .
	 * @param time The  to read.
	 * @return An integer from 1 to 12 that represents the month in .
	 */
	function GetMonth(time:cs.system.DateTime):Int;
	/**
	 * Returns the number of months in the specified year in the specified era.
	 * @param year An integer that represents the year.
	 * @param era An integer that represents the era.
	 * @return The number of months in the specified year in the specified era.
	 */
	function GetMonthsInYear(year:Int, era:Int):Int;
	/**
	 * Returns the year in the specified .
	 * @param time The  to read.
	 * @return An integer that represents the year in .
	 */
	function GetYear(time:cs.system.DateTime):Int;
	/**
	 * Determines whether the specified date in the specified era is a leap day.
	 * @param year An integer that represents the year.
	 * @param month An integer from 1 to 12 that represents the month.
	 * @param day An integer from 1 to 31 that represents the day.
	 * @param era An integer that represents the era.
	 * @return if the specified day is a leap day; otherwise, .
	 */
	function IsLeapDay(year:Int, month:Int, day:Int, era:Int):Bool;
	/**
	 * Determines whether the specified month in the specified year in the specified
	 * era is a leap month.
	 * @param year An integer that represents the year.
	 * @param month An integer from 1 to 12 that represents the month.
	 * @param era An integer that represents the era.
	 * @return This method always returns , unless overridden by a derived class.
	 */
	function IsLeapMonth(year:Int, month:Int, era:Int):Bool;
	/**
	 * Determines whether the specified year in the specified era is a leap year.
	 * @param year An integer that represents the year.
	 * @param era An integer that represents the era.
	 * @return if the specified year is a leap year; otherwise, .
	 */
	function IsLeapYear(year:Int, era:Int):Bool;
	/**
	 * Returns a  that is set to the specified date and time in the specified era.
	 * @param year An integer that represents the year.
	 * @param month An integer from 1 to 12 that represents the month.
	 * @param day An integer from 1 to 31 that represents the day.
	 * @param hour An integer from 0 to 23 that represents the hour.
	 * @param minute An integer from 0 to 59 that represents the minute.
	 * @param second An integer from 0 to 59 that represents the second.
	 * @param millisecond An integer from 0 to 999 that represents the millisecond.
	 * @param era An integer that represents the era.
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
