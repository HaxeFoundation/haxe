package cs.system.globalization;

/** Represents the Hebrew calendar. */
@:native("System.Globalization.HebrewCalendar")
extern class HebrewCalendar extends cs.system.globalization.Calendar {
	/** Represents the current era. This field is constant. */
	static var HebrewEra(default, never):Int;
	function new():Void;
	/**
	 * Returns a  that is the specified number of months away from the specified .
	 * @param time The  to which to add .
	 * @param months The number of months to add.
	 * @return The  that results from adding the specified number of months to the
	 * specified .
	 */
	function AddMonths(time:cs.system.DateTime, months:Int):cs.system.DateTime;
	/**
	 * Returns a  that is the specified number of years away from the specified .
	 * @param time The  to which to add .
	 * @param years The number of years to add.
	 * @return The  that results from adding the specified number of years to the
	 * specified .
	 */
	function AddYears(time:cs.system.DateTime, years:Int):cs.system.DateTime;
	/**
	 * Returns the day of the month in the specified .
	 * @param time The  to read.
	 * @return An integer from 1 to 30 that represents the day of the month in the
	 * specified .
	 */
	function GetDayOfMonth(time:cs.system.DateTime):Int;
	/**
	 * Returns the day of the week in the specified .
	 * @param time The  to read.
	 * @return A  value that represents the day of the week in the specified .
	 */
	function GetDayOfWeek(time:cs.system.DateTime):cs.system.DayOfWeek;
	/**
	 * Returns the day of the year in the specified .
	 * @param time The  to read.
	 * @return An integer from 1 to 385 that represents the day of the year in the
	 * specified .
	 */
	function GetDayOfYear(time:cs.system.DateTime):Int;
	/**
	 * Returns the number of days in the specified month in the specified year in the
	 * specified era.
	 * @param year An integer that represents the year.
	 * @param month An integer from 1 to 13 that represents the month.
	 * @param era An integer that represents the era. Specify either  or .
	 * @return The number of days in the specified month in the specified year in the
	 * specified era.
	 */
	function GetDaysInMonth(year:Int, month:Int, era:Int):Int;
	/**
	 * Returns the number of days in the specified year in the specified era.
	 * @param year An integer that represents the year.
	 * @param era An integer that represents the era. Specify either  or .
	 * @return The number of days in the specified year in the specified era.
	 */
	function GetDaysInYear(year:Int, era:Int):Int;
	/**
	 * Returns the era in the specified .
	 * @param time The  to read.
	 * @return An integer that represents the era in the specified . The return value
	 * is always .
	 */
	function GetEra(time:cs.system.DateTime):Int;
	/**
	 * Calculates the leap month for a specified year and era.
	 * @param year A year.
	 * @param era An era. Specify either  or .
	 * @return A positive integer that indicates the leap month in the specified year
	 * and era. The return value is 7 if the  and  parameters specify a leap year, or 0
	 * if the year is not a leap year.
	 */
	function GetLeapMonth(year:Int, era:Int):Int;
	/**
	 * Returns the month in the specified .
	 * @param time The  to read.
	 * @return An integer from 1 to 13 that represents the month in the specified .
	 */
	function GetMonth(time:cs.system.DateTime):Int;
	/**
	 * Returns the number of months in the specified year in the specified era.
	 * @param year An integer that represents the year.
	 * @param era An integer that represents the era. Specify either  or .
	 * @return The number of months in the specified year in the specified era. The
	 * return value is either 12 in a common year, or 13 in a leap year.
	 */
	function GetMonthsInYear(year:Int, era:Int):Int;
	/**
	 * Returns the year in the specified  value.
	 * @param time The  to read.
	 * @return An integer that represents the year in the specified  value.
	 */
	function GetYear(time:cs.system.DateTime):Int;
	/**
	 * Determines whether the specified date in the specified era is a leap day.
	 * @param year An integer that represents the year.
	 * @param month An integer from 1 to 13 that represents the month.
	 * @param day An integer from 1 to 30 that represents the day.
	 * @param era An integer that represents the era. Specify either  or .
	 * @return if the specified day is a leap day; otherwise, .
	 */
	function IsLeapDay(year:Int, month:Int, day:Int, era:Int):Bool;
	/**
	 * Determines whether the specified month in the specified year in the specified
	 * era is a leap month.
	 * @param year An integer that represents the year.
	 * @param month An integer from 1 to 13 that represents the month.
	 * @param era An integer that represents the era. Specify either  or .
	 * @return if the specified month is a leap month; otherwise, .
	 */
	function IsLeapMonth(year:Int, month:Int, era:Int):Bool;
	/**
	 * Determines whether the specified year in the specified era is a leap year.
	 * @param year An integer that represents the year.
	 * @param era An integer that represents the era. Specify either  or .
	 * @return if the specified year is a leap year; otherwise, .
	 */
	function IsLeapYear(year:Int, era:Int):Bool;
	/**
	 * Returns a  that is set to the specified date and time in the specified era.
	 * @param year An integer that represents the year.
	 * @param month An integer from 1 to 13 that represents the month.
	 * @param day An integer from 1 to 30 that represents the day.
	 * @param hour An integer from 0 to 23 that represents the hour.
	 * @param minute An integer from 0 to 59 that represents the minute.
	 * @param second An integer from 0 to 59 that represents the second.
	 * @param millisecond An integer from 0 to 999 that represents the millisecond.
	 * @param era An integer that represents the era. Specify either  or .
	 * @return The  that is set to the specified date and time in the current era.
	 */
	function ToDateTime(year:Int, month:Int, day:Int, hour:Int, minute:Int, second:Int, millisecond:Int, era:Int):cs.system.DateTime;
	/**
	 * Converts the specified year to a 4-digit year by using the  property to
	 * determine the appropriate century.
	 * @param year A 2-digit year from 0 through 99, or a 4-digit Hebrew calendar year
	 * from 5343 through 5999.
	 * @return If the  parameter is a 2-digit year, the return value is the
	 * corresponding 4-digit year. If the  parameter is a 4-digit year, the return
	 * value is the unchanged  parameter.
	 */
	function ToFourDigitYear(year:Int):Int;
}
