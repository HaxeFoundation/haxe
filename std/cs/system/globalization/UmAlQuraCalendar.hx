package cs.system.globalization;

/** Represents the Saudi Hijri (Um Al Qura) calendar. */
@:native("System.Globalization.UmAlQuraCalendar")
extern class UmAlQuraCalendar extends cs.system.globalization.Calendar {
	/** Represents the current era. This field is constant. */
	static var UmAlQuraEra(default, never):Int;
	function new():Void;
	/**
	 * Calculates a date that is a specified number of months away from a specified
	 * initial date.
	 * @param time The date to which to add months. The  class supports only dates from
	 * 04/30/1900 00.00.00 (Gregorian date) through 11/16/2077 23:59:59 (Gregorian
	 * date).
	 * @param months The positive or negative number of months to add.
	 * @return The date yielded by adding the number of months specified by the 
	 * parameter to the date specified by the  parameter.
	 */
	function AddMonths(time:cs.system.DateTime, months:Int):cs.system.DateTime;
	/**
	 * Calculates a date that is a specified number of years away from a specified
	 * initial date.
	 * @param time The date to which to add years. The  class supports only dates from
	 * 04/30/1900 00.00.00 (Gregorian date) through 11/16/2077 23:59:59 (Gregorian
	 * date).
	 * @param years The positive or negative number of years to add.
	 * @return The date yielded by adding the number of years specified by the 
	 * parameter to the date specified by the  parameter.
	 */
	function AddYears(time:cs.system.DateTime, years:Int):cs.system.DateTime;
	/**
	 * Calculates the day of the month on which a specified date occurs.
	 * @param time The date value to read. The  class supports only dates from
	 * 04/30/1900 00.00.00 (Gregorian date) through 11/16/2077 23:59:59 (Gregorian
	 * date).
	 * @return An integer from 1 through 30 that represents the day of the month
	 * specified by the  parameter.
	 */
	function GetDayOfMonth(time:cs.system.DateTime):Int;
	/**
	 * Calculates the day of the week on which a specified date occurs.
	 * @param time The date value to read. The  class supports only dates from
	 * 04/30/1900 00.00.00 (Gregorian date) through 11/16/2077 23:59:59 (Gregorian
	 * date).
	 * @return A  value that represents the day of the week specified by the 
	 * parameter.
	 */
	function GetDayOfWeek(time:cs.system.DateTime):cs.system.DayOfWeek;
	/**
	 * Calculates the day of the year on which a specified date occurs.
	 * @param time The date value to read. The  class supports only dates from
	 * 04/30/1900 00.00.00 (Gregorian date) through 11/16/2077 23:59:59 (Gregorian
	 * date).
	 * @return An integer from 1 through 355 that represents the day of the year
	 * specified by the  parameter.
	 */
	function GetDayOfYear(time:cs.system.DateTime):Int;
	/**
	 * Calculates the number of days in the specified month of the specified year and
	 * era.
	 * @param year A year.
	 * @param month An integer from 1 through 12 that represents a month.
	 * @param era An era. Specify  or .
	 * @return The number of days in the specified month in the specified year and era.
	 * The return value is 29 in a common year and 30 in a leap year.
	 */
	function GetDaysInMonth(year:Int, month:Int, era:Int):Int;
	/**
	 * Calculates the number of days in the specified year of the specified era.
	 * @param year A year.
	 * @param era An era. Specify  or .
	 * @return The number of days in the specified year and era. The number of days is
	 * 354 in a common year or 355 in a leap year.
	 */
	function GetDaysInYear(year:Int, era:Int):Int;
	/**
	 * Calculates the era in which a specified date occurs.
	 * @param time The date value to read.
	 * @return Always returns the  value.
	 */
	function GetEra(time:cs.system.DateTime):Int;
	/**
	 * Calculates the leap month for a specified year and era.
	 * @param year A year.
	 * @param era An era. Specify  or .
	 * @return Always 0 because the  class does not support leap months.
	 */
	function GetLeapMonth(year:Int, era:Int):Int;
	/**
	 * Calculates the month in which a specified date occurs.
	 * @param time The date value to read. The  class supports only dates from
	 * 04/30/1900 00.00.00 (Gregorian date) through 11/16/2077 23:59:59 (Gregorian
	 * date).
	 * @return An integer from 1 through 12 that represents the month in the date
	 * specified by the  parameter.
	 */
	function GetMonth(time:cs.system.DateTime):Int;
	/**
	 * Calculates the number of months in the specified year of the specified era.
	 * @param year A year.
	 * @param era An era. Specify  or .
	 * @return Always 12.
	 */
	function GetMonthsInYear(year:Int, era:Int):Int;
	/**
	 * Calculates the year of a date represented by a specified .
	 * @param time The date value to read. The  class supports only dates from
	 * 04/30/1900 00.00.00 (Gregorian date) through 11/16/2077 23:59:59 (Gregorian
	 * date).
	 * @return An integer that represents the year specified by the  parameter.
	 */
	function GetYear(time:cs.system.DateTime):Int;
	/**
	 * Determines whether the specified date is a leap day.
	 * @param year A year.
	 * @param month An integer from 1 through 12 that represents a month.
	 * @param day An integer from 1 through 30 that represents a day.
	 * @param era An era. Specify  or .
	 * @return if the specified day is a leap day; otherwise, . The return value is
	 * always  because the  class does not support leap days.
	 */
	function IsLeapDay(year:Int, month:Int, day:Int, era:Int):Bool;
	/**
	 * Determines whether the specified month in the specified year and era is a leap
	 * month.
	 * @param year A year.
	 * @param month An integer from 1 through 12 that represents a month.
	 * @param era An era. Specify  or .
	 * @return Always  because the  class does not support leap months.
	 */
	function IsLeapMonth(year:Int, month:Int, era:Int):Bool;
	/**
	 * Determines whether the specified year in the specified era is a leap year.
	 * @param year A year.
	 * @param era An era. Specify  or .
	 * @return if the specified year is a leap year; otherwise, .
	 */
	function IsLeapYear(year:Int, era:Int):Bool;
	/**
	 * Returns a  that is set to the specified date, time, and era.
	 * @param year A year.
	 * @param month An integer from 1 through 12 that represents a month.
	 * @param day An integer from 1 through 29 that represents a day.
	 * @param hour An integer from 0 through 23 that represents an hour.
	 * @param minute An integer from 0 through 59 that represents a minute.
	 * @param second An integer from 0 through 59 that represents a second.
	 * @param millisecond An integer from 0 through 999 that represents a millisecond.
	 * @param era An era. Specify  or .
	 * @return The  that is set to the specified date and time in the current era.
	 */
	function ToDateTime(year:Int, month:Int, day:Int, hour:Int, minute:Int, second:Int, millisecond:Int, era:Int):cs.system.DateTime;
	/**
	 * Converts the specified year to a four-digit year by using the  property to
	 * determine the appropriate century.
	 * @param year A 2-digit year from 0 through 99, or a 4-digit Um Al Qura calendar
	 * year from 1318 through 1450.
	 * @return If the  parameter is a 2-digit year, the return value is the
	 * corresponding 4-digit year. If the  parameter is a 4-digit year, the return
	 * value is the unchanged  parameter.
	 */
	function ToFourDigitYear(year:Int):Int;
}
