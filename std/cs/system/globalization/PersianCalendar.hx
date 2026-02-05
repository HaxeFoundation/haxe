package cs.system.globalization;

/** Represents the Persian calendar. */
@:native("System.Globalization.PersianCalendar")
extern class PersianCalendar extends cs.system.globalization.Calendar {
	/** Represents the current era. This field is constant. */
	static var PersianEra(default, never):Int;
	function new():Void;
	/**
	 * Returns a  object that is offset the specified number of months from the
	 * specified  object.
	 * @param time The  to which to add months.
	 * @param months The positive or negative number of months to add.
	 * @return A  object that represents the date yielded by adding the number of
	 * months specified by the  parameter to the date specified by the  parameter.
	 */
	function AddMonths(time:cs.system.DateTime, months:Int):cs.system.DateTime;
	/**
	 * Returns a  object that is offset the specified number of years from the
	 * specified  object.
	 * @param time The  to which to add years.
	 * @param years The positive or negative number of years to add.
	 * @return The  object that results from adding the specified number of years to
	 * the specified  object.
	 */
	function AddYears(time:cs.system.DateTime, years:Int):cs.system.DateTime;
	/**
	 * Returns the day of the month in the specified  object.
	 * @param time The  to read.
	 * @return An integer from 1 through 31 that represents the day of the month in the
	 * specified  object.
	 */
	function GetDayOfMonth(time:cs.system.DateTime):Int;
	/**
	 * Returns the day of the week in the specified  object.
	 * @param time The  to read.
	 * @return A  value that represents the day of the week in the specified  object.
	 */
	function GetDayOfWeek(time:cs.system.DateTime):cs.system.DayOfWeek;
	/**
	 * Returns the day of the year in the specified  object.
	 * @param time The  to read.
	 * @return An integer from 1 through 366 that represents the day of the year in the
	 * specified  object.
	 */
	function GetDayOfYear(time:cs.system.DateTime):Int;
	/**
	 * Returns the number of days in the specified month of the specified year and era.
	 * @param year An integer from 1 through 9378 that represents the year.
	 * @param month An integer that represents the month, and ranges from 1 through 12
	 * if  is not 9378, or 1 through 10 if  is 9378.
	 * @param era An integer from 0 through 1 that represents the era.
	 * @return The number of days in the specified month of the specified year and era.
	 */
	function GetDaysInMonth(year:Int, month:Int, era:Int):Int;
	/**
	 * Returns the number of days in the specified year of the specified era.
	 * @param year An integer from 1 through 9378 that represents the year.
	 * @param era An integer from 0 through 1 that represents the era.
	 * @return The number of days in the specified year and era. The number of days is
	 * 365 in a common year or 366 in a leap year.
	 */
	function GetDaysInYear(year:Int, era:Int):Int;
	/**
	 * Returns the era in the specified  object.
	 * @param time The  to read.
	 * @return Always returns .
	 */
	function GetEra(time:cs.system.DateTime):Int;
	/**
	 * Returns the leap month for a specified year and era.
	 * @param year An integer from 1 through 9378 that represents the year to convert.
	 * @param era An integer from 0 through 1 that represents the era.
	 * @return The return value is always 0.
	 */
	function GetLeapMonth(year:Int, era:Int):Int;
	/**
	 * Returns the month in the specified  object.
	 * @param time The  to read.
	 * @return An integer from 1 through 12 that represents the month in the specified 
	 * object.
	 */
	function GetMonth(time:cs.system.DateTime):Int;
	/**
	 * Returns the number of months in the specified year of the specified era.
	 * @param year An integer from 1 through 9378 that represents the year.
	 * @param era An integer from 0 through 1 that represents the era.
	 * @return Returns 10 if the  parameter is 9378; otherwise, always returns 12.
	 */
	function GetMonthsInYear(year:Int, era:Int):Int;
	/**
	 * Returns the year in the specified  object.
	 * @param time The  to read.
	 * @return An integer from 1 through 9378 that represents the year in the specified
	 * .
	 */
	function GetYear(time:cs.system.DateTime):Int;
	/**
	 * Determines whether the specified date is a leap day.
	 * @param year An integer from 1 through 9378 that represents the year.
	 * @param month An integer that represents the month and ranges from 1 through 12
	 * if  is not 9378, or 1 through 10 if  is 9378.
	 * @param day An integer from 1 through 31 that represents the day.
	 * @param era An integer from 0 through 1 that represents the era.
	 * @return if the specified day is a leap day; otherwise, .
	 */
	function IsLeapDay(year:Int, month:Int, day:Int, era:Int):Bool;
	/**
	 * Determines whether the specified month in the specified year and era is a leap
	 * month.
	 * @param year An integer from 1 through 9378 that represents the year.
	 * @param month An integer that represents the month and ranges from 1 through 12
	 * if  is not 9378, or 1 through 10 if  is 9378.
	 * @param era An integer from 0 through 1 that represents the era.
	 * @return Always returns  because the  class does not support the notion of a leap
	 * month.
	 */
	function IsLeapMonth(year:Int, month:Int, era:Int):Bool;
	/**
	 * Determines whether the specified year in the specified era is a leap year.
	 * @param year An integer from 1 through 9378 that represents the year.
	 * @param era An integer from 0 through 1 that represents the era.
	 * @return if the specified year is a leap year; otherwise, .
	 */
	function IsLeapYear(year:Int, era:Int):Bool;
	/**
	 * Returns a  object that is set to the specified date, time, and era.
	 * @param year An integer from 1 through 9378 that represents the year.
	 * @param month An integer from 1 through 12 that represents the month.
	 * @param day An integer from 1 through 31 that represents the day.
	 * @param hour An integer from 0 through 23 that represents the hour.
	 * @param minute An integer from 0 through 59 that represents the minute.
	 * @param second An integer from 0 through 59 that represents the second.
	 * @param millisecond An integer from 0 through 999 that represents the
	 * millisecond.
	 * @param era An integer from 0 through 1 that represents the era.
	 * @return A  object that is set to the specified date and time in the current era.
	 */
	function ToDateTime(year:Int, month:Int, day:Int, hour:Int, minute:Int, second:Int, millisecond:Int, era:Int):cs.system.DateTime;
	/**
	 * Converts the specified year to a four-digit year representation.
	 * @param year An integer from 1 through 9378 that represents the year to convert.
	 * @return An integer that contains the four-digit representation of .
	 */
	function ToFourDigitYear(year:Int):Int;
}
