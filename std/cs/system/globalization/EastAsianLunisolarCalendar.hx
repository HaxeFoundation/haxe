package cs.system.globalization;

/** Represents a calendar that divides time into months, days, years, and eras, and has dates that are based on cycles of the sun and the moon. */
@:native("System.Globalization.EastAsianLunisolarCalendar")
extern class EastAsianLunisolarCalendar extends cs.system.globalization.Calendar {
	/**
	 * Calculates the date that is the specified number of months away from the
	 * specified date.
	 * @param time The  to which to add .
	 * @param months The number of months to add.
	 * @return A new  that results from adding the specified number of months to the 
	 * parameter.
	 */
	function AddMonths(time:cs.system.DateTime, months:Int):cs.system.DateTime;
	/**
	 * Calculates the date that is the specified number of years away from the
	 * specified date.
	 * @param time The  to which to add .
	 * @param years The number of years to add.
	 * @return A new  that results from adding the specified number of years to the 
	 * parameter.
	 */
	function AddYears(time:cs.system.DateTime, years:Int):cs.system.DateTime;
	/**
	 * Calculates the celestial stem of the specified year in the sexagenary (60-year)
	 * cycle.
	 * @param sexagenaryYear An integer from 1 through 60 that represents a year in the
	 * sexagenary cycle.
	 * @return A number from 1 through 10.
	 */
	function GetCelestialStem(sexagenaryYear:Int):Int;
	/**
	 * Calculates the day of the month in the specified date.
	 * @param time The  to read.
	 * @return An integer from 1 through 31 that represents the day of the month
	 * specified in the  parameter.
	 */
	function GetDayOfMonth(time:cs.system.DateTime):Int;
	/**
	 * Calculates the day of the week in the specified date.
	 * @param time The  to read.
	 * @return One of the  values that represents the day of the week specified in the 
	 * parameter.
	 */
	function GetDayOfWeek(time:cs.system.DateTime):cs.system.DayOfWeek;
	/**
	 * Calculates the day of the year in the specified date.
	 * @param time The  to read.
	 * @return An integer from 1 through 354 in a common year, or 1 through 384 in a
	 * leap year, that represents the day of the year specified in the  parameter.
	 */
	function GetDayOfYear(time:cs.system.DateTime):Int;
	/**
	 * Calculates the number of days in the specified month of the specified year and
	 * era.
	 * @param year An integer that represents the year.
	 * @param month An integer from 1 through 12 in a common year, or 1 through 13 in a
	 * leap year, that represents the month.
	 * @param era An integer that represents the era.
	 * @return The number of days in the specified month of the specified year and era.
	 */
	function GetDaysInMonth(year:Int, month:Int, era:Int):Int;
	/**
	 * Calculates the number of days in the specified year and era.
	 * @param year An integer that represents the year.
	 * @param era An integer that represents the era.
	 * @return The number of days in the specified year and era.
	 */
	function GetDaysInYear(year:Int, era:Int):Int;
	/**
	 * Calculates the leap month for the specified year and era.
	 * @param year An integer that represents the year.
	 * @param era An integer that represents the era.
	 * @return A positive integer from 1 through 13 that indicates the leap month in
	 * the specified year and era. -or- Zero if this calendar does not support a leap
	 * month, or if the  and  parameters do not specify a leap year.
	 */
	function GetLeapMonth(year:Int, era:Int):Int;
	/**
	 * Returns the month in the specified date.
	 * @param time The  to read.
	 * @return An integer from 1 to 13 that represents the month specified in the 
	 * parameter.
	 */
	function GetMonth(time:cs.system.DateTime):Int;
	/**
	 * Calculates the number of months in the specified year and era.
	 * @param year An integer that represents the year.
	 * @param era An integer that represents the era.
	 * @return The number of months in the specified year in the specified era. The
	 * return value is 12 months in a common year or 13 months in a leap year.
	 */
	function GetMonthsInYear(year:Int, era:Int):Int;
	/**
	 * Calculates the year in the sexagenary (60-year) cycle that corresponds to the
	 * specified date.
	 * @param time A  to read.
	 * @return A number from 1 through 60 in the sexagenary cycle that corresponds to
	 * the  parameter.
	 */
	function GetSexagenaryYear(time:cs.system.DateTime):Int;
	/**
	 * Calculates the terrestrial branch of the specified year in the sexagenary
	 * (60-year) cycle.
	 * @param sexagenaryYear An integer from 1 through 60 that represents a year in the
	 * sexagenary cycle.
	 * @return An integer from 1 through 12.
	 */
	function GetTerrestrialBranch(sexagenaryYear:Int):Int;
	/**
	 * Returns the year in the specified date.
	 * @param time The  to read.
	 * @return An integer that represents the year in the specified .
	 */
	function GetYear(time:cs.system.DateTime):Int;
	/**
	 * Determines whether the specified date in the specified era is a leap day.
	 * @param year An integer that represents the year.
	 * @param month An integer from 1 through 13 that represents the month.
	 * @param day An integer from 1 through 31 that represents the day.
	 * @param era An integer that represents the era.
	 * @return if the specified day is a leap day; otherwise, .
	 */
	function IsLeapDay(year:Int, month:Int, day:Int, era:Int):Bool;
	/**
	 * Determines whether the specified month in the specified year and era is a leap
	 * month.
	 * @param year An integer that represents the year.
	 * @param month An integer from 1 through 13 that represents the month.
	 * @param era An integer that represents the era.
	 * @return if the  parameter is a leap month; otherwise, .
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
	 * Returns a  that is set to the specified date, time, and era.
	 * @param year An integer that represents the year.
	 * @param month An integer from 1 through 13 that represents the month.
	 * @param day An integer from 1 through 31 that represents the day.
	 * @param hour An integer from 0 through 23 that represents the hour.
	 * @param minute An integer from 0 through 59 that represents the minute.
	 * @param second An integer from 0 through 59 that represents the second.
	 * @param millisecond An integer from 0 through 999 that represents the
	 * millisecond.
	 * @param era An integer that represents the era.
	 * @return A  that is set to the specified date, time, and era.
	 */
	function ToDateTime(year:Int, month:Int, day:Int, hour:Int, minute:Int, second:Int, millisecond:Int, era:Int):cs.system.DateTime;
	/**
	 * Converts the specified year to a four-digit year.
	 * @param year A two-digit or four-digit integer that represents the year to
	 * convert.
	 * @return An integer that contains the four-digit representation of the 
	 * parameter.
	 */
	function ToFourDigitYear(year:Int):Int;
}
