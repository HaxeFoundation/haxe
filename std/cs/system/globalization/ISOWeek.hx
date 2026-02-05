package cs.system.globalization;

/** Provides static members to support the ISO week date that is part of the ISO 8601 date and time standard issued by the International Organization for Standardization (ISO). */
@:native("System.Globalization.ISOWeek")
extern class ISOWeek {
	/**
	 * Calculates the ISO week number of a given Gregorian date.
	 * @param date A date in the Gregorian calendar.
	 * @return A number between 1 and 53 represnting the ISO week number of the given
	 * Gregorian date.
	 */
	static function GetWeekOfYear(date:cs.system.DateTime):Int;
	/**
	 * calculates the number of weeks in a given ISO week-numbering year.
	 * @param year An ISO week-numbering year (also called an ISO year informally).
	 * @return The number of ISO weeks in the year. Returns either 52 or 53.
	 */
	static function GetWeeksInYear(year:Int):Int;
	/**
	 * Calculates the ISO week-numbering year (also called ISO year informally) mapped
	 * to the input Gregorian date.
	 * @param date A date in the Gregorian calendar.
	 * @return The ISO week-numbering year, between 1 and 9999
	 */
	static function GetYear(date:cs.system.DateTime):Int;
	/**
	 * Calculates the Gregorian date at which the week-numbering year will end.
	 * @param year An ISO week-numbering year (also called an ISO year informally).
	 * @return The Gregorian date at which the week-numbering year will end.
	 */
	static function GetYearEnd(year:Int):cs.system.DateTime;
	/**
	 * Calculates the Gregorian date at which the week-numbering year will start.
	 * @param year An ISO week-numbering year (also called an ISO year informally).
	 * @return The Gregorian date at which the week-numbering year will start.
	 */
	static function GetYearStart(year:Int):cs.system.DateTime;
	/**
	 * Maps the ISO week date represented by a specified ISO year, week number, and day
	 * of week to the equivalent Gregorian date.
	 * @param year An ISO week-numbering year (also called an ISO year informally).
	 * @param week The ISO week number in the given ISO week-numbering year.
	 * @param dayOfWeek The day of week inside the given ISO week.
	 * @return The Gregorian date equivalent to the input ISO week date.
	 */
	static function ToDateTime(year:Int, week:Int, dayOfWeek:cs.system.DayOfWeek):cs.system.DateTime;
}
