package cs.system;

/** Represents a time zone. */
@:native("System.TimeZone")
extern class TimeZone {
	/**
	 * Gets the time zone of the current computer.
	 * @return A  object that represents the current local time zone.
	 */
	static var CurrentTimeZone(default, never):cs.system.TimeZone;
	/**
	 * Gets the daylight saving time zone name.
	 * @return The daylight saving time zone name.
	 */
	var DaylightName(default, never):String;
	/**
	 * Gets the standard time zone name.
	 * @return The standard time zone name.
	 */
	var StandardName(default, never):String;
	/**
	 * Returns a value indicating whether the specified date and time is within a
	 * daylight saving time period.
	 * @param time A date and time.
	 * @return if  is in a daylight saving time period; otherwise, .
	 */
	static function IsDaylightSavingTime(time:cs.system.DateTime, daylightTimes:cs.system.globalization.DaylightTime):Bool;
	/**
	 * Returns the daylight saving time period for a particular year.
	 * @param year The year that the daylight saving time period applies to.
	 * @return A  object that contains the start and end date for daylight saving time
	 * in .
	 */
	function GetDaylightChanges(year:Int):cs.system.globalization.DaylightTime;
	/**
	 * Returns the Coordinated Universal Time (UTC) offset for the specified local
	 * time.
	 * @param time A date and time value.
	 * @return The Coordinated Universal Time (UTC) offset from .
	 */
	function GetUtcOffset(time:cs.system.DateTime):cs.system.TimeSpan;
	/**
	 * Returns a value indicating whether the specified date and time is within a
	 * daylight saving time period.
	 * @param time A date and time.
	 * @return if  is in a daylight saving time period; otherwise, .
	 */
	function IsDaylightSavingTime(time:cs.system.DateTime):Bool;
	/**
	 * Returns the local time that corresponds to a specified date and time value.
	 * @param time A Coordinated Universal Time (UTC) time.
	 * @return A  object whose value is the local time that corresponds to .
	 */
	function ToLocalTime(time:cs.system.DateTime):cs.system.DateTime;
	/**
	 * Returns the Coordinated Universal Time (UTC) that corresponds to a specified
	 * time.
	 * @param time A date and time.
	 * @return A  object whose value is the Coordinated Universal Time (UTC) that
	 * corresponds to .
	 */
	function ToUniversalTime(time:cs.system.DateTime):cs.system.DateTime;
}
