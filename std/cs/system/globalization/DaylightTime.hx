package cs.system.globalization;

/** Defines the period of daylight saving time. */
@:native("System.Globalization.DaylightTime")
extern class DaylightTime {
	/**
	 * Gets the time interval that represents the difference between standard time and
	 * daylight saving time.
	 * @return The time interval that represents the difference between standard time
	 * and daylight saving time.
	 */
	var Delta(default, never):cs.system.TimeSpan;
	/**
	 * Gets the object that represents the date and time when the daylight saving
	 * period ends.
	 * @return The object that represents the date and time when the daylight saving
	 * period ends. The value is in local time.
	 */
	var End(default, never):cs.system.DateTime;
	/**
	 * Gets the object that represents the date and time when the daylight saving
	 * period begins.
	 * @return The object that represents the date and time when the daylight saving
	 * period begins. The value is in local time.
	 */
	var Start(default, never):cs.system.DateTime;
	function new(start:cs.system.DateTime, end:cs.system.DateTime, delta:cs.system.TimeSpan):Void;
}
