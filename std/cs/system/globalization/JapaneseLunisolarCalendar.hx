package cs.system.globalization;

/** Represents time in divisions, such as months, days, and years. Years are calculated as for the Japanese calendar, while days and months are calculated using the lunisolar calendar. */
@:native("System.Globalization.JapaneseLunisolarCalendar")
extern class JapaneseLunisolarCalendar extends cs.system.globalization.EastAsianLunisolarCalendar {
	/** Specifies the current era. */
	static var JapaneseEra(default, never):Int;
	function new():Void;
	/**
	 * Retrieves the era that corresponds to the specified .
	 * @param time The  to read.
	 * @return An integer that represents the era specified in the  parameter.
	 */
	function GetEra(time:cs.system.DateTime):Int;
}
