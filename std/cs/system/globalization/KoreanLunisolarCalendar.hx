package cs.system.globalization;

/** Represents time in divisions, such as months, days, and years. Years are calculated using the Gregorian calendar, while days and months are calculated using the lunisolar calendar. */
@:native("System.Globalization.KoreanLunisolarCalendar")
extern class KoreanLunisolarCalendar extends cs.system.globalization.EastAsianLunisolarCalendar {
	/** Specifies the Gregorian era that corresponds to the current  object. */
	static var GregorianEra(default, never):Int;
	function new():Void;
	/**
	 * Retrieves the era that corresponds to the specified .
	 * @param time The  to read.
	 * @return An integer that represents the era specified by the  parameter. The
	 * return value for a  object is always the  value.
	 */
	function GetEra(time:cs.system.DateTime):Int;
}
