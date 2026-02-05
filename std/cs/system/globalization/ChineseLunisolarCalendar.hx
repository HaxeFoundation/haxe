package cs.system.globalization;

/** Represents time in divisions, such as months, days, and years. Years are calculated using the Chinese calendar, while days and months are calculated using the lunisolar calendar. */
@:native("System.Globalization.ChineseLunisolarCalendar")
extern class ChineseLunisolarCalendar extends cs.system.globalization.EastAsianLunisolarCalendar {
	/** Specifies the era that corresponds to the current  object. */
	static var ChineseEra(default, never):Int;
	function new():Void;
	/**
	 * Retrieves the era that corresponds to the specified  type.
	 * @param time The  type to read.
	 * @return An integer that represents the era in the  parameter.
	 */
	function GetEra(time:cs.system.DateTime):Int;
}
