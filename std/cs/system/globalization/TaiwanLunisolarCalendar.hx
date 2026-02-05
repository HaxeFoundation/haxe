package cs.system.globalization;

/** Represents the Taiwan lunisolar calendar. As for the Taiwan calendar, years are calculated using the Gregorian calendar, while days and months are calculated using the lunisolar calendar. */
@:native("System.Globalization.TaiwanLunisolarCalendar")
extern class TaiwanLunisolarCalendar extends cs.system.globalization.EastAsianLunisolarCalendar {
	function new():Void;
	/**
	 * Retrieves the era that corresponds to the specified .
	 * @param time The  to read.
	 * @return An integer that represents the era specified in the  parameter.
	 */
	function GetEra(time:cs.system.DateTime):Int;
}
