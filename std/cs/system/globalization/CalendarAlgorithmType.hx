package cs.system.globalization;

/** Specifies whether a calendar is solar-based, lunar-based, or lunisolar-based. */
@:native("System.Globalization.CalendarAlgorithmType")
extern enum abstract CalendarAlgorithmType(Int) {
	var LunarCalendar = 2;
	var LunisolarCalendar = 3;
	var SolarCalendar = 1;
	var Unknown = 0;
}
