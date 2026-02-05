package cs.system.globalization;

/** Specifies whether a calendar is solar-based, lunar-based, or lunisolar-based. */
@:native("System.Globalization.CalendarAlgorithmType")
extern enum CalendarAlgorithmType {
	LunarCalendar;
	LunisolarCalendar;
	SolarCalendar;
	Unknown;
}
