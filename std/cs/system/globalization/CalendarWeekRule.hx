package cs.system.globalization;

/** Defines different rules for determining the first week of the year. */
@:native("System.Globalization.CalendarWeekRule")
extern enum CalendarWeekRule {
	FirstDay;
	FirstFourDayWeek;
	FirstFullWeek;
}
