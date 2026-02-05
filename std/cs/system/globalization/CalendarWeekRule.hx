package cs.system.globalization;

/** Defines different rules for determining the first week of the year. */
@:native("System.Globalization.CalendarWeekRule")
extern enum abstract CalendarWeekRule(Int) {
	var FirstDay = 0;
	var FirstFourDayWeek = 2;
	var FirstFullWeek = 1;
}
