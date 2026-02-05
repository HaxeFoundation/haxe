package cs.system;

/** Specifies the day of the week. */
@:native("System.DayOfWeek")
extern enum abstract DayOfWeek(Int) {
	var Friday = 5;
	var Monday = 1;
	var Saturday = 6;
	var Sunday = 0;
	var Thursday = 4;
	var Tuesday = 2;
	var Wednesday = 3;
}
