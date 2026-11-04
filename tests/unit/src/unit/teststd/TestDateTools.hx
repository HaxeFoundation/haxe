package unit.teststd;

class TestDateTools extends unit.Test {
	public function test() {
		// leap year
		var d = new Date(2012, 1, 17, 1, 2, 3);
		eq(DateTools.getMonthDays(d), 29);

		// seconds/delta
		var diff = DateTools.seconds(59);
		var d2 = DateTools.delta(d, diff);
		eq(d2.toString(), "2012-02-17 01:03:02");

		eq(DateTools.format(d, "%d"), "17"); // day
		eq(DateTools.format(d, "%a"), "Fri"); // abbreviated day name
		eq(DateTools.format(d, "%w"), "5"); // weekday
		eq(DateTools.format(d, "%b"), "Feb"); // abbreviated month name
		eq(DateTools.format(d, "%B"), "February"); // full month name
		eq(DateTools.format(d, "%y"), "12"); // year without century
		eq(DateTools.format(d, "%Y"), "2012"); // year
		eq(DateTools.format(d, "%j"), "048"); // day of the year
		eq(DateTools.format(d2, "%F %T"), "2012-02-17 01:03:02");

		var d3 = new Date(2004, 4, 3, 21, 50, 39);
		eq(DateTools.format(d3, "%A"), "Monday"); // day name
		eq(DateTools.format(d3, "%w"), "1"); // weekday
		eq(DateTools.format(d3, "%m"), "05"); // month
		eq(DateTools.format(d3, "%Y"), "2004"); // year
		eq(DateTools.format(d3, "%j"), "124"); // day of the year

		// neko and php use the native strftime, which doesn't support the %- flag everywhere
		#if !(neko || php)
		eq(DateTools.format(d, "%-j"), "48"); // day of the year (no padding)
		eq(DateTools.format(d3, "%-d"), "3"); // day (no padding)
		eq(DateTools.format(d3, "%-m"), "5"); // month (no padding)
		eq(DateTools.format(d3, "%-y"), "4"); // year without century (no padding)
		#end

		eq(DateTools.getDayOfYear(new Date(2026, 0, 1, 0, 0, 0)), 1);
		eq(DateTools.getDayOfYear(new Date(2026, 11, 31, 23, 59, 59)), 365);
		eq(DateTools.getDayOfYear(new Date(2024, 11, 31, 0, 0, 0)), 366);
		eq(DateTools.getDayOfYear(new Date(2000, 2, 1, 0, 0, 0)), 61);
		// during daylight saving time in time zones that have it
		eq(DateTools.getDayOfYear(new Date(2026, 3, 1, 0, 30, 0)), 91);
		eq(DateTools.getDayOfYear(new Date(2026, 6, 1, 0, 0, 0)), 182);

		//UTC based timestamp generation
		#if (js || flash || php || cpp || python)
		eq(DateTools.makeUtc(1982, 10, 10, 14, 2, 20), 405784940000.);
		#end
	}
}
