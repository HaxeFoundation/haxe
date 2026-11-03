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

		// neko uses the native strftime, which doesn't support the %- flag everywhere
		#if !neko
		eq(DateTools.format(d, "%d"), "17"); // day
		eq(DateTools.format(d, "%a"), "Fri"); // abbreviated day name
		eq(DateTools.format(d, "%w"), "5"); // weekday
		eq(DateTools.format(d, "%b"), "Feb"); // abbreviated month name
		eq(DateTools.format(d, "%B"), "February"); // full month name
		eq(DateTools.format(d, "%y"), "12"); // year without century
		eq(DateTools.format(d, "%Y"), "2012"); // year
		eq(DateTools.format(d, "%-j"), "48"); // day of the year (no padding)
		eq(DateTools.format(d2, "%F %T"), "2012-02-17 01:03:02");

		var d = new Date(2004, 4, 3, 21, 50, 39);
		eq(DateTools.format(d, "%-d"), "3"); // day (no padding)
		eq(DateTools.format(d, "%A"), "Monday"); // day name
		eq(DateTools.format(d, "%w"), "1"); // weekday
		eq(DateTools.format(d, "%m"), "05"); // month
		eq(DateTools.format(d, "%-m"), "5"); // month (no padding)
		eq(DateTools.format(d, "%-y"), "4"); // year without century (no padding)
		eq(DateTools.format(d, "%Y"), "2004"); // year
		eq(DateTools.format(d, "%j"), "124"); // day of the year
		#end

		//UTC based timestamp generation
		#if (js || flash || php || cpp || python)
		eq(DateTools.makeUtc(1982, 10, 10, 14, 2, 20), 405784940000.);
		#end
	}
}
