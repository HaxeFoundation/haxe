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

		//UTC based timestamp generation
		#if (js || flash || php || cpp || python)
		eq(DateTools.makeUtc(1982, 10, 10, 14, 2, 20), 405784940000.);
		#end
	}
}
