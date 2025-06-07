package issues.aidan;

class Issue146 extends utest.Test {
	function test() {
		CoroRun.runScoped(_ -> {
			Assert.equals("time is 123456", 'time is ${123456i64}');
		});
	}
}