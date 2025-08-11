package issues.aidan;

using issues.aidan.Issue167.Foo;

private class Foo {
	@:coroutine public static function bar(s:String) {
		delay(100);

		return s;
	}
}

class Issue167 extends utest.Test {
	function test() {
		Assert.equals("test", CoroRun.run(() -> {
			"test".bar();
		}));
	}
}