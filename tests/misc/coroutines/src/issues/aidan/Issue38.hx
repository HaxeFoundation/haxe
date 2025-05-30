package issues.aidan;

@:coroutine function foo() : String {
	return suspend(cont -> {
		cont.resume('Hello, World!', null);
	});
}

class Issue38 extends utest.Test {
	function test() {
		Assert.equals("Hello, World!", CoroRun.run(foo));
	}
}