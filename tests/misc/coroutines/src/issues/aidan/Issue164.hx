package issues.aidan;

class Issue164 extends utest.Test {
	@:coroutine function f() {
		{};
		throw "this won't run";
	}

	function test() {
		Assert.raises(() -> CoroRun.run(f), String);
	}
}