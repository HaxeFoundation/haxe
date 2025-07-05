package issues.aidan;

private enum abstract Foo(Int) {
	var Bar;
}

class Issue160 extends utest.Test {
	@:coroutine function foo(f:Foo) {
		return 0;
	}

	function test() {
		CoroRun.runScoped(_ -> {
			foo(Bar);
		});

		Assert.pass('Should not result in a compilation error');
	}
}