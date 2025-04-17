package issues.aidan;

@:coroutine function foo() : String {
	return Coroutine.suspend(cont -> {
		cont.resume('Hello, World!', null);
	});
}

class Issue38 extends utest.Test {
	function test() {
		Assert.equals("Hello, World!", Coroutine.run(foo));
	}
}