package issues.aidan;

class C1 {
	public function new() {}

	@:coroutine public function await() {}
}

class C2 {
	public function new() {}

	@:coroutine public function await() {}
}

class Issue91 extends utest.Test {
	function test() {
		final c1 = new C1();
		final c2 = new C2();
		CoroRun.run(() -> {
			c1.await();
			c2.await();
		});
		Assert.pass();
	}
}