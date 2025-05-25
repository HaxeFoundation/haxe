package issues.aidan;

@:coroutine function doSomethingUsefulOne() {
	return 13;
}

@:coroutine function doSomethingUsefulTwo() {
	return 29;
}

class Issue93 extends utest.Test {
	public function test() {
		Assert.equals(42, Coroutine.run(() -> doSomethingUsefulOne() + doSomethingUsefulTwo()));
	}
}