package issues.aidan;

@:coroutine function doSomethingUsefulOne() {
	return 13;
}

@:coroutine function doSomethingUsefulTwo() {
	return 29;
}

@:coroutine function doSomethingUsefulOneYield() {
	yield();
	return 13;
}

@:coroutine function doSomethingUsefulTwoYield() {
	yield();
	return 29;
}

function sum(a:Int, b:Int) {
	return a + b;
}

function id(a:Int) {
	return a;
}

class Issue93 extends utest.Test {
	public function test() {
		Assert.equals(13, CoroRun.run(() -> doSomethingUsefulOne()));
		Assert.equals(13, CoroRun.run(() -> id(doSomethingUsefulOne())));
		Assert.equals(42, CoroRun.run(() -> doSomethingUsefulOne() + doSomethingUsefulTwo()));
		Assert.equals(42, CoroRun.run(() -> doSomethingUsefulOneYield() + doSomethingUsefulTwo()));
		Assert.equals(42, CoroRun.run(() -> doSomethingUsefulOne() + doSomethingUsefulTwoYield()));
		Assert.equals(42, CoroRun.run(() -> doSomethingUsefulOneYield() + doSomethingUsefulTwoYield()));
		Assert.equals(42, CoroRun.run(() -> sum(doSomethingUsefulOne(), doSomethingUsefulTwo())));
		Assert.equals(42, CoroRun.run(() -> sum(doSomethingUsefulOneYield(), doSomethingUsefulTwo())));
		Assert.equals(42, CoroRun.run(() -> sum(doSomethingUsefulOne(), doSomethingUsefulTwoYield())));
		Assert.equals(42, CoroRun.run(() -> sum(doSomethingUsefulOneYield(), doSomethingUsefulTwoYield())));
		Assert.equals(42, CoroRun.run(() -> id(doSomethingUsefulOne() + doSomethingUsefulTwo())));
		Assert.equals(42, CoroRun.run(() -> id(doSomethingUsefulOneYield() + doSomethingUsefulTwo())));
		Assert.equals(42, CoroRun.run(() -> id(doSomethingUsefulOne() + doSomethingUsefulTwoYield())));
		Assert.equals(42, CoroRun.run(() -> id(doSomethingUsefulOneYield() + doSomethingUsefulTwoYield())));
		Assert.equals(42, CoroRun.run(() -> id(sum(doSomethingUsefulOne(), doSomethingUsefulTwo()))));
		Assert.equals(42, CoroRun.run(() -> id(sum(doSomethingUsefulOneYield(), doSomethingUsefulTwo()))));
		Assert.equals(42, CoroRun.run(() -> id(sum(doSomethingUsefulOne(), doSomethingUsefulTwoYield()))));
		Assert.equals(42, CoroRun.run(() -> id(sum(doSomethingUsefulOneYield(), doSomethingUsefulTwoYield()))));
	}
}