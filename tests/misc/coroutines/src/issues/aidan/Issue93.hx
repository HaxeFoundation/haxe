package issues.aidan;

import haxe.coro.Coroutine.yield;

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
		Assert.equals(13, Coroutine.run(() -> doSomethingUsefulOne()));
		Assert.equals(13, Coroutine.run(() -> id(doSomethingUsefulOne())));
		Assert.equals(42, Coroutine.run(() -> doSomethingUsefulOne() + doSomethingUsefulTwo()));
		Assert.equals(42, Coroutine.run(() -> doSomethingUsefulOneYield() + doSomethingUsefulTwo()));
		Assert.equals(42, Coroutine.run(() -> doSomethingUsefulOne() + doSomethingUsefulTwoYield()));
		Assert.equals(42, Coroutine.run(() -> doSomethingUsefulOneYield() + doSomethingUsefulTwoYield()));
		Assert.equals(42, Coroutine.run(() -> sum(doSomethingUsefulOne(), doSomethingUsefulTwo())));
		Assert.equals(42, Coroutine.run(() -> sum(doSomethingUsefulOneYield(), doSomethingUsefulTwo())));
		Assert.equals(42, Coroutine.run(() -> sum(doSomethingUsefulOne(), doSomethingUsefulTwoYield())));
		Assert.equals(42, Coroutine.run(() -> sum(doSomethingUsefulOneYield(), doSomethingUsefulTwoYield())));
		Assert.equals(42, Coroutine.run(() -> id(doSomethingUsefulOne() + doSomethingUsefulTwo())));
		Assert.equals(42, Coroutine.run(() -> id(doSomethingUsefulOneYield() + doSomethingUsefulTwo())));
		Assert.equals(42, Coroutine.run(() -> id(doSomethingUsefulOne() + doSomethingUsefulTwoYield())));
		Assert.equals(42, Coroutine.run(() -> id(doSomethingUsefulOneYield() + doSomethingUsefulTwoYield())));
		Assert.equals(42, Coroutine.run(() -> id(sum(doSomethingUsefulOne(), doSomethingUsefulTwo()))));
		Assert.equals(42, Coroutine.run(() -> id(sum(doSomethingUsefulOneYield(), doSomethingUsefulTwo()))));
		Assert.equals(42, Coroutine.run(() -> id(sum(doSomethingUsefulOne(), doSomethingUsefulTwoYield()))));
		Assert.equals(42, Coroutine.run(() -> id(sum(doSomethingUsefulOneYield(), doSomethingUsefulTwoYield()))));
	}
}