package callstack;

import haxe.coro.Coroutine.yield;

@:coroutine function recursion(i:Int, acc:Int) {
	yield();
	return if (i > 0) {
		recursion(i - 1, acc + i);
	} else {
		Top.topCall1();
	}
}

@:coroutine function bar() {
	yield();

	recursion(4, 0);
}