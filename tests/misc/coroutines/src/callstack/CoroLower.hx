package callstack;

import haxe.coro.Coroutine.yield;

@:coroutine function foo() {
	yield();

	SyncMiddle.syncFun1();
}