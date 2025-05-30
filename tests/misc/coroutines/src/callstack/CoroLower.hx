package callstack;

@:coroutine function foo() {
	yield();

	SyncMiddle.syncFun1();
}