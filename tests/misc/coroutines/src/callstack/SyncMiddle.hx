package callstack;

function syncFun2() {
	Coroutine.run(() -> CoroUpper.bar());
}

function syncFun1() {
	syncFun2();
}