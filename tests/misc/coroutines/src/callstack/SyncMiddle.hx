package callstack;

function syncFun2() {
	CoroRun.run(() -> CoroUpper.bar());
}

function syncFun1() {
	syncFun2();
}