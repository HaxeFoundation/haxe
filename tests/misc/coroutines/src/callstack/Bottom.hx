package callstack;

function entry() {
	CoroRun.run(() -> CoroLower.foo());
}