package callstack;

function entry() {
	Coroutine.run(() -> CoroLower.foo());
}