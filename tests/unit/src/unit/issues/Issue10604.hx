package unit.issues;

private abstract Promise<T>(Dynamic) {
	@:from static inline function ofData<T>(d:T):Promise<T>
		throw 0;
}

@:callable
private abstract Next<In, Out>(In->Promise<Out>) from In->Promise<Out> to In->Promise<Out> {
	@:from static function ofSafeSync<In, Out>(f:In->Out):Next<In, Out>
		return x -> f(x); // error: Recursive implicit cast
}

class Issue10604 extends Test {
	function test() {
		utest.Assert.pass();
	}
}
