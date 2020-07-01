package unit.issues;

class Issue9678 extends unit.Test {
	function test() {
		var called = 0;
		function returnVoid() {
			called++;
		}
		new C(42).next(returnVoid).handle(_ -> called++);
		eq(2, called);
	}

	@:keep static function explicitVoidArg(arg:Void) {}
}

private class C<T> {
	final v:T;
	public function new(v:T) this.v = v;
	public function next<S>(f:()->S):C<S> return new C(f());
	public function handle(cb:T->Void) {cb(v);}
}
