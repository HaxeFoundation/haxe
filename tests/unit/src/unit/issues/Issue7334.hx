package unit.issues;

class Issue7334 extends Test {
	function test() {
		var foo = 0;
		new Signal1<Int>(i -> foo = i).dispatch(1);
		eq(1, foo);
	}
}

class Signal1<T> extends BaseSignal<T->Void> {
	public function new(handler:T->Void) {
		this.dispatch = value -> handler(value);
	}
}

class BaseSignal<T> {
	public var dispatch:T;
}
