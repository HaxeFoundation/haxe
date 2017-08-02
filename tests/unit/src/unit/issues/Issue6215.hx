package unit.issues;

class Issue6215 extends unit.Test {
	function test() {
		var a:Action<Issue6215> = null;
		var signal:Abstr<Child> = null;
		signal += a;
	}
}

private class Child extends Issue6215 {}

private typedef Action<T> = T->Void;

private abstract Abstr<T>(Dynamic) {
	@:op(x += y) function opAdd(fn:Action<T>) return this;
}