package unit.issues;

private typedef A = {
	function bar():String;
}

private class C {
	public function new() { }

	public function bar() {
		return "bar";
	}
}

class Issue8537 extends unit.Test {
	function test() {
		eq("bar", extract(new C()));
	}

	static function extract(a:A) {
		return invoke(a.bar);
	}

	static function invoke(f:() -> String) {
		return f();
	}
}