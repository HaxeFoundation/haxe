package unit.issues;

private class C {
	extern inline function get() {
		var intStruct = { i: 0 };
		return intStruct;
	}

	var a = [get(), get(), get()];

	public var b:Bool;
	public function new() {
		b = a[0] == a[1];
	}
}

class Issue4886 extends Test {
	function test() {
		var c = new C();
		f(c.b);
	}
}