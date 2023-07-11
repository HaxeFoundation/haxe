package unit.issues;

import unit.HelperMacros.typedAs;

private abstract O(String) {
	public function new(s)
		this = s;

	@:to function toInt()
		return 42;
}

private abstract A<T>(T) from T {}

class Issue6751 extends Test {
	function test() {
		function make<T>(o:A<T>)
			return o;

		var o = new O("hello");
		var a = make(o);
		typedAs(a, (null : A<O>));
		eq(Std.string(a), "hello");
	}
}
