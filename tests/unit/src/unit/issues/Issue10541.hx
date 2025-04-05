package unit.issues;

@:publicFields
private abstract Foo(Int) from Int {
	static function make(f:Float) {
		return Std.int(f);
	}
}

class Issue10541 extends Test {
	function test() {
		eq(1, Foo.make(1.2));
	}
}
