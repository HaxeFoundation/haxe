package unit.issues;

import utest.Assert;

@:build(unit.issues.misc.Issue12030Macro.build())
private class Foo {
	public function new() {}

	public function bar() {
		static var last = 0;
		if (last == 0) {
			last = 1;
			return 'A';
		} else {
			return 'B';
		}
	}
}

class Issue12030 extends Test {
	function test() {
		var foo = new Foo();
		eq('A', foo.bar());
		eq('B', foo.bar());
	}
}
