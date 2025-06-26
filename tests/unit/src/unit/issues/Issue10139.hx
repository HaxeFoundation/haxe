package unit.issues;

import utest.Assert;

private abstract class A { // This class has uninitialized final vars, which requires a constructor
	public final x:Int; // Example of an uninitialized final var
}

private class B extends A {
	public function new() {
		x = 10; // Cannot access field or identifier x for writing
	}
}

class Issue10139 extends Test {
	function test() {
		eq(10, new B().x);
	}
}
