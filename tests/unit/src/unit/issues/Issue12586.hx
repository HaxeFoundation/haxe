package unit.issues;

import utest.Assert;

private enum E {
	A;
	B;
}

class Issue12586 extends Test {
	function test() {
		final callback:() -> E = () -> A;

		eq(A, callback());
	}
}
