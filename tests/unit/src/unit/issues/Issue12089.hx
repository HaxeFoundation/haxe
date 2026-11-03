package unit.issues;

class Issue12089 extends Test {
	function test() {
		var c = new Complex(1, 2);
		feq(1., c.i);
		feq(2., c.j);
	}
}
