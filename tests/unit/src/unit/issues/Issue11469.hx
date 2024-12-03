package unit.issues;

class Issue11469 extends Test {
	function test() {
		static var c = 10;
		static var d = c + 1;
		eq(11, d);
	}
}
