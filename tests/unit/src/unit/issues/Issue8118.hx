package unit.issues;

class Issue8118 extends unit.Test {
	static var v:RootEnum;
	function test() {
		v = A(10);
		eq(10, switch v { case A(i): i; });
	}
}