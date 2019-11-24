package unit.issues;

class Issue7883 extends unit.Test {
	function test() {
		(0:A).x.f();
		noAssert();
	}
}

private abstract A(Int) from Int {
	public var x(get,never):A;
	function get_x() return 1;

	public function f() {}

	public function getX():A return 1;
}
