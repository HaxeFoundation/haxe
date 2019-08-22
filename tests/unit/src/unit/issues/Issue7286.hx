package unit.issues;

class Issue7286 extends unit.Test {
	function test() {
		new TestMap();
		noAssert();
	}
}

private abstract TestMap(Dynamic) {
	public function new() {
		fromObj();
	}

	inline function fromObj() {
		this = { };
	}
}