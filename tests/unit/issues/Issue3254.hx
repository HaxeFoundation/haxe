package unit.issues;

private class Data {
	static public function test() { return 12; }
}

@:forward
private abstract AData(Data) { }

class Issue3254 extends Test {
	function test() {
		eq(12, AData.test());
	}
}