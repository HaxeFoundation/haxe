package unit.issues;

class Issue4988 extends Test {
	static var value:Dynamic;

	function test() {
		try {
			var d:{i:Null<Int>} = null;
			value = (d.i > 0);
			t(false);
		} catch(e:Dynamic) {
			t(true);
		}
	}
}