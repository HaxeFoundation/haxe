package unit.issues;

class Issue4988 extends Test {
	function test() {
		try {
			var d:{i:Null<Int>} = null;
			foo(d.i > 0);
		} catch(e:Dynamic) {}
	}

	function foo(_) {}
}