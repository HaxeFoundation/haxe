package unit.issues;

class Issue11356 extends unit.Test {
	function test() {
		var status = [1];
		for (a in status) [
			{
				var tmp = null;
			}
		];
		utest.Assert.pass();
	}
}
