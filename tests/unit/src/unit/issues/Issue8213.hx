package unit.issues;

class Issue8213 extends unit.Test {
	function test() {
		var s = "hi";
		switch s {
			case null:
			case "hi":
			case "no":
		}
		utest.Assert.pass();
	}
}