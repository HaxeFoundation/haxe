package unit.issues;

class Issue4076 extends Test {
	function test() {
		var i:Int;
		var b = true;
		while (b && ((i = 18) < 18)) {
		  ++i;
		}
	}
}