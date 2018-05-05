package unit.issues;

class Issue4702 extends Test {
	function test() {
		var t = 5;
		var s = "";
		for (i in 0...t) {
			s += "," + t--;
		}
		eq(",5,4,3,2,1", s);
	}
}