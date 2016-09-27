package unit.issues;

class Issue2718 extends Test {
	function test() {
		var testMap = new Map<Int, String>();
		var x0:UInt = 0;
		var x1:UInt = 1;
		var x2:UInt = 2;

		testMap[x0] = "0";
		testMap[x1] = "1";
		testMap[x2] = "2";

		eq("0", testMap[x0]);
		eq("1", testMap[x1]);
		eq("2", testMap[x2]);
	}
}