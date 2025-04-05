package unit.issues;

class Issue11317 extends Test {
	function test() {
		var array:Array<String> = ["0", "1", "2", "3", "4"];
		array.resize(0);
		array.resize(5);
		utest.Assert.same([null, null, null, null, null], array);
	}
}
