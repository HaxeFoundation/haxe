package unit.issues;

class Issue5288 extends Test {
	function test() {
		var n : UInt = 3;
        var c = -cast(n & 1, Int);
        eq(-1, c);
	}
}