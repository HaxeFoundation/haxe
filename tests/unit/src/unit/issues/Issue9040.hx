package unit.issues;

class Issue9040 extends unit.Test {
	static var to:Int = 10;
	function test() {
		aeq([0, 2, 3, 4, 6, 8, 9], [for (i in 0...10) if(i % 2 == 0) i else if(i % 3 == 0) i]);
	}
}