package unit.issues;
import unit.Test;

private class Test1 {
	public var x:Int = 42;
}

private class Test2 extends Test1 {
	public function new() { }
}

class Issue2722 extends Test {
	function test() {
		var test2 = new Test2();
		eq(42, test2.x);
	}
}