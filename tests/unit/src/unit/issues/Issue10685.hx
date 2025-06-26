package unit.issues;

@:structInit
private abstract class A {
	public final x:Int;
}

@:structInit
private class B extends A {
	public var y:Int;
}

class Issue10685 extends Test {
	function test() {
		var b:B = {x: 1, y: 2};
		eq(1, b.x);
		eq(2, b.y);
	}
}
