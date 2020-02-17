package unit.issues;

class Issue5449 extends unit.Test {
	function test() {
		var s:Struct = {y:20};
		eq(20, s.y);
		eq(100, s.x);

		var s:Struct = {x:50, y:10};
		eq(10, s.y);
		eq(50, s.x);
	}
}

@:structInit
private class Struct {
	public var x:Int = 100;
	public var y:Int;
}