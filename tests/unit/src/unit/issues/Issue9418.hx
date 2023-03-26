package unit.issues;

class Issue9418 extends unit.Test {
	function test() {
		var c:GrandChild = {int: 42, string: 'hi'};
		eq(42, c.int);
		eq('hi', c.string);
	}
}

@:structInit private class Base {
	public final string:String;
	public final int:Int;
}

@:structInit private class Child extends Base {
}

@:structInit private class GrandChild extends Child {
}
