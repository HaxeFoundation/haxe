package unit.issues;

class Issue6822 extends Test {
	function test() {
		var o:BiggerThing = { something:1, somethingElse:2 };
		eq(o.something, 1);
		eq(o.somethingElse, 2);
	}
}

@:structInit
private class Thing {
	public var something:Int;
}

@:structInit
private class BiggerThing extends Thing {
	public var somethingElse:Float;
}
