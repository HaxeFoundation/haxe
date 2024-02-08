package unit.issues;

import unit.Test;

class Issue9177 extends Test {
	public function test() {
		eq(123, ({}:C).x);
		eq("hi", ({}:C).y);
	}
}

@:structInit
private class C {
	public var x:Int;
	public var y:String;
	public function new(x:Int = 123, y:String = "hi") {
		this.x = x;
		this.y = y;
	}
}