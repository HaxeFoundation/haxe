package unit.issues;

import unit.Test;

class Issue9177 extends Test {
	public function test() {
		eq(123, ({}:C).x);
	}
}

@:structInit
private class C {
	public var x:Int;
	public function new(x:Int = 123) {
		this.x = x;
	}
}