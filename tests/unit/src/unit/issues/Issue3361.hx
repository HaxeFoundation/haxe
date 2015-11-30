package unit.issues;

private interface I {
	public var v(default, never):Float;
}

private class C implements I {
	public var v:Int;
	public function new() { }
}

private interface I2 {
	public var v(never, default):Int;
}

private class C2 implements I2 {
	public var v:Float;
	public function new() { }
}

class Issue3361 extends Test {
	function test() {
		var c = new C();
		var i:I = c;
		c.v = 12;
		feq(12., i.v);

		var c2 = new C2();
		var i2:I2 = c2;
		i2.v = 12;
		feq(12., c.v);
	}
}