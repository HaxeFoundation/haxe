package unit.issues;

private class Vec {
	public var x:Float;

	public inline function new() {}
}

class Issue13229 extends unit.Test {
	static var count = 3;

	@:analyzer(ignore)
	function test() {
		var v = new Vec();
		var acc = 0.;
		for (i in 0...count) {
			var a = i * 1.5;
			if (i == 0)
				v.x = 10.0;
			acc += v.x + a * 0;
		}
		feq(30.0, acc);
	}
}
