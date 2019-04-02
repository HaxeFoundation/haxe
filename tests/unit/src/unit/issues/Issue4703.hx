package unit.issues;

private class C {
	public var x:Int;
	public var y:Int;

	public inline function new(x:Int) {
		this.x = 1;
	}
}

class Issue4703 extends Test {

	function test() {
		var m = new C(12);
		if (Math.random() > 0.5) {
			m.y = 1;
		} else {
			m.y = 2;
		}
		useeeee(m.y);
		noAssert();
	}

	static function useeeee(d:Dynamic) { }
}