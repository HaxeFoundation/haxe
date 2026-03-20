package unit.issues;

class Issue5362 extends unit.Test {
	function test() {
		var a:UInt = Std.random(256);
		var b = messType(a.toInt());
		eq(a, b);
	}

	static inline function messType(r:Int):Int {
		return 0xFF & r;
	}
}
