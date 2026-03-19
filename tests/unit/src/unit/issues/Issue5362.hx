package unit.issues;

class Issue5362 extends unit.Test {
	function test() {
		#if loose_numeric_casts
		var a:UInt = Std.random(256);
		var b = messType(a);
		eq(a, b);
		#else
		noAssert();
		#end
	}

	static inline function messType(r:Int):Int {
		return 0xFF & r;
	}
}
