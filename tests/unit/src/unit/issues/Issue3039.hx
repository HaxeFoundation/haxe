package unit.issues;

class Issue3039 extends Test {
	static inline var a:Int = 1;
	static inline var b:Float = 2;
	static inline var c = Std.int(a + b);

	function test() {
		eq(3, c);
	}
}