package unit.issues;

class Issue3543 extends Test {
	function test() {
		#if loose_numeric_casts
		var a = Std.int((3 : UInt) / 2);
		eq(1, a);
		#else
		noAssert();
		#end
	}
}
