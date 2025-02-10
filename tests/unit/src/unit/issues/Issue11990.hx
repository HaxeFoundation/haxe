package unit.issues;

class Issue11990 extends unit.Test {
	public function test() {
		#if (cpp || jvm || hl)
		var x:Single = std.Math.NaN;
		eq(false, x == x);
		#else
		noAssert();
		#end
	}
}
