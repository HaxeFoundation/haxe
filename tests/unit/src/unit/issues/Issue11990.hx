package unit.issues;

class Issue11990 extends unit.Test {
	public function test() {
		#if (cpp || jvm || hl)
		var x:Single = std.Math.NaN;
		eq(true, x != x);
		var x:Float = std.Math.NaN;
		eq(true, x != x);
		#else
		noAssert();
		#end
	}
}
