package unit.issues;

class Issue9974 extends unit.Test {
#if !neko
	function test() {
		var r = Std.random(0x7FFFFFFF);
		t(r >= 0);
	}
#end
}