package unit.issues;

class Issue3877 extends unit.Test {
	function test() {
		var u:Null<UInt> = null;
		eq("null", Std.string(u));
		eq("null", "" + u);
	}
}
