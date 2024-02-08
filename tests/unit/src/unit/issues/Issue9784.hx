package unit.issues;

private class C implements I {
	public var v(get,never):String;
	function get_v():String return "";

	public function f(i:I) {
		trace(i.v); // Could not resolve accessor
	}
}

private interface I {
	var v(get,never):String;
	function f(i:I):Void;
}

class Issue9784 extends unit.Test {
	function test() {
		utest.Assert.pass();
	}
}