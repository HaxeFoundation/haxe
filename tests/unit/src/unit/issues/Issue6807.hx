package unit.issues;

class Issue6807 extends unit.Test {
	function test() {
		#if ((dce != 'no') && !eval)
		t(null == Type.resolveClass('unit.issues._Issue6807.ShouldBeRemovedByDce'));
		#else
		noAssert();
		#end
	}
}

private class ShouldBeRemovedByDce {
	public var a = 0;
	public var b(get,never):Int;
	function get_b() return 0;
}