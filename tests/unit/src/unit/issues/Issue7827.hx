package unit.issues;

import utest.Assert;
private class Base {}

private class Child extends Base {
    public function new() {}
}

private interface I {
    final x:Base;
}

private class C implements I {
	public final x:Child = new Child();
}

class Issue7827 extends unit.Test {
	function test() {
		Assert.pass();
	}
}