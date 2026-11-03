package unit.issues;

import utest.Assert;

private class Foo {
    var prop(never, set):Int;

	function set_prop(i:Int) {
		return i;
	}

    public function new() {}
}

class Issue12058 extends Test {
    function test() {
        Assert.notNull(new Foo());
    }
}