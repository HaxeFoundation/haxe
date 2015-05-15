package unit.issues;

@:forward @:native("Short")
private abstract TestAbstract(TestClass) from TestClass to TestClass {
    public inline function new() {
        this = new TestClass();
    }
    public function getValue2() {
        return this.value * 2;
    }
}

private class TestClass {
    public var value:Int = 1;
    public function new() { }
}

class Issue4158 extends Test {
	function test() {
		var o = new TestAbstract();
		eq(2, o.getValue2());
	}
}