package unit.issues;

@:generic
private class Value<T> {
    public var v :T;

    public function new (v :T) {
        this.v = v;
    }
}

class Issue4273 extends Test {
	function test() {
		var anything = new Value<Dynamic>(666);
		eq(666, anything.v);
	}
}