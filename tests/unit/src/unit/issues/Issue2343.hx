package unit.issues;

private abstract MyInt(Int) from Int {
    public function work() return this;
}

private class Foo<T:MyInt> {
	public var result:Int;
    public function new(val : T) {
        result = val.work();
    }
}

class Issue2343 extends unit.Test {
	function test() {
		var foo = new Foo<MyInt>(1);
		eq(1, foo.result);

		// we cannot actually test this because it is delayed
		//t(unit.TestType.typeError(var foo2 = new Foo<String>("1")));
	}
}