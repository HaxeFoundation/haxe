package unit.issues;

@:generic class MyGeneric<@:const T> {
    public function new() {}

    public function foo() {
        return T;
    }
}


class Issue6979 extends unit.Test {
	function test() {
        var a:MyGeneric<1> = new MyGeneric();
        unit.HelperMacros.typedAs(a.foo(), 1);
        eq(3, a.foo() + 2);
	}
}