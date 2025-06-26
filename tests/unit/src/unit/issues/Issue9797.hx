package unit.issues;

private abstract class AbstractClass {
	abstract function test<T>():Void;
}

private class ConcreteClass extends AbstractClass {
	function test<T>():Void {}
}

class Issue9797 extends unit.Test {
	function test() {
		utest.Assert.pass();
	}
}