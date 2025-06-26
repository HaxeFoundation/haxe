package unit.issues;

class Issue8944 extends Test {
	function test() {
		Child.test1(1, 2);
		Child.test2();
		Child.test3(1);
		noAssert();
	}
}

@:keep
private class Parent {
	static public function test1(a:Int, b:Int = null, c:Int, d:Int = null) {}
	static public function test2(a:Int) {}
	static public function test3() {}
}

@:keep
private class Child extends Parent {
	static public function test1(a:Int, b:Int) {}
	static public function test2() {}
	static public function test3(a:Int) {}
}