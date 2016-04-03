package unit.issues;

class BaseClass {
	public function new() {}
}

class Child1 extends BaseClass {}

class Issue4310 extends Test {
	function test() {
		var child1:BaseClass = new Child1();
		cast(child1, BaseClass);
	}
}