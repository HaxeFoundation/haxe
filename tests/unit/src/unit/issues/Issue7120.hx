package unit.issues;

private class Base {
	public function new() { }
	public function test() return 12;
}

private class Child1 extends Base { }
private class Child2 extends Base { }
private class Child3 extends Base { }

class Issue7120 extends unit.Test {
	function test1() {
		var t = try 1. catch(e:Dynamic) 1;
		feq(1., t);
	}

	function test2() {
		var t = try new Child1() catch(e:String) new Child2() catch(e:Dynamic) new Child3();
		eq(12, t.test());
	}
}