package unit.issues;

private class A extends B {
	public function new() {
		super([
			"sample" => C.E
		]);
	}
}

private class B {
	public var map:Map<String, C>;
	public function new(str:Map<String, C>) {
		map = str;
	}
}

private enum C {
	E;
}

class Issue5520 extends unit.Test {
	function test() {
		var a = new A();
		eq(C.E, a.map["sample"]);
	}
}