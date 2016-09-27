package unit.issues;

private class Base {
	public function new() { }
	public function covariant():Base {
		return this;
	}
}

private class Child extends Base {
	public override function covariant():Child {
		return this;
	}
}


class Issue4222 extends Test {
	function test() {
		var c = new Child();
		var v = c.covariant();
	}
}