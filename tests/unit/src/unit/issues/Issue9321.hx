package unit.issues;

class Issue9321 extends unit.Test {
	function test() {
		eq(new Child().arg, "child");
	}
}

@:keep
private class Base {
	function new(arg = "base") {}
}

@:keep
private class Child extends Base {
	public final arg:String;
	public function new(arg = "child") {
		super();
		this.arg = arg;
	}
}

@:keep
private class GrandChild extends Child {
	public function new() {
		use(this);
		super();
	}
	@:pure(false) static function use(v:Any) {}
}
