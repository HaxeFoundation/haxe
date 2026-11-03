package unit.issues;

@:generic
private class Base<Const> {
	public var value:String;

	public function new() {
		value = "Base";
	}

	public function get() {
		var f = function() {
			return value;
		}
		return f();
	}
}

private class Child extends Base<"Hello"> {
	public function new() {
		super();
		value = "Child";
	}
}

class Issue12209 extends unit.Test {
	function test() {
		eq("Child", new Child().get());
		eq("Base", new Base<"Hello">().get());
		eq("Base", new Base<"World">().get());
	}
}
