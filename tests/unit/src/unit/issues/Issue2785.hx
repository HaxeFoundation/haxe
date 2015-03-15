package unit.issues;
import unit.Test;

private class Base {
	public function new() {}

	@:extern
	public var foo(default, never):Int;

	@:getter(foo)
	@:keep
	public function get_foo() {
		return 1;
	}
}

private class Child extends Base {
	@:getter(foo)
	override public function get_foo() {
		return 2;
	}
}

class Issue2785 extends Test {
	#if flash
	function test() {
		var base = new Base();
		eq(1, base.foo);

		var child = new Child();
		eq(2, child.foo);
	}
	#end
}