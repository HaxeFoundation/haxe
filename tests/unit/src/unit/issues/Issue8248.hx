package unit.issues;

#if flash
private class NoProtected {}

private class Base extends NoProtected {
	public var x:Int;

	public function new() {
		x = f();
	}

	@:protected function f() return 1;
}

private class Child extends Base {
}

private class GrandChild extends Child {
	override function f() return 2;
}

private class ExternChild extends Lib {}
private class ExternGrandChild extends ExternChild {
	@:protected // TODO: should we generate `protected` automatically here?
	override function f() return "bye";

	public function getF() return f();
}
#end

class Issue8248 extends unit.Test {
	#if flash
	function test() {
		eq(new GrandChild().x, 2);
		eq(new ExternGrandChild().getF(), "bye");
	}
	#end
}
