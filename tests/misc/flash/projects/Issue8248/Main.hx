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
	@:protected
	override function f() return "bye";

	public function getF() return f();
}

class Main {
	static function main() {
		var gc = new GrandChild();
		var egc = new ExternGrandChild();
		var s:String = egc.getF();
	}
}
