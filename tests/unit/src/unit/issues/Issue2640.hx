package unit.issues;
import unit.Test;

abstract A<T>(Int) {
	public function new(i) {
		this = i;
	}

	macro public function test(self) {
		var t = haxe.macro.Context.typeof(self);
		return macro $v{haxe.macro.TypeTools.toString(t)};
	}
}

class Issue2640 extends Test {
	function test() {
		var a = new A<String>(12);
		eq(a.test(), "unit.issues.A<String>");
	}
}