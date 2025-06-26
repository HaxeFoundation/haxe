package unit.issues;

import utest.Assert;
import utest.UTest;

@:callable
private abstract EndFun(Void->Void) {
	@:from public static function fromFun(f:Void->Void):EndFun {
		return cast function() {
			f();
			f = () -> throw "double end";
		};
	}
}

private class C {
	var onEnd:Void->Void = null;

	public function new() {}

	public function call(f:EndFun) {
		f();
	}

	static public function run() {
		var m = new C();
		m.call(m.onEnd == null ? function() {} : m.onEnd);
		m.call(m.onEnd ?? function() {});
	}
}

class Issue11706 extends Test {
	function test() {
		C.run();
		Assert.pass();
	}
}
