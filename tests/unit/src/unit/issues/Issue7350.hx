package unit.issues;

class Issue7350 extends Test {
	// Immediate return in try: generates `do return 42 end` before
	// `return _hx_pcall_default` — without do...end this is a Lua syntax error
	// (two returns at the same block level).
	function immediateReturnInTry():Int {
		try {
			return 42;
		} catch (e:Dynamic) {
			return -1;
		}
	}

	// Multiple returns at the try-block level: each needs do...end wrapping
	// to avoid syntax errors before the appended _hx_pcall_default.
	function multiReturnInTry(x:Int):Int {
		try {
			if (x > 10) return 100;
			if (x > 0) return x * 2;
			return 0;
		} catch (e:Dynamic) {
			return -1;
		}
	}

	function test() {
		eq(42, immediateReturnInTry());
		eq(100, multiReturnInTry(20));
		eq(10, multiReturnInTry(5));
		eq(0, multiReturnInTry(-1));
	}
}
