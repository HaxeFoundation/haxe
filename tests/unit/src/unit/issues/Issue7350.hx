package unit.issues;

class Issue7350 extends Test {
	// Return null through try-catch: null must propagate correctly,
	// not be confused with _hx_pcall_default sentinel (Lua-specific).
	function returnNullFromTry():Dynamic {
		try {
			return null;
		} catch (e:Dynamic) {
			return "error";
		}
	}

	// Return false through try-catch: falsy values must propagate.
	function returnFalseFromTry():Bool {
		try {
			return false;
		} catch (e:Dynamic) {
			return true;
		}
	}

	// Return zero through try-catch.
	function returnZeroFromTry():Int {
		try {
			return 0;
		} catch (e:Dynamic) {
			return -1;
		}
	}

	// Try body falls through without returning: code after try-catch
	// must execute (pcall sentinel must NOT trigger a return).
	function fallThroughTry():Int {
		var x = 1;
		try {
			x = 2;
		} catch (e:Dynamic) {
			x = 3;
		}
		return x + 10;
	}

	// Exception in try: catch block should handle it and return.
	function returnFromCatch():String {
		try {
			throw "boom";
		} catch (e:Dynamic) {
			return "caught";
		}
	}

	// Nested try-catch: inner return must propagate through outer.
	function nestedTryCatch():Int {
		try {
			try {
				return 99;
			} catch (e:Dynamic) {
				return -1;
			}
		} catch (e:Dynamic) {
			return -2;
		}
	}

	// Try-catch in a loop: return exits the function, not just the loop.
	function returnFromTryInLoop():Int {
		for (i in 0...5) {
			try {
				if (i == 3)
					return i;
			} catch (e:Dynamic) {
				return -1;
			}
		}
		return -2;
	}

	// Try-catch where try falls through and catch is not entered:
	// subsequent code must see side effects from the try body.
	function sideEffectsInTry():String {
		var buf = new StringBuf();
		buf.add("a");
		try {
			buf.add("b");
		} catch (e:Dynamic) {
			buf.add("x");
		}
		buf.add("c");
		return buf.toString();
	}

	function test() {
		eq(null, returnNullFromTry());
		eq(false, returnFalseFromTry());
		eq(0, returnZeroFromTry());
		eq(12, fallThroughTry());
		eq("caught", returnFromCatch());
		eq(99, nestedTryCatch());
		eq(3, returnFromTryInLoop());
		eq("abc", sideEffectsInTry());
	}
}
