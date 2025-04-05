package unit.issues;

import unit.Test;

private class Func {
	public var f:Int->Int;

	public function new(f:Int->Int) {
		this.f = f;
	}
}

class Issue11919 extends Test {
	static function getInt() {
		return 5;
	}

	function test() {
		static var localFunction = new Func(a -> a);
		eq(2, localFunction.f(2));

		static var declaredLocal = {
			var f = getInt();
			f;
		}
		eq(5, declaredLocal);

		static var caughtVar = {
			try {
				throw "foo";
			} catch (s:String) {
				s;
			}
		}
		eq("foo", caughtVar);

		static var loopBreak = {
			var acc = 0;
			for (i in 0...getInt()) {
				acc += i;
				if (i == 2) {
					break;
				}
			}
			acc;
		}
		eq(3, loopBreak);

		static var loopContinue = {
			var acc = 0;
			for (i in 0...getInt()) {
				if (i & 1 == 0) {
					continue;
				}
				acc += i;
			}
			acc;
		}
		eq(4, loopContinue);
	}
}
